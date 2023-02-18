# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse","rPraat","furrr", "sosprosody","bezier"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

tar_source("Helpers")

# Change the number of workers here as needed
# this is running on a 12-core machine with 24 threads, if you don't set the
# number of workers then plan() will reserve the maximum number it can take
future::plan(future::multisession(workers = 10))

list(
  ## Track directories for modifications
  ## This will be outdated any time a file in the directory is added or removed
  tar_target(dir_01,
             command = "01_Mono/",
             format = 'file'),
  
  ## Track scripts for changes
  tar_target(extract_takes_script,
             command = "Scripts/extract_takes.praat",
             format = "file"),
  tar_target(gen_pitchtier_script,
             command = "Scripts/gen_manips_pitchtiers.praat",
             format = "file"),
  tar_target(force_align_script,
             command = "Scripts/force_align.sh",
             format = "file"),
  
  # If the mono recordings folder has been changed, the numbering of the
  # extracted takes is no longer valid/reliable, so all the files must be
  # removed and replaced to ensure that the files are as expected
  tar_target(clear_old_recordings,
             command = {
               dir_01
               # Add files in additional directories as needed
               files_to_remove <-
                 list.files("02_PossibleRecordings/",
                            recursive = TRUE,
                            full.names = TRUE)
               file.remove(files_to_remove)
             }),
  
  ## Execute scripts
  tar_target(
    name = extract_takes,
    command = {
      # Extract takes whenever there have been changes in the mono directory
      clear_old_recordings
      run_praatscript(script_path = extract_takes_script,
                      fromDir = "../01_Mono",
                      outDir = "../02_PossibleRecordings")
    }
  ),
  tar_target(
    name = generate_pitchtiers,
    command = {
      # Regenerate the pitchtiers if
      extract_takes
      run_praatscript(script_path = gen_pitchtier_script,
                      fromDir = "../02_PossibleRecordings",
                      outDir = "../02_PossibleRecordings/PitchTiers",
                      use_defaults = TRUE)
    }
  ),
  tar_target(
    name = run_force_aligner,
    command = {
      generate_pitchtiers
      shell(paste("bash", force_align_script))
    }
  ),
  ## Some batch processing stuff to run since it takes a while
  tar_target(process_textgrids,
             command = {
               run_force_aligner
               batch_process_textgrids("02_PossibleRecordings/MFA_textgrids/", parallelize = TRUE) |> 
                 tidyr::separate(col = "file",
                                 sep = "_",
                                 into = c("utterance","session", "tune", "take"),
                                 remove = FALSE)
               
             }),
  tar_target(process_pitchtiers,
             command = {
               generate_pitchtiers
               batch_process_pitchtiers("02_PossibleRecordings/PitchTiers/", parallelize = TRUE) |> 
                 tidyr::separate(col = "file",
                                 sep = "_",
                                 into = c("utterance","session", "tune", "take"),
                                 remove = FALSE)
               
             }),
  tar_target(two_syl_words,
             c("grandma", "branning", "bronville", "greenview", "broadway")),
  tar_target(three_syl_words,
             c("grandmother",
               "maryland",
               "weatherman",
               "evansville",
               "middleman",
               "governor",
               "manager",
               "northerner",
               "weaverville")),
  tar_target(nuclear_words,
             c(two_syl_words, three_syl_words)),
  tar_target(tune_order, c("LHH","HHH","HLL","LHSLL","LSHLL")),
  tar_target(textgrid_df,
             command = 
               tar_read("process_textgrids") |> 
               mutate(is_nuclear = word_label %in% nuclear_words,
                      wordset = ifelse(utterance %in% three_syl_words, "syl3", "syl2"))),
  tar_target(files_to_omit, command = check_pronunciations(textgrid_df)),
  ## Analysis files to track
  tar_target(average_contours,
             command = {
               tg_df <- 
                 textgrid_df |> 
                 filter(!file %in% files_to_omit$file)
               
               # Extract nuclear regions
               nuclear_regions <- get_nuclear_textgrids(tg_df, nuclear_words)
               
               # Preprocess pitchtracks
               pitchtier_df <- 
                 process_pitchtiers  |> 
                 preprocess_pitchtracks(nuclear_regions, 
                                        runmed_k = 5, 
                                        time_normalize = TRUE,
                                        .fromzero = TRUE) |> 
                 mutate(tune = factor(tune, levels = tune_order),
                        wordset = ifelse(utterance %in% three_syl_words, "syl3", "syl2"))
               
               # Calculate average pitch contours for each tune-utterance pair
               avg_contour_df <- 
                 pitchtier_df |>
                 # Fix coding issue with points outside of final boundary
                 mutate(is_nuclear = fix_OOB_nuclear_coding(is_nuclear),
                        tune = factor(tune, levels = tune_order)) |>
                 average_pitchtracks(section_by = "is_nuclear",
                                     pulses_per_section = c("FALSE" = 15,
                                                            "TRUE" = 30),
                                     time_by = "timepoint_norm",
                                     aggregate_by = file ~ utterance + tune + session,
                                     parallelize = TRUE) |>
                 # Smooth average points a little
                 running_median_smooth(.k = 5,
                                       .from = "avg_hz",
                                       .to = "avg_hz_smooth",
                                       .group = c("utterance", "tune")) |>
                 mutate(wordset = ifelse(utterance %in% three_syl_words, "syl3", "syl2"))
             }),
  tar_quarto(render_EDA, 
             path = "Writeups/raw_recordings_eda.qmd",
             extra_files = c("02_PossibleRecordings/",
                             "Helpers/EDA_helpers.R",
                             "Helpers/pitcherror_helpers.R"),
             deployment = 'worker'),
  tar_quarto(render_flowchart,
             path = "Writeups/flowchart.qmd",
             deployment = 'worker'),
  tar_target(get_all_spectral_measures,
             command = {
               get_dir_spectral_measures(
                 wavfile_from = "02_PossibleRecordings",
                 tg_from = "02_PossibleRecordings/MFA_textgrids",
                 tg_to = "02_PossibleRecordings/SpectralMeasuresTextgrids",
                 rewrite_tg = TRUE,
                 use_existing = FALSE)
               
               return(0)
             }),
  tar_quarto(render_spectral_measures,
             path = "Writeups/spectral_measures_analysis.qmd",
             extra_files = "02_PossibleRecordings/SpectralMeasuresTextgrids/spectral_measures.txt",
             deployment = 'worker'),
  tar_target(chosen_files,
             command = {
               render_EDA
               "CalculatedFiles/chosen_recordings.txt"
             },
             format = 'file'),
  tar_target(chosen_filepaths,
             command = file.path("02_PossibleRecordings",
                                 paste0(read_lines(chosen_files),".wav")),
             cue = tar_cue(depend = TRUE)),
  tar_target(move_chosen_recordings,
             command = {
               move_from <- chosen_filepaths
               move_to <- gsub("02_PossibleRecordings",
                               "03_ChosenRecordings",
                               move_from,
                               perl = TRUE)
               message("Clearing outdated chosen recordings")
               files_to_remove <- list.files("03_ChosenRecordings",
                                             recursive=TRUE, 
                                             full.names=TRUE) 
               # Don't delete the manually fixed files!!!!
               files_to_remove <- files_to_remove[!grepl("FixedTextGrids", files_to_remove)]
               file.remove(files_to_remove)
               file.copy(from = move_from, to = move_to)
               return(move_to)
             },
             format = "file"),
  tar_target(avg_phone_durations,
             command = {
               render_EDA
               "CalculatedFiles/avg_phone_duration_by_utt.rds"
             },
             format = "file"),
  tar_target(syllabification_spreadsheet,
             "syllabification.csv",
             format = 'file'),
  tar_target(syllable_specification, read_csv(syllabification_spreadsheet)),
  # TODO: on next opportunity to improve this section, wrap these next few
  #       targets into syllable_helpers.R
  tar_target(syllable_durations,
             command = 
               {
                 get_syllable_durations(avg_phone_durations, 
                                        syllable_specification) 
               }),
  tar_target(write_syllable_tgs_and_dts,
             command ={
               move_chosen_recordings
               textgrid_dir <- process_syllable_tiers(chosen_files,
                                                      syllable_specification,
                                                      syllable_durations)
               md5sum_dir(textgrid_dir)
             }) ,
  tar_target(apply_dt_manipulation,
             command = 
               {
                 write_syllable_tgs_and_dts
                 move_chosen_recordings # Ensure this is done first
                 file.remove(list.files("04_ScaledRecordings", full.names = TRUE))
                 run_praatscript("Scripts/apply_dt_manipulation.praat",
                                 use_defaults = TRUE,
                                 method = base::shell,
                                 praat_path = "Praat.exe")
                 md5sum_dir('04_ScaledRecordings/')
               }),
  tar_target(scaling_factors_to_try,
             command = c(1.00, .95, .90, .85, .80, .75, .70, .65)),
  tar_target(write_constant_scale_files,
             command = {
               apply_dt_manipulation
               filedir <- paste0("05_NudgedRecordings/ConstantScaling_", 
                                 scaling_factors_to_try*100)
               message("Applying constant duration manipulations: ", filedir)
               file.remove(list.files(filedir, 
                                      recursive = TRUE, 
                                      full.names = TRUE))
               write_constant_scaling_dts(scale_by = scaling_factors_to_try)
               return(list(dir = filedir,
                           md5 = md5sum_dir(filedir)))
               # return(filedir)
             },
             pattern = map(scaling_factors_to_try)),
  tar_target(apply_dt_nudges,
             command = {
               # write_constant_scale_files
               # filedir <- paste0("05_NudgedRecordings/ConstantScaling_", scaling_factors_to_try*100)
               filedir <- write_constant_scale_files[['dir']]
               message("Nudging files for ", filedir)
               run_praatscript("Scripts/apply_dt_manipulation.praat",
                               fromDir = "../04_ScaledRecordings",
                               dtDir = paste0("../",filedir,"/DurationTiers"),
                               outDir = paste0("../",filedir),
                               use_defaults = TRUE,
                               debug = FALSE,
                               praat_path = "Praat.exe",
                               method = base::shell)
               return(list(dir = filedir,
                           md5 = md5sum_dir(filedir, .recursive=TRUE)))
             },priority =.99,
             pattern = map(write_constant_scale_files)),
  
  ##############################
  # Resynthesis hyperparameters
  tar_target(hyp_nsteps, 5), # Affects # of PA and BT scaling steps, change to 7 later
  tar_target(hyp_nalignment, 5), # Affects # of alignment steps
  tar_target(hyp_nbezier, 13), # Affects quantization of bezier curves
  #################################
  # Resynthesis-related targets for Exp 1
  tar_target(resynthesis_praatscript,
             command = "Scripts/resynthesize_exp1.praat",
             format = "file"),
  tar_target(save_nuclear_regions_praatscript,
             command = "Scripts/save_nuclear_regions.praat",
             format = "file"),
  tar_target(resynthesize_files,
             command = {
               apply_dt_nudges
               wav_outdir <- 
                 run_resynthesis(
                   wav_fromDir = apply_dt_nudges[['dir']],
                   resynth_script = resynthesis_praatscript,
                   wav_outDir_pattern = "06_ResynthesizedRecordings/ConstantScaling_",
                   nSteps = hyp_nsteps
                 )
               non_relative_path <- gsub("^../","", wav_outdir)
               return(list(dir = wav_outdir,
                           md5 = md5sum_dir(non_relative_path,.recursive=FALSE)))
             },
             priority = .90,
             resources =tar_resources(future = 
                                        tar_resources_future(
                                          resources = list(num_cores=4))),
             pattern = map(apply_dt_nudges)),
  tar_target(make_nuclear_regions,
             command = {
               save_nuclear_regions_praatscript
               save_nuclear_region_csv(resynthesize_files[['dir']])
             },
             pattern = map(resynthesize_files)),
  tar_target(resynth_pt_df,
             command = {
               save_raw_pt_dfs(resynthesize_files[['dir']])
             },
             pattern = map(resynthesize_files)),
  tar_target(preprocess_resynthesis,
             command = {
               resynth_pt_df
               make_nuclear_regions
               
               save_preprocessed_resynthesis(resynthesize_files[['dir']],
                                             "exp1",
                                             tune_order,
                                             three_syl_words)
             },
             format = 'file',
             pattern = map(resynthesize_files)),
  tar_target(exp1_average_resynthesis,
             command = {
               save_avg_resynthesis(preprocess_resynthesis,
                                    "exp1",
                                    tune_order)
             },
             format = 'file',
             pattern = map(preprocess_resynthesis)),
  tar_quarto(render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             extra_files = list.files("CalculatedFiles",
                                      pattern = "exp1_avg_resynth",
                                      full.names = TRUE)),
  ###########################
  # Experiment 2 (bitonal accent) targets
  
  ###########################
  # Experiment 3 (alignment manip) targets
  tar_target(exp3_resynthesis_praatscript,
             command = "Scripts/resynthesize_exp3_straight.praat",
             format = "file"),
  tar_target(exp3_resynthesize_files,
             command = {
               apply_dt_nudges
               wav_outdir <- 
                 run_resynthesis(
                   wav_fromDir = apply_dt_nudges[['dir']],
                   resynth_script = exp3_resynthesis_praatscript,
                   wav_outDir_pattern = "06c_ResynthesizedRecordingsExp3/ConstantScaling_",
                   nSteps = hyp_nsteps
                 )
               non_relative_path <- gsub("^../","", wav_outdir)
               return(list(dir = wav_outdir,
                           md5 = md5sum_dir(non_relative_path,.recursive=FALSE)))
             },
             priority = .3,
             resources =tar_resources(future = 
                                        tar_resources_future(
                                          resources = list(num_cores=4))),
             pattern = map(apply_dt_nudges)),
  tar_target(exp3_make_nuclear_regions,
             command = {
               save_nuclear_region_csv(exp3_resynthesize_files[['dir']])
             },
             pattern = map(exp3_resynthesize_files)),
  tar_target(exp3_resynth_pt_df,
             command = {
               save_raw_pt_dfs(exp3_resynthesize_files[['dir']])
             },
             pattern = map(exp3_resynthesize_files)),
  tar_target(exp3_preprocess_resynthesis,
             command = {
               exp3_resynth_pt_df
               exp3_make_nuclear_regions
               
               save_preprocessed_resynthesis(exp3_resynthesize_files[['dir']],
                                             "exp3",
                                             tune_order,
                                             three_syl_words)
             },
             format = 'file',
             pattern = map(exp3_resynthesize_files)),
  tar_target(exp3_average_resynthesis,
             command = {
               save_avg_resynthesis(exp3_preprocess_resynthesis,
                                    "exp3",
                                    tune_order)
             },
             format = 'file',
             pattern = map(exp3_preprocess_resynthesis)),
  tar_quarto(exp3_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             extra_files = list.files("CalculatedFiles",
                                      pattern = "exp3_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "exp3", 
                                   nucpath = "06c_ResynthesizedRecordingsExp3/ConstantScaling_")),
  ###########################
  # Experiment 3 (alignment manip, curved) targets
  tar_target(exp3curved_resynthesis_praatscript,
             command = "Scripts/resynthesize_exp3_curved.praat",
             format = "file"),
  tar_target(lhs_points,
             command = make_bitonal_curve2(0,.8,.25,.5,
                                           70, 120)),
  tar_target(lsh_points,
             make_bitonal_curve2(.5, 1.15, 1, 1,
                                 70, 120)),
  tar_target(exp3_bezier_points,
             command = {
               continuum_points <- 
                 make_bezier_continuum2(lhs_points,
                                        lsh_points, 
                                        hyp_nalignment, 
                                        hyp_nbezier)
               
               arrange(continuum_points, step,x) |>
                 write.csv("Scripts/bezierpoints.csv",row.names = FALSE)
               
               continuum_points
             }),
  tar_target(exp3curved_resynthesize_files,
             command = {
               apply_dt_nudges
               exp3curved_resynthesis_praatscript
               exp3_bezier_points
               wav_outdir <- 
                 run_resynthesis(
                   wav_fromDir = apply_dt_nudges[['dir']],
                   resynth_script = exp3curved_resynthesis_praatscript,
                   wav_outDir_pattern = "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_",
                   nSteps = hyp_nsteps,
                   nAlignmentSteps = hyp_nalignment,
                   nBezierPoints = hyp_nbezier
                 )
               non_relative_path <- gsub("^../","", wav_outdir)
               return(list(dir = wav_outdir,
                           md5 = md5sum_dir(non_relative_path,.recursive=FALSE)))
             },
             priority = .3,
             resources =tar_resources(future = 
                                        tar_resources_future(
                                          resources = list(num_cores=4))),
             pattern = map(apply_dt_nudges)),
  tar_target(exp3curved_make_nuclear_regions,
             command = {
               save_nuclear_region_csv(exp3curved_resynthesize_files[['dir']])
             },
             pattern = map(exp3curved_resynthesize_files)),
  tar_target(exp3curved_resynth_pt_df,
             command = {
               save_raw_pt_dfs(exp3curved_resynthesize_files[['dir']])
             },
             pattern = map(exp3curved_resynthesize_files)),
  tar_target(exp3curved_preprocess_resynthesis,
             command = {
               exp3curved_resynth_pt_df
               exp3curved_make_nuclear_regions
               
               save_preprocessed_resynthesis(exp3curved_resynthesize_files[['dir']],
                                             "exp3curved",
                                             tune_order,
                                             three_syl_words)
             },
             format = 'file',
             pattern = map(exp3curved_resynthesize_files)),
  tar_target(exp3curved_average_resynthesis,
             command = {
               save_avg_resynthesis(exp3curved_preprocess_resynthesis,
                                    "exp3curved",
                                    tune_order)
             },
             format = 'file',
             pattern = map(exp3curved_preprocess_resynthesis)),
  tar_quarto(exp3curved_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             extra_files = list.files("CalculatedFiles",
                                      pattern = "exp3curved_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "exp3curved", 
                                   nucpath = "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_")),
  ################################
  # Exp 2: L+H*, scaling in PA peak
  #
  tar_target(exp2_resynthesis_praatscript,
             command = "Scripts/resynthesize_exp2.praat",
             format = "file"),
  tar_target(exp2_bezier_points,
             command = {
               high_points <- make_bitonal_curve2(0,1,.25,.5,
                                                  70, 120)
               low_points <- make_bitonal_curve2(0,1,.25,.5,
                                                 70, 80)
               
               continuum_points <- make_bezier_continuum2(high_points, 
                                                          low_points, 
                                                          hyp_nsteps, 
                                                          hyp_nbezier)
               
               arrange(continuum_points, step,x) |>
                 write.csv("Scripts/exp2bezierpoints.csv",row.names = FALSE)
               
               continuum_points
             }),
  tar_target(exp2_resynthesize_files,
             command = {
               apply_dt_nudges
               exp2_resynthesis_praatscript
               exp2_bezier_points
               wav_outdir <- 
                 run_resynthesis(
                   wav_fromDir = apply_dt_nudges[['dir']],
                   resynth_script = exp2_resynthesis_praatscript,
                   wav_outDir_pattern = "06b_ResynthesizedRecordingsExp2/ConstantScaling_",
                   nSteps = hyp_nsteps,
                   nBezierPoints = hyp_nbezier
                 )
               non_relative_path <- gsub("^../","", wav_outdir)
               return(list(dir = wav_outdir,
                           md5 = md5sum_dir(non_relative_path,.recursive=FALSE)))
             },
             priority = .3,
             resources =tar_resources(future = 
                                        tar_resources_future(
                                          resources = list(num_cores=4))),
             pattern = map(apply_dt_nudges)),
  tar_target(exp2_make_nuclear_regions,
             command = {
               save_nuclear_region_csv(exp2_resynthesize_files[['dir']])
             },
             pattern = map(exp2_resynthesize_files)),
  tar_target(exp2_resynth_pt_df,
             command = {
               save_raw_pt_dfs(exp2_resynthesize_files[['dir']])
             },
             pattern = map(exp2_resynthesize_files)),
  tar_target(exp2_preprocess_resynthesis,
             command = {
               exp2_resynth_pt_df
               exp2_make_nuclear_regions
               
               save_preprocessed_resynthesis(exp2_resynthesize_files[['dir']],
                                             "exp2",
                                             tune_order,
                                             three_syl_words)
             },
             format = 'file',
             pattern = map(exp2_resynthesize_files)),
  tar_target(exp2_average_resynthesis,
             command = {
               save_avg_resynthesis(exp2_preprocess_resynthesis,
                                    "exp2",
                                    tune_order)
             },
             format = 'file',
             pattern = map(exp2_preprocess_resynthesis)),
  tar_quarto(exp2_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             extra_files = list.files("CalculatedFiles",
                                      pattern = "exp2_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "exp2", 
                                   nucpath = "06b_ResynthesizedRecordingsExp2/ConstantScaling_"),
             deployment = 'worker'),
  ##################################################
  # Bottom-out resyntheses, only constantscaling_100
  tar_target(monotonal_bottomout_praatscript,
             command = "Scripts/resynthesize_monotonal_bottomout.praat"),
  tar_target(bitonal_bottomout_praatscript,
             command = "Scripts/resynthesize_bitonal_bottomout.praat"),
  tar_target(LHS_bottomout_praatscript,
             command = "Scripts/resynthesize_LHS_bottomout.praat"),
  tar_target(bottomout_scaling_factors, c(100, 70)),
  tar_target(bottomout_input_dir, paste0("05_NudgedRecordings/ConstantScaling_", bottomout_scaling_factors)),
  tar_target(monotonal_bottomout_resynthesize_files,
             command = {
               apply_dt_nudges
               wav_outdir <- 
                 run_resynthesis(
                   wav_fromDir = bottomout_input_dir,
                   resynth_script = monotonal_bottomout_praatscript,
                   wav_outDir_pattern = "06d_ResynthesizedRecordingsBottomoutMonotonal/ConstantScaling_",
                   nSteps = hyp_nsteps
                 )
               non_relative_path <- gsub("^../","", wav_outdir)
               return(list(dir = wav_outdir,
                           md5 = md5sum_dir(non_relative_path,.recursive=FALSE)))
             },
             priority = .3,
             pattern = map(bottomout_input_dir)),
  tar_target(monotonal_bottomout_make_nuclear_regions,
             command = {
               save_nuclear_region_csv(monotonal_bottomout_resynthesize_files[['dir']])
             },
             pattern = map(monotonal_bottomout_resynthesize_files)),
  tar_target(monotonal_bottomout_resynth_pt_df,
             command = {
               save_raw_pt_dfs(monotonal_bottomout_resynthesize_files[['dir']])
             },
             pattern = map(monotonal_bottomout_resynthesize_files)),
  tar_target(monotonal_bottomout_preprocess_resynthesis,
             command = {
               monotonal_bottomout_resynth_pt_df
               monotonal_bottomout_make_nuclear_regions
               
               save_preprocessed_resynthesis(monotonal_bottomout_resynthesize_files[['dir']],
                                             "bottomoutMono",
                                             tune_order,
                                             three_syl_words)
             },
             pattern = map(monotonal_bottomout_resynthesize_files),
             format = 'file'),
  tar_target(monotonal_bottomout_average_resynthesis,
             command = {
               save_avg_resynthesis(monotonal_bottomout_preprocess_resynthesis,
                                    "bottomoutMono",
                                    tune_order)
             },
             pattern = map(monotonal_bottomout_preprocess_resynthesis),
             format = 'file'),
  tar_quarto(monotonal_bottomout_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             extra_files = list.files("CalculatedFiles",
                                      pattern = "bottomoutMono_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "bottomoutMono", 
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutMonotonal/ConstantScaling_")),
  tar_target(bitonal_bottomout_resynthesize_files,
             command = {
               apply_dt_nudges
               wav_outdir <- 
                 run_resynthesis(
                   wav_fromDir = bottomout_input_dir,
                   resynth_script = bitonal_bottomout_praatscript,
                   wav_outDir_pattern = "06d_ResynthesizedRecordingsBottomoutBitonal/ConstantScaling_",
                   nSteps = hyp_nsteps,
                   nBezierPoints = hyp_nbezier,
                   nAlignmentSteps = hyp_nalignment
                 )
               non_relative_path <- gsub("^../","", wav_outdir)
               return(list(dir = wav_outdir,
                           md5 = md5sum_dir(non_relative_path,.recursive=FALSE)))
             },
             priority = .3,
             pattern = map(bottomout_input_dir)),
  tar_target(bitonal_bottomout_make_nuclear_regions,
             command = {
               save_nuclear_region_csv(bitonal_bottomout_resynthesize_files[['dir']])
             },
             pattern = map(bitonal_bottomout_resynthesize_files)),
  tar_target(bitonal_bottomout_resynth_pt_df,
             command = {
               save_raw_pt_dfs(bitonal_bottomout_resynthesize_files[['dir']])
             },
             pattern = map(bitonal_bottomout_resynthesize_files)),
  tar_target(bitonal_bottomout_preprocess_resynthesis,
             command = {
               bitonal_bottomout_resynth_pt_df
               bitonal_bottomout_make_nuclear_regions
               
               save_preprocessed_resynthesis(bitonal_bottomout_resynthesize_files[['dir']],
                                             "bottomoutBi",
                                             tune_order,
                                             three_syl_words)
             },
             pattern = map(bitonal_bottomout_resynthesize_files),
             format = 'file'),
  tar_target(bitonal_bottomout_average_resynthesis,
             command = {
               save_avg_resynthesis(bitonal_bottomout_preprocess_resynthesis,
                                    "bottomoutBi",
                                    tune_order)
             },
             pattern = map(bitonal_bottomout_preprocess_resynthesis),
             format = 'file'),
  tar_quarto(bitonal_bottomout_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             extra_files = list.files("CalculatedFiles",
                                      pattern = "bottomoutMono_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "bottomoutBi", 
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutBitonal/ConstantScaling_")),
  tar_target(LHS_bottomout_resynthesize_files,
             command = {
               apply_dt_nudges
               wav_outdir <- 
                 run_resynthesis(
                   wav_fromDir = bottomout_input_dir,
                   resynth_script = LHS_bottomout_praatscript,
                   wav_outDir_pattern = "06d_ResynthesizedRecordingsBottomoutLHS/ConstantScaling_",
                   nSteps = hyp_nsteps
                 )
               non_relative_path <- gsub("^../","", wav_outdir)
               return(list(dir = wav_outdir,
                           md5 = md5sum_dir(non_relative_path,.recursive=FALSE)))
             },
             priority = .3,
             pattern = map(bottomout_input_dir)),
  tar_target(LHS_bottomout_make_nuclear_regions,
             command = {
               save_nuclear_region_csv(LHS_bottomout_resynthesize_files[['dir']])
             },
             pattern = map(LHS_bottomout_resynthesize_files)),
  tar_target(LHS_bottomout_resynth_pt_df,
             command = {
               save_raw_pt_dfs(LHS_bottomout_resynthesize_files[['dir']])
             },
             pattern = map(LHS_bottomout_resynthesize_files)),
  tar_target(LHS_bottomout_preprocess_resynthesis,
             command = {
               LHS_bottomout_resynth_pt_df
               LHS_bottomout_make_nuclear_regions
               
               save_preprocessed_resynthesis(LHS_bottomout_resynthesize_files[['dir']],
                                             "bottomoutLHS",
                                             tune_order,
                                             three_syl_words)
             },
             pattern = map(LHS_bottomout_resynthesize_files),
             format = 'file'),
  tar_target(LHS_bottomout_average_resynthesis,
             command = {
               save_avg_resynthesis(LHS_bottomout_preprocess_resynthesis,
                                    "bottomoutLHS",
                                    tune_order)
             },
             pattern = map(LHS_bottomout_preprocess_resynthesis),
             format = 'file'),
  tar_quarto(LHS_bottomout_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             extra_files = list.files("CalculatedFiles",
                                      pattern = "bottomoutLHS_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "bottomoutLHS", 
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutLHS/ConstantScaling_")),
  tar_target(assemble_resyntheses,
             command = {
               exp1_average_resynthesis
               exp2_average_resynthesis
               exp3_average_resynthesis
               exp3curved_average_resynthesis
               bitonal_bottomout_average_resynthesis
               monotonal_bottomout_average_resynthesis
               LHS_bottomout_average_resynthesis
             }),
  ##########################
  # Figure drawing targets
  tar_target(draw_praatscript,
             command = "Scripts/shellDraw.praat"),
  tar_target(selected_scaling_factor, command = "70"),
  tar_target(directories_to_draw,
             command = 
               c("06d_ResynthesizedRecordingsBottomoutBitonal/ConstantScaling_100",
                 "06d_ResynthesizedRecordingsBottomoutMonotonal/ConstantScaling_100",
                 "06d_ResynthesizedRecordingsBottomoutLHS/ConstantScaling_100",
                 "06d_ResynthesizedRecordingsBottomoutBitonal/ConstantScaling_70",
                 "06d_ResynthesizedRecordingsBottomoutMonotonal/ConstantScaling_70",
                 "06d_ResynthesizedRecordingsBottomoutLHS/ConstantScaling_70",
                 file.path(c("06_ResynthesizedRecordings",
                             "06b_ResynthesizedRecordingsExp2",
                             "06c_ResynthesizedRecordingsExp3",
                             "06c_ResynthesizedRecordingsExp3CURVED"),
                           paste0("ConstantScaling_", selected_scaling_factor)))
  ),
  tar_target(utterance_to_draw,
             command = "branning_01_HLL_002"),
  tar_target(draw_continua_praat_pictures,
             command = {
               assemble_resyntheses
               draw_experiment_recordings(
                 draw_directory = directories_to_draw,
                 utterance = utterance_to_draw,
                 script_path = draw_praatscript,
                 fig_path = "Figures/PraatDrawings/Raw",
                 wavdir_relative = "..",
                 unique_temp = TRUE,
                 remove_temp = TRUE,
                 remove_tier = 3,
                 tgpath_relative = "..",
               )
             },
             pattern = map(directories_to_draw)),
  ##################################
  # Tonal Center of Gravity (TCoG)
  # calculations and plots
  tar_quarto(exp1_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             execute_params = list(exptag = "exp1",
                                   nucpath = "06_ResynthesizedRecordings/ConstantScaling_")),
  tar_quarto(exp2_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             execute_params = list(exptag = "exp2",
                                   nucpath = "06b_ResynthesizedRecordingsExp2/ConstantScaling_")),
  tar_quarto(exp3curved_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             execute_params = list(exptag = "exp3curved",
                                   nucpath = "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_")),
  tar_quarto(exp3straight_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             execute_params = list(exptag = "exp3",
                                   nucpath = "06c_ResynthesizedRecordingsExp3/ConstantScaling_")),
  tar_quarto(bottomout_monotonal_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             execute_params = list(exptag = "bottomoutMono",
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutMonotonal/ConstantScaling_")),
  tar_quarto(bottomout_bitonal_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             execute_params = list(exptag = "bottomoutBi",
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutBitonal/ConstantScaling_")),
  tar_quarto(bottomout_LHS_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             execute_params = list(exptag = "bottomoutLHS",
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutLHS/ConstantScaling_")),
  ############
  # Cull recordings to only specific subset
  tar_target(reduce_resynthesis_input_directories,
             c("06_ResynthesizedRecordings",
               "06b_ResynthesizedRecordingsExp2",
               "06c_ResynthesizedRecordingsExp3",
               "06c_ResynthesizedRecordingsExp3CURVED",
               "06d_ResynthesizedRecordingsBottomoutMonotonal",
               "06d_ResynthesizedRecordingsBottomoutLHS",
               "06d_ResynthesizedRecordingsBottomoutBitonal")),
  tar_target(reduce_utterances_to_use, two_syl_words),
  tar_target(reduce_tune_to_use, "HLL"),
  tar_target(reduce_scaling_factor_to_use, c(100, 70)),
  tar_target(reduce_resynthesis_output_directories,
             c("07_ReducedRecordings/01_Exp1",
               "07_ReducedRecordings/02_Exp2",
               "07_ReducedRecordings/03_Exp3Straight",
               "07_ReducedRecordings/03_Exp3Curved",
               "07_ReducedRecordings/04_BottomoutMono",
               "07_ReducedRecordings/05_BottomoutLHS",
               "07_ReducedRecordings/06_BottomoutBitonal")),
  tar_target(reduce_directories,
             command = {
               input_paths <- 
                 expand.grid(directory = reduce_resynthesis_input_directories,
                             sf = paste0("ConstantScaling_", reduce_scaling_factor_to_use)) |> 
                 transmute(path = file.path(directory, sf)) |> 
                 # filter(!grepl("Bottomout.+70$", path)) |> 
                 pluck('path')
               
               output_paths <- 
                 expand.grid(directory = reduce_resynthesis_output_directories,
                             sf = paste0("ConstantScaling_", reduce_scaling_factor_to_use)) |> 
                 transmute(path = file.path(directory, sf)) |> 
                 # filter(!grepl("Bottomout.+70$", path)) |> 
                 pluck('path')
               
               data.frame(input = input_paths,
                          output = output_paths)
             }),
  tar_target(reduce_mkdir,
             command = {
               dirs_to_check <- c(reduce_resynthesis_output_directories,
                                  reduce_directories$output)
               
               for (d in dirs_to_check)
                 if (!dir.exists(d))
                   dir.create(d)
               
               return(dirs_to_check)
               
             }),
  tar_target(reduce_file_pattern,
             command  = {
               utterances <- paste0("(",
                                    paste(reduce_utterances_to_use,collapse = "|"),
                                    ")")
               paste(utterances,
                     "[0-9]+",
                     reduce_tune_to_use,
                     "[0-9]+",
                     "[0-9]",
                     "[0-9].wav",
                     sep = "_")
             }),
  tar_target(reduce_copy_wavfiles,
             command = {
               reduce_mkdir # Ensure output directories exist first
               assemble_resyntheses # Ensures this is run after resynthesis is done
               
               indir <- reduce_directories$input
               outdir <- reduce_directories$output
               
               in_filenames <- list.files(indir, 
                                          pattern = reduce_file_pattern,
                                          recursive = FALSE,
                                          full.names = FALSE)
               
               # Change eg branning_01_HLL_01_1_1.wav to branning_1_1.wav
               clear_pattern <- paste0("[0-9]+_", reduce_tune_to_use, "_[0-9]+_")
               out_filenames <- gsub(clear_pattern, "", in_filenames)
               
               in_filepaths <- file.path(indir, in_filenames)
               out_filepaths <- file.path(outdir, out_filenames)
               
               file.copy(in_filepaths, out_filepaths, overwrite = TRUE)
               
               list(dir = outdir,
                    md5 = md5sum_dir(outdir))
             },
             pattern = map(reduce_directories)),
  tar_target(reduce_copy_textgrids,
             command = {
               reduce_copy_wavfiles
               
               indir <- file.path(reduce_directories$input,"TextGrids")
               outdir <- file.path(reduce_directories$output,"TextGrids")
               
               if (!dir.exists(outdir))
                 dir.create(outdir)
               
               in_filenames <- list.files(indir, 
                                          pattern = gsub(".wav$",".TextGrid",reduce_file_pattern),
                                          recursive = FALSE,
                                          full.names = FALSE)
               
               # Change eg branning_01_HLL_01_1_1.wav to branning_1_1.wav
               clear_pattern <- paste0("[0-9]+_", reduce_tune_to_use, "_[0-9]+_")
               out_filenames <- gsub(clear_pattern, "", in_filenames)
               
               in_filepaths <- file.path(indir, in_filenames)
               out_filepaths <- file.path(outdir, out_filenames)
               
               file.copy(in_filepaths, out_filepaths, overwrite = TRUE)
               
               md5sum_dir(outdir)
             },
             pattern = map(reduce_directories)),
  ###
  # Silence norming, last time textgrids are being used
  tar_target(chop_silence_praatscript,
             "Scripts/chop_silence.praat",
             format = 'file'),
  tar_target(run_silence_norm,
             command = {
               reduce_copy_textgrids
               message(reduce_copy_wavfiles$dir)
               indir <- reduce_copy_wavfiles$dir
               outdir <- gsub("07_ReducedRecordings",
                              "08_SilenceNormedRecordings",
                              indir)
               topdir <- gsub("/ConstantScaling_.+$", "", outdir)
               tg_outdir <- file.path(outdir, "TextGrids")
               
               if (!dir.exists(topdir))
                 dir.create(topdir)
               
               if (!dir.exists(outdir))
                 dir.create(outdir)
               
               if (!dir.exists(tg_outdir))
                 dir.create(tg_outdir)
               
               rel_indir <- file.path("..", indir)
               rel_outdir <- file.path("..", outdir)
               run_praatscript(chop_silence_praatscript,
                               fromDir = rel_indir,
                               outDir = rel_outdir,
                               tgFromDir = file.path(rel_indir, "TextGrids"),
                               tgToDir = file.path(rel_outdir, "TextGrids"),
                               leftSilence = 0.03,
                               rightSilence = 0.03,
                               tierNum = 1,
                               praat_path = "Praat.exe",
                               method = base::shell)
               list(dir = outdir,
                    md5 = md5sum_dir(outdir))
             },
             pattern = map(reduce_copy_wavfiles)),
  tar_target(rms_norm_script,
             "Scripts/shell_rms_norm.praat",
             format = "file"),
  tar_target(run_rms_norm,
             command = {
               indir <- run_silence_norm$dir
               outdir <- gsub("08_SilenceNormedRecordings",
                              "09_RMSNormedRecordings",
                              indir)
               topdir <- gsub("ConstantScaling_.+$", "", outdir)
               
               if (!dir.exists(topdir))
                 dir.create(topdir)
               
               if (!dir.exists(outdir))
                 dir.create(outdir)
               
               rel_indir <- file.path("..", indir)
               rel_outdir <- file.path("..", outdir)
               run_praatscript(rms_norm_script,
                               fromDir = rel_indir,
                               outDir = rel_outdir,
                               rmsVal = 70,
                               forceClip = 0,
                               praat_path = "Praat.exe",
                               method = base::shell)
               list(dir = outdir,
                    md5 = md5sum_dir(outdir))
             },
             pattern = map(run_silence_norm)),
  tar_target(convert_to_mp3,
             command = {
               indir <- run_rms_norm$dir
               outdir <- gsub("09_RMSNormedRecordings","10_FinalFiles", indir)
               base::shell(glue::glue("bash Scripts/convert_dir_to_mp3.sh {indir} {outdir}"),
                           wait = TRUE, mustWork = TRUE)
               
               list(dir = outdir,
                    md5 = md5sum_dir(outdir))
             },
             pattern = map(run_rms_norm))
)  

