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
  packages = c("tidyverse","rPraat","furrr", "sosprosody","bezier","here"), # packages that your targets need to run
  format = "rds", # default storage format
  # Set other options as needed.
  workspace_on_error = TRUE
)

# Flags for execution
RERUN_MFA = FALSE
N_CORES = 1
N_WORKERS = 10

tar_source("Helpers")

# Change the number of workers here as needed
# this is running on a 12-core machine with 24 threads, if you don't set the
# number of workers then plan() will reserve the maximum number it can take
future::plan(future::multisession(workers = N_WORKERS))

list(
  ## Track directories for modifications
  ## This will be outdated any time a file in the directory is added or removed
  # tar_target(dir_01,
  #            command = "01_Mono/",
  #            format = 'file'),
  
  ## Track scripts for changes
  # tar_target(extract_takes_script,
  #            command = "Scripts/extract_takes.praat",
  #            format = "file"),
  tar_target(gen_pitchtier_script,
             command = "Scripts/gen_manips_pitchtiers.praat",
             format = "file"),
  tar_target(force_align_script,
             command = "Scripts/force_align.sh",
             format = "file"),
  
  # If the mono recordings folder has been changed, the numbering of the
  # extracted takes is no longer valid/reliable, so all the files must be
  # removed and replaced to ensure that the files are as expected
  # tar_target(clear_old_recordings,
  #            command = {
  #              dir_01
  #              # Add files in additional directories as needed
  #              files_to_remove <-
  #                list.files("02_PossibleRecordings/",
  #                           recursive = TRUE,
  #                           full.names = TRUE)
  #              file.remove(files_to_remove)
  #            }),
  
  ## Execute scripts
  # tar_target(
  #   name = extract_takes,
  #   command = {
  #     # Extract takes whenever there have been changes in the mono directory
  #     clear_old_recordings
  #     run_praatscript(script_path = extract_takes_script,
  #                     fromDir = "../01_Mono",
  #                     outDir = "../02_PossibleRecordings")
  #   }
  # ),
  tar_target(
    name = generate_pitchtiers,
    command = {
      # Generate the pitchtiers
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
      
      if (!RERUN_MFA)
        return(0)
      
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
               process_textgrids |> 
               mutate(is_nuclear = word_label %in% nuclear_words,
                      wordset = ifelse(utterance %in% three_syl_words, "syl3", "syl2"))),
  tar_target(files_to_omit, command = check_pronunciations(textgrid_df)),
  ## Analysis files to track
  tar_target(average_contours,
             command = {
               run_average_contours(textgrid_df, 
                                    files_to_omit,
                                    nuclear_words,
                                    process_pitchtiers,
                                    tune_order,
                                    three_syl_words,
                                    nuclear_regions)
             }),
  tar_quarto(render_EDA, 
             path = "Writeups/raw_recordings_eda.qmd",
             deployment = 'main'),
  tar_quarto(render_flowchart,
             path = "Writeups/flowchart.qmd",
             deployment = 'main'),
  # Uncomment these to try to rerun the spectral measure extraction with
  # praatsauce, but this has a tendency to hang and not execute completely.
  # I usually end up just rerunning this one manually.
  # tar_target(get_all_spectral_measures,
  #            command = {
  #              get_dir_spectral_measures(
  #                wavfile_from = "02_PossibleRecordings",
  #                tg_from = "02_PossibleRecordings/MFA_textgrids",
  #                tg_to = "02_PossibleRecordings/SpectralMeasuresTextgrids",
  #                rewrite_tg = TRUE,
  #                use_existing = FALSE)
  #              
  #            }),
  # tar_quarto(render_spectral_measures,
  #            path = "Writeups/spectral_measures_analysis.qmd",
  #            extra_files = "02_PossibleRecordings/SpectralMeasuresTextgrids/spectral_measures.txt",
  #            deployment = 'main'),
  tar_quarto(render_midphon_figures,
             path = "Writeups/midphon_figures.qmd",
             extra_files = "FullSpectralMeasures/spectral_measures.txt",
             deployment = 'main'),
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
             command = run_move_chosen_recordings(chosen_filepaths),
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
  tar_target(syllable_durations,
             command =
               get_syllable_durations(avg_phone_durations, 
                                      syllable_specification)),
  tar_target(write_syllable_tgs_and_dts,
             command =
               run_write_syllable_tgs_and_dts(move_chosen_recordings,
                                              chosen_files,
                                              syllable_specification,
                                              syllable_durations)),
  tar_target(apply_dt_manipulation,
             command = 
               run_apply_dt_manipulation(move_chosen_recordings,
                                         write_syllable_tgs_and_dts)),
  tar_target(scaling_factors_to_try,
             command = c(1.00,  .70)),
  tar_target(write_constant_scale_files,
             command = 
               run_write_constant_scale_files(apply_dt_manipulation,
                                              scaling_factors_to_try),
             pattern = map(scaling_factors_to_try)),
  tar_target(apply_dt_nudges,
             command = 
               run_apply_dt_nudges(write_constant_scale_files),
             priority =.99,
             pattern = map(write_constant_scale_files)),
  
  ##############################
  # Resynthesis hyperparameters
  tar_target(hyp_nsteps, 5),     # Set # of PA and BT scaling steps
  tar_target(hyp_nalignment, 5), # Set # of alignment steps
  tar_target(hyp_nbezier, 13),   # Set quantization of bezier curves
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
               run_resynth_target(apply_dt_nudges,
                                  tar_resynth_script = resynthesis_praatscript,
                                  hyp_nsteps = hyp_nsteps,
                                  input_dir = apply_dt_nudges[['dir']],
                                  outdir_pattern = "06_ResynthesizedRecordings/ConstantScaling_")
             },
             priority = .90,
             resources =tar_resources(future = 
                                        tar_resources_future(
                                          resources = list(num_cores=N_CORES))),
             pattern = map(apply_dt_nudges)),
  tar_target(make_nuclear_regions,
             command = {
               save_nuclear_regions_praatscript
               save_nuclear_region_csv(resynthesize_files[['dir']])
             },
             pattern = map(resynthesize_files)),
  tar_target(resynth_pt_df,
             command = 
               save_raw_pt_dfs(resynthesize_files[['dir']]),
             pattern = map(resynthesize_files)),
  tar_target(exp1_preprocess_and_average_resynthesis,
             command = 
               run_preprocess_avg_resynthesis(resynth_pt_df,
                                              make_nuclear_regions,
                                              resynthesize_files,
                                              tune_order,
                                              three_syl_words,
                                              exptag = "exp1"),
             format = 'file',
             pattern = map(resynthesize_files)
             
  ),
  tar_quarto(render_resynthesis_analysis,
             pandoc_args = "-o exp1_avg_resynth.html",
             path = "Writeups/resynthesis_analysis.qmd",
             deployment = 'main',
             extra_files = list.files("CalculatedFiles",
                                      pattern = "exp1_avg_resynth",
                                      full.names = TRUE)),
  ###########################
  # Experiment 3 (alignment manip) targets
  tar_target(exp3_resynthesis_praatscript,
             command = "Scripts/resynthesize_exp3_straight.praat",
             format = "file"),
  tar_target(exp3_resynthesize_files,
             command = 
               run_resynth_target(apply_dt_nudges,
                                  tar_resynth_script = exp3_resynthesis_praatscript,
                                  hyp_nsteps = hyp_nsteps,
                                  input_dir = apply_dt_nudges[['dir']],
                                  outdir_pattern = "06c_ResynthesizedRecordingsExp3/ConstantScaling_"),
             priority = .3,
             resources =tar_resources(future = 
                                        tar_resources_future(
                                          resources = list(num_cores=N_CORES))), 
             pattern = map(apply_dt_nudges)),
  tar_target(exp3_make_nuclear_regions,
             command = 
               save_nuclear_region_csv(exp3_resynthesize_files[['dir']]),
             pattern = map(exp3_resynthesize_files)),
  tar_target(exp3_resynth_pt_df,
             command =
               save_raw_pt_dfs(exp3_resynthesize_files[['dir']]),
             pattern = map(exp3_resynthesize_files)),
  tar_target(exp3_preprocess_and_average_resynthesis,
             command = 
               run_preprocess_avg_resynthesis(tar_resynth_pt_df = exp3_resynth_pt_df,
                                              tar_nuclear_regions = exp3_make_nuclear_regions,
                                              tar_resynth_files = exp3_resynthesize_files,
                                              tune_order,
                                              three_syl_words,
                                              exptag = "exp3"),
             format = 'file',
             pattern = map(exp3_resynthesize_files)),
  tar_quarto(exp3_render_resynthesis_analysis,
             pandoc_args = "-o exp3_avg_resynth.html",
             path = "Writeups/resynthesis_analysis.qmd",
             deployment = 'main',
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
               exp3_bezier_points
               run_resynth_target(apply_dt_nudges,
                                  tar_resynth_script = exp3curved_resynthesis_praatscript,
                                  hyp_nsteps = hyp_nsteps,
                                  input_dir = apply_dt_nudges[['dir']],
                                  outdir_pattern = "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_",
                                  nAlignmentSteps = hyp_nalignment,
                                  nBezierPoints = hyp_nbezier)
             },
             priority = .3,
             resources =tar_resources(future = 
                                        tar_resources_future(
                                          resources = list(num_cores=N_CORES))),
             pattern = map(apply_dt_nudges)),
  tar_target(exp3curved_make_nuclear_regions,
             command = 
               save_nuclear_region_csv(exp3curved_resynthesize_files[['dir']]),
             pattern = map(exp3curved_resynthesize_files)),
  tar_target(exp3curved_resynth_pt_df,
             command = 
               save_raw_pt_dfs(exp3curved_resynthesize_files[['dir']]),
             pattern = map(exp3curved_resynthesize_files)),
  tar_target(exp3curved_preprocess_and_average_resynthesis,
             command = 
               run_preprocess_avg_resynthesis(exp3curved_resynth_pt_df,
                                              exp3curved_make_nuclear_regions,
                                              exp3curved_resynthesize_files,
                                              tune_order,
                                              three_syl_words,
                                              exptag = "exp3curved"),
             format = 'file',
             pattern = map(exp3curved_resynthesize_files)),
  tar_quarto(exp3curved_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             pandoc_args = "-o exp3curved_avg_resynth.html",
             deployment = 'main',
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
               run_resynth_target(apply_dt_nudges,
                                  tar_resynth_script = exp2_resynthesis_praatscript,
                                  hyp_nsteps = hyp_nsteps,
                                  input_dir = apply_dt_nudges[['dir']],
                                  outdir_pattern = "06b_ResynthesizedRecordingsExp2/ConstantScaling_",
                                  nBezierPoints = hyp_nbezier)
             },
             priority = .3,
             resources =tar_resources(future = 
                                        tar_resources_future(
                                          resources = list(num_cores=N_CORES))),
             pattern = map(apply_dt_nudges)),
  tar_target(exp2_make_nuclear_regions,
             command = 
               save_nuclear_region_csv(exp2_resynthesize_files[['dir']]),
             pattern = map(exp2_resynthesize_files)),
  tar_target(exp2_resynth_pt_df,
             command = 
               save_raw_pt_dfs(exp2_resynthesize_files[['dir']]),
             pattern = map(exp2_resynthesize_files)),
  tar_target(exp2_preprocess_and_average_resynthesis,
             command = 
               run_preprocess_avg_resynthesis(exp2_resynth_pt_df,
                                              exp2_make_nuclear_regions,
                                              exp2_resynthesize_files,
                                              tune_order,
                                              three_syl_words,
                                              exptag = "exp2"),
             format = 'file',
             pattern = map(exp2_resynthesize_files)),
  tar_quarto(exp2_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             pandoc_args = "-o exp2_avg_resynth.html",
             extra_files = list.files("CalculatedFiles",
                                      pattern = "exp2_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "exp2", 
                                   nucpath = "06b_ResynthesizedRecordingsExp2/ConstantScaling_"),
             deployment = 'main'),
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
             command = 
               run_resynth_target(apply_dt_nudges,
                                  tar_resynth_script = monotonal_bottomout_praatscript,
                                  hyp_nsteps = hyp_nsteps,
                                  input_dir = bottomout_input_dir,
                                  outdir_pattern = "06d_ResynthesizedRecordingsBottomoutMonotonal/ConstantScaling_"),
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
             command = 
               run_preprocess_avg_resynthesis(monotonal_bottomout_resynth_pt_df,
                                              monotonal_bottomout_make_nuclear_regions,
                                              monotonal_bottomout_resynthesize_files,
                                              tune_order,
                                              three_syl_words,
                                              exptag = "bottomoutMono"),
             pattern = map(monotonal_bottomout_resynthesize_files),
             format = 'file'), 
  tar_quarto(monotonal_bottomout_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             pandoc_args = "-o bottomoutMono_avg_resynth.html",
             deployment = 'main',
             extra_files = list.files("CalculatedFiles",
                                      pattern = "bottomoutMono_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "bottomoutMono", 
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutMonotonal/ConstantScaling_")),
  tar_target(bitonal_bottomout_resynthesize_files,
             command = 
               run_resynth_target(apply_dt_nudges,
                                  tar_resynth_script = bitonal_bottomout_praatscript,
                                  hyp_nsteps = hyp_nsteps,
                                  input_dir = bottomout_input_dir,
                                  outdir_pattern = "06d_ResynthesizedRecordingsBottomoutBitonal/ConstantScaling_",
                                  nBezierPoints = hyp_nbezier,
                                  nAlignmentSteps = hyp_nalignment),
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
             command = 
               run_preprocess_avg_resynthesis(bitonal_bottomout_resynth_pt_df,
                                              bitonal_bottomout_make_nuclear_regions,
                                              bitonal_bottomout_resynthesize_files,
                                              tune_order,
                                              three_syl_words,
                                              exptag = "bottomoutBi"),
             pattern = map(bitonal_bottomout_resynthesize_files),
             format = 'file'),
  tar_quarto(bitonal_bottomout_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             pandoc_args = "-o bottomoutBi_avg_resynth.html",
             deployment = 'main',
             extra_files = list.files("CalculatedFiles",
                                      pattern = "bottomoutBi_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "bottomoutBi", 
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutBitonal/ConstantScaling_")),
  tar_target(LHS_bottomout_resynthesize_files,
             command = 
               
               run_resynth_target(apply_dt_nudges,
                                  tar_resynth_script = LHS_bottomout_praatscript,
                                  hyp_nsteps = hyp_nsteps,
                                  input_dir = bottomout_input_dir,
                                  outdir_pattern = "06d_ResynthesizedRecordingsBottomoutLHS/ConstantScaling_"),
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
             command = 
               run_preprocess_avg_resynthesis(LHS_bottomout_resynth_pt_df,
                                              LHS_bottomout_make_nuclear_regions,
                                              LHS_bottomout_resynthesize_files,
                                              tune_order,
                                              three_syl_words,
                                              exptag = "bottomoutLHS"),
             
             pattern = map(LHS_bottomout_resynthesize_files),
             format = 'file'),
  tar_quarto(LHS_bottomout_render_resynthesis_analysis,
             path = "Writeups/resynthesis_analysis.qmd",
             pandoc_args = "-o bottomoutLHS_avg_resynth.html",
             deployment = 'main',
             extra_files = list.files("CalculatedFiles",
                                      pattern = "bottomoutLHS_avg_resynth",
                                      full.names = TRUE),
             execute_params = list(exptag = "bottomoutLHS", 
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutLHS/ConstantScaling_")),
  tar_target(assemble_resyntheses,
             command = {
               exp1_preprocess_and_average_resynthesis
               exp2_preprocess_and_average_resynthesis
               exp3_preprocess_and_average_resynthesis
               exp3curved_preprocess_and_average_resynthesis
               bitonal_bottomout_preprocess_resynthesis
               monotonal_bottomout_preprocess_resynthesis
               LHS_bottomout_preprocess_resynthesis
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
  # In theory tar_quarto_rep should work but for some reason it just does not
  # want to. very annoying.
  # tar_quarto_rep(render_tcog_report,
  #                path =
  #                  "Writeups/tcog_figures.qmd",
  #                execute_params =
  #                  data.frame(exptag = c(
  #                    'exp1',
  #                    'exp2',
  #                    'exp3curved',
  #                    'exp3',
  #                    'bottomoutMono',
  #                    'bottomoutBi',
  #                    'bottomoutLHS'),
  #                    nucpath = c(
  #                      "06_ResynthesizedRecordings/ConstantScaling_",
  #                      "06b_ResynthesizedRecordingsExp2/ConstantScaling_",
  #                      "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_",
  #                      "06c_ResynthesizedRecordingsExp3/ConstantScaling_",
  #                      "06d_ResynthesizedRecordingsBottomoutMonotonal/ConstantScaling_",
  #                      "06d_ResynthesizedRecordingsBottomoutBitonal/ConstantScaling_",
  #                      "06d_ResynthesizedRecordingsBottomoutLHS/ConstantScaling_"
  #                    ),
  #                    output_file = c(
  #                      'Writeups/exp1_tcog.html',
  #                      'Writeups/exp2_tcog.html',
  #                      'Writeups/exp3curved_tcog.html',
  #                      'Writeups/exp3_tcog.html',
  #                      'Writeups/bottomoutMono_tcog.html',
  #                      'Writeups/bottomoutBi_tcog.html',
  #                      'Writeups/bottomoutLHS_tcog.html'
  #                    ))),
  tar_quarto(exp1_render_tcog,
             pandoc_args = "-o exp1_tcog.html",
             path = "Writeups/tcog_figures.qmd",
             deployment = 'main',
             execute_params = list(exptag = "exp1",
                                   nucpath = "06_ResynthesizedRecordings/ConstantScaling_")),
  tar_quarto(exp2_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             pandoc_args = "-o exp2_tcog.html",
             deployment = 'main',
             execute_params = list(exptag = "exp2",
                                   nucpath = "06b_ResynthesizedRecordingsExp2/ConstantScaling_")),
  tar_quarto(exp3curved_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             deployment = 'main',
             pandoc_args = "-o exp3curved_tcog.html",
             execute_params = list(exptag = "exp3curved",
                                   nucpath = "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_")),
  tar_quarto(exp3straight_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             pandoc_args = "-o exp3_tcog.html",
             deployment = 'main',
             execute_params = list(exptag = "exp3",
                                   nucpath = "06c_ResynthesizedRecordingsExp3/ConstantScaling_")),
  tar_quarto(bottomout_monotonal_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             deployment = 'main',
             pandoc_args = "-o bottomoutMono_tcog.html",
             execute_params = list(exptag = "bottomoutMono",
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutMonotonal/ConstantScaling_")),
  tar_quarto(bottomout_bitonal_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             deployment = 'main',
             pandoc_args = "-o bottomoutBi_tcog.html",
             execute_params = list(exptag = "bottomoutBi",
                                   nucpath = "06d_ResynthesizedRecordingsBottomoutBitonal/ConstantScaling_")),
  tar_quarto(bottomout_LHS_render_tcog,
             path = "Writeups/tcog_figures.qmd",
             deployment = 'main',
             pandoc_args = "-o bottomoutLHS_tcog.html",
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
             command = 
               run_reduce_directories(reduce_resynthesis_input_directories,
                                      reduce_scaling_factor_to_use,
                                      reduce_resynthesis_output_directories)),
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
             command = 
               run_reduce_copy_wavfiles(reduce_mkdir,
                                        assemble_resyntheses,
                                        reduce_directories,
                                        reduce_file_pattern,
                                        reduce_tune_to_use),
             pattern = map(reduce_directories)),
  tar_target(reduce_copy_textgrids,
             command = 
               run_reduce_copy_textgrids(reduce_copy_wavfiles,
                                         reduce_directories,
                                         reduce_file_pattern,
                                         reduce_tune_to_use),
             pattern = map(reduce_directories)),
  ###
  # Silence norming, last time textgrids are being used
  tar_target(chop_silence_praatscript,
             "Scripts/chop_silence.praat",
             format = 'file'),
  tar_target(run_silence_norm,
             command =
               run_silence_norm_target(reduce_copy_textgrids,
                                       reduce_copy_wavfiles,
                                       chop_silence_praatscript),
             pattern = map(reduce_copy_wavfiles)),
  tar_target(rms_norm_script,
             "Scripts/shell_rms_norm.praat",
             format = "file"),
  tar_target(run_rms_norm,
             command = 
               run_rms_norm_target(run_silence_norm,
                                   rms_norm_script),
             pattern = map(run_silence_norm)),
  tar_target(convert_to_mp3,
             command =
               run_convert_to_mp3(run_rms_norm),
             pattern = map(run_rms_norm))
)  

