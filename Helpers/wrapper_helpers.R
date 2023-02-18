################################################################################
# Originally all of these functions were written out as commands in individual
# targets, but I consolidated them here to reduce clutter and refactor
# some targets that could be combined.
#
# Some general notes:
#  - Anywhere md5sum_dir() is used is to return an md5 hash for each file in a
#    a directory. I do this because sometimes a recording will change (e.g.,
#    the resynthesis script might change slightly) and I need to ensure that
#    downstream targets rerun, but this wasn't working as intended when just
#    tracking the directory for changes. Typically md5sum_dir() is used as part
#    of a list that a target returns along with the directory; the directory
#    is typically used as input to another target.
#
################################################################################


#' Average pitch contours
#' 
#' Average the ptich contours from the recordings, all arguments are the
#' target names needed to run the calculations
#'
#' @param textgrid_df 
#' @param files_to_omit 
#' @param nuclear_words 
#' @param process_pitchtiers 
#' @param tune_order 
#' @param three_syl_words 
#' @param nuclear_regions 
#' 
run_average_contours <- function(textgrid_df, 
                                 files_to_omit,
                                 nuclear_words,
                                 process_pitchtiers,
                                 tune_order,
                                 three_syl_words,
                                 nuclear_regions) {
  # Load files
  tg_df <-
    textgrid_df |>
    filter(!file %in% files_to_omit$file)
  
  # Extract nuclear regions
  nuclear_regions <- get_nuclear_textgrids(tg_df, nuclear_words)
  
  # Preprocess pitchtracks
  pitchtier_df <-
    process_pitchtiers  |>
    preprocess_pitchtracks(
      nuclear_regions,
      runmed_k = 5,
      time_normalize = TRUE,
      .fromzero = TRUE
    ) |>
    mutate(
      tune = factor(tune, levels = tune_order),
      wordset = ifelse(utterance %in% three_syl_words, "syl3", "syl2")
    )
  
  # Calculate average pitch contours for each tune-utterance pair
  avg_contour_df <-
    pitchtier_df |>
    # Fix coding issue with points outside of final boundary
    mutate(is_nuclear = fix_OOB_nuclear_coding(is_nuclear),
           tune = factor(tune, levels = tune_order)) |>
    average_pitchtracks(
      section_by = "is_nuclear",
      pulses_per_section = c("FALSE" = 15,
                             "TRUE" = 30),
      time_by = "timepoint_norm",
      aggregate_by = file ~ utterance + tune + session,
      parallelize = TRUE
    ) |>
    # Smooth average points a little
    running_median_smooth(
      .k = 5,
      .from = "avg_hz",
      .to = "avg_hz_smooth",
      .group = c("utterance", "tune")
    ) |>
    mutate(wordset = ifelse(utterance %in% three_syl_words, "syl3", "syl2"))
}

#' Move recordings
#' 
#' Moves recordings from the possible directory to the chosen directory
#'
#' @param chosen_filepaths 
#'
run_move_chosen_recordings <- function(chosen_filepaths) {
  move_from <- chosen_filepaths
  move_to <- gsub("02_PossibleRecordings",
                  "03_ChosenRecordings",
                  move_from,
                  perl = TRUE)
  message("Clearing outdated chosen recordings")
  files_to_remove <- list.files("03_ChosenRecordings",
                                recursive = TRUE,
                                full.names = TRUE)
  # Don't delete the manually fixed files!!!!
  files_to_remove <-
    files_to_remove[!grepl("FixedTextGrids", files_to_remove)]
  file.remove(files_to_remove)
  file.copy(from = move_from, to = move_to)
  return(move_to)
}


#' Write textgrids
#'
#' @param move_chosen_recordings 
#' @param chosen_files 
#' @param syllable_specification 
#' @param syllable_durations 
#'
#' @return
#' @export
#'
#' @examples
run_write_syllable_tgs_and_dts <- function(move_chosen_recordings,
                                           chosen_files,
                                           syllable_specification,
                                           syllable_durations){
  textgrid_dir <- process_syllable_tiers(chosen_files,
                                         syllable_specification,
                                         syllable_durations)
  md5sum_dir(textgrid_dir)
}

#' Apply duration tier manipulations
#'
#' @param write_syllable_tgs_and_dts 
#'
#' @return
#' @export
#'
#' @examples
run_apply_dt_manipulation <- function(move_chosen_recordings, write_syllable_tgs_and_dts) {
  file.remove(list.files("04_ScaledRecordings", full.names = TRUE))
  run_praatscript("Scripts/apply_dt_manipulation.praat",
                  use_defaults = TRUE,
                  method = base::shell,
                  praat_path = "Praat.exe")
  md5sum_dir('04_ScaledRecordings/')
}

#' Write constant scale files
#'
#' @param apply_dt_manipulation 
#' @param scaling_factors_to_try 
#'
#' @return
#' @export
#'
#' @examples
run_write_constant_scale_files <- function(apply_dt_manipulation,
                                           scaling_factors_to_try) {
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
}

#' Nudge recordings
#' 
#' Compresses the duration of the second syllable
#'
#' @param write_constant_scale_files 
#'
#' @return
#' 
run_apply_dt_nudges <- function(write_constant_scale_files) {
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
}

#' Run a resynthesis script
#' 
#' Given a praatscript and an input/output directory, run the resynthesis
#' in praat.
#'
#' @param apply_dt_nudges 
#' @param tar_resynth_script 
#' @param hyp_nsteps 
#' @param input_dir Directory for input files
#' @param outdir_pattern Directory pattern (subdirectories=constant scaling dirs)
#' @param ... Additional parameters that only some of the scripts use
#'
#' @return Dir/md5 checksum list
run_resynth_target <- function(apply_dt_nudges,
                               tar_resynth_script,
                               hyp_nsteps,
                               input_dir,
                               outdir_pattern,
                               ...) {
  wav_outdir <- 
    run_resynthesis(
      wav_fromDir = input_dir,
      resynth_script = tar_resynth_script,
      wav_outDir_pattern = outdir_pattern,
      nSteps = hyp_nsteps
    )
  non_relative_path <- gsub("^../","", wav_outdir)
  return(list(dir = wav_outdir,
              md5 = md5sum_dir(non_relative_path,.recursive=FALSE)))
}


#' Preprocess and average resynthesis
#' 
#' This combines two previous targets that added a lot of dynamic branching.
#' The first step preprocesses all the resynthesis files, the second
#' calculates averages over pitch contours and then saves the calculated files
#'
#' @param tar_resynth_pt_df 
#' @param tar_nuclear_regions 
#' @param tar_resynth_files 
#' @param tune_order 
#' @param three_syl_words 
#' @param exptag 
#'
#' @return Filepath of calculated average file
run_preprocess_avg_resynthesis <- function(tar_resynth_pt_df,
                                           tar_nuclear_regions,
                                           tar_resynth_files,
                                           tune_order,
                                           three_syl_words,
                                           exptag)  {
  preprocessed_resynth <- save_preprocessed_resynthesis(tar_resynth_files[['dir']],
                                                        exptag,
                                                        tune_order,
                                                        three_syl_words)
  
  save_avg_resynthesis(preprocessed_resynth,
                       exptag,
                       tune_order)
}

#' Cull which files to use
#'
#' Originally I had ConstantScaling write files from 60 to 100% in steps
#' of 5%, so there were more files to cull after deciding on the final
#' duration manipulation to use. So, this function reduces 
#'
#' @param reduce_resynthesis_input_directories 
#' @param reduce_scaling_factor_to_use 
#' @param reduce_resynthesis_output_directories 
#'
#' @return
#' @export
#'
#' @examples
run_reduce_directories <- function(reduce_resynthesis_input_directories,
                                   reduce_scaling_factor_to_use,
                                   reduce_resynthesis_output_directories) {
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
}

run_reduce_copy_wavfiles <- function(reduce_mkdir,
                                     assemble_resyntheses,
                                     reduce_directories,
                                     reduce_file_pattern,
                                     reduce_tune_to_use) {
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
}

run_reduce_copy_textgrids <- function(reduce_copy_wavfiles,
                                      reduce_directories,
                                      reduce_file_pattern,
                                      reduce_tune_to_use)  {
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
}

run_silence_norm_target <- function(reduce_copy_textgrids,
                                    reduce_copy_wavfiles,
                                    chop_silence_praatscript) {
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
}

run_rms_norm_target <- function(run_silence_norm,
                                rms_norm_script) {
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
}

run_convert_to_mp3 <- function(run_rms_norm) {
  indir <- run_rms_norm$dir
  outdir <- gsub("09_RMSNormedRecordings","10_FinalFiles", indir)
  base::shell(glue::glue("bash Scripts/convert_dir_to_mp3.sh {indir} {outdir}"),
              wait = TRUE, mustWork = TRUE)
  
  list(dir = outdir,
       md5 = md5sum_dir(outdir))
}