#' Run resynthesis script
#' 
#' Runs a script using the given directory containing wav files.
#' Resynthesized files are saved in the directory with the given directory.
#' This function will create associated subdirectories for textgrids and
#' pitchtiers. This function is intended to be used in a targets workflow
#' where the input directory consists of files that have already been scaled
#' to a specific duration. The scaling factor is extracted from that and appended
#' to `wav_outDir_pattern` for the output directory.
#' 
#' The dots may be used to pass additional inputs to the praat script other
#' than the following (basically non-directory arguments):
#' 
#'  - fromDir
#'  - tgFromDir
#'  - outDir
#'  - ptDir
#'  - tgToDir
#'
#' @param wav_fromDir Directory, from `05_NudgedRecordings/ConstantScaling_X`
#' @param resynth_script Resynthesis script to run
#' @param wav_outDir_pattern Where to save output files, as the subdirectory pattern
#' within another directory: `06_ResynthesizedRecordings/ConstantScaling_`
#' @param ... Other named arguments to pass to the praat script. 
#'
#' @return The output directory that files were saved to
run_resynthesis <- function(wav_fromDir,
                            resynth_script = "Scripts/resynthesize_continua.praat",
                            wav_outDir_pattern = "06_ResynthesizedRecordings/ConstantScaling_",
                            ...) {
  # wav_fromDir = apply_dt_nudges in pipeline
  
  # Extract scaling factor
  sf <- str_match(wav_fromDir, "_([0-9]+)$")[2]
  
  # Establish subdirectories
  tg_fromDir <- paste0(wav_fromDir, "/TextGrids")
  wav_outDir <- paste0(wav_outDir_pattern, sf)
  tg_outDir <- paste0(wav_outDir, "/TextGrids")
  pt_outDir <- paste0(wav_outDir, "/PitchTiers")
  
  message(glue::glue("Resynthesizing from {wav_fromDir}\n into {wav_outDir}"))
  
  # If we need to make new directories, do so
  for (filedir in c(wav_outDir, tg_outDir, pt_outDir)){
    if (!dir.exists(filedir)) {
      message("Creating ", filedir)
    }
      dir.create(filedir)
  }
  
  # If this is running anew, we need to clear out all old files
  file.remove(list.files(wav_outDir,
                         recursive = TRUE,
                         full.names = TRUE))
  
  # Make the directories relative to the script
  wav_fromDir <- paste0("../", wav_fromDir)
  tg_fromDir <- paste0("../", tg_fromDir)
  wav_outDir <- paste0("../", wav_outDir)
  tg_outDir <- paste0("../", tg_outDir)
  pt_outDir <- paste0("../", pt_outDir)
  
  # Run the praat script
  message(run_praatscript(resynth_script,
                          fromDir = wav_fromDir,
                          tgFromDir = tg_fromDir,
                          outDir = wav_outDir,
                          ptDir = pt_outDir,
                          tgToDir = tg_outDir,
                          ...,
                          use_defaults = TRUE,
                          debug = TRUE,
                          method = base::shell, 
                          praat_path = "Praat.exe"))
  output <- run_praatscript(resynth_script,
                            fromDir = wav_fromDir,
                            tgFromDir = tg_fromDir,
                            outDir = wav_outDir,
                            ptDir = pt_outDir,
                            tgToDir = tg_outDir,
                            ...,
                            use_defaults = TRUE,
                            debug = FALSE,
                            method = base::shell, 
                            praat_path = "Praat.exe")
 
  # Load the files 
  return(wav_outDir)
}

#' Save nuclear regions
#' 
#' Given a directory that contains a textgrid subdirectory, extract the
#' nuclear regions from each textgrid using the `save_nuclear_regions.praat`
#' script.
#'
#' @param resynth_directory Directory containing a TextGrids subdirectory 
#'
#' @return File path to the output csv file
save_nuclear_region_csv <- function(resynth_directory) {
  message(resynth_directory)
  
  sf <- str_match(resynth_directory, "_([0-9]+)$")[2]
  file_dir <- paste0(resynth_directory, "/TextGrids")
  outfile <- "nuclear_regions.csv"
  run_praatscript("Scripts/save_nuclear_regions.praat",
                  tgDir = file_dir,
                  outputFile = outfile,
                  use_defaults = TRUE,
                  praat_path = "Praat.exe",
                  method = base::shell)
  return(file.path(file_dir, outfile))
}


#' Extract raw pitch tiers
#' 
#' Extract the information from a directory of pitch tier files and save
#' a dataframe with all files' information together.
#'
#' @param resynth_directory Directory containing wav files
#'
#' @return File path to a saved RDS file for the dataframe
save_raw_pt_dfs <- function(resynth_directory) {
  # Resynth directory = where resynthesized files are located,
  # pulls from target resynthesize_files; relativized to script directory
  
  pathstring <- gsub("^../","", resynth_directory) # Remove relativization
  message("Making pitchtier dataframes for ", pathstring)
  
  sf <- str_match(pathstring, "_([0-9]+)$")[2]
  file_dir <- paste0(pathstring, "/PitchTiers/")
  pitchtier_df <- 
    batch_process_pitchtiers(file_dir,
                             parallelize = TRUE) |>
    tidyr::separate(col = "file",
                    sep = "_",
                    into = c("utterance","session", "tune", "take", "pa_val", "bt_val"),
                    remove = FALSE)
  
  outfile <- file.path(file_dir, "pitchtier_df.RDS")
  saveRDS(pitchtier_df, outfile)
  return(outfile)
}

#' Preprocess resynthesis pitch tracks
#' 
#' Apply preprocessing steps to the raw pitch tracks extracted by 
#' `save_raw_pt_dfs`
#'
#' @param resynth_directory Directory of resynthesized wav files
#' @param exp_tag Which experiment this is for
#' @param tune_order Tune order to use for factors
#' @param three_syl_words Vector of three syllable words
#'
#' @return Filepath to the preprocessed pitch tier dataframe RDS file
#' in CalculatedFiles/
save_preprocessed_resynthesis <- function(resynth_directory, 
                                          exp_tag = "exp1",
                                          tune_order,
                                          three_syl_words) {
  pathstring <- gsub("^../","", resynth_directory)
  message("Preprocessing ", pathstring)
  sf <- str_match(resynth_directory, "_([0-9]+)$")[2]
  nuc_path <- paste0(pathstring, "/TextGrids/nuclear_regions.csv")
  pt_path <- paste0(pathstring, "/PitchTiers/pitchtier_df.RDS")
  
  preprocessed_pt <- 
    readRDS(pt_path)  |> 
    left_join(read_csv(nuc_path)) |>
    mutate(is_nuclear = timepoint >= tmin & timepoint <= tmax) |> 
    preprocess_pitchtracks(runmed_k = 5, 
                           time_normalize = TRUE,
                           .fromzero = TRUE) |> 
    mutate(tune = factor(tune, levels = tune_order),
           wordset = ifelse(utterance %in% three_syl_words, 
                            "syl3", "syl2"))
  outfile <- paste0("CalculatedFiles/", exp_tag, "_prepped_pt_",sf,".RDS")
  saveRDS(preprocessed_pt, outfile)
  return(outfile)
}

#' Save averaged pitch contours
#' 
#' Averaging pitch contours takes a while, so this runs those calculations
#' and saves the output as an RDS file.
#' This function calculates 15 equally spaced points for the prenuclear region
#' and 30 equally spaced points for the nuclear region.
#'
#' @param preprocessed_path Path to the preprocessed pitch tier RDS file
#' @param exp_tag Which experiment is this
#' @param tune_order Tune order
#'
#' @return File path to the averaged pitch tracks
save_avg_resynthesis <- function(preprocessed_path, exp_tag = "exp1", tune_order) {
  message(preprocessed_path)
  sf <- str_match(preprocessed_path, "_([0-9]+).RDS$")[2]
  prepped_df <- readRDS(preprocessed_path)
  
  # Calculate average contours, takes some time
  avg_resynthesis <- 
    prepped_df |>
    # Fix coding issue with points outside of final boundary
    mutate(is_nuclear = fix_OOB_nuclear_coding(is_nuclear),
           tune = factor(tune, levels = tune_order)) |>
    average_pitchtracks(section_by = "is_nuclear",
                        pulses_per_section = c("FALSE" = 15,
                                               "TRUE" = 30),
                        time_by = "timepoint_norm",
                        aggregate_by = file ~ tune + wordset + pa_val + bt_val,
                        parallelize = TRUE)
  
  outfile <- paste0("CalculatedFiles/", exp_tag, "_avg_resynth_", sf, ".RDS")
  
  saveRDS(avg_resynthesis, outfile)
  
  # Return path to averaged contours rds file
  return(outfile)
}