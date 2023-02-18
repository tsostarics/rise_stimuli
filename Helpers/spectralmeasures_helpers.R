#' Process spectral measurements for select files
#' 
#' Given a .txt file containing a list of recordings and the directory they're in,
#' copy the files to a new directory. 
#' Then, get their textgrids and add a nuclear tier containing a single labeled
#' interval with the nuclear word and save them in a new directory.
#' Then, run the shellSauce script at the given directory on the selected
#' wave files and new textgrids, saving a new file with the spectral
#' measurements.
#' 
#' Note this is done because these calculations are very time-intensive yet
#' the script operates over all files in a directory, so we need a temporary
#' directory to
#'
#' @param filelist 
#' @param wavfile_from 
#' @param tg_from 
#' @param wavfile_to 
#' @param tg_to 
#' @param shellSauce_path 
#' @param interval_num 
#' @param wavfrom_relative 
#' @param tgfrom_relative 
#' @param output_file 
#' @param remove_files 
#' @param debug 
# This will calculate spectral measures for a limited set of files
# given by a textfile with the names of files to read. This function
# can copy these to a smaller directory if needed.
get_file_spectral_measures <- function(filelist = "filelist_for_spectral_measures.txt",
                                       wavfile_from = "02_PossibleRecordings",
                                       tg_from = "02_PossibleRecordings/MFA_textgrids",
                                       wavfile_to = "SpectralMeasureFiles",
                                       tg_to = "SpectralMeasureFiles",
                                       shellSauce_path = "Scripts/praatSauce/shellSauce.praat",
                                       wavfrom_relative = "../..",
                                       tgfrom_relative = "../..",
                                       output_file = "spectral_measures.txt",
                                       interval_num = 3,
                                       remove_files = FALSE,
                                       debug = FALSE) {
  
  files <- read_lines(filelist)
  
  # Remove previous files in case the needed files change between runs
  if (remove_files) {
    file.remove(list.files(wavfile_to, full.names = TRUE))
    file.remove(list.files(tg_to, full.names = TRUE))
    
    file.copy(from = file.path(wavfile_from, paste0(files, ".wav")),
              to = file.path(wavfile_to, paste0(files, ".wav")))
    
    
    for (filename in files) {
      curfile <- file.path(tg_from, paste0(filename, ".Textgrid"))
      tg <- add_nuclear_tier(curfile)
      
      tg.write(tg, file.path(tg_to, paste0(filename, ".TextGrid")))
    }
  }
  
  use_existing <- as.integer(!remove_files)
  
  run_praatscript(shellSauce_path,
                  inputdir = file.path(wavfrom_relative, wavfile_to),
                  textgriddir = file.path(tgfrom_relative, tg_to),
                  outputdir = file.path(tgfrom_relative, tg_to),
                  outputfile = output_file,
                  interval_tier = interval_num,
                  skip_these_labels = "^$|^\\s+$",
                  Measure = "every n milliseconds",
                  Points = 10,
                  f0min = 40,
                  f0max = 300,
                  formantTracking = 0,
                  useExistingFormants = use_existing,
                  useExistingPitch = use_existing,
                  use_defaults = TRUE,
                  debug= debug,
                  method = base::shell)
}

#' Calculate spectral measures for directory
#' 
#' This function requires `sosprosody` to be installed and loaded:
#' `devtools::install_github('tsostarics/sosprosody')`
#' 
#' Given a directory containing wav files and a directory containing their
#' associated textgrids, use shellSauce to calculate spectral measures.
#'
#' Very important: Praat scripts evaluate directories relative to the script
#' directory, which often does not equal the user's working directory.
#' Use the _relative arguments to tell praat how to get to the wavfile and
#' tg directories.
#' 
#' For example, say Projects is the working directory here:
#' Project/
#'   Stimuli/
#'     Audio/
#'       a.wav
#'       b.wav
#'     TextGrids/
#'       a.TextGrid
#'       b.TextGrid
#'   Scripts/
#'     PraatSauce/
#'       shellSauce.Praat
#'       
#' then wavfile_from should be Stimuli/Audio, tg_from is Stimuli/TextGrids,
#' and both _relative arguments would be ../.. because the praatscript needs to
#' look 2 directories up the tree (once into Scripts, then into Project).
#' We could also set things up like this, where the output directory (tg_to)
#' is set to SpectralMeasures/. This will also cause all the intermediate
#' files for shellSauce to be saved to this directory.
#' 
#' Project/
#'   Stimuli/
#'     Audio/
#'       a.wav
#'     TextGrids/
#'       a.TextGrid
#'   Scripts/
#'     PraatSauce/
#'   SpectralMeasures/
#'     a.TextGrid
#'     a.Pitch
#'     a.Formant
#'     
#' note: you may need to change the `praat_path` and `method` arguments
#' in the `run_praatscript` call. There are other parameters (such as the
#' f0 min and max) that can be manually changed too.
#'
#' @param wavfile_from Directory containing wav files, relative to current
#' working directory
#' @param tg_from Directory containing tg files, relative to current working
#' directory
#' @param tg_to Directory to save new textgrids to, also serves as the output
#' directory. If not modifying textgrids, set to the same value as `tg_from`, 
#' but note that this will
#' @param shellSauce_path Path to shellsauce script, realtive to current working
#' directory
#' @param wavfrom_relative Where `wavfile_from` is relative to `shellsauce_path`
#' @param tgfrom_relative Where `tg_from` is relative to `shellsauce_path`
#' @param output_file Where to save the output file, as a .txt file path relative
#' to the current working directory
#' @param interval_num Index of the interval tier to use. The praat script
#' will provide measures for every labeled interval on this tier.
#' @param rewrite_tg Logical, whether to rewrite the textgrids, defaults to TRUE
#' but recommended to set to FALSE if not modifying textgrids
#' @param use_existing Logical, for the `useExistingFormants` and `useExistingPitch`
#' arguments in shellSauce.praat. Defaults to FALSE, but if you rerun this script
#' with the same files you can change to TRUE.
#' @param debug Logical, set to TRUE to return the command line call without
#' running the script.
#'
#' @return See `run_praatscript` 
get_dir_spectral_measures <- function(wavfile_from = "02_PossibleRecordings",
                                      tg_from = "02_PossibleRecordings/MFA_textgrids",
                                      tg_to = "SpectralMeasureFiles",
                                      shellSauce_path = "Scripts/praatSauce/shellSauce.praat",
                                      wavfrom_relative = "../..",
                                      tgfrom_relative = "../..",
                                      output_file = "spectral_measures.txt",
                                      interval_num = 3,
                                      rewrite_tg = TRUE,
                                      use_existing = FALSE,
                                      debug = FALSE) {
  # Whether to rewrite textgrids in tg_to directory
  if (rewrite_tg) { 
    file.remove(list.files(tg_to, full.names = TRUE))
    files <- list.files(tg_from, pattern = ".TextGrid$")
    for (filename in files) {
      curfile <- file.path(tg_from, filename)
      tg <- add_nuclear_tier(rPraat::tg.read(curfile))
      
      tg.write(tg, file.path(tg_to, filename))
    }
  }
  
  run_praatscript(shellSauce_path,
                  inputdir = file.path(wavfrom_relative, wavfile_from),
                  textgriddir = file.path(tgfrom_relative, tg_to),
                  outputdir = file.path(tgfrom_relative, tg_to),
                  outputfile = output_file,
                  interval_tier = interval_num,
                  skip_these_labels = "^$|^\\s+$",
                  Measure = "every n milliseconds",
                  Points = 10,
                  f0min = 40,
                  f0max = 300,
                  formantTracking = 0,
                  useExistingFormants = as.integer(use_existing),
                  useExistingPitch = as.integer(use_existing),
                  use_defaults = TRUE,
                  debug= debug,
                  method = base::shell)
}

get_dir_spectral_measures2 <- function(wavfile_from = "02_PossibleRecordings",
                                       file_pattern = "_HLL.+",
                                      tg_from = "02_PossibleRecordings/MFA_textgrids",
                                      tg_to = "SpectralMeasureFiles",
                                      shellSauce_path = "Scripts/praatSauce/shellSauce.praat",
                                      wavfrom_relative = "../..",
                                      tgfrom_relative = "../..",
                                      output_file = "spectral_measures.txt",
                                      interval_num = 3,
                                      rewrite_tg = TRUE,
                                      use_existing = FALSE,
                                      debug = FALSE) {
  requireNamespace('rPraat', quietly = TRUE)
  requireNamespace('sosprosody', quietly = TRUE)
  # Whether to rewrite textgrids in tg_to directory
  if (rewrite_tg) { 
    file.remove(list.files(tg_to, full.names = TRUE))
    files <- list.files(tg_from, pattern = paste0(file_pattern, ".TextGrid$"))
    for (filename in files) {
      curfile <- file.path(tg_from, filename)
      
      file.copy(curfile, file.path(tg_to, filename))
      # Nuclear tier already exists
      # tg <- add_nuclear_tier(rPraat::tg.read(curfile,encoding = "auto"))
      # tg.write(tg, file.path(tg_to, filename))
    }
  }
  
  run_praatscript(shellSauce_path,
                  inputdir = file.path(wavfrom_relative, wavfile_from),
                  textgriddir = file.path(tgfrom_relative, tg_to),
                  outputdir = file.path(tgfrom_relative, tg_to),
                  outputfile = output_file,
                  interval_tier = interval_num,
                  skip_these_labels = "^$|^\\s+$",
                  Measure = "every n milliseconds",
                  Points = 10,
                  f0min = 40,
                  f0max = 300,
                  formantTracking = 0,
                  useExistingFormants = as.integer(use_existing),
                  useExistingPitch = as.integer(use_existing),
                  use_defaults = TRUE,
                  debug= debug,
                  method = base::shell)
}
