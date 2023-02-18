#' Draw continua as praat picture
#' 
#' This function is intended to be used with the `shellDraw.praat` script.
#' This function writes a textfile containing files to load into praat,
#' then loads the given textgrid and wav files and draws each one to the
#' Praat picture window, then saves the file to the given output file path
#' and quits praat.
#' 
#' A few very important notes:
#'  - Every wav file should be the same duration, and should equal the duration
#'  of the given textgrid
#'  - wavdir and wavpattern are used in a non-recursive `list.files` call
#'  - the `_relative` arguments are where the associated files/directories are
#'  relative to the praat script
#'  - The file does need some manual cleanup in illustrator to remove the
#'  excess red boundaries and change the red continua lines to black, but
#'  this isn't too hard.
#'  - You'll need to download the SILDoulos IPA93 font from here:
#'  https://www.wfonts.com/font/sildoulos-ipa93
#'  - Files are drawn in alphabetical order
#'
#' @param wavdir Directory containing wav files of interest
#' @param wavpattern Pattern denoting which files to select, a regular expression
#' passed to `list.files`
#' @param wavdir_relative Where the wavfile directory is relative to the praat
#' script 
#' @param tgpath Path to the textgrid file to use
#' @param tgpath_relative Where the textgrid is relative to the praat script
#' @param outfilepath Filepath for the final .eps file, relative to the praat
#' script
#' @param drawscript Path to the `shellDraw.praat` script relative to the current
#' working directory
#' @param tempdir Directory to save the temp file, should always be the same
#' directory as the
#' @param praat_path Path to the praat executable
#' @param method Method to use for making the system call, defaults to
#' `base::shell`
#' @param remove_tier Integer tier to remove if desired, set to 0 to not remove
#' any tier
#' @param tempfile Temporary file to write text files to 
#' @param unique_temp If you're running this script multiple times in parallel
#' without manually setting the tempfile each time, set this to `TRUE` and it
#' will append a random string of numbers to the filename 
#' (`as.integer(Sys.time()) / rnorm(1, 5, 1)`). Defaults to `FALSE`.
#' @param remove_temp Whether to remove the temp file after executing, defaults to
#' `TRUE`, recommended to keep as `TRUE` if you anticipate running this multiple
#' times and/or running in parallel, otherwise you'll build up a ton of text files
#' in your directory
#'
#' @return Output from the praat script system call
draw_continuum <- function(wavdir = "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_70",
                           wavpattern = "branning_01_HLL",
                           wavdir_relative = "..",
                           tgpath = "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_70/TextGrids/branning_01_HLL_002_1_1.TextGrid",
                           tgpath_relative = "..",
                           outfilepath = "../Figures/praat.eps",
                           drawscript = "Scripts/shellDraw.praat",
                           tempfile = "shellDraw_files.txt",
                           tempdir = "Scripts",
                           praat_path = "Praat.exe",
                           method = base::shell,
                           remove_tier = 3,
                           unique_temp = FALSE,
                           remove_temp = TRUE) {
  # Create temp file containing the files to draw
  files_to_draw <- list.files(wavdir, pattern = wavpattern)
  filepaths <- file.path(wavdir_relative, wavdir, files_to_draw)
  tempfile_R <- file.path(tempdir, tempfile)
  
  # If these need to be unique files, add random numbers to the filename
  if (unique_temp){
    prepend <- as.integer(as.integer(Sys.time()) / rnorm(1, 5, 1))
    tempfile <- paste(prepend, tempfile, sep = "_")
    tempfile_R <- file.path(tempdir, tempfile)
  }
  
  writeLines(filepaths, tempfile_R)
  
  # Run drawing script with provided arguments
  system_call <- run_praatscript(drawscript, 
                  filesAt = tempfile,
                  tgFile = file.path(tgpath_relative, tgpath),
                  outFile = outfilepath,
                  removeTier = remove_tier,
                  use_defaults = TRUE,
                  praat_path = praat_path,
                  method = method,
                  debug = TRUE)
  
  system_call <- gsub("--run", "--new-send", system_call)
  
  output <- method(system_call)
  
  # Remove the file if needed
  if (remove_temp)
    file.remove(tempfile_R)
  
  output
}

#' Draw continuum wrapper
#'
#' Does some additional string preprocessing for the target before passing
#' off to draw_continuum. This is only here to clean up the
#' `draw_continua_praat_pictures` target's code a bit.
#'
#' @param draw_directory Directory of input files
#' @param utterance Utterance to use
#' @param script_path Path to drawing script
#' @param fig_path Path to save figures
#' @param ... Other arguments to `draw_continuum`
#'
#' @return
draw_experiment_recordings <- function(draw_directory, utterance, script_path, fig_path, ...) {
  filename <- gsub("(_ResynthesizedRecordings)|(ConstantScaling_[0-9]+)|(/)",
                   "", 
                   draw_directory,
                   perl = TRUE)
  filename <- paste0(filename, "_", utterance, ".eps")
  filepath <- file.path("../Figures/PraatDrawings/Raw", filename)
  tgpath <- file.path(draw_directory, 
                      "TextGrids", 
                      paste0(utterance, "_1_1.TextGrid"))
  
  draw_continuum(wavdir = draw_directory,
                 wavpattern = utterance,
                 tgpath = tgpath,
                 outfilepath = filepath,
                 drawscript = script_path,
                 ...)
  
  return(file.path(fig_path, filename))
}