#' Add nuclear tier to textgrid
#' 
#' Copies the last word on the words tier to a new nuclear tier
#'
#' @param tg Textgrid containing a words tier and no nuclear tier
#'
#' @return Textgrid with a new nuclear tier
add_nuclear_tier <- function(tg) {
  nuc_word_i <- length(tg$words$label) - 1L
  nuc_word <- tg$words$label[nuc_word_i]
  tg <- tg.insertNewIntervalTier(tg,newTierName = "nuclear")
  nuc_tier_i <- which(names(tg)=='nuclear')
  word_tier_i <- which(names(tg) == "words")
  nuc_tmin <- tg.getIntervalStartTime(tg, word_tier_i, nuc_word_i)
  nuc_tmax <- tg.getIntervalEndTime(tg, word_tier_i, nuc_word_i)
  
  tg <- tg.insertBoundary(tg, nuc_tier_i,time = nuc_tmin,label = nuc_word)
  tg <- tg.insertBoundary(tg, nuc_tier_i,time = nuc_tmax, label = "")
  
  tg
}

#' Add syllable tier to textgrid
#' 
#' This function depends on a manually written lookup table for the syllabification
#' of nuclear words used in these materials. This function will add a new tier
#' that marks off the individual syllables based on how the phones in the nuclear
#' word are coded in the spreadsheet.
#'
#' @param tg Textgrid that does not contain a syllable teir
#' @param syl_specs manually created syllable specifications
#'
#' @return Textgrid with syllable tier
add_syllable_tier <- function(tg, syl_specs) {
  # Get which word interval has the nuclear word
  nuc_word_i <- length(tg$words$label) - 1L
  nuc_word <- tg$words$label[nuc_word_i]
  
  # Get which tier is the phones tier, need index number later
  phone_tier_i <- which(names(tg) == "phones")
  
  # Add a new blank interval tier and save its index
  tg <- tg.insertNewIntervalTier(tg,newTierName = "syllable")
  syl_tier_i <- which(names(tg)=='syllable')
  
  # Look up the phone-syl correspondences from the syllable specification
  nuc_syllables <- syl_specs[syl_specs$utterance == nuc_word,]
  
  # Get the total number of phones and which interval contains the first phone
  n_phones <- length(nuc_syllables$phone_label)
  first_phone <- length(tg$phones$label) - n_phones
  
  # Add a new boundary at the start of the syllable
  # boundary_time <- tg.getIntervalStartTime(tg, phone_tier_i, first_phone)
  # tg <- tg.insertBoundary(tg, syl_tier_i, boundary_time, label = "")
  
  # Labels of new intervals are inserted into the interval on the right,
  # which is counter what Praat usually does
  phone_j <- 1
  prev_label <- ""
  
  for (phone_i in seq(first_phone, first_phone + n_phones-1)) {
    # Get the syllable this phone belongs to and what time it starts
    syl_number <- as.character(nuc_syllables[[phone_j,'syllable']])
    phone_start <- tg.getIntervalStartTime(tg, phone_tier_i, phone_i)
    
    # If the syllable label/number has changed, add a boundary with
    # the syllable number as the label
    if (syl_number != prev_label) 
      tg <- tg.insertBoundary(tg, 
                              syl_tier_i, 
                              phone_start,
                              label = syl_number)
    
    prev_label <- syl_number
    phone_j <- phone_j + 1
  }
  
  # Add a final blank interval
  final_boundary_time <- tg.getIntervalStartTime(tg,
                                                 phone_tier_i,
                                                 length(tg$phones$label))
  
  tg.insertBoundary(tg, syl_tier_i, final_boundary_time, label = "")
}

#' Create a duration tier based on a textgrid tier
#' 
#' This function creates a duration tier where each named interval is
#' scaled to a new duration. `scale_by` determines how each interval is scaled;
#' it is a named numeric vector where the names are the labels of the intervals
#' you want to scale and the values are what you want to scale them to. If
#' an interval is named but is not found in this, it will be scaled to the
#' value given by `.default_scale`, which in most cases should be 1. Warning
#' that you must be careful when you have intervals with duplicate labels.
#'
#'
#'
#' @param textgrid Textgrid with an interval tier
#' @param tier Name of the interval tier to get labels from
#' @param scale_by Named numeric vector
#' @param .default_scale For intervals not scaled to values in `scale_by`, what
#' value should they be scaled to? Defaults to 1 (no change)
#' @param epsilon Precision, see https://www.fon.hum.uva.nl/praat/manual/Intro_8_2__Manipulation_of_duration.html
#'
#' @return DurationTier object
dt_from_tg_tier <- function(textgrid,
                            tier = "syllable",
                            scale_by = c("1" = 1/3,
                                         "2" = 1/2),
                            .default_scale = 1,
                            epsilon = .0000001) {
  stopifnot(tier %in% names(textgrid))
  
  tg_t1 <- textgrid[[tier]][['t1']]
  tg_t2 <- textgrid[[tier]][['t2']]
  tg_tmin <- as.numeric(class(textgrid)['tmin'])
  tg_tmax <- as.numeric(class(textgrid)['tmax'])
  tg_labels <- textgrid[[tier]][['label']]
  filename <- gsub("TextGrid$", "DurationTier", class(textgrid)[['name']], 
                   perl = TRUE)
  
  dt_values <- make_nuclear_dt_values(tg_t1,
                                      tg_t2,
                                      tg_tmin,
                                      tg_labels,
                                      scale_by,
                                      .default_scale,
                                      epsilon)
  
  dt <- list(t = dt_values[['t']],
             f = dt_values[['f']],
             tmin = tg_tmin,
             tmax = tg_tmax)
  
  class(dt) <- c("list", 
                 tmin = as.character(tg_tmin),
                 tmax = as.character(tg_tmax),
                 type = "DurationTier",
                 name = filename)
  dt
}

#' Make duration tier values given interval tier information
#' 
#' Helper for `dt_from_tg_tier`, iterates through time values on the interval
#' tier and places the points in the correct places
#'
#' @param t1 
#' @param t2 
#' @param tmin 
#' @param int_labels 
#' @param scale_by 
#' @param .default_scale 
#' @param epsilon 
#'
#' @return
#' @export
#'
#' @examples
make_nuclear_dt_values <- function(t1,
                                   t2,
                                   tmin = 0,
                                   int_labels,
                                   scale_by = c(syl1 = 1/3,
                                                syl2 = 2),
                                   .default_scale = 1,
                                   epsilon = .0000001) {
  n_intervals <- length(int_labels)
  
  stopifnot(n_intervals == length(t1) && n_intervals == length(t2))
  dt_f1 <- numeric(n_intervals)
  dt_f2 <- numeric(n_intervals)
  dt_t1 <- numeric(n_intervals)
  dt_t2 <- numeric(n_intervals)
  # Pad sides
  
  prev_labeled = FALSE
  for (int_i in seq_len(n_intervals)) {
    cur_label <- int_labels[int_i]
    cur_start <- t1[int_i]
    cur_end <- t2[int_i]
    
    # If this is an interval not specified by scale_by, use the default value
    is_empty <- !cur_label %in% names(scale_by)
    scale_factor <- ifelse(is_empty, .default_scale, scale_by[cur_label])
    
    # The beginning and end of the interval use the scale factor
    dt_f1[int_i] <- scale_factor
    dt_f2[int_i] <- scale_factor
    
    # Prioritize exact placement for non-empty intervals, otherwise place points
    # to given precision value
    dt_t1[int_i] <- ifelse(is_empty || prev_labeled, 
                           cur_start + epsilon,
                           cur_start)
    dt_t2[int_i] <- ifelse(is_empty, cur_end - epsilon, cur_end)
    
    if (!is_empty)
      prev_labeled <- TRUE
  }
  
  # Combing the t1,t2 and f1,f2 vectors into a
  # single vector through pairwise pivoting
  dt_t <- numeric(n_intervals * 2)
  dt_f <- numeric(n_intervals * 2)
  indices <- seq(1, n_intervals*2, 2)
  original_i <- 1
  
  for (i in indices) {
    dt_t[i] <- dt_t1[original_i]
    dt_t[i + 1] <- dt_t2[original_i]
    dt_f[i] <- dt_f1[original_i]
    dt_f[i + 1] <- dt_f2[original_i]
    original_i <- original_i + 1
  }
  
  list(t = dt_t,
       f = dt_f) 
}

#' Calculate scaling factor
#' 
#' Given target syllable durations and a textgrid with a syllable tier,
#' calculate how far off each syllable interval is from the target.
#' Eg, target duration is 450 but actual duration is 300, the interval
#' would need to be scaled by 1.5
#'
#' @param syllable_durations Target syllable durations dataframe calculated
#' by the exploratory analysis
#' @param tg Textgrid with a syllable tier
#'
#' @return Named numeric vector to pass to `scale_by` arguments
get_scaling_factors <- function(syllable_durations, tg) {
  stopifnot('nuclear' %in% names(tg))
  
  # Note: Hard coded and expects add_nuclear_tier's output
  utterance <- tg$nuclear$label[2] 
  
  # Get the target durations for the utterance (=durations for wordset)
  target_durations <- 
    syllable_durations[syllable_durations$utterance == utterance,]
  
  # Find where the labeled syllable intervals are
  which_labeled <- tg$syllable$label != ""
  syl_labels <- tg$syllable$label[which_labeled]
  
  # Calculate durations of each interval
  from_durations <- tg$syllable$t2[which_labeled] - tg$syllable$t1[which_labeled] 
  
  # Extract numeric values for target durations
  to_durations <- target_durations[['syl_duration']]
  
  # Calculate scaling factors to feed to duration tier later
  scaling_factors <- to_durations / from_durations
  names(scaling_factors) <- syl_labels# usually c('1','2')
  
  scaling_factors
}


#' Write files needed for duration manipulations
#' 
#' Given a list of files, the manual syllable specification, and the calculated
#' syllable durations, write the durationtier file needed to manipulate the
#' syllables of each file to the target value. This will also write a textgrid
#' that can be used with the eventual scaled files. Note the directories
#' are hard coded here. Note that the textgrid calculations are performed by
#' `sosprosody::scale_tg_by_dt` and are based on area calculations from Praat's
#' source code.
#'
#' @param filelist List of files, usually the chosen recordings
#' @param syllable_specification Manual syllable specification
#' @param syllable_durations Calculated syllable target durations
#' 
#' @return nothing, writes a bunch of files in some directories
write_scaling_files <- function(filelist, syllable_specification, syllable_durations) {
  filenames <- read_lines(filelist)
  
  tgs <- paste0(filenames, ".TextGrid")
  dts <- paste0(filenames, ".DurationTier")
  
  # Note: This was changed from the raw MFA textgrids to the manually
  # fixed textgrids for only the limited set of chosen recordings.
  # input_tg_paths <- file.path("02_PossibleRecordings/MFA_textgrids",tgs)
  input_tg_paths <- file.path("03_ChosenRecordings/FixedTextGrids", tgs)
  
  # Output for textgrids, but we're actually not going to use these textgrids
  # because the manually fixed ones already have the relevant syllable and
  # nuclear tiers.
  unscaled_tg_paths <- file.path("03_ChosenRecordings/TextGrids", tgs)
  scaled_tg_paths <- file.path("04_ScaledRecordings/TextGrids", tgs)
  dt_paths <- file.path("03_ChosenRecordings/DurationTiers", dts)
  
  for (tg_i in seq_along(input_tg_paths)) {
    # Load textgrid and add nuclear and syllable tiers, save this textgrid
    tg <- 
      rPraat::tg.read(input_tg_paths[tg_i], encoding = 'auto') |>
      drop_tiers(c('nuclear','syllable')) |> 
      add_nuclear_tier() |> 
      add_syllable_tier(syllable_specification)
    rPraat::tg.write(tg, fileNameTextGrid = unscaled_tg_paths[tg_i])
    
    # Look up scaling factors for the current textgrid
    scaling_factors <- get_scaling_factors(syllable_durations, tg)
    
    # Create the durationtier for this file
    dt <- dt_from_tg_tier(textgrid = tg,
                          tier = "syllable",
                          scale_by = scaling_factors)
    dt.write(dt, dt_paths[tg_i])
    
    # Scale the textgrid by the duration tier, save to scaled textgrid directory    
    tg_scaled <- scale_textgrid_by_dt(tg, dt)
    tg.write(tg_scaled, scaled_tg_paths[tg_i])
  }
  
  return(0)
}

#' Drop tiers
#' 
#' helper to remove tiers based on names
#'
#' @param tg Textgrid
#' @param remove_tiers Names of tiers to remove 
#'
#' @return Textgrid without the given tiers. If they didn't exist to begin with,
#' then the textgrid will be unchanged.
drop_tiers <- function(tg, remove_tiers) {
  remove_tiers <- remove_tiers[remove_tiers %in% names(tg)]
  
  for (tier in remove_tiers)
    tg[[tier]] <- NULL
  
  tg
}

#' Write files for constant duration manipulations
#' 
#' This function is used to create the duration tiers used to scale the
#' final syllable of textgrids by a given number. This is intended to be
#' used with a directory of files that have already had their syllables scaled
#' to be the same target duration.
#' 
#' For example, using `scale_by = .75` would scale the final syllable duration
#' to be only 75% of its original duration (25% shorter). Using 1 would result
#' in no change, >1 will increase duration, and <1 will reduce duration.
#' Do not use negative numbers.
#'
#' @param scaled_tg_dir Directory containing textgrids
#' @param scale_by Amount to scale final syllable by
#' @param output_subdir Where to put the subdirectories for the new textgrids
#' and duration tiers
#'
#' @return Subdirectory containing duration tiers
write_constant_scaling_dts <- function(scaled_tg_dir = "04_ScaledRecordings/TextGrids",
                                       scale_by = .8,
                                       output_subdir = "05_NudgedRecordings/ConstantScaling_"){
  output_subdir <- paste0(output_subdir, round(scale_by, 2)*100)
  out_tg_dir <- paste0(output_subdir, "/TextGrids")
  out_dt_dir <- paste0(output_subdir, "/DurationTiers")
  
  if (!dir.exists(output_subdir)) {
    dir.create(output_subdir)
    dir.create(out_tg_dir)
    dir.create(out_dt_dir)
  }
  
  tgs <- list.files(scaled_tg_dir)
  dts <- gsub(".TextGrid$", ".DurationTier", tgs)
  
  input_tg_paths <- file.path(scaled_tg_dir,tgs)
  output_tg_paths <- file.path(out_tg_dir, tgs)
  output_dt_paths <- file.path(out_dt_dir, dts)
  
  for (tg_i in seq_along(input_tg_paths)) {
    # Load textgrid and add nuclear and syllable tiers, save this textgrid
    tg <- rPraat::tg.read(input_tg_paths[tg_i]) 
    
    # Get the syllables from the syllable textgrid, the labels are either 1,2
    # or 1,2,3 after removing the blanks on the ends
    syllables <- tg$syllable$label
    syllables <- as.integer(syllables[syllables != ""])
    n_syllables <- max(syllables)
    
    # Set all syllables that aren't the final syllable to 1
    scale_factors <- rep(1, n_syllables)
    # Set the names to the syllable labels (just their numeric indices)
    names(scale_factors) <- syllables
    # Change the final syllable to use the scaling factor we're interested in
    scale_factors[n_syllables] <- scale_by
    
    # Create the durationtier for this file
    dt <- dt_from_tg_tier(textgrid = tg,
                          tier = "syllable",
                          scale_by = scale_factors)
    dt.write(dt, output_dt_paths[tg_i])
    
    # Scale the textgrid by the duration tier, save to scaled textgrid directory    
    tg_scaled <- scale_textgrid_by_dt(tg, dt)
    tg.write(tg_scaled, output_tg_paths[tg_i])
  }
  
  output_subdir
}

