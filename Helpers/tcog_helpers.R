#' Calculate TCoG from files
#' 
#' Given a path to a .PitchTier and .TextGrid file, calculate TCoG-T
#' for the 2nd interval on the given tier. This is intended to be used with
#' TextGrids that contain a single labeled interval containing the region of
#' interest (eg nuclear tier) or where the interval of interest is the first,
#' but not only, labeled interval (eg syllable tier).
#' 
#' I don't recommend using this with a large number of files, as the time
#' it takes to read 2*n files will cause it to take a while.
#'
#' @param pt_path Path to .PitchTier file
#' @param tg_path Path to .TextGrid file, must match the duration of the 
#' PitchTier file for accurate results
#' @param tier Character name of the tier to use
#' @param norm_time Logical, defaults to `TRUE`, whether to treat the result
#' as a proportion of the interval or to return the absolute time (`FALSE`)
#'
#' @return TCoG-T for the file
tcog_from_pt_tg <- function(pt_path = "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_70/PitchTiers/branning_01_HLL_002_1_1.PitchTier",
                            tg_path = "06c_ResynthesizedRecordingsExp3CURVED/ConstantScaling_70/TextGrids/branning_01_HLL_002_1_1.TextGrid",
                            tier = "syllable",
                            norm_time = TRUE) {
  tier_index <- ifelse(tier == 'nuclear', 3, 4)
  
  tg <- tg.read(tg_path, encoding = "auto")
  nuclear_start <- rPraat::tg.getIntervalStartTime(tg,tier_index, 2)
  nuclear_end <- rPraat::tg.getIntervalEndTime(tg,tier_index, 2)
  pt <- pt.read(pt_path, encoding = "auto")
  
  # dplyr::between is inclusive [x,y]
  where_pt_is_nuclear <- between(pt$t, nuclear_start, nuclear_end)
  nuclear_pt_t <- pt$t[where_pt_is_nuclear]
  nuclear_pt_f <- pt$f[where_pt_is_nuclear]
  
  if (norm_time)
    nuclear_pt_t <- nuclear_pt_t/ max(nuclear_pt_t)
  
  get_tcog(f = nuclear_pt_f, t = nuclear_pt_t)
  
}

#' Calculate TCoG
#' 
#' Calculates the Tonal Center of Gravity given two numeric vectors. The
#' vectors should be equal in length. For TCoG-T, pass the time values to
#' `t` and the frequency values to `f`; for TCoG-F, swap the two.
#'
#' @param f Numeric vector of frequency values
#' @param t Numeric vector of time values, also the domain over which the
#' average is calculated
#'
#' @return Single numeric TCoG value
get_tcog <- function(f, t) {
  stopifnot(length(f) == length(t))
  sum(f * t) / sum(f)
}

#' Calculate Weighted TCoG
#' 
#' Same usage as `get_tcog` but with an extra arguments for the weights.
#' All input vectors must be the same length.
#'
#' @param f Numeric vector of frequency values
#' @param t Numeric vector of time values
#' @param w Numeric vector of weights
#'
#' @return Single numeric weighted TCoG value
get_weighted_tcog <- function(f, t, w){
  stopifnot(length(f) == length(t) && length(t) == length(w))
  sum(f* t *w) / sum(f * w)
}


#' Extract timestamps for syllable nuclei
#' 
#' Helper function to extract the parameters used for calculating Gaussian
#' weights for the weighted TCoG. This function will look up the manually
#' specified syllable information and find the interval index for the stressed
#' vowel of the nuclear word. Then, the function determines the midpoint
#' of the vowel to set as the mean and half the duration of the stressed syllable
#' to use as the standard deviation. This is returned in both absolute times
#' and as a proportion of the stressed syllable.
#' 
#' The `nuclear_words` and `tunes` arguments are used to construct regular
#' expressions for identifying files to use. Because all steps in a continuum
#' have the same exact TextGrid, we only need to load 1 of them. These options
#' further reduce the number of files to look at. For example, if we only care
#' about calculating values for `HLL` files, we can limit our query to only those
#' files.
#'
#' @param tg_dir Directory containing TextGrids
#' @param syl_specs Manually specified syllable information
#' @param nuclear_words Which words/utterances to look up values for
#' @param tunes Which tunes to look up values for
#'
#' @return Dataframe containing the filename used for calculations, the 
#' utterance, and the stressed syllable values
get_nucleus_times <- function(tg_dir, 
                              syl_specs = tar_read("syllable_specification"),
                              nuclear_words = tar_read("nuclear_words"), 
                              tunes = "HLL") {
  tg_patterns <- 
    expand.grid(a = nuclear_words,
                b = "_.._",
                c = tunes,
                d = "_..._1_1.TextGrid") |>
    apply(MARGIN = 1,
          FUN = \(x) paste0(x,collapse = ""))
  
  tg_files <- 
    vapply(tg_patterns,
           \(x)
           list.files(tg_dir, pattern = x),
           "char",
           USE.NAMES = FALSE)
  
  nucleus_times <- 
    map_dfr(tg_files,
            \(f) {
              print(f)
              # Load textgrid and extract utterance
              tg <- tg.read(fileNameTextGrid = file.path(tg_dir, f), "auto")
              utterance <- str_match(f, "^[a-z]+")[1]
              # TODO: add str_match for tune, add tune to the dataframe
              # Look up the index for the nucleus
              nucleus_i <- syl_specs[syl_specs$utterance == utterance &
                                       syl_specs$syl_position_narrow == "nucleus" &
                                       syl_specs$syllable == 1, "phone_i"][[1]]
              
              # Get number of phones in nucleus word from syllable specification
              n_nuclear_phones <- max(syl_specs[syl_specs$utterance == utterance, "phone_i"])
              n_phones <- tg.getNumberOfIntervals(tg, 2)
              
              # Extract syllable times
              syl_duration <- tg.getIntervalDuration(tg, 4, 2)
              syl_start <- tg.getIntervalStartTime(tg, 4, 2)
              syl_end <- tg.getIntervalEndTime(tg, 4, 2)
              
              # Extract nucleus times 
              nucleus_interval <- n_phones - n_nuclear_phones - 1 + nucleus_i
              nucleus_dur <- tg.getIntervalDuration(tg, 2, nucleus_interval)
              nucleus_start <- tg.getIntervalStartTime(tg, 2, nucleus_interval)
              nucleus_end <- tg.getIntervalEndTime(tg, 2, nucleus_interval)
              nucleus_mid_dur <- nucleus_dur / 2
              data.frame(file = f,
                         utterance = utterance,
                         # The time from the start of the recording where the
                         # nucleus midpoint is located
                         mean_abs = nucleus_mid_dur + nucleus_start,
                         sd_abs = nucleus_mid_dur,
                         # The time of the nucleus midpoint as a proportion of
                         # the total syllable duration
                         mean_prop = (nucleus_mid_dur + nucleus_start - syl_start) / syl_duration,
                         sd_prop = nucleus_mid_dur / syl_duration)
            })
  
  nucleus_times
}

#' Calculate tcog for all files in a directory
#' 
#' Given a directory containing subdirectories named `PitchTiers` and 
#' `TextGrids`, calculate TCoG for every file. This takes a while to run
#' and I generally do not recommend using it.
#'
#' @param wavdir Directory containing wavfiles, but more importantly has
#' PitchTiers and TextGrids subdirectories
#' @param tier Tier to calculate TCoG for
#'
#' @return
tcog_directory <- function(wavdir, tier = 'syllable') {
  filenames <- list.files(wavdir,include.dirs = FALSE)
  filenames <- filenames[grepl(".wav$", filenames, perl = TRUE)]
  filenames <- gsub(".wav", "", filenames, perl = TRUE)
  tg_files <- file.path(wavdir, "TextGrids", paste0(filenames, ".TextGrid"))
  pt_files <- file.path(wavdir, "PitchTiers", paste0(filenames, ".PitchTier"))
  
  tcog_values <- 
    vapply(seq_along(filenames),
           \(i)
           tcog_from_pt_tg(pt_files[i], tg_files[i], tier, TRUE),
           1.0)
  data.frame(file = filenames,
             tcog = tcog_values)
}

#' Plot TCoGs for bezier curves
#' 
#' Given two bezier curves via `make_bitonal_curve2` (or from `pointsOnBezier`), 
#' create a continuum between the two (5 steps, quantized to 13 points) and
#' plot the quantized curves with their TCOG values. These values are 
#' unweighted since there's no real notion of segmental string at this level
#' of abstraction, only syllable boundaries.
#'
#' @param lhs_points Curve 1, intended to be L+H* bezier curve
#' @param lsh_points Curve 2, intended ot be L*+H bezier curve
#'
#' @return plots using ggplot the quantized bezier curves with their TCoGs
plot_ideal_bezier_tcog <- function(lhs_points, lsh_points) {
  continuum_points <- make_bezier_continuum2(lhs_points, lsh_points, 5, 13)
  
  # Calculate unweighted TCoG for the continuum of points
  tcogs <- 
    continuum_points |> 
    arrange(step, x) |> 
    group_by(step) |> 
    summarize(tcog_t = get_tcog(t = x, f = y),
              tcog_f = get_tcog(t = y, f = x)) 
  
  continuum_points |> 
    ggplot(aes(x = x, y = y, group = step)) +
    # Plot the bezier curves
    geom_line(size = 1) +
    geom_point() +
    # Annotate the step numbers
    geom_text(data= tcogs,
              aes(label = step,
                  x = tcog_t+.035,
                  y = tcog_f+2.25),
              color = 'gray30') +
    # Add the TCoG values as stars
    geom_point(data= tcogs,
               aes(x = tcog_t,
                   y = tcog_f),
               size = 2,
               shape = 8,
               color = 'gray30') +
    scale_color_brewer(palette = "Set1") +
    theme_bw(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          legend.position = 'none') +
    ylab("Hz") +
    xlab("Proportion of Stressed Syllable Duration") +
    coord_fixed(ratio = 1/100)
}

#' Calculate Weighted TCoG from preprocessed pitchtiers
#' 
#' Given a path to a preprocessed pitch tier file (created by the 
#' `preprocess_resynthesis` series of targets) and the nucleus information
#' from `get_nucleus_times`, calculate the weighted TCoG values for each
#' recording. This is much faster than `tcog_directory`. The `tunes` argument
#' should match what was passed to `get_nucleus_times`.
#' 
#' Note that this will calculate TCoG for the whole nuclear region, as determined
#' by pulses where `is_nuclear` is `TRUE`. One observation is that you can get
#' odd results if the standard deviation is too small, and this tends to happen
#' with very short vowels like in "bit". To override the standard deviations
#' computed by `get_nucleus_times`, pass a numeric value to `.override_sd`,
#' which should be interpreted as a percentage of the stressed syllable.
#'
#' @param pt_df_path Path to RDS file containing preprocessed pitchtier 
#' dataframe
#' @param nucleus_times Output of `get_nucleus_times` 
#' @param tunes Tunes to filter by, should be the same as what was passed to
#' `get_nucleus_times`
#' @param .override_sd Defaults to NULL, set to a numeric value >0 to override
#' the computed standard deviations. Useful when there are short vowels. 
#'
#' @return Dataframe containing weighted TCoG-T and TCoG-F for the nuclear 
#' region of every pitchtier in `pt_df_path` as filtered by `tunes`
weighted_tcog_from_prepped_regions <- function(pt_df_path, 
                                               nucleus_times, 
                                               tunes = "HLL",
                                               .override_sd = NULL) {
  pt_df <- readRDS(pt_df_path)
  
  if (!is.null(.override_sd))
    nucleus_times$sd_prop = .override_sd
  
  nucleus_times <- dplyr::select(nucleus_times,
                                 -file, -mean_abs, -sd_abs)
  
  pt_df |> 
    dplyr::filter(tune %in% tunes,
                  is_nuclear) |> 
    dplyr::select(-semitones_from, -erb, -tmin, -tmax) |> 
    group_by(utterance, tune, bt_val,pa_val) |> 
    # Time normalize each file
    mutate(timepoint_norm = timepoint - min(timepoint), 
           timepoint_norm = timepoint_norm / max(timepoint_norm)) |> 
    left_join(nucleus_times, by = 'utterance') |>
    mutate(weight= dlogis(timepoint_norm, mean_prop, sd_prop),
           weight = weight / max(weight)) |> 
    summarize(tcog_t_raw = get_weighted_tcog(f = hz, t = timepoint_norm, w = weight),
              tcog_f_raw = get_weighted_tcog(t = hz, f = timepoint_norm, w = weight),
              tcog_t_centered = get_weighted_tcog(f = abs(hz-90), t = timepoint_norm, w = weight),
              tcog_f_centered = get_weighted_tcog(t = hz-90, f = timepoint_norm, w = weight),
              tcog_t_semitone = get_weighted_tcog(f = abs(semitone_difference), t = timepoint_norm, w = weight),
              tcog_f_semitone = get_weighted_tcog(t = semitone_difference, f = timepoint_norm, w = weight))
}

#' Calculate unweighted TCoG values
#' 
#' See `weighted_tcog_from_prepped_regions`; this function does the same
#' thing but with unweighted calculations.
#'
#' @param pt_df_path Path to RDS file containing preprocessed pitchtier 
#' dataframe
tcog_from_prepped_regions <- function(pt_df_path) {
  # Only looking at the nuclear region in these
  pt_df <- readRDS(pt_df_path)
  
  pt_df |> 
    dplyr::filter(is_nuclear) |>
    group_by(bt_val,pa_val, utterance, tune) |> 
    # Time normalize each file
    mutate(timepoint_norm = timepoint - min(timepoint), 
           timepoint_norm = timepoint_norm / max(timepoint_norm)) |>  
    summarize(tcog_t_raw = get_tcog(f = hz, t = timepoint_norm),
              tcog_f_raw = get_tcog(f = timepoint_norm, t = hz),
              tcog_t_centered = get_tcog(f = abs(hz-90), t = timepoint_norm),
              tcog_f_centered = get_tcog(f = timepoint_norm, t = hz-90),
              tcog_t_semitone = get_tcog(f = abs(semitone_difference), t = timepoint_norm),
              tcog_f_semitone = get_tcog(f = timepoint_norm, t = semitone_difference))
}

#' Calculate unweighted TCoG values
#' 
#' See `weighted_tcog_from_prepped_regions`; this function does the same
#' thing but with unweighted calculations. Same as the other just removing
#' the filter for the resynthesized files.
#'
#' @param pt_df_path Path to RDS file containing preprocessed pitchtier 
#' dataframe
tcog_from_prepped_regions2 <- function(pt_df_path) {
  # Only looking at the nuclear region in these
  pt_df <- readRDS(pt_df_path)
  
  pt_df |> 
    group_by(bt_val,pa_val, utterance, tune) |> 
    # Time normalize each file
    mutate(timepoint_norm = timepoint - min(timepoint), 
           timepoint_norm = timepoint_norm / max(timepoint_norm)) |>  
    summarize(tcog_t = get_tcog(f = hz, t = timepoint_norm),
              tcog_f = get_tcog(f = timepoint_norm, t = hz))
}

#' Plot empirical TCoG values
#'
#' Given the results of `tcog_from_prepped_regions` or 
#' `weighted_tcog_from_prepped_regions`, plot the TCoG values for each utterance
#' along each step of the continuum. Importantly, this function assumes that
#' you've filtered the recordings using the `tunes` argument in the TCoG
#' functions to only a single tune (e.g., `tunes='HLL'`).
#' 
#' Each step of the continuum for a specific utterance appears as the colored
#' "chains" in each facet. Each color denotes a different boundary tone value,
#' where Red=1 (L-L%) and is conveniently always the lowest chain; Orange=5 
#' (H-H%) and is always the highest chain. The intermediate boundary tone steps
#' ascend monotonically from step 1 to 5.
#' 
#' The pitch accent steps are also from 1 to 5. Step 1 is the filled circle
#' and step 5 is the filled triangle. The intermediate steps are shown as
#' open circles. They are connected in step order (i.e., 1-2-3-4-5). Generally,
#' they go from left to right.
#' 
#' The full distribution for all utterances is shown in the background of
#' each facet to facilitate comparison of where an utterance is relative to
#' other utterances.
#'
#' @param processed_tcog Results of one of the `tcog_from_prepped_regions`
#' functions. Both functions' outputs are treated the same.
#' @param three_syl_words Three syllable words to code for wordset, although
#' this isn't actually used in the current version
#'
#' @return A ggplot for the empirical TCoG values. Should be saved as a pdf with
#' square dimensions (e.g., 8inx8in)
plot_recording_tcog <- function(processed_tcog, three_syl_words, .which_tcog='raw') {
  stopifnot(.which_tcog %in% c('raw','centered','semitone'))
  
  tcog_t_var <- paste0('tcog_t_',.which_tcog)
  tcog_f_var <- paste0('tcog_f_',.which_tcog)
  
  tcog <- 
    processed_tcog |>
    mutate(wordset = ifelse(utterance %in% three_syl_words, "syl3","syl2"),
           grp = paste0(utterance, bt_val),
           shp = case_when(pa_val == 1 ~ 16,
                           pa_val == 5 ~ 17,
                           TRUE ~ 21),
           sz = case_when(pa_val %in% c(1,5) ~ 2,
                          TRUE ~ 1.2)) |> 
    dplyr::filter(tune == "HLL") |> 
    mutate(grp = paste0(utterance, bt_val)) |> 
    ungroup()
  
  tcog |> 
    ggplot(aes(x = !!sym(tcog_t_var), 
               y = !!sym(tcog_f_var),
               color = bt_val,
               group = grp,
               shape = I(shp))) +
    geom_path(data = dplyr::select(tcog, -utterance),
              color = 'gray90',
              alpha = .8) +
    geom_point(data = dplyr::select(tcog, -utterance),
               color = "gray90",
               alpha = .8,
               shape = 16)+
    geom_path() + 
    geom_point(aes(size = I(sz)), fill = 'white') + 
    # facet_grid(wordset~.) +
    facet_wrap(~utterance) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal(base_size = 14) +
    theme(legend.position = 'none',
          panel.grid.major = element_line(size = .3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA, color = "black")) +
    xlab("TCoG-T\n(% of nuclear word)") +
    ylab("TCoG-F (Hz)") +
    # coord_fixed(ratio = 1/260) +
    scale_x_continuous(labels = scales::label_percent(suffix = ""))
}
