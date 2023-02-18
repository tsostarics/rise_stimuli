#' Calculate semitone difference
#' 
#' Helper to calculate the difference, in semitones, between pairwise
#' elements of two numeric vectors.
#'
#' @param hz1 Numeric vector
#' @param hz2 Numeric vector of same length as `hz1`
#'
#' @return A numeric vector of same length as input with the semitone differences
#' of hz2 from corresponding elements in hz1
semitone_difference <- function(hz1, hz2) {
  stopifnot(length(hz1) == length(hz2))
  vapply(seq_along(hz1),
         \(i) {
           hz_to_semitones(hz2[i], hz1[i],.quiet = TRUE)
         },
         1.0)
}

#' Code pitch errors by threshold method
#' 
#' Classifies a pulse as an error if its difference from its previous pulse
#' is too steep of a rise or fall.
#'
#' @param pitchtier_df Pitchtier dataframe
#' @param rise_threshold Threshold for rise steepness
#' @param fall_threshold Threshold for fall steepness
#'
#' @return Dataframe with logical error column
code_errors_threshold <- function(pitchtier_df, 
                                  rise_threshold = 1.2631578947,
                                  fall_threshold = 1.7142857143) {
  pitchtier_df |> 
    dplyr::mutate(timepoint_next = lead(timepoint, order_by = timepoint),
                  hz_next = lead(hz, order_by = timepoint),
                  st_diff = semitone_difference(hz_next, hz),
                  time_diff_ms = (timepoint_next - timepoint)*1000,
                  error = case_when(st_diff < 0 & 
                                      abs(st_diff) > 
                                      ((time_diff_ms) * (fall_threshold)) ~ TRUE,
                                    st_diff > 0 &
                                      abs(st_diff) >
                                      ((time_diff_ms) * (rise_threshold)) ~ TRUE,
                                    TRUE ~ FALSE))
}

#' Cluster errors
#' 
#' Helps identify more errors based on results of `forecast_errors`, essentially
#' establishing clusters of pulses rather than individual pulses
#'
#' @param error_df Error-coded df
#' @param threshold Threshold for doubling
#' @param .group Column indexing individual recordings
#' @param .pitchval Column indexing pitch values (default hz)
#'
#' @return Updates the error column
cluster_errors <- function(error_df,
                           threshold = 1.8,
                           .group = "file",
                           .pitchval = "hz") {
  pulses_df <- 
    error_df |> 
    dplyr::group_by(across(all_of(.group))) |> 
    dplyr::mutate(pulse_i = seq_len(n()))
  
  forecast_df <- forecast_errors(pulses_df)
  
  hzi <- paste0(.pitchval, c(".x",".y"))
  
  pulses_df |> 
    dplyr::left_join(forecast_df, by = c("pulse_i", .group)) |> 
    dplyr::mutate(error = abs(semitone_difference(.data[[hzi[2]]], 
                                                  .data[[hzi[1]]])) > threshold,
                  error = ifelse(is.na(error), FALSE, error)) |> 
    dplyr::select(-hzi[2]) |> 
    dplyr::rename(!!sym(.pitchval) := hzi[1])
}

#' Check for pitch doubling
#' 
#' Evaluates if adjacent pitch pulses are erroneously doubled (*1.8) or halved
#' (*.52)
#'
#' @param pitchtier_df Pitchtier dataframe
#' @param .group Column indexing individual recordings
#' @param .pitchval Column containing pitch values (default hz)
#' @param .time_by Column containing time values (default timepoint)
#'
#' @return Dataframe with error column
check_pitch_doubling <- function(pitchtier_df,
                                 .group = 'file',
                                 .pitchval = 'hz',
                                 .time_by = 'timepoint') {
  has_error_col <- "error" %in% names(pitchtier_df)
  
  checked_df <- 
    pitchtier_df |> 
    dplyr::group_by(across(all_of(.group))) |> 
    dplyr::mutate(lead_hz = lead(.data[[.pitchval]], order_by = .data[[.time_by]]),
                  lag_hz = lag(.data[[.pitchval]], order_by = .data[[.time_by]]),
                  lead_r = lead_hz / .data[[.pitchval]],
                  lag_r = .data[[.pitchval]] / lag_hz,
                  ratio = case_when(is.na(lead_r) ~ lag_r,
                                    is.na(lag_r) ~ lead_r,
                                    TRUE ~ lead_r),
                  multiplier_error = ratio > 1.8 | ratio < .52) |>
    dplyr::select(-lead_hz, -lag_hz, -lead_r, -lag_r)
  
  if (has_error_col)
    checked_df <- 
    checked_df |> 
    mutate(error = error | multiplier_error)
  
  checked_df
}

#' Get contiguous groups
#' 
#' Given a set of differences, return groupings based on where the differences
#' start and stop being -1. This is used to group together pulses based on
#' forecast::tsoutliers
#'
#' @param diffs Differences between adjacent pulse indexes
#'
#' @return Group assignments for elements of diffs
assign_contiguous_groups <- function(diffs) {
  contiguous_groups <- diffs[NA]
  grp_i <-  1
  for (diff_i in seq_along(diffs)) {
    cur_val <- diffs[diff_i]
    if (cur_val != -1 | is.na(cur_val))
      grp_i = grp_i + 1
    contiguous_groups[diff_i] <- grp_i
  }
  
  contiguous_groups
}


#' Use forecast to find outliers
#' 
#' Uses forecast::tsoutliers to identify time series outliers.
#'
#' @param pulses_df Pulse dataframe
#' @param .group Column indexing individual recordings
#' @param .pitchval Column containing pitch values
#' @param method How to evaluate the pitch value for a group of errors. Defaults
#' to `stats::median`
#'
#' @return Dataframe with errors coded
forecast_errors <- function(pulses_df,
                            .group = "file",
                            .pitchval = "hz",
                            method= stats::median) {
  pulses_df |> 
    dplyr::group_by(across(all_of(.group))) |> 
    dplyr::summarize(pulse_i = forecast::tsoutliers(.data[[.pitchval]])[[1]],
                     !!sym(.pitchval) := forecast::tsoutliers(.data[[.pitchval]])[[2]]) |> 
    dplyr::group_by(across(all_of(.group))) |> 
    dplyr::mutate(pulse_diff = pulse_i - lead(pulse_i), 
                  pulse_diff = ifelse(is.na(pulse_diff),
                                      lag(pulse_i) - pulse_i, 
                                      pulse_diff),
                  contig_group = assign_contiguous_groups(pulse_diff)) |> 
    dplyr::filter(pulse_diff == -1) |> 
    group_by(across(all_of(c(.group, "contig_group")))) |> 
    dplyr::mutate(!!sym(.pitchval) := method(.data[[.pitchval]])) |> 
    dplyr::ungroup() |> 
    dplyr::select(-pulse_diff, -contig_group)
}
