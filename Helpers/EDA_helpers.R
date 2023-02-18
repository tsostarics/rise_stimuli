plot_averaged_contours <- function(raw_pitchtier_df,
                                   avg_pitchtier_df,
                                   .x = "timepoint_norm",
                                   .y = "hz_runmed",
                                   .group = "file",
                                   .color = "is_nuclear",
                                   .yavg = "avg_hz_smooth",
                                   .facet_by = utterance ~ tune) {
  
  ggplot(raw_pitchtier_df,
         aes(x = !!sym(.x), 
             y = !!sym(.y), 
             group = !!sym(.group), 
             color = !!sym(.color)
         )) +
    geom_line(size = 1,
              alpha = .25) +
    geom_point(data = avg_pitchtier_df,
               aes(x = !!sym(.x), 
                   y = !!sym(.yavg), 
                   color = !!sym(.color)),
               inherit.aes = FALSE,
               shape = 1) +
    facet_grid(.facet_by) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    scale_color_manual(values = c('dodgerblue4','firebrick')) +
    scale_x_continuous(labels = c('0', '.25', '.5', '.75', '1')) +
    theme(legend.position = "none") 
}



#' Fix out of bounds pulses
#' 
#' Occasionally a pulse is beyond the bounds of the textgrid, and so will
#' pop up as `FALSE` for `is_nuclear`. This function changes those pulses at the
#' end to be `TRUE`.
#'
#' @param nuclear_coding Logical vector, will change trailing TRUEs to FALSEs 
#'
#' @return Logical vector
fix_OOB_nuclear_coding <- function(nuclear_coding) {
  n_points <- length(nuclear_coding)
  for (i in rev(seq_len(n_points))) {
    if (nuclear_coding[i])
      break
  }
  
  nuclear_coding[seq(i,n_points)] <- TRUE
  nuclear_coding
}

#' Get duration of pitch track
#' 
#' Calculates the duration of an utterance on the basis of its first and
#' last pitch pulse.
#'
#' @param pitchtier_df Dataframe from `batch_process_pitchtiers` 
#' @param .grouping Column indexing individual recordings, usually `file`
#'
#' @return Summarized dataframe with durations of each grouping
get_pitchtrack_durations <- function(pitchtier_df,
                                     .grouping) {
  # .grouping is a 1 sided formula with unquoted varnames to 
  # pass to group_by
  pitchtier_df |> 
    group_by(across(all_of(labels(terms(.grouping))))) |> 
    summarize(duration = max(timepoint) - min(timepoint)) |> 
    ungroup()
}

plot_duration_histogram <- function(fileduration_df,
                                    .group = 'tune',
                                    .facetby = tune ~ utterance,
                                    .color = "red") {
  lhs <- as.character(.facetby[[2]])
  rhs <- as.character(.facetby[[3]])
  agg <- ifelse(.group == lhs, rhs, lhs)
  strip_bg <- 
    ifelse(lhs == .group,
           list(theme(strip.background.y = element_rect(color = .color,size = 2))),
           list(theme(strip.background.x = element_rect(color = .color,size = 2))))
  
  fileduration_df |>
    ggplot(aes(x = duration)) +
    geom_histogram(data = select(file_durations, -!!sym(agg)),
                   aes(group = {{.group}}),
                   fill = .color,
                   alpha = .2) +
    geom_histogram() +
    facet_grid(.facetby) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = .3)) +
    strip_bg
}