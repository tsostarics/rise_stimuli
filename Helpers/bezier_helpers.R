#' Make bezier curve for bitonal pitch accent
#' 
#' Given the anchor and control points for the low and high tonal targets of a
#' bitonal pitch accent (L+H\* or L\*+H), compute a bezier curve and then 
#' quantize it to a resolution of `npoints` points that are equally spaced
#' along the curve. Note that this does not necessarily mean they are equally
#' spaced along the x-axis; this is especially noticeable in areas of high
#' curvature. Check `make_bitonal_curve2` for a different method based on
#' `max.dist` in `pointsOnBezier`
#'
#' @param anchor_L_t Time for the low tonal target
#' @param anchor_H_t Time for the high tonal target
#' @param control_L_t Time for the low target's control point
#' @param control_H_t Time for the high target's control point
#' @param L_freq Frequency value for the low tone
#' @param H_freq Frequency value for the high tone
#' @param npoints Number of points to quantize the curve by
#'
#' @return Output of pointsOnBezier
make_bitonal_curve <- function(anchor_L_t,
                               anchor_H_t,
                               control_L_t,
                               control_H_t,
                               L_freq,
                               H_freq,
                               npoints = 7
) {
  point_matrix <- 
    matrix(c(
      anchor_L_t, L_freq,
      control_L_t, L_freq,
      control_H_t, H_freq,
      anchor_H_t, H_freq
    ), byrow = TRUE, nrow = 4)
  
  
  pointsOnBezier(point_matrix, n = npoints)
}

#' Make bezier curve for bitonal pitch accent
#' 
#' Given the anchor and control points for the low and high tonal targets of a
#' bitonal pitch accent (L+H\* or L\*+H), compute a bezier curve and return
#' equally spaced points along the x-axis. This results in a large number of
#' points (~1060), and so will need to be reduced later; 
#' `make_bitonal_continuum2` handles that part.
#'
#' @param anchor_L_t Time for the low tonal target
#' @param anchor_H_t Time for the high tonal target
#' @param control_L_t Time for the low target's control point
#' @param control_H_t Time for the high target's control point
#' @param L_freq Frequency value for the low tone
#' @param H_freq Frequency value for the high tone
#'
#' @return Output of pointsOnBezier
make_bitonal_curve2 <- function(anchor_L_t,
                               anchor_H_t,
                               control_L_t,
                               control_H_t,
                               L_freq,
                               H_freq) {
  point_matrix <- 
    matrix(c(
      anchor_L_t, L_freq,
      control_L_t, L_freq,
      control_H_t, H_freq,
      anchor_H_t, H_freq
    ), byrow = TRUE, nrow = 4)
  
  
  pointsOnBezier(point_matrix,
                 method = 'max.dist', 
                 max.dist = .05)
}

#' Continuum between quantized bezier curves
#' 
#' Given two bezier curves quantized to the same number of points, interpolate
#' an `nsteps` continuum between the two. This is accomplished by creating
#' continuum values between each pair of quantized points. This function should
#' be used with the output of `make_bitonal_curve`
#'
#' @param curve1 Output of pointsOnBezier
#' @param curve2 Output of pointsOnBezier, of same number of points as curve1
#' @param nsteps Number of steps to interpolate
#'
#' @return A dataframe with steps, x, and y values
make_bezier_continuum <- function(curve1, curve2, nsteps = 5) {
  points1 <- curve1$points
  points2 <- curve2$points
  stopifnot(nrow(curve1) == nrow(curve2))
  
  
  timepoint_continuum <- 
    map2(points1[,1], points2[,1],
         \(x, y)
         seq(x, y, length.out = nsteps))
  
  value_continuum <- 
    map2(points1[,2], points2[,2],
         \(x, y)
         seq(x, y, length.out = nsteps))
  
  data.frame(step = rep(seq_len(nsteps), times = length(timepoint_continuum)),
             x = c(timepoint_continuum, recursive = TRUE),
             y = c(value_continuum, recursive = TRUE))
}



#' Equal time indices
#' 
#' Helper function to access the points dataframe and return `npoints` equally
#' spaced t values within the range of values of the curve
#'
#' @param curve Output of `make_bitonal_curve2`
#' @param npoints Number of points to quantize to
#'
#' @return Numeric vector of length `npoints`
equal_time_indices <- function(curve, npoints = 30) {
  ends <- range(curve$points[,1])
  indices <- seq(ends[1], ends[2], length.out = npoints)
  indices
}

#' Get values given quantized time values
#' 
#' Helper to get "y" values for `npoints` equally spaced "x" values
#'
#' @param curve Output of `make_bitonal_curve2`
#' @param npoints Number of points to quantize to
#'
#' @return Numeric vector of length `npoints`
interpolate_points_on_bezier <- function(curve, npoints = 30) {
  indices <- equal_time_indices(curve, npoints)
  
  sosprosody:::interpolate_pitchpoints(indices,
                                       curve$points[,1],
                                       curve$points[,2])
}

#' Continuum between quantized curves
#' 
#' Differs from `make_bezier_continuum` in that this function quantizes the
#' curves differently. This function returns equally-spaced points along the
#' x-axis, then interpolates, while the other function returns equally-spaced
#' points along the curve, then interpolates. This function does a better job
#' at capturing areas of high curvature and being more transparent in its
#' implementation.
#'
#' @param curve1 Output of `make_bitonal_curve2`
#' @param curve2 Output of `make_bitonal_curve2`
#' @param nsteps Number of steps for continuum, defaults to 5
#' @param npoints Number of points to quantize each curve to, defaults to 30
#'
#' @return A dataframe with steps, x, and y values
make_bezier_continuum2 <- function(curve1, curve2, nsteps = 5, npoints = 30){
  indices1 <- equal_time_indices(curve1, npoints)
  indices2 <- equal_time_indices(curve2, npoints)
  vals1 <- interpolate_points_on_bezier(curve1, npoints)
  vals2 <- interpolate_points_on_bezier(curve2, npoints)
  
  timepoint_continuum <- 
    map2(indices1, indices2,
         \(x, y)
         seq(x, y, length.out = nsteps))
  
  value_continuum <- 
    map2(vals1, vals2,
         \(x, y)
         seq(x, y, length.out = nsteps))
  
  data.frame(step = rep(seq_len(nsteps), times = length(timepoint_continuum)),
             x = c(timepoint_continuum, recursive = TRUE),
             y = c(value_continuum, recursive = TRUE))
}



# lhs_points <- make_bitonal_curve(0,.8,.25,.5,
#                                  70, 120,
#                                  npoints = 27)
# 
# lsh_points <- make_bitonal_curve(.5, 1.15, 1, 1,
#                                  70, 120,
#                                  npoints = 27)
# 
# 
# lhs_points2 <- make_bitonal_curve2(0,.8,.25,.5,
#                                  70, 120)
# 
# lsh_points2 <- make_bitonal_curve2(.5, 1.15, 1, 1,
#                                  70, 120)
# 
# continuum_points <- make_bezier_continuum(lhs_points, lsh_points)
# continuum_points2 <- make_bezier_continuum2(lhs_points2, lsh_points2, 5, 13)
# 
# continuum_points |>
#   ggplot(aes(x, y, group = step, color = factor(step)))+
#   geom_line() +
#   geom_point() +
#   geom_point(data = summarize(group_by(continuum_points, step), 
#                               tcogt = get_tcog(t= x, f = y),
#                               tcogf = get_tcog(t= y, f = x)),
#              aes(x = tcogt,
#                  y = tcogf,
#                  color = factor(step)),
#              shape = 8) +
#   theme_bw() +
#   theme(legend.position = 'none')

# arrange(continuum_points, step,x) |>
#   write.csv("Scripts/bezierpoints.csv",row.names = FALSE)
