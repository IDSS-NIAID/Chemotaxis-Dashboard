# summaries.R
# summarization functions for raw track data

#' summarize_tracks
#'
#' @param trackRaw A data frame from the trackRaw table.
#'
#' @return A data frame with summarized track information.
#' @export
#'
#' @importFrom dplyr group_by summarize mutate select left_join
#' @importFrom purrr map2 map2_dbl
#' @importFrom stats sd
summarize_tracks <- function(trackRaw) {
  # for those pesky no visible binding notes
  if(FALSE)
    chanID <- trackID <- sID <- treatment <- frames <- x <- y <- theta <- v_x <- v_y <-
      delta_y <- delta_x <- distance_traveled <- v <- NULL

  track_summ <- trackRaw |>
    group_by(chanID, trackID, sID, treatment) |>
    summarize(
      # calculate smooth functions of x, y, and theta over time
      x     = map2(list(frames), list(x), ~
                     {
                       if(length(.x) < 4)
                        return(splinefun(.x, .y))
                       
                       tmp <- smooth.spline(.x, .y)
                       splinefun(tmp$x, tmp$y)
                     }),
      y     = map2(list(frames), list(y), ~
                     {
                       if(length(.x) < 4)
                        return(splinefun(.x, .y))
                       
                       tmp <- smooth.spline(.x, .y)
                       splinefun(tmp$x, tmp$y)
                     }),
      sd_theta = sd(theta),
      theta = map2(list(frames), list(theta), ~
                     {
                       if(length(.x) < 4)
                        return(splinefun(.x, .y))
                       
                       tmp <- smooth.spline(.x, .y)
                       splinefun(tmp$x, tmp$y)
                     }),
      frames = map(list(frames), ~ unique(.x)),
      n_frames = n()
    ) |>
    ungroup() |>
    
    mutate(
      # calculate velocity over time (multiply by 2 to calculate velocity in micrometers  per minute - frames are every 30 seconds)
      v_x = map2(x, frames, function(x, f) x(f, deriv = 1) * 2),
      v_y = map2(y, frames, function(y, f) y(f, deriv = 1) * 2),
      v = map2(v_x, v_y, ~ sqrt(.x^2 + .y^2) * sign(.y)), # going down = positive velocity, going up = negative velocity
      
      # convert functions of x, y, and theta back to values
      x     = map2(    x, frames, function(    x, f)     x(f)),
      y     = map2(    y, frames, function(    y, f)     y(f)),
      theta = map2(theta, frames, function(theta, f) theta(f)),
    
      # Chemotactic efficiency
      # calculate net change in y direction - return a value for each track (this could be negative if the cell travels in the wrong direction)
      delta_y = map_dbl(y, ~ .x[length(.x)] - .x[1]),

      # calculate the total distance traveled using the distance formula
      distance_traveled = map2_dbl(x, y, ~ sum( sqrt( (.x[-1]-.x[-length(.x)])^2+(.y[-1]-.y[-length(.y)])^2 ) )),
      
      # chemotactic efficiency for each track is the change in y (delta_y) divided by the total distance (distance_traveled) 
      ce = delta_y / distance_traveled * 100,
      
      # Angle of migration
      # We need the total change in x direction to calculate the angle of migration
      delta_x = map_dbl(x, ~ abs(.x[length(.x)] - .x[1])),
      
      # this finds the angle of migration between the start and end point, converts from radians to degrees
      # zero degrees would represent a net movement straight in the vertical direction. angle is measured from this vertical line of 0 degrees
      angle_migration = 180*(abs(atan(delta_x/delta_y)))/pi,

      # average instantaneous angle of migration (theta)
      av_theta = map_dbl(theta, ~ mean(.x)),
      
      #peak velocity
      max_v = map_dbl(v, ~ max(.x)),
      
      #average velocity and standard deviation
      av_velocity = map_dbl(v, ~ mean(.x)),
      sd_velocity = map_dbl(v, ~ sd(.x))
    ) |>
  
    #deleting columns with intermediate variables (used for calculation but not needed in end file)
    select(-delta_y, -delta_x)

  track_summ
}