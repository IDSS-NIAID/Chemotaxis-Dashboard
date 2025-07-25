# summaries.R
# summarization functions for raw track data

#' summarize_tracks
#'
#' @param trackRaw A data frame from the trackRaw table.
#'
#' @return A data frame with summarized track information.
#' @export
#'
#' @importFrom dplyr group_by summarize mutate select
#' @importFrom stats sd
summarize_tracks <- function(trackRaw) {
  # for those pesky no visible binding notes
  if(FALSE)
    chanID <- trackID <- sID <- treatment <- frames <- x <- y <- theta <- v_x <- v_y <-
      delta_y <- delta_x <- distance_traveled <- v <- n_frames <- NULL

  track_summ <- trackRaw |>
    group_by(chanID, trackID, sID, treatment) |>

    summarize(
      n_frames = n(),

      # We need the total change in x and y direction for calculations below (direction of change only matters for y)
      delta_x = abs(x[n_frames] - x[1]),
      delta_y = y[n_frames] - y[1],

      # calculate the total distance traveled
      distance_traveled = sqrt( delta_x^2 + delta_y^2 ),

      # Chemotactic efficiency: ce for each track is the change in y (delta_y) divided by the total distance (distance_traveled) 
      ce = delta_y / distance_traveled * 100,
      
      # Angle of migration: angle of migration between the start and end point, converted from radians to degrees
      # zero degrees would represent a net movement straight in the vertical direction. angle is measured from this vertical line of 0 degrees
      angle_migration = 180*(abs(atan(delta_x/delta_y)))/pi,

      # average instantaneous angle of migration (theta)
      av_theta = mean(theta),
      
      #peak velocity
      max_v = max(v),
      
      #average velocity and standard deviation
      av_velocity = mean(v),
      sd_velocity = sd(v)
    ) |>
  
    #deleting columns with intermediate variables (used for calculation but not needed outside `summarize_tracks`)
    select(-delta_y, -delta_x) |>
    
    ungroup()

  track_summ
}