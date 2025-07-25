# qc_plots.R
# Figures and code belonging to the QC tab

#' qc_track_len
#' Figure to qc track length
#' 
#' @param dat A data frame exported from the `trackRaw` table in the database
#' 
#' @return A ggplot2 object
#' @export
#' @importFrom dplyr arrange group_by mutate summarize ungroup
#' @importFrom ggplot2 aes geom_segment ggplot labs
qc_track_len <- function(dat)
{
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    frames <- max_time <- min_time <- trackID <- track_ordered <- NULL

  # return blank plot if there is nothing to plot
  if(nrow(dat) == 0)
    return(plot_nothing('Please pick a subset to plot'))
  
  group_by(dat, trackID) |>
    summarize(min_time = min(frames) / 2,
              max_time = max(frames) / 2) |>
    ungroup() |>
    
    arrange(min_time) |>
    mutate(track_ordered = 1:length(min_time)) |>
    
    ggplot(aes(x = min_time, xend = max_time, y = track_ordered, yend = track_ordered)) +
    geom_segment() +
    labs(x = 'Time (min)', y = 'Track number')
}


#' qc_n_cells
#' Figure to qc number of tracks/cells over time
#' 
#' @param dat A data frame exported from the `trackRaw` table in the database
#' 
#' @return A ggplot2 object
#' @export
qc_n_cells <- function(dat)
{
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    frames <- n_tracks <- time <- trackID <- NULL
  
  # return blank plot if there is nothing to plot
  if(nrow(dat) == 0)
    return(plot_nothing('Please pick a subset to plot'))
  
  tmp <- group_by(dat, frames) |>
    summarize(n_tracks = length(unique(trackID))) |>
    ungroup() |>
    
    mutate(time = frames / 2)
  
  ggplot(tmp, aes(time, n_tracks)) +
    geom_line() +
    labs(x = 'Time (min)', y = '# Tracks')
}
