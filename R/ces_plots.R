# ces_plots.R
# Figures and code belonging to the Cross-Experiment Summary tab

#' ces_v
#' Figure for the distribution of summary velocities
#' 
#' @param dat A data frame exported from the `chanRaw` table. Must contain columns `expID`, `chanID`, `sID`, `treatment`, `time` (equal to `frames` / 2), and `v` (either `v_x` or `v_y`)
#' @param ylab Character value specifying the y-label of the velocity to plot. Should correspond to either 'x' (undirected) or 'y' (directed) velocities.
#' @param wrap_by Character value specifying the column to wrap the plot by. Valid options are `none` (default), `expID`, `chanID`, `sID`, `treatment`, `sID/treatment`.
#' 
#' @return A ggplot object
#' @export
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes expand_limits geom_line geom_hline facet_wrap xlab ylab
#' @importFrom grDevices rgb
ces_v <- function(dat, ylab, wrap_by = 'none')
{
  # check that we have the variables needed
  if(!all(c('expID', 'chanID', 'time', 'v', 'sID', 'treatment') %in% names(dat)))
    stop('Data frame must contain columns `expID`, `chanID`, `sID`, `treatment`, `time`, and `v`')
  
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    expID <- chanID <- sID <- treatment <- time <- v <- v_x <- v_y <- grp <- NULL
  
  ylims <- range(c(dat$v_x, dat$v_y))
  
  # generate the figure
  plt <- mutate(dat, grp = paste(expID, chanID)) |>
    
    ggplot(aes(time, v, group = grp)) +
    
    geom_line(color = rgb(0, 0, 0, 0.3)) +
    
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.2, color = rgb(0,0,1)) +
    
    ylab(ylab) +
    expand_limits(y = ylims) +
    xlab('Time (s)')
  
  # split out by one of these variables if specified
  if(wrap_by == 'expID')
    plt <- plt + facet_wrap(~expID)
  
  if(wrap_by == 'chanID')
    plt <- plt + facet_wrap(~chanID)
  
  if(wrap_by == 'sID')
    plt <- plt + facet_wrap(~sID)
  
  if(wrap_by == 'treatment')
    plt <- plt + facet_wrap(~treatment)
  
  if(wrap_by == 'sID/treatment')
    plt <- plt + facet_wrap(~sID + treatment)
  
  return(plt)
}
