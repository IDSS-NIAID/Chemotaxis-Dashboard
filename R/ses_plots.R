# ses_plots.R
# Figures and code belonging to the Single Experiment Statistics tab

#' ses_tracks_time
#' Plot the number of tracks over time
#' 
#' @param dat A data frame from the trackRaw table. Must contain columns `sID`, `chanID`, `trackID`, `treatment`, `x`, `y`, and `frames`.
#' 
#' @return A ggplot object
#' @export
#' @importFrom dplyr arrange mutate
#' @importFrom ggplot2 aes facet_wrap geom_hline geom_path ggplot scale_color_gradient2 scale_y_reverse
#' @importFrom grDevices rgb
ses_tracks_time <- function(dat)
{
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    chanID <- frames <- lab <- minutes <- sID <- trackID <- treatment <- x <- y <- NULL
  
  dat %>%
    arrange(chanID, trackID, frames) |> 
    
    mutate(lab = paste0(chanID, ": ", sID, ", ", treatment),
           minutes = frames / 2) %>%
    
    ggplot(aes(x = x, y = y, group = trackID, color = minutes)) +
    geom_path() + #connects observations in the order in which they appear in the dataset
    
    ylab('Directed Movement') +
    xlab('Non-directed Movement') +
    
    facet_wrap(~ lab) + #produces multi-panel plot, separate by 'lab'
    
    scale_y_reverse() +
    scale_color_gradient2(low  = 'blue',
                          mid  = rgb(  0, .62, .45),
                          high = rgb(.9, .62, 0),
                          midpoint = 30) +
    
    geom_hline(yintercept = 0, linetype = 2) + #draws a horizontal line at y = 0
    geom_hline(yintercept = 1, linetype = 2) #draws a horizontal line at y = 1
}



#' ses_tracks_v
#' Plot the velocity of tracks over time
#' 
#' @param dat A data frame from the trackRaw table. Must contain columns `sID`, `chanID`, `trackID`, `treatment`, `v_x`, `v_y`, and `frames`.
#' 
#' @return A ggplot object
#' @export
#' @importFrom dplyr arrange filter group_by mutate ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 aes element_blank facet_wrap geom_hline ggplot scale_color_manual stat_smooth theme ylab
#' @importFrom splines bs
ses_tracks_v <- function(dat)
{
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    chanID <- d <- frames <- grp <- minutes <- sID <- trackID <- treatment <- v <- NULL
  
  tracks_v <- pivot_longer(dat, starts_with('v'), names_to = 'd', values_to = 'v') |>
    filter(d != 'v') |> #filters out total velocity (v) if it gets included
    #d = direction, pivot_longer makes data frame longer by making a column 'd' for direction (x-undirected, y-directed)

    # join channels that have the same sample and treatment
    group_by(sID, treatment) |>
    mutate(joint_channels = paste0(paste(unique(chanID), collapse = '/'), ": ", unique(sID), ', ', unique(treatment))) |>
    ungroup() |>
    
    # sort and plot
    arrange(chanID, d, trackID, frames) |>
    mutate(minutes = frames / 2,
           grp = paste(chanID, d)) |>

    #plots velocity (v) over time (Frame), grouping the data by grp which is channel and direction, coloring them by direction
    ggplot(aes(minutes, v, group = grp, color = d)) +
    
    stat_smooth(method = lm, formula = y ~ bs(x, df = 3), se = FALSE) +
    stat_smooth(method = lm, formula = y ~ 1, se = FALSE, linetype = 2, linewidth = .5) +
    
    scale_color_manual(values = c('black', 'gold3'), labels = c('Undirected', 'Directed')) +
    theme(legend.title = element_blank(),
          legend.position = 'top') +
    ylab(expression(paste('Relative Velocity (', mu, 'm / min)'))) +
    geom_hline(yintercept = 0, linetype = 3, linewidth = .5)
  
  #if sample and treatment are available, split by these labels; if not, use channels
  if(!(any(is.na(dat$sID)))){
    tracks_v <- tracks_v + facet_wrap(~joint_channels)
  }else{
    tracks_v <- tracks_v + facet_wrap(~chanID)
  }
  
  return(tracks_v)
}


#' ses_angle_migration
#' Plot the net angle of migration
#' 
#' @param dat A data frame from the trackSummary table. Must contain columns `chanID`, `sID`, `treatment` and `angle_migration`.
#' 
#' @return A ggplot object
#' @export
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes geom_boxplot geom_jitter geom_violin ggplot xlab ylab
ses_angle_migration <- function(dat)
{
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    angle_migration <- chanID <- lab <- sID <- treatment <-  NULL
  
  ### angle of migration
  dat |> 
    mutate(lab = paste0(chanID, ": ", sID, "\n", treatment)) |>

    ggplot(aes(x=as.character(chanID), y=angle_migration, group=lab)) +
    
    geom_violin(scale = "width", width = 0.5) + 
    geom_jitter(width = 0.1,size = 0.75,alpha=0.3) + 
    geom_boxplot(width = 0.05) +
    
    ylab("Migration angle\n(degrees from vertical)") + 
    xlab("Channel")
}


#' ses_angle_migration_time
#' Plot the angle of migration over time
#'
#' @param dat A data frame from the trackRaw table. Must contain columns `chanID`, `sID`, `treatment`, `theta`, and `frames`.
#' 
#' @return A ggplot object
#' @export
#' @importFrom dplyr group_by mutate summarize ungroup
#' @importFrom ggplot2 aes facet_wrap geom_smooth ggplot xlab ylab
#' @importFrom stats quantile
#' @importFrom tidyr pivot_longer
ses_angle_migration_time <- function(dat)
{
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    angle <- chanID <- frames <- lab <- minutes <- quantile <- 
      sID <- theta <- treatment <- `95` <- NULL

  dat |>
    mutate(theta = abs(theta),
           lab = paste0(chanID, ": ", sID, ", ", treatment),
           minutes = frames / 2) |>
    
    # calculate quantiles
    group_by(lab, minutes) |>
    summarize(mean   = mean(theta),
              median = median(theta),
              `95`   = quantile(theta, probs = 0.975)) |>
    ungroup() |>
    
    # pivot longer for plotting
    pivot_longer(cols = c(mean, median, `95`), names_to = 'quantile', values_to = 'angle') |>
    mutate(quantile = factor(quantile, levels = c('mean', 'median', '95'))) |>
    
    # plot
    ggplot(aes(x = minutes, y = angle, linetype = quantile)) +
    geom_smooth(method = 'loess', formula = y ~ x, se = FALSE, linewidth = 0.5, color = 'black') +
    facet_wrap(~lab) +
    
    xlab("Time (minutes)") +
    ylab("Instantaneous\nAngle of migration")
}


#' ses_chemotactic_efficiency
#' 
#' @param dat A data frame from the trackSummary table. Must contain columns `chanID`, `sID`, `treatment` and `ce`.
#' 
#' @return A ggplot object
#' @export
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes geom_violin geom_jitter geom_boxplot ggplot xlab ylab
ses_chemotactic_efficiency <- function(dat)
{
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    ce <- chanID <- lab <- sID <- treatment <- NULL
  
  ### chemotactic efficiency
  dat |>
    mutate(lab = paste0(chanID, ": ", sID, "\n", treatment)) |>
    
    ggplot(aes(x=as.character(chanID), y=ce, group=lab)) +
    
    geom_violin(scale = "width", width = 0.5) + 
    geom_jitter(width = 0.1, size = 0.75, alpha=0.3) + 
    geom_boxplot(width = 0.05) +
    
    ylab("Chemotactic efficiency\n(% vertical movement)") + 
    xlab("Channel")
}


