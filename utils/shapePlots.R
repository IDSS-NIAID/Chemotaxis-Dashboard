library(ggplot2)
library(dplyr)
library(purrr)
library(readr)

#############
# Functions #
#############

#' Preprocess experiment data
#' 
#' @param experiment experiment date in form YYYYMMDD
#' @return data frame containing shape data with meta data
#' @example preprocess(20180215)
preprocess <- function(experiment){
  system('cd ~')
  shapes_dir <- '/data/IDSS_projects/shape_data'
  shapes <- system('ls /data/IDSS_projects/shape_data', intern = TRUE) %>%
    grep(pattern = experiment, value = TRUE)
  
  # SHAPE DATA #
  dat_shape <- map2_df(shapes_dir, shapes, ~
                         {
                           paste(.x, .y, sep = '/') %>%
                             read_delim(delim = ',') %>%
                             mutate(f = .y,
                                    Track = as.integer(substr(Track,2,nchar(Track)-1)),
                                    channel =  strsplit(f, split = '_CH')[[1]][2] %>%
                                      substr(1,1) %>% 
                                      as.integer,
                                    sample = strsplit(f,split='_CH')[[1]][2] %>%
                                      substr(3,4),
                                    treatment = strsplit(f,split='_CH')[[1]][2]%>%
                                      substr(6,nchar(strsplit(f,split='_CH')[[1]][2])-10),
                                    joint_channel = paste(channel, sample, treatment, sep = ", ")
                             )
                         }) %>%
    
    bind_rows() %>%
    
    filter(!is.na(X) & !is.na(Y)) 
  
    return(dat_shape)

}
 

#' Graphing area over time, split by each Track.
#' 
#' This function takes in data for only one channel and returns track-level plots
#' @param dat_sub data.frame processed by preprocess()
#' @param join_channel_select string of the joint_channel label
#' @return outputs pdf containing line plot of area over time for each track
#' @example graph_area_byTrack(preprocess(20180215),'CH4, RT, Basal')
graph_area_byTrack <- function(dat_sub, joint_channel_select){
  # filter only to the selected channel
  dat_sub <- dat_sub %>% filter(joint_channel == joint_channel_select)
  pdf(file = paste0('plots/',tools::file_path_sans_ext(dat_sub$f[1]),'_areaTracks.pdf'))
  for (t in unique(dat_sub$Track)){
    dat_sub_t <-  filter(dat_sub,Track == t)
    p <- ggplot(dat_sub_t, mapping = aes(Frame,Area)) + geom_line() + ggtitle(paste("Track",t))
    print(p)
  }
  dev.off()
  
}

#' Graphing perimeter over time, split by each Track
#' 
#' This function takes in data for only one channel and returns track-level plots.
#' @param dat_sub data.frame processed by preprocess()
#' @param join_channel_select string of the joint_channel label
#' @return outputs pdf containing line plot of perimeter over time for each track
#' @example graph_perim_byTrack(preprocess(20180215),'CH4, RT, Basal')
graph_perim_byTrack <- function(dat_sub, joint_channel_select){
  # filter only to the selected channel
  dat_sub <- dat_sub %>% filter(joint_channel == joint_channel_select)
  pdf(file = paste0('plots/',tools::file_path_sans_ext(dat_sub$f[1]),'_perimTracks.pdf'))
  for (t in unique(dat_sub$Track)){
    dat_sub_t <-  filter(dat_sub,Track == t)
    p <- ggplot(dat_sub_t, mapping = aes(Frame,Perimeter)) + geom_line() + ggtitle(paste("Track",t))
    print(p)
  }
  dev.off()
  
}

#' Channel-level plot creation from shape data
#' 
#' Filters out outliers of greater than 3rd quartile + 1.5(IQR) and then makes plots from that data
#' Still need to work out some issues with the outliers -- see notes at the end of the script
#' @param dat_sub data.frame of data processed by preprocess()
#' @return data frame containing graphs of area and perimeter over time and area and perimeter distribution by channel
#' @example exp_summ <- channel_shape(preprocess(20171106))
channel_shape <- function(dat_sub){

  # Group data by track so as to look at trends in each individual track
  dat_byTrack <- dat_sub %>% group_by(f, channel, sample, treatment, Track) %>%
    summarize(
      frames = list(Frame),
      area = list(Area),
      perimeter = list(Perimeter)
    ) %>%
    ungroup() %>%
    mutate(
      joint_channel = paste(channel, sample, treatment, sep = ", "),
      avg_area = sapply(area, function(x) mean(x)),
      fq_area = sapply(area, function(x) quantile(x,0.25)),
      tq_area = sapply(area, function(x) quantile(x,0.75)),
      up_bound_area = sapply(area, function(x) quantile(x,0.75) + 1.5*(quantile(x,0.75)-quantile(x,0.25))),
      avg_perim = sapply(perimeter, function(x) mean(x)),
      fq_perim = sapply(perimeter, function(x) quantile(x,0.25)),
      tq_perim = sapply(perimeter, function(x) quantile(x,0.75)),
      up_bound_perim = sapply(perimeter, function(x) quantile(x,0.75) + 1.5*(quantile(x,0.75)-quantile(x,0.25)))
    )
  
  # Filter out outliers for each track
  # I'm not sure of a more effective way to do this at present
  outliers <- select(dat_byTrack,joint_channel,Track,up_bound_perim,up_bound_area)
  new_dat_sub <- data.frame()
  for (t in unique(dat_sub$Track)){
    up_bound_perim_t <- outliers %>% filter(Track == t) %>% select(up_bound_perim)
    up_bound_area_t <- outliers %>% filter(Track == t) %>% select(up_bound_area)
    dat_sub_t <- filter(dat_sub, Track == t & Perimeter < up_bound_perim_t[[1]][1] & Area < up_bound_area_t[[1]][1])
    new_dat_sub <- rbind(new_dat_sub, dat_sub_t)
  }
  dat_sub <- new_dat_sub
  
  # Group data by frame so as to look at trends over time
  dat_byFrame <- dat_sub %>% group_by(f, channel, sample, treatment, Frame) %>%
    summarize(
      area = list(Area),
      perimeter = list(Perimeter)) %>%
    ungroup() %>%
    mutate(
      joint_channel = paste(channel, sample, treatment, sep = ", "),
      avg_area = sapply(area, function(x) mean(x)),
      fq_area = sapply(area, function(x) quantile(x,0.25)),
      tq_area = sapply(area, function(x) quantile(x,0.75)),
      avg_perim = sapply(perimeter, function(x) mean(x)),
      fq_perim = sapply(perimeter, function(x) quantile(x,0.25)),
      tq_perim = sapply(perimeter, function(x) quantile(x,0.75))
    )
  
  exp_summ <- list()

  # Average perimeter per channel
  exp_summ$perim_plot <- ggplot(dat_byFrame, mapping = aes(Frame,avg_perim,group=joint_channel,color=factor(joint_channel)))+
    geom_line() + 
    facet_wrap(~joint_channel) + 
    ylab("Average Perimeter")
  
  # Average area per channel
  exp_summ$area_plot <- ggplot(dat_byFrame, mapping = aes(Frame,avg_area,group=joint_channel,color=factor(joint_channel)))+
    geom_line() + 
    facet_wrap(~joint_channel) + 
    ylab("Average Area")
  
  # Average perimeter per channel, with perimeter of each track at each frame overlaid in points
  exp_summ$perim_plot_points <- ggplot(NULL) +
    geom_point(data=dat_sub, mapping = aes(Frame,Perimeter,group=Track),alpha=0.1,size = 0.3)+
    geom_line(data=dat_byFrame, mapping = aes(Frame,avg_perim,group=joint_channel,color=factor(joint_channel))) + 
    facet_wrap(~joint_channel) + 
    ylab("Average Perimeter")
  
  # Average area per channel, with perimeter of each track at each frame overlaid in points
  exp_summ$area_plot_points <- ggplot(NULL) +
    geom_point(data=dat_sub, mapping = aes(Frame,Area,group=Track),alpha=0.1,size = 0.3)+
    geom_line(data=dat_byFrame, mapping = aes(Frame,avg_area,group=joint_channel,color=factor(joint_channel))) + 
    facet_wrap(~joint_channel) + 
    ylab("Average Area")
  
  # Average perimeter per channel, with perimeter of each track at each frame overlaid in points
  # SMOOTHED 
  exp_summ$perim_smooth_plot <- ggplot(NULL) +
    geom_point(data=dat_sub, mapping = aes(Frame,Perimeter,group=Track),alpha=0.1,size = 0.3)+
    geom_smooth(data=dat_byFrame, mapping = aes(Frame,avg_perim,group=joint_channel)) + 
    ylab("Average Perimeter") +
    facet_wrap(~joint_channel)
  
  # Average area per channel, with perimeter of each track at each frame overlaid in points
  # SMOOTHED 
  exp_summ$area_smooth_plot <- ggplot(NULL) +
    geom_point(data=dat_sub, mapping = aes(Frame,Area,group=Track),alpha=0.1,size = 0.3)+
    geom_smooth(data=dat_byFrame, mapping = aes(Frame,avg_area,group=joint_channel)) + 
    ylab("Average Area") +
    facet_wrap(~joint_channel)
  
  # Average perimeter per channel, with IQR
  # strategy: add first and third quartile to dat_byFrame and plot those as light lines, fill between
  exp_summ$perim_outlined_plot <- ggplot(dat_byFrame)+
    geom_line(mapping = aes(Frame,fq_perim,group=joint_channel),alpha=0.15) + 
    geom_line(mapping = aes(Frame,tq_perim,group=joint_channel),alpha=0.15) + 
    # not sure what method I should use for smoothing, definitely not linear
    geom_smooth(mapping = aes(Frame,avg_perim,group=joint_channel), method="gam") + 
    facet_wrap(~joint_channel) + 
    ylab("Average Perimeter")

  # Average area per channel, with IQR
  # strategy: add first and third quartile to dat_byFrame and plot those as light lines, fill between
  exp_summ$area_outlined_plot <- ggplot(dat_byFrame)+
    geom_line(mapping = aes(Frame,fq_area,group=joint_channel),alpha=0.15) + 
    geom_line(mapping = aes(Frame,tq_area,group=joint_channel),alpha=0.15) + 
    # not sure what method I should use for smoothing, definitely not linear
    geom_smooth(mapping = aes(Frame,avg_area,group=joint_channel), method="gam") + 
    facet_wrap(~joint_channel) + 
    ylab("Average Area")
  
  # Cell area violin plot -- cell area over all time points per channel
  exp_summ$area_violin <- ggplot(dat_sub) + 
    geom_violin(mapping=aes(joint_channel,Area,group=joint_channel),width=0.5,scale="width") +
    geom_boxplot(mapping=aes(joint_channel,Area,group=joint_channel),width=0.1) +
    geom_jitter(mapping=aes(joint_channel,Area,group=joint_channel),width=0.4,size=1,alpha=0.2)
  
  # Cell perimeter violin plot -- cell perimeter over all time points per channel  
  exp_summ$perim_violin <- ggplot(dat_sub) + 
    geom_violin(mapping=aes(joint_channel,Perimeter,group=joint_channel),width=0.5,scale="width") +
    geom_boxplot(mapping=aes(joint_channel,Perimeter,group=joint_channel),width=0.1) +
    geom_jitter(mapping=aes(joint_channel,Perimeter,group=joint_channel),width=0.4,size=1,alpha=0.2)
    
  
  return(exp_summ)
  
}

## NOTES ON OUTLIERS ##
# Rilyn, 8/12/2022 #
# There are still quite a few outliers that are not caught by the track_automated.R code.
# I have a few theories for this -- first off, the outliers appear to be two-three cells merged together
# Some of the cells that merge are untracked -- because of this, adjusting the threshold in track_automated.R may not catch these outliers
# We can try to weed out outliers after the fact, which is what I've attempted to do here, but some are still coming through with the cutoff of 3rd quartile + 1.5(IQR)
# I don't want to throw out too many data points, so it might be worth comparing both raising the threshold and taking out high outliers
# Somewhere around ~130-150 pixels appears to be the cut-off for one cell but I haven't had time to check this extensively against the raw data
