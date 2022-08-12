##########
# Set-up #
##########

#Make sure these packages are installed before running the code
#install.packages('gganimate')
#install.packages('transformr')
#install.packages('ggplot2')

#load in these packages
library(ggplot2)
library(dplyr)
library(gganimate)
library(gridExtra)
library(RColorBrewer)

##############
# User Input #
##############
#the following variables must be set by the user: 
  #filename: name of the file to pull the data from 
  #delim: whether the selected data file is comma or tab delimited

##################
# Preparing data #
##################

# generates color function for specific subsets of tracks when needed
#myColors <- brewer.pal(9,"Set1") #using a color set from the package
#need to assign the different colors from the set to each level from track variable
#names(myColors) <- levels(Tracks)
# color scale variable adds color scheme in ggplot formula using mycolors scheme created
##add colorScale function to any ggplot to customize color
#colorScale <- scale_colour_manual(name = "Track",values = myColors) 

##############################
# Generating plots functions #
##############################
#the following code creates a set of slides which show the movement of the selected tracks frame-by-frame

#' PATH PLOTS: plots the frame-by-frame path of each Track 
#' 
#' This function should create a pdf file and plot the desired frame-by-frame path plots inside
#' @param filename the full path to the file from the working directory
#' @param delim either ',' or '\t', depending on how the data file is delimited. for results_csv files delim = '\t' and for trackResults files delim =','
#' @return outputs a pdf of frame-by-frame paths for each Track in the data file
#' @example path_plots('results_csv/20140904279_CH6.csv',delim='\t')
#' @example path_plots('trackResults/20180215_CH5_RT_fMLF/20180215_CH5_RT_fMLF.csv', delim=',')
path_plots <- function(filename, delim){
  labelName <- tools::file_path_sans_ext(basename(filename))
  dat <- read.delim(filename, sep = delim)
  #for ease of viewing, we want a consistent scale between plots, so we set limits on x and y
  xmin <- min(dat$X)
  xmax <- max(dat$X)
  ymin <- min(dat$Y)
  ymax <- max(dat$Y)
  pdf(file = paste("track_plot_output/",labelName,"_path_plot.pdf",sep=""))
  for (i in 1:max(unique(dat$Frame))){
    temp <- filter(dat, Frame <= i) #makes a subset of the data including only the desired frames
    #plots this subset of the data, color coordinated by track
    p <- ggplot(temp, mapping = aes(X, Y, color = factor(Track), group = Track)) + geom_path()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ 
      ggtitle(paste("Frame",i)) + theme(legend.key.size = unit(0.25, 'cm'), legend.key.width = unit(0.05,'cm'),legend.key.height = unit(0.1,'cm')) 
    print(p)
    #at the end of the loop, we should have slides that show the movement of the cells frame by frame
  }
  dev.off()
}

#' POINT PLOTS: plots only the position of each cell at each time point
#' 
#' This function should create a pdf file and plot the desired frame-by-frame point plots inside
#' @param filename the full path to the file from the working directory
#' @param delim either ',' or '\t', depending on how the data file is delimited. for results_csv files delim = '\t' and for trackResults files delim =','
#' @return outputs a pdf of frame-by-frame paths for each Track in the data file
#' @example point_plots('results_csv/20140904279_CH6.csv',delim='\t')
#' @example point_plots('trackResults/20180215_CH5_RT_fMLF/20180215_CH5_RT_fMLF.csv', delim=',')
point_plots <- function(filename,delim){
  labelName <- tools::file_path_sans_ext(basename(filename))
  dat <- read.delim(filename, sep = delim)
  #for ease of viewing, we want a consistent scale between plots, so we set limits on x and y
  xmin <- min(dat$X)
  xmax <- max(dat$X)
  ymin <- min(dat$Y)
  ymax <- max(dat$Y)
  pdf(file = paste("track_plot_output/",labelName,"_point_plot.pdf",sep=""))
  for (i in 1:max(unique(dat$Frame))){
    temp <- filter(dat, Frame == i) #makes a subset of the data including only the desired frames
    #plots this subset of the data, color coordinated by track
    p <- ggplot(temp, mapping = aes(X, Y, color = factor(Track), group = Track)) + geom_point()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ ggtitle(paste("Frame",i)) + 
      theme(legend.key.size = unit(0.25, 'cm'), legend.key.width = unit(0.05,'cm'),legend.key.height = unit(0.1,'cm')) 
    print(p)
    #at the end of the loop, we should have slides that show the movement of the cells frame by frame
  }
  dev.off()
}

#' HIGHLIGHT: highlights a specific track in the plot
#' 
#' @param dat the data.frame where the position over time data is stored. Should have Track, Frame, X, and Y columns
#' @param track_num the selected track
#' @return outputs a single plot with the selected track highlighted
highlight <- function(dat,track_num){
  Tracks<- dat$Track #easier to type Track 
  #Creating conditional inputs based on track selection and color selection
  ##These inputs will be used in the highlighting ggplot function
  track_select<- if_else(Tracks== track_num, 'red',"black")
  #another conditional input used for opacity
  track_select_op<- if_else(Tracks== track_num, 2,.20)
  #ggplot function creating a point graphs with specific color and opacity based
  ##on the track chosen to be highlighted
  highlight <- ggplot(dat, mapping = 
                        aes(X,Y, color=(Track), group=Track)) + 
    geom_path(color= track_select, alpha=track_select_op)
  return(highlight)
}

#' HIGHLIGHT PLOTS: highlights a specific track in the frame-by-frame plots
#'
#' @param filename the full path to the file from the working directory
#' @param delim either ',' or '\t', depending on how the data file is delimited. for results_csv files delim = '\t' and for trackResults files delim =','
#' @param track_num the selected track to be highlighted
#' @return outputs a pdf file containing frame-by-frame plots
#' @example highlight_plots('trackResults/20180215_CH5_RT_fMLF/20180215_CH5_RT_fMLF.csv', delim=',',track_num=4)
highlight_plots <- function(filename, delim, track_num){
  labelName <- tools::file_path_sans_ext(basename(filename))
  #reading in the data set 
  dat <- read.delim(filename, sep = delim)
  pdf(file = paste("track_plot_output/",labelName,"_highlight_plot_", track_num, ".pdf",sep=""))
  xmin <- min(dat$X)
  xmax <- max(dat$X)
  ymin <- min(dat$Y)
  ymax <- max(dat$Y)
  for (i in 1:max(unique(dat$Frame))){
    temp <- filter(dat, Frame <= i) #makes a subset of the data including only the desired frames
    #plots this subset of the data, color coordinated by track
    print(highlight(temp,track_num) + xlim(xmin,xmax) + ylim(ymin,ymax))
    #at the end of the loop, we should have slides that show the movement of the cells frame by frame
  }
  dev.off()
}

#' COLORPLOT: highlights each completed track one at a time
#' 
#' This function produces a pdf with one page per track, with the particular track highlighted.
#' It can be useful for comparing tracks to the raw data
#' @param filename the full path to the file from the working directory
#' @param delim either ',' or '\t', depending on how the data file is delimited. for results_csv files delim = '\t' and for trackResults files delim =','
#' @return outputs a pdf file containing the completed tracks, with each page highlighting a different completed track
#' @example colorplot('trackResults/20171107_CH2_NL_fMLF/20171107_CH2_NL_fMLF.csv',delim=',')
colorplot <- function(filename,delim){
  labelName <- tools::file_path_sans_ext(basename(filename))
  #reading in the data set 
  dat <- read.delim(filename, sep = delim)
  xmin <- min(dat$X)
  xmax <- max(dat$X)
  ymin <- min(dat$Y)
  ymax <- max(dat$Y)
  pdf(file = paste("track_plot_output/",labelName,"_colorplot_path.pdf",sep=""))
  for (i in min(dat$Track):max(dat$Track)){
    #plots this subset of the data, color coordinated by track
    p <- ggplot(dat, mapping = aes(X, Y, color = ifelse(Track == i,"red","blue"), group = Track)) + geom_path()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ ggtitle(paste("Track",i)) 
    print(p)
  }
  dev.off()
}

