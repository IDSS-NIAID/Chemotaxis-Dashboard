##########
# Set-up #
##########

#Make sure these packages are installed before running the code
install.packages('gganimate')
install.packages('transformr')
install.packages('ggplot2')

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
  #starting_frame: frame to start the plot at
  #steps: how many frames to view
  #can also subset which Tracks to view using starting_track and ending_track
  #user will have the option to view all frames and all tracks instead of choosing a subset

##################
# Preparing data #
##################

# generates color function for specific subsets of tracks when needed
myColors <- brewer.pal(9,"Set1") #using a color set from the package
#need to assign the different colors from the set to each level from track variable
names(myColors) <- levels(Tracks)
# color scale variable adds color scheme in ggplot formula using mycolors scheme created
##add colorScale function to any ggplot to customize color
colorScale <- scale_colour_manual(name = "Track",values = myColors) 

##############################
# Generating plots functions #
##############################
#the following code creates a set of slides which show the movement of the selected tracks frame-by-frame

# PATH PLOTS: plots the frame-by-frame path of each Track 
# the following code should create a pdf file and plot the desired frame-by-frame plots inside
path_plots <- function(dat, labelName, starting_frame, steps){
  pdf(file = paste("track_plot_output/",labelName,"_path_plot.pdf",sep=""))
  for (i in starting_frame:steps){
    temp <- filter(dat, Frame <= i) #makes a subset of the data including only the desired frames
    #plots this subset of the data, color coordinated by track
    p <- ggplot(temp, mapping = aes(X, Y, color = factor(Track), group = Track)) + geom_path()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ 
      ggtitle(paste("Frame",i)) 
    print(p)
    #at the end of the loop, we should have slides that show the movement of the cells frame by frame
  }
  dev.off()
}

# POINT PLOTS: plots only the position of each cell at each time point
point_plots <- function(dat, labelName, starting_frame, steps, size_input){
  pdf(file = paste("track_plot_output/",labelName,"_point_plot.pdf",sep=""))
  for (i in starting_frame:steps){
    temp <- filter(dat, Frame == i) #makes a subset of the data including only the desired frames
    #plots this subset of the data, color coordinated by track
    p <- ggplot(temp, mapping = aes(X, Y, color = factor(Track), group = Track)) + geom_point(size = size_input)+ xlim(xmin,xmax) + ylim(ymin,ymax)+ ggtitle(paste("Frame",i)) 
    print(p)
    #at the end of the loop, we should have slides that show the movement of the cells frame by frame
  }
  dev.off()
}

# HIGHLIGHT: highlights a specific track in the plot
highlight <- function(dat,num){
  Tracks<- dat$Track #easier to type Track 
  #Creating conditional inputs based on track selection and color selection
  ##These inputs will be used in the highlighting ggplot function
  track_select<- if_else(Tracks== num, 'red',"black")
  #another conditional input used for opacity
  track_select_op<- if_else(Tracks== num, 2,.20)
  
  #ggplot function creating a point graphs with specific color and opacity based
  ##on the track chosen to be highlighted
  highlight <- ggplot(dat, mapping = 
                        aes(X,Y, color=(Track), group=Track)) + 
    geom_path(color= track_select, alpha=track_select_op)+ xlim(xmin,xmax) + ylim(ymin,ymax)
  print(highlight)
}

highlight_plots <- function(dat,labelName,starting_frame,steps,num){
  pdf(file = paste("track_plot_output/",labelName,"_highlight_plot_", num, ".pdf",sep=""))
  for (i in starting_frame:steps){
    temp <- filter(dat, Frame <= i) #makes a subset of the data including only the desired frames
    #plots this subset of the data, color coordinated by track
    highlight(temp,num)
    #at the end of the loop, we should have slides that show the movement of the cells frame by frame
  }
  dev.off()
}

# COLORPLOT: highlights each completed track one at a time
# produces a pdf with one page per track, with the particular track highlighted
# useful for comparing tracks to the raw data
colorplot <- function(dat,labelName){
  pdf(file = paste("track_plot_output/",labelName,"_colorplot_path.pdf",sep=""))
  for (i in starting_track:ending_track){
    #plots this subset of the data, color coordinated by track
    p <- ggplot(dat, mapping = aes(X, Y, color = ifelse(Track == i,"red","blue"), group = Track)) + geom_path()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ ggtitle(paste("Track",i)) 
    print(p)
  }
  dev.off()
}

###########
# Testing #
###########
#choosing a file to test the code on
filename <- "results_csv/20140904279_CH6.csv"
labelName <- tools::file_path_sans_ext(basename(filename))

#reading in the data set 
test_data <- read.delim(filename, sep = "\t")

#choosing number of tracks to view
starting_track <- 1
ending_track <- max(test_data$Track)

#editing a copy of the dataset to the user's preferences
dat <- filter(test_data, Track <= ending_track & Track >= starting_track)

#choosing starting frame and number of steps (Frames) to view
#steps is the number of frames we want the loop to walk through. 
steps <- max(dat$Frame) #this will walk through all the frames in the dataset 
starting_frame <- 1

#we will pull data from the dataset specified by the above user input
#for ease of viewing, we want a consistent scale between plots, so we set limits on x and y
xmin <- min(dat$X)
xmax <- max(dat$X)
ymin <- min(dat$Y)
ymax <- max(dat$Y)

#choose track to highlight
num <- 15

#function calls
point_plots(dat,labelName,starting_frame,steps)
path_plots(dat,labelName,starting_frame,steps)
highlight(dat,num)
highlight_plots(dat,labelName,starting_frame,steps,num)


