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
#the following variables must be set by the user

#choosing a file to test the code on
#filename = "utils/results_csv/19000101_CH2_nl_fMLF8.csv"
#channel = "19000101_CH2_nl_fMLF8"
filename = "results_csv/20140904279_CH6.csv"
channel = "20140904279_CH6"

#choosing starting frame and number of steps (Frames) to view
#steps is the number of frames we want the loop to walk through. For quicker testing, I have set steps to 20
#steps <- max(dat$Frame) #this will walk through all the frames in the dataset 
steps <- 25
starting_frame <- 1

#choosing number of tracks to view
starting_track <- 1
ending_track <- 8

##################
# Preparing data #
##################

#reading in the data set and editing a copy of it to the user's preferences
test_data <- read.delim(filename, sep = "\t")
test_data2 <- filter(test_data, Track <= ending_track & Track >= starting_track)


#we will pull data from the dataset specified by the above user input
dat <- test_data2
#for ease of viewing, we want a consistent scale between plots, so we set limits on x and y
xmin <- min(dat$X)
xmax <- max(dat$X)
ymin <- min(dat$Y)
ymax <- max(dat$Y)

# generates color function for specific subsets of tracks when needed
myColors <- brewer.pal(9,"Set1") #using a color set from the package
#need to assign the different colors from the set to each level from track variable
names(myColors) <- levels(Tracks)
# color scale variable adds color scheme in ggplot formula using mycolors scheme created
##add colorScale function to any ggplot to customize color
colorScale <- scale_colour_manual(name = "Track",values = myColors) 

####################
# Generating plots #
####################
#the following code creates a set of slides which show the movement of the selected tracks frame-by-frame

# PATH PLOTS: plots the frame-by-frame path of each Track 
# the following code should create a pdf file and plot the desired frame-by-frame plots inside
path_plots <- function(dat, channel, starting_frame, steps){
  pdf(file = paste("track_plot_output/",channel,"_plot_steps=",steps,"_path.pdf",sep=""))
  for (i in starting_frame:steps){
    temp <- filter(dat, Frame <= i) #makes a subset of the data including only the desired frames
    #plots this subset of the data, color coordinated by track
    p <- ggplot(temp, mapping = aes(X, Y, color = factor(Track), group = Track)) + geom_path()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ 
      ggtitle(paste("Frame",i)) 
    print(p)
    #ggsave(paste("track_plot_output/testingPlots_",i,".png",sep=""),p)
    #at the end of the loop, we should have slides that show the movement of the cells frame by frame
  }
  dev.off()
}

# POINT PLOTS: plots only the position of each cell at each time point
point_plots <- function(dat, channel, starting_frame, steps){
  pdf(file = paste("track_plot_output/",channel,"_plot_steps=",steps,"_point.pdf",sep=""))
  for (i in starting_frame:steps){
    temp <- filter(dat, Frame == i) #makes a subset of the data including only the desired frames
    #plots this subset of the data, color coordinated by track
    p <- ggplot(temp, mapping = aes(X, Y, color = factor(Track), group = Track)) + geom_point()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ ggtitle(paste("Frame",i)) 
    print(p)
    #ggsave(paste("track_plot_output/testingPlots_",i,".png",sep=""),p)
    #at the end of the loop, we should have slides that show the movement of the cells frame by frame
  }
  dev.off()
}

#function for highlighting specific track

#need to select a specific track to highlight assign num to track 56
num<- 56
#easier to type Track 
Tracks<- test_data$Track

#Creating conditional inputs based on track selection and color selection
##These inputs will be used iin the highlighting ggplot function
track_select<- if_else(Tracks== num, 'red',"black")
#another conditional input used for opacity
track_select_op<- if_else(Tracks== num, 2,.20)

#ggplot function creating a point graphs with specific color and opacity based
##on the track chosen to be highlighted
highlight <- ggplot(test_data, mapping = 
                      aes(X,Y, color=(Track), group=Track)) + 
  geom_point(color= track_select, alpha=track_select_op)

#calling function test if it works
highlight



###########
# Testing #
###########

point_plots(dat,channel,starting_frame,steps)
path_plots(dat,channel,starting_frame,steps)
