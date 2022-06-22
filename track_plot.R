library(ggplot2)
library(dplyr)

#choosing a file to test the code on
filename = "utils/results_csv/19000101_CH2_nl_fMLF8.csv"
test_data <- read.delim(filename, sep = "\t")
test_data2 <- filter(test_data, Track < 5)

#plot x and y with color coding by track
ggplot(test_data2, mapping = aes(X, Y, color = Track, group = Track)) + geom_path() + xlim(-0.15,0.15) + ylim(-0.05,1.25)


#the following code creates a set of slides which show the movement of the selected tracks frame-by-frame
dat <- test_data2
#steps is the number of frames we want the loop to walk through. For quicker testing, I have set steps to 20
#steps <- max(dat$Frame) 
steps <- 20

#for ease of viewing, we want a consistent scale between plots, so we set limits on x and y
#I have set xlim and ylim manually, it would be better to set them automatically to match the largest plot
xmin <- -0.05
xmax <- 0.05
ymin <- -0.05
ymax <- 0.4
#the for loop is working in reverse to make the output easier to view. user clicks backwards through the slides
for (i in steps:1){
  temp <- filter(dat, Frame <= i) #makes a subset of the data including only the desired frames
  #plots this subset of the data, color coordinated by track
  print(ggplot(temp, mapping = aes(X, Y, color = Track, group = Track)) + geom_path()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ ggtitle(paste("Frame",i))) 
  #at the end of the loop, we should have slides that show the movement of the cells frame by frame
}