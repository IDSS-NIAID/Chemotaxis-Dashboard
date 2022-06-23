install.packages('gganimate')
install.packages('transformr')

library(ggplot2)
library(dplyr)
library(gganimate)
library(gridExtra)

#choosing a file to test the code on
filename = "utils/results_csv/19000101_CH2_nl_fMLF8.csv"
channel = "19000101_CH2_nl_fMLF8"
test_data <- read.delim(filename, sep = "\t")
test_data2 <- filter(test_data, Track < 5)

#plot x and y with color coding by track
ggplot(test_data2, mapping = aes(X, Y, color = Track, group = Track)) + geom_path() + xlim(-0.15,0.15) + ylim(-0.05,1.25)


#the following code creates a set of slides which show the movement of the selected tracks frame-by-frame
dat <- test_data2
#steps is the number of frames we want the loop to walk through. For quicker testing, I have set steps to 20
#steps <- max(dat$Frame) 
steps <- 10

#for ease of viewing, we want a consistent scale between plots, so we set limits on x and y
#I have set xlim and ylim manually, it would be better to set them automatically to match the largest plot
xmin <- -0.15
xmax <- 0.15
ymin <- -0.05
ymax <- 1

#the following code should create a pdf file and plot the desired frame-by-frame plots inside
pdf(file = paste("track_plot_output/",channel,"_plot_steps=",steps,".pdf",sep=""))
for (i in 1:steps){
  temp <- filter(dat, Frame <= i) #makes a subset of the data including only the desired frames
  #plots this subset of the data, color coordinated by track
  p <- ggplot(temp, mapping = aes(X, Y, color = Track, group = Track)) + geom_path()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ ggtitle(paste("Frame",i)) 
  print(p)
  #ggsave(paste("track_plot_output/testingPlots_",i,".png",sep=""),p)
  #at the end of the loop, we should have slides that show the movement of the cells frame by frame
}
dev.off()

#trying to save the plots into one pdf without using pdf(), using ggsave() instead
#following code doesn't yet work
plot_list <- list()
for (i in 1:steps){
  temp <- filter(dat, Frame <= i) #makes a subset of the data including only the desired frames
  #plots this subset of the data, color coordinated by track
  p <- ggplot(temp, mapping = aes(X, Y, color = Track, group = Track)) + geom_path()+ xlim(xmin,xmax) + ylim(ymin,ymax)+ ggtitle(paste("Frame",i)) 
  append(plot_list, p)
  #at the end of the loop, we should have slides that show the movement of the cells frame by frame
}

ggsave(filename = paste("track_plot_output/",channel,"_plot_steps=",steps,".pdf",sep=""),
       plot = marrangeGrob(plot_list, nrow = 1, ncol = 1),
       width = 15, height = 9)



#in this next bit of code, we will create a gif which shows the movement of the different tracks over time
#following code doesn't yet work
p <- ggplot(dat, aes(X, Y, color = Track, group = Track)) + geom_path() + xlim(xmin,xmax) + ylim(ymin,ymax)+ ggtitle(paste("Frame",i)) 
print(p)
p + transition_time(Frame) + labs(title = 'Frame: {frame_time}', x = 'X position', y = 'Y position')  + ease_aes('linear')

