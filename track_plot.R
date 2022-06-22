install.packages("tidyverse")
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library("gridExtra")


filename = "utils/results_csv/19000101_CH2_nl_fMLF8.csv"
test_data <- read.delim(filename, sep = "\t")
test_data2 <- filter(test_data, Track < 5)

#plot x and y with color coding by track
ggplot(test_data2, mapping = aes(X, Y, color = Track)) + geom_point() 

#trying to plot consecutive time points
#for (i in 1:max(test_data$Frame)){
#  temp <- filter(test_data, Frame <= i)
#  ggplot(temp, mapping = aes(X, Y, color = Track)) + geom_point()
  
#}