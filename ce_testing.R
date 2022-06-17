#I was a little overwhelmed by trying to incorporate this function into the existing structure
#so I wrote a separate file to calculate the chemotactic efficiency for each track
#as of right now, it is very clunky and probably overly complicated, but I think it does what it is supposed to

library(dplyr)
#reading in a dataset to test the code on
#filename = "utils/results_csv/19000101_CH1_nl_Buffer.csv"
filename = "utils/results_csv/19000101_CH2_nl_fMLF8.csv"
test_data <- read.delim(filename, sep = "\t")

#finds max and min y for each track
maxAndMin <- test_data %>% group_by(Track) %>%
  summarise(
    MaxYByTrack = max(Y, na.rm = T),
    MinYByTrack = min(Y, na.rm = T)
  ) %>%
  arrange(Track)

#after finding the max and min Y position on each track, we bind the difference between the max and min as a new column
maxAndMinDiff <- cbind(maxAndMin, maxAndMin$MaxYByTrack-maxAndMin$MinYByTrack)
names(maxAndMinDiff) <- append(names(maxAndMin),"Diff") #name the new column of difference "Diff"

#the for loop that follows calculates the total distance traveled by each track and appends it to totalDistancesVector
#the end result of the loop is a vector where every entry corresponds to the total distance traveled by the Track with the same index
#I know for loops are very bad for computational efficiency but I couldn't immediately think of a better way to do it. We can hopefully change it to be more efficient

#initialize empty vector
totalDistancesVector = c()
#since the tracks are labeled in numerical order, the maximum track number gives the number of times the loop must cycle (one cycle per track)
for (i in 1:max(test_data$Track)){
  testing <- filter(test_data, Track == i) #filter to just the data for the selected track
  currentX <- testing$X #extract the x positions for the track
  diffX = (currentX[-1]-currentX[-length(currentX)]) #takes the distance between consecutive x positions
  
  #the process is repeated for the y positions
  currentY <- testing$Y
  diffY = (currentY[-1]-currentY[-length(currentY)])

  #the total distance traveled by each track is found by the formula below.
  #after the total distance is calculated, it is appended to the totalDistancesVector
  totalDistance = sum(sqrt(diffX^2+diffY^2))
  totalDistancesVector <- append(totalDistancesVector,totalDistance, i)
}

#the chemotactic efficiency is found by dividing the difference between the max y and min y for each track (maxAndMinDiff$Diff)
#by the total distance traveled (totalDistancesVector) for each track
#ce_vector contains the chemotactic efficiency for each track
ce_vector = maxAndMinDiff$Diff / totalDistancesVector
print(ce_vector)

mean(ce_vector)
