#make sure to call this before using these functions
library(dplyr)

#############
# Functions #
#############

# CALCULATING MINIMUM DISTANCE
# This function calculates the minimum distance to another track for each track and each frame in the dataset
# It returns a two-dimensional list which stores all the minimum distances
all_min_dist <- function(dat){
  all_dist_list <- list()
  for (i in min(dat$Track):max(dat$Track)){
    min_dist <- c()
    for(j in min(dat$Frame):max(dat$Frame)){
      temp <- filter(dat, Frame == j)
      if(i %in% temp$Track){
        track_i <- temp[temp$Track == i,]
        dist_i <- sqrt((track_i$X - temp$X)^2 + (track_i$Y - temp$Y)^2)
        dist_i <- dist_i[dist_i != 0]
        if (length(dist_i)!= 0){
          min_dist <- append(min_dist,min(dist_i)) }
      }
    }
    all_dist_list[[i]] <- min_dist
  }
  return(all_dist_list)
}

# THRESHOLD BY FRAME
# Returns a list of frames where the selected track's closest neighbor is within the threshold distance
# Can be used to have more flexibility when selecting tracks, can eliminate only bad frames instead of whole track
thresholding_by_frame <- function(listname,threshold,track_num){
  cur_track <- listname[[track_num]]
  bad_frame <- c()
  for (i in 1:length(cur_track)){
    if (cur_track[i] < threshold){
      #if the distance is less than the threshold, appends the Frame to the list of bad frames
      bad_frame <- append(bad_frame, i)
    }
  }
  return(bad_frame)
}

# THRESHOLD BY TRACK
# Returns TRUE if all distances to another track are above the threshold
# Returns false if another track is too close at any point
# Leads to a more conservative sample of non-overlapping tracks
thresholding_by_track <- function(listname,threshold,track_num){
  cur_track <- listname[[track_num]] 
  #if any distance in the list is below the threshold, it returns FALSE
  if(min(cur_track) < threshold){ 
    return(FALSE)
  }
  return(TRUE)
}

# FINDING ALL ELIGIBLE TRACKS
# should return a vector of all eligible tracks based on a threshold
# only returns tracks that are never within a certain distance (determined by threshold) of another cell
# this calls the stricter thresholding_by_track, so if any values are below the threshold, the whole track is thrown out
find_tracks <- function(dat,threshold){
  theList <- all_min_dist(dat) #getting all min distances
  tracks_vec <- c()
  for (i in 1:length(theList)){
    t <- thresholding_by_track(theList,threshold,i)
    if(t){
      tracks_vec <- append(tracks_vec,i) #only appends eligible tracks
    }
  }
  return(tracks_vec)
}

# FINDING ALL INELIGIBLE FRAMES
# returns a list of all ineligible frames for each track based on a threshold
# returns frames where the track gets too close to another track
# gives more flexibility in choosing tracks to examine
find_frames <- function(dat,threshold){
  theList <- all_min_dist(dat) #getting all min distances
  frames_list <- list()
  for (i in 1:length(theList)){
    t <- thresholding_by_frame(theList,threshold,i)
    frames_list[[i]] <- t
  }
  return(frames_list)
}
  
###########
# Testing #
###########

filename = "results_csv/20140904279_CH4.csv"
dat <- read.delim(filename, sep = "\t")
list1 <- all_min_dist(dat)
thresholding_by_frame(list1,0.2,3)
thresholding_by_track(list1,0.1,3)
find_tracks(dat,0.1)
find_frames(dat,0.1)
