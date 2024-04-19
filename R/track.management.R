#' Split Tracks
#' 
#' Identify and split tracks that have large periods of missing location data.
#' 
#' @param dat_sub A data frame containing raw tracking data.
#' @param max_gap The maximum number of frames that can be missing before a track is split. Default is 5 frames (2.5 minutes).
#' 
#' @return A data frame with the same columns as `dat_sub`, but with tracks split
#' @export
#' @importFrom dplyr group_by mutate select ungroup n
split_tracks <- function(dat_sub, max_gap = 5)
{
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    Track <- Frame <- experiment <- channel <- counter <- NULL
  
  # return blank data frame if there is nothing to split
  if(nrow(dat_sub) == 0)
    return(dat_sub)
  
  # split tracks that have large gaps in them
  dat_sub %>%
    group_by(experiment, channel, Track) %>%
    
    mutate(diff = c(0, Frame[-1] - Frame[-n()]),
           split = diff > max_gap,
           counter = cumsum(split),
           Track = as.numeric(paste0(Track, '.', counter))) %>%
    
    ungroup() %>%
    
    select(-diff, -split, -counter)
}
