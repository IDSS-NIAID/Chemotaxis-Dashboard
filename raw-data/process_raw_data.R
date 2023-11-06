# process_raw_data.R

library(ChemotaxisDashboard)

# process test data
processed_data <- process_experiments('19000101',
                                      source_dir = system.file("extdata", package = "ChemotaxisDashboard"),
                                      results_dir = file.path(system('git rev-parse --show-toplevel', intern = TRUE), '.data'),
                                      seed = 923847)
                                      


#####################################
# data.frames for RSQLite db tables #
#####################################

#' users
#' User table for authentication
#' 
#' @param user Character, username - used in: 
#'   `access`
#' @param password Character, password
users <- data.frame(user = "shinyuser", # default user
                    password = "12345")


#' access
#' User access table defining which experiments the user can access
#' 
#' @param user Character, maps to `users$user`
#' @param expID Character, maps to `expSummary$expID`
access <- data.frame(user = "shinyuser",
                     expID = "19000101") # default user has access to test data only


#' expSummary
#' Experiment summary table
#' 
#' @param expID Character, experiment ID - used in: 
#'   `access`
#'   `expStats`
#'   `chanSummary`
#'   `chanRaw`
#'   `trackSummary`
#'   `trackRaw`
#' @param tracks_time Character, path to figure of tracks over time for each channel
#' @param tracks_v Character, path to figure of velocity over time for each grouping
#' @param angle_migration Character, path to figure of angle of migration viloin plots for each channel
#' @param ce Character, path to figure of chemotactic efficiency violin plots for each channel
expSummary <- processed_data$expSummary


#' expStats
#' Table of summary statistics for each experiment
#' 
#' @param expID Character, maps to `expSummary$expID`
#' @param within Character, group for the comparison (i.e. within normals treated with fMLF8)
#' @param between Character, contrast for the comparison (i.e. between channels 3 and 4)
#' @param test Character, test used to compare the `between` groups
#' @param stat Double, test statistics comparing the `between` groups
#' @param p Double, p-value for `stat`
expStats <- processed_data$expStats


#' chanSummary
#' Channel summary table
#' 
#' @param expID Character, maps to `expSummary$expID`
#' @param chanID Integer, channel ID - used in:
#'   `chanRaw`
#'   `trackRaw`
#'   `trackSummary`
#' @param sID Character, sample ID - used in:
#' @param treatment Character, treatment applied to this channel
#' @param tot_finished Integer, Total number of cells that reached the bottom ledge
#' @param prop_finished Double, proportion of cells that reached the bottom ledge
#' @param ce_median Double, median chemotactic efficiency
#' @param ce_mean Double, mean chemotactic efficiency
#' @param ce_sd Double, standard deviation of chemotactic efficiency
#' @param angle_median Double, median angle of migration
#' @param angle_mean Double, mean angle of migration
#' @param angle_sd Double, standard deviation of angle of migration
#' @param max_v_median Double, median maximum velocity
#' @param max_v_mean Double, mean maximum velocity
#' @param max_v_sd Double, standard deviation of maximum velocity
#' @param dvud Double, dissimilarity score comparing directed and undirected trajectories
#' @param dvud_p Double, p-value for `dvud`
chanSummary <- processed_data$chanSummary


#' chanRaw
#' Table of smoothed trajectories over all tracks in a channel
#' 
#' @param expID Character, maps to `expSummary$expID`
#' @param chanID Integer, maps to `chanSummary$chanID`
#' @param x Double, smoothed x-position for the channel
#' @param y Double, smoothed y-position for the channel
#' @param frames Integer, frame (sampled every 30 seconds)
#' @param v_x Double, velocity in the x direction (undirected)
#' @param v_y Double, velocity in the y direction (directed)
#' @param v Double, total velocity
chanRaw <- processed_data$chanRaw


#' trackSummary
#' Track summary table
#' 
#' @param expID Character, maps to `expSummary$expID`
#' @param chanID Integer, maps to `chanSummary$chanID`
#' @param trackID Integer, track ID - used in:
#'   `trackRaw`
#' @param ce Double, chemotactic efficiency
#' @param angle_migration Double, angle of migration
#' @param max_v Double, maximum velocity in μm per minute
#' @param av_velocity Double, mean velocity in μm per minute
#' @param finished  Logical, TRUE when the cell passed the bottom ledge
trackSummary <- processed_data$trackSummary


#' trackRaw
#' Raw track information
#' 
#' @param expID Character, maps to `expSummary$expID`
#' @param chanID Integer, maps to `chanSummary$chanID`
#' @param trackID Integer, maps to `trackSummary$trackID`
#' @param x Double, x-position for the track
#' @param y Double, y-position for the track
#' @param frame Integer, frame (sampled every 30 seconds)
#' @param v_x Double, velocity in the x direction (undirected)
#' @param v_y Double, velocity in the y direction (directed)
#' @param v Double, total velocity
trackRaw <- processed_data$trackRaw


# export for internal use
usethis::use_data(users,
                  access,
                  expSummary,
                  expStats,
                  chanSummary,
                  chanRaw,
                  trackSummary,
                  trackRaw,
                  internal = TRUE, overwrite = TRUE)
