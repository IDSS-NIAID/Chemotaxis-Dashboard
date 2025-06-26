# process_19000101_data.R

library(ChemotaxisDashboard)

# process test data
processed_data <- process_experiments(experiment = '19000101',
                                      source_dir = system.file("extdata", package = "ChemotaxisDashboard"),
                                      results_dir = file.path(system('git rev-parse --show-toplevel', intern = TRUE), 'shiny'),
                                      seed = 923847,
                                      ledge_dist = 260,
                                      ledge_upper = 0,
                                      ledge_lower = 1)


#####################################
# data.frames for RSQLite db tables #
#####################################

users <- data.frame(user = "shinyuser", # default user
                    password = "12345")

expSummary <- processed_data$expSummary

expStats <- processed_data$expStats

chanSummary <- processed_data$chanSummary

chanRaw <- processed_data$chanRaw

trackSummary <- processed_data$trackSummary

trackRaw <- processed_data$trackRaw


# export for internal use
usethis::use_data(users,
                  expSummary,
                  expStats,
                  chanSummary,
                  chanRaw,
                  trackSummary,
                  trackRaw,
                  internal = TRUE, overwrite = TRUE)
