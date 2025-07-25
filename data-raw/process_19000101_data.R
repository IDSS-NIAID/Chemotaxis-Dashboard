# process_19000101_data.R

library(ChemotaxisDashboard)

# process test data
processed_data <- process_experiments(experiment = '19000101',
                                      source_dir = system.file("extdata", package = "ChemotaxisDashboard"),
                                      results_dir = file.path(here::here(), 'shiny'),
                                      ledge_dist = 260,
                                      ledge_upper = 0,
                                      ledge_lower = 1)


#####################################
# data.frames for RSQLite db tables #
#####################################

chanSummary <- processed_data$chanSummary

trackRaw <- processed_data$trackRaw


# export for internal use
usethis::use_data(chanSummary,
                  trackRaw,
                  internal = TRUE, overwrite = TRUE)
