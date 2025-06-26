# Test experiment processing

# Load packages
library(RSQLite)
library(dplyr)


###########################
# Set up test environment #
###########################

root <- here::here()

# if running in a temporary path, `root` will be empty -> stick the db in the current working directory
if(length(root) != 1)
{
  db_path <- 'chemo-dash.sqlite'
}else{
  db_path <- file.path(root, 'shiny', 'chemo-dash.sqlite')
}

# check if a database has been set up with test data
if(!file.exists(db_path))
{
  dbinit(db_path)
}

# connect to database
con <- dbConnect(SQLite(), db_path)

# load test data
channel_summ <- dbGetQuery(con, "SELECT * FROM chanSummary WHERE expID = '19000101'")


###############################
# Check `process_experiments` #
###############################

test_that("process_experiments returns expected results", {
  
  # proportion of cells traveled from top shelf to bottom shelf
  expect_equal(channel_summ$prop_finished, c(0, 1, 1, 0, 1, 1))
  
  # chemotactic efficiency
  expect_equal(round(channel_summ$ce_mean, 2), c(0.05, 0.95, 0.95, 0.06, 0.99, 0.99))
  
  # angle of migration
  expect_equal(round(channel_summ$angle_mean, 2), c(59.53, 2.25, 2.24, 57.97, 1.08, 1.08))
  
  # maximum velocity
  expect_equal(round(channel_summ$max_v_mean, 3), c(11.593, 9.131, 9.089, 5.760, 7.269, 7.163))
})


#######################################
# Check that the data haven't changed #
#######################################

processed_data <- process_experiments(experiment = '19000101',
                                      source_dir = system.file("extdata", package = "ChemotaxisDashboard"),
                                      results_dir = file.path(root, 'shiny'),
                                      seed = 923847,
                                      ledge_dist = 260,
                                      ledge_upper = 0,
                                      ledge_lower = 1)

if(length(processed_data) > 0)
{
  test_that("processed data hasn't changed", {
  
    # proportion of cells travled from top shelf to bottom shelf
    expect_equal(processed_data$chanSummary$tot_finished, channel_summ$tot_finished)
  
    # chemotactic efficiency
    expect_equal(round(processed_data$chanSummary$ce_mean, 2), round(channel_summ$ce_mean, 2))
  })
}

############
# Clean up #
############

dbDisconnect(con)
