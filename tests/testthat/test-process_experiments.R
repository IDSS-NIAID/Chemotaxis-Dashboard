# Test experiment processing

# Load packages
library(RSQLite)
library(dplyr)


###########################
# Set up test environment #
###########################

root <- here::here()
db_path <- file.path(root, 'shiny', 'chemo-dash.sqlite')

# if running in a temporary path, `shiny` will not exist -> make sure it does
if(!dir.exists('shiny'))
  dir.create('shiny')

# check if a database has been set up with test data
if(!file.exists(db_path))
  dbinit(db_path)

# connect to database
con <- dbConnect(SQLite(), db_path)

# load test data
channel_summ <- dbGetQuery(con, "SELECT * FROM chanSummary WHERE expID = '19000101'")


#######################################
# Check that the data haven't changed #
#######################################

processed_data <- process_experiments(experiment = '19000101',
                                      source_dir = system.file("extdata", package = "ChemotaxisDashboard"),
                                      results_dir = file.path(root, 'shiny'),
                                      ledge_dist = 260,
                                      ledge_upper = 0,
                                      ledge_lower = 1)

if(length(processed_data) > 0)
{
  test_that("processed data hasn't changed", {
  
    # proportion of cells travled from top shelf to bottom shelf
    expect_equal(processed_data$chanSummary$expID[1], '19000101')
  
    # chemotactic efficiency
    expect_equal(round(processed_data$trackRaw$v_x, 2)[1], 0.04)
  })
}

############
# Clean up #
############

dbDisconnect(con)
