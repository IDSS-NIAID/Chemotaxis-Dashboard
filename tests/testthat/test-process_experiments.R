# Test experiment processing

# Load packages
library(RSQLite)
library(dplyr)


###########################
# Set up test environment #
###########################

root <- suppressWarnings(system('git rev-parse --show-toplevel', intern = TRUE, ignore.stderr = TRUE))

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


#################################
# Check `compare_two_functions` #
#################################

test_that("compare_two_functions returns expected results", {
  
  # check results in the database (need to re-run `root/data-raw/process_19000101_data.R` to update installed data)
  expect_equal(filter(channel_summ, chanID == 1)$dvud_p, 0.66)
  expect_equal(filter(channel_summ, chanID == 4)$dvud_p, 0.19)  
})


###############################
# Check `process_experiments` #
###############################

test_that("process_experiments returns expected results", {
  
  # proportion of cells travled from top shelf to bottom shelf
  expect_equal(channel_summ$prop_finished, c(0, 1, 1, 0, 1, 1))
  
  # chemotactic efficiency
  expect_equal(round(channel_summ$ce_mean, 2), c(0.00, 0.96, 0.96, -0.01, 0.99, 0.99))
  
  # angle of migration
  expect_equal(round(channel_summ$angle_mean, 2), c(45.37, 2.00, 2.10, 42.23, 1.04, 1.00))
  
  # maximum velocity
  expect_equal(round(channel_summ$max_v_mean, 3), c(9.716, 9.113, 9.323, 4.658, 7.261, 7.186))
})


############
# Clean up #
############

dbDisconnect(con)
