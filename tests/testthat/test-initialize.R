# test initialization functions

test_that("dbinit works", {
  
  # try initializing the database in the current working directory
  expect_no_error(dbinit('chemo-dash.sqlite'))
  
  # try reading from the database
  con <- dbConnect(RSQLite::SQLite(), 'chemo-dash.sqlite')

  expect_equal(dbGetQuery(con, "SELECT user FROM access WHERE expID = '19000101'")[[1]], 'shinyuser')
  
  dbDisconnect(con)
})


test_that("get_test_data works", {
  
  expect_type(get_test_data(), 'list')
  
  expect_true(is.data.frame(get_test_data()$expSummary))
  
  expect_equal(names(get_test_data()), 
               c('users', 'access', 'expSummary', 'expStats', 'chanSummary', 'chanRaw', 'trackSummary', 'trackRaw'))
})
  
# clean up (do a quick sanity check first)
if(as.Date(file.info('chemo-dash.sqlite')$ctime) == as.Date(Sys.time()))
  file.remove('chemo-dash.sqlite')
