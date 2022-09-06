#' Test one_experiment.R
#' 
#' In order to test one_experiment.R, we assume the necessary csv files exist. If they don't, we'll need to create that first.

root <- system('git rev-parse --show-toplevel', intern = TRUE)

# create csv files if needed
if(!file.exists(paste0(root, '/utils/results_csv/19000101_CH1_nl_Buffer.csv')))
   source(paste0(root, '/utils/simulated_data.R'))

# run preprocess.R
system(paste0('Rscript ', root, '/utils/preprocess.R experiment=19000101 data_dir=', root, '/tmp/ seed=243678 sig.figs=2'))

# load output from one_experiment
load(paste0(root, '/tmp/19000101.RData'))


#################################
# Check `compare_two_functions` #
#################################

test_that("compare_two_functions returns expected results", {
  expect_equal(channel_summ$directed_v_undirected[[1]]['p'], c(p = 0.65))
  expect_equal(channel_summ$directed_v_undirected[[4]]['p'], c(p = 0.16))
})


##########################
# Check `one_experiment` #
##########################

test_that("one_experiment returns expected results", {
  # proportion of cells travled from top shelf to bottom shelf
  expect_equal(channel_summ$prop_finished, c(0, 1, 1, 0, 1, 1))
  
  # chemotactic efficiency
  expect_equal(round(channel_summ$ce_mean, 2), c(0.00, 0.96, 0.96, -0.01, 0.99, 0.99))
  
  # angle of migration
  expect_equal(round(channel_summ$angle_mean, 2), c(45.37, 2.00, 2.10, 42.23, 1.04, 1.00))
  
  # maximum velocity
  expect_equal(round(channel_summ$max_v_mean, 3), c(0.019, 0.018, 0.018, 0.009, 0.014, 0.014))
})