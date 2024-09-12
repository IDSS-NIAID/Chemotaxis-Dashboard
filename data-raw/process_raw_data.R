# process_raw_data.R

library(ChemotaxisDashboard)

library(stringr)
library(purrr)

library(DBI)
library(RSQLite)


# root directory of the repo
root <- system('git rev-parse --show-toplevel', intern = TRUE)
dat_path <- file.path(root, 'shiny')
db_path <- file.path(dat_path, 'chemo-dash.sqlite')


# check what has been done already
if(file.exists(db_path) & file.size(db_path) > 0)
{
  con <- dbConnect(SQLite(), db_path)
  done <- dbGetQuery(con, 'SELECT DISTINCT expID FROM expSummary')$expID
}else{
  dbinit(db_path)
  done <- character()
}


# get all experiments
all_experiments <- list.files(file.path(root, 'data-raw', 'results_csv')) |>
  grep(pattern = '19000101', value = TRUE, invert = TRUE, fixed = TRUE) |>
  str_split(pattern = '_') |>
  sapply(`[`, 1) |>
  str_replace('-$', '') |>
  unique()
all_experiments <- all_experiments[!all_experiments %in% done]


# process experiments that haven't been processed yet
for(i in 1:length(all_experiments))
{
  processed_data <- process_experiments(experiment = all_experiments[i],
                                        source_dir = file.path(root, 'data-raw', 'results_csv'),
                                        results_dir = file.path(root, 'shiny'),
                                        seed = 923847)
  
  # add access to processed data
  processed_data$access <- data.frame(user = 'kuhnslab',
                                      expID = processed_data$expSummary$expID)
  
  # add new records to the database
  dbinit(db_path, processed_data)
}

##### helpful queries #####

# need to do this once (pick a better password)
# dbExecute(con, 'INSERT INTO users VALUES ("kuhnslab", "12345")')

# this one is rather odd - need to look into it
# dbGetQuery(con, "DELETE FROM chanRaw WHERE expID='20161221' AND chanID=4")
