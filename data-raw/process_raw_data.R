# process_raw_data.R

devtools::load_all()

library(stringr)
library(purrr)

library(DBI)
library(RSQLite)


# root directory of the repo
root <- here::here()
dat_path <- file.path(root, 'shiny')
db_path <- file.path(dat_path, 'chemo-dash.sqlite')


# check what has been done already
con <- dbConnect(SQLite(), db_path)
  
if(file.exists(db_path) & file.size(db_path) > 0)
{
  done <- dbGetQuery(con, 'SELECT DISTINCT expID FROM chanSummary')$expID
}else{
  done <- character()
}


# get all experiments
all_experiments <- list.files(file.path(root, 'data-raw', 'results_csv')) |>
  grep(pattern = '19000101', value = TRUE, invert = TRUE, fixed = TRUE) |>
  grep(pattern = 'old', value = TRUE, invert = TRUE, fixed = TRUE) |>
  str_split(pattern = '_') |>
  sapply(`[`, 1) |>
  str_replace('-$', '') |>
  unique()
all_experiments <- all_experiments[!all_experiments %in% done]


# process experiments that haven't been processed yet
for(i in 1:length(all_experiments))
{
  paste0("Adding ", all_experiments[i], '\n') |>
    cat()

  processed_data <- process_experiments(experiment = all_experiments[i],
                                        source_dir = file.path(root, 'data-raw', 'results_csv'),
                                        results_dir = file.path(root, 'shiny'))
  
  # add new records to the database
  dbinit(con = con, data = processed_data)
}

dbDisconnect(con)
