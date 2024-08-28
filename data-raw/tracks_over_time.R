

library(RSQLite)
library(DBI)

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

library(dplyr)


### connect to database ###
config <- config::get(file = file.path(system('git rev-parse --show-toplevel', intern = TRUE),
                                       'shiny', 'config.yaml'))

# if db_path doesn't exist, try parsing it
if(!file.exists(config$db_path))
  config$db_path <- eval(parse(text = config$db_path))

# load database
con <- dbConnect(SQLite(), config$db_path)


### pull some data to work with ###

ids <- dbGetQuery(con, 'SELECT DISTINCT expID FROM expSummary')$expID

#' @param x numeric vector
#' @param span numeric smoothing parameter
#' @param direction character direction of smoothing (i.e. 'forward' or 'reverse')
ct_smooth <- function(x, span = 6, direction = 'forward')
{
  stopifnot(direction %in% c('forward', 'reverse'))
  
  if(direction == 'reverse')
    x <- rev(x)
    
  # directional smoothing
  end <- 1:length(x)
  start <- sapply(end - span, function(.x) ifelse(.x < 1, 1, .x))
  x <- sapply(1:length(x), function(.x) mean(x[start[.x]:end[.x]]))
  
  # put back int he correct direction if needed
  if(direction == 'reverse')
    return(rev(x))
  
  return(x)
}


for(i in 2:length(ids))
{
  channels <- dbGetQuery(con, paste0('SELECT chanID FROM chanSummary WHERE expID = "', ids[i], '"'))$chanID
  for(j in 1:length(channels))
  {
    span <- 7
    dat <- dbGetQuery(con, paste0('SELECT * FROM trackRaw WHERE expID = "', ids[i], '" AND chanID = ', channels[j])) %>%
      filter(y > 0)
    
    nSum <- dat %>%
      group_by(frames) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      
      # smooth
      mutate(n_smoothF = ct_smooth(n, span, direction = 'forward'),
             n_smoothR = ct_smooth(n, span, direction = 'reverse'),
             n_smooth = (n_smoothF + n_smoothR) / 2)
    
    ggplot(nSum, aes(frames, n)) +
      geom_line() +
      #geom_smooth(method = 'loess', se = FALSE) +
      #geom_line(aes(frames, n_smoothF), color = 'red') +
      #geom_line(aes(frames, n_smoothR), color = 'blue') +
      geom_line(aes(frames, n_smooth), color = 'blue') +
      geom_hline(yintercept = mean(filter(nSum, frames > 30 & frames < 60)$n), linetype = 'dashed') +
      xlab(paste0(ids[i], ' - ', channels[j]))
    
    ggsave(paste0('n_tracks_over_time/', ids[i], '_', channels[j], '.png'), width = 10, height = 5)
      
  }
}

for(i in 2:length(ids))
{
  channels <- dbGetQuery(con, paste0('SELECT chanID FROM chanSummary WHERE expID = "', ids[i], '"'))$chanID
  for(j in 1:length(channels))
  {
    dat <- dbGetQuery(con, paste0('SELECT * FROM trackRaw WHERE expID = "', ids[i], '" AND chanID = ', channels[j])) %>%
      filter(y > 0)
    
    tracks <- filter(dat, frames == -1)
    
    for(k in 0:120)
    {
      dat_sub <- filter(dat, frames == k)
    }
  }
}

