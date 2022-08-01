install.packages("ClusterR")
install.packages("cluster")
install.packages("gtools")
install.packages("animation")
install.packages("dplyr")
install.packages("GGally")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("foreach")
install.packages()

# Loading package
library(ClusterR)
library(cluster)
library(gtools)
library(dplyr)
library(purrr)
library(viridis)
library(readr)
library(GGally)
library(hrbrthemes)
library(remotes)
library(tidyr)
library(animation)
library(foreach)




View("~/Chemotaxis-Dashboard/data/19000101.RData")

root <- system('git rev-parse --show-toplevel', intern = TRUE)

paste0(root, '/utils/one_experiment.R') %>%
  source()



# all updated results can be found here
results_dir <- paste(root, 'utils', 'results_csv', sep = '/')

paste0(root, '/utils/historical_data.R')

View(X19000101_CH6_c_fMLF8)
  
clust_data <- X19000101_CH6_c_fMLF8
  

results_meta <- tibble(
    f = results,
    
    dat = strsplit(f, '_', fixed = TRUE),
    
    date = {map_chr(dat, `[`, 1) %>% 
        substr(1, 8) %>% 
        as.Date(format = '%Y%m%d')},
    
    channel = map_int(dat, ~ 
                        {grep(.x, pattern = 'CH[1-6]', value = TRUE) %>%
                            substr(3, 3) %>%
                            as.integer()}),
    
    sample = map_chr(dat, ~
                       {.x[.x != ''][-1] %>%
                           gsub(pattern = '.csv', replacement = '', fixed = TRUE) %>%
                           grep(pattern = '^[0-9]$', invert = TRUE, value = TRUE) %>% # drop any single digit numbers
                           grep(pattern = 'CH[1-6]', invert = TRUE, value = TRUE) %>% # drop channel
                           grep(pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4', ignore.case = TRUE, invert = TRUE, value = TRUE) %>% # drop attractant
                           grep(pattern = 'I8RA', invert = TRUE, value = TRUE)}[1]), # catch a typo
    
    treatment = map_chr(dat, ~ 
                          {.x %>%
                              gsub(pattern = '.csv', replacement = '', fixed = TRUE) %>%
                              grep(.x, pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4|I8RA',
                                   ignore.case = TRUE, value = TRUE)}[1])) %>%
  
  mutate(sample = tolower(sample)) %>% # inconsistent capitalization
  
  dplyr::select(-dat)


##### read in data #####
dat <- map2_df(results_dir, results, ~ 
                 {
                   paste(.x, .y, sep = '/') %>%
                     read_delim(delim = '\t', col_types = 'dddd') %>%
                     mutate(f = .y)
                 }) %>%
  
  bind_rows() %>%
  
  left_join(results_meta, by = 'f') %>%
  
  # pull experiment name
  mutate(experiment = map_chr(f, ~ strsplit(.x, '_CH', fixed = TRUE)[[1]][1])) %>%
  
  filter(!is.na(X) & !is.na(Y))

View(one_experiment(track_summ_select))
View(one_experiment(dat, root = '', sig.figs = 4))
                    

channel_summ <- foreach(current_experiment = track_summ_select$experiment, .combine = rbind) %dopar%
  {
    filter(dat, experiment == current_experiment) %>%
      one_experiment()
  }

save(channel_summ, track_summ_select, file = paste(root, 'data/historical.RData', sep = '/'))

View(channel_summ)




View(dat)

## removing categorical data for clustering
dat_1 <- dat[, -5][,-7][,-8]
dat1 <- dat_1[,-7]
View(dat1)


## function removes categorical variables and NAs to use for clustering ##
rem_categor<- function(dat){
  dat %>% select_if(is.numeric) %>% na.omit
  
}

clust_data<- rem_categor(dat)
View(clust_data)


#removing NA's in new data set 
#x <- c(NA, clust_data)
View(x)

#removing NA and setting x as numeric
#x<- lapply(x, as.numeric)
View(x)

# kmeans function using clust_data data set and 3 clusters
kmeans.re <- kmeans(as.numeric(x[[4]]), 3) 


###rescale_dat_1 <- dat1 %>%
  #mutate(track_scal = scale(Track),
         #frame_scal = scale(Frame),
         #x_scal = scale(X),
         #y_scal = scale(Y),
         #date_scal = scale(date),
         #chan_scal = scale(channel)
         # %>% select(-c(Track, Frame, X, Y, date, channel)))


#calling clustering function
kmeans.re

# Cluster identification for 
# each observation
kmeans.re$cluster


x <- c(NA, clust_data)
x<- lapply(x, as.numeric)

cluster_visual <- function(clust_dat,x){
  #kmeans function using clust_data data set and 3 clusters
  
  kmeans.re <- kmeans(as.numeric(x[[2]]), 3) 
  #confusion matrix
  cm <- table(dat$Frame, kmeans.re$cluster)
  print(cm)
  #plotting clustering using ggplot
    clust_graph <- ggplot(clust_dat, mapping= aes(abs(X),Y, col=kmeans.re$cluster))+
  geom_point(color=factor(kmeans.re$cluster))
    print(clust_graph)

}

cluster_visual(clust_data,x)


## Plotiing cluster centers
kmeans.re$centers
kmeans.re$centers[, c("X", "Y")]


#### parallel plot ggally ####


ggparcoord(channel_summ,
           columns = 10:12, groupColumn = 6,
showPoints = TRUE, 
title = "Parallel Coordinate Plot for the chemotaxis Data",
alphaLines = 0.1
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )



View(channel_summ)

ggparcoord(channel_summ,
           columns = 2:3, groupColumn = 1,
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the chemotaxis Data",
           alphaLines = 0.1
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )




