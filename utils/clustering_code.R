install.packages("ClusterR")
install.packages("cluster")
install.packages("gtools")
install.packages("animation")
install.packages("dplyr")
install.packages("GGally")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("foreach")
install.packages("autoplot")
install.packages("factoextra")

# Loading package
library(ClusterR)
library(cluster)
library(gridExtra)
library(factoextra)
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




load(file.path("~/Chemotaxis-Dashboard/testdata/historical.RData"),
     dataenv <- new.env() )


root <- system('git rev-parse --show-toplevel', intern = TRUE)

paste0(root, '/utils/one_experiment.R') %>%
  source()


# all updated results can be found here
results_dir <- paste(root, 'utils', 'results_csv', sep = '/')

paste0(root, '/utils/historical_data.R')


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




## function removes categorical variables and NAs to use for clustering ##
rem_categor<- function(dat){
  dat %>% select_if(is.numeric) %>% na.omit
  
}


## calls function and makes data set without categorical
clust_data<- rem_categor(dataenv$all$track_summ)


#clustering code#


  km.res <- kmeans(clust_data, 4, nstart = 25)
  agg<- aggregate(Track, by=list(cluster=km.res$cluster), mean)
  km.res$size
  km.res$centers
 
  
  
  
#elbow method function determiing # clusters#

wssplot <- function(data, nc=15, seed=1234){
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
      set.seed(seed)
      wss[i] <- sum(kmeans(clust_data, centers=i)$withinss)}
    plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")
    wss
  }
  

wssplot(clust_data, nc=3, seed=1000)



## k means clustering matrices choose variable to form groups#
  #confusion matrix
  cm <- table(dataenv$exp_summ$tracks_time$data$treatment, km.res$cluster)
  print(cm)
  
cluster <- factor(km.res$cluster)
  
  
# cluster graph choosing label variable and row variable for clustering
 cluster_graph <- clust_data %>%
   as_tibble() %>%
    mutate(cluster = km.res$cluster,
         state = row.names(Track)) %>%
  ggplot(aes(max_v, angle_migration) )+ 
   geom_point(alpha= 3, size=2, 
   color=c("gold","blue","dark green", "orange")[cluster])
 
 cluster_graph
 
 
#### parallel plot ggally ####

View(dataenv$exp_summ$tracks_time$data)

ggparcoord(dataenv$exp_summ$tracks_time$data,
           columns = 12:13, groupColumn = 9,
showPoints = TRUE,
title = "Parallel Coordinate Plot for the chemotaxis Data",
alphaLines = 0.1
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )


View(dat)
View(channel_summ)

ggparcoord(dat,
           columns = 3:4, groupColumn = 9,
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the chemotaxis Data",
           alphaLines = 0.1
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )




