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
#km.res <- kmeans(clust_data, 3, nstart = 25)
agg<- aggregate(Track, by=list(cluster=km.res$cluster), mean)
km.res$size
km.res$centers




#elbow method function determining # clusters#
#input data set, number clusters and seed
#nc= number of clusters

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data %>% select_if(is.numeric) %>% na.omit, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(clust_data, nc=3, seed=1000)



## k means clustering matrices choose variable to form groups#
#confusion matrix
cm <- table(dataenv$all$track_summ$treatment, km.res$cluster)
print(cm)



# easier naming to be used in cluster function
cluster <- factor(km.res$cluster)
use_clust<- dataenv$all$track_summ



# started to add shiny commands to cluster function, more of a tryout

function_clust <- function(dataset, x_axis, y_axis, label){
    dat <- sample_select_mod()
    
    if(nrow(dat) == nrow(all$track_summ) & nrow(all$track_summ) > 6)
    {
      plot_nothing()
      
    }else { km.res <- kmeans(dataset %>% select_if(is.numeric) %>% na.omit,
                   3, nstart = 25)
  
  clust_fun<- dataset %>%
    as_tibble() %>%
    
    mutate(cluster = km.res$cluster,
    ) %>%
    ggplot(aes({{x_axis}}, {{y_axis}}) )+ 
    geom_point(alpha= 3, size=2, 
               color=c("gold","blue","dark green")[cluster]) +
    geom_text(aes(label={{label}}), 
              alpha=3, size= 1.5, color=c("black", "white", "white")[cluster])
  print(clust_fun)
  
  
  
    }}



function_clust(use_clust, angle_migration, 
              av_velocity, sample) 



#cluster function without the shiny app commands#

function_clust <- function(dataset, x_axis, y_axis, label){
  clust_fun<- dataset %>%
    as_tibble() %>%
    
    mutate(cluster = km.res$cluster,
    ) %>%
    ggplot(aes({{x_axis}}, {{y_axis}}) )+ 
    geom_point(alpha= 3, size=2, 
               color=c("gold","blue","dark green")[cluster]) +
    geom_text(aes(label={{label}}), 
              alpha=3, size= 1.5, color=c("black", "white", "white")[cluster])
  print(clust_fun)
  
}

function_clust(use_clust, angle_migration, 
               av_velocity, sample) 




# cluster table function#

cluster_data_frame<- function(clust_df) { 
  cm <- table(clust_df$channel, km.res$cluster)  
  print(cm)
  cluster_dataset <- as.data.frame(cm)  
  View(cluster_dataset)
  
}

cluster_data_frame(use_clust)




#### parallel plot ggally ####

View(dataenv$exp_summ$tracks_time$data)

ggparcoord(use_clust,
           columns = 12:15, groupColumn = 3,
           showPoints = TRUE,
           title = "Parallel Coordinate Plot for the chemotaxis Data",
           alphaLines = 0.1
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )




sample_select_mod <- callModule(
  module = selectizeGroupServer,
  id = "sampleFilters",
  data = all$track_summ,
  vars = c('experiment', 'date', 'sample', 'treatment', 'channel')
)

output$selectedSamples <- renderTable({
  with(sample_select_mod(),
       tibble(`#Samples` = length(unique(sample)),
              `#Experiements` = length(unique(experiment)),
              `#Treatments` = length(unique(treatment)),
              `Total # of Obs` = length(v_y)))
})

