library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(dplyr)
library(purrr)

library(ggplot2)
library(cowplot)
library(patchwork)

theme_cowplot() %>%
    theme_set()
 

####################
# Global Variables #
####################

# load historical data (data.frame has summary data for each channel, called channel_summ)
load('data/historical.RData')


##########
# Server #
##########
shinyServer(function(input, output, session) {
    
    ################################
    # Cross-Experiment Summary Tab #
    ################################
    
    sample_select_mod <- callModule(
        module = selectizeGroupServer,
        id = "sampleFilters",
        data = channel_summ,
        vars = c('experiment', 'date', 'sample', 'treatment', 'channel')
    )
    
    output$selectedSamples <- renderTable({
        with(sample_select_mod(),
             tibble(`#Samples` = length(unique(sample)),
                    `#Experiements` = length(unique(experiment)),
                    `#Treatments` = length(unique(treatment)),
                    `Total # of Obs` = length(smooth_v_y)))
    })
    
    # this will pull all summary tracks (one for each experiment) from the filtered data set
    get_summary_tracks <- function(dat, direction)
    {
        if(direction == 'y')
        {
            dat <- rename(dat, smooth = smooth_v_y)
        }else{
            dat <- rename(dat, smooth = smooth_v_x)
        }
        
        dat %>%
            pmap_df(function(smooth, experiment, channel, sample, treatment, ...){
                tibble(experiment = experiment,
                       channel = channel,
                       sample = sample,
                       treatment = treatment,
                       Frame = smooth$x,
                       v = smooth$y)
            }) %>%
            
            mutate(grp = paste(experiment, channel, sep = '_'))
    }
    
    # generalized realtive velocity plot
    relative_velocity_plot <- function(direction = 'y')
    {
        dat <- sample_select_mod()
        
        if(nrow(dat) == nrow(channel_summ))
        {
            tibble(x = 1, y = 1, label = 'Please pick a subset to plot') %>%
                ggplot(aes(x = x, y = y, label = label)) +
                geom_text() +
                ylab('') +
                xlab('') +
                theme(axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      axis.line = element_blank()) %>%
                return()
        }else{
            plt <- get_summary_tracks(dat, direction) %>%
                ggplot(aes(Frame, v, group = grp)) +
                
                geom_line(color = rgb(0,0,0,.3)) +
                
                geom_hline(yintercept = 0, linetype = 2, size = 0.2, color = rgb(0,0,1)) +
                
                ylab(paste0('Realitve Velocity (', direction, ')'))
            
            if(input$splitPlotsBy == 'None')
                return(plt)
            
            if(input$splitPlotsBy == 'Experiment')
                return(plt + facet_wrap(~ experiment))
            
            if(input$splitPlotsBy == 'Channel')
                return(plt + facet_wrap(~ channel))
            
            if(input$splitPlotsBy == 'Sample')
                return(plt + facet_wrap(~ sample))
            
            if(input$splitPlotsBy == 'Treatment')
                return(plt + facet_wrap(~ treatment))
            
            if(input$splitPlotsBy == 'Sample/Treatment')
                return(plt + facet_wrap(~ sample + treatment))
            
            # just in case...
            return(plt)
        }
    }

    # plot relative velocity for y (vertical/directed velocity)
    output$vy <- renderPlot({
        relative_velocity_plot('y')
    })

    # plot realative velocity for x (horizontal/undirected velocity)
    output$vx <- renderPlot({
        relative_velocity_plot('x')
    })
    
    
    #############################
    # Single-Experiment Figures #
    #############################
    
    exp_select_mod <- callModule(
        module = selectizeGroupServer,
        id = "expFilters",
        data = track_summ_select,
        vars = c('experiment', 'date')
    )
    
    # this will collapse all tracks from the desired experiment into a flat tibble
    collapse_tracks <- function(dat)
    {
        pmap_df(dat, function(experiment, channel, sample, treatment, Track, smooth_x, smooth_y, ...){
                tibble(experiment = experiment,
                       channel = channel,
                       sample = sample,
                       treatment = treatment,
                       Track = Track,
                       Frame = smooth_x$x,
                       X = smooth_x$y,
                       Y = smooth_y$y)
        })
    }
    
    output$tracks_time <- renderPlot({
        
        # pull the experiment to plot
        dat_sub <- exp_select_mod()
        
        # if the subset of all experiments contains only 1 experiment, proceed
        if(length(unique(dat_sub$experiment)) > 1)
        {
            plot(NA, NA, ylim = 0:1, xlim = 0:1, bty = 'n', xaxt = 'n', yaxt = 'n', ylab = '', xlab = '')
            text(.5, .5, 'Please pick a single experiment')
        }else{
            load(paste0('data/', dat_sub$experiment, '.RData'))
            
            collapse_tracks(dat) %>%
            arrange(channel, Track, Frame) %>%
            mutate(lab = paste0(channel, ": ", sample, ", ", treatment)) %>%
            ggplot(aes(x = X, y = Y, group = Track, color = Frame)) +
            geom_path() +
            facet_wrap(~ lab) +
            scale_color_gradient2(low  = 'blue',
                                  mid  = rgb(  0, .62, .45),
                                  high = rgb(.9, .62, 0),
                                  midpoint = 60) +
            geom_hline(yintercept = 0, linetype = 2) +
            geom_hline(yintercept = 1, linetype = 2)
        }
    })
})
