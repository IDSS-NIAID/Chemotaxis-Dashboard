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
load('historical.RData')


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
        vars = c('experiment', 'dt', 'sample', 'treatment', 'channel')
    )
    
    output$selectedSamples <- renderTable({
        with(sample_select_mod(),
             tibble(`#Samples` = length(unique(sample)),
                    `#Experiements` = length(unique(experiment)),
                    `#Treatments` = length(unique(treatment)),
                    `Total # of Obs` = length(smooth_v_y)))
    })
    
    # this will pull all summary tracks (one for each experiment) from the filtered data set
    get_summary_tracks <- function(direction)
    {
        if(direction == 'y')
        {
            dat <- sample_select_mod() %>%
                rename(smooth = smooth_v_y)
        }else{
            dat <- sample_select_mod() %>%
                rename(smooth = smooth_v_x)
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
        plt <- get_summary_tracks(direction) %>%
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

    # plot relative velocity for y (vertical/directed velocity)
    output$vy <- renderPlot({
        relative_velocity_plot('y')
    })

    # plot realative velocity for x (horizontal/undirected velocity)
    output$vx <- renderPlot({
        relative_velocity_plot('x')
    })
})
