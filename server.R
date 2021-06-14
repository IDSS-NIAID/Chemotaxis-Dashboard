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
    
    ######################
    # Select Samples Tab #
    ######################
    
    sample_select_mod <- callModule(
        module = selectizeGroupServer,
        id = "sampleFilters",
        data = channel_summ,
        vars = c('experiment', 'sample', 'treatment', 'channel')
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
    
    ###############
    # Figures Tab #
    ###############

    # ### velocity vs time ###
    # 
    # hairball <- function(){
    #     isolate({
    #         filter(dat(), !is.na(X) & !is.na(Y) & !is.na(Track)) %>%
    #             mutate(Track = as.factor(Track)) %>%
    #             group_by(sample, Track) %>%
    #             mutate(X = X - X[1],
    #                    Y = Y - Y[1]) %>%
    #             
    #             ggplot(aes(X, Y, color = Track)) +
    #             geom_smooth(method = 'loess', formula = y~x, se = FALSE, span = 1, size = .2) +
    #             theme(legend.position = 'none') +
    #             
    #             facet_wrap(~ sample)
    #     })
    # }
    # 
    # output$hairball <- renderPlot({
    #     hairball()
    # })

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

    output$vy <- renderPlot({
        relative_velocity_plot('y')
    })

    output$vx <- renderPlot({
        relative_velocity_plot('x')
    })


    # ##############
    # # Videos Tab #
    # ##############
    # 
    # output$processed_vid <- renderImage({
    #     # return processed image
    #     list(src = '../../TrackmateExample.gif',
    #          width = 1276/2,
    #          height = 388/2,
    #          alt = "processed gif")
    # }, deleteFile = FALSE)
    # 
    # 
    # ################
    # # Download Tab #
    # ################
    # 
    # output$downloadResults <- downloadHandler(
    #     filename = function(){
    #         'chemotaxis.xlsx'
    #     },
    #     
    #     content = function(file){
    #         isolate({
    #             filter(dat(), !is.na(v.x)) %>%
    #                 group_by(sample) %>%
    #                 
    #                 # velocity statistics across samples
    #                 summarize(v_sd.x = sd(v.x) / sqrt(length(v.x) - 1),
    #                           v_mean.x = mean(v.x),
    #                           
    #                           v_sd.y = sd(v.y) / sqrt(length(v.y) - 1),
    #                           v_mean.y = mean(v.y),
    #                           
    #                           v_sd   = sd(v) / sqrt(length(v) - 1),
    #                           v_mean   = mean(v),
    #                           
    #                           x.lower = v_mean.x + qnorm(0.025)*v_sd.x,
    #                           x.upper = v_mean.x + qnorm(0.975, lower.tail = TRUE)*v_sd.x,
    #                           
    #                           y.lower = v_mean.y + qnorm(0.025)*v_sd.y,
    #                           y.upper = v_mean.y + qnorm(0.975, lower.tail = TRUE)*v_sd.y) %>%
    #                 
    #                 ungroup() %>%
    #                 
    #                 list(dat()) %>%
    #                 
    #                 write_xlsx(path = file)
    #         })
    #     }
    # )
    # 
    # output$downloadFigures <- downloadHandler(
    #     filename = function(){
    #         'chemotaxis.png'
    #     },
    #     
    #     content = function(file){
    #         ggsave(file, plot = hairball() + vy() + vx(), device = 'png',
    #                height = 5, width = 15)
    #     }
    # )
    # 
    # output$downloadVideo <- downloadHandler(
    #     filename = function(){
    #         'chemotaxis.gif'
    #     },
    #     
    #     content = function(file){
    #         file.copy('../../TrackmateExample.gif', '../')
    #         file.rename('../TrackmateExample.gif', file)
    #     }
    # )
})
