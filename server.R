library(shiny)
library(shinydashboard)
library(shinybusy)

library(dplyr)
library(readr)
library(purrr)

library(writexl)

library(ggplot2)
library(cowplot)
library(patchwork)

theme_cowplot() %>%
    theme_set()

options(dplyr.summarise.inform = FALSE)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ####################
    # Global Variables #
    ####################
    
    # get paths to files
    # root directory of the git repo
    root <- function(){
        system('git rev-parse --show-toplevel', intern = TRUE)
    }
    
    
    ###################
    # File Upload Tab #
    ###################
    
    process_input <- reactive({
        req(input$f)
        
        # process input file(s)
        show_modal_spinner()
        # call docker container to process file(s)
        Sys.sleep(5)
        remove_modal_spinner()
        
        return(TRUE)
    })
    
    output$ori_vid <- renderImage({
        # initiate processing of uploaded file
        processing <- process_input()
        
        # return uploaded image
        list(src = input$f$datapath,
             width = 1280/5,
             height = 1024/5,
             alt = "Uploaded gif")
    }, deleteFile = TRUE)
    
    
    ###############
    # Figures Tab #
    ###############
    
    ### read in tracks ###
    
    dat <- reactive({
        # samples to read in
        files <- paste0('../../raw/20051108__/', c('CH1_nl/CH1_nl_pre.csv',
                                                   'CH2_nl_fMLF8/CH2_nl_fMLF8_pre.csv',
                                                   'CH3_nl_fMLF8/CH3_nl_fMLF8_pre.csv',
                                                   'CH4_jd/CH4_jd_pre.csv',
                                                   'CH5_jd_fMLF8/CH5_jd_fMLF8_pre.csv',
                                                   'CH6_jd_fMLF8/CH6_jd_fMLF8_pre.csv'))
        
        # read in samples and bind into a single data frame
        map(files, ~ read_csv(.x, col_types = 'dddd')) %>%
            bind_rows(.id = 'sample') %>%
            mutate(sample = case_when(sample == '1' ~ 'CH1_nl',
                                      sample == '2' ~ 'CH2_nl_fMLf8',
                                      sample == '3' ~ 'CH3_nl_fMLF8',
                                      sample == '4' ~ 'CH4_jd',
                                      sample == '5' ~ 'CH5_jd_fMLF8',
                                      sample == '6' ~ 'CH6_jd_fMLF8',
                                      TRUE          ~ '')) %>%
            
            # velocity
            group_by(sample, Track) %>%
            mutate(v.x = c(NA, (X[-1] - X[-length(X)]) / (Frame[-1] - Frame[-length(Frame)])),
                   v.y = c(NA, (Y[-1] - Y[-length(Y)]) / (Frame[-1] - Frame[-length(Frame)])),
                   v   = sqrt(v.x^2 + v.y^2) * sign(v.y)) %>% # going down = positive velocity, going up = negative velocity
            ungroup()
    })
    
    ### velocity vs time ###

    hairball <- function(){
        isolate({
            filter(dat(), !is.na(X) & !is.na(Y) & !is.na(Track)) %>%
                mutate(Track = as.factor(Track)) %>%
                group_by(sample, Track) %>%
                mutate(X = X - X[1],
                       Y = Y - Y[1]) %>%
                
                ggplot(aes(X, Y, color = Track)) +
                geom_smooth(method = 'loess', formula = y~x, se = FALSE, span = 1, size = .2) +
                theme(legend.position = 'none') +
                
                facet_wrap(~ sample)
        })
    }
    
    output$hairball <- renderPlot({
        hairball()
    })

    vy <- function(){
        isolate({
            filter(dat(), !is.na(v.y)) %>%
                group_by(sample, Frame) %>%
                summarize(v.y = mean(v.y)) %>%
                
                ggplot(aes(Frame, v.y)) +
                geom_smooth(method = 'loess', formula = y~x) +
                
                facet_wrap(~ sample) +
                
                ylab('Relative Velocity (y)')
        })
    }
        
    output$vy <- renderPlot({
        vy()
    })
    
    vx <- function(){
        isolate({
            dat_copy <- dat()
            
            filter(dat_copy, !is.na(v.x)) %>%
                group_by(sample, Frame) %>%
                summarize(v.x = mean(v.x)) %>%
                
                ggplot(aes(Frame, v.x)) +
                geom_smooth(method = 'loess', formula = y~x) +
                
                facet_wrap(~ sample) +
                
                ylab('Relative Velocity (x)')    
        })
    }
    
    output$vx <- renderPlot({
        vx()
    })
    
    
    ##############
    # Videos Tab #
    ##############
    
    output$processed_vid <- renderImage({
        # return processed image
        list(src = '../../TrackmateExample.gif',
             width = 1276/2,
             height = 388/2,
             alt = "processed gif")
    }, deleteFile = FALSE)
    
    
    ################
    # Download Tab #
    ################
    
    output$downloadResults <- downloadHandler(
        filename = function(){
            'chemotaxis.xlsx'
        },
        
        content = function(file){
            isolate({
                filter(dat(), !is.na(v.x)) %>%
                    group_by(sample) %>%
                    
                    # velocity statistics across samples
                    summarize(v_sd.x = sd(v.x) / sqrt(length(v.x) - 1),
                              v_mean.x = mean(v.x),
                              
                              v_sd.y = sd(v.y) / sqrt(length(v.y) - 1),
                              v_mean.y = mean(v.y),
                              
                              v_sd   = sd(v) / sqrt(length(v) - 1),
                              v_mean   = mean(v),
                              
                              x.lower = v_mean.x + qnorm(0.025)*v_sd.x,
                              x.upper = v_mean.x + qnorm(0.975, lower.tail = TRUE)*v_sd.x,
                              
                              y.lower = v_mean.y + qnorm(0.025)*v_sd.y,
                              y.upper = v_mean.y + qnorm(0.975, lower.tail = TRUE)*v_sd.y) %>%
                    
                    ungroup() %>%
                    
                    list(dat()) %>%
                    
                    write_xlsx(path = file)
            })
        }
    )
    
    output$downloadFigures <- downloadHandler(
        filename = function(){
            'chemotaxis.png'
        },
        
        content = function(file){
            ggsave(file, plot = hairball() + vy() + vx(), device = 'png',
                   height = 5, width = 15)
        }
    )
    
    output$downloadVideo <- downloadHandler(
        filename = function(){
            'chemotaxis.gif'
        },
        
        content = function(file){
            file.copy('../../TrackmateExample.gif', '../')
            file.rename('../TrackmateExample.gif', file)
        }
    )
})