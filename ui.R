library(shiny)
library(shinydashboard)
#library(shinybusy)
library(shinyWidgets)

# allows for large file upload size
#options(shiny.maxRequestSize=30*1024^2)
#use_busy_spinner()

dashboardPage(
    #################### Header ####################
    dashboardHeader(title = "Chemotaxis Dashboard", titleWidth = 400),
    
    #################### Sidebar ####################
    dashboardSidebar(
        sidebarMenu(
            menuItem("Cross-Experiment Summary", tabName = 'cross_experiment_summary_tab', icon = icon('list')),
            menuItem("Single-Experiment Figures", tabName = 'single_experiment_figures', icon = icon('chart-line'))#,
            #menuItem("Videos", tabName = 'videos', icon = icon('film')),
            #menuItem("Download Results", tabName = 'fileDownload', icon = icon('download'))
        )
    ),
    
    #################### Body ####################
    dashboardBody(tabItems(
        
        
        ########## Cross-Experiment Summary Tab ##########
        tabItem(tabName = 'cross_experiment_summary_tab',
                fluidRow(h3("Select Samples to View")),
                fluidRow(
                    column(width = 9, offset = 1,
                           selectizeGroupUI(
                               id = "sampleFilters",
                               inline = FALSE,
                               params = list(
                                   experiment = list(inputId = "experiment", title = "Experiment", placeholder = 'select'),
                                   date = list(inputId = "date", title = "Date", placeholder = 'select'),
                                   sample = list(inputId = "sample", title = "Sample", placeholder = 'select'),
                                   treatment = list(inputId = "treatment", title = "Treatment", placeholder = 'select'),
                                   channel = list(inputId = "channel", title = "Channel", placeholder = 'select')
                               )
                           )
                    )),
                fluidRow(
                    column(1),
                    column(9,
                           tableOutput("selectedSamples"))),
                fluidRow(h3("Cross-Experiment Figures")),
                fluidRow(column(10, 
                                radioButtons('splitPlotsBy', 'Select variable(s) to split by', 
                                             selected = 'None', inline = TRUE,
                                             choices = c('Experiment', 'Channel', 'Sample', 'Treatment', 
                                                         'Sample/Treatment', 'None')))),
                fluidRow(
                    column(1),
                    column(10,
                           plotOutput('vy'))),
                fluidRow(
                    column(1),
                    column(10,
                           plotOutput('vx')))
                ),
        
        ########## Single-Experiment Figures ##########
        tabItem(tabName = 'single_experiment_figures',
                fluidRow(h3("Select Experiment to View")),
                fluidRow(
                    column(width = 9, offset = 1,
                           selectizeGroupUI(
                               id = "expFilters",
                               inline = FALSE,
                               params = list(
                                   experiment = list(inputId = "experiment", title = "Experiment", placeholder = 'select'),
                                   date = list(inputId = "date", title = "Date", placeholder = 'select')
                               )
                           ))
                ),
                fluidRow(
                    column(1),
                    column(10,
                           plotOutput('tracks_time'))
                ),
                fluidRow(
                    column(1),
                    column(10,
                           plotOutput('tracks_v'))
                ),
                fluidRow(
                  column(1),
                  column(10,
                         plotOutput('angle_migration_plot'))
                ),
                fluidRow(
                  column(1),
                  column(10,
                         plotOutput('ce_plot'))
                ),
                fluidRow(
                  column(1),
                  column(10,
                         plotOutput('finished_plot'))
                  
        )
        )
))
)