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
            menuItem("Sample Selection", tabName = 'samples', icon = icon('list'))#,
            #menuItem("Figures", tabName = 'figures', icon = icon('chart-line')),
            #menuItem("Videos", tabName = 'videos', icon = icon('film')),
            #menuItem("Download Results", tabName = 'fileDownload', icon = icon('download'))
        )
    ),
    
    #################### Body ####################
    dashboardBody(tabItems(
        
        
        ########## File Upload ##########
        tabItem(tabName = 'samples',
                fluidRow(h3("Select Samples to View")),
                fluidRow(
                    column(width = 9, offset = 1,
                           selectizeGroupUI(
                               id = "sampleFilters",
                               inline = FALSE,
                               params = list(
                                   experiment = list(inputId = "experiment", title = "Experiment (date)", placeholder = 'select'),
                                   sample = list(inputId = "sample", title = "Sample", placeholder = 'select'),
                                   treatment = list(inputId = "treatment", title = "Treatment", placeholder = 'select'),
                                   channel = list(inputId = "channel", title = "Channel", placeholder = 'select')
                               )
                           )
                    )),
                fluidRow(
                    column(1),
                    column(9,
                           tableOutput("selectedSamples")))
                )#,
        
        
        ########## Figure ##########
        # tabItem(tabName = 'figures',
        #         fluidRow(column(1), h3('Figures')),
        #         fluidRow(
        #             column(1),
        #             column(10,
        #                    plotOutput('hairball'))),
        #         fluidRow(
        #             column(1),
        #             column(10,
        #                    plotOutput('vy'))),
        #         fluidRow(
        #             column(1),
        #             column(10,
        #                    plotOutput('vx')))
        #         ),
        
        
        ########## Videos ##########
        # tabItem(tabName = 'videos',
        #         fluidRow(column(1), h3('Processed Video')),
        #         fluidRow(
        #             column(1),
        #             column(12,
        #                    plotOutput('processed_vid')))
        #         ),
        
        
        ########## Downloads #########
        # tabItem(tabName = 'fileDownload',
        #         fluidRow(column(1), h3("Download Results")),
        #         fluidRow(
        #             column(1),
        #             column(9,
        #                    downloadButton('downloadResults', 'Download Results'))),
        #         fluidRow(
        #             column(1),
        #             column(9,
        #                downloadButton('downloadFigures', 'Download Figures'))),
        #         fluidRow(
        #             column(1),
        #             column(9,
        #                    downloadButton('downloadVideo', 'Download Processed Video')))
        #         )
        ))
)