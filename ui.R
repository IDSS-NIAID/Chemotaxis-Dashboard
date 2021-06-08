library(shiny)
library(shinydashboard)
library(shinybusy)

options(shiny.maxRequestSize=30*1024^2)
use_busy_spinner()

dashboardPage(
    #################### Header ####################
    dashboardHeader(title = "Chemotaxis Dashboard", titleWidth = 400),
    
    #################### Sidebar ####################
    dashboardSidebar(
        sidebarMenu(
            menuItem("File Upload", tabName = 'fileUpload', icon = icon('upload')),
            menuItem("Figures", tabName = 'figures', icon = icon('chart-line')),
            menuItem("Videos", tabName = 'videos', icon = icon('film')),
            menuItem("Download Results", tabName = 'fileDownload', icon = icon('download'))
        )
    ),
    
    #################### Body ####################
    dashboardBody(tabItems(
        
        
        ########## File Upload ##########
        tabItem(tabName = 'fileUpload',
                fluidRow(column(1), h3("Upload Files")),
                fluidRow(
                    column(1),
                    column(9,
                           fileInput('f',
                                     'Raw clips for upload',
                                     multiple = TRUE,
                                     accept = '.gif'))),
                fluidRow(
                    column(1),
                    column(9,
                           plotOutput('ori_vid')))
                ),
        
        
        ########## Figure ##########
        tabItem(tabName = 'figures',
                fluidRow(column(1), h3('Figures')),
                fluidRow(
                    column(1),
                    column(10,
                           plotOutput('hairball'))),
                fluidRow(
                    column(1),
                    column(10,
                           plotOutput('vy'))),
                fluidRow(
                    column(1),
                    column(10,
                           plotOutput('vx')))
                ),
        
        
        ########## Videos ##########
        tabItem(tabName = 'videos',
                fluidRow(column(1), h3('Processed Video')),
                fluidRow(
                    column(1),
                    column(12,
                           plotOutput('processed_vid')))
                ),
        
        
        ########## Downloads #########
        tabItem(tabName = 'fileDownload',
                fluidRow(column(1), h3("Download Results")),
                fluidRow(
                    column(1),
                    column(9,
                           downloadButton('downloadResults', 'Download Results'))),
                fluidRow(
                    column(1),
                    column(9,
                       downloadButton('downloadFigures', 'Download Figures'))),
                fluidRow(
                    column(1),
                    column(9,
                           downloadButton('downloadVideo', 'Download Processed Video')))
                )
        ))
)