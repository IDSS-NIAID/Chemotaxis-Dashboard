#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'
#' @export
#' @import rlang
#' 
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' 
#' @importFrom tibble tibble
#' 
#' @importFrom purrr pmap_df
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' 
#' @importFrom grDevices rgb
#' @importFrom grDevices png
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off
#' @importFrom splines bs
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 facet_wrap
#' 
#' @importFrom shiny callModule
#' @importFrom shiny renderTable
#' @importFrom shiny renderPlot
#' @importFrom shiny reactive
#' @importFrom shiny downloadHandler
#' 
#' @importFrom shinyWidgets selectizeGroupServer
app_server <- function(input, output, session)
{
  # global figure to return when there is nothing to plot
  plot_nothing <- function()
  {
    tibble(x = 1, y = 1, label = 'Please pick a subset to plot') %>%
      ggplot(aes(x = .data$x, y = .data$y, label = .data$label)) +
      geom_text() +
      ylab('') +
      xlab('') +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()) %>%
      return()
  }
  
  empty_table <- function(){
    data.frame()
  }
  
  ################################
  # Cross-Experiment Summary Tab #
  ################################
  
  get_channel_summ <- reactive({
    if(file.exists('data/historical.RData'))
      load('data/historical.RData')
    
    # or else it will use ChemotaxisDashboard:::all_experiments
    all_experiments$channel_summ
  })
  
  sample_select_mod <- callModule(
    module = selectizeGroupServer,
    id = "sampleFilters",
    data = get_channel_summ(),
    vars = c('experiment', 'date', 'sample', 'treatment', 'channel')
  )
  
  output$selectedSamples <- renderTable({
    with(sample_select_mod(),
         tibble(`#Samples` = length(unique(sample)),
                `#Experiements` = length(unique(experiment)),
                `#Treatments` = length(unique(treatment)),
                `Total # of Obs` = length(v_y)))
  })
  
  # this will pull all summary tracks (one for each experiment) from the filtered data set
  get_summary_tracks <- function(dat, direction)
  {
    if(direction == 'y')
    {
      dat <- rename(dat, smooth = .data$v_y)
    }else{
      dat <- rename(dat, smooth = .data$v_x)
    }
    
    dat %>%
      pmap_df(function(smooth, experiment, channel, sample, treatment, frames, ...){
        tibble(experiment = experiment,
               channel = channel,
               sample = sample,
               treatment = treatment,
               Frame = frames,
               v = smooth)
      }) %>%
      
      mutate(grp = paste(.data$experiment, .data$channel, sep = '_'))
  }
  
  # generalized relative velocity plot
  relative_velocity_plot <- function(direction = 'y')
  {
    dat <- sample_select_mod()
    
    if(nrow(dat) == nrow(all_experiments$channel_summ) & nrow(all_experiments$channel_summ) > 6)
    {
      plot_nothing()
    }else{
      plt <- get_summary_tracks(dat, direction) %>%
        ggplot(aes(.data$Frame, .data$v, group = .data$grp)) +
        
        geom_line(color = rgb(0,0,0,.3)) +
        
        geom_hline(yintercept = 0, linetype = 2, size = 0.2, color = rgb(0,0,1)) +
        
        ylab(paste0('Relaitve Velocity (', direction, ')'))
      
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
  
  # plot relative velocity for x (horizontal/undirected velocity)
  output$vx <- renderPlot({
    relative_velocity_plot('x')
  })
  
  
  #############################
  # Single-Experiment Figures #
  #############################
  
  get_track_summ_select <- reactive({
    if(file.exists('data/historical.RData'))
      load('data/historical.RData')
    
    # or else it will use ChemotaxisDashboard:::track_summ_select
    track_summ_select
  })
  
  # experiment selection module
  exp_select_mod <- callModule(
    module = selectizeGroupServer,
    id = "expFilters",
    data = get_track_summ_select(),
    vars = c('experiment', 'date')
  )
  
  # get tracks for an experiment
  # this will collapse all tracks from the desired experiment into a flat tibble
  # (only call this if we have exactly one experiment in the subset)
  dat_sub <- reactive({
    f <- exp_select_mod()$experiment
    
    if(file.exists(paste0('data/', f, '.RData')))
      load(paste0('data/', f, '.RData'))
    
    # or else it will use ChemotaxisDashboard:::exp_summ
    exp_summ
  })
  
  
  # plot time-coded track paths
  output$tracks_time <- renderPlot({
    
    # pull the experiment to plot
    tmp <- exp_select_mod()
    
    # if the subset of all experiments contains only 1 experiment, proceed
    if(length(unique(tmp$experiment)) > 1)
    {
      plot_nothing()
    }else{
      dat_sub()$tracks_time # see historical_data.R and one_experiment.R for pre-processing of these figures
    }
  })
  
  # plot velocity measures
  output$tracks_v <- renderPlot({
    # pull the experiment to plot
    tmp <- exp_select_mod()
    
    # if the subset of all experiments contains only 1 experiment, proceed
    if(length(unique(tmp$experiment)) > 1)
    {
      plot_nothing()
    }else{
      dat_sub()$tracks_v # see historical_data.R and one_experiment.R for pre-processing of these figures
    }
  })
  
  output$angle_migration_plot <- renderPlot({
    
    # pull the experiment to plot
    tmp <- exp_select_mod()
    
    # if the subset of all experiments contains only 1 experiment, proceed
    if(length(unique(tmp$experiment)) > 1)
    {
      plot_nothing()
    }else{
      dat_sub()$angle_migration_plot # see historical_data.R and one_experiment.R for pre-processing of these figures
    }
  })
  
  output$ce_plot <- renderPlot({
    
    # pull the experiment to plot
    tmp <- exp_select_mod()
    
    # if the subset of all experiments contains only 1 experiment, proceed
    if(length(unique(tmp$experiment)) > 1)
    {
      plot_nothing()
    }else{
      dat_sub()$ce_plot # see historical_data.R and one_experiment.R for pre-processing of these figures
    }
  })
  
  output$stats_table <- renderTable({
    tmp <- exp_select_mod()
    if(input$chooseSummStats == 'Proportion Finished'){
      dat_sub()$finished_table
    }
    else if(input$chooseSummStats == 'Angle of Migration'){
      dat_sub()$angle_table
    }
    else if(input$chooseSummStats == 'Chemotactic Efficiency'){
      dat_sub()$ce_table
    } 
    else{
      empty_table()
    }
  })
  
  output$downloadTime <- downloadHandler(
    # I want the filename to include the experiment automatically, but I'm not sure how to do that
    filename = function() paste0(exp_select_mod()$experiment,"_timeTracks.png"),
    content = function(file){
        png(file)
        print(dat_sub()$tracks_time)
        dev.off()
    }
  )
  
  output$downloadv <- downloadHandler(
    filename = function() paste0(exp_select_mod()$experiment,"_vPlot.png"),
    content = function(file){
      png(file)
      print(dat_sub()$tracks_v)
      dev.off()
    }
  )
  
  output$downloadAngle <- downloadHandler(
    filename = function() paste0(exp_select_mod()$experiment,"_AngleMigrationViolin.png"),
    content = function(file){
      png(file)
      print(dat_sub()$angle_migration_plot)
      dev.off()
    }
  )
  
  output$downloadCe <- downloadHandler(
    filename = function() paste0(exp_select_mod()$experiment,"_ceViolin.png"),
    content = function(file){
      png(file)
      print(dat_sub()$ce_plot)
      dev.off()
    }
  )
  
  output$downloadAllFigs <- downloadHandler(
    filename = function() paste0(exp_select_mod()$experiment,"_allFigs.pdf"),
    content = function(file){
      pdf(file)
      print(dat_sub()$tracks_time)
      print(dat_sub()$tracks_v)
      print(dat_sub()$angle_migration_plot)
      print(dat_sub()$ce_plot)
      dev.off()
    }
  )
  
  output$downloadFinished <- downloadHandler(
    filename = function() paste0(exp_select_mod()$experiment,"_finished.csv"),
    content = function(file){
      write.csv(dat_sub()$finished_table,file)
    }
  )
  
  #for some reason, even though it is the same code as above, this doesn't work
  output$downloadAngleTab <- downloadHandler(
    filename = function() paste0(exp_select_mod()$experiment,"_angle.csv"),
    content = function(file){
      write.csv(as.matrix(dat_sub()$angle_table),file)
    }
  )
  
  output$downloadCeTab <- downloadHandler(
    filename = function() paste0(exp_select_mod()$experiment,"_ce.csv"),
    content = function(file){
      write.csv(as.matrix(dat_sub()$ce_table),file)
    }
  )
}
