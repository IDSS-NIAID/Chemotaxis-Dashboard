#' Run a local instance of the Chemotaxis-Dashboard
#' 
#' This function will start up a local instance of the Chemotaxis-Dashboard from within RStudio.
#'
#' @details You can also use Docker (see https://github.com/IDSS-NIAID/JoesFlow for more documentation on using Docker), which includes a sample dataset.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom ggplot2 theme_set
#' @importFrom cowplot theme_cowplot
run_app <- function()
{
  # load historical data (data.frame has summary data for each channel, called channel_summ)
  if(file.exists('data/historical.RData'))
  {
    load('data/historical.RData')
  }else{
    load('testdata/historical.RData')
  }
  
  # use `theme_cowplot()`
  theme_cowplot() %>%
    theme_set()
  
  shinyApp(ui = app_ui(),
           server = app_server)
}