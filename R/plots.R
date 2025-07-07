#' plot_nothing
#' This will be called as a placeholder when there is nothing to plot
#' @param message the message to display
#' 
#' @return an empty ggplot object
#' 
#' @export
#' @importFrom ggplot2 ggplot aes geom_text theme element_blank
#' @importFrom dplyr tibble
plot_nothing <- function(message = 'Please pick a subset to plot')
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
    x <- y <- label <- NULL
  
  tibble(x = 1, y = 1, label = message) |>
    ggplot(aes(x = x, y = y, label = label)) +
    geom_text() +
    ylab('') +
    xlab('') +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank()) |>
    return()
}
