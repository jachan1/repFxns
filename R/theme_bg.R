#' @title Biostats Group ggplot Theme
#'
#'
#' theme with no background or gridlines just with x and y axis
#' @keywords ggplot themes
#' @export

theme_bg <- function(){
  theme(panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.line.x = element_line(colour = "gray20"),
      axis.line.y = element_line(colour = "gray20"),
      panel.background=element_blank())
}