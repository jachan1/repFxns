#' A Rounding Function
#'
#' This function rounds a number and returns a string. 
#' @param html breaks and special characters given in html (default TRUE). 
#' @param p adds in a 'p' before number (default FALSE)
#' @keywords round string html
#' @export
#' @examples
#' txtRound(c(0.002, 2.3049))
#' ## [1] "0.002" "2.305"
#' 
#' txtRound(c(0.000002, 2.3049), p=T)
#' ## [1] "p&nbsp;&lt;&nbsp;0.001" "p&nbsp;=&nbsp;2.305" 
#' 
#' txtRound(c(0.000002, 2.3049), p=T, html=F)
#' ## [1] "p < 0.001" "p = 2.305"

txtRound <- function(x, digits=3, p=F, html=T){
  tout <- paste(round(x, digits))
  p_txt="p"; ps_txt<-s_txt<-"&nbsp;"; lt_txt="&lt;"; eq_txt="=";
  if(!html){
    ps_txt<-s_txt<-" "; lt_txt="<";
  }
  if(!p) p_txt<-ps_txt<-eq_txt<-""; 
  ifelse(x < (1/10^digits), 
         paste0(p_txt, ps_txt, lt_txt, s_txt, 1/10^digits), 
         paste0(p_txt, ps_txt, eq_txt, ps_txt, tout))
}