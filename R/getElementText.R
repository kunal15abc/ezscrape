#' A Webscrape Function
#'
#' This function allows you to extract text from an element and returns vector
#' @param html html code of the page
#' @param cssSelector css selector of the element
#' @export
#' @examples
#' getElementText()

getElementText <- function(html,cssSelector){
  html %>%
    html_nodes(cssSelector) %>%   
    html_text() %>%
    str_trim() %>%
    unlist()
}