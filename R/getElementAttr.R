#' A Webscrape Function
#'
#' This function allows you to extract attribute value from the element and returns vector.
#' @param html html code of the page
#' @param cssSelector css selector of the element
#' @param Attr attribute name whose value to be fetched
#' @export
#' @examples
#' getElementAttr()

getElementAttr <- function(html,cssSelector,Attr){
  html %>%
    html_nodes(cssSelector) %>%   
    html_attr(Attr) %>%
    str_trim() %>%
    unlist()
}