#' A Webscrape Function
#'
#' This function allows you to extract attribute value from the element and returns vector.
#' @param html html code of the page
#' @param cssSelector css selector of the element
#' @param Attr attribute name whose value to be fetched
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' html <- xml2::read_html("https://www.drugshortagescanada.ca")
#' getElementAttr(html,"table tbody tr td","href")

getElementAttr <- function(html,cssSelector,Attr){
  html %>%
    rvest::html_nodes(cssSelector) %>%   
    rvest::html_attr(Attr) %>%
    stringr::str_trim() %>%
    unlist()
}