#' A Webscrape Function
#'
#' This function allows you to extract text from an element and returns vector
#' @param html html code of the page
#' @param cssSelector css selector of the element
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' html <- xml2::read_html("https://www.drugshortagescanada.ca")
#' getElementText(html,"table tbody tr td")

getElementText <- function(html,cssSelector){
  html %>%
    rvest::html_nodes(cssSelector) %>%   
    rvest::html_text() %>%
    stringr::str_trim() %>%
    unlist()
}