#' A Webscrape Function
#'
#' This function gives to table in R based on position of table on the page.
#' @param html html code of the page
#' @param n sequence of the table on the page
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' html <- xml2::read_html("https://www.drugshortagescanada.ca")
#' getReportTable(html,1)

getReportTable <- function(html,n) {
    temp <- rvest::html_nodes(html,"table")[[n]]
    rvest::html_table(temp)
}
