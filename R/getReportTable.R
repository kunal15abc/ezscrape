#' A Webscrape Function
#'
#' This function gives to table in R based on position of table on the page.
#' @param html html code of the page
#' @param n sequence of the table on the page
#' @export
#' @examples
#' getReportTable()

getReportTable <- function(html,n) {
  html %>%
    html_nodes("table") %>%
    .[[n]] %>%
    html_table()
}
