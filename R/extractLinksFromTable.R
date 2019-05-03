#' A Webscrape Function
#'
#' This function allows you to extract all the links from table.
#' @param url url where table exists.
#' @param tableID id attribute of the table
#' @export
#' @examples
#' extractLinksFromTable("https://www.drugshortagescanada.ca","drug-shortage-reports")

extractLinksFromTable <- function(url,tableID) {
  html <- xml2::read_html(url)
  
  cssSelectorLinks <- paste("#",tableID," tbody tr td a", sep = "")
  tableLinks <- getElementAttr(html, cssSelectorLinks, "href")
  rowCount <- length(tableLinks)/3
  
  dataTable <- data.frame(matrix(ncol = 3, nrow= rowCount))
  for(n in 1:3) {
    dataTable[,n] <- tableLinks[seq(n,length(tableLinks),3)]
  }
  return(dataTable)
}