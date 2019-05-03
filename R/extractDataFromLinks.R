#' A Webscrape Function
#'
#' This function allows you to extract data from series of links.
#' @param url url where table exists.
#' @param links a vector containing links
#' @export
#' @examples
#' links <- extractLinksFromTable("https://www.drugshortagescanada.ca","drug-shortage-reports")
#' extractDataFromLinks("https://www.drugshortagescanada.ca",links[,3])


extractDataFromLinks <- function(url,links) {
  newUrl <- paste(url,links[1],sep="")
  
  html <- xml2::read_html(newUrl)
  
  reportTable <- getReportTable(html,1)
  
  reportDF <- data.frame(matrix(ncol = length(reportTable[,1])))
  colnames(reportDF) <- reportTable[,1]
  
  for (n in 1:length(links)) {
    tempurl <- paste(url,links[n],sep = "")
    html <- xml2::read_html(tempurl)
    temptable <- getReportTable(html,1)
    tempDF <- data.frame(matrix(ncol = length(temptable[,1])))
    colnames(tempDF) <- temptable[,1]
    tempDF[1,] <- temptable[,2]
    reportDF <- dplyr::bind_rows(reportDF,tempDF)
  }
  reportDF <- reportDF[-1,]
  return(reportDF)
}