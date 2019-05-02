#' A Webscrape Function
#'
#' This function allows you to extract all the data from table and returns dataframe
#' @param url url where table exists.
#' @param tableID id attribute of the table
#' @export
#' @examples
#' extractTableData()

extractTableData <- function(url,tableID){
  html <- read_html(url)
  
  cssSelectorHead <- paste("#",tableID," thead tr th", sep = "")
  cssSelectorBody <- paste("#",tableID," tbody tr td", sep = "")
  
  tableHeader <- getElementText(html,cssSelectorHead)
  colCount <- length(tableHeader)
  
  tableData <- getElementText(html,cssSelectorBody)
  rowCount <- length(tableData)/colCount
  rowCount
  
  dataTable <- data.frame(matrix(ncol = colCount, nrow= rowCount))
  colnames(dataTable) <- tableHeader
  
  for(n in 1:colCount) {
    dataTable[,n] <- tableData[seq(n,length(tableData),colCount)]
  }
  
  # outputFileName <- paste("Output/",tableID,".csv",sep = "")
  # write.csv(dataTable,file=outputFileName, row.names = FALSE)
  return(dataTable)
}