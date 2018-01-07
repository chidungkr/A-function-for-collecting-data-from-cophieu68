

rm(list = ls())

library(httr)
library(stringr)
library(magrittr)
library(rvest)
library(tidyverse)
library(lubridate)



convertDate <- function(dt) {
  paste0(str_sub(dt, 7, 10), "-", str_sub(dt, 4, 5), "-", str_sub(dt, 1, 2))
}


subComma <- function(v) {
  str_replace_all(v, ",", "")
}


convertPercent <- function(v) {
  str_replace_all(v, "%", "") %>% 
    as.numeric()*0.01
}



get_symbols_CP68 <- function (symbol, from, to){
  url<-"http://www.cophieu68.vn/historyprice.php"

  fd <- list(id = symbol)
  
  resp <- POST(url, body = fd, endcode = "form")
  resp %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="navigator"]/li[7]/a') %>%
    html_attrs() -> tmp
  
  positionString <- str_locate(tmp[1], "[0-9]") %>% 
    as.data.frame() %$% 
    start
  lastPage <- as.numeric(str_sub(unlist(tmp[1]), positionString,positionString + 1))
  

  symbolData <- data.frame(
    date = character(),
    adjusted = numeric(),
    change1 = numeric(),
    change2 = numeric(),
    close = numeric(),
    volume = numeric(),
    open = numeric(),
    high = numeric(),
    low = numeric(),
    volume_Reconcile = numeric(),
    foreign_buy = numeric(),
    foreign_sell = numeric(),
    value = numeric()
  )
  

  for(i in 1:lastPage){

    fd <- list(
      currentPage = i,
      id = symbol
    )
    
    resp <- POST(url, body = fd, endcode = "form")
    
    
    resp %>% 
      read_html() %>% 
      html_nodes(xpath = '//*[@id="content"]/table') %>%
      html_table() %>% 
      as.data.frame() -> tmp
    
    tmp <- tmp[-1, 2:14]
    tmp[, 1] <- tmp[, 1] %>% convertDate() %>% ymd()
    #tmp[, 1] <- as.Date(convertDate(tmp[,1]), format = "%Y-%m-%d")
    #check dieu kien de continue
    if(min(tmp[, 1]) > to) {next}
    if(max(tmp[, 1]) < from) {break}
    tmp[, c(6, 10:12)] <- apply(tmp[, c(6, 10:12)], 2, subComma)
    tmp[, 4] <- convertPercent(tmp[, 4])
    tmp[, c(2:3, 5:13)] <- apply(tmp[, c(2:3, 5:13)], 2, as.numeric)
    
    ten <- c("date",
             "adjusted",
             "change1",
             "change2",
             "close",
             "volume",
             "open",
             "high",
             "low",
             "volume_Reconcile",
             "foreign_buy",
             "foreign_sell",
             "value")
    
    colnames(tmp) <- ten
    symbolData <- rbind(symbolData, tmp)
  }
  
  symbolData <- data.frame(symbolData, row.names = symbolData[, 1])
  colnames(symbolData) <- ten
  symbolData %<>% 
    mutate(symbol = symbol) %>% 
    select(symbol, date, open, high, low, close, volume, adjusted, everything()) %>% 
    as.tibble()
  cat(paste0("#", symbol, " from ", from, " to ", to, " already collected"))
  invisible(subset(symbolData, and(date >= from, date <= to)))
}


# Test function: 

u <- get_symbols_CP68("SSI", "2016-01-01", today())

u %>% str()
u %>% head(n = 10)

u %>% ggplot(aes(date, open)) + geom_line()



