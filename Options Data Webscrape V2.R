market_open_time <- as.POSIXct("09:30:00", format = "%H:%M:%S")
market_close_time <- as.POSIXct("16:00:00", format = "%H:%M:%S")
current_time <- Sys.time()

if (current_time >= market_open_time && current_time <= market_close_time) {

library(tidyverse)
library(rvest)
library(lubridate)
library(bizdays)
library(quantmod)
library(httr)

####Setting dates to be pulled####

user_a = user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")


dates_get = function(link){
  x = read_html(session(link, user_a)) 
  y = html_element(x, xpath = '/html/body/div[1]/main/section/section/section/article/section[2]/div/div[1]/div') |> 
    html_text()
  
  date_parts <- as.vector(strsplit(y, " ")[[1]])
  
  date_parts = date_parts[date_parts != ""]
  
  date_vector = c()
  
  for (i in 1:(length(date_parts) - 2)){
    date_vector[i] = paste(date_parts[i], date_parts[i + 1], date_parts[i + 2], sep = " " )
  }
  
  z = 1:length(date_vector)
  z = z %% 3 - 1 == 0
  
  date_vector = date_vector[z]
  
  out = as.Date(date_vector, format = "%b %d, %Y") |>
    as.POSIXct() |>
    as.numeric()
  
  out
}
  
  ############

x = dates_get("https://finance.yahoo.com/quote/TQQQ/options")
y = dates_get("https://finance.yahoo.com/quote/SQQQ/options")
z = dates_get("https://finance.yahoo.com/quote/QQQ/options")


tlink = "https://finance.yahoo.com/quote/TQQQ/options?date="
slink = "https://finance.yahoo.com/quote/SQQQ/options?date="
ilink = "https://finance.yahoo.com/quote/QQQ/options?date="

slinks = paste0(slink, y)
tlinks = paste0(tlink, x)
ilinks = paste0(ilink, z)


td1 = as.data.frame(matrix("",nrow = 0, ncol = 11))

get_options = function(link){
  x  = session(link, user_a)
  
  if 
  
  webpage = read_html(x)
  table_node = html_nodes(webpage, "table")
  browser()
  td1 <<- rbind(td1, as.data.frame(html_table(table_node[1], fill = TRUE)))
  td1 <<- rbind(td1, as.data.frame(html_table(table_node[2], fill = TRUE)))
  
}

all_links = c(slinks,tlinks,ilinks)

for (i in 1:length(all_links)){
  print(i)
  browser()
  get_options(all_links[i])
}


s = getQuote("SQQQ")
t = getQuote("TQQQ")
i = getQuote("QQQ")


td1$QuoteTime = as.character(t[1])
td1$TQQQ = as.numeric(t[2])
td1$SQQQ = as.numeric(s[2])
td1$QQQ = as.numeric(i[2])
td1$PSQ = NA




getSymbols("TQQQ", from = Sys.Date()-60)
tmu = as.vector(na.omit(Delt(TQQQ[,6], type = "log")))
tv = sd(tmu)/(1/252)^.5

td1$HVol = tv

op = "C:\\Users\\Charlie/Desktop/R Stuff/Options Pull"
opf = file.path(op, paste("Options_Pull", Sys.Date(), sep = "_"))

if(file.exists(opf)){
  td = read.csv(opf)
  td = rbind(td,td1)
} else {
  td = td1
}

write.csv(td, file = opf, row.names = FALSE)

} else {}


