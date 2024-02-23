market_open_time <- as.POSIXct("09:30:00", format = "%H:%M:%S")
market_close_time <- as.POSIXct("16:00:00", format = "%H:%M:%S")
current_time <- Sys.time()

if (current_time >= market_open_time && current_time <= market_close_time) {
 
library(tidyverse)
library(rvest)
library(lubridate)
library(bizdays)
library(quantmod)
  
####Setting dates to be pulled####
dateT = read_html("https://finance.yahoo.com/quote/TQQQ/options?p=TQQQ")
x = dateT |>
  html_nodes("option") |>
  html_text() |>
  as.Date(format = "%B %d, %Y") |>
  as.POSIXct() |>
  as.numeric()

dateS = read_html("https://finance.yahoo.com/quote/SQQQ/options?p=SQQQ")
y = dateS |>
  html_nodes("option") |>
  html_text() |>
  as.Date(format = "%B %d, %Y") |>
  as.POSIXct() |>
  as.numeric()

dateI = read_html("https://finance.yahoo.com/quote/QQQ/options?p=QQQ")
z = dateI |>
  html_nodes("option") |>
  html_text() |>
  as.Date(format = "%B %d, %Y") |>
  as.POSIXct() |>
  as.numeric()

dateII = read_html("https://finance.yahoo.com/quote/PSQ/options?p=PSQ")
a = dateII |>
  html_nodes("option") |>
  html_text() |>
  as.Date(format = "%B %d, %Y") |>
  as.POSIXct() |>
  as.numeric()


tlink = "https://finance.yahoo.com/quote/TQQQ/options?p=TQQQ&date="
slink = "https://finance.yahoo.com/quote/SQQQ/options?p=SQQQ&date="
ilink = "https://finance.yahoo.com/quote/QQQ/options?p=QQQ&date="
iilink = "https://finance.yahoo.com/quote/PSQ/options?p=PSQ&date="


slinks = paste0(slink, y)
tlinks = paste0(tlink, x)
ilinks = paste0(ilink, z)
iilinks = paste0(iilink, a)


td1 = as.data.frame(matrix("",nrow = 0, ncol = 11))

for (i in 1:length(x)){
  
  webpaget = read_html(tlinks[i])
  
  table_nodet = html_nodes(webpaget, "table")
 
  td1 = rbind(td1, as.data.frame(html_table(table_nodet[1], fill = TRUE)))
  td1 = rbind(td1, as.data.frame(html_table(table_nodet[2], fill = TRUE)))
}

for (i in 1:length(y)){
  
  webpages = read_html(slinks[i])
  
  table_nodes = html_nodes(webpages, "table")
  
  td1 = rbind(td1, as.data.frame(html_table(table_nodes[1], fill = TRUE)))
  td1 = rbind(td1, as.data.frame(html_table(table_nodes[2], fill = TRUE)))
  
}

for (i in 1:length(z)){
  
  webpages = read_html(ilinks[i])
  
  table_nodes = html_nodes(webpages, "table")
  
  td1 = rbind(td1, as.data.frame(html_table(table_nodes[1], fill = TRUE)))
  td1 = rbind(td1, as.data.frame(html_table(table_nodes[2], fill = TRUE)))
  
}

for (i in 1:length(a)){
  
  webpages = read_html(iilinks[i])
  
  table_nodes = html_nodes(webpages, "table")
  
  td1 = rbind(td1, as.data.frame(html_table(table_nodes[1], fill = TRUE)))
  td1 = rbind(td1, as.data.frame(html_table(table_nodes[2], fill = TRUE)))
  
}


s = getQuote("SQQQ")
t = getQuote("TQQQ")
i = getQuote("QQQ")
ii = getQuote("PSQ")

td1$QuoteTime = as.character(t[1])
td1$TQQQ = as.numeric(t[2])
td1$SQQQ = as.numeric(s[2])
td1$QQQ = as.numeric(i[2])
td1$PSQ = as.numeric(ii[2])



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


