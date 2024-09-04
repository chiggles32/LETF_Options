market_open_time <- as.POSIXct("09:30:00", format = "%H:%M:%S")
market_close_time <- as.POSIXct("16:00:00", format = "%H:%M:%S")
current_time <- Sys.time()
current_day = weekdays(Sys.Date())

if ((current_time >= market_open_time && current_time <= market_close_time) && !(current_day %in% c("Saturday", "Sunday"))) {

  
  # Getting market data
  # libraries
  require("httr");require("stringr");require("RCurl");require("lubridate");require("data.table");require("tidyverse")

  # User/session info
  
  
  # pw = new.env()
  assign("app_key", "GrcH92i98v1KJt5THVgZ2fnjQU2ACSdI", envir = .GlobalEnv)
  assign("secret", "KO1EBnTHZcrFef7E", envir = .GlobalEnv)
  
  redirect_url = 'https://127.0.0.1'
  
  
  assign("acc_token", readRDS("C:/Users/Charlie/Desktop/Options Data and Trading Models/cs_tokens.rds")$access_token, envir = .GlobalEnv)
  
  # Put cAT in all calls to ensure we are good
  
  checkAccessToken = function(printMsg = TRUE, forceget = FALSE){
    if (file.exists("C:/Users/Charlie/Desktop/Options Data and Trading Models/cs_tokens.rds")){
      tokens = readRDS("C:/Users/Charlie/Desktop/Options Data and Trading Models/cs_tokens.rds")
      
      getNewAccToken = (as.POSIXct(tokens$access_token_exp, tz = Sys.timezone()) < Sys.time())
      getNewRefToken = (as.POSIXct(tokens$refresh_token_exp, tz = Sys.timezone()) < Sys.time())
    }
    if (!file.exists("C:/Users/Charlie/Desktop/Options Data and Trading Models/cs_tokens.rds")){
      getNewAccToken = NULL
      getNewRefToken = NULL
    }
    # renew only if ref token is good but access exp
    if ((getNewAccToken | forceget) & !getNewRefToken){
      URL = 'https://api.schwabapi.com/v1/oauth/token'
      payload = list('grant_type' = 'refresh_token', 'refresh_token' = tokens$refresh_token)
      pg = httr::POST(url = URL,
                      body = payload,
                      httr::add_headers(`Authorization` = paste0("Basic ", base64Encode(paste0(app_key, ":", secret))[1]),
                                        `Content-Type` = 'application/x-www-form-urlencoded'),
                      encode = "form")
      got_time = as.POSIXct(pg[["date"]], tz = Sys.timezone())
      
      if (httr::status_code(pg) == 200){
        
        resp = httr::content(pg)
        
        resp = c(resp,
                 list(access_token_exp = got_time + minutes(30),
                      refresh_token_exp = tokens$refresh_token_exp))
        
        saveRDS(resp, "C:/Users/Charlie/Desktop/Options Data and Trading Models/cs_tokens.rds")
        assign("acc_token", resp$access_token, envir = .GlobalEnv)
        assign("ref_token", resp$refresh_token, envir = .GlobalEnv)
        if(printMsg){cat("\n", sprintf("%-30s : %-20s", "Renewed 30-minute Access Token", "File Saved"))}
        
      } else{
        cat("\n", sprintf("%-30s : %-20s", "Unable to get new access token ", httr::content(pg)$error_description))
      }
      
    }
    if (is.null(getNewAccToken) & is.null(getNewRefToken)){if (printMsg){cat("\n", sprintf("%-30s : %-20s", "Error", "Token File Does Not Exist"))}}
    
    if (getNewRefToken){if (printMsg){cat("\n", sprintf("%-30s : %-20s", "7 Day Refresh token expired", "Please Renew"))}}
    
    if(!getNewAccToken & !getNewRefToken){if (printMsg){cat("\n", sprintf("%-30s : %-20s", "Both tokens valid", "No Action Required"))}}
    
  }
  
  # server = "https://api.schwabapi.com/marketdata/v1/"
  
  get_data = function(symbol) {
    
    checkAccessToken(TRUE)
    
    URL = paste0("https://api.schwabapi.com/marketdata/v1/chains?symbol=", symbol)
    
    price = httr::GET(url = URL,
                      httr::add_headers(`accept` = 'application/json',
                                        `Authorization` = paste0("Bearer ", acc_token))
    )
    
    if (price$status_code == 200){
      price<<-price
    } else {
      cat("\nFailed to get quote, Status Code: ", price$status_code)
    }
    #data
  }
  
  strike_data = function(x){
    df = data.frame(matrix(ncol = length(x[[1]]), nrow = 1))
    colnames(df) = names(x[[1]])
    df[1,] = x[[1]]
    df = df |>
      select(-optionDeliverablesList)
    df
  }
  
  expiries_data = function(x){
    
    do.call(rbind, lapply(x, strike_data))
    
  }
  
  option_chain = function(ticker){
    
    api_resp = content(get_data(ticker))
    
    calls = do.call(rbind, lapply(api_resp$callExpDateMap, expiries_data))
    puts = do.call(rbind, lapply(api_resp$putExpDateMap, expiries_data))
    
    rbind(calls, puts)
    
  }
  
  # Get a bunch of other LETFs
  
  tickers = list("QQQ", "TQQQ", "SQQQ",  "SPXL", "SPXS", "TNA", "TZA", "IWM", "NVDU", "NVDD")
  
  
  # "SPY",
  # , "NVDA"
  all_data = do.call(rbind, lapply(tickers, option_chain))
  
  #### Saving Files
  
  op = "C:/Users/Charlie/Desktop/R Stuff/Options Pull"
  opf = file.path(op, paste("CS_Options_Pull", Sys.Date(), sep = "_"))
  
  if(file.exists(opf)){
    options_file = read.csv(opf)
    options_file = rbind(options_file, all_data)
  } else {
    options_file = all_data
  }
  
  write.csv(options_file, file = opf, row.names = FALSE)
  
  cat("\nholy batman\n")
  
 } else {}
 











#### Useful functions

# strftime(as_datetime( /1000), '%Y-%m-%d %H:%M:%OS3')
