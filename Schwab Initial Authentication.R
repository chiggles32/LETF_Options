# As per "Quantroom" youtube video
# Libraries

require("httr");require("stringr");require("RCurl");require("lubridate");require("data.table")

# User/session info

pw = new.env()
assign("app_key", "GrcH92i98v1KJt5THVgZ2fnjQU2ACSdI", envir = pw )
assign("secret", "KO1EBnTHZcrFef7E", envir = pw )

redirect_url = 'https://127.0.0.1'

cs_login_auth = function(app_key,redirect_url){paste0('https://api.schwabapi.com/v1/oauth/authorize?&client_id=',app_key,'&redirect_uri=',redirect_url)}
cs_login_auth(app_key = pw$app_key, redirect_url = redirect_url)

# Copy and paste URL in browser, follow steps, copy and paste end URL
## This is the manual part. Can we automate?

respURL = ''

get_cs_code = function(respURL){paste0(str_sub(respURL, start = str_locate(respURL,pattern = "code=")[2]+1,
                                               end=str_locate(respURL,pattern = "%40&")[1]-1), "@")
}
cs_code = get_cs_code(respURL)

generate_tokens = function(app_key, secret, cs_code, redirect_url){
  #auth url
  URL = 'https://api.schwabapi.com/v1/oauth/token'
  
  payload = list('grant_type'='authorization_code',
                 'code'= cs_code,
                 'redirect_uri'= redirect_url)
  #request
  pg <- httr::POST(url=URL,
                   body = payload,
                   httr::add_headers(`Authorization` = paste0("Basic ",base64Encode(paste0(app_key,":",secret))[1]),
                                     `Content-Type` = 'application/x-www-form-urlencoded'),
                   encode="form"
  )
  
  if (httr::status_code(pg) == 200|201){
    
    got_time = as.POSIXct(pg[["date"]],tz = Sys.timezone())
    
    resp <- httr::content(pg)
    
    resp <- c(resp,
             list(access_token_exp = got_time + minutes(30),
                  refresh_token_exp = got_time + days(7)))
    
    setwd("C:/Users/Charlie/Desktop/Options Data and Trading Models")
    
    saveRDS(resp, "cs_tokens.rds")
  }else {
    cat("\nError in Authentication: check your app_key, secret, cs_code, and redirect_url")
    resp <- NULL
  }
  resp
}

resp <- generate_tokens(app_key = pw$app_key, secret = pw$secret, cs_code = cs_code, redirect_url = redirect_url)

# if error check expiration time, its quick
# strftime(as_datetime( /1000), '%Y-%m-%d %H:%M:%OS3')

if(!is.null(resp)){
  acc_token = resp$access_token
  ref_token = resp$refresh_token
} else {
  cat("\nWarning: Page response is NULL")
}




#############

assign("acc_token", readRDS("cs_tokens.rds")$access_token, envir = pw)

checkAccessToken = function(printMsg){
  if (file.exists("cs_tokens.rds")){
    tokens = readRDS("cs_tokens.rds")
    
    getNewAccToken = (as.POSIXct(tokens$access_token_exp, tz = Sys.timezone()) < Sys.time())
    getNewRefToken = (as.POSIXct(tokens$refresh_token_exp, tz = Sys.timezone()) < Sys.time())
  }
  if (!file.exists("cs_tokens.rds")){
    getNewAccToken = NULL
    getNewRefToken = NULL
  }
  # renew only if ref token is good but access exp
  if (getNewAccToken & !getNewRefToken){
    URL = 'https://api.schwabapi.com/v1/oauth/token'
    payload = list('grant_type' = 'refresh_token', 'refresh_token' = tokens$refresh_token)
    pg = httr::POST(url = URL,
                    body = payload,
                    httr::add_headers(`Authorization` = paste0("Basic ", base64Encode(paste0(pw$app_key, ":", pw$secret))[1]),
                                      `Content-Type` = 'application/x-www-form-urlencoded'),
                    encode = "form")
    got_time = as.POSIXct(pg[["date"]], tz = Sys.timezone())
    
    if (httr::status_code(pg) == 200){
      
      resp = httr::content(pg)
      
      resp = c(resp,
               list(access_token_exp = got_time + minutes(30),
                    refresh_token_exp = tokens$refresh_token_exp))
      setwd("C:/Users/Charlie/Desktop/Options Data and Trading Models")
      saveRDS(resp, "cs_tokens.rds")
      acc_token = resp$access_token
      ref_token = resp$refresh_token
      if(printMsg){cat("\n", sprintf("%-30s : %-20s", "Renewed 30-minute Access Token", "File Saved"))}
      
    } else{
      cat("\n", sprintf("%-30s : %-20s", "Unable to get new access token ", httr::content(pg)$error_description))
    }
    
  }
  if (is.null(getNewAccToken) & is.null(getNewRefToken)){if (printMsg){cat("\n", sprintf("%-30s : %-20s", "Error", "Token File Does Not Exist"))}}
  
  if (getNewRefToken){if (printMsg){cat("\n", sprintf("%-30s : %-20s", "7 Day Refresh token expired", "Please Renew"))}}
  
  if(!getNewAccToken & !getNewRefToken){if (printMsg){cat("\n", sprintf("%-30s : %-20s", "Both tokens valid", "No Action Required"))}}
  
}


