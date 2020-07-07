library(lubridate, warn.conflicts = FALSE)
library(curl)
library(jsonlite)

getDayofYear <- function(year, month, day) {
  date <- sprintf("%d-%d-%d",year,month,day)
  day <- yday(date)
  day
}

temperatureC = function(t){
  (t-32)*(5/9)
}

getCityLocation = function(address){
  gurl <- "https://geocode.xyz/LOCATION?json=1&auth=969875307170091649490x6532"
  req <- gsub("LOCATION",address,gurl)
  con=curl(req)
  res = readLines(con)
  resobj <- fromJSON(res)
  loc = c(resobj$latt,resobj$longt)
  loc
}

