source("./projectfunctions.R")
library(dplyr)


rawdata <- read.csv(unz("city_temperature.zip","city_temperature.csv"), stringsAsFactors = TRUE)


stockholmdata <- rawdata %>%
                 select(everything()) %>%
                 filter(City=="Stockholm" & AvgTemperature > -50 & Year >= 1995 & Day >= 1) %>%
                 mutate(AvgTemperatureC = temperatureC(AvgTemperature)) %>%
                 mutate(DayofYear = getDayofYear(Year, Month, Day)) %>%
                 arrange(Year,DayofYear) %>%
                 distinct()
                 
location <-distinct(rawdata%>%arrange(Country,City), paste0(Country,"+",City))
location = t(location)
latitude = numeric(length(location))
longitude = numeric(length(location))

for(i in 1:length(location)){
  loc = location[i]
  coord <- tryCatch({
    getCityLocation(loc)
  },
  error = function(e){
    print(e)
    numeric(2)
  }
  ) 
  latitude[i]<-coord[1]
  longitude[i]<-coord[2]
  print(paste(i,"of",length(location),":",loc,latitude[i],longitude[i],sep = "  "))
  #Sys.sleep(1.5)
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<2){} 
}

locdf<- data.frame(
  "location" = unlist(as.list(location)),
  "latitude"=unlist(as.list(latitude)),
  "longitude"=unlist(as.list(longitude))
)

locdf$latitude[latitude==0]
which(locdf$latitude[latitude==0])
length(which(locdf$latitude==0))
locdf$location[(which(locdf$latitude==0))]

write.csv(locdf,"locations.csv",row.names = F)



