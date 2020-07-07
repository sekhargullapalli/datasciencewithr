source("./dataloader.R")
library(dplyr)
library(moments)

t_dayofyear <- stockholmdata %>% 
              group_by(DayofYear) %>%
              summarise(min= min(AvgTemperatureC),max = max(AvgTemperatureC), avg = mean(AvgTemperatureC), spread = diff(range(AvgTemperatureC)) ) %>%
              distinct() 







