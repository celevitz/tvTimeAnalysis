## Carly Levitz
## 2023-07-08
## six months of data

rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/tv-time-personal-data/"

input <- read.csv(paste0(directory,"cleanedTVdata.csv")
                  ,stringsAsFactors = FALSE) 

currentquarter <- 2
currentyear <- 2023

previousquarter <- 1
previousquarteryear <- 2023

previousyearsamequarter <- 2022

## Compare this past quarter to the previous quarter, and last year at this time

periodsofinterest <- input %>%
  mutate(timeperiod = case_when(year == currentyear & 
                                  quarter == currentquarter ~ "current quarter"
                      ,year == previousquarteryear & 
                          quarter == previousquarter ~ "previous quarter"
                      ,year == previousyearsamequarter & 
                          quarter == currentquarter ~ "last year same quarter" 
                      ,TRUE ~ "exclude"
                      ) ) %>%
  filter(timeperiod != "exclude")
    
    
    
    
summary <- 
  # number of episodes watched
  periodsofinterest %>%
  group_by(timeperiod) %>%
  summarise(numberEpisodes=n()) %>%
  
  full_join(
    # number of unique shows
    periodsofinterest %>%
    group_by(timeperiod) %>%
    select(timeperiod,tv_show_name) %>%
    distinct() %>%
    summarise(numberUniqueShows=n()) ) %>%
  
  full_join (
    # amount of time
    periodsofinterest %>%
    group_by(timeperiod) %>%
    summarise(minutes = sum(run_time)) %>%
    mutate(hours = round(minutes/60,1)))


    


