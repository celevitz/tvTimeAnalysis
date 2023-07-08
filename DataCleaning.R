## Carly Levitz
## 2023-07-08
## Look at the data

rm(list=ls())
library(tidyverse)
library(openxlsx)

directory <- "/Users/carlylevitz/Documents/Data/tv-time-personal-data/"

## Set up the data
rewatched <- read.csv(paste0(directory,"rewatched_episode.csv")
                      ,stringsAsFactors = FALSE)
watched <- read.csv(paste0(directory,"seen_episode.csv")
                    ,stringsAsFactors = FALSE)
runtimegenre <- read.xlsx(paste0(directory,"ShowsGenreServiceRun-time.xlsx"))

combined <- rewatched %>%
  bind_rows(watched) %>%
  select(!c(cpt,fb_action_id,tweet_id)) %>%
  mutate(tv_show_name = trimws(tv_show_name,"both")) %>%
  left_join(runtimegenre %>%
              mutate(tv_show_name = trimws(tv_show_name,"both"))) %>%
  # extract month and year
  mutate(year = as.numeric(substr(updated_at,1,4))
         ,monthNumber=as.numeric(substr(updated_at,6,7)) 
         ,quarter = case_when(monthNumber %in% c(1,2,3) ~ 1
                              ,monthNumber %in% c(4,5,6) ~ 2
                              ,monthNumber %in% c(7,8,9) ~ 3
                              ,monthNumber %in% c(10,11,12) ~ 4))

# not sure - there's one show without a name?!
table(combined$tv_show_name[is.na(combined$tv_show_name)])

write.csv(combined,paste0(directory,"cleanedTVdata.csv"),row.names = FALSE)
