###Oakton STEM Speaker Series March 2, 2021
###https://github.com/gjm112/Oakton\_STEM\_Series\_baseball
###https://sabr.org/sabermetrics/data

##########################################
#Lahman Database
##########################################
library(tidyverse)
library(Lahman)
Batting %>% 
  subset(yearID <= 2019 & yearID >= 2000) %>%
  arrange(-HR) %>% 
  left_join(People) %>%
  select(nameFirst,nameLast,yearID,HR) %>%
  head(10)




library(dplyr)
library(Lahman)
#Take data from 2015 - 2019
#dat <- subset(Batting, yearID <= 2019 & yearID >= 2015)
#RBI from 2015 to 2019
Batting %>% 
  subset(yearID <= 2019 & yearID >= 2015) %>%
  group_by(playerID) %>% 
  summarise(HR = sum(HR), Hits = sum(H), RBI = sum(RBI)) %>%
  arrange(-RBI) %>% 
  left_join(People) %>%
  select(nameFirst,nameLast,HR,Hits,RBI)




##########################################
#Statcast data
##########################################
#Shout out to Bill Petti!
  scrape_statcast_savant_pitcher_date <- function(start_date, end_date) {
    
    # extract year
    year <- substr(start_date, 1,4)
    
    # Base URL.
    url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=",year,"%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=",start_date,"&game_date_lt=",end_date,"&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
    
    payload <- utils::read.csv(url)
    
    if (length(payload$pitch_type) > 0) {
      
      # Clean up formatting.
      payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
      payload$des <- as.character(payload$des)
      payload$game_pk <- as.character(payload$game_pk) %>% as.numeric()
      payload$on_1b <- as.character(payload$on_1b) %>% as.numeric()
      payload$on_2b <- as.character(payload$on_2b) %>% as.numeric()
      payload$on_3b <- as.character(payload$on_3b) %>% as.numeric()
      payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
      payload$release_pos_z <- as.character(payload$release_pos_z) %>% as.numeric()
      payload$release_pos_y <- as.character(payload$release_pos_y) %>% as.numeric()
      payload$pfx_x <- as.character(payload$pfx_x) %>% as.numeric()
      payload$pfx_z <- as.character(payload$pfx_z) %>% as.numeric()
      payload$hc_x <- as.character(payload$hc_x) %>% as.numeric()
      payload$hc_y <- as.character(payload$hc_y) %>% as.numeric()
      payload$woba_denom <- as.character(payload$woba_denom) %>% as.numeric()
      payload$woba_value <- as.character(payload$woba_value) %>% as.numeric()
      payload$babip_value <- as.character(payload$babip_value) %>% as.numeric()
      payload$iso_value <- as.character(payload$iso_value) %>% as.numeric()
      payload$plate_z <- as.character(payload$plate_z) %>% as.numeric()
      payload$plate_x <- as.character(payload$plate_x) %>% as.numeric()
      payload$vx0 <- as.character(payload$vx0) %>% as.numeric()
      payload$vy0 <- as.character(payload$vy0) %>% as.numeric()
      payload$vz0 <- as.character(payload$vz0) %>% as.numeric()
      payload$ax <- as.character(payload$ax) %>% as.numeric()
      payload$ay <- as.character(payload$ay) %>% as.numeric()
      payload$az <- as.character(payload$az) %>% as.numeric()
      payload$sz_top <- as.character(payload$sz_top) %>% as.numeric()
      payload$sz_bot <- as.character(payload$sz_bot) %>% as.numeric()
      payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as.numeric()
      payload$launch_speed <- as.character(payload$launch_speed) %>% as.numeric()
      payload$launch_speed_angle <- as.character(payload$launch_speed_angle) %>% as.numeric()
      payload$launch_angle <- as.character(payload$launch_angle) %>% as.numeric()
      payload$estimated_ba_using_speedangle <- as.character(payload$estimated_ba_using_speedangle) %>% as.numeric()
      payload$estimated_woba_using_speedangle <- as.character(payload$estimated_woba_using_speedangle) %>% as.numeric()
      payload$effective_speed <- as.character(payload$effective_speed) %>% as.numeric()
      payload$release_speed <- as.character(payload$release_speed) %>% as.numeric()
      payload$zone <- as.character(payload$zone) %>% as.numeric()
      payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as.numeric()
      payload$release_extension <- as.character(payload$release_extension) %>% as.numeric()
      payload$barrel <- with(payload, ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
      payload$home_team <- as.character(payload$home_team)
      payload$away_team <- as.character(payload$away_team)
      
      return(payload)
    }
    
    else {
      vars <- names(payload)
      df <- lapply(vars, function(x) x <- NA)
      names(df) <- names(payload)
      payload_na <- bind_rows(df)
      
      return(payload_na)
      
      Sys.sleep(sample(x = runif(20, min = .01, max = 1), size = 1))
    }
  }

  #Shout out to Bill Petti (@billpetti).  
  #https://billpetti.github.io/2018-02-19-build-statcast-database-rstats/
library(dplyr)
library(tidyverse)
library(baseballr)

#Scraping the data from stat cast
#mlb2020 <- scrape_statcast_savant_pitcher_date("2020-07-23","2020-07-25")

#Saving and loading the files to my local machine.
#save(mlb2020, file = "/Users/gregorymatthews/Dropbox/Talks/openWARLoyolaHighSchool/mlb2020.RData")
#load("/Users/gregorymatthews/Dropbox/Talks/openWARLoyolaHighSchool/mlb2020.RData")







library(ggplot2)
ggplot(aes(x = release_speed, fill = pitch_name), data = mlb2020) +
  geom_boxplot() 



library(ggplot2)
pitches <- c("Curveball","4-Seam Fastball","Slider","Sinker")
mlb2020 %>% 
  subset(pitch_name %in% pitches) %>% 
  ggplot(aes(x = release_speed))  + 
  geom_density(aes(x = release_speed, after_stat(count)), alpha = 0.5) + 
  facet_wrap(~pitch_name) + theme_bw()
#Claps for Quang Nguyen for suggesting theme_bw()


library(ggplot2)
mlb2020 %>%
  subset(events %in% c("single","double","triple","home_run")) %>%
  ggplot(aes(y = -hc_y, x = hc_x, color = events)) +
  geom_point() + 
  ggtitle("hits")


library(ggplot2)
mlb2020 %>% 
  subset(events %in% c("single","double","triple","home_run")) %>%
  ggplot(aes(y = -hc_y, x = hc_x, color = events)) + 
  geom_point() + 
  ggtitle("hits")


library(ggplot2)
mlb2020 %>% 
  subset(description %in% c("ball","called_strike")) %>% 
  ggplot(aes(x = plate_x, y = plate_z, color = description)) +
  geom_point() + xlim(-4,4) + ylim(-2,8)







##########################################
#openWAR
##########################################


  #Paper link: https://arxiv.org/abs/1312.7158
  install.packages("xslt")
#Package with functions
devtools::install_github("beanumber/openWAR")
#Package containing the data
devtools::install_github("beanumber/openWARData")
library(openWAR)
ds = getData(start = "2013-06-24")
dim(ds)
head(ds$description)

  library(openWAR)
  library(openWARData)
dim(MLBAM2017)
head(MLBAM2017$description)



  library(openWARData)
data(MLBAM2013)
plot(subset(MLBAM2013, event %in% c("Single","Double")))

#Fenway hits
data(MLBAM2013)
plot(subset(MLBAM2013, stadium%in%c("Fenway Park")))

#Wrigley Hits
data(MLBAM2013)
plot(subset(MLBAM2013, stadium%in%c("Wrigley Field")))


##########################################
#Astros Cheating Scandal
##########################################
  #Github repo: https://github.com/gjm112/Astros_sign_stealing
  #Paper: http://bit.ly/Astros_Cheat
  library(RCurl)
#Bangs file 
bangs <- read.csv("https://raw.githubusercontent.com/gjm11/Astros_sign_stealing/master/data/astros_bangs_20200127.csv")

#Bangs file combined with pitchinfo.com data.  
githubURL <- ("https://github.com/gjm112/Astros_sign_stealing/blob/master/data/bangs-merged-final.rds?raw=true")
download.file(githubURL,"/Users/gregorymatthews/bangs-merged-final.rds", mode = "wb")
bangs_merged_final <- readRDS(file = "/Users/gregorymatthews/bangs-merged-final.rds")

  df <- bangs_merged_final
players <- df %>%
  group_by(batter) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(9) %>%
  pull(batter) %>%
  as.character()

p <- df %>%
  filter(batter %in% players) %>%
  group_by(batter, has_bangs) %>%
  summarize(n = n()) %>%
  ggplot(., aes(x = reorder(batter, n), y = n, fill = has_bangs)) 

p + geom_bar(stat = "identity") +
  scale_fill_manual("bangs", values = c("#002D62", "#EB6E1F"),
                    labels = c("no", "yes")) +
  labs(y = "number of pitches",
       x = "") +
  theme_bw() +
  coord_flip()


