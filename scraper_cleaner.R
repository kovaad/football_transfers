#clean environment
rm(list =ls())

#install required packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(AER,tidyverse,lspline, fixest, modelsummary, ggpubr, reshape2, kableExtra, dplyr, ggcorrplot, data.table)

devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)

#TODO

#using team_urls get all links 
#using tm_team_transfers get all transfers 
#get all the transfers and create dataframe of transfers and loans

#create network between countries and clubs - all to be visualized 
#network metrics to be calculated

#subsequent success to be examined also on two levels - team level and player level



# Data prep ---------------------------------------------------------------

#download the data

years <- seq(1992, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

df_final <- tibble()
for (year in years) {
  for (country in countries) {
    tryCatch({
    team_urls <- tm_league_team_urls(country_name = country, start_year = year)
    transfers <- tm_team_transfers(team_url = team_urls, transfer_window = "all")
    df_final <- bind_rows(df_final, transfers)
    print(c(country, year))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

#cleaning the data

View(df_final %>% filter( is.na(team_name) ))

df_final <- df_final %>% mutate(country = ifelse(is.na(country) & (team_name == "VfB Leipzig"), "Germany", country ))

df_final <- df_final %>% mutate(country = ifelse(is.na(country) & (team_name == "AS Cannes"), "France", country ))

df_final <- df_final %>% mutate(country = ifelse(is.na(country) & (team_name == "FC Gueugnon"), "France", country ))

df_final <- df_final %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final$player_age <- as.numeric(df_final$player_age)

write.csv(df_final,"data/df_final.csv", row.names = FALSE)

View(df_final2 %>% filter( is.na(country) ))

df_final2 <- df_final2 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final2$player_age <- as.numeric(df_final2$player_age)

write.csv(df_final2,"data/df_final2.csv", row.names = FALSE)

View(df_final3 %>% filter( is.na(country) ))

df_final3 <- df_final3 %>% mutate(country = ifelse(is.na(country) & (team_name == "Chievo Verona"), "Italy", country ))

df_final3 <- df_final3 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final3$player_age <- as.numeric(df_final3$player_age)

write.csv(df_final3,"data/df_final3.csv", row.names = FALSE)

View(df_final4 %>% filter( is.na(country) ))

df_final4 <- df_final4 %>% mutate(country = ifelse(is.na(country) & (team_name == "Chievo Verona"), "Italy", country ))

df_final4 <- df_final4 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final4$player_age <- as.numeric(df_final4$player_age)

write.csv(df_final4,"data/df_final4.csv", row.names = FALSE)

View(df_final5 %>% filter( is.na(country) ))

df_final5 <- df_final5 %>% mutate(country = ifelse(is.na(country) & (team_name == "Chievo Verona"), "Italy", country ))

df_final5 <- df_final5 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final5$player_age <- as.numeric(df_final5$player_age)

write.csv(df_final5,"data/df_final5.csv", row.names = FALSE)

View(df_final6 %>% filter( is.na(country) ))

df_final6 <- df_final6 %>% mutate(country = ifelse(is.na(country) & (team_name == "Chievo Verona"), "Italy", country ))

df_final6 <- df_final6 %>% mutate(country = ifelse(is.na(country) & (team_name == "Istres Football Club"), "France", country ))

df_final6 <- df_final6 %>% mutate(country = ifelse(is.na(country) & (team_name == "US Livorno 1915"), "Italy", country ))

df_final6 <- df_final6 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final6$player_age <- as.numeric(df_final6$player_age)

write.csv(df_final6,"data/df_final6.csv", row.names = FALSE)

View(df_final7 %>% filter( is.na(country) ))

df_final7 <- df_final7 %>% mutate(country = ifelse(is.na(country) & (team_name == "US Livorno 1915"), "Italy", country ))

df_final7 <- df_final7 %>% mutate(country = ifelse(is.na(country) & (team_name == "Chievo Verona"), "Italy", country ))

df_final7 <- df_final7 %>% mutate(country = ifelse(is.na(country) & (team_name == "ACD Treviso"), "Italy", country ))

df_final7 <- df_final7 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final7$player_age <- as.numeric(df_final7$player_age)

write.csv(df_final7,"data/df_final7.csv", row.names = FALSE)

View(df_final8 %>% filter( is.na(country) ))

df_final8 <- df_final8 %>% mutate(country = ifelse(is.na(country) & (team_name == "US Livorno 1915"), "Italy", country ))

df_final8 <- df_final8 %>% mutate(country = ifelse(is.na(country) & (team_name == "Chievo Verona"), "Italy", country ))

df_final8 <- df_final8 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final8$player_age <- as.numeric(df_final8$player_age)

write.csv(df_final8,"data/df_final8.csv", row.names = FALSE)

View(df_final9 %>% filter( is.na(country) ))

df_final9 <- df_final9 %>% mutate(country = ifelse(is.na(country) & (team_name == "Athlétic Club Arlésien"), "France", country ))

df_final9 <- df_final9 %>% mutate(country = ifelse(is.na(country) & (team_name == "Chievo Verona"), "Italy", country ))

df_final9 <- df_final9 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final9$player_age <- as.numeric(df_final9$player_age)

write.csv(df_final9,"data/df_final9.csv", row.names = FALSE)

View(df_final10 %>% filter( is.na(country) ))

df_final10 <- df_final10 %>% mutate(country = ifelse(is.na(country) & (team_name == "Thonon Évian Grand Genève FC"), "France", country ))

df_final10 <- df_final10 %>% mutate(country = ifelse(is.na(country) & (team_name == "Chievo Verona"), "Italy", country ))

df_final10 <- df_final10 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final10$player_age <- as.numeric(df_final10$player_age)

write.csv(df_final10,"data/df_final10.csv", row.names = FALSE)

View(df_final11 %>% filter( is.na(country) ))

df_final11 <- df_final11 %>% mutate(country = ifelse(is.na(country) & (team_name == "Thonon Évian Grand Genève FC"), "France", country ))

df_final11 <- df_final11 %>% mutate(country = ifelse(is.na(country) & (team_name == "Chievo Verona"), "Italy", country ))

df_final11 <- df_final11 %>% mutate(country = ifelse(is.na(country) & (team_name == "US Livorno 1915"), "Italy", country ))

df_final11 <- df_final11 %>% mutate(country = ifelse(is.na(country) & (team_name == "GFC Ajaccio"), "France", country ))

df_final11 <- df_final11 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final11$player_age <- as.numeric(df_final11$player_age)

write.csv(df_final11,"data/df_final11.csv", row.names = FALSE)

View(df_final12 %>% filter( is.na(country) ))

df_final12 <- df_final12 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final12$player_age <- as.numeric(df_final12$player_age)

write.csv(df_final12,"data/df_final12.csv", row.names = FALSE)

View(df_final13 %>% filter( is.na(country) ))

df_final13 <- df_final13 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final13$player_age <- as.numeric(df_final13$player_age)

write.csv(df_final13,"data/df_final13.csv", row.names = FALSE)

View(df_final14 %>% filter( is.na(country) ))

df_final14 <- df_final14 %>% mutate(country = ifelse(is.na(country), "Spain", country ))

df_final14 <- df_final14 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final14$player_age <- as.numeric(df_final14$player_age)

write.csv(df_final14,"data/df_final14.csv", row.names = FALSE)

View(df_final15 %>% filter( is.na(country) ))

df_final15 <- df_final15 %>% mutate(country = ifelse(is.na(country), "Spain", country ))

df_final15 <- df_final15 %>% mutate(player_age = ifelse(player_age %in% c("-", "N/A"), NA, player_age ))

df_final15$player_age <- as.numeric(df_final15$player_age)

write.csv(df_final15,"data/df_final15.csv", row.names = FALSE)

#putting it all together into one df

nums <- c("", seq(2,15,1))

df <- tibble()
for (num in nums) {
  temp <- read_csv(paste0("data/df_final", num, ".csv"))
  df <- bind_rows(df, temp)
  print(num)
}

View(df)

write.csv(df,"data/df.csv", row.names = FALSE)

#NOTE: there was a problem with Spain 1992 team_urls[13] FC Burgos no transfers available - no data - left it out

df <- read_csv("data/df.csv")

#league tables

years <- seq(1992, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

df_tables <- tibble()
for (year in years) {
  for (country in countries) {
    tryCatch({
      temp <- tm_matchday_table(country_name=country, start_year=year, matchday=50)
      temp <- temp %>% mutate(season = year)
      df_tables <- bind_rows(df_tables, temp)
      print(c(country, year))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

View(df_tables %>% filter( is.na(pts) ))

df_tables <- df_tables %>% mutate(pts = ifelse(is.na(pts), 2 * w + d ,pts))

#add 2 point for win points

#1992 - germany,france,italy,spain
#1993 - germany,france,italy,spain
#1994 - germany,france,spain

#Wimbledon 1997 is a major bug in the data - only 21 matches recorded -- need to be fixed !!!
# see real position here: https://en.wikipedia.org/wiki/1997–98_Wimbledon_F.C._season

setDT(df_tables)

df_tables[squad == "Wimbledon" & season == 1997 ][,4] <- 15

df_tables[squad == "Wimbledon" & season == 1997][,6] <- 38

df_tables[squad == "Wimbledon" & season == 1997][,7] <- 10

df_tables[squad == "Wimbledon" & season == 1997][,8] <- 14

df_tables[squad == "Wimbledon" & season == 1997][,9] <- 14

df_tables[squad == "Wimbledon" & season == 1997][,10] <- 34

df_tables[squad == "Wimbledon" & season == 1997][,11] <- 46

df_tables[squad == "Wimbledon" & season == 1997][,12] <- -12

df_tables[squad == "Wimbledon" & season == 1997][,13] <- 44

df_tables[squad == "Sheff Wed" & season == 1997][,4] <- 16

df_tables[squad == "Everton" & season == 1997][,4] <- 17

df_tables[squad == "Bolton" & season == 1997][,4] <- 18

df_tables[squad == "Barnsley FC" & season == 1997][,4] <- 19

df_tables[squad == "Crystal Palace" & season == 1997][,4] <- 20

#further issue - 2019 season in France was terminated 

write.csv(df_tables,"data/df_tables.csv", row.names = FALSE)

#Note: 2021 current league table used here - may exclude from final analysis as season not yet finished

df_tables <- read_csv("data/df_tables.csv")

View(df_tables)

###
#download also halfway tables - could be important for winter signings 
    #-- then look at improvement of performance i.e. change in position, points etc.

years <- seq(1992, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

df_tables_all <- tibble()
for (year in years) {
  for (country in countries) {
    tryCatch({
      temp <- tm_matchday_table(country_name=country, start_year=year, matchday=50)
      temp <- temp %>% mutate(season = year)
      temp <- temp %>% mutate(period = "final")
      temp2 <- tm_matchday_table(country_name=country, start_year=year, matchday=ifelse(temp$p[1] %in% c(42,34,38),temp$p[1]/2,ifelse(temp$country %in% c("England", "France", "Italy", "Spain"),19,17)))
      temp2 <- temp2 %>% mutate(season = year)
      temp2 <- temp2 %>% mutate(period = "mid")
      df_tables_all <- bind_rows(df_tables_all, temp)
      df_tables_all <- bind_rows(df_tables_all, temp2)
      print(c(country, year))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

df_tables_all <- df_tables_all %>% mutate(pts = ifelse(is.na(pts), 2 * w + d ,pts))

#mitigate problem with Wimbledon fc 1997 season
library(data.table)
setDT(df_tables_all)

df_tables_all[squad == "Wimbledon" & season == 1997 & period == "final"][,4] <- 15

df_tables_all[squad == "Wimbledon" & season == 1997 & period == "final"][,6] <- 38

df_tables_all[squad == "Wimbledon" & season == 1997 & period == "final"][,7] <- 10

df_tables_all[squad == "Wimbledon" & season == 1997 & period == "final"][,8] <- 14

df_tables_all[squad == "Wimbledon" & season == 1997 & period == "final"][,9] <- 14

df_tables_all[squad == "Wimbledon" & season == 1997 & period == "final"][,10] <- 34

df_tables_all[squad == "Wimbledon" & season == 1997 & period == "final"][,11] <- 46

df_tables_all[squad == "Wimbledon" & season == 1997 & period == "final"][,12] <- -12

df_tables_all[squad == "Wimbledon" & season == 1997 & period == "final"][,13] <- 44

df_tables_all[squad == "Sheff Wed" & season == 1997 & period == "final"][,4] <- 16

df_tables_all[squad == "Everton" & season == 1997 & period == "final"][,4] <- 17

df_tables_all[squad == "Bolton" & season == 1997 & period == "final"][,4] <- 18

df_tables_all[squad == "Barnsley FC" & season == 1997 & period == "final"][,4] <- 19

df_tables_all[squad == "Crystal Palace" & season == 1997 & period == "final"][,4] <- 20

#once also in the Premier League everybody has 19 matches played in 2021 season needs to be adjusted by hand like this
#THIS IS STILL IMPORTANT AND TO BE DONE
#also in other leagues minor adjusments due to this needed

df_tables_all[squad == "Salernitana" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Salernitana" & season == 2021 & period == "mid"][,7] <- 3

df_tables_all[squad == "Salernitana" & season == 2021 & period == "mid"][,10] <- 13

df_tables_all[squad == "Salernitana" & season == 2021 & period == "mid"][,11] <- 43

df_tables_all[squad == "Salernitana" & season == 2021 & period == "mid"][,12] <- -30

df_tables_all[squad == "Salernitana" & season == 2021 & period == "mid"][,13] <- 11

df_tables_all[squad == "Udinese Calcio" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Udinese Calcio" & season == 2021 & period == "mid"][,9] <- 7

df_tables_all[squad == "Udinese Calcio" & season == 2021 & period == "mid"][,10] <- 28

df_tables_all[squad == "Udinese Calcio" & season == 2021 & period == "mid"][,11] <- 34

df_tables_all[squad == "Udinese Calcio" & season == 2021 & period == "mid"][,12] <- -6

df_tables_all[squad == "Cagliari Calcio" & season == 2021 & period == "mid"][,4] <- 20

df_tables_all[squad == "Salernitana" & season == 2021 & period == "mid"][,4] <- 19

df_tables_all[squad == "Liverpool" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Liverpool" & season == 2021 & period == "mid"][,9] <- 2

df_tables_all[squad == "Liverpool" & season == 2021 & period == "mid"][,11] <- 16

df_tables_all[squad == "Liverpool" & season == 2021 & period == "mid"][,12] <- 34

df_tables_all[squad == "Spurs" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Spurs" & season == 2021 & period == "mid"][,7] <- 11

df_tables_all[squad == "Spurs" & season == 2021 & period == "mid"][,8] <- 3

df_tables_all[squad == "Spurs" & season == 2021 & period == "mid"][,10] <- 26

df_tables_all[squad == "Spurs" & season == 2021 & period == "mid"][,11] <- 22

df_tables_all[squad == "Spurs" & season == 2021 & period == "mid"][,12] <- 4

df_tables_all[squad == "Spurs" & season == 2021 & period == "mid"][,13] <- 36

df_tables_all[squad == "Wolves" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Wolves" & season == 2021 & period == "mid"][,7] <- 8

df_tables_all[squad == "Wolves" & season == 2021 & period == "mid"][,10] <- 14

df_tables_all[squad == "Wolves" & season == 2021 & period == "mid"][,12] <- 0

df_tables_all[squad == "Wolves" & season == 2021 & period == "mid"][,13] <- 28

df_tables_all[squad == "Brighton" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Brighton" & season == 2021 & period == "mid"][,7] <- 5

df_tables_all[squad == "Brighton" & season == 2021 & period == "mid"][,8] <- 9

df_tables_all[squad == "Brighton" & season == 2021 & period == "mid"][,10] <- 17

df_tables_all[squad == "Brighton" & season == 2021 & period == "mid"][,11] <- 20

df_tables_all[squad == "Brighton" & season == 2021 & period == "mid"][,12] <- -3

df_tables_all[squad == "Brighton" & season == 2021 & period == "mid"][,13] <- 24

df_tables_all[squad == "Leicester" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Leicester" & season == 2021 & period == "mid"][,7] <- 7

df_tables_all[squad == "Leicester" & season == 2021 & period == "mid"][,10] <- 33

df_tables_all[squad == "Leicester" & season == 2021 & period == "mid"][,12] <- -3

df_tables_all[squad == "Leicester" & season == 2021 & period == "mid"][,13] <- 25

df_tables_all[squad == "Aston Villa" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Aston Villa" & season == 2021 & period == "mid"][,9] <- 11

df_tables_all[squad == "Aston Villa" & season == 2021 & period == "mid"][,10] <- 25

df_tables_all[squad == "Aston Villa" & season == 2021 & period == "mid"][,11] <- 30

df_tables_all[squad == "Aston Villa" & season == 2021 & period == "mid"][,12] <- -5

df_tables_all[squad == "Crystal Palace" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Crystal Palace" & season == 2021 & period == "mid"][,7] <- 5

df_tables_all[squad == "Crystal Palace" & season == 2021 & period == "mid"][,10] <- 27

df_tables_all[squad == "Crystal Palace" & season == 2021 & period == "mid"][,11] <- 27

df_tables_all[squad == "Crystal Palace" & season == 2021 & period == "mid"][,12] <- 0

df_tables_all[squad == "Crystal Palace" & season == 2021 & period == "mid"][,13] <- 23

df_tables_all[squad == "Everton" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Everton" & season == 2021 & period == "mid"][,9] <- 10

df_tables_all[squad == "Everton" & season == 2021 & period == "mid"][,10] <- 24

df_tables_all[squad == "Everton" & season == 2021 & period == "mid"][,11] <- 34

df_tables_all[squad == "Everton" & season == 2021 & period == "mid"][,12] <- -10

df_tables_all[squad == "Leeds" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Leeds" & season == 2021 & period == "mid"][,7] <- 4

df_tables_all[squad == "Leeds" & season == 2021 & period == "mid"][,10] <- 21

df_tables_all[squad == "Leeds" & season == 2021 & period == "mid"][,11] <- 37

df_tables_all[squad == "Leeds" & season == 2021 & period == "mid"][,12] <- -16

df_tables_all[squad == "Leeds" & season == 2021 & period == "mid"][,13] <- 19

df_tables_all[squad == "Watford" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Watford" & season == 2021 & period == "mid"][,9] <- 13

df_tables_all[squad == "Watford" & season == 2021 & period == "mid"][,10] <- 22

df_tables_all[squad == "Watford" & season == 2021 & period == "mid"][,11] <- 36

df_tables_all[squad == "Watford" & season == 2021 & period == "mid"][,12] <- -14

df_tables_all[squad == "Burnley" & season == 2021 & period == "mid"][,6] <- 19

df_tables_all[squad == "Burnley" & season == 2021 & period == "mid"][,8] <- 10

df_tables_all[squad == "Burnley" & season == 2021 & period == "mid"][,9] <- 8

df_tables_all[squad == "Burnley" & season == 2021 & period == "mid"][,10] <- 16

df_tables_all[squad == "Burnley" & season == 2021 & period == "mid"][,11] <- 27

df_tables_all[squad == "Burnley" & season == 2021 & period == "mid"][,12] <- -11

df_tables_all[squad == "Burnley" & season == 2021 & period == "mid"][,13] <- 13

df_tables_all[squad == "Spurs" & season == 2021 & period == "mid"][,4] <- 4

df_tables_all[squad == "Arsenal" & season == 2021 & period == "mid"][,4] <- 5

df_tables_all[squad == "Man Utd" & season == 2021 & period == "mid"][,4] <- 6

df_tables_all[squad == "Leicester" & season == 2021 & period == "mid"][,4] <- 9

df_tables_all[squad == "Brighton" & season == 2021 & period == "mid"][,4] <- 10

df_tables_all[squad == "Crystal Palace" & season == 2021 & period == "mid"][,4] <- 11

df_tables_all[squad == "Southampton" & season == 2021 & period == "mid"][,4] <- 12

df_tables_all[squad == "Aston Villa" & season == 2021 & period == "mid"][,4] <- 13

#replace names where different than in df

df_tables_all[squad == "Milan"][,5] <- "AC Milan"

df_tables_all[squad == "AC Parma"][,5] <- "Parma"

df_tables_all[squad == "Atalanta"][,5] <- "Atalanta BC"

df_tables_all[squad == "Napoli"][,5] <- "SSC Napoli"

df_tables_all[squad == "Udinese"][,5] <- "Udinese Calcio"

df_tables_all[squad == "Empoli"][,5] <- "FC Empoli"

df_tables_all[squad == "AC Venezia 1907"][,5] <- "Venezia"

df_tables_all[squad == "Wimbledon"][,5] <- "MK Dons"

#TODO Delete final tables for 2021/22 leagues

df_tables_all <- df_tables_all[ !(season == 2021 & period == 'final')] 

#otherwise this is done now -- save
write.csv(df_tables_all,"data/df_tables_all.csv", row.names = FALSE)

df_tables_all <- read_csv("data/df_tables_all.csv")

View(df_tables_all)



# Merge with ranking tables -----------------------------------------------

#names of teams not the same for all teams - need to solve this

#look for different ones

data <- sort(setdiff(unique(df$team_name),unique(df_tables$squad)))

data <- tibble(data)

data2 <- sort(setdiff(unique(df_tables$squad),unique(df$team_name)))

data2 <- tibble(data2)

#save for later comparison
write.csv(data2,"data/names.csv", row.names = FALSE)

#using adist fuzzy match closest strings
data$name2 <- data2$data2[apply(adist(data$data, data2$data2), 1, which.min)]

#correct incorrect matches by hand

data <- data %>% mutate(name2 = ifelse(data == "AC Perugia Calcio", "Perugia", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "AC Reggiana 1919", "Reggiana", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "ACN Siena 1904", "Siena", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "ACR Messina", "Messina Peloro", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Athletic Carpi 2021", "Carpi", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Athlétic Club Arlésien", "AC Arles", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Bayern Munich", "FC Bayern", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Charlton Athletic", "Charlton", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Delfino Pescara 1936", "Pescara", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "ESTAC Troyes", "Troyes", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Istres Football Club", "FC Istres", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Milton Keynes Dons", "MK Dons", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Palermo FC", "US Palermo", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Parma Calcio 1913", "Parma", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Queens Park Rangers", "QPR", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "RCD Espanyol Barcelona", "Espanyol", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Sporting Club de Toulon", "SC Toulon Var", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Swansea City", "Swansea", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Thonon Évian Grand Genève FC", "Évian", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Tottenham Hotspur", "Spurs", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Wigan Athletic", "Wigan", name2 ))

data <- data %>% mutate(name2 = ifelse(data == "Wolverhampton Wanderers", "Wolves", name2 ))

View(data)

#using this helper dataframe create new team names column with these shorter team names

View(df %>% mutate(team_name_mod = ifelse(df$team_name == data$data, data$name2, df$team_name)))

names(data) <- c("team_name","team_name2")

data

write.csv(data,"data/names_helper.csv", row.names = FALSE)

data <- read_csv("data/names_helper.csv")

merged <- merge(df, data, by = "team_name", all.x = TRUE)

merged <- merged %>% mutate(team_name2 = ifelse(is.na(team_name2), team_name, team_name2))

View(merged)

write.csv(merged,"data/df_name_corr.csv", row.names = FALSE)

merged <- read_csv("data/df_name_corr.csv")

#potential further resources

years <- seq(1992, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

transfer_balances <- tibble()
for (year in years) {
  for (country in countries) {
    temp <- tm_team_transfer_balances(country_name = country, start_year = year) 
    temp <- temp %>% mutate(year = year)
    transfer_balances <- bind_rows(transfer_balances, temp)
  }
}

write.csv(transfer_balances,"data/transfer_balances1.csv", row.names = FALSE)

years <- seq(2018, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

transfer_balances2 <- tibble()
for (year in years) {
  for (country in countries) {
    temp <- tm_team_transfer_balances(country_name = country, start_year = year) 
    temp <- temp %>% mutate(year = year)
    transfer_balances2 <- bind_rows(transfer_balances2, temp)
    print(year)
  }
}

write.csv(transfer_balances2,"data/transfer_balances2.csv", row.names = FALSE)

transfer_balances3 <- tibble()
  for (country in countries) {
    temp <- tm_team_transfer_balances(country_name = country, start_year = 2021) 
    temp <- temp %>% mutate(year = 2021)
    transfer_balances3 <- bind_rows(transfer_balances3, temp)
    print(year)
}

write.csv(transfer_balances3,"data/transfer_balances3.csv", row.names = FALSE)

#merge these

nums <- c(seq(1,3,1))

transfer_balance_df <- tibble()
for (num in nums) {
  temp <- read_csv(paste0("data/transfer_balances", num, ".csv"))
  transfer_balance_df <- bind_rows(transfer_balance_df, temp)
  print(num)
}

View(transfer_balance_df)

transfer_balance_df <- setDT(transfer_balance_df)

to_filter <- sapply(transfer_balance_df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

transfer_balance_df[is.na(expenditure_euros)]

#correct Burgos and save




years <- seq(1992, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

player_market_values <- tibble()
for (year in years) {
  for (country in countries) {
    temp <- get_player_market_values(country_name = country, start_year = year) 
    temp <- temp %>% mutate(year = year)
    player_market_values <- bind_rows(player_market_values, temp)
  }
}
    

write.csv(player_market_values,"data/player_market_values1.csv", row.names = FALSE)

#next batch to be downloaded

years <- seq(2006, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

player_market_values2 <- tibble()
for (year in years) {
  for (country in countries) {
    temp <- get_player_market_values(country_name = country, start_year = year) 
    temp <- temp %>% mutate(year = year)
    player_market_values2 <- bind_rows(player_market_values2, temp)
  }
}

write.csv(player_market_values2,"data/player_market_values2.csv", row.names = FALSE)

years <- seq(2009, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

player_market_values3 <- tibble()
for (year in years) {
  for (country in countries) {
    temp <- get_player_market_values(country_name = country, start_year = year) 
    temp <- temp %>% mutate(year = year)
    player_market_values3 <- bind_rows(player_market_values3, temp)
    print(year)
  }
}

write.csv(player_market_values3,"data/player_market_values3.csv", row.names = FALSE)

years <- seq(2019, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

player_market_values4 <- tibble()
for (year in years) {
  for (country in countries) {
    temp <- get_player_market_values(country_name = country, start_year = year) 
    temp <- temp %>% mutate(year = year)
    player_market_values4 <- bind_rows(player_market_values4, temp)
    print(year)
  }
}

write.csv(player_market_values4,"data/player_market_values4.csv", row.names = FALSE)

years <- seq(2020, 2021,1)

countries <- c("England", "Germany", "France", "Italy", "Spain")

player_market_values5 <- tibble()
for (year in years) {
  for (country in countries) {
    temp <- get_player_market_values(country_name = country, start_year = year) 
    temp <- temp %>% mutate(year = year)
    player_market_values5 <- bind_rows(player_market_values5, temp)
    print(year)
  }
}

write.csv(player_market_values5,"data/player_market_values5.csv", row.names = FALSE)

#merge these

nums <- c(seq(1,5,1))

player_market_values_df <- tibble()
for (num in nums) {
  temp <- read_csv(paste0("data/player_market_values", num, ".csv"))
  player_market_values_df <- bind_rows(player_market_values_df, temp)
  print(num)
}

View(player_market_values_df)

#create teamwise table 

team_market_values_df <- player_market_values_df|>
  dplyr::group_by(squad, year) |> 
  dplyr::summarize(market_value = sum(player_market_value_euro, na.rm = T), avg_age = mean(player_age, na.rm = T), avg_height = mean(player_height_mtrs, na.rm = T))

View(team_market_values_df)

#before 2004 market values of players not available -- all is set to 0 -- cannot use for regression

team_market_values_df <- team_market_values_df |>  
  rename(team_name = squad)

team_market_values_df_merged <- merge(team_market_values_df, data, by = "team_name", all.x = TRUE)

team_market_values_df_merged <- team_market_values_df_merged %>% mutate(team_name2 = ifelse(is.na(team_name2), team_name, team_name2))

#save

write.csv(team_market_values_df_merged,"data/team_market_values.csv", row.names = FALSE)

team_market_values <- read_csv("data/team_market_values.csv")

#more advanced stats?
fb_big5_advanced_season_stats(season_end_year= c(2019:2021), stat_type= "shooting", team_or_player= "team")


