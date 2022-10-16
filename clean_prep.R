#clean environment
rm(list =ls())

#install required packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, dplyr, data.table, igraph, fixest)

#import raw data
df <- read_csv("data/df_name_corr.csv")

edges <- read_csv("data/df_name_corr.csv")

df <- as.data.table(df)

#data prep - cleaning and filtering

#filter on transfers

transfer_all <- df[is_loan == FALSE]

to_filter <- sapply(transfer_all, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

loan_all <- df[is_loan == TRUE]

edgelist_all <- df[,.(team_name2, club_2, transfer_fee, player_age,minutes_played,goals, season, window, player_name, transfer_type, country, country_2, is_loan)]

to_filter <- sapply(edgelist_all, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

#create separate edgelists for all transfer windows

for (year in unique(edgelist_all$season)) {
  assign(paste0("edgelist_all_sum_", as.character(year)),edgelist_all[season == year & window == "Summer"])
  assign(paste0("edgelist_all_wint_", as.character(year)),edgelist_all[season == year & window == "Winter"])
}

#NAs in club_2 column refer to retirement, replace NA in country_2 with Unknown

for (year in unique(edgelist_all$season)) {
  temp <- get(paste0("edgelist_all_sum_", year))
  temp <- temp %>% mutate(club_2 = ifelse(is.na(club_2) & (is.na(minutes_played) | minutes_played == 0) & player_age >= 30,"Retired", club_2), 
                          country_2 = ifelse(is.na(country_2),"Unknown", country_2))
  temp <- subset(temp,!(is.na(club_2) & is.na(minutes_played | minutes_played == 0) & is.na(transfer_fee) | player_age > 30))
  assign(paste0("edgelist_all_sum_", as.character(year)),temp)

  temp2 <- get(paste0("edgelist_all_wint_", year))
  temp2 <- temp2 %>% mutate(club_2 = ifelse(is.na(club_2) & (is.na(minutes_played)| minutes_played == 0) & player_age >= 30,"Retired", club_2), 
                            country_2 = ifelse(is.na(country_2),"Unknown", country_2))
  temp2 <- subset(temp2,!(is.na(club_2) & is.na(minutes_played | minutes_played == 0) & is.na(transfer_fee) | player_age > 30))
  assign(paste0("edgelist_all_wint_", as.character(year)),temp2)
}

#deal with remainders if needed
for (year in unique(edgelist_all$season)) {
  to_filter <- sapply(get(paste0("edgelist_all_sum_", year)), function(x) sum(is.na(x)))
  print(year)
  print(to_filter[to_filter > 0])
}

edgelist_all_sum_1999 <- subset(edgelist_all_sum_1999,!(is.na(club_2)))

edgelist_all_sum_1994 <- subset(edgelist_all_sum_1994,!(is.na(club_2)))

edgelist_all_sum_2015[is.na(club_2),10] <- "Arrivals"

edgelist_all_sum_2015[is.na(club_2),1] <- "Bay. Leverkusen"

edgelist_all_sum_2015[is.na(club_2),2] <- "E. Frankfurt"

edgelist_all_sum_1996 <- subset(edgelist_all_sum_1996,!(is.na(club_2)))

edgelist_all_sum_2008 <- subset(edgelist_all_sum_2008,!(is.na(club_2)))

edgelist_all_sum_2000 <- subset(edgelist_all_sum_2000,!(is.na(club_2)))

edgelist_all_sum_2003 <- subset(edgelist_all_sum_2003,!(is.na(club_2)))

edgelist_all_sum_2017[is.na(club_2),2] <- "Retired"

edgelist_all_sum_2002 <- subset(edgelist_all_sum_2002,!(is.na(club_2)))

for (year in unique(edgelist_all$season)) {
  to_filter <- sapply(get(paste0("edgelist_all_wint_", year)), function(x) sum(is.na(x)))
  print(year)
  print(to_filter[to_filter > 0])
}

edgelist_all_wint_1993 <- subset(edgelist_all_wint_1993,!(is.na(club_2)))

edgelist_all_wint_1995 <- subset(edgelist_all_wint_1995,!(is.na(club_2)))

edgelist_all_wint_2015 <- subset(edgelist_all_wint_2015,!(is.na(club_2)))

edgelist_all_wint_1996 <- subset(edgelist_all_wint_1996,!(is.na(club_2)))

edgelist_all_wint_2011 <- subset(edgelist_all_wint_2011,!(is.na(club_2)))

edgelist_all_wint_2009[is.na(club_2) & team_name2 == "Espanyol",2] <- "Panthrakikos"

edgelist_all_wint_2009[is.na(club_2) & team_name2 == "Espanyol",12] <- "Greece"

edgelist_all_wint_2009[is.na(club_2) & team_name2 == "Espanyol",13] <- TRUE

edgelist_all_wint_2009 <- subset(edgelist_all_wint_2009,!(is.na(club_2)))

edgelist_all_wint_2000 <- subset(edgelist_all_wint_2000,!(is.na(club_2)))

edgelist_all_wint_2005 <- subset(edgelist_all_wint_2005,!(is.na(club_2)))

edgelist_all_wint_2003 <- subset(edgelist_all_wint_2003,!(is.na(club_2)))

edgelist_all_wint_1999 <- subset(edgelist_all_wint_1999,!(is.na(club_2)))

edgelist_all_wint_2006 <- subset(edgelist_all_wint_2006,!(is.na(club_2)))

edgelist_all_wint_2012 <- subset(edgelist_all_wint_2012,!(is.na(club_2)))

edgelist_all_wint_2018 <- subset(edgelist_all_wint_2018,!(is.na(club_2)))

#deal with NAs in transfer fees - where loan or retired -> 0

for (year in unique(edgelist_all$season)) {
  temp <- get(paste0("edgelist_all_sum_", year))
  temp <- temp %>% mutate(transfer_fee = ifelse(is.na(transfer_fee) & (is_loan == TRUE | club_2 == "Retired"),0, transfer_fee))
  assign(paste0("edgelist_all_sum_", as.character(year)),temp)
  
  temp2 <- get(paste0("edgelist_all_wint_", year))
  temp2 <- temp2 %>% mutate(transfer_fee = ifelse(is.na(transfer_fee) & (is_loan == TRUE | club_2 == "Retired"),0, transfer_fee))
  assign(paste0("edgelist_all_wint_", as.character(year)),temp2)
}

for (year in unique(edgelist_all$season)) {
  to_filter <- sapply(get(paste0("edgelist_all_sum_", year)), function(x) sum(is.na(x)))
  print(year)
  print(to_filter[to_filter > 0])
}


for (year in unique(edgelist_all$season)) {
  to_filter <- sapply(get(paste0("edgelist_all_wint_", year)), function(x) sum(is.na(x)))
  print(year)
  print(to_filter[to_filter > 0])
}

#Next cleaning step -- if the club_2 has U17, U18, U19, U20, U21, B or II in its name, then 0

for (year in unique(edgelist_all$season)) {
  temp <- get(paste0("edgelist_all_sum_", year))
  x <- c("U17", "U18", "U19", "U20", "U21","II", " B")
  temp <- temp %>% mutate(transfer_fee = ifelse(is.na(transfer_fee) & grepl(paste(x, collapse = "|"), club_2),0, transfer_fee))
  assign(paste0("edgelist_all_sum_", as.character(year)),temp)
  
  temp2 <- get(paste0("edgelist_all_wint_", year))
  temp2 <- temp2 %>% mutate(transfer_fee = ifelse(is.na(transfer_fee) & grepl(paste(x, collapse = "|"), club_2),0, transfer_fee))
  assign(paste0("edgelist_all_wint_", as.character(year)),temp2)
}

for (year in unique(edgelist_all$season)) {
  to_filter <- sapply(get(paste0("edgelist_all_sum_", year)), function(x) sum(is.na(x)))
  print(year)
  print(to_filter[to_filter > 0])
}


for (year in unique(edgelist_all$season)) {
  to_filter <- sapply(get(paste0("edgelist_all_wint_", year)), function(x) sum(is.na(x)))
  print(year)
  print(to_filter[to_filter > 0])
}

#another cleaning step -- if someone is below 21 years of age and has no data on transfer fee, minutes played and goals -- all 0

for (year in unique(edgelist_all$season)) {
  temp <- get(paste0("edgelist_all_sum_", year))
  temp <- temp %>% mutate(transfer_fee = ifelse(is.na(transfer_fee) & player_age <21,0, transfer_fee))
  assign(paste0("edgelist_all_sum_", as.character(year)),temp)
  
  temp2 <- get(paste0("edgelist_all_wint_", year))
  temp2 <- temp2 %>% mutate(transfer_fee = ifelse(is.na(transfer_fee) & is.na(minutes_played) & is.na(goals) &  player_age < 21,0, transfer_fee),
                            minutes_played = ifelse(is.na(transfer_fee) & is.na(minutes_played) & is.na(goals) &  player_age < 21,0, minutes_played),
                            goals = ifelse(is.na(transfer_fee) & is.na(minutes_played) & is.na(goals) &  player_age < 21,0, goals))
  assign(paste0("edgelist_all_wint_", as.character(year)),temp2)
}

for (year in unique(edgelist_all$season)) {
  to_filter <- sapply(get(paste0("edgelist_all_sum_", year)), function(x) sum(is.na(x)))
  print(year)
  print(to_filter[to_filter > 0])
}

#replace remainder with 0
for (year in unique(edgelist_all$season)) {
  get(paste0("edgelist_all_sum_", year))[is.na(get(paste0("edgelist_all_sum_", year)))] <- 0
}

for (year in unique(edgelist_all$season)) {
  to_filter <- sapply(get(paste0("edgelist_all_wint_", year)), function(x) sum(is.na(x)))
  print(year)
  print(to_filter[to_filter > 0])
}

#replace remainder with 0
for (year in unique(edgelist_all$season)) {
  get(paste0("edgelist_all_wint_", year))[is.na(get(paste0("edgelist_all_wint_", year)))] <- 0
}

#merge these together

edgelist_all_clean <- data.table()
for (year in unique(edgelist_all$season)) {
  edgelist_all_clean <- bind_rows(edgelist_all_clean,get(paste0("edgelist_all_sum_", as.character(year))))
  edgelist_all_clean <- bind_rows(edgelist_all_clean,get(paste0("edgelist_all_wint_", as.character(year))))
}

#create by team table with node characteristics to be merged
node_chars <- edgelist_all_clean |> group_by(season, window,team_name2) |> summarize(arr_count = length(player_name[transfer_type == "Arrivals"]),
                                                                               dep_count = length(player_name[transfer_type == "Departures"]),
                                                                               arr_mean_age = mean(player_age[transfer_type == "Arrivals"], na.rm = T),
                                                                               dep_mean_age = mean(player_age[transfer_type == "Departures"], na.rm = T),
                                                                               arr_mean_fees = mean(transfer_fee[transfer_type == "Arrivals"], na.rm = T),
                                                                               dep_mean_fees = mean(transfer_fee[transfer_type == "Departures"], na.rm = T),
                                                                               arr_mean_mins = mean(minutes_played[transfer_type == "Arrivals"], na.rm = T),
                                                                               dep_mean_mins = mean(minutes_played[transfer_type == "Departures"], na.rm = T),
                                                                               arr_mean_goals = mean(goals[transfer_type == "Arrivals"], na.rm = T),
                                                                               dep_mean_goals = mean(goals[transfer_type == "Departures"], na.rm = T),
                                                                               arr_share_loan = sum(is_loan[transfer_type == "Arrivals"]/length(player_name[transfer_type == "Arrivals"])),
                                                                               dep_share_loan = sum(is_loan[transfer_type == "Departures"]/length(player_name[transfer_type == "Departures"]))
)


to_filter <- sapply(node_chars, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

#replace age with mean, fees, minutes, goals with 0 if there were no transfers
node_chars <- node_chars %>% mutate(arr_mean_age = ifelse(is.na(arr_mean_age) & arr_count == 0,mean(arr_mean_age, na.rm = T), arr_mean_age),
                                    dep_mean_age = ifelse(is.na(dep_mean_age) & dep_count == 0,mean(dep_mean_age, na.rm = T), dep_mean_age),
                                    arr_mean_fees = ifelse(is.na(arr_mean_fees) & arr_count == 0, 0, arr_mean_fees),
                                    dep_mean_fees = ifelse(is.na(dep_mean_fees) & dep_count == 0, 0 , dep_mean_fees),
                                    arr_mean_mins = ifelse(is.na(arr_mean_mins) & arr_count == 0, 0, arr_mean_mins),
                                    dep_mean_mins = ifelse(is.na(dep_mean_mins) & dep_count == 0, 0 , dep_mean_mins),
                                    arr_mean_goals = ifelse(is.na(arr_mean_goals) & arr_count == 0, 0, arr_mean_goals),
                                    dep_mean_goals = ifelse(is.na(dep_mean_goals) & dep_count == 0, 0 , dep_mean_goals)
                                    )

to_filter <- sapply(node_chars, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

#replace all remainder of missing data with 0
node_chars[is.na(node_chars)] <- 0

to_filter <- sapply(node_chars, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

write.csv(node_chars,"data/node_chars.csv", row.names = FALSE)

node_chars <- read_csv("data/node_chars.csv")

node_chars <- as.data.table(node_chars)

#create weighted graphs from transfer windows
for (year in unique(edgelist_all$season)) {
  temp <- eval(parse(text = paste0("edgelist_all_sum_", year)))
  temp <- temp %>% 
    mutate(team_name2_new = ifelse(transfer_type == "Arrivals", club_2, team_name2),
                          club_2_new = ifelse(transfer_type == "Arrivals", team_name2, club_2),
                          team_name2 = team_name2_new, club_2 = club_2_new) %>%
    select(-team_name2_new, -club_2_new) |> 
    drop_na() |> 
    mutate(transfer_fee = transfer_fee+1) |> 
    rename(
      weight = transfer_fee)
  
  assign(paste0("g_sum_", as.character(year)),graph_from_data_frame(temp, directed = TRUE, vertices = NULL))
  
  temp2 <- eval(parse(text = paste0("edgelist_all_wint_", year)))
  temp2 <- temp2 %>% mutate(team_name2_new = ifelse(transfer_type == "Arrivals", club_2, team_name2),
                          club_2_new = ifelse(transfer_type == "Arrivals", team_name2, club_2),
                          team_name2 = team_name2_new, club_2 = club_2_new) %>%
    select(-team_name2_new, -club_2_new)|> 
    drop_na() |> 
    mutate(transfer_fee = transfer_fee+1) |> 
    rename(
      weight = transfer_fee)
  assign(paste0("g_wint_", as.character(year)),graph_from_data_frame(temp2, directed = TRUE, vertices = NULL))
}

V(g_wint_2000)

vcount(g_wint_2000)#638

E(g_wint_2000)

ecount(g_wint_2000)#1546

#plot
plot(g, edge.arrow.size=.4, edge.curved=.1, vertex.size=0.1, arrow.size = 0.1, arrow.width = 0.01, vertex.label.cex=0.7, )

#Degree distribution
degree1 <- degree(g_wint_2000)
in_degree1 <- degree(g_wint_2000, mode = "in")
out_degree1 <- degree(g_wint_2000, mode = "out")
degree_dist1 <- degree_distribution(g_wint_2000)
strength1 <- graph.strength(g_wint_2000)
in_strength1 <- graph.strength(g_wint_2000, mode = "in")
out_strength1 <- graph.strength(g_wint_2000, mode = "out")

hist(degree1)
hist(in_degree1)
hist(out_degree1)
hist(strength1)

# import league tables

df_tables_all <- read_csv("data/df_tables_all.csv")

df_tables_all <- setDT(df_tables_all)

#building nodelist
for (year in unique(edgelist_all$season)) {
  
  club <- V(get(paste0("g_wint_", as.character(year))))$name
  in_degree <- degree(get(paste0("g_wint_", as.character(year))), mode = "in")
  out_degree <- degree(get(paste0("g_wint_", as.character(year))), mode = "out")
  in_strength <- strength(get(paste0("g_wint_", as.character(year))), mode = "in")
  out_strength <- strength(get(paste0("g_wint_", as.character(year))), mode = "out")
  betweenness <- betweenness(get(paste0("g_wint_", as.character(year))))
  in_coreness <- coreness(get(paste0("g_wint_", as.character(year))), mode = "in")
  out_coreness <- coreness(get(paste0("g_wint_", as.character(year))), mode = "out")
  #alpha <- alpha_centrality(get(paste0("g_wint_", as.character(year))))
  Node_char <- data.frame(club, in_degree,out_degree, in_strength, betweenness,out_strength, in_coreness, out_coreness) %>% setorder(., club)
  
  row.names(Node_char) <- NULL
  
  assign(paste0("Node_char_wint", as.character(year)), Node_char %>% 
           filter(club %in% df_tables_all[season == year & period == "mid"]$squad))
}

for (year in unique(edgelist_all$season)) {
  
  club <- V(get(paste0("g_sum_", as.character(year))))$name
  in_degree <- degree(get(paste0("g_sum_", as.character(year))), mode = "in")
  out_degree <- degree(get(paste0("g_sum_", as.character(year))), mode = "out")
  in_strength <- strength(get(paste0("g_sum_", as.character(year))), mode = "in")
  out_strength <- strength(get(paste0("g_sum_", as.character(year))), mode = "out")
  betweenness <- betweenness(get(paste0("g_sum_", as.character(year))))
  in_coreness <- coreness(get(paste0("g_sum_", as.character(year))), mode = "in")
  out_coreness <- coreness(get(paste0("g_sum_", as.character(year))), mode = "out")
  #alpha <- alpha_centrality(get(paste0("g_sum_", as.character(year))))
  
  Node_char <- data.frame(club, in_degree,out_degree,in_strength, out_strength, betweenness,in_coreness, out_coreness) %>% setorder(., club)
  
  row.names(Node_char) <- NULL
  
  assign(paste0("Node_char_sum", as.character(year)), Node_char %>% 
           filter(club %in% df_tables_all[season == year & period == "mid"]$squad)) #here also mid due to 2021
}


#other metrics to be considered: clustering(tranitivity), degrree, others 

#transitivity is said to be erronous for directed graphs - NEED to handle somehow

#next comes merging with table

#merging summer window network metrics with mid and final table performance

for (year in unique(edgelist_all$season)) {
  table_temp <- df_tables_all |> 
    filter(season == year & period == "mid") |> 
    select(c("squad","country", "season","p","rk", "g_diff", "pts"))
  merge_temp <- merge(get(paste0("Node_char_sum", as.character(year))),
        table_temp, by.x = "club", by.y = "squad")
  table_temp2 <- df_tables_all |> 
    filter(season == year & period == "final") |> 
    select(c("squad","p","rk", "g_diff", "pts"))
  assign(paste0("Node_final_sum", as.character(year)),
         merge(merge_temp, table_temp2, by.x = "club", by.y = "squad", suffixes = c("_mid", "_final")))
}

#merge past year to summer

for (year in unique(edgelist_all$season)[-c(1)]) {
  table_temp <- df_tables_all |> 
    filter(season == year-1 & period == "final") |> 
    select(c("squad","country", "season","p","rk", "g_diff", "pts"))
  merge_temp <- merge(get(paste0("Node_final_sum", as.character(year))),
                      table_temp, by.x = "club", by.y = "squad", suffixes = c("_now","_before"))
  assign(paste0("Node_final_sum_final", as.character(year)),merge_temp)
}


#Node_final_sum2021 to be fixed

#merging winter window performance with position and subsequent final table performance

for (year in unique(edgelist_all$season)) {
  table_temp <- df_tables_all |> 
    filter(season == year & period == "mid") |> 
    select(c("squad","country", "season","p","rk", "g_diff", "pts"))
  merge_temp <- merge(get(paste0("Node_char_wint", as.character(year))),
                      table_temp, by.x = "club", by.y = "squad")
  table_temp2 <- df_tables_all |> 
    filter(season == year & period == "final") |> 
    select(c("squad","p","rk", "g_diff", "pts"))
  assign(paste0("Node_final_wint", as.character(year)),
         merge(merge_temp, table_temp2, by.x = "club", by.y = "squad", suffixes = c("_mid", "_final")))
}

#merge past year to winter

for (year in unique(edgelist_all$season)[-c(1)]) {
  table_temp <- df_tables_all |> 
    filter(season == year-1 & period == "final") |> 
    select(c("squad","country", "season","p","rk", "g_diff", "pts"))
  merge_temp <- merge(get(paste0("Node_final_wint", as.character(year))),
                      table_temp, by.x = "club", by.y = "squad", suffixes = c("_now","_before"))
  assign(paste0("Node_final_wint_final", as.character(year)),merge_temp)
}

#Node_final_wint2021 to be fixed

#put these together into one giant dataframe
Node_final_sum <- data.table()
for (year in unique(edgelist_all$season)) {
  Node_final_sum <- bind_rows(Node_final_sum,get(paste0("Node_final_sum", as.character(year))))
}


Node_final_wint <- data.table()
for (year in unique(edgelist_all$season)) {
  Node_final_wint <- bind_rows(Node_final_wint,get(paste0("Node_final_wint", as.character(year))))
}


#put final_final together into one giant dataframe
Node_final_sum_final <- data.table()
for (year in  c(1993:2020)) {
  Node_final_sum_final <- bind_rows(Node_final_sum_final,get(paste0("Node_final_sum_final", as.character(year))))
}
write.csv(Node_final_sum_final,"data/Node_final_sum_final.csv", row.names = FALSE)

Node_final_sum_final <- read_csv("data/Node_final_sum_final.csv")


#put final_final together into one giant dataframe
Node_final_wint_final <- data.table()
for (year in  c(1993:2020)) {
  Node_final_wint_final <- bind_rows(Node_final_wint_final,get(paste0("Node_final_wint_final", as.character(year))))
}
write.csv(Node_final_wint_final,"data/Node_final_wint_final.csv", row.names = FALSE)

#then regressions - points per game will be dependent, all else independent


#summer transfer window effects
Node_final_sum <- Node_final_sum |> mutate(pts_per_game_mid = pts_mid/p_mid, pts_per_game_final = pts_final/p_final)

#did they perform good in the next half-year?
feols(  pts_per_game_mid ~ in_degree + out_degree + betweenness + in_closeness + 
          out_closeness + in_coreness + out_coreness + 
          as.factor(country) + as.factor(season) , data = Node_final_sum , vcov = 'hetero' )
#did they perform good in the next year?
feols(  pts_per_game_final ~ in_degree + out_degree + betweenness + in_closeness + 
          out_closeness + in_coreness + out_coreness + 
          as.factor(country) + as.factor(season) , data = Node_final_sum , vcov = 'hetero' )

#winter transfer window effect transfer window effects
Node_final_wint <- Node_final_wint |> mutate(pts_per_game_mid = pts_mid/p_mid, pts_per_game_final = (pts_final-pts_mid)/p_mid) |> 
  mutate(pts_per_game_d = pts_per_game_final - pts_per_game_mid)

#are they high-performers subsequently
feols(  pts_per_game_final ~ in_degree + out_degree + betweenness + in_closeness + 
          out_closeness + in_coreness + out_coreness + 
          as.factor(country) + as.factor(season) , data = Node_final_wint , vcov = 'hetero' )

#did they perform better subsequently
feols(  pts_per_game_d ~ in_degree + out_degree + betweenness + in_closeness + 
          out_closeness + in_coreness + out_coreness + 
          as.factor(country) + as.factor(season) , data = Node_final_wint , vcov = 'hetero' )

#did they perform better subsequently 2 - taking into account performance at the time of window
feols(  pts_per_game_final ~ pts_per_game_mid + in_degree + out_degree + betweenness + in_closeness + 
          out_closeness + in_coreness + out_coreness + 
          as.factor(country) + as.factor(season) , data = Node_final_wint , vcov = 'hetero' )

#do worse performing teams have more intake, less betweenness etc.
feols(  pts_per_game_mid ~ in_degree + out_degree + betweenness + in_closeness + 
          out_closeness + in_coreness + out_coreness + 
          as.factor(country) + as.factor(season) , data = Node_final_sum , vcov = 'hetero' )


#analysis of overall network

transfer_all <- df[is_loan == FALSE]

loan_all <- df[is_loan == TRUE]

edgelist_all <- transfer_all[,.(team_name2, club_2, transfer_fee, season, window, player_name, transfer_type, country, country_2)]

loan_edgelist_all <- loan_all[,.(team_name2, club_2, transfer_fee, season, window, player_name, transfer_type, country, country_2)]

#see global characteristics e.g. small-wordiness and transitivity

#create visualization of evolving network


