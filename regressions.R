#clean environment
rm(list =ls())

#install required packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, dplyr, data.table, igraph, fixest, ggplot2, cowplot, kableExtra, stargazer)

#load in tables

node_chars <- read_csv("data/node_chars.csv")

node_chars <- as.data.table(node_chars)

node_values <- read_csv("data/team_market_values.csv")

node_values <- as.data.table(node_values)

node_sum <- read_csv("data/Node_final_sum_final.csv")

node_sum <- as.data.table(node_sum)

node_wint <- read_csv("data/Node_final_wint_final.csv")

node_wint <- as.data.table(node_wint)

#some further joins

node_sum_val <- merge(node_sum,
           node_values, by.x = c("club", "season_before"), by.y = c("team_name2", "year"))

node_sum_final <- merge(node_sum_val, 
           node_chars |> filter(window == "Summer"), by.x = c("club", "season_now"), by.y = c("team_name2", "season"))

node_wint_val <- merge(node_wint,
                      node_values, by.x = c("club", "season_before"), by.y = c("team_name2", "year"))

node_wint_final <- merge(node_wint_val, 
                        node_chars |> filter(window == "Winter"), by.x = c("club", "season_now"), by.y = c("team_name2", "season"))


node_sum_final <- node_sum_final |>  mutate_at(vars(country_now, season_now), factor)

node_wint_final <- node_wint_final |>  mutate_at(vars(country_now, season_now), factor)



#then regressions come here

## summer ## 
#did they perform good in the next year after summer?
summer_full <- lm( rk_final ~ rk + in_degree + out_degree + betweenness + in_coreness + out_coreness + 
                                  country_now + season_now , data = node_sum_final  )

summer_full_controlls <- lm(  rk_final ~ rk + in_degree + out_degree + betweenness + in_coreness + out_coreness + 
                                   country_now +season_now + market_value + avg_age + avg_height +
                                   arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                   arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_sum_final  )

#to appendix with points 
summer_full_appendix <- lm(  rk_final ~ rk + in_degree + out_degree +
                         country_now + season_now, data = node_sum_final  )

summer_full_controlls_appendix <- lm(  rk_final ~ rk + in_degree + out_degree +
                                   country_now + season_now + market_value + avg_age + avg_height +
                                   arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                   arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_sum_final  )


summer_full_appendix2 <- lm(  rk_final ~ rk + betweenness +
                               country_now + season_now, data = node_sum_final  )

summer_full_controlls_appendix2 <- lm(  rk_final ~ rk + betweenness +
                                         country_now + season_now + market_value + avg_age + avg_height +
                                         arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                         arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_sum_final  )


summer_full_appendix3 <- lm(  rk_final ~ rk + in_coreness + out_coreness +
                               country_now + season_now, data = node_sum_final  )

summer_full_controlls_appendix3 <- lm(  rk_final ~ rk + in_coreness + out_coreness +
                                         country_now + season_now + market_value + avg_age + avg_height +
                                         arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                         arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_sum_final  )




#did they perform good in the next half-year?
summer_mid <- lm( rk_mid ~ rk + in_degree + out_degree + betweenness + in_coreness + out_coreness + 
                        country_now + season_now , data = node_sum_final )

summer_mid_controls <- lm(  rk_mid ~ rk + in_degree + out_degree + betweenness + in_coreness + out_coreness + 
                                 season_now + market_value + avg_age + avg_height +
                                 arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                 arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_sum_final  )

#in the appendix
summer_mid_appendix <- lm(  rk_mid ~ rk + in_degree + out_degree +
          country_now + season_now , data = node_sum_final  )

summer_mid_controls_appendix <- lm(  rk_mid ~ rk + in_degree + out_degree +
                                          country_now + season_now + market_value + avg_age + avg_height +
                                          arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                          arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_sum_final  )


summer_mid_appendix2 <- lm(  rk_mid ~ rk + betweenness +
                              country_now + season_now , data = node_sum_final  )

summer_mid_controls_appendix2 <- lm(  rk_mid ~ rk + betweenness +
                                       country_now + season_now + market_value + avg_age + avg_height +
                                       arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                       arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_sum_final  )


summer_mid_appendix3 <- lm(  rk_mid ~ rk + in_coreness + out_coreness +
                              country_now + season_now , data = node_sum_final  )

summer_mid_controls_appendix3 <- lm(  rk_mid ~ rk + in_coreness + out_coreness +
                                       country_now + season_now + market_value + avg_age + avg_height +
                                       arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                       arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_sum_final  )



## winter ##

#did they perform good in the next half-year?
winter_full <- lm(  rk_final ~ rk_mid + in_degree + out_degree + betweenness + in_coreness + out_coreness + 
                        country_now + season_now , data = node_wint_final  )

winter_full_controls <- lm(  rk_final ~ rk_mid + rk + in_degree + out_degree + betweenness + in_coreness + out_coreness + 
                                  country_now + season_now + market_value + avg_age + avg_height +
                                  arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                  arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_wint_final  )

winter_full_appendix <- lm(  rk_final ~ rk_mid + in_degree + out_degree + 
                                  country_now + season_now , data = node_wint_final  )

winter_full_controls_appendix <- lm(  rk_final ~ rk_mid + pts + in_degree + out_degree +  
                                           country_now + season_now + market_value + avg_age + avg_height +
                                           arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                           arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_wint_final  )


winter_full_appendix2 <- lm(  rk_final ~ rk_mid + betweenness + 
                               country_now + season_now , data = node_wint_final  )

winter_full_controls_appendix2 <- lm(  rk_final ~ rk_mid + pts + betweenness +  
                                        country_now + season_now + market_value + avg_age + avg_height +
                                        arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                        arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_wint_final  )

winter_full_appendix3 <- lm(  rk_final ~ rk_mid + in_coreness + out_coreness + 
                                country_now + season_now , data = node_wint_final  )

winter_full_controls_appendix3 <- lm(  rk_final ~ rk_mid + pts + in_coreness + out_coreness + 
                                         country_now + season_now + market_value + avg_age + avg_height +
                                         arr_mean_age + dep_mean_age + arr_mean_fees + dep_mean_fees + arr_mean_mins + dep_mean_mins +
                                         arr_mean_goals + dep_mean_goals + arr_share_loan + dep_share_loan, data = node_wint_final  )



# Summarize findings:

stargazer(summer_full, summer_full_controlls, summer_mid, summer_mid_controls, omit = c("country_now", "market_value",
                                                       "arr_mean_age", "dep_mean_goals"), 
          omit.labels = c("League and year control","Squad characteristics", "Transfer info",  "Player performance"),
          keep = c("rk", "in_degree", "out_degree", "betweenness","in_coreness", "out_coreness"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "5pt", 
          dep.var.labels = c("Final table ranking", "Final table ranking", "Mid table ranking", "Mid table ranking"),
          covariate.labels = c("Ranking last season", "In-degree", "Out-degree", "Betweenness", "In-coreness", "Out-coreness"),
          type="latex")

stargazer(winter_full, winter_full_controls, omit = c("country_now", "market_value",
                                                                                        "arr_mean_age", "dep_mean_goals"), 
          omit.labels = c("League and year control","Squad characteristics", "Transfer info",  "Player performance"),
          keep = c("rk", "in_degree", "out_degree", "betweenness","in_coreness", "out_coreness"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "5pt", 
          dep.var.labels = c("Final table ranking", "Final table ranking"),
          covariate.labels = c("Mid table ranking", "In-degree", "Out-degree", "Betweenness", "In-coreness", "Out-coreness"),
          type="latex")


stargazer(summer_full_appendix, summer_full_controlls_appendix, summer_mid_appendix, summer_mid_controls_appendix, omit = c("country_now", "market_value",
                                                                                        "arr_mean_age", "dep_mean_goals"), 
          omit.labels = c("League and year control","Squad characteristics", "Transfer info",  "Player performance"),
          keep = c("rk", "in_degree", "out_degree"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "5pt", 
          dep.var.labels = c("Final table points", "Final table points", "Mid table points", "Mid table points"),
          covariate.labels = c("Rank last season", "In-degree", "Out-degree"),
          type="latex")

stargazer(summer_full_appendix2, summer_full_controlls_appendix2, summer_mid_appendix2, summer_mid_controls_appendix2, omit = c("country_now", "market_value",
                                                                                                                            "arr_mean_age", "dep_mean_goals"), 
          omit.labels = c("League and year control","Squad characteristics", "Transfer info",  "Player performance"),
          keep = c("rk", "betweenness"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "5pt", 
          dep.var.labels = c("Final table points", "Mid table points", "Mid table points", "Mid table points"),
          covariate.labels = c("Rank last season", "Betweenness"),
          type="latex")

stargazer(summer_full_appendix3, summer_full_controlls_appendix3, summer_mid_appendix3, summer_mid_controls_appendix3, omit = c("country_now", "market_value",
                                                                                                                            "arr_mean_age", "dep_mean_goals"), 
          omit.labels = c("League and year control","Squad characteristics", "Transfer info",  "Player performance"),
          keep = c("rk", "in_coreness", "out_coreness"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "5pt", 
          dep.var.labels = c("Final table points", "Mid table points", "Mid table points", "Mid table points"),
          covariate.labels = c("Rank last season", "In-coreness", "Out-coreness"),
          type="latex")



stargazer(winter_full_appendix, winter_full_controls_appendix, omit = c("country_now", "market_value",
                                                                                                                            "arr_mean_age", "dep_mean_goals"), 
          omit.labels = c("League and year control","Squad characteristics", "Transfer info",  "Player performance"),
          keep = c("rk", "in_degree", "out_degree"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "5pt", 
          dep.var.labels = c("Final table rank", "Mid table rank", "Mid table points", "Mid table points"),
          covariate.labels = c("Points last season", "In-degree", "Out-degree"),
          type="latex")


stargazer(winter_full_appendix2, winter_full_controls_appendix2, omit = c("country_now", "market_value",
                                                                        "arr_mean_age", "dep_mean_goals"), 
          omit.labels = c("League and year control","Squad characteristics", "Transfer info",  "Player performance"),
          keep = c("rk", "betweenness"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "5pt", 
          dep.var.labels = c("Final table rank", "Mid table rank", "Mid table points", "Mid table points"),
          covariate.labels = c("Points last season", "Betweenness"),
          type="latex")

stargazer(winter_full_appendix3, winter_full_controls_appendix3, omit = c("country_now", "market_value",
                                                                        "arr_mean_age", "dep_mean_goals"), 
          omit.labels = c("League and year control","Squad characteristics", "Transfer info",  "Player performance"),
          keep = c("rk", "in_coreness", "out_coreness"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "5pt", 
          dep.var.labels = c("Final table rank", "Mid table rank", "Mid table points", "Mid table points"),
          covariate.labels = c("Points last season", "In-coreness", "Out-coreness"),
          type="latex")















#analysis of overall network

#create weighted network metrics for three year moving window of all transfer windows during the period
for (year in unique(edgelist_all$season)) {
  temp <- eval(parse(text = paste0("edgelist_all_sum_", year)))
  temp <- temp %>% 
    mutate(team_name2_new = ifelse(transfer_type == "Arrivals", club_2, team_name2),
           club_2_new = ifelse(transfer_type == "Arrivals", team_name2, club_2),
           team_name2 = team_name2_new, club_2 = club_2_new) %>%
    select(-team_name2_new, -club_2_new)
  
  assign(paste0("g_sum_", as.character(year)),graph_from_data_frame(temp, directed = TRUE, vertices = NULL))
  
  temp2 <- eval(parse(text = paste0("edgelist_all_wint_", year)))
  temp2 <- temp2 %>% mutate(team_name2_new = ifelse(transfer_type == "Arrivals", club_2, team_name2),
                            club_2_new = ifelse(transfer_type == "Arrivals", team_name2, club_2),
                            team_name2 = team_name2_new, club_2 = club_2_new) %>%
    select(-team_name2_new, -club_2_new)
  assign(paste0("g_wint_", as.character(year)),graph_from_data_frame(temp2, directed = TRUE, vertices = NULL))
}


transfer_all <- df[is_loan == FALSE]

loan_all <- df[is_loan == TRUE]

edgelist_all <- transfer_all[,.(team_name2, club_2, transfer_fee, season, window, player_name, transfer_type, country, country_2)]

loan_edgelist_all <- loan_all[,.(team_name2, club_2, transfer_fee, season, window, player_name, transfer_type, country, country_2)]


avg_path_length <- c()
clustering <- c()
avg_degree <- c()
years <- sort(unique(edgelist_all$season))
set.seed(10)
for(i in seq_along(years)){
  year[i] <- years[[i]]
  avg_path_length[i] <- mean_distance(get(paste0("g_sum_", as.character(years[[i]])))) / mean_distance(erdos.renyi.game(n = vcount(get(paste0("g_sum_", as.character(years[[i]])))), p.or.m = ecount(get(paste0("g_sum_", as.character(years[[i]])))), type = 'gnm'))
  clustering[i] <- transitivity(get(paste0("g_sum_", as.character(years[[i]]))), type="global")/ transitivity(erdos.renyi.game(n = vcount(get(paste0("g_sum_", as.character(years[[i]])))), p.or.m = ecount(get(paste0("g_sum_", as.character(years[[i]])))), type = 'gnm'),type="global") 
  avg_degree[i] <- mean(degree(get(paste0("g_sum_", as.character(years[[i]])))))
}

overall_df_sum <- data.frame(years,avg_path_length,clustering, avg_degree)

overall_df_sum <- overall_df_sum |> mutate(sw = clustering/ avg_path_length)

summer <- ggplot(overall_df_sum, aes(years,sw)) +
  geom_point(color = "#F8766D")+
  geom_line(color = "#F8766D") +
  labs(x = "Year", y = "Value") +
  scale_x_continuous(breaks = years) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

avg_path_length2 <- c()
clustering2 <- c()
avg_degree2 <- c()
set.seed(10)
for(i in seq_along(years)){
  year[i] <- years[[i]]
  avg_path_length2[i] <- mean_distance(get(paste0("g_wint_", as.character(years[[i]])))) / mean_distance(erdos.renyi.game(n = vcount(get(paste0("g_wint_", as.character(years[[i]])))), p.or.m = ecount(get(paste0("g_sum_", as.character(years[[i]])))), type = 'gnm'))
  clustering2[i] <- transitivity(get(paste0("g_wint_", as.character(years[[i]]))), type="global")/ transitivity(erdos.renyi.game(n = vcount(get(paste0("g_wint_", as.character(years[[i]])))), p.or.m = ecount(get(paste0("g_sum_", as.character(years[[i]])))), type = 'gnm'),type="global") 
  avg_degree2[i] <- mean(degree(get(paste0("g_wint_", as.character(years[[i]])))))
}

overall_df_wint <- data.frame(years,avg_path_length2,clustering2, avg_degree2)

overall_df_wint <- overall_df_wint |> mutate(sw = clustering2/ avg_path_length2)

winter <- ggplot(overall_df_wint, aes(years,sw)) +
  geom_point(color = "#00BFC4")+
  geom_line(color = "#00BFC4") +
  labs(x = "Year", y = "") +
  scale_x_continuous(breaks = years) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

plot_grid(summer, winter, labels = "AUTO")


overall_df_sum_long <- melt(overall_df_sum |> select(years,clustering, avg_path_length, avg_degree),  id.vars = 'years', variable.name = 'series')

summer2 <- ggplot(overall_df_sum_long, aes(years,value)) +
  geom_line(aes(colour = series)) +
  labs(x = "Year", y = "Value") +
  scale_color_discrete(name="Network metric") +
  scale_x_continuous(breaks = years) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), legend.position="top")

overall_df_wint_long <- melt(overall_df_wint |> select(years,clustering2, avg_path_length2, avg_degree2),  id.vars = 'years', variable.name = 'series')

winter2 <- ggplot(overall_df_wint_long, aes(years,value)) +
  geom_line(aes(colour = series)) +
  labs(x = "Year", y = "Value") +
  scale_x_continuous(breaks = years) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")
  
plot_grid(summer2, winter2, labels = "AUTO")

transfer_fee_table <- edgelist_all_clean |>
  group_by(season, window) |>
  summarise(sum_transfer_fees = sum(transfer_fee, na.rm = TRUE), mean_age = mean(player_age), loan_share = sum(is_loan/n()))

ggplot(edgelist_all_clean |>
         group_by(season, window, transfer_type) |> tally(), aes(season, n, fill=window)) +
  geom_col(position = "dodge") +
  labs(x = "Year",
       y = "Number of transfers") +
  theme_bw() +
  scale_fill_discrete(name="Transfer window")+
  scale_x_continuous(breaks = years) +
  theme(axis.text.x = element_text(angle = 90), legend.position="none") +
  facet_wrap(~ transfer_type)


loans <- ggplot(transfer_fee_table, aes(season, loan_share, color=window)) +
  geom_line()  +
  labs(x = "Year",
       y = "Share of loan transfers of all players transferred") +
  theme_bw() +
  scale_color_discrete(name="Transfer window")+
  scale_x_continuous(breaks = years) +
  theme(axis.text.x = element_text(angle = 90), legend.position="none")

ages <- ggplot(transfer_fee_table, aes(season, mean_age, color=window)) +
  geom_line() +
  labs(x = "Year",
       y = "Average age of players transferred") +
  theme_bw() +
  scale_color_discrete(name="Transfer window")+
  scale_x_continuous(breaks = years) +
  theme(axis.text.x = element_text(angle = 90),  legend.position="none")

plot_grid(loans, ages, labels = "AUTO")

ggplot(transfer_fee_table, aes(season, sum_transfer_fees, fill=window)) +
  geom_col(position = "dodge") +
  labs(x = "Year",
       y = "Total transfer fee (euro)") +
  theme_bw() +
  scale_fill_discrete(name="Transfer window")+
  scale_x_continuous(breaks = years) +
  theme(axis.text.x = element_text(angle = 90))




