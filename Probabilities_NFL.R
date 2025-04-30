library(ggplot2)
library(tidyverse)
library(dplyr)
library(stargazer)
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(reshape2)
library(writexl)
library(openxlsx)

#### NFL Data ############
NFL_data <- load_pbp(2024)

### likelihood of going for it on 4th down###
NFL_data$receiving_yards[is.na(NFL_data$receiving_yards)] <- 0
NFL_data$rushing_yards[is.na(NFL_data$rushing_yards)] <- 0

fourth_down_data <- NFL_data %>%
  filter(down == 4)%>%
  rename(distance_to_EZ = yardline_100)%>%
  mutate(success = ifelse(first_down == 1 | touchdown == 1, 1, 0),
         yards_gained = receiving_yards + rushing_yards)%>%
  select(home_team, away_team, season_type, posteam, distance_to_EZ, half_seconds_remaining, game_seconds_remaining, ydstogo,
         score_differential, touchdown, first_down, yards_gained, play_type, success)

#### Logistic Modeln WITH TIME REMAINING ####
logit_model <- glm(success ~ distance_to_EZ + ydstogo + score_differential + game_seconds_remaining, data = fourth_down_data, family = binomial)
stargazer(logit_model, type = "text")

wb <- loadWorkbook("/Users/jakeblumengarten/R/Flagler-Palm-Coast-Football-Project/Datasets/probabilities.xlsx")

#### Score is 0-0 with 8 minutes left in the game###
predict_data_0 <- expand.grid(
  distance_to_EZ = seq(1, 99, by = 1), 
  ydstogo = seq(1, 10, by = 1), 
  score_differential = 0, 
  game_seconds_remaining = 480)

predict_data_0$prob_success <- predict(logit_model, newdata = predict_data_0, type = "response")
probability_grid_0 <- dcast(predict_data_0, distance_to_EZ ~ ydstogo, value.var = "prob_success")

writeData(wb, sheet = "tie_game", x = probability_grid_0)


##### MODEL NO TIME ####
logit_model_2 <- glm(success ~ distance_to_EZ + ydstogo + score_differential, data = fourth_down_data, family = binomial)
stargazer(logit_model_2, type = "text")

### EPA model ###
EPA <- lm(epa ~ yardline_100 + ydstogo + score_differential + play_type, data = NFL_data)
stargazer(EPA, type = "text")

### EPA df = 0 PASS ###
predict_grid_PASS0 <- expand.grid(
  yardline_100 = seq(1, 99, by = 1),
  ydstogo = seq(1, 10, by = 1),
  score_differential = 0, 
  play_type = "pass"
)

predict_grid_PASS0$expected_EPA <- predict(EPA, newdata = predict_grid_PASS0)
expected_EPA_table_PASS0 <- dcast(predict_grid_PASS0, yardline_100 ~ ydstogo, value.var = "expected_EPA")

writeData(wb, sheet = "EPA_df=0_PASS", x = expected_EPA_table_PASS0)

### EPA df = 0 RUN ###
predict_grid_RUN0 <- expand.grid(
  yardline_100 = seq(1, 99, by = 1),
  ydstogo = seq(1, 10, by = 1),
  score_differential = 0, 
  play_type = "run"
)

predict_grid_RUN0$expected_EPA <- predict(EPA, newdata = predict_grid_RUN0)
expected_EPA_table_RUN0 <- dcast(predict_grid_RUN0, yardline_100 ~ ydstogo, value.var = "expected_EPA")


writeData(wb, sheet = "EPA_df=0_RUN", x = expected_EPA_table_RUN0)

#### Team is down 3 #####
predict_data_down_3 <- expand.grid(
  distance_to_EZ = seq(1, 99, by = 1), 
  ydstogo = seq(1, 10, by = 1), 
  score_differential = -3)

predict_data_down_3$prob_success <- predict(logit_model_2, newdata = predict_data_down_3, type = "response")
probability_grid_down_3 <- dcast(predict_data_down_3, distance_to_EZ ~ ydstogo, value.var = "prob_success")

writeData(wb, sheet = "down_3", x = probability_grid_down_3)


#### Team is up 3 #####
predict_data_up_3 <- expand.grid(
  distance_to_EZ = seq(1, 99, by = 1), 
  ydstogo = seq(1, 10, by = 1), 
  score_differential = 3)

predict_data_up_3$prob_success <- predict(logit_model_2, newdata = predict_data_up_3, type = "response")
probability_grid_up_3 <- dcast(predict_data_up_3, distance_to_EZ ~ ydstogo, value.var = "prob_success")

writeData(wb, sheet = "up_3", x = probability_grid_up_3)

### Tie Game ###
predict_data_tie <- expand.grid(
  distance_to_EZ = seq(1, 99, by = 1), 
  ydstogo = seq(1, 10, by = 1), 
  score_differential = 0)

predict_data_tie$prob_success <- predict(logit_model_2, newdata = predict_data_tie, type = "response")
probability_grid_up_3 <- dcast(predict_data_tie, distance_to_EZ ~ ydstogo, value.var = "prob_success")

writeData(wb, sheet = "tie", x = probability_grid_up_3)

### Down 7 ###
predict_data_down_7 <- expand.grid(
  distance_to_EZ = seq(1, 99, by = 1), 
  ydstogo = seq(1, 10, by = 1), 
  score_differential = -7)

predict_data_down_7$prob_success <- predict(logit_model_2, newdata = predict_data_down_7, type = "response")
probability_grid_down_7 <- dcast(predict_data_down_7, distance_to_EZ ~ ydstogo, value.var = "prob_success")

writeData(wb, sheet = "down_7", x = probability_grid_down_7)

### Up 7 ###
predict_data_up_7 <- expand.grid(
  distance_to_EZ = seq(1, 99, by = 1), 
  ydstogo = seq(1, 10, by = 1), 
  score_differential = 7)

predict_data_up_7$prob_success <- predict(logit_model_2, newdata = predict_data_up_7, type = "response")
probability_grid_up_7 <- dcast(predict_data_up_7, distance_to_EZ ~ ydstogo, value.var = "prob_success")

writeData(wb, sheet = "up_7", x = probability_grid_up_7)


### Save Excel ###
saveWorkbook(wb, "probabilities.xlsx", overwrite = TRUE)


