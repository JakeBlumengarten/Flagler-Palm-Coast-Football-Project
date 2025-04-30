library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stargazer)
library(tidyverse)
library(writexl)
library(openxlsx)
library(ggplot2)

# Load the data
df <- read_excel("Datasets/FPC_data.xlsx", sheet = "Pbp")

game_summary_statistics <- df %>%
  filter(Effective_ODK == "O")%>%
  group_by(Game, Season, Win) %>%
  summarise(
    Total_Plays = n(),
    Total_Yards = sum(`GN/LS`, na.rm = TRUE),
    Rush_yards = sum(ifelse(Play_type_LC== "run", `GN/LS`, 0), na.rm = TRUE),
    Pass_yards = sum(ifelse(Play_type_LC== "pass", `GN/LS`, 0), na.rm = TRUE),
    Pass_plays = sum(Play_type_LC== "pass", na.rm = TRUE),
    Run_plays = sum(Play_type_LC== "run", na.rm = TRUE),
    Penalties_Received = sum(Result_LC == "penalty", na.rm = TRUE),
    Average_Yards_Per_Play = mean(`GN/LS`, na.rm = TRUE),
    Completed_Passes = sum(Play_type_LC== "pass" & Result_LC %in% c("complete", "complete, td"), na.rm = TRUE),
    Incomplete_Passes = sum(Play_type_LC== "pass" & Result_LC == "incomplete", na.rm = TRUE),
    Pass_Completion_Rate = Completed_Passes / Pass_plays,
    Positive_rushes = sum(Play_type_LC== "run" & `GN/LS` > 0, na.rm = TRUE),
    TDs = sum(TD, na.rm = TRUE),
    FGs_Made = sum(is_FG, na.rm = TRUE),
    TO_TD = sum(is_TO_TD, na.rm = TRUE)
  )


df <- df %>%
  mutate(success = ifelse(First_down == 1 | TD == 1, 1, 0),
         score_differential = FPC_score - Opponent_score)

wb <- loadWorkbook("/Users/jakeblumengarten/R/Flagler-Palm-Coast-Football-Project/Datasets/probabilities.xlsx")


logit_model_3 <- glm(success ~ `YARD LN` + DIST + score_differential, data = df, family = binomial)


#### Team is down 3 #####
predict_data_down_3 <- expand.grid(
  `YARD LN` = seq(1, 99, by = 1), 
  DIST = seq(1, 10, by = 1), 
  score_differential = -3)

predict_data_down_3$prob_success <- predict(logit_model_3, newdata = predict_data_down_3, type = "response")
probability_grid_down_3 <- dcast(predict_data_down_3, `YARD LN` ~ DIST, value.var = "prob_success")

writeData(wb, sheet = "down_3", x = probability_grid_down_3)


#### Team is up 3 #####
predict_data_up_3 <- expand.grid(
  `YARD LN` = seq(1, 99, by = 1), 
  DIST = seq(1, 10, by = 1), 
  score_differential = 3)

predict_data_up_3$prob_success <- predict(logit_model_3, newdata = predict_data_up_3, type = "response")
probability_grid_up_3 <- dcast(predict_data_up_3, `YARD LN` ~ DIST, value.var = "prob_success")

writeData(wb, sheet = "up_3", x = probability_grid_up_3)

### Tie Game ###
predict_data_tie <- expand.grid(
  `YARD LN` = seq(1, 99, by = 1), 
  DIST = seq(1, 10, by = 1), 
  score_differential = 0)

predict_data_tie$prob_success <- predict(logit_model_3, newdata = predict_data_tie, type = "response")
probability_grid_up_3 <- dcast(predict_data_tie, `YARD LN` ~ DIST, value.var = "prob_success")

writeData(wb, sheet = "tie", x = probability_grid_up_3)

### Down 7 ###
predict_data_down_7 <- expand.grid(
  `YARD LN` = seq(1, 99, by = 1), 
  DIST = seq(1, 10, by = 1), 
  score_differential = -7)

predict_data_down_7$prob_success <- predict(logit_model_3, newdata = predict_data_down_7, type = "response")
probability_grid_down_7 <- dcast(predict_data_down_7, `YARD LN` ~ DIST, value.var = "prob_success")

writeData(wb, sheet = "down_7", x = probability_grid_down_7)

### Up 7 ###
predict_data_up_7 <- expand.grid(
  `YARD LN` = seq(1, 99, by = 1), 
  DIST = seq(1, 10, by = 1), 
  score_differential = 7)

predict_data_up_7$prob_success <- predict(logit_model_3, newdata = predict_data_up_7, type = "response")
probability_grid_up_7 <- dcast(predict_data_up_7, `YARD LN` ~ DIST, value.var = "prob_success")

writeData(wb, sheet = "up_7", x = probability_grid_up_7)


### Save Excel ###
saveWorkbook(wb, "Datasets/probabilities.xlsx", overwrite = TRUE)

         