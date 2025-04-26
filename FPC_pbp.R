library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stargazer)
library(tidyverse)
library(writexl)
library(openxlsx)

# Load the data
df <- read_excel("FPC_data.xlsx", sheet = "Pbp")

df <- df%>%
  filter(ODK != "S")

# Add score columns
df$FPC_Score <- 0
df$Opponent_Score <- 0

fpc_score <- 0
opp_score <- 0

for (i in 1:nrow(df)) {
  odk <- df$ODK[i]
  play_type <- tolower(as.character(df$`PLAY TYPE`[i]))
  result <- tolower(as.character(df$RESULT[i]))
  
  # Reset score if it's the first row, or new game starts
  if (i == 1 || df$Game[i] != df$Game[i - 1]) {
    fpc_score <- 0
    opp_score <- 0
  }
  
  if (!is.na(odk)) {
    if (odk == "O") {
      if (grepl("td", result)) fpc_score <- fpc_score + 6
      if (grepl("2-point", play_type) && grepl("good", result)) fpc_score <- fpc_score + 2
      if (grepl("extra pt", play_type) && grepl("good", result)) fpc_score <- fpc_score + 1
      if (grepl("fg", play_type) && grepl("good", result)) fpc_score <- fpc_score + 3
      if (grepl("safety", play_type)) opp_score <- opp_score + 2
    }
    
    if (odk == "D") {
      if (grepl("td", result)) opp_score <- opp_score + 6
      if (grepl("2-point", play_type) && grepl("good", result)) opp_score <- opp_score + 2
      if (grepl("extra pt", play_type) && grepl("good", result)) opp_score <- opp_score + 1
      if (grepl("fg", play_type) && grepl("good", result)) opp_score <- opp_score + 3
      if (grepl("safety", play_type)) fpc_score <- fpc_score + 2
    }
  }
  
  df$FPC_Score[i] <- fpc_score
  df$Opponent_Score[i] <- opp_score
}

# Save the results
write_xlsx(df, "FPC_data_with_scores.xlsx")

df <- df%>%
  mutate(score_diff = FPC_Score - Opponent_Score)

# Load necessary libraries
library(readxl)
library(dplyr)

# Read the Excel file
data <- read_excel("FPC_data_with_scores.xlsx")

# Convert relevant columns to appropriate types
data <- data %>%
  mutate(
    `YARD LN` = as.numeric(`YARD LN`),
    `PLAY TYPE` = as.factor(`PLAY TYPE`),
    RESULT = as.factor(RESULT),
    Game = as.factor(Game)
  )

# Create summary statistics
# Load necessary libraries
library(readxl)
library(dplyr)

# Read the Excel file
df <- read_excel("FPC_data_with_scores.xlsx")

summary_statistics <- df %>%
  filter(ODK == "O") %>%
  mutate(
    Play_Type_LC = tolower(`PLAY TYPE`),
    Result_LC = tolower(RESULT),
    Is_Pass_TD = grepl("pass", Play_Type_LC) & grepl("td", Result_LC),
    Is_Rush_TD = grepl("run", Play_Type_LC) & grepl("td", Result_LC),
    Is_FG_Made = (grepl("FG", Play_Type_LC)) & grepl("Good", Result_LC),
    Is_Safety = grepl("safety", Result_LC) | grepl("end zone", Result_LC) | grepl("safety", Play_Type_LC),
    Is_2Pt = grepl("2pt", Result_LC) | grepl("two point", Result_LC),
    Is_ExtraPt_Made = grepl("extra pt", Play_Type_LC) & grepl("good", Result_LC)
  ) %>%
  group_by(Game, Season, Win) %>%
  summarise(
    Total_Plays = n(),
    Total_Yards = sum(`GN/LS`, na.rm = TRUE),
    Rush_yards = sum(ifelse(`PLAY TYPE` == "Run", `GN/LS`, 0), na.rm = TRUE),
    Pass_yards = sum(ifelse(`PLAY TYPE` == "Pass", `GN/LS`, 0), na.rm = TRUE),
    Pass_plays = sum(`PLAY TYPE` == "Pass", na.rm = TRUE),
    Run_plays = sum(`PLAY TYPE` == "Run", na.rm = TRUE),
    Penalties_Received = sum(RESULT == "Penalty", na.rm = TRUE),
    Average_Yards_Per_Play = mean(`GN/LS`, na.rm = TRUE),
    Completed_Passes = sum(`PLAY TYPE` == "Pass" & RESULT %in% c("Complete", "Complete, TD"), na.rm = TRUE),
    Incomplete_Passes = sum(`PLAY TYPE` == "Pass" & RESULT == "Incomplete", na.rm = TRUE),
    Pass_Completion_Rate = Completed_Passes / Pass_plays,
    Positive_rushes = sum(`PLAY TYPE` == "Run" & `GN/LS` > 0, na.rm = TRUE),
    Pass_TDs = sum(Is_Pass_TD, na.rm = TRUE),
    Rush_TDs = sum(Is_Rush_TD, na.rm = TRUE),
    FGs_Made = sum(Is_FG_Made, na.rm = TRUE),
    Safeties = sum(Is_Safety, na.rm = TRUE),
    Two_Pt_Conversions = sum(Is_2Pt, na.rm = TRUE),
    Extra_Points_Made = sum(Is_ExtraPt_Made, na.rm = TRUE),
    Total_Points = Pass_TDs * 6 + Rush_TDs * 6 + FGs_Made * 3 + Two_Pt_Conversions * 2 + Safeties * 2 + Extra_Points_Made * 1,
  )

