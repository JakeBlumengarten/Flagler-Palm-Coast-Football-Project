library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(scales)
pbp_data_2_0 <- read_excel("Datasets/pbp_data_2.0.xlsx", 
                           sheet = "R-test")

pbp_data_2_0_clean%>%
  filter(ODK == "O") %>%
  group_by(`PLAY TYPE`) %>%
  summarize(
    Plays = n(),
    Avg_Yards_Gained = mean(GN_LS, na.rm = TRUE),
    TDs_Scored = sum(TD, na.rm = TRUE),
    Completions = sum(str_detect(RESULT, "Complete"), na.rm = TRUE),
    Fumbles = sum(Fumble, na.rm = TRUE)
  )

pbp_data_2_0_clean %>%
  filter(ODK == "O") %>%
  group_by(QTR) %>%
  summarize(
    Plays = n(),
    Avg_Yards_Gained = mean(GN_LS, na.rm = TRUE),
    TDs_Scored = sum(TD, na.rm = TRUE),
    Completions = sum(str_detect(RESULT, "Complete"), na.rm = TRUE),
    Fumbles = sum(Fumble, na.rm = TRUE)
  )

pbp_data_2_0_clean %>%
  filter(ODK == "O") %>%
  group_by(DN) %>%
  summarize(
    Plays = n(),
    Avg_Yards_Gained = mean(GN_LS, na.rm = TRUE),
    TDs_Scored = sum(TD, na.rm = TRUE),
    Completions = sum(str_detect(RESULT, "Complete"), na.rm = TRUE),
    Fumbles = sum(Fumble, na.rm = TRUE),
    First_Downs = sum(as.numeric(First_down), na.rm = TRUE)
  )

pbp_data_2_0_clean%>%
  filter(ODK == "D") %>%
  group_by(`PLAY TYPE`) %>%
  summarize(
    Plays = n(),
    Avg_Yards_Gained = mean(GN_LS, na.rm = TRUE),
    TDs_Scored = sum(TD, na.rm = TRUE),
    Completions = sum(str_detect(RESULT, "Complete"), na.rm = TRUE),
    Fumbles = sum(Fumble, na.rm = TRUE)
  )

pbp_data_2_0_clean %>%
  filter(ODK == "D") %>%
  group_by(QTR) %>%
  summarize(
    Plays = n(),
    Avg_Yards_Gained = mean(GN_LS, na.rm = TRUE),
    TDs_Scored = sum(TD, na.rm = TRUE),
    Completions = sum(str_detect(RESULT, "Complete"), na.rm = TRUE),
    Fumbles = sum(Fumble, na.rm = TRUE)
  )

pbp_data_2_0_clean %>%
  filter(ODK == "D") %>%
  group_by(DN) %>%
  summarize(
    Plays = n(),
    Avg_Yards_Gained = mean(GN_LS, na.rm = TRUE),
    TDs_Scored = sum(TD, na.rm = TRUE),
    Completions = sum(str_detect(RESULT, "Complete"), na.rm = TRUE),
    Fumbles = sum(Fumble, na.rm = TRUE),
    First_Downs = sum(as.numeric(First_down), na.rm = TRUE)
  )


pbp_data_2_0 %>%
  filter(ODK == "O") %>%  # Only offensive plays
  group_by(`GAP`) %>%
  summarize(
    Avg_Yards_Gained = mean(as.numeric(`GN/LS`), na.rm = TRUE),
    TDs_Scored = sum(as.numeric(TD), na.rm = TRUE),
    Fumbles = sum(as.numeric(Fumble), na.rm = TRUE),
    Completions = sum(str_detect(RESULT, "Complete"), na.rm = TRUE),
    Plays = n())

pbp_data_2_0 %>%
  filter(ODK == "D") %>%  # Only defensive plays
  group_by(`GAP`) %>%
  summarize(
    Avg_Yards_Gained = mean(as.numeric(`GN/LS`), na.rm = TRUE),
    TDs_Scored = sum(as.numeric(TD), na.rm = TRUE),
    Fumbles = sum(as.numeric(Fumble), na.rm = TRUE),
    Completions = sum(str_detect(RESULT, "Complete"), na.rm = TRUE),
    Plays = n())

pbp_data_2_0 %>%
  filter(ODK == "O",`PLAY TYPE`=="Pass") %>%
  group_by(Cover) %>%
  summarize(
    Avg_Yards_gained = mean(as.numeric(`GN/LS`), na.rm = TRUE),
    TDs_Scored = sum(as.numeric(TD), na.rm = TRUE),
    Completions = sum(str_detect(RESULT, "Complete"), na.rm = TRUE),
    Incompletions = sum(str_detect(RESULT, "Incomplete"), na.rm = TRUE),
    Interceptions = sum(as.numeric(Int))
  )

pbp_data_2_0 %>%
  filter(ODK == "O",`PLAY TYPE`=="Run") %>%
  group_by(GAP) %>%
  summarize(
    Avg_Yards_gained = mean(as.numeric(`GN/LS`), na.rm = TRUE),
    TDs_Scored = sum(as.numeric(TD), na.rm = TRUE),
    Fumbles = sum(as.numeric(Fumble), na.rm = TRUE)
  )



pbp_data_2_0_clean <- pbp_data_2_0 %>%
  mutate(
    GN_LS = as.numeric(`GN/LS`),
    TD = as.numeric(TD),
    Fumble = as.numeric(Fumble),
    Int = as.numeric(Int),
    Sack = as.numeric(Sack),
    Explosive = as.numeric(Explosive),
    Negative = as.numeric(Negative),
    Red_Zone = as.numeric(`Red Zone`),
    Goal_Line = as.numeric(`Goal Line`)
  )



# Original data (from previous step)
df <- data.frame(
  Quarter = c(1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4),
  Down = c(1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4),
  Count = c(64,30,22,3, 77,58,40,13, 89,60,36,11, 67,49,30,9),
  PassConvPct = c(0.13,0.13,0.09,0, 0.06,0.16,0.13,0.23, 0.07,0.12,0.17,0.27, 0.07,0.20,0.03,0.11),
  RunConvPct = c(0.22,0.13,0.09,0, 0.17,0.05,0.08,0.38, 0.10,0.08,0.36,0.55, 0.04,0.12,0.17,0)
)

# Reshape wide -> long for plotting
conversion_long <- pivot_longer(
  df,
  cols = c("PassConvPct", "RunConvPct"),
  names_to = "PlayType",
  values_to = "ConversionPct"
)

# Make sure PlayType is a factor with nice labels
conversion_long$PlayType <- factor(conversion_long$PlayType,
                                   levels = c("PassConvPct", "RunConvPct"),
                                   labels = c("Pass", "Run"))

# Plot
ggplot(conversion_long, aes(x = factor(Down), y = ConversionPct, fill = PlayType)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Quarter) +
  labs(
    title = "Pass vs Run Conversion Rates by Down and Quarter",
    x = "Down",
    y = "Conversion %",
    fill = "Play Type"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("Pass" = "red", "Run" = "grey")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )


conversion_df_D <- data.frame(
  Quarter = c(1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4),
  Down = c(1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4),
  Count = c(72,55,39,5, 74,53,30,4, 41,24,14,2, 49,35,19,9),
  PassConvPct = c(0.07,0.11,0.28,0, 0.03,0.06,0.17,0, 0.02,0.21,0.14,0, 0.10,0.06,0,0.22),
  RunConvPct = c(0.07,0.11,0.10,0, 0.11,0.08,0.03,0, 0.15,0.17,0.14,0, 0.08,0.20,0.16,0.33)
)

conversion_long_D <- pivot_longer(
  conversion_df_D,
  cols = c("PassConvPct", "RunConvPct"),
  names_to = "PlayType",
  values_to = "ConversionPct"
)

conversion_long_D$PlayType <- factor(conversion_long_D$PlayType,
                                     levels = c("PassConvPct", "RunConvPct"),
                                     labels = c("Pass", "Run"))

ggplot(conversion_long_D, aes(x = factor(Down), y = ConversionPct, fill = PlayType)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Quarter) +
  labs(
    title = "Pass vs Run Conversion Rates Allowed by Down and Quarter",
    x = "Down",
    y = "Conversion %",
    fill = "Play Type"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("Pass" = "red", "Run" = "grey")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )
