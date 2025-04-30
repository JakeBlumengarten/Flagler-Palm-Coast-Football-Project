library(ggplot2)
library(tidyverse)
library(dplyr)
library(stargazer)
library(readxl)

FPC_data <- read_excel("Datasets/FPC_data.xlsx")
FPC_data <- FPC_data %>%
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date))



########### How do sacks effect oppoint points? ######################

ggplot(FPC_data, aes(x=Sacks, y= opponent_points))+
  geom_point()+
  geom_smooth(se = FALSE)

model_1 <- lm(data = FPC_data, opponent_points~Sacks)
stargazer(model_1, type = "text")

########### How do Tackles For Loss effect oppoint points? ###########

ggplot(FPC_data, aes(x=TFL, y= opponent_points))+
  geom_point()+
  geom_smooth(se = FALSE)

model_2 <- lm(data = FPC_data, opponent_points~TFL)
stargazer(model_2, type = "text")

########### How do QB pass attempts effect points for? ###############

filtered_data <- FPC_data%>%
  filter(QB_Att > 0)

ggplot(filtered_data, aes(x=QB_Att, y= points_for))+
  geom_point()+
  geom_smooth(se = FALSE)
  
model_3 <- lm(data = filtered_data, points_for~QB_Att)
stargazer(model_3, type = "text")

######### How do QB pass attempts effect the outcome of the game? #####

ggplot(FPC_data, aes(x = QB_Att, y = Result)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE, color = 'blue')

model_4 <- lm(data = FPC_data, Result~QB_Att)
stargazer(model_4, type = "text")

######### How do QB completion % effect the outcome of the game? #####

ggplot(FPC_data, aes(x = `QB_C%`, y = Result)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE, color = 'blue')

model_5 <- glm(Result ~`QB_C%`, data = FPC_data, family = binomial)
stargazer(model_5, type = "text")


######### How do QBR effect the outcome of the game? ##################

ggplot(FPC_data, aes(x = `QB Rate`, y = Result)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE, color = 'blue')

model_6 <- glm(Result ~ `QB Rate`, data = FPC_data, family = binomial)
stargazer(model_6, type = "text")
