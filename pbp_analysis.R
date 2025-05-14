ggplot(summary_statistics, aes(x=Total_Yards, y= Win))+
  geom_smooth(se = FALSE)+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))+
  scale_x_continuous(breaks = seq(0,600, by = 50))

model_pbp_1 <- glm(data = summary_statistics, Win~Total_Yards + I(Total_Yards^2), family = "binomial")
stargazer(model_pbp_1, type = "text")

ggplot(summary_statistics, aes(x=TDs, y= Win))+
  geom_smooth(se = FALSE)+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))+
  scale_x_continuous(breaks = seq(0,8, by = 1))

model_pbp_2 <- glm(data = summary_statistics, Win~TDs + I(TDs^2), family = "binomial")
stargazer(model_pbp_2, type = "text")

ggplot(summary_statistics, aes(x=Penalties_Received, y= Win))+
  geom_smooth(se = FALSE)+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))+
  scale_x_continuous(breaks = seq(0,15, by = 1))

model_pbp_3 <- glm(data = summary_statistics, Win~Penalties_Received + I(Penalties_Received^2), family = "binomial")
stargazer(model_pbp_3, type = "text")

new_model <- glm(data = summary_statistics, Win~Penalties_Received + I(Penalties_Received^2) + TDs + I(TDs^2) + Total_Yards + I(Total_Yards^2), family = "binomial")
stargazer(new_model, type = "text")
