ggplot(summary_statistics, aes(x=Total_Yards, y= Win))+
  geom_smooth(se = FALSE)+
  ylim(0,1)

model_1 <- glm(data = summary_statistics, Win~Total_Yards + I(Total_Yards^2), family = "binomial")
stargazer(model_1, type = "text")


ggplot(summary_statistics, aes(x=TDs, y= Win))+
  geom_smooth(se = FALSE)+
  ylim(0,1)
