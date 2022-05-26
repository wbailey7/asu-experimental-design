#Question 1
#I belong to a golf club in my neighborhood. I divide the year into three golf 
#seasons: summer (June-September), winter (November-March), and shoulder 
#(October, April, and May). I believe that I play my best golf during the summer 
#(because I have more time and the course isn't crowded) and shoulder 
#(because the course isn't crowded) seasons, and my worst golf is during the winter 
#because when all of the part-year residents show up, the course is crowded, 
#play is slow, and I get frustrated). Data from the last year are shown in the following table.

summer <- c(83, 85, 85, 87, 90, 88, 88, 84, 91, 90)
shoulder <- c(91, 87, 84, 87, 85, 86, 83, NA, NA, NA)
winter <- c(94, 91, 87, 85, 87, 91, 92, 86, NA, NA)

#Create Boxplot
boxplot(summer, shoulder, winter,
        names=c("Summer","Shoulder", "Winter"))

#Put into DF
df <- stack(data.frame(summer, shoulder, winter))
names(df)[names(df) == 'values'] <- 'score'
names(df)[names(df) == 'ind'] <- 'season'

#one way anova
one.way <- aov(score ~ season, data = df)
summary(one.way)

mod <- lm(score ~ season, data = df)
par(mfrow=c(1,2))

qqnorm(resid(mod), main="Normal QQ Plot")
qqline(resid(mod))
plot(na.omit(df$score), resid(mod),
     ylab="Residuals", xlab="Score",
     main="Golf Score Experiment")
abline(0,0)