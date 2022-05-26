library(agricolae)

#Chocolate data - response = Blood Plasma Levels 1 hour after consumption (antioxidant capacity)
DC<-c(118.8, 122.6, 115.6, 113.6, 119.5, 115.9, 115.8, 115.1, 116.9, 115.4, 115.6, 107.9)
DC_MK <-c(105.4, 101.1, 102.7, 97.1, 101.9, 98.9, 100.0, 99.8, 102.6, 100.9, 104.5, 93.5)
MC <-c(102.1, 105.8, 99.6, 102.7, 98.8, 100.9, 102.8, 98.7, 94.7, 97.8, 99.7, 98.6)

#Create Boxplot
boxplot(DC, DC_MK, MC,
        names=c("Dark","Dark-Milk", "Milk"))

#Put into DF
chocolate <- stack(data.frame(DC, DC_MK, MC))
names(chocolate)[names(chocolate) == 'values'] <- 'response'
names(chocolate)[names(chocolate) == 'ind'] <- 'factor'

#one way anova
one.way <- aov(response ~ factor, data = chocolate)
summary(one.way)

#using linear model
mod <- lm(response ~ factor, data = chocolate)
anova(mod)

#F-Test
qf(0.95, df1=2, df2=33)

#R-Squared
summary(mod)

#Fisher T-Tests
print(LSD.test(one.way, "factor"))


#Random Factor Experiment - Strength Data
#May see loom to loom variation. Select 4 looms at random and make four strength
# determinations on the fabric manufactured on each loom.
l1 <- c(98, 97, 99, 96)
l2 <- c(91, 90, 93, 92)
l3 <- c(96, 95, 97, 95)
l4 <- c(95, 96, 99, 98)

#Put into DF
loom <- stack(data.frame(l1, l2, l3, l4))
names(loom)[names(loom) == 'values'] <- 'strength'
names(loom)[names(loom) == 'ind'] <- 'loom'

#one way anova
one.way <- aov(strength ~ loom, data = loom)
summary(one.way)

#Variance Components
sig_tau <- (29.729 - 1.896) / 4
total_var = 1.896 + sig_tau
total_var

#Since Sig Tau >> 1.896, most variability is loom-to-loom


#REML Method (Max Likelihood)
library(lme4)
reml_fit <- lmer(strength~ 1 + (1|loom), REML=T, data=loom)
summary(reml_fit)


###########################
# ONE FACTOR ANOVA EXAMPLE #
# Engineer is interested in relationship between RF power setting and etch rate for
# single wafer plasma etching tool
# Testing four levels of RF Power: 160, 180, 200, 220W
# Five wafers at each power level in randomized order

rf.160 <- c(575, 542, 530, 539, 570)
rf.180 <- c(565, 593, 590, 579, 610)
rf.200 <- c(600, 651, 610, 637, 629)
rf.220 <- c(725, 700, 715, 685, 710)

#Create Boxplot
boxplot(rf.160, rf.180, rf.200, rf.220, 
        names=c("160W","180W", "200W", "220W"))

#Put into DF
power <- stack(data.frame(rf.160, rf.180, rf.200, rf.220))
names(power)[names(power) == 'values'] <- 'etch_rate'
names(power)[names(power) == 'ind'] <- 'power'

#one way anova
one.way <- aov(etch_rate ~ power, data = power)
summary(one.way)

#Fisher T-Tests
print(LSD.test(one.way, "power"))

#Using Model
#using linear model
mod <- lm(etch_rate ~ power, data = power)
summary(mod)

#Constant Variance
plot(power$power, resid(mod),
     ylab="Residuals", xlab="Power",
     main="Etch Rate Experiment")
abline(0,0)

#Plot normal QQ
par(mfrow=c(1,2))

qqnorm(resid(mod), main="Normal QQ Plot")
qqline(resid(mod))
