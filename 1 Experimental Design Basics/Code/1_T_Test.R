#Define cement data
modified <-c(16.85, 16.40, 17.21, 16.35, 16.52, 17.04, 16.96, 17.15, 16.59, 16.57)
unmodified <-c(16.62, 16.75, 17.37, 17.12, 16.98, 16.87, 17.34, 17.02, 17.08, 17.27)

#Plot normal QQ
par(mfrow=c(1,2))

qqnorm(modified, main="Modified")
qqline(modified)

qqnorm(unmodified, main="Unmodified")
qqline(unmodified)

#Normal Test - H0: drawn from normal distribution
shapiro.test(modified)
shapiro.test(unmodified)

#Create Boxplot
boxplot(modified, unmodified,
        names=c("Modified","Unmodified"))

#Variance Test - H0: variances are equal
var.test(modified, unmodified)

#T Test (equal variances)
t.test(modified, unmodified, alternative="two.sided", var.equal=TRUE)

#Define Florescence Data
nerve <-c(6625, 6000, 5450, 5200, 5175, 4900, 4750, 4500, 3985, 900, 450, 2800)
muscle <-c(3900, 3500, 3450, 3200, 2980, 2800, 2500, 2400, 2200, 1200, 1150, 1130)

#Plot normal QQ
par(mfrow=c(1,2))

qqnorm(nerve, main="Nerve")
qqline(nerve)

qqnorm(muscle, main="Muscle")
qqline(muscle)

#Normal Test - H0: drawn from normal distribution
shapiro.test(nerve)
shapiro.test(muscle)

#Create Boxplot
boxplot(nerve, muscle,
        names=c("nerve","muscle"))

#Variance Test - H0: variances are equal
var.test(nerve, muscle)

#T Test (unequal variances)
t.test(nerve, muscle, alternative="greater", var.equal=FALSE)


#Specimen to specimen variation (paired t test - differences)
tip1 <-c(7, 3, 3, 4, 8, 3, 2, 9, 5, 4)
tip2 <-c(6, 3, 5, 3, 8, 2, 4, 9, 4, 5)

t.test(tip1, tip2, paired = TRUE, alternative = "two.sided")
