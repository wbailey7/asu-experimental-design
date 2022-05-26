flow.125 <-c(2.7, 4.6, 2.6, 3.0, 3.2, 3.8)
flow.200 <-c(4.6, 3.4, 2.9, 3.5, 4.1, 5.1)

#Plot normal QQ
par(mfrow=c(1,2))

qqnorm(flow.125, main="125")
qqline(flow.125)

qqnorm(flow.200, main="200")
qqline(flow.200)

#Normal Test - H0: drawn from normal distribution
shapiro.test(flow.125)
shapiro.test(flow.200)

#Create Boxplot
boxplot(flow.125, flow.200,
        names=c("125","200"))

#Variance Test - H0: variances are equal
var.test(flow.125, flow.200)

#T Test
t.test(flow.125, flow.200, alternative="two.sided", var.equal=TRUE)

#Paired T Test
t.test(flow.125, flow.200, paired = TRUE, alternative = "two.sided")
