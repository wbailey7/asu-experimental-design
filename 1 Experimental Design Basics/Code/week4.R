library(agricolae)
library(ggplot2)

#Randomized Complete Block Design for Vascular Graft Experiment
psi.8500 <- c(90.3, 89.2, 98.2, 93.9, 87.4, 97.9)
psi.8700 <- c(92.5, 89.5, 90.6, 94.7, 87.0, 95.8)
psi.8900 <- c(85.5, 90.8, 89.6, 86.2, 88.0, 93.4)
psi.9100 <- c(82.5, 89.5, 85.6, 87.4, 78.9, 90.7)
block    <- c(1, 2, 3, 4, 5, 6)

#Organize Data
df <- cbind(stack(data.frame(psi.8500, psi.8700, psi.8900, psi.9100)), block)
names(df)[names(df) == 'values'] <- 'yield'
names(df)[names(df) == 'ind'] <- 'pressure'

#Plot Data
p <- ggplot(df, aes(x=factor(pressure), y=yield)) +
  geom_boxplot() + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
p

#ANOVA
fit <- aov(yield ~ factor(block) + factor(pressure), data = df)
summary(fit)

#Fisher LSD Test
print(LSD.test(fit, "factor(pressure)"))

#Residual Analysis
#Pressure Versus Residuals
pressure_residuals <- 
  ggplot(df) + 
  geom_point(aes(factor(pressure), resid(fit)), alpha = 1/4) + 
  geom_hline(yintercept = 0) +
  geom_smooth(aes(factor(pressure), resid(fit)), se = FALSE, size = 0.5) +
  labs(x="Pressure", y = "Residual")

pressure_residuals
#Pressure doesn't have any significant variance in residuals

#Block Versus Residuals
block_residuals <- 
  ggplot(df) + 
  geom_point(aes(factor(block), resid(fit)), alpha = 1/4) + 
  geom_hline(yintercept = 0) +
  geom_smooth(aes(factor(block), resid(fit)), se = FALSE, size = 0.5) +
  labs(x="Block", y = "Residual")

block_residuals
#Low variance in block 6.

#Fitted Versus Residuals
fitted_residuals <- 
  ggplot(df) + 
  geom_point(aes(fitted(fit), resid(fit)), alpha = 1/4) + 
  geom_hline(yintercept = 0) +
  geom_smooth(aes(fitted(fit), resid(fit)), se = FALSE, size = 0.5) +
  labs(x="Fitted", y = "Residual")

fitted_residuals