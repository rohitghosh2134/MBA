getwd()


mba <- read.csv("C:/MBA/MBA/Courses/QAM2/mbastudt.csv")
class(mba)
summary(mba)
head(mba)
mba1 <- mba[-c(1,4:13)]
head(mba1)
tail(mba1)
str(mba1)
class(mba1)
dim(mba1)

mba1$percentage

attach(mba1)
percentage
salary

plot(percentage, salary, xlab = "percent", ylab = "salary",
     main = "factors impacting salary", sub = "salary vs grade10 percent")

fit <- lm(salary~percentage, data=mba1)

# regression summary
summary(fit)

# anova table
anova(fit)

# confidence interval on coefficient estimates
confint(fit,level=0.95)

# residuals from the estimation
resid <- as.matrix(residuals(fit))
print(resid)
df.residual(fit)
summary(resid)
# fitted values
#fitted.values(fit)
fitted(fit) # point predictions
#------------------------------------------------------------
# standardized residuals
library(robustHD)
stdzresid <- standardize(resid, centerFun = mean,
                         scaleFun = sd)
stdzresid
#-----------------------------------------------------------
# normal probability plot of residuals
probDist <- pnorm(stdzresid)
probDist
plot(sort(probDist))
plot(ppoints(length(stdzresid)), sort(probDist), 
     main = "PP Plot", xlab = "Observed Probability", 
     ylab = "Expected Probability")

abline(0,1)

#qqnorm(stdzresid, ylab="Standardized Residuals", xlab="Normal Scores")
#qqline(stdzresid)
shapiro.test(stdzresid)
ks.test(stdzresid,"pnorm")
#library(dgof)
#ks.test(stdzresid, "pnorm", 0, 1)
#----------------------------------------------------------------
# checking for homoscedasticity
stdzfitted <- standardize(fitted(fit), 
                          centerFun = mean, 
                          scaleFun = sd)
stdzfitted
plot(stdzfitted, stdzresid)
abline(h=0)
plot(resid) #checking for time trends

# Breusch Pagan test
library(lmtest)
bptest(fit)
#---------------------------------------------------------------
#checking for autocorrelation among residuals
# H0: no correlation among the residuals; H1: correlation exists
library(car)
durbinWatsonTest(fit, simulate = FALSE)
#-----------------------------------------------------------
cook <- cooks.distance(fit)

leverage <- hatvalues(fit)

#mahalanobis(mba1, colMeans(mba1), cov(mba1))

Zscore <- (fitted(fit) - mean(salary))/sd(salary)

write.csv(cbind(percentage, salary, cook, leverage, Zscore),
          "outlier.csv")

#-----------------------------------------------------------
# prediction interval

percentage
newdata <- data.frame(percentage=60)
newdata
predict(fit, newdata, interval="prediction",se.fit=TRUE)
predict(fit, newdata, interval="confidence",se.fit=TRUE)
#-----------------------------------------------------------

