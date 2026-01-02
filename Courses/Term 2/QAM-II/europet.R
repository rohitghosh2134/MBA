europ <- read.csv("europet.csv")
summary(europ)
colnames(europ) <- c("Week", "Sales", "TV", "Radio","FV", "FP",
                     "Temp","Prec", "Holiday", "Visits")
head(europ)
str(europ)
class(europ)
dim(europ)

attach(europ)

#Q1
summary(europ[c("Sales", "FV", "TV", "Radio")])

#Q2
plot(FV, Sales, xlab = "Fuel Volume", ylab = "Sales",
     main = "Sales vs FV")

fit1 <- lm(Sales~FV, data=europ)

summary(fit1)

confint(fit1,level=0.95)

#------------------------------------------------------------
resid1 <- as.matrix(residuals(fit1))
print(resid1)
summary(resid1)

library(robustHD)
stdzresid1 <- standardize(resid1, centerFun = mean, 
                         scaleFun = sd)
stdzresid1

stdzfitted1 <- standardize(fitted(fit1), centerFun = mean, 
                          scaleFun = sd)
stdzfitted1
plot(stdzfitted1, stdzresid1)
abline(h=0)
library(lmtest)
bptest(fit1,studentize = FALSE)
#-----------------------------------------------------------
# normal probability plot of residuals
probDist1 <- pnorm(stdzresid1)
probDist1
plot(ppoints(length(stdzresid1)), sort(probDist1), 
     main = "PP Plot", xlab = "Observed Probability", 
     ylab = "Expected Probability")

abline(0,1)

shapiro.test(stdzresid1)
ks.test(stdzresid1, "pnorm")
#-----------------------------------------------------------
library(car)
durbinWatsonTest(fit1, simulate = TRUE)
#-----------------------------------------------------------
# prediction interval
newdata <- data.frame(FV=56259)
newdata
predict(fit1, newdata, interval="confidence",
        se.fit=TRUE)

newdata1 <- data.frame(FV=62853)
newdata1
predict(fit1, newdata1, interval="confidence",
        se.fit=TRUE)

newdata2 <- data.frame(FV=68549)
newdata2
predict(fit1, newdata2, interval="confidence",
        se.fit=TRUE)
#-----------------------------------------------------------