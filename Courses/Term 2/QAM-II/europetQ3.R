europ <- read.csv("europet.csv")
summary(europ)
colnames(europ) <- c("Week", "Sales", 
                     "TV", "Radio","FV", "FP",
          "Temp","Prec", "Holiday", "Visits")
head(europ)
str(europ)
class(europ)
dim(europ)

attach(europ)

#Q3
fit2 <- lm(Sales~TV + Radio, data=europ)

summary(fit2)

confint(fit2,level=0.95)
#------------------------------------------------------------
resid2 <- as.matrix(residuals(fit2))
print(resid2)
summary(resid2)

library(robustHD)
stdzresid2 <- standardize(resid2, centerFun = mean, 
                         scaleFun = sd)
stdzresid2

stdzfitted2 <- standardize(fitted(fit2), centerFun = mean, 
                          scaleFun = sd)
stdzfitted2
plot(stdzfitted2, stdzresid2)
abline(h=0)
library(lmtest)
bptest(fit2, studentize = FALSE)
#-----------------------------------------------------------
# normal probability plot of residuals
probDist2 <- pnorm(stdzresid2)
probDist2
plot(ppoints(length(stdzresid2)), sort(probDist2), 
     main = "PP Plot", xlab = "Observed Probability", 
     ylab = "Expected Probability")

abline(0,1)

shapiro.test(stdzresid2)
ks.test(stdzresid2, "pnorm")
#-----------------------------------------------------------
library(car)
durbinWatsonTest(fit2, simulate = FALSE)
vif(fit2)
#-----------------------------------------------------------
# prediction interval
TV <- 40
Radio <- 80
newdata3 <- data.frame(TV,Radio)
newdata3
predict(fit2, newdata3, interval="confidence",
        se.fit=TRUE)

predict(fit2, newdata3, interval="prediction",
        se.fit=TRUE)
#-----------------------------------------------------------
