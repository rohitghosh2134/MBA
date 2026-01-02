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

#Q4a.
fit3 <- lm(Sales~TV + Radio + Temp, data=europ)

summary(fit3)

confint(fit3,level=0.95)
#------------------------------------------------------------
resid3 <- as.matrix(residuals(fit3))
print(resid3)
summary(resid3)

library(robustHD)
stdzresid3 <- standardize(resid3, centerFun = mean, 
                         scaleFun = sd)
stdzresid3

stdzfitted3 <- standardize(fitted(fit3), centerFun = mean, 
                          scaleFun = sd)
stdzfitted3
plot(stdzfitted3, stdzresid3)
abline(h=0)
bptest(fit3, studentize = FALSE)
#-----------------------------------------------------------
# normal probability plot of residuals
probDist3 <- pnorm(stdzresid3)
probDist3
plot(ppoints(length(stdzresid3)), sort(probDist3), 
     main = "PP Plot", xlab = "Observed Probability", 
     ylab = "Expected Probability")

abline(0,1)

shapiro.test(stdzresid3)
ks.test(stdzresid3, "pnorm")
#-----------------------------------------------------------
library(car)
durbinWatsonTest(fit3, simulate = FALSE)
vif(fit3)
#-----------------------------------------------------------
#Q4b.
fit4 <- lm(Sales~TV + Temp, data=europ)

summary(fit4)

confint(fit4,level=0.95)
#------------------------------------------------------------
resid4 <- as.matrix(residuals(fit4))
print(resid4)
summary(resid4)

library(robustHD)
stdzresid4 <- standardize(resid4, centerFun = mean, 
                          scaleFun = sd)
stdzresid4

stdzfitted4 <- standardize(fitted(fit4), centerFun = mean, 
                           scaleFun = sd)
stdzfitted4
plot(stdzfitted4, stdzresid4)
abline(h=0)
bptest(fit4, studentize = FALSE)
#-----------------------------------------------------------
# normal probability plot of residuals
probDist4 <- pnorm(stdzresid4)
probDist4
plot(ppoints(length(stdzresid4)), sort(probDist4), 
     main = "PP Plot", xlab = "Observed Probability", 
     ylab = "Expected Probability")

abline(0,1)

shapiro.test(stdzresid4)
ks.test(stdzresid4, "pnorm")
#-----------------------------------------------------------
library(car)
durbinWatsonTest(fit4, simulate = FALSE)
vif(fit4)
#-----------------------------------------------------------