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

#Q5a.
fit5 <- lm(Sales~TV + Radio + FV + FP + Temp 
           + Prec + Holiday+ Visits, data=europ)

summary(fit5)

confint(fit5,level=0.95)
#------------------------------------------------------------
resid5 <- as.matrix(residuals(fit5))
print(resid5)
summary(resid5)

library(robustHD)
stdzresid5 <- standardize(resid5, centerFun = mean, 
                         scaleFun = sd)
stdzresid5

stdzfitted5 <- standardize(fitted(fit5), centerFun = mean, 
                          scaleFun = sd)
stdzfitted5
plot(stdzfitted5, stdzresid5)
abline(h=0)
bptest(fit5, studentize = FALSE)
#-----------------------------------------------------------
# normal probability plot of residuals
probDist5 <- pnorm(stdzresid5)
probDist5
plot(ppoints(length(stdzresid5)), sort(probDist5), 
     main = "PP Plot", xlab = "Observed Probability", 
     ylab = "Expected Probability")

abline(0,1)

shapiro.test(stdzresid5)
ks.test(stdzresid5, "pnorm")
#-----------------------------------------------------------
library(car)
durbinWatsonTest(fit5, simulate = FALSE)
vif(fit5)
#-----------------------------------------------------------
#Q5a continued.
fit6 <- lm(Sales~TV + FV + FP + Temp 
           + Prec + Holiday+ Visits, data=europ)

summary(fit6)

confint(fit6,level=0.95)
#------------------------------------------------------------
resid6 <- as.matrix(residuals(fit6))
print(resid6)
summary(resid6)

library(robustHD)
stdzresid6 <- standardize(resid6, centerFun = mean, 
                          scaleFun = sd)
stdzresid6

stdzfitted6 <- standardize(fitted(fit6), centerFun = mean, 
                           scaleFun = sd)
stdzfitted6
plot(stdzfitted6, stdzresid6)
abline(h=0)
bptest(fit6)
#-----------------------------------------------------------
# normal probability plot of residuals
probDist6 <- pnorm(stdzresid6)
probDist6
plot(ppoints(length(stdzresid6)), sort(probDist6), 
     main = "PP Plot", xlab = "Observed Probability", 
     ylab = "Expected Probability")

abline(0,1)

shapiro.test(stdzresid6)
ks.test(stdzresid6, "pnorm")
#-----------------------------------------------------------
library(car)
durbinWatsonTest(fit6, simulate = FALSE)
vif(fit6)
#-----------------------------------------------------------
#standardized beta coefficients
stdzeurop <- standardize(europ, centerFun = mean, 
            scaleFun = sd)

stdzfit <- lm(Sales~TV + FV + FP + Temp 
   + Prec + Holiday+ Visits, data=stdzeurop)

summary(stdzfit)
