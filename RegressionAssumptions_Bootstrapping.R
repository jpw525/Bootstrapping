# thesis data study 1
setwd("~/Desktop")
thesis1 <- read.csv("Study1.csv", header = TRUE)
thesis1$sex <- as.factor(thesis1$sex)

library(car)
#A researcher comes in with some data of aggression, they aren't sure what to do because there variables are skewed. Previous research suggests that aggression declines with age, but studies have looked at older populations and they want to know if age still predicts aggression in their younger sample.

#testing age 
regression1 <- lm(directagg~age, data=thesis1)
summary(regression1)
#save residuals and predicted values
thesis1$resid <- regression1$residuals
thesis1$pred <- regression1$fitted.values
#assumption checks:
qqPlot(thesis1$resid, main= "Aggression and Age", ylab="Residuals") #normality assumption not met

plot(thesis1$resid~thesis1$pred, type="p", main="Residuals and Predicted Values", xlab="Predicted Values", ylab="Residuals", pch=20) #predicted values ~ residuals, homogeneity of variance assumption not met

hist(thesis1$directagg, main="Histogram of Aggression", xlab="Direct Aggression", col="grey") #floor effect of directagg

#the assumptions are not met to run a regression so we use bootstrapping to test for significance

##### Bootstrap Approach ######

#bootstrap t-value for directagg~age
bootstrap_ts <- rep(0, 10000)   # Initializing a vector for storing the results.

for (i in 1:10000) {
  bootsample <- thesis1[sample(nrow(thesis1),nrow(thesis1),replace=T), ]   # The sample function pulls nrow(Dataset) indices with replacement from 1:nrow(Dataset); these rows are then retained in bootsample.
  bootreg <- lm(directagg~age, data=bootsample)
  #pull out the beta for age instead of the t for age
  boot_t <- summary(bootreg)$coefficients[2,3]  # This is the t value from the summary table.
  bootstrap_ts[i] <- as.numeric(boot_t)
}

quantile(bootstrap_ts, c(.025,.5,.975)) #t ~ -3.96
sd(bootstrap_ts)
qt(c(.025, .975), df=1000) #tcrit= +/-1.962


###instead of doing that, you can bootstrap CI's around your beta and see if they cross 0
###for this, I write the bootstrap code (will use the boot function next)
bootstrap_age <- rep(0, 10000)   # Initializing a vector for storing the results.

for (i in 1:10000) {
  bootsample <- thesis1[sample(nrow(thesis1),nrow(thesis1),replace=T), ]   # The sample function pulls nrow(Dataset) indices with replacement from 1:nrow(Dataset); these rows are then retained in bootsample.
  bootreg <- lm(directagg~age, data=bootsample)
  #pull out the beta for age instead of the t for age
  boot_t <- summary(bootreg)$coefficients[2,1]  # This is the beta value from the summary table.
  bootstrap_age[i] <- as.numeric(boot_t)
}

quantile(bootstrap_age, c(.025,.5,.975))
sd(bootstrap_age)



####
#as they thought, age predicted aggression. However, they come in the next week and they say they just read a paper saying the sex predicts aggression. Now, we add sex to the model (I checked using bootstrap and there is not sex by age interaction so i am just added sex)

#now I will add a control variables to the model and will get CI's around R2 and my betas
regression3 <- lm(directagg~sex+age, data=thesis1)
summary(regression3)

thesis1$resid3 <- regression3$residuals
thesis1$pred3 <- regression3$fitted.values

qqPlot(thesis1$resid3) #normality no too bad but is a bit off in the tails
plot(thesis1$resid3~thesis1$pred, type="p") #assumption not bad
#let's bootstrap anyways

library(boot)
#get CI's for regression coefficients
###using boot function
# Bootstrap 95% CI for regression coefficients 
# function to obtain regression weights 
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=thesis1, statistic=bs, 
                R=1000, formula=directagg~sex+age)
# view results
results
plot(results, index=1) # intercept 
plot(results, index=2) # sex 
plot(results, index=3) # age 

# get 95% confidence intervals , us BCA
boot.ci(results, type="bca", index=1) # intercept 
boot.ci(results, type="bca", index=2) # sex 
boot.ci(results, type="bca", index=3) # age

#sex and age both don't cross 0, confirming significance
#dataEllipse(results$index=2, resuts$t[,3], xlab="Sex", ylab="Age", cex=.3,levels=c(.5,.95,.99), robust=t)

#diagnostic test of the statistic and the bootstrapped distribution to deletion of individual observations
#the horizontal axis the the measure of influence of each observation on the coefficient
#numbers on bottom are obseration indices of points (e.g. observation 96 decreasing sex coefficient but increasing age coefficient)
#horizontal dashed lines are the quantiles of the bootstrap distribtion of each coefficient centered at the value of the coefficient for the original sample
#the points connecting the lines show the quantiles estimated only from bootstrap samples in which each observation idd not appear.
jack.after.boot(results, index=2, main = "(a) sex coefficient")
jack.after.boot(results, index=3, main = "(b) age coefficient")



# Bootstrap 95% CI for R-Squared
##now i am using the boot function instead of writing for loop
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
results <- boot(data=thesis1, statistic=rsq, 
                R=1000, formula=directagg~sex+age)

# view results
results 
plot(results)

# get 95% confidence interval for rsq, we don't have a very high rsq :(
boot.ci(results, type="bca")


###graph sex effect
males <- subset(thesis1, sex==1)
females <- subset(thesis1, sex==2)
hist(males$directagg,main='Histogram of Direct Aggression',xlab='Direct Aggression',col='#31a35470')
#breaks function adjusts the bin sizes. add breaks=  to your histogram code)
hist(females$directagg,col='#756bb170',add=T)
legend(7,20,c('Women','Men'),pch=c(20,20),col=c('#756bb1','#31a354'), cex=.85)

#convert age codes into years (1 = 18, 2, = 19, etc.)
thesis1$age1 <- thesis1$age+17

##graph age effect
plot(thesis1$age1,thesis1$directagg,main='Age and Direct Aggression',xlab='Age(yrs)',ylab='Direct Aggression',pch=20, col="blue", cex = .5 )
abline(lm(thesis1$directagg~thesis1$age1),col='red')


#####----References-----####
# http://www.statmethods.net/advstats/bootstrapping.html


