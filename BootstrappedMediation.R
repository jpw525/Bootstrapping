##For this project I ran mediation analysis with an ordinal DV. First, I do mediation the old fashioned way (Baron & Kenny, 1986), then I do it with bootstrapping. I have removed most of the analysis to just show example code.

#Anger, Mate Value, and Aggression
# Target: 1 = target high in mate value, 0 = target low in mate value
library(MASS)
library(car)
library(ggplot2)
library(psy)
library(e1071) 

#################################
#N=202, undergrads
emotionUT <- read.csv("~/Documents/School/Research/Emotion.csv")

emotionUT$sex <- as.factor(emotionUT$sex)
emotionUT$cond <- as.factor(emotionUT$cond)
summary(emotionUT$directagg)
summary(emotionUT$indirectagg)


library(psych)
library(psy)
#get cronbach's alphas for each scale
cronbach(emotionUT[,c(4,6,7,10,13,89:93)])#global = cronbach = 0.909
cronbach(emotionUT[,c(25,27,28,31,32,79:83)])#inclusion = cronbach = 0.862
cronbach(emotionUT[,c(36,38,42:45,84:88)])#dominance = cronbach = 0.814
cronbach(emotionUT[,c(47,48,50,52,54,56,94:99)])#mate = cronbach = 0.852
cronbach(emotionUT[,c(67,72)])#anger = cronbach = 0.81
cronbach(emotionUT[,c(68,71)])#embarrass = cronbach = 0.89
cronbach(emotionUT[,c(69,70)])#fear = cronbach = 0.68
cronbach(emotionUT[,c(73,76)])#sad = cronbach = 0.75


#create ordinal aggression terms to run ordinal aggression
emotionUT$aggtotalf <- as.ordered(emotionUT$aggtotal)
emotionUT$directaggf <- as.ordered(emotionUT$directagg)
emotionUT$indirectaggf <- as.ordered(emotionUT$indirectagg)


#subset gender
emotionUTmen<- subset(emotionUT, sex==1)
emotionUTwomen<- subset(emotionUT, sex==0)


##ordinal regression
sexmate1 <- polr(indirectaggf~sex*mate, data=emotionUT, Hess=TRUE)
sexmate2 <- polr(indirectaggf~sex+mate, data=emotionUT, Hess=TRUE)
#test for interaction between sex and mate value
anova(sexmate1,sexmate2) 

#extract p-values
ctable1 <- coef(summary(sexmate2)) #no interaction (p=.4)
p <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable1, "p value" = p)
ci <- confint.default(sexmate2)


###4 step regression approach to mediation
#(running analysis silmultaneously with strength, cond, and mate)

#step 1
direct1 <- polr(directaggf~sex+cond+s11_strength+mate, data = emotionUT, Hess=TRUE)
summary(direct1)
ctable <- coef(summary(direct1)) #no interaction (p=.4)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

direct1.1 <- polr(directaggf~sex+cond+mate, data = emotionUT, Hess=TRUE)
anova(direct1, direct1.1)#strength is significant

direct1.2 <- polr(directaggf~sex+s11_strength+mate, data = emotionUT, Hess=TRUE)
anova(direct1, direct1.2)#condition is significant


#step 2
mediate <- lm(anger~cond+sex+s11_strength+mate, data=emotionUT)
summary(mediate)

#step 3
direct2 <- polr(directaggf~sex+cond+s11_strength+mate+anger, data=emotionUT, Hess=TRUE)
summary(direct2)
ctable <- coef(summary(direct2)) #no interaction (p=.4)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
direct2a <- polr(directaggf~sex+cond+s11_strength+mate, data=emotionUT, Hess=TRUE)
summary(direct2a)
anova(direct2,direct2a)

####bootstrapped mediation 

library(arm)
library(mediation)
med.fit <- lm(anger~cond+sex+s11_strength+mate, data=data)
out.fit <- polr(directaggf~anger+cond+sex+s11_strength+mate, data=data, Hess=T)
med.out <-  mediate(med.fit, out.fit,treat = "cond", mediator = "anger", boot=T, sims=100)
summary(med.out)

#boot = true uses non parametric bootstrapping
#boot = false uses quasi-bayesian monte carlo
#In addition, we can use the nonparametric bootstrap rather than the quasi-Bayesian Monte Carlo simulation for variance estimation via the boot = TRUE argument,
#ACME = average causal mediation effect
#ADE = average direct effect

####references

# Preacher, K. and Hayes, A. (2008). Asymptotic and resampling strategies for assessing and comparing indirect effects in multiple mediator models. 

##bootstrap mediation code creds to:
#Imai, Keele, Tingley, and Yamamoto (2017). Causal Mediation Analysis Using R.