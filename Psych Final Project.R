#Descriptive statistics:
#Mean and SD for each question
#Mean and SD for the overall construct that you are measuring
#Frequency by gender

#Descriptive Statistics: Mean
mean(data_stresseating$eating1)
mean(data_stresseating$eating2)
mean(data_stresseating$eating3)
mean(data_stresseating$eating4)
mean(data_stresseating$eating5)
mean(data_stresseating$eating6)
mean(data_stresseating$eating7)
mean(data_stresseating$eating8)
mean(data_stresseating$eating9)
mean(data_stresseating$eating10)
mean(data_stresseating$eating11)
mean(data_stresseating$eating12)
mean(data_stresseating$lifesat1)
mean(data_stresseating$lifesat2)
mean(data_stresseating$lifesat3)
mean(data_stresseating$lifesat4)
mean(data_stresseating$lifesat5)
mean(data_stresseating$lifesat6)
mean(data_stresseating$SUM_EATING)
mean(data_stresseating$SUM_LIFESAT)

#Descriptive Statistics: SD
sd(data_stresseating$eating1)
sd(data_stresseating$eating2)
sd(data_stresseating$eating3)
sd(data_stresseating$eating4)
sd(data_stresseating$eating5)
sd(data_stresseating$eating6)
sd(data_stresseating$eating7)
sd(data_stresseating$eating8)
sd(data_stresseating$eating9)
sd(data_stresseating$eating10)
sd(data_stresseating$eating11)
sd(data_stresseating$eating12)
sd(data_stresseating$lifesat1)
sd(data_stresseating$lifesat2)
sd(data_stresseating$lifesat3)
sd(data_stresseating$lifesat4)
sd(data_stresseating$lifesat5)
sd(data_stresseating$lifesat6)
sd(data_stresseating$SUM_EATING)
sd(data_stresseating$SUM_LIFESAT)

#Descriptive Statistics: Frequency by Gender
table(data_stresseating$Gender)

#Cronbach’s alpha 
library(psych)
data_reversed <- data_stresseating
names(data)
columns_to_reverse <- c("lifesat1","lifesat2","lifesat3","lifesat4","lifesat5","lifesat6")
data_reversed[ ,columns_to_reverse] <- 6 - data_stresseating[ ,columns_to_reverse]
names(data_reversed)

alpha((data_reversed[ ,3:14]))

#McDonald’s omega
omega(data_reversed[ ,3:14], nfactors = 1)
 
#confirmatory factor analysis standardized 
library(lavaan)
eating_structure <- "eating =~ eating1 + eating2 +eating3 + eating4 +eating5 +eating6 + eating7 + eating8 + eating9 + eating10 + eating11 + eating12
                      lifesat =~ lifesat1 + lifesat2 +lifesat3 +lifesat4 + lifesat5 + lifesat6"
eating_fit <- cfa (model = eating_structure, data = data_reversed, estimator = "MLR")
summary(eating_fit, fit.measures = T, standardized = T)

#concurrent validity testing
modeleating <- lm(SUM_LIFESAT ~ SUM_EATING, data = data_stresseating)
summary(modeleating)

#exploratory factor analysis 
eigenvalues <- eigen(cor(data_stresseating[ ,3:22]))
plot(eigenvalues$values)
fa.parallel((data_stresseating[ ,3:22]))
efa_results <- fa(data_stresseating[ ,3:22], nfactors = 2, rotate = "oblimin")
print(efa_results$loadings, cutoff = 0.3)

#test bias
modeleating1 <- lm(SUM_LIFESAT ~ SUM_EATING, data = data_stresseating)
summary(modeleating1)
model2 <- lm(SUM_LIFESAT ~ SUM_EATING + Gender + SUM_EATING:Gender, data = data_stresseating)
anova(modeleating1, model2)
