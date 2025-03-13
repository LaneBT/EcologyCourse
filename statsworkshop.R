#stats workshop...example data

#Load Library ####
install.packages("tidyverse")
library(tidyverse)
install.packages("MASS")
library("MASS")

getwd()

#read in data ####

mtcars <- read.csv("mtcars.csv")
survey <- read.csv("survey.csv")
restingbehavior <- read.csv("Resting_Behavior_clean.csv")
iris <- read.csv("iris.csv")


#statistical tests
# Continuous y continuous X data ####
#Linear regression (linear model)

#linear regression- does car weight predict miles per gallon
#lm=linear model, ~ means as a function of (y~x)
mpgmodel <- lm(mpg~wt, data=mtcars)
summary(mpgmodel)


#testing assumptions of linear model
#plot(x,y)
plot(mtcars$wt,mtcars$mpg) #looks linear = good linearity

#normality of residuals
hist(resid(mpgmodel)) #normal enough

#test of normality.. do shapiro test of residuals
shapiro.test(resid(mpgmodel)) #p=0.10 = not significantly different from normal = normal
#if not normal, data transformation can help

#homogeneity of varience
plot(fitted(mpgmodel),resid(mpgmodel)) #fitted values are line of best fit values, residuals are distance from them, should be flat line)
#looks good, not crazy skewed...random
#need to incorporate varience structure to model if doesnt work (ask luke!)

#could also use correlation if needed


#Catagorical x vs Continous Y ####

#removing first column
iris <- iris[,-1] #brackets allow choose places in data from, before comma = row, after=column
#if need to remove multiple use c()
#does sepal width vary by species--AVNOVA
sepalanova <- aov(Sepal.Width~Species,data=iris)
summary(sepalanova)
#know sigificant effect of species of sepal width, dont know what kind of effect!!

#Tukey post-hoc test give pairwise comparison
TukeyHSD(sepalanova)
#very significant! all pairwise comparisons significant

#trying to do t-test
#select everything in this datafram where Iris species = setosa, ! means everything EXCEPT
#remove setisa by selecting rows ither than ones where species = setosa
irisnosetosa <- iris[!iris$Species=="setosa",]

#now we can use this for T test!!! only two variables (species)
versicolorDF <- iris[iris$Species=="versicolor",]
virginicaDF <- iris[iris$Species=="virginica",]

sepalttest <- t.test(versicolorDF$Sepal.Width,virginicaDF$Sepal.Width)
#summary function doesnt work on ttest so use print
print(sepalttest)

#another way...can do either
sepalttest1 <- t.test(Sepal.Width~Species,data=irisnosetosa)
print(sepalttest1)

#Catagorical y versus Continuous X ####
#logistic regression... use when have a binary on y against continuous X

logModel <- glm(am~cyl,family="binomial",data=mtcars) #glm is generalized linear model, have to specify family of distributions
summary(logModel)

#catagorical y versus catagorical X ####
#contingency table, use chi squared

survey$Smoke <- as.factor(survey$Smoke)
survey$Exer <- as.factor(survey$Exer) #factor is a string of characters that have particular identities (levels)
tbl <- table(survey$Smoke,survey$Exer)
show(tbl)

chisq.test(tbl)


#look at painted dog data ####

contactSummary <- restingbehavior%>%
  group_by(Dog_ID)%>%
  summarize(meanPropContact=mean(propContact),
            sdPropContact=sd(propContact),
            semPropContact=sd(propContact)/sqrt(length(propContact)))
#error somehwere here

contactSummary%>%
  ggplot(aes(x=Dog_ID,y=meanPropContact,fill=Dog_ID))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=meanPropContact-semPropContact,ymax=meanPropContact))
#missing something at end here but idk what...check R file
