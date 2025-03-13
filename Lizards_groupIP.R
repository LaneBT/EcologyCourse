#I wrote this without the csv so it will need some editing, but generally its a good outline I think
#dont forget to go back through and add code to remove nas

#read in dataframe
lizards <- read.csv("data/lizards.csv")
lizards <- lizards[c(1:45),c(1:9)]

#remove outliers in different object
LQ1 <- quantile(lizards$flee_distance,.25,na.rm=TRUE)
LQ3 <- quantile(lizards$flee_distance,.75,na.rm=TRUE)
IQR <- IQR(lizards$flee_distance,na.rm=TRUE)
lizards_rm <- subset(lizards,lizards$flee_distance>(LQ1-1.5*IQR)&lizards$flee_distance<(LQ3+1.5*IQR))

#add packages
library(tidyverse)
library("MASS")
library(ggplot2)
library(tidyr)

#subset for T-tests didnt use t-test, but this is here in case need subsetting...ignore)-----
green <- subset.data.frame(lizards,lizards$species=="green",na.rm=T)
segrei <- subset.data.frame(lizards,lizards$species=="segrei",na.rm=T)
gecko <- subset.data.frame(lizards,lizards$species=="gecko",na.rm=T)

#by substrate (change once the catagories are definately established)
metal <- subset.data.frame(lizards,lizards$subtrate=="metal",na.rm=T)
brick <- subset.data.frame(lizards,lizards$subtrate=="brick",na.rm=T)
tree <- subset.data.frame(lizards,lizards$substrate=="tree",na.rm=T)
rock <- subset.data.frame(lizards,lizards$substrate=="rock",na.rm=T)
wood <- subset.data.frame(lizards,lizards$substrate=="wood",na.rm=T)
plaster <- subset.data.frame(lizards,lizards$substrate=="plaster",na.rm=T)

# Broad anova and posthoc tukey----- USE THIS!!------

#species vs flee distance
lizanova_flee <- aov(flee_distance~species,data=lizards)
summary(lizanova_flee) #Tells if there is a significant effect of species on flee distance
TukeyHSD(lizanova_flee) #gives pairwise comparisons

#same but no outliers --> INSIGIFICANT
lizanova_fleerm <- aov(flee_distance~species,data=lizards_rm)
summary(lizanova_fleerm) 
TukeyHSD(lizanova_fleerm) 

#body length vs flee distance
lizanova_bl <- aov(body_length~species,data=lizards)
summary(lizanova_bl)
TukeyHSD(lizanova_bl) 

#substrate vs flee distance
lizanova_sub <- aov(flee_distance~substrate,data=lizards)
summary(lizanova_sub)
TukeyHSD(lizanova_sub) 

#caught vs flee distance
lizanova_C <- aov(flee_distance~caught,data=lizards)
summary(lizanova_C)
TukeyHSD(lizanova_C) 

ggplot(lizards_rm,aes(x=caught,y=flee_distance))+
  geom_boxplot() #better graph

ggplot(lizards,aes(x=species,y=flee_distance,col=caught))+
  geom_jitter() 

#site vs flee distance
lizanova_s <- aov(flee_distance~site,data=lizards)
summary(lizanova_s)
TukeyHSD(lizanova_s)

#Plots: Extra-----

lizards <- lizards  %>%
  mutate(Species=case_when(
    species=="segrei" ~ "Brown",
    species=="green" ~ "Green",
    species=="gecko" ~ "Gecko"))

boxplot(data.=lizards,lizards$flee_distance~lizards$Species,
        xlab="Lizard Species",
        ylab="Flee Distance (cm)",
        main="Flee Distance between Lizard Species",
        col=(c("brown","tan","dark green")))
#just geckos so can actually see box
boxplot(data.=gecko,gecko$flee_distance~gecko$species,
        xlab="Gecko",
        ylab="Flee Distance (cm)",
        ylim=c(0,25),
        main="Zoomed In: Gecko Flee Distance",
        col=(c("tan","green","brown")))

lizards_flee <- lizards
lizards_flee <- lizards_flee[,-1:-4]
lizards_flee <- lizards_flee[,-2:-5]
lizards_flee <- lizards_flee[-2,]

aggregate(.~Species,data=lizards_flee,mean)


##minus outliers
boxplot(data=lizards_rm,lizards_rm$flee_distance~lizards_rm$species,
        xlab="Lizard Species",
        ylab="Flee Distance (cm)",
        main="Flee Distance between Lizard Species",
        col=(c("tan","green","brown")))


boxplot(data=lizards,lizards$flee_distance~lizards$substrate,
        xlab="Substrate",
        ylab="Flee Distance (cm)",
        main="Flee Distance between Substrate Background",
        col=(c("red","grey","white","light grey","green","brown")))

#plot of body length versus flee distance, colored by species
ggplot(lizards,aes(x=body_length,y=flee_distance,col=species))+
  geom_jitter()

#substrate preference versus species
ggplot(lizards,aes(x=species,fill=caught))+
  geom_bar(position=fill)

ggplot(lizards,aes(x=species,fill=caught))+
  geom_bar(position="dodge")


#Chi square table: species/substrate-----

#chi squared test
lizobserved <- table(lizards$species,lizards$substrate)
lizchi <- chisq.test(lizobserved)
lizchi

lizchi$observed
lizchi$expected


#body size (x) effecting flee distance (y): linear Regression-----
BLmodel <- lm(flee_distance~body_length, data=lizards)
summary(BLmodel)
#Checking linearity
plot(lizards$body_length,lizards$flee_distance)
#normailty of residuals
hist(resid(BLmodel))
#homogeneity of varience...looking for randomness
plot(fitted(BLmodel),resid(BLmodel))


#Caught or not caught


#GeckoPreferences...CAN IGNORE THIS------
ggplot(gecko,aes(x=species, y=flee_distance))+
  geom_boxplot()

#remove outliers
gQ1 <- quantile(gecko$flee_distance,.25)
gQ3 <- quantile(gecko$flee_distance,.75)
IQR <- IQR(gecko$flee_distance)
gecko_rm <- subset(gecko,gecko$flee_distance>(gQ1-1.5*IQR)&gecko$flee_distance<(gQ3+1.5*IQR))

ggplot(gecko_rm,aes(x=species, y=flee_distance))+
  geom_boxplot()

#gecko substrate pref
ggplot(gecko,aes(x=substrate))+
  geom_bar()

#gecko flee distance
ggplot(gecko,aes(x=species,y=flee_distance,col=caught))+
  geom_jitter()

ggplot(gecko,aes(x=flee_distance,fill=caught))+
  geom_dotplot()
