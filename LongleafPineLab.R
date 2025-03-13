###### Longleaf Pine lab Script 2023 #########

#Read in Data----
setwd("/Users/laneytellegen/Desktop/R/ecology_2023")
getwd()

data_f <- read.csv("data/data_forest-1.csv")
data_g <- read.csv("data/data_grass-1.csv")

#checking data
head(data_f)
tail(data_f)
str(data_f)
summary(data_f)



# Preston Plot ----
# Bins
Bins<-c(0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8196)

#Make Preston data frame
PrestonData<-data.frame(Min=Bins[-length(Bins)],Max=Bins[-1]) #Create a data frame with mins and maxes from the bins above
#1st column is called Min, and it's our vector "Bins", but without the final category. The second column is called Max and is all categories minus the 1st one

PrestonData$f <- rep(NA)
PrestonData$g <- rep(NA)

PrestonData$Name <- paste(PrestonData$Min,PrestonData$Max,sep="-")


## Fill in PrestonData
count.bin<-function(X,Min,Max){
  Number<-sum(X>Min & X<Max) #Count the number of cases where X is greater than the min and less than the max
  Number<-Number+sum(X==Min)/2 #Add in the number of cases where X is equal to the minimum devided by two
  Number<-Number+sum(X==Max)/2 #do the same thing for X equals the maximum
  return(Number) #return the total number
}

for(r in 1:nrow(PrestonData)){
  #calculate the number of species in the bin for each row (r) of the data frame
  PrestonData$f[r]<-count.bin(X=data_f$Count,Min=PrestonData$Min[r],Max = PrestonData$Max[r]) 
  PrestonData$g[r]<-count.bin(X=data_g$Count,Min=PrestonData$Min[r],Max = PrestonData$Max[r]) #do the same for the grassland
}


## Plot
barplot(height=rbind(PrestonData$f,PrestonData$g), #The heights of the bars come form the PrestonData f and g columns
        names.arg=PrestonData$Name, #The names for the x axis lables come form PrestonData$Names
        col=c('Blue','Red'), #Set the colors for the bars
        las=2, #Rotate the x axis labels vertically
        beside=T, #place the bars beside eachother (default is stacked)
        xlab='Abundance',#add the x axis name
        ylab='Number of species', #add the y axis name
        main="Species Abundance")

legend('topright',legend=c('Bayhead Swamp',"Longleaf Pine"),fill=c("Blue","Red"),cex=0.75)

#Results statements for Preston Plot: Figure describes abundance distribution, giving idea into community structure
#Both the longleaf pine and bayhead swamp had similar amounts of rare species, but the longleaf pine forest had more dominading, abundant species


# Species Accumulation Curve ----
# Create new dataframe
AccumData<-data.frame(
  #Lets make a column called group that goes from group 1 to the number of groups we have
  Group=seq(from=1,to=max(max(data_f$Group,data_g$Group)),by=1), 
  F=rep(NA),G=rep(NA)) #put in placeholders for f and g


## Fill in Data
for(r in 1:nrow(AccumData)){
  #for each row, the value is the number of groups that are equal to or less than the group of intrest
  AccumData$F[r]<-sum(data_f$Group<=AccumData$Group[r])
  AccumData$G[r]<-sum(data_g$Group<=AccumData$Group[r]) #do it again for G
}

##Plot
plot(y=c(0,max(c(AccumData$F,AccumData$G))),
     x=c(1,max(AccumData$Group)),
     type='n',
     ylab='number of species',
     xlab="Group Number",main="Species Accumulation Curve")

points(F~Group,
       data=AccumData,
       type="o",
       pch=16,
       col='blue')
points(G~Group,
       data=AccumData,
       type="o",
       pch=16,
       col='red')

legend('bottomright',legend=c("Bayhead Swamp","Longleaf Pine"),lty=1,col=c('blue','red'),pch=16)

#Figure is species accumulation curve... Demonstrates that sampling was more sufficient for the bayhead swamp
#more sampling needs to be done in the forest, ie we did not find all, or a suffienct (probably) amount of species. 
#More horizontal than verticle is good.

## Comparing Shannon----

#pi
f_pi <- data_f$Count/sum(data_f$Count)
g_pi <- data_g$Count/sum(data_g$Count)

# H' --> Sannon Diveristy of the two habitats, higher in g = more diverse
f_H <- sum(f_pi*log(f_pi))*-1
g_H <- sum(g_pi*log(g_pi))*-1

#f_H=3.02
#g_H=3.13
#The greater the shannon, the greater the species diveristy,,so grassland has more species diverisity
#Note: more sensitive to addition and deletion of rare species

# evenness
f_E <- f_H/log(nrow(data_f))
g_E <- g_H/log(nrow(data_g))

#Shannon evenness... abundance of species over plots, higher the number the more even
#f_E=0.78, g_E=0.73 --> forest is more even

### calculate variance
f_v<-((sum(f_pi*(log(f_pi)^2))-(sum(f_pi*log(f_pi))^2))/sum(data_f$Count))-((nrow(data_f)-1)/(2*(sum(data_f$Count)^2))) #calculate variance for f
g_v<-((sum(g_pi*(log(g_pi)^2))-(sum(g_pi*log(g_pi))^2))/sum(data_g$Count))-((nrow(data_g)-1)/(2*(sum(data_g$Count)^2))) #do the same for g


### calculate t
t<-abs(f_H-g_H)/(f_v+g_v)^(1/2)

### calculate df
df<-((f_v+g_v)^2)/(f_v^2/sum(data_f$Count)+g_v^2/sum(data_g$Count))

### calculate p value
p<-2*pt(t,df,lower.tail=F)

#p is significant--> significant differnece in shannon diversity (diveristy is a mix of evenness and richness)


# Simpson Diveristy ----

f_N <- sum(data_f$Count) #sum of all abundance
g_N <- sum(data_g$Count)

f_n <- data_f$Count #abundance of each species
g_n <- data_g$Count

#calc Ds (simpson diveristy)
f_Ds <- (f_N*(f_N-1))/sum(f_n*(f_n-1))
g_Ds <- (g_N*(g_N-1))/sum(g_n*(g_n-1))

#f_Ds=14.57, g_Ds=15.73 --> higher simpson = more diveirsity, grassland is more diverse

#evenness
f_Ed <- f_Ds/nrow(data_f)
g_Ed <- g_Ds/nrow(data_g)

#f_Ed = 0.297, g_Ed= 0.733, higher = more even, so grasslands more even

#end: two figures, Shannon diversity and evenness, test to see if sig dif btw shannon values, simpson diverisy and evenness
