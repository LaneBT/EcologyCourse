###### Class Activity 2: Population Ecology ######


###### Question 1: Life Tables #####

#Create the following life table in R
lifetable<-data.frame(survivorship=c(1, .454, .321, .226, 0),survivalrate=c(.50, .75, .74, 0, NA),fecundity=c(0, 0, 1, 3, NA))

lifetable


#Using the life table, predict population growth and age structure over time

#Initiate a dataframe with age class as rows, and time as columns
pop<-data.frame(matrix(NA,nrow=5,ncol=26))
names(pop)<-paste("t",0:25,sep="")
rownames(pop)<-paste("age",0:4,sep="")
pop

#Set age distribution at t=0
N0<-c(40,20,18,5,0)
pop[,1]<-N0
pop

#Do a for loop to calculate survival and reproduction over 25 years
for(i in 2:26){
  pop[2,i]<-pop[1,i-1]*lifetable$survivalrate[1]
  pop[3,i]<-pop[2,i-1]*lifetable$survivalrate[2]
  pop[4,i]<-pop[3,i-1]*lifetable$survivalrate[3]
  pop[5,i]<-pop[4,i-1]*lifetable$survivalrate[4]
  pop[1,i]<-pop[3,i]*lifetable$fecundity[3]+pop[4,i]*lifetable$fecundity[4]
}

pop


#Calculate total population size

totpop<-colSums(pop)
totpop


#Calculate lambda

lambda<-totpop[2:26]/totpop[1:25]
lambda


#Plot total population over time

plot(0:25,totpop,type="b",pch=16,ylim=c(0,500),xlab="Year",ylab="Total population size",main="Population size in Navada Greater Sage Grouse (post management)")


#Stable age distribution through time?

#Plot the abundances of the age classes separately
plot(0:25,pop[1,],type="b",col="orange",ylim=c(0,60),xlab="year", ylab="Population size", main="population size by age class (years)")
points(0:25,pop[2,],type="b",col="red")
points(0:25,pop[3,],type="b",col="blue")
points(0:25,pop[4,],type="b",col="darkgreen")
legend("topright",c("0 years","1 year","2 years","3 years"),col=c("orange","red","blue","darkgreen"),lty=1,y.intersp=.5)

#Calculate relative abundance
totpop5<-rbind(totpop,totpop,totpop,totpop,totpop)
relabun<-pop/totpop5
relabun




###### Question 2: Logistic growth #####

#Explicit form of logistic growth equation:  N(t) = K/(1 + (K-N0)/N0*exp(-r*t))

N0<-20
r<-0.5
K<-350

year<-0:50
pop<-K/(1+(K-N0)/N0*exp(-r*year))
pop
plot(year,pop,ylim=c(0,360),type="l")


#Try new values for r
r<-1.5
pop2<-K/(1+(K-N0)/N0*exp(-r*year))
pop2

plot(year,pop,ylim=c(0,360),type="l")
points(year,pop2,col="red",type="l")
legend("right",c("r=0.5","r=1.5"),col=c("black","red"),lty=1,y.intersp=.5)


r<--.05
pop3<-K/(1+(K-N0)/N0*exp(-r*year))

plot(year,pop,ylim=c(0,360),type="l")
points(year,pop2,col="red",type="l")
points(year,pop3,col="blue",type="l")
legend("right",c("r=0.5","r=1.5","r=-0.5"),col=c("black","red","blue"),lty=1,y.intersp=.5)

r<--.01
pop4<-K/(1+(K-N0)/N0*exp(-r*year))

plot(year,pop,ylim=c(0,360),type="l")
points(year,pop2,col="red",type="l")
points(year,pop3,col="blue",type="l")
points(year,pop4,col="green",type="l")
legend("right",c("r=0.5","r=1.5","r=-0.5", "r=-0.1"),col=c("black","red","blue","green"),lty=1,y.intersp=.5)

r<-2
pop5<-K/(1+(K-N0)/N0*exp(-r*year))

plot(year,pop,ylim=c(0,360),type="l")
points(year,pop2,col="red",type="l")
points(year,pop3,col="blue",type="l")
points(year,pop4,col="green",type="l")
points(year,pop5,col="orange",type="l")
legend("right",c("r=2","r=0.5","r=1.5","r=-0.5", "r=-0.1"),col=c("orange","black","red","blue","green"),lty=1,y.intersp=1)

r<-0
pop6<-K/(1+(K-N0)/N0*exp(-r*year))

plot(year,pop,ylim=c(0,360),type="l")
points(year,pop2,col="red",type="l")
points(year,pop3,col="blue",type="l")
points(year,pop4,col="green",type="l")
points(year,pop5,col="orange",type="l")
points(year,pop6,col="purple",type="l")
legend("right",c("r=2","r=0.5","r=1.5","r=-0.5", "r=0.1","r=0"),col=c("orange","black","red","blue","green","purple"),lty=1,y.intersp=1)

r<--1
pop7<-K/(1+(K-N0)/N0*exp(-r*year))

plot(year,pop,ylim=c(0,360),type="l")
points(year,pop2,col="red",type="l")
points(year,pop3,col="blue",type="l")
points(year,pop4,col="green",type="l")
points(year,pop5,col="orange",type="l")
points(year,pop6,col="purple",type="l")
points(year,pop7,col="yellow",type="l")
legend("right",c("r=2","r=1.5","r=0.5","r=0","r=-0.1","r=-0.5","r=-1"),col=c("orange","red","black","purple","green","blue","yellow"),lty=1,y.intersp=1)


#change initial population size to be greater than K (re-write!)
N0<-700
r<-0.5
K<-350

year<-0:50
pop<-K/(1+(K-N0)/N0*exp(-r*year))
pop
plot(year,pop,ylim=c(0,700),type="l",main='Logistic model of population growth: N0>K (N0=700)')
