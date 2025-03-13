#Intro R Lab, class notes----
#four dashes make a table of contents type thing

#creating objects: add object then carrot <- then value
answer <- 5+3
#command enter runs code in console

#Vectors----
a <- c(1,2,3,4)
b <- c(1:4)
#in environment, a is numeric, b is integer, describes what type of info it is
c <- rep(c(1,2,3,4),3)
#make a vector called c that is repeating this chain of nubmers 3 times
c
d <- rep(a,3)
e <- rep(b,each=3)

#sort vector a in decreasing order
sort(a,decreasing=T)
#save it as object f
f <- sort(a,decreasing=T)

#Subsetting----
weights <- c(21,30,39,54,55)
weights>50
#brackets mean take only this part of the data
weights[weights>50]
weights[weights<30|weights>50] # | means or
weights[weights<=30&weights>20] # & means and

weights_low <- weights[weights<40]
weights_high <- weights[weights>=40]

#practice----
heights1 <- c(82,78,96,NA,68,72,93,101,87)
heights2 <- c(94,NA,77,83,98,86,80,79,90)
heights3 <- c(93,106,116,97,NA,124,114,89,120)

#remove function is rm(thing), can do down in console bc dont really need to save

mean(heights1,na.rm=T) #cant do mean if there is an NA so na.rm removes it
#the things in parathesis are arguments, vector pulling from, second removes

max(heights1,na.rm=T)
min(heights1,na.rm=T)

# Remove NA's ----
heights1 <- na.omit(heights1)
heights2 <- na.omit(heights2)
heights3 <- na.omit(heights3)

# Making a Data Frame----

heights <- c(heights1,heights2,heights3)
tree.sp <- rep(c("Loblolly","Slash","Longleaf"),8) # quotes tell r we are using words

# List is the columns you are making, 24 obs (rows), 2 variables (column)
# $ means column
data <- data.frame(Pine=tree.sp,Heights=heights)
View(data) #opens tab to view whatever you made

#check out data
head(data) #top of data
tail(data) #end of data
str(data) #structure, shows same info as environment

# Add Column----
data$DBH <- runif(24,min=1.4,max=3.9)

#Graphing----
boxplot(data=data,Heights~Pine)

#Reordering
data$Pine <- factor(data$Pine,levels=c("Slash","Loblolly","Longleaf"))

#Other Changes
boxplot(data=data,Heights~Pine,
        main="Heights of Common Pines", #title
        ylab="Heights (yards)", #axis labels
        cex.lab=1.25, #font size
        col="pink")
#more color
boxplot(data=data,Heights~Pine,
        main="Heights of Common Pines", #title
        ylab="Heights (yards)", #axis labels
        cex.lab=1.25, #font size
        col=c("pink","orange","purple"))

boxplot(data=data,Heights~Pine,
        main="Heights of Common Pines", #title
        ylab="Heights (yards)", #axis labels
        cex.lab=1.25, #font size
        col=c("light green","green","dark green"))

