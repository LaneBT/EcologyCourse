# In class
#reading data into R
getwd()
data <- read.csv("spider_data.csv")

#dropping NA values
is.na(data)
which(is.na(data))
data <- na.omit(data)

#calculating web area
data$Web.Area <- pi*((data$Web.Diam.1+data$Web.Diam.2)/2/2)^2

data$Web.Area

#fixing any obtuse angles
data$Orientation[data$Orientation>90] <- 180-data$Orientation[data$Orientation>90]
#check it worked
data$Orientation[data$Orientation>90]


#subsetting by species for stats
GC <- subset.data.frame(data,data$Spider.ID=="Gasteracantha cancriformis")
NC <- subset.data.frame(data,data$Spider.ID=="Nephila clavipes")
LV <- subset.data.frame(data,data$Spider.ID=="Leucauge venusta")

# t test, web height
#hypotheses: the two means are different and nephela will be higher
t.test(NC$Web.Height,LV$Web.Height)

t.test(GC$Web.Height,LV$Web.Height)
t.test(NC$Web.Height,GC$Web.Height)

sd(GC$Web.Height)
sd(NC$Web.Height)
sd(LV$Web.Height)


#boxplot
data1 <- rbind(GC,LV,NC)
boxplot(data=data1,data1$Web.Height~data1$Spider.ID,
        xlab="Spider Species",
        ylab="Web Height (cm)",
        main="Web Heights of Orb-Weaving Spiders",
        col=(c("orange","magenta","yellow")))

#chi squared test
observed <- table(data1$Spider.ID,data1$Substrate)
test <- chisq.test(observed)
test

test$observed
test$expected

#Independent Work
#3 more T tests

# t test, body length
t.test(NC$Body.Length,LV$Body.Length)
t.test(GC$Body.Length,LV$Body.Length)
t.test(NC$Body.Length,GC$Body.Length)

sd(GC$Body.Length)
sd(NC$Body.Length)
sd(LV$Body.Length)

boxplot(data=data1,data1$Body.Length~data1$Spider.ID,
        xlab="Spider Species",
        ylab="Body Length (mm)",
        main="Body Lengths of Orb-Weaving Spiders",
        col=(c("orange","magenta","yellow")))

# t test, number of radii
t.test(NC$Radii,LV$Radii)
t.test(GC$Radii,LV$Radii)
t.test(NC$Radii,GC$Radii)

sd(GC$Radii)
sd(NC$Radii)
sd(LV$Radii)

boxplot(data=data1,data1$Radii~data1$Spider.ID,
        xlab="Spider Species",
        ylab="Number of Web Radii",
        main="Web Radii of Orb-Weaving Spiders",
        col=(c("orange","magenta","yellow")))

# t test, largest web diameter
t.test(NC$Web.Diam.1,LV$Web.Diam.1)
t.test(GC$Web.Diam.1,LV$Web.Diam.1)
t.test(NC$Web.Diam.1,GC$Web.Diam.1)

sd(NC$Web.Diam.1)
sd(GC$Web.Diam.1)
sd(LV$Web.Diam.1)

boxplot(data=data1,data1$Web.Diam.1~data1$Spider.ID,
        xlab="Spider Species",
        ylab="Largest Web Diameter (cm)",
        main="Web Diameter of Orb-Weaving Spiders",
        ylim=c(0,150),
        col=(c("orange","magenta","yellow")))

# t test, web area
t.test(NC$Web.Area,LV$Web.Area)
t.test(GC$Web.Area,LV$Web.Area)
t.test(NC$Web.Area,GC$Web.Area)

sd(NC$Web.Area)
sd(GC$Web.Area)
sd(LV$Web.Area)

boxplot(data=data1,data1$Web.Area~data1$Spider.ID,
        xlab="Spider Species",
        ylab="Web Area (cm^2)",
        main="Web Area of Orb-Weaving Spiders",
        ylim=c(0,6000),
        col=(c("orange","magenta","yellow")))


#Strand Density
# t test, web area
t.test(NC$Strand.Density,LV$Strand.Density)
t.test(GC$Strand.Density,LV$Strand.Density)
t.test(NC$Strand.Density,GC$Strand.Density)

sd(NC$Strand.Density)
sd(LV$Strand.Density)
sd(GC$Strand.Density)

boxplot(data=data1,data1$Strand.Density~data1$Spider.ID,
        xlab="Spider Species",
        ylab="Web Straond Density",
        main="Web Strand Density of Orb-Weaving Spiders",
        col=(c("orange","magenta","yellow")))

#Orientation
t.test(NC$Orientation,LV$Orientation)
t.test(GC$Orientation,LV$Orientation)
t.test(NC$Orientation,GC$Orientation)

sd(NC$Orientation)
sd(LV$Orientation)
sd(GC$Orientation)

boxplot(data=data1,data1$Orientation~data1$Spider.ID,
        xlab="Spider Species",
        ylab="Web Orientation",
        main="Web Orientation of Orb-Weaving Spiders",
        col=(c("orange","magenta","yellow")))
#Maybe look for something to remove outliers

#Chi Squared tests

#chi squared test: Stabilimentum
observed2 <- table(data1$Spider.ID,data1$Stabilimentum)
test2 <- chisq.test(observed2)
test2

test2$observed
test2$expected
