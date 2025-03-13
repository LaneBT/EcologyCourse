getwd()
microbecompdata <- read.csv("MicrobeComposition.csv",stringsAsFactors = TRUE)
microbdivdata <- read.csv("MicrobeDiversity.csv",stringsAsFactors = TRUE)

microbdivdata$Treatment

hist(microbdivdata$Abundance)
hist(microbdivdata$ShannonDiversity)
hist(microbdivdata$ShannonEvenness)
hist(microbdivdata$InverseSimpsonDiversity)


colors()




microbdivdata$Treatment <- factor(microbdivdata$Treatment,levels = c('Control0','Desk5','Desk15','Floor5','Floor15','Garden5','Garden15'))

boxplot(microbdivdata$Abundance~microbdivdata$Treatment,
        cex.axis=.6,
        main='Bacterial Abundance by Treatment',
        xlab='Treatment',
        ylab='Abundance',
        col=c('navajowhite2','rosybrown','rosybrown','orange3','orange3','olivedrab','olivedrab'))

boxplot(microbdivdata$ShannonDiversity~microbdivdata$Treatment,
        cex.axis=.6,
        main='Bacterial Shannon Diversity by Treatment',
        xlab='Treatment',
        ylab='Shannon Diversity',
        col=c('navajowhite2','rosybrown','rosybrown','orange3','orange3','olivedrab','olivedrab'))

boxplot(microbdivdata$ShannonEvenness~microbdivdata$Treatment,
        cex.axis=.6,
        main='Bacterial Shannon Evenness by Treatment',
        xlab='Treatment',
        ylab='Shannon Evenness',
        col=c('navajowhite2','rosybrown','rosybrown','orange3','orange3','olivedrab','olivedrab'))

boxplot(microbdivdata$InverseSimpsonDiversity~microbdivdata$Treatment,
        cex.axis=.6,
        main='Bacterial Inverse Simpson Diversity by Treatment',
        xlab='Treatment',
        ylab='Inverse Simpson Diversity',
        col=c('navajowhite2','rosybrown','rosybrown','orange3','orange3','olivedrab','olivedrab'))



microbcompdata$Treatment <- factor(microbcompdata$Treatment,levels = c('Control0','Desk5','Desk15','Floor5','Floor15','Garden5','Garden15'))

boxplot(microbdivdata$Richness~microbdivdata$Treatment,
        cex.axis=.6,
        main='Bacterial Richness by Treatment',
        xlab='Treatment',
        ylab='Species Richness',
        col=c('navajowhite2','rosybrown','rosybrown','orange3','orange3','olivedrab','olivedrab'))


microbdatasub <- subset(microbdivdata,Treatment%in%c('Desk5','Desk15'))
t.test(microbdatasub$Abundance~microbdatasub$Treatment)

#other plot...not necessary??
microbecompdata$Treatment <- factor(microbecompdata$Treatment,levels=c('Control10','Desk5','Desk15','Floor5','Floor15','Garden5','Garden15'))
microbcompsub <- microbecompdata[which(rowSums(microbecompdata[,5:18])>0&microbecompdata$Name!='ColemanGreen'),]
microbcompsub2 <- microbcompsub[,5:18]
