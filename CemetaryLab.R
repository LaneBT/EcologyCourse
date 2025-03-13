## End result: four tables, 4 chi squared tests, focus on organization in code
#clinton=rural, wilkes barre = coal miners, Newsberry=small town, NOLA=big city

##What we are doing: make age class, dx= number of individuals that died in that age group
##lx' is number of individuals alive at start of age class, lx standardizes lx' to make it proportional
#3 qx = mortality rate at start of age class

#Read in / subset data ####
getwd()
table <- read.csv("data/cemetery.csv")

#subset by sex
F <- subset.data.frame(table,table$Sex=="F")
M <- subset.data.frame(table,table$Sex=="M")


# OUR CEMETARY DATA: Create dx ####
AgeClass <- c("0-9","10-19","20-29","30-39","40-49", "50-59", "60-69","70-79","80-89","90-99","100-109","110+")

dx_F <- c(sum(F$Age >=0 & F$Age<=9),sum(F$Age >=10 & F$Age<=19),sum(F$Age >=20 & F$Age<=29),sum(F$Age >=30 & F$Age<=39),sum(F$Age >=40 & F$Age<=49),sum(F$Age >=50 & F$Age<=59),sum(F$Age >=60 & F$Age<=69),sum(F$Age >=70 & F$Age<=79),sum(F$Age >=80 & F$Age<=89),sum(F$Age >=90 & F$Age<=99),sum(F$Age >=100 & F$Age<=109),sum(F$Age >=110))

dx_M <- c(sum(M$Age >=0 & M$Age<=9),sum(M$Age >=10 & M$Age<=19),sum(M$Age >=20 & M$Age<=29),sum(M$Age >=30 & M$Age<=39),sum(M$Age >=40 & M$Age<=49),sum(M$Age >=50 & M$Age<=59),sum(M$Age >=60 & M$Age<=69),sum(M$Age >=70 & M$Age<=79),sum(M$Age >=80 & M$Age<=89), sum(M$Age >=90 & M$Age<=99),sum(M$Age >=100 & M$Age<=109),sum(M$Age >=110))

#Make life table dataframe -- FEMALE 
table_F <- as.data.frame.array(cbind(AgeClass,dx_F))
#lx1
table_F$lx1 <- NA

table_F$dx_F <- as.numeric(table_F$dx_F)
sum(table_F$dx_F) #471

table_F$lx1[1] <- 471

#for each age class, subtract the previous lx value - the current dx value. 
table_F$lx1[2] <- as.numeric(table_F$lx1[1]) - as.numeric(table_F$dx_F[1]) 
table_F$lx1[3] <- as.numeric(table_F$lx1[2]) - as.numeric(table_F$dx_F[2])
table_F$lx1[4] <- as.numeric(table_F$lx1[3]) - as.numeric(table_F$dx_F[3])
table_F$lx1[5] <- as.numeric(table_F$lx1[4]) - as.numeric(table_F$dx_F[4])
table_F$lx1[6] <- as.numeric(table_F$lx1[5]) - as.numeric(table_F$dx_F[5])
table_F$lx1[7] <- as.numeric(table_F$lx1[6]) - as.numeric(table_F$dx_F[6])
table_F$lx1[8] <- as.numeric(table_F$lx1[7]) - as.numeric(table_F$dx_F[7])
table_F$lx1[9] <- as.numeric(table_F$lx1[8]) - as.numeric(table_F$dx_F[8])
table_F$lx1[10] <- as.numeric(table_F$lx1[9]) - as.numeric(table_F$dx_F[9])
table_F$lx1[11] <- as.numeric(table_F$lx1[10]) - as.numeric(table_F$dx[10])
table_F$lx1[12] <- as.numeric(table_F$lx1[11]) - as.numeric(table_F$dx[11])

#make lx
table_F$lx <- 1000*(table_F$lx1/471)

#Make qx
table_F$qx <- 1000*(table_F$dx_F/table_F$lx1)



# Repeat for Males 

table_M <- as.data.frame.array(cbind(AgeClass,dx_M))
#lx1
table_M$lx1 <- NA

table_M$dx_M <- as.numeric(table_M$dx_M)
sum(table_M$dx_M) #455

table_M$lx1[1] <- 455

#for each age class, subtract the previous lx value - the current dx value. 
table_M$lx1[2] <- as.numeric(table_M$lx1[1]) - as.numeric(table_M $dx_M[1]) 
table_M$lx1[3] <- as.numeric(table_M$lx1[2]) - as.numeric(table_M $dx_M[2])
table_M$lx1[4] <- as.numeric(table_M$lx1[3]) - as.numeric(table_M$dx_M[3])
table_M$lx1[5] <- as.numeric(table_M$lx1[4]) - as.numeric(table_M$dx_M[4])
table_M$lx1[6] <- as.numeric(table_M$lx1[5]) - as.numeric(table_M$dx_M[5])
table_M$lx1[7] <- as.numeric(table_M$lx1[6]) - as.numeric(table_M$dx_M[6])
table_M$lx1[8] <- as.numeric(table_M$lx1[7]) - as.numeric(table_M$dx_M[7])
table_M$lx1[9] <- as.numeric(table_M$lx1[8]) - as.numeric(table_M$dx_M[8])
table_M$lx1[10] <- as.numeric(table_M$lx1[9]) - as.numeric(table_M$dx_M[9])
table_M$lx1[11] <- as.numeric(table_M$lx1[10]) - as.numeric(table_M$dx[10])
table_M$lx1[12] <- as.numeric(table_M$lx1[11]) - as.numeric(table_M$dx[11])

#make lx
table_M$lx <- 1000*(table_M$lx1/455)

#Make qx
table_M$qx <- 1000*(table_M$dx_M/table_M$lx1)






#PLOTS ####

#survivorship

table_F$AgeMax <- c(9,19,29,39,49,59,69,79,89,99,109,110)
table_M$AgeMax <- c(9,19,29,39,49,59,69,79,89,99,109,110)

plot(log(lx)~AgeMax,data=table_M,
     type='o',
     col='blue',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Survivorship (lx)',
     main='Survivorship: New Orleans')
points(log(lx)~AgeMax,data=table_F,
       type='o',
       col='red')
legend('bottomleft',
       legend=c('Males','Females'),
       lty=1,
       pch=1,
       col=c('blue','red'))
# in the alter years, women are surviving longer...

#mortality curve

plot(qx~AgeMax,data=table_M,
     type='o',
     col='blue',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Mortality (qx)',
     main='Mortality Curve: New Orleans')
points(qx~AgeMax,data=table_F,
       type='o',
       col='red')
legend('topleft',
       legend=c('Males','Females'),
       lty=1,
       pch=1,
       col=c('blue','red'))
# crazy dip is becuase there are very few people that made it to like 119
#in the younger years (20 to 40 ish), women have higher mortality rates CHILDBIRTH, war?
#40-100 males have higher mortality rates, women live a little bit longer? history time specific things:
#men engaged in dangerous work women werent, socioeconomic status/class
#yellow fever outbreak!!

#Stats - Chi Squared ####

#manipulate dataframe a little
#doing chi square on dx vallues

chitable_dx <- as.matrix(cbind(dx_F,dx_M))

test_dx <- chisq.test(chitable_dx)
test_dx
#p=2.54e^-06, significant! difference in number of individuals that die at a given age class in general btw M/F

test_dx$expected
test_dx$observed
#dont want these tables in results!!! but can use to craft results statements

#lab report: no introduction, just hypotheses and predictions, no methods, 
#Yes results (4 chi squares and 4 figures, at least 1 survivorship and 1 mortality) + discussion, 3 references





## Second in class comparision, include in report, NOLA TOTAL ####

#use cemetary data we collected, already called 'table;

dx <- c(sum(table$Age >=0 & table$Age<=9),sum(table$Age >=10 & table$Age<=19),sum(table$Age >=20 & table$Age<=29),sum(table$Age >=30 & table$Age<=39),sum(table$Age >=40 & table$Age<=49),sum(table$Age >=50 & table$Age<=59),sum(table$Age >=60 & table$Age<=69),sum(table$Age >=70 & table$Age<=79),sum(table$Age >=80 & table$Age<=89),sum(table$Age >=90 & table$Age<=99),sum(table$Age >=100 & table$Age<=109),sum(table$Age >=110))
table_total <- as.data.frame.array(cbind(AgeClass,dx))

table_total$lx1 <- NA
table_total$dx <- as.numeric(table_total$dx)
sum(table_total$dx)
table_total$lx1[1] <- 926


table_total$lx1[2] <- as.numeric(table_total$lx1[1]) - as.numeric(table_total$dx[1]) 
table_total$lx1[3] <- as.numeric(table_total$lx1[2]) - as.numeric(table_total$dx[2])
table_total$lx1[4] <- as.numeric(table_total$lx1[3]) - as.numeric(table_total$dx[3])
table_total$lx1[5] <- as.numeric(table_total$lx1[4]) - as.numeric(table_total$dx[4])
table_total$lx1[6] <- as.numeric(table_total$lx1[5]) - as.numeric(table_total$dx[5])
table_total$lx1[7] <- as.numeric(table_total$lx1[6]) - as.numeric(table_total$dx[6])
table_total$lx1[8] <- as.numeric(table_total$lx1[7]) - as.numeric(table_total$dx[7])
table_total$lx1[9] <- as.numeric(table_total$lx1[8]) - as.numeric(table_total$dx[8])
table_total$lx1[10] <- as.numeric(table_total$lx1[9]) - as.numeric(table_total$dx[9])
table_total$lx1[11] <- as.numeric(table_total$lx1[10]) - as.numeric(table_total$dx[10])
table_total$lx1[12] <- as.numeric(table_total$lx1[11]) - as.numeric(table_total$dx[11])

#make lx
table_total$lx <- 1000*(table_total$lx1/926)
#Make qx
table_total$qx <- 1000*(table_total$dx/table_total$lx1)

#read in new data frame - WB, coal mining town ####
wb <- read.csv("data/wb_total.csv")

wb$lx1 <- NA
sum(wb$dx)
wb$lx1[1] <- 165

wb$lx1[2] <- as.numeric(wb$lx1[1]) - as.numeric(wb$dx[1]) 
wb$lx1[3] <- as.numeric(wb$lx1[2]) - as.numeric(wb$dx[2])
wb$lx1[4] <- as.numeric(wb$lx1[3]) - as.numeric(wb$dx[3])
wb$lx1[5] <- as.numeric(wb$lx1[4]) - as.numeric(wb$dx[4])
wb$lx1[6] <- as.numeric(wb$lx1[5]) - as.numeric(wb$dx[5])
wb$lx1[7] <- as.numeric(wb$lx1[6]) - as.numeric(wb$dx[6])
wb$lx1[8] <- as.numeric(wb$lx1[7]) - as.numeric(wb$dx[7])
wb$lx1[9] <- as.numeric(wb$lx1[8]) - as.numeric(wb$dx[8])
wb$lx1[10] <- as.numeric(wb$lx1[9]) - as.numeric(wb$dx[9])
wb$lx1[11] <- as.numeric(wb$lx1[10]) - as.numeric(wb$dx[10])
wb$lx1[12] <- as.numeric(wb$lx1[11]) - as.numeric(wb$dx[11])

wb$lx <- 1000*(wb$lx1/165)
wb$qx <- 1000*(wb$dx/wb$lx1)



#comparison between nola total and wb (coal mining) ####
#survivorship

table_total$AgeMax <- c(9,19,29,39,49,59,69,79,89,99,109,110)
wb$AgeMax <- c(9,19,29,39,49,59,69,79,89,99,109,110)

plot(log(lx)~AgeMax,data=table_total,
     type='o',
     col='green',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Survivorship (lx)',
     main='Survivorship: NOLA vs Wilkes Barre Totals')
points(log(lx)~AgeClass,data=wb,
       type='o',
       col='black')
legend('bottomleft',
       legend=c('New Orleans Total','Wilkes Barre (Coal Mining Town)'),
       lty=1,
       pch=1,
       col=c('green','black'))


#mortality curve

plot(qx~AgeMax,data=table_total,
     type='o',
     col='green',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Mortality (qx)',
     main='Mortality Curve: NOLA vs Wilkes Barre')
points(qx~AgeClass,data=wb,
       type='o',
       col='black')
legend('topleft',
       legend=c('New Orleans Total','Wilkes Barre (Coal Mining Town)'),
       lty=1,
       pch=1,
       col=c('green','black'))

#Chi squared - COMPLETE!!!!
dx_wb <- wb$dx
chitable_NoWb <- as.matrix(cbind(dx,dx_wb))

test_NoWb <- chisq.test(chitable_NoWb)
test_NoWb #correct

#Comparison btw coal mining M and F ####

wb_F <- read.csv('data/wbf.csv')


wb_F$lx1 <- NA
sum(wb_F$dx)
wb_F$lx1[1] <- 88

wb_F$lx1[2] <- as.numeric(wb_F$lx1[1]) - as.numeric(wb_F$dx[1]) 
wb_F$lx1[3] <- as.numeric(wb_F$lx1[2]) - as.numeric(wb_F$dx[2])
wb_F$lx1[4] <- as.numeric(wb_F$lx1[3]) - as.numeric(wb_F$dx[3])
wb_F$lx1[5] <- as.numeric(wb_F$lx1[4]) - as.numeric(wb_F$dx[4])
wb_F$lx1[6] <- as.numeric(wb_F$lx1[5]) - as.numeric(wb_F$dx[5])
wb_F$lx1[7] <- as.numeric(wb_F$lx1[6]) - as.numeric(wb_F$dx[6])
wb_F$lx1[8] <- as.numeric(wb_F$lx1[7]) - as.numeric(wb_F$dx[7])
wb_F$lx1[9] <- as.numeric(wb_F$lx1[8]) - as.numeric(wb_F$dx[8])
wb_F$lx1[10] <- as.numeric(wb_F$lx1[9]) - as.numeric(wb_F$dx[9])
wb_F$lx1[11] <- as.numeric(wb_F$lx1[10]) - as.numeric(wb_F$dx[10])
wb_F$lx1[12] <- as.numeric(wb_F$lx1[11]) - as.numeric(wb_F$dx[11])

wb_F$lx <- 1000*(wb_F$lx1/88)
wb_F$qx <- 1000*(wb_F$dx/wb_F$lx1)



wb_M <- read.csv('data/wbm.csv')

wb_M$lx1 <- NA
sum(wb_M$dx)
wb_M$lx1[1] <- 77

wb_M$lx1[2] <- as.numeric(wb_M$lx1[1]) - as.numeric(wb_M$dx[1]) 
wb_M$lx1[3] <- as.numeric(wb_M$lx1[2]) - as.numeric(wb_M$dx[2])
wb_M$lx1[4] <- as.numeric(wb_M$lx1[3]) - as.numeric(wb_M$dx[3])
wb_M$lx1[5] <- as.numeric(wb_M$lx1[4]) - as.numeric(wb_M$dx[4])
wb_M$lx1[6] <- as.numeric(wb_M$lx1[5]) - as.numeric(wb_M$dx[5])
wb_M$lx1[7] <- as.numeric(wb_M$lx1[6]) - as.numeric(wb_M$dx[6])
wb_M$lx1[8] <- as.numeric(wb_M$lx1[7]) - as.numeric(wb_M$dx[7])
wb_M$lx1[9] <- as.numeric(wb_M$lx1[8]) - as.numeric(wb_M$dx[8])
wb_M$lx1[10] <- as.numeric(wb_M$lx1[9]) - as.numeric(wb_M$dx[9])
wb_M$lx1[11] <- as.numeric(wb_M$lx1[10]) - as.numeric(wb_M$dx[10])
wb_M$lx1[12] <- as.numeric(wb_M$lx1[11]) - as.numeric(wb_M$dx[11])

wb_M$lx <- 1000*(wb_M$lx1/77)
wb_M$qx <- 1000*(wb_M$dx/wb_M$lx1)

#Figures and Stats
#survivorship

plot(log(lx)~AgeClass,data=wb_M,
     type='o',
     col='blue',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Survivorship (lx)',
     main='Survivorship: Wilkes Barre')
points(log(lx)~AgeClass,data=wb_F,
       type='o',
       col='pink')
legend('bottomleft',
       legend=c('Wilkes Barre Men','Wilkes Barre Women'),
       lty=1,
       pch=1,
       col=c('blue','pink'))


#mortality curve

plot(qx~AgeClass,data=wb_M,
     type='o',
     col='blue',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Mortality (qx)',
     main='Mortality Curve: Wilkes Barre')
points(qx~AgeClass,data=wb_F,
       type='o',
       col='pink')
legend('topleft',
       legend=c('Wilkes Barre Men','Wilkes Barre Women'),
       lty=1,
       pch=1,
       col=c('blue','pink'))

#Chi squared - Fixed!!
dx_wbm <- wb_M$dx
dx_wbf <- wb_F$dx
chitable_wb <- as.matrix(cbind(dx_wbm,dx_wbf))
chitable_wb1 <- chitable_wb[1:11,] #removes rows, cant have 0,0 rows for chi squared to work


test_wb1 <- chisq.test(chitable_wb1)
test_wb1

#Nola M (table_M) Vs WB(wb_M) M ####
#survivorship
plot(log(lx)~AgeMax,data=table_M,
     type='o',
     col='blue',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Survivorship (lx)',
     main='Survivorship: NOLA men vs Wilkes Barre men')
points(log(lx)~AgeClass,data=wb_M,
       type='o',
       col='black')
legend('bottomleft',
       legend=c('New Orleans Men','Wilkes Barre Men'),
       lty=1,
       pch=1,
       col=c('blue','black'))


#mortality curve

plot(qx~AgeMax,data=table_M,
     type='o',
     col='blue',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Mortality (qx)',
     main='Mortality Curve: NOLA men vs Wilkes Barre men')
points(qx~AgeClass,data=wb_M,
       type='o',
       col='black')
legend('topleft',
       legend=c('New Orleans Men','Wilkes Barre Men'),
       lty=1,
       pch=1,
       col=c('blue','black'))

#Chi squared - This did not work.

chitable_WBNm <- as.matrix(cbind(dx_wbm,dx_M))

chitable_WBNm1 <- chitable_WBNm[1:11,]

test_WBNm1 <- chisq.test(chitable_WBNm1)
test_WBNm1



#Nola F (table_F) Vs WB F (wb_F) ####

plot(log(lx)~AgeMax,data=table_F,
     type='o',
     col='Red',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Survivorship (lx)',
     main='Survivorship')
points(log(lx)~AgeClass,data=wb_F,
       type='o',
       col='black')
legend('bottomleft',
       legend=c('New Orleans Women','Wilkes Barre Women'),
       lty=1,
       pch=1,
       col=c('red','black'))


#mortality curve

plot(qx~AgeMax,data=table_F,
     type='o',
     col='red',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Mortality (qx)',
     main='Mortality Curve')
points(qx~AgeClass,data=wb_F,
       type='o',
       col='black')
legend('topleft',
       legend=c('New Orleans Women','Wilkes Barre Women'),
       lty=1,
       pch=1,
       col=c('red','black'))

#Chi squared - This did not work.

chitable_WBNf <- as.matrix(cbind(dx_wbf,dx_F))

test_WBNf <- chisq.test(chitable_WBNf)
test_WBNf






#Cmparison clinton total verus NOLA total DONT NEED #### 

CT_total <- read.csv('data/clinton_total.csv')

CT_total$lx1 <- NA
sum(CT_total$dx)
CT_total$lx1[1] <- 1747

CT_total$lx1[2] <- as.numeric(CT_total$lx1[1]) - as.numeric(CT_total$dx[1]) 
CT_total$lx1[3] <- as.numeric(CT_total$lx1[2]) - as.numeric(CT_total$dx[2])
CT_total$lx1[4] <- as.numeric(CT_total$lx1[3]) - as.numeric(CT_total$dx[3])
CT_total$lx1[5] <- as.numeric(CT_total$lx1[4]) - as.numeric(CT_total$dx[4])
CT_total$lx1[6] <- as.numeric(CT_total$lx1[5]) - as.numeric(CT_total$dx[5])
CT_total$lx1[7] <- as.numeric(CT_total$lx1[6]) - as.numeric(CT_total$dx[6])
CT_total$lx1[8] <- as.numeric(CT_total$lx1[7]) - as.numeric(CT_total$dx[7])
CT_total$lx1[9] <- as.numeric(CT_total$lx1[8]) - as.numeric(CT_total$dx[8])
CT_total$lx1[10] <- as.numeric(CT_total$lx1[9]) - as.numeric(CT_total$dx[9])
CT_total$lx1[11] <- as.numeric(CT_total$lx1[10]) - as.numeric(CT_total$dx[10])
CT_total$lx1[12] <- as.numeric(CT_total$lx1[11]) - as.numeric(CT_total$dx[11])

CT_total$lx <- 1000*(CT_total$lx1/1747)
CT_total$qx <- 1000*(CT_total$dx/CT_total$lx1)

#Figures and Stats
#survivorship

plot(log(lx)~AgeMax,data=table_total,
     type='o',
     col='orange',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Survivorship (lx)',
     main='Survivorship')
points(log(lx)~AgeClass,data=CT_total,
       type='o',
       col='purple')
legend('bottomleft',
       legend=c('New Orleans Total','Clinton Total'),
       lty=1,
       pch=1,
       col=c('orange','purple'))


#mortality curve

plot(qx~AgeMax,data=table_total,
     type='o',
     col='orange',
     lty=1,
     pch=1,
     xlab='Age',
     ylab='Mortality (qx)',
     main='Mortality')
points(qx~AgeClass,data=CT_total,
       type='o',
       col='purple')
legend('topleft',
       legend=c('New Orleans Total','Clinton Total'),
       lty=1,
       pch=1,
       col=c('orange','purple'))

#Chi squared - This did not work.
dx_ctt <- CT_total$dx
chitable_NoCt <- as.matrix(cbind(dx_ctt,dx))

test_NoCt <- chisq.test(chitable_NoCt)
test_NoCt
