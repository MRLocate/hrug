######## Title ########
# Title: Sweave Example
# Author: Mark Locatelli
# Date: April 2, 2015

######## Run some Libraries ###########
library(tools)

######## Get the Data ###############

AirlineDataPath <- "C:/Users/mlocatelli/Dropbox/SWEAVE"

setwd(AirlineDataPath)

AirlineData <- read.csv("Airline Ontime Data.csv")
# AirlineData <- read.csv("Airline Ontime Data Jan15.csv")

AirlineDataSummary <- as.data.frame(cbind(AirlineData$DAY_OF_WEEK, AirlineData$CARRIER, 
                            AirlineData$ORIGIN_CITY_NAME, AirlineData$ORIGIN_STATE_NM, 
                            AirlineData$DEST_CITY_NAME, AirlineData$DEST_STATE_NM, 
                            AirlineData$DEP_DELAY, AirlineData$DEP_DEL15, 
                            AirlineData$ARR_DELAY, AirlineData$ARR_DEL15, 
                            AirlineData$DISTANCE))

colnames(AirlineDataSummary) <- c('Day of the Week', 'Carrier', 'Origin City', 
                                  'Origin State', 'Destination City', 
                                  'Destination State', 'Delay (min)', 
                                  'Delay 15 min?', 'Arrival Dealy (min)', 
                                  'Arrival Delay 15 min?', 'Distance (mi)')

########### Make Some Plots of the Data ######

DelayPlot <- hist(AirlineData$DEP_DEL15, breaks = c(0,0.5,1), xlab="Ontime vs. Delay", 
                  main="Ontime (0) vs. Delayed Flights (1)", col=c("lightgreen","red"), 
                  xlim=c(0,1), ylim=c(0, 10000))

########### Some Descriptive Variables & Tables #########

RatioDelay <- mean(AirlineData$DEP_DEL15, na.rm=TRUE)

PercentDelay <- RatioDelay*100

CarrierTable <- table(AirlineData$CARRIER,AirlineData$DEP_DEL15)

DayTable <- table(AirlineData$DAY_OF_WEEK, AirlineData$DEP_DEL15)

DestTable <- table(AirlineData$DEST_CITY_NAME, AirlineData$DEP_DEL15)

OriginTable <- table(AirlineData$ORIGIN_CITY_NAME, AirlineData$DEP_DEL15)

########## Getting Data Ready for Model #########

####### Origin State Name Dummys #################
M <- cbind(AirlineData, rep(0, nrow(AirlineData)))
names(M)[ncol(M)] <- "origin.AK"
M$origin.AK[M$ORIGIN_STATE_NM == "Alaska"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.AZ"
M$origin.AZ[M$ORIGIN_STATE_NM == "Arizona"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.CA"
M$origin.CA[M$ORIGIN_STATE_NM == "California"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.CO"
M$origin.CO[M$ORIGIN_STATE_NM == "Colorado"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.GA"
M$origin.GA[M$ORIGIN_STATE_NM == "Georgia"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.IL"
M$origin.IL[M$ORIGIN_STATE_NM == "Illinois"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.NV"
M$origin.NV[M$ORIGIN_STATE_NM == "Nevada"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.NJ"
M$origin.NJ[M$ORIGIN_STATE_NM == "New Jersey"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.NY"
M$origin.NY[M$ORIGIN_STATE_NM == "New York"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.OR"
M$origin.OR[M$ORIGIN_STATE_NM == "Oregon"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.TX"
M$origin.TX[M$ORIGIN_STATE_NM == "Texas"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.TER"
M$origin.TER[M$ORIGIN_STATE_NM == "U.S. Pacific Trust Territories and Possessions"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.UT"
M$origin.UT[M$ORIGIN_STATE_NM == "Utah"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.VA"
M$origin.VA[M$ORIGIN_STATE_NM == "Virginia"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "origin.WA"
M$origin.WA[M$ORIGIN_STATE_NM == "Washington"] <- 1

####### Destination State Name Dummys #################
M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.AK"
M$dest.AK[M$DEST_STATE_NM == "Alaska"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.AZ"
M$dest.AZ[M$DEST_STATE_NM == "Arizona"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.CA"
M$dest.CA[M$DEST_STATE_NM == "California"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.CO"
M$dest.CO[M$DEST_STATE_NM == "Colorado"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.GA"
M$dest.GA[M$DEST_STATE_NM == "Georgia"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.IL"
M$dest.IL[M$DEST_STATE_NM == "Illinois"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.NV"
M$dest.NV[M$DEST_STATE_NM == "Nevada"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.NJ"
M$dest.NJ[M$DEST_STATE_NM == "New Jersey"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.NY"
M$dest.NY[M$DEST_STATE_NM == "New York"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.OR"
M$dest.OR[M$DEST_STATE_NM == "Oregon"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.TX"
M$dest.TX[M$DEST_STATE_NM == "Texas"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.TER"
M$dest.TER[M$DEST_STATE_NM == "U.S. Pacific Trust Territories and Possessions"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.UT"
M$dest.UT[M$DEST_STATE_NM == "Utah"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.VA"
M$dest.VA[M$DEST_STATE_NM == "Virginia"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "dest.WA"
M$dest.WA[M$DEST_STATE_NM == "Washington"] <- 1

######## Day of week dummys ##################

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "day.2"
M$day.2[M$DAY_OF_WEEK == "2"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "day.3"
M$day.3[M$DAY_OF_WEEK == "3"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "day.4"
M$day.4[M$DAY_OF_WEEK == "4"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "day.5"
M$day.5[M$DAY_OF_WEEK == "5"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "day.6"
M$day.6[M$DAY_OF_WEEK == "6"] <- 1

M <- cbind(M, rep(0, nrow(M)))
names(M)[ncol(M)] <- "day.7"
M$day.7[M$DAY_OF_WEEK == "7"] <- 1

######## Build some Models #######################

day.model <- glm(formula = M$DEP_DEL15 ~ 1 +  M$day.2 + M$day.3 + M$day.4 + M$day.5 + 
                   M$day.6 + M$day.7, family = binomial)

dest.model <- glm(formula = M$DEP_DEL15 ~ 1 +  M$dest.AK + M$dest.AZ + M$dest.CA + 
                    M$dest.CO + M$dest.GA + M$dest.IL + M$dest.NV + M$dest.NJ + 
                    M$dest.NY + M$dest.OR + M$dest.TX + M$dest.TER + M$dest.UT + 
                    M$dest.VA + M$dest.WA, family = binomial)

origin.model <- glm(formula = M$DEP_DEL15 ~ 1 +  M$origin.AK + M$origin.AZ + M$origin.CA 
                    + M$origin.CO + M$origin.GA + M$origin.IL + M$origin.NV + M$origin.NJ 
                    + M$origin.NY + M$origin.OR + M$origin.TX + M$origin.TER 
                    + M$origin.UT + M$origin.VA + M$origin.WA, family = binomial)

sigvar.model <- glm(formula =  DEP_DEL15 ~ 1 +   day.2 +  day.4 +  day.7 +  dest.CA 
                    +  dest.IL +  dest.NJ +  dest.TX +  dest.TER +  origin.AZ
                    +  origin.CA +  origin.CO +  origin.IL +  origin.NJ +  origin.TX 
                    +  origin.TER +  origin.VA, family = binomial, data= M)

####### Plot of Prob of Delay versus actuals ######

ModelData <- as.data.frame(cbind(AirlineDataSummary, M$day.2, M$day.3, M$day.4, M$day.5, 
                                 M$day.6, M$day.7, M$dest.AK, M$dest.AZ, M$dest.CA, 
                                 M$dest.CO, M$dest.GA,  M$dest.IL, M$dest.NV, M$dest.NJ, 
                                 M$dest.NY, M$dest.OR, M$dest.TX, M$dest.TER, M$dest.UT, 
                                 M$dest.VA, M$dest.WA, M$origin.AK, M$origin.AZ, 
                                 M$origin.CA, M$origin.CO, M$origin.GA, M$origin.IL, 
                                 M$origin.NV, M$origin.NJ, M$origin.NY, M$origin.OR, 
                                 M$origin.TX, M$origin.TER,M$origin.UT, M$origin.VA, 
                                 M$origin.WA))

MData <- ModelData[complete.cases(ModelData[,8]),]

y <- predict(sigvar.model, type='response')
MData <- cbind(MData, y)
names(MData)[ncol(MData)] <- "ProbDelay"
MData[order(MData$ProbDelay),]

AggData <- aggregate(MData[,'Delay 15 min?'], by=MData[c('ProbDelay')], FUN=length)
names(AggData)[ncol(AggData)] <- "N"
AggData.means <- aggregate(MData[,'Delay 15 min?'], by=MData[c('ProbDelay')], FUN=mean)
names(AggData.means)[ncol(AggData.means)] <- "Actual Percent Delayed"

range <- c(0,1)
X11();plot.new()
plot(AggData.means, xlim= range, ylim=range, type = "p", col = "red", 
     xlab="Predicted Prob. of Delay", main="Actual vs. Model")
# par(new=T)
# plot(MData$ProbDelay, col = "red", ylim=range, type = "p")


####### Generate The Report #######################

# model.description <- 'AirlineOntimeModel'

Sweave('AirlineOntimeModel.Rnw', encoding = "utf8", quiet = F)
# file.rename('AirlineOntimeModel.tex', paste(model.description, '.tex', sep=''))

texi2pdf('AirlineOntimeModel.tex', quiet=F)

rmarkdown::render("AirlineOntimeModel.Rmd")