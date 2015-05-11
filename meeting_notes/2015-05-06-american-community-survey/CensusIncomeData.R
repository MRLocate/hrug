######## Title ########
# Title: ACS.R Example
# Author: Mark Locatelli
# Date: April 20, 2015

######## Run some Libraries ###########
library(tools)
library(acs)

library(maptools)
library(RColorBrewer)
library(scales)
library(dplyr)
library(classInt)
library(rgdal)
library(ggplot2)
library(ggmap)
library(Cairo)
library(zipcode)
data(zipcode)

######## Get the Data ###############

CensusDataPath <- "/Users/mloco/Dropbox/HRUG/hrug/meeting_notes/2015-05-06-american-community-survey"

setwd(CensusDataPath)

CensusData <- read.acs("ACS_13_5YR_S1901_with_ann.csv", endyear='2013', span=5)

CensusDataSummary <- estimate(CensusData[,c(1:4,45:52)])

colnames(CensusDataSummary) <- c('Households', 'Families', 
                                 'Married.Couple.Families', 'Nonfamily.Households',
                                 'Household.Median.Income', 'Family.Median.Income', 
                                 'Married.Couple.Family.Median.Income',  
                                 'Nonfamily.Household.Median.Income', 
                                 'Household.Mean.Income', 'Family.Mean.Income', 
                                 'Married.Couple.Family.Mean.Income', 
                                 'Nonfamily.Household.Mean.Income')

########### Some Descriptive Variables & Tables #########

HouseholdIncome <- estimate(CensusData[,45])

MedianIncome <- estimate(CensusData[,45])

MeanIncome <- estimate(CensusData[,49])

Households <- estimate(CensusData[,1])

Families <- estimate(CensusData[,2])

Kailua <- CensusData[31,]

Kaneohe <- CensusData[38,]

Waimanalo <- CensusData[81,]

Hilo <- CensusData[21,]

acs.colnames(Kailua[,c(5,9,13,17,21,25,29,33,37,41)]) <- c("<$10k","$10k-15k","$15k-25k",
                                                           "$25k-35k","$35k-50k",
                                                           "$50k-75k","$75k-100k",
                                                           "$100k-150k","$150k-200k",
                                                           "$200k+")

acs.colnames(Kaneohe[,c(5,9,13,17,21,25,29,33,37,41)]) <- c("<$10k","$10k-15k","$15k-25k",
                                                           "$25k-35k","$35k-50k",
                                                           "$50k-75k","$75k-100k",
                                                           "$100k-150k","$150k-200k",
                                                           "$200k+")

acs.colnames(Waimanalo[,c(5,9,13,17,21,25,29,33,37,41)]) <- c("<$10k","$10k-15k","$15k-25k",
                                                            "$25k-35k","$35k-50k",
                                                            "$50k-75k","$75k-100k",
                                                            "$100k-150k","$150k-200k",
                                                            "$200k+")

acs.colnames(Hilo[,c(5,9,13,17,21,25,29,33,37,41)]) <- c("<$10k","$10k-15k","$15k-25k",
                                                         "$25k-35k","$35k-50k",
                                                         "$50k-75k","$75k-100k",
                                                         "$100k-150k","$150k-200k",
                                                         "$200k+")

########### Make Some Plots of the Data ######

HouseholdPlot <- plot(CensusData[,1])

HouseholdIncomePlot <- plot(CensusData[,45])

X11(width=9,height=6.5,pointsize=9);plot

png(paste(CensusDataPath,"/PlotIncomeDist.png", sep = ""),
    units="in",width= 9,height= 6,pointsize=9,res=300)

plot(Kailua[,c(5,9,13,17,21,25,29,33,37,41)],col="blue", err.col="lightblue", 
     ylim=c(0,25))
par(new=T)
plot(Kaneohe[,c(5,9,13,17,21,25,29,33,37,41)],col="red", err.col="pink", 
     ylim=c(0,25), xlab="Household Income", ylab="Percent of Households")
par(new=T)
plot(Waimanalo[,c(5,9,13,17,21,25,29,33,37,41)],col="darkgreen", err.col="green", 
     ylim=c(0,25), xlab="Household Income", ylab="Percent of Households")
par(new=T)
plot(Hilo[,c(5,9,13,17,21,25,29,33,37,41)],col="orange", err.col="yellow", 
     ylim=c(0,25), xlab="Household Income", ylab="Percent of Households")
legend("top", legend=c("Kailua($89,016)", "Kaneohe($85,608)","Waimanalo($68,563)",
                       "Hilo($52,822)"),
       text.col=c("blue","red","darkgreen","orange"), pch=1, 
       col=c("blue","red","darkgreen","orange"))

dev.off()

########## Start to build Zip Code Heat Map #########

## set the working directory.
# map.dir <- "I:/DATA/MAPS"
map.dir <- "/Users/mloco/Dropbox/HRUG/hrug/meeting_notes/2015-05-06-american-community-survey/Map Data Folder"

## load the shapefile
HISHP <- readOGR(dsn = map.dir, layer = "zcta10")
HISHP <- fortify(HISHP, region="ZCTA5CE10")

####### Add data here ###################################################

# Extract Zipcode data from Census Data:
HIZIPData <- as.data.frame(CensusDataSummary[7:nrow(CensusDataSummary),])

# Extract Zip codes from row.names:
HIZIP <- substring(row.names(HIZIPData), 7)

# Create field with zip codes in data:
HIZIPData <- cbind(HIZIP, HIZIPData)

# Grab just the fields for Zip Code & Household Median Income
MapData <- HIZIPData[,c("HIZIP","Household Median Income")]
colnames(MapData) <- c("id","Income")
MapData$id <- as.character(MapData$id)
PlotData <- left_join(HISHP, MapData)

# Get City Names to go along with zip codes
HiZipCityData <- left_join(HIZIPData, zipcode, by = c("HIZIP" = "zip"))

#### Build zipcode figure #################################################

p <- ggplot() +
  geom_polygon(data = PlotData, aes(x = long, y = lat, group = group,
                                    fill = Income)) +
  geom_polygon(data = PlotData, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  scale_fill_distiller(palette = "Greens", labels = dollar,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Hawaii Household Median Income\nby Zip Code",
       fill = "")
ggsave(p, file = "IncomeMap.png", width = 6.5, height = 4, type = "cairo-png")

######## Build some Models #######################

# Import some Interesting Data (Credit Card Default Rates)
PDRate <- read.csv("Map Data Folder/PD.zip.csv")
colnames(PDRate) <- c("Zip.Code","PD.Rate")

# Build a Model Data Set:
Model.Data <- left_join(HiZipCityData, PDRate, by = c("HIZIP" = "Zip.Code"))

# Simple Logistic Regression
HouseholdIncome.model <- glm(formula = PD.Rate ~ 1 + Household.Median.Income, 
                             data = Model.Data, family = binomial)

####### Plot of Prob of Delay versus actuals ######




####### Generate The Report #######################

