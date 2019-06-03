#Get Working Directory
getwd()

#set working Directory
setwd("F:/")

#Removing all the stored objects
rm(list = ls())

#Installing required packages.
install.packages("xlsx")
install.packages('DMwR')
install.packages("imputeTS")
install.packages("randomForest")
install.packages('usdm')
install.packages("rpart")
install.packages("tree")
install.packages("party")
install.packages("h20")
install.packages("mlr")
install.packages("e1071")
install.packages("MASS")
install.packages("RColorBrewer")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("fBasics")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("scales")
install.packages("magritter")
install.packages("ggpubr")
install.packages("lattice")
install.packages("devtools")
install.packages("vcd")
install.packages("corrgram")
install.packages("caret")
install.packages("randomForest")

#Loading Libraries.
library(corrgram)
library(DMwR)
library(e1071)
library(party)
library(imputeTS)
library(usdm)
library(xlsx)
library(randomForest)
library(caret)
library(RColorBrewer)
library(rpart.plot)
library(randomForest)
library(rpart)
library(tree)
library(MASS)
library(corrplot)
library(magrittr)
library(randomForest)
library(devtools)
library(ggplot2)
library(scales)
library(vcd)
library(lattice)
library(fBasics)
library(data.table)
library(ggpubr)
library(tidyverse)
library(dplyr)
library(plyr)

#Reading Data from CSV file wih Header = True because first row contains column names.
AbsentData <- read.xlsx("Absent.xls",sheetIndex = 1,header = TRUE)

#Viewing First six rows of dataset
head(AbsentData)
#All the Categorical variables are already encoded.So we doesn't need to do that operation at later stages.

#Renaming Variables to get better undersatnding.
names(AbsentData)[names(AbsentData) == 'ID'] <- 'Id'
names(AbsentData)[names(AbsentData) == 'Reason.for.absence'] <- 'AbsentReason'
names(AbsentData)[names(AbsentData) == 'Month.of.absence'] <- 'AbsentMonth'
names(AbsentData)[names(AbsentData) == 'Day.of.the.week'] <- 'WeekDay'
names(AbsentData)[names(AbsentData) == 'Seasons'] <- 'Season'
names(AbsentData)[names(AbsentData) == 'Transportation.expense'] <- 'Expenses'
names(AbsentData)[names(AbsentData) == 'Distance.from.Residence.to.Work'] <- 'ResidenceDistance'
names(AbsentData)[names(AbsentData) == 'Service.time'] <-'ServiceTime'
names(AbsentData)[names(AbsentData) == 'Age'] <- 'Age'
names(AbsentData)[names(AbsentData) == 'Work.load.Average.day.'] <- 'AverageWorkLoad'
names(AbsentData)[names(AbsentData) == 'Hit.target'] <- 'HitTarget'
names(AbsentData)[names(AbsentData) == 'Disciplinary.failure'] <- 'DiscplineFailure'
names(AbsentData)[names(AbsentData) == 'Education'] <- 'Education'
names(AbsentData)[names(AbsentData) == 'Son'] <- 'Son'
names(AbsentData)[names(AbsentData) == 'Social.drinker'] <- 'SocialDrinker'
names(AbsentData)[names(AbsentData) == 'Social.smoker'] <- 'SocialSmoker'
names(AbsentData)[names(AbsentData) == 'Pet'] <- 'Pet'
names(AbsentData)[names(AbsentData) == 'Weight'] <- 'Weight'
names(AbsentData)[names(AbsentData) == 'Height'] <- 'Height'
names(AbsentData)[names(AbsentData) == 'Body.mass.index'] <- 'BodyMassIndex'
names(AbsentData)[names(AbsentData) == 'Absenteeism.time.in.hours'] <- 'AbsentTime'


#Viewing first Six rows of dataset after renaming variables.
head(AbsentData)

#Viewing Summary of a dataset
summary(AbsentData)

#Viewing structure of a sataset
str(AbsentData)

#Viewing Detailed Summary of Variables
basicStats(AbsentData$Id)
basicStats(AbsentData$AbsentReason)
basicStats(AbsentData$AbsentMonth)
basicStats(AbsentData$WeekDay)
basicStats(AbsentData$Season)
basicStats(AbsentData$Expenses)
basicStats(AbsentData$ResidenceDistance)
basicStats(AbsentData$ServiceTime)
basicStats(AbsentData$Age)
basicStats(AbsentData$AverageWorkLoad)
basicStats(AbsentData$HitTarget)
basicStats(AbsentData$DiscplineFailure)
basicStats(AbsentData$Education)
basicStats(AbsentData$Son)
basicStats(AbsentData$SocialDrinker)
basicStats(AbsentData$SocialSmoker)
basicStats(AbsentData$Pet)
basicStats(AbsentData$Weight)
basicStats(AbsentData$Height)
basicStats(AbsentData$BodyMassIndex)
basicStats(AbsentData$AbsentTime)

#From above analysis Categorical variables are 
#Id,AbsentReason,AbsentMonth,WeekDay,Season,DiscplineFailure,Education,SocialDrinker,SocialSmoker,Pet,Son.
#Contineous Variable are
#ResidenceDistance,ServiceTime,Age,AverageWorkLoad,Expenses,HitTarget,Weight,Height,BodyMassIndex,AbsentTime.


#Checking Missing Values in the DataSet
MissingValues = data.frame(sapply(AbsentData, function(x) sum(is.na(x))))
MissingValues
#There are missing values.

#Treating Missing Values
#Converting Rows into columns
MissingValues$columns = row.names(MissingValues)
row.names(MissingValues) = NULL
MissingValues

#Renaming the Variable Name
names(MissingValues)[1] = "MissingPercentage"
MissingValues

#Calculate Percentage
MissingValues$MissingPercentage = (MissingValues$MissingPercentage/nrow(AbsentData))*100
MissingValues

#Arranging in Descending Order
MissingValues = MissingValues[order(MissingValues$MissingPercentage),]
MissingValues

#Using Mean method to impute Contineous Variables
AbsentData$Weight[is.na(AbsentData$Weight)] = mean(AbsentData$Weight,na.rm=T)
AbsentData$Height[is.na(AbsentData$Height)] = mean(AbsentData$Height,na.rm=T)
AbsentData$BodyMassIndex[is.na(AbsentData$BodyMassIndex)] = mean(AbsentData$BodyMassIndex,na.rm=T)
AbsentData$AbsentTime[is.na(AbsentData$AbsentTime)] = mean(AbsentData$AbsentTime,na.rm=T)
AbsentData$HitTarget[is.na(AbsentData$HitTarget)] = mean(AbsentData$HitTarget,na.rm=T)
AbsentData$Expenses[is.na(AbsentData$Expenses)] = mean(AbsentData$Expenses,na.rm=T)
AbsentData$AverageWorkLoad[is.na(AbsentData$AverageWorkLoad)] = mean(AbsentData$AverageWorkLoad,na.rm=T)
AbsentData$Age[is.na(AbsentData$Age)] = mean(AbsentData$Age,na.rm=T)
AbsentData$ServiceTime[is.na(AbsentData$ServiceTime)] = mean(AbsentData$ServiceTime,na.rm=T)
AbsentData$ResidenceDistance[is.na(AbsentData$ResidenceDistance)] = mean(AbsentData$ResidenceDistance,na.rm=T)

#Since percentage of Missing values are very less so I am dropping categorical attributes with missing values 
AbsentData = na.omit(AbsentData)
#Checking Missing Values present or not
sapply(AbsentData, function(x) sum(is.na(x)))
#All missing values are treated

#Viewing Structure of the DataSet
str(AbsentData)

#Column names of the DataSet
colnames(AbsentData)


#Analyzing Data using Visualizations

#Univariate Analysis

#Analyzing Distribution of AbsentTime
ggplot(AbsentData,aes(x=AbsentData$AbsentTime))+geom_histogram(aes(y=..density..),
      bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+xlab("Absenteeism in Hours")+
      ylab("Frequency")+ggtitle("Absenteeism in Hours")+theme(text=element_text(size=10))
#From the Distribution of variable it is Left Skewed.


#Analyzing Distribution of variable BodyMassIndex.
ggplot(AbsentData,aes(x=AbsentData$BodyMassIndex))+geom_histogram(aes(y=..density..),
      bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+xlab("Body Mass Index")+
      ylab("Frequency")+ggtitle("Body Mass Index")+theme(text=element_text(size=10))
#From the Distribution of variable it is Left Skewed.

#Analyzing Distribution of variable Height.
ggplot(AbsentData,aes(x=AbsentData$Height))+geom_histogram(aes(y=..density..),
      bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+xlab("Height")+
  ylab("Frequency")+ggtitle("Height")+theme(text=element_text(size=10))
#From the Distribution of variable it is Left Skewed. 

#Analyzing Distribution of variable Weight.
ggplot(AbsentData,aes(x=AbsentData$Weight))+geom_histogram(aes(y=..density..),
  bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
  xlab("Weight")+ylab("Frequency")+ggtitle("Weight")+theme(text=element_text(size=10))
#From the Distribution of variable it is Right Skewed. 

#Analyzing Distribution of variable HitTarget
ggplot(AbsentData,aes(x=AbsentData$HitTarget))+geom_histogram(aes(y=..density..),
  bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
  xlab("Hit Target")+ylab("Frequency")+ggtitle("Hit Target")+theme(text=element_text(size=10))
#From the Distribution of variable it is Right Skewed. 

#Analyzing Distribution of variable Expenses
ggplot(AbsentData,aes(x=AbsentData$Expenses))+geom_histogram(aes(y=..density..),
  bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
  xlab("Expenses")+ylab("Frequency")+ggtitle("Expenses")+
  theme(text=element_text(size=10))
#From the Distribution of variable it is Left Skewed. 

#Analyzing Distribution of variable AverageWorkLoad
ggplot(AbsentData,aes(x=AbsentData$AverageWorkLoad))+geom_histogram(aes(y=..density..),
    bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
    xlab("Average Work Load")+ylab("Frequency")+ggtitle("Average Work Load")+
    theme(text=element_text(size=10))
#From the Distribution of variable it is Left Skewed. 

#Analyzing Distribution of variable Age
ggplot(AbsentData,aes(x=AbsentData$Age))+geom_histogram(aes(y=..density..),
  bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
  xlab("Age")+ylab("Frequency")+ggtitle("Age")+
  theme(text=element_text(size=10))
#From the Distribution of variable it is Left Skewed. 

#Analyzing Distribution of variable ServiceTime
ggplot(AbsentData,aes(x=AbsentData$ServiceTime))+geom_histogram(aes(y=..density..),
  bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
  xlab("Service Time")+ylab("Frequency")+ggtitle("Service Time")+
  theme(text=element_text(size=10))
#From the Distribution of variable it is Left Skewed. 

#Analyzing Distribution of variable ResidentDistance
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance))+geom_histogram(aes(y=..density..),
  bandwidth=5,fill='cornsilk',colour="red")+geom_density()+theme_bw()+
  xlab("Resident Distance")+ylab("Frequency")+ggtitle("Resident Distance")+
  theme(text=element_text(size=10))
#From the Distribution of variable it is Left Skewed.

#Making Copy to make changes into it for batter visuaization and understanding of categorical data.
AbsentData1 = AbsentData

#Frequency Table for Categorical Variable AbsentMonth
count(AbsentData1,'WeekDay')
#Converting AbsentMonth to categorical/binning
AbsentData1$WeekDay[AbsentData1$WeekDay==2]="Monday"
AbsentData1$WeekDay[AbsentData1$WeekDay==3]="Tuesday"
AbsentData1$WeekDay[AbsentData1$WeekDay==4]="Wednesday"
AbsentData1$WeekDay[AbsentData1$WeekDay==5]="Thursday"
AbsentData1$WeekDay[AbsentData1$WeekDay==6]="Friday"
#Frequency Table for Categorical Variabe WeekDay
count(AbsentData1,'WeekDay')
#Now we are getting clear picture of values.

#Frequency Table for Categorical Variabe that is 'Season'
count(AbsentData1,'Season')
#Converting Season to categorical/binning
AbsentData1$Season[AbsentData1$Season==1]="Summer"
AbsentData1$Season[AbsentData1$Season==2]="Autum"
AbsentData1$Season[AbsentData1$Season==3]="Winter"
AbsentData1$Season[AbsentData1$Season==4]="Spring"
#Frequency Table for Categorical Variabe Season
count(AbsentData1,'Season')

#Frequency Table for Categorical variable DisplineFailure
count(AbsentData1,'DiscplineFailure')
#Converting DiscplineFailure to categorical/binning
AbsentData1$DiscplineFailure[AbsentData1$DiscplineFailure==0]="No"
AbsentData1$DiscplineFailure[AbsentData1$DiscplineFailure==1]="Yes"
#Frequency Table for Categorical Variabe DiscplineFailure
count(AbsentData1,'DiscplineFailure')

#Frequency Table for Categorical Variable Education 
count(AbsentData1,'Education')
#Converting Education to categorical/binning
AbsentData1$Education[AbsentData1$Education==1]="High School"
AbsentData1$Education[AbsentData1$Education==2]="Graduate"
AbsentData1$Education[AbsentData1$Education==3]="Post Graduate"
AbsentData1$Education[AbsentData1$Education==4]="Master& Doctor"

#Frequency Table for Categorical Variable Education
count(AbsentData1,'Education')

#Frequency Table for Categorical Variable SocialDrinker
count(AbsentData1$SocialDrinker)
#Converting Social Drinker to categorical/binning
AbsentData1$SocialDrinker[AbsentData1$SocialDrinker==1]="Yes"
AbsentData1$SocialDrinker[AbsentData1$SocialDrinker==0]="No"
#Frequency Table for Categorical Variabe SocialDrinker
count(AbsentData1$SocialDrinker)

#Frequency Table for Categorical Variable SocialSmoker
count(AbsentData1$SocialSmoker)
#Converting Social Smoker to categorical/binning
AbsentData1$SocialSmoker[AbsentData1$SocialSmoker==1]="Yes"
AbsentData1$SocialSmoker[AbsentData1$SocialSmoker==0]="No"
#Frequency Table for Categorical Variabe SocialDrinker
count(AbsentData1$SocialSmoker)

#Frequency Table for Categorical Variable Son
count(AbsentData1$Son)
#Converting Social Smoker to categorical/binning
AbsentData1$Son[AbsentData1$Son==0]="Zero"
AbsentData1$Son[AbsentData1$Son==1]="One"
AbsentData1$Son[AbsentData1$Son==2]="Two"
AbsentData1$Son[AbsentData1$Son==3]="Three"
AbsentData1$Son[AbsentData1$Son==4]="Four"
#Frequency Table for Categorical Variabe Son
count(AbsentData1$Son)

#Frequency Table for Categorical Variable Pet
count(AbsentData1$Pet)
#Converting Social Smoker to categorical/binning
AbsentData1$Pet[AbsentData1$Pet==0]="Zero"
AbsentData1$Pet[AbsentData1$Pet==1]="One"
AbsentData1$Pet[AbsentData1$Pet==2]="Two"
AbsentData1$Pet[AbsentData1$Pet==5]="Five"
AbsentData1$Pet[AbsentData1$Pet==4]="Four"
AbsentData1$Pet[AbsentData1$Pet==8]="Eight"
#Frequency Table for Categorical Variabe Pet
count(AbsentData1$Pet)

#Frequency Table for Categorical Variable AbsentMonth
count(AbsentData1$AbsentMonth)
#Converting AbsentMonth to categorical/binning
#Value of month with 0 is not giving clear idea so I merged it into month 1.
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==0]=1
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==1]="January"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==2]="Feburary"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==3]="March"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==4]="April"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==5]="May"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==6]="June"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==7]="July"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==8]="August"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==9]="September"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==10]="October"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==11]="November"
AbsentData1$AbsentMonth[AbsentData1$AbsentMonth==12]="December"
#Frequency Table for Categorical Variabe Month
count(AbsentData1$AbsentMonth)

#Frequency Table for Categorical Variable AbsentReason
count(AbsentData1$AbsentReason)
#Converting Absent Reason to categorical/binning
AbsentData1$AbsentReason[AbsentData1$AbsentReason==0]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==1]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==2]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==3]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==4]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==5]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==6]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==7]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==8]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==9]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==10]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==11]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==12]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==13]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==14]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==15]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==16]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==17]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==18]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==19]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==20]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==21]="ICD"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==22]="CID"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==23]="CID"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==24]="CID"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==25]="CID"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==26]="CID"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==27]="CID"
AbsentData1$AbsentReason[AbsentData1$AbsentReason==28]="CID"
#Frequency Table for Categorical Variable AbsentReason
count(AbsentData1$AbsentReason)


#Frequency Table for Categorical Variable Id
count(AbsentData1$Id)

# Code to view Colours in R
grDevices::colors() 

#Plotting Categorical Variable that is 'Id' Using Bar Graph and Pie Chart
#Bar Graph
ggplot(AbsentData1,aes(x=factor(Id),fill=factor(Id)))+geom_bar()+theme_classic()+
  xlab("Id")+ylab("Count")+labs(fill="Id")+geom_text(aes(label=..count..),
                    stat="count",position=position_stack(1.05))+ggtitle('Id')

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(Id)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Id")+
  guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
                            stat="count",position=position_stack(0.5))

#Plotting Categorical Variable that is 'AbsentReason' Using Bar Graph and Pi Chart.
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(AbsentReason),fill=factor(AbsentReason)))+geom_bar()+theme_classic()+
  xlab("Absent Reason")+ylab("Count")+labs(fill="AbsentReason")+ggtitle("Absent Reason")+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))
#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(AbsentReason)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Absent Reason")+
  guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
                                   stat="count",position=position_stack(0.5))

#Plotting Categorical Variable that is 'Month' using Bar Graph and Pi Chart.
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(AbsentMonth),fill=factor(AbsentMonth)))+geom_bar()+theme_classic()+
  xlab("Absent Months")+ylab("Count")+labs(fill="Absent Months")+ggtitle("Absent Month")+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(AbsentMonth)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Absent Month")+
  guides(fill = guide_legend(reverse = FALSE))+geom_text(aes(label=..count..),
                                    stat="count",position=position_stack(0.5))
                 
#Plotting Categorical Variable that is 'WeekDay' Using Bar Graph
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(WeekDay),fill=factor(WeekDay)))+geom_bar()+theme_classic()+
  xlab("Day of the Week")+ylab("Count")+labs(fill="WeekDay")+ggtitle("Week Days")+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(WeekDay)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Week Days")+
  guides(fill = guide_legend(reverse = FALSE))+geom_text(aes(label=..count..),
                                    stat="count",position=position_stack(0.5))

#Plotting Categorical Variable that is 'Season' Using Bar Graph and Pie Chart
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(Season),fill=factor(Season)))+geom_bar()+theme_classic()+
  xlab("Season")+ylab("Count")+labs(fill="Season")+ggtitle("Season")+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(Season)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Season")+
  guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
                                                        stat="count",position=position_stack(0.5))

#Plotting Categorical Variable that is 'DiscplineFailure' Using Bar Graph
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(DiscplineFailure),fill=factor(DiscplineFailure)))+geom_bar()+theme_classic()+
  xlab("Discpline Failure")+ylab("Count")+labs(fill="Discpline Failure")+ggtitle("Discpline Failure")+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(DiscplineFailure)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Discpline Failure")+
  guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
                                                        stat="count",position=position_stack(0.5))

#Plotting Categorical Variable that is 'Education' Using Bar Graph
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(Education),fill=factor(Education)))+geom_bar()+theme_classic()+
  xlab("Education")+ylab("Count")+labs(fill="Education")+ggtitle('Education')+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(Education)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Education")+
  guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
                                                        stat="count",position=position_stack(0.5))

#Plotting Categorical Variable that is 'Social Drinker' Using Bar Graph
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(SocialDrinker),fill=factor(SocialDrinker)))+geom_bar()+theme_classic()+
  xlab("Social Drinker")+ylab("Count")+labs(fill="Social Drinker")+ggtitle('Social Drinker')+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(SocialDrinker)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Social Drinker")+
  guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
                                                        stat="count",position=position_stack(0.5))

#Plotting Categorical Variable that is 'Social Smoker' Using Bar Graph
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(SocialSmoker),fill=factor(SocialSmoker)))+geom_bar()+theme_classic()+
  xlab("Social Smoker")+ylab("Count")+labs(fill="Social Smoker")+ggtitle('Social Smoker')+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(SocialSmoker)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Social Smoker")+
  guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
                                                        stat="count",position=position_stack(0.5))

#Plotting Categorical Variable that is 'Son' Using Bar Graph
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(Son),fill=factor(Son)))+geom_bar()+theme_classic()+
  xlab("Number of Sons")+ylab("Count")+labs(fill="Social Smoker")+ggtitle('Number of Sons')+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(Son)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Number of Sons")+
  guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
                                                        stat="count",position=position_stack(0.5))

#Plotting Categorical Variable that is 'Pet' Using Bar Graph
#Bar Grapgh
ggplot(AbsentData1,aes(x=factor(Pet),fill=factor(Pet)))+geom_bar()+theme_classic()+
  xlab("Number of Pets")+ylab("Count")+labs(fill="Social Smoker")+ggtitle('Number of Pets')+
  geom_text(aes(label=..count..),stat="count",position=position_stack(1.05))

#Pie Chart
ggplot(AbsentData1,aes(x = "", fill = factor(Pet)))+geom_bar(width = 1, size = 1, color = "black")+
  theme_classic()+theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
                        plot.title = element_text(hjust = 0.5, color = "#666666"))+coord_polar(theta = "y")+
  scale_x_discrete("")+labs(x = NULL, y = NULL, fill = NULL,title = "Number of Pets")+
  guides(fill = guide_legend(reverse = TRUE))+geom_text(aes(label=..count..),
                                                        stat="count",position=position_stack(0.5))


#Bi-variate Analysis

#Analyzing Numerical attributes

#checking the relationship between 'ResidenceDistance' and 'ServiceTime' variable
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance,y=AbsentData$ServiceTime))+geom_point(col='red')+
  ggtitle("Residence Distance v/s Service Time ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Residence Distance")+
  ylab("Service Time")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.9)

#checking the relationship between 'ResidenceDistance' and 'Age' variable
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance,y=AbsentData$Age))+geom_point(col='red')+
  ggtitle("Residence Distance v/s Age ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Residence Distance")+
  ylab("Age")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.9)

#checking the relationship between 'ResidenceDistance' and 'AverageWorkLoad' variable
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance,y=AbsentData$AverageWorkLoad))+geom_point(col='red')+
  ggtitle("Residence Distance v/s Average Workload ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Residence Distance")+
  ylab("Average Work Load")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.9)

#checking the relationship between 'ResidenceDistance' and 'Expenses' variable
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance,y=AbsentData$Expenses))+geom_point(col='red')+
  ggtitle("Residence Distance v/s Expenses ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Residence Distance")+
  ylab("Expenses")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.9)

#checking the relationship between 'ResidenceDistance' and 'HitTarget' variable
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance,y=AbsentData$HitTarget))+geom_point(col='red')+
  ggtitle("Residence Distance v/s Hit Target ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Residence Distance")+
  ylab("Hit Target")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.9)

#checking the relationship between 'ResidenceDistance' and 'Weight' variable
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance,y=AbsentData$Weight))+geom_point(col='red')+
  ggtitle("Residence Distance v/s Weight ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Residence Distance")+
  ylab("Weight")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.9)

#checking the relationship between 'ResidenceDistance' and 'Height' variable
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance,y=AbsentData$Height))+geom_point(col='red')+
  ggtitle("Residence Distance v/s Height ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Residence Distance")+
  ylab("Height")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.9)

#checking the relationship between 'ResidenceDistance' and 'Body Mass Index' variable
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance,y=AbsentData$BodyMassIndex))+geom_point(col='red')+
  ggtitle("Residence Distance v/s Body Mass Index ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Residence Distance")+
  ylab("Body Mass Index")+stat_cor(method = "pearson", label.x = 0.5, label.y =0.9)

#checking the relationship between 'ResidenceDistance' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$ResidenceDistance,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Residence Distance v/s Absent Time ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Residence Distance")+
  ylab("Absent Time")+stat_cor(method = "pearson", label.x = 1, label.y =100)

#checking the relationship between 'Service Time' and 'Age' variable
ggplot(AbsentData,aes(x=AbsentData$ServiceTime,y=AbsentData$Age))+geom_point(col='red')+
  ggtitle("Service Time v/s Age ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Service Time")+
  ylab("Age")+stat_cor(method = "pearson", label.x = 1, label.y =100)

#checking the relationship between 'Service Time' and 'Average Work Load' variable
ggplot(AbsentData,aes(x=AbsentData$ServiceTime,y=AbsentData$AverageWorkLoad))+geom_point(col='red')+
  ggtitle("Service Time v/s Average Work Load ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Service Time")+
  ylab("Average Work Load")+stat_cor(method = "pearson", label.x = 1, label.y =100)

#checking the relationship between 'Service Time' and 'Expenses' variable
ggplot(AbsentData,aes(x=AbsentData$ServiceTime,y=AbsentData$Expenses))+geom_point(col='red')+
  ggtitle("Service Time v/s Expenses ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Service Time")+
  ylab("Expenses")+stat_cor(method = "pearson", label.x = 1, label.y =100)

#checking the relationship between 'Service Time' and 'Hit Target' variable
ggplot(AbsentData,aes(x=AbsentData$ServiceTime,y=AbsentData$HitTarget))+geom_point(col='red')+
  ggtitle("Service Time v/s Hit Target ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Service Time")+
  ylab("Hit Target")+stat_cor(method = "pearson", label.x = 1, label.y =100)

#checking the relationship between 'Service Time' and 'Weight' variable
ggplot(AbsentData,aes(x=AbsentData$ServiceTime,y=AbsentData$Weight))+geom_point(col='red')+
  ggtitle("Service Time v/s Weight ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Service Time")+
  ylab("Weight")+stat_cor(method = "pearson", label.x = 1, label.y =100)

#checking the relationship between 'Service Time' and 'Height' variable
ggplot(AbsentData,aes(x=AbsentData$ServiceTime,y=AbsentData$Height))+geom_point(col='red')+
  ggtitle("Service Time v/s Height ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Service Time")+
  ylab("Height")+stat_cor(method = "pearson", label.x = 1, label.y =150)

#checking the relationship between 'Service Time' and 'Body Mass Index' variable
ggplot(AbsentData,aes(x=AbsentData$ServiceTime,y=AbsentData$BodyMassIndex))+geom_point(col='red')+
  ggtitle("Service Time v/s Body Mass Index ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Service Time")+
  ylab("Body Mass Index")+stat_cor(method = "pearson", label.x = 1, label.y =40)

#checking the relationship between 'Service Time' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$ServiceTime,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Service Time v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Service Time")+
  ylab("Absent Time")+stat_cor(method = "pearson", label.x = 1, label.y =100)

#checking the relationship between 'Age' and 'Average Work Load' variable
ggplot(AbsentData,aes(x=AbsentData$Age,y=AbsentData$AverageWorkLoad))+geom_point(col='red')+
  ggtitle("Age v/s Average Work Load ")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Age")+
  ylab("Average Work Load")+stat_cor(method = "pearson")

#checking the relationship between 'Age' and 'Expenses' variable
ggplot(AbsentData,aes(x=AbsentData$Age,y=AbsentData$Expenses))+geom_point(col='red')+
  ggtitle("Age v/s Expenses")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Age")+
  ylab("Expenses")+stat_cor(method = "pearson")

#checking the relationship between 'Age' and 'Hit Target' variable
ggplot(AbsentData,aes(x=AbsentData$Age,y=AbsentData$HitTarget))+geom_point(col='red')+
  ggtitle("Age v/s Hit Target")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Age")+
  ylab("Hit Target")+stat_cor(method = "pearson",label.x=30,label.y=75)

#checking the relationship between 'Age' and 'Weight' variable
ggplot(AbsentData,aes(x=AbsentData$Age,y=AbsentData$Weight))+geom_point(col='red')+
  ggtitle("Age v/s Weight")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Age")+
  ylab("Weight")+stat_cor(method = "pearson",label.x=20,label.y=25)

#checking the relationship between 'Age' and 'Height' variable
ggplot(AbsentData,aes(x=AbsentData$Age,y=AbsentData$Height))+geom_point(col='red')+
  ggtitle("Age v/s Height")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Age")+
  ylab("Height")+stat_cor(method = "pearson",label.x=20,label.y=125)

#checking the relationship between 'Age' and 'Body Mass Index' variable
ggplot(AbsentData,aes(x=AbsentData$Age,y=AbsentData$BodyMassIndex))+geom_point(col='red')+
  ggtitle("Age v/s Body Mass Index")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Age")+
  ylab("Body Mass Index")+stat_cor(method = "pearson",label.x=20,label.y=40)

#checking the relationship between 'Age' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$Age,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Age v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Age")+
  ylab("Absent Time")+stat_cor(method = "pearson",label.x=20,label.y=40)

#checking the relationship between 'Age' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$Age,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Age v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Age")+
  ylab("Absent Time")+stat_cor(method = "pearson",label.x=20,label.y=40)

#checking the relationship between 'Age' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$Age,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Age v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Age")+
  ylab("Absent Time")+stat_cor(method = "pearson",label.x=20,label.y=40)

#checking the relationship between 'Average Work Load' and 'Expenses' variable
ggplot(AbsentData,aes(x=AbsentData$AverageWorkLoad,y=AbsentData$Expenses))+geom_point(col='red')+
  ggtitle("Average Work Load v/s Expenses")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Average Work Load")+
  ylab("Expenses")+stat_cor(method = "pearson",label.x=20,label.y=40)

#checking the relationship between 'Average Work Load' and 'Hit Target' variable
ggplot(AbsentData,aes(x=AbsentData$AverageWorkLoad,y=AbsentData$HitTarget))+geom_point(col='red')+
  ggtitle("Average Work Load v/s Hit Target")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Average Work Load")+
  ylab("Hit Target")+stat_cor(method = "pearson",label.x=20,label.y=80)

#checking the relationship between 'Average Work Load' and 'Weight' variable
ggplot(AbsentData,aes(x=AbsentData$AverageWorkLoad,y=AbsentData$Weight))+geom_point(col='red')+
  ggtitle("Average Work Load v/s Weight")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Average Work Load")+
  ylab("Weight")+stat_cor(method = "pearson",label.x=20,label.y=40)

#checking the relationship between 'Average Work Load' and 'Height' variable
ggplot(AbsentData,aes(x=AbsentData$AverageWorkLoad,y=AbsentData$Height))+geom_point(col='red')+
  ggtitle("Average Work Load v/s Height")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Average Work Load")+
  ylab("Height")+stat_cor(method = "pearson",label.x=20,label.y=150)

#checking the relationship between 'Average Work Load' and 'Body Mass Index' variable
ggplot(AbsentData,aes(x=AbsentData$AverageWorkLoad,y=AbsentData$BodyMassIndex))+geom_point(col='red')+
  ggtitle("Average Work Load v/s Body Mass Index")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Average Work Load")+
  ylab("Bosy Mass Index")+stat_cor(method = "pearson",label.x=20,label.y=40)

#checking the relationship between 'Average Work Load' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$AverageWorkLoad,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Average Work Load v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Average Work Load")+
  ylab("Absent Time")+stat_cor(method = "pearson")

#checking the relationship between 'Expenses' and 'Hit Target' variable
ggplot(AbsentData,aes(x=AbsentData$Expenses,y=AbsentData$HitTarget))+geom_point(col='red')+
  ggtitle("Expenses v/s Hit Target")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Expenses")+
  ylab("Hit Target")+stat_cor(method = "pearson")

#checking the relationship between 'Expenses' and 'Weight' variable
ggplot(AbsentData,aes(x=AbsentData$Expenses,y=AbsentData$Weight))+geom_point(col='red')+
  ggtitle("Expenses v/s Weight")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Expenses")+
  ylab("Weight")+stat_cor(method = "pearson")

#checking the relationship between 'Expenses' and 'Height' variable
ggplot(AbsentData,aes(x=AbsentData$Expenses,y=AbsentData$Height))+geom_point(col='red')+
  ggtitle("Expenses v/s Height")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Expenses")+
  ylab("Height")+stat_cor(method = "pearson")

#checking the relationship between 'Expenses' and 'Body Mass Index' variable
ggplot(AbsentData,aes(x=AbsentData$Expenses,y=AbsentData$BodyMassIndex))+geom_point(col='red')+
  ggtitle("Expenses v/s Body Mass Index")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Expenses")+
  ylab("Body Mass Index")+stat_cor(method = "pearson")

#checking the relationship between 'Expenses' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$Expenses,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Expenses v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Expenses")+
  ylab("Absent Time")+stat_cor(method = "pearson")

#checking the relationship between 'Hit Target' and 'Weight' variable
ggplot(AbsentData,aes(x=AbsentData$HitTarget,y=AbsentData$Weight))+geom_point(col='red')+
  ggtitle("Hit Target v/s Weight")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Hit Target")+
  ylab("Weight")+stat_cor(method = "pearson")

#checking the relationship between 'Hit Target' and 'Height' variable
ggplot(AbsentData,aes(x=AbsentData$HitTarget,y=AbsentData$Height))+geom_point(col='red')+
  ggtitle("Hit Target v/s Height")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Hit Target")+
  ylab("Height")+stat_cor(method = "pearson")

#checking the relationship between 'Hit Target' and 'Body Mass Index' variable
ggplot(AbsentData,aes(x=AbsentData$HitTarget,y=AbsentData$BodyMassIndex))+geom_point(col='red')+
  ggtitle("Hit Target v/s Body Mass Index")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Hit Target")+
  ylab("Body Mass Index")+stat_cor(method = "pearson")

#checking the relationship between 'Hit Target' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$HitTarget,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Hit Target v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Hit Target")+
  ylab("Absent Time")+stat_cor(method = "pearson")

#checking the relationship between 'Weight' and 'Height' variable
ggplot(AbsentData,aes(x=AbsentData$Weight,y=AbsentData$Height))+geom_point(col='red')+
  ggtitle("Weight v/s Height")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Weight")+
  ylab("Height")+stat_cor(method = "pearson")

#checking the relationship between 'Weight' and 'Body Mass Index' variable
ggplot(AbsentData,aes(x=AbsentData$Weight,y=AbsentData$BodyMassIndex))+geom_point(col='red')+
  ggtitle("Weight v/s Body Mass Index")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Weight")+
  ylab("Body Mass Index")+stat_cor(method = "pearson")

#checking the relationship between 'Weight' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$Weight,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Weight v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Weight")+
  ylab("Absent Time")+stat_cor(method = "pearson")

#checking the relationship between 'Height' and 'Body Mass Index' variable
ggplot(AbsentData,aes(x=AbsentData$Height,y=AbsentData$BodyMassIndex))+geom_point(col='red')+
  ggtitle("Height v/s Body Mass Index")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Height")+
  ylab("Body Mass Index")+stat_cor(method = "pearson")

#checking the relationship between 'Height' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$Height,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Height v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Height")+
  ylab("Absent Time")+stat_cor(method = "pearson")

#checking the relationship between 'Body Mass Index' and 'Absent Time' variable
ggplot(AbsentData,aes(x=AbsentData$BodyMassIndex,y=AbsentData$AbsentTime))+geom_point(col='red')+
  ggtitle("Body Mass Index v/s Absent Time")+
  geom_smooth(method=lm, se=FALSE,col='black') + xlab("Body Mass Index")+
  ylab("Absent Time")+stat_cor(method = "pearson")


#Checking Relationship among all Numerical attributes using Pair Plot Matrix
panel.cor <- function(x, y, digits = 2, cex.cor, ...) # FUNCTION TO CREATE PAIR PLOT MATRIX
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
PanelData=AbsentData[c(6,7,8,9,10,11,18,19,20,21)]
colors <- c('black', 'red')
pairs(PanelData, upper.panel = panel.cor,col=colors)

#Biaviate Analysis for Categorical Variables

#Checking relationship between Id and Absent Reason
Table1 <- table(AbsentData1$AbsentReason,AbsentData1$Id)
#Viewing Table
Table1
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table1)
#Analyzing Categorical 'Table1' using BarPlot Method
c1<-c("blue","red")
barplot(as.matrix(Table1),main="Id V/S Absent Reason",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE, col=c1)
legend("topright", legend = c("ICD","CID"), fill = c1,ncol=1,cex=0.75)

#Checking relationship between Id and Absent Month
Table2 <- table(AbsentData1$AbsentMonth,AbsentData1$Id)
#Viewing Two Table
Table2
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table2)
#Analyzing Categorical 'Table1' using BarPlot Method
c2<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
barplot(as.matrix(Table2),main="Id V/S Absent Month",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c2)

#Checking relationship between Id and Week Day
Table3 <- table(AbsentData1$Id,AbsentData1$WeekDay)
#Viewing Two Table
Table3
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table3)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
barplot(as.matrix(Table3),main="Id V/S Week Day",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between Id and Season
Table4 <- table(AbsentData1$Id,AbsentData1$Season)
#Viewing Two Table
Table4
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table4)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
barplot(as.matrix(Table4),main="Id V/S Season",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between Id and Discpline Failure
Table5 <- table(AbsentData1$Id,AbsentData1$DiscplineFailure)
#Viewing Two Table
Table5
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table5)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
barplot(as.matrix(Table5),main="Id V/S Discpline Failure",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between Id and Education
Table6 <- table(AbsentData1$Id,AbsentData1$Education)
#Viewing Two Table
Table6
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table6)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
barplot(as.matrix(Table6),main="Id V/S Education",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between Id and Social Driker
Table7 <- table(AbsentData1$Id,AbsentData1$SocialDrinker)
#Viewing Two Table
Table7
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table7)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
barplot(as.matrix(Table7),main="Id V/S Social Drinker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between Id and Social Smoker
Table8 <- table(AbsentData1$Id,AbsentData1$SocialSmoker)
#Viewing Two Table
Table8
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table8)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
barplot(as.matrix(Table8),main="Id V/S Social Smoker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between Id and Son
Table9 <- table(AbsentData1$Id,AbsentData1$Son)
#Viewing Two Table
Table9
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table9)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
barplot(as.matrix(Table9),main="Id V/S Number of Sons",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between Id and Pet
Table10 <- table(AbsentData1$Id,AbsentData1$Pet)
#Viewing Two Table
Table10
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table10)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red","green","black","orange","darkgreen","yellow","darkblue","maroon","grey","tomato","brown")
barplot(as.matrix(Table10),main="Id V/S Number of Pets",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between AbsentReason and AbsentMonth
Table11 <- table(AbsentData1$AbsentReason,AbsentData1$AbsentMonth)
#Viewing Two Table
Table11
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table11)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red")
barplot(as.matrix(Table11),main="Absent Reason V/S Absent Month",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)


#Checking relationship between AbsentReason and Week Day
Table13 <- table(AbsentData1$AbsentReason,AbsentData1$WeekDay)
#Viewing Two Table
Table13
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table13)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red")
barplot(as.matrix(Table13),main="Absent Reason V/S Week Day",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between AbsentReason and Season
Table14 <- table(AbsentData1$AbsentReason,AbsentData1$Season)
#Viewing Two Table
Table14
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table14)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red")
barplot(as.matrix(Table14),main="Absent Reason V/S Season",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between AbsentReason and Discpline Failure
Table15 <- table(AbsentData1$AbsentReason,AbsentData1$DiscplineFailure)
#Viewing Two Table
Table15
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table15)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red")
barplot(as.matrix(Table15),main="Absent Reason V/S Discpline Failure",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between AbsentReason and Education
Table16 <- table(AbsentData1$AbsentReason,AbsentData1$Education)
#Viewing Two Table
Table16
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table16)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red")
barplot(as.matrix(Table16),main="Absent Reason V/S Education",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between AbsentReason and Social Drinker
Table17 <- table(AbsentData1$AbsentReason,AbsentData1$SocialDrinker)
#Viewing Two Table
Table17
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table17)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red")
barplot(as.matrix(Table17),main="Absent Reason V/S Social Drinker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between AbsentReason and Social Smoker
Table18 <- table(AbsentData1$AbsentReason,AbsentData1$SocialSmoker)
#Viewing Two Table
Table18
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table18)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red")
barplot(as.matrix(Table18),main="Absent Reason V/S Social Smoker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between AbsentReason and Son
Table19 <- table(AbsentData1$AbsentReason,AbsentData1$Son)
#Viewing Two Table
Table19
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table19)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red")
barplot(as.matrix(Table19),main="Absent Reason V/S Son",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between AbsentReason and Pet
Table20 <- table(AbsentData1$AbsentReason,AbsentData1$Pet)
#Viewing Two Table
Table20
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table20)
#Analyzing Categorical 'Table1' using BarPlot Method
c3<-c("blue","red")
barplot(as.matrix(Table20),main="Absent Reason V/S Pet",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c3)

#Checking relationship between AbsentMonth and WeekDay
Table21 <- table(AbsentData1$AbsentMonth,AbsentData1$WeekDay)
#Viewing Two Table
Table21
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table21)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","green","orange","white","brown","grey","purple","maroon","pink")
barplot(as.matrix(Table21),main="Absent Reason V/S WeekDay",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between AbsentMonth and Season
Table22 <- table(AbsentData1$AbsentMonth,AbsentData1$Season)
#Viewing Two Table
Table22
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table22)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","green","orange","white","brown","grey","purple","maroon","pink")
barplot(as.matrix(Table22),main="Absent Reason V/S Season",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between AbsentMonth and Discpline Failure
Table23 <- table(AbsentData1$AbsentMonth,AbsentData1$DiscplineFailure)
#Viewing Two Table
Table23
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table23)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","green","orange","white","brown","grey","purple","maroon","pink")
barplot(as.matrix(Table23),main="Absent Reason V/S Discpline Failure",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between AbsentMonth and Education
Table24 <- table(AbsentData1$AbsentMonth,AbsentData1$Education)
#Viewing Two Table
Table24
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table24)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","green","orange","white","brown","grey","purple","maroon","pink")
barplot(as.matrix(Table24),main="Absent Reason V/S Education",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between AbsentMonth and Social Drinker
Table25 <- table(AbsentData1$AbsentMonth,AbsentData1$SocialDrinker)
#Viewing Two Table
Table25
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table25)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","green","orange","white","brown","grey","purple","maroon","pink")
barplot(as.matrix(Table25),main="Absent Reason V/S Social Drinker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)


#Checking relationship between AbsentMonth and Social Smoker
Table26 <- table(AbsentData1$AbsentMonth,AbsentData1$SocialSmoker)
#Viewing Two Table
Table26
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table26)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","green","orange","white","brown","grey","purple","maroon","pink")
barplot(as.matrix(Table26),main="Absent Reason V/S Social Smoker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between AbsentMonth and Son
Table27 <- table(AbsentData1$AbsentMonth,AbsentData1$Son)
#Viewing Two Table
Table27
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table27)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","green","orange","white","brown","grey","purple","maroon","pink")
barplot(as.matrix(Table27),main="Absent Reason V/S Son",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between AbsentMonth and Pet
Table28 <- table(AbsentData1$AbsentMonth,AbsentData1$Pet)
#Viewing Two Table
Table28
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table28)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","green","orange","white","brown","grey","purple","maroon","pink")
barplot(as.matrix(Table28),main="Absent Reason V/S Pet",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Week Day and Season
Table29 <- table(AbsentData1$WeekDay,AbsentData1$Season)
#Viewing Two Table
Table29
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table29)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","orange")
barplot(as.matrix(Table29),main="Week Day V/S Season",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Week Day and Discpline Failure
Table30 <- table(AbsentData1$WeekDay,AbsentData1$DiscplineFailure)
#Viewing Two Table
Table30
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table30)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","orange")
barplot(as.matrix(Table30),main="Week Day V/S Discpline Failure",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Week Day and Education
Table31 <- table(AbsentData1$WeekDay,AbsentData1$Education)
#Viewing Two Table
Table31
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table31)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","orange")
barplot(as.matrix(Table31),main="Week Day V/S Education",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Week Day and Social Drinker
Table32 <- table(AbsentData1$WeekDay,AbsentData1$SocialDrinker)
#Viewing Two Table
Table32
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table32)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","orange")
barplot(as.matrix(Table32),main="Week Day V/S Social Drinker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Week Day and Social Smoker
Table33 <- table(AbsentData1$WeekDay,AbsentData1$SocialSmoker)
#Viewing Two Table
Table33
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table33)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","orange")
barplot(as.matrix(Table33),main="Week Day V/S Social Smoker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)


#Checking relationship between Week Day and Son
Table34 <- table(AbsentData1$WeekDay,AbsentData1$Son)
#Viewing Two Table
Table34
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table34)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","orange")
barplot(as.matrix(Table34),main="Week Day V/S Son",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Week Day and Pet
Table35 <- table(AbsentData1$WeekDay,AbsentData1$Pet)
#Viewing Two Table
Table35
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table35)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","yellow","orange")
barplot(as.matrix(Table35),main="Week Day V/S Pet",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Season and Discpline Failure
Table36 <- table(AbsentData1$Season,AbsentData1$DiscplineFailure)
#Viewing Two Table
Table36
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table36)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","orange")
barplot(as.matrix(Table36),main="Season V/S Discpline Failure",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Season and Education
Table37 <- table(AbsentData1$Season,AbsentData1$Education)
#Viewing Two Table
Table37
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table37)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","orange")
barplot(as.matrix(Table37),main="Season V/S Education",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)


#Checking relationship between Season and Social Drinker
Table38 <- table(AbsentData1$Season,AbsentData1$SocialDrinker)
#Viewing Two Table
Table38
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table38)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","orange")
barplot(as.matrix(Table38),main="Season V/S Social Drinker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Season and Social Smoker
Table39 <- table(AbsentData1$Season,AbsentData1$SocialSmoker)
#Viewing Two Table
Table39
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table39)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","orange")
barplot(as.matrix(Table39),main="Season V/S Social Smoker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)


#Checking relationship between Season and Son
Table40 <- table(AbsentData1$Season,AbsentData1$Son)
#Viewing Two Table
Table40
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table40)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","orange")
barplot(as.matrix(Table40),main="Season V/S Son",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Season and Pet
Table41 <- table(AbsentData1$Season,AbsentData1$Pet)
#Viewing Two Table
Table41
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table41)
#Analyzing Categorical 'Table1' using BarPlot Method
c4<-c("blue","red","black","orange")
barplot(as.matrix(Table41),main="Season V/S Pet",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c4)

#Checking relationship between Discpline Failure and Education
Table42 <- table(AbsentData1$DiscplineFailure,AbsentData1$Education)
#Viewing Two Table
Table42
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table42)
#Analyzing Categorical 'Table1' using BarPlot Method
c5<-c("black","orange")
barplot(as.matrix(Table42),main="Discpline Failure V/S Education",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c5)

#Checking relationship between Discpline Failure and Social Drinker
Table43 <- table(AbsentData1$DiscplineFailure,AbsentData1$SocialDrinker)
#Viewing Two Table
Table43
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table43)
#Analyzing Categorical 'Table1' using BarPlot Method
c5<-c("black","orange")
barplot(as.matrix(Table43),main="Discpline Failure V/S Social Drinker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c5)

#Checking relationship between Discpline Failure and Social Smoker
Table44 <- table(AbsentData1$DiscplineFailure,AbsentData1$SocialSmoker)
#Viewing Two Table
Table44
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table44)
#Analyzing Categorical 'Table1' using BarPlot Method
c5<-c("black","orange")
barplot(as.matrix(Table44),main="Discpline Failure V/S Social Smoker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c5)

#Checking relationship between Discpline Failure and Son
Table45 <- table(AbsentData1$DiscplineFailure,AbsentData1$Son)
#Viewing Two Table
Table45
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table45)
#Analyzing Categorical 'Table1' using BarPlot Method
c5<-c("black","orange")
barplot(as.matrix(Table45),main="Discpline Failure V/S Son",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c5)

#Checking relationship between Discpline Failure and Pet
Table46 <- table(AbsentData1$DiscplineFailure,AbsentData1$Pet)
#Viewing Two Table
Table46
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table46)
#Analyzing Categorical 'Table1' using BarPlot Method
c5<-c("black","orange")
barplot(as.matrix(Table46),main="Discpline Failure V/S Pet",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c5)

#Checking relationship between Education and Social Drinker
Table47 <- table(AbsentData1$Education,AbsentData1$SocialDrinker)
#Viewing Two Table
Table47
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table47)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table47),main="Education V/S Social Drinker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Education and Social Smoker
Table48 <- table(AbsentData1$Education,AbsentData1$SocialSmoker)
#Viewing Two Table
Table48
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table48)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table48),main="Education V/S Social Smoker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Education and Son
Table48 <- table(AbsentData1$Education,AbsentData1$Son)
#Viewing Two Table
Table48
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table48)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table48),main="Education V/S Son",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Education and Pet
Table49 <- table(AbsentData1$Education,AbsentData1$Pet)
#Viewing Two Table
Table49
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table49)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table49),main="Education V/S Pet",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Social Drinker and Social Smoker
Table50 <- table(AbsentData1$SocialDrinker,AbsentData1$SocialSmoker)
#Viewing Two Table
Table50
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table50)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table50),main="Social Drinker V/S Socail Smoker",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Social Drinker and Son
Table51 <- table(AbsentData1$SocialDrinker,AbsentData1$Son)
#Viewing Two Table
Table51
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table51)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table51),main="Social Drinker V/S Son",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Social Drinker and Pet
Table52 <- table(AbsentData1$SocialDrinker,AbsentData1$Pet)
#Viewing Two Table
Table52
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table52)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table52),main="Social Drinker V/S Pet",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Social Smoker and Son
Table53 <- table(AbsentData1$SocialSmoker,AbsentData1$Son)
#Viewing Two Table
Table53
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table53)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table53),main="Social Smoker V/S Son",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Social Smoker and Pet
Table54 <- table(AbsentData1$SocialSmoker,AbsentData1$Pet)
#Viewing Two Table
Table54
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table54)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table54),main="Social Smoker V/S Pet",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Son and Pet
Table55 <- table(AbsentData1$Son,AbsentData1$Pet)
#Viewing Two Table
Table55
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table55)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table55),main="Son V/S Pet",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)


#OULIER ANALYSIS

#Univariate Outlier Analysis
#Making a Copy of Data
AbsentDataCopy =AbsentData

#Detecting Outliers in 'Absent Time' using Box Plot Method (Target Variable)
ggplot(AbsentData, aes(x ="", y = AbsentData$AbsentTime)) + geom_boxplot() 
#There are outliers 

#Removing Outliers using Box Plot Method from Target Variable
XYZ = AbsentDataCopy$AbsentTime[AbsentDataCopy$AbsentTime %in% boxplot.stats(AbsentData$AbsentTime)$out]
AbsentDataCopy = AbsentDataCopy[which(!AbsentDataCopy$AbsentTime %in% XYZ),]

#Detecting Outliers in 'Absent Time' using Box Plot Method
ggplot(AbsentDataCopy, aes(x ="", y = AbsentDataCopy$AbsentTime)) + geom_boxplot() 
#Making Changes to Actual DataSet
AbsentData = AbsentDataCopy
#There is an outliers 

#Detecting Outliers in 'Service Time' using Box Plot Method 
ggplot(AbsentData, aes(x ="", y = AbsentData$ServiceTime)) + geom_boxplot() 
#There are outliers 

#Analyzing the relationship of Service Time attribute with Target Attribute AbsentTime
ggplot(AbsentData, aes(x=AbsentData$ServiceTime,y=AbsentData$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Service Time")+ylab("Absent Time")+
  ggtitle("Analysing relation of Service Time with Absent Time")
#Removing Outliers using Box Plot Method
XYZ = AbsentDataCopy$ServiceTime[AbsentDataCopy$ServiceTime %in% boxplot.stats(AbsentData$ServiceTime)$out]
AbsentDataCopy = AbsentDataCopy[which(!AbsentDataCopy$ServiceTime %in% XYZ),]
#Analyzing after removing outliers
#Boxplot for Service Time Attributes
ggplot(AbsentDataCopy, aes(x ="", y = AbsentDataCopy$ServiceTime)) + geom_boxplot() 
#Verifying Relationship between Humidity and Target Variable
ggplot(AbsentDataCopy, aes(x=AbsentDataCopy$ServiceTime,y=AbsentDataCopy$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Service Time")+ylab("Absent Time")+
  ggtitle("Analysing relationship of Service Time with Absent Time")
#Comparing Correlation of BikeData Set and BikeDataCopy Data Set
cor(AbsentData$ServiceTime,AbsentData$AbsentTime)
cor(AbsentDataCopy$ServiceTime,AbsentDataCopy$AbsentTime) 
#Since Correlation is moving towards more stronger relationship we will accept it.Moving closer to 1.
AbsentData = AbsentDataCopy
#Verifying Changes
cor(AbsentData$ServiceTime,AbsentData$AbsentTime)
cor(AbsentDataCopy$ServiceTime,AbsentDataCopy$AbsentTime)  

#Detecting Outliers in 'Residence Distance' using Box Plot Method
ggplot(AbsentData, aes(x ="", y = AbsentData$ResidenceDistance)) + geom_boxplot() 
#There is no outliers

#Detecting Outliers in 'Age' using Box Plot Method
ggplot(AbsentData, aes(x ="", y = AbsentData$Age)) + geom_boxplot() 
#There are outliers
#Analyzing the relationship of Age attribute with Target Attribute AbsentTime
ggplot(AbsentData, aes(x=AbsentData$Age,y=AbsentData$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Age")+ylab("Absent Time")+
  ggtitle("Analyzing relation of Age with Absent Time")
#Removing Outliers using Box Plot Method
XYZ = AbsentDataCopy$Age[AbsentDataCopy$Age %in% boxplot.stats(AbsentData$Age)$out]
AbsentDataCopy = AbsentDataCopy[which(!AbsentDataCopy$Age %in% XYZ),]
#Analyzing after removing outliers
#Boxplot for Service Time Attributes
ggplot(AbsentDataCopy, aes(x ="", y = AbsentDataCopy$Age)) + geom_boxplot() 
#Verifying Relationship between Age and Target Variable
ggplot(AbsentDataCopy, aes(x=AbsentDataCopy$Age,y=AbsentDataCopy$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Age")+ylab("Absent Time")+
  ggtitle("Analysing relationship of Age with Absent Time")
#Comparing Correlation of AbsentData Set and AbsentDataCopy Data Set
cor(AbsentData$Age,AbsentData$AbsentTime)
cor(AbsentDataCopy$Age,AbsentDataCopy$AbsentTime) 
#Since Correlation is moving towards more stronger relationship we will accept it..
AbsentData = AbsentDataCopy
#Verifying Changes
cor(AbsentData$Age,AbsentData$AbsentTime)
cor(AbsentDataCopy$Age,AbsentDataCopy$AbsentTime) 

#Detecting Outliers in 'Average Work Load' using Box Plot Method
ggplot(AbsentData, aes(x ="", y = AbsentData$AverageWorkLoad)) + geom_boxplot() 
#There are outliers.
#Analyzing the relationship of Average work load attribute with Target Attribute AbsentTime
ggplot(AbsentData, aes(x=AbsentData$AverageWorkLoad,y=AbsentData$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Average Work Load")+ylab("Absent Time")+
  ggtitle("Analyzing relation of Average Work Load with Absent Time")
#Removing Outliers using Box Plot Method
XYZ = AbsentDataCopy$AverageWorkLoad[AbsentDataCopy$AverageWorkLoad %in% boxplot.stats(AbsentData$AverageWorkLoad)$out]
AbsentDataCopy = AbsentDataCopy[which(!AbsentDataCopy$AverageWorkLoad %in% XYZ),]
#Analyzing after removing outliers
#Boxplot for Service Time Attributes
ggplot(AbsentDataCopy, aes(x ="", y = AbsentDataCopy$AverageWorkLoad)) + geom_boxplot() 
#Verifying Relationship between Age and Target Variable
ggplot(AbsentDataCopy, aes(x=AbsentDataCopy$AverageWorkLoad,y=AbsentDataCopy$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Average Work Load")+ylab("Absent Time")+
  ggtitle("Analysing relationship of Average Work Load with Absent Time")
#Comparing Correlation of AbsentData Set and AbsentDataCopy Data Set
cor(AbsentData$AverageWorkLoad,AbsentData$AbsentTime)
cor(AbsentDataCopy$AverageWorkLoad,AbsentDataCopy$AbsentTime) 
#Since Correlation is moving towards more stronger relationship we will accept it..
AbsentData = AbsentDataCopy
#Verifying Changes
cor(AbsentData$AverageWorkLoad,AbsentData$AbsentTime)
cor(AbsentDataCopy$AverageWorkLoad,AbsentDataCopy$AbsentTime) 

#Detecting Outliers in 'Expenses' using Box Plot Method
ggplot(AbsentData, aes(x ="", y = AbsentData$Expenses)) + geom_boxplot() 
#There are outliers.
#Analyzing the relationship of Expenses attribute with Target Attribute AbsentTime
ggplot(AbsentData, aes(x=AbsentData$Expenses,y=AbsentData$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Expenses")+ylab("Absent Time")+
  ggtitle("Analyzing relation of Expenses with Absent Time")
#Removing Outliers using Box Plot Method
XYZ = AbsentDataCopy$Expenses[AbsentDataCopy$Expenses %in% boxplot.stats(AbsentData$Expenses)$out]
AbsentDataCopy = AbsentDataCopy[which(!AbsentDataCopy$Expenses %in% XYZ),]
#Analyzing after removing outliers
#Boxplot for Service Time Attributes
ggplot(AbsentDataCopy, aes(x ="", y = AbsentDataCopy$Expenses)) + geom_boxplot() 
#Verifying Relationship between Age and Target Variable
ggplot(AbsentDataCopy, aes(x=AbsentDataCopy$Expenses,y=AbsentDataCopy$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Expenses")+ylab("Absent Time")+
  ggtitle("Analysing relationship of Expenses with Absent Time")
#Comparing Correlation of AbsentData Set and AbsentDataCopy Data Set
cor(AbsentData$Expenses,AbsentData$AbsentTime)
cor(AbsentDataCopy$Expenses,AbsentDataCopy$AbsentTime) 
#Since Correlation is moving towards more stronger relationship we will accept it..
AbsentData = AbsentDataCopy
#Verifying Changes
cor(AbsentData$Expenses,AbsentData$AbsentTime)
cor(AbsentDataCopy$Expenses,AbsentDataCopy$AbsentTime) 

#Detecting Outliers in 'Hit Target' using Box Plot Method
ggplot(AbsentData, aes(x ="", y = AbsentData$HitTarget)) + geom_boxplot() 
#There are outliers.
#Analyzing the relationship of Expenses attribute with Target Attribute AbsentTime
ggplot(AbsentData, aes(x=AbsentData$HitTarget,y=AbsentData$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Hit Target")+ylab("Absent Time")+
  ggtitle("Analyzing relation of Hit Target with Absent Time")
#Removing Outliers using Box Plot Method
XYZ = AbsentDataCopy$HitTarget[AbsentDataCopy$HitTarget %in% boxplot.stats(AbsentData$HitTarget)$out]
AbsentDataCopy = AbsentDataCopy[which(!AbsentDataCopy$HitTarget %in% XYZ),]
#Analyzing after removing outliers
#Boxplot for Service Time Attributes
ggplot(AbsentDataCopy, aes(x ="", y = AbsentDataCopy$HitTarget)) + geom_boxplot() 
#Verifying Relationship between Age and Target Variable
ggplot(AbsentDataCopy, aes(x=AbsentDataCopy$HitTarget,y=AbsentDataCopy$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Hit Target")+ylab("Absent Time")+
  ggtitle("Analysing relationship of Hit Target with Absent Time")
#Comparing Correlation of AbsentData Set and AbsentDataCopy Data Set
cor(AbsentData$HitTarget,AbsentData$AbsentTime)
cor(AbsentDataCopy$HitTarget,AbsentDataCopy$AbsentTime) 
#Since Correlation is moving towards more stronger relationship we will accept it..
AbsentData = AbsentDataCopy
#Verifying Changes
cor(AbsentData$HitTarget,AbsentData$AbsentTime)
cor(AbsentDataCopy$HitTarget,AbsentDataCopy$AbsentTime) 

#Detecting Outliers in 'Weight' using Box Plot Method
ggplot(AbsentData, aes(x ="", y = AbsentData$Weight)) + geom_boxplot() 
#There are no outliers.

#Detecting Outliers in 'Height' using Box Plot Method
ggplot(AbsentData, aes(x ="", y = AbsentData$Height)) + geom_boxplot() 
#There are are outliers.
#Analyzing the relationship of Height attribute with Target Attribute AbsentTime
ggplot(AbsentData, aes(x=AbsentData$Height,y=AbsentData$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Height")+ylab("Absent Time")+
  ggtitle("Analyzing relation of Height Target with Absent Time")
#Removing Outliers using Box Plot Method
XYZ = AbsentDataCopy$Height[AbsentDataCopy$Height %in% boxplot.stats(AbsentData$Height)$out]
AbsentDataCopy = AbsentDataCopy[which(!AbsentDataCopy$Height %in% XYZ),]
#Analyzing after removing outliers
#Boxplot for Service Time Attributes
ggplot(AbsentDataCopy, aes(x ="", y = AbsentDataCopy$Height)) + geom_boxplot() 
#Verifying Relationship between Age and Target Variable
ggplot(AbsentDataCopy, aes(x=AbsentDataCopy$Height,y=AbsentDataCopy$AbsentTime))+geom_point()+
  geom_smooth()+xlab("Height")+ylab("Absent Time")+
  ggtitle("Analysing relationship of Height Target with Absent Time")
#Comparing Correlation of AbsentData Set and AbsentDataCopy Data Set
cor(AbsentData$Height,AbsentData$AbsentTime)
cor(AbsentDataCopy$Height,AbsentDataCopy$AbsentTime) 
#Since Correlation is moving towards more stronger relationship we will accept it..
AbsentData = AbsentDataCopy
#Verifying Changes
cor(AbsentData$Height,AbsentData$AbsentTime)
cor(AbsentDataCopy$Height,AbsentDataCopy$AbsentTime) 

#Detecting Outliers in 'Body Mass Index' using Box Plot Method
ggplot(AbsentData, aes(x ="", y = AbsentData$BodyMassIndex)) + geom_boxplot() 
#There are no outliers.

#Feature Engineering or Feature Selection
#Verifying Correlation among all the Numeric Attributes in the dataset
corrgram(AbsentData[,c('ResidenceDistance','ServiceTime','Age','AverageWorkLoad','Expenses','HitTarget','Weight'
                       ,'Height','BodyMassIndex','AbsentTime')],
         order = T,upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#BodyMassIndex and Weight are higly correlated So I will consider one out of two.Dropping Weight.
#HitTarget,ResidenceDistance have no or weak relation with Target variable so dropping it.


#Dimension Reduction Process
AbsentDataModel= subset(AbsentData,select=-c(Weight,HitTarget,ResidenceDistance))

#First Five rows of Dataset with selected features.
head(AbsentDataModel)

#Normalizing Contineous Variables
CNames = c("Expenses","ServiceTime","Age","AverageWorkLoad","Height","BodyMassIndex")
for(i in CNames){
  print(i)
  AbsentDataModel[i] = (AbsentDataModel[i] - min(AbsentDataModel[i]))/
    (max(AbsentDataModel[i] - min(AbsentDataModel[i])))
}
#Checking Normalized Variables
AbsentDataModel$Expenses
AbsentDataModel$ServiceTime
AbsentDataModel$Age
AbsentDataModel$AverageWorkLoad
AbsentDataModel$Height
AbsentDataModel$BodyMassIndex

#Reflecting changes to main DataSet
AbsentData = AbsentDataModel

#Modal Development

#Developing Model for predicing the number of losses evry month in future.
FeaturesSelected=AbsentData[c("Id" ,"AbsentReason" ,"AbsentMonth","WeekDay","Season","Expenses","ServiceTime",
                            "Age","AverageWorkLoad","DiscplineFailure","Education","Son","SocialDrinker",
                            "SocialSmoker","Pet","Height","BodyMassIndex","AbsentTime")]

#Divide data into train and test using stratified sampling method

train.index = sample(1:nrow(FeaturesSelected),0.8*nrow(FeaturesSelected))
Train = FeaturesSelected[ train.index,]
Test  = FeaturesSelected[-train.index,]



#Decision Tree Model
#Fit a tree based on Training Data
DecisionTree = rpart(AbsentTime~.,Train,method='anova')
DecisionTree
#Plotting Decision Tree
rpart.plot(DecisionTree,type=5)
#Check hoq the model is doing using test dataset
Predictions = predict(DecisionTree,Test)
Predictions
#Root Mean Squared Error
RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
}
RMSE(Test$AbsentTime,Predictions)
#RMSE :- 3.195121

#MAE
MAE = function(y, yhat){
  mean(abs(y - yhat))
}
MAE(Test[,18],Predictions)
#Error Rate :- 1.954684

#Plotting Predictions Graph
plot(cumsum(Predictions), xlab = "Absenteeism in 2011",
     ylab = "Trend of Absenteeism",
     type = "b")
#Saving Decison Tree Predictions
DecisonTreePredictions = Predictions

#Training Random Forest Model
RandomForest = randomForest(AbsentTime~.,data=Train,importance = TRUE)
RandomForest
#Plot Random Forest
plot(RandomForest)
#Predicting for Test Data
Predictions = predict(RandomForest,Test)
Predictions
#MAE
MAE(Test[,12],Predictions)
#Error Rate = 3.421036
RMSE(Test$AbsentTime,Predictions)
#Error Rate = 3.059135

#Plotting Predictions Graph
plot(cumsum(Predictions), xlab = "Absenteeism in 2011",
     ylab = "Trend of Absenteeism",
     type = "b")
RandomForestPredictions = Predictions

#Parameter Tuning For Random Forest
RandomForest = randomForest(AbsentTime~.,data=Train,importance = TRUE,ntree = 800, mtry = 6)
RandomForest
#Plot Random Forest
plot(RandomForest)
#Predicting for Test Data
Predictions = predict(RandomForest,Test)
Predictions
#MAPE
MAE(Test[,12],Predictions)
#Error Rate = 3.419991
RMSE(Test$AbsentTime,Predictions)
#Error Rate = 3.060209
#Plotting Predictions Graph
plot(cumsum(Predictions), xlab = "Absenteeism in 2011",
     ylab = "Trend of Absenteeism",
     type = "b")
RandomForestPredictions = Predictions

#Checking Variable Impotance
# check Variable  Importance 
Importance <- importance(RandomForest)
Importance
#Sorting Variables  
Sort <- names(sort(Importance[,1],decreasing =T))
Sort
# draw varimp plot 
varImpPlot(RandomForest,type = 2)

#Tuning Random Forest Dimensional reduction
#Remove Three variables  which is  contributing  less are
#WeekDay,Education and Social Smoker

Train1 = Train[c("Id" ,"AbsentReason" ,"AbsentMonth","Season","Expenses","ServiceTime",
                 "Age","AverageWorkLoad","DiscplineFailure","Son","SocialDrinker"
                 ,"Pet","Height","BodyMassIndex","AbsentTime")]
Test1 = Test[c("Id" ,"AbsentReason" ,"AbsentMonth","Season","Expenses","ServiceTime",
               "Age","AverageWorkLoad","DiscplineFailure","Son","SocialDrinker"
               ,"Pet","Height","BodyMassIndex","AbsentTime")]

#Developing Model wil Reduced Attributes
RandomForest = randomForest(AbsentTime~.,data=Train1,importance = TRUE,ntree = 1200, mtry = 6)
RandomForest
#Plot Random Forest
plot(RandomForest)
#Predicting for Test Data
Predictions = predict(RandomForest,Test)
Predictions
#MAE
MAE(Test1$AbsentTime,Predictions)
#Error Rate :- 1.949283
RMSE(Test1$AbsentTime,Predictions)
#Error Rate :- 3.054139
#Plotting Predictions Graph
plot(cumsum(Predictions), xlab = "Absenteeism in 2011",
     ylab = "Trend of Absenteeism",
     type = "b")
RandomForestPredictions = Predictions

#Linear Regression Model
#Checking Multicollinearity
vif(Train[,-18])
vifcor(Train[,-18],th=0.9) 
#Removing Id,Expenses,ServiceTime,Age,SocialDrinker as their VIF is quite high as compared to others.

Train2 = Train[c("AbsentReason" ,"AbsentMonth","Season","AverageWorkLoad","DiscplineFailure","Son",
                 "Pet","Height","BodyMassIndex","AbsentTime")]
Test2 = Test[c("AbsentReason" ,"AbsentMonth","Season","AverageWorkLoad","DiscplineFailure","Son",
               "Pet","Height","BodyMassIndex","AbsentTime")]

#Developing Linear Regression Model
LinearRegression = lm(AbsentTime~.,data=Train2)
#Summary of the model
summary(LinearRegression)
#Prediction on test data
Predictions = predict(LinearRegression,Test2[,-10])
Predictions
#Checking Accuracy of the Model
MAE(Test2$AbsentTime,Predictions)
#Error RAte :- 2.228576
RMSE(Test2$AbsentTime,Predictions)
#Error RAte :- 3.304952
#Plotting Predictions Graph
plot(cumsum(Predictions), xlab = "Absenteeism in 2011",
     ylab = "Trend of Absenteeism",
     type = "b")
LinearRegressionPredictions = Predictions


#Solving Business Problem 
#Visualization

#Checking relationship between Son and Absent Time
Table <- table(AbsentData1$AbsentTime,AbsentData1$Son)
#Viewing Two Table
Table
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table),main="Son V/S Absent Time",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)

#Checking relationship between Pet and Absent Time
Table <- table(AbsentData1$AbsentTime,AbsentData1$Pet)
#Viewing Two Table
Table
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table),main="Pet V/S Absent Time",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)


#Checking relationship between Social Smoker and Absent Time
Table <- table(AbsentData1$AbsentTime,AbsentData1$SocialSmoker)
#Viewing Two Table
Table
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table),main="Social Smoker V/S Absent Time",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)


#Checking relationship between Social Drinker and Absent Time
Table <- table(AbsentData1$AbsentTime,AbsentData1$SocialDrinker)
#Viewing Two Table
Table
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table),main="Social Drinker V/S Absent Time",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)


Table <- table(AbsentData1$AbsentTime,AbsentData1$Education)
#Viewing Two Table
Table
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table),main="Education V/S Absent Time",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)


Table <- table(AbsentData1$AbsentTime,AbsentData1$DiscplineFailure)
#Viewing Two Table
Table
#To display the joint distribution (which gives the proportion for each cell):
prop.table(Table)
#Analyzing Categorical 'Table1' using BarPlot Method
c6<-c("black","orange","red","blue")
barplot(as.matrix(Table),main="Discpline Failure V/S Absent Time",ylab="Frequency",cex.lab = 1,
        cex.main = 1,beside=TRUE,col=c6)


ggplot(AbsentData, aes_string(x = AbsentData$Season,y = AbsentData$AbsentTime)) +
  geom_bar(stat="identity",fill =  "red") + theme_bw() +  xlab("Season") + ylab('Absenteeism')

ggplot(AbsentData, aes_string(x = AbsentData$WeekDay,y = AbsentData$AbsentTime)) +
  geom_bar(stat="identity",fill =  "red") + theme_bw() +  xlab("Week Day") + ylab('Absenteeism')

ggplot(AbsentData, aes_string(x = AbsentData$AbsentMonth,y = AbsentData$AbsentTime)) +
  geom_bar(stat="identity",fill =  "red") + theme_bw() +  xlab("Month") + ylab('Absenteeism')

ggplot(AbsentData, aes_string(x = AbsentData$AbsentReason,y = AbsentData$AbsentTime)) +
  geom_bar(stat="identity",fill =  "red") + theme_bw() +  xlab("Absent Reason") + ylab('Absenteeism')

ggplot(AbsentData, aes_string(x = AbsentData$Id,y = AbsentData$AbsentTime)) +
  geom_bar(stat="identity",fill =  "red") + theme_bw() +  xlab("Id") + ylab('Absenteeism')
