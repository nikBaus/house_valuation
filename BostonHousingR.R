library(janitor)
library(tidyverse)
library(data.table)
setwd("C:/Users/user/Desktop/R Programs")

#loading the dataset
boston <- read.csv('boston_housing.csv')
setDT(boston)

#Performing EDA
head(boston)
mv <- is.na(boston)
colSums(mv)
str(boston)
summary(boston)

#Renaming the columns for readability
colnames(boston)<- c("CrimeRate","Zoned","NRetail","River","NOX","RoomNo","Age",
                     "Access","Dist","Tax","PTRatio","BlackPop","LowerClass","Value")

#filtering the houses valued at 50 and above for the sake of accuracy
boston <- boston[Value < 50, ]

#Apparently, variables CrimeRate, Zoned, NRetail, River, Age, LowerClass are the variables with missing values
head(boston,10)
nrow(boston)

boston[ ,.N, BlackPop][order(-BlackPop)]
boston1 <- boston[BlackPop >= 200, ]

#Working on the null values and outliers
#CrimeRate
boston1[is.na(boston1$CrimeRate), ][order(-Value)]
summary(boston$CrimeRate)
boston1[is.na(boston1$CrimeRate), CrimeRate := 0.24522]
boston1[, .N, CrimeRate][order(-N)]
nrow(boston1)

#Zoned
boston1[is.na(boston1$Zoned), Zoned := 11.85]
hist(boston1$Zoned)
summary(boston1$Zoned)

#NRetail
boston1[is.na(boston1$NRetail), NRetail:= 10.45]
summary(boston1$NRetail)
hist(boston1$NRetail)

#River
boston1[is.na(boston1$River), River := 1]
summary(boston1)

#Age
boston1[is.na(boston1$Age), Age := 66.51]
boston1[is.na(boston1$LowerClass), LowerClass := 12.317]

#Null values have been sorted. Checking outliers

#Plotting the density chart of all the variables
crime <- density(boston1$CrimeRate)
plot(crime, frame = T, col = "Green", main = "CrimeRate Density Plot")

zone <- density(boston1$Zoned)
plot(zone, frame = T, col = "Green", main = "Zone Density Plot")

retail <- density(boston1$NRetail)
plot(retail, frame = T, col = "Green", main = "NRetail Density Plot")

nitro <- density(boston1$NOX)
plot(nitro, frame = T, col = "Green", main = "NO2 Density Plot")

room <- density(boston1$RoomNo)
plot(room, frame = T, col = "Green", main = "RoomNo Density Plot")

age <- density(boston1$Age)
plot(age, frame = T, col = "Green", main = "Age Density Plot")

access <- density(boston1$Access)
plot(access, frame = T, col = "Green", main = "Access Density Plot")

distance <- density(boston1$Dist)
plot(distance, frame = T, col = "Green", main = "Distance Density Plot")

tax <- density(boston1$Tax)
plot(tax, frame = T, col = "Green", main = "Tax Density Plot")

ptratio <- density(boston1$PTRatio)
plot(ptratio, frame = T, col = "Green", main = "PTr Density Plot")

blackpop <- density(boston1$BlackPop)
plot(blackpop, frame = T, col = "Green", main = "BlackPop Density Plot")

lowerclass <- density(boston1$LowerClass)
plot(lowerclass, frame = T, col = "Green", main = "LowerClass Density Plot")

#For some columns, removing the outliers will reduce the quality of the data as 
##it is expected that they will have a high variance. e.g. CrimeRate

#Checking for multicollinearity
library(ggcorrplot)

corr = round(cor(boston1), 2)
ggcorrplot(corr, hc.order = T, type = "lower", lab = T)

#Dist and Tax have a collinearity of 0.89, so one will be dropped for the model

#Performing train test split
library(caTools)
sample <- sample.split(boston1$Value, SplitRatio = 0.8) 
train <- subset(boston1, sample == T)
test <- subset(boston1, sample == F)

train
test

#Creating the regression model
Model1 <- lm(Value~CrimeRate+Zoned+NRetail+River+NOX+RoomNo+Age+
                     Access+Dist+Tax+PTRatio+BlackPop+LowerClass, data = train)
summary(Model1)
#From the model, it can be seen that the NRetail, River and BlackPop variables are irrelevant to the model; so they will be dropped

Model2 <- lm(Value~CrimeRate+Zoned+NOX+RoomNo+Age+
              Access+Tax+PTRatio+LowerClass, data = train)
summary(Model2)
hist(Model2$residuals)

#Testing the model
ModelT <- predict(Model2, newdata = test)

round(ModelT,1)

plot(ModelT, test$Value, xlab = "Predicted", ylab = "Actual")
abline(a = 0, b=1, col = "red", lwd = 2)

result <- data.table(round(ModelT,1), test$Value)
result[, difference:= V1-V2]
result[,error := ((V2-V1)/V2) * 100]
result$error <- round(result$error, 2)
result

mean(result$difference)


