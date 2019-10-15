##################   DATA EXTRACTION #################

# LOADING THE DATA SET
Traindata =  read.csv("2016.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = c('NA',''))

# CHECKING THE STRUCTURE OF THE DATASET
View(Traindata)
str(Traindata)
summary(Traindata)


# CHECKING FOR MISSING VALUES 
is.na(Traindata) 
missingTraindata = colSums(is.na(Traindata))
View(missingTraindata)


#### DATA FORMATING ####

# CHANGING DATE INTO DATE FORMAT
'install.packages(lubridate)'
library(lubridate)
Traindata$FL_DATE <- dmy(Traindata$FL_DATE)

# CHANGING THE DEPARTURE AND ARRIVAL VARIABLES IN TO TIME-FORMAT

'install.packages("chron")'
library(chron)
Traindata$CRS_DEP_TIME = times(sub("(.{2})", "\\1:", sprintf("%04d:00", Traindata$CRS_DEP_TIME)))
#Traindata$DEP_TIME = times(sub("(.{2})", "\\1:", sprintf("%04d:00", Traindata$DEP_TIME)))
Traindata$CRS_ARR_TIME = times(sub("(.{2})", "\\1:", sprintf("%04d:00", Traindata$CRS_ARR_TIME)))
#Traindata$ARR_TIME = times(sub("(.{2})", "\\1:", sprintf("%04d:00", Traindata$ARR_TIME)))

#write.csv(Traindata, file = "FINALDATA.csv", row.names = F)
#######  DATA CLEANING   ####################

# FILLING MISSING DATA 
# A FLIGHT IS DELAYED FIRST AND THEN CANCELLED SO THEREFORE IF CANCELLED = 1 THEN DEPARTURE-DELAY = 1 and DEP time = 0
# A FLIGHT IS DELAYED FIRST AND THEN CANCELLED SO THEREFORE IF CANCELLED = 1 THEN ARRIVAL-DELAY = 1 and ARR time = 1

Traindata$DEP_DEL15=as.numeric(Traindata$DEP_DEL15)
Traindata[is.na(Traindata$DEP_DEL15), "DEP_DEL15"] = 1 

Traindata$DEP_TIME=as.integer(Traindata$DEP_TIME)
Traindata[is.na(Traindata$DEP_TIME), "DEP_TIME"] = 0

Traindata$DEP_DELAY_NEW=as.integer(Traindata$DEP_DELAY_NEW)
Traindata[is.na(Traindata$DEP_DELAY_NEW), "DEP_DELAY_NEW"] = 0

Traindata$ARR_DEL15=as.integer(Traindata$ARR_DEL15)
Traindata[is.na(Traindata$ARR_DEL15), "ARR_DEL15"] = 1 

Traindata$ARR_DELAY_NEW=as.integer(Traindata$ARR_DELAY_NEW)
Traindata[is.na(Traindata$ARR_DELAY_NEW), "ARR_DELAY_NEW"] = 0

Traindata$ARR_TIME=as.integer(Traindata$ARR_TIME)
Traindata[is.na(Traindata$ARR_TIME), "ARR_TIME"] = 0

Traindata$AIR_TIME=as.integer(Traindata$AIR_TIME)
Traindata[is.na(Traindata$AIR_TIME), "AIR_TIME"] = 0

#############################################################################################################

library(car)
pairs(Traindata[c("DEP_DELAY_NEW", "ARR_DELAY_NEW")])
pairs(Traindata[c("DISTANCE", "AIR_TIME")]) 
pairs(Traindata[c("ORIGIN_CITY_MARKET_ID", "DEST_AIRPORT_ID")]) 


##############################################################################################################
# DIVIDING THE DATA IN TRAIN AND VALIDATION SET  

library(caTools)        
set.seed(12345)
ind=sample(2, nrow(Traindata), replace=TRUE, prob=c(0.7, 0.3))
TRAIN = Traindata[ind==1,]
TEST = Traindata[ind==2,]

# DIVIDING TRAIN AND VALIDATION
ind=sample(2, nrow(TRAIN), replace=TRUE, prob=c(0.7, 0.3))
TREN = TRAIN[ind==1,]
VALID = TRAIN[ind==2,]

#  TARGET VARIABLE IN FACTORS 
TRAIN$ARR_DEL15=as.factor(TRAIN$ARR_DEL15)
TEST$ARR_DEL15=as.factor(TEST$ARR_DEL15)
TREN$ARR_DEL15=as.factor(TREN$ARR_DEL15)
VALID$ARR_DEL15= as.factor(VALID$ARR_DEL15)

#@@@@@@@@@@@@@@@@@@@@@@ LOGISTIC REGRESSION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

set.seed(12345)
library(caret)
logisticRegModel <- train(ARR_DEL15 ~ ORIGIN_AIRPORT_ID + DEST_AIRPORT_SEQ_ID + DEP_DEL15 + DEP_DELAY_NEW + DISTANCE + AIR_TIME,
                          data=TREN, method = 'glm', family = 'binomial')

logRegPrediction <- predict(logisticRegModel, VALID)
logRegConfMat <- confusionMatrix(logRegPrediction, VALID[,"ARR_DEL15"],positive = '1')
logRegConfMat

############ Final model  ##################

logisticRegModel <- train(ARR_DEL15 ~ ORIGIN_AIRPORT_ID + DEST_AIRPORT_SEQ_ID + DEP_DEL15 + DEP_DELAY_NEW + DISTANCE + AIR_TIME,
                          data=TRAIN, method = 'glm', family = 'binomial')

logRegPrediction <- predict(logisticRegModel, TEST)
logRegConfMat <- confusionMatrix(logRegPrediction, TEST[,"ARR_DEL15"],positive = '1')
logRegConfMat

num = as.factor(logRegPrediction)
num1 = as.factor(TEST[,"ARR_DEL15"])    

ROC = roc(num1,num)
ROC$auc
plot(ROC)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

#@@@@@@@@@@@@ DECISION TREE MODEL WITH FINE TUNING  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@# 
library(pROC)
library(caret)
library(rpart)
library(rpart.plot)

set.seed(12345)
Decision_tree  = rpart(ARR_DEL15 ~ FL_DATE +ORIGIN_AIRPORT_ID + DEST_AIRPORT_SEQ_ID + DEP_DEL15 + DEP_DELAY_NEW + DISTANCE + AIR_TIME,
                       data = TREN,method = 'class',
                       control = rpart.control( cp = 0.01, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                                surrogatestyle = 0, maxdepth = 30))

Decision_tree
plot(Decision_tree)
print(Decision_tree)

Decision_prediction = predict(Decision_tree, VALID,type = "class")


#Confusion matrix FOR VALIDATION SET

confusion_matrix_reg <- confusionMatrix(factor(Decision_prediction),VALID$ARR_DEL15, positive = '1')
confusion_matrix_reg

###### FINAL DECISION TREE ##############

set.seed(12345)
Decision_tree  = rpart(ARR_DEL15 ~ FL_DATE +ORIGIN_AIRPORT_ID + DEST_AIRPORT_SEQ_ID + DEP_DEL15 + DEP_DELAY_NEW + DISTANCE + AIR_TIME,
                       data = TRAIN,method = 'class',
                       control = rpart.control( cp = 0.01, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                                surrogatestyle = 0, maxdepth = 30))

Decision_tree
plotcp(Decision_tree)
print(Decision_tree)

Decision_prediction = predict(Decision_tree, TEST,type = "class")


#Confusion matrix 
confusion_matrix_reg <- confusionMatrix(factor(Decision_prediction),TEST$ARR_DEL15, positive = '1')
confusion_matrix_reg

num = as.integer(Decision_prediction)
num1 = as.integer(TEST$ARR_DEL15)    

ROC = roc(num1,num)
ROC$auc
  
# Visualize the decision tree with rpart.plot

rpart.plot(Decision_tree, box.palette="yellow", shadow.col= "black", nn=TRUE)

summary(Decision_tree)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%  RANDOM FOREST %%%%%%%%%%%%%%%%%%%##

# RANDOM FOREST MODEL WITH FINE TUNING

'TRAIN$ARR_DEL15=as.integer(TRAIN$ARR_DEL15)
TEST$ARR_DEL15=as.integer(TEST$ARR_DEL15)
TREN$ARR_DEL15=as.integer(TREN$ARR_DEL15)
VALID$ARR_DEL15= as.integer(VALID$ARR_DEL15)'

library(randomForest)
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)



set.seed(12345)
mod.rf <- randomForest(ARR_DEL15 ~  ORIGIN_AIRPORT_ID + DEST_AIRPORT_SEQ_ID + DEP_DEL15 + DEP_DELAY_NEW + DISTANCE + AIR_TIME,
                       data = TREN, prob=TRUE)
plot(mod.rf)
importance(mod.rf)

'print(random)
Plot = varImpPlot(random, scale = T, col="blue")
getTree(random, k = 1, labelVar=TRUE)'

Forest_prediction = predict(mod.rf, VALID, type="class")

Random_confusion <- confusionMatrix(factor(Forest_prediction),VALID$ARR_DEL15,positive = '1')
Random_confusion

######## FINAL RANDOM MODEL ########

mod.rf <- randomForest(ARR_DEL15 ~  ORIGIN_AIRPORT_ID + DEST_AIRPORT_SEQ_ID + DEP_DEL15 + DEP_DELAY_NEW + DISTANCE + AIR_TIME,
                       data = TRAIN, prob=TRUE)
plot(mod.rf)
importance(mod.rf)

Forest_prediction = predict(mod.rf, TEST, type="class")

Random_confusion <- confusionMatrix(factor(Forest_prediction),TEST$ARR_DEL15,positive = '1')
Random_confusion

num = as.integer(Forest_prediction)
num1 = as.integer(TEST$ARR_DEL15)    

ROC = roc(num1,num)
ROC$auc

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
