# FE582 Assignment3 Disha An
# Set or query graphical parameters
par(mfrow=c(1,1))
# Select Walt Disney Co (DIS) here
library("quantmod")
#Allows us to import the data we need and calculate the technical indicators
library("rpart")
#Gives us access to the decision trees we will be using. 
library("rpart.plot")

startDate = as.Date("2015-01-01")
#The beginning of the date range we want to look at
endDate = as.Date("2016-01-01")
#The end of the date range we want to look at
getSymbols("DIS", src = "yahoo", from = startDate, to = endDate, auto.assign = TRUE) 

# Select 10 different technical indicators
# 1. Momentum
Momentum3 <- momentum(Op(DIS), n = 3)
# 2. Moving Average (DEMA)
DEMA3 <- DEMA(Op(DIS), n = 3)
# 3. Volume
Volume<- Vo(DIS)
# 4. Relative Strength Index
RSI3<-RSI(Op(DIS), n = 3)
# 5. Rate of Change
ROC3 <- ROC(Op(DIS), n = 3)
# 6. ZLEMA
ZLEMA3 <- ZLEMA(Op(DIS), n = 3)
# 7. WMA
WMA3 <- WMA(Op(DIS), n = 3)
# 8. DPR
DPO3 <- DPO(Op(DIS), n = 3)
# 9. CCI
CCI3 <- CCI(Op(DIS), n = 3)
# 10. CMF
CMO3 <- CMO(Op(DIS), n = 3)

# Construct a decision tree
#Calculate the difference between the close price and open price
PriceChange<- Cl(DIS) - Op(DIS)
# Construct a class for the price goes up and goes down
Class<-ifelse(PriceChange>0,"UP","DOWN")
#Create our data set
DataSet<-data.frame(Momentum3,DEMA3,Volume,RSI3,ROC3,ZLEMA3,WMA3,DPO3,CCI3,CMO3,Class)
#Name the columns
colnames(DataSet)<-c("Momentum","DEMA","Volume","RSI","ROC","ZLEMA","WMA","DPO","CCI","CMO","Class") 
#Get rid of the data where the indicators are being calculated
DataSet<-DataSet[-c(1:4),]
DataSet <- DataSet[-c(247:248),]
nrow(DataSet)
#Use 2/3 of the data to build the tree
TrainingSet<-DataSet[1:164,]
#And leave out 1/3 data to test our strategy
TestSet<-DataSet[165:246,]

DecisionTree<-rpart(Class~Momentum+DEMA+Volume+RSI+ROC+ZLEMA+WMA+DPO+CCI+CMO,data = TrainingSet,cp=.001)
#Specifying the indicators to we want to use to predict the class and controlling the growth of the tree by setting the minimum amount of information gained (cp) needed to justify a split.

prp(DecisionTree,type=2,extra=8)
#Nice plotting tool with a couple parameters to make it look good. 

printcp(DecisionTree)
#shows the minimal cp for each trees of each size.

plotcp(DecisionTree,upper="splits")
#plots the average geometric mean for trees of each size.

# Prue the tree and calculate the best fit
PrunedDecisionTree<-prune(DecisionTree,cp=0.00100)
#I am selecting the complexity parameter (cp) that has the lowest cross-validated error (xerror)
prp(PrunedDecisionTree, type=2, extra=8)

# Calculate the accuracy of prediction and present the confusion matrix
printcp(PrunedDecisionTree)
table(predict(PrunedDecisionTree,TestSet,type="class"),TestSet[,11],dnn=list('predicted','actual'))
# The accuracy of this prediction is (34+32)/(34+32+11+5)=80.49%

# Propose alternative models
# This time I want to reduce the indexes we used
# 1.DEMA
DEMA1 <- DEMA(Op(DIS),n=3)
# 2.DPO
DPO1 <- DPO(Op(DIS),n=3)
# 3.RSI
RSI3<-RSI(Op(DIS), n= 3)
#Calculate a 3-period relative strength index (RSI) off the open price
# 4.EMA
EMA5<-EMA(Op(DIS),n=5)
#Calculate a 5-period exponential moving average (EMA)
EMAcross<- Op(DIS)-EMA5
#Let's explore the difference between the open price and our 5-period EMA
# 5. MACD
MACD<-MACD(Op(DIS),fast = 12, slow = 26, signal = 9)
#Calculate a MACD with standard parameters
MACDsignal<-MACD[,2]
#Grab just the signal line to use as our indicator.
# 6.SMI
SMI<-SMI(Op(DIS),n=13,slow=25,fast=2,signal=9) 
#Stochastic Oscillator with standard parameters
SMI<-SMI[,1]
# 7.ALMA
ALMA1 <- ALMA(Op(DIS),n = 3)
# 8. WMA
WMA3 <- WMA(Op(DIS), n = 3)
# 9. DPR
DPO3 <- DPO(Op(DIS), n = 3)
# 10. CCI
CCI3 <- CCI(Op(DIS), n = 3)
# 11. CMF
CMO3 <- CMO(Op(DIS), n = 3)

DataSetAlt<-data.frame(DEMA1,DPO1,RSI3,EMAcross,MACDsignal,SMI,ALMA1,WMA3,DPO3,CCI3,CMO3,Class)
#Create our data set
colnames(DataSetAlt)<-c("DEMA","DPO","RSI3","EMAcross","MACDsignal","Stochastic","ALMA","WMA","DPO","CCI","CMO","Class") 
#Get rid of the data where the indicators are being calculated
DataSetAlt<-DataSetAlt[-c(1:33),]
nrow(DataSetAlt)
DataSetAlt<-DataSetAlt[-c(218:219),]
nrow(DataSetAlt)
#Use 2/3 of the data to build the tree
TrainingSetAlt<-DataSetAlt[1:146,]
#And leave out 1/3 data to test our strategy
TestSetAlt<-DataSetAlt[147:217,]

DecisionTreeAlt<-rpart(Class~DEMA+DPO+RSI3+EMAcross+MACDsignal+Stochastic+ALMA+WMA+DPO+CCI+CMO,data=TrainingSetAlt,cp=.001)
#Specifying the indicators to we want to use to predict the class and controlling the growth of the tree by setting the minimum amount of information gained (cp) needed to justify a split.

prp(DecisionTreeAlt,type=2,extra=8)
#Nice plotting tool with a couple parameters to make it look good. If you want to play around with the visualization yourself, here is a great resource.

printcp(DecisionTreeAlt)
#shows the minimal cp for each trees of each size.
plotcp(DecisionTreeAlt,upper="splits")
#plots the average geometric mean for trees of each size.

PrunedDecisionTreeAlt<-prune(DecisionTreeAlt,cp=0.039437)
#I am selecting the complexity parameter (cp) that has the lowest cross-validated error (xerror)

prp(PrunedDecisionTreeAlt, type=2, extra=8)

table(predict(PrunedDecisionTreeAlt,TestSetAlt,type="class"),TestSetAlt[,12],dnn=list('predicted','actual'))
# The accuracy of this prediction is 88.73%

