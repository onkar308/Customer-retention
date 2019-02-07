#project customer retention
data = read.csv("dataset_CC.csv")
view(data)
library(caTools)
set.seed(122)
spl<-sample.split(data$customerID ,SplitRatio = 0.6)
train=subset(data,spl == T)
test=subset(data,spl==F)
combi <- data
summary(combi)
sum(is.na(combi$TotalCharges))
combi$TotalCharges[combi$TotalCharges == ""] = 0
#imputing missing values
simple = combi[,c(17,18,19,20,21)]
library(mice)
imputed = complete(mice(simple))
combi$TotalCharges <- imputed$TotalCharges
sum(is.na(combi$TotalCharges))
#total charges = tenure*monthlycharges
combi=combi[,-20]
#visualization
library(ggplot2)
ggplot(combi)+geom_bar(aes(churn))
ggplot(combi)+geom_bar(aes(combi$Churn))
str(combi$Churn)
ggplot(combi)+geom_histogram(aes(combi$MonthlyCharges))
#visualizing variables under churn count 
ggplot(combi)+geom_bar(aes(combi$InternetService,fill = combi$Churn))
ggplot(combi)+geom_bar(aes(combi$PhoneService ,fill = combi$Churn))
ggplot(combi)+geom_bar(aes(combi$Contract,fill = combi$Churn))
ggplot(combi)+geom_bar(aes(combi$MultipleLines,fill = combi$Churn))
ggplot(combi)+geom_bar(aes(combi$tenure,fill = combi$Churn))

#dropping numerical coloumns and phone service as it is subset of multiple lines
combi=combi[,-c("PhoneService")]
combi=combi[,-7]
combi=combi[,-18]
combi=combi[,-1]
str(combi)
combi$customerID = data$customerID
#converting factors into numeric
combi$gender=as.numeric(combi$gender)
combi$SeniorCitizen=as.numeric(combi$SeniorCitizen)
combi$Partner = as.numeric(combi$Partner)
combi$Dependents=as.numeric(combi$Dependents)
combi$MultipleLines=as.numeric(combi$InternetService)
combi$InternetService=as.numeric(combi$InternetService)
combi$OnlineBackup = as.numeric(combi$OnlineBackup)
combi$OnlineSecurity = as.numeric(combi$OnlineSecurity)
combi$DeviceProtection=as.numeric(combi$DeviceProtection)
combi$TechSupport=as.numeric(combi$TechSupport)
combi$StreamingMovies = data$StreamingMovies
combi$StreamingMovies=as.numeric(combi$StreamingMovies)
combi$Contract = as.numeric(combi$Contract)
combi$PaperlessBilling = as.numeric(combi$PaperlessBilling)
combi$PaymentMethod = as.numeric(combi$PaymentMethod)
combi$StreamingTV = as.numeric(combi$StreamingTV)
combi$SeniorCitizen[combi$SeniorCitizen == 0] = 1
combi$SeniorCitizen[combi$SeniorCitizen == 1] = 2
combi$tenure[combi$tenure == 0] = 1
combi = combi[-18]
#splitting prepared data into train and test
set.seed(122)
Cspl<-sample.split(combi,SplitRatio = 0.6)
Ctrain=subset(combi,Cspl == T)
Ctrain$Churn = as.numeric(Ctrain$Churn)
Ctrain$Churn = as.factor(Ctrain$Churn)
Ctrain$Churn[Ctrain$Churn == 1] = 0
Ctrain$Churn[Ctrain$Churn == 2] = 1

Ctest=subset(combi,Cspl==F)
Ctest$Churn = as.numeric(Ctest$Churn)
Ctest$Churn[Ctest$Churn == 1] = 0
Ctest$Churn[Ctest$Churn == 2] = 1
ctest1<- Ctest[-17]

#applying logistic model
library(glmnet)

logistic_mod = glm(Ctrain$Churn~.,data = Ctrain[,-17],method = "class")
pred = predict(rpart_mod,ctest1)
table(pred>0.5)
table(pred>0.4)
table(Ctest$Churn)

