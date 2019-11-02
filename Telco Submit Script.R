  ##Install and load Libraries
install.packages ("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("class")

library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(readr)

#Let's import the dataset

WA_F <- read_csv("https://raw.githubusercontent.com/lukejohnsaid1989/Telco-Churn-Project/master/Telco%20Customer%20ChurnWA_Fn-UseC_-Telco-Customer-Churn.csv")

mydata <-WA_F

#Let's load the Libraries



#Let's take a look at our dataset
head(mydata)

#Check for NAs
sum(is.na(mydata))
#We have 11 Na's

#Let's look at our output 

base::table(mydata$Churn)
sum(is.na(mydata$Churn))

ggplot(mydata, aes(mydata$Churn)) + geom_bar(fill = "#FF6666") + theme_classic() + xlab("Churn")
mydata$Churn[mydata$Churn == "Yes"] = 1
mydata$Churn[mydata$Churn == "No"] = 0

base::table(mydata$Churn)
sum(is.na(mydata$Churn))

#Let's start looking at our variables one by one

####### VARIABLE ANALYSIS #######

#Lets check our colun names
ColVec <- colnames(mydata)
ColVec

#We will not consider Customer ID

################# GENDER ##################

base::table(mydata$gender)
sum(is.na(mydata$gender))
#No Na's

ggplot(mydata, aes(mydata$gender, mydata$Churn)) + geom_jitter()
#No particular correlation between Gender and Churn

#Let's create a Dummy var for Gender
Genderframe <-as.data.frame(mydata$gender)

dmy <- dummyVars(" ~ .", data = Genderframe)
Genderframe2 <- data.frame(predict(dmy, newdata = Genderframe))
Genderframe2 

############## END GENDER ###############

########## Senior Citizen ###########

base::table(mydata$SeniorCitizen)
sum(is.na(mydata$SeniorCitizen))

ggplot(mydata, aes(mydata$Churn)) + geom_bar()
ggplot(mydata, aes(mydata$SeniorCitizen, mydata$Churn)) + geom_jitter()

############## End Senior Citizen ###########

############## Partner ################

base::table(mydata$Partner)
sum(is.na(mydata$Partner))

mydata$Partner[mydata$Partner == "Yes"] = 1 
mydata$Partner[mydata$Partner == "No"] = 0 

base::table(mydata$Partner)
sum(is.na(mydata$Partner))

ggplot(mydata, aes(mydata$Partner)) + geom_bar()
ggplot(mydata, aes(mydata$Partner, mydata$Churn)) + geom_jitter()

########### Dependents ##########

base::table(mydata$Dependents)
sum(is.na(mydata$Dependents))

mydata$Dependents[mydata$Dependents == "Yes"] = 1
mydata$Dependents[mydata$Dependents == "No"] = 0

base::table(mydata$Dependents)
sum(is.na(mydata$Dependents))

ggplot(mydata, aes(mydata$Dependents)) + geom_bar()
ggplot(mydata, aes(mydata$Dependents, mydata$Churn)) + geom_jitter()

######### end Dependents ###############

########### Lets look at Tenure ###########

base::table(mydata$tenure)
sum(is.na(mydata$tenure))

ggplot(mydata, aes(mydata$tenure)) + geom_histogram(binwidth = 2)
ggplot(mydata, aes(mydata$Churn, mydata$tenure)) + geom_violin()

TenureFrame <- as.data.frame(mydata$tenure)
x = 1
while (x<=nrow(TenureFrame)){
  if ((TenureFrame$`mydata$tenure`[x] > 0) & (TenureFrame$`mydata$tenure`[x] <= 20)) 
        {TenureFrame$`mydata$tenure`[x] = "Between 1 and 20"}
  else if ((TenureFrame$`mydata$tenure`[x] > 20) & (TenureFrame$`mydata$tenure`[x] <= 40)) 
        {TenureFrame$`mydata$tenure`[x] = "Between 21 and 40"}
  else if ((TenureFrame$`mydata$tenure`[x] > 40) & (TenureFrame$`mydata$tenure`[x] <= 60)) 
        {TenureFrame$`mydata$tenure`[x] = "Between 41 and 60"}
  else if (TenureFrame$`mydata$tenure`[x] > 60) 
  {TenureFrame$`mydata$tenure`[x] = "Over 61"}
  x = x + 1
}

base::table(TenureFrame$`mydata$tenure`)

# let's create dummy var!

dmy <- dummyVars(" ~ .", data = TenureFrame)
TenureFrame2 <- data.frame(predict(dmy, newdata = TenureFrame))
TenureFrame2 

######################## End of Tenure ##############################

###### Let's look at phone service ########

base::table(mydata$PhoneService)
sum(is.na(mydata$PhoneService))

mydata$PhoneService[mydata$PhoneService == "Yes"] = 1
mydata$PhoneService[mydata$PhoneService == "No"] = 0

base::table(mydata$PhoneService)
sum(is.na(mydata$PhoneService))

################### end of Phone Service ######################

########## Lets look at the rest ############

base::table(mydata$MultipleLines)
sum(is.na(mydata$MultipleLines))
base::table(mydata$InternetService)
sum(is.na(mydata$InternetService))
base::table(mydata$OnlineSecurity)
sum(is.na(mydata$OnlineSecurity))
#No internet service is duplicate in the previous two variables
base::table(mydata$OnlineBackup)
sum(is.na(mydata$OnlineBackup))
base::table(mydata$DeviceProtection)
sum(is.na(mydata$DeviceProtection))
base::table(mydata$TechSupport)
sum(is.na(mydata$TechSupport))
base::table(mydata$StreamingTV)
sum(is.na(mydata$StreamingTV))
base::table(mydata$StreamingMovies)
sum(is.na(mydata$StreamingMovies))

##### Let's make a dummy var with all these

ServiceFrame <- cbind.data.frame(mydata$PhoneService, mydata$MultipleLines, mydata$InternetService, mydata$OnlineSecurity, mydata$OnlineBackup, mydata$DeviceProtection, mydata$TechSupport, mydata$StreamingTV, mydata$StreamingMovies)

dmy <- dummyVars(" ~ .", data = ServiceFrame)
ServiceFrame2 <- data.frame(predict(dmy, newdata = ServiceFrame))
ServiceFrame2 

#Some columns we dont need

ServiceFrame2 <- cbind.data.frame(ServiceFrame2$X.mydata.PhoneService.0, ServiceFrame2$X.mydata.PhoneService.1, ServiceFrame2$X.mydata.MultipleLines.No, ServiceFrame2$X.mydata.MultipleLines.Yes, ServiceFrame2$X.mydata.InternetService.DSL, ServiceFrame2$X.mydata.InternetService.Fiber.optic, ServiceFrame2$X.mydata.InternetService.No, ServiceFrame2$X.mydata.OnlineSecurity.No, ServiceFrame2$X.mydata.OnlineSecurity.Yes, ServiceFrame2$X.mydata.OnlineBackup.No, ServiceFrame2$X.mydata.OnlineBackup.Yes, ServiceFrame2$X.mydata.DeviceProtection.No, ServiceFrame2$X.mydata.DeviceProtection.Yes, ServiceFrame2$X.mydata.TechSupport.No, ServiceFrame2$X.mydata.TechSupport.Yes, ServiceFrame2$X.mydata.StreamingTV.No, ServiceFrame2$X.mydata.StreamingTV.Yes, ServiceFrame2$X.mydata.StreamingMovies.No, ServiceFrame2$X.mydata.StreamingMovies.Yes)

####### Services Complete #########

#### Lets look at contract ####

base::table(mydata$Contract)
sum(is.na(mydata$Contract))

ContractFrame <- cbind.data.frame(mydata$Contract)

dmy <- dummyVars(" ~ .", data = ContractFrame)
ContractFrame2 <- data.frame(predict(dmy, newdata = ContractFrame))
ContractFrame2 

################## end of Contract #############ContractFrame

### Lets look at Paperless billing

base::table(mydata$PaperlessBilling)
sum(is.na(mydata$PaperlessBilling))

mydata$PaperlessBilling[mydata$PaperlessBilling == "Yes"] = 1
mydata$PaperlessBilling[mydata$PaperlessBilling == "No"] = 0

base::table(mydata$PaperlessBilling)
sum(is.na(mydata$PaperlessBilling))

######### End of paperless billing ########

#### Lets look at payment method

base::table(mydata$PaymentMethod)
sum(is.na(mydata$PaymentMethod))

PaymentFrame <- as.data.frame(mydata$PaymentMethod)

dmy <- dummyVars(" ~ .", data = PaymentFrame)
PaymentFrame2 <- data.frame(predict(dmy, newdata = PaymentFrame))
PaymentFrame2 

############ Ready with Payment method #########

##### Lets look at monthly charges ######

base::table(mydata$MonthlyCharges)
sum(is.na(mydata$MonthlyCharges))

#ggplot(mydata, aes(mydata$MonthlyCharges)) + geom_histogram(binwidth = 2)

###### Let's look at Total Charges ######

#We have Nas

x=1

while (x <= nrow(mydata))
{
  if ((is.na(mydata$TotalCharges[x])) == TRUE)
    
  { mydata$TotalCharges[x] <- (mydata$MonthlyCharges[x]*mydata$tenure[x])}
  
  x = x + 1
}
sum(is.na(mydata$TotalCharges))
sum(is.na(mydata))

######################## Split the Data and Build Logistic Regression model #####

newdata <- cbind.data.frame(mydata$SeniorCitizen, mydata$Partner, mydata$Dependents, mydata$tenure, Genderframe2, PaymentFrame2, ServiceFrame2, TenureFrame2, mydata$PaperlessBilling, ContractFrame2, mydata$MonthlyCharges, mydata$TotalCharges, mydata$Churn)

### Scale Data

x = 1

while (x<=length(colnames(newdata))) {
  newdata[,x] <- scale(as.numeric(newdata[,x], center = TRUE, scale = TRUE))
  x = x+1
}

newdata$`mydata$Churn` <-mydata$Churn

### Split the data

bound = 0.5
train = newdata[1:(bound*(nrow(newdata))),]
test = newdata[(((nrow(mydata))*bound)+1):nrow(newdata),]
head(train)
head(test)
print("The number of training rows are")
nrow(train)
print("The number of test rows are")
nrow(test)

### Create Model

model <- glm( as.numeric(`mydata$Churn`) ~.
              ,family=binomial(link='logit'),data=train, control = list(maxit = 200))
summary(model)

### Make Prediction

predictChurn <- predict(model, newdata = test,type='response')


### Measure Accuracy

conf_mat <- base::table(test$`mydata$Churn`,round(predictChurn))
conf_mat

cat("Test accuracy: ", sum(diag(conf_mat))/sum(conf_mat))

conf.mat <- confusionMatrix(as.factor(round(predictChurn)), as.factor(test$`mydata$Churn`))

conf.mat


