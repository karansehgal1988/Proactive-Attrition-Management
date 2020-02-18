options(scipen = 999)

# Reading the data in R
data =read.csv("//Users/karansehgal//Downloads//Proactive Attrition Management-Logistic Regression Case Study.csv",stringsAsFactors=T) 

#Structure and summary of dataset
str(data) 
View(data)
summary(data)

colnames(data)


# For factor variable

#Use lapply() to coerce and replace the chosen columns:

cols =  c("CHILDREN","CREDITA","CREDITAA","CREDITB","CREDITC","CREDITDE","CREDITGY","CREDITZ","PRIZMRUR","PRIZMUB",
          "PRIZMTWN", "REFURB", "WEBCAP", "TRUCK", "RV", "OCCPROF", "OCCCLER",  "OCCCRFT", 
          "OCCSTUD",  "OCCHMKR",  "OCCRET", "OCCSELF", "OWNRENT", "MARRYUN", "MARRYYES", "MARRYNO", "MAILORD", "MAILRES", 
          "MAILFLAG", "TRAVEL", "PCOWN", "CREDITCD", "RETCALLS", "RETACCPT", "NEWCELLY", "NEWCELLN","INCMISS" ,
          "INCOME", "MCYCLE", "CREDITAD", "SETPRCM", "RETCALL", "CALIBRAT")



data[cols] = lapply(data[cols],factor)
str(data)

# Finding numeric/other variables.
data_num = sapply(data,is.numeric)    # Numeric Variables 
data_char = !sapply(data,is.numeric)  # Other Variables


# Defining User function for Descriptive Statistics
mystats = function(x)
{
  nmiss = sum(is.na(x))
  m = mean(x,na.rm = T)
  max = max(x,na.rm = T)
  min = min(x,na.rm = T)
  s = sd(x,na.rm = T)
  p1 = quantile(x,0.01,na.rm = T)
  p5 = quantile(x,0.05,na.rm = T)
  p10 = quantile(x,0.10,na.rm = T)
  q1 = quantile(x,0.25,na.rm = T)
  q2 = quantile(x,0.50,na.rm = T)
  q3 = quantile(x,0.75,na.rm = T)
  p95 = quantile(x,0.95,na.rm = T)
  p99 = quantile(x,0.99,na.rm = T)
  uc = m+3*s
  lc = m-3*s
  outliers_flag = max>uc|min<lc
  return(c(nmiss = nmiss,outliers_flag = outliers_flag,mean = m,stdev = s,max = max, min = min,p1 = p1,p5 = p5,p10 = p10,q1=q1,q2=q2,q3=q3,p95=p95,p99=p99,upper_limit = uc,lower_limit = lc))
  
}

# Finding the Descriptive Statistics for data set having Numeric Variables
stats = apply(data[,data_num],2,mystats)
stats = t(stats)
stats = as.data.frame(stats)
View(stats)

write.csv(stats,"stats.csv")
# Defining user defined fuction for outlier treatment
outliers = function(x){
  
  quantiles = quantile(x,c(0.05,0.95),na.rm = T)
  x[x<quantiles[1]] = quantiles[1]
  x[x>quantiles[2]] = quantiles[2]
  x
}

# Treating outliers in the training dataset using the user defined function
data[,data_num] = data.frame(apply(data[,data_num],2,outliers)) 

#Missing Values Treatment

#Count of missing values column-wise
stats[stats$nmiss>1,]


# impute for the missing values using either knn imputation or Mice package

colnames(data[apply(data,2,anyNA)])

library(mice)

data = complete(mice(data,m=1,method = "mean",maxit = 1,exclude = c("CHURNDEP")))

# Checking Significance of a categorical variable
data1 = data
str(data1)

chisqure = function(x){
  y = chisq.test(data1$CHURN,x)
  t = y$p.value
return(t=t)
  }

significance = data.frame(apply(data1[cols],2,chisqure))
significance = round(significance,digits=4)
row.names(significance)[significance$apply.data1.cols...2..chisqure.>=0.05]

#Removing unwanted and Insignificant variables

data1[,c("CALIBRAT","CUSTOMER","RETCALLS","CSA","CHILDREN", "CREDITGY", "CREDITZ","TRUCK", "RV", "OCCPROF", "OCCCLER", 
         "OCCCRFT", "OCCSTUD",  "OCCHMKR" ,"OCCSELF",  "MARRYYES", "MAILFLAG", "TRAVEL","PCOWN" , "NEWCELLN", "MCYCLE", "CREDITAD","REFER","RETCALL","RETACCPT")] = list(NULL)

#EPQDays rounded off to Whole Number

data1$EQPDAYS = round(data1$EQPDAYS,digits = 0)
str(data1)

# Unfactoring the factored Variables
library(varhandle)
data1$CREDITA = unfactor(data1$CREDITA)
data1$CREDITAA = unfactor(data1$CREDITAA)
data1$CREDITB = unfactor(data1$CREDITB)
data1$CREDITC = unfactor(data1$CREDITC)
data1$CREDITDE = unfactor(data1$CREDITDE)
data1$PRIZMRUR = unfactor(data1$PRIZMRUR)
data1$PRIZMUB = unfactor(data1$PRIZMUB)
data1$PRIZMTWN = unfactor(data1$PRIZMTWN)
data1$REFURB  = unfactor(data1$REFURB)
data1$WEBCAP = unfactor(data1$WEBCAP)  
data1$OCCRET = unfactor(data1$OCCRET)
data1$OWNRENT = unfactor(data1$OWNRENT)     
data1$MARRYUN = unfactor(data1$MARRYUN)
data1$MARRYNO = unfactor(data1$MARRYNO)
data1$MAILORD  = unfactor(data1$MAILORD)
data1$MAILRES = unfactor(data1$MAILRES)
data1$CREDITCD = unfactor(data1$CREDITCD)
data1$NEWCELLY = unfactor(data1$NEWCELLY)
data1$INCMISS = unfactor(data1$INCMISS)
data1$SETPRCM = unfactor(data1$SETPRCM)
data1$INCOME = unfactor(data1$INCOME)

# Transform Continous Variables to making the data normally distributed
data1$REVENUE = log(data1$REVENUE)
data1$MOU = log(data1$MOU)
data1$RECCHRGE = log(data1$RECCHRGE)
data1$OVERAGE = log(data1$OVERAGE+1)
data1$OVERAGE = sqrt(data1$OVERAGE)
data1$DROPVCE = log(data1$DROPVCE+1)
data1$BLCKVCE = log(data1$BLCKVCE+1)
data1$UNANSVCE = log(data1$UNANSVCE+1)
data1$MOUREC = log(data1$MOUREC+1)
data1$OUTCALLS = log(data1$OUTCALLS+1)
data1$PEAKVCE = log(data1$PEAKVCE+1)
data1$OPEAKVCE = log(data1$OPEAKVCE+1)
data1$DROPVCE = log(data1$DROPVCE+1)
data1$DROPBLK = log(data1$DROPBLK+1)


# Splitting the data into two parts - 1) Calibration data & 2) Validation data

calibration_data = data1[!is.na(data1$CHURNDEP),]

validation_data = data1[is.na(data1$CHURNDEP),]

# Deleting CHURNDEP column from both Calibration and Validation Dataset
calibration_data[,c("CHURNDEP")] = list(NULL)
validation_data[,c("CHURNDEP")] = list(NULL)


# Model1 Creation using Logistic regression

model = glm(CHURN~.,data = calibration_data,family = "binomial")
summary(model)



#Some Viariables have only single value and is insignificant to the model hence we have to delete these variables

calibration_data[,c("CALLFWDV")] = list(NULL)
validation_data[,c("CALLFWDV")] = list(NULL)

# Model2 Creation using Logistic regression


model1 = glm(CHURN~.,data = calibration_data,family = "binomial")

summary(model1)

# Using StepAIC to select the signifincant variables

library(MASS)
stepAIC(model1,direction = "both")

View(data1)



# Model3 Creation using Logistic regression using significant variables

model2 = glm(formula = CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + 
               CHANGEM + CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + 
               THREEWAY + MOUREC + INCALLS + PEAKVCE + OPEAKVCE + MONTHS + 
               UNIQSUBS + ACTVSUBS + PHONES + EQPDAYS + AGE1 + CREDITAA + 
               CREDITB + CREDITC + CREDITDE + PRIZMUB + REFURB + WEBCAP + 
               MARRYNO + MAILRES + NEWCELLY + INCMISS + INCOME + 
               SETPRC, family = "binomial", data = calibration_data)

summary(model2) 

# Checking for multicollinearity of independent varaibles
car::vif(model2)  

# Model4 Creation using Logistic regression using significant variables with multicollinearity variables eleiminated

model3 = glm(formula = CHURN ~ MOU +  OVERAGE + ROAM + 
               CHANGEM + CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE+
               THREEWAY + INCALLS + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + EQPDAYS + AGE1 +  
               CREDITB + CREDITDE + PRIZMUB + REFURB + WEBCAP + MAILRES + NEWCELLY + SETPRC, family = "binomial", data = calibration_data)



car::vif(model3)
summary(model3)


# Prediction for Calibartion dataset
pred_train_data=predict(model3,calibration_data,type = "response")
calibration_data$pred_CHURN_prob = pred_train_data


# Predictions for Validation Dataset 
pred_test_data=predict(model3,validation_data,type = "response")
validation_data$pred_CHURN_prob = pred_test_data


# decile for Calibration Dataset (can also find the threshold probability according to business domains)
deloc_train = quantile(calibration_data$pred_CHURN_prob,probs = seq(0.1,0.9,by=0.1))
calibration_data$decile = findInterval(calibration_data$pred_CHURN_prob,c(-Inf,deloc_train,Inf))
calibration_data$decile = as.factor(calibration_data$decile)
library(dplyr)
library(varhandle)
calibration_data1$CHURN = unfactor(calibration_data$CHURN)
gr_tr = group_by(calibration_data,decile)
decile_train = summarise(gr_tr,counts = n(),max_prob = max(pred_CHURN_prob),min_prob = min(pred_CHURN_prob),total_default=sum(CHURN),non_default_cnt = counts - total_default)
decile_train  = data.frame(decile_train)
decile_train = arrange(decile_train,desc(decile))
decile_train = mutate(decile_train,Bad_percentage = (total_default/sum(total_default)*100))
decile_train = mutate(decile_train,Bad_cumilative = cumsum(Bad_percentage))
decile_train
write.csv(decile_train,"decile_train.csv")

#decile for Validation Dataset (can also find the threshold probability according to business domains)
deloc_test = quantile(validation_data$pred_CHURN_prob,probs = seq(0.1,0.9,by=0.1))
validation_data$decile = findInterval(validation_data$pred_CHURN_prob,c(-Inf,deloc_train,Inf))
validation_data$decile = as.factor(validation_data$decile)
library(dplyr)
library(varhandle)
validation_data$CHURNDEP = unfactor(validation_data$CHURN)
gr_tr1 = group_by(validation_data,decile)
decile_test = summarise(gr_tr1,counts = n(),max_prob = max(pred_CHURN_prob),min_prob = min(pred_CHURN_prob),total_default=sum(CHURN),non_default_cnt = counts - total_default)
decile_test  = data.frame(decile_test)
decile_test = arrange(decile_test,desc(decile))
decile_test = mutate(decile_test,Bad_percentage = (total_default/sum(total_default)*100))
decile_test = mutate(decile_test,Bad_cumilative = cumsum(Bad_percentage))
decile_test
write.csv(decile_test,"decile_test.csv")

# KS Analysis for Calibration Dataset
KS_train=decile_train
library(dplyr)
KS_train = mutate(KS_train,Bad_percentage = (total_default/sum(total_default)*100))
KS_train = mutate(KS_train,Bad_cumilative = cumsum(Bad_percentage))
KS_train = mutate(KS_train,Good_percentage = (non_default_cnt/sum(non_default_cnt)*100))
KS_train = mutate(KS_train,Good_cumilative = cumsum(Good_percentage))
KS_train = mutate(KS_train,KS = abs((Bad_cumilative - Good_cumilative)))
KS_train
write.csv(KS_train,"ks_train.csv")

# KS Analysis for Validation Dataset
KS_test=decile_test
library(dplyr)
KS_test = mutate(KS_test,Bad_percentage = (total_default/sum(total_default)*100))
KS_test = mutate(KS_test,Bad_cumilative = cumsum(Bad_percentage))
KS_test = mutate(KS_test,Good_percentage = (non_default_cnt/sum(non_default_cnt)*100))
KS_test = mutate(KS_test,Good_cumilative = cumsum(Good_percentage))
KS_test = mutate(KS_test,KS = abs((Bad_cumilative - Good_cumilative)))
KS_test
write.csv(KS_test,"ks_test.csv")
#__________________________________________________________________________________________________________


# Confusion Matrix for Calibration Dataset
calibration_data$pred_CHURN = ifelse(calibration_data$pred_CHURN_prob>=0.45,1,0)
calibration_data$pred_CHURN = as.factor(calibration_data$pred_CHURN)
calibration_data$CHURN = as.factor(calibration_data$CHURN)
library(caret)
confusionMatrix(calibration_data$CHURN,calibration_data$pred_CHURN)



# Confusion Matrix for Validation Dataset
validation_data$pred_CHURN = ifelse(validation_data$pred_CHURN_prob>=0.45,1,0)
validation_data$pred_CHURN = as.factor(validation_data$pred_CHURN)
validation_data$CHURN = as.factor(validation_data$CHURN)
library(caret)
confusionMatrix(validation_data$CHURN,validation_data$pred_CHURN)

#---------------------------------------------------------------------------------------------------------------

# Plotting of ROC Curve, Gain Chart and Lift Chart 

# ROC curve
library(ROCR)
z = prediction(calibration_data$pred_CHURN_prob,calibration_data$CHURN)
i = performance(z,"tpr","fpr")
plot(i)
summary(i)

# Gain Chart

library(CustomerScoringMetrics)

cumGainsChart(calibration_data$pred_CHURN_prob,calibration_data$CHURN)

# Lift Chart

library(CustomerScoringMetrics)

liftChart(calibration_data$pred_CHURN_prob,calibration_data$CHURN)


#AUROC value 
library(InformationValue)

#For Calibration Dataset 
InformationValue::AUROC(calibration_data$CHURN,calibration_data$pred_CHURN_prob)

#For Validation Dataset
InformationValue::AUROC(validation_data$CHURN,validation_data$pred_CHURN_prob)


# SomersD parameter

# For Calibration Dataset
InformationValue::somersD(calibration_data$CHURN,calibration_data$pred_CHURN_prob)

# For Validation Dataset
InformationValue::somersD(validation_data$CHURN,validation_data$pred_CHURN_prob)

# KS plot

# For Calibration Dataset
InformationValue::ks_plot(calibration_data$CHURN,calibration_data$pred_CHURN_prob)
#For Validation Dataset
InformationValue::ks_plot(validation_data$CHURN,validation_data$pred_CHURN_prob)


#-------------------------------------------------------------------------------------------#

# Important variables List

library(dplyr)
impvar = varImp(model3)
important_var = mutate(impvar,variables = row.names(impvar))
top5 = important_var[order(important_var$Overall,decreasing = T)[1:5],]
top5 


# Decision Tree to help in making business decisions 

library(rpart)

calibration_data1 = calibration_data
calibration_data1[,c("pred_CHURN_prob" ,"decile" , "pred_CHURN")] = list(NULL)
decisiontree = rpart(CHURN~.,data = calibration_data1)
library(rpart.plot)
rpart.plot(decisiontree)
