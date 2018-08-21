#Set wroking Directory
setwd("D:/Edwisor#2/Project#2")
getwd()


#Load library
library(readxl)
library(dplyr)
library(ggplot2)
library(DMwR)
library(corrgram)
library(rpart)
library(usdm)
library(MLmetrics)
library(randomForest)
library(caret)

#Load CSV
bike_data = read.csv("day.csv")

#check the summary and distribution of the data
summary(bike_data)
str(bike_data)

#Convert desire variable in factor
bike_data$dteday = as.Date(bike_data$dteday)
bike_data$yr     = as.factor(bike_data$yr)
bike_data$workingday = as.factor(bike_data$workingday)
bike_data$holiday = as.factor(bike_data$holiday)

#QQ graph for the target variable price
qqnorm(bike_data$cnt)

#Histogram of the dependent variable
ggplot(data = bike_data,aes(bike_data$cnt)) + geom_histogram(bins=30)

#check the distribution for few of continuous variable
qqnorm(bike_data$temp)
hist(bike_data$temp)

qqnorm(bike_data$windspeed)
hist(bike_data$windspeed)

qqnorm(bike_data$registered)
hist(bike_data$registered)

#create dummy variables

bike_data$season_spring <-  as.factor(ifelse((bike_data$season == 1 ),1,0))
bike_data$season_summer <-  as.factor(ifelse((bike_data$season == 2 ),1,0))
bike_data$season_fall   <-  as.factor(ifelse((bike_data$season == 3 ),1,0))
bike_data$season_winter <-  as.factor(ifelse((bike_data$season == 4 ),1,0))


bike_data$mnth_jan <-  as.factor(ifelse((bike_data$mnth == 1 ),1,0))
bike_data$mnth_feb <-  as.factor(ifelse((bike_data$mnth == 2 ),1,0))
bike_data$mnth_mar <-  as.factor(ifelse((bike_data$mnth == 3 ),1,0))
bike_data$mnth_apr <-  as.factor(ifelse((bike_data$mnth == 4 ),1,0))
bike_data$mnth_may <-  as.factor(ifelse((bike_data$mnth == 5 ),1,0))
bike_data$mnth_june <-  as.factor(ifelse((bike_data$mnth == 6 ),1,0))
bike_data$mnth_july <-  as.factor(ifelse((bike_data$mnth == 7 ),1,0))
bike_data$mnth_aug <-  as.factor(ifelse((bike_data$mnth == 8 ),1,0))
bike_data$mnth_sep <-  as.factor(ifelse((bike_data$mnth == 9 ),1,0))
bike_data$mnth_oct <-  as.factor(ifelse((bike_data$mnth == 10 ),1,0))
bike_data$mnth_nov <-  as.factor(ifelse((bike_data$mnth == 11 ),1,0))
bike_data$mnth_dec <-  as.factor(ifelse((bike_data$mnth == 12 ),1,0))

bike_data$weekday_sun <-  as.factor(ifelse((bike_data$weekday == 0 ),1,0))
bike_data$weekday_mon <-  as.factor(ifelse((bike_data$weekday == 1 ),1,0))
bike_data$weekday_tues <-  as.factor(ifelse((bike_data$weekday == 2 ),1,0))
bike_data$weekday_wed <-  as.factor(ifelse((bike_data$weekday == 3 ),1,0))
bike_data$weekday_thus <-  as.factor(ifelse((bike_data$weekday == 4 ),1,0))
bike_data$weekday_fri <-  as.factor(ifelse((bike_data$weekday == 5 ),1,0))
bike_data$weekday_sat <-  as.factor(ifelse((bike_data$weekday == 6 ),1,0))

bike_data$weather_clear <- as.factor(ifelse((bike_data$weathersit == 1),1,0))
bike_data$weather_mist  <- as.factor(ifelse((bike_data$weathersit == 2),1,0))
bike_data$weather_snow  <- as.factor(ifelse((bike_data$weathersit == 3),1,0))


#Remove the varible for which dummy variables are created
bike_data <- bike_data[, setdiff(names(bike_data), c('dteday','instant','season',
                                                     'mnth','weekday','weathersit'))]


#Numeric data
Numeric_index = sapply(bike_data,is.numeric)
Numeric_data = bike_data[,Numeric_index]
num_names = colnames(Numeric_data)

#categorical data
categorical_data = bike_data[,!Numeric_index]
cate_name = colnames(categorical_data)


#Plot boxplot for outlier detection

#For temp
ggplot(data = bike_data,aes_string(x= "cnt",y= num_names[1]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "cnt",y = num_names[1] )+
  ggtitle(paste("BoxPlot for",num_names[1]))

#For atemp
ggplot(data = bike_data,aes_string(x= "cnt",y= num_names[2]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "cnt",y = num_names[2] )+
  ggtitle(paste("BoxPlot for",num_names[2]))

#For hum
ggplot(data = bike_data,aes_string(x= "cnt",y= num_names[3]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "cnt",y = num_names[3] )+
  ggtitle(paste("BoxPlot for",num_names[3]))

#For windspeed
ggplot(data = bike_data,aes_string(x= "cnt",y= num_names[4]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "cnt",y = num_names[4] )+
  ggtitle(paste("BoxPlot for",num_names[4]))

#For casual
ggplot(data = bike_data,aes_string(x= "cnt",y= num_names[5]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "cnt",y = num_names[5] )+
  ggtitle(paste("BoxPlot for",num_names[5]))

#For registered
ggplot(data = bike_data,aes_string(x= "cnt",y= num_names[6]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "cnt",y = num_names[6] )+
  ggtitle(paste("BoxPlot for",num_names[6]))


#So we have found out that the outliers are present in the below variables
#hum,windspeed,casual
#now we have to remove the outliers

#dataset got convert into tibble and it will return matrix which boxplot.stats cannot use
#so convert it back to data frame

#As the dataset is very small so instead of deleting the outlier we will replace it
#with NA

for (i in num_names){
  print(i)
  index = bike_data[,i][bike_data[,i] %in% boxplot.stats(bike_data[,i])$out]
  #Replace the outlier with the NA
  bike_data[,i][bike_data[,i] %in% index] = NA
}


#Find out the number of missing valuein each variable
missing_value = data.frame(apply(bike_data,2,function(x){sum(is.na(x))}))

#Impute the missing value using knn imputation
bike_data = knnImputation(bike_data,k = 5)

#Find out the correlation between independent variable and dependent variable
corrgram(bike_data[,Numeric_index],order = FALSE,upper.panel = panel.pie , text.panel = panel.txt,
         main = "correlation plot") 

#As we have seen from the correlation plot that wieght and body mass index in highing positive correlated
#we have drop any one of the variable.

bike_data <- bike_data[, setdiff(names(bike_data), c('atemp'))]


#Rearrange the column name, Make the dependent variables as first column
#let me save my dependent variable in different dateframe
dep_var = as.data.frame(bike_data[,c('cnt')])
colnames(dep_var)[1] = "Bike_Count"

#Remove the dependent variable from the dataset.
bike_data = bike_data[,setdiff(names(bike_data), c('cnt'))]

#Join the dep_var dataset and bike_data
bike_data = cbind(data.frame(dep_var,bike_data))


#Divide the data in training and test sample

# 70% of the sample size
smp_size <- floor(0.70 * nrow(bike_data))

# set the seed
set.seed(123)
Train_Index <- sample(seq_len(nrow(bike_data)), size = smp_size)

train <- bike_data[Train_Index, ]
test <- bike_data[-Train_Index, ]

#As we have done our data pre processing 
#Create Models

#***************************************************************************
#Linear regression model
lm_model = lm(Bike_Count ~. , data = train)
summary(lm_model)

#Make predictions
prediction_lm = predict(lm_model,test[,-1])

MAPE(test[,1],prediction_lm)

######################### improve ########
lm_model_improve = lm(Bike_Count ~ workingday+temp+casual+registered
                  +weekday_sun, data = train)
summary(lm_model_improve)

#Make predictions
prediction_lm_improve = predict(lm_model_improve,test[,-1])

MAPE(test[,1],prediction_lm_improve)
#0.077062
##########################  CROSS VALIDATION #########################################

# NOw we do the cross validation to check whether our model is overfit or underfit
#first will pridiction the dependent variable using training data set and then calculate the 
#difference between actual and predicted.

#Repeat the same with test data set

#First we will take create model


lm_model_cv = lm(Bike_Count ~ workingday+temp+casual+registered
                 +weekday_sun , data = train)


#Make predictions with train data
prediction_lm_cv = predict(lm_model_cv,train[,-1])

#Calculate MAPE for train data set
MAPE(train[,1],prediction_lm_cv)
#ERROR : 7%


#Make predictions with test data
prediction_lm_cv_test = predict(lm_model_cv,test[,-1])

#Calculate MAPE for test data set
MAPE(test[,1],prediction_lm_cv_test)
#ERROR : 8%

#From above we have seen that the error for training is less but the model is fail to 
# Generalize the new examples
#this might be the case of overfitting(variance).

#Now we will split the data into 60.40 ratio of train and testing data set

# 90% of the sample size
smp_size1 <- floor(0.6 * nrow(bike_data))

# set the seed
set.seed(123)
Train_Index1 <- sample(seq_len(nrow(bike_data)), size = smp_size1)

train_cv <- bike_data[Train_Index1, ]
test_cv <- bike_data[-Train_Index1, ]

lm_model_cv_train = lm(Bike_Count ~ workingday+temp+casual+registered
                       +weekday_sun , data = train_cv)
#Make predictions
prediction_lm_new = predict(lm_model_cv_train,test_cv)

MAPE(test_cv[,1],prediction_lm_new)

#Now we have seen that the MAPE is reduced
#MAPE ERROR : 3.4%
#ACCURACY : 96.6%

#create a new data set
Predicted_Bike_Rent = data.frame("Predicted_Bike_Rent" = prediction_lm_new)

#we will create a small data set with predicted hours and actual hours
Predicted_dataset = cbind(data.frame(Predicted_Bike_Rent,test_cv))

#Plot residual vs Fitted plot
plot(lm_model_cv_train)

######################################RANDOM FOREST####################################

# set the seed
set.seed(123)

rf = randomForest(Bike_Count ~., data = train)
print(rf)
attributes(rf)

rf_predict_test = predict(rf,test[,-1])
MAPE(test[,1],rf_predict_test)

#Above we have calculated the MAPE with training data set and testing data set 
#with training dataset error rate will be less because data is already seen by the model
#but when we calculated the error with the data which is not seen by the model(Also call OOB sample)
#there is difference between the error and it is more accurate result.

#Now we plot the graph of the error
plot(rf)

#from above plot we have abserved that the as the number of tree grows the oob error intially drop down 
#and then became constant. We are not able to improve the error after 100 trees.so we tune the model

#tune mtry

tune_mtry = tuneRF(train[,-1],train[,1],
                   stepFactor = 0.5,
                   plot = TRUE,
                   ntreeTry = 250,
                   trace = TRUE,
                   improve = 0.5)

#stepFactor = at each step mtry will be inflated of deflated by this value
#plot = whether to plot oob error as a function of mtry
#ntreetry = number of tree that we want to try i.e 100
#improve = the relative improvement in the oob error must be by this much(value) for search to continue

# so we have seen the oob error is relatively very high when mtry is 5 and minimum when mtry is 15
#let create the rf model again with ntree = 300 and mtry = 22

rf_improve = randomForest(Bike_Count ~., data = train,
                          ntree = 250,
                          mtry = 22,
                          importance = TRUE,
                          proximity = TRUE)
print(rf_improve)

rf_improve_predict = predict(rf_improve,test[,-1])

#Calculate Mean absolute percentage error
MAPE(test[,1],rf_improve_predict)

#create a new data set
Predicted_absent_hours_rf = data.frame("Predicted_bike_rent" = rf_improve_predict)

#we will create a small data set with predicted hours and actual hours
Predicted_dataset_rf = cbind(data.frame(Predicted_absent_hours_rf,test))

#now we have see the actual error is reduce 
#MAPE is 3%
#Accuracy is 97%

#now we will make histogram of number of the nodes for the tree size
hist(treesize(rf_improve), main = "Number of the nodes for tree", col="red")

#so we have seen that some tree which is having 195 node and there are few which are having
# 160 node but majority of the trees has close to 175 nodes and frequeny close to 100.

#variable importance 
varImpPlot(rf_improve)

#there will be graph first will tell if we remove one variable what will be the increase in mean square error
#second graph will capture the gini and it tell us that how pure the node is at the end of the tree if we 
#remove significat variable it show how much on an average gini decreases.
varImpPlot(rf_improve,sort = TRUE,n.var = 15, main = "Top-15-important variable")

#Actual mse and gini value of each variable
importance(rf_improve)

#how many time Variable used in the random forest model.
varUsed(rf_improve)

#Not find the table for the first random forest tree
#Note for terminal node we don't have the left and right daughter
getTree(rf_improve,1,labelVar = TRUE)
