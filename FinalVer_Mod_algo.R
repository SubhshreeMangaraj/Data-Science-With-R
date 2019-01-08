#------------------SHIP_Modelling_Algo--------------------------------------
#Dataset : SHIP
#Team members: Mangaraj & Saha
#Task: Apply various modelling algorithm
#---------------------------------------------------------------------------

#---------------------Begin of import libraries-----------------------------
library(naniar)
library(dplyr)
library(imputeTS)
library(rio)
library(lattice)
library(survival)
library(ggplot2)
library(Hmisc)
library(mice)
library(colorspace)
library(grid)
library(data.table)
library(VIM) #requires the above 3 libraries to be loaded
library(iterators)
library(itertools)
library(missForest)
library(randomForest)
library(GGally)
library(purrr)
library(varhandle)
library(readxl)
library(csvy)
library(feather)
library(fst)
library(hexView)
library(rmatio)
library(Biocomb)
library(gtools)
library(Rcpp)
library(FSelector)
library(caret)
library(mlbench)
library(corrplot)
library(MASS)
library(party)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(mice)
library(backports)
#---------------------End of import libraries--------------------------------

#---------------------Begin of Modelling Algo1: Decision Tree----------------

#---------------------Decision Tree with all the features--------------------

#import preprocessed data
dataset1 = readRDS('C:/Users/subha/Documents/allnumeric.rds')

#data with all int and and target as factor
df_data_dt<-data.frame(dataset1)

df_data_dt$liver_fat<-as.factor(df_data_dt$liver_fat)

#partioning data into training and test set
row.number <- sample(1:nrow(df_data_dt), 0.8*nrow(df_data_dt))
train_dt = df_data_dt[row.number,]
test_dt = df_data_dt[-row.number,]
dim(train_dt)
dim(test_dt)

#Decision tree using "party package"
#Train
dt1<-ctree(liver_fat~., data = train_dt)
dt1
plot(dt1)
#with controls
dt1<-ctree(liver_fat~., data = train_dt, controls = ctree_control(mincriterion = 0.9, minsplit = 100))
dt1
plot(dt1)

#predict
predict(dt1,test_dt,type="prob")
predict(dt1,test_dt)

#Decision tree with "rpart package"
dt1_rpart<-rpart(liver_fat~.,train_dt)
rpart.plot(dt1_rpart)
rpart.plot(dt1_rpart,extra = 1)
rpart.plot(dt1_rpart,extra = 2)
rpart.plot(dt1_rpart,extra = 4)

#predict
predict(dt1_rpart,test_dt,type="prob")
predict(dt1_rpart,test_dt)

#Misclassification error for train data for party package
mis_err_train<-table(predict(dt1),train_dt$liver_fat)
print(mis_err_train)
1-sum(diag(mis_err_train))/sum(mis_err_train)#misclassification rate

#Misclassification error for test data party package
test_pred_party<-predict(dt1,newdata=test_dt)
mis_err_test<-table(test_pred_party, test_dt$liver_fat)
print(mis_err_test)
1-sum(diag(mis_err_test))/sum(mis_err_test)#misclassification rate

#---------------------End of Modelling Algo1: Decision Tree------------------

#---------------------Begin of Modelling Algo1: Random Forest----------------

#import data

data_random = readRDS('C:/Users/subha/Documents/allfactor.rds')

#data with all int and and target as factor
df_data_rf<-data.frame(data_random)
df_data_rf$liver_fat<-as.factor(df_data_rf$liver_fat)
table(df_data_rf$liver_fat)

#Data Partition
set.seed(123)
row.number <- sample(1:nrow(df_data_rf), 0.7*nrow(df_data_rf))
train_rf = df_data_rf[row.number,]
test_rf= df_data_rf[-row.number,]
dim(train_rf)
dim(test_rf)

View(test_rf)

#Random Forest
rf1<- randomForest(liver_fat~.,data = train_rf)
print(rf1)
attributes(rf1)
rf1$confusion

#prediction
p1_rf<-predict(rf1,test_rf)
p2_rf<-predict(rf1,train_rf)
print(p1_rf)
head(p1_rf)

library(e1071)
#confusion matrix
confusionMatrix(p2_rf,train_rf$liver_fat)
confusionMatrix(p1_rf,test_rf$liver_fat)

#error rate for random forest
plot(rf1)

#tuning of random forest model
ncol(train_rf)
t<-tuneRF(train_rf[-158], train_rf[158],
          stepFactor = 0.5,
          plot = TRUE,
          ntreeTry = 300,
          trace = TRUE,
          improve = 0.05)

hist(treesize(rf1),
     main = "No of nodes of tree",
     col = "blue")

#variable importance
varImpPlot(rf1,
           sort = T,
           n.var = 10,
           main = "top 10 var imp"
)
importance(rf1)
varUsed(rf1)

#partial dependent plot
partialPlot(rf1, train_rf, stea_alt75_s2, "1")

#extract single tree from forest
getTree(rf1, 1 , labelVar = TRUE)

#multi dimensional scaling plot of proximity matrix
MDSplot(rf1, train_rf$liver_fat)
#---------------------End of Modelling Algo1: Random Forest------------------


#---------------------Begin of Modelling Algo1: KNN--------------------------

#import data

data_knn = readRDS('C:/Users/subha/Documents/allfactor.rds')

#data with all int and and target as factor
df_data_knn<-data.frame(data_knn)
table(df_data_knn$liver_fat)
df_data_knn$liver_fat<-as.factor(df_data_knn$liver_fat)

#Data Partition
set.seed(123)
row.number <- sample(1:nrow(df_data_knn), 0.7*nrow(df_data_knn))
train_knn = df_data_knn[row.number,]
test_knn= df_data_knn[-row.number,]
dim(train_knn)
dim(test_knn)

#KNN
library(caret)
library(lattice)
library(ggplot2)
knn_control<-trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)

set.seed(123)
knn_fit<-train(liver_fat~.,
               data = train_knn,
               method = "knn",
               tuneLength = 20,
               trControl = knn_control)

#Model performance
knn_fit

plot(knn_fit)

varImp(knn_fit)

#prediction
pred_knn<-predict(knn_fit, newdata = test_knn)
confusionMatrix(pred_knn, test_knn$liver_fat)
#---------------------End of Modelling Algo1: KNN----------------------------

#---------------------Begin of Modelling Algo1: Linear Regression------------
#import data

data_linreg = readRDS('C:/Users/subha/Documents/df_merge.rds')

str(data_linreg)

row.number <- sample(1:nrow(data_linreg), 0.8*nrow(data_linreg))
train_lin = data_linreg[row.number,]
test_lin = data_linreg[-row.number,]
dim(train_lin)
dim(test_lin)

#Explore the data.
ggplot(train_lin, aes(liver_fat)) + geom_density(fill="blue")

#default model
model1 = lm((liver_fat)~., data=train_lin)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

out_linreg<-lm(liver_fat~.,data = data_linreg)
out_linreg

#---------------------End of Modelling Algo1: Linear Regression--------------