#------------------SHIP_Filter_Classification------------------------------
#Dataset : SHIP
#Feature Selection process: Filter Method
#Model : Classification (1.KNN 2.Decision Tree)
#Evaluation : Accuracy
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
#---------------------End of import libraries-------------------------------

#---------------------Begin of loading of Dataset---------------------------

dataset1 = readRDS('C:/Users/subha/Documents/R/MyRProg/181121_ship.rds')
df<-data.frame(dataset1)

#colnames(df)[which(names(df) == "liver_fat")] <- "liver_fat_s2"

#taking only the comon columns in all the 3 waves to work with the analysis anlong with target variable

common_vars_3w <- c("atc_c07a_s0","atc_c07a_s1","atc_c07a_s2","atc_c07aa_s0","atc_c07aa_s1","atc_c07aa_s2","atc_c07ab_s0","atc_c07ab_s1","atc_c07ab_s2","atc_c08_s0","atc_c08_s1","atc_c08_s2","atc_c08ca01_s0","atc_c08ca01_s1","atc_c08ca01_s2","atc_c08ca05_s0","atc_c08ca05_s1","atc_c08ca05_s2","atc_c08ca08_s0","atc_c08ca08_s1","atc_c08ca08_s2",
                    "atc_c08da01_s0","atc_c08da01_s1","atc_c08da01_s2","atc_c09aa02_s0","atc_c09aa02_s1","atc_c09aa02_s2","atc_c09aa05_s0","atc_c09aa05_s1","atc_c09aa05_s2","atc_h02a_s0","atc_h02a_s1","atc_h02a_s2","blt_beg_s0","blt_beg_s1","blt_beg_s2","chol_s_s0","chol_s_s1","chol_s_s2","crea_s_s0","crea_s_s1","crea_s_s2","crea_u_s0","crea_u_s1","crea_u_s2","earm_s0","earm_s1","earm_s2","fib_cl_s0","fib_cl_s1","fib_cl_s2","fs_s0","fs_s1","ggt_s_s0","ggt_s_s1","ggt_s_s2","gluc_s_s0","gluc_s_s1","gluc_s_s2","goiter_s0","goiter_s1","goiter_s2","gout_s0","gout_s1","gout_s2","hba1c_s0","hba1c_s1","hba1c_s2","hbs_ag_s0","hdl_s_s1","hdl_s_s2","hdl_s_s0","hgb_s1","hgb_s2","hgb_s0","hrs_s_s1","hrs_s_s2","hs_crp_s0","hs_crp_s1","hyperlipid_s0","hyperlipid_s1","hyperlipid_s2","igf1_s0","igf1_s1","jodid_u_s0","jodid_u_s1","jodid_u_s2","knoten_s0","knoten_s1","knoten_s2","ldl_s_s0","ldl_s_s1","ldl_s_s2","lip_s_s0","lip_s_s1","lip_s_s2","parity_w_s1","parity_w_s2","quick_s0","quick_s1","quick_s2","sd_volg_s0","sd_volg_s1","sd_volg_s2","sleeph_s0","sleeph_s2","sleepp_s0","sleepp_s2","som_bmi_s0","som_bmi_s2","som_huef_s0","som_huef_s2","som_tail_s0","som_tail_s2","stea_alt75_s0","stea_alt75_s2","stea_s0","stea_s2","tsh_s0","tsh_s1","tsh_s2","age_ship_s0","age_ship_s1","age_ship_s2","exdate_ship_s0","exdate_ship_s1","exdate_ship_s2","parity_w_s0","parity_w_s1","parity_w_s2","menopaus_yn_w_s0","menopaus_yn_w_s1","menopaus_yn_w_s2","menopaus_w_s0","menopaus_w_s1","menopaus_w_s2","fruehgeb_w_s0","fruehgeb_w_s2","hormonrepl_w_s0","hormonrepl_w_s1","hormonrepl_w_s2","pillever_w_s0","pillever_w_s2","pillnow_w_s0","pillnow_w_s2","abstain_s0","abstain_s1","abstain_s2","smoking_s0","smoking_s1","smoking_s2","diabetes_s0","diabetes_s1","diabetes_s2","alkligt_s0","alkligt_s1","alkligt_s2","asat_s_s0","asat_s_s1","asat_s_s2","tg_s_s0","tg_s_s1","tg_s_s2","liver_fat")

df_subset_3waves_common <- df[common_vars_3w]
str(df_subset_3waves_common)
export(df_subset_3waves_common, "df_subset_3waves_common.csv")
#---------------------End of loading of Dataset-----------------------------

#---------------------Begin of Preprocessing of Dataset for CFS-------------

#for CFS method we need the correlation matrix
#the correlation matrix only takes numeric matrix as input
#so we need to convert the dataset into numeric one

#STEP1 : Deleting irrelavant columns such as date and time

str(df_subset_3waves_common$blt_beg_s0)
df_subset_3waves_common$blt_beg_s0<-NULL

str(df_subset_3waves_common$blt_beg_s1)
df_subset_3waves_common$blt_beg_s1<-NULL

str(df_subset_3waves_common$blt_beg_s2)
df_subset_3waves_common$blt_beg_s2<-NULL

str(df_subset_3waves_common$exdate_ship_s0)
df_subset_3waves_common$exdate_ship_s0<-NULL

str(df_subset_3waves_common$exdate_ship_s1)
df_subset_3waves_common$exdate_ship_s1<-NULL

str(df_subset_3waves_common$exdate_ship_s2)
df_subset_3waves_common$exdate_ship_s2<-NULL

#STEP2: Converting categorical values to numeric(stea_alt75_s0,stea_alt75_s2,stea_s0,stea_s2)

df_subset_3waves_common$stea_alt75_s0<-unfactor(df_subset_3waves_common$stea_alt75_s0)
str(df_subset_3waves_common$stea_alt75_s0)
df_subset_3waves_common$stea_alt75_s0<-as.numeric(factor(df_subset_3waves_common$stea_alt75_s0))

df_subset_3waves_common$stea_alt75_s2<-unfactor(df_subset_3waves_common$stea_alt75_s2)
str(df_subset_3waves_common$stea_alt75_s2)
df_subset_3waves_common$stea_alt75_s2<-as.numeric(factor(df_subset_3waves_common$stea_alt75_s2))

df_subset_3waves_common$stea_s0<-unfactor(df_subset_3waves_common$stea_s0)
str(df_subset_3waves_common$stea_s0)
df_subset_3waves_common$stea_s0<-as.numeric(factor(df_subset_3waves_common$stea_s0))

df_subset_3waves_common$stea_s2<-unfactor(df_subset_3waves_common$stea_s2)
str(df_subset_3waves_common$stea_s2)
df_subset_3waves_common$stea_s2<-as.numeric(factor(df_subset_3waves_common$stea_s2))

#STEP3 : Converting all factor types to numeric

#Viewing all the factor type columns
fac_vars <- sapply(df_subset_3waves_common, is.factor)
fac_vars_nam <- names(df_subset_3waves_common[, fac_vars])
View(fac_vars_nam)

#converting factor to numeric

w_fac <- which( sapply( df_subset_3waves_common, class ) == 'factor' )
df_subset_3waves_common[w_fac] <- lapply( df_subset_3waves_common[w_fac], function(x) as.numeric(as.character(x)) )
str(df_subset_3waves_common)

#STEP4: Converting all integer columns to numeric

w_int <- which( sapply( df_subset_3waves_common, class ) == 'integer' )
df_subset_3waves_common[w_int] <- lapply( df_subset_3waves_common[w_int], function(x) as.numeric(x) )
str(df_subset_3waves_common)

export(df_subset_3waves_common, "df_subset_3waves_common_pp.csv")
#---------------------End of Preprocessing of Dataset for CFS---------------

#---------------------Begin of Filter Method1: CFS--------------------------
#Correlation-based Feature Selection, CFS

df_temp<-df_subset_3waves_common


#Deleting unlabeled data from the dataset
df_temp <- df_temp[!is.na(df_temp$liver_fat),]
NROW(df_temp)
export(df_temp, "df_temp_labeleddata.csv")

#Applying positive and Negtive labels to class variable(0 <- negaive, 1 <- positive)
View(df_temp$liver_fat)

for(i in 1:nrow(df_temp)){
  if(df_temp$liver_fat[i]  < 10){
    df_temp$liver_fat[i] <- 0
  }
  else
    df_temp$liver_fat[i] <- 1
}
df_temp_cor<-df_temp

#converting class label to factor type for CFS
df_temp[,ncol(df_temp)]<-as.factor(df_temp[,ncol(df_temp)])
View(df_temp$liver_fat)

#Applying feature selection : CFS



cfs(df_temp)
subset <- select.cfs(df_temp)
f <- as.simple.formula(df_temp, "Liverfat")
print(names(f))
subset


#Applying feature selection : Correlation based feature selection using "caret"

#data frame with label as numeric
str(df_temp_cor$liver_fat)
corr_matrix_s<-cor(df_temp_cor, use = "pairwise.complete.obs", method = "spearman")
corr_matrix_p<-cor(df_temp_cor, use = "pairwise.complete.obs", method = "pearson")

#the correlation matric has null values. so we put 100 in place of numm lavues coz 
#we dont want them to taken into consideration

corr_matrix_s_null=as.matrix(corr_matrix_s)
corr_matrix_s_null[is.na(corr_matrix_s_null)] <-100
corr_matrix_s_null=as.data.frame(corr_matrix_s_null)
export(corr_matrix_s_null, "corr_matrix_s_null.csv")

highlyCorrelated_s<-findCorrelation(corr_matrix_s_null, cutoff = 0.5, verbose = FALSE, names = FALSE)
print(highlyCorrelated_s)

corrplot(cor(df_temp_cor[,-highlyCorrelated_s]), type = 'lower')

dataset_cor_fs<-select(df_temp_cor,-highlyCorrelated_s)
colnames(dataset_cor_fs)
ncol(df_temp)
str(df_temp)
#----------------------------Wrapper: featureSelecter-----------------------

df_temp_w<-df_temp
df_temp_w$liver_fat<-as.numeric(as.character(df_temp_w$liver_fat))

selectFeature <- function(train, test, cls.train, cls.test, features) {
  ## identify a feature to be selected
  current.best.accuracy <- -Inf #nagtive infinity
  selected.i <- NULL
  for(i in 1:ncol(train)) {
    current.f <- colnames(train)[i]
    if(!current.f %in% features) {
      model <- knn(train=train[,c(features, current.f)],      test=test[,c(features, current.f)], cl=cls.train, k=3)
      test.acc <- sum(model == cls.test) / length(cls.test)
      
      if(test.acc > current.best.accuracy) {
        current.best.accuracy <- test.acc
        selected.i <- colnames(train)[i]
      }
    }
  }
  return(selected.i)
}


##
library(caret)
set.seed(1)
set.seed(1)
inTrain <- createDataPartition(df_temp_w$liver_fat, p = .6)[[1]]
allFeatures <- colnames(df_temp_w)[-158]
train <- df_temp_w[ inTrain,-158]
test  <- df_temp_w[-inTrain,-158]
cls.train <- df_temp_w$liver_fat[inTrain]
cls.test <- df_temp_w$liver_fat[-inTrain]

# use correlation to determine the first feature
cls.train.numeric <- rep(c(0, 1), c(sum(cls.train == "R"),   sum(cls.train == "M")))
features <- c()
current.best.cor <- 0
for(i in 1:ncol(train[,-158])) {
  if(current.best.cor < abs(cor(train[,i], cls.train.numeric))) {
    current.best.cor <- abs(cor(train[,i], cls.train.numeric))
    features <- colnames(train)[i]
  }
}
print(features)

# select the 2 to 10 best features using knn as a wrapper classifier
for (j in 2:10) {
  selected.i <- selectFeature(train, test, cls.train, cls.test, features)
  print(selected.i)
  
  # add the best feature from current run
  features <- c(features, selected.i)
}

#---------------------End of Filter Method1: CFS----------------------------


#---------------------Begin of Modelling Algo1: Linear Regression-----------

#dividing data into training and test data (80% is training and 20% is test)

dataset_cor_fs["liver_fat"]<- NA
dataset_cor_fs$liver_fat<-df_temp$liver_fat
str(dataset_cor_fs$liver_fat)
dataset_cor_fs$liver_fat<-as.character(dataset_cor_fs$liver_fat)
dataset_cor_fs$liver_fat<-as.numeric(dataset_cor_fs$liver_fat)

row.number <- sample(1:nrow(dataset_cor_fs), 0.8*nrow(dataset_cor_fs))
train = dataset_cor_fs[row.number,]
test = dataset_cor_fs[-row.number,]
dim(train)
dim(test)

#Explore the data.
ggplot(train, aes(liver_fat)) + geom_density(fill="blue")

#default model
model1 = lm((liver_fat)~., data=train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

pred1 <- predict(model1, newdata = test)
rmse <- sqrt(sum((exp(pred1) - test$liver_fat)^2)/length(test$liver_fat))
c(RMSE = rmse, R2=summary(model1)$r.squared)

par(mfrow=c(1,1))
plot(test$liver_fat, exp(pred1))



#----------------------------------------------------------------------------
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
  
#---------------------Begin of Modelling Algo1: Decision Tree----------------

#---------------------Decision Tree with all the features--------------------

#data with all int and and target as factor
df_data_dt<-df_temp

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

data_random = readRDS('C:/Users/subha/Documents/df_merge.rds')

#data with all int and and target as factor
df_data_rf<-data_random
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

data_knn = readRDS('C:/Users/subha/Documents/df_merge.rds')

#data with all int and and target as factor
df_data_knn<-data_knn
table(df_data_knn$liver_fat)

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