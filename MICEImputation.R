#------------------SHIP_Feature Random Forest--------------------------
#------------------WRAPPER APPROACH: BORUTA using MICE-----------------
#Dataset : SHIP
#Team members: Mangaraj & Saha
#Task: Apply various modelling algorithm: Here we apply the modeling algorithms
#on all the features of the dataset(no external feature selection method applid)
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
library(Boruta)
#---------------------End of import libraries-----------------------------

#---------------------Begin of loading of Dataset---------------------------

#importing dataset
dataset1 = readRDS('C:/Users/subha/Documents/R/MyRProg/181121_ship.rds')
#converting dataset into dataframe
df<-data.frame(dataset1) 

#keeping only labeled data and discarding rest
df <- df[!is.na(df$liver_fat),]
#chcking the number of rows after unlabeled data is discarded
nrow(df)

#converting target variable to categorical
#anything above 10 is positive and below 10 is negative
for(i in 1:nrow(df)){
  if(df$liver_fat[i]  < 10){
    df$liver_fat[i] <- 0
  }
  else
    df$liver_fat[i] <- 1
}

#converting the numeric type of target variable to factor
df$liver_fat<-as.factor(df$liver_fat)

#cheking structure of target variable
str(df$liver_fat)

#deleting irrelevant columns such as date, time and id
df$blt_beg_s0<-NULL
df$blt_beg_s1<-NULL
df$blt_beg_s2<-NULL
df$exdate_ship_s0<-NULL
df$exdate_ship_s1<-NULL
df$exdate_ship_s2<-NULL
df$zz_nr<-NULL
df$exdate_ship0_s0<-NULL

#converting all integer type variable to numeric
w_int <- which( sapply( df, class ) == 'integer' )
df[w_int] <- lapply( df[w_int], function(x) as.numeric(x) )
str(df)

#-------------------Handling missing values for factor type columns---------------------

#Replacing all missing values with "None"
w_fac <- which( sapply( df, class ) == 'factor' )
df[w_fac] <- lapply( df[w_fac], function(x) 
  ifelse(is.na(x),"none",x))

#After introducing "None", the type changes to charater
#converting it back to factor
w_chr <- which( sapply( df, class ) == 'character' )
df[w_chr] <- lapply( df[w_chr], function(x) as.factor(x) )

#checking structure after handeling missing values of type factor
str(df)

#-------------------Handling missing values for numeric type columns---------------------

#Here we use MICE package to impute the missing values in the dataset
#We take out columns that has missing values more than 5%
#We will deal with them later
#We will impose mice function on the dataset excuding numeric columns having missing
#value more than 5%

#Calculating the number of NA values in a single column
na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
print(na_count)

#Calculating the percentage of NA values 
x <- NROW(df)
y <- (na_count/x) * 100
na_countP<-round(y,2)
View(na_countP)
na_countP<-data.frame(na_countP)

#numerical columns with more than 5% missing values
misscol5<- c("menopaus_w_s0", 
             "quick_s0", 
             "alb_u_s0", 
             "testo_m_s0", 
             "dheas_m_s0", 
             "il6_s0", 
             "ige_s0", 
             "fs_s0", 
             "flag", 
             "menopaus_w_s1",
             "quick_s1", 
             "asat_s_s1",
             "ggt_s_s1", 
             "lip_s_s1", 
             "testo_m_s1",
             "jodid_u_s1", 
             "igf1_s1", 
             "fs_s1",
             "age_ship_s2",
             "menopaus_w_s2",
             "alkligt_s2",
             "sleeph_s2",
             "som_bmi_s2",
             "som_tail_s2",
             "som_huef_s2", 
             "hgb_s2", 
             "hba1c_s2", 
             "quick_s2",
             "fib_cl_s2",
             "crea_s_s2", 
             "hrs_s_s2",
             "gluc_s_s2",
             "asat_s_s2",
             "ggt_s_s2",
             "lip_s_s2", 
             "chol_s_s2",
             "tg_s_s2", 
             "hdl_s_s2",
             "ldl_s_s2",
             "tsh_s2",
             "jodid_u_s2",
             "crea_u_s2",
             "sd_volg_s2",
             "sc_sondercodes_s0" ,
             "lvm_s0",
             "lvmi_s0",
             "onsetsmok_s0",
             "alb_crea_u_s0",
             "menopause_w_s0", 
             "use_mht_w_s0",
             "imt_s0", 
             "crp_s_s0")

#deleting the numerical columns with more than 5% missing values
df<-df[,-which(names(df) %in% misscol5)]
#checking the column size
ncol(df)

#---------------------------MICE Imputation-----------------------------------
imputed1<-mice(df, m = 2, method = "mean", seed = 500)
imputed <- complete(imputed1)
sapply(imputed, function(x) sum(is.na(x)))

#Calculating the number of NA values in a single column
na_count1 <-sapply(imputed, function(y) sum(length(which(is.na(y)))))
na_count1 <- data.frame(na_count1)
print(na_count1)

#Calculating the percentage of NA values 
x <- NROW(imputed)
y <- (na_count1/x) * 100
na_count1<-round(y,2)
View(na_count1)

#validating MICE------

#---------------------

export(imputed, "imputed.rds")
export(imputed, "imputed.csv")

##The mice function imputed almost all values but still there are few columns with missing 
##data. so we put those again into mice function

#checking what all columns still have missing values
w_colnames_miss<-colnames(imputed)[apply(is.na(imputed), 2, any)]

#creating a matrix with the above columns
df1<-imputed[w_colnames_miss]
View(df1)
imputed2<-mice(df1, m = 5, method = "mean", seed = 500)

#creating the imputed matrix
imputed_m2<-complete(imputed2)

#checking what all columns still have missing values
w_colnames_miss1<-colnames(imputed_m2)[apply(is.na(imputed_m2), 2, any)]

#Calculating the number of NA values in a single column
na_count2 <-sapply(imputed_m2, function(y) sum(length(which(is.na(y)))))
na_count2 <- data.frame(na_count2)
print(na_count2)

#Calculating the percentage of NA values 
x <- NROW(imputed_m2)
y <- (na_count2/x) * 100
na_count2<-round(y,2)
View(na_count2)

# assigining new imputed values to main preprocessed table
imputed$age_ship_s1<-imputed_m2$age_ship_s1
imputed$hf_kldb_75_s0<-imputed_m2$hf_kldb_75_s0
imputed$alcg30d_s0<-imputed_m2$alcg30d_s0
imputed$mort_chd_s0<- -99
imputed$mort_ca_s0<- -99
imputed$mort_cvd_s0<- -99

#formaing target variable
dataset1$liver_fat<-as.numeric(dataset1$liver_fat)
df_temp<-dataset1
df_temp <- df_temp[!is.na(df_temp$liver_fat),]
imputed$liver_fat<-df_temp$liver_fat
str(imputed$liver_fat)
for(i in 1:nrow(df)){
  if(imputed$liver_fat[i]  < 10){
    imputed$liver_fat[i] <- 0
  }
  else
    imputed$liver_fat[i] <- 1
}

#converting the numeric type of target variable to factor
imputed$liver_fat<-as.factor(imputed$liver_fat)

#------------------------------Boruta-----------------------------
#importing dataset
imputedf = readRDS('C:/Users/subha/Documents/imputed.rds')

#formaing target variable
dataset1$liver_fat<-as.numeric(dataset1$liver_fat)
df_temp<-dataset1
df_temp <- df_temp[!is.na(df_temp$liver_fat),]
imputedf$liver_fat<-df_temp$liver_fat
str(imputedf$liver_fat)
for(i in 1:nrow(df_temp)){
  if(imputedf$liver_fat[i]  < 10){
    imputedf$liver_fat[i] <- 0
  }
  else
    imputedf$liver_fat[i] <- 1
}

imputedf$mort_chd_s0<- -99
imputedf$mort_ca_s0<- -99
imputedf$mort_cvd_s0<- -99

#converting the numeric type of target variable to factor
imputedf$liver_fat<-as.factor(imputedf$liver_fat)

#checking what all columns still have missing values
w_colnames_miss<-colnames(imputedf)[apply(is.na(imputedf), 2, any)]
#creating a matrix with the above columns
df1<-imputedf[w_colnames_miss]
View(df1)
imputed2<-mice(df1, m = 5, method = "mean", seed = 500)

#creating the imputed matrix
imputed_m2<-complete(imputed2)
# assigining new imputed values to main preprocessed table
imputedf$age_ship_s1<-imputed_m2$age_ship_s1
imputedf$hf_kldb_75_s0<-imputed_m2$hf_kldb_75_s0
imputedf$alcg30d_s0<-imputed_m2$alcg30d_s0

df_bor<-data.frame(imputedf)
table(df_bor$liver_fat)

#applying wrapper method on dataset
set.seed(222)
boruta<- Boruta(liver_fat~., data = df_bor, doTrace = 2, maxRuns = 500)
print(boruta)
plot(boruta)
plotImpHistory(boruta)
getNonRejectedFormula(boruta)
getConfirmedFormula(boruta)

#tentative fix

boruta_ten<-TentativeRoughFix(boruta)
print(boruta_ten)
attStats(boruta)
plotImpHistory(boruta)
#---------------------Begin of Random Forest with Boruta features-------

rf_bor<-randomForest(liver_fat ~ age_ship_s0 + som_bmi_s0 + som_tail_s0 + som_huef_s0 + 
                       hrs_s_s0 + fs_s0 + stea_alt75_s0 + stea_s0 + age_ship_s1 + 
                       menopaus_yn_w_s1 + som_tail_s1 + crea_s_s1 + hrs_s_s1 + age_ship_s2 + 
                       hyperlipid_s2 + gout_s2 + som_bmi_s2 + som_tail_s2 + som_huef_s2 + 
                       hba1c_s2 + crea_s_s2 + hrs_s_s2 + gluc_s_s2 + hdl_s_s2 + 
                       stea_alt75_s2 + stea_s2 + atc_c07ab_s2 + atc_c08ca01_s2 + 
                       age_ship0_s0 + strata2_s0 + lvm_s0 + lvmi_s0 + lvh_s0 + avs_s0 + 
                       mac_s0 + fs_risk_s0 + metsyn_s0 + waistc_s0 + waiidf_s0 + 
                       gfr_mdrd_s0 + imt_s0 + plaque_s0 + stenos_s0 + antihyp_s0 + 
                       mort_time_birth_s0 + som_groe_s2 + som_gew_s2,
                     data = train_rf)

print(rf_bor)

#prediction using boruta features

p_bor<-predict(rf_bor, test_rf)
confusionMatrix(p_bor, test_rf$liver_fat)

#randomforest with getconfirmed formula

rf_bor_con<-randomForest(liver_fat ~ age_ship_s0 + som_bmi_s0 + som_tail_s0 + som_huef_s0 + 
                           hrs_s_s0 + alat_s_s0 + ggt_s_s0 + tg_s_s0 + ferri_s0 + stea_alt75_s0 + 
                           stea_s0 + age_ship_s1 + som_bmi_s1 + som_tail_s1 + som_huef_s1 + 
                           fib_cl_s1 + hrs_s_s1 + tg_s_s1 + hs_crp_s1 + pillnow_w_s2 + 
                           diabetes_s2 + hyperlipid_s2 + knoten_s2 + stea_alt75_s2 + 
                           stea_s2 + atc_c08_s2 + atc_c08ca01_s2 + atc_h02a_s2 + age_ship0_s0 + 
                           diabp_s0 + metsyn_s0 + waistc_s0 + waiidf_s0 + antihyp_s0 + 
                           mort_time_birth_s0 + som_gew_s2,
                         data = train_rf)

print(rf_bor_con)

#predict

p_bor_con<-predict(rf_bor_con, test_rf)
confusionMatrix(p_bor_con, test_rf$liver_fat)
wvar<-varImp(rf_bor_con)
print(wvar)
#---------------------End of Random Forest with Boruta features---------