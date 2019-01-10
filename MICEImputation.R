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
imputed1<-mice(df, m = 5, method = "mean", seed = 500)