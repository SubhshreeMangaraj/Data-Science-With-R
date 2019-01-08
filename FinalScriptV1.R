#------------------SHIP_EDA_Preprocessing-----------------------------------
#Dataset : SHIP
#Team members: Mangaraj & Saha
#Final Script V1
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
#---------------------End of import libraries-------------------------------

#---------------------Begin of loading of Dataset---------------------------

dataset1 = readRDS('C:/Users/subha/Documents/R/MyRProg/181121_ship.rds')
df_temp<-data.frame(dataset1)
df<-data.frame(dataset1)

#General explorations
View(df) 
names(df)   #variable names
dim(df)     #number of rows and columns
str(df)     #structure of data
summary(df) #summary of the data
class(df)   #type of data
head(df)
tail(df)

#Segregating labeled data

df <- df[!is.na(df$liver_fat),]
NROW(df)

df_temp <- df_temp[!is.na(df_temp$liver_fat),]
NROW(df_temp)

#Histogram

hist(df$liver_fat,
     main = "Histogram~Liver fat",
     xlab = "Liver fat",
     col = "orange")

#Box plot

boxplot(df$age_ship_s0,
        main = toupper("Boxplot of Age"),
        ylab = "Age in years",
        col = "magenta")

boxplot(df$age_ship_s0~df$liver_fat,col="Yellow")

#-----------------Begin of: Missing values analysis-----------------------------

#listing names of columns with missing values
list_na<-colnames(df)[apply(df, 2, anyNA)]
list(list_na)

#Calculating the number of NA values in a single column
na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
print(na_count)

#Calculating the percentage of NA values 
x <- NROW(df)
y <- (na_count/x) * 100
na_countP<-round(y,2)
print(na_countP)
na_countP<-data.frame(na_countP)

#Checking the feature if there is more than 5% values, missing
for(i in 1:400){
  temp<-df[,i]


  break
}

#Visualising missing data
#md.pattern(df)
str(df)
# Create the loop.vector (all the columns)
#par(mfrow= c(2,2))
#loop.vector <- 1:4

#for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x
 # x <- df[,i]
  
  # Plot histogram of x
  #hist(x,
   #    main = paste("Question", i),
   #    xlab = "Scores",
   #    xlim = c(0, 100))
#}
#-----------------End of: Missing values analysis-------------------------------

#-----------------Begin of: Preprocessing---------------------------------------

#Phase1: Dataframe containing only numeric values : df_n

#for CFS method we need the correlation matrix
#the correlation matrix only takes numeric matrix as input
#so we need to convert the dataset into numeric one

df_n<-df
str(df_n$stea_alt75_s0)
View(colnames(df_n))
export(df_n, "df_n.csv")
#STEP1 : Deleting irrelavant columns such as date and time and id

df_n$blt_beg_s0<-NULL
df_n$blt_beg_s1<-NULL
df_n$blt_beg_s2<-NULL
df_n$exdate_ship_s0<-NULL
df_n$exdate_ship_s1<-NULL
df_n$exdate_ship_s2<-NULL
df_n$zz_nr<-NULL
df_n$exdate_ship0_s0<-NULL

#STEP2: Converting categorical(non-binary) values to numeric(stea_alt75_s0,stea_alt75_s2,stea_s0,stea_s2)
df_n$stea_alt75_s0<-unfactor(df_n$stea_alt75_s0)
df_n$stea_alt75_s0<-as.numeric(factor(df_n$stea_alt75_s0))

df_n$stea_alt75_s2<-unfactor(df_n$stea_alt75_s2)
df_n$stea_alt75_s2<-as.numeric(factor(df_n$stea_alt75_s2))

df_n$stea_s0<-unfactor(df_n$stea_s0)
df_n$stea_s0<-as.numeric(factor(df_n$stea_s0))

df_n$stea_s2<-unfactor(df_n$stea_s2)
df_n$stea_s2<-as.numeric(factor(df_n$stea_s2))

df_n$mort_icd10_s0<-unfactor(df_n$mort_icd10_s0)
df_n$mort_icd10_s0<-as.numeric(factor(df_n$mort_icd10_s0))

str(df_n$sc_sondercodes_s0)
df_n$sc_sondercodes_s0<-as.numeric(df_n$sc_sondercodes_s0)

#STEP3 : Converting all factor types to numeric
w_fac <- which( sapply( df_n, class ) == 'factor' )
df_n[w_fac] <- lapply( df_n[w_fac], function(x) as.numeric(as.character(x)) )
str(df_n)

#STEP4: Converting all integer columns to numeric
w_int <- which( sapply( df_n, class ) == 'integer' )
df_n[w_int] <- lapply( df_n[w_int], function(x) as.numeric(x) )
str(df_n)

#step5:Applying positive and Negtive labels to class variable(0 <- negaive, 1 <- positive)
for(i in 1:nrow(df_n)){
  if(df_n$liver_fat[i]  < 10){
    df_n$liver_fat[i] <- 0
  }
  else
    df_n$liver_fat[i] <- 1
}
export(df_n, "allnumeric.rds")
#corr<-cor(df_n, use = "pairwise.complete.obs", method = "spearman")

#----------------------------------------------------------------------

#Phase2: Dataframe containing numeric, factor and integer values : df_nf
#Random Forest and Naive Bayes
df_nf<-data.frame(dataset1)

#keeping only labeled data
df_nf <- df_nf[!is.na(df_nf$liver_fat),]
NROW(df)

#converting the value numerical values of liver fat to categorical
for(i in 1:nrow(df_nf)){
  if(df_nf$liver_fat[i]  < 10){
    df_nf$liver_fat[i] <- 0
  }
  else
    df_nf$liver_fat[i] <- 1
}

df_nf$liver_fat<-as.character(df$liver_fat)
df_nf$liver_fat<-as.numeric(df$liver_fat)

#deleting irrelevant columns
df_nf$blt_beg_s0<-NULL
df_nf$blt_beg_s1<-NULL
df_nf$blt_beg_s2<-NULL
df_nf$exdate_ship_s0<-NULL
df_nf$exdate_ship_s1<-NULL
df_nf$exdate_ship_s2<-NULL
df_nf$zz_nr<-NULL
df_nf$exdate_ship0_s0<-NULL

#converting all integer values to numeric
w_num <- which( sapply( df_nf, class ) == 'integer' )
df_nf[w_num] <- lapply( df_nf[w_num], function(x) as.numeric(x) )
str(df_nf)

#converting numeric into categorical values
str(df_nf$age_ship0_s0)

w_cat <- which( sapply( df_nf, class ) == 'numeric' )
df_nf[w_cat] <- lapply( df_nf[w_cat], function(x) 
                cut(x, breaks = 2, labels = c("0","1")))
str(df_nf)

#replacing missing values by "none"
df_nf=as.matrix(df_nf)
df_nf[is.na(df_nf)] <-"None"
df_nf=as.data.frame(df_nf)

df_nf$liver_fat<-dataset1$liver_fat
str(df_nf$liver_fat)
df_nf$liver_fat<-as.factor(df_nf$liver_fat)

export(df_nf, "allfactor.rds")
export(df_nf, "allfactor.csv")

table(df_nf$liver_fat)
#rf2<- randomForest(liver_fat~.,data = df_nf)

#-------------------------------------------------------------------------------

#-----------------End of: Preprocessing-----------------------------------------

