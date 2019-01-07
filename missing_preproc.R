#------------------Missing values processing--------------------------------
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

data = readRDS('C:/Users/subha/Documents/R/MyRProg/181121_ship.rds')
df_m<-data.frame(data)

data <- data[!is.na(data$liver_fat),]
NROW(data)

common_vars_3w_m <- c("atc_c07a_s0","atc_c07a_s1","atc_c07a_s2","atc_c07aa_s0","atc_c07aa_s1","atc_c07aa_s2","atc_c07ab_s0","atc_c07ab_s1","atc_c07ab_s2","atc_c08_s0","atc_c08_s1","atc_c08_s2","atc_c08ca01_s0","atc_c08ca01_s1","atc_c08ca01_s2","atc_c08ca05_s0","atc_c08ca05_s1","atc_c08ca05_s2","atc_c08ca08_s0","atc_c08ca08_s1","atc_c08ca08_s2",
                    "atc_c08da01_s0","atc_c08da01_s1","atc_c08da01_s2","atc_c09aa02_s0","atc_c09aa02_s1","atc_c09aa02_s2","atc_c09aa05_s0","atc_c09aa05_s1","atc_c09aa05_s2","atc_h02a_s0","atc_h02a_s1","atc_h02a_s2","blt_beg_s0","blt_beg_s1","blt_beg_s2","chol_s_s0","chol_s_s1","chol_s_s2","crea_s_s0","crea_s_s1","crea_s_s2","crea_u_s0","crea_u_s1","crea_u_s2","earm_s0","earm_s1","earm_s2","fib_cl_s0","fib_cl_s1","fib_cl_s2","fs_s0","fs_s1","ggt_s_s0","ggt_s_s1","ggt_s_s2","gluc_s_s0","gluc_s_s1","gluc_s_s2","goiter_s0","goiter_s1","goiter_s2","gout_s0","gout_s1","gout_s2","hba1c_s0","hba1c_s1","hba1c_s2","hbs_ag_s0","hdl_s_s1","hdl_s_s2","hdl_s_s0","hgb_s1","hgb_s2","hgb_s0","hrs_s_s1","hrs_s_s2","hs_crp_s0","hs_crp_s1","hyperlipid_s0","hyperlipid_s1","hyperlipid_s2","igf1_s0","igf1_s1","jodid_u_s0","jodid_u_s1","jodid_u_s2","knoten_s0","knoten_s1","knoten_s2","ldl_s_s0","ldl_s_s1","ldl_s_s2","lip_s_s0","lip_s_s1","lip_s_s2","parity_w_s1","parity_w_s2","quick_s0","quick_s1","quick_s2","sd_volg_s0","sd_volg_s1","sd_volg_s2","sleeph_s0","sleeph_s2","sleepp_s0","sleepp_s2","som_bmi_s0","som_bmi_s2","som_huef_s0","som_huef_s2","som_tail_s0","som_tail_s2","stea_alt75_s0","stea_alt75_s2","stea_s0","stea_s2","tsh_s0","tsh_s1","tsh_s2","age_ship_s0","age_ship_s1","age_ship_s2","exdate_ship_s0","exdate_ship_s1","exdate_ship_s2","parity_w_s0","parity_w_s1","parity_w_s2","menopaus_yn_w_s0","menopaus_yn_w_s1","menopaus_yn_w_s2","menopaus_w_s0","menopaus_w_s1","menopaus_w_s2","fruehgeb_w_s0","fruehgeb_w_s2","hormonrepl_w_s0","hormonrepl_w_s1","hormonrepl_w_s2","pillever_w_s0","pillever_w_s2","pillnow_w_s0","pillnow_w_s2","abstain_s0","abstain_s1","abstain_s2","smoking_s0","smoking_s1","smoking_s2","diabetes_s0","diabetes_s1","diabetes_s2","alkligt_s0","alkligt_s1","alkligt_s2","asat_s_s0","asat_s_s1","asat_s_s2","tg_s_s0","tg_s_s1","tg_s_s2","liver_fat")
df_subset_3waves_common_m <- df[common_vars_3w_m]
df_merge<-df_subset_3waves_common_m
#Deleting unlabeled data from the dataset
df_subset_3waves_common_m <- df_subset_3waves_common_m[!is.na(df_subset_3waves_common_m$liver_fat),]
NROW(df_subset_3waves_common_m)

str(df_subset_3waves_common_m)
#----------------------------------------------------------------------
View(factor_list)

df_merge1<-df_merge[factor_list]
str(df_merge1)

w_intn <- which( sapply( df_merge, class ) == 'integer' )
df_merge[w_intn] <- lapply( df_merge[w_intn], function(x) as.numeric(x) )
str(df_merge)
NROW(df_merge)
df_merge <- df_merge[!is.na(df_merge$liver_fat),]
NROW(df_merge)


#----------------------------------------------------------------------
#-----listing out all the colums of factor type------
factor_list<-c(1:81)
i<-1
for (n in names(df_subset_3waves_common_m))
  if (is.factor(df_subset_3waves_common_m[[n]])) {
    factor_list[[i]]<-n
    i=i+1
    #print(levels(temp_set1[[n]]))
  }
View(factor_list) #list of columns of typefactor

common_vars_3w_fac <- c("atc_c07a_s0","atc_c07a_s1","atc_c07a_s2","atc_c07aa_s0","atc_c07aa_s1","atc_c07aa_s2","atc_c07ab_s0","atc_c07ab_s1","atc_c07ab_s2","atc_c08_s0","atc_c08_s1","atc_c08_s2","atc_c08ca01_s0","atc_c08ca01_s1","atc_c08ca01_s2","atc_c08ca05_s0","atc_c08ca05_s1","atc_c08ca05_s2","atc_c08ca08_s0","atc_c08ca08_s1","atc_c08ca08_s2",
                      "atc_c08da01_s0","atc_c08da01_s1","atc_c08da01_s2","atc_c09aa02_s0","atc_c09aa02_s1","atc_c09aa02_s2","atc_c09aa05_s0","atc_c09aa05_s1","atc_c09aa05_s2","atc_h02a_s0","atc_h02a_s1","atc_h02a_s2","blt_beg_s0","blt_beg_s1","blt_beg_s2","earm_s0","earm_s1","earm_s2","goiter_s0","goiter_s1","goiter_s2","gout_s0","gout_s1","gout_s2","hyperlipid_s0","hyperlipid_s1","hyperlipid_s2","knoten_s0","knoten_s1","knoten_s2","parity_w_s1","parity_w_s2","stea_alt75_s0","stea_alt75_s2","stea_s0","stea_s2","parity_w_s0","parity_w_s1","parity_w_s2","menopaus_yn_w_s0","menopaus_yn_w_s1","menopaus_yn_w_s2","fruehgeb_w_s0","fruehgeb_w_s2","hormonrepl_w_s0","hormonrepl_w_s1","hormonrepl_w_s2","pillever_w_s0","pillever_w_s2","pillnow_w_s0","pillnow_w_s2","abstain_s0","abstain_s1","abstain_s2","smoking_s0","smoking_s1","smoking_s2","diabetes_s0","diabetes_s1","diabetes_s2","liver_fat")

df_factor <- df_subset_3waves_common_m[common_vars_3w_fac]

str(df_factor)

df_factor=as.matrix(df_factor)
df_factor[is.na(df_factor)] <-"None"
df_factor=as.data.frame(df_factor)
str(df_factor$parity_w_s0)

df_factor$liver_fat<-as.character(df_factor$liver_fat)
df_factor$liver_fat<-as.numeric(df_factor$liver_fat)

for(i in 1:nrow(df_factor)){
  if(df_factor$liver_fat[i]  < 10){
    df_factor$liver_fat[i] <- 0
  }
  else
    df_factor$liver_fat[i] <- 1
}

str(df_factor$blt_beg_s0)
df_factor$blt_beg_s0<-NULL

str(df_factor$blt_beg_s1)
df_factor$blt_beg_s1<-NULL

str(df_factor$blt_beg_s2)
df_factor$blt_beg_s2<-NULL

df_factor$liver_fat<-as.factor(df_factor$liver_fat)

rf1<- randomForest(liver_fat~.,data = df_factor)
str(df_factor)

#-----listing out all the colums of numeric type------

w_int <- which( sapply( df_subset_3waves_common_m, class ) == 'integer' )
df_subset_3waves_common_m[w_int] <- lapply( df_subset_3waves_common_m[w_int], function(x) as.numeric(x) )
str(df_subset_3waves_common_m)


common_vars_3w_m_num <- c("chol_s_s0","chol_s_s1","chol_s_s2","crea_s_s0","crea_s_s1","crea_s_s2","crea_u_s0","crea_u_s1","crea_u_s2","fib_cl_s0","fib_cl_s1","fib_cl_s2","fs_s0","fs_s1","ggt_s_s0","ggt_s_s1","ggt_s_s2","gluc_s_s0","gluc_s_s1","gluc_s_s2","hba1c_s0","hba1c_s1","hba1c_s2","hdl_s_s1","hdl_s_s2","hdl_s_s0","hgb_s1","hgb_s2","hgb_s0","hrs_s_s1","hrs_s_s2","hs_crp_s0","hs_crp_s1","igf1_s0","igf1_s1","jodid_u_s0","jodid_u_s1","jodid_u_s2","ldl_s_s0","ldl_s_s1","ldl_s_s2","lip_s_s0","lip_s_s1","lip_s_s2","quick_s0","quick_s1","quick_s2","sd_volg_s0","sd_volg_s1","sd_volg_s2","sleepp_s0","sleepp_s2","som_bmi_s0","som_bmi_s2","som_huef_s0","som_huef_s2","som_tail_s0","som_tail_s2","tsh_s0","tsh_s1","tsh_s2","age_ship_s0","age_ship_s1","age_ship_s2","menopaus_w_s0","menopaus_w_s1","menopaus_w_s2","alkligt_s0","alkligt_s1","alkligt_s2","asat_s_s0","asat_s_s1","asat_s_s2","tg_s_s0","tg_s_s1","tg_s_s2")


View(numeric_list) #list of columns of type numeric

df_numeric<-df_subset_3waves_common_m[common_vars_3w_m_num]

str(df_numeric)

#----------------------
summary(df_numeric$chol_s_s0)
df_numeric$chol_s_s0<-data$chol_s_s0
df_numeric$chol_s_s0<-cut(df_numeric$chol_s_s0, breaks = 2, labels = c("0","1"))
df_merge$chol_s_s0<-df_numeric$chol_s_s0

summary(df_numeric$chol_s_s1)
df_numeric$chol_s_s1<-data$chol_s_s1
df_numeric$chol_s_s1<-cut(df_numeric$chol_s_s1, breaks = 2, labels = c("0","1"))
df_merge$chol_s_s1<-df_numeric$chol_s_s1

summary(df_numeric$chol_s_s2)
df_numeric$chol_s_s2<-data$chol_s_s2
df_numeric$chol_s_s2<-cut(df_numeric$chol_s_s2, breaks = 2, labels = c("0","1"))
df_merge$chol_s_s2<-df_numeric$chol_s_s2

summary(df_numeric$crea_s_s0)
df_numeric$crea_s_s0<-data$crea_s_s0
df_numeric$crea_s_s0<-cut(df_numeric$crea_s_s0, breaks = 2, labels = c("0","1"))
df_merge$crea_s_s0<-df_numeric$crea_s_s0

summary(df_numeric$crea_s_s1)
df_numeric$crea_s_s1<-cut(df_numeric$crea_s_s1, breaks = 2, labels = c("0","1"))
df_merge$crea_s_s1<-df_numeric$crea_s_s1

summary(df_numeric$crea_s_s2)
df_numeric$crea_s_s2<-cut(df_numeric$crea_s_s2, breaks = 2, labels = c("0","1"))
df_merge$crea_s_s2<-df_numeric$crea_s_s2

summary(df_numeric$crea_u_s0)
df_numeric$crea_u_s0<-cut(df_numeric$crea_u_s0, breaks = 2, labels = c("0","1"))
df_merge$crea_u_s0<-df_numeric$crea_u_s0

summary(df_numeric$crea_u_s1)
df_numeric$crea_u_s1<-cut(df_numeric$crea_u_s1, breaks = 2, labels = c("0","1"))
df_merge$crea_u_s1<-df_numeric$crea_u_s1

summary(df_numeric$crea_u_s2)
df_numeric$crea_u_s2<-cut(df_numeric$crea_u_s2, breaks = 2, labels = c("0","1"))
df_merge$crea_u_s2<-df_numeric$crea_u_s2

summary(df_numeric$fib_cl_s0)
df_numeric$fib_cl_s0<-cut(df_numeric$fib_cl_s0, breaks = 2, labels = c("0","1"))
df_merge$fib_cl_s0<-df_numeric$fib_cl_s0

summary(df_numeric$fib_cl_s1)
df_numeric$fib_cl_s1<-cut(df_numeric$fib_cl_s1, breaks = 2, labels = c("0","1"))
df_merge$fib_cl_s1<-df_numeric$fib_cl_s1

summary(df_numeric$fib_cl_s2)
df_numeric$fib_cl_s2<-cut(df_numeric$fib_cl_s2, breaks = 2, labels = c("0","1"))
df_merge$fib_cl_s2<-df_numeric$fib_cl_s2

summary(df_numeric$fs_s0)
df_numeric$fs_s0<-cut(df_numeric$fs_s0, breaks = 2, labels = c("0","1"))
df_merge$fs_s0<-df_numeric$fs_s0

summary(df_numeric$fs_s1)
df_numeric$fs_s1<-cut(df_numeric$fs_s1, breaks = 2, labels = c("0","1"))
df_merge$fs_s1<-df_numeric$fs_s1

summary(df_numeric$ggt_s_s0)
df_numeric$ggt_s_s0<-cut(df_numeric$ggt_s_s0, breaks = 2, labels = c("0","1"))
df_merge$ggt_s_s0<-df_numeric$ggt_s_s0

summary(df_numeric$fib_cl_s0)
df_numeric$fib_cl_s0<-cut(df_numeric$fib_cl_s0, breaks = 2, labels = c("0","1"))
df_merge$fib_cl_s0<-df_numeric$fib_cl_s0

summary(df_numeric$ggt_s_s1)
df_numeric$ggt_s_s1<-cut(df_numeric$ggt_s_s1, breaks = 2, labels = c("0","1"))
df_merge$ggt_s_s1<-df_numeric$ggt_s_s1

summary(df_numeric$fib_cl_s2)
df_numeric$fib_cl_s2<-cut(df_numeric$fib_cl_s2, breaks = 2, labels = c("0","1"))
df_merge$fib_cl_s2<-df_numeric$fib_cl_s2

df_numeric$ggt_s_s2<-cut(df_numeric$ggt_s_s2, breaks = 2, labels = c("0","1"))
df_numeric$gluc_s_s0<-cut(df_numeric$gluc_s_s0, breaks = 2, labels = c("0","1"))
df_numeric$gluc_s_s1<-cut(df_numeric$gluc_s_s1, breaks = 2, labels = c("0","1"))
df_numeric$gluc_s_s2<-cut(df_numeric$gluc_s_s2, breaks = 2, labels = c("0","1"))
df_numeric$hba1c_s0<-cut(df_numeric$hba1c_s0, breaks = 2, labels = c("0","1"))
df_numeric$hba1c_s1<-cut(df_numeric$hba1c_s1, breaks = 2, labels = c("0","1"))
df_numeric$hba1c_s2<-cut(df_numeric$hba1c_s2, breaks = 2, labels = c("0","1"))
df_numeric$hdl_s_s1<-cut(df_numeric$hdl_s_s1, breaks = 2, labels = c("0","1"))

df_merge$ggt_s_s2<-df_numeric$ggt_s_s2
df_merge$gluc_s_s0<-df_numeric$gluc_s_s0
df_merge$gluc_s_s1<-df_numeric$gluc_s_s1
df_merge$gluc_s_s2<-df_numeric$gluc_s_s2
df_merge$hba1c_s0<-df_numeric$hba1c_s0
df_merge$hba1c_s1<-df_numeric$hba1c_s1
df_merge$hba1c_s2<-df_numeric$hba1c_s2
df_merge$hdl_s_s1<-df_numeric$hdl_s_s1

df_numeric$hdl_s_s2<-cut(df_numeric$hdl_s_s2, breaks = 2, labels = c("0","1"))
df_numeric$hdl_s_s0<-cut(df_numeric$hdl_s_s0, breaks = 2, labels = c("0","1"))
df_numeric$hgb_s1<-cut(df_numeric$hgb_s1, breaks = 2, labels = c("0","1"))
df_numeric$hgb_s2<-cut(df_numeric$hgb_s2, breaks = 2, labels = c("0","1"))
df_numeric$hgb_s0<-cut(df_numeric$hgb_s0, breaks = 2, labels = c("0","1"))
df_numeric$hrs_s_s1<-cut(df_numeric$hrs_s_s1, breaks = 2, labels = c("0","1"))
df_numeric$hrs_s_s2<-cut(df_numeric$hrs_s_s2, breaks = 2, labels = c("0","1"))
df_numeric$hs_crp_s0<-cut(df_numeric$hs_crp_s0, breaks = 2, labels = c("0","1"))

df_merge$hdl_s_s2<-df_numeric$hdl_s_s2
df_merge$hdl_s_s0<-df_numeric$hdl_s_s0
df_merge$hgb_s1<-df_numeric$hgb_s1
df_merge$hgb_s2<-df_numeric$hgb_s2
df_merge$hgb_s0<-df_numeric$hgb_s0
df_merge$hrs_s_s1<-df_numeric$hrs_s_s1
df_merge$hrs_s_s2<-df_numeric$hrs_s_s2
df_merge$hs_crp_s0<-df_numeric$hs_crp_s0

df_numeric$hs_crp_s1<-cut(df_numeric$hs_crp_s1, breaks = 2, labels = c("0","1"))
df_numeric$igf1_s0<-cut(df_numeric$igf1_s0, breaks = 2, labels = c("0","1"))
df_numeric$igf1_s1<-cut(df_numeric$igf1_s1, breaks = 2, labels = c("0","1"))
df_numeric$jodid_u_s0<-cut(df_numeric$jodid_u_s0, breaks = 2, labels = c("0","1"))
df_numeric$jodid_u_s1<-cut(df_numeric$jodid_u_s1, breaks = 2, labels = c("0","1"))
df_numeric$jodid_u_s2<-cut(df_numeric$jodid_u_s2, breaks = 2, labels = c("0","1"))
df_numeric$ldl_s_s0<-cut(df_numeric$ldl_s_s0, breaks = 2, labels = c("0","1"))
df_numeric$ldl_s_s1<-cut(df_numeric$ldl_s_s1, breaks = 2, labels = c("0","1"))

df_merge$hs_crp_s1<-df_numeric$hs_crp_s1
df_merge$igf1_s0<-df_numeric$igf1_s0
df_merge$igf1_s1<-df_numeric$igf1_s1
df_merge$jodid_u_s0<-df_numeric$jodid_u_s0
df_merge$jodid_u_s1<-df_numeric$jodid_u_s1
df_merge$jodid_u_s2<-df_numeric$jodid_u_s2
df_merge$ldl_s_s0<-df_numeric$ldl_s_s0
df_merge$ldl_s_s1<-df_numeric$ldl_s_s1

df_numeric$ldl_s_s2<-cut(df_numeric$ldl_s_s2, breaks = 2, labels = c("0","1"))
df_numeric$lip_s_s0<-cut(df_numeric$lip_s_s0, breaks = 2, labels = c("0","1"))
df_numeric$lip_s_s1<-cut(df_numeric$lip_s_s1, breaks = 2, labels = c("0","1"))
df_numeric$lip_s_s2<-cut(df_numeric$lip_s_s2, breaks = 2, labels = c("0","1"))
df_numeric$quick_s0<-cut(df_numeric$quick_s0, breaks = 2, labels = c("0","1"))
df_numeric$quick_s1<-cut(df_numeric$quick_s1, breaks = 2, labels = c("0","1"))
df_numeric$quick_s2<-cut(df_numeric$quick_s2, breaks = 2, labels = c("0","1"))
df_numeric$sd_volg_s0<-cut(df_numeric$sd_volg_s0, breaks = 2, labels = c("0","1"))

df_merge$ldl_s_s2<-df_numeric$ldl_s_s2
df_merge$lip_s_s0<-df_numeric$lip_s_s0
df_merge$lip_s_s1<-df_numeric$lip_s_s1
df_merge$lip_s_s2<-df_numeric$lip_s_s2
df_merge$quick_s0<-df_numeric$quick_s0
df_merge$quick_s1<-df_numeric$quick_s1
df_merge$quick_s2<-df_numeric$quick_s2
df_merge$sd_volg_s0<-df_numeric$sd_volg_s0

df_numeric$sd_volg_s1<-cut(df_numeric$sd_volg_s1, breaks = 2, labels = c("0","1"))
df_numeric$sd_volg_s2<-cut(df_numeric$sd_volg_s2, breaks = 2, labels = c("0","1"))

df_merge$sd_volg_s1<-df_numeric$sd_volg_s1
df_merge$sd_volg_s2<-df_numeric$sd_volg_s2
#-------------------------------
str(data$sleeph_s0)
df_numeric$sleeph_s0<-data$sleeph_s0
str(df_numeric$sleeph_s0)
df_numeric$sleeph_s0<-cut(df_numeric$sleeph_s0, breaks = 2, labels = c("0","1"))

df_numeric$sleeph_s2<-data$sleeph_s2
df_numeric$sleeph_s2<-cut(df_numeric$sleeph_s2, breaks = 2, labels = c("0","1"))

df_merge$sleeph_s0<-df_numeric$sleeph_s0
df_merge$sleeph_s2<-df_numeric$sleeph_s2
#------------------------------
df_numeric$som_bmi_s0<-cut(df_numeric$som_bmi_s0, breaks = 2, labels = c("0","1"))
df_numeric$som_bmi_s2<-cut(df_numeric$som_bmi_s2, breaks = 2, labels = c("0","1"))
df_numeric$som_huef_s0<-cut(df_numeric$som_huef_s0, breaks = 2, labels = c("0","1"))
df_numeric$som_huef_s2<-cut(df_numeric$som_huef_s2, breaks = 2, labels = c("0","1"))

df_merge$som_bmi_s0<-df_numeric$som_bmi_s0
df_merge$som_bmi_s2<-df_numeric$som_bmi_s2
df_merge$som_huef_s0<-df_numeric$som_huef_s0
df_merge$som_huef_s2<-df_numeric$som_huef_s2

df_numeric$som_tail_s0<-cut(df_numeric$som_tail_s0, breaks = 2, labels = c("0","1"))
df_numeric$som_tail_s2<-cut(df_numeric$som_tail_s2, breaks = 2, labels = c("0","1"))
df_numeric$tsh_s0<-cut(df_numeric$tsh_s0, breaks = 2, labels = c("0","1"))
df_numeric$tsh_s1<-cut(df_numeric$tsh_s1, breaks = 2, labels = c("0","1"))
df_numeric$tsh_s2<-cut(df_numeric$tsh_s2, breaks = 2, labels = c("0","1"))
df_numeric$age_ship_s0<-cut(df_numeric$age_ship_s0, breaks = 2, labels = c("0","1"))
df_numeric$age_ship_s1<-cut(df_numeric$age_ship_s1, breaks = 2, labels = c("0","1"))

df_merge$som_tail_s0<-df_numeric$som_tail_s0
df_merge$som_tail_s2<-df_numeric$som_tail_s2
df_merge$tsh_s0<-df_numeric$tsh_s0
df_merge$tsh_s1<-df_numeric$tsh_s1
df_merge$tsh_s2<-df_numeric$tsh_s2
df_merge$age_ship_s0<-df_numeric$age_ship_s0
df_merge$age_ship_s1<-df_numeric$age_ship_s1

df_numeric$age_ship_s2<-cut(df_numeric$age_ship_s2, breaks = 2, labels = c("0","1"))
df_numeric$menopaus_w_s0<-cut(df_numeric$menopaus_w_s0, breaks = 2, labels = c("0","1"))
df_numeric$menopaus_w_s1<-cut(df_numeric$menopaus_w_s1, breaks = 2, labels = c("0","1"))
df_numeric$menopaus_w_s2<-cut(df_numeric$menopaus_w_s2, breaks = 2, labels = c("0","1"))
df_numeric$alkligt_s0<-cut(df_numeric$alkligt_s0, breaks = 2, labels = c("0","1"))
df_numeric$alkligt_s1<-cut(df_numeric$alkligt_s1, breaks = 2, labels = c("0","1"))
df_numeric$alkligt_s2<-cut(df_numeric$alkligt_s2, breaks = 2, labels = c("0","1"))

df_merge$age_ship_s2<-df_numeric$age_ship_s2
df_merge$menopaus_w_s0<-df_numeric$menopaus_w_s0
df_merge$menopaus_w_s1<-df_numeric$menopaus_w_s1
df_merge$menopaus_w_s2<-df_numeric$menopaus_w_s2
df_merge$alkligt_s0<-df_numeric$alkligt_s0
df_merge$alkligt_s1<-df_numeric$alkligt_s1
df_merge$alkligt_s2<-df_numeric$alkligt_s2

df_numeric$asat_s_s0<-cut(df_numeric$asat_s_s0, breaks = 2, labels = c("0","1"))
df_numeric$asat_s_s1<-cut(df_numeric$asat_s_s1, breaks = 2, labels = c("0","1"))
df_numeric$asat_s_s2<-cut(df_numeric$asat_s_s2, breaks = 2, labels = c("0","1"))
df_numeric$tg_s_s0<-cut(df_numeric$tg_s_s0, breaks = 2, labels = c("0","1"))
df_numeric$tg_s_s1<-cut(df_numeric$tg_s_s1, breaks = 2, labels = c("0","1"))
df_numeric$tg_s_s2<-cut(df_numeric$tg_s_s2, breaks = 2, labels = c("0","1"))

df_merge$asat_s_s0<-df_numeric$asat_s_s0
df_merge$asat_s_s1<-df_numeric$asat_s_s1
df_merge$asat_s_s2<-df_numeric$asat_s_s2
df_merge$tg_s_s0<-df_numeric$tg_s_s0
df_merge$tg_s_s1<-df_numeric$tg_s_s1
df_merge$tg_s_s2<-df_numeric$tg_s_s2

str(df_merge)

df_merge$blt_beg_s0<-NULL
df_merge$blt_beg_s1<-NULL
df_merge$blt_beg_s2<-NULL
df_merge$liver_fat<-NULL
df_merge$exdate_ship_s0<-NULL
df_merge$exdate_ship_s1<-NULL
df_merge$exdate_ship_s2<-NULL

df_merge$liver_fat<-df_factor$liver_fat
View(df_merge)

df_merge=as.matrix(df_merge)
df_merge[is.na(df_merge)] <-"None"
df_merge=as.data.frame(df_merge)
export(df_merge, "df_merge.csv")

str(df_numeric)
df_numeric=as.matrix(df_numeric)
df_numeric[is.na(df_numeric)] <-"None"
df_numeric=as.data.frame(df_numeric)

df_numeric$liver_fat<-df_factor$liver_fat

rf2<- randomForest(liver_fat~.,data = df_merge)

export(df_merge, "df_merge.rds")
#--------------------------------------------------------------------

  
