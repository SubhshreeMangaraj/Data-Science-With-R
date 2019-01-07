
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
library(reshape2)
library(ggplot2)
library(dplyr)
library(Amelia)
#---------------------End of import libraries-------------------------------

#---------------------Begin of loading of Dataset---------------------------

dataset1 = readRDS('C:/Users/subha/Documents/R/MyRProg/181121_ship.rds')
?dataset1
df<-data.frame(dataset1) #all factor
df1<-data.frame(dataset1) #factor + numeric

df <- df[!is.na(df$liver_fat),]
df1 <- df1[!is.na(df1$liver_fat),]
NROW(df)


for(i in 1:nrow(df)){
  if(df$liver_fat[i]  < 10){
    df$liver_fat[i] <- 0
  }
  else
    df$liver_fat[i] <- 1
}

for(i in 1:nrow(df1)){
  if(df1$liver_fat[i]  < 10){
    df1$liver_fat[i] <- 0
  }
  else
    df1$liver_fat[i] <- 1
}

df$liver_fat<-as.character(df$liver_fat)
df$liver_fat<-as.numeric(df$liver_fat)

df1$liver_fat<-as.character(df1$liver_fat)
df1$liver_fat<-as.numeric(df1$liver_fat)


df$blt_beg_s0<-NULL
df$blt_beg_s1<-NULL
df$blt_beg_s2<-NULL
df$exdate_ship_s0<-NULL
df$exdate_ship_s1<-NULL
df$exdate_ship_s2<-NULL
df$zz_nr<-NULL
df$exdate_ship0_s0<-NULL

df1$blt_beg_s0<-NULL
df1$blt_beg_s1<-NULL
df1$blt_beg_s2<-NULL
df1$exdate_ship_s0<-NULL
df1$exdate_ship_s1<-NULL
df1$exdate_ship_s2<-NULL
df1$zz_nr<-NULL
df1$exdate_ship0_s0<-NULL

w_int <- which( sapply( df, class ) == 'integer' )
df[w_int] <- lapply( df[w_int], function(x) as.numeric(x) )

w_int1 <- which( sapply( df1, class ) == 'integer' )
df[w_int1] <- lapply( df1[w_int1], function(x) as.numeric(x) )

summary(df$age_ship_s0)
df$age_ship_s0<-as.numeric(as.character(df$age_ship_s0))
df$age_ship_s0<-cut(df$age_ship_s0, breaks = 3, labels = c("0","1","2")) 
df$age_ship_s1<-df1$age_ship_s1
df$age_ship_s1<-as.numeric(as.character(df$age_ship_s1))
df$age_ship_s1<-cut(df$age_ship_s1, breaks = 3, labels = c("0","1","2"))
df$age_ship_s2<-df1$age_ship_s2
df$age_ship_s2<-as.numeric(as.character(df$age_ship_s2))
df$age_ship_s2<-cut(df$age_ship_s2, breaks = 3, labels = c("0","1","2"))
df$age_ship0_s0<-df1$age_ship0_s0
df$age_ship0_s0<-as.numeric(as.character(df$age_ship0_s0))
df$age_ship0_s0<-cut(df$age_ship0_s0, breaks = 3, labels = c("0","1","2"))

str(df$age_ship0_s0)


#listing all numeric features
library("dplyr")
num<-select_if(df, is.numeric)
View(colnames(num))


num2cat <- function(colnam) {
  n<-paste("df",colnam,sep = '$',collapse = "")
  print(n)
  colnam <- cut(as.numeric(n), breaks = 2, labels = c("0","1"))
}


for (i in colnames(df)) {
  for (j in colnames(num)) {
    if( i == j){
      num2cat(i)
    }
    
  }
  
}

str(df)


df$menopaus_w_s0<-cut(df$menopaus_w_s0, breaks = 2, labels = c("0","1"))
df$kaffee_s0<-cut(df$kaffee_s0, breaks = 2, labels = c("0","1"))
df$alkligt_s0<-cut(df$alkligt_s0, breaks = 2, labels = c("0","1"))
df$sleeph_s0<-cut(df$sleeph_s0, breaks = 2, labels = c("0","1"))
df$som_bmi_s0<-cut(df$som_bmi_s0, breaks = 2, labels = c("0","1"))
df$som_tail_s0<-cut(df$som_tail_s0, breaks = 2, labels = c("0","1"))
df$som_huef_s0<-cut(df$som_huef_s0, breaks = 2, labels = c("0","1"))
df$hgb_s0<-cut(df$hgb_s0, breaks = 2, labels = c("0","1"))
df$hba1c_s0<-cut(df$hba1c_s0, breaks = 2, labels = c("0","1"))
df$quick_s0<-cut(df$quick_s0, breaks = 2, labels = c("0","1"))
df$fib_cl_s0<-cut(df$fib_cl_s0, breaks = 2, labels = c("0","1"))
df$crea_s_s0<-cut(df$crea_s_s0, breaks = 2, labels = c("0","1"))
df$hrs_s_s0<-cut(df$hrs_s_s0, breaks = 2, labels = c("0","1"))
df$gluc_s_s0<-cut(df$gluc_s_s0, breaks = 2, labels = c("0","1"))
df$asat_s_s0<-cut(df$asat_s_s0, breaks = 2, labels = c("0","1"))
df$alat_s_s0<-cut(df$alat_s_s0, breaks = 2, labels = c("0","1"))

df$ggt_s_s0<-cut(df$ggt_s_s0, breaks = 2, labels = c("0","1"))
df$lip_s_s0<-cut(df$lip_s_s0, breaks = 2, labels = c("0","1"))
df$chol_s_s0<-cut(df$chol_s_s0, breaks = 2, labels = c("0","1"))
df$tg_s_s0<-cut(df$tg_s_s0, breaks = 2, labels = c("0","1"))
df$hdl_s_s0<-cut(df$hdl_s_s0, breaks = 2, labels = c("0","1"))
df$ldl_s_s0<-cut(df$ldl_s_s0, breaks = 2, labels = c("0","1"))
df$lipo_a_s0<-cut(df$lipo_a_s0, breaks = 2, labels = c("0","1"))
df$apoa1_s0<-cut(df$apoa1_s0, breaks = 2, labels = c("0","1"))
df$apob_s0<-cut(df$apob_s0, breaks = 2, labels = c("0","1"))
df$tsh_s0<-cut(df$tsh_s0, breaks = 2, labels = c("0","1"))
df$ferri_s0<-cut(df$ferri_s0, breaks = 2, labels = c("0","1"))
df$cdt_s0<-cut(df$cdt_s0, breaks = 2, labels = c("0","1"))
df$jodid_u_s0<-cut(df$jodid_u_s0, breaks = 2, labels = c("0","1"))
df$alb_u_s0<-cut(df$alb_u_s0, breaks = 2, labels = c("0","1"))
df$crea_u_s0<-cut(df$crea_u_s0, breaks = 2, labels = c("0","1"))
df$hs_crp_s0<-cut(df$hs_crp_s0, breaks = 2, labels = c("0","1"))

df$testo_m_s0<-cut(df$testo_m_s0, breaks = 2, labels = c("0","1"))
df$dheas_m_s0<-cut(df$dheas_m_s0, breaks = 2, labels = c("0","1"))
df$il6_s0<-cut(df$il6_s0, breaks = 2, labels = c("0","1"))
df$prl_s0<-cut(df$prl_s0, breaks = 2, labels = c("0","1"))
df$igf1_s0<-cut(df$igf1_s0, breaks = 2, labels = c("0","1"))
df$igfbp3_s0<-cut(df$igfbp3_s0, breaks = 2, labels = c("0","1"))
df$ige_s0<-cut(df$ige_s0, breaks = 2, labels = c("0","1"))
df$udpdkrea_s0<-cut(df$udpdkrea_s0, breaks = 2, labels = c("0","1"))
df$fs_s0<-cut(df$fs_s0, breaks = 2, labels = c("0","1"))
df$sd_volg_s0<-cut(df$sd_volg_s0, breaks = 2, labels = c("0","1"))
df$avmean_s0<-cut(df$avmean_s0, breaks = 2, labels = c("0","1"))
df$avmeanapp_s0<-cut(df$avmeanapp_s0, breaks = 2, labels = c("0","1"))
df$stmean_s0<-cut(df$stmean_s0, breaks = 2, labels = c("0","1"))
df$stmeanapp_s0<-cut(df$stmeanapp_s0, breaks = 2, labels = c("0","1"))
df$zaehne_s0<-cut(df$zaehne_s0, breaks = 2, labels = c("0","1"))
df$flag<-cut(df$flag, breaks = 2, labels = c("0","1"))

df$age_ship_s1<-cut(df$age_ship_s1, breaks = 2, labels = c("0","1"))
df$menopaus_w_s1<-cut(df$menopaus_w_s1, breaks = 2, labels = c("0","1"))
df$alkligt_s1<-cut(df$alkligt_s1, breaks = 2, labels = c("0","1"))
df$som_bmi_s1<-cut(df$som_bmi_s1, breaks = 2, labels = c("0","1"))
df$som_tail_s1<-cut(df$som_tail_s1, breaks = 2, labels = c("0","1"))
df$som_huef_s1<-cut(df$som_huef_s1, breaks = 2, labels = c("0","1"))
df$hgb_s1<-cut(df$hgb_s1, breaks = 2, labels = c("0","1"))
df$hba1c_s1<-cut(df$hba1c_s1, breaks = 2, labels = c("0","1"))
df$quick_s1<-cut(df$quick_s1, breaks = 2, labels = c("0","1"))
df$fib_cl_s1<-cut(df$fib_cl_s1, breaks = 2, labels = c("0","1"))
df$crea_s_s1<-cut(df$crea_s_s1, breaks = 2, labels = c("0","1"))
df$hrs_s_s1<-cut(df$hrs_s_s1, breaks = 2, labels = c("0","1"))
df$gluc_s_s1<-cut(df$gluc_s_s1, breaks = 2, labels = c("0","1"))
df$asat_s_s1<-cut(df$asat_s_s1, breaks = 2, labels = c("0","1"))
df$ggt_s_s1<-cut(df$ggt_s_s1, breaks = 2, labels = c("0","1"))
df$lip_s_s1<-cut(df$lip_s_s1, breaks = 2, labels = c("0","1"))

df$chol_s_s1<-cut(df$chol_s_s1, breaks = 2, labels = c("0","1"))
df$tg_s_s1<-cut(df$tg_s_s1, breaks = 2, labels = c("0","1"))
df$hdl_s_s1<-cut(df$hdl_s_s1, breaks = 2, labels = c("0","1"))
df$ldl_s_s1<-cut(df$ldl_s_s1, breaks = 2, labels = c("0","1"))
df$tsh_s1<-cut(df$tsh_s1, breaks = 2, labels = c("0","1"))
df$jodid_u_s1<-cut(df$jodid_u_s1, breaks = 2, labels = c("0","1"))
df$crea_u_s1<-cut(df$crea_u_s1, breaks = 2, labels = c("0","1"))
df$hs_crp_s1<-cut(df$hs_crp_s1, breaks = 2, labels = c("0","1"))
df$testo_m_s1<-cut(df$testo_m_s1, breaks = 2, labels = c("0","1"))
df$igf1_s1<-cut(df$igf1_s1, breaks = 2, labels = c("0","1"))
df$fs_s1<-cut(df$fs_s1, breaks = 2, labels = c("0","1"))
df$sd_volg_s1<-cut(df$sd_volg_s1, breaks = 2, labels = c("0","1"))
df$age_ship_s2<-cut(df$age_ship_s2, breaks = 2, labels = c("0","1"))
df$menopaus_w_s2<-cut(df$menopaus_w_s2, breaks = 2, labels = c("0","1"))
df$alkligt_s2<-cut(df$alkligt_s2, breaks = 2, labels = c("0","1"))
df$sleeph_s2<-cut(df$sleeph_s2, breaks = 2, labels = c("0","1"))

df$som_bmi_s2<-cut(df$som_bmi_s2, breaks = 2, labels = c("0","1"))
df$som_tail_s2<-cut(df$som_tail_s2, breaks = 2, labels = c("0","1"))
df$som_huef_s2<-cut(df$som_huef_s2, breaks = 2, labels = c("0","1"))
df$hgb_s2<-cut(df$hgb_s2, breaks = 2, labels = c("0","1"))
df$hba1c_s2<-cut(df$hba1c_s2, breaks = 2, labels = c("0","1"))
df$quick_s2<-cut(df$quick_s2, breaks = 2, labels = c("0","1"))
df$fib_cl_s2<-cut(df$fib_cl_s2, breaks = 2, labels = c("0","1"))
df$crea_s_s2<-cut(df$crea_s_s2, breaks = 2, labels = c("0","1"))
df$hrs_s_s2<-cut(df$hrs_s_s2, breaks = 2, labels = c("0","1"))
df$gluc_s_s2<-cut(df$gluc_s_s2, breaks = 2, labels = c("0","1"))
df$asat_s_s2<-cut(df$asat_s_s2, breaks = 2, labels = c("0","1"))
df$ggt_s_s2<-cut(df$ggt_s_s2, breaks = 2, labels = c("0","1"))
df$lip_s_s2<-cut(df$lip_s_s2, breaks = 2, labels = c("0","1"))
df$chol_s_s2<-cut(df$chol_s_s2, breaks = 2, labels = c("0","1"))
df$tg_s_s2<-cut(df$tg_s_s2, breaks = 2, labels = c("0","1"))
df$hdl_s_s2<-cut(df$hdl_s_s2, breaks = 2, labels = c("0","1"))

df$ldl_s_s2<-cut(df$ldl_s_s2, breaks = 2, labels = c("0","1"))
df$tsh_s2<-cut(df$tsh_s2, breaks = 2, labels = c("0","1"))
df$jodid_u_s2<-cut(df$jodid_u_s2, breaks = 2, labels = c("0","1"))
df$crea_u_s2<-cut(df$crea_u_s2, breaks = 2, labels = c("0","1"))
df$sd_volg_s2<-cut(df$sd_volg_s2, breaks = 2, labels = c("0","1"))
df$sex_s0<-cut(df$sex_s0, breaks = 2, labels = c("0","1"))
df$age_ship0_s0<-cut(df$age_ship0_s0, breaks = 2, labels = c("0","1"))
df$exloc_ship0_s0<-cut(df$exloc_ship0_s0, breaks = 2, labels = c("0","1"))
df$w_sample_s0<-cut(df$w_sample_s0, breaks = 2, labels = c("0","1"))
df$psu_s0<-cut(df$psu_s0, breaks = 2, labels = c("0","1"))
df$strata1_s0<-cut(df$strata1_s0, breaks = 2, labels = c("0","1"))
df$fpc1_s0<-cut(df$fpc1_s0, breaks = 2, labels = c("0","1"))
df$strata2_s0<-cut(df$strata2_s0, breaks = 2, labels = c("0","1"))
df$fpc2_s0<-cut(df$fpc2_s0, breaks = 2, labels = c("0","1"))
df$edyrs_s0<-cut(df$edyrs_s0, breaks = 2, labels = c("0","1"))
df$kldb_75_s0<-cut(df$kldb_75_s0, breaks = 2, labels = c("0","1"))

df$hf_kldb_75_s0<-cut(df$hf_kldb_75_s0, breaks = 2, labels = c("0","1"))
df$siops_s0<-cut(df$siops_s0, breaks = 2, labels = c("0","1"))
df$mps_s0<-cut(df$mps_s0, breaks = 2, labels = c("0","1"))
df$isei_s0<-cut(df$isei_s0, breaks = 2, labels = c("0","1"))
df$inceq_s0<-cut(df$inceq_s0, breaks = 2, labels = c("0","1"))
df$doc4wks_s0<-cut(df$doc4wks_s0, breaks = 2, labels = c("0","1"))
df$doc12mths_s0<-cut(df$doc12mths_s0, breaks = 2, labels = c("0","1"))
df$genintdoc12m_s0<-cut(df$genintdoc12m_s0, breaks = 2, labels = c("0","1"))
df$heartr_s0<-cut(df$heartr_s0, breaks = 2, labels = c("0","1"))
df$sysbp_s0<-cut(df$sysbp_s0, breaks = 2, labels = c("0","1"))
df$diabp_s0<-cut(df$diabp_s0, breaks = 2, labels = c("0","1"))
df$lvm_s0<-cut(df$lvm_s0, breaks = 2, labels = c("0","1"))
df$lvmi_s0<-cut(df$lvmi_s0, breaks = 2, labels = c("0","1"))
df$packyrs_s0<-cut(df$packyrs_s0, breaks = 2, labels = c("0","1"))
df$onsetsmok_s0<-cut(df$onsetsmok_s0, breaks = 2, labels = c("0","1"))
df$ncigd_s0<-cut(df$ncigd_s0, breaks = 2, labels = c("0","1"))

df$alcg7d_s0<-cut(df$alcg7d_s0, breaks = 2, labels = c("0","1"))
df$alcg30d_s0<-cut(df$alcg30d_s0, breaks = 2, labels = c("0","1"))
df$gfr_mdrd_s0<-cut(df$gfr_mdrd_s0, breaks = 2, labels = c("0","1"))
df$chol_hdl_s0<-cut(df$chol_hdl_s0, breaks = 2, labels = c("0","1"))
df$alb_crea_u_s0<-cut(df$alb_crea_u_s0, breaks = 2, labels = c("0","1"))
df$menopause_w_s0<-cut(df$menopause_w_s0, breaks = 2, labels = c("0","1"))
df$menostat_w_s0<-cut(df$menostat_w_s0, breaks = 2, labels = c("0","1"))
df$use_mht_w_s0<-cut(df$use_mht_w_s0, breaks = 2, labels = c("0","1"))
df$imt_s0<-cut(df$imt_s0, breaks = 2, labels = c("0","1"))
df$ffs_s0<-cut(df$ffs_s0, breaks = 2, labels = c("0","1"))
df$mcs_sf12_s0<-cut(df$mcs_sf12_s0, breaks = 2, labels = c("0","1"))
df$pcs_sf12_s0<-cut(df$pcs_sf12_s0, breaks = 2, labels = c("0","1"))
df$medic7d_s0<-cut(df$medic7d_s0, breaks = 2, labels = c("0","1"))
df$mort_all_s0<-cut(df$mort_all_s0, breaks = 2, labels = c("0","1"))
df$mort_cvd_s0<-cut(df$mort_cvd_s0, breaks = 2, labels = c("0","1"))
df$mort_chd_s0<-cut(df$mort_chd_s0, breaks = 2, labels = c("0","1"))

df$mort_ca_s0<-cut(df$mort_ca_s0, breaks = 2, labels = c("0","1"))
df$mort_time_s0<-cut(df$mort_time_s0, breaks = 2, labels = c("0","1"))
df$mort_time_birth_s0<-cut(df$mort_time_birth_s0, breaks = 2, labels = c("0","1"))
df$na_s_s0<-cut(df$na_s_s0, breaks = 2, labels = c("0","1"))
df$k_s_s0<-cut(df$k_s_s0, breaks = 2, labels = c("0","1"))
df$ca_s_s0<-cut(df$ca_s_s0, breaks = 2, labels = c("0","1"))
df$mg_s_s0<-cut(df$mg_s_s0, breaks = 2, labels = c("0","1"))
df$crp_s_s0<-cut(df$crp_s_s0, breaks = 2, labels = c("0","1"))
df$som_groe_s2<-cut(df$som_groe_s2, breaks = 2, labels = c("0","1"))
df$som_gew_s2<-cut(df$som_gew_s2, breaks = 2, labels = c("0","1"))
df$mort_time_s0<-as.numeric(df$mort_time_s0)
df$mort_time_s0<-cut(df$mort_time_s0, breaks = 2, labels = c("0","1"))
df$mort_time_birth_s0<-as.numeric(df$mort_time_birth_s0)
df$mort_time_birth_s0<-cut(df$mort_time_birth_s0, breaks = 2, labels = c("0","1"))
df$hrs_s_s0<-as.numeric(df$hrs_s_s0)
df$hrs_s_s0<-cut(df$hrs_s_s0, breaks = 2, labels = c("0","1"))crea_s_s1
df$crea_s_s1<-as.numeric(df$crea_s_s1)
df$crea_s_s1<-cut(df$crea_s_s1, breaks = 2, labels = c("0","1"))
df$hrs_s_s1<-as.numeric(df$hrs_s_s1)
df$hrs_s_s1<-cut(df$hrs_s_s1, breaks = 2, labels = c("0","1"))
df$strata2_s0<-as.numeric(df$strata2_s0)
df$strata2_s0<-cut(df$strata2_s0, breaks = 2, labels = c("0","1"))
df$fpc2_s0<-as.numeric(df$fpc2_s0)
df$fpc2_s0<-cut(df$fpc2_s0, breaks = 2, labels = c("0","1"))
df$kldb_75_s0<-as.numeric(df$kldb_75_s0)
df$kldb_75_s0<-cut(df$kldb_75_s0, breaks = 2, labels = c("0","1"))
df$kldb_75_s0<-as.numeric(df$kldb_75_s0)
df$kldb_75_s0<-cut(df$kldb_75_s0, breaks = 2, labels = c("0","1"))
df$hf_kldb_75_s0<-as.numeric(df$hf_kldb_75_s0)
df$hf_kldb_75_s0<-cut(df$hf_kldb_75_s0, breaks = 2, labels = c("0","1"))
df$doc4wks_s0<-as.numeric(df$doc4wks_s0)
df$doc4wks_s0<-cut(df$doc4wks_s0, breaks = 2, labels = c("0","1"))
df$doc12mths_s0<-as.numeric(df$doc12mths_s0)
df$doc12mths_s0<-cut(df$doc12mths_s0, breaks = 2, labels = c("0","1")) 
df$genintdoc12m_s0<-as.numeric(df$genintdoc12m_s0)
df$genintdoc12m_s0<-cut(df$genintdoc12m_s0, breaks = 2, labels = c("0","1")) onsetsmok_s0
df$onsetsmok_s0<-as.numeric(df$onsetsmok_s0)
df$onsetsmok_s0<-cut(df$onsetsmok_s0, breaks = 2, labels = c("0","1"))
df$ncigd_s0<-as.numeric(df$ncigd_s0)
df$ncigd_s0<-cut(df$ncigd_s0, breaks = 2, labels = c("0","1"))
df$gfr_mdrd_s0<-as.numeric(df$gfr_mdrd_s0)
df$gfr_mdrd_s0<-cut(df$gfr_mdrd_s0, breaks = 2, labels = c("0","1"))
df$ffs_s0<-as.numeric(df$ffs_s0)
df$ffs_s0<-cut(df$ffs_s0, breaks = 2, labels = c("0","1"))
df$som_groe_s2<-as.numeric(df$som_groe_s2)
df$som_groe_s2<-cut(df$som_groe_s2, breaks = 2, labels = c("0","1"))
df$zaehne_s0<-as.numeric(df$zaehne_s0)
df$zaehne_s0<-cut(df$zaehne_s0, breaks = 2, labels = c("0","1"))
df$quick_s2<-as.numeric(df$quick_s2)
df$quick_s2<-cut(df$quick_s2, breaks = 2, labels = c("0","1"))
df$crea_s_s2<-as.numeric(df$crea_s_s2)
df$crea_s_s2<-cut(df$crea_s_s2, breaks = 2, labels = c("0","1"))
df$hrs_s_s2<-as.numeric(df$hrs_s_s2)
df$hrs_s_s2<-cut(df$hrs_s_s2, breaks = 2, labels = c("0","1"))
df$psu_s0<-as.numeric(df$psu_s0)
df$psu_s0<-cut(df$psu_s0, breaks = 2, labels = c("0","1"))
df$strata1_s0<-as.numeric(df$strata1_s0)
df$strata1_s0<-cut(df$strata1_s0, breaks = 2, labels = c("0","1"))
df$fpc1_s0<-as.numeric(df$fpc1_s0)
df$fpc1_s0<-cut(df$fpc1_s0, breaks = 2, labels = c("0","1"))
df$edlevel_s0<-as.numeric(df$edlevel_s0)
df$edlevel_s0<-cut(df$edlevel_s0, breaks = 2, labels = c("0","1"))
df$edyrs_s0<-as.numeric(df$edyrs_s0)
df$edyrs_s0<-cut(df$edyrs_s0, breaks = 2, labels = c("0","1"))
df$sc_sondercodes_s0<-as.numeric(df$sc_sondercodes_s0)
df$sc_sondercodes_s0<-cut(df$sc_sondercodes_s0, breaks = 2, labels = c("0","1"))

str(df$sc_sondercodes_s0)



df$liver_fat<-as.character(df$liver_fat)
df$liver_fat<-as.factor(df$liver_fat)
str(df$liver_fat)


df=as.matrix(df)
df[is.na(df)] <-"None"
df=as.data.frame(df)

export(df, "preprocessed_data.rds")
export(df, "preprocessed_check.csv")

#---------------------End of loading of Dataset-----------------------------

#---------------------Begin of Plotting missing values----------------------------

ggplot_missing <- function(x){
  
  df %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
ggplot_missing(bank.mis)

missmap(bank.mis)

#---------------------Begin of FS:Wrapper:Boruta----------------------------
library(Boruta)

set.seed(222)
boruta<- Boruta(liver_fat~., data = df, doTrace = 2, maxRuns = 500)
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


#data partition
set.seed(123)
row.number <- sample(1:nrow(df), 0.7*nrow(df))
train_rf = df[row.number,]
test_rf= df[-row.number,]
dim(train_rf)
dim(test_rf)

#Random Forest
str(df)
rf1_all<- randomForest(liver_fat~.,data = train_rf)
print(rf1_all)
attributes(rf1_all)
rf1_all$confusion

#prediction
p1_rf_all<-predict(rf1_all,test_rf)
p2_rf_all<-predict(rf1_all,train_rf)
confusionMatrix(p1_rf_all, test_rf$liver_fat)


#random forest with boruta features
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

rf_bor_con<-randomForest(liver_fat ~ age_ship_s0 + som_tail_s0 + som_huef_s0 + hrs_s_s0 + 
                           fs_s0 + stea_alt75_s0 + stea_s0 + age_ship_s1 + som_tail_s1 + 
                           hrs_s_s1 + age_ship_s2 + gout_s2 + som_bmi_s2 + som_tail_s2 + 
                           som_huef_s2 + crea_s_s2 + hrs_s_s2 + gluc_s_s2 + hdl_s_s2 + 
                           stea_alt75_s2 + stea_s2 + atc_c08ca01_s2 + age_ship0_s0 + 
                           strata2_s0 + lvm_s0 + lvmi_s0 + lvh_s0 + avs_s0 + mac_s0 + 
                           fs_risk_s0 + metsyn_s0 + waistc_s0 + waiidf_s0 + imt_s0 + 
                           plaque_s0 + stenos_s0 + mort_time_birth_s0 + som_gew_s2,
                         data = train_rf)

print(rf_bor_con)

#predict

p_bor_con<-predict(rf_bor_con, test_rf)
confusionMatrix(p_bor_con, test_rf$liver_fat)
#------------------------------------------------------------------------------------
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$finalDecision),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)
nrow(boruta.df)
#---------------------End of FS:Wrapper:Boruta------------------------------

#---------------------Begin of of Naive Bayes------------------------------
library(psych)
library(glmnet)
library(Matrix)
library(naivebayes)

df_en<-df

df$sc_sondercodes_s0<-as.numeric(df$sc_sondercodes_s0)
df$sc_sondercodes_s0<-cut(df$sc_sondercodes_s0, breaks = 2, labels = c("0","1"))

pairs.panels(df)

#data partition
set.seed(222)
row.number <- sample(1:nrow(df), 0.7*nrow(df))
train_en = df[row.number,]
test_en= df[-row.number,]
dim(train_en)
dim(test_en)

naive_en<-naive_bayes(liver_fat~.,data = train_en, usekernel = T)
plot(naive_en)
predict_en<-predict(naive_en, train_en)
predict_en_test<-predict(naive_en, test_en)
#confusion matrix - train
(tab1<-table(predict_en, train_en$liver_fat))
1 - sum(diag(tab1))/sum(tab1)

#confusion matrix - test
(tab2<-table(predict_en_test, test_en$liver_fat))
1 - sum(diag(tab2))/sum(tab2)

#---------------------End of of Naive Bayes------------------------------
library(HybridFS)
FS=HybridFS(input.df=df_en,target.var.name="liver_fat")
