
install.packages("forecast")
install.packages("MAPA")
install.packages("smooth")
install.packages("fpp2")
install.packages("imputeTS")
install.packages("dse")
install.packages("rminer")
install.packages("Boruta")
install.packages("clue")
install.packages("doBy")
install.packages("Amelia")
install.packages("knncat")
install.packages("lubridate")
install.packages("gplots")
install.packages("gtools")
install.packages("RColorBrewer")
install.packages("pheatmap")
#install.packages("made4")
install.packages("corrplot")


library(forecast)
library(fpp2)
library(MAPA)
library(smooth)
library(ggplot2)
library(imputeTS)
library (dse)
library (useful)
library(tidyverse) 
library(glmnet) 
library(Boruta)
library(rminer)
library(clue)
library(doBy)
library(Amelia)
library(knncat)
library(lubridate)
library(gplots)
library(gtools)
library(RColorBrewer)
library(pheatmap)
library(corrplot)
#library(made4)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
#   D A T A     P R E P A R A T I O N 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

setwd("C:/Users/s1354719/Documents/BNS/Projects/201806_LTV/Canada/Data")

library(lubridate)
library(tidyverse) 

library(readr)
library(haven)
###### Importing data
#all_data <- read_sas("./SampleWindow_TimeSeries.sas7bdat")
all_data <- read_sas("./randomsample_active_201710.sas7bdat")

data_active <- as.data.frame(all_data)
dim(data_active)
#str(data_active)
names(data_active)
nobs=dim(data_active)[1]
table(data_active$flag_revolver_12m)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

names_ts <- names(data_active %>% select(contains("total_revenue")))
names_ts <- names_ts[c(1:60)]
#names_ts <- rev(names_ts)

names_ts_after <- names(data_active %>% select(contains("rev_aft")))
names_ts_after <- names_ts_after[c(1:12)]


# Building Revenue AFTER metric
data_active$Avg_Revenue_after <- rowMeans(data_active[,c("rev_aft_1","rev_aft_2","rev_aft_3",
                                                         "rev_aft_4","rev_aft_5","rev_aft_6",
                                                         "rev_aft_7","rev_aft_8","rev_aft_9",
                                                         "rev_aft_10","rev_aft_11","rev_aft_12")])
summary(data_active$Avg_Revenue_after)
hist(data_active$Avg_Revenue_after)


data_active$Sum_Revenue_after <- rowSums(data_active[,c("rev_aft_1","rev_aft_2","rev_aft_3",
                                                         "rev_aft_4","rev_aft_5","rev_aft_6",
                                                         "rev_aft_7","rev_aft_8","rev_aft_9",
                                                         "rev_aft_10","rev_aft_11","rev_aft_12")])
summary(data_active$Sum_Revenue_after)
hist(data_active$Sum_Revenue_after)


# Converting string to numeric
#data_active$cad_rate_0 <- as.numeric(data_active$cad_rate_0)
#table(data_active$cad_rate_0)

data_active$Revenue_LastMonth <- data_active$rev_1
#hist(data_active$rev_1)


# Assigning Revenue = $1 to accounts with negative revenue
data_active$Avg_Revenue_after[data_active$Avg_Revenue_after <= 0] <- 0
summary(data_active$Avg_Revenue_after)

data_active$Sum_Revenue_after[data_active$Sum_Revenue_after <= 0] <- 0
summary(data_active$Sum_Revenue_after)


###### Identify Client_ID
#Data_ClientID <- as.data.frame(data_active[,"chd_account_number", "iso_country_code"])
data_ClientID <- data_active[,c("ACCOUNT_NUM")]

###### Identify TARGET
data_active_Y <- data_active[,c("Avg_Revenue_after","Sum_Revenue_after")]
dim(data_active_Y)
names(data_active_Y)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

names(data_active)
# Identify first field from Bureau
position_bureau <- match("CUST_BASE_KEY",names(data_active))
data_wout_bureau <- data_active[,1:(position_bureau-1)]

# Removing sequencial variables
#drop_1 <- names(data_wout_bureau)[grep('_1', names(data_wout_bureau), 1)]
drop_2 <- names(data_wout_bureau)[grep('_2', names(data_wout_bureau), 1)]
drop_3 <- names(data_wout_bureau)[grep('_3', names(data_wout_bureau), 1)]
drop_4 <- names(data_wout_bureau)[grep('_4', names(data_wout_bureau), 1)]
drop_5 <- names(data_wout_bureau)[grep('_5', names(data_wout_bureau), 1)]
drop_6 <- names(data_wout_bureau)[grep('_6', names(data_wout_bureau), 1)]
drop_7 <- names(data_wout_bureau)[grep('_7', names(data_wout_bureau), 1)]
drop_8 <- names(data_wout_bureau)[grep('_8', names(data_wout_bureau), 1)]
drop_9 <- names(data_wout_bureau)[grep('_9', names(data_wout_bureau), 1)]
drop_10 <- names(data_wout_bureau)[grep('_10', names(data_wout_bureau), 1)]
drop_11 <- names(data_wout_bureau)[grep('_11', names(data_wout_bureau), 1)]
drop_12 <- names(data_wout_bureau)[grep('_12', names(data_wout_bureau), 1)]

drop_rev_ts1 <- names(data_wout_bureau)[grep('total_revenue_', names(data_wout_bureau), 1)]
drop_rev_ts2 <- names(data_wout_bureau)[grep('rev_aft', names(data_wout_bureau), 1)]


#all_drop_seq <- c(drop_1,drop_2,drop_3,drop_4,drop_5,drop_6,drop_7,drop_8,drop_9,drop_10,drop_11,drop_12)
all_drop_seq <- c(
  drop_2,drop_3,drop_4,drop_5,drop_6,drop_7,drop_8,drop_9,drop_10,drop_11,drop_12,
  drop_rev_ts1, drop_rev_ts2 
)
length(all_drop_seq)
sort(all_drop_seq)

#all_drop <- c(all_drop_seq,all_drop_bal)
all_drop <- c(all_drop_seq)
#not_for_drop <- all_drop[grep('sum|mean|min|max|std|R2|B_|count_|_q_|avg|rat_|der_|flag|last', all_drop, 1)]
not_for_drop <- all_drop[grep('sum|mean|min|max|std|R2|B_|count_|_q_|avg|der_|flag|last', all_drop, 1)]
length(not_for_drop)
sort(not_for_drop)

#final_drop <- c(all_drop[!all_drop %in% not_for_drop],all_drop_bal)
final_drop <- c(all_drop[!all_drop %in% not_for_drop])
sort(final_drop)

# New data
data_active2 <- as.data.frame(data_active[,!(names(data_active) %in% final_drop)])
dim(data_active2)
names(data_active2)

table(data_active2$flag_revolver_12m)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Identify NUMERIC variables
nums <- unlist(lapply(data_active2, is.numeric))  
data_active_nums <- data_active2[,nums]
dim(data_active_nums)
names(data_active_nums)


# Identify CHARACTER variables
strings <- unlist(lapply(data_active2, is.character))  
data_active_char <- data_active2[,strings]
dim(data_active_char)
names(data_active_char)
#str(data_active_char)


length(unique(data_active$mtg))
unique_values <- function(x){length(unique(x))}
any_missing <- function(x){sum(unique(is.na(x)))}
#unique <- as.data.frame(apply(data_active2,2,unique_values),apply(data_active2,2,any_missing))
unique <- as.data.frame(apply(data_active2,2,unique_values))
varnames_unique_lt5 <- names(data_active2[unique[,1]>3 & unique[,1]<=5])
varnames_unique_lt5



library(dummies)
table(data_active$PRODUCT_CODE)
table(data_active$Sub_Product_Code)
table(data_active$PROBE_ARI)
table(data_active$ACCT_PROV_STATE_CD)
table(data_active$CHANNEL)
table(data_active$GO03_CR_BUREAU_WORST_RT)
table(data_active$FRAUD_ALERT_F)

#table(data_active$num_delq_days_q_3)
#table(data_active$cash_adv_amt_q_3)
#table(data_active$count_utiliz_50perc_q_3)
#table(data_active$count_utiliz_75perc_q_3)
#table(data_active$count_utiliz_90perc_q_3)
#table(data_active$bal_eq_lim_q_3)
#table(data_active$bal_eq_lim_q_6)
#table(data_active$bal_eq_lim_q_9)
#table(data_active$bal_eq_lim_q_12)
#table(data_active$last_15_DPD_q_3)
#table(data_active$last_30_DPD_q_3)
#table(data_active$last_60_DPD_q_3)
#table(data_active$last_90_DPD_q_3)
#table(data_active$useofcash_q_3)
#table(data_active$GO75_EMPRC_EXCLSN_CD)
#table(data_active$PR01_NUM_OF_BNKRPC)
#table(data_active$AT64_NUM_ACT_TRDS_WITH_60_DY)
#table(data_active$Q5_CREDIT_LIM)
#table(data_active$Q5_MOB)
#table(data_active$Q5_Net_RAR)
#table(data_active$Q5_Outstanding_Bal)
#table(data_active$Q5_TotalRevenue)



dummy_ProdCode <- dummy("PRODUCT_CODE", as.data.frame(data_active), sep="_")
dummy_SubProdCode <- dummy("Sub_Product_Code", as.data.frame(data_active), sep="_")
dummy_ProbeARI <- dummy("PROBE_ARI", as.data.frame(data_active), sep="_")
dummy_Province <- dummy("ACCT_PROV_STATE_CD", as.data.frame(data_active), sep="_")
dummy_Channel <- dummy("CHANNEL", as.data.frame(data_active), sep="_")
dummy_BureauWorst <- dummy("GO03_CR_BUREAU_WORST_RT", as.data.frame(data_active), sep="_")
dummy_FraudAlert <- dummy("FRAUD_ALERT_F", as.data.frame(data_active), sep="_")
#names(as.data.frame(dummy_CountryTier))

dummy_NbDelqDays_3m <- dummy("num_delq_days_q_3", as.data.frame(data_active), sep="_")
dummy_CashAdvAmt_3m <- dummy("cash_adv_amt_q_3", as.data.frame(data_active), sep="_")
dummy_CountUtiliz50pct_3m <- dummy("count_utiliz_50perc_q_3", as.data.frame(data_active), sep="_")
dummy_CountUtiliz75pct_3m <- dummy("count_utiliz_75perc_q_3", as.data.frame(data_active), sep="_")
dummy_CountUtiliz90pct_3m <- dummy("count_utiliz_90perc_q_3", as.data.frame(data_active), sep="_")
dummy_BalEqLim_3m <- dummy("bal_eq_lim_q_3", as.data.frame(data_active), sep="_")
dummy_BalEqLim_6m <- dummy("bal_eq_lim_q_6", as.data.frame(data_active), sep="_")
dummy_BalEqLim_9m <- dummy("bal_eq_lim_q_9", as.data.frame(data_active), sep="_")
dummy_BalEqLim_12m <- dummy("bal_eq_lim_q_12", as.data.frame(data_active), sep="_")
dummy_Last15dpd_3m <- dummy("last_15_DPD_q_3", as.data.frame(data_active), sep="_")
dummy_Last30dpd_3m <- dummy("last_30_DPD_q_3", as.data.frame(data_active), sep="_")
dummy_Last60dpd_3m <- dummy("last_60_DPD_q_3", as.data.frame(data_active), sep="_")
dummy_Last90dpd_3m <- dummy("last_90_DPD_q_3", as.data.frame(data_active), sep="_")
dummy_UseOfCash_3m <- dummy("useofcash_q_3", as.data.frame(data_active), sep="_")
dummy_Emprc_Exclsn_CD <- dummy("GO75_EMPRC_EXCLSN_CD", as.data.frame(data_active), sep="_")
dummy_NbBankrpc <- dummy("PR01_NUM_OF_BNKRPC", as.data.frame(data_active), sep="_")
dummy_NbActTrades60d <- dummy("AT64_NUM_ACT_TRDS_WITH_60_DY", as.data.frame(data_active), sep="_")
dummy_CreditLim_Q5 <- dummy("Q5_CREDIT_LIM", as.data.frame(data_active), sep="_")
dummy_MOB_Q5 <- dummy("Q5_MOB", as.data.frame(data_active), sep="_")
dummy_NetRAR_Q5 <- dummy("Q5_Net_RAR", as.data.frame(data_active), sep="_")
dummy_OutsBal_Q5 <- dummy("Q5_Outstanding_Bal", as.data.frame(data_active), sep="_")
dummy_TotalRevenue_Q5 <- dummy("Q5_TotalRevenue", as.data.frame(data_active), sep="_")


data_active_NewDummies <- data.frame(
  dummy_ProdCode
  , dummy_SubProdCode
  , dummy_ProbeARI
  , dummy_Province 
  , dummy_Channel 
  , dummy_BureauWorst
  , dummy_FraudAlert 
  , dummy_NbDelqDays_3m 
  , dummy_CashAdvAmt_3m 
  , dummy_CountUtiliz50pct_3m 
  , dummy_CountUtiliz75pct_3m 
  , dummy_CountUtiliz90pct_3m 
  , dummy_BalEqLim_3m 
  , dummy_BalEqLim_6m 
  , dummy_BalEqLim_9m 
  , dummy_BalEqLim_12m 
  , dummy_Last15dpd_3m 
  , dummy_Last30dpd_3m 
  , dummy_Last60dpd_3m 
  , dummy_Last90dpd_3m 
  , dummy_UseOfCash_3m 
  , dummy_Emprc_Exclsn_CD 
  , dummy_NbBankrpc 
  , dummy_NbActTrades60d 
  , dummy_CreditLim_Q5 
  , dummy_MOB_Q5 
  , dummy_NetRAR_Q5 
  , dummy_OutsBal_Q5 
  , dummy_TotalRevenue_Q5 
)

library(stringr)
#nchar(new_dummies)
# Drop dummies with last character = "_" (missing values in char)
drop_dummy <- names(data_active_NewDummies)[str_sub(names(data_active_NewDummies), start= -1)=="_"]
#valid_dummies <- names(data_active_NewDummies)[!(names(data_active_NewDummies) %in% drop_dummy)]
valid_dummies <- names(data_active_NewDummies)[!(names(data_active_NewDummies) %in% c(drop_dummy))]

#data2 <- data.frame(data, dummy_RevTrans6m, dummy_EverBad, dummy_RiskProfile,dummy_CountryTier)
data_active_flags <- data_active_NewDummies[,valid_dummies]
dim(data_active_flags)
names(data_active_flags)


# Adding new FLAGS to NUMERIC data in data3
#data_active3 <- data.frame(data_active_nums,data_active_flags)
data_active3 <- data.frame(data_active_nums[ , !(names(data_active_nums) %in% varnames_unique_lt5)],data_active_flags)
dim(data_active3)
names(data_active3)
table(data_active3$status)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# # # # # #  Checking for missing values
library(mice) #For Missing Values Exploration
pMiss <- function(x){sum(is.na(x))/length(x)*100}
pct_missing <- as.data.frame(apply(data_active3,2,pMiss))
pct_missing
vars_missing <- names(data_active3)[pct_missing$`apply(data_active3, 2, pMiss)` > 0]
vars_missing


######  Identifying Variables with Missing Values > 30%
#varnames_missing_gt_30pct <- names(data[pct_missing[,1]>30])
varnames_missing_gt_30pct <- names(data_active3[pct_missing[,1]>30])
varnames_missing_gt_30pct
summary(data_active$AS50_TRANSRISK_SCORE)

#REMOVING Variables with Missing Values > 25%
data_active3 <- data_active3[ , !(names(data_active3) %in% varnames_missing_gt_30pct)]
dim(data_active3)
pct_missing <- as.data.frame(apply(data_active3,2,pMiss))
vars_missing <- names(data_active3)[pct_missing$`apply(data_active3, 2, pMiss)` > 0]
vars_missing


# * * * * * * * * * *
# # # # # #  Replacing  missing values (Flags)
#length(unique(data_active3$ccd))
#sum(unique(is.na(data_active3$ccd)))

flag_impute <- 0
for(i in 1:length(vars_missing)) 
{
  #i <- 211
  # statistics to replace outliers
  l <- length(unique(data_active3[,vars_missing[i]]))
  s <- sum(unique(is.na(data_active3[,vars_missing[i]])))
  
  if (l <= 3 & s > 0) 
  {
    data_active3[is.na(data_active3[,vars_missing[i]]),vars_missing[i]] <- 0
    #is.na(data_active3[,vars_missing[i]]) <- 0
    flag_impute <- flag_impute + 1
  } 
}
flag_impute
#length(unique(data_active3$chq))
#sum(unique(is.na(data_active3$chq)))
#data_active3[is.na(data_active3[,'chq']),'chq'] <- 0


# * * * * * * * * * *
# # # # # #  Replacing  missing values (Continuous)
pct_missing <- as.data.frame(apply(data_active3,2,pMiss))
vars_missing <- names(data_active3)[pct_missing$`apply(data_active3, 2, pMiss)` > 0]
vars_missing

Vars_For_Mean_Imput <- vars_missing
summary(data_active3[,Vars_For_Mean_Imput])
for(j in 1:length(Vars_For_Mean_Imput)) 
{ 
  data_active3[is.na(data_active3[,Vars_For_Mean_Imput[j]]),Vars_For_Mean_Imput[j]]=colMeans(data_active3[,Vars_For_Mean_Imput],na.rm=TRUE)[j]
}  
summary(data_active3[,Vars_For_Mean_Imput])

table(data_active3$flag_revolver_12m)
table(data_active3$DLQNT_FLAG)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# # # # # # Identify and replace outliers (only for CONT vars)

library(Hmisc)
#describe(data_active3$Avg_Revenue_after)
library(timeDate)
names(data_active3)
#kurtosis(data_active3$Avg_Revenue_after)
#hist(data_active3$Avg_Revenue_after)

#library(randomForest)
#library(tsoutliers)
#outlier(data_active3$flag_cash12)

Vars_For_Outlier_Imput <- names(data_active3)
Vars_For_Outlier_Imput

avg_cont1 <- as.data.frame(colMeans(data_active3[,Vars_For_Outlier_Imput]))
count_vars_replace <- 0
var_trimmed <- rep(0,length(Vars_For_Outlier_Imput))

for(i in 1:ncol(data_active3[,Vars_For_Outlier_Imput])) 
{
  #i <- 4
  # statistics to replace outliers
  lowerq = quantile(as.data.frame(data_active3[,Vars_For_Outlier_Imput])[,i],na.rm = TRUE)[2]
  upperq = quantile(as.data.frame(data_active3[,Vars_For_Outlier_Imput])[,i],na.rm = TRUE)[4]
  iqr = upperq - lowerq
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  
  quantiles <- quantile(as.data.frame(data_active3[,Vars_For_Outlier_Imput])[,i], c(.01, .95 ),na.rm = TRUE)

  #if ((sum(data_active3[,i] > quantiles[2]) > 0 | 
  #     kurtosis(data_active3[,i]) > 3) &
  #    length(unique(data_active3[,i])) > 10
  #) 

  if ((sum(as.data.frame(data_active3[,Vars_For_Outlier_Imput])[,i] > quantiles[2]) > 0 | 
       kurtosis(as.data.frame(data_active3[,Vars_For_Outlier_Imput])[,i]) > 3) &
      length(unique(as.data.frame(data_active3[,Vars_For_Outlier_Imput])[,i])) > 10
  ) 
  {
    count_vars_replace <- count_vars_replace+1
    var_trimmed[i] <- 1
  
    data_active3[,Vars_For_Outlier_Imput][as.data.frame(data_active3[,Vars_For_Outlier_Imput])[,i] > quantiles[2],i] <- quantiles[2]
    data_active3[,Vars_For_Outlier_Imput][as.data.frame(data_active3[,Vars_For_Outlier_Imput])[,i] < quantiles[1],i] <- quantiles[1]
  } 
}

#table(data_active3$DLQNT_FLAG)

#summary(data_wout_nzv[,Vars_For_Outlier_Imput])
count_vars_replace
var_trimmed
names(data_active3)[var_trimmed==1]
sort(names(data_active3)[var_trimmed==1])

# Compare ORIGINAL vs TRIMMED average
avg_cont2 <- as.data.frame(colMeans(data_active3[,Vars_For_Outlier_Imput]))
head(cbind(avg_cont1,avg_cont2),50)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# IDENTIFY constants in dataset (Check Near-Zero-Variance predictors)
library(caret)
check_nearzerovar <- nearZeroVar(data_active3, 
                                 freqCut = 95/5, uniqueCut = 10, 
                                 saveMetrics = TRUE, names= TRUE)
sum(check_nearzerovar$zeroVar)
sum(check_nearzerovar$nzv)
tail(check_nearzerovar,200)
table(data_active3$bal_eq_lim_q_3)
check_nearzerovar


zeroVar_vars <- names(data_active3)[check_nearzerovar$zeroVar==1]
nzv_vars <- names(data_active3)[check_nearzerovar$nzv=='TRUE']

#beer_train_zeroVar <- data_nums[ , (names(data_nums) %in% zeroVar_vars)]

total_zeroVar_vars <- c(zeroVar_vars,nzv_vars)
sort(total_zeroVar_vars)
total_zeroVar_vars<- total_zeroVar_vars[total_zeroVar_vars != 'DLQNT_FLAG']
#total_zeroVar_vars<- total_zeroVar_vars[total_zeroVar_vars != 'num_delq_days_0']
#total_zeroVar_vars<- total_zeroVar_vars[total_zeroVar_vars != 'num_delq_days_1']
total_zeroVar_vars<- total_zeroVar_vars[total_zeroVar_vars != 'survival_months']


###### REMOVE constants in dataset
data_active_wout_nzv <- data_active3[, !(names(data_active3) %in% total_zeroVar_vars)]
dim(data_active_wout_nzv)
dim(data_active3)
dim(data_active2)
dim(data_active)
names(data_active_wout_nzv)
table(data_active_wout_nzv$flag_revolver_12m)
table(data_active_wout_nzv$DLQNT_FLAG)


# Checking all variables with variance ZERO
variance <- as.data.frame(apply(data_active_wout_nzv,2,var))
varnames_variance_gt0 <- names(data_active_wout_nzv[variance[,1]==0])
varnames_variance_gt0


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Manual drop

sort(names(data_active_wout_nzv))
manual_drop <- c(
  "BILL_DAY"
  , "lastac_date"
  , "open_date" 
  , "survival_days"
  , "CUST_BASE_KEY"
  , "status"
  , "status_final"
  , "max_newprofi"
  , "max_profi"
  , "sum_profi_x"
  , "profile_12m"
  , "Province"
  , "survival_months" 
)

data_active4 <- data_active_wout_nzv[,!(names(data_active_wout_nzv) %in% manual_drop)]
dim(data_active4)
names(data_active4)

#table(data_active4$sum_profi_rev)
#length(unique(data_active4$sum_profi_rev))

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

unique_values <- function(x){length(unique(x))}
unique <- as.data.frame(apply(data_active4,2,unique_values))
varnames_unique_gt <- names(data_active4[unique[,1]>5])

# IDENTIFY and REMOVE highly correlated variables 
data_active_for_Corr <- data_active4[,varnames_unique_gt]
#data_activeCorr <- cor(data_active_for_Corr, use="pairwise.complete.obs")
data_activeCorr <- cor(data_active_for_Corr)
#head(data_activeCorr)
corr_cutoff <- 0.6
highCorr <- findCorrelation(data_activeCorr, corr_cutoff, verbose = FALSE, names = FALSE)
length(highCorr)
highCorr_vars<- names(data_active_for_Corr[highCorr])
highCorr_vars
sort(highCorr_vars)

# TODO: Review variables again with entire data
highCorr_vars<- highCorr_vars[highCorr_vars != 'sum_profi_rev']
highCorr_vars<- highCorr_vars[highCorr_vars != 'balance']
highCorr_vars<- highCorr_vars[highCorr_vars != 'total_products']
highCorr_vars<- highCorr_vars[highCorr_vars != 'balance_mean_6']
highCorr_vars<- highCorr_vars[highCorr_vars != 'bureau_score_0']
highCorr_vars<- highCorr_vars[highCorr_vars != 'cash_adv_amt_mean_12']
highCorr_vars<- highCorr_vars[highCorr_vars != 'credit_lim_1']
highCorr_vars<- highCorr_vars[highCorr_vars != 'credit_lim_std_12']
highCorr_vars<- highCorr_vars[highCorr_vars != 'mths_since_lastpaym']
highCorr_vars<- highCorr_vars[highCorr_vars != 'balance_1']
highCorr_vars<- highCorr_vars[highCorr_vars != 'mths_since_lastact']
highCorr_vars<- highCorr_vars[highCorr_vars != 'utiliz_rate_1']
highCorr_vars<- highCorr_vars[highCorr_vars != 'avg_cash_6']
highCorr_vars<- highCorr_vars[highCorr_vars != 'balance_max_6']
highCorr_vars<- highCorr_vars[highCorr_vars != 'bureau_score_mean_6']
highCorr_vars<- highCorr_vars[highCorr_vars != 'B_12_bureau_score_']
highCorr_vars<- highCorr_vars[highCorr_vars != 'B_6_total_revenue_']
highCorr_vars<- highCorr_vars[highCorr_vars != 'count_utiliz_75perc_q_12']
highCorr_vars<- highCorr_vars[highCorr_vars != 'crg_1']
highCorr_vars<- highCorr_vars[highCorr_vars != 'payment_amt_sum_6']
highCorr_vars<- highCorr_vars[highCorr_vars != 'payment_amt_std_12']
highCorr_vars<- highCorr_vars[highCorr_vars != 'R2_12_bureau_score_']
highCorr_vars<- highCorr_vars[highCorr_vars != 'R2_6_total_revenue_']
highCorr_vars<- highCorr_vars[highCorr_vars != 'sum_cash_6']
highCorr_vars<- highCorr_vars[highCorr_vars != 'tr_online_txn']
highCorr_vars<- highCorr_vars[highCorr_vars != 'useofcash_q_12']
highCorr_vars<- highCorr_vars[highCorr_vars != 'total_revenue_std_6']
highCorr_vars<- highCorr_vars[highCorr_vars != 'total_revenue_mean_6']
highCorr_vars<- highCorr_vars[highCorr_vars != 'mths_since_lastpaym']
highCorr_vars<- highCorr_vars[highCorr_vars != 'num_delq_days_q_6']
sort(highCorr_vars)


######  NEW DATA (Removing highly correlated variables)
data_active5 <- data_active4[ , !(names(data_active4) %in% highCorr_vars)]
dim(data_active5)
names(data_active5)
sort(names(data_active5))
table(data_active5$flag_revolver_12m)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Assign cluster for time series and create dummys
valid_ts_train <- data_active[,c(names_ts)]
names(valid_ts_train)
for(k in 1:nrow(valid_ts_train))
{
  #k<-2
  # Identifying outliers (some forecasting models are giving very volatile predictions)
  quant_bottom <- 0.01
  quant_top <- 0.99
  outlier1_id <- as.numeric(which((as.numeric(valid_ts_train[k,]) > quantile(valid_ts_train[k,],quant_top,na.rm = TRUE)[[1]])))
  outlier2_id <- as.numeric(which((as.numeric(valid_ts_train[k,]) < 0)))
  #outlier_id
  valid_ts_train[k,outlier1_id] <- quantile(valid_ts_train[k,],quant_top,na.rm = TRUE)[[1]]
  # valid_ts_train[k,outlier2_id] <- 0
}

dim(valid_ts_train)
min(valid_ts_train)
log_ts_train <- log(valid_ts_train + 1.00001)
head(log_ts_train)
dim(log_ts_train)
log_ts_train[1,]

sep_ts_train <- log_ts_train
dim(sep_ts_train)
#sep_ts_train[3,]

# Assign each observation in TEST to the highest correlated time series in TRAIN 

Avg_ts_cluster <- readRDS(file = "avg_ts_mv_seg.rds")

stop <- nrow(sep_ts_train)
#stop <- 7
#sep_ts_train[stop,]
ts_cluster_mv <- rep(0,stop)
for(r in 1:stop)
{
  #r <- 1
  nb_ts_clus <- nrow(Avg_ts_cluster)
  df_newobs <- rbind(setNames(Avg_ts_cluster[,2:61],names(sep_ts_train)),sep_ts_train[r,])
  
  if ((rowSums(is.na(sep_ts_train[r,])) < length(sep_ts_train[r,])-6) & (rowSums(sep_ts_train[r,], na.rm = TRUE) > 1))
  {
    corr_new <- cor(t(df_newobs), use = "na.or.complete")
    corr_new[nb_ts_clus+1,]
    
    max_corr_ts <- as.numeric(which(corr_new[nb_ts_clus+1,] == max(corr_new[nb_ts_clus+1,1:nb_ts_clus])))
    #max_corr_ts
    #plot(as.numeric(df_newobs[max_corr_ts,c(1:60)]))
    #lines(as.numeric(df_newobs[nb_ts_clus+1,c(1:60)]))
    ts_cluster_mv[r] <- max_corr_ts
  } else { ts_cluster_mv[r] <- nb_ts_clus + 1 }
  #ts_cluster_mv[r]
  #sep_ts_train[r,]
  
}

table(ts_cluster_mv)
#sep_ts_train$ts_cluster_mv <- ts_cluster_mv
data_active$ts_cluster_mv <- ts_cluster_mv

dummy_ts_cluster <- dummy("ts_cluster_mv", as.data.frame(data_active), sep="_")
data_active5 <- data.frame(data_active5,dummy_ts_cluster)
#data_active5 <- data_active5[ , !(names(data_active5) %in% c("ts_cluster_mv"))]
names(data_active5)



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

library(tidyverse) 
names(data_active)
data_active_X <- data.frame(data_active5,
                            data_active_Y[1],
                            data_active[,names_ts],
                            data_active[,names_ts_after])

names(data_active_X)

#names(data_active_X)[names(data_active_X)=="Avg_Revenue_after"] <- 'Target'
#names(data_active_X)[names(data_active_X)=="data_active.dpd_1"] <- 'dpd_1'

data_active_X$Target <- log(1 + data_active_X$Avg_Revenue_after)

summary(data_active_X$Avg_Revenue_after)
summary(data_active_X$Target)
hist(data_active_X$Target)
dim(data_active_X)

#library(tidyverse) 
#data_active_X <- data_active_X %>% select(-contains("rev_aft1.1"))


###### Data partition (Train vs Test)
data_active_X$seq <- seq(1, dim(data_active_X)[1])
# % of the sample size to be selected
pct_sample <- 0.6
smp_size <- floor(pct_sample * nrow(data_active_X))
# set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_active_X)), size = smp_size)


train <- data_active_X[train_ind, ]
test <- data_active_X[-train_ind, ]
dim(data_active)
dim(train)
dim(test)
names(train)
head(train)
#head(test)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

library(tidyverse) 
train_active <- train[,!(names(train) %in% names_ts)]
train_active <- train_active[,!(names(train_active) %in% names_ts_after)]
sort(names(train_active))

train_active <- train_active %>% select(-contains("Avg_Revenue_after"))
#train_active <- train_active %>% select(-contains("total_revenue_"))
#train_active <- train_active %>% select(-contains("rev_aft"))
train_active <- train_active %>% select(-contains("survival_months"))
train_active <- train_active %>% select(-contains("seq"))
train_active <- train_active %>% select(-contains("credit_lim_min_12"))
train_active <- train_active %>% select(-contains("sum_profi_trans"))
train_active <- train_active %>% select(-contains("total_merch_amt_std_3"))
train_active <- train_active %>% select(-contains("total_merch_amt_min_12"))
train_active <- train_active %>% select(-contains("new_inactive_0"))
train_active <- train_active %>% select(-contains("R2_3_balance_"))
train_active <- train_active %>% select(-contains("new_inactive_1"))
train_active <- train_active %>% select(-contains("new_transactor_0"))
train_active <- train_active %>% select(-contains("revolver_sum_3"))
train_active <- train_active %>% select(-contains("nir_0"))
#train_active <- train_active %>% select(-contains("revolver"))
train_active <- train_active %>% select(-contains("revolver_max"))
train_active <- train_active %>% select(-contains("revolver_min"))
train_active <- train_active %>% select(-contains("revolver_std"))
train_active <- train_active %>% select(-contains("revolver_0"))
train_active <- train_active %>% select(-contains("new_revolver_0"))

train_active <- train_active[ , !(names(train_active) %in% c("revolver"))]

sort(names(train_active))

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

#install.packages("Boruta")
library(Boruta)

#names(train_active)
sort(names(train_active))
set.seed(123)
boruta.train <- Boruta(Target~., data = train_active, doTrace = 2, maxRuns=30)
print(boruta.train)
#boruta.train$finalDecision

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)
tail(boruta.df[order(boruta.df$meanImp),],40)


boruta_decision <- boruta.df$decision
boruta.df$meanImp

#boruta_df <- data.frame(names(train_active)[1:74],boruta.df$meanImp,boruta_decision)
boruta_df <- data.frame(names(train_active[, !names(train_active) %in% c("Target")]),boruta.df$meanImp,boruta_decision)
names(boruta_df)
drivers_boruta <- boruta_df[order(boruta_df$boruta.df.meanImp, decreasing = T),1]
sort(drivers_boruta)

n_top_boruta <- 40
top_drivers_boruta <- as.character(drivers_boruta[1:n_top_boruta])
top_drivers_boruta
sort(top_drivers_boruta)

saveRDS(top_drivers_boruta, file = "top_drivers_revenue.rds")
top_drivers_boruta <- readRDS(file = "top_drivers_revenue.rds")


#par("mar")
par(mar=c(7,4,2,2))
par(mfrow = c(1, 1))
plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i) 
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), 
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.5)

selected_boruta <- getSelectedAttributes(final.boruta, withTentative = F)
selected_boruta


saveRDS(boruta.train, file = "boruta.train.rds")
boruta.train <- readRDS(file = "boruta.train.rds")


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Final Revenue drivers based on BORUTA algorithm
#names(train_active)
#final_rev_drivers <- c (
#  "bal_mean_12", "CHD_CURRENT_BALANCE", 
#  "rev_1", 
#  "Utiliz_1", 
#  "flag_revolver_12m", "profi_1_R",
#  "sc_1", 
#  "NbMonths_tenure", "dpd_1", "pc_balpaid_SPL", "N_SPL", "Income", 
#  "rev_mean_6", "rev_std_12", "B_9_rev_", "flag_cash6", "B_9_bal_"
#)


#train_drivers <- data.frame(train_active[,c(final_rev_drivers)])
#names(train_drivers)
#dim(train_drivers)

names_ts_cluster <- names(train_active %>% select(contains("ts_cluster_mv")))

train_drivers <- data.frame(train_active[,c(top_drivers_boruta,names_ts_cluster)])
dim(train_drivers)
names(train_drivers)
names(train_active)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Choosing optimal number of cluster of Revenue drivers segments (using k-Nearest Neighbors)
library(caret)
library(e1071)

names(train_drivers)
dim(train_drivers)
names(train_active)

#selected_boruta
train_drivers <- data.frame(train_active[,c(names(train_drivers),"Target")])
names(train_drivers)
#train_boruta <- data.frame(train_active[,c(top_drivers_boruta,"Target")])
#names(train_boruta)
#boruta.df[order(boruta.df$meanImp),]
#dim(train_boruta)

# Setting up train controls
repeats <- 3
numbers <- 10
tunel <- 20

set.seed(1234)
x <- trainControl(method = "repeatedcv", number = numbers, repeats = repeats, classProbs = FALSE, summaryFunction = twoClassSummary)

model1 <- train(Target ~ . , data = train_drivers, method = "knn",
#model1 <- train(Target ~ . , data = train_boruta, method = "knn",
                preProcess = c("center","scale"),
                trControl = trainControl(),
                metric = "Rsquared",
                tuneLength = tunel)

# Summary of model
model1
plot(model1)
#str(model1)
model1$results

#x <- predict(model1,test)

#nb.cl <- as.numeric(model1$bestTune)

vec_rsq <- model1$results["Rsquared"][[1]]
(vec_rsq/vec_rsq[1])-1

saveRDS(model1, file = "model1.rds")
# Restore the object
model1 <- readRDS(file = "model1.rds")

nb.cl <- 9

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Clustering of Revenue drivers segments (K-MEANS)
library(stats)
library(caret)
names(train_drivers)
dim(train_drivers)

# Normalizing the train data with key drivers only for k-means clustering:
#scale_train_drivers <- scale(train[,names(train_drivers)])
#scale_train_drivers <- scale(train_boruta)
scale_train_drivers <- scale(train_drivers)

# Establishing variable to store K-means clustering:
clusters.sum.squares <- rep(0.0, 30)
# Setting up cluster parameters:
cluster.params <- 2:32
set.seed(893247)
for (i in cluster.params) {
  kmeans.temp <- kmeans(scale_train_drivers, centers = i)
  clusters.sum.squares[i - 1] <- sum(kmeans.temp$withinss)
}   
clusters.sum.squares

# Plot our scree plot using the mighty ggplot2.
ggplot(NULL, aes(x = cluster.params, y = clusters.sum.squares)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  labs(x = "Number of Clusters",
       y = "Cluster Sum of Squared Distances",
       title = "Revenue Segments Scree Plot")


# * * * * * * * * * * * * * * * * * * 
# K-Means clustering
RevDrivers.kmeans <- kmeans(scale_train_drivers, centers = nb.cl, algorithm = "Hartigan-Wong")
RevDrivers.kmeans[[2]]
prop.table(RevDrivers.kmeans$size)
plot(prop.table(RevDrivers.kmeans$size),type = 'h')

train$RevDrivers_Clusters <- as.factor(RevDrivers.kmeans$cluster)
mean_table_train <- aggregate(train$Target, list(train$RevDrivers_Clusters), mean)
aggregate(train$Target, list(train$RevDrivers_Clusters), median)
plot(mean_table_train)
table(train$RevDrivers_Clusters)

# Visualizing survivability by cluster assignment.:
#ggplot(train, aes(x = RevDrivers_Clusters, fill = Target)) +
#  theme_bw() + geom_bar() +
#  labs(x = "Cluster Assignment",
#       y = "Avg Revenue Next 12m",
#       title = "Results by Cluster")

# Save object
saveRDS(RevDrivers.kmeans, file = "RevDrivers.kmeans.rds")
# Restore the object
RevDrivers.kmeans <- readRDS(file = "RevDrivers.kmeans.rds")


# * * * * * * * * * * * * * * * * * * 
# AVERAGE of Revenue drivers per K-means cluster
RevDrivers.kmeans[[2]]
names(train_drivers)
table(train$RevDrivers_Clusters)
summary(scale_train_drivers)

library(doBy)
Avg_Driver_Cluster <- summaryBy(
  train_drivers  ~ 
    RevDrivers_Clusters, 
  data = train, FUN = function(x) { c(m = mean(x, na.rm=TRUE)) })
Avg_Driver_Cluster

Avg_ScaledDriver_Cluster <- summaryBy(
  scale_train_drivers  ~ 
    RevDrivers_Clusters, 
  data = train, FUN = function(x) { c(m = mean(x, na.rm=TRUE)) })
Avg_ScaledDriver_Cluster

#plot(Avg_ScaledDriver_Cluster)
#Avg_ScaledDriver_Cluster <- as.matrix(Avg_ScaledDriver_Cluster)


# * * * * * * * * * * * * * * * * * * 
# Applying clustering to TEST dataset
library(clue)
names(test[,names(train_drivers)])
scale_test_drivers <- scale(test[,names(train_drivers)])

test$kmeans_clusters <- cl_predict(RevDrivers.kmeans, scale_test_drivers)
#warnings()
#traceback()
#class(kmeans_clusters_test)
prop.table(table(test$kmeans_clusters))
plot(prop.table(table(test$kmeans_clusters)),type = 'h')

test$kmeans_clusters <- as.factor(test$kmeans_clusters)
mean_table_test <- aggregate(test$Target, list(test$kmeans_clusters), mean)
aggregate(test$Target, list(test$kmeans_clusters), median)
plot(mean_table_test)

par(mfrow = c(2, 2))
plot(mean_table_train,type = 'h',xlab="Revenue Segment", ylab="Avg Revenue (Log)",main="Avg Revenue per Segment (Train dataset)")
plot(prop.table(RevDrivers.kmeans$size),type = 'h',xlab="Revenue Segment", ylab="Segment size distribution",main="Account distribution per Segment (Train dataset)")
plot(mean_table_test,type = 'h',xlab="Revenue Segment", ylab="Avg Revenue (Log)",main="Avg Revenue per Segment (Test dataset)")
plot(prop.table(table(test$kmeans_clusters)),type = 'h',xlab="Revenue Segment", ylab="Segment size distribution",main="Account distribution per Segment (Test dataset)")
par(mfrow = c(1, 1))

library(caret)
diff_avg_target <- abs(mean_table_train$x - mean_table_test$x)
pct_diff_target <- diff_avg_target / mean_table_test$x
err_avg_target <- RMSE(mean_table_train$x, mean_table_test$x)
err_avg_target


mean_table_train
mean_table_test
prop.table(RevDrivers.kmeans$size)
prop.table(table(test$kmeans_clusters))


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# TIME SERIES FORECASTING

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

dim(train)
names(train)
#test$ts_cluster_mv <- ts_test_mv$ts_cluster_mv
#table(test$kmeans_clusters)

# Average time series for ACCT LEVEL segment
# summary for PREZ (acct level clustering)
library(doBy)
rev_after_acctseg_train <- summaryBy(
  total_revenue_59 + total_revenue_58 + total_revenue_57 + total_revenue_56 + total_revenue_55 + 
    total_revenue_54 + total_revenue_53 + total_revenue_52 + total_revenue_51 + total_revenue_50 + 
    total_revenue_49 + total_revenue_48 + total_revenue_47 + total_revenue_46 + total_revenue_45 + 
    total_revenue_44 + total_revenue_43 + total_revenue_42 + total_revenue_41 + total_revenue_40 + 
    total_revenue_39 + total_revenue_38 + total_revenue_37 + total_revenue_36 + total_revenue_35 + 
    total_revenue_34 + total_revenue_33 + total_revenue_32 + total_revenue_31 + total_revenue_30 + 
    total_revenue_29 + total_revenue_28 + total_revenue_27 + total_revenue_26 + total_revenue_25 + 
    total_revenue_24 + total_revenue_23 + total_revenue_22 + total_revenue_21 + total_revenue_20 + 
    total_revenue_19 + total_revenue_18 + total_revenue_17 + total_revenue_16 + total_revenue_15 + 
    total_revenue_14 + total_revenue_13 + total_revenue_12 + total_revenue_11 + total_revenue_10 + 
    total_revenue_9 + total_revenue_8 + total_revenue_7 + total_revenue_6 + total_revenue_5 + 
    total_revenue_4 + total_revenue_3 + total_revenue_2 + total_revenue_1 + total_revenue_0 + 
    rev_aft_1 + rev_aft_2 + rev_aft_3 + rev_aft_4 + rev_aft_5 + rev_aft_6 + rev_aft_7 + rev_aft_8 + 
    rev_aft_9 + rev_aft_10 + rev_aft_11 + rev_aft_12
  ~ 
    RevDrivers_Clusters, 
  data = train, FUN = function(x) { c(m = mean(x, na.rm=TRUE)) })
rev_after_acctseg_train


dim(test)
names(test)
#test$ts_cluster_mv <- ts_test_mv$ts_cluster_mv
#table(test$kmeans_clusters)

rev_after_acctseg_test <- summaryBy(
  total_revenue_59 + total_revenue_58 + total_revenue_57 + total_revenue_56 + total_revenue_55 + 
    total_revenue_54 + total_revenue_53 + total_revenue_52 + total_revenue_51 + total_revenue_50 + 
    total_revenue_49 + total_revenue_48 + total_revenue_47 + total_revenue_46 + total_revenue_45 + 
    total_revenue_44 + total_revenue_43 + total_revenue_42 + total_revenue_41 + total_revenue_40 + 
    total_revenue_39 + total_revenue_38 + total_revenue_37 + total_revenue_36 + total_revenue_35 + 
    total_revenue_34 + total_revenue_33 + total_revenue_32 + total_revenue_31 + total_revenue_30 + 
    total_revenue_29 + total_revenue_28 + total_revenue_27 + total_revenue_26 + total_revenue_25 + 
    total_revenue_24 + total_revenue_23 + total_revenue_22 + total_revenue_21 + total_revenue_20 + 
    total_revenue_19 + total_revenue_18 + total_revenue_17 + total_revenue_16 + total_revenue_15 + 
    total_revenue_14 + total_revenue_13 + total_revenue_12 + total_revenue_11 + total_revenue_10 + 
    total_revenue_9 + total_revenue_8 + total_revenue_7 + total_revenue_6 + total_revenue_5 + 
    total_revenue_4 + total_revenue_3 + total_revenue_2 + total_revenue_1 + total_revenue_0 + 
    rev_aft_1 + rev_aft_2 + rev_aft_3 + rev_aft_4 + rev_aft_5 + rev_aft_6 + rev_aft_7 + rev_aft_8 + 
    rev_aft_9 + rev_aft_10 + rev_aft_11 + rev_aft_12
  ~ 
    kmeans_clusters, 
  data = test, FUN = function(x) { c(m = mean(x, na.rm=TRUE)) })
rev_after_acctseg_test


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
#Avg_ts_2 <- rev_after_acctseg_train
#Avg_ts_test2 <- rev_after_acctseg_test

names(rev_after_acctseg_train)
Avg_ts_2 <- rev_after_acctseg_train[,c(1:61,62:73)]
Avg_ts_test2 <- rev_after_acctseg_test[,c(1:61,62:73)]
names(Avg_ts_2)

Avg_ts_2[,61] <- (Avg_ts_2[,60] + Avg_ts_2[,62])/2
Avg_ts_test2[,61] <- (Avg_ts_test2[,60] + Avg_ts_test2[,62])/2

plot(as.numeric(Avg_ts_2[1,2:73]),type='l')
plot(as.numeric(Avg_ts_2[2,2:73]),type='l')
plot(as.numeric(Avg_ts_2[3,2:73]),type='l')
plot(as.numeric(Avg_ts_2[4,2:73]),type='l')
plot(as.numeric(Avg_ts_2[5,2:73]),type='l')
plot(as.numeric(Avg_ts_2[6,2:73]),type='l')
plot(as.numeric(Avg_ts_2[7,2:73]),type='l')
plot(as.numeric(Avg_ts_2[8,2:73]),type='l')
plot(as.numeric(Avg_ts_2[9,2:73]),type='l')

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Build ARIMA model for time series in each Revenue segment
library(forecast)
library(zoo)
library(lubridate)
library(caret)

#Avg_ts_2 <- Avg_ts_test2
head(Avg_ts_2)
names(Avg_ts_2)
nb_ts <- dim(Avg_ts_2)[1]
#nb_ts <- 1

# * * * * * * * 
start_ts <- 1
end_ts <- dim(Avg_ts_2)[2]
nbm2predict <- 12



# * * * * * * * 
avg_ts_na <- Avg_ts_2[rowSums(is.na(Avg_ts_2)) > 0,]
dim(avg_ts_na)
head(avg_ts_na,10)

#Avg_ts_2$rowsum_ts <- rowSums(Avg_ts_2[,2:end_ts])
#Avg_ts_eq_0 <- Avg_ts_2[Avg_ts_2$rowsum_ts == 0,1:(xend_ts-xnbm2predict)]
#dim(Avg_ts_eq_0)


# * * * * * * * 
# Create monthly dummies.  Add other xvars to this matrix
xts <- as.numeric(Avg_ts_2[1,(start_ts+1):(end_ts-nbm2predict)])
xts <- ts(xts,end=c(2017, 10), frequency = 12)
#xts <- ts(xts,start=c(2012, 11), frequency = 12)
month <- month(as.Date(time(xts)))
xreg <- model.matrix(~as.factor(month))[,(start_ts+1):12]
colnames(xreg) <- c('Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
head(xreg)
tail(xreg)

nb_years <- 5
month_val <- c((nbm2predict-1):12,rep(1:12,nb_years))
#month_val <- c((nbm2predict-3):12,1:12,1:12,1:12)
#month_val <- c(3:12,1:12,1:12,1:12)
h_val <- length(month_val)
xreg_val <- model.matrix(~as.factor(month_val))[,(start_ts+1):12]
colnames(xreg_val) <- c('Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
head(xreg_val)
tail(xreg_val)

xts_test <- as.numeric(Avg_ts_test2[1,(start_ts+1):(end_ts-nbm2predict)])
xts_test <- ts(xts_test,end=c(2017, 10), frequency = 12)
month_test <- month(as.Date(time(xts_test)))
xreg_test <- model.matrix(~as.factor(month))[,(start_ts+1):12]
colnames(xreg_test) <- c('Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
head(xreg_test)
tail(xreg_test)



# * * * * * * * 
AvgError1 <- rep(NA,nb_ts)
AvgError2 <- rep(NA,nb_ts)
rmse <- rep(NA,nb_ts)

# Build ARIMA model for time series in each Revenue segment
for(k in 1:nb_ts)
{
  #k <- 1
  xts_k <- as.numeric(Avg_ts_2[k,(start_ts+1):(end_ts-nbm2predict)])
  xts_k <- ts(xts_k,end=c(2017, 10), frequency = 12)
  assign(paste("xts", k, sep = "_"), xts_k)
  ts_k <- log(xts_k+1.00001)
  ts_k <- ts(ts_k,end=c(2017, 10), frequency = 12)
  assign(paste("ts", k, sep = "_"), ts_k)
  #plot(ts_k, type='l')
  nbm <- length(ts_k)
  DateTime <- ISOdate(2015,3,1)+(1:nbm*60*60*24*30)
  
  # Identify Seasonality
  #fit <- tbats(ts_k)
  #seasonal <- !is.null(fit$seasonal)
  #seasonal
  
  fit_autoarima_k <- auto.arima(ts_k,
                                stepwise=FALSE, 
                                approximation=TRUE, 
                                trace=TRUE, 
                                lambda="auto", 
                                seasonal = TRUE, stationary = FALSE, xreg = xreg,
                                max.p =10, max.q =10, max.P =10, max.Q =10)
  assign(paste("fit_autoarima", k, sep = "_"), fit_autoarima_k)
  
  saveRDS(assign(paste("fit_autoarima", k, sep = "_"), fit_autoarima_k), 
          file = paste("fit_autoarima", k, ".rds", sep = "_"))
  
  pred_k <- forecast(fit_autoarima_k,h=h_val,xreg=xreg_val)
  forecast_k <- pred_k$mean
  assign(paste("forecast", k, sep = "_"), forecast_k)
  #}
  
  # Extracting TESt data (201503-2010802) to validate model
  #k<-1  
  xts_test_k <- as.numeric(Avg_ts_test2[k,(start_ts+1):(end_ts-nbm2predict)])
  xts_test_k <- ts(xts_test_k,end=c(2017, 10), frequency = 12)
  assign(paste("xts_test", k, sep = "_"), xts_test_k)
  ts_test_k <- log(xts_test_k+1.00001)
  ts_test_k <- ts(ts_test_k,end=c(2017, 10), frequency = 12)
  assign(paste("ts_test", k, sep = "_"), ts_test_k)
  
  # Fitting ARIMA model to TESt data
  fit_test_k <- Arima(ts_test_k, model=fit_autoarima_k, xreg=xreg_test)
  assign(paste("fit_test", k, sep = "_"), fit_test_k)
  
  # Calculating model errors (First M months)
  forecastxm_k <- exp(forecast(fit_test_k,h=nbm2predict,xreg=xreg_test)$mean)[1:nbm2predict]
  assign(paste("forecastxm", k, sep = "_"), forecastxm_k)
  
  #names(Avg_ts_test2)
  #Avg_ts_test2[1,]

  #actualtsxm_k <- ts(as.numeric(Avg_ts_test2[k,(start_ts+1):end_ts]),end=c(2017, 10), 
  #                   frequency = 12)[((end_ts-nbm2predict)+1):(end_ts-1)]
  actualtsxm_k <- ts(as.numeric(Avg_ts_test2[k,(start_ts+1):end_ts]),end=c(2018, 10), 
                     frequency = 12)[((end_ts-nbm2predict)):(end_ts-1)]
  assign(paste("actualtsxm", k, sep = "_"), actualtsxm_k)
  
  diff_forecasting_k <- abs(actualtsxm_k - forecastxm_k)
  error_forecasting_k <- diff_forecasting_k/actualtsxm_k
  
  AvgError1[k] <- mean(error_forecasting_k)
  AvgError2[k] <- sum(diff_forecasting_k)/sum(actualtsxm_k)
  rmse[k] <- RMSE(actualtsxm_k, forecastxm_k)
  
}  


fit_autoarima_1 <- readRDS(paste(file = "fit_autoarima", 1, ".rds", sep = "_"))
fit_autoarima_2 <- readRDS(paste(file = "fit_autoarima", 2, ".rds", sep = "_"))
fit_autoarima_3 <- readRDS(paste(file = "fit_autoarima", 3, ".rds", sep = "_"))
fit_autoarima_4 <- readRDS(paste(file = "fit_autoarima", 4, ".rds", sep = "_"))
fit_autoarima_5 <- readRDS(paste(file = "fit_autoarima", 5, ".rds", sep = "_"))
fit_autoarima_6 <- readRDS(paste(file = "fit_autoarima", 6, ".rds", sep = "_"))
fit_autoarima_7 <- readRDS(paste(file = "fit_autoarima", 7, ".rds", sep = "_"))
fit_autoarima_8 <- readRDS(paste(file = "fit_autoarima", 8, ".rds", sep = "_"))
fit_autoarima_9 <- readRDS(paste(file = "fit_autoarima", 9, ".rds", sep = "_"))
#fit_autoarima_10 <- readRDS(paste(file = "fit_autoarima", 10, ".rds", sep = "_"))

AvgError1
AvgError2
rmse
mean(AvgError1)
mean(AvgError2)
mean(rmse)

names(Avg_ts_2)
names(Avg_ts_test2)
# Restore the object
#model_1 <- readRDS(file = "fit_autoarima_1.rds")

m<-7
autoarima_ts <- fit_autoarima_7
autoarima_ts
#forecast(autoarima_ts,h=h_val,xreg=xreg_val)
#forecast_1
plot(forecast(autoarima_ts,h=h_val,xreg=xreg_val,level=95), main=paste("ARIMA Forecasting for account in segment",  m, sep=" "))
lines(autoarima_ts$fitted,col="green")
#lines(ts(forecast_8,start=c(2017, 11), frequency = 12),col="green")
lines(ts(as.numeric(log(Avg_ts_2[m,62:73]+1.00001)),start=c(2017, 11), frequency = 12),col="red")
#lines(ts(log(as.numeric(Avg_ts_2[m,2:61])),end=c(2017, 10), frequency = 12),col="green")
par(mfrow = c(1, 1))
accuracy(autoarima_ts)

m<-1
ts(as.numeric(log(Avg_ts_2[m,2:73]+1.00001)),start=c(2012, 11), frequency = 12)
plot(ts(as.numeric(log(Avg_ts_2[m,2:73]+1.00001)),start=c(2012, 11), frequency = 12))


forecast_1
plot(ts(as.numeric(Avg_ts_2[m,2:61]),end=c(2017, 10), frequency = 12))
lines(ts(as.numeric(Avg_ts_test2[m,2:61]),end=c(2017, 10), frequency = 12),col="red")

ts_test <- fit_test_1
#forecast(ts_test,h=12,xreg=xreg_val_test)$mean
plot(forecast(ts_test,xreg=xreg_val))
lines(ts(log(as.numeric(Avg_ts_test2[m,2:60])),end=c(2017, 10), frequency = 12),col="green")
lines(ts_test$fitted,col="red")

#exp(forecast_1)
#forecastxm_1
#actualtsxm_1

#par("mar")
#par(mar=c(2,2,2,2))

forecast_12m <- forecastxm_13
actualts_12m <- actualtsxm_13

diff_forecasting_1 <- abs(actualts_12m - forecast_12m)
error_forecasting_1 <- abs(actualts_12m - forecast_12m)/actualts_12m
mean(error_forecasting_1)
sum(diff_forecasting_1)/sum(actualts_12m)

#rmse_1 <- RMSE(actualts_12m, forecast_12m)
#rmse_1


