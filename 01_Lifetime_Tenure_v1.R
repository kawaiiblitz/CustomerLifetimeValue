
install.packages(c("haven","readr","readxl","foreign","ggplot2","dplyr","randomForest",
                   "foreach","caret","parallel", "doParallel")) 
install.packages("haven")
install.packages("factoextra")
install.packages("mice")
install.packages("VIM")
install.packages("ROCR")
install.packages("coefplot")
install.packages("car")


install.packages("ggplot2")
install.packages("survival")
install.packages("Hmisc")
install.packages("stringr")
install.packages("rms")
install.packages("SparseM")


install.packages("randomForestSRC")
install.packages("randomForest")
install.packages("pec")
install.packages("party")
install.packages("riskRegression")
install.packages("psych")
install.packages("survminer")
install.packages("InformationValue")
install.packages("smbinning")

install.packages("caret", dependencies = TRUE)
install.packages("tidyr")
install.packages("brotools")
install.packages("pastecs")

install.packages("stringr")
install.packages("olsrr")

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
library(haven)
library(mice) #For Missing Values Exploration
library(VIM) #For Missing Values Graphs
library(ROCR)
library(coefplot)
library(factoextra)
#library(car)
#library(plyr)
library(dplyr)
library(dummies)
library(ISwR)
library(ggplot2)
library(survival)
library(Hmisc)
library(stringr)
library(survminer)
library(rms)
library(SparseM)

library(randomForestSRC)
library(pec)
library(party)
library(riskRegression)
library(psych)
library(InformationValue)
library(smbinning) 

library(purrr)
library(tidyr)
library(caret)

library(stringr)
library(olsrr)

#remove.packages(party)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# E S T I M A T E D     T E N U R E
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

setwd("C:/Users/s1354719/Documents/BNS/Projects/201806_LTV/Canada/Data")

library(lubridate)
library(tidyverse) 
library(readr)
library(haven)
###### Importing data (TRAIN)
data_import <- read_sas("./RandomSample_All.sas7bdat")

#setwd("C:/Users/s1354719/Documents/BNS/Projects/201806_LTV/Canada/Data/subsample")

data_all <- as.data.frame(data_import)
dim(data_all)
names(data_all)
nobs=dim(data_all)[1]
table(data_all$closed_flag)

summary(data_import$survival_months)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Identify TARGET
data_all$status <- 0
data_all[data_all$closed_flag==1,]$status <- 1
table(data_all$status)

names(data_all)[names(data_all)=="survival_months"] <- 'time'

data_Y <- data_all[,c("time","status")]
dim(data_Y)
names(data_Y)
#names(data_Y)[names(data_Y)=="survival_months"] <- 'time'


# Identify TIME SERIES
names_ts <- names(data_all %>% select(contains("total_revenue")))
names_ts <- names_ts[c(1:60)]
#names_ts <- rev(names_ts)

names_ts_after <- names(data_all %>% select(contains("rev_aft")))
names_ts_after <- names_ts_after[c(1:12)]

# Identify Client_ID
data_ClientID <- data_all[,c("ACCOUNT_NUM")]
head(data_ClientID)
dim(data_ClientID)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
library(survival)
# Create the life table survival object for train
# The functions survfit() and Surv() create a life table survival object.
# The life table object is passed to the plot() function to create the KM plot.
#data.survfit <- survfit(Surv(time, status == 0) ~ 1, data=train_X[,!(names(train_X) %in% c("seq"))])
#data.survfit <- survfit(Surv(time, status == 1) ~ 1, data=train_X[,!(names(train_X) %in% c("seq"))])
data.survfit <- survfit(Surv(time, status) ~ 1, data=data_all)
data.survfit

# Plot the Kaplan-Meier curve for aml.
# By default, R includes the confidence interval. 
par("mar")
#par(mar=c(0.8,0.8,0.8,0.8))
par(mar=c(9.638554,4.819277,2.409639,2.409639))

plot(data.survfit, xlab = "Time (months)", ylab="Proportion surviving", main="Survival in Data")

# The summary() function displays the life table
summary(data.survfit)
data.survfit$surv


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

names(data_all)
# Identify first field from Bureau
position_bureau <- match("CUST_BASE_KEY",names(data_all))
position_bureau
data_wout_bureau <- data_all[,1:(position_bureau-1)]
names(data_wout_bureau)

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

#drop_rev_ts1 <- names(data_wout_bureau)[grep('total_revenue_', names(data_wout_bureau), 1)]
#drop_rev_ts2 <- names(data_wout_bureau)[grep('rev_aft', names(data_wout_bureau), 1)]

all_drop_seq <- c(
  drop_2,drop_3,drop_4,drop_5,drop_6,drop_7,drop_8,drop_9,drop_10,drop_11,drop_12,
  names_ts, names_ts_after
  #  drop_rev_ts1, drop_rev_ts2 
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
data_all2 <- as.data.frame(data_all[,!(names(data_all) %in% final_drop)])
dim(data_all2)
names(data_all2)

table(data_all2$status)
summary(data_all2$time)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Identify NUMERIC variables
nums <- unlist(lapply(data_all2, is.numeric))  
data_all_nums <- data_all2[,nums]
dim(data_all_nums)
names(data_all_nums)


# Identify CHARACTER variables
strings <- unlist(lapply(data_all2, is.character))  
data_all_char <- data_all2[,strings]
dim(data_all_char)
names(data_all_char)
#str(data_all_char)

length(unique(data_all$mtg))
unique_values <- function(x){length(unique(x))}
any_missing <- function(x){sum(unique(is.na(x)))}
#unique <- as.data.frame(apply(data_all2,2,unique_values),apply(data_all2,2,any_missing))
unique <- as.data.frame(apply(data_all2,2,unique_values))
varnames_unique_lt5 <- names(data_all2[unique[,1]>3 & unique[,1]<=5])
varnames_unique_lt5



library(dummies)
table(data_all$PRODUCT_CODE)
table(data_all$Sub_Product_Code)
#table(data_all$CLS_RSN_CD)
table(data_all$PROBE_ARI)
#table(data_all$PROBE_CRI)
#table(data_all$CURR_BILL_CODE)
#table(data_all$STEP_FLAG)
table(data_all$ACCT_PROV_STATE_CD)
table(data_all$CHANNEL)
#table(data_all$sub_channel)
#table(data_all$cri_0)
#table(data_all$ari_0)
#table(data_all$GO26_HIT_NOHIT_EDIT_REJCT)
#table(data_all$GO75_EMPRC_EXCLSN_CD)
table(data_all$GO03_CR_BUREAU_WORST_RT)
#table(data_all$PR34_BNKRPY_NARRTV_CDS)
table(data_all$FRAUD_ALERT_F)
#table(data_all$CNTC_PH_NUM)
#table(data_all$AS56_TRANSRISK_EXCLSN_CD)
#table(data_all$CVSC110_CV_BANKRUPTCY_SCORE)

#table(data_all$num_delq_days_q_3)
#table(data_all$cash_adv_amt_q_3)
#table(data_all$count_utiliz_50perc_q_3)
#table(data_all$count_utiliz_75perc_q_3)
#table(data_all$count_utiliz_90perc_q_3)
#table(data_all$bal_eq_lim_q_3)
#table(data_all$bal_eq_lim_q_6)
#table(data_all$bal_eq_lim_q_9)
#table(data_all$bal_eq_lim_q_12)
#table(data_all$last_15_DPD_q_3)
#table(data_all$last_30_DPD_q_3)
#table(data_all$last_60_DPD_q_3)
#table(data_all$last_90_DPD_q_3)
#table(data_all$useofcash_q_3)
#table(data_all$GO75_EMPRC_EXCLSN_CD)
#table(data_all$PR01_NUM_OF_BNKRPC)
#table(data_all$AT64_NUM_ACT_TRDS_WITH_60_DY)
#table(data_all$Q5_CREDIT_LIM)
#table(data_all$Q5_MOB)
#table(data_all$Q5_Net_RAR)
#table(data_all$Q5_Outstanding_Bal)
#table(data_all$Q5_TotalRevenue)



dummy_ProdCode <- dummy("PRODUCT_CODE", as.data.frame(data_all), sep="_")
dummy_SubProdCode <- dummy("Sub_Product_Code", as.data.frame(data_all), sep="_")
dummy_ProbeARI <- dummy("PROBE_ARI", as.data.frame(data_all), sep="_")
dummy_Province <- dummy("ACCT_PROV_STATE_CD", as.data.frame(data_all), sep="_")
dummy_Channel <- dummy("CHANNEL", as.data.frame(data_all), sep="_")
dummy_BureauWorst <- dummy("GO03_CR_BUREAU_WORST_RT", as.data.frame(data_all), sep="_")
dummy_FraudAlert <- dummy("FRAUD_ALERT_F", as.data.frame(data_all), sep="_")
#names(as.data.frame(dummy_CountryTier))

dummy_NbDelqDays_3m <- dummy("num_delq_days_q_3", as.data.frame(data_all), sep="_")
dummy_CashAdvAmt_3m <- dummy("cash_adv_amt_q_3", as.data.frame(data_all), sep="_")
dummy_CountUtiliz50pct_3m <- dummy("count_utiliz_50perc_q_3", as.data.frame(data_all), sep="_")
dummy_CountUtiliz75pct_3m <- dummy("count_utiliz_75perc_q_3", as.data.frame(data_all), sep="_")
dummy_CountUtiliz90pct_3m <- dummy("count_utiliz_90perc_q_3", as.data.frame(data_all), sep="_")
dummy_BalEqLim_3m <- dummy("bal_eq_lim_q_3", as.data.frame(data_all), sep="_")
dummy_BalEqLim_6m <- dummy("bal_eq_lim_q_6", as.data.frame(data_all), sep="_")
dummy_BalEqLim_9m <- dummy("bal_eq_lim_q_9", as.data.frame(data_all), sep="_")
dummy_BalEqLim_12m <- dummy("bal_eq_lim_q_12", as.data.frame(data_all), sep="_")
dummy_Last15dpd_3m <- dummy("last_15_DPD_q_3", as.data.frame(data_all), sep="_")
dummy_Last30dpd_3m <- dummy("last_30_DPD_q_3", as.data.frame(data_all), sep="_")
dummy_Last60dpd_3m <- dummy("last_60_DPD_q_3", as.data.frame(data_all), sep="_")
dummy_Last90dpd_3m <- dummy("last_90_DPD_q_3", as.data.frame(data_all), sep="_")
dummy_UseOfCash_3m <- dummy("useofcash_q_3", as.data.frame(data_all), sep="_")
dummy_Emprc_Exclsn_CD <- dummy("GO75_EMPRC_EXCLSN_CD", as.data.frame(data_all), sep="_")
dummy_NbBankrpc <- dummy("PR01_NUM_OF_BNKRPC", as.data.frame(data_all), sep="_")
dummy_NbActTrades60d <- dummy("AT64_NUM_ACT_TRDS_WITH_60_DY", as.data.frame(data_all), sep="_")
dummy_CreditLim_Q5 <- dummy("Q5_CREDIT_LIM", as.data.frame(data_all), sep="_")
dummy_MOB_Q5 <- dummy("Q5_MOB", as.data.frame(data_all), sep="_")
dummy_NetRAR_Q5 <- dummy("Q5_Net_RAR", as.data.frame(data_all), sep="_")
dummy_OutsBal_Q5 <- dummy("Q5_Outstanding_Bal", as.data.frame(data_all), sep="_")
dummy_TotalRevenue_Q5 <- dummy("Q5_TotalRevenue", as.data.frame(data_all), sep="_")


data_all_NewDummies <- data.frame(
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
drop_dummy <- names(data_all_NewDummies)[str_sub(names(data_all_NewDummies), start= -1)=="_"]
#valid_dummies <- names(data_all_NewDummies)[!(names(data_all_NewDummies) %in% drop_dummy)]
valid_dummies <- names(data_all_NewDummies)[!(names(data_all_NewDummies) %in% c(drop_dummy))]

#data2 <- data.frame(data, dummy_RevTrans6m, dummy_EverBad, dummy_RiskProfile,dummy_CountryTier)
data_all_flags <- data_all_NewDummies[,valid_dummies]
dim(data_all_flags)
names(data_all_flags)


# Adding new FLAGS to NUMERIC data in data3
#data_all3 <- data.frame(data_all_nums,data_all_flags)
data_all3 <- data.frame(data_all_nums[ , !(names(data_all_nums) %in% varnames_unique_lt5)],data_all_flags)
dim(data_all3)
names(data_all3)
table(data_all3$status)

#summaryBy(TotalRevenue ~ status, data = data_all3, FUN = function(x) { c(m = mean(x)) } )


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# # # # # #  Checking for missing values
library(mice) #For Missing Values Exploration
pMiss <- function(x){sum(is.na(x))/length(x)*100}
pct_missing <- as.data.frame(apply(data_all3,2,pMiss))
pct_missing
vars_missing <- names(data_all3)[pct_missing$`apply(data_all3, 2, pMiss)` > 0]
vars_missing


######  Identifying Variables with Missing Values > 30%
#varnames_missing_gt_30pct <- names(data[pct_missing[,1]>30])
varnames_missing_gt_30pct <- names(data_all3[pct_missing[,1]>30])
varnames_missing_gt_30pct

#REMOVING Variables with Missing Values > 25%
data_all3 <- data_all3[ , !(names(data_all3) %in% varnames_missing_gt_30pct)]
dim(data_all3)
pct_missing <- as.data.frame(apply(data_all3,2,pMiss))
vars_missing <- names(data_all3)[pct_missing$`apply(data_all3, 2, pMiss)` > 0]
vars_missing

#summaryBy(TotalRevenue 
#          ~ status, data = data_all3, 
#          FUN = function(x) { c(m = mean(x)) } )

# * * * * * * * * * *
# # # # # #  Replacing  missing values (Flags)
#length(unique(data_all3$ccd))
#sum(unique(is.na(data_all3$ccd)))

flag_impute <- 0
for(i in 1:length(vars_missing)) 
{
  #i <- 211
  # statistics to replace outliers
  l <- length(unique(data_all3[,vars_missing[i]]))
  s <- sum(unique(is.na(data_all3[,vars_missing[i]])))
  
  if (l <= 3 & s > 0) 
  {
    data_all3[is.na(data_all3[,vars_missing[i]]),vars_missing[i]] <- 0
    #is.na(data_all3[,vars_missing[i]]) <- 0
    flag_impute <- flag_impute + 1
  } 
}
flag_impute
#length(unique(data_all3$chq))
#sum(unique(is.na(data_all3$chq)))
#data_all3[is.na(data_all3[,'chq']),'chq'] <- 0


# * * * * * * * * * *
# # # # # #  Replacing  missing values (Continuous)
pct_missing <- as.data.frame(apply(data_all3,2,pMiss))
vars_missing <- names(data_all3)[pct_missing$`apply(data_all3, 2, pMiss)` > 0]
vars_missing

Vars_For_Mean_Imput <- vars_missing
summary(data_all3[,Vars_For_Mean_Imput])
for(j in 1:length(Vars_For_Mean_Imput)) 
{ 
  data_all3[is.na(data_all3[,Vars_For_Mean_Imput[j]]),Vars_For_Mean_Imput[j]]=colMeans(data_all3[,Vars_For_Mean_Imput],na.rm=TRUE)[j]
}  
summary(data_all3[,Vars_For_Mean_Imput])

table(data_all3$flag_revolver_12m)
table(data_all3$DLQNT_DAY_NUM)
table(data_all3$status)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# # # # # # Identify and replace outliers (only for CONT vars)

library(Hmisc)
#describe(data_all3$Avg_Revenue_after)
library(timeDate)
names(data_all3)

Vars_For_Outlier_Imput <- names(data_all3)
Vars_For_Outlier_Imput

avg_cont1 <- as.data.frame(colMeans(data_all3[,Vars_For_Outlier_Imput]))
count_vars_replace <- 0
var_trimmed <- rep(0,length(Vars_For_Outlier_Imput))

for(i in 1:ncol(data_all3[,Vars_For_Outlier_Imput])) 
{
  #i <- 4
  # statistics to replace outliers
  lowerq = quantile(as.data.frame(data_all3[,Vars_For_Outlier_Imput])[,i],na.rm = TRUE)[2]
  upperq = quantile(as.data.frame(data_all3[,Vars_For_Outlier_Imput])[,i],na.rm = TRUE)[4]
  iqr = upperq - lowerq
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  
  quantiles <- quantile(as.data.frame(data_all3[,Vars_For_Outlier_Imput])[,i], c(.01, .95 ),na.rm = TRUE)
  
  #if ((sum(data_all3[,i] > quantiles[2]) > 0 | 
  #     kurtosis(data_all3[,i]) > 3) &
  #    length(unique(data_all3[,i])) > 10
  #) 
  
  if ((sum(as.data.frame(data_all3[,Vars_For_Outlier_Imput])[,i] > quantiles[2]) > 0 | 
       kurtosis(as.data.frame(data_all3[,Vars_For_Outlier_Imput])[,i]) > 3) &
      length(unique(as.data.frame(data_all3[,Vars_For_Outlier_Imput])[,i])) > 10
  ) 
  {
    count_vars_replace <- count_vars_replace+1
    var_trimmed[i] <- 1
    
    data_all3[,Vars_For_Outlier_Imput][as.data.frame(data_all3[,Vars_For_Outlier_Imput])[,i] > quantiles[2],i] <- quantiles[2]
    data_all3[,Vars_For_Outlier_Imput][as.data.frame(data_all3[,Vars_For_Outlier_Imput])[,i] < quantiles[1],i] <- quantiles[1]
  } 
}

#table(data_all3$DLQNT_FLAG)

#summary(data_wout_nzv[,Vars_For_Outlier_Imput])
count_vars_replace
var_trimmed
names(data_all3)[var_trimmed==1]
sort(names(data_all3)[var_trimmed==1])

# Compare ORIGINAL vs TRIMMED average
avg_cont2 <- as.data.frame(colMeans(data_all3[,Vars_For_Outlier_Imput]))
head(cbind(avg_cont1,avg_cont2),50)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# IDENTIFY constants in dataset (Check Near-Zero-Variance predictors)
library(caret)
check_nearzerovar <- nearZeroVar(data_all3, 
                                 freqCut = 95/5, uniqueCut = 10, 
                                 saveMetrics = TRUE, names= TRUE)
sum(check_nearzerovar$zeroVar)
sum(check_nearzerovar$nzv)
tail(check_nearzerovar,200)
table(data_all3$bal_eq_lim_q_3)
check_nearzerovar


zeroVar_vars <- names(data_all3)[check_nearzerovar$zeroVar==1]
nzv_vars <- names(data_all3)[check_nearzerovar$nzv=='TRUE']

#beer_train_zeroVar <- data_nums[ , (names(data_nums) %in% zeroVar_vars)]

total_zeroVar_vars <- c(zeroVar_vars,nzv_vars)
sort(total_zeroVar_vars)

total_zeroVar_vars<- total_zeroVar_vars[total_zeroVar_vars != 'DLQNT_FLAG']
#total_zeroVar_vars<- total_zeroVar_vars[total_zeroVar_vars != 'num_delq_days_0']
#total_zeroVar_vars<- total_zeroVar_vars[total_zeroVar_vars != 'num_delq_days_1']
total_zeroVar_vars<- total_zeroVar_vars[total_zeroVar_vars != 'survival_months']


###### REMOVE constants in dataset
data_all_wout_nzv <- data_all3[, !(names(data_all3) %in% total_zeroVar_vars)]
dim(data_all_wout_nzv)
dim(data_all3)
dim(data_all2)
dim(data_all)
names(data_all_wout_nzv)
table(data_all_wout_nzv$flag_revolver_12m)
table(data_all_wout_nzv$status)


# Checking all variables with variance ZERO
variance <- as.data.frame(apply(data_all_wout_nzv,2,var))
varnames_variance_gt0 <- names(data_all_wout_nzv[variance[,1]==0])
varnames_variance_gt0


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Manual drop

#sort(names(data_all_wout_nzv))

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
)

data_all4 <- data_all_wout_nzv[,!(names(data_all_wout_nzv) %in% manual_drop)]
dim(data_all4)
names(data_all4)

table(data_all4$sum_profi_rev)
length(unique(data_all4$sum_profi_rev))

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

unique_values <- function(x){length(unique(x))}
unique <- as.data.frame(apply(data_all4,2,unique_values))
varnames_unique_gt <- names(data_all4[unique[,1]>5])

# IDENTIFY and REMOVE highly correlated variables 
data_all_for_Corr <- data_all4[,varnames_unique_gt]
#data_allCorr <- cor(data_all_for_Corr, use="pairwise.complete.obs")
data_allCorr <- cor(data_all_for_Corr)
#head(data_allCorr)
corr_cutoff <- 0.6
highCorr <- findCorrelation(data_allCorr, corr_cutoff, verbose = FALSE, names = FALSE)
length(highCorr)
highCorr_vars<- names(data_all_for_Corr[highCorr])
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
highCorr_vars<- highCorr_vars[highCorr_vars != 'TU_AGE']
highCorr_vars<- highCorr_vars[highCorr_vars != 'sum_cash_6']
highCorr_vars<- highCorr_vars[highCorr_vars != 'BR33_TOT_BAL_BNK_REVLVNG']
highCorr_vars<- highCorr_vars[highCorr_vars != 'BR147_MAX_CRNT_UTILZTN_BNK_REV']
highCorr_vars<- highCorr_vars[highCorr_vars != 'B_12_bureau_score_']
highCorr_vars<- highCorr_vars[highCorr_vars != 'AT02_NUM_ACTV_TRDS']
highCorr_vars<- highCorr_vars[highCorr_vars != 'AT34_TOT_UTILZTN']
sort(highCorr_vars)


######  NEW DATA (Removing highly correlated variables)
data_all5 <- data_all4[ , !(names(data_all4) %in% highCorr_vars)]
dim(data_all5)
names(data_all5)
table(data_all5$flag_revolver_12m)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

library(tidyverse) 
sort(names(data_all5))
data_all_X <- data.frame(data_all5,
                         data_Y
#                         data_all[,names_ts],
#                         data_all[,names_ts_after]
                         )

names(data_all_X)

#names(data_all_X)[names(data_all_X)=="Avg_Revenue_after"] <- 'Target'
#names(data_all_X)[names(data_all_X)=="data_all.dpd_1"] <- 'dpd_1'

summary(data_all_X$status)
dim(data_all_X)

###### Data partition (Train vs Test)
data_all_X$seq <- seq(1, dim(data_all_X)[1])
# % of the sample size to be selected
pct_sample <- 0.6
smp_size <- floor(pct_sample * nrow(data_all_X))
# set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_all_X)), size = smp_size)


train <- data_all_X[train_ind, ]
test <- data_all_X[-train_ind, ]
dim(data_all)
dim(train)
dim(test)
names(train)
names(test)
head(train)
#head(test)



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

#install.packages(c("survival", "survminer"))
#library("survival")
#library("survminer")

#time <- train$time_months
time <- train$time
status <- train$status
names(train)
summary(time)
summary(status)


################## 
library(survival)
# Create the life table survival object for train
# The functions survfit() and Surv() create a life table survival object.
# The life table object is passed to the plot() function to create the KM plot.
#data.survfit <- survfit(Surv(time, status == 0) ~ 1, data=train_X[,!(names(train_X) %in% c("seq"))])
#data.survfit <- survfit(Surv(time, status == 1) ~ 1, data=train_X[,!(names(train_X) %in% c("seq"))])
data.survfit <- survfit(Surv(time, status) ~ 1, data=train)
data.survfit

# Plot the Kaplan-Meier curve for aml.
# By default, R includes the confidence interval. 
plot(data.survfit, xlab = "Time (months)", ylab="Proportion surviving", main="Survival in Data")
#par("mar")
#par(mar=c(1,1,1,1))
#par(mar=c(9.638554,4.819277,2.409639,2.409639))


# The summary() function displays the life table
summary(data.survfit)
#str(data.survfit)
data.survfit$surv


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# # # # # # Using random SURVIVAL forest (from Journal of Statistics)

# 3.1. Selected Cox regression
# The following function selectCox evaluates Step 1 and Step 2 the stepwise variable selection
# strategy for the Cox regression model described in Section 2.2.
selectCox <- function(formula, data, rule = "aic") {
  require("rms")
  require("prodlim")
  fit <- cph(formula, data, surv = TRUE)
#  fit <- coxph(formula, data, surv = TRUE)
  bwfit <- fastbw(fit, rule = rule)
  if (length(bwfit$names.kept) == 0) {
    newform <- reformulate("1", formula[[2]])
    newfit <- prodlim(newform, data = data)
  } else{
    newform <- reformulate(bwfit$names.kept, formula[[2]])
    newfit <- cph(newform, data, surv = TRUE)
  }
  out <- list(fit = newfit,In = bwfit$names.kept)
  out$call <- match.call()
  class(out) <- "selectCox"
  out
}


# Step 3 of Section 2.2 is implemented using the generic function predictSurvProb. This
# passes its arguments to the method for objects of class cph
predictSurvProb.selectCox <- function(object, newdata, times, ...) {
  predictSurvProb(object[[1]], newdata = newdata, times = times, ...)
}


# 3.2. randomSurvivalForest package: rsf
# A random survival forest model is fitted with the function rsf (randomSurvivalForest) which
# results in an object of S3 class rsf. Using the built-in predict.rsf method we extract the
# averaged cumulative hazard function for each line in newdata at the event times of the original
# data set (see Section 2.3). The survival probabilities are then computed via Equation 1 and
# with the help of the function sindex (prodlim) these are evaluated at the requested times.

predictSurvProb.rsfrc <- function (object, newdata, times, ...) {
  N <- NROW(newdata)
  class(object) <- c("rsfrc", "grow")
  S <- exp(-predict.rsfrc(object, test = newdata)$ensemble)
  if(N == 1) S <- matrix(S, nrow = 1)
  Time <- object$timeInterest
  p <- cbind(1, S)[, 1 + sindex(Time, times),drop = FALSE]
  if(NROW(p) != NROW(newdata) || NCOL(p) != length(times))
    stop("Prediction failed")
  p
}

##prediction function required for pec
#predictSurvProb.rfsrc <- function(object, newdata, times, ...){
#  ptemp <- predict(object,newdata=newdata,...)$survival
#  pos <- sindex(jump.times = object$time.interest, eval.times = times)
#  p <- cbind(1,ptemp)[, pos + 1]
#  if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
#    stop("Prediction failed")
#  p
#}

# 3.3. party package: cforest
# A conditional inference forest model (Section 2.3) is fitted with the function cforest (party)
# and results in an S4 class object. We get around the class problem by creating a wrapper
# function pecCforest. The fitted cforest S4 class object is stored in a list which is supplied
# with the call. The output is assigned to the S3 class pecCforest.
pecCforest <- function(formula, data, ...) {
  require("party")
  out <- list(forest = cforest(formula, data, ...))
  class(out) <- "pecCforest"
  out$call <- match.call()
  out
}


# The treeresponse method (party) can be applied to the list element pecCforest$forest of
# the S3 class object in order to extract survival probabilities for newdata at times (see Equation
# 2). The resulting object is a list which contains for each line in newdata the Kaplan-Meier
# curve in form of a survfit object (survival). We then apply predictSurvProb.survfit to
# the elements of the list.
predictSurvProb.pecCforest <- function (object, newdata, times, ...) {
  survObj <- treeresponse(object$forest, newdata = newdata)
  p <- do.call("rbind", lapply(survObj, function(x) {
    predictSurvProb(x, newdata = newdata[1, , drop = FALSE], times = times)
  }))
  if(NROW(p) != NROW(newdata) || NCOL(p) != length(times))
    stop("Prediction failed")
  p
}


# * * * * * * * * * * * * * * * * * * * * * * *
names(train)
train_data <- train[,!(names(train) %in% c("seq"))]
names(train_data)
dim(train_data)
#summary(train_data)
#describe(train_data)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

library(tidyverse) 
train_data <- train[,!(names(train) %in% names_ts)]
train_data <- train_data[,!(names(train_data) %in% names_ts_after)]
sort(names(train_data))

train_data <- train_data %>% select(-contains("Avg_Revenue_after"))
#train_data <- train_data %>% select(-contains("total_revenue_"))
#train_data <- train_data %>% select(-contains("rev_aft"))
#train_data <- train_data %>% select(-contains("survival_months"))
train_data <- train_data %>% select(-contains("seq"))
train_data <- train_data %>% select(-contains("credit_lim_min_12"))
#train_data <- train_data %>% select(-contains("sum_profi_trans"))
train_data <- train_data %>% select(-contains("total_merch_amt_std_3"))
train_data <- train_data %>% select(-contains("total_merch_amt_min_12"))
#train_data <- train_data %>% select(-contains("new_inactive_0"))
train_data <- train_data %>% select(-contains("R2_3_balance_"))
#train_data <- train_data %>% select(-contains("new_inactive_1"))
#train_data <- train_data %>% select(-contains("new_transactor_0"))
#train_data <- train_data %>% select(-contains("revolver_sum_3"))
train_data <- train_data %>% select(-contains("nir_0"))
#train_data <- train_data %>% select(-contains("revolver"))
train_data <- train_data %>% select(-contains("revolver_max"))
train_data <- train_data %>% select(-contains("revolver_min"))
train_data <- train_data %>% select(-contains("revolver_std"))
train_data <- train_data %>% select(-contains("revolver_0"))
train_data <- train_data %>% select(-contains("new_revolver_0"))
train_data <- train_data %>% select(-contains("close_date"))
train_data <- train_data %>% select(-contains("closed_flag"))
train_data <- train_data %>% select(-contains("close_flag"))
train_data <- train_data %>% select(-contains("avg_cash_6"))
train_data <- train_data %>% select(-contains("AT42_NUM_TM_60_DY_PAST_IN_12"))
train_data <- train_data %>% select(-contains("active_flag"))


#train_data <- train_data[ , !(names(train_data) %in% c("revolver"))]
train_data <- train_data[ , !(names(train_data) %in% c("ch_off_flag"))]
train_data <- train_data[ , !(names(train_data) %in% c("num_delq_days_0"))]
train_data <- train_data[ , !(names(train_data) %in% c("crg_0"))]
train_data <- train_data[ , !(names(train_data) %in% c("nii_0"))]
train_data <- train_data[ , !(names(train_data) %in% c("ccd"))]
train_data <- train_data[ , !(names(train_data) %in% c("rat_avgcash9_limit"))]
train_data <- train_data[ , !(names(train_data) %in% c("credit_lim_0"))]
train_data <- train_data[ , !(names(train_data) %in% c("total_expense_0"))]
#train_data <- train_data[ , !(names(train_data) %in% c("B_9_total_revenue_"))]
train_data <- train_data[ , !(names(train_data) %in% c("total_expense_0"))]
#train_data <- train_data[ , !(names(train_data) %in% c("num_delq_days_q_9"))]
train_data <- train_data[ , !(names(train_data) %in% c("revolver_1"))]
#train_data <- train_data[ , !(names(train_data) %in% c("new_revolver_1"))]
#train_data <- train_data[ , !(names(train_data) %in% c("last_15_DPD_q_9"))]
#train_data <- train_data[ , !(names(train_data) %in% c("flag_other_12m"))]

train_data <- train_data[ , !(names(train_data) %in% c("R2_3_total_revenue_"))]
train_data <- train_data[ , !(names(train_data) %in% c("R2_3_crg_"))]
#train_data <- train_data[ , !(names(train_data) %in% c("B_9_total_revenue_"))]
train_data <- train_data[ , !(names(train_data) %in% c("B_3_total_revenue_"))]
train_data <- train_data[ , !(names(train_data) %in% c("B_3_balance_"))]
train_data <- train_data[ , !(names(train_data) %in% c("R2_9_bureau_score_"))]
#train_data <- train_data[ , !(names(train_data) %in% c("last_15_DPD_q_9"))]
train_data <- train_data[ , !(names(train_data) %in% c("new_inactive_1"))]
#train_data <- train_data[ , !(names(train_data) %in% c("flag_other_12m"))]

train_data <- train_data %>% select(-contains("R2_"))

sort(names(train_data))

#library(doBy)
#summaryBy(rat_avgcash9_limit
#          ~ status, data = train_data, 
#          FUN = function(x) { c(m = mean(x)) } )


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

library(Boruta)

names(train_data)
sort(names(train_data))
set.seed(123)
boruta_train_survival <- Boruta(status~., data = train_data[ , !(names(train_data) %in% c("time"))], doTrace = 2, maxRuns=30)
print(boruta_train_survival)
#boruta_train_survival$finalDecision

final.boruta <- TentativeRoughFix(boruta_train_survival)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)
tail(boruta.df[order(boruta.df$meanImp),],50)


boruta_decision <- boruta.df$decision
boruta.df$meanImp

#boruta_df <- data.frame(names(train_data)[1:74],boruta.df$meanImp,boruta_decision)
boruta_df <- data.frame(names(train_data[, !names(train_data) %in% c("status","time")]),boruta.df$meanImp,boruta_decision)
names(boruta_df)
drivers_boruta <- boruta_df[order(boruta_df$boruta.df.meanImp, decreasing = T),1]
drivers_boruta

n_top_boruta <- 50
top_drivers_boruta <- as.character(drivers_boruta[1:n_top_boruta])
top_drivers_boruta

saveRDS(top_drivers_boruta, file = "top_drivers_survival.rds")

setwd("C:/Users/s1354719/Documents/BNS/Projects/201806_LTV/Canada/Data/subsample")
top_drivers_boruta <- readRDS(file = "top_drivers_survival.rds")


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

#selected_boruta <- getSelectedAttributes(final.boruta, withTentative = F)
#selected_boruta

saveRDS(boruta_train_survival, file = "boruta_train_survival.rds")
# Restore the object
boruta_train_survival <- readRDS(file = "boruta_train_survival.rds")


# * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * * * * * * * * * * * * * * *

train_boruta <- data.frame(train_data[,c(top_drivers_boruta,"status","time")])
names(train_boruta)
sort(names(train_boruta))
str(train_boruta)


library(doBy)
summaryBy(mths_since_lastact + rat_rev_bal_1 + utiliz_rate_1 
          + AT93_AGE_OLDST_OPN_TRDE_LN_MT + B_6_total_revenue_ + AS52_TRANSRISK_FACTR_CD_1
          + CVSC110_CV_BANKRUPTCY_SCORE_2 + MOB + sum_profi_oth + crg_1 + crg_min_12
          + time
          ~ status, data = train_boruta, 
          FUN = function(x) { c(m = mean(x)) } )

summaryBy(train_boruta ~ status, data = train_boruta, 
          FUN = function(x) { c(m = mean(x)) } )


#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("MOB"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Q5_Outstanding_Bal_2"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("crg_min_12"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("sum_profi_oth"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("balance_max_6"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Q5_Outstanding_Bal_0"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Q5_Outstanding_Bal_2"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS54_TRANSRISK_FACTR_CD_1"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS54_TRANSRISK_FACTR_CD_4"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS55_TRANSRISK_FACTR_CD_4"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS54_TRANSRISK_FACTR_CD_3"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS53_TRANSRISK_FACTR_CD_2"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Q5_Outstanding_Bal_0"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Q5_MOB_2"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("ORIG_CHRG_OFF_AMT"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS52_TRANSRISK_FACTR_CD_1"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("rat_avgcash3_limit"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Q5_Net_RAR_1"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("utiliz_rate_1"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("rat_avgcash3_limit"))]


#fitform1 <- Surv(time,status == 0) ~ .
#fitform2 <- Surv(time,status == 1) ~ .
fitform3 <- Surv(time,status) ~ .

library(survminer)
library(rms)
library(SparseM)

#keep <- c("time", "status"
#          , "mths_since_lastact"
#          , "R2_6_total_revenue_"
#          , "rat_rev_bal_1"
#          , "AS52_TRANSRISK_FACTR_CD_1"
#          , "B_9_total_revenue_"
#          , "ccd_bal"
#          , "AS53_TRANSRISK_FACTR_CD_2"
#          , "CVSC110_CV_BANKRUPTCY_SCORE_2"
#          , "total_expense_std_3"
#          , "AS50_TRANSRISK_SCORE"
#          , "crg_min_12"
#          , "balance_1"
#          , "AS54_TRANSRISK_FACTR_CD_3"
#          , "Q5_Outstanding_Bal_0"
#          , "crg_1"
#          , "balance_max_6"
#          , "balance_mean_6"
#          , "MOB"
#          , "payment_amt_0"
#          , "balance"
#          , "AT93_AGE_OLDST_OPN_TRDE_LN_MT"
#          , "sum_profi_trans"
#          , "payment_amt_std_12"
#          , "new_transactor_0"
#          , "R2_12_bureau_score_"
#          , "DLQNT_DAY_NUM"
#          , "mtf_bal"
#          , "total_products"
#          , "B_6_balance_"
#)

#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS52_TRANSRISK_FACTR_CD_1"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS53_TRANSRISK_FACTR_CD_2"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS54_TRANSRISK_FACTR_CD_3"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("AS55_TRANSRISK_FACTR_CD_4"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Q5_Outstanding_Bal_2"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("rat_avgcash3_limit"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("ORIG_CHRG_OFF_AMT"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Q5_Outstanding_Bal_0"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("crg_min_12"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Q5_MOB_2"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("new_inactive_0"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("B_9_total_revenue_"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("balance_max_6"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("payment_amt_0"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("bureau_score_mean_6"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("mtf_bal"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("Payment_txn"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("new_inactive_0"))]
#train_boruta <- train_boruta[ , !(names(train_boruta) %in% c("new_inactive_0"))]


fitcox <- selectCox(fitform3, data = train_boruta, rule = "aic")
#fitcox <- selectCox(fitform3, data = train_boruta[,keep], rule = "aic")
fitcox$fit
fitcox <- selectCox(fitform3, data = train_boruta, rule = "p")
#fitcox <- selectCox(fitform3, data = train_boruta[,keep], rule = "p")
fitcox$fit
sort(fitcox$In)


#library(olsrr)
#model <- lm(status ~ ., data = train_boruta)
#ols_step_best_subset(model)


library(randomForestSRC)
data_for_rf <- train_boruta[,c(fitcox$In,"time","status")]
dim(data_for_rf)
set.seed(13)
#fitrsf <- rfsrc(fitform3, data = data_for_rf, forest = TRUE, ntree = 500)
fitrsf <- rfsrc(fitform3, data = train_boruta, forest = TRUE, ntree = 500)
fitrsf
#importance_rsf <- vimp(fitrsf)$importance
#plot(importance_rsf)
#importance_rsf*100
#names(importance_rsf)


#install.packages("party")
library(party)
set.seed(13)
#fitcforest <- pecCforest(fitform3, data = data_for_rf, controls = cforest_control(ntree = 500))
fitcforest <- pecCforest(fitform3, data = train_boruta, controls = cforest_control(ntree = 500))
fitcforest
#importance_cf <- varImp(fitcforest)
#importance_cf <- importance(fitcforest)
#plot(importance_cf)
#importance_cf <- varimp(fitcforest, conditional = FALSE)

#require("party")
#out <- cforest(fitform3, train_boruta, controls = cforest_control(ntree = 500))
#class(out) <- "pecCforest"
#out$call <- match.call()
#out
#importance_cf <- varimp(out)
#importance_cf <- vimp(out)


saveRDS(fitcforest, file = "fitcforest.rds")

setwd("C:/Users/s1354719/Documents/BNS/Projects/201806_LTV/Canada/Data")
fitcforest <- readRDS(file = "fitcforest.rds")
fitcox <- readRDS(file = "fitcox.rds")
fitrsf <- readRDS(file = "fitrsf.rds")
getwd()

# * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * * * * * * * * * * * * * * *

# Partition of TEST data (for faster scoring)
names(test)
dim(test)
test2<-test
dim(test2)
pct_sample <- 0.8
smp_size <- floor(pct_sample * nrow(test2))
names(test2)
# set the seed to make your partition reproductible
set.seed(123)
test_ind <- sample(seq_len(nrow(test2)), size = smp_size)

test_1 <- test2[test_ind,]
test_2 <- test2[-test_ind,]
dim(test_1)
dim(test_2)
#head(test_1)
#head(test_2)

#names(test_1)[names(test_1)=="survival_months"] <- 'time'
#names(test_1)[names(test_1)=="flag_closed"] <- 'status'
#names(test_1)

#test_x <- test_1[,c(names(train_boruta),"seq")]
test_x <- test_1

dim(test_x)
names(test_x)
head(test_x)
table(test_x$status)

#test_x <- test

# * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * * * * * * * * * * * * * * *
#Time_Years <- c(1:30)*365
max_time <- max(test_x$time)
Time_Months <- c(1:max_time)

#install.packages("pec")
library(pec)

#pcox <- predictSurvProb(fitcox, newdata = test, times = Time_Years)
#fitcox$In
#summary(test_x[,fitcox$In])
#hist(test_x[,fitcox$In[4]])

pcox <- predictSurvProb(fitcox, newdata = test_x, times = Time_Months)
head(pcox,10)
dim(pcox)
#min(which(pcox[1,] < 0.5))
#min(which(pcox[2,] < 0.5))
#min(which(pcox[217,] < 0.5))

#pcox[1,]
#test_x[1,c(fitcox$In,"status","time")]
#fitcox$In
#summary(test_x[,fitcox$In])
#hist(test_x[,fitcox$In[1]])
#plot(pcox[1,])


# # # # # 
prsf <- predictSurvProb(fitrsf, newdata = test_x, times = Time_Months)
head(prsf,10)
#min(which(prsf[1,] < 0.5))
#min(which(prsf[2,] < 0.5))
#min(which(prsf[3,] < 0.5))

#trsf_target <- predict(fitrsf, newdata = test_x, times = Time_Months)


# # # # # 
extends <- function(...) TRUE
# The function extends is required to ensure that cforest yields a survival probability. This
# is not necessary if only functions in party are used.
pcf <- predictSurvProb(fitcforest, newdata = test_x, times = Time_Months)
head(pcf,20)
#min(which(pcf[1,] < 0.5))
#min(which(pcf[2,] < 0.5))
plot(pcf[13,])
plot(pcf[7,])
summary(fitcforest)
#summary(pcf)
#head(test_x,10)
#lifetime_tenure[lifetime_tenure$seq=="14380",]
#lifetime_tenure[lifetime_tenure$seq=="39412",]
saveRDS(pcf, file = "pcf.rds")


# We use the function plotPredictSurvProb to plot the predicted survival curves for new
# subjects for a given modeling strategy. It applies the predictSurvProb method to predict at
# all event times but other time points can be selected. The following code produces the curves
# in Figure 1.
#ex_accts <- c(1,5,8)
head(test_x[,c("status","seq")],20)
ex_accts <- c(1,8,3,11)
test_x[ex_accts,]
#ex_accts <- c(2,6,9)
par(mfrow = c(2, 2))
lapply(ex_accts, function(x) {
  plotPredictSurvProb(fitcox, newdata = test_x[x, ], lty = 1, col=c("black"))
  plotPredictSurvProb(fitrsf, newdata = test_x[x, ], add = TRUE, lty = 2, col=c("red"))
  plotPredictSurvProb(fitcforest, newdata = test_x[x, ], add = TRUE, lty = 3, col=c("blue"))
})

legend(0.5, 0.5, c("Cox Regression", "Random Survival Forest", "Conditional Random Forest"), lty = 1:3, col=c("black", "red", "blue"))
par(mfrow = c(1, 1))


# * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * * * * * * * * * * * * * * *

# * * * Extrapolate survival probabilities using exponential model
NbRows <- dim(test_x)[1]
NbCols <- dim(test_x)[2]
#NbRows <- 2
#NbCols <- 240
### Extract last survival probability (to observe 1 acct as example)
#last_survprob <- rep(0,NbRows)
#for(i in 1:NbRows) {
#  last_survprob[i] <- pcox[,i][max_time]
#}
#summary(last_survprob)
#match(min(last_survprob),last_survprob)

#pcox[,max_time]
#summary(pcox[,max_time])
#pcox[match(min(pcox[,max_time]),pcox[,max_time]),]

MaxMonths <- 240
newTime <- seq(1, MaxMonths)
#k <- match(min(pcox[,max_time]),pcox[,max_time])


# # # # #  COX MODEL
#NbRows <- 10
median_cox <- rep(0,NbRows)
for(k in 1:NbRows)
{
  
#test[k,]
#pcox[k,]
#k <- 2
pt <- as.data.frame(pcox[k,])
pt$n <- seq(1, max_time)
names(pt) <- c("Surv_Prob","Time")
#plot(pt$Time,pt$Surv_Prob, ylim=c(0,1))
exp_model <- nls(formula = Surv_Prob ~ b0 * exp(b1 * Time), 
                 data = pt,
                 trace = FALSE, model = TRUE, 
                 start = list(b0 =1 ,b1 = 0.01))
fit2 <- fitted.values(exp_model)
err2 <- RMSE(fit2, pcox[k,])
err2
#plot(pt$Time,pt$Surv_Prob, ylim=c(0,1))
#lines(pt$Time,fitted(exp_model),col=2)
predicted_cox <- predict(exp_model, list(Time=newTime))
#plot(predicted_cox)
#NbRows <- 1
#median_cox <- rep(0,NbRows)
#for(i in 1:NbRows)
#{
median_cox[k] <- min(which(predicted_cox < 0.5))
if (median_cox[k] == Inf) {median_cox[k] = MaxMonths}

}

head(predicted_cox)
#median_cox
summary(median_cox)
hist(median_cox)



# # # # # Random Survival Forest
#NbRows <- 14
median_rsf <- rep(0,NbRows)
for(k in 1:NbRows)
{
  
  #test[k,]
  #prsf[k,]
  #k<-14
  pt <- as.data.frame(prsf[k,])
  pt$n <- seq(1, max_time)
  names(pt) <- c("Surv_Prob","Time")
  #plot(pt$Time,pt$Surv_Prob, ylim=c(0,1))
  exp_model <- nls(formula = Surv_Prob ~ b0 * exp(b1 * Time), 
                   data = pt, 
                   trace = FALSE, model = TRUE, 
                   start = list(b0 =1 ,b1 = 0.01))
  #fit2 <- fitted.values(exp_model)
  #err2 <- RMSE(fit2, prsf[k,])
  #err2
  #plot(pt$Time,pt$Surv_Prob, ylim=c(0,1))
  #lines(pt$Time,fitted(exp_model),col=2)
  predicted_rsf <- predict(exp_model, list(Time=newTime))
  #plot(predicted_rsf)
  #NbRows <- 1
  #median_cox <- rep(0,NbRows)
  #for(i in 1:NbRows)
  #{
  median_rsf[k] <- min(which(predicted_rsf < 0.5))
  if (median_rsf[k] == Inf) {median_rsf[k] = MaxMonths}
  
}

median_rsf
summary(median_rsf)
hist(median_rsf)



# # # # # Conditional Random Forest
#NbRows <- 10
#median_rsf <- rep(0,NbRows)

tail(pcf,40)

median_cf <- rep(0,NbRows)
for(k in 1:NbRows)
{
  
  #k<- 3163
  #test[k,]
  #pcf[k,]
  pt <- as.data.frame(pcf[k,])
  pt$n <- seq(1, max_time)
  names(pt) <- c("Surv_Prob","Time")
  #plot(pt$Time,pt$Surv_Prob, ylim=c(0,1))
  exp_model <- nls(formula = Surv_Prob ~ b0 * exp(b1 * Time), 
                   data = pt,
                   trace = FALSE, model = TRUE, 
                   start = list(b0 =1 ,b1 = 0.01))
  #str(exp_model)
  #fit2 <- fitted.values(exp_model)
  #err2 <- RMSE(fit2, pcf[k,])
  #err2
  #plot(pt$Time,pt$Surv_Prob, ylim=c(0,1))
  #lines(pt$Time,fitted(exp_model),col=2)
  predicted_cf <- predict(exp_model, list(Time=newTime))

  #plot(predicted_cf,type="l")
  #lines(pt$Time,fitted(exp_model),col=2,type="o")

  #NbRows <- 1
  #median_cox <- rep(0,NbRows)
  #for(i in 1:NbRows)
  #{
  median_cf[k] <- min(which(predicted_cf < 0.7))
  if (median_cf[k] == Inf) {median_cf[k] = MaxMonths}
  
}

#median_cf
summary(median_cf)
describe(median_cf)
quantile(median_cf, c(0.01,0.05,0.10,0.25,0.5,0.75,0.90,0.95,0.99))
hist(median_cf)
hist(median_cf[median_cf<100])
h = hist(median_cf) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)



# * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * * * * * * * * * * * * * * *
# Save an object to a file
saveRDS(fitcox, file = "fitcox.rds")
saveRDS(fitrsf, file = "fitrsf.rds")
saveRDS(fitcforest, file = "fitcforest.rds")

# TODO: Final inputs using FitCcForest
#sc_1, bal_1, rev_1, mths_since_lastact, CHD_LIFE_1_CYCLE_DELQ_CT, CHD_LS_AMT_PAYMENT, 
#der_overlimit_amt, Utiliz_1, sc_std_9, sc_max_12, bal_std_3, bal_mean_12, bal_min_12, 
#debittx_min_12, B_3_sc_, B_12_sc_, B_6_bal_, B_9_bal_, B_12_bal_, B_3_rev_, B_9_rev_, 
#B_9_csh_, last_15_DPD_q_12, useofcash_q_3, Income, Hold_Amount_termdepos, NbMonths_tenure, 
#n_PLOAN, max_dpd_PLOAN, pc_balpaid_PLOAN, peri_AUT, peri_MORTG, profi_1_T, der_ever_bad_N, 
#Country_Tier_1, Country_Tier_2 


# Restore the object
#readRDS(file = "fitcox.rds")
#readRDS(file = "fitrsf.rds")
#readRDS(file = "fitcforest.rds")


# * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * * * * * * * * * * * * * * *

# merge median lifetime data to NEW_TEST dataset by seq2
names(test_x)
test_x$seq2 <- seq(1, dim(test_x)[1])

median_cox_df <- as.data.frame(median_cox)
median_cox_df$seq2 <- seq(1, dim(median_cox_df)[1])

median_rsf_df <- as.data.frame(median_rsf)
median_rsf_df$seq2 <- seq(1, dim(median_rsf_df)[1])

median_cf_df <- as.data.frame(median_cf)
median_cf_df$seq2 <- seq(1, dim(median_cf_df)[1])

test_x2 <- merge(test_x, median_cox_df, by="seq2")
test_x2 <- merge(test_x2, median_rsf_df, by="seq2")
test_x2 <- merge(test_x2, median_cf_df, by="seq2")
names(test_x2)
head(test_x2)

cor(test_x2$median_cox, test_x2$median_cf)
cor(test_x2$time, test_x2$median_cox)
cor(test_x2$time, test_x2$median_rsf)
cor(test_x2$time, test_x2$median_cf)

summary(test_x2$median_cox)
summary(test_x2$median_cf)
library(Hmisc)
describe(test_x2$median_cf)

#findCorrelation(median_cox, median_rsf, median_cf)
# Ensemble Median Lifetime
#w1 <- 0.1
#w2 <- 0.1
#w3 <- 0.8
#test_x2$MedianLifetime_Mix <- round((w1*test_x2$median_cox + 
#                                       w2*test_x2$median_rsf + 
#                                       w3*test_x2$median_cf)/3,0)
#cor(test_x2$time, test_x2$MedianLifetime_Mix)
#hist(test_x2$MedianLifetime_Mix)
#describe(test_x2$MedianLifetime_Mix)

#test_x2$MedianLifetime_Mix <- apply(test_x2[,c("median_cox", "median_rsf", "median_cf")], 1, FUN=min)
#cor(test_x2$time, test_x2$MedianLifetime_Mix)

test_x2$MedianLifetime_Mix <- test_x2$median_cf
#test_x2$MedianLifetime_Mix <- test_x2$median_cox
cor(test_x2$time, test_x2$MedianLifetime_Mix)

# Confusion Matrix (Accuracy to predict FLAG_CLOSED)
test_x2$closed_pred <- 0
#test_x2$closed_pred[test_x2$median_cf < 36] <- 1
test_x2$closed_pred[test_x2$MedianLifetime_Mix <= 12] <- 1
#test_x2$closed_pred[test_x2$MedianLifetime_Mix < max_time] <- 1

val_table <- table(test_x2$status, test_x2$closed_pred)
library(caret)
confusionMatrix(val_table)


# * * * * * * * * * * * * * * * * * * * * * * *
#names(test_x2)
#names(data_all)[2] <- "account_num"
#data_merge <- merge(test_x2,data_all[,c("account_num","survival_months_aft","flag_closed_after")],by="row.names",all.x=TRUE)
#names(data_merge)

#summary(data_merge$flag_closed_after)
#summary(data_merge$survival_months_aft)

#library(psych)
#describeBy(data_merge$MedianLifetime_Mix, data_merge$flag_closed_after)

#plot(test_x2$time, test_x2$MedianLifetime_Mix)


# * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * * * * * * * * * * * * * * *
# survival time Validation

#summary(data_merge$time)
#dim(data_merge)
#summary(data_merge$time[data_merge$time < max_time])
#summary(data_merge$median_cf[data_merge$time< max_time])

test_x2$Time_Gr <- cut(as.numeric(test_x2$time), c(NA,-Inf,12,24,36,48,Inf), include.lowest=TRUE)
test_x2$PredTime_CF_Gr <- cut(as.numeric(test_x2$median_cf), c(NA,-Inf,12,24,36,48,Inf), include.lowest=TRUE)
test_x2$PredTime_RSF_Gr <- cut(as.numeric(test_x2$median_rsf), c(NA,-Inf,12,24,36,48,Inf), include.lowest=TRUE)
test_x2$PredTime_Cox_Gr <- cut(as.numeric(test_x2$median_cox), c(NA,-Inf,12,24,36,48,Inf), include.lowest=TRUE)
table(test_x2$Time_Gr)
table(test_x2$PredTime_CF_Gr)
table(test_x2$PredTime_RSF_Gr)
table(test_x2$PredTime_Cox_Gr)
prop.table(table(test_x2$Time_Gr,test_x2$PredTime_CF_Gr),1)
prop.table(table(test_x2$Time_Gr,test_x2$PredTime_Cox_Gr),1)

hist(test_x2$median_cf/12)


# * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * * * * * * * * * * * * * * *

# TODO: Stats for presentation
top_drivers_boruta

# Behavior score groups
names(test_x2)
test_x2$CRG_Gr <- cut(as.numeric(test_x2$crg_1), c(NA,0,1,2,3,4,5,6,7,8,Inf), include.lowest=TRUE)
#prop.table(table(test_x2$BS_Gr,test_x2$status),1)
table(test_x2$CRG_Gr)
library(psych)
describeBy(test_x2$MedianLifetime_Mix, test_x2$CRG_Gr)


# Profile (R, T, I)
test_x2$profile <- 2
test_x2$profile[test_x2$flag_revolver_12m == 1] <- 1
test_x2$profile[test_x2$flag_other_12m == 1] <- 3
describeBy(test_x2$MedianLifetime_Mix, test_x2$profile,2)
#describeBy(test_x2$MedianLifetime_Mix, test_x2$der_revol_tran_6mth_R,2)

#test_x2$der_revol_tran_6mth_R <- 0
#test_x2$der_revol_tran_6mth_R[test_x2$profile == 1] <- 1
#table(test_x2$der_revol_tran_6mth_R)

# Median Groups
summary(test_x2$MedianLifetime_Mix)
test_x2$MedianLT_Gr <- cut(as.numeric(test_x2$MedianLifetime_Mix), c(1,12,36,60,120,Inf), include.lowest=TRUE)
table(test_x2$MedianLT_Gr)
prop.table(table(test_x2$MedianLT_Gr))
prop.table(table(test_x2$MedianLT_Gr, test_x2$status),1)
describeBy(test_x2$status, test_x2$MedianLT_Gr,2)
prop.table(table(test_x2$status))


# Stats by Median Groups
names(test_x2)
describeBy(test_x2$lim_min_12, test_x2$MedianLT_Gr,2)
describeBy(test_x2$util_0, test_x2$MedianLT_Gr,2)
describeBy(test_x2$bal_1, test_x2$MedianLT_Gr,2)
describeBy(test_x2$profi_0_R, test_x2$MedianLT_Gr,2)
describeBy(test_x2$profi_0_T, test_x2$MedianLT_Gr,2)
describeBy(test_x2$profi_0_O, test_x2$MedianLT_Gr,2)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Survival plots (for Presentation)
#survfit(fitcox)
train_X <- train_data[,fitcox$In]
names(train_X)

res.cox <- coxph(Surv(time, status) ~ ., data = train_X)
res.cox
summary(res.cox)

survfit(res.cox)
survfit(res.cox)$surv
plot(survfit(res.cox)$surv)
library(survminer)
ggsurvplot(survfit(res.cox), data=train, palette = "#2E9FDF", ggtheme = theme_minimal())


# Fit a Cox proportional hazards model in new data 
fit_test <- survfit(res.cox, newdata = test)
str(fit_test)
dim(test)







# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Extrapolate survival probabilities using exponential model (for overall  survival)
NbRows <- dim(test)[1]
NbCols <- dim(test)[2]
max_time <- max(test$time)
Time_Months <- c(1:max_time)
#NbRows <- 2
#NbCols <- 240

MaxMonths <- 240
#k <- 30288
#k <- match(min(last_survprob),last_survprob)
#k <- 1
#test[k,]
#fit_test$surv[,k]
#surv_probs <- fit_test$surv[,k]
#surv_probs <- data.survfit$surv
surv_probs <- survfit(res.cox)$surv

pt <- as.data.frame(surv_probs)
pt$n <- seq(1, length(surv_probs))
names(pt) <- c("Surv_Prob","Time")
plot(pt$Time,pt$Surv_Prob, ylim=c(0,1))
#plot(pt$Time,log(pt$Surv_Prob))

#linear_model <- lm(Surv_Prob ~ Time, data = pt)
#summary(linear_model)
#fit1 <- fitted.values(linear_model)
#err1 = RMSE(fit1, fit_test$surv[,k])

exp_model <- nls(formula = Surv_Prob ~ b0 * exp(b1 * Time), 
                 data = pt,
                 trace = TRUE, model = TRUE, 
                 start = list(b0 =1 ,b1 = 0.01))
#str(exp_model$m)
#sqrt(sum(exp_model$m$resid()))
fit2 <- fitted.values(exp_model)
library(MLmetrics)
err2 <- RMSE(fit2, surv_probs)
err2
plot(pt$Time,pt$Surv_Prob, ylim=c(0,1),xlim=c(0,MaxMonths),xlab="Nb Months",ylab="Survival Probability")
lines(pt$Time,fitted(exp_model),col=2)


#newTime <- data.frame(Time=seq(1, MaxMonths))
newTime <- seq(1, MaxMonths)
predicted_exp <- predict(exp_model, list(Time=newTime))
#plot(predicted_exp,xlim=c(0,33),ylim=c(0,1))
plot(predicted_exp,ylim=c(0,1),xlim=c(0,MaxMonths),xlab="Nb Months",ylab="Survival Probability")

pred_median_surv <- min(which(predicted_exp < 0.5))
pred_median_surv
#min(which(surv_probs < 0.5))
summary(median_cf)



