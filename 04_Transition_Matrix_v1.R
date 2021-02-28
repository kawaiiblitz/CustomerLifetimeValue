

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# B E H A V I O R    T R A N S I T I O N    M A T R I X
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Data 201610

setwd("C:/Users/s1354719/Documents/BNS/Projects/201806_LTV/Canada/Data")

library(haven)
###### Importing data
#all_data <- read_sas("./SampleWindow_TimeSeries.sas7bdat")
X_201610 <- read_sas("./Sample_RevDrivers_201610.sas7bdat")
dim(X_201610)
# Removing some variables
names(X_201610)
#str(X_201610)
#table(X_201610$dpd_1)

X_201610 <- as.data.frame(X_201610)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

names_ts <- names(X_201610 %>% select(contains("total_revenue")))
names_ts <- names_ts[c(1:48)]
#names_ts <- rev(names_ts)

#names_ts_after <- names(X_201610 %>% select(contains("rev_aft")))
#names_ts_after <- names_ts_after[c(1:12)]


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Assign cluster for time series and create dummys

#valid_ts_train <- data_active[,c(names_ts)]
valid_ts_train <- X_201610[,c(names_ts)]
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
#head(log_ts_train)
dim(log_ts_train)
log_ts_train[1,]

sep_ts_train <- log_ts_train
dim(sep_ts_train)
#sep_ts_train[3,]

# Assign each observation in TEST to the highest correlated time series in TRAIN 

Avg_ts_cluster <- readRDS(file = "avg_ts_mv_seg.rds")
names(Avg_ts_cluster)
Avg_ts_cluster <- Avg_ts_cluster[,c(1,14:61)]
dim(Avg_ts_cluster)
names(Avg_ts_cluster)

stop <- nrow(sep_ts_train)
#stop <- 7
#sep_ts_train[stop,]
ts_cluster_mv <- rep(0,stop)
for(r in 1:stop)
{
  #r <- 1
  nb_ts_clus <- nrow(Avg_ts_cluster)
  #df_newobs <- rbind(setNames(Avg_ts_cluster[,2:61],names(sep_ts_train)),sep_ts_train[r,])
  df_newobs <- rbind(setNames(Avg_ts_cluster[,2:49],names(sep_ts_train)),sep_ts_train[r,])
  
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
X_201610$ts_cluster_mv <- ts_cluster_mv

dummy_ts_cluster <- dummy("ts_cluster_mv", as.data.frame(X_201610), sep="_")
X_201610 <- data.frame(X_201610,dummy_ts_cluster)
#data_active5 <- data_active5[ , !(names(data_active5) %in% c("ts_cluster_mv"))]
names(X_201610)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
#dummy_OutsBal_Q5 <- dummy("Q5_Outstanding_Bal", as.data.frame(X_201610), sep="_")
#dummy_TotalRevenue_Q5 <- dummy("Q5_TotalRevenue", as.data.frame(X_201610), sep="_")

#data_all_NewDummies <- data.frame(
#  dummy_OutsBal_Q5 
#  , dummy_TotalRevenue_Q5 
#)


dummy_ProdCode <- dummy("PRODUCT_CODE", as.data.frame(X_201610), sep="_")
dummy_SubProdCode <- dummy("Sub_Product_Code", as.data.frame(X_201610), sep="_")
dummy_ProbeARI <- dummy("PROBE_ARI", as.data.frame(X_201610), sep="_")
dummy_Province <- dummy("ACCT_PROV_STATE_CD", as.data.frame(X_201610), sep="_")
dummy_Channel <- dummy("CHANNEL", as.data.frame(X_201610), sep="_")
dummy_BureauWorst <- dummy("GO03_CR_BUREAU_WORST_RT", as.data.frame(X_201610), sep="_")
dummy_FraudAlert <- dummy("FRAUD_ALERT_F", as.data.frame(X_201610), sep="_")
#names(as.data.frame(dummy_CountryTier))

dummy_NbDelqDays_3m <- dummy("num_delq_days_q_3", as.data.frame(X_201610), sep="_")
dummy_CashAdvAmt_3m <- dummy("cash_adv_amt_q_3", as.data.frame(X_201610), sep="_")
dummy_CountUtiliz50pct_3m <- dummy("count_utiliz_50perc_q_3", as.data.frame(X_201610), sep="_")
dummy_CountUtiliz75pct_3m <- dummy("count_utiliz_75perc_q_3", as.data.frame(X_201610), sep="_")
dummy_CountUtiliz90pct_3m <- dummy("count_utiliz_90perc_q_3", as.data.frame(X_201610), sep="_")
dummy_BalEqLim_3m <- dummy("bal_eq_lim_q_3", as.data.frame(X_201610), sep="_")
dummy_BalEqLim_6m <- dummy("bal_eq_lim_q_6", as.data.frame(X_201610), sep="_")
dummy_BalEqLim_9m <- dummy("bal_eq_lim_q_9", as.data.frame(X_201610), sep="_")
dummy_BalEqLim_12m <- dummy("bal_eq_lim_q_12", as.data.frame(X_201610), sep="_")
dummy_Last15dpd_3m <- dummy("last_15_DPD_q_3", as.data.frame(X_201610), sep="_")
dummy_Last30dpd_3m <- dummy("last_30_DPD_q_3", as.data.frame(X_201610), sep="_")
dummy_Last60dpd_3m <- dummy("last_60_DPD_q_3", as.data.frame(X_201610), sep="_")
dummy_Last90dpd_3m <- dummy("last_90_DPD_q_3", as.data.frame(X_201610), sep="_")
dummy_UseOfCash_3m <- dummy("useofcash_q_3", as.data.frame(X_201610), sep="_")
dummy_Emprc_Exclsn_CD <- dummy("GO75_EMPRC_EXCLSN_CD", as.data.frame(X_201610), sep="_")
dummy_NbBankrpc <- dummy("PR01_NUM_OF_BNKRPC", as.data.frame(X_201610), sep="_")
dummy_NbActTrades60d <- dummy("AT64_NUM_ACT_TRDS_WITH_60_DY", as.data.frame(X_201610), sep="_")
dummy_CreditLim_Q5 <- dummy("Q5_CREDIT_LIM", as.data.frame(X_201610), sep="_")
dummy_MOB_Q5 <- dummy("Q5_MOB", as.data.frame(X_201610), sep="_")
dummy_NetRAR_Q5 <- dummy("Q5_Net_RAR", as.data.frame(X_201610), sep="_")
dummy_OutsBal_Q5 <- dummy("Q5_Outstanding_Bal", as.data.frame(X_201610), sep="_")
dummy_TotalRevenue_Q5 <- dummy("Q5_TotalRevenue", as.data.frame(X_201610), sep="_")


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
valid_dummies <- names(data_all_NewDummies)[!(names(data_all_NewDummies) %in% c(drop_dummy))]

#data2 <- data.frame(data, dummy_RevTrans6m, dummy_EverBad, dummy_RiskProfile,dummy_CountryTier)
data_all_flags <- data_all_NewDummies[,valid_dummies]
dim(data_all_flags)
names(data_all_flags)


# Adding new FLAGS to NUMERIC data in data3
X_201610 <- data.frame(X_201610,data_all_flags)
dim(X_201610)
names(X_201610)

#X16 <- as.data.frame(X_201610)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
top_drivers_revenue <- readRDS(file = "top_drivers_revenue.rds")
x_ts_cluster <- X_201610 %>% select(contains("ts_cluster_mv"))
X16 <- data.frame(X_201610[,top_drivers_revenue],x_ts_cluster)
dim(X16)
names(X16)
#nobs=dim(X16)[1]

#library(tidyverse) 
#X16 <- X16 %>% select(-contains("total_revenue"))
#names(X16)

#RevDrivers.kmeans <- readRDS(file = "RevDrivers.kmeans.rds")
#attributes(RevDrivers.kmeans)
#(RevDrivers.kmeans$centers)
#colnames(RevDrivers.kmeans$centers)



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# # # # # #  Checking for missing values
library(mice) #For Missing Values Exploration
pMiss <- function(x){sum(is.na(x))/length(x)*100}
pct_missing <- as.data.frame(apply(X16,2,pMiss))
#pct_missing[5,1]
#pct_missing$`apply(data, 2, pMiss)`
vars_missing <- names(X16)[pct_missing$`apply(X16, 2, pMiss)` > 0]
vars_missing


# * * * * * * * * * *
# # # # # #  Replacing  missing values (Flags)
flag_impute <- 0
for(i in 1:length(vars_missing)) 
{
  #i <- 211
  # statistics to replace outliers
  l <- length(unique(X16[,vars_missing[i]]))
  s <- sum(unique(is.na(X16[,vars_missing[i]])))
  
  if (l <= 3 & s > 0) 
  {
    X16[is.na(X16[,vars_missing[i]]),vars_missing[i]] <- 0
    #is.na(X16[,vars_missing[i]]) <- 0
    flag_impute <- flag_impute + 1
  } 
}
flag_impute


# * * * * * * * * * *
# # # # # #  Replacing  missing values (Continuous)
pct_missing <- as.data.frame(apply(X16,2,pMiss))
vars_missing <- names(X16)[pct_missing$`apply(X16, 2, pMiss)` > 0]
vars_missing

Vars_For_Mean_Imput <- vars_missing
#head(str(X16[,Vars_For_Mean_Imput]),200)

#Vars_For_Mean_Imput <- Vars_For_Mean_Imput[Vars_For_Mean_Imput != 'acct_close_date']

summary(X16[,Vars_For_Mean_Imput])
for(j in 1:length(Vars_For_Mean_Imput)) 
{ 
  X16[is.na(X16[,Vars_For_Mean_Imput[j]]),Vars_For_Mean_Imput[j]]=colMeans(X16[,Vars_For_Mean_Imput],na.rm=TRUE)[j]
}  
summary(X16[,Vars_For_Mean_Imput])



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# # # # # # Identify and replace outliers (only for CONT vars)

library(Hmisc)
#describe(X16$Avg_Revenue_after)
library(timeDate)
names(X16)
#kurtosis(X16$Avg_Revenue_after)
#hist(X16$Avg_Revenue_after)

#library(randomForest)
#library(tsoutliers)
#outlier(X16$flag_cash12)

Vars_For_Outlier_Imput <- names(X16)
Vars_For_Outlier_Imput

# Removing some vars from final models that do not need trimming
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'DLQNT_DAY_NUM']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'crg_1']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'crg_min_12']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_Outstanding_Bal_0']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'MOB']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_Outstanding_Bal_2']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_0']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_2']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_4']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_3']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Sub_Product_Code_RG']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_1']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_Net_RAR_1']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_Net_RAR_2']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_1']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_2']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_3']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_4']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_5']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_6']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_7']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_8']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ACCOUNT_NUM']
sort(Vars_For_Outlier_Imput)

avg_cont1 <- as.data.frame(colMeans(X16[,Vars_For_Outlier_Imput]))
count_vars_replace <- 0
var_trimmed <- rep(0,length(Vars_For_Outlier_Imput))

for(i in 1:ncol(X16[,Vars_For_Outlier_Imput])) 
{
  #i <- 4
  # statistics to replace outliers
  lowerq = quantile(as.data.frame(X16[,Vars_For_Outlier_Imput])[,i],na.rm = TRUE)[2]
  upperq = quantile(as.data.frame(X16[,Vars_For_Outlier_Imput])[,i],na.rm = TRUE)[4]
  iqr = upperq - lowerq
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  
  quantiles <- quantile(as.data.frame(X16[,Vars_For_Outlier_Imput])[,i], c(.01, .95 ),na.rm = TRUE)
  
  #if ((sum(X16[,i] > quantiles[2]) > 0 | 
  #     kurtosis(X16[,i]) > 3) &
  #    length(unique(X16[,i])) > 10
  #) 
  
  if ((sum(as.data.frame(X16[,Vars_For_Outlier_Imput])[,i] > quantiles[2]) > 0 | 
       kurtosis(as.data.frame(X16[,Vars_For_Outlier_Imput])[,i]) > 3) &
      length(unique(as.data.frame(X16[,Vars_For_Outlier_Imput])[,i])) > 10
  ) 
  {
    count_vars_replace <- count_vars_replace+1
    var_trimmed[i] <- 1
    
    X16[,Vars_For_Outlier_Imput][as.data.frame(X16[,Vars_For_Outlier_Imput])[,i] > quantiles[2],i] <- quantiles[2]
    X16[,Vars_For_Outlier_Imput][as.data.frame(X16[,Vars_For_Outlier_Imput])[,i] < quantiles[1],i] <- quantiles[1]
  } 
}

#table(X16$DLQNT_FLAG)

#summary(data_wout_nzv[,Vars_For_Outlier_Imput])
count_vars_replace
var_trimmed
names(X16)[var_trimmed==1]
sort(names(X16)[var_trimmed==1])

# Compare ORIGINAL vs TRIMMED average
avg_cont2 <- as.data.frame(colMeans(X16[,Vars_For_Outlier_Imput]))
head(cbind(avg_cont1,avg_cont2),50)

summary(X16)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Clustering of Revenue drivers segments (K-MEANS)

#library(tidyverse) 
#X16 <- X16 %>% select(-contains("dpd_1"))

#new_X16 <- data.frame(X16,X_201610[,"dpd_1"])
#summary(new_X16)
#X16 <- new_X16

library(stats)
library(caret)
library(clue)

#library(tidyverse) 
#X16 <- X16 %>% select(-contains("flag_revolver_12m"))
#X16 <- X16 %>% select(-contains("flag_transactor_12m"))
#X16 <- X16 %>% select(-contains("flag_other_12m"))

# Normalizing the train data with key drivers only for k-means clustering:
#scale_train_drivers <- scale(train_drivers[, 1:7])
names(X16)
#scale_X16 <- scale(X16[,2:46])
scale_X16 <- scale(X16)
summary(scale_X16)

# * * * * * * * * * * * * * * * * * * 
# Applying clustering to TEST dataset
RevDrivers.kmeans <- readRDS(file = "RevDrivers.kmeans.rds")
#attributes(RevDrivers.kmeans)
RevDrivers.kmeans$centers

X16$kmeans_clusters_16 <- cl_predict(RevDrivers.kmeans, scale_X16)
#warnings()
#traceback()
prop.table(table(X16$kmeans_clusters_16))
plot(prop.table(table(X16$kmeans_clusters_16)),type = 'h')

#X16$kmeans_clusters_16 <- as.factor(X16$kmeans_clusters_16)
#mean_table_test <- aggregate(X16$Target, list(X16$kmeans_clusters_16), mean)
#aggregate(X16$Target, list(X16$kmeans_clusters_16), median)
#plot(mean_table_test)


# * * * * * * * * * * * * * * * * * * 
# AVERAGE of Revenue drivers per K-means cluster
library(doBy)
names(X16)
Avg_X16_Cluster <- summaryBy(
  X16 ~ kmeans_clusters_16, 
  data = X16, FUN = function(x) { c(m = mean(x, na.rm=TRUE)) })
Avg_X16_Cluster


























# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Data 201710

setwd("C:/Users/s1354719/Documents/BNS/Projects/201806_LTV/Canada/Data")

library(haven)
###### Importing data
#all_data <- read_sas("./SampleWindow_TimeSeries.sas7bdat")
X_201710 <- read_sas("./Sample_RevDrivers_201710.sas7bdat")
dim(X_201710)
# Removing some variables
names(X_201710)
#str(X_201710)
#table(X_201710$dpd_1)

X_201710 <- as.data.frame(X_201710)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

names_ts <- names(X_201710 %>% select(contains("total_revenue")))
#names_ts <- names_ts[c(1:48)]
names_ts <- names_ts[c(1:60)]
#names_ts <- rev(names_ts)

#names_ts_after <- names(X_201710 %>% select(contains("rev_aft")))
#names_ts_after <- names_ts_after[c(1:12)]


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Assign cluster for time series and create dummys

#valid_ts_train <- data_active[,c(names_ts)]
valid_ts_train <- X_201710[,c(names_ts)]
names(valid_ts_train)
head(valid_ts_train)

for(k in 1:nrow(valid_ts_train))
{
  k<-2
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
#head(log_ts_train)
dim(log_ts_train)
log_ts_train[1,]

sep_ts_train <- log_ts_train
dim(sep_ts_train)
#sep_ts_train[3,]

# Assign each observation in TEST to the highest correlated time series in TRAIN 

Avg_ts_cluster <- readRDS(file = "avg_ts_mv_seg.rds")
names(Avg_ts_cluster)
#Avg_ts_cluster <- Avg_ts_cluster[,c(1,14:61)]
dim(Avg_ts_cluster)
names(Avg_ts_cluster)

stop <- nrow(sep_ts_train)
#stop <- 7
#sep_ts_train[stop,]
ts_cluster_mv <- rep(0,stop)
for(r in 1:stop)
{
  #r <- 1
  nb_ts_clus <- nrow(Avg_ts_cluster)
  df_newobs <- rbind(setNames(Avg_ts_cluster[,2:61],names(sep_ts_train)),sep_ts_train[r,])
  #df_newobs <- rbind(setNames(Avg_ts_cluster[,2:49],names(sep_ts_train)),sep_ts_train[r,])
  
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
X_201710$ts_cluster_mv <- ts_cluster_mv

dummy_ts_cluster <- dummy("ts_cluster_mv", as.data.frame(X_201710), sep="_")
X_201710 <- data.frame(X_201710,dummy_ts_cluster)
#data_active5 <- data_active5[ , !(names(data_active5) %in% c("ts_cluster_mv"))]
names(X_201710)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
#dummy_OutsBal_Q5 <- dummy("Q5_Outstanding_Bal", as.data.frame(X_201710), sep="_")
#dummy_TotalRevenue_Q5 <- dummy("Q5_TotalRevenue", as.data.frame(X_201710), sep="_")

#data_all_NewDummies <- data.frame(
#  dummy_OutsBal_Q5 
#  , dummy_TotalRevenue_Q5 
#)


dummy_ProdCode <- dummy("PRODUCT_CODE", as.data.frame(X_201710), sep="_")
dummy_SubProdCode <- dummy("Sub_Product_Code", as.data.frame(X_201710), sep="_")
dummy_ProbeARI <- dummy("PROBE_ARI", as.data.frame(X_201710), sep="_")
dummy_Province <- dummy("ACCT_PROV_STATE_CD", as.data.frame(X_201710), sep="_")
dummy_Channel <- dummy("CHANNEL", as.data.frame(X_201710), sep="_")
dummy_BureauWorst <- dummy("GO03_CR_BUREAU_WORST_RT", as.data.frame(X_201710), sep="_")
dummy_FraudAlert <- dummy("FRAUD_ALERT_F", as.data.frame(X_201710), sep="_")
#names(as.data.frame(dummy_CountryTier))

dummy_NbDelqDays_3m <- dummy("num_delq_days_q_3", as.data.frame(X_201710), sep="_")
dummy_CashAdvAmt_3m <- dummy("cash_adv_amt_q_3", as.data.frame(X_201710), sep="_")
dummy_CountUtiliz50pct_3m <- dummy("count_utiliz_50perc_q_3", as.data.frame(X_201710), sep="_")
dummy_CountUtiliz75pct_3m <- dummy("count_utiliz_75perc_q_3", as.data.frame(X_201710), sep="_")
dummy_CountUtiliz90pct_3m <- dummy("count_utiliz_90perc_q_3", as.data.frame(X_201710), sep="_")
dummy_BalEqLim_3m <- dummy("bal_eq_lim_q_3", as.data.frame(X_201710), sep="_")
dummy_BalEqLim_6m <- dummy("bal_eq_lim_q_6", as.data.frame(X_201710), sep="_")
dummy_BalEqLim_9m <- dummy("bal_eq_lim_q_9", as.data.frame(X_201710), sep="_")
dummy_BalEqLim_12m <- dummy("bal_eq_lim_q_12", as.data.frame(X_201710), sep="_")
dummy_Last15dpd_3m <- dummy("last_15_DPD_q_3", as.data.frame(X_201710), sep="_")
dummy_Last30dpd_3m <- dummy("last_30_DPD_q_3", as.data.frame(X_201710), sep="_")
dummy_Last60dpd_3m <- dummy("last_60_DPD_q_3", as.data.frame(X_201710), sep="_")
dummy_Last90dpd_3m <- dummy("last_90_DPD_q_3", as.data.frame(X_201710), sep="_")
dummy_UseOfCash_3m <- dummy("useofcash_q_3", as.data.frame(X_201710), sep="_")
dummy_Emprc_Exclsn_CD <- dummy("GO75_EMPRC_EXCLSN_CD", as.data.frame(X_201710), sep="_")
dummy_NbBankrpc <- dummy("PR01_NUM_OF_BNKRPC", as.data.frame(X_201710), sep="_")
dummy_NbActTrades60d <- dummy("AT64_NUM_ACT_TRDS_WITH_60_DY", as.data.frame(X_201710), sep="_")
dummy_CreditLim_Q5 <- dummy("Q5_CREDIT_LIM", as.data.frame(X_201710), sep="_")
dummy_MOB_Q5 <- dummy("Q5_MOB", as.data.frame(X_201710), sep="_")
dummy_NetRAR_Q5 <- dummy("Q5_Net_RAR", as.data.frame(X_201710), sep="_")
dummy_OutsBal_Q5 <- dummy("Q5_Outstanding_Bal", as.data.frame(X_201710), sep="_")
dummy_TotalRevenue_Q5 <- dummy("Q5_TotalRevenue", as.data.frame(X_201710), sep="_")


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
valid_dummies <- names(data_all_NewDummies)[!(names(data_all_NewDummies) %in% c(drop_dummy))]

#data2 <- data.frame(data, dummy_RevTrans6m, dummy_EverBad, dummy_RiskProfile,dummy_CountryTier)
data_all_flags <- data_all_NewDummies[,valid_dummies]
dim(data_all_flags)
names(data_all_flags)


# Adding new FLAGS to NUMERIC data in data3
X_201710 <- data.frame(X_201710,data_all_flags)
dim(X_201710)
names(X_201710)

#X17 <- as.data.frame(X_201710)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
top_drivers_revenue <- readRDS(file = "top_drivers_revenue.rds")
x_ts_cluster <- X_201710 %>% select(contains("ts_cluster_mv"))
X17 <- data.frame(X_201710[,top_drivers_revenue],x_ts_cluster)
dim(X17)
names(X17)
summary(X17)
#nobs=dim(X17)[1]

#library(tidyverse) 
#X17 <- X17 %>% select(-contains("total_revenue"))
#names(X17)

#RevDrivers.kmeans <- readRDS(file = "RevDrivers.kmeans.rds")
#attributes(RevDrivers.kmeans)
#(RevDrivers.kmeans$centers)
#colnames(RevDrivers.kmeans$centers)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# # # # # #  Checking for missing values
library(mice) #For Missing Values Exploration
pMiss <- function(x){sum(is.na(x))/length(x)*100}
pct_missing <- as.data.frame(apply(X17,2,pMiss))
#pct_missing[5,1]
#pct_missing$`apply(data, 2, pMiss)`
vars_missing <- names(X17)[pct_missing$`apply(X17, 2, pMiss)` > 0]
vars_missing


# * * * * * * * * * *
# # # # # #  Replacing  missing values (Flags)
flag_impute <- 0
for(i in 1:length(vars_missing)) 
{
  #i <- 211
  # statistics to replace outliers
  l <- length(unique(X17[,vars_missing[i]]))
  s <- sum(unique(is.na(X17[,vars_missing[i]])))
  
  if (l <= 3 & s > 0) 
  {
    X17[is.na(X17[,vars_missing[i]]),vars_missing[i]] <- 0
    #is.na(X17[,vars_missing[i]]) <- 0
    flag_impute <- flag_impute + 1
  } 
}
flag_impute


# * * * * * * * * * *
# # # # # #  Replacing  missing values (Continuous)
pct_missing <- as.data.frame(apply(X17,2,pMiss))
vars_missing <- names(X17)[pct_missing$`apply(X17, 2, pMiss)` > 0]
vars_missing

Vars_For_Mean_Imput <- vars_missing
#head(str(X17[,Vars_For_Mean_Imput]),200)

#Vars_For_Mean_Imput <- Vars_For_Mean_Imput[Vars_For_Mean_Imput != 'acct_close_date']

summary(X17[,Vars_For_Mean_Imput])
for(j in 1:length(Vars_For_Mean_Imput)) 
{ 
  X17[is.na(X17[,Vars_For_Mean_Imput[j]]),Vars_For_Mean_Imput[j]]=colMeans(X17[,Vars_For_Mean_Imput],na.rm=TRUE)[j]
}  
summary(X17[,Vars_For_Mean_Imput])



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# # # # # # Identify and replace outliers (only for CONT vars)

library(Hmisc)
#describe(X17$Avg_Revenue_after)
library(timeDate)
names(X17)
#kurtosis(X17$Avg_Revenue_after)
#hist(X17$Avg_Revenue_after)

#library(randomForest)
#library(tsoutliers)
#outlier(X17$flag_cash12)

Vars_For_Outlier_Imput <- names(X17)
Vars_For_Outlier_Imput

# Removing some vars from final models that do not need trimming
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'DLQNT_DAY_NUM']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'crg_1']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'crg_min_12']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_Outstanding_Bal_0']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'MOB']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_Outstanding_Bal_2']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_0']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_2']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_4']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_3']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Sub_Product_Code_RG']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_TotalRevenue_1']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_Net_RAR_1']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'Q5_Net_RAR_2']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_1']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_2']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_3']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_4']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_5']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_6']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_7']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ts_cluster_mv_8']
Vars_For_Outlier_Imput <- Vars_For_Outlier_Imput[Vars_For_Outlier_Imput != 'ACCOUNT_NUM']
sort(Vars_For_Outlier_Imput)

avg_cont1 <- as.data.frame(colMeans(X17[,Vars_For_Outlier_Imput]))
count_vars_replace <- 0
var_trimmed <- rep(0,length(Vars_For_Outlier_Imput))

for(i in 1:ncol(X17[,Vars_For_Outlier_Imput])) 
{
  #i <- 4
  # statistics to replace outliers
  lowerq = quantile(as.data.frame(X17[,Vars_For_Outlier_Imput])[,i],na.rm = TRUE)[2]
  upperq = quantile(as.data.frame(X17[,Vars_For_Outlier_Imput])[,i],na.rm = TRUE)[4]
  iqr = upperq - lowerq
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  
  quantiles <- quantile(as.data.frame(X17[,Vars_For_Outlier_Imput])[,i], c(.01, .99 ),na.rm = TRUE)
  
  #if ((sum(X17[,i] > quantiles[2]) > 0 | 
  #     kurtosis(X17[,i]) > 3) &
  #    length(unique(X17[,i])) > 10
  #) 
  
  if ((sum(as.data.frame(X17[,Vars_For_Outlier_Imput])[,i] > quantiles[2]) > 0 | 
       kurtosis(as.data.frame(X17[,Vars_For_Outlier_Imput])[,i]) > 3) &
      length(unique(as.data.frame(X17[,Vars_For_Outlier_Imput])[,i])) > 10
  ) 
  {
    count_vars_replace <- count_vars_replace+1
    var_trimmed[i] <- 1
    
    X17[,Vars_For_Outlier_Imput][as.data.frame(X17[,Vars_For_Outlier_Imput])[,i] > quantiles[2],i] <- quantiles[2]
    X17[,Vars_For_Outlier_Imput][as.data.frame(X17[,Vars_For_Outlier_Imput])[,i] < quantiles[1],i] <- quantiles[1]
  } 
}

#table(X17$DLQNT_FLAG)

#summary(data_wout_nzv[,Vars_For_Outlier_Imput])
count_vars_replace
var_trimmed
names(X17)[var_trimmed==1]
sort(names(X17)[var_trimmed==1])

# Compare ORIGINAL vs TRIMMED average
avg_cont2 <- as.data.frame(colMeans(X17[,Vars_For_Outlier_Imput]))
head(cbind(avg_cont1,avg_cont2),50)

summary(X17$ts_cluster_mv_8)

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Clustering of Revenue drivers segments (K-MEANS)

#library(tidyverse) 
#X17 <- X17 %>% select(-contains("dpd_1"))

#new_X17 <- data.frame(X17,X_201710[,"dpd_1"])
#summary(new_X17)
#X17 <- new_X17

library(stats)
library(caret)
library(clue)

#library(tidyverse) 
#X17 <- X17 %>% select(-contains("flag_revolver_12m"))
#X17 <- X17 %>% select(-contains("flag_transactor_12m"))
#X17 <- X17 %>% select(-contains("flag_other_12m"))

# Normalizing the train data with key drivers only for k-means clustering:
#scale_train_drivers <- scale(train_drivers[, 1:7])
names(X17)
#scale_X17 <- scale(X17[,2:46])
scale_X17 <- scale(X17)
summary(scale_X17)

# * * * * * * * * * * * * * * * * * * 
# Applying clustering to TEST dataset
RevDrivers.kmeans <- readRDS(file = "RevDrivers.kmeans.rds")
#attributes(RevDrivers.kmeans)
RevDrivers.kmeans$centers

X17$kmeans_clusters_17 <- cl_predict(RevDrivers.kmeans, scale_X17)
#warnings()
#traceback()
prop.table(table(X17$kmeans_clusters_17))
plot(prop.table(table(X17$kmeans_clusters_17)),type = 'h')

prop.table(table(X16$kmeans_clusters_16))
plot(prop.table(table(X16$kmeans_clusters_16)),type = 'h')

prop.table(RevDrivers.kmeans$size)
plot(prop.table(RevDrivers.kmeans$size),type = 'h')

#X17$kmeans_clusters_17 <- as.factor(X17$kmeans_clusters_17)
#mean_table_test <- aggregate(X17$Target, list(X17$kmeans_clusters_17), mean)
#aggregate(X17$Target, list(X17$kmeans_clusters_17), median)
#plot(mean_table_test)


# * * * * * * * * * * * * * * * * * * 
# AVERAGE of Revenue drivers per K-means cluster
library(doBy)
names(X17)
Avg_X17_Cluster <- summaryBy(
  X17 ~ kmeans_clusters_17, 
  data = X17, FUN = function(x) { c(m = mean(x, na.rm=TRUE)) })
Avg_X17_Cluster
















# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

dim(X16)
dim(X17)
names(X16)
names(X17)
transition_data <- as.data.frame(cbind(X16[,"kmeans_clusters_16"],X17[,"kmeans_clusters_17"]))
head(transition_data)
names(transition_data) <- c("Segment_16","Segment_17")

transition_n <- table(transition_data$Segment_16,transition_data$Segment_17)
transition_n
transition_pctrow <- prop.table(transition_n,1) # row percentages
transition_pctrow <- as.matrix(transition_pctrow)
transition_pctrow
# Save object
saveRDS(transition_pctrow, file = "transition_pctrow.rds")
transition_pctrow <- readRDS(file = "transition_pctrow.rds")

mpow <- function(P, n) {
  if (n == 0) diag(nrow(P))
  else if (n == 1) P
  else P %*% mpow(P, n - 1)
}


nb_years_matrix <- 30
for(k in 1:nb_years_matrix)
  {
  TransMatrix_k <- mpow(transition_pctrow,k)
  assign(paste("TransMatrix", k, sep = "_"), TransMatrix_k)
  saveRDS(assign(paste("TransMatrix", k, sep = "_"), TransMatrix_k), 
          file = paste("TransMatrix", k, ".rds", sep = "_"))
  }

TransMatrix_1
TransMatrix_2
TransMatrix_30
#transition_1 <- readRDS(file = "TransMatrix_1_.rds")
sum(TransMatrix_30)

