
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
# F O R E C A S T I N G       M O D E L S
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

setwd("C:/Users/s1354719/Documents/BNS/Projects/201806_LTV/Canada/Data")

library(lubridate)
library(tidyverse) 
library(readr)
library(haven)
###### Importing data
all_data <- read_sas("./randomsample_active_201710.sas7bdat")
#all_data <- read_sas("./Master_Table_Active_201710_ALL.sas7bdat")

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


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

data_active_ts <- data.frame(data_active[,names_ts],
                            data_active[,names_ts_after])

names(data_active_ts)

#names(data_active_ts)[names(data_active_ts)=="Avg_Revenue_after"] <- 'Target'
#names(data_active_ts)[names(data_active_ts)=="data_active.dpd_1"] <- 'dpd_1'

###### Data partition (Train vs Test)
data_active_ts$seq <- seq(1, dim(data_active_ts)[1])
# % of the sample size to be selected
pct_sample <- 0.6
smp_size <- floor(pct_sample * nrow(data_active_ts))
# set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_active_ts)), size = smp_size)

train_ts <- data_active_ts[train_ind, ]
test_ts <- data_active_ts[-train_ind, ]
dim(data_active)
dim(train_ts)
dim(test_ts)
names(train_ts)
head(train_ts)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Checking simmilarities in Times Series (TS correlations)
time_series <- train_ts[,c(names_ts,names_ts_after)]
names(time_series)

#time_series[is.na(time_series)] <- 0
head(time_series,10)
length(time_series)
dim(time_series)

time_series_na <- time_series[rowSums(is.na(time_series)) > 0,]
dim(time_series_na)
head(time_series_na,10)
time_series_no_na <- time_series[rowSums(is.na(time_series)) == 0,]
dim(time_series_no_na)
head(time_series_no_na,10)


time_series <- time_series_no_na
xstart_ts <- 1
xend_ts <- length(time_series)
xnbm2predict <- 12
train_ts2 <- time_series[,(xstart_ts):(xend_ts-xnbm2predict)]
#train_ts2 <- log_time_series[,(xstart_ts):(xend_ts-xnbm2predict)]
dim(train_ts2)

train_ts2$rowsum_ts <- rowSums(train_ts2)
ts_eq_0 <- train_ts2[train_ts2$rowsum_ts <= 0,1:(xend_ts-xnbm2predict)]
dim(ts_eq_0)

ts_wout_0 <- train_ts2[train_ts2$rowsum_ts > 0,1:(xend_ts-xnbm2predict)]
head(ts_wout_0,10)
dim(ts_wout_0)

#as.numeric(time_series[k,])
#plot(as.numeric(time_series[k,]))
quantile(time_series[1,],0.99)[[1]]
quantile(time_series[1,],0.01)[[1]]

for(k in 1:nrow(ts_wout_0))
{
  #k<-2
  # Identifying outliers (some forecasting models are giving very volatile predictions)
  quant_bottom <- 0.01
  quant_top <- 0.99
  outlier1_id <- as.numeric(which((as.numeric(ts_wout_0[k,]) > quantile(ts_wout_0[k,],quant_top)[[1]])))
  #outlier2_id <- as.numeric(which((as.numeric(ts_wout_0[k,]) < quantile(ts_wout_0[k,],quant_bottom)[[1]])))
  outlier2_id <- as.numeric(which((as.numeric(ts_wout_0[k,]) < 0)))
  #outlier_id
  ts_wout_0[k,outlier1_id] <- quantile(ts_wout_0[k,],quant_top)[[1]]
  #ts_wout_0[k,outlier2_id] <- quantile(ts_wout_0[k,],quant_bottom)[[1]]
  ts_wout_0[k,outlier2_id] <- 0
}

min(ts_wout_0)

log_time_series <- log(ts_wout_0 + 1.00001)
head(log_time_series)
names(log_time_series)

sep_ts <- log_time_series[,1:(xend_ts-xnbm2predict)]
dim(sep_ts)
head(sep_ts,10)
t_sep_ts <- t(sep_ts)
dim(t_sep_ts)
#head(t(sep_ts))

#corr_ts <- cor(sep_ts)
corr_ts <- cor(t_sep_ts)
#head(sep_ts,10)
#head(corr_ts)
dim(corr_ts)


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# Hierarchical Clustering
#library(mlbench)
#library(corrplot)
#plot1 <- corrplot(cor(t_sep_ts), 
#                  method="square",
#                  order="hclust", tl.cex=0.7, cl.cex=0.5, tl.col="black", addrect=2)

#cor.info <- cor(t_sep_ts)
#h_clusters <- hclust(dist(corr_ts))
#plot(h_clusters)
#cluster_h <- cutree(h_clusters, 7)
#str(cluster_h)
#sep_ts$ts_cluster_h <- cluster_h
#head(sep_ts,10)


# Model Based Clustering
library(mclust)
#mv_fit <- Mclust(corr_ts, G=1:10)
mv_fit <- Mclust(corr_ts, G=1:7)
#par(mfrow = c(1, 1))
#plot(mv_fit) # plot results 
summary(mv_fit) # display the best model
saveRDS(mv_fit, file = "mv_fit.rds")
mv_fit <- readRDS(file = "mv_fit.rds")

sep_ts$seq <- seq(1, dim(sep_ts)[1])
sep_ts$ts_cluster_mv <- mv_fit$classification
head(sep_ts,15)
table(sep_ts$ts_cluster_mv)


# Silhouette plot
library (cluster)
dis = corr_ts
res = mv_fit
sil = silhouette (res$classification, dis)
plot(sil)

sep_ts[1,]

sep_ts[sep_ts$ts_cluster_mv==1,]
plot(as.numeric(sep_ts[4,1:60]))
lines(as.numeric(sep_ts[9,1:60]))
lines(as.numeric(sep_ts[16,1:60]))
lines(as.numeric(sep_ts[18,1:60]))
lines(as.numeric(sep_ts[20,1:60]))

sep_ts[sep_ts$ts_cluster_mv==2,]
plot(as.numeric(sep_ts[1,1:60]))
lines(as.numeric(sep_ts[3,1:60]))
lines(as.numeric(sep_ts[10,1:60]))
lines(as.numeric(sep_ts[14,1:60]))
lines(as.numeric(sep_ts[19,1:60]))

sep_ts[sep_ts$ts_cluster_mv==3,]
plot(as.numeric(sep_ts[7,1:60]))
lines(as.numeric(sep_ts[12,1:60]))
lines(as.numeric(sep_ts[15,1:60]))
lines(as.numeric(sep_ts[21,1:60]))
lines(as.numeric(sep_ts[22,1:60]))

sep_ts[sep_ts$ts_cluster_mv==4,]
plot(as.numeric(sep_ts[5,1:60]))
lines(as.numeric(sep_ts[17,1:60]))
lines(as.numeric(sep_ts[29,1:60]))
lines(as.numeric(sep_ts[58,1:60]))
lines(as.numeric(sep_ts[76,1:60]))

sep_ts[sep_ts$ts_cluster_mv==6,]
plot(as.numeric(sep_ts[6,1:60]))
lines(as.numeric(sep_ts[32,1:60]))
lines(as.numeric(sep_ts[35,1:60]))
lines(as.numeric(sep_ts[44,1:60]))
lines(as.numeric(sep_ts[60,1:60]))

sep_ts[sep_ts$ts_cluster_mv==7,]
plot(as.numeric(sep_ts[11,1:60]))
lines(as.numeric(sep_ts[13,1:60]))
lines(as.numeric(sep_ts[25,1:60]))
lines(as.numeric(sep_ts[26,1:60]))
lines(as.numeric(sep_ts[28,1:60]))


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

library(doBy)
Avg_ts_cluster <- summaryBy(
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
    total_revenue_4 + total_revenue_3 + total_revenue_2 + total_revenue_1 + total_revenue_0 
  ~ ts_cluster_mv
  , data = sep_ts, FUN = function(x) { c(m = mean(x, na.rm=TRUE)) })

#+ rev_aft_1 + rev_aft_2 + rev_aft_3 + rev_aft_4 + rev_aft_5 + rev_aft_6 + rev_aft_7 + rev_aft_8 + 
#  rev_aft_9 + rev_aft_10 + rev_aft_11 + rev_aft_12

Avg_ts_cluster <- as.data.frame(Avg_ts_cluster)
Avg_ts_cluster
class(Avg_ts_cluster)
dim(Avg_ts_cluster)

saveRDS(Avg_ts_cluster, file = "avg_ts_mv_seg.rds")
Avg_ts_cluster <- readRDS(file = "avg_ts_mv_seg.rds")

#setNames(Avg_ts_cluster[,2:61],names(sep_ts[1:60]))

par(mfrow = c(4, 2))
plot(as.numeric(Avg_ts_cluster[1,c(2:60)]))
plot(as.numeric(Avg_ts_cluster[2,c(2:60)]))
plot(as.numeric(Avg_ts_cluster[3,c(2:60)]))
plot(as.numeric(Avg_ts_cluster[4,c(2:60)]))
plot(as.numeric(Avg_ts_cluster[5,c(2:60)]))
plot(as.numeric(Avg_ts_cluster[6,c(2:60)]))
plot(as.numeric(Avg_ts_cluster[7,c(2:60)]))
#plot(as.numeric(Avg_ts_cluster[8,c(2:60)]))
#plot(as.numeric(Avg_ts_cluster[9,c(2:60)]))
#plot(as.numeric(Avg_ts_cluster[10,c(2:60)]))
par(mfrow = c(1, 1))



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# HERE: apply clustering to TEST data
# Checking simmilarities in Times Series (TS correlations)

#time_series_test <- test_ts[,c(names_ts,names_ts_after)]
time_series_test <- test_ts[,c(names_ts)]
names(time_series_test)
#time_series_test[is.na(time_series_test)] <- 0
dim(time_series_test)
#summary(time_series)
#time_series_test[1,]
#time_series_test[rowSums(is.na(time_series_test)) >= (length(time_series_test)-6),]

plot(as.numeric(time_series_test[1,]))

ts_test_na <- time_series_test[rowSums(is.na(time_series_test)) > 0,]
head(ts_test_na)
dim(ts_test_na)
ts_test_no_na <- time_series_test[rowSums(is.na(time_series_test)) == 0,]
dim(ts_test_no_na)
head(ts_test_no_na)

#ts_test <- time_series_no_na
ts_test <- time_series_test
xstart_ts <- 1
xend_ts <- length(ts_test)
#xnbm2predict <- 12
xnbm2predict <- 0
test_ts2 <- ts_test[,(xstart_ts):(xend_ts-xnbm2predict)]
dim(test_ts2)
test_ts2[1,]
test_ts2[7,]

test_ts2$rowsum_ts <- rowSums(test_ts2, na.rm = TRUE)
# TS with all zero values will be assigned to extra cluster
ts_test_eq_0 <- test_ts2[test_ts2$rowsum_ts <= 0,1:(xend_ts-xnbm2predict)]
dim(ts_test_eq_0)
ts_test_wout_0 <- test_ts2[test_ts2$rowsum_ts > 0,1:(xend_ts-xnbm2predict)]
dim(ts_test_wout_0)

#valid_ts_test <- ts_test_wout_0[rowSums(is.na(ts_test_wout_0)) < length(ts_test_wout_0)-6,]
#dim(valid_ts_test)

valid_ts_test <- test_ts2[,1:60]
for(k in 1:nrow(valid_ts_test))
{
  #k<-2
  # Identifying outliers (some forecasting models are giving very volatile predictions)
  quant_bottom <- 0.01
  quant_top <- 0.99
  outlier1_id <- as.numeric(which((as.numeric(valid_ts_test[k,]) > quantile(valid_ts_test[k,],quant_top,na.rm = TRUE)[[1]])))
  outlier2_id <- as.numeric(which((as.numeric(valid_ts_test[k,]) < 0)))
  #outlier_id
  valid_ts_test[k,outlier1_id] <- quantile(valid_ts_test[k,],quant_top,na.rm = TRUE)[[1]]
  # valid_ts_test[k,outlier2_id] <- 0
}

dim(valid_ts_test)
min(valid_ts_test)
log_ts_test <- log( valid_ts_test + 1.00001)
head(log_ts_test)
dim(log_ts_test)
log_ts_test[1,]

sep_ts_test <- log_ts_test[,1:(xend_ts-xnbm2predict)]
dim(sep_ts_test)
#sep_ts_test[3,]

# Assign each observation in TEST to the highest correlated time series in TRAIN 

stop <- nrow(sep_ts_test)
#stop <- 7
#sep_ts_test[stop,]
ts_cluster_mv <- rep(0,stop)
for(r in 1:stop)
{
  #r <- 8
  nb_ts_clus <- nrow(Avg_ts_cluster)
  df_newobs <- rbind(setNames(Avg_ts_cluster[,2:61],names(sep_ts_test)),sep_ts_test[r,])
  
  if ((rowSums(is.na(sep_ts_test[r,])) < length(sep_ts_test[r,])-6) & (rowSums(sep_ts_test[r,], na.rm = TRUE) > 1))
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
  #sep_ts_test[r,]
  
}

table(ts_cluster_mv)
sep_ts_test$ts_cluster_mv <- ts_cluster_mv

sep_ts_test$id_sub_ts_test <- rownames(sep_ts_test)
head(sep_ts_test)
dim(sep_ts_test)

sep_ts_test[sep_ts_test$ts_cluster_mv==1,]
plot(as.numeric(sep_ts_test[12,1:60]))
lines(as.numeric(sep_ts_test[21,1:60]))
lines(as.numeric(sep_ts_test[49,1:60]))
lines(as.numeric(sep_ts_test[98,1:60]))
lines(as.numeric(sep_ts_test[143,1:60]))

sep_ts_test[sep_ts_test$ts_cluster_mv==2,]
plot(as.numeric(sep_ts_test[139,1:60]))
lines(as.numeric(sep_ts_test[9,1:60]))
lines(as.numeric(sep_ts_test[20,1:60]))
lines(as.numeric(sep_ts_test[45,1:60]))
lines(as.numeric(sep_ts_test[48,1:60]))

sep_ts_test[sep_ts_test$ts_cluster_mv==3,]
plot(as.numeric(sep_ts_test[36,1:60]))
lines(as.numeric(sep_ts_test[59,1:60]))
lines(as.numeric(sep_ts_test[76,1:60]))
lines(as.numeric(sep_ts_test[113,1:60]))
lines(as.numeric(sep_ts_test[122,1:60]))

sep_ts_test[sep_ts_test$ts_cluster_mv==4,]
plot(as.numeric(sep_ts_test[42,1:60]))
lines(as.numeric(sep_ts_test[73,1:60]))
lines(as.numeric(sep_ts_test[84,1:60]))
lines(as.numeric(sep_ts_test[90,1:60]))
lines(as.numeric(sep_ts_test[110,1:60]))

sep_ts_test[sep_ts_test$ts_cluster_mv==7,]
plot(as.numeric(sep_ts_test[11,1:60]))
lines(as.numeric(sep_ts_test[22,1:60]))
lines(as.numeric(sep_ts_test[35,1:60]))
lines(as.numeric(sep_ts_test[41,1:60]))
lines(as.numeric(sep_ts_test[66,1:60]))




# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Creating IDs to merge with original dataset (TEST)
sep_ts_test$id_ts <- rownames(sep_ts_test)
head(sep_ts_test)
dim(sep_ts_test)
names(sep_ts_test)
test_ts$id_ts <- rownames(test_ts)
head(test_ts)
dim(test_ts)
names(test_ts)

ts_test_mv <- merge(test_ts, sep_ts_test[,c(61,63)], by = "id_ts", all.y = TRUE)
head(ts_test_mv)
dim(ts_test_mv)

names(sep_ts_test)
library(doBy)
Avg_ts_test2 <- summaryBy(
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
    ts_cluster_mv, 
  data = ts_test_mv, FUN = function(x) { c(m = mean(x, na.rm=TRUE)) })

Avg_ts_test2 <- as.data.frame(Avg_ts_test2)
names(Avg_ts_test2)
names(Avg_ts_test2) <- c("ts_cluster_mv",names(setNames(Avg_ts_test2[,2:73],names(test_ts[1:72]))))
names(Avg_ts_test2)
head(Avg_ts_test2)

#Avg_ts_test2$seq <- seq(1, dim(Avg_ts_test2)[1])
#Avg_ts_test2

#saveRDS(Avg_ts_test2, file = "Avg_ts_test2.rds")
#Avg_ts_test2 <- readRDS(file = "Avg_ts_test2.rds")


par(mfrow = c(2, 2))
plot(as.numeric(Avg_ts_test2[1,c(2:60)]))
plot(as.numeric(Avg_ts_test2[2,c(2:60)]))
plot(as.numeric(Avg_ts_test2[3,c(2:60)]))
plot(as.numeric(Avg_ts_test2[4,c(2:60)]))
plot(as.numeric(Avg_ts_test2[5,c(2:60)]))
plot(as.numeric(Avg_ts_test2[6,c(2:60)]))
plot(as.numeric(Avg_ts_test2[7,c(2:60)]))
plot(as.numeric(Avg_ts_test2[8,c(2:60)]))
par(mfrow = c(1, 1))









# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# Build ARIMA model for time series in each Revenue segment (DO NOT RUN IT)

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

Avg_ts_2$rowsum_ts <- rowSums(Avg_ts_2[,2:end_ts])
Avg_ts_eq_0 <- Avg_ts_2[Avg_ts_2$rowsum_ts == 0,1:(xend_ts-xnbm2predict)]
dim(Avg_ts_eq_0)


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
  actualtsxm_k <- ts(as.numeric(Avg_ts_test2[k,(start_ts+1):end_ts]),end=c(2017, 10), 
                     frequency = 12)[(end_ts-nbm2predict):(end_ts-1)]
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
fit_autoarima_10 <- readRDS(paste(file = "fit_autoarima", 10, ".rds", sep = "_"))

AvgError1
AvgError2
rmse
mean(AvgError1)
mean(AvgError2)
mean(rmse)

# Restore the object
#model_1 <- readRDS(file = "fit_autoarima_1.rds")

m<-10
autoarima_ts <- fit_autoarima_10
autoarima_ts
#forecast(autoarima_ts,h=h_val,xreg=xreg_val)
#forecast_1
plot(forecast(autoarima_ts,h=h_val,xreg=xreg_val,level=95), main=paste("ARIMA Forecasting for account in segment",  m, sep=" "))
lines(autoarima_ts$fitted,col="red")
#lines(ts(forecast_8,start=c(2017, 11), frequency = 12),col="green")
lines(ts(as.numeric(log(Avg_ts_2[m,62:73]+1.00001)),start=c(2017, 11), frequency = 12),col="green")
#lines(ts(log(as.numeric(Avg_ts_2[m,2:61])),end=c(2017, 10), frequency = 12),col="green")
par(mfrow = c(1, 1))
accuracy(autoarima_ts)

forecast_8
plot(ts(as.numeric(Avg_ts_2[m,2:61]),end=c(2017, 10), frequency = 12))
lines(ts(as.numeric(Avg_ts_test2[m,2:61]),end=c(2017, 10), frequency = 12),col="red")

ts_test <- fit_test_1
#forecast(ts_test,h=12,xreg=xreg_val_test)$mean
plot(forecast(ts_test,xreg=xreg_val))
lines(ts(log(as.numeric(Avg_ts_test2[m,2:61])),end=c(2017, 10), frequency = 12),col="green")
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


