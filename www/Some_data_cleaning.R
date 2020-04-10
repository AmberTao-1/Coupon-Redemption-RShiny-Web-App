# Data Cleaning Part

```{r}
library(tidyverse)
# read online data
online <- read_csv("ccf_online_stage1_train.csv")
online_hyt <- online
```



```{r}
# convert online date column into date type
online_hyt$Date_received <- as.Date(online_hyt[["Date_received"]], "%Y%m%d")
online_hyt$Date <- as.Date(online_hyt[["Date"]], "%Y%m%d")
```


```{r}
online_hyt
```



```{r}
library(tidyverse)
# read offline data
offline <- read_csv("ccf_offline_stage1_train.csv")
offline_hyt <- offline
```

```{r}
# convert offline date column into date type
offline_hyt$Date_received <- as.Date(offline_hyt[["Date_received"]], "%Y%m%d")
offline_hyt$Date <- as.Date(offline_hyt[["Date"]], "%Y%m%d")
```

```{r}
offline_hyt
```



```{r}
# transfer the YYYYMMDD to a new column contains YYYYMM

#Online Part YM for Date_received & Date
online_YM_hyt <- online_hyt
online_YM_hyt$Date_received_YM <- online_YM_hyt$Date_received
online_YM_hyt$Date_received_YM<- format(as.Date(online_YM_hyt$Date_received_YM, "%Y-%m-%d"), "%Y-%m")
online_YM_hyt$Date_YM <- online_YM_hyt$Date
online_YM_hyt$Date_YM<- format(as.Date(online_YM_hyt$Date_YM, "%Y-%m-%d"), "%Y-%m")

#Offline Part YM for Date_received & Date
offline_YM_hyt <- offline_hyt
offline_YM_hyt$Date_received_YM <- offline_YM_hyt$Date_received
offline_YM_hyt$Date_received_YM<- format(as.Date(offline_YM_hyt$Date_received_YM, "%Y-%m-%d"), "%Y-%m")
offline_YM_hyt$Date_YM <- offline_YM_hyt$Date
offline_YM_hyt$Date_YM<- format(as.Date(offline_YM_hyt$Date_YM, "%Y-%m-%d"), "%Y-%m")

```

```{r}
online_YM_hyt
offline_YM_hyt
```
```{r}
# time serious coupon release count per month 
# select out all the records which is coupon give out. 

online_YM_coupon_hyt <- filter(online_YM_hyt, !is.na(Date_received))
offline_YM_coupon_hyt <- filter(offline_YM_hyt, !is.na(Date_received))
```
```{r}
online_YM_coupon_hyt
offline_YM_coupon_hyt
```
```{r}
# Arranged the data by time
online_YM_coupon_order_hyt <- online_YM_coupon_hyt %>% arrange(Date_received)
offline_YM_coupon_order_hyt <- offline_YM_coupon_hyt %>% arrange(Date_received)
```

```{r}
online_YM_coupon_order_hyt
offline_YM_coupon_order_hyt
```

```{r}
# the data is for all the coupon released in the data including coupon redeemed or not 
# group the data by day and count the number
# group by Date_received
online_YM_coupon_order_group_hyt <- online_YM_coupon_order_hyt %>% group_by(Date_received) %>% tally(name = "Coupon_sent")

offline_YM_coupon_order_group_hyt <- offline_YM_coupon_order_hyt %>% group_by(Date_received) %>% tally(name = "Coupon_sent")
```

```{r}
online_YM_coupon_order_group_hyt
offline_YM_coupon_order_group_hyt
```


```{r}
# time series plot on the coupon usage
# cleaning out the transacation with coupons
online_YM_coupon_used_hyt <- filter(online_YM_hyt, !is.na(Date_received) & !is.na(Date))
offline_YM_coupon_used_hyt <- filter(offline_YM_hyt, !is.na(Date_received)& !is.na(Date))
```
```{r}
# Arranged the data by time
online_YM_coupon_used_hyt <- online_YM_coupon_used_hyt %>% arrange(Date_received)
offline_YM_coupon_used_hyt <- offline_YM_coupon_used_hyt %>% arrange(Date_received)
```

```{r}
# the data is for all the coupon used in the transactions 
# group the data by day and count the number
# group by Date_received
# might have problem comparing the date used
online_YM_coupon_used_group_hyt <- online_YM_coupon_used_hyt %>% group_by(Date_received) %>% tally(name = "Coupon_used")

offline_YM_coupon_used_group_hyt <- offline_YM_coupon_used_hyt %>% group_by(Date_received) %>% tally(name = "Coupon_used") 
```

```{r}
# the data is for all the coupon used in the transactions 
# group the data by day and count the number
# group by Date_received
# might have problem comparing the date used
online_YM_coupon_used_group_hyt_Date <- online_YM_coupon_used_hyt %>% group_by(Date) %>% tally(name = "Coupon_used")

offline_YM_coupon_used_group_hyt_Date <- offline_YM_coupon_used_hyt %>% group_by(Date) %>% tally(name = "Coupon_used") 
```
```{r}
online_YM_coupon_used_group_hyt_Date
offline_YM_coupon_used_group_hyt_Date
```


```{r}
online_YM_coupon_used_group_hyt
offline_YM_coupon_used_group_hyt
```







```{r}
# time series plot on the coupon none usage
# cleaning out the transacation with no coupon perchacses
online_YM_coupon_noused_hyt <- filter(online_YM_hyt, is.na(Date_received) & !is.na(Date))
offline_YM_coupon_noused_hyt <- filter(offline_YM_hyt, is.na(Date_received)& !is.na(Date))
```
```{r}
# Arranged the data by time
online_YM_coupon_noused_hyt <- online_YM_coupon_noused_hyt %>% arrange(Date_received)
offline_YM_coupon_noused_hyt <- offline_YM_coupon_noused_hyt %>% arrange(Date_received)
```

```{r}
# the data is for all the coupon used in the transactions 
# group the data by day and count the number
# group by the Date
online_YM_coupon_noused_group_hyt <- online_YM_coupon_noused_hyt %>% group_by(Date) %>% tally(name = "Coupon_no")

offline_YM_coupon_noused_group_hyt <- offline_YM_coupon_noused_hyt %>% group_by(Date) %>% tally(name = "Coupon_no") 
```

```{r}
online_YM_coupon_noused_group_hyt
offline_YM_coupon_noused_group_hyt
```


```{r}
#coupon usage / coupon sent out ratio
online_Usage_Sent_Ratio_hyt <- left_join(online_YM_coupon_order_group_hyt, online_YM_coupon_used_group_hyt)


offline_Usage_Sent_Ratio_hyt <- left_join(offline_YM_coupon_order_group_hyt, offline_YM_coupon_used_group_hyt)

```
```{r}
# Calculate Ratio

online_Usage_Sent_Ratio_hyt$ratio <- online_Usage_Sent_Ratio_hyt$Coupon_used/online_Usage_Sent_Ratio_hyt$Coupon_sent


offline_Usage_Sent_Ratio_hyt$ratio <- offline_Usage_Sent_Ratio_hyt$Coupon_used/offline_Usage_Sent_Ratio_hyt$Coupon_sent
```

```{r}
# calculate transaction without coupon
online_Usage_Sent_Ratio_hyt$Coupon_no <- online_Usage_Sent_Ratio_hyt$Coupon_sent-online_Usage_Sent_Ratio_hyt$Coupon_used

offline_Usage_Sent_Ratio_hyt$Coupon_no <- offline_Usage_Sent_Ratio_hyt$Coupon_sent-offline_Usage_Sent_Ratio_hyt$Coupon_used
```

```{r}
online_Usage_Sent_Ratio_hyt
offline_Usage_Sent_Ratio_hyt
```



```{r}
#coupon usage / total trasaction ratio 
online_Usage_Total_ratio_hyt <- left_join(online_YM_coupon_used_group_hyt_Date,online_YM_coupon_noused_group_hyt)


offline_Usage_Total_ratio_hyt <- left_join(offline_YM_coupon_used_group_hyt_Date,offline_YM_coupon_noused_group_hyt)

```
```{r}
# calculate the ratio
online_Usage_Total_ratio_hyt$ratio <- online_Usage_Total_ratio_hyt$Coupon_used/(online_Usage_Total_ratio_hyt$Coupon_no+online_Usage_Total_ratio_hyt$Coupon_used)

offline_Usage_Total_ratio_hyt$ratio <- offline_Usage_Total_ratio_hyt$Coupon_used/(offline_Usage_Total_ratio_hyt$Coupon_no+offline_Usage_Total_ratio_hyt$Coupon_used)
```
```{r}
# calculate the Total
online_Usage_Total_ratio_hyt$total <- online_Usage_Total_ratio_hyt$Coupon_used+online_Usage_Total_ratio_hyt$Coupon_no
offline_Usage_Total_ratio_hyt$total <- offline_Usage_Total_ratio_hyt$Coupon_used+offline_Usage_Total_ratio_hyt$Coupon_no
```


```{r}
write.csv(online_Usage_Total_ratio_hyt,"online_Usage_Total_ratio_hyt.csv") 
write.csv(offline_Usage_Total_ratio_hyt,"offline_Usage_Total_ratio_hyt.csv")
```


```{r}
online_YM_hyt
offline_YM_hyt
```

