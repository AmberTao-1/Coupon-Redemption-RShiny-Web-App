library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(readr)
library(data.table)
library(tidyverse)
library(e1071)
library(caret)
library(lattice)

library(stringr)
library(hablar)
library(scales)
#Import Data
BigTop100 <- read.csv("BigTop100.csv")
fiftystatesCAN <- read.csv("fiftystatesCAN.csv")
uniquecities <- read.csv("uniquecities.csv")

#########################################################################################################xulu data
offline<- read.csv("ccf_offline_stage1_train.csv")
online<- read.csv("ccf_online_stage1_train.csv")
#Change data format to date
offline$Date_received <- as.Date(offline[["Date_received"]], "%Y%m%d")
offline$Date <- as.Date(offline[["Date"]], "%Y%m%d")

online$Date_received <- as.Date(online[["Date_received"]], "%Y%m%d")
online$Date <- as.Date(online[["Date"]], "%Y%m%d")
offline$Distance<-as.integer(offline[["Distance"]])

set.seed(1)
online.s <- sample(row.names(online), 0.001*dim(online)[1])
offline.s <- sample(row.names(offline), 0.001*dim(offline)[1])
online.s <- online[online.s, ]
offline.s <- offline[offline.s, ]
#########################################################################################################xulu data end

########################################################################### TYT Data Import and processing



###offline data processing
#offline<-read.csv('ccf_offline_stage1_train.csv', header = T, na.strings = NA)
#offline<-data.frame(offline)
redeemed_offline<-offline
redeemed_offline<-redeemed_offline %>%
    filter(!is.na(redeemed_offline$Date) & offline$Coupon_id !="null")  

#offline %>%
#  filter(offline$Date != 'null' & offline$Coupon_id != 'null') -> redeemed_offline

#redeemed_offline$Date <- as.Date(redeemed_offline[["Date"]], "%Y%m%d")
#redeemed_offline$Date_received <- as.Date(redeemed_offline[["Date_received"]], "%Y%m%d")
redeemed_offline$RedeemDays <- redeemed_offline$Date - redeemed_offline$Date_received
###online data processing
#online<-read.csv('ccf_online_stage1_train.csv', header = T, na.strings = NA)
#online<-data.frame(online)

redeemed_online<-online
redeemed_online<-redeemed_online %>%
  filter(!is.na(redeemed_online$Date) & online$Coupon_id !="null")  

#online %>%
  #filter(online$Date != 'null' & online$Coupon_id != 'null') -> redeemed_online

#redeemed_online$Date <- as.Date(redeemed_online[["Date"]], "%Y%m%d")
#redeemed_online$Date_received <- as.Date(redeemed_online[["Date_received"]], "%Y%m%d")
redeemed_online$RedeemDays <- redeemed_online$Date - redeemed_online$Date_received

#########################################################################
#Continue TYT part for Use Rate and Coupon Rate relationship
###offline data processing
#offline %>%
#  filter(offline$Date_received != 'null' & offline$Coupon_id != 'null') -> getcpOffline
getcpOffline<-  offline%>%
  filter(!is.na(offline$Date_received)& offline$Coupon_id != "null")


getcpOffline %>% count(Discount_rate) -> a
redeemed_offline %>% count(Discount_rate) -> b
merge(a, b, by = 'Discount_rate') -> result_Offline
result_Offline$use_rate <- result_Offline$n.y/result_Offline$n.x

names(result_Offline)[3] <- 'redNum' 
names(result_Offline)[2] <- 'getcpNum'

ratio1 <- lapply(result_Offline$Discount_rate,function(x){
  re <- unlist(str_split(x,":"))
  if(re > 1){
    r <- as.numeric(re[2])/as.numeric(re[1])
  }
  else{
    r <- 1-as.numeric(re)
  }
})
result_Offline$discount_percent_off <- unlist(ratio1)

sorted_discountOff <- result_Offline[order(result_Offline$discount_percent_off),]
sorted_discountOff$discount_percent_off <- percent(sorted_discountOff$discount_percent_off)
sorted_discountOff$discount_percent_off <- as.factor(sorted_discountOff[["discount_percent_off"]])


###online data processing
#online %>%
#  filter(online$Date_received != 'null' & online$Coupon_id != 'null') -> getcpOnline
getcpOnline<-  online%>%
  filter(!is.na(online$Date_received)& online$Coupon_id != "null")

getcpOnline %>% count(Discount_rate) -> a
redeemed_online %>% count(Discount_rate) -> b
merge(a, b, by = 'Discount_rate') -> result_Online
result_Online$use_rate <- result_Online$n.y/result_Online$n.x

names(result_Online)[3] <- 'redNum' 
names(result_Online)[2] <- 'getcpNum'

ratio <- lapply(result_Online$Discount_rate,function(x){
  re <- unlist(str_split(x,":"))
  r <- as.numeric(re[2])/as.numeric(re[1])
})
result_Online$discount_percent_off <- unlist(ratio)

sorted_discount <- result_Online[order(result_Online$discount_percent_off),]
sorted_discount$discount_percent_off <- percent(sorted_discount$discount_percent_off)
sorted_discount$discount_percent_off <- as.factor(sorted_discount[["discount_percent_off"]])


##############################################################################################################tyt data end

############################################################################################function yating 

redeemtime <- function(range = c(0, 50), type = "offline"){
  
  resptyt <- ggplot()
  
  #judge type first
  #offline part
  if (type=="offline"){
    red<-filter(redeemed_offline,redeemed_offline$RedeemDays>=range[1] & 
                  redeemed_offline$RedeemDays<=range[2])##redeem days range filter
    
    resx<-red$RedeemDays
    
  }
  else{
    red <- filter(redeemed_online,redeemed_online$RedeemDays>=range[1] & 
                    redeemed_online$RedeemDays<=range[2])
    
    resx <- red$RedeemDays
    
  }
  
  ###Calculating part end
  
  ##Output part
  #res<-data.frame(resx)
  #names(res) <- c("resx")
  
  resptyt<-resptyt+stat_bin(data = red,aes(x=resx))
  m <- layer_data(resptyt)
  resptyt<-resptyt + stat_smooth(data = m, aes(x, y))+
    xlab("Redeem Days")
  
  return(resptyt)
}

###function
UseRateCouponRatecol <- function(discount_col = c('20%','10%'),
                                 type = "offline"){
  resptyt <- ggplot()
  ##offline part
  if (type=="offline"){
    percent_offline <- data.frame()
    for (i in discount_col){
      percent_offline <- rbind(percent_offline, 
                               sorted_discountOff[sorted_discountOff$discount_percent_off == i,])
    }
    
    percent_offline <- na.omit(percent_offline)
    
    resptyt <- resptyt+
      geom_col(data = percent_offline, mapping = aes(x = Discount_rate, y=use_rate))+
      facet_wrap( ~ discount_percent_off, nrow = 2)+
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
      xlab("Discount rate")+
      ylab("Coupon Use Rate")
    
  }
  ###online part
  else{
    percent_online <- data.frame()
    for (i in discount_col){
      percent_online <- rbind(percent_online, 
                              sorted_discount[sorted_discount$discount_percent_off == i,])
    }
    
    percent_online <- na.omit(percent_online)
    
    resptyt<- resptyt +
      geom_col(data = percent_online, mapping = aes(x = Discount_rate, y=use_rate))+
      facet_wrap( ~ discount_percent_off, nrow = 2)+
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
      xlab("Discount rate")+
      ylab("Coupon Use Rate")
  }
  
  return(resptyt)
}

UseRateCouponRatebox <- function(discount_box = c('1%','17%','5%', '67%'), type = 'offline'){
  resptyt <- ggplot()
  
  if(type == 'offline'){
    if(is.na(discount_box)){
      resptyt <- resptyt +
        geom_boxplot(data = sorted_discountOff, mapping = aes(x = discount_percent_off, y = use_rate))+
        theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
        xlab("Discount Percent Off of Coupon")+
        ylab("Coupon Use Rate")
    }
    else{
      discount_offline <- data.frame()
      for (i in discount_box){
        discount_offline <- rbind(discount_offline, 
                                  sorted_discountOff[sorted_discountOff$discount_percent_off == i,])
      }
      
      discount_offline <- na.omit(discount_offline)
      
      resptyt <- resptyt+
        geom_boxplot(data = discount_offline, mapping = aes(x = discount_percent_off, y = use_rate))+
        theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
        xlab("Discount Percent Off of Coupon")+
        ylab("Coupon Use Rate")
      
    }
  }
  else{
    if(is.na(discount_box)){
      resptyt <- resptyt +
        geom_boxplot(data = sorted_discount, mapping = aes(x = discount_percent_off, y = use_rate))+
        theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
        xlab("Discount Percent Off of Coupon")+
        ylab("Coupon Use Rate")
    }
    else{
      discount_online <- data.frame()
      for (i in discount_box){
        discount_online <- rbind(discount_online, 
                                 sorted_discount[sorted_discount$discount_percent_off == i,])
      }
      
      discount_online <- na.omit(discount_online)
      
      resptyt <- resptyt +
        geom_boxplot(data = discount_online, mapping = aes(x = discount_percent_off, y = use_rate))+
        theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
        xlab("Discount Percent Off of Coupon")+
        ylab("Coupon Use Rate")
    }
  }
  
  return(resptyt)
}




############################################################################################function yating end

############################################################################################function xulu 

#####function 1 xulu

#top n (offline filter distance)
#input  n: top n
#       linetype:"offline"/"online"
#       rof :"freq" / "ratioiss"(usage/issue) / "ratioconsum" (usage/consumption)
#       distance: two side list (offline only)
topn<- function(n=5,linetype="offline",rof="freq",dist=c(0,10)){
  #########calculating part
  #basic para
  resn<-list(1:n)
  resp<-ggplot()
  
  
  #judge linetype first
  #offline part
  if (linetype=="offline") {
    
    #need to consider distance
    offwc<-filter(offline,!is.na(offline$Date) & offline$Coupon_id!="null" & offline$Distance>=dist[1] & offline$Distance<=dist[2])##distance filter
    
    #Aggregate merchant_id and order data
    offwc<-offwc %>%
      group_by(offwc$Merchant_id)%>%
      summarise(times=n()) %>%
      arrange(desc(times)) 
    
    #rof="freq"
    if (rof=="freq") {
      
      topndata <- offwc[1:n,]
      resx<-as.list(topndata[,"offwc$Merchant_id"])
      resy<-as.list(topndata[,"times"])    
      
    }
    
    #rof="ratioiss"    
    else if(rof=="ratioiss"){
      #not use
      offnu<-filter(offline,is.na(offline$Date) & offline$Coupon_id!="null" & offline$Distance>=dist[1] & offline$Distance<=dist[2])##distance filter  
      #Aggregate merchant_id and order data
      offnu<-offnu %>%
        group_by(offnu$Merchant_id)%>%
        summarise(nutimes=n()) %>%
        arrange(desc(nutimes))
      
      #join two tables and calculate ratio then order data
      offnu<-left_join(offwc,offnu,by=c("offwc$Merchant_id"="offnu$Merchant_id"))   
      names(offnu)<-c("Merchant_id","times","nutimes")
      
      offnu$ratioiss<-offnu$times/offnu$nutimes
      offnu<-offnu  %>%
        arrange(desc(ratioiss))
      
      topndata <- offnu[1:n,]
      resx<-as.list(topndata[,"Merchant_id"])
      resy<-as.list(topndata[,"ratioiss"])  
      
      
    }
    #rof="ratioconsum"     
    else if (rof=="ratioconsum") {
      #consumed but not use
      offcbnu<-filter(offline,!is.na(offline$Date) & offline$Coupon_id=="null" & offline$Distance>=dist[1] & offline$Distance<=dist[2])##distance filter  
      #Aggregate merchant_id and order data
      offcbnu<-offcbnu %>%
        group_by(offcbnu$Merchant_id)%>%
        summarise(cbnutimes=n()) %>%
        arrange(desc(cbnutimes))
      
      #join two tables and calculate ratio then order data
      offcbnu<-left_join(offwc,offcbnu,by=c("offwc$Merchant_id"="offcbnu$Merchant_id"))   
      names(offcbnu)<-c("Merchant_id","times","cbnutimes")
      
      offcbnu$ratioconsum<-offcbnu$times/offcbnu$cbnutimes
      offcbnu<-offcbnu  %>%
        arrange(desc(ratioconsum))
      
      topndata <- offcbnu[1:n,]
      resx<-as.list(topndata[,"Merchant_id"])
      resy<-as.list(topndata[,"ratioconsum"]) 
    }
    else{
      return(resp)
    }
    
  }
  
#online part
  else{
    onwc<-filter(online,!is.na(online$Date) & online$Coupon_id!="null") 
    #Aggregate merchant_id and order data
    onwc<-onwc %>%
      group_by(onwc$Merchant_id)%>%
      summarise(times=n()) %>%
      arrange(desc(times)) 
    #rof="freq"
    if (rof=="freq") {
      topndata <- onwc[1:n,]
      resx<-as.list(topndata[,"onwc$Merchant_id"])
      resy<-as.list(topndata[,"times"]) 
    }
    
    #rof="ratioiss"    
    else if(rof=="ratioiss"){
      #not use
      onnu<-filter(online,is.na(online$Date) & online$Coupon_id!="null")
      #Aggregate merchant_id and order data
      onnu<-onnu %>%
        group_by(onnu$Merchant_id)%>%
        summarise(nutimes=n()) %>%
        arrange(desc(nutimes))
      
      #join two tables and calculate ratio then order data
      onnu<-left_join(onwc,onnu,by=c("onwc$Merchant_id"="onnu$Merchant_id"))   
      names(onnu)<-c("Merchant_id","times","nutimes")
      
      onnu$ratioiss<-onnu$times/onnu$nutimes
      onnu<-onnu  %>%
        arrange(desc(ratioiss))
      
      topndata <- onnu[1:n,]
      resx<-as.list(topndata[,"Merchant_id"])
      resy<-as.list(topndata[,"ratioiss"])  
      
    }
    
    #rof="ratioconsum"     
    else if (rof=="ratioconsum") {
      #consumed but not use
      oncbnu<-filter(online,!is.na(online$Date) & online$Coupon_id=="null" ) 
      #Aggregate merchant_id and order data
      oncbnu<-oncbnu %>%
        group_by(oncbnu$Merchant_id)%>%
        summarise(cbnutimes=n()) %>%
        arrange(desc(cbnutimes))
      
      #join two tables and calculate ratio then order data
      oncbnu<-left_join(onwc,oncbnu,by=c("onwc$Merchant_id"="oncbnu$Merchant_id"))   
      names(oncbnu)<-c("Merchant_id","times","cbnutimes")
      
      oncbnu$ratioconsum<-oncbnu$times/oncbnu$cbnutimes
      oncbnu<-oncbnu  %>%
        arrange(desc(ratioconsum))
      
      topndata <- oncbnu[1:n,]
      resx<-as.list(topndata[,"Merchant_id"])
      resy<-as.list(topndata[,"ratioconsum"]) 
      
      
    }
    else{
      return(resp)
    }
  }
  #######calculating part end
  
  #######output part
  #out put a dataframe  
  res<-data.frame(resn,resx,resy)
  names(res) <- c("resn","resx","resy")
  #out put a plot
  resp<-resp+
    geom_bar(data = res,aes(x=resn,y=resy,fill=factor(resx)),stat="identity")+
    xlab("Merchant_id")+
    theme_bw()+
    theme(legend.position = "none")+
    scale_x_continuous(breaks=res$resn,labels = res$resx)
  #rof="ratio" part 2
  if (rof=="ratioiss") {
    resp<- resp +
      ylab("Coupon useage/Coupon issuance ratio")
  }
  else if (rof=="ratioconsum") {
    resp<- resp +
      ylab("Coupon useage/Consumption ratio")
  }
  else if (rof=="freq") {
    resp<- resp +
      ylab("Coupon useage times")
  }
  else{
    return(resp)
  }
  return(resp)
}




###function 2 xulu
chartCol_xulu<-function(df,colN,b=30){
  res<-ggplot(data=df)
  l<-df[,eval(colN)]
  if (is.numeric(l[[1]])) {
    res<-res+
      geom_histogram(data=df,mapping = aes_string(x=colN),bins=b)
  }
  else{
    res<-res+
      geom_bar(data=df,mapping = aes_string(x=colN))
  }
  res<-res+
    theme(axis.text.x=element_text(angle=40,vjust=NULL,hjust = 1))
  return(res)
}


######################################################################################################xulufunction end


##########################################################################xiaofei part1
## XFQ DATA IMPORT
# Distance & Coupon Usage



##offline <- fread("ccf_offline_stage1_train.csv")
##offline$Date_received <- as.Date(offline[["Date_received"]], "%Y%m%d")
##offline$Date <- as.Date(offline[["Date"]], "%Y%m%d")

l <- offline %>% 
  filter(Distance!="null") %>%
  filter(!is.na(Date) & !is.na(Coupon_id)) %>%
  mutate(Distance=as.numeric(Distance)) %>%
  group_by(Distance) %>% 
  summarise(Frequency = n())
m <- offline %>% 
  filter(Distance!="null") %>%
  filter(!is.na(Coupon_id)) %>%
  mutate(Distance=as.numeric(Distance)) %>%
  group_by(Distance) %>% 
  summarise(total = n())

lm <- l %>%
  inner_join(m) %>%
  mutate(ratio=Frequency/total)
Distance_plot <- function(data,v1="Distance",v2="Frequency"){
  data <- data[,c(v1,v2)]
  colnames(data) <- c("v1","v2")
  pic <- data %>%
    ggplot(aes(v1,v2))+
    geom_point()+
    geom_line()+
    theme_bw()+
    theme(axis.text = element_text(color='black',size=10))+
    xlab(v1)+ylab(v2)
  return(pic)
}
##########################################################################xiaofei part1 end



#####################################################################################tt part1
# HYT Data Import

# HYT Data Import
offline_Usage_Sent_Ratio_hyt <- read_csv("offline_Usage_Sent_Ratio_hyt.csv")
online_Usage_Sent_Ratio_hyt <- read_csv("online_Usage_Sent_Ratio_hyt.csv")
online_Usage_Total_ratio_hyt <- read_csv("online_Usage_Total_ratio_hyt.csv")
offline_Usage_Total_ratio_hyt <- read_csv("offline_Usage_Total_ratio_hyt.csv")

# HYT function
# Variable 1 "Ratio" "Numeric"
# Variable 2 "Coupon Usage"  "Sales-Coupon Usage"
# Variable 3,4,5 "a" "b" "c"
tsfunc_hyt <- function(type,df, l) {
  a <- NULL
  b <- NULL
  c <- NULL
  if (length(l)==3){
    a <- "a"
    b <- "b"
    c <- "c"
    # cat(a)
    # cat(b)
    # cat(c)
  } else if (length(l)==2){
    if (sum(c("Online","Offline") %in% l) ==2 | sum(c("Total","Without Coupon") %in% l) == 2){
      a <- "a"
      b <- "b"
    } else if (sum(c("Total","With Coupon or Coupon Used") %in% l) == 2){
      a <- "a"
      c <- "c"
    } else if(sum(c("Without Coupon","With Coupon or Coupon Used") %in% l) == 2){
      b <- "b"
      c <- "c"
    } else {
      return("nothing there")
    }
    # cat(a)
    # cat(b)
    # cat(c)
  } else if (length(l)==1){
    if("Online" %in% l | "Total" %in% l){
      a <- "a"
    } else if("Offline" %in% l | "Without Coupon" %in% l){
      b <- "b"
    } else if("With Coupon or Coupon Used" %in% l){
      c <- "c"
    } else {
      return("nothing there")
    }
    # cat(a)
    # cat(b)
    # cat(c)
  } else {
    return("error")
  }
  if (type == "Ratio" & df == "Coupon Usage") {
    # a is online
    # b is offline
    # Ration Coupon Usage Graph
    if(!is.null(a) & !is.null(b)){
      # plot online and offline
      ggplot() + 
        geom_line(offline_Usage_Sent_Ratio_hyt, mapping = aes(x = Date_received, y = ratio), color = "olivedrab3") +
        geom_line(online_Usage_Sent_Ratio_hyt, mapping =  aes(x = Date_received, y = ratio) , color = "skyblue2") +
        xlab("") + ylab("Probablity of Coupon Sent and Used") + 
        scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Coupon Usage Probablity Online vs. Offline") + theme_light()
    }else if(!is.null(a) & is.null(b)){
      # plot online
      ggplot() + 
        geom_line(online_Usage_Sent_Ratio_hyt, mapping =  aes(x = Date_received, y = ratio) , color = "skyblue2") +
        xlab("") + ylab("Probablity of Coupon Sent and Used") + 
        scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Coupon Usage Probablity Online") + theme_light()      
    } else if(!is.null(b) & is.null(a)){
      # plot offline
      ggplot() + 
        geom_line(offline_Usage_Sent_Ratio_hyt, mapping = aes(x = Date_received, y = ratio), color = "olivedrab3") +
        xlab("") + ylab("Probablity of Coupon Sent and Used") + 
        scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Coupon Usage Probablity Offline") + theme_light()      
    } else {
      # return erro
      return("error")
    }
  } 
  else if(type == "Ratio" & df == "Sales-Coupon Usage"){
    # a is online
    # b is offline
    # Ration Sales-Coupon Usage Graph
    if(!is.null(a) & !is.null(b)){
      # plot online and offline
      ggplot() + 
        geom_line(online_Usage_Total_ratio_hyt,mapping = aes(x = Date, y = ratio), color = "skyblue2") +
        geom_line(offline_Usage_Total_ratio_hyt,mapping = aes(x = Date, y = ratio), color = "olivedrab3") +
        xlab("") + ylab("Probablity of Purchase with Coupon") + 
        scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Purchase with Coupon Probablity Online vs. Offline") + theme_light()
    }else if(!is.null(a) & is.null(b)){
      # plot online
      ggplot() + 
        geom_line(online_Usage_Total_ratio_hyt,mapping = aes(x = Date, y = ratio), color = "skyblue2") +
        xlab("") + ylab("Probablity of Purchase with Coupon") + 
        scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Purchase with Coupon Probablity Online") + theme_light()      
    } else if(!is.null(b) & is.null(a)){
      # plot offline
      ggplot() + 
        geom_line(offline_Usage_Total_ratio_hyt,mapping = aes(x = Date, y = ratio), color = "olivedrab3") +
        xlab("") + ylab("Probablity of Purchase with Coupon") + 
        scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Purchase with Coupon Probablity Offline") + theme_light()      
    } else {
      # return error
      return("error")
    }
  }
  else if(type == "Numeric" & df == "Coupon Usage"){
    # a is total
    # b is without coupon
    # c is with coupon
    if(!is.null(a) & !is.null(b) & !is.null(c)){
      # plot all
      # online part
      onlineplot <- ggplot(online_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_sent), color = "skyblue2") + 
        geom_line(aes(x = Date_received, y = Coupon_used), color = "olivedrab3") + 
        geom_line(aes(x = Date_received, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_sent), color = "skyblue2") + 
        geom_line(aes(x = Date_received, y = Coupon_used), color = "olivedrab3") + 
        geom_line(aes(x = Date_received, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Coupon Usage") + theme_light()
      # list the 2 plot out
      list(onlineplot, offlineplot)
    }else if(!is.null(a) & is.null(b) & is.null(c)){
      # plot total
      # online part
      onlineplot <- ggplot(online_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_sent), color = "skyblue2") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_sent), color = "skyblue2") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Coupon Usage") + theme_light()
      # list the 2 plot out
      list(onlineplot, offlineplot)      
    } else if(!is.null(b) & is.null(a) & is.null(c)){
      # plot without coupon
      # online part
      onlineplot <- ggplot(online_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Coupon Usage") + theme_light()
      # list the 2 plot out
      list(onlineplot, offlineplot)
    } else if(!is.null(c) & is.null(a) & is.null(b)){
      # plot with coupon
      # online part
      onlineplot <- ggplot(online_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_used), color = "olivedrab3") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_used), color = "olivedrab3") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Coupon Usage") + theme_light()
      # list the 2 plot out
      list(onlineplot, offlineplot)      
    } else if(!is.null(a) & !is.null(b) & is.null(c)){
      # plot total & without coupon
      # online part
      onlineplot <- ggplot(online_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_sent), color = "skyblue2") + 
        geom_line(aes(x = Date_received, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_sent), color = "skyblue2") + 
        geom_line(aes(x = Date_received, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Coupon Usage") + theme_light()
      # list the 2 plot out
      list(onlineplot, offlineplot)
    } else if(!is.null(a) & !is.null(c) & is.null(b)){
      # plot total & with coupon
      # online part
      onlineplot <- ggplot(online_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_sent), color = "skyblue2") + 
        geom_line(aes(x = Date_received, y = Coupon_used), color = "olivedrab3") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_sent), color = "skyblue2") + 
        geom_line(aes(x = Date_received, y = Coupon_used), color = "olivedrab3") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Coupon Usage") + theme_light()
      # list the 2 plot out
      list(onlineplot, offlineplot)
    } else if(!is.null(b) & !is.null(c) & is.null(a)){
      # plot with coupon $ without coupon
      # online part
      onlineplot <- ggplot(online_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_used), color = "olivedrab3") + 
        geom_line(aes(x = Date_received, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Sent_Ratio_hyt) + 
        geom_line(aes(x = Date_received, y = Coupon_used), color = "olivedrab3") + 
        geom_line(aes(x = Date_received, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Coupon Usage") + theme_light()
      # list the 2 plot out
      list(onlineplot, offlineplot)
    } else {
      # return error
      return("error")
    }
  }
  else if(type == "Numeric" & df == "Sales-Coupon Usage"){
    # a is total
    # b is without coupon
    # c is with coupon
    if(!is.null(a) & !is.null(b) & !is.null(c)){
      # plot all
      # online part
      onlineplot <- ggplot(online_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = total), color = "skyblue2") + 
        geom_line(aes(x = Date, y = Coupon_used), color = "olivedrab3") + 
        geom_line(aes(x = Date, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Sales and Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = total), color = "skyblue2") + 
        geom_line(aes(x = Date, y = Coupon_used), color = "olivedrab3") + 
        geom_line(aes(x = Date, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Sales and Coupon Usage") + theme_light()
      # list the 2 plot
      list(onlineplot, offlineplot)
    }else if(!is.null(a) & is.null(b) & is.null(c)){
      # plot total
      # online part
      onlineplot <- ggplot(online_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = total), color = "skyblue2") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Sales and Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = total), color = "skyblue2") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Sales and Coupon Usage") + theme_light()
      # list the 2 plot
      list(onlineplot, offlineplot)
    } else if(!is.null(b) & is.null(a) & is.null(c)){
      # plot without coupon
      # online part
      onlineplot <- ggplot(online_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Sales and Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Sales and Coupon Usage") + theme_light()
      # list the 2 plot
      list(onlineplot, offlineplot)
    } else if(!is.null(c) & is.null(a) & is.null(b)){
      # plot with coupon
      # online part
      onlineplot <- ggplot(online_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = Coupon_used), color = "olivedrab3") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Sales and Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = Coupon_used), color = "olivedrab3") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Sales and Coupon Usage") + theme_light()
      # list the 2 plot
      list(onlineplot, offlineplot)
    } else if(!is.null(a) & !is.null(b) & is.null(c)){
      # plot total & without coupon
      # online part
      onlineplot <- ggplot(online_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = total), color = "skyblue2") + 
        geom_line(aes(x = Date, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Sales and Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = total), color = "skyblue2") + 
        geom_line(aes(x = Date, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Sales and Coupon Usage") + theme_light()
      # list the 2 plot
      list(onlineplot, offlineplot)
    } else if(!is.null(a) & !is.null(c) & is.null(b)){
      # plot total & with coupon
      # online part
      onlineplot <- ggplot(online_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = total), color = "skyblue2") + 
        geom_line(aes(x = Date, y = Coupon_used), color = "olivedrab3") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Sales and Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = total), color = "skyblue2") + 
        geom_line(aes(x = Date, y = Coupon_used), color = "olivedrab3") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Sales and Coupon Usage") + theme_light()
      # list the 2 plot
      list(onlineplot, offlineplot)
    } else if(!is.null(b) & !is.null(c) & is.null(a)){
      # plot with coupon $ without coupon
      # online part
      onlineplot <- ggplot(online_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = Coupon_used), color = "olivedrab3") + 
        geom_line(aes(x = Date, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Online Sales and Coupon Usage") + theme_light()
      # offline part
      offlineplot <- ggplot(offline_Usage_Total_ratio_hyt) + 
        geom_line(aes(x = Date, y = Coupon_used), color = "olivedrab3") + 
        geom_line(aes(x = Date, y = Coupon_no), color = "indianred1") + 
        xlab("") + ylab("Number of Orders") + scale_x_date(date_labels = "%m-%Y") +
        ggtitle("Offline Sales and Coupon Usage") + theme_light()
      # list the 2 plot
      list(onlineplot, offlineplot)
    } else {
      # return error
      return("error")
    } 
  } else {
    # return error
    return("error")
  }
}

# HYT Naive Bayes Data Import & manipulation
offlinevalid.df_hyt <- read_csv("Naive_Bayes_Validation.csv")
offline_test_hyt <- read_csv("Naive_Bayes_Test.csv")

# Control function of the NavieBayes 
# input Dataset, choose the validating DF or the test DF






#####################################################################################tt part1 end







Events <- ordered(BigTop100$Event, levels = c("50 Free", "100 Free", "200 Free", "500 Free", "1000 Free", "1650 Free", "100 Fly", "200 Fly", "100 Back", "200 Back", "100 Breast", "200 Breast", "100 IM", "200 IM", "400 IM", "200 Free Relay", "400 Free Relay", "800 Free Relay", "200 Medlay Relay", "400 Medlay Relay"))




mmss_format <- function(x, ...) {
  sec <- x%%60
  min <- x%/%60
  sec <- base::sprintf("%05.2f", sec)
  ifelse(min == 0, paste(sec), 
         paste(min, sec, sep = ":"))
}

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"


# Define UI
ui <- fluidPage(
  
#Navbar structure for UI
  navbarPage("O2O Coupon Redemption", theme = shinytheme("lumen"),
             
             
             
             
##################################################################################################xulu part2
             tabPanel("Data Overview", fluid = TRUE, icon = icon("globe-americas"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Coupon usage condition"),
                          
                          # Select linetype
                          selectInput(inputId = "linetypeFinder_xulu",
                                      label = "Choose data sourse (One-thousandth sample)",
                                      choices = c("offline"="offline.s","online"="online.s"),
                                      selected = "50 Free",
                                      width = "220px"
                          ),
                          uiOutput("obs1"),
                          uiOutput("bins"),
                          actionButton(
                            inputId = "reset_xulu",
                            label = "Reset Data",
                            icon = icon("refresh"),
                            width = "100%"
                          ),
                          verbatimTextOutput("aaa")
                        ),
                        mainPanel(
                          fluidRow(
                            column(6,
                                   DT::dataTableOutput("dataSet")),
                            column(6,
                                   plotOutput(
                                     #"plotChart",
                                     "dataplot_xulu",
                                     width = "100%",
                                     height = "300px"
                                   ))
                            ),
                          )
                      )
             ),  



##########################################################################################xulu overview end            
             
############################################################################################tt prediction 
# Hyt part Prediction
tabPanel("Prediction", fluid = TRUE, icon = icon("connectdevelop",lib = "font-awesome"),
         titlePanel("Prediction"),
         sidebarLayout(
           sidebarPanel(
             width = 2,
             selectInput(
               inputId = "data_set_predic_hyt",
               label = "Choose the data",
               choices = c("Validation","Test"),
               selected = "Validation",
               width = "100%"
             )
           ),
           mainPanel(
             DT::dataTableOutput("nb_predict_table_hyt")
           )
         )
    
),

############################################################################################tt prediction end


  navbarMenu("Divisions Comparisons", icon = icon("chart-bar"),
##############################################################################################tt part2 
tabPanel("Time Series Coupon & Sales", fluid = TRUE,
         tags$style(button_color_css),
         titlePanel("Time Series Coupon & Sales"),
         # 默认出现 Numeric Comparison,Coupon Usage的图
         # sidebar 包含两个固定位置的下拉单??? TS_Comparison_type_hyt, TS_View_type_hyt
         # 第三个位置的check box根据TS_View_type_hyt的选择变化
         sidebarLayout(
           sidebarPanel(
             width = 2,
             selectInput(
               inputId = "TS_comparison_type_hyt",
               label = "What kind of comparision",
               choices = c("Numeric","Ratio"),
               selected = "Ratio",
               width = "100%"
             ),
             selectInput(
               inputId = "TS_view_type_hyt",
               label = "Choose your analysis perspective",
               choices = c("Coupon Usage","Sales-Coupon Usage"),
               selected = "Coupon Usage",
               width = "100%"
             ),
             uiOutput("TS_select_line_hyt")
           ),
           # 根据inputId TS_Comparison_type_hyt 决定样式
           # Numeric Comparison 两列，Ratio Comparison一???
           # 需要在server中定义obserevent
           mainPanel(
             uiOutput("TS_main_hyt")
           )                        
         )
         
         # fluidRow(
         #   column(4,
         #          selectInput(inputId = "DivCompRaceA",
         #                      label = "Select Event",
         #                      choices = levels(Events),
         #                      selected = "50 Free")),
         #   column(4,
         #          sliderInput(inputId = "DivCompRankA", 
         #                      label = "Top Times Range:", 
         #                      min = 1, max = 3500, 
         #                      value = c(1,250))),
         #   column(4,
         #          checkboxGroupInput(inputId = "DivCompGenderA",
         #                             label = "Select Gender(s):",
         #                             choices = c("Male" = "M", "Female" = "F"),
         #                             selected = "M"))),
         # hr(),
         #          helpText("Tip: Highlight points to populate table"),
         # br(),
         # fluidRow(
         #   column(6,
         #          withSpinner(plotOutput(outputId = "DivCompPlotA", 
         #                                 brush = "brush_plotDiv"
         #                                 #click = "click_plotDiv"
         #                                 ))),
         # # hr(),
         # # conditionalPanel(
         # #   condition = "output.DivCompTable",
         # # column(1.5, offset = 10.5, actionButton(inputId = "DivCompClear", label = "Clear Table"))
         # # ),
         # #br(),
         # column(6,
         # dataTableOutput(outputId = "DivCompTable")
         #   ))
),





##############################################################################################xiaofei part2
### XFQ PART "Distance & Coupon Usage"
tabPanel("Distance & Coupon Usage", fluid = TRUE,
         titlePanel("Distance & Coupon Usage"),
         sidebarLayout(
           sidebarPanel(
             radioButtons("TYPE",
                          "Choose Graph:",
                          c("Frequency Graph","Ratio Graph","Both")
             )
           ),
           mainPanel(
             plotOutput("graph")
           )
         )
),
##############################################################################################xiaofei part2 end
             

##############################################################################################Xulu part3
             tabPanel("top n merchant", fluid = TRUE,
                      tags$style(button_color_css),
                      titlePanel("Top n merchant"),
                      fluidRow(
                        
                        #Linetype
                        column(4,
                               #selectInput(inputId = "DivCompRaceA",
                               selectInput(inputId = "linetype_xulu",
                                           label = "Online or Offline condition",
                                           choices = c("online" = "online", "offline" = "offline"))),
                        
                        #rof
                        column(4,
                               #selectInput(inputId = "DivCompRaceA",
                               selectInput(inputId = "rof_xulu",
                                           label = "Ranking method ",
                                           choices = c("frequency" = "freq", "usage/issuance" = "ratioiss", "usage/consumption" = "ratioconsum"),
                                           selected = "frequency")),
                        #n
                        column(4,
                               #sliderInput(inputId = "DivCompRankA",
                               sliderInput(inputId = "n_xulu", 
                                           label = "select Top n merchant", 
                                           min = 1, max = 50, 
                                           value = 5)),
                        #
                        column(4,
                               uiOutput("dist_xulu")
                               )
  
                        
                        
                        ),
                      hr(),
                               helpText("Tip:"),
                      br(),
                      fluidRow(
                        column(6,
                              # withSpinner(plotOutput(outputId = "DivCompPlotA", 
                               withSpinner(plotOutput(outputId = "topn_xulu"
                                                      #click = "click_plotDiv"
                                                      ))),
                      # hr(),
                      # conditionalPanel(
                      #   condition = "output.DivCompTable",
                      # column(1.5, offset = 10.5, actionButton(inputId = "DivCompClear", label = "Clear Table"))
                      # ),
                      #br(),
                     )),
             
             
             
             
             
#########################################################################################################xulu part3 end

#######################################################################################################tj view
tabPanel("Days From Receive To Redeem Count Distribution", fluid = TRUE,
         tags$style(button_color_css),
         titlePanel("Days From Receive To Redeem Count Distribution"),
         sidebarLayout(
           sidebarPanel(
             width = 2,
             selectInput(
               inputId = "ViewTypeAtyt",
               label = "Select type",
               choices = c("online","offline"),
               selected = "offline",
               width = "100%"
             ),
             sliderInput(inputId = "ViewRangeAtyt", 
                         label = "Redeem Days Range:", 
                         min = 0, max = 200, 
                         value = c(0,200)
             )
             
             #selectInput(
             # inputId = "TS_view_type_hyt",
             #label = "Choose your analysis perspective",
             #choices = c("Coupon Usage","Sales-Coupon Usage"),
             #selected = "Coupon Usage",
             #width = "100%"
             #),
             #uiOutput("TS_select_line_tyt")
           ),
           # 根据inputId TS_Comparison_type_hyt 决定样式
           # Numeric Comparison 两列，Ratio Comparison一列
           # 需要在server???定义obserevent
           mainPanel(
             plotOutput("main_tyt")
             # fluidPage(
             #   uiOutput("main_tyt")
             # )
           )                        
         )
),
# fluidRow(
#   column(4,
#          selectInput(inputId = "DivCompRaceA",
#                      label = "Select Event",
#                      choices = levels(Events),
#                      selected = "50 Free")),
#   column(4,
#          sliderInput(inputId = "DivCompRankA", 
#                      label = "Top Times Range:", 
#                      min = 1, max = 3500, 
#                      value = c(1,250))),
# column(4,
#        checkboxGroupInput(inputId = "DivCompGenderA",
#                           label = "Select Gender(s):",
#                           choices = c("Male" = "M", "Female" = "F"),
#                           selected = "M"))),
# hr(),
#          helpText("Tip: Highlight points to populate table"),
# br(),
# fluidRow(
#   column(6,
#          withSpinner(plotOutput(outputId = "DivCompPlotA", 
#                                 brush = "brush_plotDiv"
#                                 #click = "click_plotDiv"
#                                 ))),
# # hr(),
# # conditionalPanel(
# #   condition = "output.DivCompTable",
# # column(1.5, offset = 10.5, actionButton(inputId = "DivCompClear", label = "Clear Table"))
# # ),
# #br(),
# column(6,
# dataTableOutput(outputId = "DivCompTable")
#   ))

##########################################################################
#TYT Continue
######################################################################
tabPanel("Use Rate and Coupon Rate Relationship", fluid = TRUE,
         tags$style(button_color_css),
         titlePanel("Use Rate and Coupon Rate Relationship"),
         sidebarLayout(
           sidebarPanel(
             width = 2,
             selectInput(
               inputId = "ViewTypeBtyt",
               label = "Select type",
               choices = c("online","offline"),
               selected = "offline",
               width = "100%"
             ),
             # sliderInput(inputId = "ViewRangeAtyt", 
             #             label = "Redeem Days Range:", 
             #             min = 0, max = 200, 
             #             value = c(0,200)
             # )
             
             
             checkboxGroupInput(
               inputId = "ViewDiscountcoltyt",
               label = "Choose discount percent off category for statistic column chart",
               choices = c("0%","1%",'2%','3%','5%','10%','20%','25%','50%'),
               selected = c("10%",'20%'),
               width = "100%"
             ),
             
             checkboxGroupInput(
               inputId = "ViewDiscountboxtyt1",
               label = "Choose discount percent off category for box plot",
               choices = c("0%","1%",'2%','3%','5%','10%','20%','25%','50%'),
               selected = c("10%",'20%','50%'),
               width = "100%"
             )
             #uiOutput("TS_select_line_tyt")
           ),
           
           mainPanel(fluidPage(fluidRow(
             column(6,
                    plotOutput('column_tyt')),
             column(6,
                    plotOutput('boxplot_tyt'))
           ))
           )
           
           # plotOutput("main_tyt")
           # fluidPage(
           #   uiOutput("main_tyt")
           # )
           
         ))

##########################################################################################################tj view end
             

           
             
           
  ),


######################################################################xiaofei more 
navbarMenu("More", icon = icon("info-circle"),
           tabPanel("Data Description", fluid = TRUE,
                    fluidRow(
                      column(6,
                             h4(p("Data Description")),
                             HTML('<img src="1.png",width="1000px",height="1000px"'),
                             br(),
                             HTML('<img src="2.png",width="1000px",height="1000px"'),
                             br(),
                             HTML('<img src="3.png",width="1000px",height="1000px"'),
                             br(),
                      ))
                    
           ),
           
           tabPanel("About", fluid = TRUE,
                    fluidRow(
                      column(6,
                             #br(),
                             h4(p("About the Project")),
                             h5(p("This project is intended to facilitate useful comparisons between colleges in the NCAA, based on swimming performance, location, and academic information.  Here a prospective student-athlete, or anyone else with an interest can find schools fitting a particular set of criterion relevant to them, for example, schools close to home, with times in a particular range, and of a specified academic profile.")),
                             br(),
                             h5(p("The project began as an attempt to combine my interest in swimming with a need to practice R, a programming language used primarily for analyzing and reporting data.  It has two components.  The first is this app, which queries a dataset to return information in the form of plots, data tables etc.  The second is the dataset itself, which I assembled by tying together information from the sources below.")),
                             br(),
                             h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at gpilgrim2607@gmail.com"))
                             
                             #hr(),
                             
                      ),
                      column(6,
                             
                             h4(p("About the Author")),
                             h5(p("Yating Tao, Yitao Huang, Xiaofei Qu, Xulu Zhang")
                             ),
                             
                             br()
                      )
                    ),
                    br(),
                    hr(),
                    
           )
)

######################################################################xiaofei more end
)
)

# Define server
server <- function(input, output, session) {
##################################################################################tt part
  #Program Finder
  
  TimeFinderDF <- reactive({
    req(input$TimeFinderMin)
    TimeFinderDF <- as.data.frame(c(input$TimeFinderMin, input$TimeFinderMax))
    names(TimeFinderDF)[1] <- "UserTimes"
    TimeFinderDF$UserTimes <- as.character(TimeFinderDF$UserTimes)
    TimeFinderDF <- tidyr::separate(TimeFinderDF, col = UserTimes, c("min", "sec"), sep = ":", remove = FALSE, extra = "drop", fill = "left")
  TimeFinderDF[is.na(TimeFinderDF)] <- 0
  TimeFinderDF$sec <- as.numeric(TimeFinderDF$sec)
  TimeFinderDF$min <- as.numeric(TimeFinderDF$min)
  TimeFinderDF <- TimeFinderDF %>%
    mutate(Time = (TimeFinderDF$min*60) + TimeFinderDF$sec)
  })
  
  BigTop100_finder <- reactive({
    req(input$DivisionFinder)
    req(input$RegionFinder)
    req(input$School_TypeFinder)
    req(input$GenderFinder)
    req(input$EventFinder)
    #req(Input$School_Rank)
    filter(BigTop100, Division %in% input$DivisionFinder) %>%
      filter(Region %in% input$RegionFinder) %>%
      filter(Event %in% input$EventFinder) %>%
      filter(Time >= TimeFinderDF()$Time[1], Time <= TimeFinderDF()$Time[2]) %>%
      filter(Sex %in% input$GenderFinder) %>%
      filter(Type %in% input$School_TypeFinder) %>%
      filter(Y2019 >= input$School_RankFinder[1], Y2019 <= input$School_RankFinder[2]) %>%
      filter(RankInEvent_Team >= input$RankOnTeam[1], RankInEvent_Team <= input$RankOnTeam[2]) %>%
      group_by(Team, Event) %>%
      dplyr::mutate(Entries = n()) %>%
      dplyr::mutate(MinTime = mmss_format(min(Time))) %>%
      dplyr::mutate(MaxTime = mmss_format(max(Time)))
                      
  })
  
  fiftystatesCAN_Finder <- reactive({
    req(input$RegionFinder)
    filter(fiftystatesCAN, GeoRegion %in% input$RegionFinder)
  })   
  
  uniquecities_Finder <- reactive({
    req(input$RegionFinder)
    filter(uniquecities, Region %in% input$RegionFinder) %>%
      filter(Team %in% BigTop100_finder()$Team)
  })   

  output$scatterplotFinder <- renderPlot({
    input$EnterTimes
    input$show_NamesFinder
    input$GenderFinder
    input$DivisionFinder
    input$RegionFinder
    input$RankOnTeam
    input$School_TypeFinder
    input$School_RankFinder
    isolate({
      if (length(BigTop100_finder()$Address) == 0) {
        ggplot() +
          geom_polygon(data = fiftystatesCAN_Finder(), aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          coord_quickmap() +
          theme_void() +
          ggtitle("No programs fit selected characteristics. \nPlease modify selections.") +
          theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 20))
      } else {
        ggplot() +
          geom_polygon(data = fiftystatesCAN_Finder(), aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          geom_point(data = uniquecities_Finder(), aes(x = lon, y = lat, alpha = 0.8)) +
          {if(input$show_NamesFinder == "School Names") geom_text_repel(data = uniquecities_Finder(), aes(x = lon, y = lat, label = as.character(Team)))} +
          {if(input$show_NamesFinder == "City Names") geom_text_repel(data = uniquecities_Finder(), aes(x = lon, y = lat, label = as.character(City)))} +
          coord_quickmap() +
          guides(fill = FALSE) +
          geom_point(data = BigTop100_finder(), aes(x = lon, y = lat, color = Division, shape = Sex), alpha = 0.5) +
          theme_void() +
          labs(color = "Division", shape = "Gender"
               #, title = pretty_plot_title()
          ) +
          {if(length(input$DivisionFinder) <= 1) scale_color_manual(guide = "none", values = c("DI" = "#1E90FF", "DII" = "#FF8D1E", "DIII" = "#20FF1E"))} +
          {if(length(input$DivisionFinder) > 1)
            scale_color_manual(values = c("DI" = "blue", "DII" = "red", "DIII" = "green"))} +
            {if(length(input$GenderFinder) <= 1) scale_shape_manual(guide = "none", values = c("M" = "circle", "F" = "triangle"))} +
            {if(length(input$GenderFinder) > 1)
              scale_shape_manual(values = c("M" = "circle", "F" = "triangle"))} +
          theme(axis.text = element_blank(), axis.ticks = element_blank()) +
          theme(plot.title = element_text(hjust=0.5, face = "bold")) +
          theme(plot.background = element_rect(fill = "white"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
          guides(alpha = FALSE) +
          theme(legend.text = element_text(size = 12),
                legend.title = element_text(size = 15))
        
      }
    })
  })
  
  user_clickFinder <- reactiveValues()
  reactive({
    user_clickFinder$DT <- data.frame(matrix(0, ncol = ncol(BigTop100), nrow = 1))
    names(user_clickFinder$DT) <- colnames(BigTop100)
  })
  
  observeEvent(input$click_plotFinder, {
    add_row <-     nearPoints(BigTop100_finder(), input$click_plotFinder, xvar = "lon", yvar = "lat", threshold = 5)
    user_clickFinder$DT <- rbind(add_row, user_clickFinder$DT)
  })
  
  brushFinder <- reactive({
    req(length(user_clickFinder$DT) > 1)
    user_clickFinder$DT
  })  
  
  observeEvent({
    input$FinderClear
    #input$EnterTimes
  },{
    user_clickFinder$DT <- NULL
  })
  
  output$schoolstableFinder<-DT::renderDataTable({
    
    DT::datatable(unique(brushFinder()[,c("Name", "Class", "X.swim_time", "Team", "Relative_RankInEvent_Team", "Division", "Address", "Y2019", "Type", "Time")]), 
                  colnames = c("Sort" = "Time", "Time" = "X.swim_time", "US News School Ranking" = "Y2019", "School Type" = "Type", "Swimmer Rank In Event On Team" = "Relative_RankInEvent_Team"), 
                  rownames = FALSE, 
                  options = list(order = list(9, 'asc'),
                                 columnDefs = list(list(visible=FALSE, targets=c(9)),
                                                   list(className = "dt-center", targets = 1:7),
                                                   list(classname = "dt-right", targets = 8))
                  ))
    
  })
  
  #Program Comparisons
  
  BigTop100_SchoolComp <- reactive({
    req(input$SchoolCompGender)
    req(input$SchoolSelectA)
    req(input$SchoolCompRace)
    filter(BigTop100, Sex %in% input$SchoolCompGender) %>%
      filter(Event %in% input$SchoolCompRace) %>%
      filter(Team %in% input$SchoolSelectA | Team %in% input$SchoolSelectB)
    
  })
  reactive({
    BigTop100_SchoolComp$Time <- as.numeric(format(BigTop100_SchoolComp()$Time, nsmall = 2))
  })
  
  output$SchoolCompPlotEvent <- renderPlot({
    ggplot(data = BigTop100_SchoolComp(), aes(y = Time, x = Team, color = Team)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(position = position_jitter(width = 0.05), alpha = 0.8) +
      scale_color_manual(values=c("#1E90FF", "#20FF1E", "#FF8D1E", "#FD1EFF")) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      theme(legend.title=element_blank(), panel.grid.major = element_line(color = "white"), panel.grid.minor = element_line(color = "white")) +
      theme(plot.title = element_text(hjust=0.5, face = "bold")) +
      theme(legend.position="none") +
      scale_y_continuous(labels = scales::trans_format("identity", mmss_format)) +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 15),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15))
    
    
  }) 
  
  output$SchoolCompDT<-DT::renderDataTable({
    DT::datatable(BigTop100_SchoolComp()[,c("Name", "Team", "X.swim_time", "Class", "Rank", "Division", "Time")], 
                  colnames = c("Sort" = "Time", "Time" = "X.swim_time"), 
                  rownames = FALSE,
                  options = list(order = list(6, 'asc'),
                                 columnDefs = list(list(visible=FALSE, targets=6),
                                                   list(className = "dt-center", targets = 1:5)
                                                   #list(className = "dt-right", targets = 5)
                                 ))
                  
    )
  })
  
  
  output$SchoolCompStats<-DT::renderDataTable({
    if(input$TuitionType == "Yes"){
      DT::datatable(unique(BigTop100_SchoolComp()[,c("Team", "Type", "Y2019", "Tuition_In", "Enrollment", "Public")]),
                    colnames = c("US News Ranking" = "Y2019", "Tuition" = "Tuition_In"),
                    rownames = FALSE,
                    options = list(order = list(0, 'asc'),
                                   columnDefs = list(list(className = "dt-center", targets = 1:5)),
                                   dom = 't'
                                   
                    ))
    }
    else if(input$TuitionType == "No"){
      DT::datatable(unique(BigTop100_SchoolComp()[,c("Team", "Type", "Y2019", "Tuition_Out", "Enrollment", "Public")]),
                    colnames = c("US News Ranking" = "Y2019", "Tuition" = "Tuition_Out"),
                    rownames = FALSE,
                    options = list(order = list(0, 'asc'),
                                   dom = 't',
                                   list(columnDefs = list(list(className = "dt-center", targets = 1:5)))
                    ))
    }
  })
  
  #Division Comparisons
  
###############################################################################################xulu part4

  
  values <- reactiveValues(tbl = NULL,
                           obsList = NULL,
                           plot.df = NULL)
  
  observeEvent(input$linetypeFinder_xulu, {
    if (!NA %in% match(input$linetypeFinder_xulu, c("offline.s", "online.s"))) {
      values$tbl <- as.data.frame(get(input$linetypeFinder_xulu))
      values$obsList <- colnames(values$tbl)
      
      output$obs1 <- renderUI({
        selectInput(
          inputId = "observationInput1",
          label = "observation",
          choices =  values$obsList
        )
      })
    }
  })

  observeEvent(input$observationInput1,{
    dtf<- values$tbl
    l<-dtf[,eval(input$observationInput1)]
    if (is.numeric(l[[1]])) {
      output$bins<- renderUI({
        sliderInput(
          inputId = "bins",
          label = "Number of bins:",
          min = 1, 
          max = 50, 
          value = 30)
      })
    }
  }
  
  )
  observeEvent(input$observationInput1, {
    values$plot.df <-
      as.data.frame(values$tbl[, input$observationInput1])
    colnames(values$plot.df) <- input$observationInput1
    output$dataSet <- DT::renderDataTable({
      values$tbl
    },
    extensions = c('Scroller', 'FixedColumns'),
    options = list(
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 200,
      scroller = TRUE,
      dom = 'Bfrtip',
      fixedColumns = TRUE
    ))
  })
  
  observe({
    output$dataplot_xulu <- renderPlot({
        chartCol_xulu(df=values$plot.df,colN=input$observationInput1,b=input$bins)
      })
  })
  
  
  observeEvent(input$reset_xulu, {
    values$tbl <- NULL
    output$obs1 <- NULL
  })
  
  output$aaa <- renderPrint({
    values$obs1
  })
  
  
  
  
  observeEvent(input$linetype_xulu,{
    if (eval(input$linetype_xulu)=="offline" ) {
      output$dist_xulu<- renderUI({
        sliderInput(
          inputId = "dist_xulu",
          label = "offline merchant distance",
          min = 0, 
          max = 15, 
          value = c(0,15))
      })
    }
  
    else{
      output$dist_xulu<- renderUI({})
    }
  }
  )
  

  output$topn_xulu <- 
  renderPlot({
    topn(n=input$n_xulu,linetype=input$linetype_xulu,rof=input$rof_xulu,dist=input$dist_xulu)
  })

  
  
####################################################################################################xulu server end
  
  
  

################################################################################### server Hyt part

  # server Hyt part
  # Conditional Main panel 
  # When Select Nuemeric shows 2 graphs
  # When Select Ratio shows 1 graphs
  observeEvent(input$TS_comparison_type_hyt,{
    if (input$TS_comparison_type_hyt == "Numeric") {
      output$TS_main_hyt <- renderUI({
        fluidPage(
          fluidRow(
            plotOutput(
              "TS_online_numeric_hyt",
              width = "100%",
              height = "300px"
            )
          ),
          hr(),
          fluidRow(
            plotOutput(
              "TS_offline_numeric_hyt",
              width = "100%",
              height = "300px"
            )
          )
        )
      })
    } else if(input$TS_comparison_type_hyt == "Ratio"){
      output$TS_main_hyt <- renderUI({
        fluidRow(
          column(12,
                 plotOutput(
                   "TS_ratio_hyt",
                   width = "100%",
                   height = "300px"
                 ))
        )
      })
    } else {
      return("error")
    }
  })  
  
  # Conditional Select line checkbox
  # When Select Ratio shows 2 options
  # When Select Numeric shows 3 options
  observeEvent(input$TS_comparison_type_hyt,{
    if (input$TS_comparison_type_hyt == "Numeric") {
      output$TS_select_line_hyt <- renderUI({
        checkboxGroupInput(
          inputId = "TS_line_CB_numeric_hyt",
          label = "Choose the line to show",
          choices = c("Total","Without Coupon", "With Coupon or Coupon Used"),
          selected = c("Total","Without Coupon", "With Coupon or Coupon Used")
        )
      })
    } else if(input$TS_comparison_type_hyt == "Ratio"){
      output$TS_select_line_hyt <- renderUI({
        checkboxGroupInput(
          inputId = "TS_line_CB_ratio_hyt",
          label = "Choose the line to show",
          choices = c("Online","Offline"),
          selected = c("Online","Offline")
        )
      })
    } else {
      return("error")
    }
  })   
  
  # TS_online_numeric_hyt plot
  # Online Numeric part
  output$TS_online_numeric_hyt <- renderPlot({
    tsfunc_hyt(input$TS_comparison_type_hyt,input$TS_view_type_hyt, input$TS_line_CB_numeric_hyt)[1]
  })
  
  
  # TS_offline_numeric_hyt
  # Offline Numeric part
  output$TS_offline_numeric_hyt <- renderPlot({
    tsfunc_hyt(input$TS_comparison_type_hyt,input$TS_view_type_hyt, input$TS_line_CB_numeric_hyt)[2]
  })
  
  # TS_ratio_hyt
  # Ratio part
  output$TS_ratio_hyt <- renderPlot({
    tsfunc_hyt(input$TS_comparison_type_hyt,input$TS_view_type_hyt, input$TS_line_CB_ratio_hyt)
  })
  
  # naive bayes table
  output$nb_predict_table_hyt <- renderDataTable({
    if (input$data_set_predic_hyt == "Validation"){
      offlinevalid.df_hyt
    } else {
      offline_test_hyt
    }
  })
  
  
  ################################################################################# ttserver end
  
  ##################################################################################xiaofei server
  
  output$graph <- renderPlot({
    print("ok")
    if(input$TYPE=="Frequency Graph"){
      g1x <- Distance_plot(lm,v1="Distance",v2="Frequency")
      g1x
    }else if(input$TYPE=="Ratio Graph"){
      g2x <- Distance_plot(lm,v1="Distance",v2="ratio")
      g2x
    }else if(input$TYPE=="Both"){
      p <- ggplot(lm, aes(x = Distance))
      p <- p + geom_line(aes(y=ratio, colour = "Ratio"))
      p <- p + geom_line(aes(y=Frequency/500000, colour = "frequency"))
      p <- p + scale_y_continuous(sec.axis = sec_axis(~.*500000, name = "frequency"))
      p <- p + scale_colour_manual(values = c("blue", "red"))
      p <- p + labs(y = "Ratio",x = "Distance",colour = "Parameter")
      p <- p + theme(legend.position = c(0.8, 0.9))
      p
    }
  })
  
  ##################################################################################xiaofei serverend
  
  ###################################################################################TYT server part
  output$main_tyt <- renderPlot({
    print("ok")
    if(input$ViewTypeAtyt=="offline"){
      g1 <- redeemtime(range=input$ViewRangeAtyt,type="offline")
      g1
    }else{
      g2 <- redeemtime(range=input$ViewRangeAtyt,type="online")
      g2
    }
  })
  output$column_tyt <- renderPlot({
    print('ok')
    g3 <- UseRateCouponRatecol(discount_col = input$ViewDiscountcoltyt,type=input$ViewTypeBtyt)
    g3
  })
  output$boxplot_tyt <- renderPlot({
    print('ok')
    g4 <- UseRateCouponRatebox(discount_box = input$ViewDiscountboxtyt1,type=input$ViewTypeBtyt)
    g4
  })
  #TYT server part end
  # #######################################################################################TYT server part end
  
  
  
  BigTop100_subsetACA_DI <- reactive({
    req(input$GenderDI)
    req(input$RegionDI)
    req(input$RankDI)
    filter(BigTop100, Division == "DI") %>%
      filter(Sex %in% input$GenderDI) %>%
      filter(Region %in% input$RegionDI) %>%
      filter(Rank >= input$RankDI[1], Rank <= input$RankDI[2]) %>%
      filter(Y2019 >= input$School_RankDI[1], Y2019 <= input$School_RankDI[2]) %>%
      group_by(Team) %>%
      dplyr::mutate('No. of Top Times' = n())
  })

  BigTop100_subsetACA_DII <- reactive({
    req(input$GenderDII)
    req(input$RegionDII)
    req(input$RankDII)
    filter(BigTop100, Division == "DII") %>%
      filter(Sex %in% input$GenderDII) %>%
      filter(Region %in% input$RegionDII) %>%
      filter(Rank >= input$RankDII[1], Rank <= input$RankDII[2]) %>%
      filter(Y2019 >= input$School_RankDII[1], Y2019 <= input$School_RankDII[2]) %>%
      group_by(Team) %>%
      dplyr::mutate('No. of Top Times' = n())
  })

  BigTop100_subsetACA_DIII <- reactive({
    req(input$GenderDIII)
    req(input$RegionDIII)
    req(input$RankDIII)
    filter(BigTop100, Division == "DIII") %>%
      filter(Sex %in% input$GenderDIII) %>%
      filter(Region %in% input$RegionDIII) %>%
      filter(Rank >= input$RankDIII[1], Rank <= input$RankDIII[2]) %>%
      filter(Y2019 >= input$School_RankDIII[1], Y2019 <= input$School_RankDIII[2]) %>%
      group_by(Team) %>%
      dplyr::mutate('No. of Top Times' = n())
  })
  
  BigTop100_DivCompA <- reactive({
    req(input$DivCompGenderA)
    req(input$DivCompRankA)
    req(input$DivCompRaceA)
    filter(BigTop100, Sex %in% input$DivCompGenderA) %>%
    filter(Rank >= input$DivCompRankA[1], Rank <= input$DivCompRankA[2]) %>%
    filter(Event %in% input$DivCompRaceA)
  })
  reactive({
  BigTop100_DivCompA$Time <- as.numeric(format(BigTop100_DivCompA()$Time, nsmall = 2))
  })
  

  output$barplotDI <- renderPlot({
    ggplot() +
    geom_bar(data = BigTop100_subsetACA_DI(), aes(x = Division, y = (..count../sum(..count..)*100), fill = Type)) +
    labs(y = "Percent", x = "Divison") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("National University" = "#1E90FF", "National Liberal Arts College" = "#FD1EFF", "Regional College" = "#FF8D1E", "Regional University" = "#20FF1E"), aesthetics = "fill") +
    theme_void()
  })
  
  output$description_DI <- renderText({
    paste0("Division I is primarily made of national universities, with a sizable subset of regional universities.
           There are relatively few colleges.")
  })
  
  output$barplotDII <- renderPlot({
    ggplot() +
      geom_bar(data = BigTop100_subsetACA_DII(), aes(x = Division, y = (..count../sum(..count..)*100), fill = Type)) +
      labs(y = "Percent", x = "Divison") +
      scale_fill_manual(values = c("National University" = "#1E90FF", "National Liberal Arts College" = "#FD1EFF", "Regional College" = "#FF8D1E", "Regional University" = "#20FF1E"), aesthetics = "fill") +
      coord_polar("y", start=0) +
      theme_void()
  })
 
  output$description_DII <- renderText({
    paste0("Division II is primarily made of regional universities, with a national universities as the second largest component.
           There are relatively few national or regional colleges.")
  })

  output$barplotDIII <- renderPlot({
    ggplot() +
      geom_bar(data = BigTop100_subsetACA_DIII(), aes(x = Division, y = (..count../sum(..count..)*100), fill = Type)) +
      labs(y = "Percent", x = "Divison") +
      scale_fill_manual(values = c("National University" = "#1E90FF", "National Liberal Arts College" = "#FD1EFF", "Regional College" = "#FF8D1E", "Regional University" = "#20FF1E"), aesthetics = "fill") +
      coord_polar("y", start=0) +
      theme_void()
  })  

  output$description_DIII <- renderText({
    paste0("Division III is primarily made of national universities and national liberal arts colleges.  
          Regional universities and colleges are a smaller component")
  })
  
  output$DivCompPlotA <- renderPlot({
    topn()
  }) 

  #using brush plot
  
  brushDiv <- reactive({
    user_brushDiv <- input$brush_plotDiv
    brushedPoints(BigTop100_DivCompA(), user_brushDiv, xvar = "Division", yvar = 
                    "Time")
  })
  
  observeEvent(input$DivCompClear, {
    brushDiv <- NULL
  })
  
  #using click plot
  
  # user_clickDiv <- reactiveValues()
  # reactive({
  #   user_clickDiv$DT <- data.frame(matrix(0, ncol = ncol(BigTop100_DivCompA()), nrow = 1))
  #   names(user_clickDiv$DT) <- colnames(BigTop100_DivCompA())
  # })
  # 
  # observeEvent(input$click_plotDiv, {
  #   add_row <-     nearPoints(BigTop100_DivCompA(), input$click_plotDiv, xvar = "Division", yvar = "Time", threshold = 8)
  #   user_clickDiv$DT <- rbind(add_row, user_clickDiv$DT)
  # })
  # 
  # brushDiv <- reactive({
  #   req(length(user_clickDiv$DT) > 1)
  #   user_clickDiv$DT
  # })
  # 
  # observeEvent(input$DivCompClear, {
  #   user_clickDiv$DT <- NULL
  # })
  
  output$DivCompTable<-DT::renderDataTable({
    DT::datatable(unique(brushDiv()[,c("Name", "Team", "X.swim_time", "Rank", "Division", "Time")]),
                  colnames = c("Sort" = "Time", "Time" = "X.swim_time", "Rank In Division" = "Rank"),
                  rownames = FALSE,
                  options = list(order = list(5, 'asc'),
                                 columnDefs = list(list(visible=FALSE, targets=c(5)),
                                                   list(className = "dt-center", targets = 1:5)
                                 ))
    )
  })
  
  #session$onSessionEnded(stopApp)
} 

# Run the application 
shinyApp(ui = ui, server = server)

