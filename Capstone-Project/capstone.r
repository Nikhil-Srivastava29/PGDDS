library(lubridate)
library(reshape2)
library(zoo)
library(ggplot2)
library(MASS)
library(car)
library(cowplot)
library(DataCombine)

rawdata <- read.csv("ConsumerElectronics.csv")
productlist <- read.csv("ProductList.csv")
investments <- read.csv("Investment.csv")
nps_Score <- read.csv("NPS.csv")

# renaming the column names for readability:
colnames(rawdata) <- c("FSN_ID","Order_Date","Year","Month","Order_ID","Order_Item_ID","GMV","Units","DeliverybDays",
                       "DeliverycDays","Payment_Type","SLA","Cust_ID","Pincode","Product_Super_Category","Product_Category",
                       "Product_Sub_Category","Product_Vertical","Product_MRP","Product_Procurement_SLA")
# Check for NA values
# Following columns have NA values
#   GMV       Cust_ID     Pincode
#   4904       4904        4904
sapply(rawdata, function(x) sum(is.na(x)))
#removing the rows with NA values since they are very few and would not impact the overall results
rawdata <- na.omit(rawdata)


head(rawdata)
str(rawdata)

summary(rawdata)

# Since the data to be used is from July 2015 to June 2016, removing the orders which is earlier than July 2015 and
# orders placed after June 2016
rawdata <- subset(rawdata,(Year==2015 & Month>=7) | (Year==2016 & Month<=6))



#Creating a new Date Column for order date
rawdata$Date <- date(rawdata$Order_Date)
#finding the week of the year in which the Order was placed
rawdata$order_week <- week(rawdata$Date)

levels(as.factor(rawdata$order_week))
week(date("2016-06-30"))
week(date("2015-07-01"))
week(date("2015-12-31"))
# Jan 2016 is coming as week 1. Ideally it should be week 54 as the last week in year 2015 is week 53
# Also both 30 jun 2016 and 1st july appear as week 26.

rawdata$order_week <- ifelse(rawdata$Year==2016 & rawdata$order_week<=26,rawdata$order_week+53,rawdata$order_week)

#There are 1349 items where GMV is 0. 
#Since GMV is total revenue, such products may be considered as free products
length(which(rawdata$GMV==0))

# Revenue(GMV) should ideally multiplication of Product units and MRP
# but it is coming less as well. meaning we have given discount on those products
# there are 38558 orders where GMV(revenue) is more that MRP*units

rawdata$gmv_cal <-ifelse(rawdata$GMV<=rawdata$Product_MRP*rawdata$Units,"Equal or Less","more than gmv")
length(which(rawdata$gmv_cal=="more than gmv"))
length(which(rawdata$gmv_cal=="Equal or Less"))

#Considering only those records where GMV is equal or less than the product of units and MRP
rawdata <- subset(rawdata, gmv_cal=="Equal or Less")

#Creating new columns - List Price & Discount Offered based on the above calculation
rawdata$List_Price <- rawdata$GMV/rawdata$Units
rawdata$Discount_Percent <- round((rawdata$Product_MRP-rawdata$List_Price)*100/rawdata$Product_MRP,0)

#replacing \\N from DeliverybDays and DeliverycDays with 0
rawdata$DeliverybDays[which(rawdata$DeliverybDays=="\\N")]<-0
rawdata$DeliverycDays[which(rawdata$DeliverycDays=="\\N")]<-0

#Removing NA values from Radio and other investments with 0
investments$Other[which(is.na(investments$Other))]<-0
investments$Radio[which(is.na(investments$Radio))]<-0


#5.Holiday_column

holiday_list<-c("2015-07-18","2015-07-19","2015-08-15","2015-08-16","2015-08-17","2015-08-28","2015-08-29","2015-08-30","2015-10-15",
                "2015-10-16","2015-10-17","2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-10-11","2015-10-12","2015-11-13",
                "2015-11-14","2015-12-25","2015-12-26","2015-12-27","2015-12-28","2015-12-29","2015-12-30","2016-01-01","2016-01-02",
                "2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02","2016-02-20","2016-02-21","2016-02-14",
                "2016-02-15","2016-03-07","2016-03-08","2016-03-09","2016-05-25","2016-05-26","2016-05-27")

holiday_list<- as.Date(holiday_list)
order_week <- week(holiday_list)
holiday_year <- year(holiday_list)
order_week <- ifelse(holiday_year==2016 & order_week<=26,order_week+53,order_week)
holiday_df <- data.frame(order_week)
holiday_df$holiday_freq<-1

mrp_aggr <- aggregate(Product_MRP~Product_Vertical,rawdata,mean)

meanMRP<-mean(mrp_aggr$Product_MRP)
rawdata$PremiumProducts<-0
rawdata$MassProducts<-0
#creating grouping for Premium and Mass product
# 1=Premium Product
# 2=Mass Product
rawdata$PremiumProducts[which(rawdata$Product_MRP>meanMRP)]<-1
rawdata$MassProducts[which(rawdata$Product_MRP<=meanMRP)]<-1

# Payment type analysis
rawdata$prepaid_orders <- ifelse(rawdata$Payment_Type=="Prepaid",1,0)
rawdata$COD_orders <- ifelse(rawdata$Payment_Type=="COD",1,0)


# Merging data from different files
orders<- merge(rawdata,nps_Score, by.x=c("Year","Month"))
orders <- merge(orders,productlist, by.x = c("Product_Vertical"), by.y = c("Product"))



summary(orders)

CameraAccessory_df <- subset(orders, Product_Sub_Category=="CameraAccessory")
HomeAudio_df <- subset(orders, Product_Sub_Category=="HomeAudio")
GamingAccessory_df <- subset(orders, Product_Sub_Category=="GamingAccessory")

# identify Primary KPIs 
# Start of Aggregation
holiday_aggr <- aggregate(holiday_freq~order_week,holiday_df,FUN = sum)

kpis<-function(df){
Prod_Aggr<- aggregate(cbind(List_Price,Product_MRP,Units,SLA,Discount_Percent,Product_Procurement_SLA,NPS.Score)~ order_week,df,FUN = mean)
gmv_aggr<- aggregate(GMV~order_week,df,FUN = sum)

prodType_aggr<- aggregate(cbind(PremiumProducts,MassProducts)~order_week,df,FUN=sum)

payType_aggr <- aggregate(cbind(prepaid_orders,COD_orders)~order_week,df,FUN=sum)

#merging the various aggregates
final_df<- merge(Prod_Aggr,gmv_aggr)
final_df<- merge(final_df,prodType_aggr)
final_df<-merge(final_df,payType_aggr)
final_df<- merge(final_df,holiday_aggr,all.x = TRUE)
final_df$holiday_freq[is.na(final_df$holiday_freq)] <-0
final_df<-merge(final_df,investments, by.x="order_week",by.y="week")



return(final_df)

}

CameraAccessory_final <- kpis(CameraAccessory_df)
GamingAccessory_final <- kpis(GamingAccessory_df)
HomeAudio_final <- kpis(HomeAudio_df)

lagvar <- function(df){


lag_df <- slide(data=df,Var = "List_Price", slideBy = -1)
lag_df <- slide(data=lag_df,Var = "List_Price", slideBy = -2)
lag_df <- slide(data=lag_df,Var = "List_Price", slideBy = -3)

lag_df <- slide(data=lag_df,Var = "Discount_Percent", slideBy = -1)
lag_df <- slide(data=lag_df,Var = "Discount_Percent", slideBy = -2)
lag_df <- slide(data=lag_df,Var = "Discount_Percent", slideBy = -3)

lag_df$ListPrice_MA1 <- rollmean(lag_df$List_Price, k=2,fill=NA, align = "right")  
lag_df$ListPrice_MA2 <- rollmean(lag_df$List_Price, k=3,fill=NA, align = "right")  
lag_df$ListPrice_MA3 <- rollmean(lag_df$List_Price, k=4,fill=NA, align = "right")  
lag_df$ListPrice_MA1_factor <- (lag_df$List_Price-lag_df$ListPrice_MA1)/lag_df$List_Price
lag_df$ListPrice_MA2_factor <- (lag_df$List_Price-lag_df$ListPrice_MA2)/lag_df$List_Price
lag_df$ListPrice_MA3_factor <- (lag_df$List_Price-lag_df$ListPrice_MA3)/lag_df$List_Price

lag_df$DiscountPercent_MA1 <- rollmean(lag_df$Discount_Percent, k=2,fill=NA, align = "right")  
lag_df$DiscountPercent_MA2 <- rollmean(lag_df$Discount_Percent, k=3,fill=NA, align = "right")
lag_df$DiscountPercent_MA3 <- rollmean(lag_df$Discount_Percent, k=4,fill=NA, align = "right")
lag_df$DiscountPercent_MA1_factor <- (lag_df$Discount_Percent-lag_df$DiscountPercent_MA1)/lag_df$Discount_Percent
lag_df$DiscountPercent_MA2_factor <- (lag_df$Discount_Percent-lag_df$DiscountPercent_MA2)/lag_df$Discount_Percent
lag_df$DiscountPercent_MA3_factor <- (lag_df$Discount_Percent-lag_df$DiscountPercent_MA3)/lag_df$Discount_Percent

lag_df<-lag_df[,-c(30,31,32,36,37,38)]


lag_df<- na.omit(lag_df)

return(lag_df)
}
CameraAccessory_final <- lagvar(CameraAccessory_final)
GamingAccessory_final <- lagvar(GamingAccessory_final)
HomeAudio_final<-lagvar(HomeAudio_final)

adstock <- function(df){
df$TV_adstock <- stats::filter(df$TV,0.4,method="recursive")
df$Digital_adstock <- stats::filter(df$Digital,0.4,method="recursive")
df$Sponsorship_adstock <- stats::filter(df$Sponsorship,0.4,method="recursive")
df$ContentMarketing_adstock <- stats::filter(df$Content.Marketing,0.4,method="recursive")
df$OnlineMarketing_adstock <- stats::filter(df$Online.marketing,0.4,method="recursive")
df$Affiliates_adstock <- stats::filter(df$Affiliates,0.4,method="recursive")
df$SEM_adstock <- stats::filter(df$SEM,0.4,method="recursive")
df$Radio_adstock <- stats::filter(df$Radio,0.4,method="recursive")
df$Others_adstock <- stats::filter(df$Other,0.4,method="recursive")



return(df)
}

CameraAccessory_final <- adstock(CameraAccessory_final)
GamingAccessory_final <- adstock(GamingAccessory_final)
HomeAudio_final <- adstock(HomeAudio_final)

#Plotting the effect of advertisment on Total sales
# CAmera Accessories
plot_grid(
  ggplot(CameraAccessory_final,aes(TV,GMV))+geom_point()+ geom_smooth()+ labs(x = "TV Advertisment", y = "GMV"),
  ggplot(CameraAccessory_final,aes(Digital,GMV))+geom_point()+ geom_smooth()+ labs(x = "Digital Advertisment", y = "GMV"),
  ggplot(CameraAccessory_final,aes(Sponsorship,GMV))+geom_point()+ geom_smooth()+ labs(x = "Sponsorship", y = "GMV"),
  ggplot(CameraAccessory_final,aes(Content.Marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Content Marketing", y = "GMV"),
  ggplot(CameraAccessory_final,aes(Online.marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Online Marketing", y = "GMV"),
  ggplot(CameraAccessory_final,aes(Affiliates,GMV))+geom_point()+ geom_smooth()+ labs(x = "Affiliates", y = "GMV"),
  ggplot(CameraAccessory_final,aes(SEM,GMV))+geom_point()+ geom_smooth()+ labs(x = "SEM", y = "GMV")
)

# Home Audio
plot_grid(
  ggplot(HomeAudio_final,aes(TV,GMV))+geom_point()+ geom_smooth()+ labs(x = "TV Advertisment", y = "GMV"),
  ggplot(HomeAudio_final,aes(Digital,GMV))+geom_point()+ geom_smooth()+ labs(x = "Digital Advertisment", y = "GMV"),
  ggplot(HomeAudio_final,aes(Sponsorship,GMV))+geom_point()+ geom_smooth()+ labs(x = "Sponsorship", y = "GMV"),
  ggplot(HomeAudio_final,aes(Content.Marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Content Marketing", y = "GMV"),
  ggplot(HomeAudio_final,aes(Online.marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Online Marketing", y = "GMV"),
  ggplot(HomeAudio_final,aes(Affiliates,GMV))+geom_point()+ geom_smooth()+ labs(x = "Affiliates", y = "GMV"),
  ggplot(HomeAudio_final,aes(SEM,GMV))+geom_point()+ geom_smooth()+ labs(x = "SEM", y = "GMV")
)


# Gaming Accessory
plot_grid(
  ggplot(GamingAccessory_final,aes(TV,GMV))+geom_point()+ geom_smooth()+ labs(x = "TV Advertisment", y = "GMV"),
  ggplot(GamingAccessory_final,aes(Digital,GMV))+geom_point()+ geom_smooth()+ labs(x = "Digital Advertisment", y = "GMV"),
  ggplot(GamingAccessory_final,aes(Sponsorship,GMV))+geom_point()+ geom_smooth()+ labs(x = "Sponsorship", y = "GMV"),
  ggplot(GamingAccessory_final,aes(Content.Marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Content Marketing", y = "GMV"),
  ggplot(GamingAccessory_final,aes(Online.marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Online Marketing", y = "GMV"),
  ggplot(GamingAccessory_final,aes(Affiliates,GMV))+geom_point()+ geom_smooth()+ labs(x = "Affiliates", y = "GMV"),
  ggplot(GamingAccessory_final,aes(SEM,GMV))+geom_point()+ geom_smooth()+ labs(x = "SEM", y = "GMV")
)

plot_grid(
  ggplot(CameraAccessory_final,aes(order_week,GMV, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "GMV"),
  ggplot(HomeAudio_final,aes(order_week,GMV, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "GMV"),
  ggplot(GamingAccessory_final,aes(order_week,GMV, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "GMV"),
  ggplot(CameraAccessory_final,aes(order_week,Discount_Percent, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "Discount Percent", title="Camera Accessory"),
  ggplot(HomeAudio_final,aes(order_week,Discount_Percent, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "Discount Percent",title="Home Audio"),
  ggplot(GamingAccessory_final,aes(order_week,Discount_Percent, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "Discount Percent", title="Game Accessory")
  
)

# Model Building: Camera Accessories
CameraAccessory_model <- data.frame(scale(CameraAccessory_final[,-c(1,15:23)]))

# first Model
cameraAccessory_model_1 <- lm(GMV~.,CameraAccessory_model)

#Adjusted R-squared:  0.7561 
summary(cameraAccessory_model_1)
stepAIC(cameraAccessory_model_1)


cameraAccessory_model_2 <- lm(formula = GMV ~ List_Price + Product_MRP + Units + SLA + NPS.Score + 
                                PremiumProducts + MassProducts + prepaid_orders + TV + Sponsorship + 
                                Content.Marketing + Online.marketing + Affiliates + SEM + 
                                Radio + Other + List_Price.1 + List_Price.2 + Discount_Percent.1 + 
                                Discount_Percent.2 + Discount_Percent.3 + ListPrice_MA1_factor + 
                                ListPrice_MA2_factor + DiscountPercent_MA1_factor + DiscountPercent_MA2_factor, 
                              data = CameraAccessory_model)
summary(cameraAccessory_model_2)
vif(cameraAccessory_model_2)

cameraAccessory_model_3 <- lm(formula = GMV ~ List_Price + Product_MRP + Units + SLA + Discount_Percent + 
                                Product_Procurement_SLA + NPS.Score + PremiumProducts + MassProducts + 
                                TV + Digital + Sponsorship + Online.marketing + Affiliates + 
                                List_Price.1 + List_Price.3 + Discount_Percent.1 + Discount_Percent.2 + 
                                ContentMarketing_adstock + OnlineMarketing_adstock, 
                              data = CameraAccessory_model)
summary(cameraAccessory_model_3)
vif(cameraAccessory_model_3)

cameraAccessory_model_4 <- lm(formula = GMV ~ List_Price + Product_MRP + Units + SLA + Discount_Percent + 
                                Product_Procurement_SLA + NPS.Score + PremiumProducts + MassProducts + 
                                TV + Digital + Sponsorship + Online.marketing + 
                                List_Price.1 + List_Price.3 + Discount_Percent.1 + Discount_Percent.2 + 
                                ContentMarketing_adstock + OnlineMarketing_adstock, 
                              data = CameraAccessory_model)
summary(cameraAccessory_model_4)
vif(cameraAccessory_model_4)

# Model Building: Home Audio
HomeAudio_model <- data.frame(scale(HomeAudio_final[,-1]))

# first Model
HomeAudio_model_1 <- lm(GMV~.,HomeAudio_model)

#Adjusted R-squared:  0.608 
summary(HomeAudio_model_1)

# Model Building: Gaming Accessories
GameAccessory_model <- data.frame(scale(GamingAccessory_final[,-1]))

# first Model
GameAccessory_model_1 <- lm(GMV~.,GameAccessory_model)

# Adjusted R-squared:  0.604 
summary(GameAccessory_model_1)

