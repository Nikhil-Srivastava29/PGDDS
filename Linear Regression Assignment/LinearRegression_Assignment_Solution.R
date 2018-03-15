# Load essential libraries
library(ggplot2)
library(MASS)
library(car)
library(tidyr)
library(corrplot)


# load the CarPrice_Assignment data
car <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)
str(car)

# checking for duplicates and NA
sum(which(duplicated(car))) #there are no duplicates
sum(which(is.na(car))) # there are no NA's

#removing ID column as it is not the dependant variable
car <- car[,-1]

#Splitting variable carName for car company and car model
car<-separate(car, CarName, into=c("carcompany"), sep = "[ ]")

#Checking for name issues in drivewheel and replacing 4wd with fwd
car$drivewheel[which(car$drivewheel=="4wd")]<-"fwd"

#Checking for name issues in carcompany and replacing
car$carcompany[which(car$carcompany=="toyouta")]<-"toyota"
car$carcompany[which(car$carcompany=="maxda")]<-"mazda"
car$carcompany[which(car$carcompany=="Nissan")]<-"nissan"
car$carcompany[which(car$carcompany=="vokswagen")]<-"volkswagen"
car$carcompany[which(car$carcompany=="vw")]<-"volkswagen"

# Converting cylindernumber to numeric
car$cylindernumber[which(car$cylindernumber=="eight")]<-8
car$cylindernumber[which(car$cylindernumber=="five")]<-5
car$cylindernumber[which(car$cylindernumber=="four")]<-4
car$cylindernumber[which(car$cylindernumber=="six")]<-6
car$cylindernumber[which(car$cylindernumber=="three")]<-3
car$cylindernumber[which(car$cylindernumber=="twelve")]<-12
car$cylindernumber[which(car$cylindernumber=="two")]<-2

car$cylindernumber <- as.numeric(car$cylindernumber)


# Converting doornumber to numeric
car$doornumber[which(car$doornumber=="two")]<-2
car$doornumber[which(car$doornumber=="four")]<-4

car$doornumber <- as.numeric(car$doornumber)

#Converting categorical variables as factors
car$symboling <- as.factor(car$symboling)
car$carcompany <-as.factor(car$carcompany)
car$fueltype <- as.factor(car$fueltype)
car$aspiration <- as.factor(car$aspiration)
car$carbody <- as.factor(car$carbody)
car$drivewheel <- as.factor(car$drivewheel)
car$enginelocation <- as.factor(car$enginelocation)
car$enginetype <- as.factor(car$enginetype)
car$fuelsystem <- as.factor(car$fuelsystem)


#Converting enginelocation,fueltype,aspiration,doornumber into numeric variable
# Engine Location => Front=1 ; Rear=0
levels(car$enginelocation)<-c(1,0) 
car$enginelocation <- as.numeric(levels(car$enginelocation))[car$enginelocation]

# fueltype => diesel=1 ; gas=0
levels(car$fueltype)<-c(1,0) 
car$fueltype <- as.numeric(levels(car$fueltype))[car$fueltype]

# aspiration => std=1 ; turbo=0
levels(car$aspiration)<-c(1,0) 
car$aspiration <- as.numeric(levels(car$aspiration))[car$aspiration]

# drivewheel => fwd=1 ; rwd=0
levels(car$drivewheel)<-c(1,0) 
car$drivewheel <- as.numeric(levels(car$drivewheel))[car$drivewheel]

#Making positive values as risky and negative and zero as safe. Converting to numeric after conversion
####### need to check##########
levels(car$symboling)[1:3]<- "safe"
levels(car$symboling)[2:4]<- "risky"
levels(car$symboling)<-c(1,0) 
car$symboling <- as.numeric(levels(car$symboling))[car$symboling]

# ===============Checking for outliers====================
#Variable - wheelbase - suggests no outliers
quantile(car$wheelbase,seq(0,1,0.01)) 

#Variable - carlength	- suggests no outliers	
quantile(car$carlength,seq(0,1,0.01)) 

#Variable - carwidth	- suggests no outliers	
quantile(car$carwidth,seq(0,1,0.01))

#Variable - carheight	- suggests no outliers	
quantile(car$carheight,seq(0,1,0.01))

#Variable - curbweight	- jump between 93% and 94%		
quantile(car$curbweight,seq(0,1,0.01))
car$curbweight[which(car$curbweight>3376.08)]<-3376.08


#Variable - enginesize	- jump between 93% and 94%	
quantile(car$enginesize,seq(0,1,0.01))
car$enginesize[which(car$enginesize>183.00)]<-183.00

#Variable - boreratio	- suggest no outliers	
quantile(car$boreratio,seq(0,1,0.01))

#Variable - stroke	- suggest no outliers	
quantile(car$stroke,seq(0,1,0.01))

#Variable - compressionratio	- jump between 90% and 91%	
quantile(car$compressionratio,seq(0,1,0.01))		
car$compressionratio[which(car$compressionratio>10.9400)]<-10.9400

#Variable - horsepower	- jump between 93% and 94%	
quantile(car$horsepower,seq(0,1,0.01))		
car$horsepower[which(car$horsepower>162.00)]<-162.00

#Variable - peakrpm	- suggest no outlier	
quantile(car$peakrpm,seq(0,1,0.01))		

#Variable - citympg	- jump between 98% and 99%	
quantile(car$citympg,seq(0,1,0.01))		
car$citympg[which(car$citympg>38.00)]<-38.00

#Variable - highwaympg	- jump between 96% and 97%		
quantile(car$highwaympg,seq(0,1,0.01))		
car$highwaympg[which(car$highwaympg>43.00)]<-43.00

##########Creating Dummy variables for Factors #############

# Dummy variable for carcompany
dummy_carcompany <- data.frame(model.matrix( ~carcompany, data = car))
dummy_carcompany <- dummy_carcompany[,-1]
car <- cbind(car[,-2],dummy_carcompany)

# Dummy variable for carbody
dummy_carbody <- data.frame(model.matrix( ~carbody, data = car))
dummy_carbody <- dummy_carbody[,-1]
car <- cbind(car[,-5],dummy_carbody)

# Dummy variable for enginetype
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = car))
dummy_enginetype <- dummy_enginetype[,-1]
car <- cbind(car[,-12],dummy_enginetype)

# Dummy variable for fuelsystem
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = car))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
car <- cbind(car[,-14],dummy_fuelsystem)

##### Data Preparation step is complete ######

# plotting histogram for price
ggplot(car,aes(price))+geom_histogram()

##############

# Divide DataSet into training and test data set
#setting the seed to 100 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car), 0.7*nrow(car))
# generate the train data set
train = car[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car[-trainindices,]

### Finding coorelation matrix for Dependant variable - Price 
cor_matrix <- round(cor(car),2)
png(height=3000, width=3000, pointsize=30, file="Correlation_Plot.png")
color <- colorRampPalette(c("red","yellow","green","grey","pink","black","cyan", "blue")) 
cor_plot <- corrplot(cor_matrix, na.label="NA",method="color",col=color(8),
                     number.cex = 1, order="hclust",title = "Correalation Matrix")
cor_plot
dev.off()

#First Model execution
model_1 <-lm(price~.,data=car)
# Checking summary for model_1. NA values suggest of linear dependiencies among variables.
summary(model_1)

#performing StepAIC
step <- stepAIC(model_1)
step

model_2 <- lm(formula = price ~ symboling + fueltype + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                cylindernumber + enginesize + boreratio + stroke + compressionratio + 
                peakrpm + highwaympg + carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + enginetypedohcv + enginetypel + enginetyperotor + 
                fuelsystem2bbl + fuelsystem4bbl, data = car)


summary(model_2)
vif(model_2)
# ####Model 2 Value####
#Multiple R-squared:  0.9555,	Adjusted R-squared:  0.9433 
#F-statistic: 78.08 on 44 and 160 DF,  p-value: < 2.2e-16

#curbweight,carcompanypeugeot has high vif but is signficant: cannot be removed
# removing enginesize - high VIF but insignificant
model_3 <- lm(formula = price ~ symboling + fueltype + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                cylindernumber + boreratio + stroke + compressionratio + 
                peakrpm + highwaympg + carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + enginetypedohcv + enginetypel + enginetyperotor + 
                fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_3)
vif(model_3)

#removing carlength - high vif and insignificant
model_4 <- lm(formula = price ~ symboling + fueltype + enginelocation + 
                wheelbase + carwidth + carheight + curbweight + 
                cylindernumber + boreratio + stroke + compressionratio + 
                peakrpm + highwaympg + carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + enginetypedohcv + enginetypel + enginetyperotor + 
                fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_4)
vif(model_4)

# Removing enginetypel - high vif and High p value
model_5 <- lm(formula = price ~ symboling + fueltype + enginelocation + 
                wheelbase + carwidth + carheight + curbweight + 
                cylindernumber + boreratio + stroke + compressionratio + 
                peakrpm + highwaympg + carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + enginetypedohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_5)
vif(model_5)

# removing highwaympg - high vif and High p value
model_6 <- lm(formula = price ~ symboling + fueltype + enginelocation + 
                wheelbase + carwidth + carheight + curbweight + 
                cylindernumber + boreratio + stroke + compressionratio + 
                peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + enginetypedohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_6)
vif(model_6)

# removing carbodysedan - high vif and High p value
model_7 <- lm(formula = price ~ symboling + fueltype + enginelocation + 
                wheelbase + carwidth + carheight + curbweight + 
                cylindernumber + boreratio + stroke + compressionratio + 
                peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + enginetypedohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_7)
vif(model_7)

#removing carwidth - high vif and High p value
model_8 <- lm(formula = price ~ symboling + fueltype + enginelocation + 
                wheelbase + carheight + curbweight + 
                cylindernumber + boreratio + stroke + compressionratio + 
                peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + enginetypedohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_8)
vif(model_8)

# removing fueltype - high vif and High p value
model_9 <- lm(formula = price ~ symboling + enginelocation + 
                wheelbase + carheight + curbweight + 
                cylindernumber + boreratio + stroke + compressionratio + 
                peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + enginetypedohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_9)
vif(model_9)

#removing enginetyperotor - high vif and High p value
model_10 <- lm(formula = price ~ symboling + enginelocation + 
                wheelbase + carheight + curbweight + 
                cylindernumber + boreratio + stroke + compressionratio + 
                peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + enginetypedohcv + fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_10)
vif(model_10)

# removing boreratio - high vif and High p value
model_11 <- lm(formula = price ~ symboling + enginelocation + 
                 wheelbase + carheight + curbweight + 
                 cylindernumber + stroke + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                 carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                 carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_11)
vif(model_11)

#removing carcompanyhonda - high vif and High p value
model_12 <- lm(formula = price ~ symboling + enginelocation + 
                 wheelbase + carheight + curbweight + 
                 cylindernumber + stroke + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyisuzu + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                 carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_12)
vif(model_12)

#removing carheight - - high vif and High p value
model_13 <- lm(formula = price ~ symboling + enginelocation + 
                 wheelbase + curbweight + 
                 cylindernumber + stroke + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyisuzu + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                 carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_13)
vif(model_13)

#removing wheelbase - high vif and High p value
model_14 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + stroke + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyisuzu + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                 carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem2bbl + fuelsystem4bbl, data = car)

summary(model_14)
vif(model_14)

#removing fuelsystem2bbl - High p value
model_15 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + stroke + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyisuzu + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                 carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_15)
vif(model_15)

#remove carcompanytoyota - High p value
model_16 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + stroke + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyisuzu + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanyvolkswagen + 
                 carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_16)
vif(model_16)

#removing stroke
model_17 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyisuzu + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanyvolkswagen + 
                 carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_17)
vif(model_17)

# removing carcompanyisuzu since p value is highest
model_18 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanyvolkswagen + 
                 carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_18)
vif(model_18)

#removing carcompanyvolkswagen -> high p value
model_19 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_19)
vif(model_19)

# removing carbodyhardtop => high p value
model_20 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanyvolvo + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_20)
vif(model_20)

# removing carcompanychevrolet => highest p value
model_21 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 + carcompanydodge + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault + 
                 carcompanysubaru + carcompanyvolvo + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_21)
vif(model_21)

#removing carcompanysubaru => highest p value:
model_22 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyjaguar + carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault +carcompanyvolvo + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_22)
vif(model_22)

# removing carcompanymercury => highest p value
model_23 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyjaguar + carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                 carcompanyplymouth + carcompanyporsche + carcompanyrenault +carcompanyvolvo + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_23)
vif(model_23)

# removing carcompanypeugeot => highest p value
model_24 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyjaguar + carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + carcompanyporsche + 
                 carcompanyrenault +carcompanyvolvo + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_24)
vif(model_24)

#removing carcompanyrenault
model_25 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyjaguar + carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + carcompanyporsche + 
                 carcompanyvolvo + carbodyhatchback + 
                 carbodywagon + enginetypedohcv + fuelsystem4bbl, data = car)

summary(model_25)
vif(model_25)

# removing enginetypedohcv
model_26 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyjaguar + carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + carcompanyporsche + 
                 carcompanyvolvo + carbodyhatchback + 
                 carbodywagon + fuelsystem4bbl, data = car)

summary(model_26)
vif(model_26)

# removing carcompanymazda => high p value
model_27 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyjaguar + carcompanymitsubishi + carcompanynissan +
                 carcompanyplymouth + carcompanyporsche + 
                 carcompanyvolvo + carbodyhatchback + 
                 carbodywagon + fuelsystem4bbl, data = car)

summary(model_27)
vif(model_27)

# removing carbodyhatchback => high p value
model_28 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyjaguar + carcompanymitsubishi + carcompanynissan +
                 carcompanyplymouth + carcompanyporsche + 
                 carcompanyvolvo + carbodywagon + fuelsystem4bbl, data = car)

summary(model_28)
vif(model_28)

# removing carcompanynissan => high p value
model_29 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyjaguar + carcompanymitsubishi + 
                 carcompanyplymouth + carcompanyporsche + 
                 carcompanyvolvo + carbodywagon + fuelsystem4bbl, data = car)

summary(model_29)
vif(model_29)

# removing carcompanydodge => high p value

model_30 <- lm(formula = price ~ symboling + enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanymitsubishi + 
                 carcompanyplymouth + carcompanyporsche + 
                 carcompanyvolvo + carbodywagon + fuelsystem4bbl, data = car)

summary(model_30)
vif(model_30)

# removing symboling => high p value
model_31 <- lm(formula = price ~ enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanymitsubishi + 
                 carcompanyplymouth + carcompanyporsche + 
                 carcompanyvolvo + carbodywagon + fuelsystem4bbl, data = car)

summary(model_31)
vif(model_31)

# removing carcompanyplymouth => high p value
model_32 <- lm(formula = price ~ enginelocation + curbweight + 
                 cylindernumber + compressionratio + 
                 peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanymitsubishi +carcompanyporsche + 
                 carcompanyvolvo + carbodywagon + fuelsystem4bbl, data = car)

summary(model_32)
vif(model_32)

# removing compressionratio
model_33 <- lm(formula = price ~ enginelocation + curbweight + 
                 cylindernumber + peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanymitsubishi +carcompanyporsche + 
                 carcompanyvolvo + carbodywagon + fuelsystem4bbl, data = car)

summary(model_33)
vif(model_33)

# removing carcompanymitsubishi
model_34 <- lm(formula = price ~ enginelocation + curbweight + 
                 cylindernumber + peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanyporsche + 
                 carcompanyvolvo + carbodywagon + fuelsystem4bbl, data = car)

summary(model_34)
vif(model_34)

#removing fuelsystem4bbl
model_35 <- lm(formula = price ~ enginelocation + curbweight + 
                 cylindernumber + peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanyporsche + 
                 carcompanyvolvo + carbodywagon, data = car)

summary(model_35)
vif(model_35)

#removing carcompanyvolvo
model_36 <- lm(formula = price ~ enginelocation + curbweight + 
                 cylindernumber + peakrpm + carcompanyaudi + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanyporsche + carbodywagon, data = car)

summary(model_36)
vif(model_36)

# removing carcompanyaudi
model_37 <- lm(formula = price ~ enginelocation + curbweight + 
                 cylindernumber + peakrpm + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanyporsche + carbodywagon, data = car)

summary(model_37)
#Adjusted R-squared:  0.9103
# No linear Coorelation
vif(model_37)

# adding fueltype to check its effect
model_38 <- lm(formula = price ~ enginelocation + curbweight + fueltype +
                 cylindernumber + peakrpm + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanyporsche + carbodywagon, data = car)

summary(model_38)
#There is a decrease in Adjusted R-squared:  0.9092. P value shows insignificance
## Not considering fueltype as part of final model
vif(model_38)

# adding compressionratio to check its effect
model_39 <- lm(formula = price ~ enginelocation + curbweight + compressionratio +
                 cylindernumber + peakrpm + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanyporsche + carbodywagon, data = car)

summary(model_39)
#There is a decrease in Adjusted R-squared:  0.9094. P value shows insignificance
# Not considering compressionratio as part of final model
vif(model_39)

# adding carbodyhatchback to check its effect
model_40 <- lm(formula = price ~ enginelocation + curbweight + carbodyhatchback +
                 cylindernumber + peakrpm + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanyporsche + carbodywagon, data = car)

summary(model_40)
vif(model_40)

# adding enginetypel to check its effect
model_41 <- lm(formula = price ~ enginelocation + curbweight + enginetypel +
                 cylindernumber + peakrpm + carcompanybmw + carcompanybuick + 
                 carcompanyjaguar + carcompanyporsche + carbodywagon, data = car)

summary(model_41)
vif(model_41)
#There is a decrease in Adjusted R-squared:  0.9097. P value shows insignificance
## Not considering enginetypel as part of final model
# Retaining model_37 as final model

# predicting the results in test dataset
Predict_1 <- predict(model_37,test[,-1])
test$predicted_price <- Predict_1

# Now, we need to test the r square between actual and predicted value  . 
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared
#R-Square = 0.88

# plot Residuals vs Fitted
plot(model_37,pch=16,which=1)

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views
g <- ggplot(test, aes(x=test$carID))
g <- g + geom_line(aes(y=test$price), colour="red")
g + geom_line(aes(y=test$predicted_price), colour="blue")


########################################################################
          ##############     CONCLUSION       ################
########################################################################
#
# Final models shows the below independant varilables for dependant varible "Price"
#     1. enginelocation (Front or Rear)
#     2. curbweight
#     3. cylindernumber (all clyinder numbers irrespective of number of cylinders)
#     4. peakrpm 
#     5. cars belonging to the below company(s):
#             a. BMW
#             b. Buick
#             c. Jaguar
#             d. Porsche
#     6. carbody type = Wagon
#
# Considerations:
#   1. Positive Corelations:
#         a. Increase in 1 unit of curbweight increases the price by 9.430e+00
#         b. Increase in cylindernumber by 1, increases the price by 8.438e+02
#         c. Increase in peakrpm by 1 unit increases the price by 1.708e+00
#         d. If the Car type is BMW, the price increases by 9.506e+03
#         e. If the Car type is Buick, the price increases by 1.309e+04
#         f. If the Car type is Jaguar, the price increases by 1.191e+04
#         g. If the Car type is Porsche, the price increases by 6.116e+03
#   2. Negative Corelations:
#         a. If the engine is located in front, price decreases by 1.334e+04
#         b. If the carbody type is Wagon, price decreases by 2.200e+03
#
#
# Predictor Model Equation:
# price = -1.081e+04 -1.334e+04*(enginelocation)+9.430e+00*(curbweight)+8.438e+02*(cylindernumber)
#             +1.708e+00*(peakrpm)+9.506e+03*(carcompanybmw)+1.309e+04*(carcompanybuick)
#             +1.191e+04*(carcompanyjaguar)+ 6.116e+03*(carcompanyporsche)-2.200e+03*(carbodywagon)
# 


########################        END          ####################################