#--------------------------XYZ Company----------------------------#
# -------------------------HR Analytics---------------------------- #

###############################################################
# Section 1: Business Understanding
# Section 2: Data Understanding
# Section 3: Data Preparation & EDA
# Section 4: Model Building 
# Section 5: Model Evaluation
################################################################

# SECTION 1: Business Understanding

# Total Number of employees 4000
# 15% leave every year - 600 employees in total

# Management beleives this level of attrition is bad for the company, because of the following reasons -
# 1. Projects get delayed, which makes it difficult to meet timelines, resulting in a reputation loss
# 2. A sizeable HR department has to be maintained for the purposes of recruiting new talent
# 3. the new employees have to be trained for the job and/or given time to acclimatise themselves to the company

# AIM of Case Study

# UNDERSTAND what factors they should focus on, in order to curb attrition
# 1. what changes they should make to their workplace so that most of their employees stay.
# 2. which of these variables is most important and needs to be addressed right away.

#----------------------------------------------------------------#

# Loading the required library 

library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library("MASS")
library(car)
library(caret)
library(ROCR)
library(Information)

#--------------------------------------------------#
# Loading csv files

emp_survey_data <- read.csv("./PA-I_Case_Study_HR_Analytics/employee_survey_data.csv",stringsAsFactors=F)
general_data <- read.csv("./PA-I_Case_Study_HR_Analytics/general_data.csv",stringsAsFactors=F)
in_time <- read.csv("./PA-I_Case_Study_HR_Analytics/in_time.csv",stringsAsFactors=F)
mgr_survey_data <- read.csv("./PA-I_Case_Study_HR_Analytics/manager_survey_data.csv",stringsAsFactors=F)
out_time <- read.csv("./PA-I_Case_Study_HR_Analytics/out_time.csv",stringsAsFactors=F)

#------------------------------------#
# Section 2: Data Understanding
#------------------------------------#
str(emp_survey_data)    # 4410 obs of 4 variables 
str(general_data) # 4410 obs of 24 variables
str(in_time) # 4410 obs of 262 variables. First column is missing column name
str(mgr_survey_data) # 4410 obs of 3 variables
str(out_time) # 4410 obs of 262 variables. First column is missing column name

#------------------------------------#
# Section 3: Data Preparation & EDA

# Section 3.1: Data Preparation
#------------------------------------#

# Adding column names for the first column for in_time and out_time data set 
# (Assuming first column is EmployeeID in both data sets)

colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"
setdiff(in_time$EmployeeID,out_time$EmployeeID) # Identical EmployeeID across these In-Time & Out-Time datasets


length(unique(tolower(emp_survey_data$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(general_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(in_time$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(mgr_survey_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(out_time$EmployeeID))) # 4410, confirming EmployeeID is key


setdiff(emp_survey_data$EmployeeID,general_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,in_time$EmployeeID) # Identical EmployeeID across these datasets
setdiff(in_time$EmployeeID,mgr_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(mgr_survey_data$EmployeeID,out_time$EmployeeID) # Identical EmployeeID across these datasets

# in_time and out_time has same column names hence  trying to add differentiators by appending IN_ for in-time
# and OUT_ for out-time
cn <- gsub("([X])","IN_",colnames(in_time))
colnames(in_time) <-cn

cn <- gsub("([X])","OUT_",colnames(out_time))
colnames(out_time) <-cn

# Find the working hours of each Employee on daily basis based on in-out timing
emp_working_Hours <- in_time[1]
for(i in 2:262){
working_Hours <- round(as.numeric(difftime(strptime(out_time[,i],"%Y-%m-%d %H:%M:%S"),
                                     strptime(in_time[,i],"%Y-%m-%d %H:%M:%S"))),digits = 2)
emp_working_Hours <- cbind(emp_working_Hours,working_Hours)
}
cn <- gsub("(OUT_)","Workhrs_",cn)
colnames(emp_working_Hours)<-cn
summary(emp_working_Hours)

#Removing non working days from emp_working_hours data set (if all rows are NA for a day, day is non-working)
workhrs_na <- as.data.frame(sapply(emp_working_Hours, function(x) sum(is.na(x))))
workhrs_na_cols <- which(workhrs_na == 4410)
emp_working_Hours <- emp_working_Hours[,-workhrs_na_cols]

emp_working_Hours$AvgWorkingHrs <- apply(emp_working_Hours[,-1],1,function(x) mean(x,na.rm=TRUE))

emp_Avg_Hours <- emp_working_Hours[,c("EmployeeID","AvgWorkingHrs")]

#Merging all datasets in a single data frame
emp_df <- merge(emp_survey_data,general_data, by="EmployeeID")
emp_df <- merge(emp_df,mgr_survey_data,by="EmployeeID")
emp_df <- merge(emp_df,emp_Avg_Hours,by="EmployeeID")

# remove EmployeeCount, Over18 and StandardHours column since they hold same value for all rows.
unique(emp_df$EmployeeCount)
unique(emp_df$Over18)
unique(emp_df$StandardHours)
emp_df <- emp_df[,-c(12,19,21)]

View(emp_df) #master file
str(emp_df)
summary(emp_df)

################################################################

# Section 3.2: Exploratory Data Analysis

# Section 3.2.1 - Univariate Analysis

################################################################

# Checking Attrition rate of Employees

emp_df$Attrition <- as.factor(emp_df$Attrition)
summary(emp_df$Attrition)

Attrition_Rate <- length(which(emp_df$Attrition=="Yes"))/nrow(emp_df)
Attrition_Rate # 16.12% Attrition rate. 

bar_theme1<- theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(emp_df, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "dodge") +bar_theme1, 
          ggplot(emp_df, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=Department,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,  
          ggplot(emp_df, aes(x=Education,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          align = "h")

plot_grid(ggplot(emp_df, aes(x=EducationField,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=Gender,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=JobLevel,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=JobRole,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=factor(NumCompaniesWorked),fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          align = "h")

plot_grid(ggplot(emp_df, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          ggplot(emp_df, aes(x=as.factor(PerformanceRating),fill=Attrition))+ geom_bar(position = "dodge")+bar_theme1,
          align = "h")

# Histogram and Boxplots for numeric variables

plot_grid(ggplot(emp_df,aes(x=Age,fill=Attrition))+ geom_histogram(binwidth=10)+bar_theme1,
          ggplot(emp_df,aes(x=DistanceFromHome,fill=Attrition))+ geom_histogram(binwidth=5)+bar_theme1,
          ggplot(emp_df,aes(x=MonthlyIncome,fill=Attrition))+ geom_histogram(binwidth=10000)+bar_theme1,
          ggplot(emp_df,aes(x=PercentSalaryHike,fill=Attrition))+ geom_histogram(binwidth=5)+bar_theme1,
          ggplot(emp_df,aes(x=TotalWorkingYears,fill=Attrition))+ geom_histogram(binwidth=5)+bar_theme1,
          ggplot(emp_df,aes(x=YearsAtCompany,fill=Attrition))+ geom_histogram(binwidth=10)+bar_theme1,
          ggplot(emp_df,aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_histogram(binwidth=2)+bar_theme1,
          ggplot(emp_df,aes(x=YearsWithCurrManager,fill=Attrition))+ geom_histogram(binwidth=2)+bar_theme1,
          align = "h")

################################################################

# Section 3.2.2 - Segmented Analysis for Attrition="Yes"

################################################################

attr_emp_df<-subset(emp_df,Attrition=="Yes")
plot_grid(ggplot(attr_emp_df, aes(x=EnvironmentSatisfaction))+geom_bar(fill="light blue")+bar_theme1, 
          ggplot(attr_emp_df, aes(x=JobSatisfaction))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=WorkLifeBalance))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=BusinessTravel))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=Department))+geom_bar(fill="light blue")+bar_theme1,  
          ggplot(attr_emp_df, aes(x=Education))+geom_bar(fill="light blue")+bar_theme1,
          align = "h")

plot_grid(ggplot(attr_emp_df, aes(x=EducationField))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=Gender))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=JobLevel))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=JobRole))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=MaritalStatus))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=factor(NumCompaniesWorked)))+geom_bar(fill="light blue")+bar_theme1,
          align = "h")

plot_grid(ggplot(attr_emp_df, aes(x=StockOptionLevel))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=TrainingTimesLastYear))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=JobInvolvement))+geom_bar(fill="light blue")+bar_theme1,
          ggplot(attr_emp_df, aes(x=as.factor(PerformanceRating)))+geom_bar(fill="light blue")+bar_theme1,
          align = "h")

# Histogram and Boxplots for numeric variables

plot_grid(ggplot(attr_emp_df,aes(x=Age))+ geom_histogram(binwidth=10)+bar_theme1,
          ggplot(attr_emp_df,aes(x=DistanceFromHome))+ geom_histogram(binwidth=5)+bar_theme1,
          ggplot(attr_emp_df,aes(x=MonthlyIncome))+ geom_histogram(binwidth=10000)+bar_theme1,
          ggplot(attr_emp_df,aes(x=PercentSalaryHike))+ geom_histogram(binwidth=5)+bar_theme1,
          ggplot(attr_emp_df,aes(x=TotalWorkingYears))+ geom_histogram(binwidth=5)+bar_theme1,
          ggplot(attr_emp_df,aes(x=YearsAtCompany))+ geom_histogram(binwidth=10)+bar_theme1,
          ggplot(attr_emp_df,aes(x=YearsSinceLastPromotion))+ geom_histogram(binwidth=2)+bar_theme1,
          ggplot(attr_emp_df,aes(x=YearsWithCurrManager))+ geom_histogram(binwidth=2)+bar_theme1,
          align = "h")

################################################################

# Section 3.2.3 - Analysis on NA values

################################################################

sapply(emp_df, function(x) sum(is.na(x)))
#   Variable                    count of NA's
#   EnvironmentSatisfaction         25
#   JobSatisfaction                 20
#   WorkLifeBalance                 38
#   NumCompaniesWorked              19
#   TotalWorkingYears               9

#-------------------------------------------------------#

########################## Missing Value Imputation ##########################

#------------------------------------------------------#

# assign numeric levels 1 and 0 to Attrition column
levels(emp_df$Attrition) <-c(0,1)
emp_df$Attrition <- as.numeric(levels(emp_df$Attrition))[emp_df$Attrition]

# scale MonthlyIncome
emp_df$MonthlyIncome <- scale(emp_df$MonthlyIncome)

# create WOE and IV table for all variables
WOE_IV <- create_infotables(emp_df[,-1], y="Attrition", bins=10, parallel=FALSE)

# 1. Missing Value handling - Variable "EnvironmentSatisfaction"
WOE_IV$Tables$EnvironmentSatisfaction

#EnvironmentSatisfaction    N     Percent         WOE           IV
#1                      NA   25 0.005668934  0.26285100 0.0004272596
#2                   [1,1]  845 0.191609977  0.56154813 0.0727103366
#3                   [2,2]  856 0.194104308 -0.08912542 0.0742060233
#4                   [3,3] 1350 0.306122449 -0.18472559 0.0840105683
#5                   [4,4] 1334 0.302494331 -0.21532446 0.0970352143


# using WOE values for EnvironmentSatisfaction
emp_df$EnvironmentSatisfaction <- as.factor(emp_df$EnvironmentSatisfaction)
levels(emp_df$EnvironmentSatisfaction) <-c(.56,-.09,-.19,-.22)
emp_df$EnvironmentSatisfaction <- as.numeric(levels(emp_df$EnvironmentSatisfaction))[emp_df$EnvironmentSatisfaction]

# replace missing values with WoE for NA
emp_df$EnvironmentSatisfaction[which(is.na(emp_df$EnvironmentSatisfaction))] <- 0.26

###########
# 2. Missing Value handling - Variable "JobSatisfaction"
###########
WOE_IV$Tables$JobSatisfaction

#JobSatisfaction    N     Percent         WOE          IV
#1              NA   20 0.004535147 -1.29529362 0.004831515
#2           [1,1]  860 0.195011338  0.43557410 0.047446739
#3           [2,2]  840 0.190476190  0.02246564 0.047543607
#4           [3,3] 1323 0.300000000  0.03152187 0.047844887
#5           [4,4] 1367 0.309977324 -0.40020037 0.091057122

# WOE is not monotonic. Need to perform coarse classing by combining value of 2 and 3 in one category

#Creating a generic function to compute WOE
total_good <- length(emp_df$Attrition[which(emp_df$Attrition == 1)])
total_bad <- length(emp_df$Attrition[which(emp_df$Attrition == 0)])
computeWoE <- function(local_good, local_bad){
 woe = log(local_good/total_good) - log(local_bad/total_bad)
  return(woe)
}

# Performing coarse classing:
job_satisfaction_comb <- subset(emp_df,(JobSatisfaction==2 | JobSatisfaction==3))
comb_good <- length(job_satisfaction_comb$Attrition[which(job_satisfaction_comb$Attrition==1)])
comb_bad <- length(job_satisfaction_comb$Attrition[which(job_satisfaction_comb$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 2 and 3 === .028

# using WOE values for JobSatisfaction
emp_df$JobSatisfaction <- as.factor(emp_df$JobSatisfaction)
levels(emp_df$JobSatisfaction) <-c(.43,.28,.28,-.40)
emp_df$JobSatisfaction <- as.numeric(levels(emp_df$JobSatisfaction))[emp_df$JobSatisfaction]

# replace missing values with WoE for NA
emp_df$JobSatisfaction[which(is.na(emp_df$JobSatisfaction))] <- -1.29

#############
# 3. Missing Value handling - Variable "WorkLifeBalance"
#############
WOE_IV$Tables$WorkLifeBalance

# WorkLifeBalance    N    Percent         WOE          IV
#1              NA   38 0.00861678 -0.49092080 0.001750523
#2           [1,1]  239 0.05419501  0.86676705 0.054752396
#3           [2,2] 1019 0.23106576  0.04792828 0.055291837
#4           [3,3] 2660 0.60317460 -0.14261411 0.066975398
#5           [4,4]  454 0.10294785  0.12201610 0.068572099

# WOE is not monotonic. Need to perform coarse classing by combining value of 3 and 4 in one category

emp_df$WorkLifeBalance <- as.factor(emp_df$WorkLifeBalance)

# Performing coarse classing:
worklife_bal_comb <- subset(emp_df,(WorkLifeBalance==3 | WorkLifeBalance==4))
comb_good <- length(worklife_bal_comb$Attrition[which(worklife_bal_comb$Attrition==1)])
comb_bad <- length(worklife_bal_comb$Attrition[which(worklife_bal_comb$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 2 and 3 === -0.10

# using WOE values for WorkLifeBalance
levels(emp_df$WorkLifeBalance) <-c(.86,.04,-.10,-.10)
emp_df$WorkLifeBalance <- as.numeric(levels(emp_df$WorkLifeBalance))[emp_df$WorkLifeBalance]

# replace missing values with WoE for NA
emp_df$WorkLifeBalance[which(is.na(emp_df$WorkLifeBalance))] <- -.10


###########
# 4. Missing Value handling - Variable "NumCompaniesWorked"
###########
WOE_IV$Tables$NumCompaniesWorked

#  NumCompaniesWorked    N    Percent        WOE           IV
#1                 NA   19 0.00430839  0.3273895 0.0005142402
#2              [0,0]  586 0.13287982 -0.3647910 0.0160985101
#3              [1,1] 1558 0.35328798  0.1864906 0.0291736101
#4              [2,2]  438 0.09931973 -0.4458004 0.0460798546
#5              [3,3]  474 0.10748299 -0.5340930 0.0715324358
#6              [4,4]  415 0.09410431 -0.3387290 0.0811360223
#7              [5,6]  395 0.08956916  0.4853299 0.1058076762
#8              [7,9]  525 0.11904762  0.2628510 0.1147801276
# WOE is not monotonic. Need to perform coarse classing 

# Performing coarse classing by combining value of 1,2,3,4 in one category
num_companies_1234 <- subset(emp_df,(NumCompaniesWorked==1 | NumCompaniesWorked==2 |
                                     NumCompaniesWorked==3| NumCompaniesWorked==4))
comb_good <- length(num_companies_1234$Attrition[which(num_companies_1234$Attrition==1)])
comb_bad <- length(num_companies_1234$Attrition[which(num_companies_1234$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE  === -.07
# Performing coarse classing by combining value of 5,6,8,9 in one category
num_companies_56789 <- subset(emp_df,(NumCompaniesWorked==5 | NumCompaniesWorked==6 |
                                     NumCompaniesWorked==7| NumCompaniesWorked==8 | NumCompaniesWorked==9))
comb_good <- length(num_companies_56789$Attrition[which(num_companies_56789$Attrition==1)])
comb_bad <- length(num_companies_56789$Attrition[which(num_companies_56789$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE  === .36

# using WOE values for NumCompaniesWorked
emp_df$NumCompaniesWorked <- as.factor(emp_df$NumCompaniesWorked)
levels(emp_df$NumCompaniesWorked) <-c(-.36,-.07,-.07,-.07,-.07,-.36,.36,.36,.36,.36)
emp_df$NumCompaniesWorked <- as.numeric(levels(emp_df$NumCompaniesWorked))[emp_df$NumCompaniesWorked]

# replace missing values with WoE for NA
emp_df$NumCompaniesWorked[which(is.na(emp_df$NumCompaniesWorked))] <- .33

#############
# 5. Missing Value handling - Variable "TotalWorkingYears"
#############
WOE_IV$Tables$TotalWorkingYears


#TotalWorkingYears   N     Percent        WOE           IV
#1                 NA   9 0.002040816  0.3963824 0.0003648843
#2              [0,2] 368 0.083446712  1.3978309 0.2386678860
#3              [3,4] 315 0.071428571  0.2628510 0.2440513569
#4              [5,5] 264 0.059863946  0.1450680 0.2453738527
#5              [6,7] 618 0.140136054  0.2260370 0.2530919752
#6              [8,9] 594 0.134693878 -0.2550921 0.2611195532
#7            [10,12] 855 0.193877551 -0.2533655 0.2725253823
#8            [13,16] 432 0.097959184 -0.5026168 0.2932994067
#9            [17,22] 499 0.113151927 -0.6622893 0.3326690216
#10           [23,40] 456 0.103401361 -0.7203792 0.3743651832

# Total NA's are 0.2% of total Records. Hence removing the records.
emp_df <- emp_df[-(which(is.na(emp_df$TotalWorkingYears))),]

# WOE is not monotonic. Need to perform coarse classing by combining value of 5,6,7 in one category for binning

# Performing coarse classing:
total_ex_comb <- subset(emp_df,(TotalWorkingYears==5 | TotalWorkingYears==6 | TotalWorkingYears==7))
comb_good <- length(total_ex_comb$Attrition[which(total_ex_comb$Attrition==1)])
comb_bad <- length(total_ex_comb$Attrition[which(total_ex_comb$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 5,6 and 7 === .20

# Performing Binning for TotalWorkingYears
emp_df$TotalWorkingYears <-as.numeric(emp_df$TotalWorkingYears)
emp_df$TotalWorkExBins<-""
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears>= 0 & emp_df$TotalWorkingYears<= 2)]<-'0-2'
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears>= 3 & emp_df$TotalWorkingYears<= 4)]<-'3-4'
#combining 5,6,7 since the WOE is monotonic
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears>= 5 & emp_df$TotalWorkingYears<= 7)]<-'5-7'
#combining 8,9,10,11,12 as WOE is almost same
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears>= 8 & emp_df$TotalWorkingYears<= 12)]<-'8-12'
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears>= 13 & emp_df$TotalWorkingYears<= 16)]<-'13-16'
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears>= 17 & emp_df$TotalWorkingYears<= 22)]<-'17-22'
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears>= 23 & emp_df$TotalWorkingYears<= 40)]<-'23-40'

summary(as.factor(emp_df$TotalWorkExBins))
#0-2 13-16 17-22 23-40   3-4   5-7  8-12 
#368   432   499   456   315   882  1449 


#################################################################

# -------------------Handling Outliers & Binning---------------

#################################################################

## ---------------------------------------------------------------- ##

# Variable "PercentSalaryHike" - Checking for Outliers and Binning

## ---------------------------------------------------------------- ##

emp_df$PercentSalaryHike <- as.numeric(emp_df$PercentSalaryHike)
quantile(emp_df$PercentSalaryHike,seq(0,1,.01))
# no outliers detected

WOE_IV$Tables$PercentSalaryHike

#PercentSalaryHike   N    Percent         WOE          IV
#1           [11,11] 630 0.14285714 -0.14261411 0.002767159
#2           [12,12] 594 0.13469388 -0.07362124 0.003479133
#3           [13,13] 627 0.14217687  0.01071991 0.003495530
#4           [14,14] 603 0.13673469 -0.13105328 0.005741036
#5           [15,16] 537 0.12176871  0.08592029 0.006666333
#6           [17,18] 513 0.11632653  0.01850524 0.006706418
#7           [19,20] 393 0.08911565  0.10250835 0.007675652
#8           [21,25] 513 0.11632653  0.18040733 0.011696557

# WOE is not monotonic. Need to perform coarse classing by combining value of 13,14 in one category for binning

# Performing coarse classing for 13, 14:
hike_comb_13_14 <- subset(emp_df,(PercentSalaryHike==13 | PercentSalaryHike==14))
comb_good <- length(hike_comb_13_14$Attrition[which(hike_comb_13_14$Attrition==1)])
comb_bad <- length(hike_comb_13_14$Attrition[which(hike_comb_13_14$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 13,14 === -.06

# Performing coarse classing for 15, 16, 17, 18:
hike_comb_15_18 <- subset(emp_df,(PercentSalaryHike==15 | PercentSalaryHike==16 |
                                    PercentSalaryHike==17 | PercentSalaryHike==18))
comb_good <- length(hike_comb_15_18$Attrition[which(hike_comb_15_18$Attrition==1)])
comb_bad <- length(hike_comb_15_18$Attrition[which(hike_comb_15_18$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 15,16,17,18  === .05

# Performing Binning for PercentSalaryHike
emp_df$PercentSalaryHike <-as.numeric(emp_df$PercentSalaryHike)
emp_df$PercentHikeBins<-""
emp_df$PercentHikeBins[which(emp_df$PercentSalaryHike>= 11 & emp_df$PercentSalaryHike<= 12)]<-'11-12'
emp_df$PercentHikeBins[which(emp_df$PercentSalaryHike>= 13 & emp_df$PercentSalaryHike<= 14)]<-'13-14'
emp_df$PercentHikeBins[which(emp_df$PercentSalaryHike>= 15 & emp_df$PercentSalaryHike<= 18)]<-'15-18'
emp_df$PercentHikeBins[which(emp_df$PercentSalaryHike>= 19 & emp_df$PercentSalaryHike<= 20)]<-'19-20'
emp_df$PercentHikeBins[which(emp_df$PercentSalaryHike>= 21 & emp_df$PercentSalaryHike<= 25)]<-'21-25'

summary(as.factor(emp_df$PercentHikeBins))
# 11-12   13-14   15-18   19-20   21-25 
# 1223    1227    1047    393     511 


## ---------------------------------------------------------------- ##

# Variable "Age" - Checking for Outliers and Binning

## ---------------------------------------------------------------- ##

emp_df$Age <- as.numeric(emp_df$Age)
quantile(emp_df$Age,seq(0,1,.01))
# no outliers detected

WOE_IV$Tables$Age

#Age   N    Percent        WOE        IV
#1  [18,25] 369 0.08367347  1.0638871 0.1293502
#2  [26,28] 405 0.09183673  0.3530021 0.1421973
#3  [29,30] 384 0.08707483  0.3298617 0.1527561
#4  [31,33] 564 0.12789116  0.3722848 0.1727755
#5  [34,35] 465 0.10544218 -0.3190705 0.1823895
#6  [36,37] 357 0.08095238 -0.5387768 0.2018649
#7  [38,40] 471 0.10680272 -0.7557186 0.2486710
#8  [41,44] 453 0.10272109 -0.4835407 0.2689694
#9  [45,49] 423 0.09591837 -0.6379355 0.3002024
#10 [50,60] 519 0.11768707 -0.2259957 0.3057637

# WOE is not monotonic

# Performing coarse classing for 26-33:
age_comb_26_33 <- subset(emp_df,(Age>=26 & Age<=33))
comb_good <- length(age_comb_26_33$Attrition[which(age_comb_26_33$Attrition==1)])
comb_bad <- length(age_comb_26_33$Attrition[which(age_comb_26_33$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 26-33 === .35

# Performing coarse classing for 34-37:
temp_age <- subset(emp_df,(Age>=34 & Age<=37))
comb_good <- length(temp_age$Attrition[which(temp_age$Attrition==1)])
comb_bad <- length(temp_age$Attrition[which(temp_age$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 34-37 === -.41

# Performing coarse classing for 38-60:
age_comb_38_60 <- subset(emp_df,(Age>=38 & Age<=60))
comb_good <- length(age_comb_38_60$Attrition[which(age_comb_38_60$Attrition==1)])
comb_bad <- length(age_comb_38_60$Attrition[which(age_comb_38_60$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 38-60 === -.50

# Performing Binning for Age
emp_df$AgeBins<-""
emp_df$AgeBins[which(emp_df$Age>= 18 & emp_df$Age<= 25)]<-'18-25'
emp_df$AgeBins[which(emp_df$Age>= 26 & emp_df$Age<= 33)]<-'26-33'
emp_df$AgeBins[which(emp_df$Age>= 34 & emp_df$Age<= 37)]<-'34-37'
emp_df$AgeBins[which(emp_df$Age>= 38 & emp_df$Age<= 60)]<-'38-60'

summary(as.factor(emp_df$AgeBins))
# 18-25 26-33 34-37 38-60 
#  369  1352   819  1861 


## ---------------------------------------------------------------- ##

# Variable "DistanceFromHome" - Checking for Outliers and Binning

## ---------------------------------------------------------------- ##

emp_df$DistanceFromHome <- as.numeric(emp_df$DistanceFromHome)
quantile(emp_df$DistanceFromHome,seq(0,1,.01))
# no outliers detected

WOE_IV$Tables$DistanceFromHome


#DistanceFromHome   N    Percent         WOE           IV
#1            [1,1] 624 0.14149660 -0.05560273 0.0004292633
#2            [2,2] 633 0.14353741  0.13343993 0.0031019798
#3            [3,4] 444 0.10068027 -0.15051929 0.0052684365
#4            [5,6] 372 0.08435374 -0.19047013 0.0081350601
#5            [7,8] 492 0.11156463  0.02500130 0.0082053873
#6           [9,10] 513 0.11632653  0.18040733 0.0122262925
#7          [11,16] 441 0.10000000  0.11145135 0.0135157858
#8          [17,22] 390 0.08843537  0.11181074 0.0146636576
#9          [23,29] 501 0.11360544 -0.28993882 0.0233046423

# WOE is not monotonic

# Performing coarse classing for 1-2:
temp_dist <- subset(emp_df,(DistanceFromHome>=1 & DistanceFromHome<=2))
comb_good <- length(temp_dist$Attrition[which(temp_dist$Attrition==1)])
comb_bad <- length(temp_dist$Attrition[which(temp_dist$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 1-2 === .04

# Performing coarse classing for 3-10:
temp_dist <- subset(emp_df,(DistanceFromHome>=3 & DistanceFromHome<=10))
comb_good <- length(temp_dist$Attrition[which(temp_dist$Attrition==1)])
comb_bad <- length(temp_dist$Attrition[which(temp_dist$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 3-10 === -.01

# Performing coarse classing for 11 onwards:
temp_dist <- subset(emp_df,(DistanceFromHome>=11))
comb_good <- length(temp_dist$Attrition[which(temp_dist$Attrition==1)])
comb_bad <- length(temp_dist$Attrition[which(temp_dist$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 11-29 === -.02

# Performing Binning for DistanceFromHome
emp_df$DistancefromHomeBins<-""
emp_df$DistancefromHomeBins[which(emp_df$DistanceFromHome>= 1 & emp_df$DistanceFromHome<= 2)]<-'1-2'
emp_df$DistancefromHomeBins[which(emp_df$DistanceFromHome>= 3 & emp_df$DistanceFromHome<= 10)]<-'3-10'
emp_df$DistancefromHomeBins[which(emp_df$DistanceFromHome>= 11)]<-'11-29'

summary(as.factor(emp_df$DistancefromHomeBins))
#1-2  11-29  3-10 
#1255  1328  1818 

## ---------------------------------------------------------------- ##

# Variable "MonthlyIncome" - Checking for Outliers and Binning

## ---------------------------------------------------------------- ##

emp_df$MonthlyIncome <- as.numeric(emp_df$MonthlyIncome)
quantile(emp_df$MonthlyIncome,seq(0,1,.01))
# jump at 90% to 91%, replacing all greater than 1.54392188 with 1.54392188
emp_df$MonthlyIncome[which(emp_df$MonthlyIncome>1.54392188)] <- 1.54392188

WOE_IV$Tables$MonthlyIncome
#Scaled wOE
#     MonthlyIncome   N    Percent         WOE          IV
#1    [10090,23130] 438 0.09931973  0.11975016 0.001482632
#2    [23140,26940] 441 0.10000000 -0.03564199 0.001608138
#3    [26950,33100] 441 0.10000000  0.06400014 0.002026671
#4    [33120,42210] 441 0.10000000 -0.03564199 0.002152178
#5    [42270,49070] 441 0.10000000 -0.14261411 0.004089189
#6    [49080,57360] 441 0.10000000  0.24575657 0.010641386
#7    [57430,68770] 441 0.10000000  0.24575657 0.017193582
#8    [68830,98520] 441 0.10000000 -0.14261411 0.019130594
#9   [98540,137580] 441 0.10000000 -0.32029528 0.028314582
#10 [137700,199990] 444 0.10068027 -0.09609409 0.029214289

#MonthlyIncome   N    Percent         WOE          IV
#1  [-1.17,-0.89] 438 0.09931973  0.11975016 0.001482632
#2  [-0.89,-0.81] 441 0.10000000 -0.03564199 0.001608138
#3  [-0.81,-0.68] 441 0.10000000  0.06400014 0.002026671
#4  [-0.68,-0.48] 441 0.10000000 -0.03564199 0.002152178
#5  [-0.48,-0.34] 441 0.10000000 -0.14261411 0.004089189
#6  [-0.34,-0.16] 441 0.10000000  0.24575657 0.010641386
#7   [-0.16,0.08] 441 0.10000000  0.24575657 0.017193582
#8    [0.08,0.71] 441 0.10000000 -0.14261411 0.019130594
#9    [0.71,1.54] 441 0.10000000 -0.32029528 0.028314582
#10   [1.54,2.87] 444 0.10068027 -0.09609409 0.029214289

# WOE is not monotonic

# Performing coarse classing for 23140-68770:
temp_inc <- subset(emp_df,(MonthlyIncome>=-0.89 & MonthlyIncome<=0.08))
comb_good <- length(temp_inc$Attrition[which(temp_inc$Attrition==1)])
comb_bad <- length(temp_inc$Attrition[which(temp_inc$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 23140-68770 === .06

# Performing coarse classing for 98540-199990:
temp_inc <- subset(emp_df,(MonthlyIncome>=0.71 & MonthlyIncome<=2.87))
comb_good <- length(temp_inc$Attrition[which(temp_inc$Attrition==1)])
comb_bad <- length(temp_inc$Attrition[which(temp_inc$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 98540-199990 === -.20

# Performing Binning for MonthlyIncome
emp_df$MonthlyIncomeBins<-""
emp_df$MonthlyIncomeBins[which(emp_df$MonthlyIncome>= -1.17 & emp_df$MonthlyIncome<= -0.88)]<-'10090-23130'
emp_df$MonthlyIncomeBins[which(emp_df$MonthlyIncome>= -0.89 & emp_df$MonthlyIncome<= 0.71)]<-'23140-68770'
emp_df$MonthlyIncomeBins[which(emp_df$MonthlyIncome> 0.71 )]<-'68770+ '

summary(as.factor(emp_df$MonthlyIncomeBins))
#10090-23130 23140-68770  68770+  
#  438        2638        1325

## ---------------------------------------------------------------- ##

# Variable "YearsAtCompany" - Checking for Outliers and Binning

## ---------------------------------------------------------------- ##

emp_df$YearsAtCompany <- as.numeric(emp_df$YearsAtCompany)
quantile(emp_df$YearsAtCompany,seq(0,1,.01))

WOE_IV$Tables$YearsAtCompany


# YearsAtCompany   N    Percent         WOE         IV
#1          [0,0] 132 0.02993197  1.08952957 0.04881279
#2          [1,1] 513 0.11632653  1.00818393 0.20821653
#3          [2,2] 381 0.08639456  0.33981204 0.21936952
#4          [3,4] 714 0.16190476  0.01940218 0.21943087
#5          [5,6] 816 0.18503401 -0.43859498 0.24999523
#6          [7,8] 510 0.11564626 -0.36575766 0.26362567
#7          [9,9] 246 0.05578231 -0.57547819 0.27873827
#8        [10,14] 624 0.14149660 -0.38773657 0.29733555
#9        [15,40] 474 0.10748299 -0.68161061 0.33667579

# WOE is not monotonic

# Performing coarse classing for 0-2:
temp_yrs <- subset(emp_df,(YearsAtCompany>=0 & YearsAtCompany<=2))
comb_good <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==1)])
comb_bad <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 0-1 === .79

# Performing coarse classing for 3-8:
temp_yrs <- subset(emp_df,(YearsAtCompany>=3 & YearsAtCompany<=8))
comb_good <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==1)])
comb_bad <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 3-8 === -.25

# Performing coarse classing for 9-14:
temp_yrs <- subset(emp_df,(YearsAtCompany>=9 & YearsAtCompany<=14))
comb_good <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==1)])
comb_bad <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 9-14 === -.44

# Performing Binning for YearsAtCompany
emp_df$YearsAtCompanyBins<-""
emp_df$YearsAtCompanyBins[which(emp_df$YearsAtCompany>= 0 & emp_df$YearsAtCompany<= 2)]<-'0-2'
emp_df$YearsAtCompanyBins[which(emp_df$YearsAtCompany>= 3 & emp_df$YearsAtCompany<= 8)]<-'3-8'
emp_df$YearsAtCompanyBins[which(emp_df$YearsAtCompany>= 9 & emp_df$YearsAtCompany<= 14)]<-'9-14'
emp_df$YearsAtCompanyBins[which(emp_df$YearsAtCompany>= 15)]<-'15-40'

summary(as.factor(emp_df$YearsAtCompanyBins))
# 0-2  15-40   3-8  9-14 
# 1025   472  2036   868

## ---------------------------------------------------------------- ##

# Variable "YearsSinceLastPromotion" - Checking for Outliers and Binning

## ---------------------------------------------------------------- ##

emp_df$YearsSinceLastPromotion <- as.numeric(emp_df$YearsSinceLastPromotion)
quantile(emp_df$YearsSinceLastPromotion,seq(0,1,.01))

WOE_IV$Tables$YearsSinceLastPromotion


# YearsSinceLastPromotion    N    Percent         WOE         IV
#1                   [0,0] 1743 0.39523810  0.19476763 0.01599819
#2                   [1,1] 1071 0.24285714 -0.18913412 0.02413969
#3                   [2,3]  633 0.14353741  0.06787833 0.02481634
#4                   [4,6]  414 0.09387755 -0.61421902 0.05339376
#5                  [7,15]  549 0.12448980 -0.02051141 0.05344577
# WOE is not monotonic

# Performing coarse classing for 1-3:
temp_yrs <- subset(emp_df,(YearsSinceLastPromotion>=1 & YearsSinceLastPromotion<=3))
comb_good <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==1)])
comb_bad <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 1-3 === -.09

# Performing coarse classing for 4-15:
temp_yrs <- subset(emp_df,(YearsSinceLastPromotion>=4 & YearsSinceLastPromotion<=15))
comb_good <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==1)])
comb_bad <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 4-15 === -.24

# Performing Binning for YearsSinceLastPromotion
emp_df$YearsSinceLastPromotionBins<-""
emp_df$YearsSinceLastPromotionBins[which(emp_df$YearsSinceLastPromotion== 0)]<-'0'
emp_df$YearsSinceLastPromotionBins[which(emp_df$YearsSinceLastPromotion>= 1 & emp_df$YearsSinceLastPromotion<= 3)]<-'1-3'
emp_df$YearsSinceLastPromotionBins[which(emp_df$YearsSinceLastPromotion>= 4 & emp_df$YearsSinceLastPromotion<= 15)]<-'4-15'


summary(as.factor(emp_df$YearsSinceLastPromotionBins))
#   0  1-3 4-15 
#1739 1700  962

## ---------------------------------------------------------------- ##

# Variable "YearsWithCurrManager" - Checking for Outliers and Binning

## ---------------------------------------------------------------- ##

emp_df$YearsWithCurrManager <- as.numeric(emp_df$YearsWithCurrManager)
quantile(emp_df$YearsWithCurrManager,seq(0,1,.01))

WOE_IV$Tables$YearsWithCurrManager


# YearsWithCurrManager    N    Percent        WOE        IV
#1                [0,0]  789 0.17891156  0.9100131 0.1950035
#2                [1,1]  228 0.05170068 -0.1273466 0.1958062
#3                [2,2] 1032 0.23401361 -0.1224114 0.1991691
#4                [3,3]  426 0.09659864 -0.2186000 0.2034510
#5                [4,6]  474 0.10748299 -0.3408896 0.2145519
#6                [7,8]  969 0.21972789 -0.2791896 0.2301069
#7               [9,17]  492 0.11156463 -0.8898285 0.2947473

# WOE is not monotonic

# Performing coarse classing for 1-3:
temp_yrs <- subset(emp_df,(YearsWithCurrManager>=1 & YearsWithCurrManager<=2))
comb_good <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==1)])
comb_bad <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 1-2 === -.12

# Performing coarse classing for 4-8:
temp_yrs <- subset(emp_df,(YearsWithCurrManager>=4 & YearsWithCurrManager<=8))
comb_good <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==1)])
comb_bad <- length(temp_yrs$Attrition[which(temp_yrs$Attrition==0)])
combined_woe <- computeWoE(comb_good,comb_bad)
combined_woe  # Combined WOE for 4-8 === -.30

# Performing Binning for YearsWithCurrManager
emp_df$YearsWithCurrManagerBins<-""
emp_df$YearsWithCurrManagerBins[which(emp_df$YearsWithCurrManager== 0)]<-'0'
emp_df$YearsWithCurrManagerBins[which(emp_df$YearsWithCurrManager>= 1 & emp_df$YearsWithCurrManager<= 3)]<-'1-3'
emp_df$YearsWithCurrManagerBins[which(emp_df$YearsWithCurrManager>= 4 & emp_df$YearsWithCurrManager<= 8)]<-'4-8'
emp_df$YearsWithCurrManagerBins[which(emp_df$YearsWithCurrManager>= 9)]<-'9-15'


summary(as.factor(emp_df$YearsWithCurrManagerBins))
# 0  1-3  4-8 9-15 
# 787 1683 1440  491


#removing redundant columns after binning

emp_df <- subset(emp_df, select = -c(TotalWorkingYears,PercentSalaryHike,Age,DistanceFromHome,YearsWithCurrManager,
                                     MonthlyIncome,YearsAtCompany,YearsSinceLastPromotion))

#####################################################################
# Dummy value creation:
#####################################################################

# For variables having only two levels:
#gender "Male" is 1 and PerformanceRating "3" is 1 
#AvgWorkingHrs: If AvgWorkingHrs > 8.5, value is 1
emp_df$Gender<- ifelse(emp_df$Gender=="Male",1,0)
emp_df$PerformanceRating<- ifelse(emp_df$PerformanceRating=="3",1,0)
emp_df$AvgWorkingHrs<- ifelse(emp_df$AvgWorkingHrs>8.5,1,0)

# creating a dataframe of categorical features
emp_chr<- emp_df[,-c(1,5)]

# converting categorical attributes to factor
emp_fact<- data.frame(sapply(emp_chr, function(x) factor(x)))
str(emp_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(emp_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =emp_fact))[,-1]))

# Final dataset
emp_final<- cbind(emp_df[,c(1,5)],dummies) 
View(emp_final) #4401 obs. of  79 variables
emp_final <- emp_final[,-1]

# Coorelation Matrix
emp_final$Attrition <- as.numeric(emp_final$Attrition)
cor_df <- cor(emp_final)
cor_df

# Checking Attrition rate of Employees

Attrition_Rate <- sum(emp_df$Attrition)/nrow(emp_df)
Attrition_Rate # 26.57% churn rate. 

#################################################################
			#Section 4: Model Building 
#################################################################

###################### Logistic Regression ############################

# splitting the data between train and test
set.seed(100)
indices= sample(1:nrow(emp_final), 0.7*nrow(emp_final))
train = emp_final[indices,]
test = emp_final[-(indices),]

# first model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
vif(model_2)

#Removing AgeBins.x26.33 as it has high VIF and low p-value
model_3 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                 JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.04 + 
                 WorkLifeBalance.x0.86 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xLife.Sciences + EducationField.xMedical + 
                 JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + PerformanceRating + 
                 AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                 TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + 
                 TotalWorkExBins.x8.12 + AgeBins.x34.37 + 
                 AgeBins.x38.60 + MonthlyIncomeBins.x68770.. + YearsSinceLastPromotionBins.x4.15 + 
                 YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + 
                 YearsWithCurrManagerBins.x9.15 + JobRole.xResearch.Scientist, family = "binomial", data = train)

summary(model_3)
vif(model_3)

# removing PerformanceRating since it has high p value
model_4 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                 JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.04 + 
                 WorkLifeBalance.x0.86 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xLife.Sciences + EducationField.xMedical + 
                 JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + 
                 AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                 TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + 
                 TotalWorkExBins.x8.12 + AgeBins.x34.37 + 
                 AgeBins.x38.60 + MonthlyIncomeBins.x68770.. + YearsSinceLastPromotionBins.x4.15 + 
                 YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + 
                 YearsWithCurrManagerBins.x9.15 + JobRole.xResearch.Scientist, family = "binomial", data = train)

summary(model_4)
vif(model_4)

# removing JobRole.xResearch.Scientist as p value is high
model_5 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                 JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.04 + 
                 WorkLifeBalance.x0.86 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xLife.Sciences + EducationField.xMedical + 
                 JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + 
                 AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                 TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + 
                 TotalWorkExBins.x8.12 + AgeBins.x34.37 + 
                 AgeBins.x38.60 + MonthlyIncomeBins.x68770.. + YearsSinceLastPromotionBins.x4.15 + 
                 YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + 
                 YearsWithCurrManagerBins.x9.15 , family = "binomial", data = train)
summary(model_5)
vif(model_5)

#removing JobInvolvement.x2 as p value is high
model_6 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                 JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.04 + 
                 WorkLifeBalance.x0.86 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xLife.Sciences + EducationField.xMedical + 
                 JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x3 + AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                 TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + 
                 TotalWorkExBins.x8.12 + AgeBins.x34.37 + 
                 AgeBins.x38.60 + MonthlyIncomeBins.x68770.. + YearsSinceLastPromotionBins.x4.15 + 
                 YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + 
                 YearsWithCurrManagerBins.x9.15 , family = "binomial", data = train)
summary(model_6)
vif(model_6)

#removing MonthlyIncomeBins.x68770.. as p value is high
model_7 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                        JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.04 + 
                        WorkLifeBalance.x0.86 + BusinessTravel.xTravel_Frequently + 
                        BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                        Department.xSales + EducationField.xLife.Sciences + EducationField.xMedical + 
                        JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                        JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                        StockOptionLevel.x1 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                        JobInvolvement.x3 + AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                        TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + 
                        TotalWorkExBins.x8.12 + AgeBins.x34.37 + 
                        AgeBins.x38.60 + YearsSinceLastPromotionBins.x4.15 + 
                        YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + 
                        YearsWithCurrManagerBins.x9.15 , family = "binomial", data = train)
summary(model_7)
vif(model_7)

# removing TrainingTimesLastYear.x5 as p value is high
model_8 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                 JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.04 + 
                 WorkLifeBalance.x0.86 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xLife.Sciences + EducationField.xMedical + 
                 JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x3 + AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                 TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + 
                 TotalWorkExBins.x8.12 + AgeBins.x34.37 + 
                 AgeBins.x38.60 + YearsSinceLastPromotionBins.x4.15 + 
                 YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + 
                 YearsWithCurrManagerBins.x9.15 , family = "binomial", data = train)

summary(model_8)
vif(model_8)

#removing EducationField.xMedical as p-value is high
model_9 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                 JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.04 + 
                 WorkLifeBalance.x0.86 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xLife.Sciences +  
                 JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                 StockOptionLevel.x1 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x3 + AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                 TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + 
                 TotalWorkExBins.x8.12 + AgeBins.x34.37 + 
                 AgeBins.x38.60 + YearsSinceLastPromotionBins.x4.15 + 
                 YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + 
                 YearsWithCurrManagerBins.x9.15 , family = "binomial", data = train)

summary(model_9)
vif(model_9)

# removing EducationField.xLife.Sciences as p-value is high
model_10 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.04 + 
                  WorkLifeBalance.x0.86 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  StockOptionLevel.x1 + TrainingTimesLastYear.x6 + JobInvolvement.x3 + 
                  AvgWorkingHrs + TotalWorkExBins.x13.16 + 
                  TotalWorkExBins.x17.22 + TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + 
                  TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                  AgeBins.x34.37 + AgeBins.x38.60 + 
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, family = "binomial", data = train)

summary(model_10)
vif(model_10)

# removing StockOptionLevel.x1 as p-value is high
model_11 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.04 + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3 + 
                  AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                  TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + WorkLifeBalance.x0.86 + 
                  BusinessTravel.xTravel_Frequently + Department.xSales + JobLevel.x2 + 
                  JobRole.xManager + JobRole.xManufacturing.Director + TotalWorkExBins.x5.7 + 
                  TotalWorkExBins.x8.12 + AgeBins.x34.37 + AgeBins.x38.60 + 
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, 
                family = "binomial", data = train)

summary(model_11)
vif(model_11)

# removing WorkLifeBalance.x0.04 as p-value is high
model_12 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.28 + JobSatisfaction.x0.43 +  
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3 + 
                  AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                  TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + WorkLifeBalance.x0.86 + 
                  BusinessTravel.xTravel_Frequently + Department.xSales + JobLevel.x2 + 
                  JobRole.xManager + JobRole.xManufacturing.Director + TotalWorkExBins.x5.7 + 
                  TotalWorkExBins.x8.12 + AgeBins.x34.37 + AgeBins.x38.60 + 
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, 
                  family = "binomial", data = train)

summary(model_12)
vif(model_12)

#removing JobRole.xManager as p-value is high
model_13 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.28 + JobSatisfaction.x0.43 +  
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3 + 
                  AvgWorkingHrs + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                  TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + WorkLifeBalance.x0.86 + 
                  BusinessTravel.xTravel_Frequently + Department.xSales + JobLevel.x2 + 
                  JobRole.xManufacturing.Director + TotalWorkExBins.x5.7 + 
                  TotalWorkExBins.x8.12 + AgeBins.x34.37 + AgeBins.x38.60 + 
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, 
                family = "binomial", data = train)

summary(model_13)
vif(model_13)

#removing JobLevel.x2 as p-value is high
model_14 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.86 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3 + AvgWorkingHrs + TotalWorkExBins.x13.16 + 
                  TotalWorkExBins.x17.22 + TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + 
                  TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + AgeBins.x34.37 + AgeBins.x38.60 + 
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, family = "binomial", data = train)

summary(model_14)
vif(model_14)

# removing EnvironmentSatisfaction.x.0.22 as p-value is high
model_15 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.86 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3 + AvgWorkingHrs + TotalWorkExBins.x13.16 + 
                  TotalWorkExBins.x17.22 + TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + 
                  TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + AgeBins.x34.37 + AgeBins.x38.60 + 
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, family = "binomial", data = train)

summary(model_15)
vif(model_15)

# removing JobRole.xManufacturing.Director as p-value is high
model_16 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.86 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3 + AvgWorkingHrs + TotalWorkExBins.x13.16 + 
                  TotalWorkExBins.x17.22 + TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + 
                  TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + AgeBins.x34.37 + AgeBins.x38.60 + 
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, family = "binomial", data = train)

summary(model_16)
vif(model_16)

#removing JobInvolvement.x3 as p-value is high
model_17 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.28 + JobSatisfaction.x0.43 + WorkLifeBalance.x0.86 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  TrainingTimesLastYear.x6 + AvgWorkingHrs + TotalWorkExBins.x13.16 + 
                  TotalWorkExBins.x17.22 + TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + 
                  TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + AgeBins.x34.37 + AgeBins.x38.60 + 
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, family = "binomial", data = train)

summary(model_17)
vif(model_17)


# remove JobSatisfactionx0.028 since it is related to JobSatisfactionx0.43
model_18 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.43 + WorkLifeBalance.x0.86 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  TrainingTimesLastYear.x6 + AvgWorkingHrs + TotalWorkExBins.x13.16 + 
                  TotalWorkExBins.x17.22 + TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + 
                  TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + AgeBins.x38.60 + AgeBins.x34.37+
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, family = "binomial", data = train)

summary(model_18)
vif(model_18)

# removing BusinessTravel.xTravel_Rarely
model_19 <- glm(formula = Attrition ~ EnvironmentSatisfaction.x0.56 + 
                  JobSatisfaction.x0.43 + WorkLifeBalance.x0.86 + 
                  BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Department.xSales + JobRole.xResearch.Director + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                  TrainingTimesLastYear.x6 + AvgWorkingHrs + TotalWorkExBins.x13.16 + 
                  TotalWorkExBins.x17.22 + TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + 
                  TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + AgeBins.x38.60 + AgeBins.x34.37+
                  YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + 
                  YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15, family = "binomial", data = train)

summary(model_19)
vif(model_19)



########################################################################
final_model<- model_19

##### Model Inference ########
#Variable	                    Value affecting Attrition
#Environment Satisfaction	    Level 1 signifying Low Satisfaction
#Job Satisfaction	            Level 1 signifying Low Satisfaction
#Work Life Balance	          Level 1 signifying Bad Work Life Balance
#Business Travel	            Employees who travel Frequently
#Department	                  1.Research & Development      2.Sales
#Job Role	                    Research Director
#Marital Status	              Single
#Number of Companies Worked	  Employees who have worked in more than 5 companies
#Training Times Last Year	    Employees who have attended 6 trainings in last year
#AvgWorkingHrs	              Employees who spent more than the average of 8.5 hrs in office in the year
#Total Work Experience	      Employees who have more than 3 years of work experience
#Age	                        Employee who are 34 years or more
#Years Since Last Promotion	  Employees who haven't been promoted in last 4 years or more
#Years With Current Manager	Employees working with the same manager for more than 1 years


#######################################################################

			# Section 5: Model Evaluation

######################################################################

# predicted probabilities of Attrition for test data
test_pred = predict(final_model, test[,-1],type = "response")
summary(test_pred)
test$prob <- test_pred

# probability greater than .5 is 1 (employee will leave)
test_pred_attrition_50 <- factor(ifelse(test_pred >= 0.50, 1,0))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


# confusion matrix
test_conf <- confusionMatrix(test_pred_attrition_50, test$Attrition, positive = "1")

#Sensitivity : 0.30732         
#Specificity : 0.97043      
#Accuracy : 0.8675        
test_conf


# compute optimal probalility cutoff for better model reliability
perform_fn <- function(cutoff) 
{
  pred_attrition <- factor(ifelse(test_pred >= cutoff, 1,0))
  conf <- confusionMatrix(pred_attrition, test$Attrition, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values for plotting and initiallizing a matrix of 100 X 3.
prob_seq = seq(.002,.87,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(prob_seq[i])
} 

# plot sensitivity , specificity and accuracy with different values of probability
plot(prob_seq, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(prob_seq,OUT[,2],col="darkgreen",lwd=2)
lines(prob_seq,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


# find cutoff probability for threshold value above which represents that employee will leave
# value: 0.1686
cutoff <- prob_seq[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff
# Let's choose a cutoff value of 0.19 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1686, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#Accuracy : 0.746 
#Sensitivity : 0.7317          
# Specificity : 0.7482 

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

plot(performance_measures_test)
abline(0,1,col="grey")

####################################################################
# Lift & Gain Chart 

test1 <- test[,c(1,grep("prob", colnames(test)))]
test1 <- test1[order(-test1[,2],test1[,1]),]

gainchart <- data.frame(Decile=integer(),AttritionCount=integer(),CumulativeAttrition=integer(),GainPercent=integer(),NonAttritionCount=integer(),CumulativeNonAttrition=integer(),GainPercentNonAttrition=integer(),KS_stat=integer())
gainchart[c(1:10),] <- 0
strt<-1
stp<-nrow(test1)/10
obs<-nrow(test1)/10
for (i in 1:10){
  gainchart[i,1]  <- i
  gainchart[i,2]  <- sum(as.numeric(as.character(test1[c(strt:stp), 1])))
  ifelse ((i>=2), gainchart[i,3] <- gainchart[i-1,3] + gainchart[i,2], gainchart[i,3] <- gainchart[i,2])
  gainchart[i,5]  <- obs - gainchart[i,2]
  ifelse ((i>=2), gainchart[i,6] <- gainchart[i-1,6] + gainchart[i,5], gainchart[i,6] <- gainchart[i,5])
  strt <- strt + nrow(test1)/10
  stp <- stp + nrow(test1)/10
}
totalattri <- sum(gainchart$AttritionCount)
totalnonattri <- sum(gainchart$NonAttritionCount)
for (i in 1:10){
  gainchart[i,4]  <- gainchart[i,3]/totalattri*100
  gainchart[i,7]  <- gainchart[i,6]/totalnonattri*100
  gainchart[i,8]  <- gainchart[i,4] - gainchart[i,7]
}
gainchart$GainRandomModel <- c(10,20,30,40,50,60,70,80,90,100)
gainchart$lift <- gainchart$GainPercent/gainchart$GainRandomModel
View(gainchart)

#Decile	AttritionCount	CumulativeAttrition	GainPercent	NonAttritionCount CumulativeNonAttrition GainPercentNonAttrition KS_stat	GainRandomModel	lift
#	1		76				76				37.07317073		56.1				56.1					5.02688172		 32.04628901	10	3.707317073
#	2		46				122				59.51219512		86.1				142.2					12.74193548		 46.77025964	20	2.975609756
#	3		23				145				70.73170732		109.1				251.3					22.51792115		 48.21378617	30	2.357723577
#	4		19				164				80				113.1				364.4					32.65232975		 47.34767025	40	2
#	5		8				172				83.90243902		124.1				488.5					43.77240143		 40.13003759	50	1.67804878
#	6		11				183				89.26829268		121.1				609.6					54.62365591		 34.64463677	60	1.487804878
#	7		11				194				94.63414634		121.1				730.7					65.47491039		 29.15923595	70	1.351916376
#	8		3				197				96.09756098		129.1				859.8					77.04301075		 19.05455022	80	1.201219512
#	9		3				200				97.56097561		129.1				988.9					88.61111111		 8.949864499	90	1.08401084
#	10		5				205				100				127.1				1116					100				 0				100	1


# draw lift and gain chart
ggplot(gainchart, aes(Decile)) + geom_line(aes(y = 1, colour = "GainRandomModel")) + 
  geom_line(aes(y = lift, colour = "lift")) +geom_point(aes(y=1)) + geom_point(aes(y = lift, colour = "lift"))

ggplot(gainchart, aes(Decile)) + geom_line(aes(y = GainRandomModel, colour = "GainRandomModel")) + 
  geom_line(aes(y = GainPercent, colour = "GainPercent")) +geom_point(aes(y=GainRandomModel)) + geom_point(aes(y = GainPercent, colour = "GainPercent"))
