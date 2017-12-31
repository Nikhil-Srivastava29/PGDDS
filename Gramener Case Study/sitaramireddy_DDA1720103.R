install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("scales")
library(tidyr) 
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
options("scipen"=100, "digits"=4)

#import data from the csv file
loan<- read.csv("loan.csv",stringsAsFactors = F)
# Display the Structure of the uber_data data frame.
str(loan)

##Data Cleansing##

#checking NA values
sum(which(duplicated(loan$id)))
sum(which(duplicated(loan$member_id)))
sum(which(is.na(loan$loan_amnt)))
sum(which(is.na(loan$term)))
sum(which(is.na(loan$grade)))
sum(which(is.na(loan$home_ownership)))
sum(which(is.na(loan$verification_status)))
sum(which(is.na(loan$issue_d)))

#Removing column with only NA values
loan <- loan[, colSums(is.na(loan)) != nrow(loan)]
#Removing column with  0 and NA values
loan <- loan[, colSums(loan != 0, na.rm = TRUE) > 0]
#Removing column which has same value in each row
loan <-loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]

#Removing % symbol from int_rate and revol_util
loan$int_rate <- as.numeric(gsub("%","",as.character(loan$int_rate)))
loan$revol_util <- as.numeric(gsub("%","",as.character(loan$revol_util)))

#Standardizing data format for column issue_d
loan$issue_d <- paste("01-", loan$issue_d, sep="")
loan$issue_d<-as.Date(loan$issue_d,format="%d-%b-%y")

#Univariate Analysis

#------------------------------------DERIVED METRICS ---------------------------------------------------------
# derive  isuued year metric 
loan$issue_d_yr <- format(loan$issue_d, "%Y")

#Creating a derived metric from Salary
#We will see which salary group take most laon for this first we will create a salary groups and then draw histogram for this
loan$SalaryGroup <- 0

#)0-50000Rs
loan$SalaryGroup <- replace(loan$SalaryGroup, loan$annual_inc >= 0 & loan$annual_inc  < 50001, "Rs0-50000")
#)50000-100000Rs
loan$SalaryGroup <- replace(loan$SalaryGroup, loan$annual_inc >= 50001 & loan$annual_inc <= 100000, "Rs50,000-100,000")
#)100000-200000Rs
loan$SalaryGroup <- replace(loan$SalaryGroup, loan$annual_inc >= 100001 & loan$annual_inc <= 200000, "Rs100,001-200,000")
#)200000RsAndAbove
loan$SalaryGroup <- replace(loan$SalaryGroup, loan$annual_inc >= 200001 , "Rs200000AndAbove")



#subseting depending on loan status.
loan_cof<- subset(loan, loan$loan_status=="Charged Off")
current<-subset(loan, loan$loan_status=="Current")
Fully_paid<-subset(loan, loan$loan_status=="Fully Paid")

write.csv(loan,"loan_outputs.csv")
write.csv(loan_cof, "loan_chargedOff.csv")
#-------------------------------------- UNIVARIATE ANALYSIS-------------------------------------------------------:
# A.-------------------------effect of loan amount on loan status:
# WITH THE BELOW WE CONCLUDE THAT LOAN AMMOUNT DOES NOT EFFECT 
#1.drawing histogram of loan amount
hist(loan$loan_amnt)


#2. Grouping the Total loan amount based on loan status
# This shows that data given mainly comprises of 'Fully Paid" loans 
# followed by "Charged Off" and then "Current" loans.
loan_status <- group_by(loan,loan_status);
loan_status_by_count <- summarise(loan_status,total=n())
loan_status_by_count$percent_status_by_count <- round(100*loan_status_by_count$total/sum(loan_status_by_count$total), 2)
#create pie chart for grouped data
pie(loan_status_by_count$percent_status_by_count, col=rainbow(length(loan_status_by_count$loan_status)), 
    main="%age Distribution of total Loan Amount by Loan Status", labels=loan_status_by_count$loan_status)

#3.To see distribution in the employee length - How much experience person get most loans
#People with 10+ year of experience get most loans
ggplot(loan, aes(x = factor(loan$emp_length))) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+ylab("percentage of total loans")

#To see distribution of Salary
#The people between salary of 0-1 lac take most loans
ggplot(loan, aes(x = factor(loan$SalaryGroup))) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+ylab("percentage of total loans")

#Distribution of verification status
verification_status <- group_by(loan,verification_status);
verification_status_by_count <- summarise(verification_status,total=n())
verification_status_by_count$percent_status_by_count <- round(100*verification_status_by_count$total/sum(verification_status_by_count$total), 2)
#create pie chart for grouped data
pie(verification_status_by_count$percent_status_by_count, col=rainbow(length(verification_status_by_count$verification_status)), 
    main="%age Distribution of total Loan Amount by Loan Status", labels=verification_status_by_count$verification_status)
#50 % of the loans are verified

#Distribution of grade
ggplot(loan, aes(x = factor(loan$grade))) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+ylab("percentage of total loans")
#Mostly Garde A , B and C people get loans

#Distribution of loans state wise
ggplot(loan, aes(x = factor(loan$addr_state))) + geom_bar()+ylab("percentage of total loans")
#The most state where loan is given is CA, NY, TX

#To see  purpose of loan
ggplot(loan, aes(x = factor(loan$purpose))) + geom_bar()+ylab("percentage of total loans")
#The most state where loan is given is dept consolidation and credit card

#To see the distribution of annual income
#Here 59000 is the median
summary(loan$annual_inc)

#To see the distribution of dept to income ratip i.e dti
#Here 13.40 is the median
summary(loan$dti)
#Histogram to show dept to income ratio
ggplot(aes(x=dti), data=loan) + geom_histogram(fill='#05DBF2', color='black') +labs(title=expression(paste("Debt to Income Ratio")), 
       x='Debt / Income')

#analysis on open account
open_acc_group<-group_by(loan,loan_status)
open_acc_group_summerize<- summarise(open_acc_group,mean(open_acc))
open_acc_group_summerize

#revolvr utilization
revol_util_group<-group_by(loan,loan_status)
revol_util_group_summerize<- summarise(revol_util_group,mean(revol_util,na.rm = T))
revol_util_group_summerize

#intrest rate
int_rate_group<-group_by(loan,loan_status,term)
int_rate_group_summerize<- summarise(int_rate_group,mean(int_rate,na.rm = T))
int_rate_group_summerize

#Further analysis on total loan based on Loan duration and loan status
#This shows: People having higher term are defaulting more 
# Inference: Term will have a impact if the borrower will default. 
loansubset_36 <- subset(loan,term==" 36 months")
loansubset_60 <- subset(loan,term==" 60 months")
#Grouping all loan for 36 months duration by loan status
loan_status <- group_by(loansubset_36,loan_status);
loan_status_by_count <- summarise(loan_status,total=n())
loan_status_by_count$percent_status_by_count <- round(100*loan_status_by_count$total/sum(loan_status_by_count$total), 2)
#create pie chart for grouped data
pie(loan_status_by_count$percent_status_by_count, col=rainbow(length(loan_status_by_count$loan_status)), 
    main="Term= 36 months \n %age Distribution of total Loan Amount by Loan Status", labels=loan_status_by_count$loan_status)

#Grouping all loan for 60 months duration by loan status
loan_status <- group_by(loansubset_60,loan_status);
loan_status_by_count <- summarise(loan_status,total=n())
loan_status_by_count$percent_status_by_count <- round(100*loan_status_by_count$total/sum(loan_status_by_count$total), 2)
#create pie chart for grouped data
pie(loan_status_by_count$percent_status_by_count, col=rainbow(length(loan_status_by_count$loan_status)), 
    main="Term= 60 months \n %age Distribution of total Loan Amount by Loan Status", labels=loan_status_by_count$loan_status)


#Bivariate anaylysis and Segmented Analysis

#To see how  loans vary in each state and also the difference between defaulted and paid loan
ggplot(loan, aes(x=addr_state,y = id ,fill=loan$loan_status))+geom_bar(position="stack",stat="summary",fun.y = length) + ggtitle("Loan Status for Each Country")
#Semented Analysis - Only on Charged Off Customer to see trend
ggplot(loan_cof, aes(x=addr_state,y = id ))+geom_bar(position="stack",stat="summary",fun.y = length) + ggtitle("Defaulter country")
#People of CA ,NY , FL, TX take most loan and have most defaulter as seen in second graph

#To see for what purpose the loan was taken most and how loan status vary for them
ggplot(loan, aes(x=loan$purpose,fill=loan$loan_status))+geom_bar(aes(y =((..count..)/sum(..count..))*100) )+ ggtitle("Loan % for each Purpose")
#Semented Analysis - Only on Charged Off Customer to see trend
ggplot(loan_cof, aes(x=purpose ))+geom_bar(aes(y =((..count..)/sum(..count..))*100) ) + ggtitle("Defaulter Most Common Purpose")
#People have taken loan mostly for dept consolidation and credit card, the default % also vary for this most

#To see for what grade the loan was given most and how loan status vary for them
grade_distribution_barchart <- ggplot(loan, aes(x=loan$grade,fill=loan$loan_status))+geom_bar(aes(y =((..count..)/sum(..count..))*100) )
grade_distribution_barchart <- grade_distribution_barchart+ ggtitle("Loan % in Each Grade")
grade_distribution_barchart <- grade_distribution_barchart + labs(x="Grade", y="% Of Tax Id")
grade_distribution_barchart
#Semented Analysis - Only on Charged Off Customer to see trend
grade_distribution_chargeOff_barchart <- ggplot(loan_cof, aes(x=grade ))+geom_bar(aes(y =((..count..)/sum(..count..))*100) ) 
grade_distribution_chargeOff_barchart <- grade_distribution_chargeOff_barchart+ ggtitle("Grade With Higest Defaulter")
grade_distribution_chargeOff_barchart <- grade_distribution_chargeOff_barchart + labs(x="Grade", y="% Of Tax Id Of Charge Off User")
grade_distribution_chargeOff_barchart
#As loan grade decreases , the defaulter percentage first increase and then start to decrease
#So the highest defaulter lies in grade B, C,D

#To see defaulter in term distribution
term_distribution_barchart <- ggplot(loan, aes(x=loan$term,fill=loan$loan_status))+geom_bar(aes(y =((..count..)/sum(..count..))*100) )
term_distribution_barchart <- term_distribution_barchart+ ggtitle("Loan % for each Term")
term_distribution_barchart <- term_distribution_barchart + labs(x="Term", y="% Of Tax Id")
term_distribution_barchart
#Semented Analysis - Only on Charged Off Cstomer to see trend
term_distribution_chargeOff_barchart <- ggplot(loan_cof, aes(x=term ))+geom_bar(aes(y =((..count..)/sum(..count..))*100) ) 
term_distribution_chargeOff_barchart <- term_distribution_chargeOff_barchart+ ggtitle("Term with highest Defaulter")
term_distribution_chargeOff_barchart <- term_distribution_chargeOff_barchart + labs(x="Term", y="% Of Tax Id of Charged off user")
term_distribution_chargeOff_barchart
#The most defaulter lies in 36 month term

#To see defaulter in salary distribution
salary_distribution_barchart <- ggplot(loan, aes(x=loan$SalaryGroup,fill=loan$loan_status))+geom_bar(aes(y =((..count..)/sum(..count..))*100) )
salary_distribution_barchart <- salary_distribution_barchart + ggtitle("Loan % for each Term")
salary_distribution_barchart <- salary_distribution_barchart + labs(x="Salary Group", y="% Of Tax Id")
salary_distribution_barchart
#Semented Analysis - Only on Charged Off Cstomer to see trend
salary_distribution_chargeOff_barchart <- ggplot(loan_cof, aes(x=SalaryGroup ))+geom_bar(aes(y =((..count..)/sum(..count..))*100) ) 
salary_distribution_chargeOff_barchart <- salary_distribution_chargeOff_barchart + ggtitle("Term with highest Defaulter")
salary_distribution_chargeOff_barchart <- salary_distribution_chargeOff_barchart + labs(x="Salary Group", y="% Of Tax Id For Charged Off")
salary_distribution_chargeOff_barchart
#The people with salary range 0 to 1 lac Default Most


#---------------------------Correlation Matrix---------------------------#

# Findings from Correlation Matrix:
# 1: Strong Positive Corelation between loan amount and total payment.
# 2. Strong Positive Corelation between loan amount and funded amount.
# 3. Strong Positive Corelation between loan amount and total payment invested
# 4. Negative corelation between public records and loan amt
# 5. No relation between DTI ratio and loan amount
##### Analysis on Charged off Loans######
loan_cof$amnt_pending <- loan_cof$loan_amnt-loan_cof$total_rec_prncp

cor(loan_cof$int_rate,loan_cof$amnt_pending)
cor_matrix_dataframe <- loan_cof %>% select(c(3:5),c(7,8),annual_inc,dti,delinq_2yrs,c(30:34),c(37:44),last_pymnt_amnt,amnt_pending)


#scatter plot of Interest rate and Loan Amount with Size based on the Loan amount pending and color based on Grades
# This shows:
# 1. Grade A has very few Deliquencies for Loan Duration of 36 months than for 60 months
# 2. Each Grade has a interest rate bracket irrespective of the loan amount
# 3. 36Months Term:Grades A, B, C based on loan amount,while Grades D, E, F, G has more loans in 60Months Term
# 4. The number of deliquiencies with higher loan amount and large pending amount is higher in 60Months than for 36 Months
scatter_loanAmt <-ggplot(loan_cof, aes(x=int_rate, y=loan_amnt, col=grade, size=amnt_pending))
scatter_loanAmt+geom_point(alpha = 0.4)+facet_wrap(~term,ncol=1)

#Bar Graph of Delinquencies based on open accounts. This shows:
# folks having open accounts from 6-10 has maximum deliquencies in terms of amount pending
scatter_homeOwnership <-ggplot(loan_cof, aes(x=as.factor(open_acc), y=amnt_pending))
scatter_homeOwnership+geom_col()

#SalaryGroup Analysis based on count and amount pending
# a. by salary
# For the salary slab of 0-50,000 the default is highest and mostly loans are not verified
# Followed by 50 to 1 lac.
loan_ver_status <-loan_cof%>%
  group_by(SalaryGroup,verification_status)%>%
  summarise(total=sum(amnt_pending),count=n())
ggplot_verstat_count <- ggplot(loan_ver_status,aes(x=SalaryGroup,y=count,fill=verification_status))
ggplot_Cpct_verstat<-ggplot_verstat_count+geom_bar(position="fill",stat="identity")+scale_y_continuous(labels = percent_format())
ggplot_Cpct_verstat <- ggplot_Cpct_verstat+labs(x="SalaryGroup", y="%age distribution of Count based on verification Status")
ggplot_Cpct_verstat

# Ploting Grades and Subgrades for Charged-Off customers.
#This shows:
# Grades B3, B4 and B5 have maximum number of charged-off loans
# A1, G3,G4,G5 has least count of charged-off loans
# Based on amount pending, B3,B4 and B5 have max amount
# Based on amount pending,Amount decreases from G1 to G5. A1 has the least amount.
grades_subgrades_grp <- loan_cof %>%
  group_by(grade,sub_grade) %>%
  summarise(total=sum(amnt_pending), count=n())

ggplot(grades_subgrades_grp, aes(x=sub_grade,y=total))+geom_col()
ggplot(grades_subgrades_grp, aes(x=sub_grade,y=count))+geom_col()

# Finding top 5 states based on total amount pending for that state.
#grouping data and arranging in desc order

top5_states <- loan_cof %>%
  group_by(addr_state)%>%
  summarise(total=sum(amnt_pending)) %>%
  arrange(desc(total))

head(top5_states,5)
#Top 5 states are CA, FL NY, TX, NJ
loan_cof_top5States <- subset(loan_cof,(addr_state=="CA"|addr_state=="FL"|addr_state=="NY"|addr_state=="TX"|addr_state=="NJ"))

# Analysis on Top 5 States for Purpose of home loan

#grouping state data based on state and purpose
top5_purpose_state <- loan_cof_top5States %>%
  group_by(addr_state,purpose)%>%
  summarise(total=sum(amnt_pending),count=n()) 
# This shows:
# Debt Consolidation is the primary purpose across top 5 states leading to maximum deliquencies
# Credit Card,Small Business, and Home Improvement are other major categories across states
# Similar inference can be derived using Total Amount pending across Top states based on purpose
ggplot_state_purp_byCount <- ggplot(top5_purpose_state,aes(x=addr_state,y=count,fill=purpose))
ggplot_Cpct_verstat<-ggplot_state_purp_byCount+geom_bar(position="fill",stat="identity")+scale_y_continuous(labels = percent_format())
ggplot_Cpct_verstat+labs(x="State", y="%age distribution of Diliquent Count based on Purpose")

ggplot_state_purp_byPAmnt <- ggplot(top5_purpose_state,aes(x=addr_state,y=count,fill=purpose))
ggplot_Amntpct_verstat<-ggplot_state_purp_byPAmnt+geom_bar(position="fill",stat="identity")+scale_y_continuous(labels = percent_format())
ggplot_Amntpct_verstat+labs(x="State", y="%age distribution of Pending Amount in Diliquencies based on Purpose")


# Analysis on Top 5 States for Purpose of home loan

#grouping state data based on state and purpose
top5_grade_state <- loan_cof_top5States %>%
  group_by(addr_state,grade)%>%
  summarise(total=sum(amnt_pending),count=n()) 
# This shows:
# Grade B is the primary grade across top 5 states leading to maximum deliquencies
# Grade C,D and E are other major grades contributing to deliquencies across top states
# Similar inference can be derived using Total Amount pending across Top states based on Grade
ggplot_state_grade_byCount <- ggplot(top5_grade_state,aes(x=addr_state,y=count,fill=grade))
ggplot_Cpct_stat<-ggplot_state_grade_byCount+geom_bar(position="fill",stat="identity")+scale_y_continuous(labels = percent_format())
ggplot_Cpct_stat+labs(x="State", y="%age distribution of Diliquent Count based on Grade")

ggplot_state_grade_byPAmnt <- ggplot(top5_grade_state,aes(x=addr_state,y=count,fill=grade))
ggplot_Amntpct_stat<-ggplot_state_grade_byPAmnt+geom_bar(position="fill",stat="identity")+scale_y_continuous(labels = percent_format())
ggplot_Amntpct_stat+labs(x="State", y="%age distribution of Pending Amount in Diliquencies based on Grade")




#---------------------------------------FACTORS THAT EFFECT LOAN STATUS------------------------------
# Here we derived the factors depend on the percentage of defaulters in certain factor 
#with respect to total loans of that factor

#----------------analysis on purpose---------------------------------------------------
# from the below we conclude thatr the small business purpose have more probability of defaulter.
purpose_group<-group_by(loan,purpose)
purpose_group_summerize<- summarise(purpose_group,length(id))
purpose_group_summerize

purpose_group_charged<-group_by(loan_cof,purpose)
purpose_group_charged_summerize<- summarise(purpose_group_charged,length(id))
purpose_group_charged_summerize

purpose_group_charged_summerize$percent<- round((purpose_group_charged_summerize$`length(id)`/purpose_group_summerize$`length(id)`*100),2)

viz_plot_purpose<- ggplot(purpose_group_charged_summerize, aes(purpose,factor(percent)))+geom_point(size=4,color= "blue")
viz_plot_purpose+ylab("percentage of loans that are charged off")

varience_table<- rbind(c('purpose',var(purpose_group_charged_summerize$percent)))

#------------------------------------- analysis oN GRADE-----------------------------------------
# from the below we conclude that as the grade increases from A to G the probability of defaulter increases.

grade_group<-group_by(loan,grade)
grade_group_summerize<- summarise(grade_group,length(id))
grade_group_summerize

grade_group_charged<-group_by(loan_cof,grade)
grade_group_charged_summerize<- summarise(grade_group_charged,length(id))
grade_group_charged_summerize

grade_group_charged_summerize$percent<- grade_group_charged_summerize$`length(id)`/grade_group_summerize$`length(id)`*100
grade_group_charged_summerize

viz_plot_grade_group<- ggplot(grade_group_charged_summerize, aes(grade,percent))+geom_col()
viz_plot_grade_group+ylab("percentage of loans that are charged off")

varience_table<-rbind(varience_table,c('grade',var(grade_group_charged_summerize$percent)))

#----------------------analysis on employee length-----------------------------------------------------------



emp_length_group<-group_by(loan,emp_length)
emp_length_group_summerize<- summarise(emp_length_group,length(id))
emp_length_group_summerize

emp_length_group_charged<-group_by(loan_cof,emp_length)
emp_length_group_charged_summerize<- summarise(emp_length_group_charged,length(id))
emp_length_group_charged_summerize

emp_length_group_charged_summerize$percent<- emp_length_group_charged_summerize$`length(id)`/emp_length_group_summerize$`length(id)`*100
emp_length_group_charged_summerize

viz_plot_emp_length<- ggplot(emp_length_group_charged_summerize, aes(emp_length,percent))+geom_point(size=4,color= "blue")
viz_plot_emp_length+ylab("percentage of loans that are charged off")

varience_table<-rbind(varience_table,c('emp_length',var(emp_length_group_charged_summerize$percent)))

#-----------------------------------salary group---------------------------------------------------
# from the below we conclude that anual income with less than 50000 will have more probability of defaulters.

salary_group<-group_by(loan,SalaryGroup)
salary_group_summerize<- summarise(salary_group,length(id))
salary_group_summerize

salary_group_charged<-group_by(loan_cof,SalaryGroup)
salary_group_charged_summerize<- summarise(salary_group_charged,length(id))

salary_group_charged_summerize$percent<- salary_group_charged_summerize$`length(id)`/salary_group_summerize$`length(id)`*100
salary_group_charged_summerize
viz_plot_salary_group<- ggplot(salary_group_charged_summerize, aes(SalaryGroup,percent))+geom_point(size=4,color="blue")
viz_plot_salary_group+ylab("percentage of loans that are charged off")

varience_table<-rbind(varience_table,c('salary group',var(salary_group_charged_summerize$percent)))

#---------------------------------------------RECOMENDATIONS-------------------------------------------------

#With above analysis we can recommend that,
#As E,F,G  grade have more probability for defaulters, please reduce to give loans for such people who are in those grades.
#As probability of defaulters is more for the purpose of small business, please take necessary actions while disbursing loans to them.
#As probability of defaulters is more for low annual income people, please reduce to give loans for such people.



