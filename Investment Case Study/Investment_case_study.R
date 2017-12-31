#Checkpoint 1: Data Cleaning 1 ---

#Load the companies and rounds data (provided on the previous page) into two data frames 
#and name them companies and rounds2 respectively.

companies <- read.delim("companies.txt")
rounds2 <- read.csv("rounds2.csv")


library(tidyr)
library(dplyr)

#converting dataset companies & rounds2 to lower case for comparisions and merging
companies <- mutate_all(companies,funs(tolower))
rounds2 <- mutate_all(rounds2,funs(tolower))

#1. How many unique companies are present in rounds2?
rounds2.1 <- separate(rounds2, company_permalink, into=c("link","org", "company"), sep = "/",remove="F")
unique_companies_in_round2 <- length(unique(rounds2.1$company))

#2. How many unique companies are present in the companies file?
unique_companies_in_companies <- length(unique(companies$name))

#4 Merge the two data frames so that all variables (columns)
#in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame.How many observations are present in master_frame ?

master_frame <- merge(rounds2,companies, by.x="company_permalink", by.y="permalink")

#Table-2.1- Average Values of Investments for Each of these Funding Types		
master_frame$raised_amount_usd <-as.numeric(master_frame$raised_amount_usd)
investment_group <- group_by(master_frame,funding_round_type);
summary_inv_grp <-summarise(investment_group,inv_avg=mean(raised_amount_usd,na.rm=T))

#Countries with the highest amount of funding for the chosen investment type.
venture_df = subset(master_frame,funding_round_type=="venture")
country_group <- group_by(venture_df,country_code);
summary_by_total <- summarise(country_group,total_funding=sum(raised_amount_usd,na.rm=T))

#Identify the top three English-speaking countries in the data frame top9.
eng_countries <- read.csv("eng_countries.csv")
eng_countries <- mutate_all(eng_countries,funs(tolower))

merge_summary <- merge(summary_by_total,eng_countries, by="country_code")
eng_summary <- subset(merge_summary, official_eng=="true")
  
eng_summary <-arrange(eng_summary,official_eng, desc(total_funding))

#top 9 countries
top9 <- head(eng_summary,9)
top9

#sector analysis
master_frame <-  separate(master_frame,category_list, into=c("primary_sector"), sep = "[|]",remove="F")

#loading the mapping file as is
mapping<-read.csv("mapping.csv",check.names=FALSE)

#replacing the "0" in values of category_list with "na"
mapping$category_list <- sub("0","na",mapping$category_list)

#performing "gather" to map categories to sectors 
mapping <- gather(mapping,sector,my_val,2:10)
mapping <- mapping[!(mapping$my_val == 0),]
mapping <- mapping[,-4]
mapping <- mutate_all(mapping,funs(tolower))

# merging the category list and sector. creating a new DF to ensure data integrity and verification
master_frame2 <- merge(master_frame,mapping, by.x="primary_sector",by.y="category_list")
master_frame2 <- master_frame2[,-18]

#Checkpoint 5: Sector Analysis. 
#Creating 3 DF of investments done in US, GBR, IND, within the investment range and type="venture"
D1 <- subset(master_frame2,(country_code=="usa" &
      funding_round_type=="venture" & 
    raised_amount_usd>=5000000 & raised_amount_usd<=15000000))

D2 <- subset(master_frame2,(country_code=="gbr" &
      funding_round_type=="venture" & 
      raised_amount_usd>=5000000 & raised_amount_usd<=15000000))

D3 <- subset(master_frame2,(country_code=="ind" &
      funding_round_type=="venture" & 
      raised_amount_usd>=5000000 & raised_amount_usd<=15000000))

#Sector wise investment analysis for USA
D1_sector_group <- group_by(D1,sector);
D1_summary_by_total <- summarise(D1_sector_group,total_investment_in_sector=sum(raised_amount_usd,na.rm=T))
D1_summary_by_count <- summarise(D1_sector_group,count_investment_in_sector=n())
D1_sector <- merge(D1_summary_by_count,D1_summary_by_total, by="sector")
D1_sector <- arrange(D1_sector, desc(count_investment_in_sector), desc(total_investment_in_sector))

D1_sector
# Total number of investments in USA
sum(D1_sector$count_investment_in_sector)
# Total amount of investments in USA
sum(D1_sector$total_investment_in_sector)

#finding the company which recieved the highest investment in top sector in USA
D1_highest_sector_company_subset <- subset(D1,sector=="others")
D1_highest_sector_company_subset <- arrange(D1_highest_sector_company_subset,desc(raised_amount_usd))
head(D1_highest_sector_company_subset$name,1) 
  #####

#finding the company which recieved the highest investment in second top sector in USA
D1_second_highest_sector_company_subset <- subset(D1,sector=="social, finance, analytics, advertising")
D1_second_highest_sector_company_subset <- arrange(D1_second_highest_sector_company_subset,desc(raised_amount_usd))
head(D1_second_highest_sector_company_subset$name,1) 
####
#Sector wise investment analysis for GBR
D2_sector_group <- group_by(D2,sector);
D2_summary_by_total <- summarise(D2_sector_group,total_investment_in_sector=sum(raised_amount_usd,na.rm=T))
D2_summary_by_count <- summarise(D2_sector_group,count_investment_in_sector=n())
D2_sector <- merge(D2_summary_by_count,D2_summary_by_total, by="sector")
D2_sector <- arrange(D2_sector, desc(count_investment_in_sector), desc(total_investment_in_sector))

D2_sector
# Total number of investments in GBR
sum(D2_sector$count_investment_in_sector)
# Total amount of investments in GBR
sum(D2_sector$total_investment_in_sector)
#finding the company which recieved the highest investment in top sector in GBR
D2_highest_sector_company_subset <- subset(D2,sector=="others")
D2_highest_sector_company_subset <- arrange(D2_highest_sector_company_subset,desc(raised_amount_usd))
head(D2_highest_sector_company_subset$name,1) 
#####
#finding the company which recieved the highest investment in second top sector in GBR
D2_second_highest_sector_company_subset <- subset(D2,sector=="social, finance, analytics, advertising")
D2_second_highest_sector_company_subset <- arrange(D2_second_highest_sector_company_subset,desc(raised_amount_usd))
head(D2_second_highest_sector_company_subset$name,1) 
####
#Sector wise investment analysis for IND
D3_sector_group <- group_by(D3,sector);
D3_summary_by_total <- summarise(D3_sector_group,total_investment_in_sector=sum(raised_amount_usd,na.rm=T))
D3_summary_by_count <- summarise(D3_sector_group,count_investment_in_sector=n())
D3_sector <- merge(D3_summary_by_count,D3_summary_by_total, by="sector")
D3_sector <- arrange(D3_sector, desc(count_investment_in_sector), desc(total_investment_in_sector))

D3_sector
# Total number of investments in India
sum(D3_sector$count_investment_in_sector)
# Total amount of investments in India
sum(D3_sector$total_investment_in_sector)
#finding the company which recieved the highest investment in top sector in IND
D3_highest_sector_company_subset <- subset(D3,sector=="others")
D3_highest_sector_company_subset <- arrange(D3_highest_sector_company_subset,desc(raised_amount_usd))
head(D3_highest_sector_company_subset$name,1) 
#####
#finding the company which recieved the highest investment in second top sector in IND
D3_second_highest_sector_company_subset <- subset(D3,sector=="social, finance, analytics, advertising")
D3_second_highest_sector_company_subset <- arrange(D3_second_highest_sector_company_subset,desc(raised_amount_usd))
head(D3_second_highest_sector_company_subset$name,1) 
####

#Dowloading files for reference analysis. Kindly ignore###
write.csv(companies,"Dowloads/companies.csv")
write.csv(rounds2,"Dowloads/rounds2_export.csv")
write.csv(mapping,"Dowloads/mapping.csv")
write.csv(master_frame,"Dowloads/master_frame.csv")
write.csv(top9,"Dowloads/top9.csv")
write.csv(master_frame2,"Dowloads/master_frame2.csv")
write.csv(D1,"Dowloads/D1.csv")
write.csv(D2,"Dowloads/D2.csv")
write.csv(D3,"Dowloads/D3.csv")

  ####END OF ASSIGNMENT####
