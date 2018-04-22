# Spark - Parking Voilation dataset

# 1. Initialize spark session
# load SparkR
library(SparkR)
require(ggplot2)
# initialise the spark session
sparkR.session(master='local')

# 2. Create a Spark DataFrame and examine structure
# reading a CSV file from S3 bucket

parkingVoilations_2015 <- SparkR::read.df("s3://data-science-10/NYC-Parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", header=T,na.strings="", "CSV")
parkingVoilations_2016 <- SparkR::read.df("s3://data-science-10/NYC-Parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", header=T,na.strings="", "CSV")
parkingVoilations_2017 <- SparkR::read.df("s3://data-science-10/NYC-Parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", header=T,na.strings="", "CSV")


# 3. look at the first few rows
head(parkingVoilations_2015)
head(parkingVoilations_2016)
head(parkingVoilations_2017)

# Removing the whitespaces from the colnames for all three datasets
colnames_df<-colnames(parkingVoilations_2015)
colnames_df<-gsub(" ","_",colnames_df)
colnames(parkingVoilations_2015)<-colnames_df

colnames_df<-colnames(parkingVoilations_2016)
colnames_df<-gsub(" ","_",colnames_df)
colnames(parkingVoilations_2016)<-colnames_df

colnames_df<-colnames(parkingVoilations_2017)
colnames_df<-gsub(" ","_",colnames_df)
colnames(parkingVoilations_2017)<-colnames_df

# Creating the Month and Year Column for each year and combining this for analysis
parkingVoilations_2015$monthofIssue<-
  month(to_date(cast(unix_timestamp(parkingVoilations_2015$Issue_Date,'MM/dd/yyyy'),"timestamp")))
parkingVoilations_2016$monthofIssue<-
  month(to_date(cast(unix_timestamp(parkingVoilations_2016$Issue_Date,'MM/dd/yyyy'),"timestamp")))
parkingVoilations_2017$monthofIssue<-
  month(to_date(cast(unix_timestamp(parkingVoilations_2017$Issue_Date,'MM/dd/yyyy'),"timestamp"))) 

parkingVoilations_2015$yearofIssue<-
  year(to_date(cast(unix_timestamp(parkingVoilations_2015$Issue_Date,'MM/dd/yyyy'),"timestamp")))
parkingVoilations_2016$yearofIssue<-
  year(to_date(cast(unix_timestamp(parkingVoilations_2016$Issue_Date,'MM/dd/yyyy'),"timestamp")))
parkingVoilations_2017$yearofIssue<-
  year(to_date(cast(unix_timestamp(parkingVoilations_2017$Issue_Date,'MM/dd/yyyy'),"timestamp")))

parkingVoilations_2015$monthYear<-
  concat_ws("-",parkingVoilations_2015$monthofIssue, parkingVoilations_2015$yearofIssue)
parkingVoilations_2016$monthYear<-
  concat_ws("-",parkingVoilations_2016$monthofIssue, parkingVoilations_2016$yearofIssue)
parkingVoilations_2017$monthYear<-
  concat_ws("-",parkingVoilations_2017$monthofIssue, parkingVoilations_2017$yearofIssue)


parkingVoilations_2015$Address <- concat_ws(", ",parkingVoilations_2015$"House_Number", 
                                            parkingVoilations_2015$"Street_Name")

parkingVoilations_2016$Address <- concat_ws(", ",parkingVoilations_2016$"House_Number", 
                                            parkingVoilations_2016$"Street_Name")

parkingVoilations_2017$Address <- concat_ws(", ",parkingVoilations_2017$"House_Number", 
                                            parkingVoilations_2017$"Street_Name")

#Creating SQL tables
createOrReplaceTempView(parkingVoilations_2015,"parkingVoilations_2015_tbl")
createOrReplaceTempView(parkingVoilations_2016,"parkingVoilations_2016_tbl")
createOrReplaceTempView(parkingVoilations_2017,"parkingVoilations_2017_tbl")

# Timeperiod Considerations  for Dataset for 2015: Most of the data is for the period: Jun-14 to June-15
# The data outside this timeframe is minimal (0.8%) and hence treated as data issues which should not impact the overall analysis by great extent.
# Assumption: Entire dataset is considered for EDA

#	monthYear	count
#	10-2015 1096952
#      3-2016 1013888
#      9-2015  939355
#     11-2015  935365
#      8-2015  902634
#      4-2016  900709
#      7-2015  884785
#      5-2016  874469
#      2-2016  840354
#     1-2016  814181
#    12-2015  767085
#     6-2016  427117
#     6-2015  222040

PeriodwiseTicketCount_2015 <-summarize(groupBy(parkingVoilations_2015, 
                                               parkingVoilations_2015$monthYear),
                                       count = n(parkingVoilations_2015$monthYear))

collect(arrange(PeriodwiseTicketCount_2015,desc(PeriodwiseTicketCount_2015$count)))

# Timeperiod Considerations  for Dataset for 2016: Most of the data is for the period: Jun-15 to June-16
# The data outside this timeframe is minimal (0.1%) and hence treated as data issues which should not impact the overall analysis by great extent.
# Assumption: Entire dataset is considered for EDA
#	monthYear	count
#     	10-2015 1096952
#      	3-2016 1013888
#      	9-2015  939355
#     	11-2015  935365
#      	8-2015  902634
#      	4-2016  900709
#      	7-2015  884785
#      	5-2016  874469
#      	2-2016  840354
#     	1-2016  814181
#    	12-2015  767085
#     	6-2016  427117
#		6-2015  222040

PeriodwiseTicketCount_2016 <-summarize(groupBy(parkingVoilations_2016, parkingVoilations_2016$monthYear),
                                       count = n(parkingVoilations_2016$monthYear))

collect(arrange(PeriodwiseTicketCount_2016,desc(PeriodwiseTicketCount_2016$count)))

# Timeperiod Considerations  for Dataset for 2017: Most of the data is for the period: Jun-16 to June-17
# The data outside this timeframe is minimal (0.1%) and hence treated as data issues which should not impact the overall analysis by great extent.
# Assumption: Entire dataset is considered for EDA
#	monthYear	count
#     5-2017 1020244
#     10-2016  969330
#      3-2017  964737
#      9-2016  960537
#     11-2016  899357
#      4-2017  888402
#      1-2017  877365
#      6-2017  852187
#      2-2017  826967
#     8-2016  801258
#    12-2016  778704
#     7-2016  700475
#     6-2016  250934

PeriodWiseTicketCount_2017 <-summarize(groupBy(parkingVoilations_2017, parkingVoilations_2017$monthYear),
                                       count = n(parkingVoilations_2017$monthYear))

collect(arrange(PeriodWiseTicketCount_2017,desc(PeriodWiseTicketCount_2017$count)))

####################################################
#					1. Examine the data.
####################################################

# 1.1 Find total number of tickets for each year.
#			Ticket Count - 2015: 11809233
#			Ticket Count - 2016: 10626899
#			Ticket Count - 2017: 10803028

# examine the 2015 dataset
nrow(parkingVoilations_2015) # row count: 11809233
ncol(parkingVoilations_2015) # column count: 51

str(parkingVoilations_2015)
printSchema(parkingVoilations_2015)


# examine the size of 2016 dataset
nrow(parkingVoilations_2016) # row count: 10626899
ncol(parkingVoilations_2016) # column count: 51

str(parkingVoilations_2016)
printSchema(parkingVoilations_2016)

# examine the size of 2017 dataset
nrow(parkingVoilations_2017) # row count: 10803028
ncol(parkingVoilations_2017) # column count: 43

str(parkingVoilations_2017)
printSchema(parkingVoilations_2017)


# 1.2 Find out how many unique states the cars which got parking tickets came from.

# Unique states from which cars came in 2015 : 69
nrow(distinct(parkingVoilations_2015[,"Registration_State"]))
collect(distinct(parkingVoilations_2015[,"Registration_State"]))

# Unique states from which cars came in 2016 : 68
nrow(distinct(parkingVoilations_2016[,"Registration_State"]))
collect(distinct(parkingVoilations_2016[,"Registration_State"]))

# Unique states from which cars came in 2017 : 67
nrow(distinct(parkingVoilations_2017[,"Registration_State"]))
collect(distinct(parkingVoilations_2017[,"Registration_State"]))

# 1.3 Some parking tickets don’t have addresses on them, which is cause for concern. Find out how many such tickets there are.


# Count of tickets which don't have Address in 2015 : 4413
count_noAddr_2015<- SparkR::sql("SELECT count(Address) FROM parkingVoilations_2015_tbl where
                                Address==NULL OR length(Address)=0")
head(count_noAddr_2015)



# Count of tickets which don't have Address in 2016 : 6462
count_noAddr_2016<- SparkR::sql("SELECT count(Address) FROM parkingVoilations_2016_tbl where
                                Address==NULL OR length(Address)=0")
head(count_noAddr_2016)

# Count of tickets which don't have Address in 2017 : 2683
count_noAddr_2017<- SparkR::sql("SELECT count(Address) FROM parkingVoilations_2017_tbl where
                                Address==NULL OR length(Address)=0")
head(count_noAddr_2017)

#################################################### 

#				2. Aggregation tasks

####################################################

#2.1 How often does each violation code occur? (frequency of violation codes - find the top 5)

###### frequency of violation code in 2015
#	violation_code   count
#        21 		1630912
#        38 		1418627
#        14  		 988469
#        36  		 839197
#        37  		 795918
count_violationcode_2015<- SparkR::sql("SELECT violation_code, count(*) as count FROM 
                                       parkingVoilations_2015_tbl group by 
                                       violation_code order by count desc")

df_violationcode_2015 <- collect(count_violationcode_2015)

head(df_violationcode_2015,5)
ggplot(head(df_violationcode_2015,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")


###### frequency of violation code in 2016
#	violation_code   count
#         21 		1531587
#         36 		1253512
#         38 		1143696
#         14  		 875614
#         37  		 686610
count_violationcode_2016<- SparkR::sql("SELECT violation_code, count(*) as count FROM 
                                       parkingVoilations_2016_tbl group by 
                                       violation_code order by count desc")

df_violationcode_2016 <- collect(count_violationcode_2016)

head(df_violationcode_2016,5)
ggplot(head(df_violationcode_2016,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

###### frequency of violation code in 2017
#	violation_code   count
#          21 		1528588
#          36 		1400614
#          38 		1062304
#          14  		 893498
#          20  		 618593
count_violationcode_2017<- SparkR::sql("SELECT violation_code, count(*) as count FROM 
                                       parkingVoilations_2017_tbl group by 
                                       violation_code order by count desc")

df_violationcode_2017 <- collect(count_violationcode_2017)

head(df_violationcode_2017,5)
ggplot(head(df_violationcode_2017,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

# 2.2 How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)

###### frequency of parking ticket for each body type in 2015
#	 vehicle_body_type   count
#      SUBN 			3729346
#      4DSD 			3340014
#      VAN 				1709091
#      DELV  			 892781
#      SDN  			 524596

###### frequency of parking ticket for each vehicle make in 2015
#	 vehicle_make   count
#		FORD 		1521874
#       TOYOT 		1217087
#       HONDA 		1102614
#       NISSA  		 908783
#       CHEVR  		 897845
count_bodytype_2015<- SparkR::sql("SELECT vehicle_body_type, count(*) as count FROM 
                                  parkingVoilations_2015_tbl group by 
                                  vehicle_body_type order by count desc")

df_bodytype_2015 <- collect(count_bodytype_2015)
head(df_bodytype_2015,5)
ggplot(head(df_bodytype_2015,20), aes(x=vehicle_body_type,y=count))+ geom_bar(stat="identity")

count_make_2015<- SparkR::sql("SELECT vehicle_make, count(*) as count FROM 
                              parkingVoilations_2015_tbl group by 
                              vehicle_make order by count desc")

df_make_2015 <- collect(count_make_2015)
head(df_make_2015,5)
ggplot(head(df_make_2015,20), aes(x=vehicle_make,y=count))+ geom_bar(stat="identity")


###### frequency of parking ticket for each body type in 2016
#	 vehicle_body_type   count
#      SUBN 			3466037
#      4DSD 			2992107
#      VAN 				1518303
#      DELV  			 755282
#      SDN  			 424043

###### frequency of parking ticket for each vehicle make in 2016
#	 vehicle_make   count
#		  FORD 		1324774
#        TOYOT 		1154790
#        HONDA 		1014074
#        NISSA  	 834833
#        CHEVR  	 759663
count_bodytype_2016<- SparkR::sql("SELECT vehicle_body_type, count(*) as count FROM 
                                  parkingVoilations_2016_tbl group by 
                                  vehicle_body_type order by count desc")

df_bodytype_2016 <- collect(count_bodytype_2016)
head(df_bodytype_2016,5)
ggplot(head(df_bodytype_2016,20), aes(x=vehicle_body_type,y=count))+ geom_bar(stat="identity")

count_make_2016<- SparkR::sql("SELECT vehicle_make, count(*) as count FROM 
                              parkingVoilations_2016_tbl group by 
                              vehicle_make order by count desc")

df_make_2016 <- collect(count_make_2016)
head(df_make_2016,5)
ggplot(head(df_make_2016,20), aes(x=vehicle_make,y=count))+ geom_bar(stat="identity")


###### frequency of parking ticket for each body type in 2017
#	 vehicle_body_type   count
#      	SUBN 			3719802
#       4DSD 			3082020
#       VAN 			1411970
#		DELV  			 687330
#       SDN  			 438191

###### frequency of parking ticket for each vehicle make in 2017
#	 vehicle_make   count
#		 FORD 		1280958
#        TOYOT 		1211451
#        HONDA 		1079238
#        NISSA  	 918590
#        CHEVR  	 714655
count_bodytype_2017<- SparkR::sql("SELECT vehicle_body_type, count(*) as count FROM 
                                  parkingVoilations_2017_tbl group by 
                                  vehicle_body_type order by count desc")

df_bodytype_2017 <- collect(count_bodytype_2017)
head(df_bodytype_2017,5)
ggplot(head(df_bodytype_2017,20), aes(x=vehicle_body_type,y=count))+ geom_bar(stat="identity")


count_make_2017<- SparkR::sql("SELECT vehicle_make, count(*) as count FROM 
                              parkingVoilations_2017_tbl group by 
                              vehicle_make order by count desc")

df_make_2017 <- collect(count_make_2017)
head(df_make_2017,5)
ggplot(head(df_make_2017,20), aes(x=vehicle_make,y=count))+ geom_bar(stat="identity")


# 2.3 A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:
#	2.3.1 Violating Precincts (this is the precinct of the zone where the violation occurred)
# Ignoring Violation Precinct with value in all years dataset
# Frequencies of Violating Precincts in 2015
# 	Violation_Precinct   count
#   			19  	 598351
#				18  	 427510
#				14  	 409064
#	 			 1  	 329009
#				114 	 320963
count_violating_precinct_2015<- SparkR::sql("SELECT Violation_Precinct, count(Violation_Precinct) as count FROM 
                                            parkingVoilations_2015_tbl where Violation_Precinct!= 0 group by 
                                            Violation_Precinct order by count desc")

df_violating_precinct_2015 <- collect(count_violating_precinct_2015)
head(df_violating_precinct_2015,5)
ggplot(head(df_violating_precinct_2015,20), aes(x=Violation_Precinct,y=count))+ geom_bar(stat="identity")


# Frequencies of Violating Precincts in 2016
#	Violation_Precinct   count
#             19  		 554465
#             18  		 331704
#             14  		 324467
#			   1  		 303850
#			 114 		 291336
count_violating_precinct_2016<- SparkR::sql("SELECT Violation_Precinct, count(Violation_Precinct) as count FROM 
                                            parkingVoilations_2016_tbl where Violation_Precinct!= 0 group by 
                                            Violation_Precinct order by count desc")

df_violating_precinct_2016 <- collect(count_violating_precinct_2016)
head(df_violating_precinct_2016,5)
ggplot(head(df_violating_precinct_2016,20), aes(x=Violation_Precinct,y=count))+ geom_bar(stat="identity")

# Frequencies of Violating Precincts in 2017
# 		Violation_Precinct   count
#                 19  		 535671
#                 14  		 352450
#                  1  		 331810
#                 18  		 306920
# 				 114 		 296514
count_violating_precinct_2017<- SparkR::sql("SELECT Violation_Precinct, count(Violation_Precinct) as count FROM 
                                            parkingVoilations_2017_tbl where Violation_Precinct!= 0 group by 
                                            Violation_Precinct order by count desc")

df_violating_precinct_2017 <- collect(count_violating_precinct_2017)
head(df_violating_precinct_2017,5)
ggplot(head(df_violating_precinct_2017,20), aes(x=Violation_Precinct,y=count))+ geom_bar(stat="identity")

#	2.3.2 Issuing Precincts (this is the precinct that issued the ticket)

# Frequencies of Issuer Precincts in 2015
#	Issuer_Precinct   	count
#              19  		 579998
#              18  		 417329
#              14  		 392922
#               1  		 318778
#			  114 		 314437
count_issuer_precinct_2015<- SparkR::sql("SELECT Issuer_Precinct, count(Issuer_Precinct) as count FROM 
                                         parkingVoilations_2015_tbl where Issuer_Precinct!= 0 group by 
                                         Issuer_Precinct order by count desc")

df_issuer_precinct_2015 <- collect(count_issuer_precinct_2015)
head(df_issuer_precinct_2015,5)
ggplot(head(df_issuer_precinct_2015,20), aes(x=Issuer_Precinct,y=count))+ geom_bar(stat="identity")


# Frequencies of Issuer Precincts in 2016
#	Issuer_Precinct   	count
#              19  		 540569
#              18  		 323132
#              14  		 315311
#               1  		 295013
#			  114 		 286924
count_issuer_precinct_2016<- SparkR::sql("SELECT Issuer_Precinct, count(Issuer_Precinct) as count FROM 
                                         parkingVoilations_2016_tbl where Issuer_Precinct!= 0 group by 
                                         Issuer_Precinct order by count desc")

df_issuer_precinct_2016 <- collect(count_issuer_precinct_2016)
head(df_issuer_precinct_2016,5)
ggplot(head(df_issuer_precinct_2016,20), aes(x=Issuer_Precinct,y=count))+ geom_bar(stat="identity")

# Frequencies of Issuer_Precinct in 2017
# 		Violation_Precinct   count
#              19  			 521513
#              14  			 344977
#               1  			 321170
#              18  			 296553
#			  114 			 289950
count_issuer_precinct_2017<- SparkR::sql("SELECT Issuer_Precinct, count(Issuer_Precinct) as count FROM 
                                         parkingVoilations_2017_tbl where Issuer_Precinct!= 0 group by 
                                         Issuer_Precinct order by count desc")

df_issuer_precinct_2017 <- collect(count_issuer_precinct_2017)
head(df_issuer_precinct_2017,5)
ggplot(head(df_issuer_precinct_2017,20), aes(x=Issuer_Precinct,y=count))+ geom_bar(stat="identity")

# Find the violation code frequency across 3 precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?

# Voilation Code -14,69,46,38 and 37 are the top five violation codes across the three years in the top 3 precincts. These have exceptionally high frequency as well
# Top ten voilation codes are consistent across precincts as well

#2015 - Top 10 violation codes across the 3 issuer precincts - 19,18,15
# 		violation_code 		count
#1              14 			275108
#2              69 			150606
#3              38 			124459
#4              37  		97877
#5              31  		79315
#6              46  		75082
#7              16  		70935
#8              21  		67691
#9              47  		61436
#10             42  		52311
voilation_code_summary_TopPrecinct_2015<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                      parkingVoilations_2015_tbl where Issuer_Precinct!= 0 
                                                      AND Issuer_Precinct in (19,14,18)
                                                      group by violation_code order by count desc")

df_voilation_code_summary_TopPrecinct_2015 <- collect(voilation_code_summary_TopPrecinct_2015)
head(df_voilation_code_summary_TopPrecinct_2015,10)
ggplot(head(df_voilation_code_summary_TopPrecinct_2015,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

#2016 - Top 10 violation codes across the 3 issuer precincts - 19,18,14
# 		violation_code 		count
#1              14 			224025
#2              69 			119857
#3              46  		98097
#4              38  		97168
#5              37  		86040
#6              21  		64621
#7              31  		61053
#8              16  		60635
#9              47  		50478
#10             42  		42696

voilation_code_summary_TopPrecinct_2016<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                      parkingVoilations_2016_tbl where Issuer_Precinct!= 0 
                                                      AND Issuer_Precinct in (19,14,18)
                                                      group by violation_code order by count desc")

df_voilation_code_summary_TopPrecinct_2016 <- collect(voilation_code_summary_TopPrecinct_2016)
head(df_voilation_code_summary_TopPrecinct_2016,10)
ggplot(head(df_voilation_code_summary_TopPrecinct_2016,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

#2017 - Top 10 violation codes across the 3 issuer precincts - 19,14,1
# 		violation_code 		count
#1              14 			204922
#2              46 			122359
#3              38  		94937
#4              37  		88475
#5              69  		73736
#6              16  		71797
#7              21  		65026
#8              20  		59770
#9              31  		54198
#10             40  		35808

voilation_code_summary_TopPrecinct_2017<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                      parkingVoilations_2017_tbl where Issuer_Precinct!= 0 
                                                      AND Issuer_Precinct in (19,14,1)
                                                      group by violation_code order by count desc")

df_voilation_code_summary_TopPrecinct_2017 <- collect(voilation_code_summary_TopPrecinct_2017)
head(df_voilation_code_summary_TopPrecinct_2017,10)
ggplot(head(df_voilation_code_summary_TopPrecinct_2017,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

# 2015 - violation code frequency across 3 precincts which have issued the most number of tickets 
# Top 3 Issuing Precinct in 2015: 19, 18, 14

#  Summarizing top 5 voilation codes for 2015 for Issuer Precinct: 19
# violation_code 		count
#            38 		97154
#            37 		85007
#            14 		64133
#            21 		60215
#            16 		59675
#			 46 		46363
#            20 		34467
#            40 		29825
#            71 		18163
#            19 		15809
voilation_code_summary_precinct19_2015<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                     parkingVoilations_2015_tbl where Issuer_Precinct!= 0 AND Issuer_Precinct=19
                                                     group by violation_code order by count desc")

df_voilation_code_summary_precinct19_2015 <- collect(voilation_code_summary_precinct19_2015)
head(df_voilation_code_summary_precinct19_2015,10)
ggplot(head(df_voilation_code_summary_precinct19_2015,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

#  Summarizing top 5 voilation codes for 2015 for Issuer Precinct: 18
# violation_code 		count
#            14 		129079
#			 69  		 60618
#            31  		 32925
#            47  		 30872
#            42  		 21026
#			 38  		 20013
#            46  		 17866
#            84  		 10532
#            19   		  9212
#            37   		  9045
voilation_code_summary_precinct18_2015<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                     parkingVoilations_2015_tbl where Issuer_Precinct!= 0 AND Issuer_Precinct=18
                                                     group by violation_code order by count desc")

df_voilation_code_summary_precinct18_2015 <- collect(voilation_code_summary_precinct18_2015)
head(df_voilation_code_summary_precinct18_2015,10)
ggplot(head(df_voilation_code_summary_precinct18_2015,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")


#  Summarizing top 5 voilation codes for 2015 for Issuer Precinct: 14
# violation_code 		count
#             69 		84895
#             14 		81896
#             31 		43928
#             42 		29868
#             47 		28814
#			  46 		10853
#             17  		 9607
#             19  		 9577
#             84  		 9130
#             38  		 7292
voilation_code_summary_precinct14_2015<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                     parkingVoilations_2015_tbl where Issuer_Precinct!= 0 AND Issuer_Precinct=14
                                                     group by violation_code order by count desc")

df_voilation_code_summary_precinct14_2015 <- collect(voilation_code_summary_precinct14_2015)
head(df_voilation_code_summary_precinct14_2015,10)
ggplot(head(df_voilation_code_summary_precinct14_2015,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

# 2016 - violation code frequency across 3 precincts which have issued the most number of tickets 
# Top 3 Issuing Precinct in 2016: 19, 18, 14

#  Summarizing top 5 voilation codes for 2016 for Issuer Precinct: 19
# violation_code 		count
#            38 		77183
#            37 		75641
#            46 		73016
#            14 		61742
#            21 		58719
#			 16 		52354
#            20 		29280
#            40 		21903
#            71 		15190
#            19 		13661
voilation_code_summary_precinct19_2016<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                     parkingVoilations_2016_tbl where Issuer_Precinct!= 0 AND Issuer_Precinct=19
                                                     group by violation_code order by count desc")

df_voilation_code_summary_precinct19_2016 <- collect(voilation_code_summary_precinct19_2016)
head(df_voilation_code_summary_precinct19_2016,10)
ggplot(head(df_voilation_code_summary_precinct19_2016,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

#  Summarizing top 5 voilation codes for 2016 for Issuer Precinct: 18
# violation_code 		count
#            14 		99857
#			 69  		47881
#            47 		24009
#            31 		22809
#            42 		17678
# 			 46 		14674
#            38 		14203
#            84  		 9022
#            19  		 7490
#            20  		 7243
voilation_code_summary_precinct18_2016<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                     parkingVoilations_2016_tbl where Issuer_Precinct!= 0 AND Issuer_Precinct=18
                                                     group by violation_code order by count desc")

df_voilation_code_summary_precinct18_2016 <- collect(voilation_code_summary_precinct18_2016)
head(df_voilation_code_summary_precinct18_2016,10)
ggplot(head(df_voilation_code_summary_precinct18_2016,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

#  Summarizing top 5 voilation codes for 2016 for Issuer Precinct: 14
# violation_code 		count
#             69 		67932
#             14 		62426
#             31 		35711
#             47 		24450
#             42 		23662
#			  46 		10407
#             84  		 9233
#             17  		 8105
#             19  		 8057
#			  82  		 6275
voilation_code_summary_precinct14_2016<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                     parkingVoilations_2016_tbl where Issuer_Precinct!= 0 AND Issuer_Precinct=14
                                                     group by violation_code order by count desc")

df_voilation_code_summary_precinct14_2016 <- collect(voilation_code_summary_precinct14_2016)
head(df_voilation_code_summary_precinct14_2016,10)
ggplot(head(df_voilation_code_summary_precinct14_2016,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

# 2017 - violation code frequency across 3 precincts which have issued the most number of tickets 
# Top 3 Issuing Precinct in 2017: 19, 14, 1

#  Summarizing top 5 voilation codes for 2017 for Issuer Precinct: 19
# violation_code 		count
#             46 		86390
#             37 		72437
#             38 		72344
#             14 		57563
#             21 		54700
#			  16 		31353
#             20 		27352
#             40 		21513
#             71 		15107
#             19 		12896

voilation_code_summary_precinct19_2017<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                     parkingVoilations_2017_tbl where Issuer_Precinct!= 0 AND Issuer_Precinct=19
                                                     group by violation_code order by count desc")

df_voilation_code_summary_precinct19_2017 <- collect(voilation_code_summary_precinct19_2017)
head(df_voilation_code_summary_precinct19_2017,10)
ggplot(head(df_voilation_code_summary_precinct19_2017,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

#  Summarizing top 5 voilation codes for 2017 for Issuer Precinct: 14
# violation_code 		count
#             14 		73837
#             69 		58026
#             31 		39857
#             47 		30540
#             42 		20663
#			  46 		13435
#             84 		11111
#             19 		11062
#             82  		 8853
#             17  		 6160
voilation_code_summary_precinct14_2017<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                     parkingVoilations_2017_tbl where Issuer_Precinct!= 0 AND Issuer_Precinct=14
                                                     group by violation_code order by count desc")

df_voilation_code_summary_precinct14_2017 <- collect(voilation_code_summary_precinct14_2017)
head(df_voilation_code_summary_precinct14_2017,10)
ggplot(head(df_voilation_code_summary_precinct14_2017,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")

#  Summarizing top 5 voilation codes for 2017 for Issuer Precinct: 1
# violation_code 		count
#             14 		73522
#             16 		38937
#             20 		27841
#             46 		22534
#             38 		16989
#			  17 		13811
#             37 		13513
#             69 		11165
#             31 		11047
#             19 		10487

voilation_code_summary_precinct1_2017<- SparkR::sql("SELECT violation_code, count(violation_code) as count FROM 
                                                    parkingVoilations_2017_tbl where Issuer_Precinct!= 0 AND Issuer_Precinct=1
                                                    group by violation_code order by count desc")

df_voilation_code_summary_precinct1_2017 <- collect(voilation_code_summary_precinct1_2017)
head(df_voilation_code_summary_precinct1_2017,10)
ggplot(head(df_voilation_code_summary_precinct1_2017,20), aes(x=violation_code,y=count))+ geom_bar(stat="identity")


#2.5 Properties of parking violations across different times of the day
####################################################################################################################################

# 2.5.1 : The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.

# split the violation time into hours and time period(AM/PM)

parkingVoilations_2017$hours <- substr(parkingVoilations_2017$Violation_Time,1,2)
parkingVoilations_2017$time_period <- substr(parkingVoilations_2017$Violation_Time,6,6)
parkingVoilations_2017 <- withColumn(parkingVoilations_2017, "hours", cast(parkingVoilations_2017$hours,"int"))
parkingVoilations_2017$hours_corrected <- parkingVoilations_2017$hours%%12

parkingVoilations_2016$hours <- substr(parkingVoilations_2016$Violation_Time,1,2)
parkingVoilations_2016$time_period <- substr(parkingVoilations_2016$Violation_Time,6,6)
parkingVoilations_2016 <- withColumn(parkingVoilations_2016, "hours", cast(parkingVoilations_2016$hours,"int"))
parkingVoilations_2016$hours_corrected <- parkingVoilations_2016$hours%%12

parkingVoilations_2015$hours <- substr(parkingVoilations_2015$Violation_Time,1,2)
parkingVoilations_2015$time_period <- substr(parkingVoilations_2015$Violation_Time,6,6)
parkingVoilations_2015 <- withColumn(parkingVoilations_2015, "hours", cast(parkingVoilations_2015$hours,"int"))
parkingVoilations_2015$hours_corrected <- parkingVoilations_2015$hours%%12

#The hour column has few values like 58, 34 etc,which makes no sense, so we are converting it back 12 hour format by taking modulus of 12
# There are few values (marginal number) where hour column some special characters which will be  ignored by binning them to a seperate category.


# 2.5.2 : Find a way to deal with missing values, if any.

count(where(parkingVoilations_2017, (isNull(parkingVoilations_2017$Violation_Time)) | (parkingVoilations_2017$hours == "0" )))

missing_df <- where(parkingVoilations_2017, (isNull(parkingVoilations_2017$Violation_Time) | (parkingVoilations_2017$hours == "0" )))

#	there are 63 missing values out of 10803028 values which is 0.0006% 

count(where(parkingVoilations_2016, (isNull(parkingVoilations_2016$Violation_Time))))
#	there are 4280 missing values  out of 10626899 which is 0.0004%

count(where(parkingVoilations_2015, (isNull(parkingVoilations_2015$Violation_Time))))
# there are 1715 missing values out of 11809233 values which is 0.00015%

#the missing values can be categorised into a category OR 
#In our analysis, since the Percnetage of missing values is marginal we can choose to omit those since these values are too low to make an impact

#Also we have some invalid time values which are also ignored, since the percentage is less. 


# 2.5.3 : Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
#		

#6 bins are created.   as below. 

createOrReplaceTempView(parkingVoilations_2015,"parkingVoilations_2015_tbl")
createOrReplaceTempView(parkingVoilations_2016,"parkingVoilations_2016_tbl")
createOrReplaceTempView(parkingVoilations_2017,"parkingVoilations_2017_tbl")

####################################################################################################################################
#Analysis for the year 2017
####################################################################################################################################
time_bin <- SparkR::sql("SELECT violation_time, CASE  WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='A') THEN  '1'\
                        WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='A') THEN  '2'\
                        WHEN (hours_corrected IN (8,9,10,11) AND  time_period=='A') THEN  '3'\
                        WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='P') THEN  '4'\
                        WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='P') THEN  '5'\
                        WHEN (hours_corrected IN (8,9,10,11)AND time_period=='P') THEN  '6'\
                        ELSE '7' END  as bin_number FROM parkingVoilations_2017_tbl")
head(time_bin,5)	


####################################################################################################################################
#For each of these groups, find the 3 most commonly occurring violations						  
####################################################################################################################################
timebin_1_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2017_tbl
                                    where hours_corrected IN (0,1,2,3)  AND time_period=='A'
                                    group by violation_code
                                    order by no_of_violations desc")

head(timebin_1_violations,3)
###### 3 most commonly occurring violations	for the time bin - 1 (00-03 AM) for the year 2017
#		violation_code   no_of_violations
#			  21          77461
#             40          50948
#             78          32243						
####################################################################################################################################						

####################################################################################################################################
timebin_2_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2017_tbl 
                                    where hours_corrected IN (4,5,6,7)  AND time_period=='A'
                                    group by violation_code
                                    order by no_of_violations desc")						
head(timebin_2_violations,3)
###### 3 most commonly occurring violations	for the time bin - 2 (04-07 AM) for the year 2017
#		violation_code   no_of_violations
#             14           141276
#             21           119469
#             40           112186
####################################################################################################################################

####################################################################################################################################
timebin_3_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2017_tbl 
                                    where hours_corrected IN (8,9,10,11) AND  time_period=='A'
                                    group by violation_code
                                    order by no_of_violations desc")
head(timebin_3_violations,3)
###### 3 most commonly occurring violations	for the time bin - 3 (08-11 AM) for the year 2017
#		violation_code   no_of_violations
#             21          1182689
#             36           751422
#             38           346518
####################################################################################################################################

####################################################################################################################################
timebin_4_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2017_tbl 
                                    where hours_corrected IN (0,1,2,3)  AND time_period=='P'
                                    group by violation_code
                                    order by no_of_violations desc")
head(timebin_4_violations,3)
###### 3 most commonly occurring violations	for the time bin - 4 (00-03 PM) for the year 2017
#		violation_code   no_of_violations
#             36           588395
#             38           462758
#             37           337075
####################################################################################################################################

####################################################################################################################################
timebin_5_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2017_tbl 
                                    where hours_corrected IN (4,5,6,7)  AND time_period=='P'
                                    group by violation_code
                                    order by no_of_violations desc")

head(timebin_5_violations,3)
###### 3 most commonly occurring violations	for the time bin - 5 (04-07 PM) for the year 2017
#		violation_code   no_of_violations
#             38           203234
#             37           145784
#             14           144751									
####################################################################################################################################

####################################################################################################################################									
timebin_6_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2017_tbl 
                                    where hours_corrected IN (8,9,10,11)AND time_period=='P'
                                    group by violation_code
                                    order by no_of_violations desc")									

head(timebin_6_violations,3)									
###### 3 most commonly occurring violations	for the time bin - 5 (08-11 PM) for the year 2017
#		violation_code   no_of_violations
#              7            65593
#             38            47030
#             14            44780
####################################################################################################################################


####################################################################################################################################
#Analysis for the year 2016
####################################################################################################################################
time_bin <- SparkR::sql("SELECT violation_time, CASE  WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='A') THEN  '1'\
                        WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='A') THEN  '2'\
                        WHEN (hours_corrected IN (8,9,10,11) AND  time_period=='A') THEN  '3'\
                        WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='P') THEN  '4'\
                        WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='P') THEN  '5'\
                        WHEN (hours_corrected IN (8,9,10,11)AND time_period=='P') THEN  '6'\
                        ELSE '7' END  as bin_number FROM parkingVoilations_2016_tbl")
head(time_bin,5)							  
####################################################################################################################################
timebin_1_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2016_tbl
                                    where hours_corrected IN (0,1,2,3)  AND time_period=='A'
                                    group by violation_code
                                    order by no_of_violations desc")

head(timebin_1_violations,3)
###### 3 most commonly occurring violations	for the time bin - 1 (00-03 AM) for the year 2016
#		violation_code   no_of_violations					
#             21            72109
#             40            42098
#             78            32806
####################################################################################################################################						

####################################################################################################################################
timebin_2_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2016_tbl 
                                    where hours_corrected IN (4,5,6,7)  AND time_period=='A'
                                    group by violation_code
                                    order by no_of_violations desc")						
head(timebin_2_violations,3)
###### 3 most commonly occurring violations	for the time bin - 2 (04-07 AM) for the year 2016
#		violation_code   no_of_violations
#             14           140111
#             21           114029
#             40            91692
####################################################################################################################################

####################################################################################################################################
timebin_3_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2016_tbl 
                                    where hours_corrected IN (8,9,10,11) AND  time_period=='A'
                                    group by violation_code
                                    order by no_of_violations desc")
head(timebin_3_violations,3)
###### 3 most commonly occurring violations	for the time bin - 3 (08-11 AM) for the year 2016
#		violation_code   no_of_violations
#             21          1209243
#             36           586791
#             38           388099
####################################################################################################################################

####################################################################################################################################
timebin_4_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2016_tbl 
                                    where hours_corrected IN (0,1,2,3)  AND time_period=='P'
                                    group by violation_code
                                    order by no_of_violations desc")
head(timebin_4_violations,3)
###### 3 most commonly occurring violations	for the time bin - 4 (00-03 PM) for the year 2016
#		violation_code   no_of_violations
#             36           545717
#             38           488302
#             37           383362
####################################################################################################################################

####################################################################################################################################
timebin_5_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2016_tbl 
                                    where hours_corrected IN (4,5,6,7)  AND time_period=='P'
                                    group by violation_code
                                    order by no_of_violations desc")

head(timebin_5_violations,3)
###### 3 most commonly occurring violations	for the time bin - 5 (04-07 PM) for the year 2016
#		violation_code   no_of_violations
#             38           211267
#             37           161655
#             14           134984									
####################################################################################################################################

####################################################################################################################################									
timebin_6_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2016_tbl 
                                    where hours_corrected IN (8,9,10,11)AND time_period=='P'
                                    group by violation_code
                                    order by no_of_violations desc")									

head(timebin_6_violations,3)									
###### 3 most commonly occurring violations	for the time bin - 5 (08-11 PM) for the year 2016
#		violation_code   no_of_violations
#              7            60924
#             38            53174
#             40            44979
####################################################################################################################################

####################################################################################################################################
#Analysis for the year 2015
####################################################################################################################################
time_bin <- SparkR::sql("SELECT violation_time, CASE  WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='A') THEN  '1'\
                        WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='A') THEN  '2'\
                        WHEN (hours_corrected IN (8,9,10,11) AND  time_period=='A') THEN  '3'\
                        WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='P') THEN  '4'\
                        WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='P') THEN  '5'\
                        WHEN (hours_corrected IN (8,9,10,11)AND time_period=='P') THEN  '6'\
                        ELSE '7' END  as bin_number FROM parkingVoilations_2015_tbl")
head(time_bin,5)							  
####################################################################################################################################
timebin_1_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2015_tbl
                                    where hours_corrected IN (0,1,2,3)  AND time_period=='A'
                                    group by violation_code
                                    order by no_of_violations desc")

head(timebin_1_violations,3)
###### 3 most commonly occurring violations	for the time bin - 1 (00-03 AM) for the year 2015
#		violation_code   no_of_violations					
#             21            74054
#             40            47141
#             78            42724
####################################################################################################################################						

####################################################################################################################################
timebin_2_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2015_tbl 
                                    where hours_corrected IN (4,5,6,7)  AND time_period=='A'
                                    group by violation_code
                                    order by no_of_violations desc")						
head(timebin_2_violations,3)
###### 3 most commonly occurring violations	for the time bin - 2 (04-07 AM) for the year 2015
#		violation_code   no_of_violations
#             14           143264
#             21           118316
#             40            98134
####################################################################################################################################

####################################################################################################################################
timebin_3_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2015_tbl 
                                    where hours_corrected IN (8,9,10,11) AND  time_period=='A'
                                    group by violation_code
                                    order by no_of_violations desc")
head(timebin_3_violations,3)
###### 3 most commonly occurring violations	for the time bin - 3 (08-11 AM) for the year 2015
#		violation_code   no_of_violations
#             21          1291540
#             38           480358
#             36           396838
####################################################################################################################################

####################################################################################################################################
timebin_4_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2015_tbl 
                                    where hours_corrected IN (0,1,2,3)  AND time_period=='P'
                                    group by violation_code
                                    order by no_of_violations desc")
head(timebin_4_violations,3)
###### 3 most commonly occurring violations	for the time bin - 4 (00-03 PM) for the year 2015
#		violation_code   no_of_violations
#             38           609518
#             37           446469
#             36           357306
####################################################################################################################################

####################################################################################################################################
timebin_5_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2015_tbl 
                                    where hours_corrected IN (4,5,6,7)  AND time_period=='P'
                                    group by violation_code
                                    order by no_of_violations desc")

head(timebin_5_violations,3)
###### 3 most commonly occurring violations	for the time bin - 5 (04-07 PM) for the year 2015
#		violation_code   no_of_violations
#             38           258838
#             37           187186
#              7           182347									
####################################################################################################################################

####################################################################################################################################									
timebin_6_violations <- SparkR::sql("SELECT violation_code, count(*) as no_of_violations 
                                    FROM parkingVoilations_2015_tbl 
                                    where hours_corrected IN (8,9,10,11)AND time_period=='P'
                                    group by violation_code
                                    order by no_of_violations desc")									

head(timebin_6_violations,3)									
###### 3 most commonly occurring violations	for the time bin - 5 (08-11 PM) for the year 2015
#		violation_code   no_of_violations
#              7            89813
#             38            66023
#             40            49931
####################################################################################################################################

# 2.5.4 : Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)

####################################################################################################################################
# Analysis for the year 2017
####################################################################################################################################
# We have learnt from our earlier analysis the top 3 violoation codes for the year 2017 are 21, 36, 38
# So for these top 3 violation codes of 2017, the most common times of the day in terms of bin created earlier: 

common_times_of_the_day <-SparkR::sql("SELECT CASE    WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='A') THEN  '1'\
                                      WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='A') THEN  '2'\
                                      WHEN (hours_corrected IN (8,9,10,11) AND  time_period=='A') THEN  '3'\
                                      WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='P') THEN  '4'\
                                      WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='P') THEN  '5'\
                                      WHEN (hours_corrected IN (8,9,10,11)AND time_period=='P') THEN  '6'\
                                      ELSE '7' END  as bin_number, count(*) as no_of_violations 
                                      FROM parkingVoilations_2017_tbl 
                                      where violation_code IN ('21','36','38')
                                      group by bin_number
                                      having bin_number !='7'
                                      order by no_of_violations desc")

head(common_times_of_the_day,3)

###### 3 most common time of the day with respect to the most common violocation codes for the year 2017 
#	Time of the day(Bin)	  No of Violations
#    3(8-11 AM)         		2280629
#    4(12-3 PM)         		1199181
#    5(4-7 PM)          		230658
####################################################################################################################################

####################################################################################################################################
# Analysis for the year 2016
####################################################################################################################################
# We have learnt from our earlier analysis the top 3 violoation codes for the year 2016 are 21, 36, 38
# So for these top 3 violation codes of 2016, the most common times of the day in terms of bin created earlier: 

common_times_of_the_day <-SparkR::sql("SELECT CASE    WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='A') THEN  '1'\
                                      WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='A') THEN  '2'\
                                      WHEN (hours_corrected IN (8,9,10,11) AND  time_period=='A') THEN  '3'\
                                      WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='P') THEN  '4'\
                                      WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='P') THEN  '5'\
                                      WHEN (hours_corrected IN (8,9,10,11)AND time_period=='P') THEN  '6'\
                                      ELSE '7' END  as bin_number, count(*) as no_of_violations 
                                      FROM parkingVoilations_2016_tbl 
                                      where violation_code IN ('21','36','38')
                                      group by bin_number
                                      having bin_number !='7'
                                      order by no_of_violations desc")

head(common_times_of_the_day,3)

###### 3 most common time of the day with respect to the most common violocation codes for the year 2016 
#	Time of the day(Bin)	  No of Violations
#    3(8-11 AM)         		2184133
#    4(12-3 PM)         		1168366
#    5(4-7 PM)          		253093
####################################################################################################################################

####################################################################################################################################
# Analysis for the year 2015
####################################################################################################################################
# We have learnt from our earlier analysis the top 3 violoation codes for the year 2015 are 21, 38, 14
# So for these top 3 violation codes of 2015, the most common times of the day in terms of bin created earlier: 

common_times_of_the_day <-SparkR::sql("SELECT CASE    WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='A') THEN  '1'\
                                      WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='A') THEN  '2'\
                                      WHEN (hours_corrected IN (8,9,10,11) AND  time_period=='A') THEN  '3'\
                                      WHEN (hours_corrected IN (0,1,2,3)  AND time_period=='P') THEN  '4'\
                                      WHEN (hours_corrected IN (4,5,6,7)  AND time_period=='P') THEN  '5'\
                                      WHEN (hours_corrected IN (8,9,10,11)AND time_period=='P') THEN  '6'\
                                      ELSE '7' END  as bin_number, count(*) as no_of_violations 
                                      FROM parkingVoilations_2015_tbl 
                                      where violation_code IN ('21','38','14')
                                      group by bin_number
                                      having bin_number !='7'
                                      order by no_of_violations desc")

head(common_times_of_the_day,3)

###### 3 most common time of the day with respect to the most common violocation codes for the year 2015 
#	Time of the day(Bin)	  No of Violations
#    3(8-11 AM)         		2088907
#    4(12-3 PM)         		1039860
#    5(4-7 PM)          		420188
####################################################################################################################################

# 2.6 Let’s try and find some seasonality in this data
#First, divide the year into some number of seasons, and find frequencies of tickets for each season.
#
# Seasons are divided into groups of 3 months.
# Season-1 : Month 1 - Month 3
# Season-2 : Month 4 - Month 6
# Season-3 : Month 7 - Month 9
# Season-4 : Month 10 - Month 12
#
# No specific seasonality pattern is seen across the 4 seasons in the three years
#
# Seasonality in 2015
parkingVoilations_2015$ Seasons <-
  ifelse(parkingVoilations_2015$monthofIssue >=1 & parkingVoilations_2015$monthofIssue <=3, 'Season-1', 
         ifelse(parkingVoilations_2015$monthofIssue >=4 & parkingVoilations_2015$monthofIssue <=6, 'Season-2',
                ifelse(parkingVoilations_2015$monthofIssue >=7 & parkingVoilations_2015$monthofIssue <=9, 'Season-3',
                       ifelse(parkingVoilations_2015$monthofIssue >=10 & parkingVoilations_2015$monthofIssue <=12, 'Season-4','NA'))))


#	Seasons   	count
# Season-2 		3268456
# Season-1 		3089975
# Season-3 		2911162
# Season-4 		2539640


SeasonwiseTicketCount_2015 <-summarize(groupBy(parkingVoilations_2015, parkingVoilations_2015$Seasons), count = n(parkingVoilations_2015$Seasons))
head(arrange(SeasonwiseTicketCount_2015,desc(SeasonwiseTicketCount_2015$count)))

# Seasonality in 2016
parkingVoilations_2016$ Seasons <-
  ifelse(parkingVoilations_2016$monthofIssue >=1 & parkingVoilations_2016$monthofIssue <=3, 'Season-1', 
         ifelse(parkingVoilations_2016$monthofIssue >=4 & parkingVoilations_2016$monthofIssue <=6, 'Season-2',
                ifelse(parkingVoilations_2016$monthofIssue >=7 & parkingVoilations_2016$monthofIssue <=9, 'Season-3',
                       ifelse(parkingVoilations_2016$monthofIssue >=10 & parkingVoilations_2016$monthofIssue <=12, 'Season-4','NA'))))


#   Seasons   		count
#  Season-4 		2801028
#  Season-3 		2728663
#  Season-1 		2671331
#  Season-2 		2425877

SeasonwiseTicketCount_2016 <-summarize(groupBy(parkingVoilations_2016, parkingVoilations_2016$Seasons), count = n(parkingVoilations_2016$Seasons))
head(arrange(SeasonwiseTicketCount_2016,desc(SeasonwiseTicketCount_2016$count)))

# Seasonality in 2017

parkingVoilations_2017$Seasons <-
  ifelse(parkingVoilations_2017$monthofIssue >=1 & parkingVoilations_2017$monthofIssue <=3, 'Season-1', 
         ifelse(parkingVoilations_2017$monthofIssue >=4 & parkingVoilations_2017$monthofIssue <=6, 'Season-2',
                ifelse(parkingVoilations_2017$monthofIssue >=7 & parkingVoilations_2017$monthofIssue <=9, 'Season-3',
                       ifelse(parkingVoilations_2017$monthofIssue >=10 & parkingVoilations_2017$monthofIssue <=12, 'Season-4','NA'))))

#    Seasons   		count
#	Season-2 		3018840
# 	Season-1 		2671332
# 	Season-4 		2648920
# 	Season-3 		2463936


SeasonwiseTicketCount_2017 <-summarize(groupBy(parkingVoilations_2017, parkingVoilations_2017$Seasons), count = n(parkingVoilations_2017$Seasons))
head(arrange(SeasonwiseTicketCount_2017,desc(SeasonwiseTicketCount_2017$count)))



#Then, find the 3 most common violations for each of these season

# 3 most common voilation codes for all seasons in 2015

# For Season 1 - 3 most common violation code
# Violation_Code  	count
#             38 	419424
#             21 	370713
#             14 	271353
Season1_2015 <- subset(parkingVoilations_2015,parkingVoilations_2015$Seasons=='Season-1')
Season1_ViolationCount_2015 <-summarize(groupBy(Season1_2015, Season1_2015$Violation_Code), count = n(Season1_2015$Violation_Code))
df_Season1_ViolationCount_2015<-collect(arrange(Season1_ViolationCount_2015,desc(Season1_ViolationCount_2015$count)))
head(df_Season1_ViolationCount_2015,3)
ggplot(head(df_Season1_ViolationCount_2015,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")


# For Season 2 - 3 most common violation code
# Violation_Code  	count
#             21 	471586
#             38 	346719
#             14 	262602
Season2_2015 <- subset(parkingVoilations_2015,parkingVoilations_2015$Seasons=='Season-2')
Season2_ViolationCount_2015 <-summarize(groupBy(Season2_2015, Season2_2015$Violation_Code), count = n(Season2_2015$Violation_Code))
df_Season2_ViolationCount_2015<-collect(arrange(Season2_ViolationCount_2015,desc(Season2_ViolationCount_2015$count)))
head(df_Season2_ViolationCount_2015,3)
ggplot(head(df_Season2_ViolationCount_2015,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")

# For Season 3 - 3 most common violation code
# Violation_Code  	count
#             21 	412078
#             38 	352481
#             14 	240742
Season3_2015 <- subset(parkingVoilations_2015,parkingVoilations_2015$Seasons=='Season-3')
Season3_ViolationCount_2015 <-summarize(groupBy(Season3_2015, Season3_2015$Violation_Code), count = n(Season3_2015$Violation_Code))
df_Season3_ViolationCount_2015<-collect(arrange(Season3_ViolationCount_2015,desc(Season3_ViolationCount_2015$count)))
head(df_Season3_ViolationCount_2015,3)
ggplot(head(df_Season3_ViolationCount_2015,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")

# For Season 4 - 3 most common violation code
# Violation_Code  	count
#             21 	376535
#             38 	300003
#             14 	213772
Season4_2015 <- subset(parkingVoilations_2015,parkingVoilations_2015$Seasons=='Season-4')
Season4_ViolationCount_2015 <-summarize(groupBy(Season4_2015, Season4_2015$Violation_Code), count = n(Season4_2015$Violation_Code))
df_Season4_ViolationCount_2015<-collect(arrange(Season4_ViolationCount_2015,desc(Season4_ViolationCount_2015$count)))
head(df_Season4_ViolationCount_2015,3)
ggplot(head(df_Season4_ViolationCount_2015,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")


# 3 most common voilation codes for all seasons in 2016

# For Season 1 - 3 most common violation code
# Violation_Code  	count
#             21 	349644
#             36 	341787
#             38 	308999
Season1_2016 <- subset(parkingVoilations_2016,parkingVoilations_2016$Seasons=='Season-1')
Season1_ViolationCount_2016 <-summarize(groupBy(Season1_2016, Season1_2016$Violation_Code), count = n(Season1_2016$Violation_Code))
df_Season1_ViolationCount_2016<-collect(arrange(Season1_ViolationCount_2016,desc(Season1_ViolationCount_2016$count)))
head(df_Season1_ViolationCount_2016,3)
ggplot(head(df_Season1_ViolationCount_2016,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")

# For Season 2 - 3 most common violation code
# Violation_Code  	count
#             21 	348473
#             36 	294015
#             38 	254909
Season2_2016 <- subset(parkingVoilations_2016,parkingVoilations_2016$Seasons=='Season-2')
Season2_ViolationCount_2016 <-summarize(groupBy(Season2_2016, Season2_2016$Violation_Code), count = n(Season2_2016$Violation_Code))
df_Season2_ViolationCount_2016<-collect(arrange(Season2_ViolationCount_2016,desc(Season2_ViolationCount_2016$count)))
head(df_Season2_ViolationCount_2016,3)
ggplot(head(df_Season2_ViolationCount_2016,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")

# For Season 3 - 3 most common violation code
# Violation_Code  	count
#             21 	403720
#             38 	305360
#             14 	234943
Season3_2016 <- subset(parkingVoilations_2016,parkingVoilations_2016$Seasons=='Season-3')
Season3_ViolationCount_2016 <-summarize(groupBy(Season3_2016, Season3_2016$Violation_Code), count = n(Season3_2016$Violation_Code))
df_Season3_ViolationCount_2016<-collect(arrange(Season3_ViolationCount_2016,desc(Season3_ViolationCount_2016$count)))
head(df_Season3_ViolationCount_2016,3)
ggplot(head(df_Season3_ViolationCount_2016,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")

# For Season 4 - 3 most common violation code
# Violation_Code  	count
#             36 	433966
#             21 	429750
#             38 	274428
Season4_2016 <- subset(parkingVoilations_2016,parkingVoilations_2016$Seasons=='Season-4')
Season4_ViolationCount_2016 <-summarize(groupBy(Season4_2016, Season4_2016$Violation_Code), count = n(Season4_2016$Violation_Code))
df_Season4_ViolationCount_2016<-collect(arrange(Season4_ViolationCount_2016,desc(Season4_ViolationCount_2016$count)))
head(df_Season4_ViolationCount_2016,3)
ggplot(head(df_Season4_ViolationCount_2016,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")


# 3 most common voilation codes for all seasons in 2017

# For Season 1 - 3 most common violation code
# Violation_Code  	count
#             21 	374202
#             36 	348240
#             38 	287017
Season1_2017 <- subset(parkingVoilations_2017,parkingVoilations_2017$Seasons=='Season-1')
Season1_ViolationCount_2017 <-summarize(groupBy(Season1_2017, Season1_2017$Violation_Code), count = n(Season1_2017$Violation_Code))
df_Season1_ViolationCount_2017<-collect(arrange(Season1_ViolationCount_2017,desc(Season1_ViolationCount_2017$count)))
head(df_Season1_ViolationCount_2017,3)
ggplot(head(df_Season1_ViolationCount_2017,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")

# For Season 2 - 3 most common violation code
# Violation_Code  	count
#             21 	421184
#             36 	369902
#             38 	266909
Season2_2017 <- subset(parkingVoilations_2017,parkingVoilations_2017$Seasons=='Season-2')
Season2_ViolationCount_2017 <-summarize(groupBy(Season2_2017, Season2_2017$Violation_Code), count = n(Season2_2017$Violation_Code))
df_Season2_ViolationCount_2017<-collect(arrange(Season2_ViolationCount_2017,desc(Season2_ViolationCount_2017$count)))
head(df_Season2_ViolationCount_2017,3)
ggplot(head(df_Season2_ViolationCount_2017,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")

# For Season 3 - 3 most common violation code
# Violation_Code  	count
#             21 	385774
#             38 	244985
#             36 	239879
Season3_2017 <- subset(parkingVoilations_2017,parkingVoilations_2017$Seasons=='Season-3')
Season3_ViolationCount_2017 <-summarize(groupBy(Season3_2017, Season3_2017$Violation_Code), count = n(Season3_2017$Violation_Code))
df_Season3_ViolationCount_2017<-collect(arrange(Season3_ViolationCount_2017,desc(Season3_ViolationCount_2017$count)))
head(df_Season3_ViolationCount_2017,3)
ggplot(head(df_Season3_ViolationCount_2017,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")

# For Season 4 - 3 most common violation code
# Violation_Code  	count
#             36 	442593
#             21 	347428
#             38 	263393
Season4_2017 <- subset(parkingVoilations_2017,parkingVoilations_2017$Seasons=='Season-4')
Season4_ViolationCount_2017 <-summarize(groupBy(Season4_2017, Season4_2017$Violation_Code), count = n(Season4_2017$Violation_Code))
df_Season4_ViolationCount_2017<-collect(arrange(Season4_ViolationCount_2017,desc(Season4_ViolationCount_2017$count)),3)
head(df_Season4_ViolationCount_2017,3)
ggplot(head(df_Season4_ViolationCount_2017,20), aes(x=Violation_Code,y=count))+ geom_bar(stat="identity")


#2.7.1 - Find total occurences of the 3 most common violation codes
#For Year 2015
total_top_3_violation_code_2015 <- SparkR::sql("SELECT Violation_Code, COUNT(*) AS Count_Violation_Code FROM
                                               parkingVoilations_2015_tbl GROUP BY Violation_Code
                                               ORDER BY COUNT(*) DESC")

head(total_top_3_violation_code_2015)

#           Violation_Code Count_Violation_Code
# 1             21              1630912
# 2             38              1418627
# 3             14               988469

# CODE |	DEFINITION |	Manhattan 96th St. & below |	All Other Areas 
# 21   |	Street Cleaning: No parking where parking is not allowed by sign, street marking or traffic control device.|	$65|	$45
# 38   |  Failing to show a receipt or tag in the windshield.Drivers get a 5-minute grace period past the expired time on Muni-Meter receipts.|	$65|	$35
# 14   |	General No Standing: Standing or parking where standing is not allowed by sign, street marking or; traffic control device. |	$115 |	$115

#Average for each code
# 21 | $55
# 38 | $50
# 14 | $115

total_amount_top_3_violation_code_2015 <- SparkR::sql("SELECT '21' AS Violation_Code, COUNT(*) * 55 AS Total_Amount_USD FROM
                                                      parkingVoilations_2015_tbl 
                                                      WHERE   Violation_Code = 21
                                                      GROUP BY Violation_Code 
                                                      UNION
                                                      SELECT '38' AS Violation_Code, COUNT(*) * 50 AS Total_Amount_USD FROM
                                                      parkingVoilations_2015_tbl 
                                                      WHERE   Violation_Code = 38
                                                      GROUP BY Violation_Code     
                                                      UNION
                                                      SELECT '14' AS Violation_Code, COUNT(*) * 115 AS Total_Amount_USD FROM
                                                      parkingVoilations_2015_tbl 
                                                      WHERE   Violation_Code = 14   ")
head(total_amount_top_3_violation_code_2015)

#         Violation_Code Total_Amount_USD
# 1             14        USD113,673,935
# 2             21        USD 89,700,160
# 3             38        USD 70,931,350

#For Year 2016
total_top_3_violation_code_2016 <- SparkR::sql("SELECT Violation_Code, COUNT(*) AS Count_Violation_Code FROM
                                               parkingVoilations_2016_tbl GROUP BY Violation_Code
                                               ORDER BY COUNT(*) DESC")

head(total_top_3_violation_code_2016)

#           Violation_Code Count_Violation_Code
#1                    21  1531587
#2                    36  1253512
#3                    38  1143696

# CODE |	DEFINITION |	Manhattan 96th St. & below |	All Other Areas 
# 21   |	Street Cleaning: No parking where parking is not allowed by sign, street marking or traffic control device.|	$65|	$45
# 36	 |  Exceeding the posted speed limit in or near a designated school zone. |	$50  |	$50
# 38   |  Failing to show a receipt or tag in the windshield.Drivers get a 5-minute grace period past the expired time on Muni-Meter receipts.|	$65|	$35

#Average for each code
# 21 | $55
# 36 | $50
# 38 | $50


total_amount_top_3_violation_code_2016 <- SparkR::sql("SELECT '21' AS Violation_Code, COUNT(*) * 55 AS Total_Amount_USD FROM
                                                      parkingVoilations_2016_tbl 
                                                      WHERE   Violation_Code = 21
                                                      GROUP BY Violation_Code 
                                                      UNION
                                                      SELECT '36' AS Violation_Code, COUNT(*) * 50 AS Total_Amount_USD FROM
                                                      parkingVoilations_2016_tbl 
                                                      WHERE   Violation_Code = 36
                                                      GROUP BY Violation_Code     
                                                      UNION
                                                      SELECT '38' AS Violation_Code, COUNT(*) * 50 AS Total_Amount_USD FROM
                                                      parkingVoilations_2016_tbl 
                                                      WHERE   Violation_Code = 38   ")
head(total_amount_top_3_violation_code_2016)

#           Violation_Code Total_Amount_USD
# 1             36         USD62,675,600
# 2             21         USD84,237,285
# 3             38         USD57,184,800

#For Year 2017
total_top_3_violation_code_2017 <- SparkR::sql("SELECT Violation_Code, COUNT(*) AS Count_Violation_Code FROM
                                               parkingVoilations_2017_tbl GROUP BY Violation_Code
                                               ORDER BY COUNT(*) DESC")

head(total_top_3_violation_code_2017)

#           Violation_Code Count_Violation_Code
#1             21              1528588
#2             36              1400614
#3             38              1062304

# CODE |	DEFINITION |	Manhattan 96th St. & below |	All Other Areas 
# 21   |	Street Cleaning: No parking where parking is not allowed by sign, street marking or traffic control device.|	$65|	$45
# 36	 |  Exceeding the posted speed limit in or near a designated school zone. |	$50  |	$50
# 38   |  Failing to show a receipt or tag in the windshield.Drivers get a 5-minute grace period past the expired time on Muni-Meter receipts.|	$65|	$35

#Average for each code
# 21 | $55
# 36 | $50
# 38 | $50


total_amount_top_3_violation_code_2017 <- SparkR::sql("SELECT '21' AS Violation_Code, COUNT(*) * 55 AS Total_Amount_USD FROM
                                                      parkingVoilations_2017_tbl 
                                                      WHERE   Violation_Code = 21
                                                      GROUP BY Violation_Code 
                                                      UNION
                                                      SELECT '36' AS Violation_Code, COUNT(*) * 50 AS Total_Amount_USD FROM
                                                      parkingVoilations_2017_tbl 
                                                      WHERE   Violation_Code = 36
                                                      GROUP BY Violation_Code     
                                                      UNION
                                                      SELECT '38' AS Violation_Code, COUNT(*) * 50 AS Total_Amount_USD FROM
                                                      parkingVoilations_2017_tbl 
                                                      WHERE   Violation_Code = 38   ")
head(total_amount_top_3_violation_code_2017)

#           Violation_Code Total_Amount_USD
#1             38           USD53,115,200
#2             21           USD84,072,340
#3             36           USD70,030,700

#Inference
#There has been change in the top parking violation from 2015 to 2016 wherein "General No Standing" has not been issued.
#Over the three, approximately, the revenue numbers remain in the vicinity of 200Million USD.