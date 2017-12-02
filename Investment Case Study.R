library(dplyr)
library(tidyr)
companies <- read.delim("companies.txt", header = TRUE, na.strings=c("","NA") )
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

names(companies)[names(companies) == "permalink"] <- "company_permalink"
companies$company_permalink <- tolower(companies$company_permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

#Checkpoint1:Data Cleaning
#Table 1.1: Understand the Data Set
#1. How many unique companies are present in rounds2?
#Method1: Using Dplyr: 
count(distinct(rounds2, company_permalink))

#Method2: Without using any external packages: 
length(unique(rounds2$company_permalink))


#2. How many unique companies are present in the companies file?
# Understanding is permalink link coulmn in companes is the primary key. 
# Therefore number of rows equals the unique companies
#Method1: Using Dplyr: 
count(distinct(companies, company_permalink))

#Method2: Without using any external packages: 
length(unique(companies$company_permalink))

#5.Merge the two data frames so that all  variables (columns) 
#in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame.
#How many observations are present in master_frame ?


master_frame <- merge(rounds2, companies, by="company_permalink")
nrow(master_frame)

#Checkpoint2: Fundding Type Analysis
#Table 2.1: Average Values of Investments for Each of these Funding Types
funding_type_groups <- group_by(master_frame, funding_round_type) 
summarise(funding_type_groups, mean(raised_amount_usd, na.rm = T))
#Answers for Table 2.1 can be filled by seeing the output of above summarise statement

#Checkpoint3:Country analysis

#Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)
#For the chosen investment type, make a data frame named top9 with the top nine countries (based on the total investment amount each country has received)
 
#Creating a dataframe with only "Venture" investments
venture <- subset(master_frame, funding_round_type  == "venture")

#grouping the venture df by country and summarising to find total investments per country
venture_country_groups <- group_by(venture, country_code) 
vcg_total_funding <- summarise(venture_country_groups, sum(raised_amount_usd, na.rm = T))

#manipulations to omit NAs and filtering only top 9 countries
names(vcg_total_funding) <- c("country_code","total_funding_amt")
vcg_total_funding <- na.omit(vcg_total_funding)
top_9 <- arrange(top_n(vcg_total_funding, 9, total_funding_amt), desc(total_funding_amt))
top_9

#Table 3.1:
#Answers for Table 3.1 can be filled by seeing df top_9

#checkpoint 4: Sector Analysis 1

#Reading the mapping sheet and converting to long format 
mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE, check.names = FALSE, na.strings=c("","NA"))

mapping_long <- gather(mapping, main_sector, my_val, 2:10 )
mapping_long <- mapping_long[!(mapping_long$my_val == 0),]
mapping_long <- mapping_long[,-3]

#Creating the primary_sector column based on agreed rules 
master_frame <- separate(master_frame, category_list, c("primary_sector"), sep = "[|]", remove = F, extra = "drop" )

#merged data frame with each primary sector mapped to its main sector 
master_frame <- merge(master_frame, mapping_long, by.x ="primary_sector", by.y = "category_list", all.x = T)

#checkpoint 5: Sector Analysis2

#Creating a dataframe to filter the preferred FT Venture and investments between the threshold
venture_threshold_invest <- filter(master_frame,funding_round_type  == "venture",  raised_amount_usd >= 5000000 & raised_amount_usd < 15000000  ) 

#three separate data frames D1, D2 and D3 for each of the three countries 
#containing the observations of funding type FT falling within the 5-15 million USD range
d1 <- filter(venture_threshold_invest, country_code == "USA")
d2 <- filter(venture_threshold_invest, country_code == "GBR")
d3 <- filter(venture_threshold_invest, country_code == "IND")

#Adding columns "total amount invested in each main sector" and 
#total number (or count) of investments for each main sector
#to each of the country dataframes

#d1-> USA
usa_group <- group_by(d1, main_sector) 

usa_invest_amount <- summarise(usa_group, tot_amt_major_cateogory = sum(raised_amount_usd))
usa_invest_count <- summarise(usa_group, tot_cnt_major_cateogory = n())

d1 <- merge(d1, usa_invest_amount, by = "main_sector")
d1 <- merge(d1, usa_invest_count, by = "main_sector") 

#d2-> GBR

gbr_group <- group_by(d2, main_sector) 
gbr_invest_amount <- summarise(gbr_group, tot_amt_major_cateogory = sum(raised_amount_usd))
gbr_invest_count <- summarise(gbr_group, tot_cnt_major_cateogory = n())

d2 <- merge(d2, gbr_invest_amount, by = "main_sector")
d2 <- merge(d2, gbr_invest_count, by = "main_sector") 


#d3-> IND

ind_group <- group_by(d3, main_sector) 
ind_invest_amount <- summarise(ind_group, tot_amt_major_cateogory = sum(raised_amount_usd))
ind_invest_count <- summarise(ind_group, tot_cnt_major_cateogory = n())

d3 <- merge(d3, ind_invest_amount, by = "main_sector")
d3 <- merge(d3, ind_invest_count, by = "main_sector") 

#TABLE 5.1: C1
#1. 
nrow(d1)
#2. 
sum(d1$raised_amount_usd)
#3, 4, 5, 6, 7, 8
arrange(top_n(usa_invest_count, 3, tot_cnt_major_cateogory), desc(tot_cnt_major_cateogory))

#9, 10,
d1_sub <- d1 %>% group_by(main_sector,company_permalink) %>% summarise(sum(raised_amount_usd))  
names(d1_sub) <- c("main_sector", "compan_permalink", "sum_investment" )
d1_sub %>% filter(sum_investment == max(sum_investment))


#TABLE 5.1: C2
#1. 
nrow(d2)
#2. 
sum(d2$raised_amount_usd)
#3, 4, 5, 6, 7, 8
arrange(top_n(gbr_invest_count, 3, tot_cnt_major_cateogory), desc(tot_cnt_major_cateogory))

#9, 10,
d2_sub <- d2 %>% group_by(main_sector,company_permalink) %>% summarise(sum(raised_amount_usd))  
names(d2_sub) <- c("main_sector", "compan_permalink", "sum_investment" )
d2_sub %>% filter(sum_investment == max(sum_investment))

#TABLE 5.1: C3
nrow(d3)
#2. 
sum(d3$raised_amount_usd)
#3, 4, 5, 6, 7, 8
arrange(top_n(ind_invest_count, 3, tot_cnt_major_cateogory), desc(tot_cnt_major_cateogory))

#9, 10,
d3_sub <- d3 %>% group_by(main_sector,company_permalink) %>% summarise(sum(raised_amount_usd))  
names(d3_sub) <- c("main_sector", "compan_permalink", "sum_investment" )
d3_sub %>% filter(sum_investment == max(sum_investment))
