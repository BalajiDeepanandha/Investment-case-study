companies <- read.delim("companies.txt", header = TRUE, na.strings=c("","NA") )
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

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
count(distinct(companies, name))

#Method2: Without using any external packages: 
length(unique(companies$name))

#5.Merge the two data frames so that all  variables (columns) 
#in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame.
#How many observations are present in master_frame ?


names(companies)[names(companies) == "permalink"] <- "company_permalink"
companies$company_permalink <- tolower(companies$company_permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

master_frame <- merge(rounds2, companies, by="company_permalink")

#Table 2.1: Average Values of Investments for Each of these Funding Types
funding_type_groups <- group_by(master_frame, funding_round_type) 
summarise(funding_type_groups, mean(raised_amount_usd, na.rm = T))

#Table 3.1: 

venture <- subset(master_frame, funding_round_type  == "venture")

venture_country_groups <- group_by(venture, country_code) 
vcg_total_funding <- summarise(venture_country_groups, sum(raised_amount_usd, na.rm = T))

names(vcg_total_funding) <- c("country_code","total_funding_amt")
vcg_total_funding <- na.omit(vcg_total_funding)
arrange(top_n(vcg_total_funding, 9, total_funding_amt), desc(total_funding_amt))

#checkpoint 4

mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE, check.names = FALSE, na.strings=c("","NA"))

mapping_long <- gather(mapping, major_category, my_val, 2:10 )
mapping_long <- mapping_long[!(mapping_long$my_val == 0),]
mapping_long <- mapping_long[,-3]

master_frame <- separate(master_frame, category_list, c("primary_sector"), sep = "[|]", remove = F, extra = "drop" )
master_frame <- merge(master_frame, mapping_long, by.x="primary_sector", by.y = "category_list", all.x = T)

#Checkpoint 5
venture_threshold_invest <- filter(master_frame,funding_round_type  == "venture",  raised_amount_usd >= 5000000 & raised_amount_usd < 15000000  ) 

d1 <- filter(venture_threshold_invest, country_code == "USA")
d2 <- filter(venture_threshold_invest, country_code == "GBR")
d3 <- filter(venture_threshold_invest, country_code == "IND")

#d1-> USA
usa_group <- group_by(d1, major_category) 
usa_invest_amount <- summarise(usa_group, tot_amt_major_cateogory = sum(raised_amount_usd))
usa_invest_count <- summarise(usa_group, tot_cnt_major_cateogory = n())

d1 <- merge(d1, usa_invest_amount, by = "major_category")
d1 <- merge(d1, usa_invest_count, by = "major_category") 

#d2-> GBR

gbr_group <- group_by(d2, major_category) 
gbr_invest_amount <- summarise(gbr_group, tot_amt_major_cateogory = sum(raised_amount_usd))
gbr_invest_count <- summarise(gbr_group, tot_cnt_major_cateogory = n())

d2 <- merge(d2, gbr_invest_amount, by = "major_category")
d2 <- merge(d2, gbr_invest_count, by = "major_category") 


#d3-> IND

ind_group <- group_by(d3, major_category) 
ind_invest_amount <- summarise(ind_group, tot_amt_major_cateogory = sum(raised_amount_usd))
ind_invest_count <- summarise(ind_group, tot_cnt_major_cateogory = n())

d3 <- merge(d3, ind_invest_amount, by = "major_category")
d3 <- merge(d3, ind_invest_count, by = "major_category") 


nrow(d1)
sum(d1$raised_amount_usd)
View(usa_invest_count)

