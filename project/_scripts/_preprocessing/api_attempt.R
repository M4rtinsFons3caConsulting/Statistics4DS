# ISO3	Country Name	Bank Name	API Call	R Package Install
# USA	United States	Federal Reserve	https://fred.stlouisfed.org/docs/api/fred/	install.packages("fredr")
# GBR	United Kingdom	Bank of England	https://www.bankofengland.co.uk/boeapps/database/fromshowcolumns.asp	
# CAN	Canada	Bank of Canada	https://www.bankofcanada.ca/valet/docs
# EU	European Union	European Central Bank	https://sdw-wsrest.ecb.europa.eu/help/	install.packages("ecb")
# KOR	South Korea	Bank of Korea	https://ecos.bok.or.kr/api/	
# AUS	Australia	Reserve Bank of Australia		install.packages("readrba")
# JPN	Japan	Bank of Japan		install.packages("BOJ")
# CHE	Switzerland	Swiss National Bank		install.packages('SNBdata', repos = c('http://enricoschumann.net/R', getOption('repos')))
# NZL	New Zealand	Reserve Bank of New Zealand		https://rdrr.io/cran/RBNZ/api/
# NOR	Norway	Norges Bank	https://data.norges-bank.no/api/	 

# USA
library(fredr)

fredr_set_key("api_key")

topics <- fredr_tags()
print(topics)


# GBR

# uses csv you can download from the website

# CAN
library(httr)
library(jsonlite)

url <- "https://www.bankofcanada.ca/valet"

#Household Income
#Gross Domestic Product (GDP) - SAN_2017_21_GDP_V62305783
#Interest Rate
#Inflation Rate - INDINF_CPI_M
#Consumer Confidence
#Consumer Credit - V122644

# EU
install.packages("ecb")
library(ecb)

datasets <- ecb::get_description()
print(datasets)print(datasets)print(datasets)

