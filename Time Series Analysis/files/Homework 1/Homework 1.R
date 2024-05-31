#Please install the required packages
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("skimr")
#install.packages("EVDS")
#install.packages("lubridate")

library(data.table)
library(ggplot2)
library(skimr)
library(EVDS)
library(lubridate)
set_evds_key("5S5HKJUZTB")

#PART I
#Collecting data using the EVDS package
#How does weekly consumer credits change with
#Frequency of observations is monthly. Study period: January 2016 - December 2019
data<-get_series(c("TP.KTFTUK","TP.TUKKRE.K001", "TP.YISGUCU2.G8", "TP.AKONUTSAT1.TOPLAM"), start_date = "01-12-2015", end_date = "31-12-2019")
credit_table <- data.table(data$items) 

setnames(credit_table,
         c("TP_KTFTUK","TP_TUKKRE_K001", "TP_YISGUCU2_G8", "TP_AKONUTSAT1_TOPLAM"),
         c("CreditInterestRate", "CreditVolume", "UnemploymentRate", "HouseSales"))

#Data Cleaning - Arranging the date column
credit_table$Tarih <- as.Date(as.POSIXct(as.numeric(unlist(credit_table$UNIXTIME)), origin = "1970-01-02"))
credit_table[,"UNIXTIME" := NULL]
Date <- credit_table$Tarih
day(Date) <- 15
credit_table[,Tarih := Date]

#The amount of new consumer loans created is calculated from total volume of consumer loans.
credit_table[,"NewCredit":= shift(as.numeric(credit_table$"CreditVolume"))]
credit_table[,"CreditVolume" := as.numeric(CreditVolume)]
credit_table[,"NewCredit" := (CreditVolume-NewCredit) / 1000]

#Further data arrangements
credit_table[,"CreditInterestRate" := round(as.numeric(`CreditInterestRate`),4)]
credit_table[,"UnemploymentRate" := as.numeric(UnemploymentRate)]
credit_table[,"HouseSales" := as.numeric(HouseSales)]
credit_table <- credit_table[-1,]

head(credit_table)

#Visualizations for the new consumer loans given
ggplot(credit_table, aes(x = Tarih))+geom_line(aes(y=NewCredit)) #Time Series
ggplot(credit_table, aes(x = NewCredit))+geom_boxplot() #Box-Plot to detect outliers

#Visualizations for the interest rate:
ggplot(credit_table, aes(x = Tarih))+geom_line(aes(y=NewCredit)) #Time Series
ggplot(credit_table,aes(x = CreditInterestRate, y = NewCredit))+geom_point()+geom_smooth() #Scatter Plot with the New Credit Data

#Visualizations for the House Sales
ggplot(credit_table,aes(x= Tarih))+geom_line(aes(y = credit_table$HouseSales)) #Time Series
ggplot(credit_table,aes(y = NewCredit, x = HouseSales))+geom_point()+geom_smooth()#Scatter Plot with the New Credit Data

#Visualizations for the Unemployment Rate
ggplot(credit_table,aes(x= Tarih))+geom_line(aes(y = credit_table$UnemploymentRate)) #Time Series
ggplot(credit_table,aes(y = NewCredit, x = UnemploymentRate))+geom_point()+geom_smooth()#Scatter Plot with the New Credit Data

#PART II

setwd("C:/Users/alpsr/Desktop/Data") # Please change your working directory after downlading the data from Google Trends

#"Consumer Loans - Data collection and cleaning
google_consloan <- read.csv("tuketici kredisi.csv", header = TRUE)
google_consloan <- data.table(google_consloan)
setnames(google_consloan, c("tuketici.kredisi...Turkey."), c("SearchVolume"))
google_consloan[, "Week" := dmy(Week)]
google_consloan[, "Month" := month(Week)]
google_consloan[, "Year" := year(Week)]
google_consloan[, "SearchVolume" := as.numeric(SearchVolume)]
google_consloan[, "ConsumerLoanSV" := mean(SearchVolume), by = .(Month, Year)]
google_consloan[, c("Week", "SearchVolume") := NULL]
google_consloan <- unique(google_consloan)
google_consloan[, "Date" := make_date(year = Year, month = Month, day = 15)]

ggplot(google_consloan, aes(x = Date, y = ConsumerLoanSV))+geom_line()
ggplot(google_consloan, aes(x = credit_table$NewCredit, y = ConsumerLoanSV))+geom_point()+geom_sm

#Interest Rates - Data collection and cleaning
google_interest <- read.csv("faiz oranlari.csv", header = TRUE)
google_interest <- data.table(google_interest)
setnames(google_interest, c("faiz.oranlari"), c("SearchVolume"))
google_interest[, "Week" := dmy(Week)]
google_interest[, "Month" := month(Week)]
google_interest[, "Year" := year(Week)]
google_interest[, "SearchVolume" := as.numeric(SearchVolume)]
google_interest[, "InterestSV" := mean(SearchVolume), by = .(Month, Year)]
google_interest[, c("Week", "SearchVolume") := NULL]
google_interest <- unique(google_interest)
google_interest[, "Date" := make_date(year = Year, month = Month, day = 15)]

ggplot(google_interest, aes(x = Date, y = InterestSV))+geom_line()
ggplot(google_interest, aes(x = credit_table$CreditInterestRate, y = InterestSV))+
  geom_point()+
  geom_smooth(method = lm)

#Mortgages - Data collection and cleaning
google_mortgage <- read.csv("konut kredisi.csv", header = TRUE)
google_mortgage <- data.table(google_mortgage)
setnames(google_mortgage, c("konut.kredisi...Turkey."), c("SearchVolume"))
google_mortgage[, "Week" := dmy(Week)]
google_mortgage[, "Month" := month(Week)]
google_mortgage[, "Year" := year(Week)]
google_mortgage[, "SearchVolume" := as.numeric(SearchVolume)]
google_mortgage[, "mortgageSV" := mean(SearchVolume), by = .(Month, Year)]
google_mortgage[, c("Week", "SearchVolume") := NULL]
google_mortgage <- unique(google_mortgage)
google_mortgage[, "Date" := make_date(year = Year, month = Month, day = 15)]

ggplot(google_mortgage, aes(x = Date, y = mortgageSV))+geom_line()
ggplot(google_mortgage, aes(x = credit_table$HouseSales, y = mortgageSV))+
  geom_point()+
  geom_smooth(method = lm)

#Job Postings - Data collection and cleaning
google_jobs <- read.csv("is ilanlari.csv", header = TRUE)
google_jobs <- data.table(google_jobs)
setnames(google_jobs, c("is.ilanlari"), c("SearchVolume"))
google_jobs[, "Week" := dmy(Week)]
google_jobs[, "Month" := month(Week)]
google_jobs[, "Year" := year(Week)]
google_jobs[, "SearchVolume" := as.numeric(SearchVolume)]
google_jobs[, "JobsSV" := mean(SearchVolume), by = .(Month, Year)]
google_jobs[, c("Week", "SearchVolume") := NULL]
google_jobs <- unique(google_jobs)
google_jobs[, "Date" := make_date(year = Year, month = Month, day = 15)]

ggplot(google_jobs, aes(x = Date, y = JobsSV))+geom_line()
ggplot(google_jobs, aes(x = credit_table$UnemploymentRate, y = JobsSV))+
  geom_point()+
  geom_smooth(method = lm)
