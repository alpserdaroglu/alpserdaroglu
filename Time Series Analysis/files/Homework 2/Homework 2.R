#2017402063%%10 --> 3
#3 Consumer Price Index (2003=100)(TURKSTAT) -> "CLOTHING AND FOOTWEAR"

#Homework 2 ----

library(EVDS)
library(lubridate)
library(ggplot2 )
library(ggcorrplot)
library(skimr)
library(data.table)
library(corrplot)
library(GGally)
set_evds_key("O05EUMEwx4")

library(readxl)
library(forecast)

#Data Import and Manipulation --------------------------------------------

data <- get_series(c("TP.FG.J03", "TP.DK.USD.A.YTL", "TP.KTF17", "TP.TUFE1YI.T35", "TP.TUFE1YI.T40", "TP.TG2.Y01"),
                   start_date = "31-10-2008", end_date = "31-03-2021")

raw_data <- data.table(data$items)
setnames(raw_data, c("TP_FG_J03", "TP_DK_USD_A_YTL", "TP_KTF17", "TP_TUFE1YI_T35", "TP_TUFE1YI_T40", "TP_TG2_Y01"),
         c("PriceIndex", "USDTRY", "Interest", "PPIApparel", "PPIShoes", "CCI"))

raw_data$Tarih <- as.Date(as.POSIXct(as.numeric((raw_data$UNIXTIME)), origin = "1970-01-02"))
raw_data$Tarih <- ymd(raw_data$Tarih)
raw_data[,"Year" := year(Tarih)]
raw_data[,"Month" := as.character(lubridate::month(Tarih, label = T))]

raw_data[,"PriceIndex" := as.numeric(PriceIndex)]
raw_data[,"USDTRY" := as.numeric(USDTRY)]
raw_data[,"Interest" := as.numeric(Interest)]
raw_data[,"PPIApparel" := as.numeric(PPIApparel)]
raw_data[,"PPIShoes" := as.numeric(PPIShoes)]
raw_data[,"CCI" := as.numeric(CCI)]
raw_data[,"Month" := as.factor(Month)]

cloth_cpi <- raw_data[1:150,c("Tarih", "Year", "Month", "PriceIndex", "Interest",
                              "PPIApparel", "USDTRY", "PPIShoes", "CCI")]
str(cloth_cpi)

#Descriptive Analysis ----

ggplot(cloth_cpi, aes(x = Tarih)) + 
  geom_line(aes(y = PriceIndex, color = 'Price Index'))

cloth_cpi[, mean(PriceIndex), by=Month]

cloth_cpi[, sd(PriceIndex), by=Year]

#Checking Autocorrelation & Cross-Correlations ----

pacf(cloth_cpi$PriceIndex)

ccf(cloth_cpi$PriceIndex, cloth_cpi$Interest)
ccf(cloth_cpi$PriceIndex, cloth_cpi$USDTRY)
ccf(cloth_cpi$PriceIndex, cloth_cpi$PPIApparel)
ccf(cloth_cpi$PriceIndex, cloth_cpi$PPIShoes)
ccf(cloth_cpi$PriceIndex, cloth_cpi$CCI)

cloth_cpi[, PPIApparelLag1 := shift(PPIApparel)]
cloth_cpi[, USDTRYLag1 := shift(USDTRY)]
cloth_cpi[, PriceIndexLag1 := shift(PriceIndex)]
#cloth_cpi[, PriceIndexLag2 := shift(PriceIndex,2)]
cloth_cpi[, InterestLag1 := shift(Interest)]
cloth_cpi[, PPIShoesLag1 := shift(PPIShoes)]
cloth_cpi[, CCILag1 := shift(CCI)]

cloth_cpi <- cloth_cpi[-1]
cloth_cpi[, "Trend" := 1:.N]

ggcorrplot(cor(cloth_cpi[, c("PriceIndex", "Interest", "PPIApparel", "USDTRY", "PPIShoes")]), hc.order = TRUE,
           type = "lower", lab = TRUE)

#ggpairs(cloth_cpi[, c("PriceIndex", "InterestLag1", "PPIApparelLag1", "USDTRYLag1", "PPIShoesLag1", "CCILag1")])
ggpairs(cloth_cpi[, c("PriceIndex", "Interest", "PPIApparel", "USDTRY", "PPIShoes", "CCI")])
#Box.test(cloth_cpi$PriceIndex, lag=12, type="Ljung-Box")

#Linear Regression Version 1 ----

ts_reg_trendonly = lm(PriceIndex~Trend,cloth_cpi)
summary(ts_reg_trendonly)
cloth_cpi[, "TrendOnly" := predict(ts_reg_trendonly, cloth_cpi)]

ggplot(cloth_cpi, aes(x = Tarih)) + 
  geom_line(aes(y = PriceIndex, color = 'Price Index'))+
  geom_line(aes(y = TrendOnly, color = 'Trend'))

ggplot(cloth_cpi, aes(x = Tarih)) +
  geom_line(aes(y = PriceIndex - TrendOnly)) +
  labs(y = "Residuals", x = "Date")

#autocorrelation in the residuals#

#Linear Regression Version 2 ----

ts_reg_trendmonth = lm(PriceIndex~Trend + Month,cloth_cpi)
summary(ts_reg_trendmonth)
cloth_cpi[, "TrendMonth" := predict(ts_reg_trendmonth , cloth_cpi)]

ggplot(cloth_cpi, aes(x = Tarih)) + 
  geom_line(aes(y = PriceIndex, color = 'Price Index'))+
  geom_line(aes(y = TrendMonth, color = 'Trend'))

ggplot(cloth_cpi, aes(x = Tarih)) +
  geom_line(aes(y = PriceIndex - TrendMonth))

#Linear Regression Version 3 (not included in th report)----

cloth_cpi[, PPIApparelLag1 := shift(PPIApparel)]
cloth_cpi[, USDTRYLag1 := shift(USDTRY)]
cloth_cpi[, PriceIndexLag1 := shift(PriceIndex)]
cloth_cpi[, PPIShoesLag1 := shift(PPIShoes)]
cloth_cpi <- cloth_cpi[-1]
cloth_cpi[, "Trend" := 1:.N]

ts_reg_ppi_app = lm(PriceIndex~Trend+Month+PPIApparelLag1, cloth_cpi)
summary(ts_reg_ppi_app)

cloth_cpi[, "V3Predict" := predict(ts_reg_ppi_app, cloth_cpi)]

ggplot(cloth_cpi, aes(x = Tarih)) + 
  geom_line(aes(y = PriceIndex, color = 'Price Index'))+
  geom_line(aes(y = V3Predict, color = 'Trend'))

ggplot(cloth_cpi, aes(x = Tarih)) +
  geom_line(aes(y = PriceIndex - V3Predict))

#deneme = lm(PriceIndex~PPIApparelLag1, cloth_cpi)
#summary(deneme)

#Linear Regression Version 4 (with USDTRY & PPI Apparel)----
ts_reg_usdppi = lm(PriceIndex~Trend+Month+PPIApparelLag1+USDTRYLag1, cloth_cpi)
summary(ts_reg_usd)

checkresiduals(ts_reg_usdppi)
cloth_cpi[, "V4Predict" := predict(ts_reg_usdppi, cloth_cpi)]

pacf(cloth_cpi$PriceIndex-cloth_cpi$V4Predict)

ggplot(cloth_cpi, aes(x = Tarih)) + 
  geom_line(aes(y = PriceIndex, color = 'Price Index'))+
  geom_line(aes(y = V4Predict, color = 'Trend'))

ggplot(cloth_cpi, aes(x = USDTRY)) +
  geom_line(aes(y = PriceIndex - V4Predict))

#Linear Regression Version 5 (with USDTRY and both PPIs, not included in the report) ----

ts_reg_V5 = lm(PriceIndex~Trend+Month+PPIApparelLag1+CCILag1, cloth_cpi)
summary(ts_reg_V5)
plot(ts_reg_V5)
cloth_cpi[, "V5Predict" := predict(ts_reg_V5, cloth_cpi)]

acf(cloth_cpi$PriceIndex-cloth_cpi$V5Predict)

ggplot(cloth_cpi, aes(x = Tarih)) + 
  geom_line(aes(y = PriceIndex, color = 'Price Index'))+
  geom_line(aes(y = V5Predict, color = 'Trend'))

ggplot(cloth_cpi, aes(x = Tarih)) +
  geom_line(aes(y = PriceIndex - V4Predict))

#AutoRegressive Model Version 6 ----
ts_reg_auto = lm(PriceIndex~Trend+Month+PPIApparelLag1+USDTRYLag1+PriceIndexLag1, cloth_cpi)
summary(ts_reg_auto)

checkresiduals(ts_reg_auto, lag = 12)
cloth_cpi[, "V5Predict" := predict(ts_reg_auto, cloth_cpi)]

acf(cloth_cpi$PriceIndex-cloth_cpi$V5Predict)

Box.test(cloth_cpi$PriceIndex-cloth_cpi$V5Predict, lag=12, type="Ljung-Box")

ggplot(cloth_cpi, aes(x = Tarih)) + 
  geom_line(aes(y = PriceIndex, color = 'Price Index'))+
  geom_line(aes(y = V5Predict, color = 'Trend'))

ggplot(cloth_cpi, aes(x = Tarih)) +
  geom_line(aes(y = PriceIndex - V6Predict))

