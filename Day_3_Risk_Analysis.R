library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(devEMF)

reg = data.frame(geo=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","UK","LI","IS","NO"),ctry=c("AUT","BEL","BLG","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR","LIE","ISL","NOR"),country=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Rep.","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom","Lichenstein","Iceland","Norway"),regija=c("Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","HR","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Zemlje SIE","Zemlje SIE","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","EEA","EEA","EEA"))
# Tema
gtema <- theme_minimal() + theme(panel.background = element_rect(fill="white",linetype = 0),plot.background = element_rect(fill="white",linetype = 0),legend.box.background = element_rect(fill="white",linetype = 0),text = element_text(colour = "#000000"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(gtema)

# Paleta boja
boje_fill <- scale_fill_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))
boje_col <- scale_color_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))

# PART 0. Data imoprt ####

# mandatory funds
mandatory <- read_excel("obvezni_povijest.xlsx")
pom1 <- mandatory %>% select(date=1,value=3) %>% mutate(fund="SAVA") %>% na.omit() %>% mutate(date1=dmy_hms(date),date2=as.Date(as.numeric(date),origin="1899-12-30")) %>% mutate(date=case_when(!is.na(date1)~as.Date(date1),T~date2)) %>% select(-date1,-date2)
pom2 <- mandatory %>% select(date=5,value=7) %>% mutate(fund="KB") %>% na.omit() %>% mutate(date1=dmy_hms(date),date2=as.Date(as.numeric(date),origin="1899-12-30")) %>% mutate(date=case_when(!is.na(date1)~as.Date(date1),T~date2)) %>% select(-date1,-date2)
mandatory <- bind_rows(pom1,pom2)
rm(pom1,pom2)

# voluntary funds
voluntary <- read_excel("dobrovoljni_povijest.xlsx")
pom1 <- voluntary %>% select(date=2,value=4) %>% mutate(fund="Sava Penzija Plus")
pom2 <- voluntary %>% select(date=6,value=8) %>% mutate(fund="KB Prv otvoren dobrovolen PF")
voluntary <- bind_rows(pom1,pom2)
rm(pom1,pom2)

# nav data
pension_data <- read_excel("nav_opce.xlsx") %>% rename(date=1,fund_original=2,value=3,nav=4)
names <- tibble(fund_original = c("САВА ПЕНЗИСКИ ФОНД", "САВА ПЕНЗИЈА ПЛУС", "КБ задолжителен ПФ","КБ Прво доброволен", "Триглав задолжителен", "Триглав доброволен", "ВФП доброволен"), fund= c("SAVA PENZISKI FOND", "SAVA PENZIJA PLUS", "KB zadolzhitelen PF","KB Prvo dobrovolen", "Triglav zadolzhitelen", "Triglav dobrovolen", "VFP dobrovolen"))
pension_data <- pension_data %>% left_join(names,by="fund_original") %>% mutate(date=dmy_hm(date))

convert_to_decimal_range <- function(number, min_range, max_range) {
  adjusted_number <- number
  while (adjusted_number > max_range || adjusted_number < min_range) {
    adjusted_number <- adjusted_number / 10
  }
  return(adjusted_number)
}

pension_data <- pension_data %>% mutate(value = sapply(value, convert_to_decimal_range, min_range = 90, max_range = 300)) %>% mutate(nav=str_remove_all(nav,"\\."),nav=str_replace(nav,"\\,","\\."),nav=as.numeric(nav))



# PART 1. Performance ####

## 1.1. Total returns ####
# Calculate total return for each fund
returns_data <- pension_data %>% group_by(fund) %>% summarise(InitialValue = first(value), FinalValue = last(value), TotalReturn = (FinalValue / InitialValue - 1) * 100)

# Create a chart with ggplot2
emf("./slike/301_total_return.emf",width = 7,height = 6)
ggplot(returns_data, aes(x=fund, y=TotalReturn, fill=fund)) + geom_bar(stat="identity", width=0.5, show.legend = F,alpha=0.7) + labs(x="Fund", y="Total Return (%)") + boje_fill + geom_text(aes(label=round(TotalReturn, 2)), size=5) + plot_annotation(title="Total Return of Each Fund Since the Beginning",caption = "Source: Mapas") + coord_flip()
dev.off()
rm(returns_data)

# Calculate time in market for each fund
time_in_market_data <- pension_data %>% group_by(fund) %>% summarise(StartDate = min(date),EndDate = max(date),DaysInMarket = as.numeric(difftime(EndDate, StartDate, units = "days")))

# Create a chart with ggplot2 showing time in market for each fund
emf("./slike/302_time_in_market.emf",width = 7,height = 6)
ggplot(time_in_market_data, aes(x=fund, y=DaysInMarket, fill=fund)) +  geom_bar(stat="identity", width=0.5, alpha=0.7,show.legend = F) +  labs(x="Fund", y="Days in Market") + boje_fill +  geom_text(aes(label=DaysInMarket), size=5) + plot_annotation(title="Time in Market for Each Fund",caption = "Source: Mapas") + coord_flip()
dev.off()
rm(time_in_market_data)

## 1.2. Annualized return ####

# Calculate annualized return for each fund
annualized_returns_data <- pension_data %>% group_by(fund) %>% summarise(InitialValue = first(value),FinalValue = last(value),StartDate = min(date),EndDate = max(date),YearsInMarket = as.numeric(difftime(EndDate, StartDate, units = "days")) / 365.25,AnnualizedReturn = (FinalValue / InitialValue)^(1/YearsInMarket) - 1)

# Create a chart with ggplot2
emf("./slike/303_annualized_returns.emf",width = 7,height = 6)
ggplot(annualized_returns_data, aes(x=fund, y=AnnualizedReturn * 100, fill=fund)) +  geom_bar(stat="identity", width=0.5, alpha=0.7,show.legend = F) + labs(x="Fund", y="Annualized Return (%)") +  boje_fill + geom_text(aes(label=sprintf("%.2f%%", AnnualizedReturn * 100)), size=3.5) + plot_annotation(title="Annualized Return of Each Fund",caption = "Source: Mapas") + coord_flip()
dev.off()
rm(annualized_returns_data)

## 1.3. Alpha ####




## 1.4. Return over costs #### 


# PART 2. Volatility ####

## 2.1. Standard deviation ####



## 2.2. Beta ####



## 2.3. Sharpe ratio ####





## 2.4. Market volatility - EWMA ####





# PART 3. Risk ###

## 3.1. Sortino ratio ####



## 3.2. Value at Risk ####



## 3.3 Expected Shortfall ####


## 3.4. Maximum drawdown ####


# PART 4. Sensitivity ####

## 4.1. Gobal beta ####




## 4.2. Correlation matrix ####



# 4.3. Partial correlation ####


