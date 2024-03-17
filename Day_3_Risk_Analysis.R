library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(devEMF)
library(jsonlite)
library(scales)
library(broom)
library(corrplot)

# Tema
gtema <- theme_minimal() + theme(panel.background = element_rect(fill="white",linetype = 0),plot.background = element_rect(fill="white",linetype = 0),legend.box.background = element_rect(fill="white",linetype = 0),text = element_text(colour = "#000000"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(gtema)

# Paleta boja
boje_fill <- scale_fill_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))
boje_col <- scale_color_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))

# PART 0. Data import ####

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

rm(names)


# PART 1. Performance ####

# 1.0. Indices ####

emf("./slike/300_total_return.emf",width = 7,height = 6)
ggplot(pension_data,aes(x=date,y=value,col=fund)) + geom_line(linewidth=1.3,show.legend = F) + boje_col + facet_wrap(~fund) + labs(x="Date",y="Unit value") + plot_annotation(title="Time Series of Each Fund Unit Value", subtitle = "In MKD",caption = "Source: Mapas")
dev.off()

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

## 1.3. Creating a benchmark ####

# by individual classes
benchmark <- read_excel("Day3_data.xlsx",sheet = "benchmark_data") %>% mutate(date=as.Date(date)) 
emf("./slike/304_benchmarks_classes.emf",width = 7,height = 6)
ggplot(benchmark,aes(x=date,y=close,col=category)) + geom_line(linewidth=1.4) + boje_col + labs(x="Date",y="Close value") + plot_annotation(title = "Benchmarks for Different Asset Classes",subtitle = "Index, 100=2010-01-01",caption = "Source: EOD Historical Data")
dev.off()

# benchmark
weights <- tibble(category=c("Bonds","Stocks","Inv. Funds","Cash"),wght=c(0.5,0.25,0.15,0.05))
p1 <- ggplot(weights,aes(x=category,y=wght,fill=category)) + geom_col(alpha=0.7,show.legend = F) + boje_fill + scale_y_continuous(labels = scales::percent) + labs(x="",y="",subtitle = "Weights in Portfolio")
benchmark <- benchmark %>% select(date,category,close) %>% spread(category,close) %>% fill_(names(.)) %>% gather("category","close",-date)
composite_benchmark <- benchmark %>% left_join(weights,by="category") %>% mutate(close=close*wght) %>% group_by(date) %>% summarise(close=sum(close,na.rm=T))
p2 <- ggplot(composite_benchmark,aes(x=date,y=close)) + geom_line(linewidth=1.6) + labs(x="Date",y="Close value",subtitle = "Index, 100=2010-01-01")
emf("./slike/305_composite_benchmark.emf",width = 7,height = 6)
p1 / p2 + plot_annotation(title = "Composite Benchmark and Weights",caption = "Source: EOD Historical Data") + plot_layout(heights = c(1,2))
dev.off()

## 1.3. Alpha ####

# Calculate returns for funds
pension_returns <- pension_data %>% mutate(weekday=wday(date, week_start=1)) %>% filter(weekday==5) %>% group_by(fund) %>% arrange(fund, date) %>%  mutate(return = (value / lag(value,1) - 1)) %>%  select(-value,-fund_original,-nav, -weekday) %>% drop_na()

# Calculate returns for the benchmark
benchmark_returns <- composite_benchmark %>% mutate(weekday=wday(date, week_start=1)) %>% filter(weekday==5) %>%  mutate(benchmark_return = (close / lag(close) - 1)) %>% select(-close,-weekday) %>%  drop_na()
risk_free <- benchmark %>% filter(category=="Cash") %>% mutate(weekday=wday(date, week_start=1)) %>% filter(weekday==5) %>% mutate(rfree_return = (close / lag(close) - 1)) %>% select(-close,-weekday,-category) %>%  drop_na()

# Join fund returns with benchmark returns
joined_returns <- left_join(pension_returns, benchmark_returns, by = "date") %>% left_join(risk_free,by="date")%>% drop_na()

# Calculate excess returns (fund returns - benchmark returns)
joined_returns <- joined_returns %>% mutate(fund_excess_return = return - rfree_return, market_excess_return = benchmark_return - rfree_return)

# Run a CAPM regression for each fund to calculate alpha
capm_results <- joined_returns %>%  group_by(fund) %>%  do(tidy(lm(fund_excess_return ~ market_excess_return, data = .))) %>%  filter(term == "(Intercept)") %>%  rename(alpha = estimate) %>% mutate(fund=str_wrap(fund,15))

# Visual presentation
emf("./slike/306_alpha.emf",width = 7,height = 6)
ggplot(capm_results,aes(x=fund,y=alpha,fill=fund)) + geom_col(alpha=0.7,show.legend = F) + boje_fill + scale_y_continuous(labels = percent) + labs(x="Fund",y="Alpha") + plot_annotation(title = "Relative Performance of Each Fund",caption = "Sources: Mapas, EOD Historical Data")
dev.off()


# PART 2. Volatility ####

## 2.1. Beta ####

# Calculate returns for funds
pension_returns <- pension_data %>% mutate(weekday=wday(date, week_start=1)) %>% filter(weekday==5) %>% group_by(fund) %>% arrange(fund, date) %>%  mutate(return = (value / lag(value,1) - 1)) %>%  select(-value,-fund_original,-nav, -weekday) %>% drop_na()

# Calculate returns for the benchmark
benchmark_returns <- composite_benchmark %>% mutate(weekday=wday(date, week_start=1)) %>% filter(weekday==5) %>%  mutate(benchmark_return = (close / lag(close) - 1)) %>% select(-close,-weekday) %>%  drop_na()
risk_free <- benchmark %>% filter(category=="Cash") %>% mutate(weekday=wday(date, week_start=1)) %>% filter(weekday==5) %>% mutate(rfree_return = (close / lag(close) - 1)) %>% select(-close,-weekday,-category) %>%  drop_na()

# Join fund returns with benchmark returns
joined_returns <- left_join(pension_returns, benchmark_returns, by = "date") %>% left_join(risk_free,by="date")%>% drop_na()

# Calculate excess returns (fund returns - benchmark returns)
joined_returns <- joined_returns %>% mutate(fund_excess_return = return - rfree_return, market_excess_return = benchmark_return - rfree_return)

# Run a CAPM regression for each fund to calculate alpha
capm_results <- joined_returns %>%  group_by(fund) %>%  do(tidy(lm(fund_excess_return ~ market_excess_return, data = .))) %>% filter(term == "market_excess_return") %>%  rename(beta = estimate) %>% mutate(fund=str_wrap(fund,15))

# Visual presentation
emf("./slike/307_beta.emf",width = 7,height = 6)
ggplot(capm_results,aes(x=fund,y=beta,fill=fund)) + geom_col(alpha=0.7,show.legend = F) + boje_fill + scale_y_continuous(labels = percent) + labs(x="Fund",y="Beta") + plot_annotation(title = "Relative Volatility of Each Fund",caption = "Sources: Mapas, EOD Historical Data") + geom_text(aes(label=sprintf("%.2f%%", beta * 100)),nudge_y = 0.03)
dev.off()


## 2.2. Standard deviation ####

# Calculate returns for funds
pension_returns <- pension_data %>% mutate(weekday=wday(date, week_start=1)) %>% filter(weekday==5) %>% group_by(fund) %>% arrange(fund, date) %>%  mutate(return = (value / lag(value,1) - 1)) %>%  select(-value,-fund_original,-nav, -weekday) %>% drop_na()

# Group by fund and calculate standard deviation for each fund
std_dev_by_fund <- pension_returns %>%  group_by(fund) %>%  summarise(std_dev_return = sd(return)) %>% mutate(fund=str_wrap(fund,15))

# Visual presentation
emf("./slike/308_standard_deviation.emf",width = 7,height = 6)
ggplot(std_dev_by_fund,aes(x=fund,y=std_dev_return,fill=fund)) + geom_col(alpha=0.7,show.legend = F) + boje_fill + scale_y_continuous(labels = percent) + labs(x="",y="") + plot_annotation(title = "Volatility of Each Funds' Returns",subtitle = "Standard Deviation of Weekly Returns",caption = "Sources: Mapas, EOD Historical Data")
dev.off()

## 2.3. Sharpe ratio ####

# Calculate Sharpe Ratio for each fund
sharpe_ratios <- joined_returns %>%  group_by(fund) %>% summarise(AverageReturn = mean(return, na.rm = TRUE)*52,risk_free_rate = mean(rfree_return, na.rm=T)*52, StdDev = sd(return, na.rm = TRUE)*sqrt(52),ExcessReturn=AverageReturn - risk_free_rate,SharpeRatio = ExcessReturn / StdDev  ) %>% mutate(fund=str_wrap(fund,15))
p1 <- ggplot(sharpe_ratios, aes(x = reorder(fund, SharpeRatio), y = SharpeRatio, fill = fund)) + geom_col(alpha=0.7,show.legend = F)  + boje_fill + geom_hline(yintercept = 1, linetype = "dashed", color = "red",linewidth=1.4) +  labs (x = "", y = "Sharpe Ratio")
p2 <- ggplot(sharpe_ratios,aes(x=StdDev,y=AverageReturn,col=fund)) + geom_point(size=7) + boje_col + geom_abline(slope=1,intercept=0, linetype = "dashed", color = "red",linewidth=1.4) + labs(x="Standard deviation",y="Average Return")

# Chart the Sharpe Ratios
emf("./slike/309_sharpe_ratio.emf",width = 7,height = 6)
p1 / p2 + plot_annotation(title = "Sharpe Ratios for Each Fund",caption = "Sources: Mapas, EOD Historical Data")
dev.off()


# PART 3. Risk ###

## 3.1. Sortino ratio ####

# Calculate Sortino Ratio for each fund
sortino_ratios <- joined_returns %>%  group_by(fund) %>% summarise(AverageReturn = mean(return, na.rm = TRUE) * 52, RiskFreeRate = mean(rfree_return, na.rm = TRUE) * 52, DownsideDeviation = sqrt(mean(pmin(return - RiskFreeRate, 0)^2, na.rm = TRUE) * 52), ExcessReturn = AverageReturn - RiskFreeRate, SortinoRatio = ExcessReturn / DownsideDeviation) %>% mutate(fund = str_wrap(fund, 15))

p1 <- ggplot(sortino_ratios, aes(x = reorder(fund, SortinoRatio), y = SortinoRatio, fill = fund)) + geom_col(alpha=0.7,show.legend = F)  + boje_fill + geom_hline(yintercept = 2, linetype = "dashed", color = "red",linewidth=1.4) +  labs (x = "", y = "Sortino Ratio")
p2 <- ggplot(sortino_ratios,aes(x=DownsideDeviation,y=ExcessReturn,col=fund)) + geom_point(size=7) + boje_col + geom_abline(slope=2,intercept=0, linetype = "dashed", color = "red",linewidth=1.4) + labs(x="Downside deviation",y="Average Return")

# Chart the sortino Ratios
emf("./slike/310_sortino_ratio.emf",width = 7,height = 6)
p1 / p2 + plot_annotation(title = "Sortino Ratios for Each Fund",caption = "Sources: Mapas, EOD Historical Data")
dev.off()


## 3.2. Value at Risk ####

# First, sort the data to ensure chronological order
pension_data <- pension_data %>% arrange(fund, date)

# Calculate daily returns for each fund
pension_data <- pension_data %>%  group_by(fund) %>% mutate(daily_return = (value / lag(value) - 1)) %>%  ungroup()

# Calculate the VaR at a specified alpha level, e.g., 95% confidence (alpha = 0.05)
alpha_level <- 0.05

# Calculate the 95% yearly VaR for each fund
yearly_var_95 <- pension_data %>%  filter(!is.na(daily_return)) %>%  group_by(fund) %>% summarise(Yearly_VaR_95 = quantile(daily_return, probs = alpha_level, na.rm = TRUE) * sqrt(252)) %>% mutate(Yearly_VaR_95 = Yearly_VaR_95 * -1) %>% mutate(fund = str_wrap(fund, 15)) # Making the VaR positive for clarity
emf("./slike/311_VaR.emf",width = 7,height = 6)
ggplot(yearly_var_95,aes(x=fund,y=Yearly_VaR_95,fill=fund)) + geom_col(alpha=0.7,show.legend = F) + boje_fill + scale_y_continuous(labels = percent) + labs(x="",y="") + plot_annotation(title = "Value at Risk of Each Funds' Returns",caption = "Sources: Mapas, EOD Historical Data")
dev.off()

## 3.3 Expected Shortfall ####

# Calculate the ES at a specified alpha level, e.g., 95% confidence (alpha = 0.05)
alpha_level <- 0.05

# Calculate the 95% yearly Expected Shortfall for each fund
yearly_es_95 <- pension_data %>%  filter(!is.na(daily_return)) %>% group_by(fund) %>% summarise(ES_95 = mean(daily_return[daily_return < quantile(daily_return, probs = alpha_level, na.rm = TRUE)], na.rm = TRUE) * sqrt(252)) %>%  mutate(ES_95 = ES_95 * -1, fund = str_wrap(fund, 15))

emf("./slike/312_ES.emf",width = 7,height = 6)
ggplot(yearly_es_95,aes(x=fund,y=ES_95,fill=fund)) + geom_col(alpha=0.7,show.legend = F) + boje_fill + scale_y_continuous(labels = percent) + labs(x="",y="") + plot_annotation(title = "Expected Shortfall of Each Funds' Returns",caption = "Sources: Mapas, EOD Historical Data")
dev.off()


## 3.4. Maximum drawdown ####

# Calculate cumulative max (peak) value and drawdown for each fund
drawdown_data <- pension_data %>%  group_by(fund) %>% arrange(fund, date) %>% mutate(peak_value = cummax(value),drawdown = (value - peak_value) / peak_value) %>% ungroup()

# Calculate Maximum Drawdown for each fund
max_drawdown <- drawdown_data %>%  group_by(fund) %>% summarise(MaximumDrawdown = min(drawdown)) %>% mutate(MaximumDrawdown = abs(MaximumDrawdown),fund = str_wrap(fund, 15))

emf("./slike/313_MDD.emf",width = 7,height = 6)
ggplot(max_drawdown,aes(x=fund,y=MaximumDrawdown,fill=fund)) + geom_col(alpha=0.7,show.legend = F) + boje_fill + scale_y_continuous(labels = percent) + labs(x="",y="") + plot_annotation(title = "Maximum Drawdown for Each Fund",caption = "Sources: Mapas, EOD Historical Data")
dev.off()


# PART 4. Sensitivity ####

## 4.1. Correlation matrix ####

correlation_data <- read_excel("Day3_data.xlsx",sheet = "correlation_data") %>% mutate(date=as.Date(date)) %>% mutate(fund=str_wrap(fund,20))

correlation_matrix <- correlation_data %>% spread(fund,value) %>% drop_na() %>% select(-date)
#to make the correlation matrix plot
corrplot.mixed(cor(correlation_matrix),lower = "number",tl.pos = "lt") #it creates the correlation matrix

# Cleaning Workspace
rm(list = ls())
