library(tidyverse)
library(lubridate)
library(eurostat)
library(readxl)
library(jsonlite)
library(zoo) # For rolling operations

# Funkcija za kopiranje u excel
skopiraj <- function(df, sep="\t", dec=".", max.size=(200*1000)){
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

# Function to convert quarter string to last date of the quarter
convert_to_last_date_of_quarter <- function(date_str) {
  # Extract year and quarter
  matches <- regmatches(date_str, regexec("([0-9]{4})Q([1-4])", date_str))
  
  year <- matches[[1]][2]
  quarter <- matches[[1]][3]
  
  # Map quarter to the last month of the quarter
  last_month_of_quarter <- c("03-31", "06-30", "09-30", "12-31")
  month_day <- last_month_of_quarter[as.integer(quarter)]
  
  # Construct date string representing the last day of the quarter
  date_str <- paste(year, month_day, sep = "-")
  
  # Convert to Date
  as.Date(date_str)
}

convert_to_last_date_of_month <- function(date_str) {
  # Extract year and quarter
  matches <- regmatches(date_str, regexec("([0-9]{4})M([1-4])", date_str))
  
  year <- matches[[1]][2]
  quarter <- matches[[1]][3]
  
  # Map quarter to the last month of the quarter
  last_month_of_quarter <- c("03-31", "06-30", "09-30", "12-31")
  month_day <- last_month_of_quarter[as.integer(quarter)]
  
  # Construct date string representing the last day of the quarter
  date_str <- paste(year, month_day, sep = "-")
  
  # Convert to Date
  as.Date(date_str)
}


# Funkcija za dohvat podataka sa ureda za statistiku

ucitaj_podatke <- function(url_code){
  temp <- read_csv2(url_code)
  columns <- temp[,1] %>% pull(1)
  temp <- temp[,-1] %>% t() %>% as.data.frame()
  colnames(temp) <- columns
  temp <- temp %>% rownames_to_column("Date")
}

# 0. Data import ####

# A. NAV
# Mandatory
nav1 <- read_excel("./MAPAS_Data/NAV_SE_2006_2023_day_fond.xls",sheet = "au_nav_mpf",skip = 2,col_names = c("Date","Sava;Unit_Value","KBP;Unit_Value","Triglav;Unit_Value","Index;Unit_Value","Sava;NAV","KBP;NAV","Triglav;NAV")) %>% gather("Fund_Variable","Value",-Date) %>% separate(Fund_Variable,into = c("Fund","Variable"),sep = ";") %>% spread(Variable,Value) %>% mutate(Category="Mandatory Funds") %>% filter(!is.na(Unit_Value))
# Voluntary
nav2 <- read_excel("./MAPAS_Data/NAV_SE_2006_2023_day_fond.xls",sheet = "au_nav_vpf",skip = 2,col_names = c("Date","Sava;Unit_Value","KBP;Unit_Value","Triglav;Unit_Value","VFP;Unit_Value","Index;Unit_Value","Sava;NAV","KBP;NAV","Triglav;NAV","VFP;NAV")) %>% gather("Fund_Variable","Value",-Date) %>% separate(Fund_Variable,into = c("Fund","Variable"),sep = ";") %>% spread(Variable,Value) %>% mutate(Category="Voluntary Funds") %>% filter(!is.na(Unit_Value))
# Aggregate
nav <- rbind(nav1,nav2)
save(nav,file = "NAV.Rda")
rm(nav1,nav2)

# B. Portfolio
pfolio1 <- read_excel("./MAPAS_Data/Total_portfolio_dpf_2021_05032024.xlsx",sheet = "AllData") %>% select(Izdavach,Tip_instument,Doma_Stranstvo,Instrument,Klasa_HV,Datum_dostasuvanje,Kamatna_stapka,Valuta,ISIN,Nominala_broj_akcii,Vrednost_den,AFS_Vrednost_Den=`AFS vrednost den`,HTM_Vrednost_Den=`HTM vrednost den`,HFT_Vrednost_Den=`HFT vrednost den`,Sektor,`Rejting_Drzava S&P`,Drzava,Name) %>% mutate(Date=as.Date("2021-12-31"),Category="Voluntary")
pfolio2 <- read_excel("./MAPAS_Data/Total_portfolio_dpf_2022_06032024.xlsx",sheet = "AllData") %>% select(Izdavach,Tip_instument,Doma_Stranstvo,Instrument,Klasa_HV,Datum_dostasuvanje,Kamatna_stapka,Valuta,ISIN,Nominala_broj_akcii,Vrednost_den,AFS_Vrednost_Den=`AFS vrednost den`,HTM_Vrednost_Den=`HTM vrednost den`,HFT_Vrednost_Den=`HFT vrednost den`,Sektor,`Rejting_Drzava S&P`,Drzava,Name) %>% mutate(Date=as.Date("2022-12-31"),Category="Voluntary")
pfolio3 <- read_excel("./MAPAS_Data/Total_portfolio_zpf_2021_05032024.xlsx",sheet = "AllData") %>% select(Izdavach,Tip_instument,Doma_Stranstvo,Instrument,Klasa_HV,Datum_dostasuvanje,Kamatna_stapka,Valuta,ISIN,Nominala_broj_akcii,Vrednost_den,AFS_Vrednost_Den=`AFS vrednost den`,HTM_Vrednost_Den=`HTM vrednost den`,HFT_Vrednost_Den=`HFT vrednost den`,Sektor,`Rejting_Drzava S&P`,Drzava,Name) %>% mutate(Date=as.Date("2021-12-31"),Category="Mandatory")
pfolio4 <- read_excel("./MAPAS_Data/Total_portfolio_zpf_2022_06032024.xlsx",sheet = "AllData") %>% select(Izdavach,Tip_instument,Doma_Stranstvo,Instrument,Klasa_HV,Datum_dostasuvanje,Kamatna_stapka,Valuta,Nominala_broj_akcii,Vrednost_den,Sektor,`Rejting_Drzava S&P`,Drzava,Name) %>% mutate(Date=as.Date("2022-12-31"),Category="Mandatory",AFS_Vrednost_Den=NA,HTM_Vrednost_Den=NA,HFT_Vrednost_Den=NA,ISIN=NA)
pfolio5 <- read_excel("./MAPAS_Data/Total_portfolio_zpf_2023_08032024.xlsx",sheet = "AllData") %>% select(Izdavach,Tip_instument,Doma_Stranstvo,Instrument,Klasa_HV,Datum_dostasuvanje,Kamatna_stapka,Valuta,Nominala_broj_akcii,Vrednost_den,AFS_Vrednost_Den=`AFS vrednost den`,HTM_Vrednost_Den=`HTM vrednost den`,HFT_Vrednost_Den=`HFT vrednost den`,Sektor,`Rejting_Drzava S&P`,Drzava,Name) %>% mutate(Date=as.Date("2023-12-31"),Category="Mandatory",ISIN=NA)
portfolio <- rbind(pfolio1,pfolio2,pfolio3,pfolio4,pfolio5)
save(portfolio,file = "Portfolio.Rda")
rm(pfolio1,pfolio2,pfolio3,pfolio4,pfolio5)

# Semicolon delimited without heading

# 1.Makroekonomski indikatori ####

#Indikator k_1.1 Realne stope rasta BDP-a####

pom <- ucitaj_podatke("https://makstat.stat.gov.mk:443/PXWeb/sq/ce5c1588-de00-4e1c-aa3f-efa94f1bf744") %>% rename(gdp=2) %>% mutate(dgdp_y=(gdp/lag(gdp,4)-1)*100)
skopiraj(pom)
# preko API-ja
pom <- fromJSON("https://makstat.stat.gov.mk:443/PXWeb/api/v1/en/MakStat/BDP/BDPTrimesecni/BDPsporedESS2010/175_NacSmA_Mk_02RasKv_ml.px")


#Indikator k_1.2 Indeks industrijske proizvodnje####

# Indices of industrial production, on average 2010, by months by Sector/Division/Main industrial groupings and Month
pom <- ucitaj_podatke("https://makstat.stat.gov.mk:443/PXWeb/sq/5cf94c50-9cb2-4710-9803-17ca7027ab1c") %>% mutate(Date = make_date(ifelse(substr(Date,6,7)=="12",as.numeric(substr(Date,1,4))+1,as.numeric(substr(Date,1,4))), ifelse(substr(Date,6,7)=="12",1,as.numeric(substr(Date,6,7))+1), 1)-1)
skopiraj(pom)


#Indikator k_1.4 Inflacija ####

pom <- ucitaj_podatke("https://makstat.stat.gov.mk:443/PXWeb/sq/ce65ab5f-abed-440d-9230-5bd40ad2ff3d") %>% slice(-1) %>% rename(inflation=2) %>% mutate(Date = make_date(ifelse(substr(Date,6,7)=="12",as.numeric(substr(Date,1,4))+1,as.numeric(substr(Date,1,4))), ifelse(substr(Date,6,7)=="12",1,as.numeric(substr(Date,6,7))+1), 1)-1) %>% mutate(inflation=as.numeric(inflation)/100,inflation=inflation-100)
skopiraj(pom)


############################### 3. Indikatori za mirovinske fondove ###############################

load("NAV.Rda")
load("Portfolio.Rda")
nav <- nav %>% mutate(Date=as.Date(Date))

# 3. MIROVINCI ####
# k_4.1 Rizik profitabilnosti: Ponderirani kvartalni prinos mirovinskih fondova ####
profitability <- nav %>% filter(Fund!="Index") %>% select(Date, Category, Fund, Unit_Value, NAV) %>% mutate(Quarter=ceiling_date(Date,unit = "quarter")-1) %>% group_by(Quarter, Category, Fund) %>% filter(Date==max(Date)) %>% drop_na() %>% arrange(Quarter) %>% group_by(Category, Fund) %>% mutate(Return=Unit_Value/lag(Unit_Value,1)-1) %>% group_by(Quarter, Category) %>% summarise(Market_Return=weighted.mean(x = Return,w = NAV,na.rm = T)) %>% drop_na() %>% spread(Category,Market_Return)
skopiraj(profitability)

# k_4.2 Tržišni rizik: Portfeljni VaR #### 

# Define a function to calculate VaR
calc_var <- function(x, cl = 0.05) {
  quantile(x, probs = cl, na.rm = TRUE)
}

market_risk <- nav %>% filter(Fund=="Index") %>% select(Date, Category, Unit_Value) %>% mutate(Quarter=ceiling_date(Date,unit = "quarter")-1) %>% group_by(Quarter, Category) %>% filter(Date==max(Date)) %>% drop_na() %>% arrange(Quarter) %>% group_by(Category) %>% mutate(Return=Unit_Value/lag(Unit_Value,1)-1) %>% group_by(Category) %>% mutate(VaR=-rollapply(Return,12,calc_var, by.column = TRUE, align = "right", fill = NA))
market_risk <- market_risk %>% select(Quarter,Category,VaR) %>% drop_na() %>% spread(Category,VaR) 
skopiraj(market_risk)


# d_4.1 Rizik koncentracije: HHI po društvu ####

market_concentration_risk <- nav %>% filter(Fund!="Index") %>% select(Date, Category, Fund, NAV) %>% mutate(Quarter=ceiling_date(Date,unit = "quarter")-1) %>% group_by(Quarter, Category, Fund) %>% filter(Date==max(Date)) %>% drop_na() %>% group_by(Quarter,Category) %>% mutate(Total_NAV=sum(NAV,na.rm=T),Market_Share=NAV/Total_NAV*100,MK_Share2=Market_Share^2) %>% group_by(Quarter,Category) %>% summarise(HHI=sum(MK_Share2,na.rm=T)) %>% select(Quarter,Category,HHI) %>% drop_na() %>% spread(Category,HHI)
skopiraj(market_concentration_risk)


  
# d_4.2 Koncentracija izloženosti: Top 5 ulaganja po izdavatelju (protustrani) ####
# Top 5 izloženosti
pom1 <- portfolio %>% group_by(Date,Category,Izdavach) %>% summarise(Value=sum(Vrednost_den,na.rm=T)) %>% drop_na() %>% arrange(-Value) %>% group_by(Date,Category) %>% slice(1:5) %>% group_by(Date,Category) %>% summarise(Top5=sum(Value,na.rm=T))
# Total Assets
pom2 <- portfolio %>% group_by(Date,Category) %>% summarise(Total_Value=sum(Vrednost_den,na.rm=T))
# Combined
exposures_concentration_risk <- inner_join(pom1,pom2,by=c("Date","Category")) %>% mutate(Top5_Share=Top5/Total_Value) %>% select(Date,Category,Top5_Share) %>% spread(Category,Top5_Share)
skopiraj(exposures_concentration_risk)
rm(pom1,pom2)

# d_4.3 Valutni rizik: Ukupna otvorena valutna pozicija u valutama različitim od valute fonda kao udio u NAV-u
currency_risk <- portfolio %>% group_by(Date,Category,Valuta) %>% summarise(Value=sum(Vrednost_den,na.rm=T)) %>% filter(!is.na(Valuta)) %>% mutate(Currency_Type=ifelse(Valuta=="MKD","Domestic","Foreign")) %>% group_by(Date,Category,Currency_Type) %>% summarise(Value=sum(Value,na.rm=T)) %>% group_by(Date,Category) %>% mutate(Total_Value=sum(Value,na.rm=T)) %>% mutate(Foreign_Currency_Share=Value/Total_Value) %>% filter(Currency_Type=="Foreign") %>% select(Date,Category,Foreign_Currency_Share) %>% spread(Category,Foreign_Currency_Share)
skopiraj(currency_risk)

# d_4.4 Kamatni rizik: Duracija i ročnost ####
interest_rate_risk <- portfolio %>% filter(Tip_instument %in% c("Drzavna obvrznica","Korporativna obvrznica")) %>% mutate(Datum_dostasuvanje=as.Date(Datum_dostasuvanje),Rocnost=interval(Date,Datum_dostasuvanje)/years(1)) %>% group_by(Date,Category) %>% summarise(Rocnost=weighted.mean(x = Rocnost,w = Vrednost_den, na.rm=T)) %>% spread(Category,Rocnost)
skopiraj(interest_rate_risk)

# pom <- nav_eur %>% mutate(kvartal=ceiling_date(datum,"quarter")-1) %>% filter(kvartal==datum & vrsta0=="Mirovinski" & razina2=="Obveznice" & vrednovanje!="Metoda amortiziranog troška") %>% group_by(datum,isin,dospijece,kamata,frekvencija) %>% summarise(ytm=weighted.mean(x = ytm,w = iznos,na.rm=T),iznos=sum(iznos,na.rm=T)) %>% mutate(duracija=NA,rocnost=interval(datum,dospijece)/years(1))
# for (i in 1:nrow(pom)) {
#   tryCatch({
#     pom$duracija[i] <- bond.duration(settle = pom$datum[i],mature = pom$dospijece[i],coupon = pom$kamata[i]/100,freq = pom$frekvencija[i],yield = pom$ytm[i])
#   }, error=function(e){})
# }
# pom <- pom %>% group_by(datum) %>% summarise(ytm=weighted.mean(x = ytm,w = iznos,na.rm=T),duracija=weighted.mean(x = duracija,w = iznos,na.rm=T),rocnost=weighted.mean(x = rocnost,w = iznos,na.rm=T),iznos=sum(iznos,na.rm=T)) %>% ungroup() %>% filter(datum<=dtm) %>% select(-c(ytm,iznos))
# skopiraj(pom)
# rm(pom)

# d_4.5 Kreditni rizik: Razlika prinosa obveznica u portfelju subjekta i nerizične obveznice jednake ročnosti ####
# # Dohvat benchmark prinosa sa eurostata, sklapanje i računanje spreada
# rfree <- get_eurostat("irt_euryld_d") %>% filter(yld_curv=="SPOT_RT" & bonds=="CGB_EA_AAA") %>% mutate(mjesec=ceiling_date(time,unit = "month")-1) %>% group_by(mjesec) %>% filter(time==max(time)) %>% select(datum=mjesec,maturity,rfree=values) %>% separate(maturity,c("maturity","mjeseci"),"M") %>% separate(maturity,c("maturity","godine"),"Y") %>% mutate(godine=str_remove(godine,"_")) %>% select(-maturity) %>% mutate(godine=as.numeric(godine),mjeseci=as.numeric(mjeseci)) %>% replace(is.na(.), 0) %>% mutate(rocnost=godine*12+mjeseci,rfree=rfree/100) %>% select(datum,rocnost,rfree)
# # izračun indikatora
# spread <- nav_eur %>% filter(vrsta0=="Mirovinski" & razina1=="Imovina" & razina2=="Obveznice") %>% mutate(rocnost=interval(datum,dospijece) %/% months(1)) %>% group_by(datum,vrsta1,rocnost) %>% summarise(ytm=weighted.mean(x = ytm,w = iznos,na.rm=T),iznos=sum(iznos,na.rm=T)) %>% left_join(rfree,by=c("datum","rocnost")) %>% mutate(spread=ytm-rfree) %>% group_by(datum,vrsta1) %>% summarise(spread=weighted.mean(x = spread,w = iznos,na.rm=T)) %>% spread(vrsta1,spread) %>% rename(spread_dobrovoljan=Dobrovoljan,spread_obavezan=Obavezan)
# # Udio obveznica u porfelju
# obv <- nav_eur %>% filter(vrsta1 %in% c("Dobrovoljan","Obavezan") & datum<=dtm & razina1=="Imovina") %>% group_by(datum,vrsta1) %>% mutate(uk_im=sum(iznos)) %>% group_by(datum,vrsta1,razina2) %>% summarise(obv=sum(iznos),uk_im=mean(uk_im)) %>% ungroup() %>% filter(razina2=="Obveznice") %>% mutate(udio_obveznica=obv/uk_im) %>% select(datum,vrsta1,udio_obveznica) %>% spread(vrsta1,udio_obveznica)
# skopiraj(spread)
# skopiraj(obv)
load("kreditni_rejtinzi.Rda")
kreditni_rejtinzi <- kreditni_rejtinzi %>% rename(Date=datum,ctry=geo) %>% filter(Date>="2000-01-01") %>% group_by(Date,ctry) %>% summarise(Credit_Rating=mean(iznos,na.rm=T))
ggplot(kreditni_rejtinzi,aes(x=Date,y=Credit_Rating))  + geom_hline(aes(yintercept=10.5),col="red",linewidth=1.3) + geom_line() + facet_wrap(~ctry)
zemlje <- read_excel("kreditni_rejtinzi.xlsx",sheet = "mapiranje_ctry")
credit_risk <- portfolio %>% select(Date,Category,Drzava,Vrednost_den) %>% left_join(zemlje,by="Drzava") %>% left_join(kreditni_rejtinzi,by=c("Date","ctry"))  %>% mutate(Credit_Rating=ifelse(ctry=="MK",13,Credit_Rating)) %>% group_by(Date,Category) %>% summarise(Credit_Rating=weighted.mean(x = Credit_Rating,w = Vrednost_den,na.rm=T)) %>% spread(Category,Credit_Rating)
skopiraj(credit_risk)

