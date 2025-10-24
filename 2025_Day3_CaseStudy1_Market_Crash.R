# Packages
library(tidyverse)
library(lubridate)
library(scales)
library(devEMF)
library(eurostat)
library(ecb)
library(jsonlite)
library(patchwork)
library(fredr)

# Funkcija za kopiranje u excel
skopiraj <- function(df, sep="\t", dec=".", max.size=(200*1000)){
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

# Funkcija za decimalne zareze umjesto točaka
hr_format <- function(x) format(x, big.mark = ".", decimal.mark =",", scientific = FALSE)
hr_format_postotak <- percent_format(big.mark = ".", decimal.mark =",")

gtema <- theme_minimal() + theme(panel.background = element_rect(fill="#f9f9ff",linetype = 0),plot.background = element_rect(fill="#f9f9ff",linetype = 0),legend.box.background = element_rect(fill="#f9f9ff",linetype = 0),text = element_text(colour = "#28334AFF"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(gtema)

# paleta boja
boje_fill <- scale_fill_manual(values = c("#19234f", "#d33631", "#397367", "#345fd3", "#ab1212", "#5DA399","#0ea1f0","#ba5854","#63CCCA","#9ae5fe","#f5b0b0","#BDD358"))
boje_col <- scale_color_manual(values = c("#19234f", "#d33631", "#397367", "#345fd3", "#ab1212", "#5DA399","#0ea1f0","#ba5854","#63CCCA","#9ae5fe","#f5b0b0","#BDD358"))

# 1. Macro data ####
load("makro_scenarij.Rda")
skopiraj(makro_scenarij)

# 2. Stock market crash ####
api_key <- "08145abf6a02175d21bf50d7f12e0bd0"

pom1 <- fromJSON(paste0("http://api.marketstack.com/v2/eod?access_key=", api_key,"&symbols=GSPC.INDX&date_from=2000-01-01&date_to=2030-01-01&limit=10000"))$data %>% select(datum=date,iznos=close) %>% mutate(datum=as.Date(datum),indeks="S&P 500")
pom2 <- fromJSON(paste0("http://api.marketstack.com/v2/eod?access_key=", api_key,"&symbols=STOXX.INDX&date_from=2000-01-01&date_to=2030-01-01&limit=10000"))$data %>% select(datum=date,iznos=close) %>% mutate(datum=as.Date(datum),indeks="STOXX 600")
pom3 <- fromJSON("https://rest.zse.hr/web/Bvt9fe2peQ7pwpyYqODM/index-history/XZAG/HRZB00ICBEX6/2010-01-01/2030-01-01/json")$history %>% na.omit() %>% select(datum=date,iznos=last_value) %>% mutate(datum=as.Date(datum),iznos=as.numeric(iznos),indeks="CROBEX") 
pom4 <- fromJSON("https://www.mse.mk/api/index/MBI10/12") %>% mutate(datum=as.Date(tradingDate),indeks="MBI10") %>% select(datum,iznos=value,indeks)
dionice <- rbind(pom1,pom2,pom3,pom4)
ggplot(dionice,aes(x=datum,y=iznos,col=indeks)) + geom_line() + facet_wrap(~indeks,scales="free_y")

# A. Historical Crash ####
stocks <- dionice %>% filter(indeks %in% c("CROBEX","S&P 500")) %>% 
  mutate(kvartal=ceiling_date(datum,unit="quarter")-1) %>% group_by(kvartal) %>% filter(datum==max(datum)) %>% 
  select(kvartal,indeks,iznos) %>% ungroup()


# --- Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(stringr)
library(purrr)

# stocks: tibble with kvartal (Date), indeks (chr: "S&P 500" or "CROBEX"), iznos (numeric)

# --- Helper: build one crash path for one index ------------------------------
# Picks the PEAK within an anchor window, then takes the path from peak to an end date
build_crash_path <- function(data, index_name, anchor_start, anchor_end, crisis_end, scenario_name) {
  df <- data %>%
    filter(indeks == index_name)
  
  # find pre-crisis peak inside the anchor window
  peak_row <- df %>%
    filter(kvartal >= anchor_start, kvartal <= anchor_end) %>%
    arrange(desc(iznos), desc(kvartal)) %>%
    slice_head(n = 1)
  
  # if there is no data in the anchor window, return empty
  if (nrow(peak_row) == 0) {
    return(tibble())
  }
  
  peak_date  <- peak_row$kvartal[[1]]
  peak_level <- peak_row$iznos[[1]]
  
  path <- df %>%
    filter(kvartal >= peak_date, kvartal <= crisis_end) %>%
    arrange(kvartal) %>%
    mutate(
      scenario = scenario_name,
      indeks   = index_name,
      t        = row_number() - 1L,               # quarters since peak (0 at peak)
      level    = iznos,
      norm100  = 100 * level / peak_level
    )
  
  path
}

# --- Scenarios (you can tweak dates if you prefer slightly different windows) -
# 2008 GFC: choose peak in 2006 Q4–2008 Q1, then run to 2009 Q4
gfc_anchor_start <- as.Date("2006-12-31")
gfc_anchor_end   <- as.Date("2008-03-31")
gfc_end          <- as.Date("2009-12-31")

# 2020 Covid: choose peak in 2019 Q2–2019 Q4, then run to 2021 Q4
covid_anchor_start <- as.Date("2019-06-30")
covid_anchor_end   <- as.Date("2019-12-31")
covid_end          <- as.Date("2021-12-31")

# --- Build paths for both indices and both scenarios -------------------------
indices <- c("S&P 500", "CROBEX")

paths_list <- list(
  # 2008 crisis
  map(indices, ~build_crash_path(
    stocks, .x, gfc_anchor_start, gfc_anchor_end, gfc_end, "2008 Global Financial Crisis"
  )),
  # 2020 crisis
  map(indices, ~build_crash_path(
    stocks, .x, covid_anchor_start, covid_anchor_end, covid_end, "2020 Covid Crisis"
  ))
)

paths <- paths_list %>%
  flatten_df()

# Informative message if CROBEX had no data for 2008 (expected given your note)
if (!any(paths$scenario == "2008 Global Financial Crisis" & paths$indeks == "CROBEX")) {
  message("Note: CROBEX has no data in the 2008 anchor window, so it is omitted from the 2008 path.")
}

# --- Compute summary stats per (scenario, indeks) ----------------------------
summary_tbl <- paths %>%
  group_by(scenario, indeks) %>%
  summarize(
    peak_quarter   = min(kvartal),                         # first quarter in the path (the peak)
    trough_quarter = kvartal[which.min(norm100)],
    trough_norm    = min(norm100, na.rm = TRUE),
    drawdown_pct   = (trough_norm / 100 - 1) * 100,
    qtrs_to_trough = which.min(norm100) - 1L,
    qtrs_to_end    = n() - 1L,
    .groups = "drop"
  ) %>%
  arrange(scenario, indeks)

print(summary_tbl)

# --- Plot: normalized paths (100 at peak), faceted by scenario ---------------
pal <- c("S&P 500" = "#2C7FB8", "CROBEX" = "#F03B20")  # optional distinct colors

ggplot(paths, aes(kvartal, norm100, color = indeks)) +
  geom_hline(yintercept = 100, linewidth = 0.3, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(data = paths %>% group_by(scenario, indeks) %>% slice_min(norm100, n = 1, with_ties = FALSE),
             aes(kvartal, norm100), size = 2) +
  scale_y_continuous(labels = label_number(accuracy = 1)) + boje_col +
  scale_x_date(date_labels = "%m-%Y") +
  facet_wrap(~ scenario, scales = "free_x") +
  labs(
    title = "Historical Crash Replay (Quarterly, Normalized to 100 at Pre-Crisis Peak)",
    subtitle = "S&P 500 and CROBEX | Peak chosen within an anchor window, then path through the crisis window",
    x = "Quarter",
    y = "Index level (Peak = 100)",
    color = "Index"
  )
