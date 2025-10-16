library(tidyverse)
library(lubridate)
library(scales)
library(devEMF)
library(readxl)
library(jsonlite)
library(patchwork)
library(eurostat)
library(fredr)
library(ggrepel)
library(PerformanceAnalytics)
library(tibbletime)
library(ggforce)
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


# 0. Dohvat podataka ####
load("Z:/SSRZP/DSR/DWH/nav_opce.Rda")


# A. Mirexi
pom1 <- nav_opce %>% filter(vrsta0=="Mirovinski" & vrsta1=="Obavezan") %>% dplyr::select(datum, vrsta2, cj_udjela_u_valuti, nav) %>% group_by(datum,vrsta2) %>% summarise(mirex=weighted.mean(x = cj_udjela_u_valuti,w = nav,na.rm = T)) %>% spread(vrsta2,mirex) %>% rename(mirex_a=`Kategorija A`,mirex_b=`Kategorija B`,mirex_c=`Kategorija C`) %>% filter(!is.na(mirex_b)) %>% ungroup()

# --- Step 1: Compute daily returns for MIREX B ---
df_returns <- pom1 %>% mutate(returns=mirex_b/lag(mirex_b)-1)

# --- Step 2: Estimate annualized mean and volatility ---
stats_b <- df_returns %>%
  summarise(
    mean_b = mean(returns, na.rm = TRUE) * 252,          # expected annual return
    sd_b   = sd(returns, na.rm = TRUE) * sqrt(252)       # annual volatility
  )

print(stats_b)

# --- Step 3: Define deterministic scenarios ---

scenarios_b <- stats_b %>%
  mutate(
    adverse     = mean_b - sd_b,
    expected    = mean_b,
    optimistic  = mean_b + sd_b
  )

# --- Step 4: Project portfolio value over 20 years ---

initial_value <- 10000
years <- 20

values_b <- data.frame(
  scenario = c("Adverse", "Expected", "Optimistic"),
  annual_return = c(scenarios_b$adverse, scenarios_b$expected, scenarios_b$optimistic)
) %>%
  mutate(final_value = initial_value * (1 + annual_return) ^ years)

print(values_b)

# Convert annual rates to *approximate daily equivalents* for comparison on chart
daily_scenarios <- scenarios_b / 252

# --- Step 5: Plot historical daily returns + scenario lines ---
ggplot(df_returns, aes(x = datum, y = returns)) +
  geom_line(color = "gray40", linewidth = 0.4) +
  geom_hline(
    yintercept = as.numeric(daily_scenarios$adverse),
    color = "#e74c3c", linetype = "dashed", linewidth = 0.8
  ) +
  geom_hline(
    yintercept = as.numeric(daily_scenarios$expected),
    color = "#3498db", linetype = "solid", linewidth = 0.8
  ) +
  geom_hline(
    yintercept = as.numeric(daily_scenarios$optimistic),
    color = "#2ecc71", linetype = "dashed", linewidth = 0.8
  ) +
  labs(
    title = "MIREX B: Historical Daily Returns with Deterministic Scenarios",
    subtitle = "Horizontal lines show annualized scenarios converted to daily equivalents",
    x = "Date",
    y = "Daily Return"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_minimal(base_size = 13) +
  annotate("text", 
           x = min(df_returns$datum, na.rm = TRUE) + 200, 
           y = as.numeric(daily_scenarios$optimistic),
           label = "Optimistic", color = "#2ecc71", hjust = 0, vjust = -0.5, size = 3.5) +
  annotate("text", 
           x = min(df_returns$datum, na.rm = TRUE) + 200, 
           y = as.numeric(daily_scenarios$expected),
           label = "Expected", color = "#3498db", hjust = 0, vjust = -0.5, size = 3.5) +
  annotate("text", 
           x = min(df_returns$datum, na.rm = TRUE) + 200, 
           y = as.numeric(daily_scenarios$adverse),
           label = "Adverse", color = "#e74c3c", hjust = 0, vjust = -0.5, size = 3.5)

# --- Step 6: Plot results ---

ggplot(values_b, aes(x = scenario, y = final_value, fill = scenario)) +
  geom_col(width = 0.6,show.legend = F) +
  geom_text(aes(label = dollar(round(final_value, 0))), vjust = -0.5, size = 4) +
  labs(
    title = "Deterministic Growth of MIREX B (20-Year Horizon)",
    subtitle = "Initial value = $10,000 | Based on historical annualized return",
    x = "",
    y = "Final Portfolio Value (USD)"
  ) +
  scale_y_continuous(labels = dollar) + boje_fill

# --- Step 7: Value growth ---

pom2 <- nav_opce %>% filter(vrsta0=="Mirovinski" & vrsta1=="Obavezan") %>% dplyr::select(datum, vrsta2, cj_udjela_u_valuti, nav) %>% group_by(datum,vrsta2) %>% summarise(mirex=weighted.mean(x = cj_udjela_u_valuti,w = nav,na.rm = T)) %>% spread(vrsta2,mirex) %>% select(datum,mirex_b=3) %>% drop_na()

# --- Input parameters ---
start_value <- 13.27075

# Daily expected returns (already estimated)
r_adverse     <- 5.024976e-05
r_expected    <- 0.0001510868
r_optimistic  <- 0.0002519239

# Create date sequence based on your data period (example: from min to max date)
dates <- seq(as.Date("2002-05-02"), as.Date("2025-10-10"), by = "day")

# --- Create deterministic scenario values ---
n <- length(dates)

scenarios <- data.frame(
  datum = dates,
  adverse     = start_value * cumprod(1 + rep(r_adverse, n)),
  expected    = start_value * cumprod(1 + rep(r_expected, n)),
  optimistic  = start_value * cumprod(1 + rep(r_optimistic, n))
)

# Optional: add actual MIREX B data if available (pom2)
# pom2 should have columns: datum, mirex_b
scenarios_full <- scenarios %>%
  left_join(pom2, by = "datum") %>% fill(mirex_b,.direction = "down")

# --- Convert to long format for plotting ---
scenarios_long <- scenarios_full %>%
  pivot_longer(cols = c(adverse, expected, optimistic, mirex_b),
               names_to = "series", values_to = "value")

# --- Plot ---
ggplot(scenarios_long, aes(x = datum, y = value, color = series)) +
  geom_line(linewidth = 0.8) + boje_col +
  labs(
    title = "MIREX B vs. Deterministic Scenario Projections",
    subtitle = "Starting value = 13.27075 | Compounded daily returns",
    x = "Date",
    y = "Index Value",
    color = "Series"
  ) +
  scale_y_continuous(labels = comma)
