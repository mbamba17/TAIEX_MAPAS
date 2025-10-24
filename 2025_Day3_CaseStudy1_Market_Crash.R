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

save(dionice,file = "stock_prices.Rda")

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

# B. Scenario Shock ####

stocks <- dionice %>% filter(indeks %in% c("CROBEX","S&P 500")) %>% 
  filter(datum<="2025-09-30") %>% ungroup()
  
# --- Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(scales)
library(stringr)

# stocks: tibble with datum (Date), indeks (chr), iznos (numeric)
# Last value provided is 2025-09-30 (quarter-end). We'll base from each index's last available <= that date.

# ------------------------ PARAMETERS YOU CAN SET ------------------------------

# Choose one of: "manual" or "var"
mode <- "var"     # "manual" for user-defined 4 returns; "var" for VaR quantiles

# If mode == "manual": four quarterly % changes (as decimals)
manual_returns <- c(-0.40, -0.10, 0.20, 0.05)

# If mode == "var": four quantile probabilities (0..1)
var_probs <- c(0.01, 0.10, 0.60, 0.50)

# Base quarter-end you want to start from (your last known value)
anchor_q_end <- as.Date("2025-09-30")

# ------------------------ HELPERS --------------------------------------------

# 1) Roll daily to quarter-end levels (if your data is daily). If it's already quarterly, this keeps it.
to_quarter_end <- function(df) {
  df %>%
    mutate(q_end = ceiling_date(datum, "quarter") - days(1)) %>%
    group_by(indeks, q_end) %>%
    # last observation within each quarter as the quarter-end level
    summarise(iznos = last(iznos[order(datum)]), .groups = "drop") %>%
    arrange(indeks, q_end) %>%
    rename(datum = q_end)
}

# 2) Compute simple quarterly returns
quarterly_returns <- function(qdf) {
  qdf %>%
    group_by(indeks) %>%
    arrange(datum, .by_group = TRUE) %>%
    mutate(ret = iznos / lag(iznos) - 1) %>%
    ungroup()
}

# 3) Next 4 quarter-ends after an anchor quarter-end
next_four_qends <- function(anchor) {
  q1 <- ceiling_date(anchor + days(1), "quarter") - days(1)
  c(q1, q1 %m+% months(3), q1 %m+% months(6), q1 %m+% months(9))
}

# 4) Build simulated path given a vector of 4 returns
simulate_path <- function(base_level, rets) {
  # sequential compounding
  lvls <- numeric(length(rets))
  lvl <- base_level
  for (i in seq_along(rets)) {
    lvl <- lvl * (1 + rets[i])
    lvls[i] <- lvl
  }
  lvls
}

# ------------------------ PREP QUARTERLY SERIES ------------------------------

q_series <- to_quarter_end(stocks)

# If some index doesn't have exactly anchor_q_end, use latest <= anchor_q_end
anchors <- q_series %>%
  group_by(indeks) %>%
  filter(datum <= anchor_q_end) %>%
  slice_max(datum, n = 1, with_ties = FALSE) %>%
  ungroup()

if (nrow(anchors) == 0) stop("No index has data on or before the anchor quarter.")

# Prepare historical returns for VaR path
q_with_rets <- quarterly_returns(q_series)

# Future quarter-ends to simulate
future_qs <- next_four_qends(anchors$datum %>% max())  # calendar only; each index still uses its own base

# ------------------------ BUILD SIMULATED PATHS ------------------------------

sim_paths <- map_dfr(unique(anchors$indeks), function(ix) {
  base_row <- anchors %>% filter(indeks == ix)
  if (nrow(base_row) == 0) return(tibble())
  base_date  <- base_row$datum[[1]]
  base_level <- base_row$iznos[[1]]
  
  # If base_date < anchor_q_end (index missing 2025-09-30), we still simulate forward from its latest
  sim_dates <- next_four_qends(base_date)
  
  if (mode == "manual") {
    step_returns <- manual_returns
    label <- paste0("Simulated (Manual: ",
                    paste0(percent(manual_returns), collapse = ", "), ")")
  } else if (mode == "var") {
    # historical returns available strictly before base_date
    hist_rets <- q_with_rets %>%
      filter(indeks == ix, datum < base_date) %>%
      pull(ret) %>%
      na.omit()
    if (length(hist_rets) < 8) {
      warning("Few historical quarterly returns for ", ix, " — VaR quantiles may be unstable.")
    }
    step_returns <- as.numeric(quantile(hist_rets, probs = var_probs, na.rm = TRUE, type = 7))
    label <- paste0("Simulated (VaR probs: ", paste0(var_probs, collapse = ", "), ")")
  } else {
    stop("Unknown mode. Use 'manual' or 'var'.")
  }
  
  # Simulated levels
  sim_lvls <- simulate_path(base_level, step_returns)
  
  tibble(
    indeks   = ix,
    datum  = sim_dates,
    iznos    = sim_lvls,
    series   = label
  )
})

# Historical up to anchor (per index -> up to its base date)
historical <- q_series %>%
  inner_join(anchors %>% select(indeks, base_kvartal = datum), by = "indeks") %>%
  filter(datum <= base_kvartal) %>%
  select(indeks, datum, iznos) %>%
  mutate(series = "Historical")

plot_df <- bind_rows(historical, sim_paths)

# Sanity: order
plot_df <- plot_df %>%
  arrange(indeks, datum) %>%
  group_by(indeks) %>%
  mutate(
    # normalize to 100 at the base quarter for easier visual comparison
    base_lvl = max(iznos[datum == max(datum[series == "Historical"])], na.rm = TRUE),
    norm100  = 100 * iznos / base_lvl
  ) %>%
  ungroup()

# ------------------------ PLOT -----------------------------------------------

ggplot(plot_df, aes(datum, norm100, color = series, linetype = series)) +
  geom_hline(yintercept = 100, linewidth = 0.3, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(data = plot_df %>% filter(series != "Historical"),
             size = 2) +
  facet_wrap(~ indeks, scales = "free_x") + boje_col +
  labs(
    title = "Historical Index vs. Simulated 4-Quarter Path",
    subtitle = if (mode == "manual")
      paste0("Manual quarterly returns: ", paste0(percent(manual_returns), collapse = ", "))
    else
      paste0("VaR-based quarterly steps using historical returns | probs: ",
             paste0(var_probs, collapse = ", ")),
    x = "Quarter-end",
    y = "Index (Base quarter = 100)",
    color = "Series", linetype = "Series"
  )

# ------------------------ QUICK SUMMARY --------------------------------------

summary_tbl <- plot_df %>%
  group_by(indeks, series) %>%
  summarize(
    start_q   = min(datum),
    end_q     = max(datum),
    start_100 = norm100[datum == min(datum)],
    end_100   = norm100[datum == max(datum)],
    pct_change = (end_100 / start_100 - 1) * 100,
    .groups = "drop"
  )

print(summary_tbl)

# C. Beta Mapping ####
# --- Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(scales)
library(broom)

# stocks: tibble with columns
#   - datum  : Date (daily)
#   - indeks : chr ("S&P 500", "CROBEX")
#   - iznos  : numeric (index level)

# ------------------------ PARAMETERS -----------------------------------------
anchor_q_end   <- as.Date("2025-09-30")  # last observed quarter-end
mode           <- "var"                  # "manual" or "var"
manual_returns <- c(-0.30, -0.10, 0.12, 0.05)   # only used if mode == "manual"
var_probs      <- c(0.01, 0.10, 0.60, 0.50)     # only used if mode == "var"
beta_lookback_years <- 5                        # use last N years of quarterly returns for beta
min_obs_beta        <- 12                       # minimum overlapping quarters to run regression

# ------------------------ HELPERS --------------------------------------------

# 1) Daily -> quarter-end level (last obs in quarter)
to_quarter_end <- function(df) {
  df %>%
    mutate(q_end = ceiling_date(datum, "quarter") - days(1)) %>%
    group_by(indeks, q_end) %>%
    summarise(iznos = last(iznos[order(datum)]), .groups = "drop") %>%
    arrange(indeks, q_end) %>%
    rename(kvartal = q_end)
}

# 2) Quarterly simple returns
quarterly_returns <- function(qdf) {
  qdf %>%
    group_by(indeks) %>%
    arrange(kvartal, .by_group = TRUE) %>%
    mutate(ret = iznos / lag(iznos) - 1) %>%
    ungroup()
}

# 3) Next four quarter-ends after a base quarter-end
next_four_qends <- function(base_qend) {
  q1 <- ceiling_date(base_qend + days(1), "quarter") - days(1)
  c(q1, q1 %m+% months(3), q1 %m+% months(6), q1 %m+% months(9))
}

# 4) Simulate path from base level using a vector of 4 returns
simulate_path <- function(base_level, rets) {
  Reduce(function(lvl, r) c(lvl, tail(lvl, 1) * (1 + r)),
         rets, init = base_level) %>% tail(-1)
}

# ------------------------ PREP DATA ------------------------------------------

q_series <- to_quarter_end(stocks)

# pick base quarter for each index (latest <= anchor_q_end)
anchors <- q_series %>%
  filter(kvartal <= anchor_q_end) %>%
  group_by(indeks) %>%
  slice_max(kvartal, n = 1, with_ties = FALSE) %>%
  ungroup()

stopifnot(nrow(anchors) >= 1)

# Quarterly returns history
qrets <- quarterly_returns(q_series)

# ------------------------ SIMULATE S&P 500 PATH ------------------------------

# Base for S&P
sp_base <- anchors %>% filter(indeks == "S&P 500")
if (nrow(sp_base) == 0) stop("No S&P 500 base quarter on/before anchor_q_end.")

sp_base_date  <- sp_base$kvartal[[1]]
sp_base_level <- sp_base$iznos[[1]]
sp_future_qs  <- next_four_qends(sp_base_date)

# S&P returns for the 4 steps
if (mode == "manual") {
  sp_step_returns <- manual_returns
  sp_label <- paste0("Sim S&P (Manual: ", paste0(percent(manual_returns), collapse = ", "), ")")
} else if (mode == "var") {
  sp_hist_rets <- qrets %>%
    filter(indeks == "S&P 500", kvartal < sp_base_date) %>%
    pull(ret) %>% na.omit()
  if (length(sp_hist_rets) < 12) warning("Few historical S&P 500 quarterly returns for VaR.")
  sp_step_returns <- as.numeric(quantile(sp_hist_rets, probs = var_probs, na.rm = TRUE, type = 7))
  sp_label <- paste0("Sim S&P (VaR probs: ", paste0(var_probs, collapse = ", "), ")")
} else {
  stop("Unknown mode; use 'manual' or 'var'.")
}

sp_sim_levels <- simulate_path(sp_base_level, sp_step_returns)

sim_sp <- tibble(
  indeks  = "S&P 500",
  kvartal = sp_future_qs,
  iznos   = sp_sim_levels,
  series  = sp_label
)

# ------------------------ ESTIMATE BETA: CROBEX on S&P -----------------------

# Build overlapping return history up to the S&P base date
aligned <- qrets %>%
  select(indeks, kvartal, ret) %>%
  filter(kvartal < sp_base_date) %>%
  pivot_wider(names_from = indeks, values_from = ret) %>%
  drop_na(`S&P 500`, CROBEX)

# Restrict to lookback window (if enough data)
lookback_start <- sp_base_date %m-% years(beta_lookback_years)
aligned_lb <- aligned %>% filter(kvartal >= lookback_start)

reg_data <- if (nrow(aligned_lb) >= min_obs_beta) aligned_lb else aligned

if (nrow(reg_data) < min_obs_beta) {
  warning("Beta estimated on only ", nrow(reg_data), " overlapping quarters (less than min_obs_beta).")
}

fit <- lm(CROBEX ~ `S&P 500`, data = reg_data)
fit_tidy <- tidy(fit)
fit_glance <- glance(fit)

beta_est <- fit_tidy$estimate[fit_tidy$term == "`S&P 500`"]
rsq_est  <- fit_glance$r.squared

# ------------------------ MAP CROBEX VIA BETA --------------------------------

# CROBEX base
cr_base <- anchors %>% filter(indeks == "CROBEX")
if (nrow(cr_base) == 0) stop("No CROBEX base quarter on/before anchor_q_end.")

cr_base_date  <- cr_base$kvartal[[1]]
cr_base_level <- cr_base$iznos[[1]]
cr_future_qs  <- next_four_qends(cr_base_date)

# Use the SAME sequence of quarter returns (dates) as S&P path,
# but paths can start from index-specific base quarters. We map returns 1:1.
cr_step_returns <- as.numeric(beta_est) * sp_step_returns
cr_sim_levels   <- simulate_path(cr_base_level, cr_step_returns)

sim_cr <- tibble(
  indeks  = "CROBEX",
  kvartal = cr_future_qs,
  iznos   = cr_sim_levels,
  series  = paste0("Sim CROBEX via β×S&P (β=", round(beta_est, 2), ", R²=", round(rsq_est, 2), ")")
)

# ------------------------ HISTORICAL SERIES (to base) ------------------------

historical <- q_series %>%
  inner_join(anchors %>% select(indeks, base_kvartal = kvartal), by = "indeks") %>%
  filter(kvartal <= base_kvartal) %>%
  mutate(series = "Historical") %>%
  select(indeks, kvartal, iznos, series)

plot_df <- bind_rows(historical, sim_sp, sim_cr) %>%
  arrange(indeks, kvartal) %>%
  group_by(indeks) %>%
  mutate(
    base_lvl = max(iznos[kvartal == max(kvartal[series == "Historical"])], na.rm = TRUE),
    norm100  = 100 * iznos / base_lvl
  ) %>%
  ungroup()

# ------------------------ PLOTS ----------------------------------------------

ggplot(plot_df, aes(kvartal, norm100, color = series, linetype = series)) +
  geom_hline(yintercept = 100, linewidth = 0.3, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(data = plot_df %>% filter(series != "Historical"), size = 2) +
  facet_wrap(~ indeks, scales = "free_x") + boje_col +
  labs(
    title = "Beta-Mapped Shock: S&P Simulated Path and CROBEX via β × ΔS&P",
    subtitle = paste0(
      "Beta estimated on quarterly returns (lookback: ", beta_lookback_years, "y, ",
      nrow(reg_data), " obs).  β = ", round(beta_est, 2), ", R² = ", round(rsq_est, 2)
    ),
    x = "Quarter-end",
    y = "Index (Base quarter = 100)",
    color = "Series",
    linetype = "Series"
  )


# ------------------------ QUICK SUMMARY --------------------------------------

summary_tbl <- plot_df %>%
  group_by(indeks, series) %>%
  summarize(
    start_q    = min(kvartal),
    end_q      = max(kvartal),
    start_100  = norm100[kvartal == min(kvartal)],
    end_100    = norm100[kvartal == max(kvartal)],
    pct_change = (end_100 / start_100 - 1) * 100,
    .groups = "drop"
  )

print(summary_tbl)
