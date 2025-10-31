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

# Dohvat tečaja sa EUR/HRK (s ECB-a) i izračun kvartalnih promjena
library(ecb)
pom <- get_data("EXR.Q.USD+CHF.EUR.SP00.E") %>% mutate(datum = make_date(ifelse(substr(obstime,7,7)=="4",as.numeric(substr(obstime,1,4))+1,as.numeric(substr(obstime,1,4))), ifelse(substr(obstime,7,7)=="4",1,as.numeric(substr(obstime,7,7))*3+1), 1)-1) %>% select(datum,valuta=currency,tecaj=obsvalue) %>% arrange(datum) %>% spread(valuta,tecaj)
skopiraj(pom)

# 2. Stock market crash ####
api_key <- "08145abf6a02175d21bf50d7f12e0bd0"

pom1 <- fromJSON(paste0("http://api.marketstack.com/v2/eod?access_key=", api_key,"&symbols=GSPC.INDX&date_from=2000-01-01&date_to=2030-01-01&limit=10000"))$data %>% select(datum=date,iznos=close) %>% mutate(datum=as.Date(datum),indeks="S&P 500")
pom2 <- fromJSON(paste0("http://api.marketstack.com/v2/eod?access_key=", api_key,"&symbols=STOXX.INDX&date_from=2000-01-01&date_to=2030-01-01&limit=10000"))$data %>% select(datum=date,iznos=close) %>% mutate(datum=as.Date(datum),indeks="STOXX 600")
pom3 <- fromJSON("https://rest.zse.hr/web/Bvt9fe2peQ7pwpyYqODM/index-history/XZAG/HRZB00ICBEX6/2010-01-01/2030-01-01/json")$history %>% na.omit() %>% select(datum=date,iznos=last_value) %>% mutate(datum=as.Date(datum),iznos=as.numeric(iznos),indeks="CROBEX") 
pom4 <- fromJSON("https://www.mse.mk/api/index/MBI10/12") %>% mutate(datum=as.Date(tradingDate),indeks="MBI10") %>% select(datum,iznos=value,indeks)
dionice <- rbind(pom1,pom2,pom3,pom4)
ggplot(dionice,aes(x=datum,y=iznos,col=indeks)) + geom_line() + facet_wrap(~indeks,scales="free_y")

save(dionice,file = "stock_prices.Rda")

# 2.a. Pension fund crash ####
library(readxl)
penzioni_fondovi <- read_excel("AU_value_mpf_2008-2025.xlsx",skip = 1,col_types = c("date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","skip")) %>% 
  select(datum=1,2,3,4) %>% gather(indeks,iznos,-datum) %>% mutate(datum=as.Date(datum))
dionice <- penzioni_fondovi

# A. Historical Crash ####
stocks <- dionice %>% mutate(kvartal=ceiling_date(datum,unit="quarter")-1) %>% group_by(kvartal) %>% filter(datum==max(datum)) %>% 
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
gfc_anchor_start <- as.Date("2008-06-01")
gfc_anchor_end   <- as.Date("2008-09-30")
gfc_end          <- as.Date("2009-12-31")

# 2020 Covid: choose peak in 2019 Q2–2019 Q4, then run to 2021 Q4
covid_anchor_start <- as.Date("2019-06-30")
covid_anchor_end   <- as.Date("2019-12-31")
covid_end          <- as.Date("2021-12-31")

# --- Build paths for both indices and both scenarios -------------------------
indices <- c("SAVAz","KBPz","TRIGLAVz")

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
    subtitle = "Pension funds unit values | Peak chosen within an anchor window, then path through the crisis window",
    x = "Quarter",
    y = "Index level (Peak = 100)",
    color = "Index"
  )

# B. Scenario Shock ####

stocks <- dionice %>% filter(datum<="2025-09-30") %>% ungroup()

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

# D. Monte Carlo ####
# --- Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(scales)
library(stringr)

set.seed(42)  # reproducibility

stocks <- dionice %>% filter(datum<="2025-09-30") %>% ungroup()

# ------------------------ PARAMETERS -----------------------------------------
horizon_days        <- 252     # ~1 trading year
n_sims              <- 5000    # number of Monte Carlo paths per index
var_levels          <- c(0.01, 0.025, 0.05)  # 1%, 2.5%, 5%
lookback_years      <- 5       # use last N years of daily returns
use_block_bootstrap <- FALSE   # TRUE = simple 5-day blocks to keep some clustering
block_len           <- 5
history_years_on_plot <- 3     # how many years of historical levels to show

# ------------------------ HELPERS --------------------------------------------
# Generate next N business days (Mon-Fri) after a given date
next_n_bdays <- function(start_date, n) {
  # over-generate then filter Mon–Fri via wday (1=Mon,..,7=Sun with week_start=1)
  d <- seq.Date(from = start_date + lubridate::days(1), by = "day", length.out = n * 3)
  b <- d[lubridate::wday(d, week_start = 1) <= 5]
  b[seq_len(n)]
}

future_dates <- next_n_bdays(anchor_date, horizon_days)

# Build daily log returns over a lookback window
prep_log_rets <- function(df_idx, anchor_date, lookback_years) {
  df <- df_idx %>%
    arrange(datum) %>%
    filter(datum <= anchor_date,
           datum >= anchor_date %m-% years(lookback_years)) %>%
    mutate(r = log(iznos) - log(lag(iznos))) %>%
    filter(is.finite(r), !is.na(r))
  df$r
}

# Simple block bootstrap of log returns
sample_block_boot <- function(logrets, horizon_days, block_len) {
  n_blocks <- ceiling(horizon_days / block_len)
  # starting indices for overlapping blocks
  starts <- sample.int(length(logrets) - block_len + 1L, size = n_blocks, replace = TRUE)
  sam <- unlist(lapply(starts, function(s) logrets[s:(s + block_len - 1L)]), use.names = FALSE)
  sam[seq_len(horizon_days)]
}

# IID empirical sampling of log returns
sample_iid <- function(logrets, horizon_days) {
  sample(logrets, size = horizon_days, replace = TRUE)
}

# Simulate n_sims paths (levels) for one index
simulate_paths <- function(last_level, logrets_hist, horizon_days, n_sims,
                           use_block_bootstrap = FALSE, block_len = 5) {
  sim_mat <- matrix(NA_real_, nrow = horizon_days, ncol = n_sims)
  for (j in seq_len(n_sims)) {
    draws <- if (use_block_bootstrap) {
      sample_block_boot(logrets_hist, horizon_days, block_len)
    } else {
      sample_iid(logrets_hist, horizon_days)
    }
    # cumulative log-returns -> level path
    sim_mat[, j] <- as.numeric(last_level * exp(cumsum(draws)))
  }
  sim_mat
}

# Pointwise quantiles for each day across simulations
pointwise_quantiles <- function(sim_mat, probs) {
  apply(sim_mat, 1, quantile, probs = probs, na.rm = TRUE) %>% t() %>% as.data.frame()
}

# ------------------------ PREP DATA ------------------------------------------
# Expect: stocks with datum (Date), indeks, iznos
stopifnot(all(c("datum", "indeks", "iznos") %in% names(stocks)))
indices <- unique(stocks$indeks)

anchor_date <- max(stocks$datum, na.rm = TRUE)

# Historical levels to display (last X years)
hist_levels <- stocks %>%
  filter(datum >= anchor_date %m-% years(history_years_on_plot),
         datum <= anchor_date) %>%
  arrange(indeks, datum) %>%
  mutate(series = "Historical")

# Future business-day dates
future_dates <- next_n_bdays(anchor_date, horizon_days)

# ------------------------ RUN MONTE CARLO PER INDEX --------------------------
sim_quantiles_long <- purrr::map_dfr(indices, function(ix) {
  df_idx <- stocks %>% dplyr::filter(indeks == ix)
  last_row <- df_idx %>% dplyr::filter(datum == max(datum)) %>% dplyr::slice_tail(n = 1)
  last_level <- last_row$iznos[[1]]
  
  # Historical log returns (lookback)
  logrets <- prep_log_rets(df_idx, anchor_date, lookback_years)
  if (length(logrets) < 60) {
    warning("Very few daily returns for ", ix, " in lookback; expanding to all history.")
    logrets <- df_idx %>%
      dplyr::arrange(datum) %>%
      dplyr::mutate(r = log(iznos) - log(dplyr::lag(iznos))) %>%
      dplyr::filter(is.finite(r), !is.na(r)) %>%
      dplyr::pull(r)
  }
  
  # Simulate
  sim_mat <- simulate_paths(
    last_level, logrets, horizon_days, n_sims,
    use_block_bootstrap = use_block_bootstrap, block_len = block_len
  )
  
  # Pointwise quantiles (ensure fixed order & names)
  probs <- c(0.01, 0.025, 0.05, 0.50)
  q_mat <- apply(sim_mat, 1, stats::quantile, probs = probs, na.rm = TRUE) # 4 x T
  q_df  <- as.data.frame(t(q_mat))                                         # T x 4
  names(q_df) <- c("q1", "q2_5", "q5", "q50")
  
  tibble::tibble(
    indeks = ix,
    datum  = future_dates,
    q1     = q_df$q1,
    q2_5   = q_df$q2_5,
    q5     = q_df$q5,
    q50    = q_df$q50
  )
})

# ------------------------ COMBINE FOR PLOTTING -------------------------------
# Normalize each index to 100 at anchor_date for a clean comparison
base_levels <- hist_levels %>%
  group_by(indeks) %>%
  slice_max(datum, n = 1) %>%
  transmute(indeks, base = iznos)

hist_plot <- hist_levels %>%
  left_join(base_levels, by = "indeks") %>%
  mutate(norm100 = 100 * iznos / base)

sim_plot <- sim_quantiles_long %>%
  left_join(base_levels, by = "indeks") %>%
  mutate(
    norm_q1   = 100 * q1   / base,
    norm_q2_5 = 100 * q2_5 / base,
    norm_q5   = 100 * q5   / base,
    norm_q50  = 100 * q50  / base
  )

# ------------------------ PLOTS ----------------------------------------------
cols <- c(
  "Historical"   = "#2C7FB8",
  "VaR 2.5% path"= "#F03B20",
  "Median (50%)" = "#6A6A6A"
)

ggplot() +
  # historical
  geom_line(data = hist_plot,
            aes(datum, norm100, color = "Historical"), linewidth = 0.8) +
  # worst band: 1%..5%
  geom_ribbon(data = sim_plot,
              aes(datum, ymin = norm_q1, ymax = norm_q5, fill = "1%–5% band"),
              alpha = 0.25, inherit.aes = FALSE) +
  # VaR 2.5% path
  geom_line(data = sim_plot,
            aes(datum, norm_q2_5, color = "VaR 2.5% path"), linewidth = 1) +
  # (optional) median
  geom_line(data = sim_plot,
            aes(datum, norm_q50, color = "Median (50%)"), linewidth = 0.5, linetype = "dashed") +
  facet_wrap(~ indeks, scales = "free_y") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = c("1%–5% band" = "#F03B20")) +
  labs(
    title = "Monte Carlo (Daily): Worst-Case Bands & VaR 2.5% Path for Next Year",
    subtitle = paste0(
      "Empirical resampling of daily log returns (lookback ", lookback_years,
      "y, ", n_sims, " sims). Ribbon = 1%–5% worst band; line = 2.5% path."
    ),
    x = "Date", y = "Index (Anchor = 100)",
    color = "Series", fill = ""
  )


# ------------------------ QUICK CHECKS ---------------------------------------
# Show implied one-day VaR (historical) for reference
var_table <- stocks %>%
  group_by(indeks) %>%
  filter(datum <= anchor_date, datum >= anchor_date %m-% years(lookback_years)) %>%
  arrange(datum, .by_group = TRUE) %>%
  mutate(r = iznos / lag(iznos) - 1) %>%
  summarize(
    n_days   = sum(!is.na(r)),
    VaR_2_5d = quantile(r, probs = 0.025, na.rm = TRUE),
    VaR_1d   = quantile(r, probs = 0.01,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("VaR_"), ~ scales::percent(.x, accuracy = 0.01)))

print(var_table)

# E. GARCH simulation ####
# --- Libraries ---------------------------------------------------------------
# install.packages(c("rugarch","dplyr","tidyr","lubridate","ggplot2","purrr","scales"))
library(rugarch)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(scales)

set.seed(42)

# ------------------------ PARAMETERS -----------------------------------------
horizon_days         <- 252        # ~1 trading year
n_sims               <- 5000       # Monte Carlo paths per index
history_years_plot   <- 3          # years of history to show on the chart
fit_lookback_years   <- 7          # years of daily data to fit the model
garch_model          <- "sGARCH"   # "sGARCH" or "eGARCH"
dist_model           <- "std"      # "norm" or "std" (Student-t; better tails)
shock_override       <- TRUE       # inject a large negative shock on day 1?
shock_day            <- 1          # which simulated day gets the shock
shock_z              <- -4         # standardized innovation z_t to force (e.g., -4σ)

# ------------------------ HELPERS --------------------------------------------
# Next N business days (Mon-Fri), locale-agnostic
next_n_bdays <- function(start_date, n) {
  d <- seq.Date(from = start_date + days(1), by = "day", length.out = n * 3)
  b <- d[wday(d, week_start = 1) <= 5]
  b[seq_len(n)]
}

# Prepare daily log-returns with a lookback window
prep_logrets <- function(df_idx, anchor_date, lookback_years) {
  df_idx %>%
    arrange(datum) %>%
    filter(datum <= anchor_date,
           datum >= anchor_date %m-% years(lookback_years)) %>%
    transmute(
      datum,
      r = log(iznos) - log(lag(iznos))
    ) %>%
    filter(is.finite(r), !is.na(r))
}

# Fit (E)GARCH(1,1) to log-returns
fit_garch <- function(returns_vec, model = "sGARCH", dist = "std") {
  spec <- ugarchspec(
    variance.model = list(model = model, garchOrder = c(1,1)),
    mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
    distribution.model = dist
  )
  ugarchfit(spec = spec, data = returns_vec, solver = "hybrid")
}

# Simulate paths (returns) from fitted model with optional custom z_t shocks
sim_garch_paths <- function(fit, horizon_days, n_sims,
                            shock_override = FALSE, shock_day = 1, shock_z = -4,
                            dist_model = "std") {
  # Helper to get standardized-t draws if the model used Student-t
  rzt <- function(n, df) {
    # standardize to unit variance: t / sqrt(df/(df-2))
    rt(n, df = df) / sqrt(df / (df - 2))
  }
  
  if (shock_override) {
    # Build custom standardized innovations Z: [T x M]
    if (dist_model == "std" && "shape" %in% names(coef(fit))) {
      df <- as.numeric(coef(fit)["shape"])
      Z  <- matrix(rzt(horizon_days * n_sims, df), nrow = horizon_days, ncol = n_sims)
    } else {
      Z  <- matrix(rnorm(horizon_days * n_sims), nrow = horizon_days, ncol = n_sims)
    }
    if (shock_day < 1 || shock_day > horizon_days) stop("shock_day out of range")
    Z[shock_day, ] <- shock_z
    
    sim <- ugarchsim(
      fit,
      n.sim       = horizon_days,
      m.sim       = n_sims,
      startMethod = "sample",
      custom.dist = list(name = "sample", distfit = Z)
    )
  } else {
    # Let ugarchsim draw from the fitted distribution (norm/std)
    sim <- ugarchsim(
      fit,
      n.sim       = horizon_days,
      m.sim       = n_sims,
      startMethod = "sample"
    )
  }
  
  # Extract simulated returns (on the same scale as the fitted data, i.e., log-returns)
  rmat <- sim@simulation$seriesSim
  if (!is.matrix(rmat)) rmat <- as.matrix(rmat)
  rmat
}


# Convert simulated returns to level paths (starting from last level)
returns_to_levels <- function(last_level, rmat) {
  # rmat is (T x M) of simple or log returns? seriesSim gives simulated *returns*:
  # For rugarch, the simulated series is on the same scale as input (we used log-returns),
  # so exponentiate cumulative sum to get price levels.
  paths <- apply(rmat, 2, function(r) last_level * exp(cumsum(r)))
  # Ensure T x M
  if (is.null(dim(paths))) paths <- matrix(paths, nrow = nrow(rmat))
  paths
}

# Pointwise quantiles across simulations (rows=time)
pointwise_q <- function(mat, probs) {
  apply(mat, 1, quantile, probs = probs, na.rm = TRUE) |> t() |> as.data.frame()
}

# ------------------------ PREP DATA ------------------------------------------
stopifnot(all(c("datum","indeks","iznos") %in% names(stocks)))
anchor_date <- max(stocks$datum, na.rm = TRUE)
indices     <- unique(stocks$indeks)

# Historical levels to plot (last N years)
hist_plot <- stocks %>%
  filter(datum >= anchor_date %m-% years(history_years_plot),
         datum <= anchor_date) %>%
  arrange(indeks, datum) %>%
  mutate(series = "Historical")

# Future business days
future_dates <- next_n_bdays(anchor_date, horizon_days)

# ------------------------ RUN PER INDEX --------------------------------------
sim_quantiles <- map_dfr(indices, function(ix) {
  df_idx <- stocks %>% filter(indeks == ix)
  last_row <- df_idx %>% filter(datum == max(datum)) %>% slice_tail(n = 1)
  last_level <- last_row$iznos[[1]]
  
  # Prepare returns for fitting
  r_df <- prep_logrets(df_idx, anchor_date, fit_lookback_years)
  # If too few obs, expand to full history
  if (nrow(r_df) < 250) {
    r_df <- df_idx %>%
      arrange(datum) %>%
      transmute(datum, r = log(iznos) - log(lag(iznos))) %>%
      filter(is.finite(r), !is.na(r))
  }
  if (nrow(r_df) < 100) stop("Not enough return history to fit GARCH for ", ix)
  
  # Fit model
  fit <- fit_garch(r_df$r, model = garch_model, dist = dist_model)
  
  # Simulate returns matrix
  rmat <- sim_garch_paths(
    fit, horizon_days, n_sims,
    shock_override = shock_override,
    shock_day = shock_day, shock_z = shock_z,
    dist_model = dist_model
  )
  
  # Convert to level paths
  lmat <- returns_to_levels(last_level, rmat)
  
  # Pointwise quantiles (1%, 2.5%, 5%, median)
  probs <- c(0.01, 0.025, 0.05, 0.50)
  qdf   <- pointwise_q(lmat, probs)
  names(qdf) <- c("q1","q2_5","q5","q50")
  
  tibble(
    indeks = ix,
    datum  = future_dates,
    q1     = qdf$q1,
    q2_5   = qdf$q2_5,
    q5     = qdf$q5,
    q50    = qdf$q50
  )
})

# ------------------------ NORMALIZE & COMBINE --------------------------------
base_levels <- hist_plot %>%
  group_by(indeks) %>%
  slice_max(datum, n = 1) %>%
  transmute(indeks, base = iznos)

hist_plot_n <- hist_plot %>%
  left_join(base_levels, by = "indeks") %>%
  mutate(norm100 = 100 * iznos / base)

sim_plot_n <- sim_quantiles %>%
  left_join(base_levels, by = "indeks") %>%
  mutate(
    norm_q1   = 100 * q1   / base,
    norm_q2_5 = 100 * q2_5 / base,
    norm_q5   = 100 * q5   / base,
    norm_q50  = 100 * q50  / base
  )

# ------------------------ PLOT ------------------------------------------------
cols <- c(
  "Historical"   = "#2C7FB8",
  "VaR 2.5% path"= "#F03B20",
  "Median (50%)" = "#6A6A6A"
)

ggplot() +
  geom_line(data = hist_plot_n,
            aes(datum, norm100, color = "Historical"), linewidth = 0.8) +
  geom_ribbon(data = sim_plot_n,
              aes(datum, ymin = norm_q1, ymax = norm_q5, fill = "1%–5% band"),
              alpha = 0.25) +
  geom_line(data = sim_plot_n,
            aes(datum, norm_q2_5, color = "VaR 2.5% path"), linewidth = 1) +
  geom_line(data = sim_plot_n,
            aes(datum, norm_q50, color = "Median (50%)"),
            linewidth = 0.6, linetype = "dashed") +
  facet_wrap(~ indeks, scales = "free_y") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = c("1%–5% band" = "#F03B20")) +
  labs(
    title = paste0("GARCH Monte Carlo (", garch_model, ", dist=", dist_model, "): Worst Bands & VaR 2.5% Path"),
    subtitle = paste0(
      "Lookback fit: ", fit_lookback_years, "y. ",
      if (shock_override) paste0("Shock override: z[", shock_day, "] = ", shock_z, ". ") else "",
      n_sims, " sims, horizon ~", horizon_days, " business days."
    ),
    x = "Date", y = "Index (Anchor = 100)",
    color = "Series", fill = ""
  )

# ------------------------ quick diagnostics ----------------------------------
# Optional: print parameter estimates per index
# You can run this separately per index if you want details like alpha/beta, shape (t-dof), etc.

