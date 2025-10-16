# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

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
pom2 <- nav_opce %>% filter(vrsta0=="Mirovinski" & vrsta1=="Obavezan") %>% dplyr::select(datum, vrsta2, cj_udjela_u_valuti, nav) %>% group_by(datum,vrsta2) %>% summarise(mirex=weighted.mean(x = cj_udjela_u_valuti,w = nav,na.rm = T)) %>% spread(vrsta2,mirex) %>% select(datum,mirex_b=3) %>% drop_na()

set.seed(42)


# 1) Prep and log-returns
pom2 <- pom2 %>% arrange(datum)
start_value <- pom2$mirex_b[1]            # e.g., 13.27075
dates <- pom2$datum
n_days <- nrow(pom2)

# Daily log returns (drop first NA)
log_ret <- diff(log(pom2$mirex_b))
mu_d  <- mean(log_ret, na.rm = TRUE)      # daily drift (log scale)
sd_d  <- sd(log_ret,   na.rm = TRUE)      # daily vol (log scale)

# 2) Monte Carlo simulate daily log-returns (Normal)
N <- 5000  # number of simulated paths
# Matrix of simulated log-returns: (n_days-1) x N
sim_logrets <- matrix(rnorm((n_days - 1) * N, mean = mu_d, sd = sd_d),
                      nrow = n_days - 1, ncol = N)

# 3) Convert to price/index paths
# cumulative log-returns → levels = start_value * exp(cumlogret)
cumlog_mat <- apply(sim_logrets, 2, cumsum)                    # (n_days-1) x N
levels_mat <- rbind(rep(log(start_value), N),                  # add day 0
                    cumlog_mat + log(start_value))             # log-levels
levels_mat <- exp(levels_mat)                                  # back to levels
# levels_mat: n_days x N aligned with 'dates'

# 4) Percentile bands per day
probs <- c(0.05, 0.10, 0.50, 0.90, 0.95)
q_mat <- t(apply(levels_mat, 1, quantile, probs = probs, na.rm = TRUE))
bands <- as.data.frame(q_mat)
colnames(bands) <- paste0("p", c("05","10","50","90","95"))
bands$datum <- dates

# 5) Plot fan chart + actual
ggplot() +
  # fan ribbons
  geom_ribbon(data = bands, aes(x = datum, ymin = p10, ymax = p90), alpha = 0.20, fill = "#3498db") +
  geom_ribbon(data = bands, aes(x = datum, ymin = p05, ymax = p95), alpha = 0.12, fill = "#3498db") +
  # median path
  geom_line(data = bands, aes(x = datum, y = p50), color = "#1f78b4", linewidth = 0.7, linetype = "solid") +
  # actual series
  geom_line(data = pom2, aes(x = datum, y = mirex_b), color = "black", linewidth = 0.8) +
  labs(
    title = "MIREX B – Stochastic Simulation (Monte Carlo) vs. Actual",
    subtitle = paste0("N = ", N, " paths | Daily log-returns ~ Normal(", round(mu_d,6), ", ", round(sd_d,6), ")"),
    x = "Date", y = "Index Level"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")

# 10K investment #####

# Assuming you have 'bands_value' from previous step
# Columns: datum, value_p05, value_p10, value_p50, value_p90, value_p95

# 1️⃣ Extract last available values (end of the period)
last_values <- bands_value %>%
  slice_tail(n = 1) %>%
  select(value_p05, value_p10, value_p50, value_p90, value_p95) %>%
  pivot_longer(everything(), names_to = "quantile", values_to = "portfolio_value") %>%
  mutate(
    quantile = factor(
      quantile,
      levels = c("value_p05", "value_p10", "value_p50", "value_p90", "value_p95"),
      labels = c("5th", "10th", "50th (Median)", "90th", "95th")
    )
  )

print(last_values)

# 2️⃣ Create bar chart of quantile values
ggplot(last_values, aes(x = quantile, y = portfolio_value, fill = quantile)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = dollar(round(portfolio_value, 0))),
            vjust = -0.5, size = 4, color = "black") +
  scale_fill_manual(values = c(
    "#e74c3c", "#f39c12", "#3498db", "#2ecc71", "#27ae60"
  )) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Final Portfolio Value ($10,000 Investment in MIREX B)",
    subtitle = "Distribution of simulated outcomes at end of period (Monte Carlo)",
    x = "Quantile of Distribution",
    y = "Portfolio Value (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
