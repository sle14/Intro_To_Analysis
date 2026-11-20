# ==============================================================
# Cross-Sectional Strategy: Real Data (healthcare stocks)
# Topic: Forecasting  |  Repo: Intro-To-Analysis
# --------------------------------------------------------------
# THE IDEA
# Each month, rank healthcare stocks by how they did over the past
# year (skipping the most recent month). Buy the top group (winners)
# and short the bottom group (losers).
#
# This is the real-data version of cross-sectional-strategy.R.
# Prices come from Yahoo Finance through the quantmod package, so
# you need an internet connection the first time you run it. After
# that, the script reuses a saved copy of the data.
#
# Educational only. Not financial or investment advice.
# ==============================================================

# ---- 1. Packages -------------------------------------------------
library(tidyverse)  # dplyr, tidyr, ggplot2
library(lubridate)  # dates
library(mosaic)     # favstats()
library(quantmod)   # getSymbols(), monthlyReturn()


# ---- 2. Settings you can change ----------------------------------
start_date <- "2015-01-01"
lookback   <- 12     # rank on the past 12 months...
skip       <- 1      # skip the most recent month (see notes at the bottom)
n_groups   <- 5      # 5 groups = quintiles (1 = losers, top group = winners)
cost_per_month <- 0.002   # rough trading-cost guess: 0.2% of the portfolio per month
benchmark_ticker <- "XLV" # a healthcare sector ETF, used only for comparison

# Healthcare stocks (today's companies; see the survivorship note below)
tickers <- c(
  # Drugs and biotech
  "JNJ", "PFE", "MRK", "LLY", "ABBV", "BMY", "AMGN", "GILD", "REGN",
  "VRTX", "BIIB", "INCY", "ZTS",
  # Medical devices and tools
  "ABT", "MDT", "SYK", "BSX", "EW", "BDX", "ZBH", "BAX", "ISRG", "TMO",
  "DHR", "A", "MTD", "WAT", "ILMN", "IDXX", "RMD", "HOLX", "COO", "TFX",
  "ALGN", "XRAY", "IQV",
  # Insurers and care providers
  "UNH", "CVS", "CI", "HUM", "ELV", "CNC", "MOH", "HCA", "UHS", "DVA",
  "LH", "DGX",
  # Drug distributors
  "MCK", "CAH", "COR"
)


# ---- 3. Get the data ---------------------------------------------
# Downloads monthly returns from Yahoo Finance. The first run saves
# them to data/healthcare-monthly-returns.csv so later runs are fast
# and give the same answers. Delete that file to download fresh data.
get_returns <- function(ticker) {
  tryCatch({
    px <- suppressWarnings(
      getSymbols(ticker, src = "yahoo", from = start_date,
                 auto.assign = FALSE)
    )
    r <- monthlyReturn(Ad(px))          # Ad() = adjusted close (includes dividends)
    tibble(month = floor_date(index(r), "month"),
           stock = ticker,
           ret   = as.numeric(r))
  }, error = function(e) NULL)          # skip tickers that fail
}

cache_file <- "data/healthcare-monthly-returns.csv"
dir.create("data", showWarnings = FALSE)

if (file.exists(cache_file)) {
  raw <- read_csv(cache_file, show_col_types = FALSE)
} else {
  raw <- map_dfr(c(tickers, benchmark_ticker), get_returns)
  write_csv(raw, cache_file)
}

# Which tickers did not download? (Yahoo is not perfect.)
failed <- setdiff(c(tickers, benchmark_ticker), unique(raw$stock))
if (length(failed) > 0) cat("Could not download:", failed, "\n")


# ---- 4. Clean the data -------------------------------------------
# a) Drop the current, unfinished month.
# b) Drop each stock's first month (it is a partial month).
# c) Keep only stocks that have EVERY month. Stocks with gaps break
#    the "past return" calculation.
raw <- raw |>
  filter(month < floor_date(Sys.Date(), "month")) |>
  filter(month > min(month))

n_total <- n_distinct(raw$month)

full_history <- raw |>
  group_by(stock) |>
  summarise(n_obs = n()) |>
  filter(n_obs == n_total) |>
  pull(stock)

cat("Stocks kept:", length(setdiff(full_history, benchmark_ticker)),
    "of", length(tickers), "\n")

benchmark <- raw |>
  filter(stock == benchmark_ticker) |>
  select(month, benchmark = ret)

returns <- raw |>
  filter(stock %in% full_history, stock != benchmark_ticker)

head(returns)


# ---- 5. Build the signal -----------------------------------------
# Signal = total return from 12 months ago up to 2 months ago.
# lag() makes sure we only use PAST data (no peeking at the future).
signals <- returns |>
  arrange(stock, month) |>
  group_by(stock) |>
  mutate(
    log_ret = log1p(ret),
    cum     = cumsum(log_ret),
    signal  = dplyr::lag(cum, skip + 1) - dplyr::lag(cum, lookback + 1)
  ) |>
  ungroup() |>
  drop_na(signal)


# ---- 6. Rank stocks and form groups ------------------------------
# Every month, split the stocks into equal groups by signal.
ranked <- signals |>
  group_by(month) |>
  mutate(group = ntile(signal, n_groups)) |>
  ungroup()

# Average return of each group, each month (equal-weighted)
group_returns <- ranked |>
  group_by(month, group) |>
  summarise(avg_ret = mean(ret), .groups = "drop")

# Average return of ALL the stocks together (the "universe")
universe <- ranked |>
  group_by(month) |>
  summarise(universe = mean(ret), .groups = "drop")


# ---- 7. Long-short portfolio -------------------------------------
# Long the winners (top group), short the losers (group 1).
top <- paste0("g", n_groups)

strategy <- group_returns |>
  pivot_wider(names_from = group, values_from = avg_ret,
              names_prefix = "g") |>
  arrange(month) |>
  mutate(
    long_short     = .data[[top]] - g1,
    long_short_net = long_short - cost_per_month   # after our rough cost guess
  ) |>
  left_join(universe,  by = "month") |>
  left_join(benchmark, by = "month")


# ---- 8. Results --------------------------------------------------
# Average return of each group over the whole period
ranked |>
  group_by(group) |>
  summarise(avg_monthly_return = mean(ret))

# Summary statistics of the long-short monthly returns
favstats(~ long_short, data = strategy)

# Annualized numbers (Sharpe ignores the risk-free rate to keep it simple)
summarize_returns <- function(x) {
  tibble(
    ann_return = mean(x) * 12,
    ann_vol    = sd(x) * sqrt(12),
    sharpe     = (mean(x) * 12) / (sd(x) * sqrt(12))
  )
}

bind_rows(
  `Long-short (before costs)` = summarize_returns(strategy$long_short),
  `Long-short (after costs)`  = summarize_returns(strategy$long_short_net),
  `Healthcare Universe`       = summarize_returns(strategy$universe),
  .id = "portfolio"
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))


# ---- 9. Plots ----------------------------------------------------
# Do past winners beat past losers? Look for bars rising left to right.
ranked |>
  group_by(group) |>
  summarise(avg_monthly_return = mean(ret)) |>
  ggplot(aes(x = factor(group), y = avg_monthly_return)) +
  geom_col(fill = "#2DD4BF") +
  labs(title = "Average monthly return by rank group (Healthcare Stocks)",
       subtitle = "Group 1 = past losers, top group = past winners",
       x = "Group", y = "Average monthly return") +
  theme_minimal()

# Growth of $1
growth <- strategy |>
  transmute(
    month,
    `Long-short (after costs)` = cumprod(1 + long_short_net),
    `Winners`                  = cumprod(1 + .data[[top]]),
    `Losers`                   = cumprod(1 + g1),
    `Healthcare universe`      = cumprod(1 + universe),
    `Sector ETF (XLV)`         = cumprod(1 + benchmark)
  ) |>
  pivot_longer(-month, names_to = "portfolio", values_to = "value")

ggplot(growth, aes(x = month, y = value, color = portfolio)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Growth of $1: Cross-Sectional Strategy vs. Healthcare Stocks",
       subtitle = "Real data from Yahoo Finance. Past results do not predict the future.",
       x = NULL, y = "Value of $1", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")


# ---- 10. Try it yourself -----------------------------------------
# 1. Change lookback to 6 or 3. Does the result change a lot? If a
#    strategy only works for one setting, be suspicious.
# 2. Set skip <- 0. Does skipping the latest month matter?
# 3. Set cost_per_month <- 0.005. How fast do costs eat the profits?
# 4. Change start_date (delete the CSV in data/ first). Is the result
#    the same in a different time period?
#
# IMPORTANT NOTES
# * SURVIVORSHIP BIAS: This list only has companies that exist today.
#   Healthcare firms that collapsed, were bought out, or were delisted
#   are missing, which flatters the results. Real research uses data
#   that includes dead companies.
# * SMALL UNIVERSE: About 50 stocks in 5 groups is only ~10 stocks per
#   group, so results are noisy and one stock can swing a group.
# * WHY SKIP A MONTH? Stocks often bounce back a little after a big
#   one-month move (short-term reversal), so researchers commonly
#   leave the most recent month out of the signal.
# * COSTS ARE ROUGH: Real trading and short-selling costs vary a lot.
# * Educational only. Not financial or investment advice.
