# -------------------------
# Setup & package install
# -------------------------
# Install packages if not already installed (run once)

#required_pkgs <- c("tidyverse","readxl","writexl","plm","ConvergenceClubs","lmtest","sandwich")
#new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
#if(length(new_pkgs)) install.packages(new_pkgs)

# Load packages
library(tidyverse)
library(readxl)
library(writexl)
library(plm)               # panel regressions
library(ConvergenceClubs)  # Phillips & Sul club convergence
library(lmtest)
library(sandwich)

# -------------------------
# 1) Read the data
# -------------------------
# Change filepath to your file
filepath <- "C:/Users/Dell/Downloads/EconomicIndicatorsDataset.xlsx"  # <-- change
raw <- read_excel(filepath)

# show column names to confirm
print(names(raw))
# Expected: "Country Name" "Country Code" "Variable" "2006" ... "2024"

# ---- rename to safe names ----
raw2 <- raw %>%
  rename(
    country = `Country Name`,
    country_code = `Country Code`,
    Variable = `Variable`    # keep exact; will convert to lower-case variable next
  )

# ---- pivot to long ----
data_long <- raw2 %>%
  pivot_longer(
    cols = matches("^20\\d{2}$"),   # columns 2006..2024
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(year),
    variable = as.character(Variable), # create a working name 'variable'
    country = as.character(country),
    value = as.numeric(value)          # ensure numeric
  ) %>%
  select(country, country_code, year, variable, value)

# quick check
glimpse(data_long)
# show sample
data_long %>% filter(country == "India") %>% slice_head(n = 12) %>% print()

# ---- map long variable text to short column names ----
data_long <- data_long %>%
  mutate(
    var_short = case_when(
      variable == "Foreign direct investment, net inflows (% of GDP)" ~ "FDI_pctGDP",
      variable == "GDP growth (annual %)" ~ "gdp_growth",
      variable == "GDP per capita, PPP (constant 2021 international $)" ~ "gdp_pc",
      variable == "Trade (% of GDP)" ~ "trade_pctGDP",
      TRUE ~ NA_character_
    )
  )

# check if any unmatched variable strings exist
unique_vars <- data_long %>% distinct(variable, var_short)
print(unique_vars)

# ---- pivot wider to get one row per country-year and one column per variable ----
panel <- data_long %>%
  filter(!is.na(var_short)) %>%
  select(country, country_code, year, var_short, value) %>%
  pivot_wider(names_from = var_short, values_from = value) %>%
  arrange(country, year)

# quick sanity checks
glimpse(panel)
# check missing counts per variable
colSums(is.na(panel %>% select(FDI_pctGDP, gdp_growth, gdp_pc, trade_pctGDP)))

# show India sample
panel %>% filter(country == "India") %>% slice_head(n = 12) %>% print()



# -------------------------
# 4) Descriptive Analysis
#    - median comparison
#    - rank ordering
#    - gap/ratio India vs peers
# -------------------------

# 4.1 Median comparison by year & variable
median_by_year <- panel %>%
  group_by(year) %>%
  summarise(
    med_gdp_pc = median(gdp_pc, na.rm = TRUE),
    med_gdp_growth = median(gdp_growth, na.rm = TRUE),
    med_FDI_pctGDP = median(FDI_pctGDP, na.rm = TRUE),
    med_trade_pctGDP = median(trade_pctGDP, na.rm = TRUE)
  ) %>% ungroup()

# Example: compare India to median (join)
india_vs_median <- panel %>%
  filter(country == "India") %>%
  left_join(median_by_year, by = "year") %>%
  mutate(
    gap_gdp_pc = gdp_pc - med_gdp_pc,
    ratio_gdp_pc = ifelse(!is.na(med_gdp_pc) & med_gdp_pc != 0, gdp_pc / med_gdp_pc, NA),
    gap_gdp_growth = gdp_growth - med_gdp_growth,
    gap_FDI_pctGDP = FDI_pctGDP - med_FDI_pctGDP,
    gap_trade_pctGDP = trade_pctGDP - med_trade_pctGDP
  )

# Export descriptive tables if desired
write_xlsx(list(median_by_year = median_by_year,
                india_vs_median = india_vs_median),
           path = "descriptive_tables.xlsx")

# 4.2 Rank ordering across BRICS (per year)
rank_by_year <- panel %>%
  group_by(year) %>%
  mutate(
    rank_gdp_pc = rank(-gdp_pc, ties.method = "min"),        # 1 = highest
    rank_gdp_growth = rank(-gdp_growth, ties.method = "min"),
    rank_FDI_pctGDP = rank(-FDI_pctGDP, ties.method = "min"),
    rank_trade_pctGDP = rank(-trade_pctGDP, ties.method = "min")
  ) %>%
  ungroup()

# Extract India's rank time series
india_ranks <- rank_by_year %>% filter(country == "India") %>%
  select(year, rank_gdp_pc, rank_gdp_growth, rank_FDI_pctGDP, rank_trade_pctGDP)

# -------------------------
# 5) σ-convergence (dispersion)
#    For GDP per capita: use log(gdp_pc)
#    For other indicators: compute sd(level) and optionally sd(log1p(value))
# -------------------------

# 5.1 sigma for GDP per capita (log)
sigma_gdp <- panel %>%
  group_by(year) %>%
  summarise(sd_log_gdp_pc = sd(log(gdp_pc), na.rm = TRUE),
            mean_log_gdp_pc = mean(log(gdp_pc), na.rm = TRUE),
            n = sum(!is.na(gdp_pc))) %>%
  ungroup()

# 5.2 sigma for other indicators (levels) - you may choose log1p if values positive
sigma_other <- panel %>%
  group_by(year) %>%
  summarise(sd_gdp_growth = sd(gdp_growth, na.rm = TRUE),
            sd_FDI_pctGDP = sd(FDI_pctGDP, na.rm = TRUE),
            sd_trade_pctGDP = sd(trade_pctGDP, na.rm = TRUE)) %>%
  ungroup()

# Optionally compute sd(log1p) if you want a log-like transform for FDI/trade:
sigma_other_log1p <- panel %>%
  group_by(year) %>%
  summarise(
    sd_log1p_FDI = sd(log1p(FDI_pctGDP), na.rm = TRUE),
    sd_log1p_trade = sd(log1p(trade_pctGDP), na.rm = TRUE)
  ) %>% ungroup()

# Save sigma outputs
write_xlsx(list(sigma_gdp = sigma_gdp, sigma_other = sigma_other),
           path = "sigma_results.xlsx")

# -------------------------
# 6) β-convergence
#    Two approaches:
#    (A) Cross-sectional (classic Barro-Sala-i-Martin) using start and end years
#    (B) Panel regression (lagged log level => growth), using plm (fixed effects)
# -------------------------

# Choose a delta T for cross-sectional test (e.g., 2001->2020; here our data starts 2006)
# We'll show a rolling approach: for each country compute average annual growth between first and last available year
# (A) CROSS-SECTIONAL β (simplified)
start_year <- min(panel$year, na.rm = TRUE)
end_year   <- max(panel$year, na.rm = TRUE)

# Prepare cross-section growth between start and end
cs_growth <- panel %>%
  filter(year %in% c(start_year, end_year)) %>%
  select(country, year, gdp_pc) %>%
  pivot_wider(names_from = year, values_from = gdp_pc, names_prefix = "y") %>%
  mutate(
    avg_annual_growth = (log(y2024) - log(y2006)) / (end_year - start_year) # change the names to match your years if different
  ) %>%
  drop_na()

# Cross-sectional regression: avg_annual_growth ~ log(initial level)
cs_model <- lm(avg_annual_growth ~ log(y2006), data = cs_growth)
summary(cs_model)

# (B) PANEL β: growth_it = alpha + beta * log(y_{i,t-1}) + fixed effects + time effects
# Build lagged log GDP per capita
panel <- panel %>%
  group_by(country) %>%
  arrange(country, year) %>%
  mutate(log_gdp_pc = log(gdp_pc),
         log_gdp_pc_lag = lag(log_gdp_pc, 1),
         ann_growth = gdp_growth) %>%   # your gdp_growth variable is annual growth in %
  ungroup()

# Remove rows where lag missing
panel_beta <- panel %>% filter(!is.na(log_gdp_pc_lag) & !is.na(ann_growth))

# Convert to pdata.frame for plm
pdata <- pdata.frame(panel_beta, index = c("country", "year"))

# Fixed effects panel regression
beta_fe <- plm(ann_growth ~ log_gdp_pc_lag, data = pdata, model = "within", effect = "twoways") # twoways: country + time
summary(beta_fe)

# Robust (clustered) SEs (cluster by country)
coeftest(beta_fe, vcov = vcovHC(beta_fe, type = "HC1", cluster="group"))

# Interpretation: negative coefficient on lagged log GDP per capita => beta-convergence

# -------------------------
# 7) Club convergence (Phillips & Sul) using ConvergenceClubs
#    The ConvergenceClubs package expects a matrix with rows = units (countries), columns = years.
#    We'll run it variable-wise. IMPORTANT: NO NAs permitted. If NAs exist, address them before running.
# -------------------------

# Helper to create matrix for convergence test from panel for a given variable:
make_matrix_for_club <- function(df, varname, countries = NULL, years = NULL) {
  tmp <- df %>%
    select(country, year, all_of(varname))
  if(!is.null(countries)) tmp <- tmp %>% filter(country %in% countries)
  if(!is.null(years)) tmp <- tmp %>% filter(year %in% years)
  mat <- tmp %>%
    pivot_wider(names_from = year, values_from = all_of(varname)) %>%
    arrange(country) %>%
    column_to_rownames("country") %>%
    as.matrix()
  return(mat)
}

# Example: prepare matrix for gdp_pc (ensure no NA)
mat_gdp_pc <- make_matrix_for_club(panel, "gdp_pc")
rownames(mat_gdp_pc)  # should show countries
# Check for NA
colSums(is.na(mat_gdp_pc))
rowSums(is.na(mat_gdp_pc))

# If NAs exist, you must decide how to handle them. Example: simple linear interpolation by country:
interpolate_na_by_row <- function(mat) {
  mat2 <- t(apply(mat, 1, function(x) {
    if(any(is.na(x))) {
      # simple linear approx for NAs; if NA at edges, carry nearest non-NA (na.locf type). Here we use approx
      idx <- which(!is.na(x))
      if(length(idx) < 2) return(x) # cannot interpolate
      xp <- approx(x = idx, y = x[idx], xout = seq_along(x), rule = 2)$y
      return(xp)
    } else return(x)
  }))
  rownames(mat2) <- rownames(mat)
  colnames(mat2) <- colnames(mat)
  return(mat2)
}

# If needed: mat_gdp_pc <- interpolate_na_by_row(mat_gdp_pc)

# Run club convergence (log-t) for GDP per capita
# Note: findClubs sorts units by last year automatically, and trims initial time fraction by default
gc_gdp <- findClubs(mat_gdp_pc)
summary(gc_gdp)
# Output tells you clubs and t-statistics per club

# For other variables (note: logging may be needed for positive-only series)
mat_trade <- make_matrix_for_club(panel, "trade_pctGDP")
mat_FDI   <- make_matrix_for_club(panel, "FDI_pctGDP")
mat_growth<- make_matrix_for_club(panel, "gdp_growth")  # growth rates

# If variables have zeros/negatives, do not log; run on levels. If all positive and you prefer log, transform:
# e.g., mat_FDI_log <- log1p(mat_FDI)  # log1p handles zeros.

gc_trade <- findClubs(mat_trade)
summary(gc_trade)

gc_FDI <- findClubs(mat_FDI)
summary(gc_FDI)

gc_growth <- findClubs(mat_growth)
summary(gc_growth)

# You can plot transition paths for the clubs (ConvergenceClubs has plotting methods)
plot(gc_gdp)   # default plot for convergence.clubs object (transition paths)

# -------------------------
# 8) Output saving and notes
# -------------------------
# Save results to RDS for later inspection
saveRDS(list(
  panel = panel,
  median_by_year = median_by_year,
  sigma_gdp = sigma_gdp,
  sigma_other = sigma_other,
  beta_fe = beta_fe,
  cs_model = cs_model,
  gc_gdp = gc_gdp,
  gc_trade = gc_trade,
  gc_FDI = gc_FDI,
  gc_growth = gc_growth
), file = "convergence_results.RDS")

# Export summary tables (example)
write_xlsx(list(sigma_gdp = sigma_gdp,
                sigma_other = sigma_other,
                india_ranks = india_ranks),
           path = "convergence_summary_tables.xlsx")

# -------------------------
# End of script
# -------------------------
###############################################
###      1. DESCRIPTIVE ANALYSIS            ###
###############################################

# ---- 1.1 Median comparison across BRICS ----
median_by_year <- panel %>%
  group_by(year) %>%
  summarise(
    med_gdp_pc = median(gdp_pc, na.rm = TRUE),
    med_gdp_growth = median(gdp_growth, na.rm = TRUE),
    med_FDI_pctGDP = median(FDI_pctGDP, na.rm = TRUE),
    med_trade_pctGDP = median(trade_pctGDP, na.rm = TRUE)
  ) %>% ungroup()

# ---- 1.2 India vs Median gaps ----
india_vs_median <- panel %>%
  filter(country == "India") %>%
  left_join(median_by_year, by = "year") %>%
  mutate(
    gap_gdp_pc       = gdp_pc - med_gdp_pc,
    ratio_gdp_pc     = gdp_pc / med_gdp_pc,
    gap_gdp_growth   = gdp_growth - med_gdp_growth,
    gap_FDI_pctGDP   = FDI_pctGDP - med_FDI_pctGDP,
    gap_trade_pctGDP = trade_pctGDP - med_trade_pctGDP
  )

# ---- 1.3 Rank ordering by year (1 = best) ----
rank_by_year <- panel %>%
  group_by(year) %>%
  mutate(
    rank_gdp_pc = rank(-gdp_pc, ties.method = "min"),
    rank_gdp_growth = rank(-gdp_growth, ties.method = "min"),
    rank_FDI_pctGDP = rank(-FDI_pctGDP, ties.method = "min"),
    rank_trade_pctGDP = rank(-trade_pctGDP, ties.method = "min")
  ) %>%
  ungroup()

india_ranks <- rank_by_year %>%
  filter(country == "India") %>%
  select(year, starts_with("rank_"))


###############################################
###      2. SIGMA (σ) CONVERGENCE           ###
###############################################

# ---- 2.1 Sigma for GDP per capita (log-based) ----
sigma_gdp <- panel %>%
  group_by(year) %>%
  summarise(
    sd_log_gdp_pc = sd(log(gdp_pc), na.rm = TRUE),
    mean_log_gdp_pc = mean(log(gdp_pc), na.rm = TRUE),
    n = sum(!is.na(gdp_pc))
  ) %>% ungroup()

# ---- 2.2 Sigma for other indicators (level-based) ----
sigma_other <- panel %>%
  group_by(year) %>%
  summarise(
    sd_gdp_growth = sd(gdp_growth, na.rm = TRUE),
    sd_FDI_pctGDP = sd(FDI_pctGDP, na.rm = TRUE),
    sd_trade_pctGDP = sd(trade_pctGDP, na.rm = TRUE)
  ) %>% ungroup()

# Optional log1p dispersion for positive series
sigma_other_log1p <- panel %>%
  group_by(year) %>%
  summarise(
    sd_log1p_FDI = sd(log1p(FDI_pctGDP), na.rm = TRUE),
    sd_log1p_trade = sd(log1p(trade_pctGDP), na.rm = TRUE)
  ) %>% ungroup()


###############################################
###      3. BETA (β) CONVERGENCE            ###
###############################################

library(plm)
library(lmtest)
library(sandwich)

# ---- 3.1 Cross-sectional β-convergence ----

start_year <- min(panel$year, na.rm = TRUE)
end_year   <- max(panel$year, na.rm = TRUE)

cs_data <- panel %>%
  filter(year %in% c(start_year, end_year)) %>%
  select(country, year, gdp_pc) %>%
  pivot_wider(names_from = year, values_from = gdp_pc) %>%
  mutate(
    avg_annual_growth =
      (log(!!sym(as.character(end_year))) -
         log(!!sym(as.character(start_year)))) /
      (end_year - start_year)
  )

cs_model <- lm(
  avg_annual_growth ~ log(!!sym(as.character(start_year))),
  data = cs_data
)
summary(cs_model)


# ---- 3.2 Panel β-convergence ----

panel_beta <- panel %>%
  group_by(country) %>%
  arrange(country, year) %>%
  mutate(
    log_gdp_pc      = log(gdp_pc),
    log_gdp_pc_lag  = lag(log_gdp_pc, 1),
    ann_growth      = gdp_growth     # already %
  ) %>%
  ungroup() %>%
  filter(!is.na(log_gdp_pc_lag))

pdata <- pdata.frame(panel_beta, index = c("country", "year"))

beta_fe <- plm(
  ann_growth ~ log_gdp_pc_lag,
  data = pdata,
  model = "within",
  effect = "twoways"
)

summary(beta_fe)
coeftest(beta_fe, vcov = vcovHC(beta_fe, type = "HC1", cluster = "group"))


###############################################
###      4. CLUB CONVERGENCE (Phillips–Sul)  ###
###############################################

library(ConvergenceClubs)

# --- Helper to create country × year matrices ---
make_matrix_for_club <- function(df, varname) {
  df %>%
    select(country, year, all_of(varname)) %>%
    pivot_wider(names_from = year, values_from = all_of(varname)) %>%
    arrange(country) %>%
    column_to_rownames("country") %>%
    as.matrix()
}

# ---- Matrices for each variable ----
mat_gdp_pc  <- make_matrix_for_club(panel, "gdp_pc")
mat_trade   <- make_matrix_for_club(panel, "trade_pctGDP")
mat_FDI     <- make_matrix_for_club(panel, "FDI_pctGDP")
mat_growth  <- make_matrix_for_club(panel, "gdp_growth")

# ---- Check for NAs ----
colSums(is.na(mat_gdp_pc))
rowSums(is.na(mat_gdp_pc))

# Optional interpolation if needed
interpolate_na_by_row <- function(mat) {
  t(apply(mat, 1, function(x) {
    if (any(is.na(x))) {
      idx <- which(!is.na(x))
      approx(x = idx, y = x[idx], xout = seq_along(x), rule = 2)$y
    } else x
  }))
}

# mat_gdp_pc <- interpolate_na_by_row(mat_gdp_pc)

# ---- Run Phillips–Sul ----
gc_gdp <- findClubs(mat_gdp_pc)
summary(gc_gdp)

gc_trade <- findClubs(mat_trade)
summary(gc_trade)

gc_FDI <- findClubs(mat_FDI)
summary(gc_FDI)

gc_growth <- findClubs(mat_growth)
summary(gc_growth)

# optional plot
plot(gc_gdp)
