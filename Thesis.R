rm(list=ls())

install.packages("httr2")
library(httr2)

FOR_R$Year <- as.integer(FOR_R$Year)

Data<-FOR_R

# ============================================================
# DATA CLEANING & DEFLATING
# Panel: Denmark, Germany, France, Sweden, Norway, 
#        Finland, Netherlands, Belgium
# ============================================================

library(tidyverse)

# ============================================================
# STEP 1: RENAME COLUMNS (shorter, cleaner names)
# ============================================================

Data <- Data %>%
  rename(
    country                = `Country`,
    year                   = `Year`,
    gdp                    = `GDP`,
    output_gap             = `Outputgab`,
    inflation              = `Inflation (prc_hicp_aind)`,
    population             = `Population (demo_pjan)`,
    capital_stock          = `Capital stock (nama_10_nfa_st - N11N (Total fixede assets net))`,
    gov_consumption        = `Government consumption (nama_10_gdp - Final consumption expenditure of general government: P3_S13))`,
    tax_revenues           = `Tax Revenues`,
    rd_expenditure         = `R&D Expenditure (rd_e_gerdtot - All sectors)`,
    human_capital          = `Human Captical (higher education share (edat_lfse_03))`,
    interest_rate          = `\r\nInterest rate (rt_lt_mcby_a)`,
    private_investment     = `Private investment (nama_10_gdp = P51G (Gross fixed capital formation))`,
    unemployment           = `Unemployment (une_rt_a)`,
    spiri_milex_share_gdp        = `Share of GDP`,
    spiri_milex_per_capita       = `Per capita (curret USD)`,
    spiri_milex_share_govspend   = `Share of government spending`,
    milex_total            = `Total defence expenditure (COFOG - Totel Defence: GF02 - TE)`,
    foreign_aid_1          = `Foreign military aid - (GF0203 - TE)...19`,
    rd_defence             = `R&D COFOG = GF0204, ESA item = P2 + P51G`,
    personnel_1            = `Compensation to employees COFOG = GF0201 (or GF02), ESA item = D1...21`,
    intermediate_1         = `Intermediate consumption COFOG (GF02 - P2)`,
    gfcf_1                 = `Gross fixed capital formation COFOG - GF02 - P51G...23`,
    other_milex            = `Other`,
    import_share           = `Import share`,
    personnel_pct          = `Compensation to employees COFOG = GF0201 (or GF02), ESA item = D1...26`,
    intermediate_pct       = `Intermediate consumption`,
    gfcf_pct               = `Gross fixed capital formation COFOG - GF02 - P51G...28`,
    milex_total_pct        = `Total defence expenditure`,
    foreign_aid_pct        = `Foreign military aid - (GF0203 - TE)...30`,
    gdp_deflator           = `GDP Deflator: Price index (implicit deflator), 2020=100, euro`
  )

Data %>% filter(country == "Denmark") %>% 
  select(year, gdp) %>% head(5)

cols_to_numeric <- c(
  "capital_stock",
  "rd_expenditure", 
  "milex_total",
  "foreign_aid_1",
  "rd_defence",
  "personnel_1",
  "intermediate_1",
  "gfcf_1",
  "other_milex",
  "import_share",
  "personnel_pct",
  "intermediate_pct",
  "gfcf_pct",
  "milex_total_pct",
  "foreign_aid_pct"
)

Data <- Data %>%
  mutate(across(all_of(cols_to_numeric), as.numeric))

# Convert 0s to NA for variables where 0 means missing/unreported
Data <- Data %>%
  mutate(
    rd_defence      = ifelse(rd_defence == 0, NA, rd_defence),
    foreign_aid_1   = ifelse(foreign_aid_1 == 0, NA, foreign_aid_1)
  )


# ============================================================
# STEP 2: DEFLATE NOMINAL MILLION-EURO VARIABLES
# Divide by (gdp_deflator / 100) to get real 2020 euros
# Variables to deflate: all Eurostat nominal million-euro cols
# ============================================================

vars_to_deflate <- c(
  "gdp",
  "capital_stock",
  "gov_consumption",
  "tax_revenues",
  "rd_expenditure",
  "private_investment",
  "milex_total",
  "foreign_aid_1",
  "rd_defence",
  "personnel_1",
  "intermediate_1",
  "gfcf_1"
)

Data <- Data %>%
  mutate(across(
    all_of(vars_to_deflate),
    ~ . / (gdp_deflator / 100),
    .names = "{.col}_real"
  ))

# ============================================================
# STEP 3: CREATE REAL PER CAPITA VARIABLES
# Divide each real variable by population
# ============================================================
# (dividing millions by individual people gives tiny numbers)
Data %>% filter(country == "Denmark") %>% 
  select(year, gdp_ppp, population) %>% 
  head(5)

Data <- Data %>%
  mutate(
    gdp_pc               = (gdp_real * 1000000) / population,
    capital_stock_pc     = (capital_stock_real * 1000000) / population,
    gov_consumption_pc   = (gov_consumption_real * 1000000) / population,
    tax_revenues_pc      = (tax_revenues_real * 1000000) / population,
    rd_expenditure_pc    = (rd_expenditure_real * 1000000) / population,
    private_invest_pc    = (private_investment_real * 1000000) / population,
    milex_total_pc       = (milex_total_real * 1000000) / population,
    rd_defence_pc        = (rd_defence_real * 1000000) / population,
    personnel_pc         = (personnel_1_real * 1000000) / population,
    intermediate_pc      = (intermediate_1_real * 1000000) / population,
    gfcf_pc              = (gfcf_1_real * 1000000) / population,
    foreign_aid_1_pc     = (foreign_aid_1_real * 1000000) / population
  )

Data %>% filter(country == "Denmark") %>% 
  select(year, gdp, gdp_real, gdp_pc) %>% 
  head(5)
Data %>% filter(country == "Denmark", year == 2020) %>% 
  select(year, gdp, gdp_real, gdp_pc)

# ============================================================
# STEP 4: DESCRIPTIVE STATISTICS
# ============================================================

#run descriptive stats with more decimals for pct variables
pct_vars <- c(
  "output_gap",
  "inflation",
  "human_capital",
  "unemployment",
  "spiri_milex_share_gdp",
  "spiri_milex_share_govspend",
  "import_share",
  "personnel_pct",
  "intermediate_pct",
  "gfcf_pct",
  "milex_total_pct",
  "foreign_aid_pct"
)

other_vars <- Data %>%
  select(-country, -year, -all_of(pct_vars)) %>%
  names()

desc_levels <- Data %>%
  select(all_of(other_vars)) %>%
  summarise(across(
    everything(),
    list(
      Min    = ~round(min(., na.rm = TRUE), 1),
      Mean   = ~round(mean(., na.rm = TRUE), 1),
      Median = ~round(median(., na.rm = TRUE), 1),
      Max    = ~round(max(., na.rm = TRUE), 1),
      SD     = ~round(sd(., na.rm = TRUE), 1),
      N      = ~sum(!is.na(.))
    ),
    .names = "{.col}__{.fn}"
  ))

desc_pcts <- Data %>%
  select(all_of(pct_vars)) %>%
  summarise(across(
    everything(),
    list(
      Min    = ~round(min(., na.rm = TRUE), 4),
      Mean   = ~round(mean(., na.rm = TRUE), 4),
      Median = ~round(median(., na.rm = TRUE), 4),
      Max    = ~round(max(., na.rm = TRUE), 4),
      SD     = ~round(sd(., na.rm = TRUE), 4),
      N      = ~sum(!is.na(.))
    ),
    .names = "{.col}__{.fn}"
  ))

desc_stats <- bind_cols(desc_levels, desc_pcts) %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = Stat, values_from = value)

print(n=40,desc_stats)
write.csv(desc_stats, "descriptive_statistics.csv", row.names = FALSE)

Data %>% filter(milex_total == max(milex_total, na.rm=TRUE)) %>% 
  select(country, year, milex_total)
Data %>% filter(tax_revenues_pc == max(tax_revenues_pc, na.rm=TRUE)) %>% 
  select(country, year, tax_revenues_pc)

# ============================================================
# DESCRIPTIVE STATISTICS TABLE FOR OVERLEAF
# ============================================================

library(tidyverse)

# Select the variables we want in the table
table_vars <- c(
  # GDP
  "gdp_pc",
  # Control variables
  "output_gap",
  "inflation",
  "population",
  "capital_stock_pc",
  "gov_consumption_pc",
  "tax_revenues_pc",
  "rd_expenditure_pc",
  "human_capital",
  "interest_rate",
  "private_invest_pc",
  "unemployment",
  "import_share",
  # SIPRI military spending
  "spiri_milex_share_gdp",
  "spiri_milex_per_capita",
  "spiri_milex_share_govspend",
  # Eurostat military spending
  "milex_total_pc",
  "foreign_aid_1_pc",
  "rd_defence_pc",
  "personnel_pc",
  "intermediate_pc",
  "gfcf_pc"
)

# Clean variable labels for the table
var_labels <- c(
  "GDP per capita",
  # Controls
  "Output gap (\\%)",
  "Inflation, HICP (\\%)",
  "Population",
  "Capital stock",
  "Government consumption",
  "Tax revenues",
  "R\\&D expenditure",
  "Human capital",
  "Interest rate (\\%)",
  "Private investment",
  "Unemployment rate (\\%)",
  "Import share (\\% GDP)",
  # SIPRI
  "Milex, share of GDP",
  "Milex, per capita (USD)",
  "Milex, share of gov. spending",
  # Eurostat
  "Total defence expenditure",
  "Foreign military aid",
  "Defence R\\&D",
  "Personnel expenditure",
  "Intermediate consumption",
  "Gross fixed capital formation"
)

# Compute statistics
desc_table <- Data %>%
  select(all_of(table_vars)) %>%
  summarise(across(
    everything(),
    list(
      Mean   = ~round(mean(., na.rm = TRUE), 2),
      SD     = ~round(sd(., na.rm = TRUE), 2),
      Min    = ~round(min(., na.rm = TRUE), 2),
      Median = ~round(median(., na.rm = TRUE), 2),
      Max    = ~round(max(., na.rm = TRUE), 2),
      N      = ~sum(!is.na(.))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = Stat, values_from = value) %>%
  mutate(Variable = var_labels)

# Generate LaTeX code
latex_table <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Descriptive Statistics}\n",
  "\\label{tab:desc_stats}\n",
  "\\small\n",
  "\\begin{adjustbox}{max width=\\textwidth}\n",
  "\\begin{tabular}{llrrrrrr}\n",
  "\\hline\\hline\n",
  "Variables (per capita, constant prices, in euros) & Data source & Min & Mean & Median & Max & Std. dev. & N \\\\\n",
  "\\hline\n",
  "\\multicolumn{8}{l}{\\textit{GDP and Controls}} \\\\\n"
)

# Add rows
for (i in 1:nrow(desc_table)) {
  # Add section headers
  if (i == 14) latex_table <- paste0(latex_table, 
                                     "\\multicolumn{7}{l}{\\textit{SIPRI Military Spending}} \\\\\n")
  if (i == 17) latex_table <- paste0(latex_table, 
                                     "\\multicolumn{7}{l}{\\textit{Eurostat Military Spending}} \\\\\n")
  
  row <- desc_table[i, ]
  latex_table <- paste0(
    latex_table,
    row$Variable, " & ",
    formatC(row$Mean, format = "f", digits = 2), " & ",
    formatC(row$SD, format = "f", digits = 2), " & ",
    formatC(row$Min, format = "f", digits = 2), " & ",
    formatC(row$Median, format = "f", digits = 2), " & ",
    formatC(row$Max, format = "f", digits = 2), " & ",
    row$N, " \\\\\n"
  )
}

latex_table <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Descriptive Statistics}\n",
  "\\label{tab:desc_stats}\n",
  "\\small\n",
  "\\begin{adjustbox}{max width=\\textwidth}\n",
  "\\begin{tabular}{llrrrrrr}\n",
  "\\hline\\hline\n",
  "Variables & Data source & Min & Mean & Median & Max & Std. dev. & N \\\\n",
  "\\hline\n",
  "\\multicolumn{8}{l}{\\textit{GDP and Controls}} \\\\\n"
)
# Data sources for each variable
data_sources <- c(
  "Eurostat",    # gdp_ppp
  "AMECO",       # output_gap
  "Eurostat",    # inflation
  "Eurostat",    # population
  "Eurostat",    # capital_stock
  "Eurostat",    # gov_consumption
  "Eurostat",    # tax_revenues
  "Eurostat",    # rd_expenditure
  "Eurostat",    # human_capital
  "Eurostat",    # interest_rate
  "Eurostat",    # private_investment
  "Eurostat",    # unemployment
  "Eurostat",    # import_share
  "SIPRI",       # spiri_milex_share_gdp
  "SIPRI",       # spiri_milex_per_capita
  "SIPRI",       # spiri_milex_share_govspend
  "Eurostat",    # milex_total
  "Eurostat",    # foreign_aid
  "Eurostat",    # rd_defence
  "Eurostat",    # personnel
  "Eurostat",    # intermediate
  "Eurostat"     # gfcf
)

# Add data sources to desc_table
desc_table$Source <- data_sources

# Generate rows with source column
for (i in 1:nrow(desc_table)) {
  if (i == 14) latex_table <- paste0(latex_table,
                                     "\\multicolumn{8}{l}{\\textit{SIPRI Military Spending}} \\\\\n")
  if (i == 17) latex_table <- paste0(latex_table,
                                     "\\multicolumn{8}{l}{\\textit{Eurostat Military Spending}} \\\\\n")
  
  row <- desc_table[i, ]
  latex_table <- paste0(
    latex_table,
    row$Variable, " & ",
    row$Source, " & ",
    formatC(row$Min, format = "f", digits = 2), " & ",
    formatC(row$Mean, format = "f", digits = 2), " & ",
    formatC(row$Median, format = "f", digits = 2), " & ",
    formatC(row$Max, format = "f", digits = 2), " & ",
    formatC(row$SD, format = "f", digits = 2), " & ",
    row$N, " \\\\\n"
  )
}

latex_table <- paste0(
  latex_table,
  "\\hline\\hline\n",
  "\\multicolumn{8}{l}{\\footnotesize Note: All real variables deflated by GDP deflator (2020=100).} \\\\\n",
  "\\multicolumn{8}{l}{\\footnotesize Sources: Eurostat, SIPRI, AMECO.} \\\\\n",
  "\\end{tabular}\n",
  "\\end{adjustbox}\n",
  "\\end{table}\n"
)

cat(latex_table)
writeLines(latex_table, "descriptive_statistics.tex")

print(desc_table)

Data %>%
  summarise(
    Mean = mean(spiri_milex_share_govspend, na.rm = TRUE),
    SD   = sd(spiri_milex_share_govspend, na.rm = TRUE),
    Min  = min(spiri_milex_share_govspend, na.rm = TRUE),
    Max  = max(spiri_milex_share_govspend, na.rm = TRUE),
    median(spiri_milex_share_govspend)
  )

# ============================================================
# STEP 5: CREATE LOG FIRST DIFFERENCES
# For LP estimation following Olejnik (2023)
# dlog(x) = log(x_t) - log(x_t-1)
# ============================================================

Data <- Data %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    dlog_gdp             = log(gdp_pc) - lag(log(gdp_pc)),
    dlog_milex           = log(milex_total_pc) - lag(log(milex_total_pc)),
    dlog_gov_cons        = log(gov_consumption_pc) - lag(log(gov_consumption_pc)),
    dlog_personnel       = log(personnel_pc) - lag(log(personnel_pc)),
    dlog_intermediate    = log(intermediate_pc) - lag(log(intermediate_pc)),
    dlog_gfcf            = log(gfcf_pc) - lag(log(gfcf_pc)),
    dlog_rd_defence      = log(rd_defence_pc) - lag(log(rd_defence_pc)),
    dlog_private_invest  = log(private_invest_pc) - lag(log(private_invest_pc)),
    dlog_capital         = log(capital_stock_pc) - lag(log(capital_stock_pc))
  ) %>%
  ungroup()

# ============================================================
# STEP 6: COMPUTE CONVERSION FACTORS FOR MULTIPLIERS
# multiplier = beta * (mean GDP / mean spending variable)
# ============================================================

conversion_factors <- Data %>%
  group_by(country) %>%
  summarise(
    cf_milex        = mean(gdp_real, na.rm = TRUE) / mean(milex_total_real, na.rm = TRUE),
    cf_personnel    = mean(gdp_real, na.rm = TRUE) / mean(personnel_1_real, na.rm = TRUE),
    cf_intermediate = mean(gdp_real, na.rm = TRUE) / mean(intermediate_1_real, na.rm = TRUE),
    cf_gfcf         = mean(gdp_real, na.rm = TRUE) / mean(gfcf_1_real, na.rm = TRUE),
    cf_rd_defence   = mean(gdp_real, na.rm = TRUE) / mean(rd_defence_real, na.rm = TRUE)
  )
print(conversion_factors)

# ============================================================
# STEP 7: QUICK SANITY CHECK
# ============================================================

cat("\n--- Data dimensions ---\n")
cat("Rows:", nrow(Data), "| Columns:", ncol(Data), "\n")

cat("\n--- Years covered per country ---\n")
print(Data %>% group_by(country) %>% 
        summarise(from = min(year), to = max(year), n = n()))

cat("\n--- Sample: Denmark deflated milex (first 5 rows) ---\n")
print(Data %>% filter(country == "Denmark") %>% 
        select(year, milex_total, gdp_deflator, milex_total_real, milex_total_pc) %>% 
        head(5))

cat("\n--- Missing values in key variables ---\n")
print(colSums(is.na(Data %>% select(dlog_gdp, dlog_milex, dlog_gov_cons,
                                    dlog_personnel, dlog_gfcf))))


# ============================================================
# CONTROL VARIABLE VALIDATION REGRESSION
# Single OLS regression at h=0 to check significance of controls
# ============================================================

validation_data <- Data %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(y_fwd = lead(dlog_gdp, 1)) %>%   # h=1 not h=0
  ungroup() %>%
  filter(!is.na(y_fwd), !is.na(dlog_milex),
         !is.na(dlog_gdp), !is.na(output_gap),
         !is.na(inflation), !is.na(unemployment),
         !is.na(dlog_gov_cons), !is.na(dlog_private_invest),
         !is.na(dlog_capital))


fit_validation <- feols(
  y_fwd ~ dlog_milex
  + lag(dlog_gdp, 1)
  + output_gap
  + inflation
  + unemployment
  + dlog_gov_cons
  + dlog_private_invest
  + dlog_capital
  | country + year,
  data     = validation_data,
  cluster  = ~country,
  panel.id = ~country + year
)

# Full coefficient table with stars
etable(fit_validation,
       title   = "Control variable validation (h=0)",
       digits  = 4,
       se.below = TRUE)

# ============================================================
# LOCAL PROJECTIONS - TOTAL MILITARY SPENDING ON GDP
# Following Jordà (2005) and Olejnik (2023)
# Panel: 8 countries, 1999-2024
# ============================================================

# Install packages if needed
install.packages(c("fixest", "sandwich", "lmtest"))
library(fixest)
library(tidyverse)

# ============================================================
# SETUP
# ============================================================

H <- 5  # number of horizons (0 to 5 years ahead)
n_lags <- 2  # lag length (adjust based on AIC later)

# Store results
lp_results <- data.frame(
  horizon  = 0:H,
  beta     = NA,
  se       = NA,
  ci_lower = NA,
  ci_upper = NA
)

# ============================================================
# AIC LAG SELECTION: choose P per horizon h
# ============================================================

H      <- 5
max_lags <- 3  # maximum lags to consider

# Store chosen lag per horizon
aic_lags <- data.frame(horizon = 0:H, best_lags = NA, AIC = NA)

for (h in 0:H) {
  
  Data_h <- Data %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(y_fwd = lead(dlog_gdp, h)) %>%
    ungroup() %>%
    filter(!is.na(y_fwd), !is.na(dlog_milex),
           !is.na(dlog_gdp), !is.na(output_gap),
           !is.na(inflation), !is.na(unemployment))
  
  best_aic  <- Inf
  best_p    <- 1
  
  for (p in 1:max_lags) {
    
    formula_p <- as.formula(paste0(
      "y_fwd ~ dlog_milex",
      " + ", paste0("lag(dlog_gdp, ",   1:p, ")", collapse = " + "),
      " + ", paste0("lag(dlog_milex, ", 1:p, ")", collapse = " + "),
      " + output_gap + inflation + unemployment",
      " + dlog_gov_cons + dlog_private_invest + dlog_capital",
      " | country + year"
    ))
    
    fit_p <- feols(formula_p, data = Data_h,
                   cluster = ~country, panel.id = ~country + year)
    
    aic_p <- AIC(fit_p)
    
    if (aic_p < best_aic) {
      best_aic <- aic_p
      best_p   <- p
    }
  }
  
  aic_lags$best_lags[h + 1] <- best_p
  aic_lags$AIC[h + 1]       <- best_aic
  
  cat("Horizon", h, "-> best lags =", best_p, "| AIC =", round(best_aic, 2), "\n")
}

print(aic_lags)

for (h in 0:H) {
  n_lags <- aic_lags$best_lags[h + 1]  # use AIC-selected lag for this horizon
  # ... rest of loop unchanged
}

# ============================================================
# LP LOOP: estimate one regression per horizon h
# ============================================================


for (h in 0:H) {
  
  # Use AIC-selected lag for this horizon
  n_lags <- aic_lags$best_lags[h + 1]
  
  Data_h <- Data %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(y_fwd = lead(dlog_gdp, h)) %>%
    ungroup() %>%
    filter(!is.na(y_fwd), !is.na(dlog_milex),
           !is.na(dlog_gdp), !is.na(output_gap),
           !is.na(inflation), !is.na(unemployment))
  
  # LP regression with country and time fixed effects
  # y_{i,t+h} = alpha_i + delta_t + beta_h * dlog_milex_{i,t}
  #           + lags of y and milex + controls + error

  
  formula_h <- as.formula(paste0(
    "y_fwd ~ dlog_milex",
    " + ", paste0("lag(dlog_gdp, ",   1:n_lags, ")", collapse = " + "),
    " + output_gap + inflation + unemployment + dlog_gov_cons",
    " + dlog_private_invest + dlog_capital",
    " | country + year"
  ))
  
  fit_h <- feols(formula_h, data = Data_h,
                 cluster = ~country, # cluster SEs by country
                 panel.id = ~country + year)
  # Extract coefficient and SE on dlog_milex
  lp_results$beta[h + 1]   <- coef(fit_h)["dlog_milex"]
  lp_results$se[h + 1]     <- se(fit_h)["dlog_milex"]
  lp_results$n_lags[h + 1] <- n_lags
}
´
  
  ## Problem 2 — The collinearity warning is serious
 
#The variable 'lag(dlog_milex, 1)' has been removed because of collinearity



# ============================================================
# COMPUTE CONFIDENCE INTERVALS
# ============================================================

lp_results <- lp_results %>%
  mutate(
    ci_lower = beta - 1.96 * se,
    ci_upper = beta + 1.96 * se
  )

# ============================================================
# CONVERT ELASTICITIES TO MULTIPLIERS
# Use Denmark's conversion factor as reference
# ============================================================

cf_denmark <- conversion_factors %>%
  filter(country == "Denmark") %>%
  pull(cf_milex)

# Panel average conversion factor
cf_panel <- mean(conversion_factors$cf_milex)

lp_results <- lp_results %>%
  mutate(
    multiplier       = beta * cf_panel,
    multiplier_lower = ci_lower * cf_panel,
    multiplier_upper = ci_upper * cf_panel
  )

print(lp_results)

# ============================================================
# PLOT IRF
# ============================================================

ggplot(lp_results, aes(x = horizon)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = beta), color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 0:H) +
  labs(
    title = "Impulse Response: Total Military Spending → GDP",
    subtitle = "Panel LP, country and time fixed effects, clustered SE",
    x = "Horizon (years)",
    y = "Response of log GDP per capita"
  ) +
  theme_minimal(base_size = 12)

# Save plot
ggsave("IRF_total_milex.png", width = 8, height = 5, dpi = 300)


# ============================================================
# Check R² across horizons in your LP
# ============================================================

for (h in 0:H) {
  n_lags <- aic_lags$best_lags[h + 1]
  
  Data_h <- Data %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(y_fwd = lead(dlog_gdp, h)) %>%
    ungroup() %>%
    filter(!is.na(y_fwd), !is.na(dlog_milex),
           !is.na(dlog_gdp), !is.na(output_gap),
           !is.na(inflation), !is.na(unemployment),
           !is.na(dlog_gov_cons), !is.na(dlog_private_invest),
           !is.na(dlog_capital))
  
  formula_h <- as.formula(paste0(
    "y_fwd ~ dlog_milex",
    " + ", paste0("lag(dlog_gdp, ", 1:n_lags, ")", collapse = " + "),
    " + output_gap + inflation + unemployment + dlog_gov_cons",
    " + dlog_private_invest + dlog_capital",
    " | country + year"
  ))
  
  fit_h <- feols(formula_h, data = Data_h,
                 cluster = ~country, panel.id = ~country + year)
  
  cat("h =", h,
      "| Within R2 =", round(r2(fit_h, "war2"), 3),
      "| N =", nobs(fit_h), "\n")
}

#h=1: 0.225  ← highest fit, one-year-ahead most predictable
#h=2: 0.147  ← drops as uncertainty grows
#h=3: 0.081  ← continues declining
#h=4: 0.137  ← small uptick, possibly cyclical pattern
#h=5: 0.047  ← lowest, 5-year-ahead growth nearly unpredictable


# ============================================================
# DISAGGREGATED LP: separate IRF per spending category
# Categories: personnel, intermediate consumption, GFCF
# ============================================================

categories <- list(
  personnel    = "dlog_personnel",
  intermediate = "dlog_intermediate",
  gfcf         = "dlog_gfcf"
)

# Store results for all categories
disagg_results <- data.frame()

for (cat_name in names(categories)) {
  
  treatment_var <- categories[[cat_name]]
  
  # Conversion factor for this category (panel average)
  cf_col <- paste0("cf_", cat_name)
  cf_cat <- mean(conversion_factors[[cf_col]], na.rm = TRUE)
  
  for (h in 0:H) {
    
    n_lags <- aic_lags$best_lags[h + 1]
    
    Data_h <- Data %>%
      arrange(country, year) %>%
      group_by(country) %>%
      mutate(y_fwd = lead(dlog_gdp, h)) %>%
      ungroup() %>%
      filter(!is.na(y_fwd),
             !is.na(.data[[treatment_var]]),
             !is.na(dlog_gdp),
             !is.na(output_gap),
             !is.na(inflation),
             !is.na(unemployment))
    
    formula_h <- as.formula(paste0(
      "y_fwd ~ ", treatment_var,
      " + ", paste0("lag(dlog_gdp, ", 1:n_lags, ")", collapse = " + "),
      " + output_gap + inflation + unemployment + dlog_gov_cons",
      " + dlog_private_invest + dlog_capital",
      " | country + year"
    ))
    
    fit_h <- feols(formula_h, data = Data_h,
                   cluster = ~country, panel.id = ~country + year)
    
    beta_h <- coef(fit_h)[treatment_var]
    se_h   <- se(fit_h)[treatment_var]
    
    disagg_results <- rbind(disagg_results, data.frame(
      category   = cat_name,
      horizon    = h,
      beta       = beta_h,
      se         = se_h,
      ci_lower   = beta_h - 1.96 * se_h,
      ci_upper   = beta_h + 1.96 * se_h,
      multiplier = beta_h * cf_cat,
      mult_lower = (beta_h - 1.96 * se_h) * cf_cat,
      mult_upper = (beta_h + 1.96 * se_h) * cf_cat,
      cf         = cf_cat
    ))
  }
}

print(disagg_results)

# ============================================================
# PLOT: three panels side by side
# ============================================================

category_labels <- c(
  personnel    = "Personnel expenditure",
  intermediate = "Intermediate consumption",
  gfcf         = "Gross fixed capital formation"
)

disagg_results$category_label <- category_labels[disagg_results$category]

ggplot(disagg_results, aes(x = horizon)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = beta), color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ category_label, nrow = 1) +
  scale_x_continuous(breaks = 0:H) +
  labs(
    title    = "Impulse Response by Military Spending Category → GDP",
    subtitle = "Panel LP, country and year FE, clustered SE",
    x        = "Horizon (years)",
    y        = "Response of log GDP per capita"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave("IRF_disaggregated.png", width = 12, height = 5, dpi = 300)

# ============================================================
# MULTIPLIER PLOT: same structure but y-axis is multiplier
# ============================================================

ggplot(disagg_results, aes(x = horizon)) +
  geom_ribbon(aes(ymin = mult_lower, ymax = mult_upper),
              fill = "coral", alpha = 0.3) +
  geom_line(aes(y = multiplier), color = "coral", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 1, linetype = "dotted", color = "grey40") +
  facet_wrap(~ category_label, nrow = 1) +
  scale_x_continuous(breaks = 0:H) +
  labs(
    title    = "Fiscal Multiplier by Military Spending Category",
    subtitle = "Panel LP, country and year FE, clustered SE. Dotted line = multiplier of 1.",
    x        = "Horizon (years)",
    y        = "Multiplier"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave("IRF_disaggregated_multipliers.png", width = 12, height = 5, dpi = 300)

# ============================================================
# ACCUMULATED MULTIPLIER TABLE
# ============================================================

# We need raw betas first — run LP for each category
# and store all horizon-level betas, then cumsum them

categories <- list(
  total        = "dlog_milex",
  personnel    = "dlog_personnel",
  intermediate = "dlog_intermediate",
  gfcf         = "dlog_gfcf"
)

cf_lookup <- c(
  total        = cf_panel,
  personnel    = mean(conversion_factors$cf_personnel),
  intermediate = mean(conversion_factors$cf_intermediate),
  gfcf         = mean(conversion_factors$cf_gfcf)
)

# Storage for raw betas and SEs per category per horizon
raw_results <- data.frame()

for (cat_name in names(categories)) {
  
  treatment_var <- categories[[cat_name]]
  
  for (h in 1:H) {   # start at h=1, skip h=0 (mechanical)
    
    n_lags <- aic_lags$best_lags[h + 1]
    
    Data_h <- Data %>%
      arrange(country, year) %>%
      group_by(country) %>%
      mutate(y_fwd = lead(dlog_gdp, h)) %>%
      ungroup() %>%
      filter(!is.na(y_fwd),
             !is.na(.data[[treatment_var]]),
             !is.na(dlog_gdp),
             !is.na(output_gap),
             !is.na(inflation),
             !is.na(unemployment),
             !is.na(dlog_gov_cons),
             !is.na(dlog_private_invest),
             !is.na(dlog_capital))
    
    formula_h <- as.formula(paste0(
      "y_fwd ~ ", treatment_var,
      " + ", paste0("lag(dlog_gdp, ", 1:n_lags, ")", collapse = " + "),
      " + output_gap + inflation + unemployment + dlog_gov_cons",
      " + dlog_private_invest + dlog_capital",
      " | country + year"
    ))
    
    fit_h <- suppressMessages(
      feols(formula_h, data = Data_h,
            cluster = ~country, panel.id = ~country + year)
    )
    
    beta_h <- coef(fit_h)[treatment_var]
    se_h   <- se(fit_h)[treatment_var]
    
    raw_results <- rbind(raw_results, data.frame(
      category = cat_name,
      horizon  = h,
      beta     = beta_h,
      se       = se_h
    ))
  }
}

# ============================================================
# COMPUTE ACCUMULATED MULTIPLIERS
# acc_mult[h] = sum(beta[1:h]) * cf
# SE uses sqrt(sum(var[1:h])) — assuming independence across h
# This is conservative; Olejnik uses delta method
# ============================================================

accum_results <- data.frame()

for (cat_name in names(categories)) {
  
  cf_cat <- cf_lookup[cat_name]
  
  cat_data <- raw_results %>%
    filter(category == cat_name) %>%
    arrange(horizon)
  
  cum_beta <- cumsum(cat_data$beta)
  cum_se   <- sqrt(cumsum(cat_data$se^2))  # conservative
  
  for (i in seq_along(cum_beta)) {
    h       <- cat_data$horizon[i]
    acc_b   <- cum_beta[i]
    acc_se  <- cum_se[i]
    mult    <- acc_b * cf_cat
    mult_se <- acc_se * cf_cat
    
    # significance stars
    tstat <- mult / mult_se
    stars <- ifelse(abs(tstat) > 2.576, "***",
                    ifelse(abs(tstat) > 1.960, "**",
                           ifelse(abs(tstat) > 1.645, "*", "")))
    
    cell <- paste0(round(mult, 4), stars)
    
    accum_results <- rbind(accum_results, data.frame(
      category = cat_name,
      horizon  = h,
      mult     = mult,
      mult_se  = mult_se,
      tstat    = tstat,
      stars    = stars,
      cell     = cell
    ))
  }
}

# ============================================================
# RESHAPE TO WIDE TABLE (categories as rows, horizons as cols)
# ============================================================

table_wide <- accum_results %>%
  select(category, horizon, cell) %>%
  pivot_wider(names_from = horizon, values_from = cell) %>%
  mutate(category = recode(category,
                           total        = "Military expenditures (total)",
                           personnel    = "Military expenditures (personnel)",
                           intermediate = "Military expenditures (intermediate)",
                           gfcf         = "Mil. expenditures (equipment & infra.)"
  ))

print(table_wide)

# ============================================================
# GENERATE LATEX TABLE
# ============================================================

horizons <- 1:H

latex_acc <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Accumulated Fiscal Multipliers by Military Spending Category}\n",
  "\\label{tab:accum_multipliers}\n",
  "\\small\n",
  "\\begin{adjustbox}{max width=\\textwidth}\n",
  "\\begin{tabular}{l", paste(rep("r", length(horizons)), collapse = ""), "}\n",
  "\\hline\\hline\n",
  " & \\multicolumn{", length(horizons), "}{c}{Accumulated multipliers (years after shock)} \\\\\n",
  "\\cline{2-", length(horizons) + 1, "}\n",
  " & ", paste(horizons, collapse = " & "), " \\\\\n",
  "\\hline\n"
)

for (i in 1:nrow(table_wide)) {
  row_vals <- as.character(unlist(table_wide[i, 2:ncol(table_wide)]))
  latex_acc <- paste0(
    latex_acc,
    table_wide$category[i], " & ",
    paste(row_vals, collapse = " & "),
    " \\\\\n"
  )
}

latex_acc <- paste0(
  latex_acc,
  "\\hline\\hline\n",
  "\\multicolumn{", length(horizons) + 1, "}{l}{",
  "\\footnotesize Note: Accumulated multipliers = $\\sum_{j=1}^{h} \\hat{\\beta}_j \\cdot \\bar{cf}$. ",
  "Significance: *** p$<$0.01, ** p$<$0.05, * p$<$0.10.} \\\\\n",
  "\\multicolumn{", length(horizons) + 1, "}{l}{",
  "\\footnotesize Panel LP with country and year fixed effects. Clustered SEs by country.} \\\\\n",
  "\\end{tabular}\n",
  "\\end{adjustbox}\n",
  "\\end{table}\n"
)

cat(latex_acc)
writeLines(latex_acc, "accumulated_multipliers.tex")
