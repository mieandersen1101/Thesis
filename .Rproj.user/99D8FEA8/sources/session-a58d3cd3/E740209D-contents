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
    gdp_ppp                = `GDP (ppp)`,
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

# ============================================================
# STEP 2: DEFLATE NOMINAL MILLION-EURO VARIABLES
# Divide by (gdp_deflator / 100) to get real 2020 euros
# Variables to deflate: all Eurostat nominal million-euro cols
# ============================================================

vars_to_deflate <- c(
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
    capital_stock_pc     = (capital_stock_real / population) * 10000,
    gov_consumption_pc   = (gov_consumption_real / population) * 10000,
    tax_revenues_pc      = (tax_revenues_real / population) * 10000,
    rd_expenditure_pc    = (rd_expenditure_real / population) * 10000,
    private_invest_pc    = (private_investment_real / population) * 10000,
    milex_total_pc       = (milex_total_real / population) * 10000,
    rd_defence_pc        = (rd_defence_real / population) * 10000,
    personnel_pc         = (personnel_1_real / population) * 10000,
    intermediate_pc      = (intermediate_1_real / population) * 10000,
    gfcf_pc              = (gfcf_1_real / population) * 1000,
    foreign_aid_1_pc     = (foreign_aid_1_real / population) * 1000
  )

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

# ============================================================
# STEP 5: CREATE LOG FIRST DIFFERENCES
# For LP estimation following Olejnik (2023)
# dlog(x) = log(x_t) - log(x_t-1)
# ============================================================

Data <- Data %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    dlog_gdp             = log(gdp_ppp_pc) - lag(log(gdp_ppp_pc)),
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
    cf_milex        = mean(gdp_ppp, na.rm = TRUE) / mean(milex_total, na.rm = TRUE),
    cf_personnel    = mean(gdp_ppp, na.rm = TRUE) / mean(personnel_1, na.rm = TRUE),
    cf_intermediate = mean(gdp_ppp, na.rm = TRUE) / mean(intermediate_1, na.rm = TRUE),
    cf_gfcf         = mean(gdp_ppp, na.rm = TRUE) / mean(gfcf_1, na.rm = TRUE),
    cf_rd_defence   = mean(gdp_ppp, na.rm = TRUE) / mean(rd_defence, na.rm = TRUE)
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

