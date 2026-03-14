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

