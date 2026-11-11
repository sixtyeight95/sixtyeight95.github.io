# ---------------------------------------------------------------------------
# Hospital bills: prices over time, by country, and why.
# Used by the hospital post, infographic and Reel.
#
# Data:
#   data/hospital_procedure_prices_2022.csv
#       median price paid by private insurers for each inpatient procedure, 2022,
#       typed in from the iFHP/HCCI "International Healthcare Cost Comparison Report 2024"
#   FRED (live): CPI hospital services (CUUR0000SEMD), CPI all items (CPIAUCSL),
#                average hourly earnings, production & nonsupervisory (AHETPI)
#
# Key facts from published research (update here if newer studies come out):
#   RAND Round 5.1 (2024): employer plans paid 254% of Medicare for hospital care, 2022
#   KFF (2024 data): 47% of metro areas' hospital markets controlled by 1 or 2 systems
#   Himmelstein et al., Health Affairs (2014): admin = 25.3% of U.S. hospital budgets (2011)
#   Bai & Anderson, Health Affairs (2016): hospitals list $4.32 per $1 of cost (2013)
#   KFF / Peterson-KFF: ~20 million adults owe medical debt, >= $220 billion (2021)
# ---------------------------------------------------------------------------

site_dir <- if (file.exists("_quarto.yml")) "." else if (file.exists("../_quarto.yml")) ".." else "../.."

procedures <- read_csv(file.path(site_dir, "data", "hospital_procedure_prices_2022.csv"),
                       show_col_types = FALSE)

# U.S. price vs each other country, per procedure
procedure_ratios <- procedures %>%
  group_by(procedure) %>%
  mutate(us_price = price_usd[country == "USA"]) %>%
  ungroup() %>%
  filter(country != "USA") %>%
  mutate(us_times = us_price / price_usd)

facts <- list(
  private_vs_medicare_pct = 254,
  metros_one_or_two_pct   = 47,
  admin_share_pct         = 25,
  list_per_dollar_cost    = 4.32,
  debt_people_millions    = 20,
  debt_total_billions     = 220
)

# --- hospital prices vs pay over time, in hours of work (1980 = 100) ---------
hospital_cpi <- get_fred("CUUR0000SEMD") %>% rename(hospital = value)
all_cpi      <- get_fred("CPIAUCSL")     %>% rename(cpi = value)
hourly_pay   <- get_fred("AHETPI")       %>% rename(wage = value)

hospital_monthly <- hospital_cpi %>%
  inner_join(all_cpi, by = "date") %>%
  inner_join(hourly_pay, by = "date")

base <- hospital_monthly %>% filter(year(date) == 1980) %>%
  summarise(across(c(hospital, cpi, wage), mean))

hospital_yearly <- hospital_monthly %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(across(c(hospital, cpi, wage), mean), .groups = "drop") %>%
  mutate(hours_index = (hospital / wage) / (base$hospital / base$wage) * 100)

hospital_latest <- max(hospital_monthly$date)
hospital_now <- hospital_monthly %>%
  filter(date > hospital_latest %m-% months(12)) %>%
  summarise(across(c(hospital, cpi, wage), mean)) %>%
  mutate(hours_index = (hospital / wage) / (base$hospital / base$wage) * 100)

growth_1980 <- tibble(hospital = hospital_now$hospital / base$hospital,
                      wages    = hospital_now$wage     / base$wage,
                      prices   = hospital_now$cpi      / base$cpi)
