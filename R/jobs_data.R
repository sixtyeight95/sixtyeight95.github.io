# ---------------------------------------------------------------------------
# The "no hire, no fire" job market. All from FRED (BLS CPS and JOLTS).
#   UEMPMEAN  average weeks unemployed       UEMPMED  median weeks unemployed
#   LNS13025703  % of unemployed out 27+ weeks (long-term)
#   UNRATE    unemployment rate
#   JTSHIR    hires rate (% of employment per month)
#   JTSLDR    layoffs & discharges rate     JTSQUR  quits rate
# ---------------------------------------------------------------------------
fred_col <- function(id, name) get_fred(id) %>% rename("{name}" := value)

jobs <- fred_col("UEMPMEAN", "avg_weeks") %>%
  full_join(fred_col("UEMPMED", "median_weeks"), by = "date") %>%
  full_join(fred_col("LNS13025703", "long_term_share"), by = "date") %>%
  full_join(fred_col("UNRATE", "unemployment"), by = "date") %>%
  full_join(fred_col("JTSHIR", "hires"), by = "date") %>%
  full_join(fred_col("JTSLDR", "layoffs"), by = "date") %>%
  full_join(fred_col("JTSQUR", "quits"), by = "date") %>%
  arrange(date)

latest_of <- function(col) jobs %>% filter(!is.na(.data[[col]])) %>% slice_max(date, n = 1) %>% pull(all_of(col))
latest_date_of <- function(col) jobs %>% filter(!is.na(.data[[col]])) %>% pull(date) %>% max()
avg_in_year <- function(col, y) mean(jobs[[col]][year(jobs$date) == y], na.rm = TRUE)

jobs_now <- list(
  avg_weeks = latest_of("avg_weeks"), median_weeks = latest_of("median_weeks"),
  long_term_share = latest_of("long_term_share"), unemployment = latest_of("unemployment"),
  hires = latest_of("hires"), layoffs = latest_of("layoffs"), quits = latest_of("quits"),
  cps_date = latest_date_of("avg_weeks"), jolts_date = latest_date_of("hires"))

jobs_2019 <- list(
  avg_weeks = avg_in_year("avg_weeks", 2019), long_term_share = avg_in_year("long_term_share", 2019),
  unemployment = avg_in_year("unemployment", 2019), hires = avg_in_year("hires", 2019),
  layoffs = avg_in_year("layoffs", 2019), quits = avg_in_year("quits", 2019))
