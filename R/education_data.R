# ---------------------------------------------------------------------------
# College costs vs. pay vs. everything else.
# Used by the education post and the education infographic.
#
#   tuition: CPI "College tuition and fees" (BLS series CUUR0000SEEB01), an index
#   prices:  CPI all items (CPIAUCSL, via FRED)
#   pay:     average hourly earnings, production & nonsupervisory workers (AHETPI)
#
# The tuition index comes from the BLS API, which allows only a few requests a
# day without a key, so it is saved to data/college_tuition_index.csv and only
# downloaded again when that file is more than 30 days old.
#
# Creates:
#   college_monthly   date, tuition, cpi, wage, hours_index (1980 = 100)
#   college_yearly    the same, averaged by year
#   growth_since()    how many times bigger each one is today vs a given year
# ---------------------------------------------------------------------------
library(jsonlite)

# find the project folder (works from /posts/<post> or /social)
site_dir  <- if (file.exists("_quarto.yml")) "." else if (file.exists("../_quarto.yml")) ".." else "../.."
cache_csv <- file.path(site_dir, "data", "college_tuition_index.csv")

download_bls <- function(series_id, first_year, last_year) {
  starts <- seq(first_year, last_year, by = 10)          # BLS allows 10 years per request
  map_dfr(starts, function(start) {
    url <- paste0("https://api.bls.gov/publicAPI/v2/timeseries/data/", series_id,
                  "?startyear=", start, "&endyear=", min(start + 9, last_year))
    fromJSON(url)$Results$series$data[[1]]
  }) %>%
    filter(period != "M13") %>%                            # M13 = annual average
    mutate(date  = as.Date(paste0(year, "-", substr(period, 2, 3), "-01")),
           value = suppressWarnings(as.numeric(value))) %>%  # "-" = not collected
    filter(!is.na(value)) %>%
    select(date, value) %>%
    arrange(date)
}

cache_is_fresh <- file.exists(cache_csv) &&
  difftime(Sys.time(), file.info(cache_csv)$mtime, units = "days") < 30

if (!cache_is_fresh) {
  dir.create(dirname(cache_csv), showWarnings = FALSE)
  download_bls("CUUR0000SEEB01", 1978, year(Sys.Date())) %>% write_csv(cache_csv)
}

tuition <- read_csv(cache_csv, show_col_types = FALSE) %>% rename(tuition = value)
cpi     <- get_fred("CPIAUCSL") %>% rename(cpi = value)
pay     <- get_fred("AHETPI")   %>% rename(wage = value)

college_monthly <- tuition %>%
  inner_join(cpi, by = "date") %>%
  inner_join(pay, by = "date")

# hours of work for a year of college, scaled so that 1980 = 100
base_1980 <- college_monthly %>% filter(year(date) == 1980) %>%
  summarise(ratio = mean(tuition / wage)) %>% pull(ratio)

college_monthly <- college_monthly %>%
  mutate(hours_index = tuition / wage / base_1980 * 100)

college_yearly <- college_monthly %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(across(c(tuition, cpi, wage, hours_index), mean), .groups = "drop")

latest_month <- max(college_monthly$date)
college_now <- college_monthly %>%
  filter(date > latest_month %m-% months(12)) %>%
  summarise(across(c(tuition, cpi, wage, hours_index), mean))

growth_since <- function(y) {
  then <- college_yearly %>% filter(year == y)
  tibble(tuition = college_now$tuition / then$tuition,
         prices  = college_now$cpi     / then$cpi,
         wages   = college_now$wage    / then$wage)
}

hours_peak <- college_yearly %>% slice_max(hours_index, n = 1)
