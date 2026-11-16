# ---------------------------------------------------------------------------
# Thanksgiving dinner for 10, in hours of work.
#   data/afbf_thanksgiving.csv : American Farm Bureau Federation annual survey,
#     classic dinner for 10 (turkey, stuffing, sweet potatoes, rolls, peas,
#     cranberries, veggie tray, pumpkin pie with whipped cream, milk, coffee).
#     1987-2022 compiled by 24/7 Wall St. (a few early years estimated with CPI);
#     1986 and 2023-2025 from AFBF. Add each new year as AFBF releases it.
#   FRED: average hourly earnings, production & nonsupervisory (AHETPI), November.
# ---------------------------------------------------------------------------
site_dir <- if (file.exists("_quarto.yml")) "." else if (file.exists("../_quarto.yml")) ".." else "../.."

november_pay <- get_fred("AHETPI") %>%
  filter(month(date) == 11) %>%
  transmute(year = year(date), wage = value)

thanksgiving <- read_csv(file.path(site_dir, "data", "afbf_thanksgiving.csv"), show_col_types = FALSE) %>%
  left_join(november_pay, by = "year") %>%
  mutate(hours_of_work     = cost_for_10 / wage,
         minutes_per_guest = hours_of_work * 60 / 10)

tg_latest <- thanksgiving %>% filter(year == max(year))
tg_first  <- thanksgiving %>% filter(year == min(year))
tg_peak   <- thanksgiving %>% filter(year >= 2010) %>% slice_max(cost_for_10, n = 1)
tg_turkey_2025 <- 21.50   # AFBF 2025: 16-pound turkey
