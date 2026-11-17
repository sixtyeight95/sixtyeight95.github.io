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

# 2025 regional averages for the same menu (AFBF)
tg_region_2025 <- c(Northeast = 82.97, West = 84.97)

# what the AFBF "classic" menu includes, and common dishes it leaves out
tg_menu_in  <- c("16-lb turkey", "14-oz box of stuffing", "3 lbs sweet potatoes", "12 rolls", "1 lb frozen peas",
                 "12 oz cranberries", "small veggie tray", "2 pumpkin pies (canned filling, store crusts)",
                 "whipped cream", "1 gallon of milk", "coffee and baking basics")
tg_menu_out <- c("ham", "mashed potatoes", "mac and cheese", "green bean casserole", "gravy beyond the basics",
                 "appetizers", "drinks and alcohol", "extra desserts", "leftovers")
