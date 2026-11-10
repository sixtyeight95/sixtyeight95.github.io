# ---------------------------------------------------------------------------
# Grocery prices measured in MINUTES OF WORK.
# Used by the groceries post and the groceries infographic.
#
# minutes of work = price / average hourly pay * 60
#   prices: BLS average retail prices, U.S. city average (via FRED)
#   pay:    average hourly earnings, production & nonsupervisory workers (AHETPI)
#
# Creates:
#   grocery_monthly   item, date, price, wage, minutes
#   grocery_summary   one row per item: 1985 / 2019 / last-12-months averages
# ---------------------------------------------------------------------------

# To add or drop an item, edit this table. The id is the FRED series id.
grocery_items <- tribble(
  ~id,              ~item,
  "APU0000708111",  "Eggs (dozen)",
  "APU0000703112",  "Ground beef (lb)",
  "APU0000709112",  "Milk (gallon)",
  "APU0000702111",  "White bread (lb)",
  "APU0000717311",  "Coffee (lb)",
  "APU0000706111",  "Whole chicken (lb)",
  "APU0000704111",  "Bacon (lb)",
  "APU0000710212",  "Cheddar cheese (lb)",
  "APU0000711211",  "Bananas (lb)",
  "APU0000712112",  "Potatoes (lb)",
  "APU0000715211",  "Sugar (lb)",
  "APU0000701312",  "Rice (lb)",
  "APU0000712311",  "Tomatoes (lb)",
  "APU0000711311",  "Oranges (lb)",
  "APU0000710411",  "Ice cream (1/2 gal)"
)

wages <- get_fred("AHETPI") %>% rename(wage = value)

grocery_monthly <- grocery_items %>%
  mutate(data = map(id, get_fred)) %>%
  unnest(data) %>%
  rename(price = value) %>%
  inner_join(wages, by = "date") %>%
  mutate(minutes = price / wage * 60)

latest_month <- max(grocery_monthly$date)
last_12      <- latest_month %m-% months(11)   # first month of the last 12

avg_in <- function(x, keep) mean(x[keep])

grocery_summary <- grocery_monthly %>%
  group_by(item) %>%
  summarise(
    first_year   = year(min(date)),
    price_1985   = avg_in(price,   year(date) == 1985),
    minutes_1985 = avg_in(minutes, year(date) == 1985),
    price_2019   = avg_in(price,   year(date) == 2019),
    minutes_2019 = avg_in(minutes, year(date) == 2019),
    price_now    = avg_in(price,   date >= last_12),
    minutes_now  = avg_in(minutes, date >= last_12),
    peak_minutes = max(minutes),
    peak_date    = date[which.max(minutes)],
    .groups = "drop"
  ) %>%
  mutate(price_change_2019   = price_now / price_2019 - 1,
         minutes_change_2019 = minutes_now / minutes_2019 - 1,
         minutes_change_1985 = minutes_now / minutes_1985 - 1)

wage_2019 <- mean(wages$wage[year(wages$date) == 2019])
wage_now  <- mean(wages$wage[wages$date >= last_12 & wages$date <= latest_month])
wage_1985 <- mean(wages$wage[year(wages$date) == 1985])
