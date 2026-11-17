# ---------------------------------------------------------------------------
# What it costs to get home for Thanksgiving 2026.
#   - AAA Thanksgiving Holiday Travel Forecast, Nov 16, 2026 (travelers, airfare,
#     INRIX best/worst driving times; Thanksgiving gas record)
#   - EIA weekly U.S. regular gasoline price (FRED: GASREGW)
#   - BLS CPI airline fares (FRED: CUUR0000SETG01)
# ---------------------------------------------------------------------------
travelers_2026_m <- 81.5;  travelers_2025_m <- 81.6
car_2026_m       <- 72.9;  car_2025_m       <- 73.1
air_2026_m       <- 5.96
other_2026_m     <- 2.6     # bus, train, cruise (+4.1%)
holiday_period   <- "Tuesday, Nov 24 to Monday, Nov 30"

airfare_avg_rt   <- 800     # AAA: average domestic roundtrip booked for the holiday
airfare_change   <- 17      # % vs last year (AAA)
gas_tg_record    <- 3.58    # AAA: highest Thanksgiving Day average (2022); never above $4

gas <- get_fred("GASREGW")
gas_now      <- tail(gas, 1)
gas_tg_2025  <- gas %>% filter(date == as.Date("2025-11-24"))   # Thanksgiving week 2025
airfare_cpi  <- get_fred("CUUR0000SETG01")
airfare_yoy  <- airfare_cpi %>% filter(date %in% as.Date(c("2025-09-01", "2026-09-01"))) %>%
  summarise((last(value) / first(value) - 1) * 100) %>% pull()

# example trip: 500 miles each way, 25 mpg car, family of 4
trip_miles <- 1000; mpg <- 25; family <- 4
drive_cost_now  <- trip_miles / mpg * gas_now$value
drive_cost_2025 <- trip_miles / mpg * gas_tg_2025$value
fly_cost_family <- family * airfare_avg_rt

inrix <- tribble(~day, ~worst, ~best,
  "Tue, Nov 24", "2–8 PM", "Before 1 PM",
  "Wed, Nov 25", "Noon–7 PM", "Before noon",
  "Thu, Nov 26", "Minimal traffic", "Anytime",
  "Fri, Nov 27", "1–7 PM", "Before noon",
  "Sat, Nov 28", "Noon–9 PM", "Before 11 AM",
  "Sun, Nov 29", "Noon–8 PM", "Before 11 AM",
  "Mon, Nov 30", "2–7 PM", "Before 1 PM")
