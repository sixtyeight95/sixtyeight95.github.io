# ---------------------------------------------------------------------------
# Electricity prices and the typical monthly bill.
#   FRED: average price of electricity per kWh, U.S. city average (APU000072610)
#         CPI electricity (CUSR0000SEHF01), average hourly earnings (AHETPI)
#   EIA:  average U.S. home used about 899 kWh a month (10,791 kWh in 2022)
#   Berkeley Lab (Dec 2024, for DOE): data centers used 4.4% of U.S. electricity in
#         2023; projected 6.7%-12% by 2028
# ---------------------------------------------------------------------------
home_kwh_month <- 899
dc_share_2023 <- 4.4; dc_share_2028_low <- 6.7; dc_share_2028_high <- 12

elec <- get_fred("APU000072610") %>% rename(price = value) %>%
  inner_join(get_fred("AHETPI") %>% rename(wage = value), by = "date") %>%
  arrange(date) %>%
  mutate(bill = price * home_kwh_month, hours_for_bill = bill / wage)

elec_cpi <- get_fred("CUSR0000SEHF01") %>% arrange(date) %>%
  mutate(yoy = (value / lag(value, 12) - 1) * 100)

elec_yearly <- elec %>% mutate(year = year(date)) %>% group_by(year) %>%
  summarise(price = mean(price), bill = mean(bill), hours_for_bill = mean(hours_for_bill), .groups = "drop")

elec_now   <- elec %>% filter(date > max(date) %m-% months(12)) %>%
  summarise(price = mean(price), bill = mean(bill), hours_for_bill = mean(hours_for_bill))
elec_latest <- elec %>% slice_max(date, n = 1)
elec_2020  <- elec %>% filter(date == as.Date("2020-01-01"))
elec_yoy   <- elec_cpi %>% slice_max(date, n = 1)
since_2020 <- (elec_latest$price / elec_2020$price - 1) * 100
year_row_e <- function(y) elec_yearly %>% filter(year == y)
