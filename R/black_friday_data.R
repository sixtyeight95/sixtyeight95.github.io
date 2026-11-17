# ---------------------------------------------------------------------------
# Are Black Friday deals real? What the government's price data shows.
#   - BLS CPI, not seasonally adjusted, via FRED: how much prices actually change
#     from October to November (and December), averaged over 2010-2024
#     (October 2025 CPI was never published because of the 2025 government shutdown)
#   - Price change over the past year (September 2025 to September 2026)
#   - NRF 2026 holiday forecast (Oct 15, 2026)
# ---------------------------------------------------------------------------
bf_series <- tribble(~item, ~id,
  "Clothing",   "CPIAPPNS",
  "Toys",       "CUUR0000SERE01",
  "Computers",  "CUUR0000SEEE01",
  "All goods",  "CUUR0000SACL1",
  "Everything", "CPIAUCNS")

bf_prices <- bf_series %>% mutate(stats = map(id, function(s) {
  d <- get_fred(s) %>% mutate(y = year(date), m = month(date))
  w <- d %>% filter(y >= 2010, y <= 2024, m %in% c(10, 11, 12)) %>%
    select(y, m, value) %>% pivot_wider(names_from = m, values_from = value)
  nov <- (w$`11` / w$`10` - 1) * 100
  dec <- (w$`12` / w$`10` - 1) * 100
  sep <- d %>% filter(date %in% as.Date(c("2025-09-01", "2026-09-01"))) %>% arrange(date)
  tibble(nov_change = mean(nov), dec_change = mean(dec), years_fell = sum(nov < 0), years = length(nov),
         yoy = (sep$value[2] / sep$value[1] - 1) * 100)
})) %>% unnest(stats)

nrf_2026_t      <- 1.013   # $ trillions, Nov-Dec 2026 forecast
nrf_growth_2026 <- 4.2     # %
nrf_2025_b      <- 972     # $ billions, 2025 actual
nrf_growth_2025 <- 3.1
nrf_economy_share <- 56    # % of shoppers who say the economy will affect their spending (NRF survey)
cpi_yoy <- bf_prices$yoy[bf_prices$item == "Everything"]
real_growth <- nrf_growth_2026 - cpi_yoy
black_friday <- "Friday, November 27"
