# ---------------------------------------------------------------------------
# Inflation: headline vs core, what's driving it, and real pay. All from FRED (BLS / Fed).
# ---------------------------------------------------------------------------
yoy <- function(id, name) {
  get_fred(id) %>% arrange(date) %>%
    mutate("{name}" := (value / lag(value, 12) - 1) * 100) %>%
    select(date, all_of(name))
}

inflation <- yoy("CPIAUCSL", "all") %>%
  inner_join(yoy("CPILFESL", "core"), by = "date") %>%
  inner_join(yoy("CPIENGSL", "energy"), by = "date") %>%
  inner_join(yoy("CPIUFDSL", "food"), by = "date") %>%
  inner_join(yoy("CUSR0000SAH1", "shelter"), by = "date") %>%
  inner_join(yoy("CUSR0000SETB01", "gasoline"), by = "date") %>%
  inner_join(yoy("PPIFIS", "wholesale"), by = "date") %>%
  inner_join(yoy("AHETPI", "pay"), by = "date") %>%
  filter(!is.na(all))

real_pay <- get_fred("AHETPI") %>% rename(wage = value) %>%
  inner_join(get_fred("CPIAUCSL") %>% rename(cpi = value), by = "date") %>%
  arrange(date) %>%
  mutate(real_pay_change = ((wage / cpi) / lag(wage / cpi, 12) - 1) * 100)

fed_funds <- get_fred("FEDFUNDS") %>% rename(rate = value)

infl_now  <- inflation %>% slice_max(date, n = 1)
infl_low  <- inflation %>% filter(date >= as.Date("2025-06-01"), date <= as.Date("2026-03-01")) %>% slice_min(all, n = 1)
real_now  <- real_pay %>% slice_max(date, n = 1)
last_real_drop <- real_pay %>% filter(real_pay_change < 0, date < real_now$date %m-% months(12)) %>% slice_max(date, n = 1)
fed_now   <- fed_funds %>% slice_max(date, n = 1)
fed_low   <- fed_funds %>% filter(date >= as.Date("2026-01-01")) %>% slice_min(rate, n = 1, with_ties = FALSE)
iran_strike <- as.Date("2026-02-28")
month_label <- function(d) format(d, "%B %Y")
