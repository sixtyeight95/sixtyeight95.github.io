# ---------------------------------------------------------------------------
# Is now a good time to buy? Current housing market snapshot. All via FRED.
#   MEDLISPRIUS  median listing price (Realtor.com)     ACTLISCOUUS active listings
#   MEDDAYONMARUS median days on market                CSUSHPINSA  Case-Shiller national index
#   MSPUS        median price of new homes sold         MORTGAGE30US 30-yr fixed rate (weekly)
#   EXHOSLUSM495S existing home sales (annual rate)     CUSR0000SEHA rent of primary residence (CPI)
# ---------------------------------------------------------------------------
snap <- function(id) {
  d <- get_fred(id) %>% arrange(date)
  last <- tail(d, 1)
  yago <- d %>% filter(date <= last$date %m-% months(12)) %>% tail(1)
  tibble(id = id, date = last$date, now = last$value, year_ago = yago$value,
         change = (last$value / yago$value - 1) * 100)
}

market <- bind_rows(lapply(c("MEDLISPRIUS", "MSPUS", "CSUSHPINSA", "MORTGAGE30US", "ACTLISCOUUS",
                             "EXHOSLUSM495S", "MEDDAYONMARUS", "CUSR0000SEHA"), snap))
m <- function(id) market %>% filter(id == !!id)

payment <- function(price, rate, down = 0.10, years = 30) {
  r <- rate / 100 / 12; n <- years * 12
  price * (1 - down) * r / (1 - (1 + r)^(-n))
}

list_price <- m("MEDLISPRIUS"); mortgage <- m("MORTGAGE30US")
pay_now  <- payment(list_price$now, mortgage$now)
pay_then <- payment(list_price$year_ago, mortgage$year_ago)

# history for charts
rates_hist  <- get_fred("MORTGAGE30US") %>% filter(date >= as.Date("2019-01-01"))
listing_hist <- get_fred("MEDLISPRIUS") %>% rename(price = value) %>%
  inner_join(get_fred("MORTGAGE30US") %>% mutate(date = floor_date(date, "month")) %>%
               group_by(date) %>% summarise(rate = mean(value), .groups = "drop"), by = "date") %>%
  mutate(payment = payment(price, rate))

# payment comparison on the same monthly basis as the chart (latest month vs. 12 months earlier)
pay_latest  <- listing_hist %>% slice_max(date, n = 1)
pay_yearago <- listing_hist %>% filter(date == pay_latest$date %m-% months(12))
pay_now  <- pay_latest$payment
pay_then <- pay_yearago$payment
