# ---------------------------------------------------------------------------
# Putting the holidays on a credit card: what it really costs.
#   - Federal Reserve G.19: interest rate on credit card accounts assessed interest
#     (FRED: TERMCBCCINTNS, quarterly) and revolving consumer credit (REVOLSL)
#   - Federal funds rate (FEDFUNDS)
# Minimum payment rule is an assumption (see payoff()): interest + 1% of the balance,
# or $25, whichever is larger. Many big card issuers use a formula like this; check
# your own statement for yours.
# ---------------------------------------------------------------------------
cc_rate  <- get_fred("TERMCBCCINTNS")
cc_now   <- tail(cc_rate, 1)
cc_2019  <- cc_rate %>% filter(year(date) == 2019) %>% summarise(mean(value)) %>% pull()
revolving <- get_fred("REVOLSL")
revolving_now <- tail(revolving, 1)                   # $ millions, seasonally adjusted
fedfunds <- get_fred("FEDFUNDS")
ff_now   <- tail(fedfunds, 1)
ff_low   <- fedfunds %>% filter(date >= as.Date("2026-01-01")) %>% slice_min(value, n = 1, with_ties = FALSE)

payoff <- function(balance, apr, payment_rule) {
  months <- 0; interest <- 0
  while (balance > 0.005 && months < 1200) {
    i <- balance * apr / 100 / 12
    p <- min(balance + i, payment_rule(balance, i))
    balance <- balance + i - p; interest <- interest + i; months <- months + 1
  }
  tibble(months = months, interest = interest)
}
holiday_bill <- 1000
min_rule <- function(b, i) max(25, i + 0.01 * b)
scenarios <- bind_rows(
  payoff(holiday_bill, cc_now$value, min_rule) %>% mutate(plan = "Minimum payment only"),
  payoff(holiday_bill, cc_now$value, function(b, i) 50)  %>% mutate(plan = "$50 a month"),
  payoff(holiday_bill, cc_now$value, function(b, i) 100) %>% mutate(plan = "$100 a month"),
  payoff(holiday_bill, cc_now$value, function(b, i) 200) %>% mutate(plan = "$200 a month")
) %>% mutate(total = holiday_bill + interest)
min_case <- scenarios %>% filter(plan == "Minimum payment only")
first_min_payment <- min_rule(holiday_bill, holiday_bill * cc_now$value / 100 / 12)
interest_per_day <- holiday_bill * cc_now$value / 100 / 365
