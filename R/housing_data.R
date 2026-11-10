# ---------------------------------------------------------------------------
# Buying a typical home, measured in HOURS OF WORK.
# Used by the housing post and the housing infographic.
#
#   price:  median sales price of NEWLY BUILT houses sold in the U.S. (MSPUS, quarterly)
#   rate:   30-year fixed mortgage rate (MORTGAGE30US, weekly -> quarterly avg)
#   pay:    average hourly earnings, production & nonsupervisory workers (AHETPI)
#
# Assumptions (change them here):
down_payment_share <- 0.10   # 10% down
loan_years         <- 30
work_hours_month   <- 173    # 40 hours x 52 weeks / 12 months
#
# Creates:
#   housing_quarterly  quarter, price, rate, wage, payment, hours_per_month,
#                      down_payment, months_to_save_down, years_of_pay_price
#   housing_yearly     the same, averaged by year
# ---------------------------------------------------------------------------

to_quarter <- function(data, name) {
  data %>%
    mutate(quarter = floor_date(date, "quarter")) %>%
    group_by(quarter) %>%
    summarise("{name}" := mean(value), .groups = "drop")
}

prices <- get_fred("MSPUS")        %>% to_quarter("price")
rates  <- get_fred("MORTGAGE30US") %>% to_quarter("rate")
pay    <- get_fred("AHETPI")       %>% to_quarter("wage")

# standard mortgage payment formula (principal + interest)
monthly_payment <- function(loan, annual_rate, years) {
  r <- annual_rate / 100 / 12
  n <- years * 12
  loan * r / (1 - (1 + r)^(-n))
}

housing_quarterly <- prices %>%
  inner_join(rates, by = "quarter") %>%
  inner_join(pay,   by = "quarter") %>%
  mutate(loan                = price * (1 - down_payment_share),
         payment             = monthly_payment(loan, rate, loan_years),
         hours_per_month     = payment / wage,
         down_payment        = price * down_payment_share,
         months_to_save_down = down_payment / (wage * work_hours_month),
         years_of_pay_price  = price / (wage * work_hours_month * 12))

housing_yearly <- housing_quarterly %>%
  mutate(year = year(quarter)) %>%
  group_by(year) %>%
  summarise(across(c(price, rate, wage, payment, hours_per_month,
                     down_payment, months_to_save_down, years_of_pay_price), mean),
            .groups = "drop")

# the most recent four quarters = "today"
latest_quarter <- max(housing_quarterly$quarter)
housing_now <- housing_quarterly %>%
  filter(quarter > latest_quarter %m-% months(12)) %>%
  summarise(across(c(price, rate, wage, payment, hours_per_month,
                     down_payment, months_to_save_down, years_of_pay_price), mean))

year_row <- function(y) housing_yearly %>% filter(year == y)
peak     <- housing_quarterly %>% slice_max(hours_per_month, n = 1)
low      <- housing_quarterly %>% filter(year(quarter) >= 2000) %>% slice_min(hours_per_month, n = 1)
