# ---------------------------------------------------------------------------
# Insulin: cost to make vs. what countries pay.
# Used by the insulin post, infographic and Reel.
#
# Sources (numbers typed in by hand from the reports; update them here):
#   RAND, "Comparing Insulin Prices in the United States to Other Countries:
#     Updated Results Using 2022 Data" (Mulcahy & Schwam, 2024).
#     Table A.5 = average gross price per 100 units, all insulins, 2022
#     Table A.9 = U.S. price as % of each country (price index)
#     -> saved in data/insulin_rand_2022.csv
#   Gotham, Barber & Hill, "Production costs and potential prices for
#     biosimilars of human insulin and insulin analogues", BMJ Global Health 2018.
#     Table 1 = estimated price per 10 mL (1,000 unit) vial, incl. 20% margin
#
# A standard vial = 10 mL = 1,000 units, so price per vial = price per 100 units x 10.
# ---------------------------------------------------------------------------

site_dir <- if (file.exists("_quarto.yml")) "." else if (file.exists("../_quarto.yml")) ".." else "../.."

rand <- read_csv(file.path(site_dir, "data", "insulin_rand_2022.csv"), show_col_types = FALSE) %>%
  mutate(price_per_vial = gross_price_per_100_units_usd * 10)

# RAND: U.S. net prices estimated with a 76% gross-to-net discount;
# U.S. net prices were 233% of all other OECD countries combined.
us_gross_to_net_discount <- 0.76
us_net_index_pct         <- 233
us_gross_index_pct       <- 971   # headline gross ratio vs all other OECD countries

us_list_vial  <- rand$price_per_vial[rand$country == "U.S."]
us_net_vial   <- us_list_vial * (1 - us_gross_to_net_discount)
oecd_vial     <- rand$price_per_vial[rand$country == "Non-U.S. OECD"]

# Gotham et al. 2018: estimated cost-based price per 1,000-unit vial (US$)
cost_to_make <- tribble(
  ~insulin,                 ~low,  ~high,
  "Regular human insulin",  3.29,  4.86,
  "NPH (human)",            3.35,  4.93,
  "Glargine (Lantus-type)", 5.32,  7.38,
  "Lispro (Humalog-type)",  6.51,  8.87,
  "Aspart (Novolog-type)",  6.50,  8.86
)
cost_low  <- min(cost_to_make$low)    # cheapest human insulin, low end
cost_high <- max(cost_to_make$high)   # common analogues, high end

# countries to feature in charts (G7 + a few familiar ones)
featured <- c("UK", "France", "Australia", "Italy", "Germany", "Japan", "Canada", "Switzerland")

country_chart <- bind_rows(
  tibble(label = "Cost to make (est.)", value = (cost_low + cost_high) / 2, low = cost_low, high = cost_high,
         group = "cost"),
  rand %>% filter(country %in% featured) %>% arrange(price_per_vial) %>%
    transmute(label = country, value = price_per_vial, low = NA, high = NA, group = "country"),
  tibble(label = "33-country average", value = oecd_vial, low = NA, high = NA, group = "average"),
  tibble(label = "U.S. after discounts*", value = us_net_vial, low = NA, high = NA, group = "us_net"),
  tibble(label = "U.S. list price", value = us_list_vial, low = NA, high = NA, group = "us_list")
)
