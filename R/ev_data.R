# ---------------------------------------------------------------------------
# EVs vs gas: cost per year, and lifetime emissions.
# Used by both EV posts, their infographics and Reels.
#
# COST (per year, 15,000 miles, 5-year ownership, new 2026 models):
#   data/aaa_ev_gas_hybrid_2026.csv  typed in from AAA "Your Driving Costs" 2026, page 3
#   FRED (live): average U.S. electricity price per kWh (APU000072610)
#
# EMISSIONS (a deliberately simple, transparent model):
#   EPA:  burning 1 gallon of gasoline = 8,887 g CO2
#   EIA:  U.S. grid average 2024 = 0.785 lb CO2 per kWh; coal power ~2.30 lb/kWh
#   ICCT: battery manufacturing ~60 kg CO2 per kWh of battery (U.S., 2021)
#   Assumptions below are editable.
# ---------------------------------------------------------------------------

site_dir <- if (file.exists("_quarto.yml")) "." else if (file.exists("../_quarto.yml")) ".." else "../.."

# --- cost ----------------------------------------------------------------------
aaa <- read_csv(file.path(site_dir, "data", "aaa_ev_gas_hybrid_2026.csv"), show_col_types = FALSE) %>%
  mutate(running = fuel + maintenance,
         owning  = depreciation + insurance + registration_taxes + finance,
         class = factor(class, levels = c("Medium sedan", "Compact SUV", "Medium SUV", "Pickup truck")),
         powertrain = factor(powertrain, levels = c("Electric", "Gas", "Hybrid")))

cost_row <- function(cls, pt) aaa %>% filter(class == cls, powertrain == pt)

# public fast charging vs home charging
home_kwh_price   <- get_fred("APU000072610") %>% filter(date > max(date) %m-% months(12)) %>%
  summarise(p = mean(value)) %>% pull(p)
public_kwh_price <- 0.45   # typical U.S. DC fast-charging price, 2026 (industry trackers report ~$0.40-0.48)
public_multiplier <- public_kwh_price / home_kwh_price

# --- emissions -------------------------------------------------------------------
g_co2_per_gallon   <- 8887          # EPA, tailpipe only (production of gasoline NOT counted)
gas_mpg            <- 30            # an efficient new gas car
ev_kwh_per_mile    <- 0.30          # incl. charging losses (typical EPA label values 0.25-0.35)
grid_lb_per_kwh    <- 0.785         # EIA, U.S. 2024
coal_lb_per_kwh    <- 2.30          # EIA, coal-fired power
battery_kwh        <- 75
battery_kg_per_kwh <- 60            # ICCT, U.S.

lb_to_g <- 453.592

gas_g_per_mile  <- g_co2_per_gallon / gas_mpg
ev_g_per_mile   <- ev_kwh_per_mile * grid_lb_per_kwh * lb_to_g
coal_g_per_mile <- ev_kwh_per_mile * coal_lb_per_kwh * lb_to_g
battery_t       <- battery_kwh * battery_kg_per_kwh / 1000

breakeven_miles <- battery_t * 1e6 / (gas_g_per_mile - ev_g_per_mile)
lifetime_miles  <- 150000

emissions <- tibble(miles = seq(0, lifetime_miles, by = 1000)) %>%
  mutate(gas = miles * gas_g_per_mile / 1e6,
         ev  = battery_t + miles * ev_g_per_mile / 1e6)

lifetime_gas_t <- lifetime_miles * gas_g_per_mile / 1e6
lifetime_ev_t  <- battery_t + lifetime_miles * ev_g_per_mile / 1e6
lifetime_cut   <- 1 - lifetime_ev_t / lifetime_gas_t
