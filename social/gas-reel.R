# ---------------------------------------------------------------------------
# Reel for the gas prices & politics post:  gas-reel.mp4  (1080 x 1920, ~30 s)
# Run from this folder:  source("gas-reel.R")
# Data comes from the gas_forecasting project (weekly EIA prices + Brent crude).
# ---------------------------------------------------------------------------
source("../R/fred.R")
gas_dir <- "../../gas_forecasting"
source(file.path(gas_dir, "R", "gas_data_prep.R"))
source("../R/reel_helpers.R")

# --- numbers (same method as the article) --------------------------------------
cpi <- get_fred("CPIAUCSL") %>% rename(month = date, cpi = value)
latest_cpi <- cpi$cpi[cpi$month == max(cpi$month)]

gas_weekly <- gas_weekly %>%
  mutate(month = floor_date(date, "month")) %>%
  left_join(cpi, by = "month") %>%
  fill(cpi) %>%
  mutate(real_price = price * latest_cpi / cpi)

con <- dbConnect(RSQLite::SQLite(), file.path(gas_dir, "database", "gas_data.db"))
crude <- dbGetQuery(con, "select period, price from crude_prices")
dbDisconnect(con)

crude_monthly <- crude %>%
  mutate(month = floor_date(as.Date(period), "month")) %>%
  group_by(month) %>%
  summarise(brent = mean(price), .groups = "drop")

gas_vs_oil <- gas_monthly %>% inner_join(crude_monthly, by = "month")
oil_fit    <- lm(price ~ brent, data = gas_vs_oil)
oil_r2     <- summary(oil_fit)$r.squared
gas_vs_oil <- gas_vs_oil %>% mutate(oil_says = predict(oil_fit))

by_president <- gas_weekly %>%
  mutate(term_id = consecutive_id(president)) %>%
  group_by(term_id, president, pres_party) %>%
  summarise(avg = mean(real_price), from = year(min(date)), .groups = "drop") %>%
  mutate(name = paste0(word(president, -1), "\n", from))

# --- chart setup --------------------------------------------------------------------
chart_x <- year(gas_vs_oil$month) + (month(gas_vs_oil$month) - 1) / 12
box     <- list(x = 0.55, y = 0.5, w = 0.78, h = 0.36)
y_max   <- 6
party_color <- c(D = "#5AA2F0", R = "#F06A6A")

draw_frame <- function(t) {
  background()

  # Scene 1: hook (0 - 3.5s)
  a <- fade(t, 0, 3.5)
  txt("Does the president\nset gas prices?", 0.5, 0.6, 46, brand$white, "bold", a)
  txt("33 years of data say:\nnot directly.", 0.5, 0.43, 38, brand$amber, "bold", a * appear(t, 1.7))

  # Scene 2: gas follows oil (3.5 - 13.5s)
  a <- fade(t, 3.5, 13.5)
  if (a > 0) {
    txt("GAS FOLLOWS CRUDE OIL", 0.5, 0.8, 32, brand$white, "bold", a)
    txt("White = actual gas price", 0.5, 0.755, 24, brand$white, "bold", a)
    txt("Gold = what oil prices alone predict", 0.5, 0.725, 24, brand$amber, "bold", a)
    progress <- (t - 4.5) / 6
    draw_line_chart(chart_x, gas_vs_oil$oil_says, progress, a, box, y_max,
                    y_breaks = c(0, 2, 4, 6), y_label = function(b) paste0("$", b),
                    x_breaks = c(2000, 2010, 2020), color = brand$amber, lwd = 8)
    draw_line_chart(chart_x, gas_vs_oil$price, progress, a, box, y_max,
                    y_breaks = NULL, y_label = NULL, x_breaks = NULL,
                    color = brand$white, axes = FALSE, lwd = 4)
    txt("Price per gallon at the time", 0.55, 0.28, 22, brand$muted, "plain", a)
  }

  # Scene 3: the stat (13.5 - 17.5s)
  a <- fade(t, 13.5, 17.5)
  txt("Oil prices alone explain", 0.5, 0.64, 36, brand$white, "bold", a)
  txt(paste0(round(oil_r2 * 100), "%"), 0.5, 0.53, 110, brand$amber, "bold", a * appear(t, 14.2))
  txt("of gas price swings,\nno matter who's in office.", 0.5, 0.41, 34, brand$white, "bold", a * appear(t, 14.8))

  # Scene 4: but can presidents move oil? (17.5 - 22.5s)
  a <- fade(t, 17.5, 22.5)
  txt("But can a president\nmove oil prices?", 0.5, 0.68, 40, brand$amber, "bold", a)
  txt("Sometimes.", 0.5, 0.57, 40, brand$white, "bold", a * appear(t, 18.4))
  txt("Wars, sanctions and the\nemergency oil reserve can.", 0.5, 0.49, 30, brand$white, "plain", a * appear(t, 19))
  txt("But the biggest swings, like 2008\nand COVID, were out of anyone's hands.", 0.5, 0.38, 25, brand$mint,
      "bold", a * appear(t, 20.2))

  # Scene 5: bars per presidency (22.5 - 28s)
  a <- fade(t, 22.5, 28)
  if (a > 0) {
    txt("WHAT A GALLON COST UNDER\nEACH PRESIDENT", 0.5, 0.8, 30, brand$white, "bold", a)
    txt("(average, in today's dollars)", 0.5, 0.745, 22, brand$muted, "plain", a)
    n <- nrow(by_president)
    bar_w <- 0.1
    for (i in seq_len(n)) {
      r   <- by_president[i, ]
      x   <- 0.14 + (i - 1) * 0.145
      p   <- appear(t, 23 + (i - 1) * 0.35, 0.6)
      h   <- r$avg / 5 * 0.38 * p
      grid.rect(x, 0.3, width = bar_w, height = h, just = c("center", "bottom"),
                gp = gpar(fill = party_color[[r$pres_party]], col = NA, alpha = a))
      txt(dollars(r$avg), x, 0.3 + h + 0.018, 22, brand$white, "bold", a * p)
      txt(r$name, x, 0.265, 19, brand$white, "plain", a)
    }
    txt("The cheapest AND the priciest\nwere both Democrats.\nNo pattern by party.", 0.5, 0.165, 26, brand$amber, "bold",
        a * appear(t, 25.5))
  }

  # Scene 6: end card
  end_card(t, 28)

  txt("Data: U.S. EIA, BLS via FRED", 0.5, 0.05, 18, brand$muted)
}

render_reel(draw_frame, "gas-reel.mp4", seconds = 31)
