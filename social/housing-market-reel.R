# Reel for the "good time to buy" post: housing-market-reel.mp4 (1080 x 1920, ~25 s)
# Run from this folder:  source("housing-market-reel.R")
source("../R/fred.R")
source("../R/housing_market_data.R")
source("../R/reel_helpers.R")

usd <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
f2 <- function(x) formatC(x, format = "f", digits = 2)

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("Home prices are dipping.", 0.5, 0.62, 42, brand$white, "bold", a)
  txt("Is now a good\ntime to buy?", 0.5, 0.48, 44, brand$amber, "bold", a * appear(t, 1.5))

  a <- fade(t, 3.5, 9)
  txt("THE TYPICAL LISTED HOME", 0.5, 0.72, 30, brand$muted, "bold", a)
  txt(paste0("Price: ", usd(pay_yearago$price), " → ", usd(pay_latest$price)), 0.5, 0.6, 33, brand$mint, "bold", a * appear(t, 4.2))
  txt(paste0("Rate: ", f2(pay_yearago$rate), "% → ", f2(pay_latest$rate), "%"), 0.5, 0.5, 38, brand$red, "bold", a * appear(t, 5.2))
  txt("(a year ago → now)", 0.5, 0.43, 24, brand$muted, "plain", a * appear(t, 5.2))

  a <- fade(t, 9, 15)
  txt("So the monthly payment...", 0.5, 0.68, 34, brand$white, "bold", a)
  txt(paste0("+", usd(pay_now - pay_then)), 0.5, 0.56, 100, brand$red, "bold", a * appear(t, 10))
  txt(paste0("a month (", usd(pay_now), " now)"), 0.5, 0.45, 32, brand$white, "bold", a * appear(t, 10.6))
  txt("Rates rose more than\nprices fell.", 0.5, 0.34, 30, brand$amber, "bold", a * appear(t, 12))

  a <- fade(t, 15, 22)
  txt("THE GOOD NEWS FOR BUYERS", 0.5, 0.72, 30, brand$mint, "bold", a)
  txt(paste0("Homes for sale: +", round(m("ACTLISCOUUS")$change), "%"), 0.5, 0.61, 34, brand$white, "bold", a * appear(t, 15.6))
  txt(paste0("Sales: ", round(m("EXHOSLUSM495S")$change), "%"), 0.5, 0.53, 34, brand$white, "bold", a * appear(t, 16.4))
  txt("More room to negotiate\nthan in years.", 0.5, 0.41, 34, brand$amber, "bold", a * appear(t, 17.4))

  end_card(t, 22)
  txt("Data: Realtor.com, Freddie Mac, NAR via FRED. Not financial advice.", 0.5, 0.05, 16, brand$muted)
}
render_reel(draw_frame, "housing-market-reel.mp4", seconds = 25)
