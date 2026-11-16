# Reel for the inflation post: inflation-reel.mp4 (1080 x 1920, ~27 s)
# Run from this folder:  source("inflation-reel.R")
source("../R/fred.R")
source("../R/inflation_data.R")
source("../R/reel_helpers.R")

pct <- function(x) paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = 1), "%")
recent <- inflation %>% filter(date >= as.Date("2021-01-01"))
rx <- year(recent$date) + (month(recent$date) - 1) / 12
box <- list(x = 0.55, y = 0.5, w = 0.78, h = 0.36)

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("Is inflation back?", 0.5, 0.6, 52, brand$white, "bold", a)
  txt(paste0("Prices are up ", formatC(infl_now$all, format = "f", digits = 1), "%\nfrom a year ago."), 0.5, 0.46, 40,
      brand$red, "bold", a * appear(t, 1.5))

  a <- fade(t, 3.5, 12)
  if (a > 0) {
    txt("INFLATION SINCE 2021", 0.5, 0.79, 30, brand$white, "bold", a)
    txt("Red = all prices", 0.5, 0.755, 24, brand$red, "bold", a)
    txt("Teal = without food & energy", 0.5, 0.725, 24, brand$mint, "bold", a)
    pr <- (t - 4.3) / 6
    draw_line_chart(rx, recent$all, pr, a, box, 10, c(0, 5, 10), function(b) paste0(b, "%"), c(2022, 2024, 2026), brand$red)
    draw_line_chart(rx, recent$core, pr, a, box, 10, NULL, NULL, NULL, brand$mint, axes = FALSE)
    txt("It jumped after the\nFeb. 2026 Iran strike.", 0.5, 0.24, 30, brand$amber, "bold", a * appear(t, 10))
  }

  a <- fade(t, 12, 17.5)
  txt("WHAT'S DRIVING IT", 0.5, 0.72, 32, brand$muted, "bold", a)
  txt(paste("Gasoline", pct(infl_now$gasoline)), 0.5, 0.62, 46, brand$red, "bold", a * appear(t, 12.6))
  txt(paste("Energy", pct(infl_now$energy)), 0.5, 0.54, 40, brand$red, "bold", a * appear(t, 13.3))
  txt(paste("Everything else", pct(infl_now$core)), 0.5, 0.45, 34, brand$white, "bold", a * appear(t, 14))
  txt("It's mostly a gas shock.", 0.5, 0.35, 32, brand$amber, "bold", a * appear(t, 15))

  a <- fade(t, 17.5, 24)
  txt("THE BAD NEWS", 0.5, 0.72, 32, brand$muted, "bold", a)
  txt(paste0("Pay: ", pct(infl_now$pay), "\nPrices: ", pct(infl_now$all)), 0.5, 0.6, 44, brand$white, "bold", a * appear(t, 18.2))
  txt("Paychecks are falling\nbehind for the first\ntime since 2023.", 0.5, 0.42, 36, brand$red, "bold", a * appear(t, 19.5))
  txt("And the Fed is raising rates.", 0.5, 0.29, 30, brand$amber, "bold", a * appear(t, 21))

  end_card(t, 24)
  txt("Data: BLS and Federal Reserve via FRED", 0.5, 0.05, 18, brand$muted)
}
render_reel(draw_frame, "inflation-reel.mp4", seconds = 27)
