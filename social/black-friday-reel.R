# Reel for the Black Friday post: black-friday-reel.mp4 (1080 x 1920, ~25 s)
# Run from this folder:  source("black-friday-reel.R")
source("../R/fred.R")
source("../R/black_friday_data.R")
source("../R/reel_helpers.R")
pct <- function(x, d = 1) paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = d), "%")
items <- bf_prices %>% filter(item %in% c("Clothing", "Toys", "Computers"))

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("\"50% OFF!\"", 0.5, 0.62, 80, brand$red, "bold", a)
  txt("Are Black Friday\ndeals real?", 0.5, 0.47, 44, brand$white, "bold", a * appear(t, 1.3))

  a <- fade(t, 3.5, 11)
  txt("What people actually pay,\nOctober → November", 0.5, 0.74, 34, brand$white, "bold", a)
  txt("(average, 2010–2024)", 0.5, 0.685, 24, brand$muted, "plain", a)
  for (i in 1:3) {
    y <- 0.58 - (i - 1) * 0.11
    aa <- a * appear(t, 4.2 + i * 0.8)
    txt(items$item[i], 0.5, y, 34, brand$white, "bold", aa)
    txt(pct(items$nov_change[i]), 0.5, y - 0.05, 60, brand$mint, "bold", aa)
  }
  txt("Real... but small.", 0.5, 0.2, 40, brand$amber, "bold", a * appear(t, 8.2))

  a <- fade(t, 11, 17)
  txt("The catch:", 0.5, 0.74, 40, brand$amber, "bold", a)
  txt("prices are up from\na year ago", 0.5, 0.66, 34, brand$white, "bold", a)
  for (i in 1:3) {
    y <- 0.54 - (i - 1) * 0.1
    aa <- a * appear(t, 11.6 + i * 0.7)
    txt(paste(items$item[i], pct(items$yoy[i])), 0.5, y, 48, brand$red, "bold", aa)
  }
  txt("A 1–2% dip doesn't\nundo that.", 0.5, 0.22, 36, brand$white, "bold", a * appear(t, 14.4))

  a <- fade(t, 17, 22)
  txt("Holiday sales: +4.2%", 0.5, 0.64, 46, brand$white, "bold", a)
  txt(paste0("Prices: +", formatC(cpi_yoy, format = "f", digits = 1), "%"), 0.5, 0.56, 46, brand$red, "bold", a * appear(t, 17.8))
  txt("We're paying more\nfor about the same stuff.", 0.5, 0.44, 34, brand$mint, "bold", a * appear(t, 19))

  end_card(t, 22)
  txt("Data: BLS CPI; National Retail Federation", 0.5, 0.05, 18, brand$muted)
}
render_reel(draw_frame, "black-friday-reel.mp4", seconds = 25)
