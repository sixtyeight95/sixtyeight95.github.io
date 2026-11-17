# Reel for the Thanksgiving travel post: travel-reel.mp4 (1080 x 1920, ~25 s)
# Run from this folder:  source("travel-reel.R")
source("../R/fred.R")
source("../R/travel_data.R")
source("../R/reel_helpers.R")
usd  <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
usd2 <- function(x) paste0("$", formatC(x, format = "f", digits = 2))
count_to <- function(final, t, start, len = 1.2) final * ease((t - start) / len)

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("Going home for\nThanksgiving?", 0.5, 0.6, 48, brand$white, "bold", a)
  txt(paste0("So are ", travelers_2026_m, " million\nother Americans."), 0.5, 0.45, 38, brand$amber, "bold", a * appear(t, 1.6))

  a <- fade(t, 3.5, 10.5)
  txt("500 miles each way,\nfamily of four:", 0.5, 0.75, 36, brand$white, "bold", a)
  txt("DRIVE", 0.5, 0.63, 30, brand$mint, "bold", a * appear(t, 4.4))
  txt(usd(count_to(drive_cost_now, t, 4.4)), 0.5, 0.56, 90, brand$white, "bold", a * appear(t, 4.4))
  txt("in gas", 0.5, 0.495, 26, brand$muted, "plain", a * appear(t, 4.4))
  txt("FLY", 0.5, 0.42, 30, brand$red, "bold", a * appear(t, 6.6))
  txt(usd(count_to(fly_cost_family, t, 6.6)), 0.5, 0.35, 90, brand$white, "bold", a * appear(t, 6.6))
  txt(paste0("4 tickets at ", usd(airfare_avg_rt), ", up ", airfare_change, "%"), 0.5, 0.28, 26, brand$muted, "plain", a * appear(t, 6.6))

  a <- fade(t, 10.5, 15.5)
  txt("Gas right now", 0.5, 0.68, 36, brand$white, "bold", a)
  txt(usd2(gas_now$value), 0.5, 0.58, 110, brand$amber, "bold", a)
  txt(paste0("+", usd2(gas_now$value - gas_tg_2025$value), " vs. Thanksgiving\nweek last year"), 0.5, 0.45, 34, brand$red, "bold", a * appear(t, 11.6))

  a <- fade(t, 15.5, 22)
  txt("Beat the traffic", 0.5, 0.74, 40, brand$white, "bold", a)
  rows <- inrix %>% filter(day %in% c("Tue, Nov 24", "Wed, Nov 25", "Sun, Nov 29"))
  for (i in 1:3) {
    y <- 0.64 - (i - 1) * 0.1
    aa <- a * appear(t, 16 + i * 0.6)
    txt(rows$day[i], 0.5, y, 32, brand$white, "bold", aa)
    txt(paste0("Avoid ", sub("Noon", "noon", rows$worst[i]), "  ·  Go ", tolower(rows$best[i])), 0.5, y - 0.035, 26, brand$mint, "plain", aa)
  }
  txt("Thanksgiving Day itself:\nbarely any traffic.", 0.5, 0.29, 30, brand$amber, "bold", a * appear(t, 18.5))

  end_card(t, 22)
  txt("Data: AAA & INRIX (Nov 16, 2026); EIA", 0.5, 0.05, 18, brand$muted)
}
render_reel(draw_frame, "travel-reel.mp4", seconds = 25)
