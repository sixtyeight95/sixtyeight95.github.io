# Reel for the electric bills post: electricity-reel.mp4 (1080 x 1920, ~26 s)
# Run from this folder:  source("electricity-reel.R")
source("../R/fred.R")
source("../R/electricity_data.R")
source("../R/reel_helpers.R")

usd <- function(x) paste0("$", round(x))
d <- elec %>% filter(date >= as.Date("2000-01-01"))
dx <- year(d$date) + (month(d$date) - 1) / 12
box <- list(x = 0.55, y = 0.5, w = 0.78, h = 0.36)

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("Why does my electric\nbill keep going up?", 0.5, 0.58, 44, brand$white, "bold", a)

  a <- fade(t, 3.5, 11.5)
  if (a > 0) {
    txt("TYPICAL MONTHLY BILL", 0.5, 0.79, 32, brand$white, "bold", a)
    draw_line_chart(dx, d$bill, (t - 4.3) / 5, a, box, 200, c(0, 100, 200), function(b) paste0("$", b), c(2000, 2010, 2020))
    for (m in list(list(dt = elec_2020, at = 7.6, dxo = -0.08), list(dt = elec_latest, at = 9.4, dxo = -0.16))) {
      p <- chart_point(year(m$dt$date) + (month(m$dt$date) - 1) / 12, m$dt$bill, dx, 200, box)
      aa <- a * appear(t, m$at)
      if (aa > 0) {
        grid.circle(p$x, p$y, r = unit(0.009, "npc"), gp = gpar(fill = brand$white, col = NA, alpha = aa))
        txt(paste0(format(m$dt$date, "%Y"), ": ", usd(m$dt$bill)), p$x + m$dxo, p$y + 0.05, 26, brand$white, "bold", aa)
      }
    }
  }

  a <- fade(t, 11.5, 18)
  txt(paste0("+", round(since_2020), "%"), 0.5, 0.62, 110, brand$amber, "bold", a)
  txt("since 2020", 0.5, 0.52, 36, brand$white, "bold", a)
  txt("Why?", 0.5, 0.41, 40, brand$mint, "bold", a * appear(t, 13.5))

  a <- fade(t, 18, 23.5)
  txt("1. AI data centers", 0.5, 0.66, 36, brand$white, "bold", a * appear(t, 18.4))
  txt(paste0(dc_share_2023, "% of U.S. power in 2023,\nup to ", dc_share_2028_high, "% by 2028"), 0.5, 0.59, 26, brand$muted, "plain", a * appear(t, 18.8))
  txt("2. Grid upgrades", 0.5, 0.49, 36, brand$white, "bold", a * appear(t, 19.8))
  txt("3. Natural gas prices", 0.5, 0.41, 36, brand$white, "bold", a * appear(t, 20.8))

  end_card(t, 23.5)
  txt("Data: BLS, EIA, Berkeley Lab/DOE", 0.5, 0.05, 18, brand$muted)
}
render_reel(draw_frame, "electricity-reel.mp4", seconds = 26)
