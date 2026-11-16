# Reel for the Thanksgiving post: thanksgiving-reel.mp4 (1080 x 1920, ~25 s)
# Run from this folder:  source("thanksgiving-reel.R")
source("../R/fred.R")
source("../R/thanksgiving_data.R")
source("../R/reel_helpers.R")

usd <- function(x) paste0("$", formatC(x, format = "f", digits = 2))
hrs <- function(x) formatC(x, format = "f", digits = 1)
box <- list(x = 0.55, y = 0.5, w = 0.78, h = 0.36)

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("Thanksgiving dinner\nfor 10 people:", 0.5, 0.6, 44, brand$white, "bold", a)
  txt(paste0(usd(tg_first$cost_for_10), " in ", tg_first$year, "\n", usd(tg_latest$cost_for_10), " in ", tg_latest$year),
      0.5, 0.44, 44, brand$amber, "bold", a * appear(t, 1.5))

  a <- fade(t, 3.5, 7)
  txt("Twice the price.\nBut is it harder\nto afford?", 0.5, 0.58, 44, brand$white, "bold", a)
  txt("Let's count hours of work.", 0.5, 0.4, 32, brand$mint, "bold", a * appear(t, 4.8))

  a <- fade(t, 7, 16.5)
  if (a > 0) {
    txt("HOURS OF WORK FOR\nDINNER FOR 10", 0.5, 0.79, 30, brand$white, "bold", a)
    draw_line_chart(thanksgiving$year, thanksgiving$hours_of_work, (t - 8) / 6, a, box, 4,
                    y_breaks = 0:4, y_label = function(b) paste(b, "hrs"), x_breaks = c(1990, 2005, 2020))
    for (m in list(list(y = tg_first$year, v = tg_first$hours_of_work, dx = 0.08, dy = 0.05, at = 8.2),
                   list(y = tg_latest$year, v = tg_latest$hours_of_work, dx = -0.06, dy = -0.06, at = 14))) {
      p <- chart_point(m$y, m$v, thanksgiving$year, 4, box)
      aa <- a * appear(t, m$at)
      if (aa > 0) {
        grid.circle(p$x, p$y, r = unit(0.008, "npc"), gp = gpar(fill = brand$white, col = NA, alpha = aa))
        txt(paste0(m$y, "\n", hrs(m$v), " hrs"), p$x + m$dx, p$y + m$dy, 24, brand$white, "bold", aa)
      }
    }
  }

  a <- fade(t, 16.5, 22)
  txt("Last year's dinner took", 0.5, 0.66, 34, brand$white, "bold", a)
  txt(paste0(round(tg_latest$minutes_per_guest), " minutes"), 0.5, 0.56, 80, brand$amber, "bold", a * appear(t, 17.2))
  txt("of work per guest.\nThe least in 40 years.", 0.5, 0.44, 34, brand$white, "bold", a * appear(t, 18))

  end_card(t, 22)
  txt("Data: American Farm Bureau Federation; BLS", 0.5, 0.05, 18, brand$muted)
}
render_reel(draw_frame, "thanksgiving-reel.mp4", seconds = 25)
