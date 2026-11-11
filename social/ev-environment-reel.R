# ---------------------------------------------------------------------------
# Reel for the EV environment post:  ev-environment-reel.mp4  (1080 x 1920, ~30 s)
# Run from this folder:  source("ev-environment-reel.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/ev_data.R")
source("../R/reel_helpers.R")

comma <- function(x) formatC(round(x), format = "d", big.mark = ",")
box   <- list(x = 0.55, y = 0.5, w = 0.78, h = 0.36)
y_max <- 50
mi_k  <- emissions$miles / 1000

draw_frame <- function(t) {
  background()

  # Scene 1: hook (0 - 4s)
  a <- fade(t, 0, 4)
  txt("\"EV batteries are so dirty\nto make, EVs aren't\nreally greener.\"", 0.5, 0.6, 40, brand$white, "bold", a)
  txt("True or false?", 0.5, 0.42, 40, brand$amber, "bold", a * appear(t, 2))

  # Scene 2: the battery debt (4 - 8.5s)
  a <- fade(t, 4, 8.5)
  txt("Building an EV battery creates", 0.5, 0.64, 32, brand$white, "bold", a)
  txt(paste(battery_t, "tons"), 0.5, 0.54, 100, brand$red, "bold", a * appear(t, 4.6))
  txt("of extra CO2 before\nit drives a single mile.", 0.5, 0.42, 34, brand$white, "bold", a * appear(t, 5.3))

  # Scene 3: crossover chart draws (8.5 - 19s)
  a <- fade(t, 8.5, 19)
  if (a > 0) {
    txt("TOTAL CO2 AS YOU DRIVE", 0.5, 0.8, 30, brand$white, "bold", a)
    txt("Gas car", 0.42, 0.755, 26, brand$red, "bold", a, just = "right")
    txt("Electric car", 0.58, 0.755, 26, brand$mint, "bold", a, just = "left")
    progress <- (t - 9.5) / 6.5
    draw_line_chart(mi_k, emissions$gas, progress, a, box, y_max,
                    y_breaks = c(0, 25, 50), y_label = function(b) paste(b, "t"),
                    x_breaks = c(0, 50, 100, 150), color = brand$red)
    draw_line_chart(mi_k, emissions$ev, progress, a, box, y_max, NULL, NULL, NULL,
                    color = brand$mint, axes = FALSE)
    txt("thousand miles driven", 0.55, 0.275, 20, brand$muted, "plain", a)
    cross <- chart_point(breakeven_miles / 1000, battery_t + breakeven_miles * ev_g_per_mile / 1e6, mi_k, y_max, box)
    aa <- a * appear(t, 10.8)
    if (aa > 0) {
      grid.circle(cross$x, cross$y, r = unit(0.009, "npc"), gp = gpar(fill = brand$white, col = NA, alpha = aa))
      grid.lines(x = c(cross$x, cross$x + 0.05), y = c(cross$y + 0.01, cross$y + 0.16),
                 gp = gpar(col = brand$white, lwd = 2, alpha = aa))
      txt(paste0("EV pulls ahead\nat ~", comma(round(breakeven_miles, -3)), " miles"), cross$x + 0.06, cross$y + 0.2,
          26, brand$white, "bold", aa)
    }
  }

  # Scene 4: verdict on carbon (19 - 23s)
  a <- fade(t, 19, 23)
  txt("ON CARBON: FALSE.", 0.5, 0.68, 40, brand$mint, "bold", a)
  txt("The EV pays back its battery\nin under 2 years of driving.", 0.5, 0.57, 32, brand$white, "bold", a * appear(t, 19.7))
  txt("Lifetime: about half to\ntwo-thirds less CO2.", 0.5, 0.44, 32, brand$white, "bold", a * appear(t, 20.7))

  # Scene 5: the catch (23 - 28s)
  a <- fade(t, 23, 28)
  txt("BUT \"ZERO EMISSIONS\"\nHIDES SOMETHING", 0.5, 0.7, 34, brand$amber, "bold", a)
  txt("Lithium mining drains\nscarce desert water.", 0.5, 0.57, 30, brand$white, "plain", a * appear(t, 23.8))
  txt("Some cobalt mines in\nCongo use child labor.", 0.5, 0.46, 30, brand$white, "plain", a * appear(t, 24.8))
  txt("EVs shrink the damage.\nThey also move it.", 0.5, 0.33, 32, brand$mint, "bold", a * appear(t, 25.8))

  end_card(t, 28)
  txt("Data: EPA, EIA, ICCT, UCS, UN University", 0.5, 0.05, 18, brand$muted)
}

render_reel(draw_frame, "ev-environment-reel.mp4", seconds = 31)
