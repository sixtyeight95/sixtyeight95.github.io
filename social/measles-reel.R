# Reel for the measles post: measles-reel.mp4 (1080 x 1920, ~25 s)
# Run from this folder:  source("measles-reel.R")
source("../R/fred.R")
source("../R/measles_data.R")
source("../R/reel_helpers.R")

bars <- function(t, start, a) {
  box <- list(x0 = 0.1, x1 = 0.9, y0 = 0.3, y1 = 0.68)
  n <- nrow(measles_cases); bw <- (box$x1 - box$x0) / n
  for (i in seq_len(n)) {
    v <- measles_cases$cases[i]; yr <- measles_cases$year[i]
    grow <- ease((t - start - i * 0.12) / 0.4)
    col <- if (yr == 2026) brand$red else if (yr == 2025) brand$amber else "#3A5A73"
    h <- v / 5600 * (box$y1 - box$y0) * grow
    if (h > 0) grid.rect(box$x0 + (i - 0.5) * bw, box$y0, width = bw * 0.75, height = h,
                         just = c("center", "bottom"), gp = gpar(fill = col, col = NA, alpha = a))
    if (yr %in% c(2000, 2026)) txt(yr, box$x0 + (i - 0.5) * bw, box$y0 - 0.02, 20, brand$muted, "plain", a)
  }
  txt("5,412", box$x1 - bw * 0.5 - 0.01, 0.705, 40, brand$red, "bold", a * appear(t, start + 3.4))
}

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 4)
  txt("Measles was\nofficially eliminated\nin the U.S. in 2000.", 0.5, 0.6, 44, brand$white, "bold", a)
  txt("As of this week,\nit isn't anymore.", 0.5, 0.42, 40, brand$red, "bold", a * appear(t, 1.8))

  a <- fade(t, 4, 11)
  txt("Measles cases per year", 0.5, 0.8, 34, brand$white, "bold", a)
  bars(t, 4.2, a)
  txt("The most in more than 30 years", 0.5, 0.22, 30, brand$amber, "bold", a * appear(t, 8.3))

  a <- fade(t, 11, 16)
  txt(paste0(unvax_share_2026, "%"), 0.5, 0.6, 120, brand$amber, "bold", a)
  txt("of this year's cases were\nunvaccinated or unsure", 0.5, 0.47, 34, brand$white, "bold", a * appear(t, 11.6))
  txt(paste0(hosp_2026, "+ hospitalized. At least ", deaths_2026, " deaths."), 0.5, 0.38, 28, brand$muted, "plain", a * appear(t, 12.6))

  a <- fade(t, 16, 22)
  txt("Kindergartners with\nboth MMR shots", 0.5, 0.7, 34, brand$white, "bold", a)
  txt("95.2%", 0.5, 0.58, 64, brand$white, "bold", a * appear(t, 16.5))
  txt("2019–20", 0.5, 0.53, 24, brand$muted, "plain", a * appear(t, 16.5))
  txt("92.4%", 0.5, 0.44, 64, brand$red, "bold", a * appear(t, 17.5))
  txt("2025–26", 0.5, 0.39, 24, brand$muted, "plain", a * appear(t, 17.5))
  txt("About 95% stops outbreaks.\nNot sure you're covered?\nAsk your doctor.", 0.5, 0.27, 28, brand$mint, "bold", a * appear(t, 18.8))

  end_card(t, 22)
  txt("Data: CDC; PAHO. 2026 through mid-November.", 0.5, 0.05, 18, brand$muted)
}
render_reel(draw_frame, "measles-reel.mp4", seconds = 25)
