# Reel for the generosity post: generosity-reel.mp4 (1080 x 1920, ~25 s)
# Run from this folder:  source("generosity-reel.R")
source("../R/fred.R")
source("../R/generosity_data.R")
source("../R/reel_helpers.R")

warm <- "#F4B942"; rose <- "#F28C6F"
count_to <- function(final, t, start, len = 1.3) final * ease((t - start) / len)

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("Feels like everyone's\nonly out for themselves?", 0.5, 0.6, 40, brand$white, "bold", a)
  txt("The data says otherwise.", 0.5, 0.46, 38, warm, "bold", a * appear(t, 1.7))

  a <- fade(t, 3.5, 8)
  txt("Americans gave", 0.5, 0.66, 36, brand$white, "bold", a)
  txt(paste0("$", round(count_to(giving_total_2025_b, t, 3.9)), "B"), 0.5, 0.56, 100, warm, "bold", a)
  txt("to charity in 2025.\nA new record.", 0.5, 0.45, 34, brand$white, "bold", a * appear(t, 5))

  a <- fade(t, 8, 12.5)
  txt(paste0(formatC(count_to(volunteers_m, t, 8.4), format = "f", digits = 1), "M"), 0.5, 0.6, 100, brand$mint, "bold", a)
  txt(paste0("people volunteered,\ngiving ", volunteer_hours_b, " billion hours"), 0.5, 0.47, 34, brand$white, "bold", a * appear(t, 9.3))

  a <- fade(t, 12.5, 17)
  txt(paste0(round(count_to(neighbors_helped_m, t, 12.9), 1), "M"), 0.5, 0.6, 100, rose, "bold", a)
  txt("helped a neighbor.\nMore than half of us.", 0.5, 0.47, 34, brand$white, "bold", a * appear(t, 13.8))

  a <- fade(t, 17, 22)
  txt("This Giving Tuesday:", 0.5, 0.64, 36, brand$white, "bold", a)
  txt(giving_tuesday_2026, 0.5, 0.56, 38, warm, "bold", a)
  txt(paste0("Last year, ", round(giving_tuesday_2025_m), "M Americans\ngave $", giving_tuesday_2025_b, "B in one day."),
      0.5, 0.45, 30, brand$white, "plain", a * appear(t, 18.2))
  txt("Happy Thanksgiving.", 0.5, 0.34, 34, brand$mint, "bold", a * appear(t, 19.5))

  end_card(t, 22)
  txt("Data: Giving USA 2026, Census Bureau & AmeriCorps, GivingTuesday", 0.5, 0.05, 16, brand$muted)
}
render_reel(draw_frame, "generosity-reel.mp4", seconds = 25)
