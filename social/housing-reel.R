# ---------------------------------------------------------------------------
# Reel for the housing post:  housing-reel.mp4  (1080 x 1920, about 26 seconds)
# Run from this folder:  source("housing-reel.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/housing_data.R")
source("../R/reel_helpers.R")

hrs <- function(x) paste(round(x), "hrs")

chart_x <- year(housing_quarterly$quarter) + (month(housing_quarterly$quarter) - 1) / 12
chart_y <- housing_quarterly$hours_per_month
box     <- list(x = 0.55, y = 0.5, w = 0.78, h = 0.36)
y_max   <- 150

y1985 <- year_row(1985)
y1990 <- year_row(1990)
y2021 <- year_row(2021)

# when each labeled point gets drawn (share of the line drawn at that point)
when_drawn <- function(q) 9 + 7 * (which(housing_quarterly$quarter == q) / length(chart_x))

draw_frame <- function(t) {
  background()

  # Scene 1: hook (0 - 3.5s)
  a <- fade(t, 0, 3.5)
  txt("\"Nobody can afford\na house anymore.\"", 0.5, 0.6, 44, brand$white, "bold", a)
  txt("Is that true?", 0.5, 0.45, 40, brand$amber, "bold", a * appear(t, 1.6))

  # Scene 2: the idea (3.5 - 8s)
  a <- fade(t, 3.5, 8)
  txt("Let's measure the mortgage\nin HOURS OF WORK.", 0.5, 0.6, 38, brand$mint, "bold", a)
  txt(paste0("A full-time month is\nabout ", work_hours_month, " hours."), 0.5, 0.45, 34, brand$white, "plain",
      a * appear(t, 5.2))

  # Scene 3: the line draws itself (8 - 18s)
  a <- fade(t, 8, 18)
  if (a > 0) {
    txt("HOURS OF WORK A MONTH\nTO PAY THE MORTGAGE", 0.5, 0.8, 28, brand$white, "bold", a)
    txt("(typical new home, 10% down)", 0.5, 0.75, 22, brand$muted, "plain", a)
    draw_line_chart(chart_x, chart_y, progress = (t - 9) / 7, alpha = a, box = box,
                    y_max = y_max, y_breaks = c(0, 50, 100, 150), y_label = function(b) b,
                    x_breaks = c(1980, 2000, 2020))
    marks <- list(
      list(q = peak$quarter, v = peak$hours_per_month, lab = paste0(year(peak$quarter), ": ", hrs(peak$hours_per_month), "\nrates ~", round(peak$rate), "%"), dy = 0.045),
      list(q = low$quarter,  v = low$hours_per_month,  lab = paste0(year(low$quarter), ": ", hrs(low$hours_per_month), "\nrates ~", round(low$rate), "%"),  dy = -0.045),
      list(q = latest_quarter, v = chart_y[length(chart_y)], lab = paste0("Today\n", hrs(housing_now$hours_per_month)), dy = 0.11)
    )
    for (m in marks) {
      p  <- chart_point(year(m$q) + (month(m$q) - 1) / 12, m$v, chart_x, y_max, box)
      aa <- a * appear(t, when_drawn(m$q))
      if (aa > 0) {
        grid.circle(p$x, p$y, r = unit(0.008, "npc"), gp = gpar(fill = brand$white, col = NA, alpha = aa))
        txt(m$lab, min(p$x, 0.86), p$y + m$dy, 22, brand$white, "bold", aa)
      }
    }
  }

  # Scene 4: compare (18 - 22s)
  a <- fade(t, 18, 22)
  txt(paste0("Today: ", hrs(housing_now$hours_per_month), " a month"), 0.5, 0.62, 42, brand$amber, "bold", a)
  txt(paste0("1990: ", hrs(y1990$hours_per_month), "     2021: ", hrs(y2021$hours_per_month)), 0.5, 0.54, 32,
      brand$white, "plain", a * appear(t, 18.8))
  txt("The payment isn't the\nworst it's ever been.", 0.5, 0.43, 34, brand$mint, "bold", a * appear(t, 19.8))

  # Scene 5: the catch (22 - 26.5s)
  a <- fade(t, 22, 26.5)
  txt("THE REAL WALL:", 0.5, 0.66, 32, brand$muted, "bold", a)
  txt("The down payment", 0.5, 0.59, 44, brand$white, "bold", a)
  txt(paste0("10% down = ", round(housing_now$months_to_save_down, 1), " months of full pay"), 0.5, 0.5, 34,
      brand$red, "bold", a * appear(t, 23))
  txt(paste0("In 1985: ", round(y1985$months_to_save_down, 1), " months"), 0.5, 0.44, 30, brand$white, "plain",
      a * appear(t, 23.8))

  # Scene 6: end card
  end_card(t, 26.5)

  txt("Data: Census/HUD, Freddie Mac, BLS", 0.5, 0.05, 18, brand$muted)
}

render_reel(draw_frame, "housing-reel.mp4", seconds = 29)
