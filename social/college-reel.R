# ---------------------------------------------------------------------------
# Reel for the college post:  college-reel.mp4  (1080 x 1920, about 27 seconds)
# Run from this folder:  source("college-reel.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/education_data.R")
source("../R/reel_helpers.R")

since_1980 <- growth_since(1980)
times <- function(x) paste0(formatC(x, format = "f", digits = 1), "×")
twist_drop <- 1 - college_now$hours_index / hours_peak$hours_index

chart_x <- college_yearly$year
chart_y <- college_yearly$hours_index
box     <- list(x = 0.55, y = 0.5, w = 0.78, h = 0.36)
y_max   <- 400
when_drawn <- function(y) 12 + 6 * (which(chart_x == y) / length(chart_x))

# a number that counts up from 1.0 to its final value
count_up <- function(final, t, start, len = 1.2) 1 + (final - 1) * ease((t - start) / len)

draw_frame <- function(t) {
  background()

  # Scene 1: hook (0 - 3.5s)
  a <- fade(t, 0, 3.5)
  txt("College got WAY more\nexpensive than\neverything else.", 0.5, 0.6, 42, brand$white, "bold", a)
  txt(paste0("But something changed\nin ", hours_peak$year, "."), 0.5, 0.42, 38, brand$amber, "bold", a * appear(t, 1.8))

  # Scene 2: the multipliers (3.5 - 10s)
  a <- fade(t, 3.5, 10)
  txt("SINCE 1980", 0.5, 0.74, 30, brand$muted, "bold", a)
  rows <- list(
    list(label = "College tuition", value = since_1980$tuition, color = brand$amber, start = 4.2, y = 0.62),
    list(label = "Hourly pay",      value = since_1980$wages,   color = brand$white, start = 5.6, y = 0.5),
    list(label = "Everything else", value = since_1980$prices,  color = brand$white, start = 7.0, y = 0.38)
  )
  for (r in rows) {
    aa <- a * appear(t, r$start)
    txt(times(count_up(r$value, t, r$start)), 0.5, r$y, 60, r$color, "bold", aa)
    txt(r$label, 0.5, r$y - 0.05, 28, brand$muted, "plain", aa)
  }

  # Scene 3: hours of work line (10 - 20.5s)
  a <- fade(t, 10, 20.5)
  if (a > 0) {
    txt("A YEAR OF COLLEGE,\nIN HOURS OF WORK", 0.5, 0.8, 28, brand$white, "bold", a)
    txt("(1980 = 100 hours)", 0.5, 0.75, 22, brand$muted, "plain", a)
    draw_line_chart(chart_x, chart_y, progress = (t - 12) / 6, alpha = a, box = box,
                    y_max = y_max, y_breaks = c(0, 100, 200, 300, 400), y_label = function(b) b,
                    x_breaks = c(1980, 2000, 2020))
    marks <- list(
      list(x = 1980, v = 100, lab = "1980\n100 hrs", dx = 0.07, dy = -0.05),
      list(x = hours_peak$year, v = hours_peak$hours_index,
           lab = paste0("Peak ", hours_peak$year, "\n", round(hours_peak$hours_index), " hrs"), dx = 0, dy = 0.055),
      list(x = max(chart_x), v = chart_y[length(chart_y)],
           lab = paste0("Today\n", round(college_now$hours_index), " hrs"), dx = -0.07, dy = -0.06)
    )
    for (m in marks) {
      p  <- chart_point(m$x, m$v, chart_x, y_max, box)
      aa <- a * appear(t, when_drawn(m$x))
      if (aa > 0) {
        grid.circle(p$x, p$y, r = unit(0.008, "npc"), gp = gpar(fill = brand$white, col = NA, alpha = aa))
        txt(m$lab, p$x + m$dx, p$y + m$dy, 22, brand$white, "bold", aa)
      }
    }
  }

  # Scene 4: the twist (20.5 - 25s)
  a <- fade(t, 20.5, 25)
  txt("THE TWIST", 0.5, 0.68, 32, brand$amber, "bold", a)
  txt(paste0("Since ", hours_peak$year, ", paychecks\ngrew faster than tuition."), 0.5, 0.59, 36, brand$white, "bold", a)
  txt(paste0("College now takes ", round(twist_drop * 100), "% FEWER\nhours of work than at the peak."), 0.5, 0.47, 34, brand$mint, "bold",
      a * appear(t, 21.8))
  txt("(Still ~3× more than 1980.)", 0.5, 0.38, 28, brand$muted, "plain", a * appear(t, 23))

  # Scene 5: end card
  end_card(t, 25)

  txt("Data: U.S. Bureau of Labor Statistics", 0.5, 0.05, 18, brand$muted)
}

render_reel(draw_frame, "college-reel.mp4", seconds = 28)
