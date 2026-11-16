# Reel for the job search post: jobs-reel.mp4 (1080 x 1920, ~26 s)
# Run from this folder:  source("jobs-reel.R")
source("../R/fred.R")
source("../R/jobs_data.R")
source("../R/reel_helpers.R")

f1 <- function(x) formatC(x, format = "f", digits = 1)
series <- jobs %>% filter(date >= as.Date("2000-01-01"), !is.na(avg_weeks))
sx <- year(series$date) + (month(series$date) - 1) / 12
box <- list(x = 0.55, y = 0.5, w = 0.78, h = 0.36)

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("Applying for jobs and\nhearing nothing back?", 0.5, 0.6, 42, brand$white, "bold", a)
  txt("It's not just you.", 0.5, 0.46, 40, brand$amber, "bold", a * appear(t, 1.6))

  a <- fade(t, 3.5, 12)
  if (a > 0) {
    txt("WEEKS UNEMPLOYED\n(AVERAGE)", 0.5, 0.79, 30, brand$white, "bold", a)
    draw_line_chart(sx, series$avg_weeks, (t - 4.3) / 5.5, a, box, 45, c(0, 20, 40), function(b) paste(b, "wks"),
                    c(2000, 2010, 2020))
    p <- chart_point(max(sx), jobs_now$avg_weeks, sx, 45, box)
    aa <- a * appear(t, 9.8)
    if (aa > 0) {
      grid.circle(p$x, p$y, r = unit(0.009, "npc"), gp = gpar(fill = brand$white, col = NA, alpha = aa))
      txt(paste0("Now: ", f1(jobs_now$avg_weeks), " weeks"), p$x - 0.2, p$y + 0.07, 26, brand$white, "bold", aa)
    }
  }

  a <- fade(t, 12, 18.5)
  txt("WELCOME TO THE\n\"NO HIRE, NO FIRE\" MARKET", 0.5, 0.71, 32, brand$amber, "bold", a)
  txt(paste0("Laid off: ", f1(jobs_now$layoffs), "% a month\n(near record low)"), 0.5, 0.57, 34, brand$mint, "bold", a * appear(t, 12.8))
  txt(paste0("Hired: ", f1(jobs_now$hires), "% a month\n(2019: ", f1(jobs_2019$hires), "%)"), 0.5, 0.43, 34, brand$red, "bold", a * appear(t, 14))

  a <- fade(t, 18.5, 23)
  txt("Have a job? It's safe.", 0.5, 0.62, 38, brand$white, "bold", a)
  txt("Lose one? Expect\na longer search.", 0.5, 0.5, 38, brand$amber, "bold", a * appear(t, 19.4))
  txt(paste0(round(jobs_now$long_term_share), "% of job seekers have\nbeen looking 6+ months."), 0.5, 0.37, 30, brand$white,
      "plain", a * appear(t, 20.5))

  end_card(t, 23)
  txt("Data: U.S. Bureau of Labor Statistics", 0.5, 0.05, 18, brand$muted)
}
render_reel(draw_frame, "jobs-reel.mp4", seconds = 26)
