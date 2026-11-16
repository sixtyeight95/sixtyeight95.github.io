# Infographic: how long it takes to find a job (jobs-infographic.png, 1080 x 1350)
# Run from this folder:  source("jobs-infographic.R")
source("../R/fred.R")
source("../R/jobs_data.R")
library(ragg)
library(grid)

f1 <- function(x) formatC(x, format = "f", digits = 1)
series <- jobs %>% filter(date >= as.Date("2000-01-01"), !is.na(avg_weeks))
recessions <- tibble(start = as.Date(c("2001-03-01", "2007-12-01", "2020-02-01")),
                     end   = as.Date(c("2001-11-01", "2009-06-01", "2020-04-01")))

chart <- ggplot(series, aes(date, avg_weeks)) +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
            fill = brand$grid, alpha = 0.8) +
  geom_line(color = brand$amber, linewidth = 1.2) +
  annotate("point", x = max(series$date), y = jobs_now$avg_weeks, color = brand$white, size = 3) +
  annotate("text", x = max(series$date), y = jobs_now$avg_weeks + 5, label = paste0("Now\n", f1(jobs_now$avg_weeks), " wks"),
           color = brand$white, family = brand$font, fontface = "bold", size = 4.2, hjust = 0.8, lineheight = 0.9) +
  annotate("text", x = as.Date("2011-07-01"), y = 42.5, label = "Great Recession", color = brand$muted,
           family = brand$font, size = 3.6) +
  scale_y_continuous(limits = c(0, 46), breaks = seq(0, 40, 10), labels = function(x) paste(x, "wks"), expand = c(0, 0)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = brand$font, base_size = 14) +
  theme(plot.background = element_rect(fill = brand$navy, color = NA),
        panel.background = element_rect(fill = brand$navy, color = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = brand$grid), axis.text = element_text(color = brand$muted))

txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}

agg_png("jobs-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("HOW LONG DOES IT TAKE\nTO FIND A JOB NOW?", 0.5, 0.915, 25, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt("Average number of weeks unemployed Americans have been looking for work.", 0.5, 0.825, 10.5, brand$mint)
txt("Shaded = recessions", 0.5, 0.795, 9.5, brand$muted)

print(chart, vp = viewport(x = 0.5, y = 0.615, width = 0.92, height = 0.33))

txt("THE \"NO HIRE, NO FIRE\" JOB MARKET", 0.5, 0.42, 11.5, brand$amber, "bold")
tiles <- tribble(~x, ~now, ~then, ~label,
  0.2, paste0(f1(jobs_now$layoffs), "%"), paste0(f1(jobs_2019$layoffs), "%"), "laid off per month\n(near record low)",
  0.5, paste0(f1(jobs_now$hires), "%"), paste0(f1(jobs_2019$hires), "%"), "hired per month\n(slow)",
  0.8, paste0(round(jobs_now$long_term_share), "%"), paste0(round(jobs_2019$long_term_share), "%"), "of job seekers out\n6+ months")
for (i in 1:3) {
  txt(tiles$now[i], tiles$x[i], 0.37, 22, brand$white, "bold")
  txt(paste("2019:", tiles$then[i]), tiles$x[i], 0.335, 9.5, brand$muted)
  txt(tiles$label[i], tiles$x[i], 0.295, 9.5, brand$mint)
}

grid.rect(x = 0.5, y = 0.185, width = 0.88, height = 0.1, gp = gpar(fill = brand$band, col = NA))
txt("WHAT IT MEANS", 0.5, 0.215, 11, brand$amber, "bold")
txt(paste0("If you have a job, it's relatively safe. If you lose one, the search is longer:\n",
           "about ", round(jobs_now$avg_weeks), " weeks on average, versus ", round(jobs_2019$avg_weeks), " in 2019. Unemployment is only ",
           f1(jobs_now$unemployment), "%."), 0.5, 0.175, 9.5, brand$white)

txt(paste0("Average and long-term figures: BLS household survey, ", format(jobs_now$cps_date, "%b %Y"),
           ". Hiring and layoff rates: BLS JOLTS, ", format(jobs_now$jolts_date, "%b %Y"),
           ", as % of all jobs per month.\nThe average is pulled up by people out a very long time; the median job seeker has been looking ",
           round(jobs_now$median_weeks), " weeks."), 0.5, 0.105, 7, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: U.S. Bureau of Labor Statistics via FRED", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved jobs-infographic.png")
