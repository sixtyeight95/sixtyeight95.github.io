# Infographic: Thanksgiving dinner in hours of work  (thanksgiving-infographic.png, 1080 x 1350)
# Run from this folder:  source("thanksgiving-infographic.R")
source("../R/fred.R")
source("../R/thanksgiving_data.R")
library(ragg)
library(grid)

hrs <- function(x) paste(formatC(x, format = "f", digits = 1), "hrs")
marks <- thanksgiving %>% filter(year %in% c(tg_first$year, tg_peak$year, tg_latest$year)) %>%
  mutate(hj = case_when(year == tg_first$year ~ 0, year == tg_latest$year ~ 0.9, TRUE ~ 0.5),
         vj = ifelse(year == tg_latest$year, 1.5, -0.45))

chart <- ggplot(thanksgiving, aes(year, hours_of_work)) +
  geom_area(fill = brand$band, alpha = 0.6) +
  geom_line(color = brand$amber, linewidth = 1.4) +
  geom_point(data = marks, color = brand$white, size = 3) +
  geom_text(data = marks, aes(label = paste0(year, "\n", hrs(hours_of_work)), hjust = hj, vjust = vj),
            color = brand$white, family = brand$font, fontface = "bold", size = 4.1, lineheight = 0.9) +
  scale_y_continuous(limits = c(0, 4.2), breaks = 0:4, labels = function(x) paste(x, "hrs"), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1990, 2020, 10), expand = expansion(mult = c(0.03, 0.04))) +
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

agg_png("thanksgiving-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("THANKSGIVING DINNER,\nIN HOURS OF WORK", 0.5, 0.915, 25, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt("How long the average worker has to work to buy the classic dinner for 10.", 0.5, 0.825, 10.5, brand$mint)

print(chart, vp = viewport(x = 0.5, y = 0.6, width = 0.92, height = 0.38))

tiles <- tribble(~x, ~value, ~label,
  0.2, paste0("$", formatC(tg_latest$cost_for_10, format = "f", digits = 2)), paste0("dinner for 10\n(", tg_latest$year, ")"),
  0.5, paste0(round(tg_latest$minutes_per_guest), " min"), "of work\nper guest",
  0.8, paste0("$", formatC(tg_turkey_2025, format = "f", digits = 2)), "for a 16-lb\nturkey")
for (i in 1:3) {
  txt(tiles$value[i], tiles$x[i], 0.355, 22, brand$amber, "bold")
  txt(tiles$label[i], tiles$x[i], 0.305, 10, brand$muted)
}

grid.rect(x = 0.5, y = 0.205, width = 0.88, height = 0.1, gp = gpar(fill = brand$band, col = NA))
txt("THE TAKEAWAY", 0.5, 0.235, 11, brand$amber, "bold")
txt(paste0("Dinner prices jumped in 2022, but paychecks caught up. In ", tg_latest$year, ", the classic\n",
           "dinner took the fewest hours of work in the survey's 40-year history."), 0.5, 0.195, 9.6, brand$white)

txt(paste0("Classic dinner for 10: turkey, stuffing, sweet potatoes, rolls, peas, cranberries, veggie tray, pumpkin pie,\n",
           "whipped cream, milk, coffee. Pay = average hourly earnings in November, before taxes. ",
           tg_latest$year + 1, " survey due this week."), 0.5, 0.12, 7, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: American Farm Bureau Federation; BLS earnings via FRED", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved thanksgiving-infographic.png")
