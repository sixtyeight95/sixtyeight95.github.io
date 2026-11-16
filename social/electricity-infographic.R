# Infographic: why electric bills keep rising (electricity-infographic.png, 1080 x 1350)
# Run from this folder:  source("electricity-infographic.R")
source("../R/fred.R")
source("../R/electricity_data.R")
library(ragg)
library(grid)

usd0 <- function(x) paste0("$", round(x))
d <- elec %>% filter(date >= as.Date("2000-01-01"))

chart <- ggplot(d, aes(date, bill)) +
  geom_area(fill = brand$band, alpha = 0.6) +
  geom_line(color = brand$amber, linewidth = 1.3) +
  annotate("point", x = elec_2020$date, y = elec_2020$bill, color = brand$white, size = 3) +
  annotate("text", x = elec_2020$date, y = elec_2020$bill + 25, label = paste0("Jan 2020\n", usd0(elec_2020$bill)),
           color = brand$white, family = brand$font, fontface = "bold", size = 4, lineheight = 0.9) +
  annotate("point", x = elec_latest$date, y = elec_latest$bill, color = brand$white, size = 3) +
  annotate("text", x = elec_latest$date, y = elec_latest$bill + 25, label = paste0("Now\n", usd0(elec_latest$bill)),
           color = brand$white, family = brand$font, fontface = "bold", size = 4, lineheight = 0.9, hjust = 0.8) +
  scale_y_continuous(limits = c(0, 210), breaks = seq(0, 200, 50), labels = function(x) paste0("$", x), expand = c(0, 0)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.03))) +
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

agg_png("electricity-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("WHY DOES MY ELECTRIC\nBILL KEEP GOING UP?", 0.5, 0.915, 25, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt(paste0("Monthly bill for a home using ", home_kwh_month, " kWh (about the U.S. average), at average prices."),
    0.5, 0.825, 10.5, brand$mint)

print(chart, vp = viewport(x = 0.5, y = 0.63, width = 0.92, height = 0.34))

tiles <- tribble(~x, ~value, ~label,
  0.2, paste0("+", round(since_2020), "%"), "price per kWh\nsince Jan 2020",
  0.5, paste0("+", formatC(elec_yoy$yoy, format = "f", digits = 1), "%"), "in just the\nlast year",
  0.8, paste0(formatC(elec_now$hours_for_bill, format = "f", digits = 1), " hrs"), "of work to pay\nthe monthly bill")
for (i in 1:3) {
  txt(tiles$value[i], tiles$x[i], 0.415, 22, brand$amber, "bold")
  txt(tiles$label[i], tiles$x[i], 0.365, 9.8, brand$muted)
}

grid.rect(x = 0.5, y = 0.235, width = 0.88, height = 0.15, gp = gpar(fill = brand$band, col = NA))
txt("WHAT'S BEHIND IT", 0.5, 0.29, 11, brand$amber, "bold")
txt(paste0("• AI data centers: ", dc_share_2023, "% of U.S. power in 2023, projected ", dc_share_2028_low, "–",
           dc_share_2028_high, "% by 2028.\n",
           "• Utilities are spending heavily on new power lines and grid upgrades.\n",
           "• Natural gas, the biggest fuel for U.S. power, has seen big price swings.\n",
           "• Experts disagree on how much of the rise each one explains."), 0.5, 0.228, 9.3, brand$white)

txt(paste0("Prices: BLS average price per kWh, U.S. city average. Typical use: EIA (", home_kwh_month,
           " kWh/month). Your bill also depends on your state,\nyour utility's fixed charges, and how much you use. ",
           "Data centers: Lawrence Berkeley National Laboratory for the U.S. Dept. of Energy (2024)."), 0.5, 0.13, 7, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: BLS via FRED; EIA; Berkeley Lab/DOE", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved electricity-infographic.png")
