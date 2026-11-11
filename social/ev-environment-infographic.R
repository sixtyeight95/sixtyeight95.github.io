# ---------------------------------------------------------------------------
# Infographic: EV vs gas lifetime CO2 (the crossover)
#   ev-environment-infographic.png  1080 x 1350
# Run from this folder:  source("ev-environment-infographic.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/ev_data.R")
library(ragg)
library(grid)

long <- emissions %>% pivot_longer(c(gas, ev), names_to = "car", values_to = "tons")
cross_t <- battery_t + breakeven_miles * ev_g_per_mile / 1e6
comma0 <- function(x) formatC(round(x, -3), format = "d", big.mark = ",")

chart <- ggplot(long, aes(miles / 1000, tons, color = car)) +
  geom_line(linewidth = 1.6) +
  annotate("point", x = breakeven_miles / 1000, y = cross_t, color = brand$white, size = 3.5) +
  annotate("segment", x = breakeven_miles / 1000 + 1, xend = 44, y = cross_t - 0.5, yend = 4.5, color = brand$white) +
  annotate("text", x = 45, y = 4, hjust = 0, color = brand$white,
           family = brand$font, fontface = "bold", size = 4.3,
           label = paste0("EV pulls ahead at\n~", comma0(breakeven_miles), " miles")) +
  annotate("segment", x = 0.5, xend = 4, y = battery_t + 0.5, yend = 29, color = brand$mint) +
  annotate("text", x = 5, y = 31, hjust = 0, color = brand$mint, family = brand$font, size = 3.9,
           label = paste0("EV starts ", battery_t, " tons behind\n(making the battery)")) +
  annotate("text", x = 150, y = lifetime_gas_t, hjust = 1, vjust = -0.6, color = brand$red, family = brand$font,
           fontface = "bold", size = 4.5, label = paste0("Gas: ", round(lifetime_gas_t), " tons")) +
  annotate("text", x = 150, y = lifetime_ev_t, hjust = 1, vjust = 2.6, color = brand$mint, family = brand$font,
           fontface = "bold", size = 4.5, label = paste0("EV: ", round(lifetime_ev_t), " tons")) +
  scale_color_manual(values = c(gas = brand$red, ev = brand$mint), guide = "none") +
  scale_x_continuous(breaks = seq(0, 150, 50), labels = function(x) paste0(x, "k mi"), expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10), labels = function(x) paste(x, "t"), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = brand$font, base_size = 14) +
  theme(plot.background = element_rect(fill = brand$navy, color = NA),
        panel.background = element_rect(fill = brand$navy, color = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = brand$grid),
        axis.text = element_text(color = brand$muted))

txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}

agg_png("ev-environment-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage()
grid.rect(gp = gpar(fill = brand$navy, col = NA))

txt("ARE EVS REALLY\nBETTER FOR THE PLANET?", 0.5, 0.915, 25, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt("Total CO2 as you drive: building the battery, then every mile after.", 0.5, 0.825, 10.5, brand$mint)
txt("Gas car", 0.42, 0.792, 12, brand$red, "bold", just = "right")
txt("Electric car", 0.58, 0.792, 12, brand$mint, "bold", just = "left")

print(chart, vp = viewport(x = 0.5, y = 0.6, width = 0.92, height = 0.36))

txt("ON CARBON: YES", 0.5, 0.385, 13, brand$mint, "bold")
txt(paste0("Over ", comma0(lifetime_miles), " miles, our simple model shows the EV emitting about ",
           round(lifetime_cut * 100), "% less CO2.\nMajor studies find 60–68% less for U.S. EVs, and the gap grows as the grid gets cleaner."),
    0.5, 0.34, 9.8, brand$white)

grid.rect(x = 0.5, y = 0.225, width = 0.88, height = 0.13, gp = gpar(fill = brand$band, col = NA))
txt("THE CATCH", 0.5, 0.272, 11, brand$amber, "bold")
txt(paste0("• On an all-coal grid, an EV is about as dirty per mile as a gas car.\n",
           "• Lithium mining can drain scarce water (up to 65% of local use in Chile's Atacama).\n",
           "• Much of the world's cobalt comes from Congo, where child labor is documented.\n",
           "• \"Zero emissions\" moves the damage; it doesn't erase it."), 0.5, 0.215, 9.2, brand$white)

txt(paste0("Model: ", gas_mpg, "-mpg gas car (EPA: 8,887 g CO2/gallon, tailpipe only); EV at ", ev_kwh_per_mile,
           " kWh/mile on the 2024 U.S. grid (EIA);\n", battery_kwh, " kWh battery at ", battery_kg_per_kwh,
           " kg CO2/kWh (ICCT). Building the rest of the car is similar for both and left out."), 0.5, 0.12, 7, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: EPA, EIA, ICCT, Union of Concerned Scientists, UN University", 0.5, 0.012, 7.5, brand$muted)
dev.off()
message("Saved ev-environment-infographic.png")
