# ---------------------------------------------------------------------------
# Infographic: EV vs gas vs hybrid, total cost per year (AAA 2026)
#   ev-cost-infographic.png  1080 x 1350
# Run from this folder:  source("ev-cost-infographic.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/ev_data.R")
library(ragg)
library(grid)

pt_colors <- c(Electric = brand$mint, Gas = brand$red, Hybrid = brand$amber)
kk <- function(x) paste0("$", formatC(x / 1000, format = "f", digits = 1), "k")

chart <- ggplot(aaa, aes(x = total_per_year, y = fct_rev(class), fill = powertrain, group = fct_rev(powertrain))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = kk(total_per_year), color = powertrain), position = position_dodge(width = 0.8),
            hjust = -0.15, family = brand$font, fontface = "bold", size = 4) +
  scale_fill_manual(values = pt_colors, guide = "none") +
  scale_color_manual(values = pt_colors, guide = "none") +
  scale_x_continuous(limits = c(0, 20500), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = brand$font, base_size = 14) +
  theme(plot.background = element_rect(fill = brand$navy, color = NA),
        panel.background = element_rect(fill = brand$navy, color = NA),
        panel.grid = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(color = brand$white, size = 14))

sedan_ev  <- cost_row("Medium sedan", "Electric")
sedan_gas <- cost_row("Medium sedan", "Gas")
usd <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))

txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}

agg_png("ev-cost-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage()
grid.rect(gp = gpar(fill = brand$navy, col = NA))

txt("DOES AN EV ACTUALLY\nSAVE YOU MONEY?", 0.5, 0.915, 25, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt("Total cost per year to own a new 2026 vehicle, 15,000 miles a year.", 0.5, 0.825, 10.5, brand$mint)

lx <- c(0.33, 0.5, 0.67); pts <- c("Electric", "Gas", "Hybrid")
for (i in 1:3) {
  grid.rect(lx[i] - 0.045, 0.792, width = 0.018, height = 0.012, gp = gpar(fill = pt_colors[[pts[i]]], col = NA))
  txt(pts[i], lx[i] - 0.03, 0.792, 11, pt_colors[[pts[i]]], "bold", just = "left")
}

print(chart, vp = viewport(x = 0.5, y = 0.585, width = 0.92, height = 0.38))

# the medium sedan story
txt("WHY (MEDIUM SEDAN, PER YEAR)", 0.5, 0.37, 11.5, brand$amber, "bold")
txt("Fuel + maintenance", 0.28, 0.33, 10, brand$muted, "bold")
txt(paste0("EV ", usd(sedan_ev$running), "  vs  Gas ", usd(sedan_gas$running)), 0.28, 0.297, 12, brand$white, "bold")
txt(paste0("EV saves ", usd(sedan_gas$running - sedan_ev$running)), 0.28, 0.265, 12, brand$mint, "bold")
txt("Depreciation, insurance, fees, interest", 0.72, 0.33, 10, brand$muted, "bold")
txt(paste0("EV ", usd(sedan_ev$owning), "  vs  Gas ", usd(sedan_gas$owning)), 0.72, 0.297, 12, brand$white, "bold")
txt(paste0("EV costs ", usd(sedan_ev$owning - sedan_gas$owning), " more"), 0.72, 0.265, 12, brand$red, "bold")

grid.rect(x = 0.5, y = 0.185, width = 0.88, height = 0.085, gp = gpar(fill = brand$band, col = NA))
txt("THE BOTTOM LINE", 0.5, 0.21, 11, brand$amber, "bold")
txt(paste0("EVs are cheaper to run but lose value fast, so they're about even in SUVs, pricier in sedans\n",
           "and pickups. Hybrids are cheapest in every class. The $7,500 federal EV credit ended Sept. 2025."),
    0.5, 0.175, 9.3, brand$white)

txt(paste0("Averages for top-selling 2026 models, 5-year ownership. AAA's EV fuel costs line up with home electricity\n",
           "prices; relying on public fast chargers (~$", public_kwh_price, "/kWh vs ~$", round(home_kwh_price, 2),
           " at home) raises EV fuel costs about ", round(public_multiplier, 1), "x."), 0.5, 0.11, 7.2, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: AAA Your Driving Costs 2026; BLS electricity prices", 0.5, 0.012, 7.5, brand$muted)
dev.off()
message("Saved ev-cost-infographic.png")
