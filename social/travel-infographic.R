# Infographic: what it costs to get home for Thanksgiving (travel-infographic.png, 1080 x 1350)
# Run from this folder:  source("travel-infographic.R")
source("../R/fred.R")
source("../R/travel_data.R")
library(ragg)
library(grid)

txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}
usd  <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
usd2 <- function(x) paste0("$", formatC(x, format = "f", digits = 2))

agg_png("travel-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("WHAT IT COSTS TO GET\nHOME FOR THANKSGIVING", 0.5, 0.92, 24, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.862, 0.862), gp = gpar(col = brand$amber, lwd = 4))
txt(paste0(travelers_2026_m, " million Americans are traveling. Here's what they're paying."), 0.5, 0.835, 10.5, brand$mint)

# drive vs fly
txt("A 500-MILE TRIP HOME (AND BACK), FAMILY OF 4", 0.5, 0.785, 11, brand$amber, "bold")
grid.roundrect(0.27, 0.665, width = 0.42, height = 0.19, r = unit(0.015, "npc"), gp = gpar(fill = "#16263D", col = NA))
txt("DRIVE", 0.27, 0.735, 12, brand$mint, "bold")
txt(usd(drive_cost_now), 0.27, 0.68, 34, brand$white, "bold")
txt(paste0("in gas (25 mpg)\nvs ", usd(drive_cost_2025), " last Thanksgiving"), 0.27, 0.615, 9.5, brand$muted)
grid.roundrect(0.73, 0.665, width = 0.42, height = 0.19, r = unit(0.015, "npc"), gp = gpar(fill = "#16263D", col = NA))
txt("FLY", 0.73, 0.735, 12, brand$red, "bold")
txt(usd(fly_cost_family), 0.73, 0.68, 34, brand$white, "bold")
txt(paste0("4 roundtrip tickets at ", usd(airfare_avg_rt), "\neach, up ", airfare_change, "% from last year"), 0.73, 0.615, 9.5, brand$muted)
txt("No wonder nearly 9 in 10 holiday travelers drive.", 0.5, 0.545, 10.5, brand$white, "bold")

# gas
txt("GAS", 0.5, 0.495, 11, brand$amber, "bold")
txt(paste0(usd2(gas_now$value), "/gal"), 0.3, 0.45, 22, brand$white, "bold")
txt(paste0("national average\n(week of Nov ", format(gas_now$date, "%d"), ")"), 0.3, 0.405, 9, brand$muted)
txt(paste0("+", usd2(gas_now$value - gas_tg_2025$value)), 0.7, 0.45, 22, brand$red, "bold")
txt("more than Thanksgiving\nweek last year", 0.7, 0.405, 9, brand$muted)
txt(paste0("AAA: Thanksgiving gas has never topped $4. The record is ", usd2(gas_tg_record), " (2022)."), 0.5, 0.36, 9.5, brand$white)

# best/worst
grid.rect(x = 0.5, y = 0.215, width = 0.88, height = 0.21, gp = gpar(fill = brand$band, col = NA))
txt("WHEN TO HIT THE ROAD", 0.5, 0.3, 11, brand$amber, "bold")
rows <- inrix %>% filter(day %in% c("Tue, Nov 24", "Wed, Nov 25", "Thu, Nov 26", "Sun, Nov 29"))
txt("Worst", 0.55, 0.27, 9.5, brand$red, "bold"); txt("Best", 0.8, 0.27, 9.5, brand$mint, "bold")
for (i in seq_len(nrow(rows))) {
  y <- 0.245 - (i - 1) * 0.028
  txt(rows$day[i], 0.13, y, 10, brand$white, "bold", just = "left")
  txt(rows$worst[i], 0.55, y, 10, brand$white)
  txt(rows$best[i], 0.8, y, 10, brand$white)
}
txt("Go early. Afternoons before and after the holiday are the worst.", 0.5, 0.135, 9.5, brand$white, "bold")

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: AAA & INRIX holiday forecast (Nov 16, 2026); EIA gas prices", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved travel-infographic.png")
