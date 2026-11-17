# Infographic: are Black Friday deals real? (black-friday-infographic.png, 1080 x 1350)
# Run from this folder:  source("black-friday-infographic.R")
source("../R/fred.R")
source("../R/black_friday_data.R")
library(ragg)
library(grid)

txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}
pct <- function(x, d = 1) paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = d), "%")

agg_png("black-friday-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("ARE BLACK FRIDAY\nDEALS REAL?", 0.5, 0.92, 26, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.857, 0.857), gp = gpar(col = brand$amber, lwd = 4))
txt("Yes, but they're a lot smaller than the \"50% OFF\" signs.", 0.5, 0.83, 11, brand$mint)

txt("HOW MUCH PRICES ACTUALLY DROP FROM OCTOBER TO NOVEMBER", 0.5, 0.78, 10.5, brand$amber, "bold")
txt("(average of 2010–2024, what people really paid across all stores)", 0.5, 0.757, 8.5, brand$muted)

items <- bf_prices %>% filter(item %in% c("Clothing", "Toys", "Computers"))
for (i in seq_len(nrow(items))) {
  y <- 0.705 - (i - 1) * 0.075
  txt(items$item[i], 0.1, y, 13, brand$white, "bold", just = "left")
  w <- abs(items$nov_change[i]) / 5 * 0.45
  grid.rect(0.33, y, width = w, height = 0.035, just = c("left", "center"), gp = gpar(fill = brand$mint, col = NA))
  txt(pct(items$nov_change[i]), 0.33 + w + 0.015, y, 14, brand$mint, "bold", just = "left")
  txt(paste0("fell in ", items$years_fell[i], " of ", items$years[i], " years"), 0.9, y, 9, brand$muted, just = "right")
}
txt(paste0("Clothes get even cheaper by December: ", pct(items$dec_change[1]), " vs. October."), 0.5, 0.49, 10, brand$white, "bold")

# this year's price increases
grid.rect(x = 0.5, y = 0.33, width = 0.88, height = 0.22, gp = gpar(fill = brand$band, col = NA))
txt("THE CATCH: GIFTS COST MORE THIS YEAR", 0.5, 0.415, 11.5, brand$amber, "bold")
for (i in seq_len(nrow(items))) {
  x <- c(0.23, 0.5, 0.77)[i]
  txt(pct(items$yoy[i]), x, 0.36, 24, brand$red, "bold")
  txt(items$item[i], x, 0.32, 10, brand$white)
}
txt("Price change, September 2025 to September 2026. A 1–2% dip doesn't undo a 3–10% jump.",
    0.5, 0.25, 8.8, brand$white)

txt(paste0("Holiday sales are forecast to grow ", nrf_growth_2026, "%. With prices up ", formatC(cpi_yoy, format = "f", digits = 1), "%,"),
    0.5, 0.175, 10, brand$white, "bold")
txt("that's barely any more stuff in the cart.", 0.5, 0.15, 10, brand$white, "bold")
txt("Tip: use a price tracker to see if a \"deal\" really beats the usual price.", 0.5, 0.11, 9.5, brand$mint, "bold")

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: BLS Consumer Price Index (not seasonally adjusted); National Retail Federation forecast", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved black-friday-infographic.png")
