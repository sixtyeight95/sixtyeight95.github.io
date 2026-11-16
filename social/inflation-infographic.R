# Infographic: Is inflation back?  (inflation-infographic.png, 1080 x 1350)
# Run from this folder:  source("inflation-infographic.R")
source("../R/fred.R")
source("../R/inflation_data.R")
library(ragg)
library(grid)

pct <- function(x) paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = 1), "%")
recent <- inflation %>% filter(date >= as.Date("2021-01-01")) %>%
  select(date, `All prices` = all, `Without food & energy` = core) %>%
  pivot_longer(-date, names_to = "series", values_to = "rate")

chart <- ggplot(recent, aes(date, rate, color = series)) +
  geom_hline(yintercept = 2, linetype = "dashed", color = brand$muted, linewidth = 0.5) +
  annotate("text", x = as.Date("2024-01-01"), y = 1.55, label = "Fed's 2% goal", hjust = 0,
           color = brand$muted, family = brand$font, size = 3.6) +
  geom_vline(xintercept = iran_strike, color = brand$amber, linewidth = 0.5, linetype = "dotted") +
  annotate("text", x = iran_strike, y = 8.6, label = "Iran strike\n(Feb 2026)", hjust = 1.05,
           color = brand$amber, family = brand$font, size = 3.6, lineheight = 0.9) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("All prices" = brand$red, "Without food & energy" = brand$mint), guide = "none") +
  scale_y_continuous(limits = c(0, 9.5), breaks = seq(0, 9, 3), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
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

agg_png("inflation-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("IS INFLATION BACK?", 0.5, 0.925, 27, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.885, 0.885), gp = gpar(col = brand$amber, lwd = 4))
txt(paste0("Prices are up ", formatC(infl_now$all, format = "f", digits = 1), "% from a year ago (", month_label(infl_now$date),
           "), up from ", formatC(infl_low$all, format = "f", digits = 1), "% in ", format(infl_low$date, "%B"), "."),
    0.5, 0.853, 10.5, brand$mint)
txt("All prices", 0.42, 0.82, 12, brand$red, "bold", just = "right")
txt("Without food & energy", 0.58, 0.82, 12, brand$mint, "bold", just = "left")

print(chart, vp = viewport(x = 0.5, y = 0.635, width = 0.92, height = 0.34))

txt("WHAT'S DRIVING IT (CHANGE FROM A YEAR AGO)", 0.5, 0.435, 11, brand$amber, "bold")
tiles <- tribble(~x, ~value, ~label,
  0.14, infl_now$gasoline, "Gasoline", 0.38, infl_now$energy, "All energy",
  0.62, infl_now$food, "Food", 0.86, infl_now$shelter, "Housing costs")
for (i in 1:4) {
  txt(pct(tiles$value[i]), tiles$x[i], 0.39, 21, ifelse(tiles$value[i] > 10, brand$red, brand$white), "bold")
  txt(tiles$label[i], tiles$x[i], 0.355, 10, brand$muted, "bold")
}

grid.rect(x = 0.5, y = 0.245, width = 0.88, height = 0.12, gp = gpar(fill = brand$band, col = NA))
txt("WHAT IT MEANS FOR YOU", 0.5, 0.285, 11, brand$amber, "bold")
txt(paste0("Pay is up ", formatC(infl_now$pay, format = "f", digits = 1), "%, prices ", formatC(infl_now$all, format = "f", digits = 1),
           "%. For the first time since early 2023, the average paycheck\n",
           "buys less than a year ago. The Fed has started raising rates again (",
           formatC(fed_low$rate, format = "f", digits = 2), "% → ", formatC(fed_now$rate, format = "f", digits = 2), "%),\n",
           "which means pricier loans and credit cards."), 0.5, 0.235, 9.3, brand$white)

txt(paste0("Consumer Price Index, all urban consumers, and CPI less food and energy. Pay = average hourly earnings before taxes.\n",
           "Wholesale (producer) prices are up ", formatC(infl_now$wholesale, format = "f", digits = 1),
           "%, which often shows up in store prices later."), 0.5, 0.135, 7.2, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: U.S. Bureau of Labor Statistics, Federal Reserve, via FRED", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved inflation-infographic.png")
