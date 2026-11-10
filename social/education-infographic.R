# ---------------------------------------------------------------------------
# Infographic: a year of college, in hours of work (1980 = 100)
#   education-infographic.png  1080 x 1350
#
# Run from this folder:  source("education-infographic.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/education_data.R")
library(ragg)
library(grid)

since_1980 <- growth_since(1980)
times <- function(x) paste0(formatC(x, format = "f", digits = 1), "×")

labels <- tribble(
  ~year,              ~hours,                      ~label,                                        ~vjust, ~hjust,
  1980,               100,                         "1980\n100 hours",                             1.4,    0.3,
  hours_peak$year,    hours_peak$hours_index,      paste0("Peak (", hours_peak$year, ")\n", round(hours_peak$hours_index), " hours"), -0.35, 0.5,
  year(latest_month), college_now$hours_index,     paste0("Today\n", round(college_now$hours_index), " hours"), 1.4, 0.8
)

chart <- ggplot(college_yearly, aes(year, hours_index)) +
  geom_area(fill = brand$band, alpha = 0.6) +
  geom_line(color = brand$amber, linewidth = 1.3) +
  geom_point(data = labels, aes(year, hours), color = brand$white, size = 3) +
  geom_text(data = labels, aes(year, hours, label = label, vjust = vjust, hjust = hjust),
            color = brand$white, family = brand$font, size = 4, lineheight = 0.9) +
  scale_y_continuous(limits = c(0, 450), breaks = seq(0, 400, 100), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1980, 2020, 10), expand = expansion(mult = c(0.01, 0.04))) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = brand$font, base_size = 14) +
  theme(plot.background  = element_rect(fill = brand$navy, color = NA),
        panel.background = element_rect(fill = brand$navy, color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = brand$grid),
        axis.text = element_text(color = brand$muted))

# --- words ------------------------------------------------------------------
headline <- "COLLEGE,\nIN HOURS OF WORK"
subhead  <- paste0("If a year of college took 100 hours of work to pay for in 1980,\n",
                   "here's how many hours the same year of college has taken since.")

tiles <- tribble(
  ~x,    ~value,                         ~label,                  ~color,
  0.22,  times(since_1980$tuition),      "College tuition",       brand$amber,
  0.5,   times(since_1980$wages),        "Hourly pay",            brand$white,
  0.78,  times(since_1980$prices),       "Everything else",       brand$white
)

twist_drop <- 1 - college_now$hours_index / hours_peak$hours_index
callout_t <- "THE TWIST"
callout_b <- paste0("Since ", hours_peak$year, ", paychecks have grown faster than tuition. A year of college now takes\n",
                    "about ", round(twist_drop * 100), "% fewer hours of work than at the peak. Still nearly ",
                    round(college_now$hours_index / 100), "× what it took in 1980.")

how_note <- paste0("College tuition and fees as tracked by the BLS consumer price index. What a given family pays after grants can be lower.\n",
                   "Pay = average hourly earnings before taxes. \"Today\" = average of the last 12 months.")
link_text <- "Full story: sixtyeight95.github.io"
source_tx <- "Data: U.S. Bureau of Labor Statistics (CPI tuition index, CPI, earnings)"

# --- draw the page ------------------------------------------------------------
txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color,
                      fontface = face, lineheight = 1))
}

agg_png("education-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage()
grid.rect(gp = gpar(fill = brand$navy, col = NA))

txt(headline, 0.5, 0.915, 26, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt(subhead, 0.5, 0.815, 11, brand$mint)

print(chart, vp = viewport(x = 0.5, y = 0.6, width = 0.92, height = 0.36))

txt("HOW MUCH BIGGER SINCE 1980", 0.5, 0.395, 10.5, brand$mint, "bold")
for (i in seq_len(nrow(tiles))) {
  txt(tiles$value[i], tiles$x[i], 0.35, 22, tiles$color[i], "bold")
  txt(tiles$label[i], tiles$x[i], 0.312, 10, brand$muted, "bold")
}

grid.rect(x = 0.5, y = 0.225, width = 0.88, height = 0.1, gp = gpar(fill = brand$band, col = NA))
txt(callout_t, 0.5, 0.255, 11, brand$amber, "bold")
txt(callout_b, 0.5, 0.215, 9.5, brand$white)

txt(how_note, 0.5, 0.13, 7.8, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt(link_text, 0.95, 0.048, 12, brand$mint, just = "right")
txt(source_tx, 0.5, 0.012, 7.5, brand$muted)
dev.off()

message("Saved education-infographic.png")
