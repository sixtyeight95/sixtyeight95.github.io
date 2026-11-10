# ---------------------------------------------------------------------------
# Infographic: a typical home's mortgage, in hours of work per month
#   housing-infographic.png  1080 x 1350
#
# Run from this folder:  source("housing-infographic.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/housing_data.R")
library(ragg)
library(grid)

hrs <- function(x) paste(round(x), "hrs")

y1985 <- year_row(1985)
y1990 <- year_row(1990)
y2021 <- year_row(2021)
y2023 <- year_row(2023)

# labeled moments on the chart
labels <- tribble(
  ~quarter,        ~hours,                  ~label,                                                ~vjust, ~hjust,
  peak$quarter,    peak$hours_per_month,    paste0(hrs(peak$hours_per_month), " (", year(peak$quarter), ")\nrates near ", round(peak$rate), "%"), -0.25, 0.5,
  low$quarter,     low$hours_per_month,     paste0(hrs(low$hours_per_month), " (", year(low$quarter), ")\nrates ", round(low$rate), "%"),  1.25, 0.5,
  latest_quarter,  housing_quarterly$hours_per_month[housing_quarterly$quarter == latest_quarter],
                   paste0("Today\n", hrs(housing_now$hours_per_month)), 1.4, 0.75
)

chart <- ggplot(housing_quarterly, aes(quarter, hours_per_month)) +
  geom_area(fill = brand$band, alpha = 0.6) +
  geom_line(color = brand$amber, linewidth = 1.2) +
  geom_point(data = labels, aes(quarter, hours), color = brand$white, size = 3) +
  geom_text(data = labels, aes(quarter, hours, label = label, vjust = vjust, hjust = hjust),
            color = brand$white, family = brand$font, size = 3.9, lineheight = 0.9) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 50),
                     labels = function(x) paste(x, "hrs"), expand = c(0, 0)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.06))) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = brand$font, base_size = 14) +
  theme(plot.background  = element_rect(fill = brand$navy, color = NA),
        panel.background = element_rect(fill = brand$navy, color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = brand$grid),
        axis.text = element_text(color = brand$muted))

# --- words ------------------------------------------------------------------
headline <- "BUYING A HOME,\nIN HOURS OF WORK"
subhead  <- paste0("How many hours a month the average worker has to work to cover the mortgage\n",
                   "on a typical newly built U.S. home. (A full-time month is about ", work_hours_month, " hours.)")

tiles <- tribble(
  ~x,   ~label, ~value,
  0.2,  "1990", hrs(y1990$hours_per_month),
  0.4,  "2021", hrs(y2021$hours_per_month),
  0.6,  "2023", hrs(y2023$hours_per_month),
  0.8,  "TODAY", hrs(housing_now$hours_per_month)
)

callout_t <- "THE CATCH"
callout_b <- paste0("Interest rates, more than prices, drive the swings in the monthly payment. But the house itself\n",
                    "costs more pay than in the '80s: a 10% down payment now equals ", round(housing_now$months_to_save_down, 1),
                    " months of full pay, vs ", round(y1985$months_to_save_down, 1), " in 1985.")

how_note <- paste0("Median price of new homes sold (", paste0("$", format(round(housing_now$price, -2), big.mark = ",")),
                   " today), 10% down, 30-year fixed. Principal and interest only, no taxes or insurance.\n",
                   "Pay = average hourly earnings before taxes, one earner (", dollars(housing_now$wage), "/hr today).")
link_text <- "Full story: sixtyeight95.github.io"
source_tx <- "Data: Census/HUD home prices, Freddie Mac mortgage rates, BLS earnings, via FRED"

# --- draw the page ------------------------------------------------------------
txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color,
                      fontface = face, lineheight = 1))
}

agg_png("housing-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage()
grid.rect(gp = gpar(fill = brand$navy, col = NA))

txt(headline, 0.5, 0.915, 26, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt(subhead, 0.5, 0.815, 11, brand$mint)

print(chart, vp = viewport(x = 0.5, y = 0.6, width = 0.92, height = 0.36))

for (i in seq_len(nrow(tiles))) {
  is_today <- tiles$label[i] == "TODAY"
  txt(tiles$value[i], tiles$x[i], 0.375, 20, if (is_today) brand$amber else brand$white, "bold")
  txt(tiles$label[i], tiles$x[i], 0.34, 10, brand$muted, "bold")
}

grid.rect(x = 0.5, y = 0.24, width = 0.88, height = 0.1, gp = gpar(fill = brand$band, col = NA))
txt(callout_t, 0.5, 0.27, 11, brand$amber, "bold")
txt(callout_b, 0.5, 0.23, 9.5, brand$white)

txt(how_note, 0.5, 0.135, 7.8, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt(link_text, 0.95, 0.048, 12, brand$mint, just = "right")
txt(source_tx, 0.5, 0.012, 7.5, brand$muted)
dev.off()

message("Saved housing-infographic.png")
