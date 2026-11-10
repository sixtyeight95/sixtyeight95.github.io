# ---------------------------------------------------------------------------
# Infographic: groceries in minutes of work (2019 vs now)
#   groceries-infographic.png  1080 x 1350  (Facebook / Instagram portrait)
#
# Run from this folder:  source("groceries-infographic.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/groceries_data.R")
library(ragg)
library(grid)

# items to show (keep it to about 9 so it reads on a phone)
show_items <- c("Coffee (lb)", "Ground beef (lb)", "Eggs (dozen)", "White bread (lb)", "Milk (gallon)",
                "Whole chicken (lb)", "Bacon (lb)", "Bananas (lb)", "Cheddar cheese (lb)")

chart_data <- grocery_summary %>%
  filter(item %in% show_items) %>%
  mutate(direction = ifelse(minutes_change_2019 > 0.05, "harder",
                            ifelse(minutes_change_2019 < -0.05, "easier", "same")),
         item = fct_reorder(item, minutes_change_2019),
         change_label = paste0(ifelse(minutes_change_2019 > 0, "+", ""),
                               round(minutes_change_2019 * 100), "%"))

colors <- c(harder = brand$red, same = brand$muted, easier = brand$mint)

# --- the chart: an arrow per item from 2019 to now ---------------------------
chart <- ggplot(chart_data, aes(y = item)) +
  geom_segment(aes(x = minutes_2019, xend = minutes_now, yend = item, color = direction),
               linewidth = 1.6, arrow = arrow(length = unit(0.16, "cm"), type = "closed")) +
  geom_point(aes(x = minutes_2019), color = brand$white, size = 2.4) +
  geom_text(aes(x = pmax(minutes_2019, minutes_now) + 0.6, label = change_label, color = direction),
            hjust = 0, family = brand$font, fontface = "bold", size = 4.6) +
  scale_color_manual(values = colors, guide = "none") +
  scale_x_continuous(limits = c(0, 21), breaks = seq(0, 20, 5),
                     labels = function(x) paste(x, "min")) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = brand$font, base_size = 14) +
  theme(plot.background  = element_rect(fill = brand$navy, color = NA),
        panel.background = element_rect(fill = brand$navy, color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = brand$grid),
        axis.text.y = element_text(color = brand$white, size = 14),
        axis.text.x = element_text(color = brand$muted))

# --- words ------------------------------------------------------------------
pct <- function(x) paste0(round(x * 100), "%")

price_range <- chart_data %>% summarise(lo = min(price_change_2019), hi = max(price_change_2019))
wage_growth <- wage_now / wage_2019 - 1

since_1985 <- grocery_summary %>% filter(!is.na(minutes_1985))
cheaper_1985 <- sum(since_1985$minutes_change_1985 < 0)

headline <- "YOUR GROCERIES,\nIN MINUTES OF WORK"
subhead  <- paste0("Prices are up ", pct(price_range$lo), " to ", pct(price_range$hi),
                   " since 2019. But paychecks are up ", pct(wage_growth), " too.\n",
                   "So we asked: how long do you have to work to buy each one?")
legend_t <- "Dot = 2019    Arrow tip = today"

callout_t <- "THE LONG VIEW"
callout_b <- paste0("Compared with 1985, ", cheaper_1985, " of the ", nrow(since_1985),
                    " groceries we track take LESS work to buy today.\n",
                    "The one exception: ground beef, near its highest level since records began in 1984.")

how_note <- paste0("Minutes of work = price ÷ average hourly pay before taxes (",
                   dollars(wage_2019), " in 2019, ", dollars(wage_now), " now),\n",
                   "for the ~80% of private-sector workers who aren't managers. ",
                   "\"Today\" = average of the last 12 months.")
link_text <- "Full story: sixtyeight95.github.io"
source_tx <- "Data: U.S. Bureau of Labor Statistics average prices and earnings, via FRED"

# --- draw the page ------------------------------------------------------------
txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color,
                      fontface = face, lineheight = 1))
}

agg_png("groceries-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage()
grid.rect(gp = gpar(fill = brand$navy, col = NA))

txt(headline, 0.5, 0.915, 26, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt(subhead, 0.5, 0.81, 10.5, brand$mint)

txt("Got harder", 0.3, 0.763, 11, brand$red, "bold")
txt("About the same", 0.5, 0.763, 11, brand$muted, "bold")
txt("Got easier", 0.7, 0.763, 11, brand$mint, "bold")
txt(legend_t, 0.5, 0.737, 9.5, brand$muted)

print(chart, vp = viewport(x = 0.5, y = 0.5, width = 0.92, height = 0.45))

grid.rect(x = 0.5, y = 0.21, width = 0.86, height = 0.1,
          gp = gpar(fill = brand$band, col = NA))
txt(callout_t, 0.5, 0.24, 11, brand$amber, "bold")
txt(callout_b, 0.5, 0.2, 10, brand$white)

txt(how_note, 0.5, 0.122, 8, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt(link_text, 0.95, 0.048, 12, brand$mint, just = "right")
txt(source_tx, 0.5, 0.012, 7.5, brand$muted)
dev.off()

message("Saved groceries-infographic.png")
