# ---------------------------------------------------------------------------
# SixtyEight95 brand images
#   profile.png  1080 x 1080  (Facebook profile picture; shows as a circle)
#   cover.png    1640 x 624   (Facebook cover photo)
#
# Run:  source("make_brand_images.R")   (saves next to this script)
# Change colors or text in the "settings" block and re-run.
# ---------------------------------------------------------------------------

library(ggplot2)
library(ragg)

# --- settings ---------------------------------------------------------------
background <- "#0F1B2D"   # dark navy
band_99    <- "#1F4E5A"   # outer band  (99.7%)
band_95    <- "#2A9D8F"   # middle band (95%)
band_68    <- "#7FE0CF"   # inner band  (68%)
curve_line <- "#FFFFFF"
accent     <- "#F4B942"   # amber
text_color <- "#FFFFFF"
font       <- "Bahnschrift"

tagline    <- "Real data on what life costs.\nSources shown, no spin."

out_dir <- if (exists("here", mode = "function")) getwd() else getwd()

# --- the bell curve ---------------------------------------------------------
curve <- data.frame(x = seq(-3.6, 3.6, length.out = 600))
curve$y <- dnorm(curve$x)

band <- function(sd) subset(curve, abs(x) <= sd)

bell_layers <- function(label_size = 0) {
  layers <- list(
    geom_area(data = band(3), aes(x, y), fill = band_99),
    geom_area(data = band(2), aes(x, y), fill = band_95),
    geom_area(data = band(1), aes(x, y), fill = band_68),
    geom_line(data = curve, aes(x, y), color = curve_line, linewidth = 1.6, lineend = "round"),
    geom_segment(aes(x = -3.6, xend = 3.6, y = 0, yend = 0), color = curve_line, linewidth = 1.2)
  )
  if (label_size > 0) {
    layers <- c(layers, list(
      annotate("text", x = 0,    y = 0.14, label = "68",   family = font, fontface = "bold",
               size = label_size,       color = background),
      annotate("text", x = 1.5,  y = 0.045, label = "95",  family = font, fontface = "bold",
               size = label_size * 0.6, color = background),
      annotate("text", x = -1.5, y = 0.045, label = "95",  family = font, fontface = "bold",
               size = label_size * 0.6, color = background),
      annotate("text", x = 2.6,  y = -0.022, label = "99.7", family = font,
               size = label_size * 0.4, color = band_68)
    ))
  }
  layers
}

blank_theme <- theme_void() +
  theme(plot.background = element_rect(fill = background, color = NA),
        panel.background = element_rect(fill = background, color = NA),
        plot.margin = margin(0, 0, 0, 0))

# --- profile picture (everything kept inside the center circle) -------------
profile <- ggplot() +
  bell_layers(label_size = 16) +
  annotate("text", x = 0, y = -0.095, label = "SIXTYEIGHT95", family = font, fontface = "bold",
           size = 13, color = text_color) +
  annotate("segment", x = -1.1, xend = 1.1, y = -0.145, yend = -0.145, color = accent, linewidth = 2) +
  coord_cartesian(xlim = c(-5.2, 5.2), ylim = c(-0.27, 0.56), expand = FALSE) +
  blank_theme

agg_png(file.path(out_dir, "profile.png"), width = 1080, height = 1080, res = 144)
print(profile)
dev.off()

# --- cover photo ------------------------------------------------------------
# Facebook overlays the profile picture at the bottom-left of the cover on
# desktop, so the main content sits center/right.
cover <- ggplot() +
  bell_layers(label_size = 13) +
  annotate("text", x = 4.6, y = 0.27, label = "SixtyEight95", family = font, fontface = "bold",
           size = 18, color = text_color, hjust = 0) +
  annotate("segment", x = 4.65, xend = 7.4, y = 0.205, yend = 0.205, color = accent, linewidth = 2.2) +
  annotate("text", x = 4.6, y = 0.15, label = tagline, family = font,
           size = 6.2, color = band_68, hjust = 0, vjust = 1, lineheight = 1.1) +
  coord_cartesian(xlim = c(-5.5, 15), ylim = c(-0.06, 0.46), expand = FALSE) +
  blank_theme

agg_png(file.path(out_dir, "cover.png"), width = 1640, height = 624, res = 144)
print(cover)
dev.off()

message("Saved profile.png and cover.png to ", out_dir)
