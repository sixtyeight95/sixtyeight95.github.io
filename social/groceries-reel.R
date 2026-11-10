# ---------------------------------------------------------------------------
# Reel (vertical video) for the groceries post
#   groceries-reel.mp4   1080 x 1920, 24 frames per second, about 25 seconds
#
# Run from this folder:  source("groceries-reel.R")
# How it works: draws every frame as a PNG, then stitches them into a video.
# To change the story, edit the "scenes" (start/end times in seconds).
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/groceries_data.R")
library(ragg)
library(grid)
library(av)

fps      <- 24
duration <- 26           # seconds
frames_dir <- file.path(tempdir(), "groceries_reel_frames")
unlink(frames_dir, recursive = TRUE)
dir.create(frames_dir)

# --- numbers ------------------------------------------------------------------
pct <- function(x) paste0(round(x * 100), "%")
show_items <- c("Coffee (lb)", "Ground beef (lb)", "Eggs (dozen)", "Milk (gallon)",
                "Whole chicken (lb)", "White bread (lb)", "Bacon (lb)", "Cheddar cheese (lb)")
items <- grocery_summary %>%
  filter(item %in% show_items) %>%
  arrange(desc(minutes_change_2019)) %>%
  mutate(order = row_number(),
         color = case_when(minutes_change_2019 >  0.05 ~ brand$red,
                           minutes_change_2019 < -0.05 ~ brand$mint,
                           TRUE                        ~ brand$muted),
         label = paste0(ifelse(minutes_change_2019 > 0, "+", ""), round(minutes_change_2019 * 100), "%"))

wage_growth  <- wage_now / wage_2019 - 1
price_lo     <- min(grocery_summary$price_change_2019)
price_hi     <- max(grocery_summary$price_change_2019)
since_1985   <- grocery_summary %>% filter(!is.na(minutes_1985))
cheaper_1985 <- sum(since_1985$minutes_change_1985 < 0)

# --- helpers --------------------------------------------------------------------
ease   <- function(x) { x <- pmin(pmax(x, 0), 1); x * x * (3 - 2 * x) }   # smooth 0 -> 1
fade   <- function(t, start, end, len = 0.4) ease((t - start) / len) * ease((end - t) / len)
appear <- function(t, start, len = 0.5) ease((t - start) / len)

txt <- function(label, x, y, size, color = brand$white, face = "plain", alpha = 1, just = "center") {
  if (alpha <= 0) return(invisible())
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color,
                      fontface = face, alpha = alpha, lineheight = 1.05))
}

# --- one frame ------------------------------------------------------------------
draw_frame <- function(t) {
  grid.newpage()
  grid.rect(gp = gpar(fill = brand$navy, col = NA))

  # brand bar at the top the whole time
  txt("SixtyEight95", 0.5, 0.94, 22, brand$mint, "bold")

  # Scene 1: the hook (0 - 3.5s)
  a <- fade(t, 0, 3.5)
  txt("Groceries feel\ninsanely expensive.", 0.5, 0.6, 46, brand$white, "bold", a)
  txt("But are they?", 0.5, 0.45, 40, brand$amber, "bold", a * appear(t, 1.6))

  # Scene 2: the idea (3.5 - 7.5s)
  a <- fade(t, 3.5, 7.5)
  txt("Forget dollars.", 0.5, 0.62, 40, brand$white, "bold", a)
  txt("How many MINUTES\nOF WORK does it take\nto buy each one?", 0.5, 0.47, 38, brand$mint, "bold",
      a * appear(t, 4.6))

  # Scene 3: prices vs paychecks (7.5 - 11.5s)
  a <- fade(t, 7.5, 11.5)
  txt("SINCE 2019", 0.5, 0.7, 30, brand$muted, "bold", a)
  txt(paste0("Prices up ", pct(price_lo), "-", pct(price_hi)), 0.5, 0.6, 40, brand$red, "bold", a)
  txt(paste0("Paychecks up ", pct(wage_growth)), 0.5, 0.5, 40, brand$mint, "bold", a * appear(t, 8.7))
  txt("So what actually got\nharder to afford?", 0.5, 0.37, 32, brand$white, "plain", a * appear(t, 9.8))

  # Scene 4: the arrows (11.5 - 20s)
  a <- fade(t, 11.5, 20)
  if (a > 0) {
    txt("MINUTES OF WORK TO BUY ONE", 0.5, 0.82, 30, brand$white, "bold", a)
    txt("white dot = 2019    arrow = today", 0.5, 0.785, 22, brand$muted, "plain", a)
    x_of <- function(m) 0.44 + m / 20 * 0.4          # minutes -> page position
    for (i in seq_len(nrow(items))) {
      row <- items[i, ]
      y   <- 0.72 - (i - 1) * 0.062
      p   <- appear(t, 12 + (i - 1) * 0.55, 0.6)     # this row's progress
      txt(sub(" \\(.*\\)", "", row$item), 0.39, y, 26, brand$white, "plain", a, just = "right")
      x0 <- x_of(row$minutes_2019)
      x1 <- x0 + (x_of(row$minutes_now) - x0) * p
      if (p > 0) {
        if (abs(x1 - x0) > 0.012) {
          grid.segments(x0, y, x1, y,
                        arrow = arrow(length = unit(0.022, "npc"), type = "closed"),
                        gp = gpar(col = row$color, fill = row$color, lwd = 9, alpha = a, lineend = "butt"))
        }
        grid.circle(x0, y, r = unit(0.009, "npc"), gp = gpar(fill = brand$white, col = NA, alpha = a))
        txt(row$label, max(x0, x1) + 0.025, y, 24, row$color, "bold", a * p, just = "left")
      }
    }
    txt("Coffee, beef and eggs really got worse.\nMost of the cart? Same or better.", 0.5, 0.17, 26,
        brand$amber, "bold", a * appear(t, 17))
  }

  # Scene 5: the long view (20 - 23.5s)
  a <- fade(t, 20, 23.5)
  txt("And compared with 1985...", 0.5, 0.66, 34, brand$muted, "bold", a)
  txt(paste0(cheaper_1985, " of ", nrow(since_1985), " groceries"), 0.5, 0.56, 50, brand$mint, "bold", a)
  txt("take LESS work to buy today.", 0.5, 0.48, 34, brand$white, "bold", a)
  txt("The one exception: beef.", 0.5, 0.38, 34, brand$red, "bold", a * appear(t, 21.5))

  # Scene 6: end card (23.5 - 26s)
  a <- appear(t, 23.5)
  txt("Follow SixtyEight95", 0.5, 0.6, 44, brand$white, "bold", a)
  txt("Real data on what life costs.", 0.5, 0.52, 30, brand$mint, "plain", a)
  txt("Full story: link in caption", 0.5, 0.44, 28, brand$amber, "bold", a)

  # source line the whole time
  txt("Data: U.S. Bureau of Labor Statistics", 0.5, 0.05, 18, brand$muted)
}

# --- render all frames, then the video ------------------------------------------------
n_frames <- duration * fps
files <- file.path(frames_dir, sprintf("frame_%04d.png", seq_len(n_frames)))
for (i in seq_len(n_frames)) {
  agg_png(files[i], width = 1080, height = 1920, res = 144)
  draw_frame((i - 1) / fps)
  dev.off()
}

av_encode_video(files, output = "groceries-reel.mp4", framerate = fps, verbose = FALSE)
message("Saved groceries-reel.mp4")
