# ---------------------------------------------------------------------------
# Helpers for making Reels (vertical videos) frame by frame.
#
#   fade(t, start, end)   1 while a scene is showing, fading in/out at the edges
#   appear(t, start)      0 -> 1 starting at `start` (for things that pop in)
#   txt(...)              centered text in the brand font
#   draw_line_chart(...)  a line chart that "draws itself" as progress goes 0 -> 1
#   render_reel(draw_frame, "file.mp4", seconds)
# ---------------------------------------------------------------------------
library(ragg)
library(grid)
library(av)

ease   <- function(x) { x <- pmin(pmax(x, 0), 1); x * x * (3 - 2 * x) }
fade   <- function(t, start, end, len = 0.4) ease((t - start) / len) * ease((end - t) / len)
appear <- function(t, start, len = 0.5) ease((t - start) / len)

txt <- function(label, x, y, size, color = brand$white, face = "plain", alpha = 1, just = "center") {
  if (alpha <= 0) return(invisible())
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color,
                      fontface = face, alpha = alpha, lineheight = 1.05))
}

background <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = brand$navy, col = NA))
  txt("SixtyEight95", 0.5, 0.94, 22, brand$mint, "bold")
}

end_card <- function(t, start) {
  a <- appear(t, start)
  txt("Follow SixtyEight95", 0.5, 0.6, 44, brand$white, "bold", a)
  txt("Real data on what life costs.", 0.5, 0.52, 30, brand$mint, "plain", a)
  txt("Full story: link in caption", 0.5, 0.44, 28, brand$amber, "bold", a)
}

# x, y: numeric vectors. progress: 0 -> 1 how much of the line is drawn.
# box: where the chart sits on the page (x, y = center; w, h = size, all 0-1).
draw_line_chart <- function(x, y, progress, alpha, box, y_max, y_breaks, y_label,
                            x_breaks, color = brand$amber, axes = TRUE, lwd = 7) {
  if (alpha <= 0) return(invisible())
  pushViewport(viewport(x = box$x, y = box$y, width = box$w, height = box$h,
                        xscale = range(x), yscale = c(0, y_max)))
  if (!axes) { y_breaks <- c(); x_breaks <- c() }   # axes = FALSE: just add another line
  for (b in y_breaks) {
    grid.lines(unit(range(x), "native"), unit(c(b, b), "native"),
               gp = gpar(col = brand$grid, lwd = 2, alpha = alpha))
    grid.text(y_label(b), unit(-0.02, "npc"), unit(b, "native"), just = "right",
              gp = gpar(fontfamily = brand$font, fontsize = 20, col = brand$muted, alpha = alpha))
  }
  for (b in x_breaks) {
    grid.text(b, unit(b, "native"), unit(-0.04, "npc"),
              gp = gpar(fontfamily = brand$font, fontsize = 20, col = brand$muted, alpha = alpha))
  }
  n_show <- max(2, round(length(x) * ease(progress)))
  grid.lines(unit(x[1:n_show], "native"), unit(y[1:n_show], "native"),
             gp = gpar(col = color, lwd = lwd, alpha = alpha, linejoin = "round"))
  popViewport()
}

# point on the chart -> position on the page (to place labels)
chart_point <- function(xv, yv, x, y_max, box) {
  list(x = box$x - box$w / 2 + (xv - min(x)) / diff(range(x)) * box$w,
       y = box$y - box$h / 2 + yv / y_max * box$h)
}

render_reel <- function(draw_frame, output, seconds, fps = 24) {
  frames_dir <- file.path(tempdir(), paste0("frames_", tools::file_path_sans_ext(output)))
  unlink(frames_dir, recursive = TRUE)
  dir.create(frames_dir)
  files <- file.path(frames_dir, sprintf("frame_%04d.png", seq_len(seconds * fps)))
  for (i in seq_along(files)) {
    agg_png(files[i], width = 1080, height = 1920, res = 144)
    draw_frame((i - 1) / fps)
    dev.off()
  }
  av_encode_video(files, output = output, framerate = fps, verbose = FALSE)
  message("Saved ", output)
}
