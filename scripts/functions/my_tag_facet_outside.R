## Modified from tag_facet_outside function in egg package v0.4.5
my_tag_facet_outside <- function(
    p, open = c("(", ""), close = c(")", "."), tag_fun_top = function(i) letters[i], 
    tag_fun_right = utils::as.roman, rm_strip_titles = F, legend_tag = F,
    x = c(0, 0), y = c(0.5, 1), hjust = c(0, 0), vjust = c(0.5, 1),
    fontface = c(1, 1), family = "", draw = TRUE, ...
    )
{
  if (rm_strip_titles) {
    p <- p + theme(
      strip.text = element_blank(),
      strip.background = element_blank())
  }
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags_top <- paste0(open[1], tag_fun_top(unique(lay$COL)), 
                     close[1])
  if (legend_tag == F | p$theme$legend.position == "none") {
    tags_right <- ""
  } else {
    tags_right <- paste0(
      open[2], tag_fun_right(unique(lay$ROW)), close[2]
      )
  }
  
  tl <- lapply(tags_top, grid::textGrob, x = x[1], y = y[1], 
               hjust = hjust[1], vjust = vjust[1], gp = grid::gpar(fontface = fontface[1], 
                                                                   fontfamily = family, ...))
  rl <- lapply(tags_right, grid::textGrob, x = x[2], y = y[2], 
               hjust = hjust[2], vjust = vjust[2], gp = grid::gpar(fontface = fontface[2], 
                                                                   fontfamily = family, ...))
  g <- ggplot_gtable(gb)
  g <- gtable::gtable_add_rows(g, grid::unit(1, "line"), pos = 0)
  l <- unique(g$layout[grepl("panel", g$layout$name), "l"])
  g <- gtable::gtable_add_grob(g, grobs = tl, t = 1, l = l)
  wm <- do.call(grid::unit.pmax, lapply(rl, grid::grobWidth))
  g <- gtable::gtable_add_cols(g, wm, pos = max(l))
  t <- unique(g$layout[grepl("panel", g$layout$name), "t"])
  g <- gtable::gtable_add_grob(g, grobs = rl, t = t, l = max(l) + 1)
  g <- gtable::gtable_add_cols(g, unit(2, "mm"), pos = max(l))
  if (draw) {
    grid::grid.newpage()
    grid::grid.draw(g)
  }
  return(g)
}
# debugonce(my_tag_facet_outside)