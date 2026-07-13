#### FUNCTIONS TO REMOVE TITLES FROM PLOTS


## Plot title ----
rm_plot_title <- function(p = p_map) {
  p <- p + theme(plot.title = element_blank()) # remove plot title
  return(p)
}


## X-axis title ----
rm_xaxis_title <- function(p = p_map) {
  p <- p + theme(axis.title.x = element_blank()) # remove x axis title
  return(p)
}


## Y-axis title ----
rm_yaxis_title <- function(p = p_map) {
  p <- p + theme(axis.title.y = element_blank()) # remove y axis title
  return(p)
}


## Legend title ----
rm_legend_title <- function(p = p_map) {
  p <- p + theme(legend.title = element_blank()) # remove legend title
  return(p)
}

## Full legend ----
rm_legend <- function(p = p_map) {
  p <- p + theme(legend.position = "none") # completely remove legend
  return(p)
}