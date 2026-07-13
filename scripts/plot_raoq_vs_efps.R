#### PLOT RaoQ vs single EFP

### Authors: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))

# Measure script run time
library(tictoc)
tic("Script run time")



### Options --------------------------------------------------------------------
# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F: ")) # ask if output should be saved
dat_in <- as.character(read.table("data/data_version.txt"))
vers_out <- dat_in

labelling <- F #as.logical(readline(prompt = "Plot point labels (can be visually overwhelming)? T/F: ")) # ask if plots should print point labels


plot_efp <- "GPPsat" # EFP to be plotted against Rao Q formulations



### Utilities ------------------------------------------------------------------
## Functions
source("scripts/functions/safe_load_packages.R")
# source("scripts/functions/min_max_norm.R")
# source("scripts/functions/plot_scatterplot_models.R")
# source("scripts/functions/plot_best.R")

## Packages
required_packages <- c(
  "RColorBrewer", # manipulate ggplot colors
  "dplyr",        # tidy data manipulation
  "ggplot2",      # tidy plot
  "ggrepel",      # repelled labels
  "glue",         # glue strings
  "patchwork",    # combine and arrange plots
  "readr",        # tidy read/save
  "rlang",        # quoting
  "scales",       # package for rescaling
  "tidyr"         # reorganize tibbles
)
safe_load_packages(required_packages)

## Other
source("scripts/utils/MyThemes.R")
source("scripts/utils/MyCols.R")
source("scripts/utils/MyPlotSpecs.R")
source("scripts/utils/units.R")



### Data -----------------------------------------------------------------------
dat <- read_csv(glue("data/inter/data4analysis_efps_{dat_in}.csv"), show_col_types = F) #%>% 
  # dplyr::filter(!IGBP %in% c("DBF", "MF")) %>% # test with/without DBF/MF
  # dplyr::filter(!SITE_ID %in% c("HEAL", "TEAK", "TOOL")) # test with/without outliers






### Process data ---------------------------------------------------------------
dat <- dat %>%
  mutate(
    GROUP = if_else(condition = IGBP %in% c("DBF", "MF"),
                    true = "Dense forests",
                    false = "Sparse vegetation"),
    .after = IGBP
  )



### Plot Function --------------------------------------------------------------
plot_function <- function(data = dat, x = "RaoQ_NIRv", y = plot_efp, labelling = labelling) {
  ## Quote
  predictor_sym <- sym(x)
  plot_sym <- sym(y)
  
  ## Data
  data <- data %>% select(!!plot_sym, !!predictor_sym, SITE_ID, IGBP) %>% drop_na(!!plot_sym, !!predictor_sym)
  
  ## Model and labels
  m1 <- data %>% lm(formula = glue("{y} ~ {x}"))
  r2 <- summary(m1)$r.squared %>% magrittr::multiply_by(100) %>% round(digits = 1) %>% format(nsmall = 1)
  labelcap <- bquote(
    R^2 ~ "=" ~ .(r2) ~ "%" ~
      # " Intercept =" ~ .(signif(m1$coef[[1]], 2)) ~
      # " Slope =" ~ .(signif(m1$coef[[2]], 2)) ~
      "  p =" ~ .(signif(summary(m1)$coef[2,4], 2)) ~
      "  n =" ~ .(nrow(drop_na(dat, !!plot_sym, !!predictor_sym)))
  )
  
  ## Plot
  p_out <- data %>%
    ggplot(aes(x = !!predictor_sym, y = !!plot_sym)) +
    # facet_wrap(. ~ IGBP, scales = "free_x") + theme_facets + # test by groups
    geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T, color = "gray25") +
    geom_point(aes(fill = IGBP), color = point_border_color, shape = point_shape, size = point_size_medium_small, na.rm = T) +
    scale_fill_manual(values = CatCol_igbp) +
    guides(fill = guide_legend(title = "IGBP class", override.aes = list(size = point_size_big))) +
    labs(caption = labelcap) +
    xlab(raoq_units[x]) + ylab(efp_units[y]) + # axis title
    theme_bw() +
    NULL
  
  # Optionally: add labels to points, controlled by "labelling" parameter at the start of the script
  if (labelling == T) {p_out <- p_out + geom_label_repel(aes(label = SITE_ID), alpha = 0.5, max.overlaps = 5, na.rm = T)}
  
  ## Output
  return(p_out)
}
# debugonce(plot_function)



### Plot Rao Q vs specified EFP ------------------------------------------------
## Bands ----
predictor <- "RaoQ_S2"
p_scatter1 <- plot_function(x = predictor, y = plot_efp, labelling = labelling)
p_scatter1


## NDVI ----
predictor <- "RaoQ_NDVI"
p_scatter2 <- plot_function(x = predictor, y = plot_efp, labelling = labelling)
p_scatter2


## NIRv ----
predictor <- "RaoQ_NIRv"
p_scatter3 <- plot_function(x = predictor, y = plot_efp, labelling = labelling)
p_scatter3



### Combine plots --------------------------------------------------------------
p_23 <- (p_scatter2 | p_scatter3) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "a") & theme_combine
# p_23

p_efp_raoq <- (p_scatter1 | p_scatter2 | p_scatter3) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "a") &
  theme_combine +
  theme_transparent + theme(legend.key.size = unit(0, "mm")) + # for poster
  NULL


# p_efp_satbiodiv <- (p_scatter1 | p_scatter2 | p_scatter3 | p_scatter4) +
#   plot_layout(guides = 'collect') +
#   plot_annotation(tag_levels = "a") &http://127.0.0.1:42187/graphics/5a6c3768-0fe4-465b-b287-0f1471c0cd11.png
#   theme_combine
# p_efp_satbiodiv



### Save -----------------------------------------------------------------------
if (savedata) {
  scal <- 20
  width <- 31.8 * scal
  height <- 6.1 * scal
  
  ## RaoQ_NIRv vs EFP
  ggsave(filename = glue("results/scatterplots/{plot_efp}-RaoQ_NIRv_{vers_out}.jpg"),
         plot = p_scatter3, device = "jpeg",
         width = 508, height = 285.75, units = "mm", dpi = 300)
  
  ## EFP-RaoQ scatterplots
  ggsave(filename = glue("results/scatterplots/{plot_efp}-RaoQ_all_{vers_out}.jpg"),
         plot = p_efp_raoq, device = "jpeg",
         width = 508, height = 285.75, units = "mm", dpi = 300)
  # transparent png
  ggsave(filename = glue("results/scatterplots/{plot_efp}-RaoQ_all_{vers_out}.png"),
         plot = p_efp_raoq, device = "png", bg = "transparent",
         width = width, height = height, units = "mm", dpi = 300)
  
  # ## EFP-RaoQ by groups
  # ggsave(filename = glue("results/scatterplots/{plot_efp}-raoQ_bands_byIGBP_{vers_out}.jpg"),
  #        plot = p_scatter1, device = "jpeg",
  #        width = 508, height = 285.75, units = "mm", dpi = 300)
  # ## EFP-raoQ by groups
  # ggsave(filename = glue("results/scatterplots/{plot_efp}-raoQ_NDVI_byIGBP_{vers_out}.jpg"),
  #        plot = p_scatter2, device = "jpeg",
  #        width = 508, height = 285.75, units = "mm", dpi = 300)
  # ## EFP-raoQ by groups
  # ggsave(filename = glue("results/scatterplots/{plot_efp}-raoQ_NIRv_byIGBP_{vers_out}.jpg"),
  #        plot = p_scatter3, device = "jpeg",
  #        width = 508, height = 285.75, units = "mm", dpi = 300)
}



# ### Plot relationships ---------------------------------------------------------
# ## Plotting function ----
# plot_bivariate <- function(
    #     data = dat,
#     X = x_names, # c("RaoQ_NIRv"), # vector of strings for predictor variables
#     Y = y_names, # c("GPPsat"), # vector of strings for predicted variables
#     Xlabs = x_labels, #c(RaoQ_NIRv = expression(paste(RaoQ[NIRv], " [-]"))), # vector of named expressions or strings for predictor labels
#     Ylabs = y_labels, #c(GPPsat = expression(paste(GPP[sat], " [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]"))), # vector of named expressions or strings for predicted labels
#     add_point_labs = F, # add site labels to plot? (likely overplotting)
#     savedata = savedata,
#     savename = "GPPsat-RaoQ",
#     savewidth = 10.66667 + 1,
#     saveheight = 6
# ) {
#   ## Legend bar range ----
#   size_names <- data %>%
#     select(all_of(X)) %>%
#     names() %>%
#     paste0("_cv")
#   
#   size_range <- data %>%
#     select(all_of(size_names)) %>%
#     range(na.rm = T)
#   
#   
#   ## Function for purrr::mapping the plots ----
#   mypurrrplot <- function(
    #     # parent arguments: data, Xlabs, Ylabs, size_range?
#     x = X[1], # default x
#     y = Y[1] # default y
#   ) {
#     ## Data
#     data <- data %>% drop_na(all_of(c(x, y)))
#     
#     
#     ## Quote
#     # Variable symbols
#     xsym <- sym(x)
#     ysym <- sym(y)
#     
#     sizesym <- sym(paste0(x, "_ccv"))
#     
#     # Axes labels
#     # extract corresponding x-y labels from vectors of labels
#     xlabel <- unname(Xlabs[x])
#     ylabel <- Ylabs[y]
#     
#     sizelabel <- expression(paste(RaoQ[CV]))
#     #bquote(paste("CV of ", .(xlabel[[1]]))) # combine a string prefix with an expression
#     
#     
#     ## Model
#     m1 <- data %>% lm(formula = paste0(x, " ~ ", y))
#     r2 <- summary(m1)$r.squared %>% magrittr::multiply_by(100) %>% round(digits = 1) %>% format(nsmall = 1)
#     labelcap <- bquote(
#       R^2 ~ "=" ~ .(r2) ~ "%" ~
#         # " Intercept =" ~ .(signif(m1$coef[[1]], 2)) ~
#         # " Slope =" ~ .(signif(m1$coef[[2]], 2)) ~
#         "  p =" ~ .(signif(summary(m1)$coef[2,4], 2)) ~
#         "  n =" ~ .(nrow(data))
#     )
#     
#     
#     ## Plot
#     p1 <- data %>%
#       ggplot(aes(x = !!xsym, y = !!ysym)) +
#       # facet_wrap(. ~ IGBP, scales = "free_x") + theme_facets + # test by groups
#       geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T, color = "gray25") +
#       geom_point(
#         aes(fill = IGBP, size = !!sizesym),
#         alpha = 0.9, color = point_border_color, shape = point_shape, na.rm = T
#       ) +
#       scale_fill_manual(values = CatCol_igbp) +
#       scale_size_continuous(
#         transform = "log",
#         limits = size_range,
#         breaks = c(25, 50, 100, 200),
#         labels = c("25%", "50%", "100%", "200%"),
#         range = c(point_size_very_small, point_size_medium)
#       ) +
#       guides(
#         fill = guide_legend(title = "IGBP", override.aes = list(size = point_size_medium)),
#         size = guide_legend(title = sizelabel, override.aes = list(color = NA, fill = "gray25", stroke = line_width_thick)),
#       ) +
#       labs(caption = labelcap) +
#       xlab(xlabel) + ylab(ylabel) + # axis title
#       theme_bw() +
#       NULL
#     
#     ## Add labels
#     if (add_point_labs) {
#       p1 <- p1 + ggrepel::geom_label_repel(aes(label = SITE_ID), alpha = 0.5, max.overlaps = 7.5, na.rm = T) # add site labels
#     }
#     
#     ## Output
#     return(p1)
#   }
#   # debugonce(mypurrrplot)
#   
#   
#   ## Create all combinations of xi and yj ----
#   combinations <- expand.grid(
#     x = X,
#     y = Y,
#     stringsAsFactors = FALSE
#   )
#   
#   # x*y list of plots for x-y pairs
#   p_list <- pmap(.l = combinations, .f = function(x, y) mypurrrplot(x, y))
#   
#   
#   
#   ### Combine plots ------------------------------------------------------------
#   ## Remove titles ----
#   ## Remove y-axis labels (keep only on left column)
#   p_list[!1:length(p_list) %in% seq(from = 1, to = length(p_list), by = length(X))] <- map(.x = p_list[!1:length(p_list) %in% seq(from = 1, to = length(p_list), by = length(X))], .f = ~rm_yaxis_title(p = .x))
#   # p_list[!1:length(p_list) %in% seq(from = 1, to = length(p_list), by = length(X))] <- map(.x = p_list[!1:length(p_list) %in% seq(from = 1, to = length(p_list), by = length(X))], .f = ~rm_legend_title(p = .x))
#   # remove by: index = complement of sequence of integers that appear at the start of each row
#   
#   
#   ## Remove x-axis labels & legends (keep only on bottom row)
#   p_list[!1:length(p_list) %in% (1+length(p_list)-length(X)):length(p_list)] <- map(.x = p_list[!1:length(p_list) %in% (1+length(p_list)-length(X)):length(p_list)], .f = ~rm_xaxis_title(p = .x))
#   # p_list[!1:length(p_list) %in% (1+length(p_list)-length(X)):length(p_list)] <- map(.x = p_list[!1:length(p_list) %in% (1+length(p_list)-length(X)):length(p_list)], .f = ~rm_legend(p = .x))
#   # remove by: index = !final row
#   
#   
#   ## Combine ----
#   p_out <- wrap_plots(p_list, ncol = length(X), nrow = length(Y)) +
#     plot_layout(guides = 'collect') +
#     plot_annotation(tag_levels = "a") &
#     theme(
#       # axis.text = element_text(size = rel(1)),
#       axis.title = element_text(face = "bold", size = rel(1.4)),
#       # legend.position = "bottom", legend.direction = "horizontal",
#       legend.text = element_text(size = rel(1.1)),
#       legend.title = element_text(size = rel(1.4)),
#       plot.caption = element_text(size = rel(1.1)),
#       plot.tag = element_text(face = "bold", size = rel(1.4)),
#       plot.margin = unit(c(0, 0, 0, 0), "cm"), # remove margins around individual plots
#       # strip.text = element_text(size = rel(1.5)), # facet title strips
#       # strip.background = element_rect(fill = "white")
#     ) +
#     theme_transparent +
#     NULL
#   
#   
#   
#   ### Save ---------------------------------------------------------------------
#   if (savedata) {
#     width <- savewidth
#     height <- saveheight
#     
#     ## JPEG file
#     ggsave(filename = glue::glue("results/scatterplots/{savename}_{vers_out}.jpg"),
#            plot = p_out, device = "jpeg",
#            width = width, height = height, dpi = 300)
#     # transparent png
#     ggsave(filename = glue::glue("results/scatterplots/{savename}_{vers_out}.png"),
#            plot = p_out, device = "png", bg = "transparent",
#            width = width, height = height, dpi = 300)
#   }
#   
#   
#   
#   ### Output -------------------------------------------------------------------
#   return(p_out)
# }
# debugonce(plot_bivariate)
# 
# 
# 
# ## Plot ----
# p1 <- plot_bivariate(
#   X = x_names, Y = c("GPPsat"),
#   Xlabs = x_labels, Ylabs = y_labels[names(y_labels) %in% c("GPPsat")],
#   savedata = T, savename = "GPPsat-RaoQ"
# )
# p1
# 
# p2 <- plot_bivariate(
#   X = x_names, Y = c("CUEeco", "GPPsat"),
#   Xlabs = x_labels, Ylabs = y_labels[names(y_labels) %in% c("CUEeco", "GPPsat")],
#   savedata = T, savename = "CUEeco+GPPsat-RaoQ", saveheight = 12
#   )
# p2
# 
# pall <- plot_bivariate(
#   X = x_names, Y = y_names,
#   Xlabs = x_labels, Ylabs = y_labels,
#   savedata = T, savename = "EFPs-RaoQ", savewidth = 16.5, saveheight = 21
# )
# pall
# 
# 
# 
### End ------------------------------------------------------------------------
print("End of script.")