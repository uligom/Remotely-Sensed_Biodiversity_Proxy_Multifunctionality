### CALCULATE MULTIFUNCTIONALITY ###

### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Version History ------------------------------------------------------------
# v00, 17.05.2024:    Multifunctionality from average and threshold approach.



### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))

## Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F: ")) # ask if output should be saved
efp_in <- as.character(read.table("data/efp_version.txt"))
if (savedata) {
  vers_out <- efp_in
  cat(paste0(vers_out, "\n"), file = "data/emf_version.txt")
}



### Utilities ------------------------------------------------------------------
## Functions
source("scripts/functions/safe_load_packages.R")
source("scripts/functions/min_max_norm.R")
source("scripts/functions/my_hcut.R")
source("scripts/functions/rm_plot_titles.R")

## Packages
required_packages <- c(
  "dplyr",        # tidy data manipulation
  "dendextend",   # cluster analyses
  "factoextra",   # plot multivariate analyses
  "ggplot2",      # tidy plots
  "glue",         # glue strings
  # "ggradar",      # radar plots
  "gridExtra",    # grid graphics
  "patchwork",    # combine plots
  "purrr",        # map functions
  "readr",        # read csv files
  "tidyr"         # clean and reshape tidy data
)
safe_load_packages(required_packages)


## Other
source("scripts/utils/MyCols.R")
source("scripts/utils/MyPlotSpecs.R")
source("scripts/utils/MyThemes.R")



### Data -----------------------------------------------------------------------
## EFPs (functions):
load(glue("data/inter/efps_names_{efp_in}.RData")); efps_names <- efps_names[efps_names != "NEP99"]

## Dataset of EFPs & predictors ----
dat <- read_csv(glue::glue("data/inter/data_efps_clim_{efp_in}.csv"), show_col_types = F) %>% # EFPs + climate
  select(SITE_ID, all_of(efps_names))

## Vector names
load(file = glue::glue("data/inter/efps_names_{efp_in}.RData")) # efps_names


## Correlation matrix ----
methodo <- "kendall"
text_color <- "gray25"
corr_mat <- dat %>%
  dplyr::select(all_of(efps_names)) %>% # EFPs
  mutate(across(.cols = where(is.double), .fns = min_max_norm)) %>% # normalize
  cor(use = "complete.obs", method = methodo)

corr_df <- corr_mat %>% as_tibble() %>% 
  mutate(
    across(.cols = everything(), .fns = ~if_else(.x == 1, NA_real_, .x)),
    rownames = names(.)
  ) %>% 
  pivot_longer(cols = !rownames, names_to = "colnames", values_to = "corr")

corr_df %>% filter(corr == max(corr, na.rm = T))

# Plot
p_corr <- corr_mat %>%
  ggcorrplot::ggcorrplot(
    method = "square", show.diag = F,
    type = "full", colors = RColorBrewer::brewer.pal(5, "RdBu")[c(1, 3, 5)],
    # p.mat = NULL, sig.level = 0.05, insig = "pch", # for p-values (need to feed matrix of p-values to 'p.mat')
    lab = T, lab_col = text_color, lab_size = rel(3)
  ) +
  # Visuals
  theme_bw() +
  theme(
    axis.text  = element_text(color = text_color, size = rel(1.25)),
    axis.text.x = element_text(angle = 90, hjust = 0),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.key.height = unit(1, "null"), # define legend bar height relative to panel height
    legend.key.width = unit(1, "cm"),
    legend.text  = element_text(color = text_color, size = rel(1.25)),
    legend.title = element_blank()
  ) +
  # theme( # transparent background
  #   panel.background  = element_rect(fill = 'transparent'), # transparent panel bg
  #   plot.background   = element_rect(fill = 'transparent', color = NA), # transparent plot bg
  #   # panel.grid.major = element_blank(), # remove major gridlines
  #   panel.grid.minor  = element_blank(), # remove minor gridlines
  #   legend.background = element_rect(fill = 'transparent'), # transparent legend bg
  #   legend.box.background = element_rect(colour = 'transparent', fill = 'transparent') # transparent legend panel and legend box
  # ) +
  NULL
p_corr

if (savedata) {
  ggsave(glue::glue("/EFPs_{methodo}_{vers_out}.jpg"),
         plot = p_corr, device = "jpeg", path = "results/metrics_corr",
         width = 8, height = 6, dpi = 300)
}


### Processing -----------------------------------------------------------------
## Normalize ----
# Important to compare functions in order to 1) build clusters, 2) calculate multifunctionality
# Should be between 0 and 1
dat_norm <- dat %>%
  # group_by(IGBP) %>% # normalize within biomes based on Manning et al., 2018
  mutate(across(.cols = where(is.double), .fns = min_max_norm)) #%>% # ignore warning of Inf output since it already gives NA
  # ungroup()


## Agglomerative cluster analysis ----
dat_mat <- dat_norm %>% select(-SITE_ID, -contains("IGBP")) %>% t()
X <- dist(dat_mat, method = "euclidean")


## Plot specs ----
line_width <- 2
common_theme <- theme_bw() + theme_combine


## Elbow plot to determine number of clusters ----
# DECREASE in total within sum of squares should be MINIMIZED after best number of clusters (inflection point)
p_elbow <- fviz_nbclust(
  x = dat_mat, FUNcluster = my_hcut, method = "wss", k.max = nrow(dat_mat)-1 # maximum number of clusters = n – 1
  ) +
  geom_line(aes(group = 1), linewidth = line_width, color = "steelblue") + 
  geom_point(group = 1, size = 5, color = "steelblue") +
  common_theme +
  theme(title = element_blank())
print(p_elbow)

# Save plot
if (savedata) {
  ggplot2::ggsave(filename = glue::glue("results/cluster/inflection_{vers_out}.jpg"), plot = p_elbow, device = "jpeg",
                  width = 400, height = 300, units = "mm", dpi = 300)
}


## Silhouette plot to determine number of clusters ----
# Average silhouette width should be MAXIMIZED at best number of clusters
p_sil <- fviz_nbclust(
  x = dat_mat, FUNcluster = my_hcut, method = "silhouette", k.max = nrow(dat_mat)-1, linecolor = NA
) +
  geom_line(aes(group = 1), linewidth = line_width, color = "steelblue", na.rm = T) + 
  geom_vline(xintercept = NULL, linetype = 2) +
  geom_point(group = 1, size = 5, color = "steelblue", na.rm = T) +
  common_theme +
  theme(title = element_blank())
print(p_sil)

# Save plot
if (savedata) {
  ggplot2::ggsave(filename = glue::glue("results/cluster/silhouette_{vers_out}.jpg"), plot = p_sil, device = "jpeg",
                  width = 400, height = 300, units = "mm", dpi = 300)
}


## Plot Dendogram ----
K <- 8; my_palette <- Eight_categorical2

p_dendr <- eclust(dat_mat, FUN = "hclust", k = K, graph = T, hc_metric = "euclidean", hc_method = "complete") %>% 
  fviz_dend(
    k_colors = "black", # color of branches
    #lwd = line_width # line width of branches ==> NOT WORKING AS OF factoextra 2.0.0
    color_labels_by_k = F, cex = 1.2, # color and size of labels
    rect = T, rect_border = my_palette, rect_fill = T, rect_lty = "blank" # boxes around clusters
    ) +
  xlab("Ecosystem Functional Properties") +
  common_theme +
  theme(
    # axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    title = element_blank()
    )
print(p_dendr)

# Save plot
if (savedata) {
  ggplot2::ggsave(filename = glue::glue("results/cluster/dendrogram_{vers_out}.jpg"), plot = p_dendr, device = "jpeg",
                  width = 400, height = 300, units = "mm", dpi = 300)
}


## Combine plots ----
p_elbow <- rm_xaxis_title(p_elbow) # remove x-axis title

p_hca <- (p_dendr | (p_elbow / p_sil)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = text_size_big))
print(p_hca)

# Save plot
if (savedata) {
  ggplot2::ggsave(filename = glue::glue("results/cluster/hierarchical_clustering_{vers_out}.jpg"), plot = p_hca, device = "jpeg",
                  width = 400, height = 300, units = "mm", dpi = 150)
  ggplot2::ggsave(filename = glue::glue("results/cluster/hierarchical_clustering_{vers_out}.pdf"), plot = p_hca, device = "pdf",
                  width = 400, height = 300, units = "mm", dpi = 600)
}



## Reduce dimensions (average clustered functions) (MANUAL) ----
# efps_sub <- c("Cfuns", "WUEfuns", "CUEeco", "Gsmax")
efps_sub <- c("Cfuns", "CUEeco", "EF", "EFampl", "Gsmax", "LUE", "Rfuns", "WUEfuns")
dat_sub <- dat_norm %>% 
  rowwise() %>% 
  mutate(
    Cfuns = mean(c(GPPsat, NEPmax), na.rm = T),
    Rfuns = mean(c(Rb, Rbmax), na.rm = T),
    WUEfuns = mean(c(WUE, uWUE), na.rm = T),
    across(.cols = where(is.double), .fns = ~ if_else(is.nan(.x), NA_real_, .x)) # convert NaN into NA
  ) %>%
  ungroup() %>% # ungroup rowwise
  select(SITE_ID, all_of(efps_sub)) # remove non-averaged functions



### Calculate multifunctionality -----------------------------------------------
dat_emf <- dat_sub


## Average method ----
dat_emf <- dat_emf %>% 
  pivot_longer(cols = all_of(efps_sub), names_to = "EFP_name", values_to = "EFP_value") %>% 
  group_by(SITE_ID) %>% 
  mutate(
    EMFavg = mean(EFP_value, na.rm = T)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = EFP_name, values_from = EFP_value) %>% 
  glimpse()


## Threshold method ----
threshold <- 0.5 # 50% threshold as in the example in Manning et al., 2018

dat_emf <- dat_emf %>% 
  pivot_longer(cols = all_of(efps_sub), names_to = "EFP_name", values_to = "EFP_value") %>% 
  group_by(SITE_ID) %>% 
  mutate(
    EMFthr = sum(EFP_value > threshold, na.rm = T) / length(efps_sub)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = EFP_name, values_from = EFP_value) %>% 
  glimpse()



# ## Radar chart method (NOT IMPLEMENTED) ----
# # WARNING: area of radar plot increases quadratically, not linearly (overevaluation of differences).
# # Also, radar plots are often criticized as the circular layout is harder to read 
# # and choice of ordering might lead to misleading interpretation.
# 
# # IDEA: mutlidimensional area inscribed within vertices of all PCA vectors, divided by number of dimensions considered (PCs).
#
# ## PCA
# dat_pca <- dat_sub %>% left_join(readr::read_csv("data/input/igbp.csv", show_col_types = F), by = "SITE_ID") %>% dplyr::filter(IGBP != "CVM") %>% drop_na()
# 
# ## Run PCA without multiple imputation
# pca_result <- FactoMineR::PCA(dat_pca %>% dplyr::select(-SITE_ID, -IGBP), scale.unit = T, ncp = 10, graph = F)
# pca1 <- ade4::dudi.pca(dat_pca %>% dplyr::select(-SITE_ID, -IGBP), center = TRUE, scale = TRUE, scannf = FALSE, nf = 10)
# 
# 
# ## Plot
# plot_elements_dark <- "gray25"
# normal_text <- 3; title_text <- 1.2; subtitle_text <- 1.1; # relative size to parent
# fviz_pca_biplot(pca_result,
#                 axes = c(1, 2),
#                 col.ind = dat_pca$IGBP, #"grey50",
#                 # col.ind = NA, #plot_elements_light, #"white",
#                 geom.ind = "point",
#                 palette = CatCol_igbp,#'futurama',
#                 label = "var",
#                 col.var = plot_elements_dark,
#                 labelsize = 2,
#                 repel = TRUE,
#                 pointshape = 16,
#                 pointsize = 2,
#                 alpha.ind = 0.67,
#                 arrowsize = 0.5) +
#   labs(title = "",
#        x = "PC1",
#        y = "PC2",
#        fill = "IGBP") +
#   guides(fill = guide_legend(title = "")) +
#   # theme(title = element_blank(),
#   #       text = element_text(size = rel(normal_text)),
#   #       axis.line = element_blank(),
#   #       axis.ticks = element_blank(),
#   #       axis.title = element_text(size = rel(title_text), face = "bold"),
#   #       # axis.text = element_text(size = rel(normal_text)),
#   #       # plot.margin = unit(c(0, 0, 0, 0), "cm"),
#   #       # legend.position = "none"
#   #       # legend.text = element_text(size = rel(subtitle_text)),
#   #       legend.key.height = unit(5, "mm"),
#   #       legend.key.width = unit(2, "mm")
#   # ) +
#   NULL
# 
# pca_load <- pca_result$var$coord %>% # add loadings
#   as_tibble(rownames = "var") %>%
#   pivot_longer(cols = !var, names_to = "PC", values_to = "loading") %>% 
#   mutate(PC = paste0("PC", stringr::str_sub(PC, start = 5)))
# 
# library(ggradar)
# dat_emf %>% 
#   dplyr::slice_sample(n = 20) %>%
#   select(SITE_ID, all_of(efps_sub)) %>% 
#   drop_na() %>% 
#   ggradar()
# 
# 
#
### Plot -----------------------------------------------------------------------
p_emf <- dat_emf %>%
  pivot_longer(cols = contains("EMF"), names_to = "VARIABLENAME", values_to = "DATAVALUE") %>%
  ggplot() +
  geom_point(aes(SITE_ID, DATAVALUE), alpha = 0.8, color = "#808080", size = 3) +
  facet_wrap(. ~ VARIABLENAME, scales = "free_y") +
  labs(caption = paste("n =", nrow(dat_emf))) +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 270), # rotate x axis text
        panel.grid.major = element_line(),
        plot.caption = element_text(size = 24),  # caption text
        plot.margin = margin(10, 10, 10, 10, unit = "mm"), # margins around plot
        strip.text = element_text(size = 20),    # subplots title text
        strip.background = element_blank()       # subplots title with no border
  )
print(p_emf)


## Save plot
if (savedata) {
  ggsave(filename = glue::glue("EMFs_{vers_out}.jpg"),
         plot = p_emf, device = "jpeg", path = "results/scatterplots",
         width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
}



### Vector names of variables --------------------------------------------------
## Define names of Ecosystem MultiFunctionality
emf_names <- dat_emf %>% dplyr::select(-SITE_ID, -contains("IGBP"), -contains("Rao"), -any_of(efps_names), -any_of(efps_sub)) %>% names()
if (savedata) {
  ## Save
  save(emf_names, file = glue::glue("data/inter/emf_names_{vers_out}.RData"))
}



### Save data ------------------------------------------------------------------
if (savedata) {
  dat_emf <- dat_emf %>% select(SITE_ID, all_of(emf_names))
  write_csv(dat_emf, glue::glue("data/inter/data_emf_{vers_out}.csv"))
}



### End ------------------------------------------------------------------------
print("End of script.")