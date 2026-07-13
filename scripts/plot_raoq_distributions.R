#### PLOT RaoQ vs EFPs + EMFs

### Authors: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))



### Options --------------------------------------------------------------------
# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F: ")) # ask if output should be saved
dat_in <- as.character(read.table("data/data_version.txt"))
vers_out <- dat_in


### Utilities ------------------------------------------------------------------
## Functions
source("scripts/functions/safe_load_packages.R")
source("scripts/functions/my_tag_facet_outside.R")
source("scripts/functions/rm_plot_titles.R")

## Packages
required_packages <- c(
  "dplyr",        # tidy data manipulation
  "ggplot2",      # tidy plots
  "ggpubr",       # advanced ggplot functionalities
  "ggrepel",      # repelled labels
  "patchwork",    # combine plots
  "RColorBrewer", # plot color functionalities
  "reshape2",     # reshape data
  "purrr",        # map functions
  "readr",        # read table format files
  "rstatix",      # Pipe-Friendly Framework for Basic Statistical Tests
  "scales",       # modify scales of axes
  "stringr",      # tidy string manipulation
  "tidyr"         # clean and reshape tidy data
)
safe_load_packages(required_packages)

## Other utils
source("scripts/utils/MyThemes.R")
source("scripts/utils/MyCols.R")
source("scripts/utils/MyPlotSpecs.R")
source("scripts//utils/units.R")



### Data -----------------------------------------------------------------------
## Data
dat <- read_csv(glue::glue("data/inter/data4analysis_efps_{dat_in}.csv"), show_col_types = F)

## Variable names
x_names <- c("RaoQ_S2", "RaoQ_NDVI", "RaoQ_NIRv")

load("data/inter/efps_names_v04.RData")
y_names <- efps_names
# fill_names <- paste0(x_names, "_std")

## Variable labels
# Named vectors or Vectors with same order as variable names vectors
x_labels <- c(expression(paste(RaoQ[bands], " [-]")), expression(paste(RaoQ[NDVI], " [-]")), expression(paste(RaoQ[NIRv], " [-]")))
names(x_labels) <- x_names
y_labels <- efp_units
# fill_legend <- c(expression(paste(RaoQ[bands], " [-]")), expression(paste(RaoQ[NDVI], " [-]")), expression(paste(RaoQ[NIRv], " [-]")))


### Coefficient of variation of RaoQ -------------------------------------------
## Calculate "normalized std" (~coefficient of variation (with median though))
dat <- dat %>% 
  rename_with(.cols = contains("RaoQ") & !contains("std"), .fn = ~paste0(., "_median")) %>% # re-add aggregation specification
  pivot_longer(cols = contains("RaoQ"), names_to = c("name", ".value"), names_pattern = "(.+)_(.+)$") %>% # pivot into three columns: name-median-std
  mutate(
    cv = std / median * 100, # coefficient of variation (%)
    cv = if_else(is.finite(cv), true = cv, false = NA_real_), # remove Inf values when dividing by zero
    ccv = (1 + (1 / (4 * n()))) * cv # corrected cv for small sample sizes.
    # Robert R. Sokal (with Internet Archive). (1995). Biometry. W.H. Freeman. http://archive.org/details/biometryprincipl00soka_0
    # https://www.scribd.com/document/390978290/Sokal-Rohlf-Biometry-3d-1995#page=72
  ) %>% 
  pivot_wider(names_from = "name", names_glue = "{name}.{.value}", values_from = c(median, std, cv, ccv)) %>% 
  rename_with(.cols = contains("RaoQ") & contains("median"), .fn = ~str_replace(string = ., pattern = "_median", replacement = "")) %>% 
  glimpse()



### Plot CV of RaoQ distributions ----------------------------------------------
## Plot specs ----
my_alpha <- alpha_transparent


## Data for tests ----
dat_distr <- dat %>% 
  select(SITE_ID, IGBP, contains(".ccv")) %>% 
  mutate(
    PFT = case_when(
      IGBP %in% c("DBF", "EBF", "ENF", "MF") ~ "Forests",
      IGBP %in% c("CSH", "OSH", "WSA") ~ "Other woody",
      IGBP %in% c("GRA", "SAV", "WET") ~ "Grasslands",
      .default = IGBP
    ),
    .after = IGBP
  )


## Test normality ----
print("=> Testing assumption of normality on variables' residuals.")

## Perform Shapiro-Wilk's test
test_names <- names(dat_distr %>% select(contains("RaoQ")))

shapiro_stats <- dat_distr %>% 
  select(PFT, contains("RaoQ")) %>%
  # drop_na() %>% 
  group_by(PFT) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(
    shapiro_stats = map(
      .x = data, .f = ~shapiro_test(
        data = .x, vars = test_names)
      )
    ) %>%
  select(-data) %>% 
  unnest(cols = shapiro_stats) %>% 
  rename(Metric = variable) %>% 
  mutate(normality = if_else(p > 0.1, TRUE, FALSE)) # Patrick Royston (1995). Remark AS R94: A remark on Algorithm AS 181: The WW test for normality. Applied Statistics, 44, 547–551. doi:10.2307/2986146.

## QQ plot
ggqqplot(
  data = dat_distr %>% pivot_longer(cols = contains("RaoQ"), names_to = "Metric", values_to = "RaoQ_CV"),
  x = "RaoQ_CV", facet.by = c("Metric", "PFT"), na.rm = T
) +
  theme_bw() + theme_transp_strip + NULL

print(glue::glue("{nrow(shapiro_stats) - shapiro_stats %>% pull(normality) %>% sum()} out of {nrow(shapiro_stats)} variables did NOT meet the assumption of normality."))


## Test significant differences ----
print("=> Identifying significant differences.")

## Data
dat_signif <- dat_distr %>% 
  select(PFT, contains("RaoQ")) %>%
  pivot_longer(cols = contains("RaoQ"), names_to = "Metric", values_to = "RaoQ_CV") %>% 
  mutate(
    Metric = factor(
      Metric, levels = test_names
    )
  ) %>% 
  left_join(shapiro_stats %>% select(PFT, Metric, normality), by = c("PFT", "Metric"))


signif_out <- dat_signif %>% 
  group_by(PFT) %>% 
  group_modify(
    .f = ~ {
      if (sum(.x$normality, na.rm = T) == nrow(.x)) { # if all subgroups (variables) are normally distributed
        # Use parametric test
        pairwise_t_test(data = .x, formula = RaoQ_CV ~ Metric, p.adjust.method = "holm") %>%
          add_xy_position(formula = RaoQ_CV ~ Metric)
        } else {
          # Use non-parametric test
          pairwise_wilcox_test(data = .x, formula = RaoQ_CV ~ Metric, p.adjust.method = "holm") %>%
            add_xy_position(formula = RaoQ_CV ~ Metric)
          # significance breaks + symbols: ?rstatix::add_significance
        }
    }
  ) %>%
  ungroup()


## Data for plotting ----
dat_plot <- dat_distr %>% 
  rename_with(.cols = contains(".ccv"), .fn = ~str_replace(., pattern = ".ccv", replacement = "")) %>%
  pivot_longer(cols = contains("RaoQ"), names_to = "Metric", values_to = "RaoQ_CV") #%>% 
  # mutate(
  #   Metric = factor(
  #     Metric, levels = x_names#, labels = x_labels
  #   )
  # )


## Plot ----
p_distr <- dat_plot %>%
  ggplot(aes(x = Metric, y = RaoQ_CV, color = Metric, fill = Metric)) +
  geom_jitter(
    alpha = my_alpha, size = 0.25,
    #color = "gray75",
    width = 0.25, na.rm = T
  ) +
  geom_violin(
    quantile.linetype = "solid",
    quantile.linewidth = c(line_width_thin, line_width_medium, line_width_thin),
    na.rm = T
    ) +
  stat_pvalue_manual(
    data = signif_out, label = "p.adj.signif", tip.length = 0,
    y.position = c(-50, -25, 0)
    ) +
  # geom_pwc(
  #   label = "p.adj.signif", hide.ns = F, method = "wilcox_test",
  #   method.args = list(formula = RaoQ_CV ~ Metric, p.adjust.method = "holm"),
  #   tip.length = 0, y.position = c(-50, -25, 0)
  #   ) +
  facet_wrap(. ~ PFT, nrow = 1) +
  ylab("Within-site Rao Q's coefficient of variation (%)") +
  scale_x_discrete(labels = x_labels) +
  scale_color_manual(values = Three_colors4, labels = x_labels) +
  scale_fill_manual(
    values = paste0(Three_colors4, my_alpha * 100), # add alpha manually (only needed for area fill color)
    labels = x_labels
    ) +
  theme_bw() +
  theme_transp_strip +
  theme(
    axis.text.x = element_blank(), #element_text(angle = 90, vjust = 0.5),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()
  ) +
  NULL
p_distr

## Add panels' tags
p_distr <- my_tag_facet_outside(p_distr, open = "", close = "")


## Save
if (savedata) {
  ## JPEG file
  ggsave(filename = glue::glue("results/distributions/RaoQcv_{vers_out}.jpg"),
         plot = p_distr, device = "jpeg",
         width = 8, height = 4.5, dpi = 300)
}



### End ------------------------------------------------------------------------
print("End of script.")