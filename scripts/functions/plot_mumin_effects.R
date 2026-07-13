#### PLOT COEFFICIENTS OF MULTIMODEL INFERENCE

### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)


#### Function ----------------------------------------------------------------
plot_mumin_effects <- function(test = "main") {
  ### Function input -----------------------------------------------------------
  if (test == "main") {test_vers <- "v01."}
  else if (test == "lai") {test_vers <- "v01b."}
  else if (test == "nirv") {test_vers <- "v02."}
  else if (test == "soil") {test_vers <- "v03."}
  # else if (test == "iav") {test_vers <- "v04."}
  
  
  
  ### Utilities ----------------------------------------------------------------
  ## Functions
  source("scripts/functions/safe_load_packages.R")
  source("scripts/functions/rm_plot_titles.R")
  
  ## Packages
  required_packages <- c(
    "dplyr",        # tidy data manipulation
    "ggplot2",      # tidy plots
    "glue",         # glue strings
    "patchwork",    # combine plots
    "purrr",        # map functions
    "RColorBrewer", # plot color functionalities
    "readr",        # read table format files
    "scales",       # modify scales of axes
    "stringr",      # tidy string manipulation
    "tictoc",       # measure time
    "tidyr"         # clean and reshape tidy data
  )
  safe_load_packages(required_packages)
  
  ## Other
  source("scripts/utils/MyThemes.R")
  source("scripts/utils/MyCols.R")
  source("scripts/utils/MyPlotSpecs.R")
  source("scripts/utils/units.R")
  
  
  
  ### Script settings ----------------------------------------------------------
  tic() # measure run time
  
  # Data settings
  savedata <- as.logical(readline(prompt = "Save the output of the script? T/F: ")) # ask if output should be saved
  
  efp_in <- as.character(read.table("data/efp_version.txt"))
  emf_in <- as.character(read.table("data/emf_version.txt"))
  # iav_in <- as.character(read.table("data/iav_version.txt"))
  struct_in <- as.character(read.table("data/struct_version.txt"))
  dat_in <- as.character(read.table("data/data_version.txt"))
  vers <- as.character(read.table("data/mumin_analysis_version.txt")) #paste0(test_vers, stringr::str_extract(dat_in, "[:digit:]+[:punct:]?[:digit:]*[:punct:]?[:digit:]*"))
  # v01.030, 08.05.2024:  Main analyses.
  # v01b.030,14.05.2024:  Testing measured LAImax in models.
  # v02.030, 21.05.2024:  Testing NIRv_median together with RaoQ_NIRv.
  # v03.030, 27.05.2024:  Testing soil variables.
  # v04.030, 27.05.2024:  Testing only IAV climate variables.
  
  
  
  ### Process ------------------------------------------------------------------
  for (eee in 1:2) {
    ## Select response variables ----
    if (eee == 1) {response <- "efp"} else if (eee == 2) {response <- "emf"}
    # "efp" = EFPs calculated on full site timeseries
    # # "iav" = stability of EFPs (not supported)
    # "emf" = multifunctionality of EFPs
    
    
    ## Select subset of predictors ----
    subx <- "all"
    # "no"      = exclude all biodiversity variables
    # "all"     = include all biodiversity predictors
    
    
    ## Select RaoQ input ----
    raoq_in <- "nirv"
    # "bands" = raoQ from all bands
    # "ndvi" = raoQ from NDVI
    # "nirv" = raoQ from NIRv
    
    
    
    ### More output settings ---------------------------------------------------
    vers_in <- glue("{response}_{subx}_{raoq_in}_{test}_{vers}")
    vers_out <- paste0(vers_in, "")
    
    
    
    ### Data -------------------------------------------------------------------
    dat <- read_csv(glue("results/multimodel_inference/prediction_and_relaimpo_{vers_in}.csv"), show_col_types = F)
    
    
    
    ### Vector names -----------------------------------------------------------
    ## EFPs
    load(file = glue("data/inter/efps_names_{efp_in}.RData"))
    
    ## EMF
    load(file = glue("data/inter/emf_names_{emf_in}.RData"))
    
    ## Meteorology & other
    load(file = glue("data/inter/clim_names_{efp_in}.RData"));  clim_names <- clim_names[clim_names %in% dat$variable]
    # if (response == "efp") { # analysis on full-timeseries EFPs
    #   load(file = glue("data/inter/clim_names_{efp_in}.RData"));  clim_names <- clim_names[clim_names %in% dat$variable]
    # } else if (response == "iav") { # analysis on stability of EFPs
    #   load(file = glue("data/inter/clim_names_{efp_in}.RData")) # mean Meteorology
    #   load(file = glue("data/inter/clim_iav_names_{iav_in}.RData")) # Meteorology variability
    #   clim_names <- c(clim_names[clim_names %in% dat$variable], clim_iav_names[clim_iav_names %in% dat$variable]);
    # } else if (response == "emf") { # analysis on multifunctionality of EFPs
    # load(file = glue("data/inter/clim_names_{efp_in}.RData"));  clim_names <- clim_names[clim_names %in% dat$variable]
    # }
    
    ## Soil properties
    soil_names <- c("AWCh1", "AWCh2", "AWCh3", "CLAY", "SAND", "SILT", "ORCDRC", "PHIHOX")
    
    ## Structure
    load(file = glue("data/inter/struct_names_{struct_in}.RData"));  struct_names <- struct_names[struct_names %in% dat$variable]
    
    
    
    ## Omit NAs ----
    dat <- dat %>% drop_na()
    
    
    # ## Select significant predictors ----
    # alpha <- 0.05
    # 
    # dat <- dat %>% dplyr::filter(p_val < alpha) # exclude non-significant predictors for plotting
    # 
    # 
    ## Number of observations ----
    # Order of n_obs same as EFPs
    n_obs <- dat %>%
      drop_na() %>%
      select(prediction, n) %>% 
      unique() %>% 
      pull(n)
    
    
    
    ### Plot -------------------------------------------------------------------
    ## Labels and options ----
    ## Add color labels
    if (any(soil_names %in% unique(dat$variable))) {
      accessible_palette <- setNames(Four_colorblind, c("Biodiversity proxy", "Mean meteorology", "Structural properties", "Soil properties"))
    } else {
      accessible_palette <- setNames(Three_colorblind2, c("Biodiversity proxy", "Mean meteorology", "Structural properties")) 
    }
    
    ## Add predictor labels
    dat <- dat %>% 
      mutate(var_type = case_when(
        variable %in% clim_names & !variable %in% soil_names ~ "Mean meteorology",
        variable %in% soil_names ~ "Soil properties",
        variable %in% struct_names ~ "Structural properties",
        variable == "NIRv_median" ~ "Structural properties",
        str_detect(variable, "Rao") ~ "Biodiversity proxy",
        T ~ "Other"
      )
      ) %>% print(n = Inf)
    
    
    ## Add levels for y axis order
    # variable as factor with levels to define order on y axis
    var_names <- dat %>% dplyr::arrange(desc(var_type), desc(variable)) %>% pull(variable) %>% unique()
    y_names <- dat %>% arrange(prediction) %>% pull(prediction) %>% unique()
    if (response == "efp") {
      y_labels <- efp_labels[intersect(y_names, efps_names)]
    } else if (response == "emf") {
      y_labels <- emf_labels[intersect(y_names, emf_names)]
    }
    
    var_labels <- var_names %>% str_replace_all("_", " ")
    
    dat <- dat %>% 
      mutate(
        variable = factor(
          variable,
          levels = var_names,
          labels = var_labels
        ),
        prediction = factor(
          prediction,
          levels = y_names,
          labels = y_labels
        ),
        var_type = factor(
          var_type,
          levels = c("Biodiversity proxy", "Mean meteorology", "Structural properties", "Soil properties")
        ),
        rel_cat = case_when( # add relative importance categories
          # rel_importance >= 0.0 & rel_importance < 0.1 ~ 0.00,
          # rel_importance >= 0.1 & rel_importance < 0.2 ~ 0.25,
          # rel_importance >= 0.2 & rel_importance < 0.3 ~ 0.50,
          # rel_importance >= 0.3 & rel_importance < 0.4 ~ 0.75,
          # rel_importance >= 0.4 & rel_importance < 1.0 ~ 1.00
          rel_importance >= 0.00 & rel_importance < 0.15 ~ 0.00,
          rel_importance >= 0.15 & rel_importance < 0.30 ~ 0.50,
          rel_importance >= 0.30 & rel_importance < 1.00 ~ 1.00
        ) %>% factor(
          # levels = c(0.00, 0.25, 0.50, 0.75, 1.00),
          # labels = c("0%-10%", "10%-20%", "20%-30%", "30%-40%", "40%-100%")
          levels = c(0.00, 0.50, 1.00),
          labels = c("0%-15%", "15%-30%", "30%-100%")
        ),
      ) %>%
      arrange(prediction, var_type, variable) %>% 
      glimpse()
    
    
    # x limits
    x_lim <- max(abs(dat %>% mutate(val_std = if_else(estimate > 0, true = estimate + std_error, false = estimate - std_error)) %>% pull(val_std)), na.rm = T) * 1.1 # identify max coefficient across analyses and round, with a buffer
    # # x_lim <- 0.05 * ceiling(max(abs(dat$estimate), na.rm = T) / 0.05) * 1.1 # identify max coefficient across analyses and round, with a buffer
    # x_breaks <- c(-0.05 * round(x_lim / 0.05), 0, 0.05 * round(x_lim / 0.05))
    # x_labels <- c(format(-0.05 * round(x_lim / 0.05), nsmall = 2), format(-0.05 * round(x_lim / 0.05) / 2, nsmall = 2), "0.00", format(0.05 * round(x_lim / 0.05) / 2, nsmall = 2), format(0.05 * round(x_lim / 0.05), nsmall = 2))
    
    # plot specifications
    line_width <- line_width_thick
    point_size <- point_size_medium
    
    
    ## Plot model coefficients ----
    p_effects <- list() # initialize list of plots
    for (pp in 1:length(unique(dat$prediction))) {
      print(glue("=> Plotting effects on {y_names[pp]}."))
      
      # data (pp)
      dat_pp <- dat %>% dplyr::filter(prediction == unique(dat$prediction)[pp])
      
      # caption (pp)
      labelcap <- bquote(
        R^2 ~ "=" ~ .(sprintf("%.1f", signif(unique(dat_pp$R2), 3) * 100)) ~ "%" ~
          "  RMSE =" ~ .(sprintf("%.1f", signif(unique(dat_pp$RMSE), 2))) ~
          "  n =" ~ .(unique(dat_pp$n))
        )
      
      p_effects[[pp]] <- dat_pp %>% 
        ggplot(aes(x = estimate, y = variable)) +
        geom_vline(xintercept = 0, color = text_color_background) + # 0 line
        # geom_point( ### debug with shape check ###
        #   aes(color = var_type, shape = rel_cat),
        #   alpha = 1, size = 12, stroke = line_width, na.rm = T, show.legend = T
        # ) +
        geom_errorbar( # draw errorbars without transparency
          aes(color = var_type, xmin = estimate - std_error, xmax = estimate + std_error),
          alpha = 1, height = 0, linewidth = line_width, na.rm = T, show.legend = T
        ) +
        geom_point( # add white fill, full-color stroke points to cover errorbars in the central region
          aes(color = var_type),
          alpha = 1, fill = "white",
          stroke = line_width, size = point_size, shape = 21, na.rm = T, show.legend = T
        ) +
        geom_point( # draw round filled shape with transparency
          aes(alpha = rel_cat, color = var_type),
          size = point_size, shape = 19, na.rm = T, show.legend = T
        ) +
        geom_text( # add significance asterisks
          aes(color = var_type, label = ifelse(p_val < 0.05, "*", NA)),
          nudge_x = x_lim * 0.1, nudge_y = 0.1,
          size = point_size * 1.5, na.rm = T, show.legend = F
        ) +
        scale_alpha_discrete(
          # breaks = c(0.05, 0.3, 0.55), labels = c("Low", "Medium", "High"), # display alpha categories
          range = c(0, 1), # display transparency range
          # limits = c(0.04, 0.56), oob = squish # replaces out of bounds values with the nearest limit
          drop = F
        ) +
        scale_discrete_manual( # custom colors
          aesthetics = "color",
          values = accessible_palette,
          na.translate = F
        ) +
        # discrete_scale( # custom shape to check that everything is working as intended
        #   aesthetics = "shape",
        #   scale_name = "rel_cat",
        #   palette = manual_pal(c(25, 23, 24)),
        #   drop = F
        # ) +
        scale_x_continuous(
          breaks = waiver(),
          # labels = x_labels,
          limits = c(-x_lim, x_lim), # define same x axis limits for all subplots
          n.breaks = 5 # "algorithm may choose a slightly different number to ensure nice break labels" <-- do what you want then #!*@$!!
        ) +
        # xlim(c(-x_lim, x_lim)) +
        xlab("Effect coefficient") +
        labs(
          title = y_labels[pp],
          caption = labelcap
        ) +
        guides(
          # shape = guide_legend(title = "Relaimpo check", override.aes = list(stroke = line_width)),
          alpha = guide_legend(title = "Relative importance", override.aes = list(size = point_size)),
          color = guide_legend(title = "Predictor type", override.aes = list(size = point_size))
        ) + # legend titles
        theme_bw() +
        theme_combine +
        theme(
          axis.title.x = element_blank(), # remove x axis title
          axis.title.y = element_blank(), # remove y axis title
          legend.background = element_rect(fill = "transparent"),
          plot.caption = element_text(color = text_color_background), # color of caption label
          plot.margin = unit(c(0, 10, 0, 0), "mm")
        ) +
        NULL
      
      ## Save
      if (savedata) {
        # single multimodel effect plots
        ggplot2::ggsave(
          filename = glue("results/multimodel_inference/singleEFPs/mumin_coefficients_{y_names[pp]}_{vers_out}.jpg"), plot = p_effects[[pp]],
          device = "jpeg", width = 16, height = 9, dpi = 150
          )
      }
      
      ## Remove legend for combined patchwork
      if (pp != length(unique(dat$prediction))) {
        p_effects[[pp]] <- p_effects[[pp]] + theme(legend.position = "none")
      }
    }
    
    
    
    ## Combine ----
    n_rows <- ceiling(length(p_effects) / 3)
    
    # ## Remove x-axis labels & legends (keep only on bottom row)
    # if (n_rows > 1) {
    #   p_effects[1:(length(p_effects)-length(p_effects)/n_rows)] <- map(.x = p_effects[1:(length(p_effects)-length(p_effects)/n_rows)], .f = ~rm_xaxis_title(p = .x))
    #   # remove by: index != final row
    # }
    
    ## Combine
    if (response %in% c("efp", "iav")) {
      p_out <- wrap_plots(p_effects, nrow = n_rows) + guide_area() +
        plot_layout(guides = 'collect') +
        plot_annotation(tag_levels = "A")
    } else if (response == "emf") {
      p_out <- wrap_plots(p_effects, nrow = n_rows) +
        plot_layout(guides = 'collect') +
        plot_annotation(tag_levels = "A")
    }
    p_out
    
    
    
    ### Save -------------------------------------------------------------------
    if (savedata) {
      scal <- 2
      width <- 16 #508
      if (eee == response %in% c("efp", "iav")) {
        height <- 18
      } else if (response == "emf") {
        height <- 6 #285.75 #/ 2 * n_rows
      }
      
      # combined multimodel effects plot
      ggplot2::ggsave(filename = glue("results/multimodel_inference/mumin_coefficients_{vers_out}.jpg"), plot = p_out, device = "jpeg",
                      width = width, height = height, dpi = 150 * scal)
      
      # # transparent png
      # ggplot2::ggsave(filename = glue("results/multimodel_inference/mumin_coefficients_{vers_out}.png"), plot = p_out, device = "png",
      #                 bg = "transparent", width = width, height = height, units = "mm", dpi = 300 * scal)
      
      # if (subx == "main") {
      #   # cross validation effects plot
      #   ggplot2::ggsave(filename = glue("results/multimodel_inference/crossval_coefficients_{vers_out}.jpg"), plot = p_crossval, device = "jpeg",
      #                   width = width, height = height, units = "mm", dpi = 300 * scal)
      # }
    }
    
    
    
  } # end loop for type of predicted variables (EFPs vs EMFs)
  
  
  
  ### End ----------------------------------------------------------------------
  print("End of script.")
}


# #### Debug ---------------------------------------------------------------------
# debugonce(plot_mumin_effects)