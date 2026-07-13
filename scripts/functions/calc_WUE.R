#### Function for filtering, uWUE calculation (Mirco Migliavacca)
# reorganized and improved, added additional information in output (Ulisse Gomarasca)

## Arguments -----
# data    = dataframe containing fluxnet data for single sites
# site    = character, name of the site
# SWfilt  = Value of minimum threshold for ShortWave filter, e.g. to exclude for
#           nighttime data below 20, 100 or 200.


### Function -------------------------------------------------------------------
calc_WUE <- function(
    data, site, year,
    SWfilt = 200,
    plotting = plotting_efps
)
{
  
  
  ## Utilities ----
  source("scripts/functions/plot_timeseries.R")
  source("scripts/functions/safe_load_packages.R")
  
  required_packages <- c("bigleaf", "dplyr", "rlang", "tidyr")
  safe_load_packages(required_packages)
  
  
  ## Quote & settings ----
  if (rlang::is_empty(year)) {
    site_year <- paste0("site ", site)
  } else if (!rlang::is_empty(year)) {
    data <- data %>% dplyr::mutate(YEAR = lubridate::year(DATETIME), .before = everything()) # add year
    
    year <- data %>% dplyr::pull(YEAR) %>% unique()
    site_year <- paste0("site-year ", site, "-", year)
  }
  
  
  ### Processing ----
  print(glue::glue('..computing WUE Metrics for {site_year}.'))
  
  
  ## Filtering ----
  ## Daylight (+ Friction Velocity filter)
  data <- data %>% dplyr::filter(SW_IN > SWfilt) # filter instead of replacing with NA (SWfilt should be 200 for water EFPs)
  # NB: u* filter should not be necessary for WUE metrics
  
  ## Using only measured data: removing NA for wind speed
  data <- data %>% tidyr::drop_na(WS)
  
  ## Convert units if needed ----
  if (!"VPD_kPa" %in% names(data)) {data <- data %>% dplyr::mutate(VPD_kPa = VPD / 10)} # convert units of VPD from [hPa] to [kPa]
  
  
  ## Subset for calculations and omit NAs ----
  data_subset <- data %>% 
    dplyr::select(DATETIME, GPP, NEE, LE, VPD_kPa, TA) %>% 
    tidyr::drop_na()
  
  
  ## Calculate EFPs ----
  if (nrow(data_subset) == 0) { # if no data is available after filtering
    warning(glue::glue("The {site_year} was skipped because of empty data."))
    output <- tibble(
      WUE = NA_real_,
      # WUE_NEE = NA_real_,
      # IWUE = NA_real_,
      uWUE = NA_real_
    )
    
    
  } else if (nrow(data_subset) != 0) { # if dataframe is not empty
    ## Calculate WUE metrics
    wue_metrics <- data_subset %>% 
      as.data.frame() %>% 
      bigleaf::WUE.metrics(GPP = "GPP", NEE = "NEE", LE = "LE", VPD = "VPD_kPa", Tair = "TA",
                           constants = bigleaf::bigleaf.constants())
    
    ## Format output
    output <- tibble(var = names(wue_metrics), value = wue_metrics) %>% # convert to tibble
      tidyr::pivot_wider(names_from = var, values_from = value) %>% 
      dplyr::select(WUE, uWUE) # variables of interest
    
    
    ## Plot variable timeseries
    if (plotting & site %in% rand_sites) {
      if (savedata) {savepath <- "results/timeseries/efps"} else {savepath <- NA}

      # Data
      dat_plot <- data_subset %>%
        select(DATETIME) %>%
        mutate(
          WUE = output$WUE,
          uWUE = output$uWUE
        )

      # Plot
      dat_plot %>% plot_timeseries(y = "WUE", site = site, savepath = savepath) # here it will be a horizontal line
      dat_plot %>% plot_timeseries(y = "uWUE", site = site, savepath = savepath) # here it will be a horizontal line
    }
  }
  
  
  ### Output ----
  return(output)
}



# ### Debug ----------------------------------------------------------------------
# debugonce(calc_WUE)