#### Function for filtering, EF calculation (Mirco Migliavacca)
# reorganized and improved, added additional information in output (Ulisse Gomarasca)

### Arguments ----
# data    = dataframe containing fluxnet data for single sites
# site    = character, name of the site
# elevation = Scalar. Single value of altitude for the site
# SWfilt  = Value of minimum threshold for ShortWave filter, e.g. to exclude for
#           nighttime data below 20, 100 or 200.
# USfilt  = Value of minimum threshold for USTAR filter.


### Function ----
calc_EF <- function(
    data, site, year,
    SWfilt = 200, USfilt = 0.2, EFfilt = c(-4, 5),
    plotting = plotting_efps
    )
{
  ## Utilities ----
  source("scripts/functions/plot_timeseries.R")
  source("scripts/functions/safe_load_packages.R")
  
  required_packages <- c("bigleaf", "dplyr", "tidyr")
  safe_load_packages(required_packages)
  
  
  ## Quote & settings ----
  if (rlang::is_empty(year)) {
    site_year <- paste0("site ", site)
  } else if (!rlang::is_empty(year)) {
    data <- data %>% dplyr::mutate(YEAR = lubridate::year(DATETIME), .before = everything()) # add year
    
    year <- data %>% dplyr::pull(YEAR) %>% unique()
    site_year <- paste0("site-year ", site, "-", year)
  }
  
  
  ### Processing -----------------------------
  print(glue::glue('....computing evaporative fraction parameters (EF, EFampl) for {site_year}.'))
  
  ## Filter data ----
  ## Daylight (+ Friction Velocity filter)
  data <- data %>% dplyr::filter(SW_IN > SWfilt & USTAR > USfilt) # filter instead of replacing with NA (SWfilt should be 200 for water EFPs)
  
  ## Using only measured data: removing NA for wind speed
  data <- data %>% tidyr::drop_na(WS)
  
  
  ## Subset for calculations and omit NAs ----
  data_subset <- data %>% 
    dplyr::select(DATETIME, LE, H) %>% 
    tidyr::drop_na()
  

  ## Calculation of metrics ----
  if (nrow(data_subset) == 0) { # if no data is available after filtering
    warning(glue::glue("The {site_year} was skipped because of empty data."))
    output <- tibble(
      EF = NA_real_,
      EFampl = NA_real_
    )
    
    
  } else if (nrow(data_subset) != 0) { # if dataframe is not empty
    ## Calculate
    data_subset <- data_subset %>% 
      mutate(EF = LE / (LE + H)) %>% 
      drop_na() # necessary for amplitude to compare same amount of data in both quantiles?
    
    
    ## Filter
    data_subset <- data_subset %>% 
      filter(
        EF > min(EFfilt) &
          EF < max(EFfilt)
      )
    
    
    ## Plot variable timeseries
    if (plotting & site %in% rand_sites) {
      if (savedata) {savepath <- "results/timeseries/efps"} else {savepath <- NA}
      
      # Plot
      data_subset %>% plot_timeseries(y = "EF", site = site, savepath = savepath)
    }
    
    
    ## Aggregate output
    output <- data_subset %>% 
      summarise(
        EFampl = quantile(EF, 0.75, na.rm = TRUE) - quantile(EF, 0.25, na.rm = TRUE),
        EF = median(EF, na.rm = TRUE) # NB after EFampl otherwise overwriting EF values
      )
  }
  
  
  ### Output ----
  return(output)
}


# ### Debug ----
# debugonce(calc_EF)