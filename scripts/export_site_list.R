#### EXTRACT SITE LIST

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))


# Data settings
efp_in <- as.character(read.table("data/efp_version.txt"))
emf_in <- as.character(read.table("data/emf_version.txt"))

savedata <- as.logical(readline(prompt = "Save the output of the script? T/F: ")) # ask if output should be saved
if (savedata) {
  vers_out <- paste0(efp_in, stringr::str_extract(emf_in, "(?<=[:digit:])[:digit:]{1}")) # output version
}



### Utilities ------------------------------------------------------------------
## Functions
source("scripts/functions/safe_load_packages.R")

## Packages
required_packages <- c(
  "dplyr",        # tidy data manipulation
  "glue",         # glue strings
  "readr"         # read csv files
)
safe_load_packages(required_packages)

## Themes
source("scripts/utils/MyCols.R")



### Data -----------------------------------------------------------------------
dat_efps <- read_csv(glue("data/inter/data_efps_clim_{efp_in}.csv"), show_col_types = F)
dat_emf <- read_csv(glue("data/inter/data_emf_{emf_in}.csv"), show_col_types = F)

# dat_years <- read_csv(glue("data/data4analysis_ByYears_{vers_in}.csv"), show_col_types = F)



### Isolate SITE_ID ------------------------------------------------------------
sites <- bind_rows(
  dat_efps %>% select(SITE_ID),
  dat_emf %>% select(SITE_ID),
  # dat_iav %>% select(SITE_ID)
  # dat_years %>% select(SITE_ID)
  ) %>%
  unique()
sites



### Save -----------------------------------------------------------------------
if (savedata) {
  write_csv(sites, "data/output/site_list_all.csv")
}


### End ------------------------------------------------------------------------
print("End of script.")