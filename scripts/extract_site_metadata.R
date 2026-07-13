#### EXTRACT IGBP CLASS AND SITE COORDINATES

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F: ")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
## Functions
source("scripts/functions/safe_load_packages.R")

## Packages
required_packages <- c(
  "dplyr",      # tidy data manipulation
  "janitor",    # clean names
  "readr",      # read csv files
  "stringr",    # tidy string manipulation
  "tidyr"       # clean and reshape tidy data
)
safe_load_packages(required_packages)




### Data files -----------------------------------------------------------------
## Site list
site_list <- read_csv("data/output/site_list_all.csv", show_col_types = F)

## Metadata from Jake
# for files in //minerva/BGI/work_1/scratch/fluxcom/sitecube_proc/model_files/
fluxcom_meta <- read_delim("data/input/flux_networks/metadata_fromJake.csv", delim = ";", show_col_types = F) %>% 
  rename(SITE_ID = `...1`, IGBP = pft) %>% 
  clean_names("all_caps")



### Fill missing DOIs ------------------------------------------------------
miss_dois <- fluxcom_meta %>% dplyr::filter(is.na(DOI))

fluxcom_meta <- fluxcom_meta %>% 
  mutate(
    DOI = case_when(
      SITE_ID == "CA-Gro" ~ "https://doi.org/10.17190/AMF/1902823",
      SITE_ID == "US-Me1" ~ "https://doi.org/10.17190/AMF/1902834",
      SITE_ID == "US-Me3" ~ "https://doi.org/10.17190/AMF/1902835",
      .default = DOI
    )
  )


### Output metadata ------------------------------------------------------------
dat_out <- site_list %>% 
  left_join(read_csv("data/igbp.csv", show_col_types = F), by = "SITE_ID") %>% 
  left_join(read_csv("data/coords.csv", show_col_types = F), by = "SITE_ID") %>% 
  left_join(fluxcom_meta, by = c("SITE_ID", "IGBP"))


### Save -----------------------------------------------------------------------
if (savedata) {
  write_csv(dat_out, glue::glue("results/all_sites_metadata_table.csv")) # sources and DOIs
}



### End ------------------------------------------------------------------------
print("End of script.")