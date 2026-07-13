### Function -------------------------------------------------------------------
do_multimodel_relaimpo <- function(
    data = dat_norm, y_names = y_names, predictors = predictors,        # inputs
    vif_threshold = vif_threshold,                                      # parameters
    savedata = savedata, eval_file = eval_file, vers_out = vers_out     # output
) {
  ### Utilities ----------------------------------------------------------------
  required_packages <- c("dplyr", "MuMIn", "rlang", "tictoc", "tidyr")
  safe_load_packages(required_packages)
  
  source("scripts/functions/dredged_relaimpo.R")
  
  
  
  ### Loop EFPs ----------------------------------------------------------------
  relimp_all <- tibble() # initialize tibble of relative importance results
  vif_table <- tibble(variable = character())  # initialize VIF output table
  
  for (ii in 1:length(y_names)) {
    y_test <- rlang::sym(y_names[[ii]]) # current predicted variable
    
    ## 0) Test collinearity (VIF) ----
    ## Select parameters
    df_vifed <- data %>% 
      dplyr::select(
        !c(SITE_ID, !!!syms(y_names)), # 1) exclude SITE_ID and all predicted variables
        !!y_test,                      # 2) keep current predicted variable
        !!!syms(predictors)            # 3) keep all input predictors
      ) %>%
      tidyr::drop_na()
    
    # Number of datapoints for VIF
    n_vif <- nrow(df_vifed)
    
    ## Global options for na.action (needed)
    options(na.action = "na.fail")
    
    ## Announce
    txt <- glue::glue("==> Computing Variance Inflation Factor for {y_test} over {n_vif} data points.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    ## Run VIF
    n_excl <- 1 # initialize while index
    var_excl_list <- c()
    while (n_excl > 0) {
      ## Generalized Linear Model with all (remaining) predictors
      # (cf. normality assumption)
      formula1 <- as.formula(glue::glue("{y_test} ~ ."))
      fm1 <- lm(formula1, data = df_vifed) # generalized linear model
      
      ## VIF table: calculate variable inflation factor for each (remaining) predictor
      vif_temp <- bind_rows(vif(fm1)) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "VIF") %>% 
        print(n = Inf)
      
      ## Add to output VIF table
      vif_table <- vif_table %>%
        full_join(
          vif_temp %>% 
            rename("{paste0('VIF_', as.character(y_test))}" := VIF),
          by = "variable"
          )
      
      # check how many variables have vif above threshold
      n_excl <- vif_temp %>% 
        dplyr::filter(VIF > vif_threshold) %>%
        nrow()
      
      txt <- glue::glue("{n_excl} variables above inflation factor of {vif_threshold}.")
      print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
      
      if (n_excl > 0) {
        ## exclude variables with values over 10
        var_excl <- vif_temp %>%
          dplyr::filter(VIF > vif_threshold) %>% 
          dplyr::filter(VIF == max(VIF)) %>% # identify variable with highest vif
          pull(variable) %>% rlang::sym() # extract symbol of excluded variable
        
        var_excl_list <- c(var_excl_list, as.character(var_excl))
        
        txt <- glue::glue("Removing variable with highest vif value ({var_excl}).")
        print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
        
        df_vifed <- df_vifed %>% dplyr::select(-!!var_excl) # remove variable from list of predictors
      }
    }
    if (is.null(var_excl_list)) {var_excl_list <- "NONE"}
    txt <- glue::glue("Test of variance inflation factor excluded the following variables: {paste(var_excl_list, collapse = ', ')}.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    predictors_vifed <- predictors[predictors %in% (df_vifed %>% dplyr::select(-!!y_test) %>% names())]
    txt <- glue::glue("Included predictors: {paste(predictors_vifed, collapse = ', ')}.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    
    
    ## 1) Subset data ----
    txt <- glue::glue("==> Performing multimodel inference to explain {y_test}.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    
    ## 1.1) Extract predictors for specific EFP
    if (class(predictors_vifed)[1] == "tbl_df") {
      # Handle missing main predictors for certain EFP (main analysis)
      if (!y_names[[ii]] %in% unique(predictors_vifed$prediction)) {
        txt <- glue::glue("Model could not be performed for {y_test}: no input predictors.")
        print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
        next
      }
      
      # Extract predictors
      predictors_efp <- predictors_vifed %>% dplyr::filter(prediction == as.character(y_test)) %>% dplyr::pull(variable)
      
    } else {
      # Extract predictors
      predictors_efp <- predictors_vifed
    }
    
    
    # Extract variables for the model (select e.g. GPPSAT and predictors)
    df <- data %>%
      dplyr::select(SITE_ID, !!!predictors_efp, !!y_test) %>% # here it is important to only select one predicted variable, and all previously selected predictors (thorough)
      tidyr::drop_na() # important to drop NA (not accepted by dredge), since this step was removed from the whole dataframe processing
    
    
    ## 1.2) Save input data ----
    if (savedata) {
      efp_vers_out <- paste0(vers_out, "_", as.character(y_test))
      # Data
      write_csv(df, glue::glue("data/output/data_mumin_input_{efp_vers_out}.csv"))
      # Site list
      dat_sites <- df %>% dplyr::select(SITE_ID)
      write_csv(dat_sites, glue::glue("data/output/site_list_{efp_vers_out}.csv"))
    }
    df <- df %>% dplyr::select(-SITE_ID) # remove site index for numerical analysis
    
    
    ## 2) Model selection ----
    formula0 <- as.formula(glue::glue("{y_test} ~ ."))
    fm0 <- lm(formula0, data = df)
    
    
    ## 2.1) feed output(s) to dredge separately and confront AIC
    # NB: output size (and computing time!) of 'dredge' function increases exponentially with the number of predictors
    # look into 'pdredge' for a version with parallel computing
    # if (gb == 1 & sr == 1) {
    #   dd0 <- dredge(fm0, beta = "partial.sd", rank = AICc) # initialize output of loops
    # } else {
    # tic() # start timer
    
    # Automated model selection
    dd0 <- dredge(fm0, beta = "partial.sd", rank = AICc)
    
    ## 2.2) Extract ensemble of best models
    max_delta <- 4
    dlt40 <- subset(dd0, delta < max_delta) # Extract models with difference in AIC from best model's AIC < 2 or 4. Generally < 4 is suggested unless more restrictive
    
    
    ## 2.3) Summary
    summar <- summary(model.avg(object = dlt40, revised.var = FALSE)) # summarize results
    # The 'subset' (or 'conditional') average only averages over the models where the parameter appears.
    # An alternative, the 'full' average assumes that a variable is included in every model,
    # but in some models the corresponding coefficient (and its respective variance) is set to zero.
    # Unlike the 'subset average', the full average does not have a tendency of biasing the value away from zero.
    # The 'full' average is a type of shrinkage estimator, and for variables with a weak relationship
    # to the response it is smaller than 'subset' estimators.
    
    
    ## 3) Calculate importance of predictors ----
    ## Relative importance over all models (lmg) and weighted mean of lmg
    df <- data %>% # redefine model input data
      dplyr::select(!!!predictors_efp, !!y_test) %>% # here it is important to only select one predicted variable, and all previously selected predictors (through)
      tidyr::drop_na()
    
    ## Normal Analysis on AICc
    relimp <- tryCatch( # try to perform relative importance analysis, otherwise output warning and empty file
      {dredged_relaimpo(data = df, y = as.character(y_test), models = dlt40) %>% # variable values are weighted lmg; R2 is weighted over all models
          tidyr::pivot_longer(cols = !c(R2, AICc, RMSE), names_to = "variable", values_to = "rel_importance") %>%  # transform in long format with column for lmg
          dplyr::mutate(n = nrow(df)) %>%
          dplyr::relocate(variable, .before = R2)
        
      }, error = function(err) {
        txt <- glue::glue("Relative importance could not be correctly performed for {y_test}. Skipping current model.")
        warning(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
        return(tibble(variable = NA_character_,	R2 = NA_real_, AICc = NA_real_, RMSE = NA_real_, rel_importance = NA_real_, n = NA_real_))
        next
      })
    
    if (relimp[1,] %>% is.na() %>% sum() != length(relimp)) {
      txt <- glue::glue("Multimodel inference and relative importance based on Akaike's Information Criterion corrected for small sample sizes correctly performed for {y_test}.")
      print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    }
    
    
    ## 4) Prepare Output ----
    relimp_all <- bind_rows(
      relimp_all,
      bind_cols(
        prediction = as.character(y_test),
        relimp
        ) %>% 
        left_join(
          summar$coefmat.subset %>% # full averages: variables are assumed to be included in every model, and effects set to zero if missing.
            as_tibble(rownames = "variable") %>%
            dplyr::filter(variable != "(Intercept)") %>% 
            dplyr::rename(p_val = `Pr(>|z|)`) %>% # p-value of multimodel inference (p-value of relaimpo missing from output!)
            janitor::clean_names(),
          by = "variable"
          )
      ) %>% 
      relocate(c(R2, AICc, RMSE, n), .after = last_col())
    
  } # end for loop for multimodel inference + relative importance analysis on single EFPs
  
  
  ## Save VIF table ----
  if (savedata) {
    write_csv(vif_table, glue::glue("results/multimodel_inference/vif_{vers_out}.csv")) 
  }
  
  
  ### Output -------------------------------------------------------------------
  return(relimp_all)
} # end function


# ## Debug -----------------------------------------------------------------------
# debugonce(do_multimodel_relaimpo)