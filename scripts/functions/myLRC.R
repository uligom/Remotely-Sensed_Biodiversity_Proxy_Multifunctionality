### Light Response Curve function for calculations (Mirco Migliavacca)
# added warning handling for predominant NA content (Ulisse Gomarasca, David Martini)

# Calculate light response curve fit:
myLRC <- function(data) {
  require(bigleaf)
  require(generics)
  require(rlang)
  
  if (sum(is.na(data$NEE))/length(data$NEE) >= 0.8) {
    warning("More than 80% of the data are NA. Not calculating light response curve and returning NA.")
    return(NA)
  } else if (sum(is.na(data$NEE))/length(data$NEE) < 0.8) {
    result = tryCatch({
      fitLRC <- bigleaf::light.response(as.data.frame(data), NEE = "NEE", Reco = "RECO", PPFD = "PPFD",
                               PPFD_ref = 2000) # variable names changed to this format in temporary input dataframe
      
      ## extract coefficients
      out <- tibble(
        LUE    = summary(fitLRC)[["coefficients"]][1], # alpha = ecosystem quantum yield (umol CO2 m-2 s-1) = slope of the light response curve, and is a measure for the light use efficiency of the canopy
        GPPsat = summary(fitLRC)[["coefficients"]][2] # GPPsat estimate, i.e., GPP_ref = GPP at the reference PPFD (usually at saturating light)
        )
      return(out)
    }, error = function(err) {
      warning("Error in the light response curve function. Returning NA.")
      out <- tibble(
        LUE    = NA_real_,
        GPPsat = NA_real_
      )
      return(out)
    })
  }
}

# ### Debug -----------------------
# debugonce(myLRC)