#### FORMATTED LABELS FOR PLOTTING, INCL. UNITS

### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)


### Labels without units -------------------------------------------------------
## Named vector of expressions
efp_labels <- c(
  CUEeco = expression("CUE"[eco]),
  E0     = expression("E0"),
  EF     = expression("EF"),
  EFampl = expression("EF"[ampl]),
  GPPsat = expression("GPP"[sat]),
  Gsmax  = expression("Gs"[max]),
  LUE    = expression("LUE"),
  NEPmax = expression("NEP"[max]),
  Rb     = expression("Rb"),
  Rbmax  = expression("Rb"[max]),
  uWUE   = expression("WUE"),
  WUE    = expression("uWUE")
)

emf_labels <- c(
  EMFavg = expression("EMF"[average]),
  EMFthr = expression("EMF"[threshold])
)

predictor_labels <- c(
  LAImax    = expression("LAI"[max]),
  Hc        = expression("Hc"),
  VPD       = expression("VPD"),
  Temp      = expression("Temperature"),
  SWin      = expression("SW"[IN]),
  Precip    = expression("Precipitation"),
  RaoQ_NIRv = expression("RaoQ"[NIRv])
)

## Single labels
CUEeco_label <- expression("CUE"[eco])
E0_label     <- expression("E0")
EF_label     <- expression("EF")
EFampl_label <- expression("EF"[ampl])
GPPsat_label <- expression("GPP"[sat])
Gsmax_label  <- expression("Gs"[max])
LUE_label    <- expression("LUE")
NEPmax_label <- expression("NEP"[max])
Rb_label     <- expression("Rb")
Rbmax_label  <- expression("Rb"[max])
uWUE_label   <- expression("WUE")
WUE_label    <- expression("uWUE")



### Labels with units ----------------------------------------------------------
## Named vector of expressions
efp_units <- c(
  CUEeco = expression(paste(CUE[eco], " [-]")),
  E0     = expression(paste("E0 [K]")),
  EF     = expression(paste("EF [-]")),
  EFampl = expression(paste(EF[ampl], " [-]")),
  LUE    = expression(paste("LUE [-]")),
  GPPsat = expression(paste(GPP[sat], " [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]")),
  Gsmax  = expression(paste(Gs[max], " [",m," ", s^{-1},"]")),
  NEPmax = expression(paste(NEP[max], " [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]")),
  Rb     = expression(paste("Rb [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]")),
  Rbmax  = expression(paste("Rbmax [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]")),
  uWUE   = expression(paste("WUE [",gC," ", kgH[2],O^{-1},"]")),
  WUE    = expression(paste("uWUE [",gC," ", kgH[2],O^{-1},"]"))
)

raoq_units <- c(
  RaoQ_S2   = expression(paste(RaoQ[bands], " [-]")),
  RaoQ_NDVI = expression(paste(RaoQ[NDVI], " [-]")),
  RaoQ_NIRv = expression(paste(RaoQ[NIRv], " [-]"))
)

## Single labels
CUEeco_unit <- expression(paste(CUE[eco], " [-]"))
E0_unit     <- expression(paste("E0 [K]"))
EF_unit     <- expression(paste("EF [-]"))
EFampl_unit <- expression(paste(EF[ampl], " [-]"))
LUE_unit    <- expression(paste("LUE [-]"))
GPPsat_unit <- expression(paste(GPP[sat], " [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]"))
Gsmax_unit  <- expression(paste(Gs[max], " [",m," ", s^{-1},"]"))
NEPmax_unit <- expression(paste(NEP[max], " [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]"))
Rb_unit     <- expression(paste("Rb [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]"))
Rbmax_unit  <- expression(paste("Rbmax [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]"))
uWUE_unit   <- expression(paste("WUE [",gC," ", kgH[2],O^{-1},"]"))
WUE_unit    <- expression(paste("uWUE [",gC," ", kgH[2],O^{-1},"]"))