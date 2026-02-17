#' Helper function to weight and correct the risk and scores
#'
#' @param x The risk or score value to be weighted
#'
#' @examples
#' cf_ind_importance(x = 0.5)
#' cf_ind_importance(x = c(0.1,0.5,1.5))
#'
#' @return
#' A transformed variable after applying a inverse weighing function so that lower values will gain more impact when applied in a weighed.mean function. A numeric value.
#'
#' @export
cf_ind_importance <- function(x) {
  y <- 1 / (x  + 0.2)
  
  return(y)
}


#' Function to check user weather table and, if not supplied, insert default weather
#'
#' @param dt (data.table) Monthly weather table with the following columns:
#' * year (optional; if absent, weather is replicated across the simulation period)
#' *month (1 - 12)
#' *W_TEMP_MEAN_MONTH (°C)
#' *W_PREC_SUM_MONTH (mm)
#' *W_ET_REF_MONTH (mm)
#' *W_ET_ACT_MONTH (mm; optional, can be NA)
#' *W_ET_REFACT (fraction; optional, defaults to 0.75 if missing or NA and W_ET_REF_MONTH is supplied)
#' If not supplied, default monthly weather based on the Netherlands is added
#' @param dt.time Table with all year and month combinations of the simulation period. Must contain columns year and month. Created using \link{rc_time_period}
#' 
#' @returns
#' A data table returning in order the columns year, month, W_TEMP_MEAN_MONTH, W_PREC_SUM_MONTH, W_ET_REF_MONTH, W_ET_ACT_MONTH, W_ET_REFACT
#' covering the simulation period. When year is not supplied in dt, rows are expanded for all years.
#' @export
#'
rc_update_weather <- function(dt = NULL, dt.time = dt.time){
  # Add visible bindings
  W_ET_REFACT = W_ET_ACT_MONTH = W_ET_REF_MONTH = . = time = NULL
  
  # Validate dt.time 
  checkmate::assert_data_table(dt.time, min.rows = 1)
  checkmate::assert_names(names(dt.time), must.include = c("year", "month"))
  checkmate::assert_integerish(dt.time$year, lower = 1, any.missing = FALSE)
  checkmate::assert_integerish(dt.time$month, lower = 1, upper = 12, any.missing = FALSE)
  
  # Check if there is a weather data table
  if(!is.null(dt)){
    
    # Create weather data table
    dt <- copy(dt)
    
    # Check inputs
    checkmate::assert_data_table(dt, min.rows = 12)
    req <- c("month", "W_TEMP_MEAN_MONTH", "W_PREC_SUM_MONTH")
    checkmate::assert_names(colnames(dt), must.include = req)
    checkmate::assert(
      any(c("W_ET_REF_MONTH", "W_ET_ACT_MONTH") %in% names(dt)),
      msg = "At least one of 'W_ET_REF_MONTH' or 'W_ET_ACT_MONTH' must be provided."
    )
    checkmate::assert_integerish(dt$month, lower = 1, upper = 12, any.missing = FALSE)
    checkmate::assert_numeric(dt$W_TEMP_MEAN_MONTH, lower = rc_minval('W_TEMP_MEAN_MONTH'), upper = rc_maxval('W_TEMP_MEAN_MONTH'), any.missing = FALSE)
    checkmate::assert_numeric(dt$W_PREC_SUM_MONTH, lower = rc_minval('W_PREC_SUM_MONTH'), upper = rc_maxval('W_PREC_SUM_MONTH'), any.missing = FALSE)
    
    # Check if both reference and actual ET are provided
    if ("W_ET_REF_MONTH" %in% colnames(dt) && "W_ET_ACT_MONTH" %in% colnames(dt)) {
      checkmate::assert(
        !all(is.na(dt$W_ET_REF_MONTH)) || !all(is.na(dt$W_ET_ACT_MONTH)),
        msg = "At least one of W_ET_REF_MONTH and W_ET_ACT_MONTH should not contain NA values."
      )
      # Check ranges, allow NAs
      checkmate::assertNumeric(dt$W_ET_REF_MONTH, lower = rc_minval('W_ET_REF_MONTH'), upper = rc_maxval('W_ET_REF_MONTH'), any.missing = TRUE)
      checkmate::assertNumeric(dt$W_ET_ACT_MONTH, lower = rc_minval('W_ET_ACT_MONTH'), upper = rc_maxval('W_ET_ACT_MONTH'), any.missing = TRUE)
    } else if ("W_ET_REF_MONTH" %in% colnames(dt)) {
      # Only reference ET provided: no NA allowed
      checkmate::assertNumeric(dt$W_ET_REF_MONTH, lower = rc_minval('W_ET_REF_MONTH'), upper = rc_maxval('W_ET_REF_MONTH'), any.missing = FALSE)
    } else if ("W_ET_ACT_MONTH" %in% colnames(dt)) {
      # Only actual ET provided: no NA allowed
      checkmate::assertNumeric(dt$W_ET_ACT_MONTH, lower = rc_minval('W_ET_ACT_MONTH'), upper = rc_maxval('W_ET_ACT_MONTH'), any.missing = FALSE)
    }
    
    # check if year is supplied
    if("year" %in% colnames(dt)){
      checkmate::assert_integerish(dt$year, lower = 1, any.missing = FALSE)
      
      # Ensure missing ET column exists (as NA) to keep schema stable
      if (!"W_ET_REF_MONTH" %in% names(dt)) dt[, W_ET_REF_MONTH := NA_real_]
      if (!"W_ET_ACT_MONTH" %in% names(dt)) dt[, W_ET_ACT_MONTH := NA_real_]
      
      # Enforce complete coverage of simulation window
      off_time <- dt.time[!dt, on = .(year, month)]
      checkmate::assert_true(nrow(off_time) == 0,
                             .var.name = "weather must contain all months in the simulation window")
      
     
      # check that there are no duplicate year/month combinations
      checkmate::assert_true(!any(duplicated(dt, by = c("year", "month"))))
     
      # set weather table to cover simulation period and order
      dt <- dt[dt.time, on = .(year, month)]
      
      # remove time column
      if("time" %in% names(dt)) dt[, time := NULL]
      
      setcolorder(dt, c("year","month","W_TEMP_MEAN_MONTH","W_PREC_SUM_MONTH","W_ET_REF_MONTH","W_ET_ACT_MONTH"))
      
    }else{
      # Require full monthly coverage when no 'year' is present
      checkmate::assert(setequal(unique(dt$month), 1:12) && nrow(dt) == 12,
                                    msg = "When 'year' is absent, provide exactly one row per month (12 rows)")
  
    
        # Ensure missing ET column exists (as NA) to keep schema stable
          if (!"W_ET_REF_MONTH" %in% names(dt)) dt[, W_ET_REF_MONTH := NA_real_]
          if (!"W_ET_ACT_MONTH" %in% names(dt)) dt[, W_ET_ACT_MONTH := NA_real_]
        setkey(dt, month)
        dt <- dt[dt.time, on = .(month)]
        
        # remove time column
        if("time" %in% names(dt)) dt[, time := NULL]
        
        # Reorder columns
          setcolorder(dt, c("year","month","W_TEMP_MEAN_MONTH","W_PREC_SUM_MONTH","W_ET_REF_MONTH","W_ET_ACT_MONTH"))
          
         }
    
    # Define values of ET correction factor
    if("W_ET_REF_MONTH" %in% colnames(dt)){
      
      # create W_ET_REFACT column if non-existent
      if (!"W_ET_REFACT" %in% colnames(dt)) dt[, W_ET_REFACT := NA_real_]
      
      # set NA W_ET_REFACT values to 0.75
      dt[, W_ET_REFACT := fifelse(is.na(W_ET_REFACT), 0.75, W_ET_REFACT)]
      
      # check supplied values
      checkmate::assert_numeric(dt$W_ET_REFACT, lower = rc_minval('W_ET_REFACT'), upper = rc_maxval('W_ET_REFACT'))
    }
}else{
  # If no weather data has been supplied, set to standard Dutch conditions
  # Set default weather
  dt <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                   W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                   W_ET_ACT_MONTH = NA_real_,
                   W_ET_REFACT = rep(0.75, 12)
                   )
 
  # Join to time table
  dt <- dt[dt.time, on = .(month)]
  
  # remove time column
  if("time" %in% names(dt)) dt[, time := NULL]
  
  # Reorder columns
  setcolorder(dt, c("year","month","W_TEMP_MEAN_MONTH","W_PREC_SUM_MONTH","W_ET_REF_MONTH","W_ET_ACT_MONTH"))
}

  return(dt)
}



#' Function to check user given RothC simulation parameters, or provide defaults if none are given
#'
#' @param parms (list) List containing the columns dec_rates, c_fractions, initialisation_method, unit, method, poutput, start_date, end_date
#' @param crops (data.table) Data table with crop rotation information. Should at least contain the columns B_LU_START (YYYY-MM-DD) and B_LU_END (YYYY-MM-DD). If start_date and end_date are not supplied in parms, at least one of crops and amendments required. 
#' @param amendments (data.table) Data table with amendment input information. Should at least contain the column P_DATE_FERTILIZATION (YYYY-MM-DD). If start_date and end_date are not supplied in parms, at least one of crops and amendments required. 
#' 
#' @returns
#' A list containing parameters to run the RothC simulation, with columns dec_rates, c_fractions, initialisation_method, unit, method, poutput, start_date, end_date
#' 
#' 
#' @export
#'
#' 
rc_update_parms <- function(parms = NULL, crops = NULL, amendments = NULL){
  
 
  # Add visible bindings
  c_fractions = start_date = end_date = NULL
  
  # Checks names parms
  if(!is.null(parms)){
    checkmate::assert_list(parms)
    checkmate::assert_subset(names(parms), choices = c("dec_rates", "c_fractions", "initialisation_method", "unit", "method", "poutput", "start_date", "end_date"), empty.ok = TRUE)
  }else{
    parms <- list()
  }
  
  # add checks on decomposition rates
  # Define default decomposition rates
  k1 = 10; k2 = 0.3; k3 = 0.66; k4 = 0.02
  
  # if dec_rates supplied, check inputs and overwrite defaults
  if(!is.null(parms$dec_rates)){
    # check supplied decomposition rates
    checkmate::assert_numeric(parms$dec_rates, lower = 0, upper = 30, min.len = 1,max.len = 4)
    checkmate::assert_subset(names(parms$dec_rates),choices = c("k1", "k2", "k3", "k4"),empty.ok = TRUE)
    
    # Use supplied decomposition rates
    rcp <-  c(names(parms$dec_rates),colnames(parms$dec_rates))
    if('k1' %in% rcp){k1 <- parms$dec_rates[['k1']]}
    if('k2' %in% rcp){k2 <- parms$dec_rates[['k2']]}
    if('k3' %in% rcp){k3 <- parms$dec_rates[['k3']]}
    if('k4' %in% rcp){k4 <- parms$dec_rates[['k4']]}
  } 
  


  # add checks on c_fractions
  # define default c-fractions
  fr_IOM = 0.049; fr_DPM = 0.015; fr_RPM = 0.125; fr_BIO = 0.015
  
  # if c_fractions supplied, check input and overwrite defaults
  if(!is.null(parms$c_fractions)){
    # check inputs: initial C distribution over pools
    checkmate::assert_numeric(parms$c_fractions, lower = 0, upper = 1, any.missing = TRUE, 
                              min.len = 1, max.len = 4, null.ok = FALSE)
    checkmate::assert_subset(names(parms$c_fractions),choices = c("fr_IOM", "fr_DPM", "fr_RPM", "fr_BIO"),empty.ok = TRUE)
    
    # remove NA c_fractions
    parms$c_fractions <- parms$c_fractions[!is.na(parms$c_fractions)]
    
    # Use supplied distribution
    rcp <-  c(names(parms$c_fractions))
    if('fr_IOM' %in% rcp){fr_IOM <- parms$c_fractions[['fr_IOM']]}
    if('fr_DPM' %in% rcp){fr_DPM <- parms$c_fractions[['fr_DPM']]}
    if('fr_RPM' %in% rcp){fr_RPM <- parms$c_fractions[['fr_RPM']]}
    if('fr_BIO' %in% rcp){fr_BIO <- parms$c_fractions[['fr_BIO']]}
    
    # check supplied fractions do not exceed 1
    # Note: fr_IOM is excluded from sum check as it's a coefficient in the power-law
    # formula (CIOM = fr_IOM * toc^1.139), while other fractions partition the
    # reactive C pool (toc - CIOM)
    if ((fr_DPM + fr_RPM + fr_BIO) > 1 + 1e-10) {
      stop("Sum of c_fractions (fr_DPM + fr_RPM + fr_BIO) exceeds 1; please reduce one or more fractions.")
      }
  }
  
 
  
  # Check the format of start_date and end_date
  # create empty variables
  start_date <- NULL
  end_date <- NULL
  
  # Check and read supplied values
  if(!is.null(parms$start_date)){
    # check start_date 
    checkmate::assert_date(as.Date(parms$start_date))
    
    start_date <- parms$start_date
  }
  
  if(!is.null(parms$end_date)){
    checkmate::assert_date(as.Date(parms$end_date)) 
    
    end_date <- parms$end_date
  }
  # Check if one of start_date or end_date is not supplied, derive from crop/amendment data
  if (is.null(start_date) || is.null(end_date)) {
  dates <- c(
    if (!is.null(crops)) as.Date(crops$B_LU_START) else as.Date(character()),
    if (!is.null(crops)) as.Date(crops$B_LU_END) else as.Date(character()),
    if (!is.null(amendments)) as.Date(amendments$P_DATE_FERTILIZATION) else as.Date(character())
    )
  
  dates <- dates[!is.na(dates)]
  if(length(dates) == 0){
    stop("No dates found in crops/amendments to derive missing start_date/end_date; supply parms$start_date/end_date.")
  }
  if (is.null(start_date)) start_date <- min(dates)
  if (is.null(end_date)) end_date   <- max(dates)
}
 
# Check is values are logical
  if(as.Date(start_date) > as.Date(end_date)) stop('Start_date is not before end_date')
  
  
  # add checks on initialisation_method
  initialisation_method <- 'none'
  
  if(!is.null(parms$initialisation_method)){
    # check type
    checkmate::assert_character(parms$initialisation_method,any.missing = FALSE, len = 1)
    checkmate::assert_choice(parms$initialisation_method, choices = c(
      'spinup_analytical_bodemcoolstof',
      'spinup_analytical_heuvelink',
      'spinup_simulation',
      'none'))
   
    # define type
    initialisation_method <- parms$initialisation_method

  }
  
  # Add checks on unit
  unit <- "A_SOM_LOI"
  
  if(!is.null(parms$unit)){
      # check output format
    checkmate::assert_character(parms$unit,len=1)
    checkmate::assert_choice(parms$unit, choices = c(
      'A_SOM_LOI',
      'psoc',
      'cstock',
      'psomperfraction'
      ))
    
    unit <- parms$unit
    
  }
  
  
  # add checks on method
  method <- "adams"
  if(!is.null(parms$method)){
    # check supplied method
    checkmate::assert_choice(parms$method, choices = c("adams"))
    # define method
    method = parms$method
  }
  
  # add checks on poutput
  # set pouput to 'month'
  poutput <- 'month'
  if(!is.null(parms$poutput)){
    # check supplied poutput
    checkmate::assert_character(parms$poutput, len=1)
    checkmate::assert_choice(parms$poutput, choices = c('year', 'month'))
    
    poutput <- parms$poutput
  }
  
  out = list(initialisation_method = initialisation_method,
             dec_rates = c(k1 = k1, k2 = k2, k3 = k3, k4 = k4),
             c_fractions = c(fr_IOM = fr_IOM, fr_DPM = fr_DPM, fr_RPM = fr_RPM, fr_BIO = fr_BIO),
             unit = unit,
             method = method,
             poutput = poutput,
             start_date = start_date,
             end_date = end_date)
  
  # return output
  return(out)
}

#' Function to check input tables of soil, crop, and amendment data
#'
#' @param soil_properties (list) List with soil properties: A_C_OF, soil organic carbon content (g/kg) or B_C_ST03, soil organic carbon stock (Mg C/ha), preferably for soil depth 0.3 m; A_CLAY_MI, clay content (\%); A_DENSITY_SA, dry soil bulk density (g/cm3)
#' @param rothc_rotation (data.table) Table with crop rotation details and crop management actions that have been taken. Includes also crop inputs for carbon. See details for desired format.
#' @param rothc_amendment (data.table) A table with amendment data. Includes at least the columns P_DATE_FERTILIZATION and P_HC, and optionally P_DOSE, P_C_OF and/or B_C_OF_AMENDMENT.
#'
#' @returns
#' Error messages indicating if input data is not in order
#' 
#' @export
#'
rc_check_inputs <- function(soil_properties,
                            rothc_rotation = NULL,
                            rothc_amendment = NULL){
  
  # Add visual bindings
  
  # Check soil properties
  checkmate::assert_data_table(soil_properties)
  if(length(soil_properties$A_C_OF) != 0)  checkmate::assert_numeric(soil_properties$A_C_OF, lower = rc_minval('A_C_OF'), upper = rc_maxval('A_C_OF'), any.missing = FALSE, len = 1)
  if(length(soil_properties$B_C_ST03) != 0)  checkmate::assert_numeric(soil_properties$B_C_ST03, lower = rc_minval('B_C_ST03'), upper = rc_maxval('B_C_ST03'), any.missing = FALSE, len = 1)
  if((length(soil_properties$A_C_OF) == 0 || is.na(soil_properties$A_C_OF)) &&
     (length(soil_properties$B_C_ST03) == 0 || is.na(soil_properties$B_C_ST03))){
       stop('Both A_C_OF and B_C_ST03 are missing in soil_properties')}
  checkmate::assert_numeric(soil_properties$A_CLAY_MI, lower = rc_minval('A_CLAY_MI'), upper = rc_maxval('A_CLAY_MI'), len = 1)
  checkmate::assert_numeric(soil_properties$A_DENSITY_SA, lower = rc_minval('A_DENSITY_SA'), upper = rc_maxval('A_DENSITY_SA'), len = 1)
  
  # Check crop properties if supplied
  if(!is.null(rothc_rotation)){
    checkmate::assert_data_table(rothc_rotation, null.ok = TRUE, min.rows = 1)

    req <- c("B_LU_START", "B_LU_END","B_LU_HC","B_C_OF_CULT")
    checkmate::assert_names(colnames(rothc_rotation), must.include = req)
    
    checkmate::assert_numeric(rothc_rotation$B_LU_HC, lower = rc_minval('B_LU_HC'), upper = rc_maxval('B_LU_HC'), any.missing = FALSE)
    checkmate::assert_numeric(rothc_rotation$B_C_OF_CULT, lower = rc_minval('B_C_OF_CULT'), upper = rc_maxval('B_C_OF_CULT'), any.missing = FALSE)
    checkmate::assert_date(as.Date(rothc_rotation$B_LU_START), any.missing = F)
    checkmate::assert_date(as.Date(rothc_rotation$B_LU_END), any.missing = F)
    if(any(rothc_rotation$B_LU_START > rothc_rotation$B_LU_END)) {
      bad_rows <- which(rothc_rotation$B_LU_START > rothc_rotation$B_LU_END)
      stop(sprintf('Crop start date after end date in row: %s', paste(bad_rows, collapse=", ")))
    }
    }

  # Check amendment properties if supplied
  if(!is.null(rothc_amendment)){
    checkmate::assert_data_table(rothc_amendment, null.ok = TRUE, min.rows = 1)
    
    req <- c("P_HC","P_DATE_FERTILIZATION")
    checkmate::assert_names(colnames(rothc_amendment), must.include = req)
    
    checkmate::assert_date(as.Date(rothc_amendment$P_DATE_FERTILIZATION), any.missing = FALSE)
    checkmate::assert_numeric(rothc_amendment$P_HC, lower = rc_minval('P_HC'), upper = rc_maxval('P_HC'), any.missing = FALSE)
    if ("P_NAME" %in% names(rothc_amendment))
      checkmate::assert_character(rothc_amendment$P_NAME, any.missing = TRUE)
    if ("P_DOSE" %in% names(rothc_amendment))
       checkmate::assert_numeric(rothc_amendment$P_DOSE, lower = rc_minval('P_DOSE'), upper = rc_maxval('P_DOSE'), any.missing = TRUE)
    if ("P_C_OF" %in% names(rothc_amendment))
      checkmate::assert_numeric(rothc_amendment$P_C_OF, lower = rc_minval('P_C_OF'), upper = rc_maxval('P_C_OF'), any.missing = TRUE)
    if ("B_C_OF_AMENDMENT" %in% names(rothc_amendment))
      checkmate::assert_numeric(rothc_amendment$B_C_OF_AMENDMENT, lower = rc_minval('B_C_OF_AMENDMENT'), upper = rc_maxval('B_C_OF_AMENDMENT'), any.missing = TRUE)
  }
}


#' Function to calculate the dry soil bulk density based on Dutch pedotransfer functions
#'
#' @param dt (data table) Contains the columns A_CLAY_MI (clay content \%) and at least one of A_SOM_LOI (organic matter content, \%) and A_C_OF (organic carbon content, g C/kg)
#'
#' @returns
#' Data table with the dry soil bulk density (g/cm3)
#' 
#' @export
rc_calculate_bd <- function(dt){
  # add visible bindings
  dens.sand = A_SOM_LOI = A_C_OF = dens.clay = cf = A_CLAY_MI = A_DENSITY_SA = NULL

  # Check input data
  checkmate::assert_subset(colnames(dt), choices = c("A_SOM_LOI", "A_CLAY_MI", "A_C_OF"))
  if(is.null(dt$A_SOM_LOI)){
    checkmate::assert_numeric(dt$A_C_OF, lower = rc_minval('A_C_OF'), upper = rc_maxval('A_C_OF'), any.missing = F)
  }else{
  checkmate::assert_numeric(dt$A_SOM_LOI, lower = rc_minval('A_SOM_LOI'), upper = rc_maxval('A_SOM_LOI'), any.missing = F)
  }
  checkmate::assert_numeric(dt$A_CLAY_MI, lower = rc_minval('A_CLAY_MI'), upper = rc_maxval('A_CLAY_MI'), any.missing = F)
  
  # Add copy of data table
  dt <- copy(dt)
  
  # calculate soil texture dependent density (CBAV, 2019)
  if(!is.null(dt$A_SOM_LOI)){
  dt[, dens.sand := (1 / (0.02525 * A_SOM_LOI + 0.6541))]
  dt[, dens.clay :=  (0.00000067*A_SOM_LOI^4 - 0.00007792*A_SOM_LOI^3 + 0.00314712*A_SOM_LOI^2 - 0.06039523*A_SOM_LOI + 1.33932206)]
  }else{
    dt[, dens.sand := (1 / (0.02525 * (A_C_OF*2/10) + 0.6541))]
    dt[, dens.clay :=  (0.00000067*(A_C_OF*2/10)^4 - 0.00007792*(A_C_OF*2/10)^3 + 0.00314712*(A_C_OF*2/10)^2 - 0.06039523*(A_C_OF*2/10) + 1.33932206)]
  }
  
  # fraction clay correction
  dt[, cf := pmin(1, A_CLAY_MI/25)]
  
  # clay dependent density
  dt[, A_DENSITY_SA := cf * dens.clay + (1-cf) * dens.sand]
  return(dt)
}

#' Function to calculate the C input of your crop
#'
#' @param dt (data table) Table with crop rotation details and crop management actions that have been taken. For more information see details.
#'
#' @returns
#' Data table with total C input from crops
#' 
#' @details
#' Crop data table
#' Contains the following columns:
#' * B_LU_YIELD (numeric), the mean crop yield (kg dry matter/ha)
#' * B_LU_HI (numeric), the harvest index of the crop
#' * B_LU_HI_RES (numeric), fraction of biomass that is residue
#' * B_LU_RS_FR (numeric), Root-to-shoot ratio of the crop
#' * M_CROPRESIDUE (logical), indicator of whether crop residue is incorporated into the soil
#' 
#' @export
rc_calculate_bcof <- function(dt){
  # Add visible bindings
  cin_aboveground = B_LU_YIELD = B_LU_HI = cin_roots = B_LU_RS_FR = NULL
  cin_residue = M_CROPRESIDUE = B_LU_HI_RES = B_C_OF_CULT = NULL
  
  # Check input data
  req <- c("B_LU_YIELD", "B_LU_HI", "B_LU_HI_RES", "B_LU_RS_FR", "M_CROPRESIDUE")
  checkmate::assert_names(colnames(dt), must.include = req)
  checkmate::assert_numeric(dt$B_LU_YIELD, lower = rc_minval('B_LU_YIELD'), upper = rc_maxval('B_LU_YIELD'), any.missing = F)
  checkmate::assert_numeric(dt$B_LU_HI, lower = rc_minval('B_LU_HI'), upper = rc_maxval('B_LU_HI'), any.missing = F)
  checkmate::assert_numeric(dt$B_LU_HI_RES, lower = rc_minval('B_LU_HI_RES'), upper = rc_maxval('B_LU_HI_RES'), any.missing = F)
  checkmate::assert_numeric(dt$B_LU_RS_FR, lower = rc_minval('B_LU_RS_FR'), upper = rc_maxval('B_LU_RS_FR'), any.missing = F)
  checkmate::assert_logical(dt$M_CROPRESIDUE)
  
  # Make copy of input table
  dt.crop <- copy(dt)
  
  # Calculate C inputs from roots and crop residue (kg C/ha)
  dt.crop[, cin_aboveground := B_LU_YIELD / B_LU_HI * 0.5]
  dt.crop[, cin_roots := cin_aboveground * B_LU_RS_FR]
  dt.crop[, cin_residue := fifelse(M_CROPRESIDUE, cin_aboveground * B_LU_HI_RES, 0)]
  dt.crop[, B_C_OF_CULT := cin_roots + cin_residue]
  
return(dt.crop)
}



#' Function to extend the user crop input file to cover the full range of simulation years
#'
#' @param crops (data.table) Input crop table for the RothC model. See details for further information
#' @param simyears (numeric) number of simulation years of the RothC model
#' @param start_date (character, formatted YYYY-MM-DD) Date in which simulation starts
#' @param end_date (character, formatted YYYY-MM-DD) Required if simyears is not supplied
#'
#' @returns
#' An extended crop input file to be used in the rotsee package
#' 
#' @details
#' Crops: crop table
#' Includes the columns: 
#' * B_LU_START (start of crop rotation),
#' * B_LU_END (end of crop rotation),
#' * B_LU (a crop id), 
#' * B_LU_NAME (a crop name, optional),
#' * B_LU_HC, the humification coefficient of crop organic matter (-). When not supplied, default RothC value will be used
#' * B_C_OF_CULT, the organic carbon input on field level (kg C/ha)
#' 
#' 
#' @export
#'
rc_extend_crops <- function(crops,start_date, end_date = NULL, simyears = NULL){
  # add visible bindings
  id = B_LU_START = B_LU_END = yr_rep = year_start = year_end = NULL
  year_start_ext = year_end_ext = NULL
  
  # Check input data
  checkmate::assert_data_table(crops,null.ok = FALSE, min.rows = 1)
  # Copy crop table and standardize column names
  crops <- as.data.table(crops)
  setnames(crops,toupper(colnames(crops)))
  
  req <- c("B_LU_START", "B_LU_END",  "B_LU_HC", "B_C_OF_CULT")
  checkmate::assert_names(colnames(crops), must.include = req)
  if ("B_LU_NAME" %in% names(crops)) checkmate::assert_character(crops$B_LU_NAME, any.missing = FALSE)
  checkmate::assert_numeric(crops$B_LU_HC, lower = rc_minval('B_LU_HC'), upper = rc_maxval('B_LU_HC'), any.missing = F)
  checkmate::assert_numeric(crops$B_C_OF_CULT, lower = rc_minval('B_C_OF_CULT'), upper = rc_maxval('B_C_OF_CULT'), any.missing = F)
  checkmate::assert_date(as.Date(crops$B_LU_START), any.missing = F)
  checkmate::assert_date(as.Date(crops$B_LU_END), any.missing = F)
  checkmate::assert_date(as.Date(start_date))
  checkmate::assert(
    !any(grepl("-02-29$", c(crops$B_LU_START, crops$B_LU_END))),
    msg = "February 29th dates are not allowed in B_LU_START or B_LU_END to avoid leap year complications during date shifting"
  )
  if(!is.null(end_date)){checkmate::assert_date(as.Date(end_date))}
  if(!is.null(simyears)){checkmate::assert_numeric(simyears, lower = 1, len = 1, any.missing = FALSE)}
  if(is.null(end_date) && is.null(simyears)) stop('both end_date and simyears are missing in the input')
  if(max(year(crops$B_LU_END)) < year(start_date))  stop('crop rotation plan is outside of simulation period')
  if(any(crops$B_LU_START >= crops$B_LU_END)) stop('Crop end date must be after crop start date')
  
  
  # Define total length of crop rotation (years)
  rotation_length <- max(year(crops$B_LU_END)) - year(start_date) + 1
  
  # Add an unique ID
  crops[,id := .I]
  
  # Define simyears if not supplied
  if(is.null(simyears)){
    months_diff <- 12L * (year(end_date) - year(start_date)) + (month(end_date) - month(start_date)) + 1L
        simyears <- months_diff / 12
      }
  
  # Determine number of duplications
  duplications <- max(1L, ceiling(simyears / rotation_length))
  
  # Extend crop table for the number of years
  crops_ext <- crops[rep(id, each = duplications)]
 
  # Update  B_LU_START and B_LU_END for all repetitions of rotation block
  # Define start year for each repetition
  crops_ext[,`:=` (year_start = year(B_LU_START), year_end = year(B_LU_END))]
  
  # create helper column to identify years of repetition
  crops_ext[,yr_rep := 1:.N, by = id]
  
  # Recalculate start years based on length of crop rotation and number of repetitions
  crops_ext[,`:=` (year_start_ext = year_start + (yr_rep - 1) * ceiling(rotation_length),
                      year_end_ext = year_end + (yr_rep - 1) * ceiling(rotation_length))]
 
  # Update B_LU_START and B_LU_END
  crops_ext[, `:=` (B_LU_START = paste0(year_start_ext,substr(B_LU_START, start = 5, stop = 10)),
                       B_LU_END = paste0(year_end_ext,substr(B_LU_END, start = 5, stop = 10)))]

  # filter only the years for simulation
  if (!is.null(end_date)) {
     this.crops <- crops_ext[as.Date(B_LU_START) <= as.Date(end_date), ]
    } else {
      this.crops <- crops_ext[year_start_ext <= (year(start_date) + simyears),]
    }
  
  # Select relevant columns
  this.crops <- this.crops[, .SD, .SDcols =!names(this.crops) %in% c("id", "year_start", "year_end",
                                                       "yr_rep", "year_start_ext", "year_end_ext")
                                 ]
  
  # order crop file
  setorder(this.crops, B_LU_START)

  return(this.crops)
  
}
  


#' Function to extend the user amendments input file to cover the full range of simulation years
#'
#' @param amendments (data table) Input amendments table for the RothC model. See details for further information
#' @param simyears (numeric) number of simulation years of the RothC model
#' @param start_date (date, formatted YYYY-MM-DD) Date in which simulation starts
#' @param end_date (date, formatted YYYY-MM-DD) Required if simyears is not supplied
#'
#' @returns
#' An extended amendment input file to be used in the rotsee package
#' 
#' @details
#' amendments: amendment table
#' Includes the columns:
#' * P_ID (character), ID of the soil amendment product
#' * P_NAME (character), name of the soil amendment product, optional
#' * B_C_OF_AMENDMENT (numeric), the organic carbon input from soil amendment product on a field level (kg C/ha)
#' * P_DOSE (numeric), applied dose of soil amendment product (kg/ha), required if B_C_OF_AMENDMENT is not supplied
#' * P_C_OF (numeric), organic carbon content of the soil amendment product (g C/kg), required if B_C_OF_AMENDMENT is not supplied
#' * P_HC (numeric), the humification coefficient of the soil amendment product (fraction)
#' * P_DATE_FERTILIZATION (date), date of fertilizer application (formatted YYYY-MM-DD)
#' 
#' @export
#'
rc_extend_amendments <- function(amendments,start_date, end_date = NULL, simyears = NULL){
  
  # Add visible bindings
  id = yr_rep = year_ext =  P_DATE_FERTILIZATION = NULL
  
  # Check input data
  checkmate::assert_data_table(amendments, null.ok = FALSE, min.rows = 1)
  
  amendments <- as.data.table(amendments)
  setnames(amendments,toupper(colnames(amendments)))
  
  req <- c("P_HC","P_DATE_FERTILIZATION")
  checkmate::assert_names(colnames(amendments), must.include = req)
  if ("P_NAME" %in% names(amendments)) checkmate::assert_character(amendments$P_NAME, any.missing = TRUE)
  if ("P_DOSE" %in% names(amendments)) checkmate::assert_numeric(amendments$P_DOSE, lower = rc_minval('P_DOSE'), upper = rc_maxval('P_DOSE'), any.missing = TRUE)
  if ("P_C_OF" %in% names(amendments)) checkmate::assert_numeric(amendments$P_C_OF, lower = rc_minval('P_C_OF'), upper = rc_maxval('P_C_OF'), any.missing = TRUE)
  checkmate::assert_numeric(amendments$P_HC, lower = rc_minval('P_HC'), upper = rc_maxval('P_HC'), any.missing = F)
  checkmate::assert_date(as.Date(amendments$P_DATE_FERTILIZATION))
  if(!is.null(end_date)){checkmate::assert_date(as.Date(end_date))}
  if(!is.null(simyears)){checkmate::assert_numeric(simyears, lower = 1)}
  if(is.null(end_date) && is.null(simyears)) stop('both end_date and simyears are missing in the input')
  if(max(year(amendments$P_DATE_FERTILIZATION)) < year(start_date))  stop ('amendment plan is outside of simulation period')
  checkmate::assert(
    !any(grepl("-02-29$", amendments$P_DATE_FERTILIZATION)),
    msg = "February 29th dates are not allowed in P_DATE_FERTILIZATION to avoid leap year complications during date shifting"
  )
  
    
  # Define total length of amendment rotation (years)
  rotation_length <- max(year(amendments$P_DATE_FERTILIZATION)) - year(start_date) + 1
    
  # Add an unique ID
  amendments[,id := .I]
  
  # Define simyears if not supplied
  if (is.null(simyears)) {
        months_diff <- 12L * (year(end_date) - year(start_date)) + (month(end_date) - month(start_date)) + 1L
        simyears <- months_diff / 12
      }
  
  # Determine number of duplications
  duplications <- max(1L, ceiling(simyears / rotation_length))
    
  # Extend amendment table for the number of years
  amendments_ext <- amendments[rep(id, each = duplications)]
    
  # Update P_DATE_FERTILIZATION for all repetitions of rotation block
  amendments_ext[, yr_rep := 1:.N, by = id]
  amendments_ext[, year_ext := year(P_DATE_FERTILIZATION) + (yr_rep - 1) * rotation_length]
  amendments_ext[, P_DATE_FERTILIZATION := paste0(year_ext,substr(P_DATE_FERTILIZATION, start = 5, stop = 10))]
    
  # filter only the years for simulation
  if (!is.null(end_date)) {
    this.amendments <- amendments_ext[as.Date(P_DATE_FERTILIZATION) <= as.Date(end_date), ]
    } else {
      this.amendments <- amendments_ext[year_ext <= (year(start_date) + simyears),]
    }

  # Select relevant columns
  this.amendments <- this.amendments[, .SD, .SDcols =!names(this.amendments) %in% 
                                         c("id", "year_ext","yr_rep")
    ]

  # order amendments file
  setorder(this.amendments, P_DATE_FERTILIZATION)
  
  #return amendments file 
  return(this.amendments)
}



#' Function to create data table with dates and months of the entire simulation period
#'
#' @param start_date start date of the simulation period (formatted as YYYY-MM-DD, either as date or as character)
#' @param end_date end date of the simulation period (formatted as YYYY-MM-DD, either as date or as character), should be after start date
#'
#' @returns
#' Table with all year and month combinations of the simulation period
#' @export
#'
rc_time_period <- function(start_date, end_date){
  #add visible bindings
  time = NULL
  
  # perform inputs checks
  checkmate::assert_date(as.Date(start_date))
  checkmate::assert_date(as.Date(end_date))
  if (as.Date(start_date) > as.Date(end_date)) {
     stop("start_date must be on/before end_date")
  }
  
  # Create a complete set of year-month combinations for the simulation period
  dt.time <- CJ(year = min(year(start_date)):max(year(end_date)), month = 1:12)
  
  # Make selection of dates between start and end date
  dt.time <-  dt.time[,date := as.Date(paste(year, month, "01", sep = "-"))]
  
  dt.time <- dt.time[date >= as.Date(paste(year(start_date), month(start_date), "01", sep = "-")) &
                       date <= as.Date(paste(year(end_date), month(end_date), "01", sep = "-"))]
  dt.time[,date := NULL]
  
  # Format time
  dt.time[, time := .I / 12 - 1/12]
  
  #return output
  return(dt.time)
}




#' Get minimum value of provided parameter from the parameter data table
#'
#' @param this.parameter (character) parameter name for which a minimum value is needed
#'
#' @returns
#' minimum allowed value of the supplied parameter
#' 
rc_minval <- function(this.parameter) {
  # add visual binding
  data_type = code = value_min = NULL
  
  # validate input
  checkmate::assert_character(this.parameter, any.missing = FALSE, len = 1)
  
  # access parameter data
  parameters <- rotsee::rc_params
  
  # get minimum value of parameter
  out <- parameters[code == this.parameter, value_min]
  if (length(out) != 1L || is.na(out)) {
    stop(sprintf("No unique minimum bound found in parameters for code '%s'", this.parameter))
    }
  
  return(out)
}



#' Get maximum value of provided parameter from the parameter data table
#'
#' @param this.parameter (character) parameter name for which a maximum value is needed
#'
#' @returns
#' maximum allowed value of the supplied parameter

rc_maxval <- function(this.parameter){
  # add visual binding
  data_type = code = value_max = NULL
  
  # validate input
  checkmate::assert_character(this.parameter, any.missing = FALSE, len = 1)
  
  # access parameter data
  parameters <- rotsee::rc_params
  
  # get maximum value of parameter
  out <- parameters[code == this.parameter, value_max]
  if (length(out) != 1L || is.na(out)) {
    stop(sprintf("No unique maximum bound found in parameters for code '%s'", this.parameter))
  }
  
  return(out)
}

#' Function to determine W_ET_REFACT given a crop profile and supplied weather table, based on Dutch Makkink factors. 
#' NOTE: this function sets W_ET_REFACT to crop-specific Makkink factors during growth periods and 0.36 for non-crop months (alternative to the general default of 0.75)
#'
#' @param weather (data.table) weather data table containing columns year, month, W_TEMP_MEAN_MONTH, W_PREC_SUM_MONTH, and either W_ET_REF_MONTH or W_ET_ACT_MONTH.
#' @param crop (data.table) data table with crop information. Contains at least the columns B_LU_START, B_LU_END, and D_MAKKINK_JAN through DEC for the unique crops.
#' @param dt.time (data.table) Data table with the combination of years and months spanning the entire simulation period. Can be generated using \link{rc_time_period}.
#'
#' @returns
#' Weather table with W_ET_REFACT column added
#' 
#' @export
#'
rc_set_refact <- function(weather, crop, dt.time){
  # add visible bindings
  B_LU_END  = B_LU_START = W_ET_REFACT = . = NULL
  
  # check weather table
  checkmate::assert_data_table(weather)
  checkmate::assert_names(names(weather), must.include = c("year",
                                                           "month"))
  
  # check crop table
  checkmate::assert_data_table(crop,min.rows = 1)
  checkmate::assert_names(names(crop), must.include = c("B_LU_START",
                                                        "B_LU_END"))
  checkmate::assert_date(as.Date(crop$B_LU_START), any.missing = FALSE)
  checkmate::assert_date(as.Date(crop$B_LU_END), any.missing = FALSE)
  
  
  # validate D_MAKKINK inputs
  month_abbrevs <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                     "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  for (month in month_abbrevs) {
    col_name <- paste0("D_MAKKINK_", month)
    checkmate::assert_numeric(crop[[col_name]], 
                              lower = rc_minval(col_name), 
                              upper = rc_maxval(col_name))
  }
  
  # check dt.time 
  checkmate::assert_data_table(dt.time, min.rows = 1)
  checkmate::assert_names(names(dt.time), must.include = c("year", "month"))
  checkmate::assert_integerish(dt.time$year, lower = 1, any.missing = FALSE)
  checkmate::assert_integerish(dt.time$month, lower = 1, upper = 12, any.missing = FALSE)
  
  # copy data tables
  weather <- copy(weather)
  crop <- copy(crop)
  
  # Expand weather data to simulation period
  weather_ext <- merge(dt.time, weather, by = c("year", "month"), all.x = TRUE)

  # establish crop growth
  # base crop cover on supplied start and end dates of crop growth
  crop_refact <- crop[, {
    
    # Create a sequence of year-month combinations when crops are growing
    seq_dates <- seq.Date(as.Date(B_LU_START), as.Date(B_LU_END), by = 'month')
    
    # Derive year and month of growth
    growth_months <- month(seq_dates)
    growth_years <- year(seq_dates)
    
    # Set correct W_ET_REFACT value
    values_refact <- sapply(growth_months, function(m) {
      col_name <- paste0("D_MAKKINK_", toupper(month.abb[m]))
      get(col_name)
      
    }) 
   
    # Extract year and month
    list(year = growth_years, month = growth_months, W_ET_REFACT = values_refact)
  }, by = 1:nrow(crop)][, .(year, month, W_ET_REFACT)]

  # if multiple crops growing, take highest value of W_ET_REFACT
  crop_refact <- crop_refact[, .SD[which.max(W_ET_REFACT)], by = .(year, month)]
 
  # Remove provided refact values
  if("W_ET_REFACT" %in% colnames(weather_ext)) weather_ext[, W_ET_REFACT := NULL]

  # Merge weather and crop data table, fill missing dates values with 0.36
  dt <- merge(weather_ext, crop_refact, by = c("year", "month"), all.x = TRUE)
  
  dt[, W_ET_REFACT := fifelse(is.na(W_ET_REFACT), 0.36, W_ET_REFACT)]
  
  return(dt)
}


#' Function to plot information on C pools when rc_sim is run in visualize mode
#'
#' @param dt (data.table) data table with monthly information on the state of the pools CDPM, CRPM, CBIO, and CHUM
#' @param event (data.table) data table with information on c inputs, contains columns time, var, and value
#' @param save_dir (character) directory to save PNGs; defaults to current working directory
#'
#' @returns
#' data table with monthly totals and changes in different C pools, plots of their trends
#'
rc_visualize_plot <- function(dt, event = NULL, save_dir = getwd()){
  
  # add visible bindings
  pool = value = CDPM = CRPM = CBIO = CHUM = soc = time = . = NULL
  
  # copy data table
  dt <- copy(dt)
  
  # set data table to long format
  dt_long <- melt(dt, id.vars = "time", variable.name = "pool")
  
if(!is.null(event)){  
  # Copy event table
  event <- copy(event)
  
  # calculate total soc input for each event
  event_soc <- event[, .(soc = sum(value)), by=.(time)]
}

  # plot variables on a continuous scale
  if(!is.null(event)){
  out_cont <- ggplot2::ggplot()+ 
    ggplot2::geom_line(data = dt_long, ggplot2::aes(x = time, y = value, colour = pool))+
    ggplot2::geom_point(data = event_soc, ggplot2::aes(x = time, y = soc))+
    ggplot2::scale_y_continuous()+
    ggplot2::labs(title = "Carbon Pools (linear scale)", y = "Value [kg C/ha]", x = "Time [yr]")
  
  } else {
    out_cont <- ggplot2::ggplot()+ 
      ggplot2::geom_line(data = dt_long, ggplot2::aes(x = time, y = value, colour = pool))+
      ggplot2::scale_y_continuous()+
      ggplot2::labs(title = "Carbon Pools (linear scale)", y = "Value [kg C/ha]", x = "Time [yr]") 
  }
  ## calculate fraction change

  dt_change <- copy(dt)
  dt_change[,CDPM := CDPM - shift(CDPM, type = "lag") ]
  dt_change[, CRPM := CRPM - shift(CRPM, type = "lag")]
  dt_change[, CBIO := CBIO - shift(CBIO, type = "lag")]
  dt_change[, CHUM := CHUM - shift(CHUM, type = "lag")]
  dt_change[, soc := soc - shift(soc, type = "lag")]
  dt_change_long <- melt(dt_change, id.vars = "time", variable.name = "pool", na.rm = TRUE)
  
  # plot variables on a continuous scale
  if(!is.null(event)) {
   out_change_cont <- ggplot2::ggplot()+ 
    ggplot2::geom_line(data = dt_change_long, ggplot2::aes(x = time, y = value, colour = pool))+
    ggplot2::geom_point(data = event_soc, ggplot2::aes(x = time, y = soc))+
    ggplot2::scale_y_continuous()+
    ggplot2::labs(title = "Change in Carbon Pools", y = "Delta C [kg C/ha]", x = "Time [yr]")
  } else{
    out_change_cont <- ggplot2::ggplot()+ 
      ggplot2::geom_line(data = dt_change_long, ggplot2::aes(x = time, y = value, colour = pool))+
      ggplot2::scale_y_continuous()+
      ggplot2::labs(title = "Change in Carbon Pools", y = "Delta C [kg C/ha]", x = "Time [yr]")
  }
  
  # Save plots to files
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path(save_dir, "carbon_pools_linear.png"), plot = out_cont, width = 10, height = 6)
  ggplot2::ggsave(file.path(save_dir, "carbon_pools_change.png"), plot = out_change_cont, width = 10, height = 6)
  
  invisible(list(
    data  = list(total = dt, delta = dt_change),
    plots = list( linear = out_cont, change = out_change_cont)
    ))
  
}

#' Function to re-calculate RothC decomposition factors based on tillage intensity
#'
#' @param M_TILLAGE_SYSTEM (character) gives the tillage system applied. Options include NT (no-till), ST (shallow-till), CT (conventional-till) and DT (deep-till). Defaults to CT.
#' @param B_REGION (character) The region of the location
#' @param soil_properties (data.table) list of soil properties. See details for more information
#'
#' @returns
#' A list with altered decomposition rates based on soil properties and tillage rates
#' 
#' @details
#' This function recalculates the standard RothC decomposition rates based on a supplied region and soil properties.
#' 
#' Soil_properties: soil properties table required to select appropriate adjustment factors for Brazil.
#' Should include the following columns:
#' * A_SAND_MI, the sand content of the soil (\%)
#' * A_C_OF (numeric), soil organic carbon content (g C/kg), preferably for soil depth 0.3 m.
#' * A_DENSITY_SA (numeric), dry soil bulk density(g/cm3). Required if A_C_OF is supplied. In case this is not know, can be calculated using function \link{rc_calculate_bd} given a clay and organic matter content
#' * B_C_ST03 (numeric), soil organic carbon stock (Mg C/ha), preferably for soil depth 0.3 m. Required if A_C_OF is not supplied. If both are supplied, B_C_ST03 will be used to set soil organic carbon stocks.
#' 
#' 
#' @export
rc_tillage_correction <- function(M_TILLAGE_SYSTEM = 'CT',
                               B_REGION,
                               soil_properties = NULL){
  
  # check input parameters
  checkmate::assert_character(M_TILLAGE_SYSTEM)
  checkmate::assert_subset(M_TILLAGE_SYSTEM, c('CT', 'ST', 'NT', 'DT'))
  checkmate::assert_character(B_REGION)

  # define standard decomposition rates
  dec_rates <- c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02)
  
  # define region
  if(B_REGION == 'brazil'){
    # Brazil adaptations based on Hyun & Yoo (2024) (https://doi.org/10.1016/j.scitotenv.2023.168010)
    
    if(M_TILLAGE_SYSTEM == 'NT'){
      # non-conventional tillage
      
      if(soil_properties$A_SAND_MI <= 35){ # adaptation for low sand content (TN4)
        dec_rates <- c(k1 = 10 * 0.72, k2 = 0.3 * 0.97, k3 = 0.66 * 0.99, k4 = 0.02 * 0.94)
        
      }else if(soil_properties$A_SAND_MI >= 37.6){ # adaptations for high sand content
        toc_kg <- if (!is.null(soil_properties$B_C_ST03) && length(soil_properties$B_C_ST03) > 0) {
          soil_properties$B_C_ST03 * 1000
          } else {
            soil_properties$A_C_OF / 1000 * soil_properties$A_DENSITY_SA * 0.3 * 100 * 100
            }
        if(toc_kg < 75.7){
          # high sand + low C stocks (TN2)
          dec_rates <- c(k1 = 10 * 1.71, k2 = 0.3 * 0.35, k3 = 0.66 * 0.38, k4 = 0.02 * 0.87)
        }else{
          # high sand + high C stocks (TN1)
          dec_rates <- c(k1 = 10 * 1.54, k2 = 0.3 * 0.35, k3 = 0.66 * 1.42, k4 = 0.02 * 0.42)
        }
      }else{
        # sand content between 35 and 37.6% (TN3)
        dec_rates <- c(k1 = 10 * 1.54, k2 = 0.3 * 2.15, k3 = 0.66 * 2.38, k4 = 0.02 * 2.93)
      }
    }else{
      # Conventional tillage
      dec_rates <- c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02)
    }
    
  }else if(B_REGION == 'france'){
    if(M_TILLAGE_SYSTEM == 'NT'){
      dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02) * 0.93
    }
  }
    
  
  return(dec_rates)
}