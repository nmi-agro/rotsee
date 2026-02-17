#' Simulate SOC evolution using Roth-C
#'
#' This function calculates the change in carbon stock or C pools (in kg C per ha) based on organic matter amendments, crop rotation, and long-term averaged weather conditions.
#'
#' @param soil_properties (data.table) Data table with soil properties: A_C_OF, soil organic carbon content (g/kg) or B_C_ST03, soil organic carbon stock (Mg C/ha), preferably for soil depth 0.3 m; A_CLAY_MI, clay content (\%); A_DENSITY_SA, dry soil bulk density (g/cm3)
#' @param A_DEPTH (numeric) Depth for which soil sample is taken (m). Default set to 0.3.
#' @param B_DEPTH (numeric) Depth of the cultivated soil layer (m), simulation depth. Default set to 0.3.
#' @param rothc_rotation (data.table) Table with crop rotation details and crop management actions that have been taken. Includes also crop inputs for carbon. See details for desired format.
#' @param rothc_amendment (data.table) A table with the following column names: P_DATE_FERTILIZATION, P_HC, and B_C_OF_AMENDMENT and/or P_DOSE and P_C_OF. See details for desired format.
#' @param rothc_parms (list) A list with simulation parameters controlling the dynamics of RothC Model. For more information, see details.
#' @param weather (data.table) Table with following column names: year (optional), month, W_TEMP_MEAN_MONTH, W_PREC_SUM_MONTH, W_ET_REF_MONTH, W_ET_ACT_MONTH, W_ET_REFACT. For more information, see details.
#' @param M_TILLAGE_SYSTEM (character) gives the tillage system applied. Options include NT (no-till), ST (shallow-till), CT (conventional-till) and DT (deep-till). Defaults to CT.
#' @param irrigation (data.table) Table with the following column names: B_DATE_IRRIGATION, B_IRR_AMOUNT. See details for more information.
#' @param visualize (boolean) If TRUE, run rc_sim in visualize mode. Results are directly plotted in c pools per month to allow for direct interpretation. 
#'
#' @details
#' This function simulates the fate of SOC given the impact of soil properties, weather and management.
#' The soil_properties table is required. 
#' When no weather inputs are given, these are estimated from long-term average weather conditions in the Netherlands.
#'
#' soil_properties: soil properties table.
#' Includes the columns:
#' * A_C_OF (numeric), soil organic carbon content (g C/kg), preferably for soil depth 0.3 m.
#' * B_C_ST03 (numeric), soil organic carbon stock (Mg C/ha), preferably for soil depth 0.3 m. Required if A_C_OF is not supplied. If both are supplied, B_C_ST03 will be used to set soil organic carbon stocks.
#' * A_CLAY_MI (numeric), clay fraction (\%)
#' * A_DENSITY_SA (numeric), dry soil bulk density(g/cm3). In case this is not know, can be calculated using function \link{rc_calculate_bd} given a clay and organic matter content
#' 
#' rothc_amendment: amendment table. Input can be duplicated to cover the entire simulation period using \link{rc_extend_amendments}
#' Includes the columns:
#' * B_C_OF_AMENDMENT (numeric), the organic carbon input from soil amendment product on a field level (kg C/ha)
#' * P_DOSE (numeric), applied dose of soil amendment product (kg/ha), required if B_C_OF_AMENDMENT is not supplied
#' * P_C_OF (numeric), organic carbon content of the soil amendment product (g C/kg), required if B_C_OF_AMENDMENT is not supplied
#' * P_HC (numeric), the humification coefficient of the soil amendment product (fraction)
#' * P_DATE_FERTILIZATION (date), date of fertilizer application (formatted YYYY-MM-DD)
#' 
#' rothc_rotation: crop table. Input can be duplicated to cover the entire simulation period using \link{rc_extend_crops}
#' Includes the columns: 
#' * B_LU_START (start of crop rotation),
#' * B_LU_END (end of crop rotation),
#' * B_LU_HC, the humification coefficient of crop organic matter (-). When not supplied, default RothC value will be used
#' * B_C_OF_CULT, the organic carbon input from plants on field level (kg C/ha). In case not known, can be calculated using function \link{rc_calculate_bcof}
#'
#' rothc_parms: parameters to adapt calculations (optional)
#' May include the following columns:
#' * initialisation_method (character) scenario to initialise the carbon pools. options 'spinup_analytical_bodemcoolstof','spinup_analytical_heuvelink', 'spinup_simulation', 'none', default is 'none'
#' * c_fractions (list) Distribution over the different C pools. If not supplied nor calculated via model initialisation, default RothC distribution is used
#' * dec_rates (list) list of decomposition rates of the different pools. If not supplied, default RothC values are used
#' * unit (character) Unit in which the output should be given. Options: 'A_SOM_LOI' (\% organic matter),'psoc' (g C/kg), 'psomperfraction' (\% organic matter of each fraction), 'cstock' (kg C/ha of each fraction)
#' * method (character) method to solve ordinary differential equations, see \link[deSolve]{ode} for options. default is adams.
#' * poutput (character) Resolution of data output. Options: 'year', 'month'
#' * start_date (character, formatted "YYYY-MM-DD") Start date of simulation period. If not provided, first date of crop rotation or amendment application is taken.
#' * end_date (character, formatted "YYYY-MM-DD") End date of simulation period. If not provided, last date of crop rotation or amendment application is taken.
#' 
#' weather: Weather table. If no table is given, average Dutch conditions are used
#' Includes the columns:
#' * year (integer) optional, should span the entire simulation period. If not supplied, month must include all 12 months which will be auto-expanded across simulation period
#' * month
#' * W_TEMP_MEAN_MONTH (temperature in °C)
#' * W_PREC_SUM_MONTH (precipitation in mm)
#' * W_ET_REF_MONTH (reference evapotranspiration in mm)
#' * W_ET_ACT_MONTH (actual evapotranspiration in mm)
#' * W_ET_REFACT (factor to recalculate reference to actual evapotranspiration).  When not provided, defaults to 0.75 for all months. Can be set given crop information using \link{rc_set_refact}, which applies crop-specific Makkink factors during growth periods and 0.36 for non-crop months.
#' 
#' Irrigation: Irrigation table, optional.
#' Includes the columns:
#' * B_DATE_IRRIGATION (date, formatted YYYY-MM-DD) Date of field irrigation
#' * B_IRR_AMOUNT (numeric) Irrigation amount (mm)
#'
#' @import deSolve
#' 
#' @returns Table with development of C pools in the field over the given simulation period
#'
#' @export
rc_sim <- function(soil_properties,
                   A_DEPTH = 0.3,
                   B_DEPTH = 0.3,
                   rothc_rotation = NULL,
                   rothc_amendment = NULL,
                   rothc_parms = NULL,
                   weather = NULL,
                   M_TILLAGE_SYSTEM = NULL,
                   irrigation = NULL,
                   visualize = FALSE){
  
  # add visual bindings
  a_depth = toc = A_CLAY_MI = A_C_OF = B_C_ST03 = A_DENSITY_SA = A_SOM_LOI = psoc = NULL
  var = time = cf_abc = ciom.ini = biohum.ini = cbio.ini = chum.ini = CIOM0 = CDPM0 = CRPM0 = CBIO0 = CHUM0 = NULL
  soc = CDPM = CRPM = CBIO = CHUM = CIOM = . = NULL
  
  
  # Check input data and create defaults when necessary

  # Check input data for soil, crop, and amendment data
  rc_check_inputs(soil_properties = soil_properties,
                  rothc_rotation = rothc_rotation,
                  rothc_amendment = rothc_amendment)

   # Check and update parameter table rothc_parms
  rothc_parms <- rc_update_parms(parms = rothc_parms, crops = rothc_rotation, amendments = rothc_amendment)
 
  # Define decomposition rates
  k1 <- rothc_parms$dec_rates[["k1"]]
  k2 <- rothc_parms$dec_rates[["k2"]]
  k3 <- rothc_parms$dec_rates[["k3"]]
  k4 <- rothc_parms$dec_rates[["k4"]]

  # Define unit of output
  unit <- rothc_parms$unit
  
  # Define start_date
  start_date <- rothc_parms$start_date
  
  #Define end_date
  end_date <- rothc_parms$end_date
  
  # Define method
  method <- rothc_parms$method
  
  # Define wanted output
  poutput <- rothc_parms$poutput
  
  # Define initialise
  initialisation_method <- rothc_parms$initialisation_method
  
  # Define dates of complete simulation period
  dt.time <- rc_time_period(start_date = start_date, end_date = end_date)
  
  # Check and update weather data (see rc_helpers)
  dt.weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
  # add checks
  checkmate::assert_numeric(A_DEPTH, lower = rc_minval("A_DEPTH"), upper = rc_maxval("A_DEPTH"), any.missing = FALSE, len = 1)
  checkmate::assert_numeric(B_DEPTH, lower = rc_minval("B_DEPTH"), upper = rc_maxval("B_DEPTH"), any.missing = FALSE, len = 1)
  checkmate::assert_logical(visualize, any.missing = FALSE, len = 1)
  if(!is.null(M_TILLAGE_SYSTEM)){
  checkmate::assert_character(M_TILLAGE_SYSTEM)
  }else{
    M_TILLAGE_SYSTEM <- 'CT'
  }

  # rothC model parameters

  # prepare the RothC model inputs
  # create an internal crop rotation file
  if(!is.null(rothc_rotation)){
    dt.crop <- rc_input_crop(dt = rothc_rotation)
  } else {
    dt.crop = NULL
  }
  
  # create an internal amendment file
  if (!is.null(rothc_amendment)) {
    dt.org <- rc_input_amendment(dt = rothc_amendment)
  } else {
    dt.org <- NULL
  }

  # make rate modifying factors input database
  if(!is.null(dt.crop)){
  dt.rmf <- rc_input_rmf(dt = dt.crop,
                         A_CLAY_MI = soil_properties$A_CLAY_MI,
                         B_DEPTH = B_DEPTH,
                         M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         dt.time = dt.time,
                         dt.weather = dt.weather,
                         dt.irrigation = irrigation)
 
  }else{
    dt.rmf <- rc_input_rmf(A_CLAY_MI = soil_properties$A_CLAY_MI,
                           B_DEPTH = B_DEPTH,
                           dt.time = dt.time,
                           dt.weather = dt.weather,
                           dt.irrigation = irrigation)
    
  }
 
  # combine RothC input parameters
  rothc.parms <- list(k1 = k1,k2 = k2, k3=k3, k4=k4, R1 = dt.rmf$R1, abcd = dt.rmf$abcd, time = dt.rmf$time)
  
  # estimate default crop rotation plan, the building block
  event.crop <- rc_input_event_crop(crops = dt.crop, dt.time = dt.time)
  
  # estimate Carbon input via manure, compost and organic residues
  event.man <- rc_input_event_amendment(amendment = dt.org, dt.time = dt.time)

  # prepare EVENT database with all C inputs over time 
  rothc.event <- rc_input_events(crops = event.crop,amendment = event.man)
  
  # make internal data.table 
  dt.soc <- data.table(A_C_OF = soil_properties$A_C_OF,B_C_ST03 = soil_properties$B_C_ST03, A_CLAY_MI = soil_properties$A_CLAY_MI,a_depth = A_DEPTH,b_depth = B_DEPTH, A_DENSITY_SA = soil_properties$A_DENSITY_SA)
 
  # calculate total organic carbon (kg C / ha)
  if(length(dt.soc$B_C_ST03) != 0) {
    dt.soc[,toc := B_C_ST03 * 1000]
  }else{
    # Correct A_C_OF for sampling depth 
    dt.soc[a_depth < 0.3 & A_CLAY_MI <= 10, A_C_OF := A_C_OF * (1 - 0.19 * ((0.20 - (pmax(0.10, a_depth) - 0.10))/ 0.20))]
    dt.soc[a_depth < 0.3 & A_CLAY_MI > 10, A_C_OF := A_C_OF * (1 - 0.33 * ((0.20 - (pmax(0.10, a_depth) - 0.10))/ 0.20))]
    
    dt.soc[,toc := A_C_OF / 1000 * A_DENSITY_SA * 1000 * B_DEPTH * 100 * 100]
  }

  # initialise the RothC pools if requested 
  if(initialisation_method != 'none'){
    
    # derive the initial distribution of C pools (original data.tables are used as input)
    cpools = as.list(rc_initialise(crops = rothc_rotation, 
                                        amendment = rothc_amendment,
                                        dt.time = dt.time,
                                        dt.soc = dt.soc,
                                        soil_properties = soil_properties,
                                        rothc.event = rothc.event,
                                        start_date = start_date,
                                        dt.weather = weather,
                                        rothc.parms = rothc.parms,
                                        initialisation_method = initialisation_method))
    # set initial cpools
    dt.soc[,CIOM0 := cpools$CIOM0]
    dt.soc[,CDPM0 := cpools$CDPM0]
    dt.soc[,CRPM0 := cpools$CRPM0]
    dt.soc[,CBIO0 := cpools$CBIO0]
    dt.soc[,CHUM0 := pmax(0, toc-CIOM0-CDPM0-CRPM0-CBIO0)]
  
  }else{
    # Define C pools from user supplied fractions
    c_fractions <- as.list(rothc_parms$c_fractions)
    
    # calculate initial pool sizes based on supplied fraction distribution (kg C / ha)
    dt.soc[,CIOM0 := c_fractions$fr_IOM * ((toc*0.001)^1.139)*1000]
    dt.soc[,CDPM0 := c_fractions$fr_DPM * (toc-CIOM0)]
    dt.soc[,CRPM0 := c_fractions$fr_RPM * (toc-CIOM0)]
    dt.soc[,CBIO0 := c_fractions$fr_BIO * (toc-CIOM0)]
    dt.soc[,CHUM0 := pmax(0, toc-CIOM0-CDPM0-CRPM0-CBIO0)]
  }

  # extract relevant columns
  rothc.ini <- dt.soc[,list(CIOM0,CDPM0,CRPM0,CBIO0,CHUM0)]
  
  # run RothC model

  # set time vector for RothC, add event times
  rothc.times <- rothc.parms$time
  rothc.times <- c(rothc.event$time,rothc.times)
  rothc.times <- sort(unique(rothc.times))
  rothc.parms$time <- NULL


  # set initial distribution of C pool
  y  = c(CDPM = rothc.ini$CDPM0,
         CRPM = rothc.ini$CRPM0,
         CBIO = rothc.ini$CBIO0,
         CHUM = rothc.ini$CHUM0)
  
  # set random seed
  set.seed(123)
  
  # run the model

  out <- deSolve::ode(y = y,
                      times = rothc.times,
                      rc_ode,
                      parms = rothc.parms,
                      events=list(data=rothc.event),
                      method = method,
                      rtol = 0.1,
                      atol = 1)
  
  # set to data.table
  out <- as.data.table(out)
  
  # estimate total SOC (kg C/ha)
  out[,soc := round(CDPM + CRPM + CBIO + CHUM + dt.soc$CIOM0)]
  
  # save debug output if requested
  if(visualize == TRUE){
    out_vis <- copy(out)
    
    utils::write.csv(out_vis, "rothc_flows_vis.csv", row.names = FALSE)
    message("Visualize mode: C flows saved to rothc_flows_vis.csv")
    
    # create visualization of C flows
    rc_visualize_plot(out_vis, event = rothc.event, save_dir = getwd())
  }
 
 
  # select type output
  if(unit=='A_SOM_LOI') {
    # Output in organic matter content [\%]
    
    # subset the RothC simulation result
    rothc.soc <- out[,list(time=time, soc)]

    # Set required soil parameters
    rothc.soc[,A_DENSITY_SA := mean(dt.soc$A_DENSITY_SA)]
    rothc.soc[,A_CLAY_MI := mean(dt.soc$A_CLAY_MI)]
    rothc.soc[,A_DEPTH := dt.soc$a_depth]
    rothc.soc[,B_DEPTH := dt.soc$b_depth]
    
    # set C stocks (kg C/ha) to organic matter content (\%)
    rothc.soc[,A_SOM_LOI := soc * 100 * 2 / (A_DENSITY_SA * 1000 * B_DEPTH * 100 * 100)]
    
    # Correct A_SOM_LOI for sampling depth
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI <= 10, A_SOM_LOI := A_SOM_LOI / (1 - 0.19 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI > 10, A_SOM_LOI := A_SOM_LOI / (1 - 0.33 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
   
    # select output variables
    out <- rothc.soc[,.(time, A_SOM_LOI, soc)]
  } else if (unit == 'psoc') {
    # Output in organic carbon content [g C/kg]
    
    # subset the RothC simulation result
    rothc.soc <- out[,list(time = time, soc)]
    
    # Set required soil parameters
    rothc.soc[,A_DENSITY_SA := mean(dt.soc$A_DENSITY_SA)]
    rothc.soc[,A_CLAY_MI := mean(dt.soc$A_CLAY_MI)]
    rothc.soc[,A_DEPTH := dt.soc$a_depth]
    rothc.soc[,B_DEPTH := dt.soc$b_depth]
    
    # set C stocks (kg C/ha) to organic carbon content (g C/kg)
    rothc.soc[,psoc := soc * 1000 / (A_DENSITY_SA * 1000 * B_DEPTH * 100 * 100)]
    
    # Correct A_SOM_LOI for sampling depth
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI <= 10, psoc := psoc / (1 - 0.19 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI > 10, psoc := psoc / (1 - 0.33 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
    
    # select output variables
    out <- rothc.soc[,.(time, soc, psoc)]
    
  } else if (unit == 'psomperfraction'){
    # Output in %SOM per rothc pool
    
    # subset the RothC simulation result
    rothc.soc <- copy(out)
    
    # estimate bulk density
    rothc.soc[,A_DENSITY_SA := mean(dt.soc$A_DENSITY_SA)]
    rothc.soc[,A_CLAY_MI := mean(dt.soc$A_CLAY_MI)]
    rothc.soc[,A_DEPTH := A_DEPTH]
    rothc.soc[,CIOM := dt.soc$CIOM0]
   
    # do unit conversion for all pools, convert to %SOM
    cols <- c('soc','CDPM','CRPM','CBIO','CHUM','CIOM')
    rothc.soc[,c(cols) := lapply(.SD,function(x) x * 100 * 2 / (A_DENSITY_SA * 1000 * B_DEPTH * 100 * 100)),.SDcols = cols]
    
    # Correct soc for sampling depth
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI <= 10, soc := soc / (1 - 0.19 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI > 10, soc := soc / (1 - 0.33 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
    
    # select output variables
    out <- rothc.soc[,.(time = time, A_SOM_LOI = soc,CDPM,CRPM,CBIO,CHUM,CIOM)]
    
  } else if (unit=='cstock'){
    # Output in kg C/ha
    out <- out[,list(time = time, soc,CDPM,CRPM,CBIO,CHUM,CIOM = dt.soc$CIOM0)]
      }

  # Add date information to output
  out <- merge(out, dt.time, by = 'time')
  
  # if requested, provide information in the scale of years
  if(poutput=='year'){
    out <- out[abs(time - round(time)) < 1e-5]
  }

  # return output
  return(out)
}