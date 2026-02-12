# rotsee v0.2.3 2026-02-11
## changed
* Rename organization to `nmi-agro`

# rotsee v0.2.2 2026-01-09
## added
* test helper factories to aid with unit test creation

## changed
* split total C input `B_C_OF_INPUT` into separate C input from plants `B_C_OF_CULT` and C input from amendments `B_C_OF_AMENDMENT` 
* replaced weather parameter `W_ET_POT_MONTH` with `W_ET_REF_MONTH` to align with scientific standard
* renamed function `rc_calculate_B_C_OF` to `rc_calculate_bcof` for ease of application

## fixed 
* calculation of total C input in DR_amendment initialisation now correctly expresses kg C/ha when amendment inputs are provided as `P_C_OF` and `P_DOSE`

# rotsee v0.2.1 2025-12-18
## added 
* new function `rc_set_refact` to calculate `W_ET_REFACT` for the weather data table based on grown crops
* unit tests for `rc_set_refact` and `rc_check_inputs`

## changed
* news of `rotsee v0.1.4`, to better reflect the updates of introduced parameters table
* updated parameter table with new inputs

# rotsee v0.2.0 2025-12-10

## added
* Added multiple initialisation options to `rc_initialise`, called under "initialisation_method"
* linked `rc_sim` and `rc_initialise`
* Additional rate modification factor based on tillage, steered by input parameter `M_TILLAGE_SYSTEM`. The impact of usability of this factor can be adapted by others based on their findings.

## changed
* Input checks on multiple functions

# rotsee v0.1.8 2025-11-12
## added
* option to run `rc_sim` in visualize mode (give input visualize = TRUE), to receive output table and visualization of C flows

# rotsee v0.1.7 2025-10-16
## added
* rc_sim gains optional `irrigation` input (forwarded to `rc_input_rmf`) to include irrigation in soil moisture deficit calculation.
* Weather input supports `W_ET_REFACT` (factor to convert reference to actual ET; defaults to 0.75 when missing).

## changed
* Parameter name `W_ET_POT_MONTH` to `W_ET_REF_MONTH` to avoid ambiguous use of potential evapotranspiration


# rotsee v0.1.6 2025-10-31
## added
* unit tests of `rc_update_weather` and `rc_sim` to evaluate different weather inputs

## changed
* weather input now optionally takes year as input. If supplied it should cover the entire simulation period
* soil moisture deficit calculations in `rc_input_rmf` are now based on monthly weather and changes per year


# rotsee v0.1.5  2025-10-20
## added
* unit tests for `rc_update_parms`

## changed
* Description correctly reflects used roxygen version
* allow input of partial c_fractions distribution in rothc_parms


# rotsee v0.1.4 2025-10-16
## added
* package table `parameters.rda`, with information on all used parameters in the rotsee package
* file `rothc_params.csv`, which developers can edit to add additional parameters
* script `update_parmtable.R` under `data-raw`, which developers can use to update `parameters.rda` with updated `rothc_params.csv`

## changed
* Validation of input data based on information in `parameters.rda`


# rotsee v0.1.3 2025-10-15
## added
* unit tests for `rc_multicore` and `rc_parallel`

## changed
* Reworked `rc_multicore` and `rc_parallel` for a clearer workflow for multicore calculations
* Align required inputs for `rc_multicore` with `rc_sim`
* Tightened outputs to only time, OM content, and C content of different pools

## removed
* `rc_shi_field`, with core functions incorporated into `rc_parallel`


# rotsee v0.1.2 2025-10-13
## added
* unit tests for `rc_calculate_B_C_OF` and `rc_time_period`

## changed
* Loosened checks for `rc_calculate_B_C_OF` and `rc_update_weather` to allow additional input columns


# rotsee v0.1.0 2025-09-24
## added
* B_LU_START and B_LU_END as input parameters in rothc_rotation
* start_date and end_date to replace simyears
* Helper functions rc_extend_crops and rc_extend_amendments to extend user input tables
* Helper function rc_time_period to generate a table of all years and months in the simulation period

## changed
* Base soil cover on user supplied crop growing dates
* Calculate actual evapotranspiration based on simple rothc correction factor
* Corrected accumulated soil moisture deficit calculation


# rotsee v0.0.4 2025-09-24
## Changed
* Split `rc_input_events` into `rc_input_event_crop`, `rc_input_event_amendment`, and merge these in `rc_input_events`
* Added unit tests for `rc_input_event_crop`, `rc_input_event_amendment`, and `rc_input_events`
* Expand README with some introductory text about the package
  
  
# rotsee v0.0.3 2025-09-01
## Added
* function rc_check_inputs to check input data
* Helper function rc_calculate_bd to estimate dry soil bulk density based on soil properties
* Helper function rc_calculate_B_C_OF to calculate crop C inputs based on crop management

## changed
* Input data of crops and amendments to be optional
* Option to input total crop C input or general crop management data
* Changed amendment inputs to single event date, format yyyy-mm-dd


# rotsee V0.0.2 2025-08-28
## Added
* Add GitHub Action to run R-CMD-CHECK for PR's
* Added helper functions to check input weather and parameter data, and insert default values when not supplied
* Added unit tests for helper functions

## Fixed
* Add missing dependencies for `roxygen2`, `devtools` and `usethis`


# rotsee v0.0.1 2025-07-31
## Added
* function `rc_ode` the set of ordinairy differential equations to run RothC
* function `rc_sim()` to run a simulation for RothC
* function `rc_helpers` to add supportive functions
* function `rc_input_amendment` to prepare the C input file from manure, FYM or compost
* function `rc_input_crop` to prepare the C input file from crop residues
* function `rc_input_rmf` to prepare the rate modifying factors input file
* function `rc_input_events` to prepare the event object needed for the ODE
* function `rc_input_scenario` to prepare scenarios (for NL at the moment)
* function `rc_sim` to simulate a RothC simulation for a single field
* script `rc_tables` to define structure of package tables
* function `rc_initialise` a function to select different initialisation options (draft)
* function `rc_multicore` and `rc_parallel` to allow multi-core calculations
* function `rc_shi_field` to estimate soil health index based on saturation degree (draft)
* package tables `rc_crops`, `rc_makkink` and `rc_parms`
* README, description and changelog
