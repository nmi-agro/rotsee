test_that("rc_sim correctly checks input validity", {
  soil_properties <- create_soil_properties()
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3

  M_TILLAGE_SYSTEM = 'CT'
  
  rothc_rotation <- create_rotation()
    
 
  rothc_amendment <- create_amendment()
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  irrigation <- create_irrigation()

  # All correct
 result_all <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms, irrigation = irrigation)
 
 expect_s3_class(result_all, "data.table")
 expect_true(nrow(result_all) > 0)
  
  # No amendment table (allowed)
  result_no_amend <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = NULL, 
                         weather = weather, rothc_parms = parms, irrigation = irrigation)
                      
  expect_s3_class(result_no_amend, "data.table")
  expect_true(nrow(result_no_amend) > 0)                 
  
    # No crop table (allowed)
  result_no_crop <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH,  M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = NULL, rothc_amendment = rothc_amendment, 
                   weather = weather, irrigation = irrigation)
  
  expect_s3_class(result_no_crop, "data.table")
  expect_true(nrow(result_no_crop) > 0) 
  
  # No weather table (allowed)
  result_no_weath <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = NULL, rothc_parms = parms, irrigation = irrigation)
  
  expect_s3_class(result_no_weath, "data.table")
  expect_true(nrow(result_no_weath) > 0) 
  
  
  # no irrigation (allowed)
  result_no_irri <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = NULL, rothc_parms = parms)
  
  expect_s3_class(result_no_irri, "data.table")
  expect_true(nrow(result_no_irri) > 0) 
  
})


test_that("rc_sim correctly returns different output formats", {
  # Set correct  input files
  soil_properties <- create_soil_properties()
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  M_TILLAGE_SYSTEM = 'CT'
  
  rothc_rotation <- create_rotation()
  
  rothc_amendment <- create_amendment()
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  
  # unit = "A_SOM_LOI
  parms_somloi <- parms
  
  parms_somloi$unit <- "A_SOM_LOI"
  
  result_somloi <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_somloi
  )

  expect_s3_class(result_somloi, "data.table")
  expect_true("A_SOM_LOI" %in% names(result_somloi))
  expect_true("year" %in% names(result_somloi))
  expect_false(any(is.na(result_somloi$A_SOM_LOI)))
  
  # unit = "psoc"
  parms_psoc <- parms
  
  parms_psoc$unit <- "psoc"
  
  result_psoc <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_psoc
  )
  
  expect_s3_class(result_psoc, "data.table")
  expect_true("soc" %in% names(result_psoc))
  expect_true("year" %in% names(result_psoc))
  expect_false(any(is.na(result_psoc$soc)))
  
  # unit = "psomperfraction"
  parms_pspf <- parms
  
  parms_pspf$unit <- "psomperfraction"
  
  result_pspf <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_pspf
  )
  
  expect_s3_class(result_pspf, "data.table")
  expect_true(all(c("CDPM", "CRPM", "CBIO", "CHUM", "CIOM") %in% names(result_pspf)))
  expect_true("year" %in% names(result_pspf))
  expect_false(any(is.na(result_pspf$CDPM)))
  
  
  # unit = "cstock"
  parms_cstock <- parms
  
  parms_cstock$unit <- "cstock"
  
  result_cstock <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_cstock
  )
  
  expect_s3_class(result_cstock, "data.table")
  expect_true(all(c("soc", "CDPM", "CRPM", "CBIO", "CHUM", "CIOM") %in% names(result_cstock)))
  expect_true("year" %in% names(result_cstock))
  expect_false(any(is.na(result_cstock$soc)))
  
  # invalid unit (should error)
  parms_invalid <- parms
  
  parms_invalid$unit <- "invalid_unit"
  
  expect_error(rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_invalid
  ), "Must be element of set")
}
)

test_that("rc_sim returns yearly output when poutput is 'year'", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  rothc_amendment <- create_amendment()
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  result <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms
  )
  
  # Verify yearly output: should have one row per year
  expect_equal(nrow(result), 19)  
  expect_true("year" %in% names(result))
  expect_equal(result$year, c(2022:2040))
})

test_that("rc_sim runs in visualize mode and produces visualize output", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  rothc_amendment <- create_amendment()
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  # Save current working directory and switch to temp directory
  old_wd <- getwd()
  temp_dir <- file.path(tempdir(), "rotsee_vis_test")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  setwd(temp_dir)
  
  # Restore working directory on exit and remove temporary directory(even if test fails)
  on.exit({
    setwd(old_wd)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)
  

  # Run in debug mode
  expect_no_error(
    rc_sim(
      soil_properties = soil_properties,
      rothc_rotation = rothc_rotation,
      rothc_amendment = rothc_amendment,
      weather = weather,
      rothc_parms = parms,
      visualize = TRUE
    )
  )
 
  # Check that debug files were created
  expect_true(file.exists("rothc_flows_vis.csv"))
  expect_true(file.exists("carbon_pools_linear.png"))
  expect_true(file.exists("carbon_pools_change.png"))
  
  # Check that debug file has expected columns
  vis_output <- fread("rothc_flows_vis.csv")
  expect_true(all(c("time", "CDPM", "CRPM", "CBIO", "CHUM", "soc") %in% names(vis_output)))

})

  test_that("rc_sim correctly runs with different weather data", {
    # Set up correct input data
    soil_properties <- create_soil_properties()
    
    A_DEPTH = 0.3
    
    B_DEPTH = 0.3
    
    
    M_TILLAGE_SYSTEM = 'CT'
    
    rothc_rotation <- create_rotation()
    
    
    rothc_amendment <- create_amendment()
    
    parms <- create_parms()
    
  # Generate weather data
  weather_all <- create_weather()[rep(1:.N, 19)][, year := rep(2022:2040, each = 12)]
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_all, rothc_parms = parms))
  
  # Only W_ET_REF_MONTH
  weather_ref <- copy(weather_all)[, W_ET_ACT_MONTH := NULL]
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_ref, rothc_parms = parms))
  
  # Only W_ET_ACT_MONTH
  weather_act <- copy(weather_all)[, W_ET_REF_MONTH := NULL]
  weather_act[, W_ET_ACT_MONTH := rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5), 19)]

  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_act, rothc_parms = parms))
  
  # No years (allowed)
  weather_noyr <- create_weather()
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_noyr, rothc_parms = parms))
  
  })

test_that("rc_sim works with different initialisation methods", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  rothc_amendment <- create_amendment()
  
  weather <- create_weather()
  
  init_methods <- c('spinup_analytical_bodemcoolstof', 
                    'spinup_analytical_heuvelink', 
                    'spinup_simulation',
                    'none')
  
  for (method in init_methods) {
    parms <- list(
      initialisation_method = method,
      unit = "A_SOM_LOI",
      method = "adams",
      poutput = "year",
      start_date = "2022-04-01",
      end_date = "2024-10-01"
    )
    
    result <- rc_sim(soil_properties = soil_properties,
                     A_DEPTH = 0.3, B_DEPTH = 0.3,
                     rothc_rotation = rothc_rotation,
                     rothc_amendment = rothc_amendment,
                     weather = weather,
                     rothc_parms = parms)
    
    expect_s3_class(result, "data.table")
    expect_true(nrow(result) > 0)
  }
})


test_that("rc_sim with initialisation_method none correctly uses c_fractions", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  parms <- create_parms()
  
 
  # Should work with c_fractions supplied
  expect_no_error(rc_sim(soil_properties = soil_properties,
                         A_DEPTH = 0.3, B_DEPTH = 0.3,
                         rothc_rotation = rothc_rotation,
                         rothc_parms = parms))
  
  # Should work without c_fractions supplied
  parms_no_fractions <- list(
    initialisation_method = 'none',
    unit = "A_SOM_LOI",
    start_date = "2022-04-01",
    end_date = "2024-10-01"
  )

  expect_no_error(rc_sim(soil_properties = soil_properties,
                      A_DEPTH = 0.3, B_DEPTH = 0.3,
                      rothc_rotation = rothc_rotation,
                      rothc_parms = parms_no_fractions))
  
})



  

test_that("rc_sim handles irrigation with different output units", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  irrigation <- create_irrigation()
  
  # Test with A_SOM_LOI output
  parms_som <- list(
    unit = "A_SOM_LOI",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_som <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_som,
    irrigation = irrigation
  )
  
  expect_s3_class(result_som, "data.table")
  expect_true("A_SOM_LOI" %in% names(result_som))
  
  # Test with psoc output
  parms_psoc <- list(
    unit = "psoc",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_psoc <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_psoc,
    irrigation = irrigation
  )
  
  expect_s3_class(result_psoc, "data.table")
  expect_true(all(c("soc", "psoc") %in% names(result_psoc)))
  
  # Test with cstock output
  parms_cstock <- list(
    unit = "cstock",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_cstock <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_cstock,
    irrigation = irrigation
  )
  
  expect_s3_class(result_cstock, "data.table")
  expect_true("soc" %in% names(result_cstock))
})


test_that("rc_sim handles irrigation timing variations", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  # Early season irrigation
  irrigation_early <- data.table(
    B_DATE_IRRIGATION = c("2022-04-15"),
    B_IRR_AMOUNT = c(20)
  )
  
  result_early <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_early
  )
  
  expect_s3_class(result_early, "data.table")
  
  # Late season irrigation
  irrigation_late <- data.table(
    B_DATE_IRRIGATION = c("2022-09-15"),
    B_IRR_AMOUNT = c(20)
  )
  
  result_late <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_late
  )
  
  expect_s3_class(result_late, "data.table")
})

test_that("rc_sim with irrigation and different W_ET_REFACT values", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  # Weather with custom W_ET_REFACT
  weather_custom <- create_weather()
  weather_custom[, W_ET_REFACT := seq(0.6, 0.9, length.out = 12)]# Variable factor
  
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(30)
  )
  
  parms <- list(
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather_custom,
    rothc_parms = parms,
    irrigation = irrigation
  )
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("rc_sim handles long-term simulation with irrigation", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  # Irrigation events spread over multiple years
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2023-07-01", "2024-07-01", 
                          "2025-07-01", "2026-07-01"),
    B_IRR_AMOUNT = c(25, 30, 20, 28, 22)
  )
  
  parms <- list(
    start_date = "2022-04-01",
    end_date = "2027-10-01"
  )
  
  result <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation
  )
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  # Should have multiple years of output
  expect_true(max(result$year) - min(result$year) >= 4)
})

test_that("rc_sim handles irrigation with initialization", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(30)
  )
  
  # Test with initialisation = 'spinup_analytical_bodemcoolstof'
  parms_init_bc <- list(
    initialisation_method = 'spinup_analytical_bodemcoolstof',
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_init_bc <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_init_bc,
    irrigation = irrigation
  )
  
  expect_s3_class(result_init_bc, "data.table")
  
  # Test with initialisation = 'spinup_analytical_heuvelink'
  parms_init_heuv <- list(
    initialisation_method = 'spinup_analytical_heuvelink',
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_init_heuv <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_init_heuv,
    irrigation = irrigation
  )
  
  expect_s3_class(result_init_heuv, "data.table")
})

test_that("rc_sim handles extreme irrigation scenarios", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  # Minimal irrigation (0 mm)
  irrigation_min <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(0)
  )
  
  result_min <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_min
  )
  
  expect_s3_class(result_min, "data.table")
  
  # Maximum irrigation (1000 mm - extreme case)
  irrigation_max <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(1000)
  )
  
  result_max <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_max
  )
  
  expect_s3_class(result_max, "data.table")
})
  


test_that("rc_sim provides correct output in years or months", {
  
  # Set up correct input data
  soil_properties <- create_soil_properties()
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  
  M_TILLAGE_SYSTEM = 'CT'
  
  rothc_rotation <- create_rotation()
  
  rothc_amendment <- create_amendment()
  
  parms <- create_parms()
  parms$poutput <- 'month'
  
  # Weather data
  
  weather <- create_weather()[rep(1:.N, 19)][, year := rep(2022:2040, each = 12)]
  
  
  result <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms)
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 223)
})


test_that("rc_sim runs correctly when start_date of simulation is after first crop growth", {
  # set up correct input data
  soil_properties <- create_soil_properties()
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  
  M_TILLAGE_SYSTEM = 'CT'
  
  rothc_rotation <- create_rotation()
  
  rothc_amendment <- create_amendment()
  
  parms <- create_parms()
  parms$start_date <- "2022-06-01"
  
  weather <- create_weather()[rep(1:.N, 19)][, year := rep(2022:2040, each = 12)]
  
  # run the package
  result <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms)
  
  # check outputs
  expect_s3_class(result, "data.table")
  expect_equal(result$month, rep(6, 19))
  })