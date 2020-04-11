#' Example MAVEn dataset.
#'
#' A dataset containing 2 cycles of metabolism and activity readings. This data
#' was collected by J. Waters and owned by J. Waters and Providence College.
#'
#' @format A data frame with 4219 rows and 66 variables: 
#' \describe{
#'   \item{Seconds} seconds time base 
#'   \item{Date_Time} Date and time as maintained in the MAVEn
#'   \item{CO2ppm} Carbon dioxide concentration (parts per million)
#'   \item{WVppt} Water Vapor Pressure
#'   \item{RefBP} Reference Barometric Pressure
#'   \item{Volts_4} Port for measurement probe. Renamed to TC1 (thermocouple 1)
#'   \item{Volts_5} Port for measurement probe. Renamed to TC2 (thermocouple 2)
#'   \item{Volts_6} Port for measurement probe. Renamed to TC3 (thermocouple 3)
#'   \item{FRB_mlmin} Flow rate in the baseline chamber (ml/min)
#'   \item{FRC_mlmin} Flow rate in the sample/animal chamber (ml/min)
#'   \item{Chamber} Chamber ID being measured
#'   \item{TempC} Temperature (deg C)
#'   \item{RH_pct} Relative humidity
#'   \item{Light_Lux} Ambient light (lux)
#'   \item{BP_kPa} Barometric pressure kPA 
#'   \item{Act_1} Activity in chamber 1
#'   \item{Act_2} Activity in chamber 2 
#'   \item{Act_3} Activity in chamber 3
#'   \item{Act_4} Activity in chamber 4 
#'   \item{Act_5} Activity in chamber 5
#'   \item{Act_6} Activity in chamber 6 
#'   \item{Act_7} Activity in chamber 7
#'   \item{Act_8} Activity in chamber 8 
#'   \item{Act_9} Activity in chamber 9
#'   \item{Act_10} Activity in chamber 10 
#'   \item{Act_11} Activity in chamber 11
#'   \item{Act_12} Activity in chamber 12 
#'   \item{Act_13} Activity in chamber 13
#'   \item{Act_14} Activity in chamber 14 
#'   \item{Act_15} Activity in chamber 15
#'   \item{Act_16} Activity in chamber 16 
#'   \item{c_FRC_mlmin} Unit converted flow rate (not used) 
#'   \item{c_CO2ppm} Unit converted CO2 concentration (not used)
#'   \item{CO2_mlmin} Carbon dioxide production rate (mL per min)
#'   \item{FlyChamber1} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber2} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber3} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber4} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber5} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber6} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber7} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber8} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber9} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber10} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber11} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber12} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber13} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber14} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber15} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{FlyChamber16} Is this chamber being measured right now? 1=yes; 0=no
#'   \item{CO2_mlminFly1} Carbon dioxide production (ml/min) measurements for
#'   fly 1 
#'   \item{CO2_mlminFly2} Carbon dioxide production (ml/min) measurements
#'   for fly 2 
#'   \item{CO2_mlminFly3} Carbon dioxide production (ml/min)
#'   measurements for fly 3 
#'   \item{CO2_mlminFly4} Carbon dioxide production
#'   (ml/min) measurements for fly 4 
#'   \item{CO2_mlminFly5} Carbon dioxide
#'   production (ml/min) measurements for fly 5 
#'   \item{CO2_mlminFly6} Carbon
#'   dioxide production (ml/min) measurements for fly 6 
#'   \item{CO2_mlminFly7}
#'   Carbon dioxide production (ml/min) measurements for fly 7
#'   \item{CO2_mlminFly8} Carbon dioxide production (ml/min) measurements for
#'   fly 8 
#'   \item{CO2_mlminFly9} Carbon dioxide production (ml/min) measurements
#'   for fly 9 
#'   \item{CO2_mlminFly10} Carbon dioxide production (ml/min)
#'   measurements for fly 10 
#'   \item{CO2_mlminFly11} Carbon dioxide production
#'   (ml/min) measurements for fly 11 
#'   \item{CO2_mlminFly12} Carbon dioxide
#'   production (ml/min) measurements for fly 12 
#'   \item{CO2_mlminFly13} Carbon
#'   dioxide production (ml/min) measurements for fly 13 
#'   \item{CO2_mlminFly14}
#'   Carbon dioxide production (ml/min) measurements for fly 14
#'   \item{CO2_mlminFly15} Carbon dioxide production (ml/min) measurements for
#'   fly 15 
#'   \item{CO2_mlminFly16} Carbon dioxide production (ml/min)
#'   measurements for fly 16 }
#'
#' @source J. Waters