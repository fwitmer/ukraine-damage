##########################################################################
# 26 Mar 2025
#
#	read & subset ACLED data
#
# source: https://acleddata.com/data/
#
#
##########################################################################

# default dates are for backwards compatibility
load_ACLED <- function(dataDir,
                       start_date = as.Date("2022-02-01"),
                       end_date = as.Date("2023-09-30")) {
  #dataDir <- acledDir
  require(dplyr)

  if (!dir.exists(dataDir))
    print(paste("ERROR invalid dataDir", dataDir))
  
  setwd(dataDir)
  
  print(paste("Reading ACLED CSV file from", dataDir))
  acled <- read.csv(file.path(dataDir, "ACLED_2022-02-01-2023-12-08-Ukraine.csv"))
  #colnames(acled)
  print(paste("Subset events by dates:", start_date, end_date))
  acled <- acled %>%
    mutate(date_obj = as.Date(event_date, format = "%d %B %Y"))
  #range(acled$date_obj) # 1 Feb 2022 - 31 Dec 2023
  acled <- acled %>%
    filter(date_obj >= start_date & date_obj <= end_date)
  print(paste("    Total events:", nrow(acled))) # 60060

  print("Remove geo_precision = 3 (province)")
  table(acled$geo_precision) # 1 = town, 2 = small region, 3 = province
  #     1     2     3 
  # 25226 34361   473
  acled <- acled %>%
    filter(geo_precision == 1 | geo_precision == 2)
  print(table(acled$geo_precision))
  #     1     2 
  # 25226 34361
  print(paste("    Total geo subset events:", nrow(acled))) # 
  
  return(acled)
  
}
