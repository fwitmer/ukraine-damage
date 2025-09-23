##########################################################################
# 10 Dec 2024
#
#	read & subset VIINA data
#
# source: https://github.com/zhukovyuri/VIINA
#
#
##########################################################################

load_VIINA <- function(dataDir,
                       start_date = as.Date("2022-02-01"),
                       end_date = as.Date("2023-09-30")) {
  require(dplyr)

  if (!dir.exists(dataDir))
    print(paste("ERROR invalid dataDir", dataDir))
  
  setwd(dataDir)
  
  print(paste("Reading CSV files from", dataDir))
  viina <- read.csv("event_1pd_latest_2022.csv")
  viina <- rbind(viina, read.csv("event_1pd_latest_2023.csv"))
  # TODO: subset dates to maybe stop at Oct 2023, but for now, just don't add 2024
  #viina <- rbind(viina, read.csv("event_1pd_latest_2024.csv"))
  
  #unique(viina$date)
  #colnames(viina)

  viina <- viina %>%
    mutate(date_obj = as.Date(as.character(date), format="%Y%m%d"))
  
  print(paste("Subsetting events by dates:", start_date, end_date))
  viina_subset <- viina %>%
    filter(date_obj >= start_date & date_obj <= end_date)
  print(paste("    Total events:", nrow(viina_subset))) # 318147

  print("Subseting rows by bombing variables")
  damage_events <- viina_subset %>% 
    filter((t_airstrike_b == 1 | t_armor_b == 1 | t_artillery_b == 1 |
              t_ied_b == 1 | t_property_b == 1))
  print(paste("    Total damage events:", nrow(damage_events))) # 76475
  
  #browser()
  # remove GEO_PRECISION ADM1 and ADM2
  table(damage_events$GEO_PRECISION)
  #  ADM1   ADM2   ADM3 STREET 
  # 14315   3292  55739   3129 
  geo_subset <- damage_events %>%
    filter(GEO_PRECISION == "ADM3" | GEO_PRECISION == "STREET")
  print('After removing ADM1 and ADM2:')
  print(table(geo_subset$GEO_PRECISION))
  #  ADM3 STREET 
  # 55739   3129 
  print(paste("    Total geo_subset events:", nrow(geo_subset))) # 58868
  # 3129 / 58868 * 100 = 5.3% of events are 'STREET' level
  street_pct <- table(geo_subset$GEO_PRECISION)['STREET'] / nrow(geo_subset) * 100
  print(paste("    Percentage of events that are 'STREET':", street_pct))
  
  return(geo_subset)
}

####### Example usage #########
#end_date <- as.Date("2023-09-30")
#source(file.path(viinaDir, "LoadVIINA.R"))
#viina <- load_VIINA(viinaDir, end_date=end_date)
#nrow(viina) # 58868

