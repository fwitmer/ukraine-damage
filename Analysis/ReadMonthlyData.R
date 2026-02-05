##########################################################################
# 4 Feb 2025
# 
# Author: Frank Witmer
#
#	Functions to read media events and SAR data aggregated to ADM3 or ADM4
#
# Border data source: Humanitarian Data Exchange, OCHA
#   https://data.humdata.org/dataset/cod-ab-ukr
#   version: ukr_admbnda_adm3_sspe_20240416.shp
#
##########################################################################

library(sf)
library(dplyr) 
library(lubridate) # for SAR month shift
library(stringr) # for str_replace_all

##########################
# join GHS data
#   GHS_urban units are 10x10m pixel counts
# GHS source:
#   https://human-settlement.emergency.copernicus.eu/ghs_buS2023.php
##########################
join_GHS <- function(baseDir, borders_str, ID_var, target_df) {
  print("Joining GHS urban data...")
  GHS_Dir <- file.path(baseDir, "GHS_Mask")
  GHS_file <- file.path(GHS_Dir, paste0("UrbanCounts_",borders_str,".csv"))
  GHS_area <- read.csv(GHS_file)
  
  joined_ghs <- target_df %>%
    left_join(GHS_area[c(ID_var, "GHS_urban")], by = ID_var)
  #is(target_df); is(joined_ghs)
  #head(joined_ghs)
  return(joined_ghs)
}


##########################
# read events, rename monthly data fields, return slimmed df
##########################
read_events <- function(baseDir, borders_str, events_str) {
  #borders_str <- "ADM3"; events_str <- "ACLED"
  analysisDir <- file.path(baseDir, "Analysis/")
  if (!dir.exists(analysisDir))
    print(paste("ERROR invalid analysisDir", analysisDir))
  folder <- paste(borders_str, events_str, sep="_")
  inDir <- file.path(analysisDir, folder)
  inFile <- file.path(inDir, paste0("ukr_",tolower(folder),".shp"))
  
  print(paste("Reading & renaming columns for", folder))
  adm_evnts_sf <- st_read(inFile)
  
  # drop geometry since shapefiles don't support column names > 10 chars
  adm_evnts <- adm_evnts_sf %>%
    st_drop_geometry()
  #colnames(adm_evnts)
  
  # rename columns from "m02_2022" to "ACLED_02_2022" using events_str
  adm_evnts <- adm_evnts %>%
    rename_with(~ gsub("^m", paste0(events_str, "_"), .), starts_with("m"))
  
  ID_var <- paste0(borders_str, "_PCODE")
  joined_ghs <- join_GHS(baseDir, borders_str, ID_var, adm_evnts)
  
  adm_evnts_subset <- joined_ghs %>%
    select(all_of(c(ID_var, "AREA_SQKM", "GHS_urban")), starts_with(events_str))
  #head(adm_evnts_subset)

  # TODO: normalize counts by AREA_SQKM here and remove duplicate code elsewhere...?
  
  return(adm_evnts_subset)
}

########################
# remove rows from df that have 0s for normalization column
########################
clean_df <- function(agg_df, norm_col) {
  if (sum(is.na(agg_df[[norm_col]])) > 0) {
    print("WARNING: removing NA polygons [THIS IS UNEXPECTED HERE]")
    agg_df <- agg_df %>%
      filter(!is.na(.data[[norm_col]]))
  }
  if (sum(agg_df[[norm_col]] <= 0) > 0) {
    print("WARNING: removing polygons with no potential SAR damage area")
    # remove those 972 districts that have no potential SAR damage area
    agg_df <- agg_df %>%
      filter(.data[[norm_col]] > 0)
  }
  #sum(agg_df[[norm_col]] <= 0) # 0
  #sum(is.na(agg_df$X3_2022)) # 0
  
  return(agg_df)
}


##########################
# read SAR data, rename monthly data fields, return slimmed df
# REQUIRES: SAR_Dir
##########################
read_sar <- function(SAR_Dir, borders_str, SAR_cols_only = TRUE) {
  #borders_str <- "ADM3"
  fname = paste("MonthlyDamageCountsBy",borders_str,".csv", sep="")
  sar_df <- read.csv(file.path(SAR_Dir, fname))
  #colnames(sar_df)
  #browser()

  # Select only columns that match the pattern XYYYY.MM
  date_cols <- grep("^X\\d{4}\\.\\d{2}$", colnames(sar_df), value = TRUE)
  print('old column names (head & tail):')
  print(head(date_cols))
  print(tail(date_cols))
  
  # Extract and shift the dates
  new_date_labels <- date_cols %>%
    gsub("^X", "", .) %>%                            # Remove the leading 'X'
    ym() %>%                                         # Parse as year-month
    (\(x) `%m+%`(x, months(1)))() %>%                # Add 1 month
    format("SAR_%m_%Y")                              # Format to SAR_MM_YYYY
  
  # Create a named vector for renaming: names are old, values are new
#  rename_vector <- setNames(new_date_labels, date_cols) # wrong order
  rename_vector <- setNames(date_cols, new_date_labels)
  
  # Apply renaming
  sar_df <- sar_df %>%
    rename(!!!rename_vector)

  print('new column names (head & tail):')
  print(head(colnames(sar_df)))
  print(tail(colnames(sar_df)))
  
  ID_var <- paste0(borders_str, "_PCODE")
  
  if (SAR_cols_only) {
    # normalize the month columns by valid_pixel_sum
    print("WARNING: this code should not be run!!!!")
    sar_df <- clean_df(sar_df, "valid_pixel_sum")
    adm_mnth_norm <- sar_df %>%
      mutate(across(
        matches("^SAR"), # Select columns starting with "SAR"
        ~ (. / valid_pixel_sum) * 100 # normalize & convert to percentage
      ))
    print("Normalized SAR area by valid area for detection and returning SAR data subset...")
    adm_sar_subset <- adm_mnth_norm %>%
      select(all_of(ID_var), starts_with("SAR"))
    #head(adm_sar_subset)
    return(adm_sar_subset)
  }

  joined_sf <- join_GHS(baseDir, borders_str, ID_var, sar_df)
  
  print("Returning raw SAR data")
  return(joined_sf)
  
  # OLD: rename columns from "X2022.02" to "SAR_02_2022"
  #sar_df <- sar_df %>%
  #  rename_with(
  #    .fn = ~ str_replace_all(., "^X(\\d{4})\\.(\\d{2})$", "SAR_\\2_\\1"),
  #    .cols = starts_with("X")
  #  )

  # OLD: rename columns from "m02_2022" to "SAR_02_2022"
  #  adm_mnth_norm <- adm_mnth_norm %>%
  #    rename_with(~ gsub("^m", paste0("SAR_"), .), starts_with("m"))

}



##########################
# read SAR data, rename monthly data fields, return slimmed df
#   normalize using GHS urban data
##########################
read_sar_GHSnorm <- function(SAR_Dir, borders_str) {
  sar_df <- read_sar(SAR_Dir, borders_str, SAR_cols_only = FALSE)
  #browser()
  
  sar_df <- clean_df(sar_df, "GHS_urban")
  # normalize the month columns by GHS_Urban
  adm_mnth_norm <- sar_df %>%
    mutate(across(
      matches("^SAR"), # Select columns starting with "SAR"
      ~ (. / GHS_urban) * 100 # normalize & convert to percentage
    ))
  print("Normalized SAR area by GHS urban area for detection and returning SAR data subset...")
  
  ID_var <- paste0(borders_str, "_PCODE")
  adm_sar_subset <- adm_mnth_norm %>%
    select(all_of(ID_var), starts_with("SAR"))
  #head(adm_sar_subset)
  
  return(adm_sar_subset)
}  

##########################
# read ACLED, VIINA, and SAR data & merge them
##########################
join_wide <- function(baseDir, SAR_DIR, borders_str, GHSnorm = FALSE) {
  # borders_str = "ADM3"
  acled <- read_events(baseDir, borders_str, "ACLED")
  viina <- read_events(baseDir, borders_str, "VIINA")
  if (GHSnorm) {
    SAR <- read_sar_GHSnorm(SAR_Dir, borders_str)
  } else {
    SAR <- read_sar(SAR_Dir, borders_str, SAR_cols_only=FALSE)
  }
  
  ID_var <- paste0(borders_str, "_PCODE")
  
  # remove duplicate columns from viina (should be "AREA_SQKM" & "GHS_urban")
  common_cols <- setdiff(intersect(names(viina), names(acled)), ID_var)
  print("Removing duplicate columns from VIINA...")
  print(common_cols)
  viina_unique <- viina %>%
    select(-all_of(common_cols))
  
  joined <- acled %>%
    left_join(viina_unique, by = ID_var) %>%
    left_join(SAR, by = ID_var)
  
  return(joined) # adm3_wide <- joined
}

