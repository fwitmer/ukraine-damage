##########################################################################
# 3 Jan 2025
#
#	combine violent event point data and SAR damage data in long format
#   aggregated at both ADM3 and ADM4 borders 
#
# and calculate correlation coefficients
#   also explore 1 month lead (lag of -1) of SAR damage data
#
# border data source: Humanitarian Data Exchange, OCHA
#   https://data.humdata.org/dataset/cod-ab-ukr
#
# TODO: would probably be better to have this read from the raw data & not the processed shapefiles/csv files
# TODO: rename this file to something like LongCorrelations.R
##########################################################################

library(sf)
library(dplyr) 
library(tidyr) # for pivot_longer
library(stringr) # for str_replace_all

baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
analysisDir <- file.path(baseDir, "Analysis/")
if (!dir.exists(analysisDir))
  print(paste("ERROR invalid analysisDir", analysisDir))

SAR_Dir <- file.path(baseDir, "SAR_GEE/Monthly_Damage/")
if (!dir.exists(SAR_Dir))
  print(paste("ERROR invalid SAR_Dir", SAR_Dir))

# load join_GHS function
source(file.path(analysisDir, "ReadMonthlyData.R"))


########################################
# read border-month data from shapefiles
#   convert event data to long format
# REQUIRES: baseDir
########################################
events_to_long <- function(borders_str, events_str, GHS_norm = FALSE) {
  #borders_str <- "ADM3"; events_str <- "ACLED"
  #brdr_evnts <- paste0(borders_str, "_", events_str)
  folder <- paste(borders_str, events_str, sep="_")
  print(paste("Processing", folder))
  inDir <- file.path(analysisDir, folder)
  inFile <- file.path(inDir, paste0("ukr_",tolower(folder),".shp"))
  
  adm_evnts_sf <- st_read(inFile)

  ID_var <- paste0(borders_str, "_PCODE")
  joined_sf <- join_GHS(baseDir, borders_str, ID_var, adm_evnts_sf)
  
  #is(adm_evnts_sf); is(joined_sf)
  head(joined_sf)
  #unique(joined_sf$m05_2022) # event counts
  #hist(joined_sf$GHS_urban)
  
  print(paste('Correlation between ADM area and urban area for', borders_str))
  print(cor(joined_sf$AREA_SQKM, joined_sf$GHS_urban))
  
  if (GHS_norm) {
    print("     Normalizing each month by GHS urban area (km2)")
    # WARNING: results in an Inf value for ADM4 due to divide by 0
    joined_sf <- joined_sf %>%
      filter(GHS_urban > 0)
    
    adm_evnts_norm <- joined_sf %>%
      mutate(across(
        matches("^m"), # Select columns starting with "m"
        ~ (. / GHS_urban) * 1000000 # normalize & convert to km2 (events/km2)
      ))
  } else { # area normalization
    #sum(joined_sf$AREA_SQKM <= 0)
    print("     Normalizing each month by district area (km2)")
    adm_evnts_norm <- joined_sf %>%
      mutate(across(
        matches("^m"), # Select columns starting with "m"
        ~ (. / AREA_SQKM)  # normalize by ADM district area
      ))
  }
  #browser()
  #head(adm_mnth_norm)
  #hist(adm_evnts_norm$m05_2022)

  sf_long_all <- pivot_longer(adm_evnts_norm, 
                              cols = matches("^m\\d{2}_\\d{4}$"), # "m??_????"
                              #cols = starts_with("m02_2022"):starts_with("m10_2023"),
                              #cols = starts_with("m02_2022"):starts_with("m09_2023"),
                              names_to = "Month", values_to = "Value")
  
  #colnames(sf_long_all)
  sf_long <- sf_long_all[, c(ID_var, "Month", "Value")]
  #colnames(sf_long) # drop the geometry info so that the left_join works
  sf_long <- sf_long %>%
    st_drop_geometry()
  
  sf_long2 <- sf_long %>%
    mutate(adm_month = paste0(.data[[ID_var]], ".", Month))
  #head(sf_long2)
  #range(sf_long2$Value) # 0 to 5634
  #hist(sf_long2$Value)
  return(sf_long2)
}

########################################
# read border-month data from csv file
#   convert SAR data to long format
# REQUIRES: baseDir
########################################
SAR_to_long <- function(borders_str, GHS_norm = FALSE) {
  #borders_str <- "ADM3"
  print(paste("Processing SAR data,", borders_str))
  ID_var <- paste0(borders_str, "_PCODE")

  if (GHS_norm) {
    SAR_Dir <- file.path(baseDir, "/SAR_GEE/Monthly_Damage/")
    if (!dir.exists(SAR_Dir))
      print(paste("ERROR invalid SAR_Dir", SAR_Dir))
    adm_mnth_norm <- read_sar_GHSnorm(SAR_Dir, borders_str) # from ReadMonthlyData.R
  } else {
    adm_mnth_norm <- read_sar(SAR_Dir, borders_str) # from ReadMonthlyData.R
  }
  #head(adm_mnth_norm) # "SAR_03_2022"
  #browser()
  
  # rename columns from "SAR_03_2022" to "m03_2022" to match the event long data
  adm_mnth_norm <- adm_mnth_norm %>%
    rename_with(
      .fn = ~ str_replace_all(., "^SAR_(\\d{2})\\_(\\d{4})$", "m\\1_\\2"),
      .cols = starts_with("SAR")
    )
  
  long_all <- pivot_longer(adm_mnth_norm, 
                           #cols = starts_with("X2022.02"):starts_with("X2023.09"),
                           #cols = starts_with("m02_2022"):starts_with("m09_2023"),
                           cols = matches("^m\\d{2}_\\d{4}$"), # "m??_????"
                           names_to = "Month", values_to = "Value")
  
  #colnames(long_all)
  long <- long_all[, c(ID_var, "Month", "Value")]

  long2 <- long %>%
    mutate(adm_month = paste0(.data[[ID_var]], ".", Month))
  #head(long2)
  return(long2)
}

join_long <- function(borders_str) { # borders_str <- "ADM3"
  acled <- events_to_long(borders_str, "ACLED", GHS_norm = FALSE)
  viina <- events_to_long(borders_str, "VIINA", GHS_norm = FALSE)
  SAR <- SAR_to_long(borders_str, GHS_norm = TRUE)
  
  acled <- acled %>%
    select(adm_month, Value) %>%
    rename(acled = Value)
  
  viina <- viina %>%
    select(adm_month, Value) %>%
    rename(viina = Value)
  
  SAR <- SAR %>%
    select(adm_month, Value) %>%
    rename(SAR = Value)
  
  joined <- acled %>%
    left_join(viina, by = "adm_month") %>%
    left_join(SAR, by = "adm_month")
  
  #head(joined)
  return(joined)
}

joined_adm3 <- join_long("ADM3")
joined_adm4 <- join_long("ADM4")

# [OLD comment, this has been fixed...]
# for the SAR data, May 2023 actually reflects SAR damage in Apr 2023
#   so apply forward shift to the SAR data to account for this
joined_adm3_lead <- joined_adm3 %>%
  mutate(
    admin_region = str_extract(adm_month, "^[^.]+"),  # Extracts the part before '.m'
    year = as.numeric(str_extract(adm_month, "\\d{4}$")),  # Extracts 4-digit year
    month = as.numeric(str_extract(adm_month, "(?<=\\.m)\\d{2}"))  # Extracts the month number
  ) %>%
  arrange(admin_region, year, month) %>%  # Ensure proper order
  group_by(admin_region) %>%  # Shift only within each admin unit
  mutate(SAR_lead = lead(SAR, n = 1)) %>%
  mutate(SAR_lead2 = lead(SAR, n = 2)) %>%
  mutate(SAR_lag = lag(SAR, n = 1)) %>%
  mutate(SAR_lag2 = lag(SAR, n = 2)) %>%
  ungroup() %>%
  select(-admin_region, -year, -month)  # Remove helper columns if not needed

head(joined_adm3_lead)
joined_adm3_lead[13377:13384,]
tail(joined_adm3_lead)
#tmp <- fix(joined_adm3_lead)

# calculate new correlation coefficients
cor(joined_adm3_lead$SAR, joined_adm3_lead$viina, use = "complete.obs")      # 0.24
cor(joined_adm3_lead$SAR_lead, joined_adm3_lead$viina, use = "complete.obs") # 0.18
#cor(joined_adm3_lead$SAR_lead2, joined_adm3_lead$viina, use = "complete.obs")
#cor(joined_adm3_lead$SAR_lag, joined_adm3_lead$viina, use = "complete.obs")
#cor(joined_adm3_lead$SAR_lag2, joined_adm3_lead$viina, use = "complete.obs")

cor(joined_adm3_lead$SAR, joined_adm3_lead$acled, use = "complete.obs")      # 0.20
cor(joined_adm3_lead$SAR_lead, joined_adm3_lead$acled, use = "complete.obs") # 0.19
#cor(joined_adm3_lead$SAR_lead2, joined_adm3_lead$acled, use = "complete.obs")
#cor(joined_adm3_lead$SAR_lag, joined_adm3_lead$acled, use = "complete.obs")
#cor(joined_adm3_lead$SAR_lag2, joined_adm3_lead$acled, use = "complete.obs")


############################################
#install.packages("corrplot")
library(corrplot)

corr_plot <- function(joined_df, borders_str, suffix="") {
  selected_data <- joined_df %>%
    select(acled, viina, SAR)
  
#  browser()
#  complete_df <- selected_data[complete.cases(selected_data), ]
#  nrow(selected_data); nrow(complete_df)
  # Compute the correlation matrix
#  cor_matrix <- cor(complete_df, use = "complete.obs")  # Use "complete.obs" to
  cor_matrix <- cor(selected_data, use = "complete.obs")  # Use "complete.obs" to handle missing values 
#  var(complete_df$acled)
#  mean(complete_df$acled)
#  max(complete_df$acled)
#  var(complete_df$viina)
#  var(complete_df$SAR)

  outFile <- file.path(analysisDir, "Correlations", paste0(borders_str, "_corrNorm",suffix,".png"))
  print(paste("writing png to:", outFile))
  par(mar = c(1, 1, 4, 1))  # Bottom, left, top, right margins
  png(outFile, width = 600, height = 450)
    corrplot(cor_matrix, method = "color", type = "upper", 
             tl.cex = 1.2, tl.col = "black", diag = FALSE,
             title=paste(borders_str, "correlations"),
             mar = c(0, 0, 2, 0),
             addCoef.col = "black", # Add correlation coefficients
             number.cex = 1.2)      # Adjust the size of the coefficients
  
  dev.off()
}

corr_plot(joined_adm3, "ADM3", "SAR_GHS")
#corr_plot(joined_adm3, "ADM3")
#corr_plot(joined_adm4, "ADM4")


hist(sqrt(joined_adm3$viina))
hist(sqrt(joined_adm3$SAR))
##################################
# calculate 3 versions of the correlation coefficient
##################################
cor_three <- function(joined_df, cor_df, method_name) {
  cor_df["VIINA-SAR", method_name] <- cor(joined_df$viina, joined_df$SAR, method = method_name)
  cor_df["ACLED-SAR", method_name] <- cor(joined_df$acled, joined_df$SAR, method = method_name)
  cor_df["VIINA-ACLED", method_name] <- cor(joined_df$viina, joined_df$acled, method = method_name)
  return(cor_df)
}
cor_df <- data.frame(cor_name = c("VIINA-SAR", "ACLED-SAR", "VIINA-ACLED"))
rownames(cor_df) <- cor_df$cor_name
cor_df <- cor_three(joined_adm3, cor_df, "pearson")
cor_df <- cor_three(joined_adm3, cor_df, "spearman")
cor_df <- cor_three(joined_adm3, cor_df, "kendall") # ~1 minute
print(cor_df)

outFile <- file.path(analysisDir, "Correlations", paste0("ADM3", "_correlations_","GHSnorm",".csv"))
write.csv(cor_df, outFile, row.names = FALSE)


# subset events to include just event-districts with media (VIINA or ACLED) and SAR
joined_adm3_mediaSAR <- joined_adm3[(joined_adm3$acled > 0 | joined_adm3$viina > 0) & joined_adm3$SAR > 0, ]
nrow(joined_adm3)         # 35380
nrow(joined_adm3_mediaSAR) # 4105

corr_plot(joined_adm3_mediaSAR, "ADM3", "_nonZeroSubset")


################################################
# calculate distance correlation
#   computationally intensive!
################################################
#install.packages("energy")
library(energy)
gc()
### Error: cannot allocate vector of size 9.3 Gb
#dcor(joined_adm3$viina, joined_adm3$SAR)
#dcor(joined_adm3$acled, joined_adm3$SAR)
#dcor(joined_adm3$viina, joined_adm3$acled)

dcor(joined_adm3_mediaSAR$viina, joined_adm3_mediaSAR$SAR) # 0.165
dcor(joined_adm3_mediaSAR$acled, joined_adm3_mediaSAR$SAR) # 0.302
dcor(joined_adm3_mediaSAR$viina, joined_adm3_mediaSAR$acled) # 0.442

# media events are the limiting factor here
nrow(joined_adm3[(joined_adm3$acled > 0 | joined_adm3$viina > 0), ]) # 5833
nrow(joined_adm3[joined_adm3$SAR > 0, ])  # 15288

# remove empty SAR district-months
joined_adm3_SAR <- joined_adm3[joined_adm3$SAR > 0, ]
nrow(joined_adm3_SAR) # 15288
dcor(joined_adm3_SAR$viina, joined_adm3_SAR$SAR) # 0.211
dcor(joined_adm3_SAR$acled, joined_adm3_SAR$SAR) # 0.304
dcor(joined_adm3_SAR$viina, joined_adm3_SAR$acled) # 0.556
gc()
