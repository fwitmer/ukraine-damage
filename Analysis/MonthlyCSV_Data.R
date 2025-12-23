##########################################################################
# 30 Jan 2025
# 
# Author: Frank Witmer
#
#	Write 3 separate CSV files to share with Corey Scher
#   original plan was to write shapefiles, but they limit column names to 10 characters
#
# Border data source: Humanitarian Data Exchange, OCHA
#   https://data.humdata.org/dataset/cod-ab-ukr
#   version: ukr_admbnda_adm3_sspe_20240416.shp
#
##########################################################################

library(sf)
library(dplyr) 
library(stringr) # for str_replace_all

baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
analysisDir <- file.path(baseDir, "Analysis/")
if (!dir.exists(analysisDir))
  print(paste("ERROR invalid analysisDir", analysisDir))

SAR_Dir <- file.path(baseDir, "SAR_GEE/Monthly_Damage/")
if (!dir.exists(SAR_Dir))
  print(paste("ERROR invalid SAR_Dir", SAR_Dir))

outDir <- file.path(analysisDir, "MonthlyData")
if (!dir.exists(outDir))
  print(paste("ERROR invalid outDir", outDir))

# load functions to read monthly event data & SAR data
source(file.path(analysisDir, "ReadMonthlyData.R"))

##########################
# write data to csv file
##########################
write_csv <- function(adm_mnthly, file_prefix) {
  outFile <- file.path(outDir, paste0(file_prefix, "_mnth.csv"))
  print(paste('Writing csv file to:', outFile))
  write.csv(adm_mnthly, outFile, row.names = FALSE)
  #outFile <- file.path(outDir, paste0(brdr_evnts,"_mnth.shp"))
  #st_write(adm_evnts_subset, outFile) # Grrr: truncates field names to length 10
}

borders_str <- "ADM3"
acled <- read_events(baseDir, borders_str, "ACLED")
write_csv(acled, paste0(borders_str, "_ACLED"))

viina <- read_events(baseDir, borders_str, "VIINA")
write_csv(viina, paste0(borders_str, "_VIINA"))

SAR <- read_sar(SAR_Dir, borders_str)
write_csv(SAR, paste0(borders_str, "_SAR"))

