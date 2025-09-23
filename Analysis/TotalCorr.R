##########################################################################
# 14 May 2025
#
#	combine violent event point data and SAR damage data in wide format
#   to calculate total correlation coefficients
#   aggregated at both ADM3 and ADM4 borders 
#
# border data source: Humanitarian Data Exchange, OCHA
#   https://data.humdata.org/dataset/cod-ab-ukr
#
##########################################################################

library(sf)
library(dplyr) 
library(tidyr) # for pivot_longer
library(stringr) # for str_replace_all
library(ggplot2)

baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
analysisDir <- file.path(baseDir, "Analysis/")
if (!dir.exists(analysisDir))
  print(paste("ERROR invalid analysisDir", analysisDir))

SAR_Dir <- file.path(baseDir, "SAR_GEE/Monthly_Damage/")
if (!dir.exists(SAR_Dir))
  print(paste("ERROR invalid SAR_Dir", SAR_Dir))

# load functions to read/join monthly event data & SAR data
source(file.path(analysisDir, "ReadMonthlyData.R"))

borders_str <- "ADM3"
adm3_wide <- join_wide(baseDir, SAR_DIR, borders_str, GHSnorm = TRUE)

##########################
# calculate sums for each variable
##########################
row_totals <- function(wide_df, borders_str) {
  # wide_df <- adm3_wide
  ID_var <- paste0(borders_str, "_PCODE")
  wide_totals <- wide_df[, c(ID_var, "AREA_SQKM")]
  
  # Extract the monthly column names for each set
  viina_cols <- grep(paste0("^", "VIINA"), colnames(wide_df), value = TRUE)
  acled_cols <- grep(paste0("^", "ACLED"), colnames(wide_df), value = TRUE)
  sar_cols <- grep(paste0("^", "SAR"), colnames(wide_df), value = TRUE)
  
  wide_totals$viina <- rowSums(wide_df[, viina_cols])
  wide_totals$acled <- rowSums(wide_df[, acled_cols])
  wide_totals$SAR <- rowSums(wide_df[, sar_cols])
  
  return(wide_totals)
}

wide_totals <- row_totals(adm3_wide, "ADM3")

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

cor_df <- cor_three(wide_totals, cor_df, "pearson")
cor_df <- cor_three(wide_totals, cor_df, "spearman")
cor_df <- cor_three(wide_totals, cor_df, "kendall")

# explore sqrt transform effect on correlation
wide_sqrt <- wide_totals[, c("viina", "acled", "SAR")] %>%
  mutate(across(
    everything(),
    ~ (sqrt(.))
  ))
cor_sqrt_df <- data.frame(cor_name = c("VIINA-SAR", "ACLED-SAR", "VIINA-ACLED"))
rownames(cor_sqrt_df) <- cor_sqrt_df$cor_name
cor_sqrt_df <- cor_three(wide_sqrt, cor_sqrt_df, "pearson")
cor_sqrt_df <- cor_three(wide_sqrt, cor_sqrt_df, "spearman")
cor_sqrt_df <- cor_three(wide_sqrt, cor_sqrt_df, "kendall")
cor_sqrt_df["VIINA-SAR", "distance"] <- dcor(wide_sqrt$viina, wide_sqrt$SAR)
cor_sqrt_df["ACLED-SAR", "distance"] <- dcor(wide_sqrt$acled, wide_sqrt$SAR)
cor_sqrt_df["VIINA-ACLED", "distance"] <- dcor(wide_sqrt$viina, wide_sqrt$acled)
print(cor_sqrt_df) # as expected, no change on coeffs for spearman & kendall

#install.packages("energy")
library(energy)
cor_df["VIINA-SAR", "distance"] <- dcor(wide_totals$viina, wide_totals$SAR)
cor_df["ACLED-SAR", "distance"] <- dcor(wide_totals$acled, wide_totals$SAR)
cor_df["VIINA-ACLED", "distance"] <- dcor(wide_totals$viina, wide_totals$acled)

