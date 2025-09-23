##########################################################################
# 5 Feb 2025
#
# Create combined 4 categories: (ACLED OR VIINA) + SAR:
#   0 nothing reported/detected
#   1 only media reported
#   2 only SAR detected
#   3 both
#
# Generate (initially just for ADM3):
#   - a monthly time series
#   - 4 separate summation maps
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

outDir <- file.path(analysisDir, "CAT_ADM3")
if (!dir.exists(outDir)) {
  print(paste("creating directory:", outDir))
  dir.create(outDir)
}


# load functions to read/join monthly event data & SAR data
source(file.path(analysisDir, "ReadMonthlyData.R"))

#adm3_wide <- join_wide(baseDir, SAR_DIR, "ADM3")
# GHS normalization shouldn't matter, but the code has been tested more
adm3_wide <- join_wide(baseDir, SAR_DIR, "ADM3", GHSnorm = TRUE)

##########################
# Create combined 4 categories: (ACLED OR VIINA) + SAR:
#   0 nothing reported/detected
#   1 only media reported
#   2 only SAR detected
#   3 both
##########################
monthly_categories <- function(wide_df, borders_str) {
  # wide_df <- adm3_wide; borders_str <- "ADM3"
  
  # Extract the monthly column names for each set
  acled_cols <- grep(paste0("^", "ACLED"), colnames(wide_df), value = TRUE)
  viina_cols <- grep(paste0("^", "VIINA"), colnames(wide_df), value = TRUE)
  sar_cols <- grep(paste0("^", "SAR"), colnames(wide_df), value = TRUE)
  
  # Extract month-year from column names (everything after the first underscore)
  months <- sub("^.*?_", "", sar_cols) # must use sar_cols here since they end 1 month earlier
  
  # Initialize a data frame for results
  ID_var <- paste0(borders_str, "_PCODE")
  results <- data.frame(wide_df[[ID_var]])
  colnames(results) <- ID_var

  # loop through each month
  #   a bit risky since it assumes colnames are all ordered by date
  for (i in seq_along(months)) {
    #print(paste(acled_cols[i], viina_cols[i], sar_cols[i]))
    cat_var <- paste0("CAT_", months[i])
    results[[cat_var]] <- 0
    events_mnth <- wide_df[[acled_cols[i]]] | wide_df[[viina_cols[i]]]
    media_only <- events_mnth & wide_df[[sar_cols[i]]] == 0
    sar_only <- !events_mnth & wide_df[[sar_cols[i]]] > 0
    both <- events_mnth & wide_df[[sar_cols[i]]] > 0
    results[media_only, cat_var] <- 1
    results[sar_only, cat_var] <- 2
    results[both, cat_var] <- 3
    #tmp <- fix(results)
  }
  #head(results, 15)
  return(results)
}

adm3_cat <- monthly_categories(adm3_wide, "ADM3")

##################################

####### load border ADM3 & ADM4 data #########
adm0_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm0_sspe_20240416.shp")
adm0_sf <- st_read(adm0_fname)

adm1_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm1_sspe_20240416.shp")
adm1_sf <- st_read(adm1_fname)

adm3_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm3_sspe_20240416.shp")
adm3_sf <- st_read(adm3_fname)

#adm4_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm4_sspe_20240416.shp")
#adm4_sf_bad_geo <- st_read(adm4_fname)
#adm4_fixed <- file.path(baseDir, "Borders/ukr_admbnda_adm4_fixed.gpkg")
#adm4_sf <- st_read(adm4_fixed)

##################################
# join categories to borders shapefile & plot
#   write monthly pngs maps to a given directory
##################################
monthly_cat_pngs <- function(adm_cat, borders_str) {
  #adm_cat <- adm3_cat; borders_str <- "ADM3"

  ID_var <- paste0(borders_str, "_PCODE")
  print("WARNING: adm3_df hardcoded here")
  adm_evnts_small <- adm3_sf[, c(ID_var, "geometry")]
  adm_cat_sf <- left_join(adm_evnts_small, adm_cat, by = ID_var)
  #colnames(adm_cat_sf)
  
#  adm_cat_sp <- as(adm_cat_sf, "Spatial")

  cat_mnths <- grep(paste0("^", "CAT"), colnames(adm_cat_sf), value = TRUE)

  if (borders_str == "ADM3")
    border_col <- "gray" #"#2b2b2b" 
  else if (borders_str == "ADM4")
    border_col <- NA # remove border color for ADM4
  else
    print(paste("ERROR: unexpected borders_str", borders_str))
  cat_colors <- c("0" = "white", "1" = "#4daf4a", "2" = "#377eb8", "3" = "#e41a1c")
  for (cat_mnth in cat_mnths) { # cat_mnth <- cat_mnths[1]
    mnth_yr_str <- gsub("CAT_", "", cat_mnth)  # Remove prefix
    date_obj <- as.Date(paste0("01_", mnth_yr_str), format = "%d_%m_%Y")
    mnth_label <- format(date_obj, "%b %Y")
    yr_mnth <- format(date_obj, "%Y_%m")
    #head(adm_cat_sf[[cat_mnth]])
    adm_cat_sf$cat_factor <- factor(as.character(adm_cat_sf[[cat_mnth]]), levels = names(cat_colors))
#    adm_cat_sf$cat_factor <- factor(as.character(adm_cat_sf[[cat_mnth]]))
    gg_plt <- ggplot(adm_cat_sf) +
      geom_sf(aes(fill = cat_factor), color = border_col) +
      geom_sf(data = adm1_sf, fill = NA, color = "#2b2b2b", linewidth = 0.1) +
      geom_sf(data = adm0_sf, fill = NA, color = "#101010", linewidth = 0.4) +
      scale_fill_manual(
        values = cat_colors,
        labels = c("0" = "No damage", 
                   "1" = "Media report", 
                   "2" = "SAR damage", 
                   "3" = "Both")) +
      labs(fill = "Category", title = paste0("Damage category by ", borders_str, " borders: ", mnth_label)) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA), # White background for the map
        plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
        panel.grid = element_blank(),  # Remove gridlines
        plot.title = element_text(size = 14, hjust = 0.5), # Adjust title position
        
        #legend.position = "right",     # Position the legend
        legend.position = "inside",
        legend.position.inside = c(0.05, 0.05),
        legend.justification = c("left", "bottom"),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
        
      )
    #print(gg_plt)
    # write plot to png file
    fname <- paste0("CAT_", borders_str, "_", yr_mnth, ".png")
    print(paste("writing file", fname))
    png_filename <- file.path(outDir, fname)
    ggsave(png_filename, plot = gg_plt, width = 8, height = 6)
    
  }
}  

monthly_cat_pngs(adm3_cat, "ADM3")

source(file.path(analysisDir, "MakeGifFunction.R"))
make_gif(analysisDir, "Cat_ADM3")


##################################
# count the number of times each district is in a category
# REQUIRES: adm3_sf
##################################
cat_counts <- function(adm_cat, borders_str) {
  #adm_cat <- adm3_cat; borders_str <- "ADM3"
  #colnames(adm_cat)
  # Initialize a data frame for results
  ID_var <- paste0(borders_str, "_PCODE")
  adm_cat_counts <- data.frame(adm_cat[[ID_var]])
  colnames(adm_cat_counts) <- ID_var

  for (i in 0:3) {
    adm_cat_counts[[paste0("COUNT_", i)]] <- rowSums(adm_cat[,-1] == i, na.rm = TRUE)
  }
  print("WARNING: adm3_sf hardcoded")
  adm_evnts_small <- adm3_sf[, c(ID_var, "geometry")]
  adm_cat_counts_sf <- left_join(adm_evnts_small, adm_cat_counts, by = ID_var)
  return(adm_cat_counts_sf)
}

cat_cnts_sf <- cat_counts(adm3_cat, "ADM3")

##################################
# map given category counts
# REQUIRES: outDir
##################################
map_counts <- function(cat_cnts_sf, cnt_var, borders_str, max_mnths, dmg_label, max_color) {
  # cnt_var <- "COUNT_0"; dmg_label <- "no damage"
  max_val <- max(cat_cnts_sf[[cnt_var]])
  print(paste("Actual max count =", max_val))
  print(paste("Using max count of", max_mnths))
#  med_val <- median(cat_cnts_sf[[cnt_var]])
  if (borders_str == "ADM3")
    border_col <- "gray" #"#2b2b2b" 
  else if (borders_str == "ADM4")
    border_col <- NA # remove border color for ADM4
  else
    print(paste("ERROR: unexpected borders_str", borders_str))
  gg_plt <- ggplot(cat_cnts_sf) +
    geom_sf(aes(fill = !!sym(cnt_var)), color = border_col) +
    geom_sf(data = adm1_sf, fill = NA, color = "#2b2b2b", linewidth = 0.1) +
    geom_sf(data = adm0_sf, fill = NA, color = "#101010", linewidth = 0.4) +
    #    scale_fill_viridis_c(option = "magma",
    scale_fill_gradient2(low = "white", high = max_color,
                         limits = c(0, max_mnths),
                         name = "Month count") +
    labs(fill = "Category", title = paste0("Number of months with ", dmg_label)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA), # White background for the map
      plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
      panel.grid = element_blank(),  # Remove gridlines
      plot.title = element_text(size = 14, hjust = 0.5), # Adjust title position
      
      #legend.position = "right",     # Position the legend
      legend.position = "inside",
      legend.position.inside = c(0.05, 0.05),
      legend.justification = c("left", "bottom"),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
      axis.title = element_blank(),
      axis.text = element_blank(), # remove the lat/lon text labels
      axis.ticks = element_blank()
    )
  #print(gg_plt)
  # write plot to png file
  fname <- paste0("CAT_", borders_str, "_", cnt_var, ".png")
  print(paste("writing file", fname))
  png_filename <- file.path(outDir, fname)
  ggsave(png_filename, plot = gg_plt, width = 8, height = 6)
}

max_mnths <- ncol(adm3_cat) - 1
map_counts(cat_cnts_sf, "COUNT_0", "ADM3", max_mnths, "no damage", "#8856a7")
map_counts(cat_cnts_sf, "COUNT_1", "ADM3", max_mnths, "media reported damage", "#4daf4a")
map_counts(cat_cnts_sf, "COUNT_2", "ADM3", max_mnths, "SAR damage", "#377eb8")
map_counts(cat_cnts_sf, "COUNT_3", "ADM3", max_mnths, "both media & SAR damage", "#e41a1c")
