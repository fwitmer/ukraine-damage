##########################################################################
# 5 Mar 2025
#
# Create composite maps using measured magnitudes (not just binary categories)
#   1) normalize counts by area & SAR by by valid possible area
#   2) calculate mean(ACLED, VIINA) and then normalized by quantile, force range 0 to 50
#   SAR normalized by quantile, force range 0 to 50 [maybe linear scaling for these data; look at the distribution]
#   3) then add mean events + SAR for a potential range of 0 to 100
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

# INSTRUCTIONS: set the transformation here
#     to support "CompQuantile_" and "CompLog_" output directories
trans <- "Quantile"
#trans <- "Log"
#trans <- "Sqrt"
#trans <- "Raw"
folder <- paste0("Comp", trans, "_ADM3")
outDir <- file.path(analysisDir, folder)
if (!dir.exists(outDir)) {
  print(paste("ERROR invalid outDir", outDir))
  print("You might need to manually create the directory...")
}


# load functions to read/join monthly event data & SAR data
source(file.path(analysisDir, "ReadMonthlyData.R"))

#adm3_wide <- join_wide(baseDir, SAR_DIR, "ADM3")
adm3_wide <- join_wide(baseDir, SAR_DIR, "ADM3", GHSnorm = TRUE)



##########################
# calculate the mean of ACLED & VIINA events & normalize by district area
##########################
mean_events_norm <- function(wide_df, borders_str) {
  # wide_df <- adm3_wide; borders_str <- "ADM3"
  
  # Get all unique months from column names
  months <- gsub("^(ACLED|VIINA)_", "", names(wide_df))
  
  # Identify unique months
  unique_months <- unique(months[months %in% months[duplicated(months)]])
  
  # Compute mean for each month
  mean_df <- wide_df %>%
    mutate(across(
      all_of(paste0("ACLED_", unique_months)), 
      ~ (. + wide_df[[sub("ACLED", "VIINA", cur_column())]]) / 2, 
      .names = "Events_{.col}"
    )) %>%
    rename_with(~ gsub("Events_ACLED_", "Events_", .), starts_with("Events_"))  
  
  # View column names to confirm
  colnames(mean_df)
  
  # Extract the monthly column names for each set
  #events_cols <- grep(paste0("^", "Events"), colnames(mean_df), value = TRUE)
  mean_df_norm <- mean_df %>%
    mutate(across(
      matches("^Events"), # Select columns starting with "Events"
      ~ (. / AREA_SQKM)  # normalize by ADM district area
    ))
  
#  tmp <- fix(mean_df_norm)
  return(mean_df_norm)
}

mean_df_norm <- mean_events_norm(adm3_wide, "ADM3")

######################################
# function to tranform data using either:
#   - quantile normalization
#   - log(x+1) transformations
######################################
transform_data <- function(mean_df_norm, method, prefix) {
  #method <- "Quantile", prefix <- "Events_"
  long_var <- paste0(prefix, "var")
  scaled_long_var <- paste0("scaled_", long_var)
  df_long <- mean_df_norm %>%
    pivot_longer(cols = starts_with(prefix), 
                 names_to = "Month", 
                 values_to = long_var)
  if (method == "Quantile") {
    # Compute ECDF-based scaling (percentile range 0-1)
    df_long <- df_long %>%
      mutate(!!sym(scaled_long_var) := ecdf(!!sym(long_var))(!!sym(long_var)))
  } else if (method == "Log") {
    df_long <- df_long %>%
      mutate(!!sym(scaled_long_var) := log(!!sym(long_var) + 1))
  } else if (method == "Sqrt") {
    df_long <- df_long %>%
      mutate(!!sym(scaled_long_var) := sqrt(!!sym(long_var)))
  } else if (method == "Raw") {
    df_long <- df_long %>%
      mutate(!!sym(scaled_long_var) := !!sym(long_var))
  } else {
    print(paste("ERROR: unexpected method:", method))
    return()
  }
  # Apply min-max normalization to ensure range is exactly [0, 50]
  df_long <- df_long %>%
    mutate(!!sym(scaled_long_var) := (!!sym(scaled_long_var) - min(!!sym(scaled_long_var))) / 
             (max(!!sym(scaled_long_var)) - min(!!sym(scaled_long_var))) * 50)
  
  pct_zeros <- sum(df_long[[scaled_long_var]] == 0)/nrow(df_long) * 100 # 56.8% zeros for SAR; 83.6% for Events
  print(paste("After scaling, pct zeros =", round(pct_zeros, 1)))
  print(paste("Range of", scaled_long_var))
  print(range(df_long[[scaled_long_var]]))

  # Create histogram of input data
  if (FALSE) {
    ggplot(df_long, aes(x = !!sym(scaled_long_var))) +
      geom_histogram(fill = "steelblue", color = "black") + # binwidth = 1, 
      labs(title = paste("Distribution of", prefix),
           x = long_var,
           y = "Frequency") +
      theme_minimal()
  }
    # Create histogram of normalized values
  ggplt <- ggplot(df_long, aes(x = !!sym(scaled_long_var))) +
    geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
    labs(title = paste(method, "distribution of", prefix),
         x = scaled_long_var,
         y = "Frequency") +
    theme_minimal()
  print(ggplt)
    
  # Convert back to wide format while keeping ADM3_PCODE
  print("WARNING: ADM3_PCODE hardcoded here")
  df_wide_scaled <- df_long %>%
    select(ADM3_PCODE, Month, !!sym(scaled_long_var)) %>%
    pivot_wider(names_from = Month, values_from = !!sym(scaled_long_var))
  #tail(df_wide_scaled)
  return(df_wide_scaled)
}

#df_wide_events_orig <- df_wide_events
df_wide_events <- transform_data(mean_df_norm, trans, "Events_")
df_wide_sar <- transform_data(mean_df_norm, trans, "SAR_")


####################
# add the two sets of monthly columns
####################

# Merge the two data frames by "ADM3_PCODE"
df_combined <- df_wide_sar %>%
  inner_join(df_wide_events, by = "ADM3_PCODE")

# Identify common months to sum the values correctly
common_months <- intersect(gsub("SAR_", "", names(df_wide_sar)[-1]), 
                           gsub("Events_", "", names(df_wide_events)[-1]))

colnames(df_combined)
comp_prefix <- paste0("Comp",trans, "_")
comp_prefix_SAR <- paste0(comp_prefix,"SAR_")
# Create "Comp_*" columns as the sum of "SAR_*" and "Events_*"
df_combined <- df_combined %>%
  mutate(across(all_of(paste0("SAR_", common_months)), 
                ~ . + df_combined[[sub("SAR", "Events", cur_column())]], 
                .names = paste0("Comp",trans,"_{.col}"))) %>%
  rename_with(~ sub(comp_prefix_SAR, comp_prefix, .), starts_with(comp_prefix_SAR))

# View column names to confirm
colnames(df_combined)
#tmp <- fix(df_combined)

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
# Write monthly pngs maps to a given directory
#   REQUIRES: analysisDir, adm1_sf, adm3_sf
##################################
monthly_comp_pngs <- function(adm_comp, trans, borders_str) {
  #adm_comp <- df_combined; borders_str <- "ADM3"

  ID_var <- paste0(borders_str, "_PCODE")
  print("WARNING: adm3_df hardcoded here")
  adm_evnts_small <- adm3_sf[, c(ID_var, "geometry")]
  adm_comp_sf <- left_join(adm_evnts_small, adm_comp, by = ID_var)
  #colnames(adm_comp_sf)
  
#  adm_comp_sp <- as(adm_comp_sf, "Spatial")
  comp_prefix <- paste0("Comp",trans, "_")
  comp_mnths <- grep(paste0("^", comp_prefix), colnames(adm_comp_sf), value = TRUE)

  if (borders_str == "ADM3")
    border_col <- "gray" #"#2b2b2b" 
  else if (borders_str == "ADM4")
    border_col <- NA # remove border color for ADM4
  else
    print(paste("ERROR: unexpected borders_str", borders_str))
  
  max_val <- 100
  print(paste("max composite value:", max_val)) 
  
  for (comp_mnth in comp_mnths) { # comp_mnth <- comp_mnths[1]
    mnth_yr_str <- gsub(comp_prefix, "", comp_mnth)  # Remove prefix
    date_obj <- as.Date(paste0("01_", mnth_yr_str), format = "%d_%m_%Y")
    mnth_label <- format(date_obj, "%b %Y")
    yr_mnth <- format(date_obj, "%Y_%m")
    #head(adm_comp_sf[[comp_mnth]])

    adm_comp_sf$comp_var <- adm_comp_sf[[comp_mnth]]

    gg_plt <- ggplot(adm_comp_sf) +
      geom_sf(aes(fill = comp_var), color=border_col) +
      #scale_fill_lajolla_c() + # https://r-charts.com/color-palettes/
      geom_sf(data = adm1_sf, fill = NA, color = "#2b2b2b", size = 0.8) +
      scale_fill_gradientn(
        colors = c("lightgray", "darkred"), # Light to strong colors
        values = scales::rescale(c(0, max_val)),
        limits = c(0, max_val),  # Force all maps to have the same limits
        na.value = "white" # Define a color for NA values
      ) +
      labs(title = paste0("Composite Damage Score (",trans,"): ", mnth_label), fill = "Value") +
      theme_minimal() +
      theme(
        legend.position = "right",     # Position the legend
        panel.background = element_rect(fill = "white", color = NA), # White background for the map
        plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
        panel.grid = element_blank(),  # Remove gridlines
        plot.title = element_text(size = 14, hjust = 0.5) # Adjust title position
      )

    # Save the plot to a PNG file w/ yr_mnth for correct sorting
    #print(gg_plt)
    fname <- paste0(comp_prefix, borders_str, "_", yr_mnth, ".png")
    print(paste("writing file", fname))
    folder <- paste0("Comp", trans, "_", borders_str)
    outDir <- file.path(analysisDir, folder)
    png_filename <- file.path(outDir, fname)
    ggsave(png_filename, plot = gg_plt, width = 8, height = 6)
  }
}  

#monthly_comp_pngs(df_combined, trans, "ADM3")

source(file.path(analysisDir, "MakeGifFunction.R"))
#make_gif(analysisDir, folder)


##################################
# Add up all the months & make a single composite damage map
#   REQUIRES: analysisDir, adm1_sf, adm3_sf
##################################
total_comp_map <- function(adm_comp, trans, borders_str) {
  #adm_comp <- df_combined; borders_str <- "ADM3"
  comp_prefix <- paste0("Comp",trans, "_")
  comp_mnths <- grep(paste0("^", comp_prefix), colnames(adm_comp), value = TRUE)
  
  sum_var <- paste0(comp_prefix, "Sum")
  adm_comp[[sum_var]] <- rowSums(adm_comp[, comp_mnths], na.rm = TRUE)

  ID_var <- paste0(borders_str, "_PCODE")
  print("WARNING: adm3_df hardcoded here")
  adm_evnts_small <- adm3_sf[, c(ID_var, "geometry")]
  adm_comp_sf <- left_join(adm_evnts_small, adm_comp, by = ID_var)

  print(paste("Range of ", sum_var))
  print(range(adm_comp_sf[[sum_var]]))
  # Apply min-max normalization to ensure range is exactly [0, 100]
  adm_comp_sf <- adm_comp_sf %>%
    mutate(!!sym(sum_var) := (!!sym(sum_var) - min(!!sym(sum_var))) / 
             (max(!!sym(sum_var)) - min(!!sym(sum_var))) * 100)
  print(paste("Range of ", sum_var, "after 0-100 normalization:"))
  print(range(adm_comp_sf[[sum_var]]))

  if (borders_str == "ADM3")
    border_col <- "gray" #"#2b2b2b" 
  else if (borders_str == "ADM4")
    border_col <- NA # remove border color for ADM4
  else
    print(paste("ERROR: unexpected borders_str", borders_str))

  max_val <- 100
  print(paste("max composite value:", max_val))
  
  # map adm_comp_sf[[sum_var]] & save to file
  gg_plt <- ggplot(adm_comp_sf) +
    geom_sf(aes(fill = !!sym(sum_var)), color=border_col) +
    #scale_fill_lajolla_c() + # https://r-charts.com/color-palettes/
    geom_sf(data = adm1_sf, fill = NA, color = "#2b2b2b", linewidth = 0.1) +
    geom_sf(data = adm0_sf, fill = NA, color = "#101010", linewidth = 0.4) +
    scale_fill_gradientn(
#      colors = c("lightgray", "darkred"), # Light to strong colors
#      colors = c("#e5e5e5", "#7d0112"), # "#3a0c24"
      colors = c("white", "#7d0112"),
      values = scales::rescale(c(0, max_val)),
      limits = c(0, max_val),  # Force all maps to have the same limits
      na.value = "white" # Define a color for NA values
    ) +
    labs(title = paste0("Composite Damage Score (",trans,"): All Months"), fill = "Value") +
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
  
  # Save the plot to a PNG file
  #print(gg_plt)
  fname <- paste0(comp_prefix, borders_str, "_AllMonths", ".png")
  print(paste("writing file", fname))
  folder <- paste0("Comp", trans, "_", borders_str)
  outDir <- file.path(analysisDir, folder)
  png_filename <- file.path(outDir, fname)
  ggsave(png_filename, plot = gg_plt, width = 8, height = 6)
}

total_comp_map(df_combined, trans, "ADM3")
