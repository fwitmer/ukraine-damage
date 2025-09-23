##########################################################################
# 17 Jan 2025
#
#	combine violent event point data and SAR damage data in wide format
#   to calculate monthly correlation coefficients
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


##########################
# calculate monthly correlations
##########################
monthly_corr <- function(wide_df) {
  # wide_df <- adm3_wide

  # Extract the monthly column names for each set
  acled_cols <- grep(paste0("^", "ACLED"), colnames(wide_df), value = TRUE)
  viina_cols <- grep(paste0("^", "VIINA"), colnames(wide_df), value = TRUE)
  sar_cols <- grep(paste0("^", "SAR"), colnames(wide_df), value = TRUE)

  # Extract month-year from column names (everything after the first underscore)
  months <- sub("^.*?_", "", sar_cols) # must use sar_cols here since they end 1 month earlier
  
  # Initialize a data frame for results
  results <- data.frame(
    Month = months,
    cor_acled_viina = numeric(length(months)),
    cor_acled_sar = numeric(length(months)),
    cor_viina_sar = numeric(length(months))
  )
  
  # loop through each month & add correlation coeff to the df
  #   a bit risky since it assumes colnames are all ordered by date
  for (i in seq_along(months)) {
    results$cor_acled_viina[i] <- cor(wide_df[[acled_cols[i]]],
                                      wide_df[[viina_cols[i]]],
                                      use="complete.obs")
    results$cor_acled_sar[i] <- cor(wide_df[[acled_cols[i]]],
                                    wide_df[[sar_cols[i]]],
                                    use="complete.obs")
    results$cor_viina_sar[i] <- cor(wide_df[[viina_cols[i]]],
                                    wide_df[[sar_cols[i]]],
                                    use="complete.obs")
  }
  #head(results)
  return(results)
}


##########################
# plot monthly correlations
##########################
plot_corr <- function(cor_df, borders_str) {
  # cor_df <- adm3_cor
  # Reshape data for plotting
  results_long <- cor_df %>%
    pivot_longer(cols = starts_with("cor_"), names_to = "Comparison", values_to = "Correlation")
  
  # Convert Month to a proper factor to ensure correct x-axis ordering
  results_long$Month <- factor(results_long$Month, levels = unique(results_long$Month))
  
  cor_plot <- ggplot(results_long, aes(x = Month, y = Correlation, color = Comparison, group = Comparison)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(
      title = paste0("Monthly Correlation Coefficients, ", borders_str),
      x = "Month",
      y = "Correlation Coefficient",
      color = "Comparison"
    ) +
    # https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
    scale_color_manual(
      values = c("cor_acled_viina" = "#6699CC", "cor_acled_sar" = "#117733", "cor_viina_sar" = "#882255"),
      labels = c(
        "cor_acled_viina" = "ACLED vs VIINA",
        "cor_acled_sar" = "ACLED vs SAR",
        "cor_viina_sar" = "VIINA vs SAR"
      )
    ) +
    scale_y_continuous(
      limits = c(0, 0.8)#, # Set min and max range for y-axis
      #breaks = seq(-1, 1, by = 0.2) # Optional: Define custom tick marks
    ) +
    theme_minimal() +  # Start with a minimal theme
    theme(
      panel.background = element_rect(fill = "white", color = "black"), # Rectangular border with black color
      panel.grid.major.x = element_blank(),
#      panel.grid.major = element_blank(), # Remove major gridlines
#      panel.grid.minor = element_blank(), # Remove minor gridlines
      axis.line = element_line(color = "black"), # Add black axes
      plot.background = element_rect(fill = "white", color = "black"), # Ensure overall white background
#      panel.background = element_rect(fill = "white", color = NA), # Set white background
#      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )

  # Save the plot to a PNG file
  outFile <- file.path(analysisDir, paste0("mnth_cor_", borders_str, ".png"))
  print(paste("Saving plot file to:", outFile))
  ggsave(
    filename = outFile,
    plot = cor_plot,
    width = 8,                          # Width in inches
    height = 6,                         # Height in inches
    dpi = 300                           # Resolution in dots per inch
  )
}

adm3_wide <- join_wide(baseDir, SAR_DIR, "ADM3")
adm4_wide <- join_wide(baseDir, SAR_DIR, "ADM4")
#sum(is.na(adm4_wide$SAR_03_2022))

adm3_cor <- monthly_corr(adm3_wide)
adm4_cor <- monthly_corr(adm4_wide)

plot_corr(adm3_cor, "ADM3")
plot_corr(adm4_cor, "ADM4")

##################################

####### load border ADM3 & ADM4 data #########
adm1_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm1_sspe_20240416.shp")
adm1_sf <- st_read(adm1_fname)

adm3_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm3_sspe_20240416.shp")
adm3_sf <- st_read(adm3_fname)

#adm4_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm4_sspe_20240416.shp")
#adm4_sf_bad_geo <- st_read(adm4_fname)
adm4_fixed <- file.path(baseDir, "Borders/ukr_admbnda_adm4_fixed.gpkg")
adm4_sf <- st_read(adm4_fixed)


library(GWmodel)
# explore 03_2023 GWR correlation map for ADM3, VIINA vs SAR

##################################
# write monthly pngs to a given directory
# 
# https://arxiv.org/pdf/1306.0413
# https://cran.r-project.org/web/packages/GWmodel/GWmodel.pdf
#
# Email response from Binbin Lu regarding distance units for great circle calculation:
#    "The unit of bandwidth is km when adaptive = FALSE and longlat = TRUE, and great circle distance will be calculate. When  longlat = FALSE, we will assume the CRS is projected instead of geographical  coordiantes. Thus, the unit of distance should not be  lat/long degrees."
##################################
monthly_corr_pngs <- function(adm_wide, borders_str) {
  #adm_wide <- adm3_wide
  #borders_str <- "ADM3"
  #colnames(adm_wide)
  
  ID_var <- paste0(borders_str, "_PCODE")
  print("WARNING: adm3_df hardcoded here")
  adm_evnts_small <- adm3_sf[, c(ID_var, "geometry")]
  adm_wide_sf <- left_join(adm_evnts_small, adm_wide, by = ID_var)
  colnames(adm_wide_sf)
  
  adm_wide_sp <- as(adm_wide_sf, "Spatial")
  #is(adm_wide_sp)
  
  sar_cols <- grep(paste0("^", "SAR"), colnames(adm_wide_sf), value = TRUE)
  
  # Extract month-year from column names (everything after the first underscore)
  months <- sub("^.*?_", "", sar_cols) # must use sar_cols here since they end 1 
  bandwidth <- ceiling(nrow(adm3_wide) * 0.3) # set bandwidth to ~30% of the data
  print(paste('     Bandwidth =', bandwidth))

  # do this 3 times for each pairing
  pair_list <- list(c("ACLED", "VIINA"), c("ACLED", "SAR"), c("VIINA", "SAR"))
  for (pair in pair_list) {
    #print(pair) }
    # pair <- pair_list[1]
    # calculate & map GWR for each month
    for (month in months) { # month <- months[14]
      #pair[1]; pair[2]
      mnth_obj <- as.Date(paste("1", month, sep="_"), format="%d_%m_%Y")
      mnth_label <- format(mnth_obj, "%b %Y")
      yr_mnth <- format(mnth_obj, "%Y_%m")
      
      var1 <- paste0(pair[1], "_", month)
      var2 <- paste0(pair[2], "_", month)

      gw_stats <- gwss(
        data = adm_wide_sp,
        vars = c(var1, var2),
        bw = bandwidth, # w/ adaptive = TRUE, bandwidth = # of nearest neighbors
        adaptive = TRUE
      )
      #names(gw_stats$SDF)
      
      if (borders_str == "ADM3")
        border_col <- "gray" #"#2b2b2b" 
      else if (borders_str == "ADM4")
        border_col <- NA # remove border color for ADM4
      else
        print(paste("ERROR: unexpected borders_str", borders_str))
      
      # View results
      gwr_results_sf <- st_as_sf(gwr_results$SDF)
      #head(gw_stats$SDF$Corr_VIINA_09_2023.SAR_09_2023)
      cor_var <- paste0("Corr_", var1, ".", var2)
      gwr_results_sf$local_cor <- gw_stats$SDF[[cor_var]]
      
      gwr_plt <- ggplot(gwr_results_sf) +
        geom_sf(aes(fill = local_cor), , color = border_col) +
        geom_sf(data = adm1_sf, fill = NA, color = "#2b2b2b", size = 0.8) +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red", midpoint = 0,
          limits = c(-1, 1),
          name = "Local Correlation"
        ) +
  #    scale_fill_viridis_c(option = "plasma", name = "Local Correlation") + 
        labs(
          title = paste0("GW Correlation Coefficients for ", borders_str, " borders, ", pair[1], " vs ", pair[2], ", ", mnth_label)
      #    subtitle = "Geographically Weighted Regression Results"
        ) +
        theme_minimal() +
        theme(
          legend.position = "right",     # Position the legend
          panel.background = element_rect(fill = "white", color = NA), # White background for the map
          plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
          panel.grid = element_blank(),  # Remove gridlines
          plot.title = element_text(size = 14, hjust = 0.5) # Adjust title position
        )
      #print(gwr_plt)
      # write plot to png file
      baseOutDir <- file.path(analysisDir, paste0(borders_str, "_MnthCorMaps"))
      if (!dir.exists(baseOutDir)) {
        print(paste("creating directory:", baseOutDir))
        dir.create(baseOutDir)
      }
      outDir <- file.path(baseOutDir, paste0(pair[1], "_", pair[2]))
      if (!dir.exists(outDir)) {
        print(paste("creating directory:", outDir))
        dir.create(outDir)
      }
      fname <- paste0(borders_str, "_", pair[1], "_", pair[2], "_", yr_mnth, ".png")
      print(paste("writing file", fname))
      png_filename <- file.path(outDir, fname)
      ggsave(png_filename, plot = gwr_plt, width = 8, height = 6)
    }
  }
}

monthly_corr_pngs(adm3_wide, "ADM3")
#monthly_corr_pngs(adm4_wide, "ADM4") # maybe not worth running this?


library(magick) # for image_animate function
###################################
# make_gifs function requires global 'analysisDir'
###################################
make_gifs <- function(borders_str) {
  baseInDir <- file.path(analysisDir, paste0(borders_str, "_MnthCorMaps"))
  if (!dir.exists(baseInDir))
    print(paste("ERROR, directory not found:", baseInDir))

  pair_list <- list(c("ACLED", "VIINA"), c("ACLED", "SAR"), c("VIINA", "SAR"))
  for (pair in pair_list) {
    print(pair)
    # pair <- pair_list[3]
    pair_str <- paste0(pair[1], "_", pair[2])
    inDir <- file.path(baseInDir, pair_str)
    if (!dir.exists(inDir))
      print(paste("ERROR, directory not found:", inDir))
  
    # List the PNG files in the output directory
    png_files <- list.files(inDir, pattern = "*.png", full.names = TRUE)
    last_file <- png_files[length(png_files)]
    # add pause by repeating last file
    png_files <- c(png_files, rep(last_file, 10))
    
    # Read the images and create the GIF
    gif <- image_read(png_files) %>%
      image_animate(fps = 2)  # Adjust frames per second as needed
    
    # Save the animated GIF
    outGifFile <- file.path(inDir, paste0(borders_str, "_", pair_str, ".gif"))
    print(paste("Writing file to:", outGifFile))
    image_write(gif, outGifFile)
  }
}

make_gifs("ADM3")




##########################
# CUT BELOW HERE
##########################

outcome <- paste0(pair[1], "_", month)
predictor <- paste0(pair[2], "_", month)
#      gwr_formula <- as.formula(paste(outcome, "~", predictor))
#    gwr_formula <- as.formula(paste("SAR_03_2023", "~", "VIINA_03_2023"))
#    gwr_formula <- as.formula(paste("VIINA_03_2023", "~", "SAR_03_2023"))
if (FALSE) { # list of adaptive bandwidths from VIINA & SAR monthly data
  # of nearest neighbors (1769 total ADM3 districts)
  bandwidths <- c(1050, 1766, 1293, 746, 130, 611, 677, 1058, 1766, 1759, 1643, 678, 1099, 926, 289, 549, 654, 579, 541, 575)
  bandwidth <- mean(bandwidths)
  # bw goes from 729 to 1550 if longlat=TRUE is used, # 1754 for gaussian
  # for VIINA vs SAR, 
}

ADAPTIVE_BW <- FALSE
if (ADAPTIVE_BW) {
  bandwidth <- bw.gwr(
    formula = gwr_formula,
    data = adm_wide_sp,
    kernel = "bisquare",
    #      kernel = "gaussian", # spatial variation is lost
    adaptive = FALSE,  # fixed
    #      adaptive = TRUE,  # Adaptive bandwidth for varying densities
    longlat = FALSE
  )
}

gwr_results <- gwr.basic(
  formula = gwr_formula,
  data = adm_wide_sp,
  bw = bandwidth, # w/ adaptive = TRUE, bandwidth = # of nearest neighbors
  adaptive = TRUE
)

var_name <- all.vars(gwr_formula)[2]
#names(gwr_results$SDF)
local_R2 <- gwr_results$SDF$Local_R2
reg_coeffs <- gwr_results$SDF[[predictor]]
gwr_results_sf$local_cor <- sqrt(local_R2) * sign(reg_coeffs)
head(gwr_results_sf$local_cor)
plot(gwr_results_sf$local_cor, gw_stats$SDF$Corr_VIINA_09_2023.SAR_09_2023)


if (FALSE) {
  library(tmap)
  tm_shape(gwr_results_sf) +
    tm_polygons("VIINA_03_2023", palette = "plasma", title = "Coefficient") +
    tm_layout(
      title = "GWR Coefficients for Damage Variable",
      legend.outside = TRUE
    )
}
