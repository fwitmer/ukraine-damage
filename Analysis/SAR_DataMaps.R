##########################################################################
# 13 Dec 2024
#
#	map SAR damage data aggregated to ADM3 & ADM4 spatial units
#
# border data source: Humanitarian Data Exchange, OCHA
#   https://data.humdata.org/dataset/cod-ab-ukr
#
##########################################################################

#baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
baseDir <- getwd()
analysisDir <- file.path(baseDir, "Analysis/")
if (!dir.exists(analysisDir))
  print(paste("ERROR invalid analysisDir", analysisDir))

source(file.path(baseDir, "LoadInstallLib.R"))
load_install_lib("sf")
load_install_lib("dplyr")
load_install_lib("ggplot2")
load_install_lib("tidyr") # for pivot_longer
load_install_lib("stringr") # for str_replace_all

# load functions to read/join monthly event data & SAR data & join_GHS function
source(file.path(analysisDir, "ReadMonthlyData.R"))


####### load border data #########
adm0_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm0_sspe_20240416.shp")
adm0_sf <- st_read(adm0_fname)

adm1_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm1_sspe_20240416.shp")
adm1_sf <- st_read(adm1_fname)

#adm3_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm3_sspe_20240416.shp")
#adm3_sf <- st_read(adm3_fname)

#adm4_fixed <- file.path(baseDir, "Borders/ukr_admbnda_adm4_fixed.gpkg")
#adm4_sf <- st_read(adm4_fixed)
#nrow(adm4_sf) # 29707

########################
# join SAR aggregated SAR data to the shapefile
########################
join_SAR_to_SF <- function(borders_str, GHS_norm = FALSE) {
  # borders_str <- "ADM3"; GHS_norm <- TRUE
  if (borders_str == "ADM3") {
    adm3_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm3_sspe_20240416.shp")
    adm_sf <- st_read(adm3_fname)
  } else if (borders_str == "ADM4") {
    adm4_fixed <- file.path(baseDir, "Borders/ukr_admbnda_adm4_fixed.gpkg")
    adm_sf <- st_read(adm4_fixed)
    #nrow(adm_sf) # 29707
  } else {
    print(paste("ERROR: unexpected borders_str:", borders_str))
  }
  
  SAR_Dir <- file.path(baseDir, "/SAR_GEE/Monthly_Damage/")
  if (!dir.exists(SAR_Dir))
    print(paste("ERROR invalid SAR_Dir", SAR_Dir))
  
  if (!GHS_norm) {
    print("ERROR: old SAR normalization no longer supported")
    return(FALSE)
  }
  adm_mnth_norm <- read_sar_GHSnorm(SAR_Dir, borders_str) # from ReadMonthlyData.R
  
  ID_var <- paste0(borders_str, "_PCODE")
  # join adm_mnth_norm to adm_sf spatial data
  adm_mnth_join <- adm_sf %>%
    left_join(adm_mnth_norm, by = ID_var)
  #colnames(adm_mnth_join)
  
  return(adm_mnth_join)
}

adm3_sar_sf <- join_SAR_to_SF("ADM3", GHS_norm = TRUE)
#adm4_sar_sf <- join_SAR_to_SF("ADM4")



#############################
# write total SAR area map to disk
#     optionally apply log transformation
#   requires global 'analysisDir'
#   requires global 'adm1_sf'
#############################
total_sar <- function(mnth_sar_sf, borders_str, sar_str, trans) {
#mnth_sar_sf <- adm3_sar_norm; borders_str <- "ADM3"; sar_str <- "SAR"; trans<-"raw"
  # extract month labels for all of 2022 and Jan-Sep 2023
  #mnth_vars <- grep("^X([0-9]{2}_2022|0[0-9]_2023)$", colnames(mnth_sar_sf), value = TRUE)
  # don't hard code the dates, just map everything in the sf object
  mnth_vars <- grep("^SAR\\_\\d{2}\\_\\d{4}$", colnames(mnth_sar_sf), value = TRUE)
  
  if (length(mnth_vars) != 20) {
    print(paste('ERROR: total months should = 20: ', length(mnth_vars)))
    return()
  }
  
  mnth_sar_sf$TotSARarea <- rowSums(st_drop_geometry(mnth_sar_sf)[, mnth_vars], na.rm = TRUE)
  print('Range of TotSARarea')
  print(range(mnth_sar_sf$TotSARarea)) # 0 to 23.37
  #hist(mnth_sar_sf$TotSARarea) # most are below 2
  if (trans == "log") { # 8 Jan 2026, this option is now broken
    leg_title <- "log(Area (%))"
    #hist(log(mnth_sar_sf$TotSARarea + 0.2))
    #hist(log(mnth_sar_sf$TotSARarea + 1))
    #log_offset <- 0.1
    #log_offset <- 0.2#; max_val <- 4.8 # max_val hardcoded based on range below
    #log_offset <- 0.5
    log_offset <- 1
    mnth_sar_sf$TotSARarea_val <- log(mnth_sar_sf$TotSARarea + log_offset) - log(log_offset)
    print('Range of log(TotSARarea)')
  } else if (trans == "raw") {
    leg_title <- "Area (%)"
    mnth_sar_sf$TotSARarea_val <- mnth_sar_sf$TotSARarea

    # look at the histogram & count number of districts == 0
#    browser()
#    hist(mnth_sar_sf$TotSARarea_val)
    sum(mnth_sar_sf$TotSARarea_val == 0) # 282
    sum(mnth_sar_sf$TotSARarea_val < .Machine$double.eps) # 282
    sum(mnth_sar_sf$TotSARarea_val < 1) # 1134
    sum(mnth_sar_sf$TotSARarea_val < 2) # 1463
    sum(mnth_sar_sf$TotSARarea_val < 4) # 1632
    sum(mnth_sar_sf$TotSARarea_val < 5) # 1670
    sum(mnth_sar_sf$TotSARarea_val > 9) # 54
    sum(mnth_sar_sf$TotSARarea_val > 16) # 27
    # use same PointDataMaps.R breaks:
    breaks <- c(0, 1, 4, 9, 16, Inf)

    # strictly positive bins
    mnth_sar_sf$AreaCat <- cut(
      mnth_sar_sf$TotSARarea_val,
      breaks = breaks,
      include.lowest = TRUE,
      right = FALSE
    ) %>% as.character()  # convert to character so we can relabel
#    browser()

    # exact zero
    mnth_sar_sf$AreaCat[abs(mnth_sar_sf$TotSARarea_val) < .Machine$double.eps] <- "0"
    map_labels <- c(
      "0"        = "0",
      "[0,1)"    = "(0–1)",
      "[1,4)"    = "[1–4)",
      "[4,9)"    = "[4–9)",
      "[9,16)"   = "[9–16)",
      "[16,Inf]" = "≥16"
    )
    mnth_sar_sf$AreaCat <- map_labels[mnth_sar_sf$AreaCat]
    labels <- c("0", "(0–1)", "[1–4)", "[4–9)", "[9–16)", "≥16")

    # convert to factor with your custom labels in the correct order
    mnth_sar_sf$AreaCat <- factor(
      mnth_sar_sf$AreaCat,
      levels = labels,
      ordered = TRUE
    )

    leg_label <- expression("Area (%)")
    #file_mod  <- "_cat"
    trans  <- "_cat"

  } else {
    print(paste("ERROR: unexpected transformation", trans))
    return(0)
  }
  print('Range of TotSARarea')
  print(range(mnth_sar_sf$TotSARarea_val)) # 0 to 4.76; 41.4 raw
  max_val <- max(mnth_sar_sf$TotSARarea_val)
  print(paste('Max map value:', max_val))
  print("category counts:")
  print(table(mnth_sar_sf$AreaCat))
#     0  (0–1)  [1–4)  [4–9) [9–16)    ≥16 
#   282    852    498     83     27     27 SAR area counts

  if (borders_str == "ADM3")
    border_col <- "gray" #"#2b2b2b" 
  else if (borders_str == "ADM4")
    border_col <- NA # remove border color for ADM4
  else
    print(paste("ERROR: unexpected borders_str", borders_str))
  
  # set map colors to match monthly count plots from PlotEventData.R
  #  scale_color_manual(values = c("ACLED" = "#377eb8", "VIINA" = "#984ea3"))
  # https://www.w3schools.com/colors/colors_picker.asp
  #map_color <- "#662200" # 20% darkness, 45% = #e64d00

#  RColorBrewer::display.brewer.all(type="seq")
  brewer_pal <- RColorBrewer::brewer.pal(5, "YlOrBr")
  names(brewer_pal) <- c("(0–1)", "[1–4)", "[4–9)", "[9–16)", "≥16")
  brewer_pal <- c("0" = "white", brewer_pal)  # exact zero is white

  map_plt <- ggplot(mnth_sar_sf) +
#    geom_sf(aes(fill = TotSARarea_val), color=border_col) +
    geom_sf(aes(fill = AreaCat), color=border_col) +
    #scale_fill_lajolla_c() + # https://r-charts.com/color-palettes/
    geom_sf(data = adm1_sf, fill = NA, color = "#2b2b2b", linewidth = 0.1) +
    geom_sf(data = adm0_sf, fill = NA, color = "#101010", linewidth = 0.4) +
    scale_fill_manual(
      values = brewer_pal,
      na.value = "white",
      drop = FALSE
    ) +
#    scale_fill_gradientn(
      #      colors = c("lightgray", map_color), # Light to strong colors
      #colors = c("#f2f2f2", map_color), # Light to strong colors #e6e6e6
#      colors = c("white", map_color), # Light to strong colors #e6e6e6
#      values = scales::rescale(c(0, max_val)),
#      limits = c(0, max_val),  # Force all maps to have the same limits
#      na.value = "white" # Define a color for NA values
#    ) +
#    labs(title = paste(sar_str, "Total Damage Detected"), fill = leg_title) +
    labs(title = paste(sar_str, "Total Damage Detected"), fill = leg_label) +
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
#      legend.text = element_text(size = 9),
#      legend.title = element_text(size = 10),
#      legend.key.size = unit(0.95, "lines"),
      axis.title = element_blank(),
      axis.text = element_blank(), # remove the lat/lon text labels
      axis.ticks = element_blank()
    )
  #print(map_plt)
  
  outDir <- file.path(analysisDir, "Total_SAR_Maps")
  if (!dir.exists(outDir)) {
    print(paste("creating directory:", outDir))
    dir.create(outDir)
  }
  png_filename <- paste0(borders_str, "_",sar_str,"_TotSARMap", trans, ".png")
  full_file <- file.path(outDir, png_filename)
  print(full_file)
  ggsave(full_file, plot = map_plt, width = 8, height = 6)
  
}

total_sar(adm3_sar_sf, "ADM3", "SAR", "raw")
#total_sar(adm3_sar_sf, "ADM3", "SAR", "log") # not helpful


####################

#############################
# write SAR maps to disk for each month
#   requires global 'analysisDir'
#   requires global 'adm1_sf'
#############################
monthly_pngs <- function(mnth_sar_sf, borders_str, sar_str) {
  folder <- paste(borders_str, sar_str, sep="_")
  outDir <- file.path(analysisDir , folder)
  if (!dir.exists(outDir)) {
    print(paste("creating directory:", outDir))
    dir.create(outDir)
  }
  #browser()
  
  sf_long <- pivot_longer(mnth_sar_sf, 
                         cols = matches("^SAR"), # select any column that starts with SAR
#                           starts_with("X02_2022"):starts_with("X10_2023"),
                         names_to = "Month", values_to = "Value")

  #colnames(sf_long)
  table(sf_long$Month)
  print(paste("total damage area:", sum(sf_long$Value))) # 78913
  max_val <- max(sf_long$Value)
  print(paste("max area:", max_val)) 
  
  # Create a plot for each month and save as a PNG file
  months <- unique(sf_long$Month)  # Get list of unique months
  
  for (month in months) { # month <- months[1]
    # Filter data for the current month
    shp_month <- subset(sf_long, Month == month)
#    mnth_obj <- as.Date(paste("1", month, sep="_"), format="%d_X%m_%Y")
    mnth_obj <- as.Date(paste("1", month, sep="_"), format="%d_SAR_%m_%Y")
    mnth_label <- format(mnth_obj, "%b %Y")
    yr_mnth <- format(mnth_obj, "%Y_%m")
    print(paste("total:", sum(shp_month$Value),"for", mnth_label))
    
    if (borders_str == "ADM3")
      border_col <- "gray" #"#2b2b2b" 
    else if (borders_str == "ADM4")
      border_col <- NA # remove border color for ADM4
    else
      print(paste("ERROR: unexpected borders_str", borders_str))
    
#    browser()
    p <- ggplot(shp_month) +
      geom_sf(aes(fill = Value), color=border_col) +
      #scale_fill_lajolla_c() + # https://r-charts.com/color-palettes/
      geom_sf(data = adm1_sf, fill = NA, color = "#2b2b2b", linewidth = 0.1) +
      geom_sf(data = adm0_sf, fill = NA, color = "#101010", linewidth = 0.4) +
      scale_fill_gradientn(
        colors = c("#f2f2f2", "darkred"), # Light to strong colors
        values = scales::rescale(c(0, max_val)),
        limits = c(0, max_val),  # Force all maps to have the same limits
        na.value = "white" # Define a color for NA values
      ) +
      labs(title = paste(sar_str, "Damage Percentage:", mnth_label), fill = "Area (%)") +
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
    
    # Save the plot to a PNG file w/ yr_mnth for correct sorting
    png_filename <- paste0(outDir, "/", borders_str, "_",sar_str,"_", yr_mnth, ".png")
    ggsave(png_filename, plot = p, width = 8, height = 6)
  }
}

#monthly_pngs(adm3_sar, "ADM3", "SAR")
monthly_pngs(adm3_sar_sf, "ADM3", "SAR")
#monthly_pngs(adm4_sar_sf, "ADM4", "SAR")


source(file.path(analysisDir, "MakeGifFunction.R"))

make_gif(analysisDir , paste("ADM3", "SAR", sep="_"))
#make_gif(analysisDir , paste("ADM4", "SAR", sep="_"))


####################################
# old code...
####################################
############################
# process file from Jamon
#   WARNING: these shapefiles are incomplete & only have 1616 polygons
############################
normADM_SAR_Jamon <- function(borders_str) { # borders_str <- "ADM4"
  adm_str_low <- tolower(borders_str)
  
  adm_mnth_fname <- file.path(baseDir, paste0("SAR_GEE/ADM3_4_Overlay/damage_area_ukr_damage_all_months_asc_dsc_v102_mmu_11GHSL_ukr_",adm_str_low,"_monthly_pivot.shp"))
  adm_mnth <- st_read(adm_mnth_fname)
  
  # check shapefile geometry
  if (sum(!st_is_valid(adm_mnth)) > 0) {
    print("WARNING: geometry problems!")
  }
  #colnames(adm_mnth)
  
  adm_perc_fname <- file.path(baseDir, paste0("SAR_GEE/ADM3_4_Overlay/ukr_",adm_str_low,"_perc-multipoly.shp"))
  adm_perc <- st_read(adm_perc_fname)
  #colnames(adm_perc)
  
  mnth_id_var <- paste0("ukr_", adm_str_low)
  perc_id_var <- paste0(borders_str, "_PCODE")
  head(adm_mnth[[mnth_id_var]])
  head(adm_perc[[perc_id_var]])
  if (identical(adm_mnth[[mnth_id_var]], adm_perc[[perc_id_var]])) {
    # assumes rows are ordered correctly
    adm_mnth$m_pre_ghsl <- adm_perc$m_pre_ghsl
  } else {
    # do a left join after removing geometry
    adm_mnth_nogeo <- st_drop_geometry(adm_mnth)
    adm_mnth_pre <- adm_mnth_nogeo %>%
      left_join(adm_perc, by = setNames(perc_id_var, mnth_id_var))
    adm_mnth_pre <- st_as_sf(adm_mnth_pre, geometry = st_geometry(adm_mnth))
    adm_mnth <- adm_mnth_pre
  }
  
  # normalize the month columns
  adm_mnth_norm <- adm_mnth %>%
    mutate(across(
      matches("^X"), # Select columns starting with "X"
      ~ (. / m_pre_ghsl) * 100 # normalize by `m_pre_ghsl` & convert to percentage
    ))
  
  #  sum(adm_mnth_norm$m_pre_ghsl <= 0) # 13
  #  sum(is.na(adm_mnth_norm$X3_2022)) # 13
  if (sum(is.na(adm_mnth_norm$m_pre_ghsl)) > 0) {
    print("WARNING: removing NA polygons") # YIKES, this filters 21,816 polygons!
    adm_mnth_norm <- adm_mnth_norm %>%
      filter(!is.na(m_pre_ghsl))
  }
  if (sum(adm_mnth_norm$m_pre_ghsl <= 0) > 0) {
    print("WARNING: removing polygons with no potential SAR damage area")
    # remove those 13 districts that have no potential SAR damage area
    adm_mnth_norm <- adm_mnth_norm %>%
      filter(m_pre_ghsl > 0)
  }
  sum(adm_mnth_norm$m_pre_ghsl <= 0) # 0
  sum(is.na(adm_mnth_norm$X3_2022)) # 0
  return(adm_mnth_norm)
}

#adm3_sar_norm <- normADM_SAR_Jamon("ADM3")
  #adm4_sar_norm <- normADM_SAR_Jamon("ADM4")

