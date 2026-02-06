##########################################################################
# 10 Dec 2024
#
#	map violent event point data with ADM3 & ADM4 data
#
# border data source: Humanitarian Data Exchange, OCHA
#   https://data.humdata.org/dataset/cod-ab-ukr
#
##########################################################################

#outDir <- "C:/Users/witmer/Dropbox/UAA/Research/Conflict_RS/AnnalsPlaceAnnihilation/Data/VIINA/"
#baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
baseDir <- getwd()
analysisDir <- file.path(baseDir, "Analysis/")
if (!dir.exists(analysisDir))
  print(paste("ERROR invalid analysisDir", analysisDir))

source(file.path(baseDir, "LoadInstallLib.R"))
load_install_lib("sf")
load_install_lib("dplyr")
load_install_lib("ggplot2")
load_install_lib("lubridate") # for months(1) addition in monthly_cnts function

#end_date <- as.Date("2022-05-30")
#end_date <- as.Date("2023-10-30")
#start_date <- as.Date("2022-02-01"); end_date <- as.Date("2023-09-30")
start_date <- as.Date("2022-03-01"); end_date <- as.Date("2023-10-31")

####### load VIINA data #########
viinaDir <- file.path(baseDir, "VIINA_Data")
source(file.path(viinaDir, "LoadVIINA.R"))
viina <- load_VIINA(viinaDir, start_date=start_date, end_date=end_date)

viina_sf <- st_as_sf(viina, coords = c("longitude", "latitude"), crs=4326)
head(viina_sf)


####### load ACLED data #########
acledDir <- file.path(baseDir, "ACLED_Data")
source(file.path(acledDir, "LoadACLED.R"))
acled <- load_ACLED(acledDir, start_date=start_date, end_date=end_date)

acled_sf <- st_as_sf(acled, coords = c("longitude", "latitude"), crs=4326)
head(acled_sf)
nrow(acled_sf) # 62231 --> 59587 --> 61331

####### load border ADM3 & ADM4 data #########
adm0_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm0_sspe_20240416.shp")
adm0_sf <- st_read(adm0_fname)

adm1_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm1_sspe_20240416.shp")
adm1_sf <- st_read(adm1_fname)

adm3_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm3_sspe_20240416.shp")
adm3_sf <- st_read(adm3_fname)
nrow(adm3_sf)  # 1769

#adm4_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm4_sspe_20240416.shp")
#adm4_sf_bad_geo <- st_read(adm4_fname)
adm4_fixed <- file.path(baseDir, "Borders/ukr_admbnda_adm4_fixed.gpkg")
adm4_sf <- st_read(adm4_fixed)
nrow(adm4_sf) # 29707

st_crs(viina_sf)
st_crs(acled_sf)
st_crs(adm3_sf)
st_crs(adm4_sf)

# check shapefile geometry
sum(!st_is_valid(adm3_sf)) # 0
sum(!st_is_valid(adm4_sf)) # 0


#########################
# aggregate points using monthly formatting of m??_????
########################
monthly_cnts <- function(points_sf, polygons_sf, start_date, end_date) {
  date_seq <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  mnth_names <- format(date_seq, "%m_%Y")
  start_time <- Sys.time()
  
  for (mnth in as.list(mnth_names)) {
    mnth_start <- as.Date(paste("1", mnth, sep="_"), format="%d_%m_%Y")
    mnth_end <- mnth_start + months(1)
    # Filter points for the current month
    points_month <- points_sf %>%
      filter(date_obj >= mnth_start & date_obj < mnth_end)
    var_name <- format(mnth_start, "m%m_%Y")
    ## add point count to each polygon; st_intersection is much slower
    polygons_sf[[var_name]] <- lengths(st_intersects(polygons_sf, points_month))
    print(paste("aggregated", sum(polygons_sf[[var_name]]), "points for", var_name))
    end_time <- Sys.time()
    cat("\tTime taken:", round(difftime(end_time, start_time, units = "secs"), 1), "seconds\n")
  }
  return(polygons_sf)
}

#######################
# write sf object to shapefile
#   keep this in a separate function in case it fails
# requires global 'analysisDir'
#######################
write_shp <- function(mnth_cnts_sf, borders_str, events_str) {
  folder <- paste(borders_str, events_str, sep="_")
  outDir <- file.path(analysisDir , folder)
  if (!dir.exists(outDir)) {
    print(paste("creating directory:", outDir))
    dir.create(outDir)
  }
  outFile <- file.path(outDir, paste0("ukr_",tolower(folder),".shp"))
  st_write(mnth_cnts_sf, outFile) # Grrr: truncates field names to length 8
  # TODO: could fix this problem by writing .csv file and then joining it back to the shapefile
}


## VIINA aggregations
# 59-77 seconds
adm3_viina <- monthly_cnts(viina_sf, adm3_sf, start_date, end_date)
head(adm3_viina)
write_shp(adm3_viina, "ADM3", "VIINA")
# 185 secs
#adm4_viina <- monthly_cnts(viina_sf, adm4_sf, start_date, end_date)
#head(adm4_viina)
#write_shp(adm4_viina, "ADM4", "VIINA")

## ACLED aggregations
adm3_acled <- monthly_cnts(acled_sf, adm3_sf, start_date, end_date) # ~66 seconds
head(adm3_acled)
write_shp(adm3_acled, "ADM3", "ACLED")

#adm4_acled <- monthly_cnts(acled_sf, adm4_sf, start_date, end_date)
#head(adm4_acled)
#write_shp(adm4_acled, "ADM4", "ACLED")


####################

#load_install_lib("gganimate")
#load_install_lib("transformr")
#load_install_lib("gifski")
load_install_lib("tidyr")

########################################################
# can re-start here, but some of the fields might be wonky from the shapefiles
########################################################
if (FALSE) { 
  inFile <- file.path(analysisDir, "ADM3_VIINA/ukr_adm3_viina.shp")
  adm3_viina <- st_read(inFile)
  colnames(adm3_viina)
  print(sum(adm3_viina$m03_2022)) # 4792
  print(sum(adm3_viina$m04_2023)) # 1958
  inFile <- file.path(analysisDir, "ADM3_ACLED/ukr_adm3_acled.shp")
  adm3_acled <- st_read(inFile)
  colnames(adm3_acled)
}

#############################
# add $CntsCat to the given sf object
#############################
set_count_categories(mnth_cnts_sf) {
  # TODO: write this to return modified mnth_cnts_sf object plus map_labels as well?
}

#############################
# write total events/km2 map to disk
#   requires global 'analysisDir'
#   requires global 'adm1_sf'
#############################
total_events_km2 <- function(mnth_cnts_sf, borders_str, events_str, log_offset = FALSE) {
  # mnth_cnts_sf <- adm3_viina; events_str <- "VIINA"
  # extract month labels for all of 2022 and Jan-Sep 2023
  #mnth_vars <- grep("^m([0-9]{2}_2022|0[0-9]_2023)$", colnames(mnth_cnts_sf), value = TRUE)
  # don't hard code the dates, just map everything in the sf object
  mnth_vars <- grep("^m\\d{2}\\_\\d{4}$", colnames(mnth_cnts_sf), value = TRUE)

  if (length(mnth_vars) != 20) {
    print(paste('ERROR: total months should = 20: ', length(mnth_vars)))
    return()
  } else {
    print('mapping sum of all dates:')
    print(mnth_vars)
  }
  total_var <- "TotalCnts"
  mnth_cnts_sf[[total_var]] <- rowSums(st_drop_geometry(mnth_cnts_sf)[, mnth_vars], na.rm = TRUE)
  print(paste("Total", events_str, "events: ", sum(mnth_cnts_sf$TotalCnts)))
  mnth_cnts_sf$CntsKm2 <- mnth_cnts_sf$TotalCnts / mnth_cnts_sf$AREA_SQKM
  range(mnth_cnts_sf$TotalCnts) # for VIINA, 0 to 3200
  print('Range of CntsKm2')
  print(range(mnth_cnts_sf$CntsKm2)) # for VIINA, 0 to 25; 0-19 for ACLED
  if (!log_offset) {
    leg_label <- expression("Cnts/Km"^2*")")
    file_mod <- "_raw"
    mnth_cnts_sf$CntsVar <- mnth_cnts_sf$CntsKm2
  } else if (log_offset <= 1) {
    leg_label <- expression("log(Cnts/Km"^2*")")
    file_mod <- "_log"
    #hist(mnth_cnts_sf$CntsKm2) # most are below 2
    #hist(log(mnth_cnts_sf$CntsKm2 + 0.2))
  #  log_offset <- 0.2; max_val <- 4.9
  #  log_offset <- 0.5; max_val <- 4.0 # max_val hardcoded based on range below
    mnth_cnts_sf$CntsVar <- log(mnth_cnts_sf$CntsKm2 + log_offset) - log(log_offset)
  } else { # sqrt breaks 
    #leg_label <- expression("sqrt(Cnts/Km"^2*")")
    #file_mod <- "_sqrt"
    #mnth_cnts_sf$CntsVar <- sqrt(mnth_cnts_sf$CntsKm2)
    
    set_count_categories(mnth_cnts_sf)

    # ---- sqrt categorical bins ----
    breaks <- c(0, 1, 4, 9, 16, Inf)

    # strictly positive bins
    mnth_cnts_sf$CntsCat <- cut(
      mnth_cnts_sf$CntsKm2,
      breaks = breaks,
      include.lowest = TRUE,
      right = FALSE
    ) %>% as.character()  # convert to character so we can relabel
#    browser()

    # exact zero
    mnth_cnts_sf$CntsCat[abs(mnth_cnts_sf$CntsKm2) < .Machine$double.eps] <- "0"
    map_labels <- c(
      "0"        = "0",
      "[0,1)"    = "(0–1)",
      "[1,4)"    = "[1–4)",
      "[4,9)"    = "[4–9)",
      "[9,16)"   = "[9–16)",
      "[16,Inf]" = "≥16"
    )
    mnth_cnts_sf$CntsCat <- map_labels[mnth_cnts_sf$CntsCat]
    #labels <- c("0", "(0–1)", "[1–4)", "[4–9)", "[9–16)", "≥16")
    labels <- unname(map_labels)

    # convert to factor with your custom labels in the correct order
    mnth_cnts_sf$CntsCat <- factor(
      mnth_cnts_sf$CntsCat,
      levels = labels,
      ordered = TRUE
    )

    leg_label <- expression("Total events/km"^2)
    file_mod  <- "_cat"

  }
#  print('Range of CntsVar')
#  print(range(mnth_cnts_sf$CntsVar)) # for VIINA, 0 to 4.83; 0-4.5 for ACLED
#  max_val <- max(mnth_cnts_sf$CntsVar)
#  print(paste('Max map value:', max_val))
  print("category counts:")
  print(table(mnth_cnts_sf$CntsCat))
#     0  (0–1)  [1–4)  [4–9) [9–16)    ≥16 
#  1098    624     31     12      3      1  VIINA counts
#  1153    570     40      4      1      1  ACLED counts
#  browser()

  if (borders_str == "ADM3")
    border_col <- "gray" #"#2b2b2b" 
  else if (borders_str == "ADM4")
    border_col <- NA # remove border color for ADM4
  else
    print(paste("ERROR: unexpected borders_str", borders_str))

  # set map colors to match monthly count plots from PlotEventData.R
  #  scale_color_manual(values = c("ACLED" = "#377eb8", "VIINA" = "#984ea3"))
  # https://www.w3schools.com/colors/colors_picker.asp
#  RColorBrewer::display.brewer.all(type="seq")
  if (events_str == "ACLED") {
    #map_color <- "#17364f" # 20% of original color: "#377eb8"
    #brewer_pal <- "Blues"
    brewer_pal <- RColorBrewer::brewer.pal(5, "GnBu") # "Blues"
  } else if (events_str == "VIINA") {
    #map_color <- "#402145" # 20% of original color: "#984ea3"
#    brewer_pal <- "Purples"
    brewer_pal <- RColorBrewer::brewer.pal(5, "BuPu") # Purples
  } else
    print(paste("ERROR: unexpected events_str", events_str))
  names(brewer_pal) <- labels[-1]   # drop first label
  brewer_pal <- c("0" = "white", brewer_pal)  # exact zero is white
  
  map_plt <- ggplot(mnth_cnts_sf) +
    #geom_sf(aes(fill = CntsVar), color=border_col) +
    geom_sf(aes(fill = CntsCat), color = border_col) +
    #scale_fill_lajolla_c() + # https://r-charts.com/color-palettes/
    geom_sf(data = adm1_sf, fill = NA, color = "#2b2b2b", linewidth = 0.1) +
    geom_sf(data = adm0_sf, fill = NA, color = "#101010", linewidth = 0.4) +
    scale_fill_manual(
      values = brewer_pal,
      na.value = "white",
      drop = FALSE
    ) +
#    scale_fill_gradientn(
#      colors = c("white", map_color), # Light to strong colors #e6e6e6
#      values = scales::rescale(c(0, max_val)),
#      limits = c(0, max_val),  # Force all maps to have the same limits
#      na.value = "white" # Define a color for NA values
#    ) +
#     labs(title = bquote(.(events_str) ~ "Total Events/Km"^2), fill = leg_label) +
    labs(fill = leg_label) +
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
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.95, "lines"),
      axis.title = element_blank(),
      axis.text = element_blank(), # remove the lat/lon text labels
      axis.ticks = element_blank()
    )
  #print(map_plt)

  
  outDir <- file.path(analysisDir, "Total_Event_Maps")
  if (!dir.exists(outDir)) {
    print(paste("creating directory:", outDir))
    dir.create(outDir)
  }
  png_filename <- paste0(borders_str, "_",events_str,"_TotEventsMap", file_mod, ".png")
  full_file <- file.path(outDir, png_filename)
  print(full_file)
  ggsave(full_file, plot = map_plt, width = 8, height = 6)
  
  return(map_plt)
}

# square root categories
viina_plt <- total_events_km2(adm3_viina, "ADM3", "VIINA", log_offset = 10)
acled_plt <- total_events_km2(adm3_acled, "ADM3", "ACLED", log_offset = 10)

#viina_plt <- viina_plt + labs(subtitle = "VIINA Total Events / km²")
#acled_plt <- acled_plt + labs(subtitle = "ACLED Total Events / km²")

load_install_lib("patchwork")
combined_plt <- viina_plt / acled_plt #+ # "/" stacks vertically
#  plot_annotation(tag_levels = 'A', tag_suffix = ")")

load_install_lib("cowplot")

final_plot <- ggdraw(combined_plt) +
  draw_label(
    "A) VIINA Total Events / km²",
    x = 0.1, y = 0.98,
    hjust = 0, vjust = 1,
    size = 12
#    fontface = "bold"
  ) +
  draw_label(
    "B) ACLED Total Events / km²",
    x = 0.1, y = 0.48,
    hjust = 0, vjust = 1,
    size = 12
#    fontface = "bold"
  )


outDir <- file.path(analysisDir, "Total_Event_Maps")
#png_filename <- paste0("ADM3", "_","Combined","_TotEventsMap", "_sqrt", ".png")
png_filename <- paste0("ADM3", "_","Combined","_TotEventsMap", "_cat", ".png")
full_file <- file.path(outDir, png_filename)
print(full_file)
ggsave(full_file, plot = final_plot, width = 7, height = 9)


#total_events_km2(adm3_viina, "ADM3", "VIINA", log_offset = FALSE)
#total_events_km2(adm3_acled, "ADM3", "ACLED", log_offset = FALSE)

if (FALSE) {
  total_events_km2(adm3_viina, "ADM3", "VIINA", log_offset = 0.1)
  total_events_km2(adm3_acled, "ADM3", "ACLED", log_offset = 0.1)
  
  total_events_km2(adm3_viina, "ADM3", "VIINA", log_offset = 1)
  total_events_km2(adm3_acled, "ADM3", "ACLED", log_offset = 1)
}

#############################
# write event count maps to disk for each month
#   requires global 'analysisDir'
#   requires global 'adm1_sf'
# Feb 2026, change trans='sqrt' option to use nonlinear categories, which breaks the other trans options
#############################
monthly_pngs <- function(mnth_cnts_sf, borders_str, events_str, trans="counts") {

  folder <- paste(borders_str, events_str, sep="_")
  outDir <- file.path(analysisDir , folder)
  if (!dir.exists(outDir)) {
    print(paste("creating directory:", outDir))
    dir.create(outDir)
  }
  
  if (trans == "counts") {
    leg_label <- "Counts"
    file_mod <- "_counts"
    sf_long <- pivot_longer(mnth_cnts_sf, 
                            cols = matches("^m\\d{2}_\\d{4}$"), # "m??_????"
                            #cols = starts_with("m02_2022"):starts_with("m09_2023"),
                            #cols = starts_with("m02_2022"):starts_with("m10_2023"),
                            #cols = starts_with("m02_2022"):starts_with("m10_2022"),
                            #cols = starts_with("m02_2022"):starts_with("m04_2022"),
                            names_to = "Month", values_to = "Count")
    #colnames(sf_long)
    table(sf_long$Month)
    print(paste("total num events:", sum(sf_long$Count))) # 78913
    sf_long$CntsVar <- sf_long$Count
  } else {
    adm_evnts_norm <- mnth_cnts_sf %>%
      mutate(across(
        matches("^m"), # Select columns starting with "m"
        ~ (. / AREA_SQKM)  # normalize by ADM district area
      ))
    sf_long <- pivot_longer(adm_evnts_norm, 
                            cols = matches("^m\\d{2}_\\d{4}$"), # "m??_????"
                            names_to = "Month", values_to = "CntsKm2")
    # used to be CntKm2, perhaps intentionally singular since for one month, but change to CntsKm2 for potential future function
    print(paste("total area of events:", sum(sf_long$CntsKm2)))
    if (trans == "area") {
      #browser()
      leg_label <- "Counts/Km2"
      file_mod <- "_area"
      #colnames(sf_long)
      table(sf_long$Month)
      sf_long$CntsVar <- sf_long$CntsKm2
    } else if (trans == "sqrt") {
      leg_label <- "sqrt(Cnts/Km2)"
      file_mod <- "_sqrt"
      #table(sf_long$Month)
      sf_long$CntsVar <- sqrt(sf_long$CntsKm2)
    } else if (trans == "cat") {

      # ---- sqrt categorical bins ----
      #breaks <- c(0, 1, 4, 9, 16, Inf)
      breaks <- c(0, 0.1, 0.2, 0.4, 1.0, Inf)

      # strictly positive bins
      sf_long$CntsCat <- cut(
        sf_long$CntsKm2,
        breaks = breaks,
        include.lowest = TRUE,
        right = FALSE
      ) %>% as.character()  # convert to character so we can relabel
#      browser()
#      print(table(sf_long$CntsCat))

      # exact zero
      sf_long$CntsCat[abs(sf_long$CntsKm2) < .Machine$double.eps] <- "0"
      # left hand side must match cuts from above exactly
      map_labels <- c(
        "0"         = "0",
        "[0,0.1)"   = "(0 - 0.1)",
        "[0.1,0.2)" = "[0.1 - 0.2)",
        "[0.2,0.4)" = "[0.2 - 0.4)",
        "[0.4,1)"   = "[0.4 - 1.0)",
        "[1,Inf]"   = "≥ 1.0"
      )
      bad_levels <- setdiff(unique(sf_long$CntsCat), names(map_labels))
      if (length(bad_levels) > 0) {
        stop("Unmapped CntsCat levels:\n", paste(bad_levels, collapse = ", "))
      }
      sf_long$CntsCat <- map_labels[sf_long$CntsCat]
      labels <- unname(map_labels) 

      # convert to factor with your custom labels in the correct order
      sf_long$CntsCat <- factor(
        sf_long$CntsCat,
        levels = labels,
        ordered = TRUE
      )

      leg_label <- expression("Total events/km"^2)
      file_mod  <- "_cat"

    } else {
      print("ERROR: unexpected tranformation")
      return(FALSE)
    }
  }
  
#  max_counts <- max(sf_long$CntsVar)
#  print(paste("max value:", max_counts)) # 617
  print(max(sf_long$CntsKm2)) # 2.77
  print("category counts:")
  print(table(sf_long$CntsCat))
  print(paste("number of NAs should be 0:", sum(is.na(sf_long$CntsCat))))
  print(nrow(sf_long))

#  subfolder <- paste(folder, sep="_", file_mod)
  subfolder <- paste(folder, sep="", file_mod)
  outDir <- file.path(outDir , subfolder)
  if (!dir.exists(outDir)) {
    print(paste("creating directory:", outDir))
    dir.create(outDir)
  }
  
  # Create a plot for each month and save as a PNG file
  months <- unique(sf_long$Month)  # Get list of unique months
  
  if (events_str == "ACLED") {
#    map_color <- "#17364f" # 20% of original color: "#377eb8"
    brewer_pal <- RColorBrewer::brewer.pal(5, "GnBu") # "Blues"
  } else if (events_str == "VIINA") {
    #map_color <- "#402145" # 20% of original color: "#984ea3"
    brewer_pal <- RColorBrewer::brewer.pal(5, "BuPu") # Purples
  } else
    print(paste("ERROR: unexpected events_str", events_str))

  names(brewer_pal) <- labels[-1]   # drop first label
  brewer_pal <- c("0" = "white", brewer_pal)  # exact zero is white

  if (borders_str == "ADM3")
    border_col <- "gray" #"#2b2b2b" 
  else if (borders_str == "ADM4")
    border_col <- NA # remove border color for ADM4
  else
    print(paste("ERROR: unexpected borders_str", borders_str))
  
  # timeline bar month setup: convert month strings to Date objects
  months_date <- as.Date(paste("1", months, sep = "_"), format="%d_m%m_%Y")
  n_months <- length(months)
  month_index <- seq_len(n_months)
  # timeline bar map extent
  bb <- st_bbox(sf_long)
  xmin_map <- bb["xmin"]
  xmax_map <- bb["xmax"]
  ymin_map <- bb["ymin"]
  ymax_map <- bb["ymax"]
  x_range <- xmax_map - xmin_map
  y_range <- ymax_map - ymin_map
  # timeline location at bottom center of map
  bar_xmin <- xmin_map + 0.15 * x_range
  bar_xmax <- xmin_map + 0.85 * x_range
  # height offset above the map
  bar_height <- 0.015 * y_range
  bar_gap    <- 0 # 0.02  * y_range
  bar_ymin <- ymax_map + bar_gap
  bar_ymax <- bar_ymin + bar_height
  #bar_ymin <- ymin_map + 0.94 * y_range
  #bar_ymax <- ymin_map + 0.96 * y_range
  pad <- 0.002 * x_range
  
  for (month in months) {
    # timeline bar setup
    i <- which(months == month)   # current month index
    progress <- i / n_months
    bar_xprog <- bar_xmin + progress * (bar_xmax - bar_xmin)

    # Filter data for the current month
    shp_month <- subset(sf_long, Month == month)
    mnth_obj <- as.Date(paste("1", month, sep="_"), format="%d_m%m_%Y")
    mnth_label <- format(mnth_obj, "%b %Y")
    yr_mnth <- format(mnth_obj, "%Y_%m")
    #print(paste("total num events:", sum(shp_month$CntsVar),"for", mnth_label))
    print(paste("total events/km2:", sum(shp_month$CntsKm2),"for", mnth_label))

    p <- ggplot(shp_month) +
      #geom_sf(aes(fill = CntsVar), color=border_col) +
      geom_sf(aes(fill = CntsCat), color = border_col) +
      #scale_fill_lajolla_c() + # https://r-charts.com/color-palettes/
      geom_sf(data = adm1_sf, fill = NA, color = "#2b2b2b", linewidth = 0.1) +
      geom_sf(data = adm0_sf, fill = NA, color = "#101010", linewidth = 0.4) +
      scale_fill_manual(
        values = brewer_pal,
        na.value = "white",
        drop = FALSE
      ) +
#      scale_fill_gradientn(
#        colors = c("#f2f2f2", map_color),
#        #colors = c("#f2f2f2", "darkred"), # Light to strong colors
#        values = scales::rescale(c(0, max_counts)),
#        limits = c(0, max_counts),  # Force all maps to have the same limits
#        na.value = "white" # Define a color for NA values
#      ) +
#      labs(title = paste0(events_str, " Events, ", mnth_label), fill = leg_label) +
      labs(title = paste0(events_str, " Events"), fill = leg_label) +
#      labs(fill = leg_label) +

  # timeline bar outline (drawn once, always full length)
  annotate(
    "rect",
    xmin = bar_xmin, xmax = bar_xmax,
    ymin = bar_ymin, ymax = bar_ymax,
    fill = NA,
    color = "black",
    linewidth = 0.3
  ) +
  # timeline bar background fill
  annotate(
    "rect",
    xmin = bar_xmin, xmax = bar_xmax,
    ymin = bar_ymin, ymax = bar_ymax,
    fill = "grey90",
    color = NA
  ) +
  # timeline bar fill
#  annotate("rect", xmin = bar_xmin, xmax = bar_xprog,ymin = bar_ymin, ymax = bar_ymax, fill = "grey15", color = NA ) +     
annotate(
  "rect",
  xmin = bar_xmin + pad,
  xmax = bar_xprog - pad,
  ymin = bar_ymin + 0.15 * bar_height,
  ymax = bar_ymax - 0.15 * bar_height,
  fill = "grey15",
  color = NA
) +      
  # timeline bar start & end months
  annotate(
    "text",
    x = bar_xmin,
    y = bar_ymax + 0.012 * y_range,
    label = format(min(months_date), "%b %Y"),
    hjust = 0,
    size = 3
  ) +
  annotate(
    "text",
    x = bar_xmax,
    y = bar_ymax + 0.012 * y_range,
    label = format(max(months_date), "%b %Y"),
    hjust = 1,
    size = 3
  ) +
  # timeline bar moving month label
  annotate(
    "text",
    x = bar_xprog,
    y = bar_ymin - 0.01 * y_range,
    label = mnth_label,
    size = 3,
    vjust = 1
  ) +
  coord_sf(
    xlim = c(xmin_map, xmax_map),
    ylim = c(ymin_map, ymax_map),
    expand = FALSE,
    clip = "off"
  ) +
      
      theme_minimal() +
      theme(
    plot.margin = margin(t = 25, r = 10, b = 10, l = 10), # for timeline bar
        panel.background = element_rect(fill = "white", color = NA), # White background for the map
        plot.background = element_rect(fill = "white", color = NA),  # White background for the entire plot
        panel.grid = element_blank(),  # Remove gridlines
        plot.title = element_text(size = 14, hjust = 0.5), # Adjust title position
        
        # Correct legend positioning for ggplot2 3.5.0+
        #legend.position = "right",     # Position the legend
        legend.position = "inside",
        legend.position.inside = c(0.05, 0.05),
        legend.justification = c("left", "bottom"),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.95, "lines"),
        axis.title = element_blank(),
        axis.text = element_blank(), # remove the lat/lon text labels
        axis.ticks = element_blank()
      )
    
    # Save the plot to a PNG file w/ yr_mnth for correct sorting
    png_filename <- paste0(outDir, "/", borders_str, "_",events_str, file_mod, "_", yr_mnth, ".png")
    print(png_filename)
    ggsave(png_filename, plot = p, width = 8, height = 6)
  }
}

# change to non-linear categories
monthly_pngs(adm3_viina, "ADM3", "VIINA", trans="cat")
monthly_pngs(adm3_acled, "ADM3", "ACLED", trans="cat")

if (FALSE) {
  # sqrt representation is better than raw counts
  monthly_pngs(adm3_viina, "ADM3", "VIINA", trans="sqrt")
  monthly_pngs(adm3_acled, "ADM3", "ACLED", trans="sqrt")

  monthly_pngs(adm3_viina, "ADM3", "VIINA", trans="counts")
  monthly_pngs(adm3_acled, "ADM3", "ACLED", trans="counts")
  
  # a few small areas dominate these maps resulting in not much variation over space
  monthly_pngs(adm3_viina, "ADM3", "VIINA", trans="area")
  monthly_pngs(adm3_acled, "ADM3", "ACLED", trans="area")

  #monthly_pngs(adm4_viina, "ADM4", "VIINA")
  #monthly_pngs(adm4_acled, "ADM4", "ACLED")
}


###########
# combine monthly png files into a single gif
###########
source(file.path(analysisDir, "MakeGifFunction.R"))

make_gif(analysisDir , "ADM3_VIINA/ADM3_VIINA_cat", prefix="ADM3_VIINA_cat")
make_gif(analysisDir , "ADM3_ACLED/ADM3_ACLED_cat", prefix="ADM3_ACLED_cat")

if (FALSE) {
  make_gif(analysisDir , "ADM3_VIINA/ADM3_VIINA_sqrt", prefix="ADM3_VIINA_sqrt")
  make_gif(analysisDir , "ADM3_ACLED/ADM3_ACLED_sqrt", prefix="ADM3_ACLED_sqrt")

  make_gif(analysisDir , "ADM3_VIINA/ADM3_VIINA_counts", prefix="ADM3_VIINA_counts")
  make_gif(analysisDir , "ADM3_ACLED/ADM3_ACLED_counts", prefix="ADM3_ACLED_counts")

  make_gif(analysisDir , paste("ADM4", "VIINA", sep="_"))
  make_gif(analysisDir , paste("ADM4", "ACLED", sep="_"))
}

####################
if (FALSE) {
  ggplot(data = adm3_sf) +
    geom_sf(aes(fill = evnt_count), color = "black") +
    scale_fill_viridis_c(option = "plasma", name = "Point Counts") +
    labs(title = "Point Counts by Polygon") +
    theme_minimal()
  
  plot(adm3_sf)
}