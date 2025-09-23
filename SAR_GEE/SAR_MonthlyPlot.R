##########################################################################
# 22 Nov 2024
#
#	plot monthly polygon aggregations of SAR raster damage data
#   aggregations performed in GEE
#
# this file is 'sourced' by PlotEventsAndSAR.R
#
# DEPENDENCY: ReadMonthlyData.R for the read_sar() function
#
##########################################################################

#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lubridate")

# Load necessary libraries
library(dplyr)
library(ggplot2)
#library(lubridate)

#################################################
# Read the CSV file & plot data
# REQUIRES valid SAR_DIR
#################################################
readAndNormData <- function(ADM_VAR, GHS_norm = FALSE) {
  #ADM_VAR <- "ADM3"

  SAR_Dir <- file.path(baseDir, "SAR_GEE/Monthly_Damage/")
  if (!dir.exists(SAR_Dir))
    print(paste("ERROR invalid SAR_Dir", SAR_Dir))
  
  agg_df <- read_sar(SAR_Dir, ADM_VAR, SAR_cols_only = FALSE)
  colnames(agg_df)
  #plot(agg_df$valid_pixel_sum, agg_df$GHS_urban)
  #plot(log(agg_df$valid_pixel_sum), log(agg_df$GHS_urban))
  monthly_columns <- grep("^SAR_\\d{2}\\_\\d{4}$", names(agg_df), value = TRUE)
#  agg_df <- agg_df %>%
#    mutate(total_damage = rowSums(select(., all_of(monthly_columns)), na.rm = TRUE))
#  agg_df$damage_pct <- agg_df$total_damage / agg_df$valid_pixel_sum * 100
  totalMnth <- data.frame(colSums(agg_df[, monthly_columns]))
  colnames(totalMnth) <- "damage_pixels"

  # create month Date column
  month_cols <- sub("^SAR_", "", monthly_columns) # remove leading 'SAR_'
  totalMnth$month <- as.Date(paste0(month_cols, "_01"), format = "%m_%Y_%d")
 
  #browser() 
  if (GHS_norm) {
    # GHS_urban units are 10m pixel counts (thresholded version of original data)
    #   so are directly comparable to the SAR damage pixel counts
    total_GHS_urban <- sum(agg_df$GHS_urban)

    # adjust this GHS normalization to subtract damaged pixels from subsequent months
    totalMnth <- totalMnth %>%
      arrange(month) %>%  # ensure data is ordered chronologically
      mutate(
        cum_damage_pixels = cumsum(damage_pixels),
        lag_cum_damage_pixels = lag(cum_damage_pixels, 1),
        adjusted_valid_pixels = total_GHS_urban - lag_cum_damage_pixels
      )
#    totalMnth["SAR_02_2022", "adjusted_valid_pixels"] <- total_GHS_urban
    # don't hard-code the month, replace the first value instead due to the lag NA
    totalMnth[1, "adjusted_valid_pixels"] <- total_GHS_urban

#    totalMnth$damage_pct <- totalMnth$damage_pixels/total_GHS_urban * 100
    totalMnth$damage_pct <- totalMnth$damage_pixels/totalMnth$adjusted_valid_pixels * 100
  } else {
    totalMnth$damage_pct_raw <- totalMnth$damage_pixels/sum(agg_df$valid_pixel_sum) * 100
    
    sum(totalMnth$damage_pct_raw) # 0.3% --> 0.6 of all valid pixels damaged
  
    # remove damaged pixels from the total valid pixel count over time
    total_valid_pixels <- sum(agg_df$valid_pixel_sum)
    totalMnth <- totalMnth %>%
      arrange(month) %>%  # ensure data is ordered chronologically
      mutate(
        cum_damage_pixels = cumsum(damage_pixels),
        lag_cum_damage_pixels = lag(cum_damage_pixels, 1),
        adjusted_valid_pixels = total_valid_pixels - lag_cum_damage_pixels
      )
    #totalMnth["SAR_02_2022", "adjusted_valid_pixels"] <- total_valid_pixels
    # don't hard-code the month, replace the first value instead due to the lag NA
    totalMnth[1, "adjusted_valid_pixels"] <- total_valid_pixels
    
    totalMnth$damage_pct <- totalMnth$damage_pixels/totalMnth$adjusted_valid_pixels * 100
    #plot(totalMnth$damage_pct_raw, totalMnth$adj_damage_pct)
    #lines(c(0:11/100), c(0:11/100))
    #round(totalMnth[, c("damage_pct_raw", "damage_pct")], 4)
  }
  
  return(totalMnth)
}

#################################################
# plot data
# REQUIRES valid SAR_DIR
#################################################
plotData <- function(ADM_VAR, totalMnth, plt_label) {
#  line_color <- "blue"
  line_color <- "#e64d00"
  plt <- ggplot(totalMnth, aes(x = month, y = damage_pct)) +
  # plt <- ggplot(totalMnth, aes(x = month, y = damage_pct_raw)) +
    geom_line(color = line_color, linewidth = 1) +
    geom_point(color = line_color, size = 2) +
    labs(
      title = paste("SAR Damage, % of", plt_label, "Area by Month"),
      x = "Month",
      y = paste0("Damage Area (% of ", plt_label,")")
    ) +
  #  theme_minimal()
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "gray"),
      panel.grid.minor = element_line(color = "lightgray"),
      panel.grid.major.x = element_blank(), # Remove major vertical lines
      panel.grid.minor.x = element_blank(),  # Remove minor vertical lines
      axis.text.x = element_text(angle = 45, hjust = 1) # Tilt x-axis text
    ) + 
    scale_x_date(
      date_breaks = "1 month",  # Show every month
      date_labels = "%b %Y"    # Format as "Abbreviated Month Year" (e.g., "Feb 2022")
    )
  
  #dataDir <- "C:/Users/witmer/Dropbox/UAA/Research/Conflict_RS/AnnalsPlaceAnnihilation/Data/SAR_Damage/"
  SAR_Dir <- file.path(baseDir, "SAR_GEE/Monthly_Damage/")
  if (!dir.exists(SAR_Dir))
    print(paste("ERROR invalid SAR_Dir", SAR_Dir))
  
  print('Saving file to:')
  fname <- paste0("SAR_Damage_per_month", ADM_VAR, "_", plt_label, ".png")
  full_file <- file.path(SAR_Dir, fname)
  print(full_file)
  ggsave(full_file, plot = plt, width = 10, height = 6, dpi = 300)

#  return(agg_df)
}

#########################################################
# main execution
#########################################################
if (sys.nframe() == 0) { # don't execute if script is being sourced
  baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
  analysisDir <- file.path(baseDir, "Analysis/")
  if (!dir.exists(analysisDir))
    print(paste("ERROR invalid analysisDir", analysisDir))
  
  # load functions to read/join monthly event data & SAR data
  source(file.path(analysisDir, "ReadMonthlyData.R"))
  
#  adm3_totalMnth <- readAndNormData("ADM3", GHS_norm = FALSE) # don't use this one
#  plotData("ADM3", adm3_totalMnth, "Possible")
#  sar_mnth <- adm3_totalMnth[, c("month", "damage_pct")]
#  head(sar_mnth)
  
  adm3_totalMnth <- readAndNormData("ADM3", GHS_norm = TRUE)
  plotData("ADM3", adm3_totalMnth, "GHS")
  
  #adm4_df <- readAndPlotData("ADM4")
}




############################## OLD ###############################

if (FALSE) {
  #install.packages("plotly")
  library(plotly)
  
  #plot(agg_df$events_km2, agg_df$damage_pct)
  writePlotlyHTML <- function(ADM_VAR) {
    text_var <- paste(ADM_VAR, "_EN", sep="")
    agg_df$tooltip_text <- agg_df[[text_var]]
    # Use aes_string() to dynamically set the text argument
    p <- ggplot(agg_df, aes(x = events_km2, y = damage_pct, text = tooltip_text)) +
      geom_point(color = "blue", size = 2) +
      labs(
        title = paste("Damage Percentage vs. VIINA Event Density by", ADM_VAR),
        x = "VIINA Events/km²",
        y = "SAR Damage (% Area)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    
    # Convert ggplot to a plotly object for interactivity
    ggplotly(p, tooltip = "text")
   
    fname <- paste("DamagePlot", ADM_VAR, ".html", sep="") 
    htmlwidgets::saveWidget(ggplotly(p, tooltip = "text"), fname)
  }
  
  printDamageTable <- function(ADM_VAR) {
    agg_df_sort <- agg_df[order(-agg_df$damage_pct), ]
    adm_name_var <- paste(ADM_VAR, "_EN", sep="")
    agg_df_sort$damage_pct <- round(agg_df_sort$damage_pct, 1)
    agg_df_sort$events_km2 <- round(agg_df_sort$events_km2, 1)
    print(agg_df_sort[1:20, c(adm_name_var, "damage_pct", "events_km2")], row.names=FALSE)
    # then copy & paste this table to ChatGPT to have it format it in html
    # TODO: would be better to generate this html code in R and not rely on ChatGPT
    #head(agg_df_sort)
    #tmp <- fix(agg_df_sort)
  }
  
  ADM_VAR <- "ADM3"
  agg_df <- readData(ADM_VAR)
  
  
  #writePlotlyHTML(ADM_VAR)
  #printDamageTable(ADM_VAR)
  #colnames(agg_df)
}

if (FALSE) {
  ADM_VAR <- "ADM4"
  agg_df <- readData(ADM_VAR)
  writePlotlyHTML(ADM_VAR)
  printDamageTable(ADM_VAR)
}

