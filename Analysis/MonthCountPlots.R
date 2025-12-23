##########################################################################
# 19 May 2025
#
# Count the number of months for each category by ADM3 unit and then scale by the sum of the SAR damage area
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

outDir <- file.path(analysisDir, "MonthCountPlots")
if (!dir.exists(outDir)) {
  print(paste("creating directory:", outDir))
  dir.create(outDir)
}


# load functions to read/join monthly event data & SAR data
source(file.path(analysisDir, "ReadMonthlyData.R"))

adm3_wide <- join_wide(baseDir, SAR_DIR, "ADM3", GHSnorm = FALSE)
#adm3_wide <- join_wide(baseDir, SAR_DIR, "ADM3", GHSnorm = TRUE)

##########################
# count SAR and media event months and plot them
# REQUIRES: outDir
##########################
SAR_mediaPlot <- function(wide_df, borders_str, events_str, dot_color, title_prefix="") {
  # wide_df <- adm3_wide; events_str <- "VIINA"

  # Extract the monthly column names for each set
  if (events_str == "BOTH") {
    media_cols <- grep("^ACLED|^VIINA", colnames(wide_df), value = TRUE)
    events_str <- "(VIINA or ACLED)"
  } else {
    media_cols <- grep(paste0("^", events_str), colnames(wide_df), value = TRUE)
    sar_cols <- grep(paste0("^", "SAR"), colnames(wide_df), value = TRUE)
  }
  # Extract month-year from column names (everything after the first underscore)
  months <- sub("^.*?_", "", sar_cols) # must use sar_cols here since they end 1 month earlier [not true anymore]

  # Initialize a data frame for results
  ID_var <- paste0(borders_str, "_PCODE")
  results <- data.frame(wide_df[[ID_var]])
  colnames(results) <- ID_var
  media_mnths <- paste0(events_str, "_mnths")
  results[[media_mnths]] <- rowSums(wide_df[, media_cols] > 0)
  results[["SAR_mnths"]] <- rowSums(wide_df[, sar_cols] > 0)
  results[["SAR_sum"]] <- rowSums(wide_df[, sar_cols])

  tail(results)
  range(results[[media_mnths]]); range(results$SAR_mnths)
  range(results$SAR_sum)
  #hist(results$SAR_sum)
  
  #######################  
  # Create a dot plot with size mapped to the count
  #######################  
  plot_data <- results %>%
    count(!!sym(media_mnths), SAR_mnths)
  
  gg_plt <- ggplot(plot_data, aes(x = !!sym(media_mnths), y = SAR_mnths)) +
    geom_point(aes(size = n), color = dot_color, alpha = 0.7) +
    scale_size_continuous(name = "Count") +
    labs(
      x = paste(events_str, "Months"),
      y = "SAR Damage Months",
      title = paste0(events_str, "-SAR Month Counts")
    ) +
    #theme_minimal()
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray"),
      panel.grid.minor = element_line(color = "lightgray"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.box.just = "center",
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
    )
  
  fname <- paste0(events_str, "-SAR_MnthCnts", ".png")
  print(paste("writing file", fname))
  png_filename <- file.path(outDir, fname)
  ggsave(png_filename, plot = gg_plt, width = 8, height = 6)
  
  #######################  
  # Create a dot plot with size mapped to the SAR area
  #######################  
  plot_data <- results %>%
    #group_by(VIINA_mnths, SAR_mnths) %>%
    group_by(!!sym(media_mnths), SAR_mnths) %>%
    summarise(
      SAR_sum_total = sum(SAR_sum, na.rm = TRUE) / 1e6,  # Convert to sq km
      .groups = "drop")

  print("SAR area range:")
  print(range(plot_data$SAR_sum_total)) # VIINA = 1.67, ACLED = 0.93
  MAX_SAR_AREA <- 1.67
  print(paste('Forcing max SAR area to:', MAX_SAR_AREA))
  #browser()
  # add SAR area by month bins
  bin_sums <- data.frame(area = rep(0,4))
  rownames(bin_sums) <- c("0-5", "6-10", "11-15", "16-20")
  bin_sums$area[1] <- sum(plot_data[plot_data[[media_mnths]] <= 5, "SAR_sum_total"])
  bin_sums$area[2] <- sum(plot_data[plot_data[[media_mnths]] > 5 & plot_data[[media_mnths]] <= 10, "SAR_sum_total"])
  bin_sums$area[3] <- sum(plot_data[plot_data[[media_mnths]] > 10 & plot_data[[media_mnths]] <= 15, "SAR_sum_total"])
  bin_sums$area[4] <- sum(plot_data[plot_data[[media_mnths]] > 15, "SAR_sum_total"])
  print('SAR area sums by month bins:')
  print(bin_sums)
  
  gg_plt <- ggplot(plot_data, aes(x = !!sym(media_mnths), y = SAR_mnths)) +
    geom_point(aes(size = SAR_sum_total), color = dot_color, alpha = 0.7) +
    scale_size_continuous(
      name = expression("SAR Area(km"^2*")"),
      limits = c(0, MAX_SAR_AREA),
#      breaks = c(0.0,0.4, 0.8, 1.2, 1.67),
      breaks = c(0.0,0.4, 0.8, 1.2, 1.6),
      #range = c(0.5, 12)       # dot size
      range = c(0.5, 8)
    ) +
    labs(
      x = paste(events_str, "Months"),
      y = "SAR Damage Months",
      title = paste0(title_prefix, " SAR Damage by ", events_str, "-SAR Month Counts")
    ) +
  #  theme_minimal()
    #theme_minimal(base_size = 14) +
    theme_minimal(base_size = 7) +
    theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_line(color = "lightgray"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center",
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
      )
  
  fname <- paste0(events_str, "-SAR_MnthCntsArea", ".png")
  print(paste("writing file", fname))
  png_filename <- file.path(outDir, fname)
  ggsave(png_filename, plot = gg_plt, width = 8, height = 6)
  
  return(gg_plt)
}

# https://www.w3schools.com/colors/colors_picker.asp
viina_plt <- SAR_mediaPlot(adm3_wide, "ADM3", "VIINA", "#a053ac", "A)") # 50% of original color: "#984ea3"
acled_plt <- SAR_mediaPlot(adm3_wide, "ADM3", "ACLED", "#3b86c4", "B)") # 50% of original color: "#377eb8"

# combine these plots together in one row after moving legend to the bottom
library(patchwork)
combined <- viina_plt | acled_plt # "|" combine on same row
#  plot_annotation(tag_levels = 'A', tag_suffix = ")") # this did not work

png_filename <- paste0("VIINA_ACLED", "_","Combined","_MnthCntsArea", ".png")
full_file <- file.path(outDir, png_filename)
print(full_file)
#ggsave(full_file, plot = combined, width = 14, height = 8) # works w/ base_size = 14
ggsave(full_file, plot = combined, width = 7, height = 4)



SAR_mediaPlot(adm3_wide, "ADM3", "BOTH", "tomato")

######################################
# output from 20 May 2025 run
######################################
if (FALSE) { 
  > SAR_mediaPlot(adm3_wide, "VIINA", "#a053ac") # 50% of original color: "#984ea3"
  [1] "writing file VIINA-SAR_MnthCnts.png"
  [1] "SAR area range:"
  [1] 0.000000 1.664628
  [1] "Forcing max SAR area to: 1.67"
  [1] "SAR area sums by month bins:"
  area
  0-5   1.1783166
  6-10  0.3001330
  11-15 0.3238003
  16-20 2.6124313
  [1] "writing file VIINA-SAR_MnthCntsArea.png"
  > SAR_mediaPlot(adm3_wide, "ACLED", "#3b86c4") # 50% of original color: "#377eb8"
  [1] "writing file ACLED-SAR_MnthCnts.png"
  [1] "SAR area range:"
  [1] 0.0000000 0.9237735
  [1] "Forcing max SAR area to: 1.67"
  [1] "SAR area sums by month bins:"
  area
  0-5   1.3873598
  6-10  0.4093480
  11-15 0.3588272
  16-20 2.2591462
  [1] "writing file ACLED-SAR_MnthCntsArea.png"
  > SAR_mediaPlot(adm3_wide, "BOTH", "tomato")
  [1] "writing file (VIINA or ACLED)-SAR_MnthCnts.png"
  [1] "SAR area range:"
  [1] 0.0000000 0.8039875
  [1] "Forcing max SAR area to: 1.67"
  [1] "SAR area sums by month bins:"
  area
  0-5   0.9337622
  6-10  0.2178159
  11-15 0.1904203
  16-20 3.0726828
  [1] "writing file (VIINA or ACLED)-SAR_MnthCntsArea.png"
  >   
  
}
