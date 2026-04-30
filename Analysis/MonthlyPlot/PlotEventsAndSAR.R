##########################################################################
# 26 Mar 2025
#
#	plot conflict event point data
#
##########################################################################

#baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
baseDir <- getwd()

analysisDir <- file.path(baseDir, "Analysis/")
if (!dir.exists(analysisDir))
  print(paste("ERROR invalid analysisDir", analysisDir))

source(file.path(baseDir, "LoadInstallLib.R"))
load_install_lib("dplyr")
load_install_lib("ggplot2")

# load functions to read/join monthly event data & SAR data
source(file.path(analysisDir, "ReadMonthlyData.R"))

sarDir <- file.path(baseDir, "SAR_GEE/")
if (!dir.exists(sarDir))
  print(paste("ERROR invalid sarDir", sarDir))

source(file.path(analysisDir, "PlotEventData.R")) # loads event data
# Combine ACLED and VIINA
combined_df <- count_and_combine(viina, acled)
events_mnth <- combined_df %>%
  select(month, value = n, source)

source(file.path(sarDir, "SAR_MonthlyPlot.R")) # does not read data
adm3_totalMnth <- readAndNormData("ADM3", GHS_norm = TRUE) # SAR_MonthlyPlot.R function
sar_mnth <- adm3_totalMnth[, c("month", "damage_pct")]
head(sar_mnth)

# Prepare SAR data
sar_mnth_clean <- sar_mnth %>%
  mutate(source = "SAR Damage") %>%
  rename(value = damage_pct)

# Find scale factor
scale_factor <- max(events_mnth$value, na.rm = TRUE) / max(sar_mnth_clean$value, na.rm = TRUE)

# Rescale SAR damage
sar_mnth_scaled <- sar_mnth_clean %>%
  mutate(value = value * scale_factor)

# Combine all
plot_data <- bind_rows(events_mnth, sar_mnth_scaled)

# set factor levels to control legend order
plot_data$source <- factor(plot_data$source, levels = c("ACLED", "VIINA", "SAR Damage"))

plt <- ggplot(plot_data, aes(x = month, y = value, color = source, linetype = source)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(
    name = "Event Counts (ACLED & VIINA)",
    sec.axis = sec_axis(~ . / scale_factor, name = "SAR Damage Percentage")
  ) +
  scale_color_manual(values = c(
  "ACLED" = "#CC79A7",     # reddish purple
  "VIINA" = "#0072B2",     # blue
  "SAR Damage" = "#E69F00" # golden orange
  )) +
  # bad colors:
#  scale_color_manual(values = c(   # https://colorbrewer2.org/#type=qualitative&scheme=Set3&n=3
#  "ACLED" = "#8dd3c7", "VIINA" = "#bebada", "SAR Damage" = "#ffffb3" )) +
  scale_linetype_manual(values = c(
    "ACLED" = "dotdash",
    "VIINA" = "dashed",
    "SAR Damage" = "solid"
  )) +
  #scale_color_manual(values = c("ACLED" = "#377eb8", "VIINA" = "#984ea3", "SAR Damage" = "#e64d00")) +
  #scale_linetype_manual(values = c("ACLED" = "solid", "VIINA" = "solid", "SAR Damage" = "dashed")) +
  labs(x = "Month", title = "Event Counts and SAR Damage by Month") +
#  theme_minimal() +
#  theme_minimal(base_size = 14) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "lightgray"),
    panel.grid.major.x = element_blank(), # Remove major vertical lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical lines
    axis.title.y.right = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1), # Tilt x-axis text
    
    # Correct legend positioning for ggplot2 3.5.0+
    legend.position = "inside",
    legend.position.inside = c(0.99, 0.98),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
#    legend.background = element_rect(fill = "white", color = "gray"),
    legend.title = element_blank(), # no legend title
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.95, "lines"),
    legend.key.width = unit(2.3, "lines"),
    legend.spacing.y = unit(0.1, "lines"),
    legend.margin = margin(3, 4, 3, 4)
  ) + 
  scale_x_date(
    limits = range(plot_data$month),
    date_breaks = "1 month",  # Show every month
    date_labels = "%b %Y",    # Format as "Abbreviated Month Year" (e.g., "Mar 2022")
    expand = expansion(add = c(10, 10))
#    expand = c(10, 10)
  )

plotDir <- file.path(analysisDir, "MonthlyPlot/")
if (!dir.exists(plotDir))
  print(paste("ERROR invalid plotDir", plotDir))

print('Saving file to:')
if (FALSE) {
  fname <- "EventsAndSAR_Mnth.png"
  full_file <- file.path(plotDir, fname)
  print(full_file)
  ggsave(full_file, plot = plt, width = 10, height = 6, dpi = 300)
}

# final version for publication
fname <- "WitmerFigure1.tif"
full_file <- file.path(plotDir, fname)
print(full_file)
ggsave(full_file, plot = plt, width = 7, height = 4.2, dpi = 600, compression = "lzw")
#ggsave(full_file, plot = plt, width = 9, height = 5.4, dpi = 600, compression = "lzw")


