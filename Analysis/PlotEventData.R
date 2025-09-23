##########################################################################
# 26 Mar 2025
#
#	plot conflict event point data
#
# this script is 'sourced' by PlotEventsAndSAR.R
##########################################################################

library(dplyr)
library(ggplot2)

baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"

#end_date <- as.Date("2022-05-30")
#end_date <- as.Date("2023-10-30")
#start_date <- as.Date("2022-02-01"); end_date <- as.Date("2023-09-30")
start_date <- as.Date("2022-03-01"); end_date <- as.Date("2023-10-31")

####### load VIINA data #########
viinaDir <- file.path(baseDir, "VIINA_Data")
source(file.path(viinaDir, "LoadVIINA.R"))
viina <- load_VIINA(viinaDir, start_date=start_date, end_date=end_date)
nrow(viina) # 58868
#table(viina$sources)

if (FALSE) { # write VIINA damage subset for Corey
  outFile <- file.path(viinaDir, paste0("viinaDamageSubset", ".csv"))
  print(paste('Writing csv file to:', outFile))
  write.csv(viina, outFile, row.names = FALSE)
}

####### load ACLED data #########
acledDir <- file.path(baseDir, "ACLED_Data")
source(file.path(acledDir, "LoadACLED.R"))
acled <- load_ACLED(acledDir, start_date=start_date, end_date=end_date)
nrow(acled) # 59587

table(acled$sub_event_type)
print(paste("pctge events that are shelling: ", 52457/59587*100))

##############################
# Create monthly counts
#   REQUIRES: valid date_obj field in the event_data df
##############################
month_counts <- function(event_data) {
  monthly_counts <- event_data %>%
    mutate(month = format(date_obj, "%Y-%m")) %>%
    count(month) %>%
    arrange(month) %>%
    mutate(month = as.Date(paste0(month, "-01"))) # Convert to Date format for plotting
  return(monthly_counts)
}

count_and_combine <- function(viina, acled) {
  viina_mnth <- month_counts(viina)
  acled_mnth <- month_counts(acled)
  
  acled_mnth <- acled_mnth %>%
    mutate(source = "ACLED")
  
  viina_mnth <- viina_mnth %>%
    mutate(source = "VIINA")
  
  combined_df <- bind_rows(acled_mnth, viina_mnth)
  
  return(combined_df)
}

plotCombined <- function(combined_df) {
  plt <- ggplot(combined_df, aes(x = month, y = n, color = source)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = "ACLED and VIINA Monthly Event Counts (Feb 2022 - Sep 2023)",
      x = "Month",
      y = "Number of Events",
      color = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "gray"),
      panel.grid.minor = element_line(color = "lightgray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      
      # Correct legend positioning for ggplot2 3.5.0+
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      limits = c(0, 5000),
      expand = c(0, 0)     # No extra space above/below the data
    ) +
    scale_color_manual(values = c("ACLED" = "#377eb8", "VIINA" = "#984ea3"))
    #scale_color_manual(values = c("ACLED" = "blue", "VIINA" = "red"))
  
  #print(plt)
  print('Saving file to:')
  fname <- "ACLED_VIINA_events_per_month.png"
  full_file <- file.path(analysisDir, fname)
  print(full_file)
  ggsave(full_file, plot = plt, width = 10, height = 6, dpi = 300)
}

##############################################
# don't execute the following if script is being 'sourced'
##############################################
if (sys.nframe() == 0) {
  combined_df <- count_and_combine(viina, acled)
  plotCombined(combined_df)
}
