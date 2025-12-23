##########################################################################
# 23 May 2024
#
#	read VIINA data
#
# source: https://github.com/zhukovyuri/VIINA
#
#
##########################################################################

#outDir <- "C:/Users/witmer/Dropbox/UAA/Research/Conflict_RS/AnnalsPlaceAnnihilation/Data/VIINA/"
baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
if (!dir.exists(baseDir))
  print(paste("ERROR invalid baseDir", baseDir))

# Load necessary libraries
library(dplyr)
library(ggplot2)
#library(lubridate)
#library(sf)


viinaDir <- file.path(baseDir, "VIINA_Data")
source(file.path(viinaDir, "LoadVIINA.R"))

start_date <- as.Date("2022-02-01")
#end_date <- as.Date("2022-05-30")
end_date <- as.Date("2023-10-30")

####### load VIINA data #########
viina <- load_VIINA(viinaDir, end_date=end_date)

if (FALSE) {   # export as a shapefile
  # WARNING: this is an old file that includes ADM1 and ADM2 events
  sf_damage <- st_as_sf(viina, coords = c("longitude", "latitude"), crs = 4326)
  st_write(sf_damage, "damage_events2022_23.shp")
}


#####
colnames(viina)
sort(table(viina$sources))
nrow(viina)

# Subset rows by city 
subset_data <- viina
#subset_data <- viina %>% 
#  filter(asciiname %in% c("Mariupol", "Kharkiv", "Chernihiv"))

# Extract year and month from the 'date' field
subset_data <- subset_data %>%
  mutate(year_month = format(date_obj, "%Y-%m"))
#subset_data <- subset_data %>%
#  mutate(year_month = as.Date(paste0(substr(date, 1, 6), "01"), format="%Y%m%d"))

start_date <- as.Date("2022-02-01")
end_date <- as.Date("2023-09-01")
# Generate a sequence of months
date_seq <- seq(from = start_date, to = end_date, by = "month")
# Format the sequence as YYYY-MM strings
yyyy_mm_list <- format(date_seq, "%Y-%m")

# filter months
subset_data2 <- subset_data %>%
  filter(year_month %in% yyyy_mm_list)
viina_mnth <- data.frame(table(subset_data2$year_month))
colnames(viina_mnth) <- c("yr_mnth", "monthly_counts")
viina_mnth$yr_mnth <- as.Date(paste0(viina_mnth$yr_mnth, "-01"))

plt <- ggplot(viina_mnth, aes(x = yr_mnth, y = monthly_counts)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "VIINA Monthly Violent Event Counts (Feb 2022 - Sep 2023)",
    x = "Month",
    y = "Number of Events"
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


#print(plt)

fname <- file.path(viinaDir, "VIINA_events_per_month.png")
print(paste("writing plot:", fname))
ggsave(fname, plot = plt, width = 10, height = 6, dpi = 300)


#################### original plots by city ######################

# Count the number of rows per month for each asciiname
monthly_counts <- subset_data %>%
  group_by(asciiname, year_month) %>%
  summarize(count = n(), .groups = 'drop')

# Custom function to format the x-axis labels
#custom_date_labels <- function(date) {
#  ifelse(month(date) == 1, format(date, "%Y\n%b"), format(date, "%b"))
#}

# Plot the number of rows per month as a line plot
plt <- ggplot(monthly_counts, aes(x = year_month, y = count, color = asciiname, group = asciiname)) +
  geom_line(size = 1) +
  labs(title = "Bombing VIINA Events per Month",
       x = "Month",
       y = "Number of Events",
       color = "City") +
  theme_minimal() +
#  scale_x_date(labels = custom_date_labels, date_breaks = "1 month", expand = c(0, 0)) +
  annotate("text", x=20, y=250, label="Warning: first and last months are partial")+
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA),   # White background for entire plot
    plot.title = element_text(hjust = 0.5), # center title
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.ticks = element_line(color = "black"),  # Add axis ticks
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_rect(fill = "white", color = "black")  # Add border to legend
#    legend.key = element_rect(fill = "white", color = "black")  # Ensure legend keys have a border
  )

print(plt)

setwd(outDir)
# Save the plot as a PNG file
#ggsave("All_events_per_month.png", plot = plt, width = 10, height = 6, dpi = 300)
ggsave("Bombing_events_per_month.png", plot = plt, width = 10, height = 6, dpi = 300)

getwd()
