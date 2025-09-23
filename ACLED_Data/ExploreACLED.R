##########################################################################
# 22 Nov 2024
#
#	read ACLED data
#
#
##########################################################################

#outDir <- "C:/Users/witmer/Dropbox/UAA/Research/Conflict_RS/AnnalsPlaceAnnihilation/Data/ACLED/"
baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
acledDir <- file.path(baseDir, "ACLED_Data")
if (!dir.exists(acledDir))
  print(paste("ERROR invalid acledDir", acledDir))

# Load necessary libraries
library(dplyr)
library(ggplot2)
#library(lubridate)
library(sf)

setwd(acledDir)

# Read the CSV file
acled <- read.csv("ACLED_2022-02-01-2023-12-08-Ukraine.csv")

colnames(acled)
table(acled$event_type) # all Explosions/Remote violence

damage_events <- acled

# Convert 'event_date' to Date
damage_events <- damage_events %>%
  mutate(event_date = as.Date(event_date, format = "%d %B %Y"))
head(damage_events$event_date)

# Subset data between February 2022 and September 2023
subset_data <- damage_events %>%
  filter(event_date >= as.Date("2022-02-01") & event_date <= as.Date("2023-09-30"))

# Create monthly counts
monthly_counts <- subset_data %>%
  mutate(month = format(event_date, "%Y-%m")) %>%
  count(month) %>%
  arrange(month) %>%
  mutate(month = as.Date(paste0(month, "-01"))) # Convert to Date format for plotting

plt <- ggplot(monthly_counts, aes(x = month, y = n)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "ACLED Monthly Violent Event Counts (Feb 2022 - Sep 2023)",
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


print(plt)

setwd(acledDir)
getwd()
# Save the plot as a PNG file
ggsave("ACLED_events_per_month.png", plot = plt, width = 10, height = 6, dpi = 300)





table(damage_events$geo_precision)

if (FALSE) {   # export as a shapefile
  sf_damage <- st_as_sf(damage_events, coords = c("longitude", "latitude"), crs = 4326)
  st_write(sf_damage, "acled_events2022_23.shp")
}

nrow(damage_events) # 65,350
head(damage_events)

# Subset rows by city 
subset_data <- damage_events %>% 
  filter(location %in% c("Mariupol", "Kharkiv", "Chernihiv"))
#tmp <- fix(subset_data)
table(subset_data$location)
