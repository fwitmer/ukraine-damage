##########################################################################
# 23 Jan 2026
#
#	1) calculate spatial lags for event and SAR data
# 2) convert wide data to long format
# 3) add temporal lag to spatial lag columns
# 4) estimate sp-ti lag regression models
# 5) calculate Moran's I of the residuals
#
##########################################################################

#baseDir <- "D:/Users/witmer/Documents/UAA/Research/Conflict_RS/Ukraine"
baseDir <- getwd()
analysisDir <- file.path(baseDir, "Analysis/")
if (!dir.exists(analysisDir))
  print(paste("ERROR invalid analysisDir", analysisDir))

SAR_Dir <- file.path(baseDir, "SAR_GEE/Monthly_Damage/")
if (!dir.exists(SAR_Dir))
  print(paste("ERROR invalid SAR_Dir", SAR_Dir))

source(file.path(baseDir, "LoadInstallLib.R"))
load_install_lib("sf")
load_install_lib("dplyr")
load_install_lib("ggplot2")
load_install_lib("tidyr") # for pivot_longer
load_install_lib("stringr") # for str_replace_all

# load functions to read/join monthly event data & SAR data
source(file.path(analysisDir, "ReadMonthlyData.R"))

borders_str <- "ADM3"
adm3_wide <- join_wide(baseDir, SAR_DIR, borders_str, GHSnorm = TRUE)

##############
# calculate spatial lag
##############
adm3_fname <- file.path(baseDir, "Borders/ukr_admbnda_adm3_sspe_20240416.shp")
adm3_sf <- st_read(adm3_fname)

# join adm3_wide to borders
ID_var <- paste0(borders_str, "_PCODE")
adm_evnts_small <- adm3_sf[, c(ID_var, "geometry")]
adm_wide_sf <- left_join(adm_evnts_small, adm3_wide, by = ID_var)
colnames(adm_wide_sf)

# calculate 1st order spatial lag
load_install_lib("spdep")

adm_wide_sf <- st_make_valid(adm_wide_sf) # ensure valid geometries
nb <- poly2nb(adm_wide_sf, queen = TRUE) # queen contiguity neighbors
print(nb)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) # row-standardized weights
print(lw)

acled_cols <- grep("^ACLED_", names(adm_wide_sf), value = TRUE)
viina_cols <- grep("^VIINA_", names(adm_wide_sf), value = TRUE)
sar_cols   <- grep("^SAR_",   names(adm_wide_sf), value = TRUE)

spatial_lag_cols <- function(sf_obj, cols, lw, suffix = "_slag") {
  for (var_name in cols) {
    sf_obj[[paste0(var_name, suffix)]] <-
      lag.listw(lw, sf_obj[[var_name]], zero.policy = TRUE)
  }
  sf_obj
}

adm_wide_sf <- adm_wide_sf |>
  spatial_lag_cols(acled_cols, lw, "_slag") |>
  spatial_lag_cols(viina_cols, lw, "_slag") |>
  spatial_lag_cols(sar_cols,   lw, "_slag")

head(adm_wide_sf)
colnames(adm_wide_sf)

# TODO handle case for _slag variables & add 1 month lag for them
var_to_long <- function(sf_obj, var_prefix) {
  # sf_obj <- adm_wide_sf; var_prefix <- "ACLED"

    # rename columns from "'var_prefix'_03_2022" to "m03_2022" to match later
  sf_obj <- sf_obj %>%
    rename_with(
      .fn = ~ str_replace_all(., paste0("^", var_prefix,"_(\\d{2})\\_(\\d{4})$"), "m\\1_\\2"),
      .cols = starts_with(var_prefix)
    )
  
  sf_long_all <- pivot_longer(sf_obj, 
                          cols = matches(paste0("^m[0-9]{2}_[0-9]{4}$")),
                          names_to = "Month",
                          values_to = var_prefix)
  
  #colnames(sf_long_all)
  borders_str <- "ADM3"
  ID_var <- paste0(borders_str, "_PCODE")
  sf_long <- sf_long_all[, c(ID_var, "Month", var_prefix)]
  #colnames(sf_long) # drop the geometry info so that the left_join works
  long_df <- sf_long %>%
    st_drop_geometry()

  long_df <- long_df %>%
    mutate(adm_month = paste0(.data[[ID_var]], ".", Month))
  return(long_df)
}

acled_long <- var_to_long(adm_wide_sf, "ACLED")
viina_long <- var_to_long(adm_wide_sf, "VIINA")
sar_long <- var_to_long(adm_wide_sf, "SAR")

joined <- acled_long %>%
  left_join(viina_long %>% select(adm_month, VIINA), by = "adm_month") %>%
  left_join(sar_long %>% select(adm_month, SAR), by = "adm_month")

head(joined)

acled_panel <- make_var_panel(adm_wide_sf, "ACLED", lw)
viina_panel <- make_var_panel(adm_wide_sf, "VIINA", lw)
sar_panel   <- make_var_panel(adm_wide_sf, "SAR",   lw)










monthly_cols <- grep(
  "^[A-Z]+_[0-9]{2}_[0-9]{4}(_slag)?$",
  names(adm_wide_sf),
  value = TRUE
)

adm_long_sf <- adm_wide_sf |>
  pivot_longer(
    cols = all_of(monthly_cols),
    names_to = c("variable", "month", "type"),
    names_pattern = "([A-Z]+)_([0-9]{2}_[0-9]{4})(?:_(slag))?",
    values_to = "value"
  ) |>
  mutate(
    month = lubridate::my(month),
    type  = ifelse(is.na(type), "raw", type)
  ) |>
  tidyr::pivot_wider(
    names_from  = type,
    values_from = value
  )

head(adm_long_sf)
colnames(adm_long_sf)
table(adm_long_sf$var)
