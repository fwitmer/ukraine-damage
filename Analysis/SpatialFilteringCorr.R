##########################################################################
# 23 Jan 2026
#
#	1) calculate spatial lags for event and SAR data
# 2) convert wide data to long format
# 3) add temporal lag to spatial lag columns
# 4) apply spatial filtering to the 3 metrics
# 5) calculate correlation coefficients of the residuals
# 6) calculate Moran's I of original vars plus residuals
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

######################
# calculate 1st order spatial lag
######################
load_install_lib("spdep")

adm_wide_sf <- st_make_valid(adm_wide_sf) # ensure valid geometries
nb <- poly2nb(adm_wide_sf, queen = TRUE) # queen contiguity neighbors; ~ 5-10 seconds
print(nb)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) # row-standardized weights
print(lw)

acled_cols <- grep("^ACLED_", names(adm_wide_sf), value = TRUE)
viina_cols <- grep("^VIINA_", names(adm_wide_sf), value = TRUE)
sar_cols   <- grep("^SAR_",   names(adm_wide_sf), value = TRUE)

spatial_lag_cols <- function(sf_obj, cols, lw, suffix = "_splag") {
  for (var_name in cols) {
    sf_obj[[paste0(var_name, suffix)]] <-
      lag.listw(lw, sf_obj[[var_name]], zero.policy = TRUE)
  }
  sf_obj
}

adm_wide_sf <- adm_wide_sf |>
  spatial_lag_cols(acled_cols, lw, "_splag") |>
  spatial_lag_cols(viina_cols, lw, "_splag") |>
  spatial_lag_cols(sar_cols,   lw, "_splag")

#head(adm_wide_sf)
colnames(adm_wide_sf)

######################
# convert each variable from wide to long format
#     handle case for _splag using the var_suffix parameter
######################
var_to_long <- function(sf_obj, var_prefix, var_suffix = "") {
  # sf_obj <- adm_wide_sf; var_prefix <- "ACLED"

    # rename columns from "'var_prefix'_03_2022" to "m03_2022" to match later
  sf_obj <- sf_obj %>%
    rename_with(
      .fn = ~ str_replace_all(., paste0("^", var_prefix,"_(\\d{2})\\_(\\d{4})",var_suffix,"$"), "m\\1_\\2"),
      .cols = starts_with(var_prefix)
    )
  
  var_long <- paste0(var_prefix, var_suffix)
  sf_long_all <- pivot_longer(sf_obj, 
                          cols = matches(paste0("^m[0-9]{2}_[0-9]{4}$")),
                          names_to = "Month",
                          values_to = var_long)
  
  #colnames(sf_long_all)
  borders_str <- "ADM3"
  ID_var <- paste0(borders_str, "_PCODE")
  sf_long <- sf_long_all[, c(ID_var, "Month", var_long)]
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
acled_splag_long <- var_to_long(adm_wide_sf, "ACLED", var_suffix="_splag")
viina_splag_long <- var_to_long(adm_wide_sf, "VIINA", var_suffix="_splag")
sar_splag_long <- var_to_long(adm_wide_sf, "SAR", var_suffix="_splag")

joined <- acled_long %>%
  left_join(viina_long %>% select(adm_month, VIINA), by = "adm_month") %>%
  left_join(sar_long %>% select(adm_month, SAR), by = "adm_month") %>%
  left_join(acled_splag_long %>% select(adm_month, ACLED_splag), by = "adm_month") %>%
  left_join(viina_splag_long %>% select(adm_month, VIINA_splag), by = "adm_month") %>%
  left_join(sar_splag_long %>% select(adm_month, SAR_splag), by = "adm_month")

head(joined)

# prepare data for lag calculation by creating month_date field
load_install_lib("lubridate")
joined <- joined %>%
  mutate(
    month_date = ymd(
      str_replace(Month, "^m(\\d{2})_(\\d{4})$", "\\2-\\1-01")
    )
  )

###########################
# calculate temporal lags for original and _splag vars
###########################
st_long <- joined %>%
  arrange(ADM3_PCODE, month_date) %>%
  group_by(ADM3_PCODE) %>%
  mutate(
    across(
#      ends_with("_splag"),
      c(ACLED, VIINA, SAR, ends_with("_splag")),
      ~ lag(.x, 1),
      .names = "{.col}_lag1"
    )
  ) %>%
  ungroup()

colnames(st_long)
# check for increasing dates
st_long %>%
  filter(ADM3_PCODE == first(ADM3_PCODE)) %>%
  select(Month, month_date)


###########################
# partial correlation controlling for space & time fixed effects
#    I don't really like this fixed effects approach
#    Note: each model takes a few minutes to run
###########################
resACLED <- resid(lm(ACLED ~ factor(month_date) + factor(ADM3_PCODE), data = st_long))
resVIINA <- resid(lm(VIINA ~ factor(month_date) + factor(ADM3_PCODE), data = st_long))
resSAR <- resid(lm(SAR ~ factor(month_date) + factor(ADM3_PCODE), data = st_long))

cor(resVIINA, resSAR, use = "complete.obs") # 0.2432
cor(resACLED, resSAR, use = "complete.obs") # 0.1048
cor(resVIINA, resACLED, use = "complete.obs") # 0.2670

###########################
# spatially filtered correlation
#    pred_var should end in "_splag" or "_splag_lag1"
###########################
spatial_filter <- function(df, y, pred_var) {
  y_vals <- df[[y]]
  pred_vals <- df[[pred_var]]
  ok <- complete.cases(y_vals, pred_vals)
#  browser()
  print(paste0('length y = ', length(y_vals), ', length complete = ', sum(ok)))
  res <- rep(NA_real_, length(y_vals))
  if (sum(ok) >= 2) {
    res[ok] <- residuals(lm(y_vals[ok] ~ pred_vals[ok]))
#   lm(f, data = df, na.action = na.exclude)$residuals
  }
  res
  }

###########################
# spatial + temporal filtered correlation
###########################
sp_plus_ti_filter <- function(df, y) {
  sp_var <- paste0(y, "_splag") # spatial only
  ti_var <- paste0(y, "_lag1") # time only
  y_vals <- df[[y]]
  sp_vals <- df[[sp_var]]
  ti_vals <- df[[ti_var]]
  ok <- complete.cases(y_vals, ti_vals)
#  browser()
  print(paste0('length y = ', length(y_vals), ', length complete = ', sum(ok)))
  res <- rep(NA_real_, length(y_vals))
  if (sum(ok) >= 2) {
    res[ok] <- residuals(lm(y_vals[ok] ~ sp_vals[ok] + ti_vals[ok]))
  }
  res
}

## filter using spatial lag and space-time lag
sp_filtered <- st_long %>%
#  arrange(ADM3_PCODE) %>% # not necessary
#  group_by(Month) %>% # spatially filter each month cross-section
  mutate(
    ACLED_sp_f = spatial_filter(pick(everything()), "ACLED", "ACLED_splag"),
    VIINA_sp_f = spatial_filter(pick(everything()), "VIINA", "VIINA_splag"),
    SAR_sp_f = spatial_filter(pick(everything()), "SAR", "SAR_splag"),
    ACLED_spti_f = spatial_filter(pick(everything()), "ACLED", "ACLED_splag_lag1"),
    VIINA_spti_f = spatial_filter(pick(everything()), "VIINA", "VIINA_splag_lag1"),
    SAR_spti_f = spatial_filter(pick(everything()), "SAR", "SAR_splag_lag1"),
    ACLED_sp_plus_ti_f = sp_plus_ti_filter(pick(everything()), "ACLED"),
    VIINA_sp_plus_ti_f = sp_plus_ti_filter(pick(everything()), "VIINA"),
    SAR_sp_plus_ti_f = sp_plus_ti_filter(pick(everything()), "SAR")
  ) %>%
  ungroup()


colnames(sp_filtered)
# splag
cor(sp_filtered$VIINA_sp_f, sp_filtered$SAR_sp_f, use = "complete.obs") 
cor(sp_filtered$ACLED_sp_f, sp_filtered$SAR_sp_f, use = "complete.obs") 
cor(sp_filtered$VIINA_sp_f, sp_filtered$ACLED_sp_f, use = "complete.obs") 
# splag_lag1
cor(sp_filtered$VIINA_spti_f, sp_filtered$SAR_spti_f, use = "complete.obs") 
cor(sp_filtered$ACLED_spti_f, sp_filtered$SAR_spti_f, use = "complete.obs") 
cor(sp_filtered$VIINA_spti_f, sp_filtered$ACLED_spti_f, use = "complete.obs")
# sp + ti 
cor(sp_filtered$VIINA_sp_plus_ti_f, sp_filtered$SAR_sp_plus_ti_f, use = "complete.obs") # 0.0746
cor(sp_filtered$ACLED_sp_plus_ti_f, sp_filtered$SAR_sp_plus_ti_f, use = "complete.obs") # 0.0571
cor(sp_filtered$VIINA_sp_plus_ti_f, sp_filtered$ACLED_sp_plus_ti_f, use = "complete.obs") # 0.1988

# spatial filtering for each month (don't use):
#  CorrVars        splag   splag_lag1   splag + lag1
# VIINA–SAR       0.2251     0.2093        0.0759
# ACLED–SAR       0.1754     0.1845        0.0467
# VIINA–ACLED     0.3350     0.3469        0.2263

# spatial filtering full dataset:
#  CorrVars        splag   splag_lag1   splag + lag1
# VIINA–SAR       0.2213     0.2085        0.0746
# ACLED–SAR       0.1759     0.1897        0.0571
# VIINA–ACLED     0.3341     0.3487        0.1988



####################
# calculate monthly Moran's I 
####################
monthly_moran_var <- function(df, var, lw) {
  df %>%
    group_by(month_date) %>%
    summarise(
      moran_I = moran.test(
        .data[[var]],
        lw,
        zero.policy = TRUE
      )$estimate["Moran I statistic"],
      p_value = moran.test(
        .data[[var]],
        lw,
        zero.policy = TRUE
      )$p.value,
      .groups = "drop"
    ) %>%
    mutate(variable = var)
}

monthly_moran_var <- function(df, var, lw) {
  df %>%
    group_by(month_date) %>%
    summarise(
      {
        x <- .data[[var]]
        ok <- !is.na(x)

        # guard against too-few observations
        if (sum(ok) < 3) {
          tibble(
            moran_I = NA_real_,
            p_value = NA_real_
          )
        } else {
          mt <- spdep::moran.test(
            x[ok],
            spdep::subset.listw(lw, ok),
            zero.policy = TRUE
          )

          tibble(
            moran_I = unname(mt$estimate["Moran I statistic"]),
            p_value = mt$p.value
          )
        }
      },
      .groups = "drop"
    ) %>%
    mutate(variable = var)
}


vars <- c("ACLED", "VIINA", "SAR", 
          "ACLED_sp_f", "VIINA_sp_f", "SAR_sp_f", 
          "ACLED_spti_f", "VIINA_spti_f", "SAR_spti_f", 
          "ACLED_sp_plus_ti_f", "VIINA_sp_plus_ti_f", "SAR_sp_plus_ti_f")

moran_raw <- bind_rows(
  lapply(vars, monthly_moran_var, df = sp_filtered, lw = lw)
)
colnames(moran_raw)
sig_raw_summary <- moran_raw %>%
  mutate(sig = p_value < 0.05) %>%
  group_by(variable) %>%
  summarise(
    n_months = n(),
    n_sig    = sum(sig, na.rm = TRUE),
    pct_sig  = n_sig / n_months * 100,
    mean_MoranI = mean(moran_I, na.rm = TRUE),
    mean_p   = mean(p_value, na.rm = TRUE),
    .groups = "drop"
  )
sig_raw_summary %>%
  mutate(
    across(
      where(is.numeric),
      ~ formatC(.x, format = "f", digits = 6)
    )
  )

