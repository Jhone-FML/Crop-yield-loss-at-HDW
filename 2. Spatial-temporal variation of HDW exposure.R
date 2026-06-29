##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 1 HDW calculation########################################
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 1 HDW calculation########################################
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 1 HDW calculation########################################
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 1 HDW calculation########################################
library(parallel)
library(doParallel)
library(foreach)
library(lubridate)
library(raster)
library(sp)
library(sf)
library(readr)
library(tidyr)
library(data.table)
library(tidyverse)
library(dplyr)
library(devEMF)

message("Loading global data ...")

ERA5L_T2m <- read_csv('.../ERA5L_T2m.csv')
ERA5L_RH  <- read_csv('.../ERA5L_RH.csv')
ERA5L_SMS <- read_csv('.../ERA5L_SMS.csv')
SM_81_18  <- read_csv('.../SM_81_18_root_mean.csv')
CMFD_RH_hourly_spline <- read_delim("..../CMFD_RH_hourly_spline.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


RF_Phenology_predictions = read.csv("....../RF_Phenology_predictions.csv", fileEncoding = "GB2312")
crop_df <- RF_Phenology_predictions


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 1.1 HDW calculation based on county data########################################
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 1.1 HDW calculation based on county data########################################
WheatHDW_FC_CFMD <- function(site_name_i) {
  # site_name_i = common_sites[5]
  
  library(parallel)
  library(doParallel)
  library(foreach)
  library(dplyr)
  library(lubridate)
  library(raster)
  library(sp)
  library(sf)
  library(readr)
  library(tidyr)
  library(data.table)
  
   
  message("Loading global data ...")
  
  #ERA5L_T2m <- read_csv('.../ERA5L_T2m.csv')
  #SM_81_18  <- read_csv('.../SM_81_18_root_mean.csv')
  #CMFD_RH_hourly_spline <- read_delim(".../CMFD_RH_hourly_spline.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


  RF_Phenology_predictions = read.csv(".../RF_Phenology_predictions.csv", fileEncoding = "GB2312")
  crop_df <- RF_Phenology_predictions
 
 
  China_map      = st_read('.../National county-level statistics.shp')
  help_data      = readxl::read_xls('.../help_data.xls')
  
  
  Site_lon_lat = distinct(RF_Phenology_predictions[,c(2:5,15)])

  help_data         = cbind(help_data, China_map$geometry)
  help_data_sf      = st_as_sf(help_data)
  Site_lon_lat_sf   = left_join(help_data_sf[,c(4:6,53)], Site_lon_lat, by = c('site'))
  Site_lon_lat_sf   = Site_lon_lat_sf[is.na(Site_lon_lat_sf$lon)!=T,]
  site_name <- read_csv("deepseek_csv_20250619_c8334f.txt")
  colnames(site_name)[2:3] <- c('NAME', 'SUBRG')
  site_name$SUBRG <- paste0('Subreg_', site_name$SUBRG)
  
  #  Site_lon_lat_sf 
  
  crop_df_mer <- left_join(crop_df, Site_lon_lat_sf[, 1:4], by = c('site', 'Provience'))
  crop_df_mer <- crop_df_mer[!is.na(crop_df_mer$NAME), ]
  crop_df_mer <- left_join(crop_df_mer, site_name[, 2:3], by = 'NAME')
  site_sub1 = unique(crop_df_mer$site[crop_df_mer$SUBRG=="Subreg_1"])
  site      = unique(crop_df_mer$site)
  
  ERA5L_SMS_colns = read.csv('.../ERA5L_SMS_colns.csv', fileEncoding = "GB2312")
  ERA5L_T2m_colns = read.csv('.../ERA5L_T2m_colns.csv', fileEncoding = "GB2312")
  
  Common_sites_1 <- intersect(site_sub1, names(SM_81_18)[!sapply(SM_81_18, anyNA)])
  
  Common_sites_2 <- intersect(site, intersect(ERA5L_SMS_colns[,2], ERA5L_T2m_colns[,2]))
  
  common_sites   = c(Common_sites_1,unique(crop_df_mer$site[crop_df_mer$SUBRG!="Subreg_1"]))
  # --- 3.  ---
  message("Caching .nc raster bricks for years 1980-2018 ...")
  
  
  # 

  temp_loc <- crop_df_mer[crop_df_mer$site == site_name_i, ]
  
  # wind speed
  
  wind_path = substr(list.files('.../Hourly Wind Speed Data/'),16,20)
  
  common_sites <- intersect(common_sites,wind_path)
  
  
  wind_file <- paste0('..../Hourly Wind Speed Data/',
                      'step_iteration_', site_name_i, '.csv')
  if (!file.exists(wind_file)) {
    warning(paste("no wind:", wind_file))
    return(NULL)
  }
  W10 <- fread(wind_file)
  colnames(W10) = c('date', 'W10')
  # temperature and RH 
  T2m = read_csv('.../ERA5L_T2m.csv',
                 col_select = c('date',site_name_i))
  T2m = T2m[year(T2m$date) %in%1981:2018,]
 
  RH=  read_csv('.../CMFD_RH_hourly_spline.csv',
                col_select = c('date',site_name_i))
  RH = RH[year(RH$date) %in% 1981:2018,]
  T2m$date = RH$date
  SM = read_csv('.../SM_81_18_root_mean.csv',
                col_select = c('date', matches(site_name_i)))
  
  TWR = T2m %>% left_join(RH, by  = 'date') %>% left_join(W10,by  = 'date')
  
  colnames(TWR) <- c("date", 'T2', 'RH', 'W10')
  
  colnames(SM)[2] <- 'value'
  expanded_SM <- SM %>% rowwise() %>% do({
    tibble(date = seq.POSIXt(from = as.POSIXct(.$date), by = "hour", length.out = 24),
           value = .$value)
  }) %>% ungroup()
  
  
  TWRSM = TWR %>% left_join( expanded_SM, by  = 'date') 
  colnames(TWRSM)[5] <- 'SM'
  library(tidyr)
  detach("package:tidyr", unload = TRUE)
  # rainfall
  Prec_daily <- read_csv('F:/Crop yield loss at HDW/HDW/Merged_Rain_data.csv',
                         col_select = c('date',site_name_i))
  colnames(Prec_daily)[2] = 'prec'
 
  expanded_Per <- Prec_daily %>% rowwise() %>% do({
    tibble(date = seq.POSIXt(from = as.POSIXct(.$date), by = "hour", length.out = 24),
           value = .$prec)
  }) %>% ungroup()
  
  
  TWRSMPer = TWRSM %>% left_join(expanded_Per, by  = 'date')
  colnames(TWRSMPer)[6] <- 'Per'
  TWRSMPer$Subreg <- temp_loc$SUBRG[1]
  
  TWRSMPer$T2 = as.numeric(TWRSMPer$T2)
  TWRSMPer$RH = as.numeric(TWRSMPer$RH)
  TWRSMPer$W10 = as.numeric(TWRSMPer$W10)
  TWRSMPer$SM = as.numeric(TWRSMPer$SM)
  TWRSMPer$Per = as.numeric(TWRSMPer$Per)
  # 60%quantile
  SM_q30_1 <- quantile(TWRSMPer$SM[year(TWRSMPer$date) <= 1992], probs = 0.6, na.rm = TRUE)
  SM_q30_2 <- quantile(TWRSMPer$SM[year(TWRSMPer$date) > 1992],  probs = 0.6, na.rm = TRUE)
  
  # date
  TWRSMPer$Date   <- as.Date(TWRSMPer$date)
  temp_loc$HDdate <- ymd(temp_loc$HDdate)
  temp_loc$MTdate <- ymd(temp_loc$MTdate)
  
  # Definition of detection functions for all categories of dry-hot wind 
  
  detect_HDW_high_temp <- function(data, subreg, year, sm, SM_q30_1, SM_q30_2) {
    if (subreg == "Subreg_1") {
      sm_thres <- ifelse(year <= 1992, SM_q30_1, SM_q30_2)
      data %>% filter(
        ((SM < sm_thres & T2 >= 31 ) | (SM >= sm_thres & T2 >= 33 )) &
          RH <= 30 & W10 >= 3 
      )
    } else if (subreg == "Subreg_2") {
      data %>% filter(T2 >= 30 & RH <= 30 & W10 >= 3  )
    } else if (subreg == "Subreg_3") {
      data %>% filter(T2 >= 32 & RH <= 30 & W10 >= 3 )
    } else if (subreg == "Subreg_4") {
      data %>% filter(T2 >= 32 & RH <= 30 & W10 >= 2 )
    } else if (subreg == "Subreg_5") {
      data %>% filter(T2 >= 31 & RH <= 30 & W10 >= 0.5 )
    } else {
      data.frame()
    }
  }
  
  detect_HDW_after_rain <- function(data) {

    rain_dates <- data %>% filter(Rain >=5) %>% pull(date)  # 小雨及以上
    HDW_dates <- NULL
    if(length(rain_dates)>0){
      HDW_dates <- NULL
      for (d in 1:length(rain_dates)) {
        post3days <- data %>%
          filter(date > rain_dates[d] & date <= rain_dates[d] + days(3)) %>%
          filter(T2 >=30 & RH <= 40 & W10 >= 3)
        if (nrow(post3days) > 0) {
          HDW_dates <- rbind(HDW_dates, post3days)
        }
      }
      return(HDW_dates)
    }
  }
  

  detect_HDW_dry_wind <- function(data) {
    data %>% filter(T2 >= 25 & RH <= 30 & W10 >= 15)
  }
  
  # Main loop: iterate through each phenological record.
  results <- list()  
  
  for (k in seq_len(nrow(temp_loc))) {
    site_row <- temp_loc[k, ]
    start_date <- site_row$HDdate
    end_date <- site_row$MTdate
    subregion <- site_row$SUBRG
    year_i <- year(start_date)
    
    weather_sub <- TWRSMPer %>%
      filter(Date >= start_date & date <= end_date+day(1) & Subreg == subregion) %>%
      mutate(
        Temp = T2, RH = RH, WS = W10, Rain = Per, SM = SM,
        DayPeriod = case_when(
          date <= start_date + (end_date - start_date) / 3 ~ "Early",
          date <= start_date + 2 * (end_date - start_date) / 3 ~ "Middle",
          TRUE ~ "Late"
        )
      ) %>%
      mutate(site = site_name_i)  #  
    weather_sub = weather_sub[-nrow(weather_sub),]
    # 1. HTLH
    ht_HDW <- detect_HDW_high_temp(weather_sub, subregion, year_i, weather_sub$SM, SM_q30_1, SM_q30_2)
    if (nrow(ht_HDW) > 0) {
      ht_HDW <- ht_HDW %>%
        mutate(Type = "HighTempLowHumidity", Period = DayPeriod, site = site_name_i)
    } else {
      ht_HDW <- data.frame(matrix(rep(NA, 14), ncol = 14))
      colnames(ht_HDW) <- c("date", "T2", "RH", "W10", "SM", "Per", "Subreg", "Date", "Temp",
                            "WS", "Rain", "DayPeriod", "Type", "Period")
      ht_HDW$Type <- "HighTempLowHumidity"
      ht_HDW$site <- site_name_i
    }
    
    # 2. PRGW
    rain_HDW_dates <- detect_HDW_after_rain(weather_sub)
    rain_HDW <- weather_sub %>%
      filter(date %in% rain_HDW_dates$date) %>%
      mutate(Type = "PostRainScorch", Period = DayPeriod, site = site_name_i)
    
    if (nrow(rain_HDW) == 0) {
      rain_HDW <- data.frame(matrix(rep(NA, 14), ncol = 14))
      colnames(rain_HDW) <- c("date", "T2", "RH", "W10", "SM", "Per", "Subreg", "Date", "Temp",
                              "WS", "Rain", "DayPeriod", "Type", "Period")
      rain_HDW$Type <- "PostRainScorch"
      rain_HDW$site <- site_name_i
    }
    
    # 3. DTWD
    dry_HDW <- detect_HDW_dry_wind(weather_sub) %>%
      mutate(Type = "DryWind", Period = DayPeriod, site = site_name_i)
    
    if (nrow(dry_HDW) == 0) {
      dry_HDW <- data.frame(matrix(rep(NA, 14), ncol = 14))
      colnames(dry_HDW) <- c("date", "T2", "RH", "W10", "SM", "Per", "Subreg", "Date", "Temp",
                             "WS", "Rain", "DayPeriod", "Type", "Period")
      dry_HDW$Type <- "DryWind"
      dry_HDW$site <- site_name_i
    }
    
    # Combination of the three types of dry-hot wind data
    combined <- bind_rows(ht_HDW, rain_HDW, dry_HDW) %>%
      mutate(Year = year_i, site = site_name_i)
    
    # Append the result to the results list.
    results <- append(results, list(combined))
  }
  
  # Finally, convert the list to a data frame.
  final_results <- bind_rows(results)

  final_results_cleaned <- final_results %>%
    filter(Type %in% c("PostRainScorch", "HighTempLowHumidity", "DryWind")) %>%
    group_by(date, T2, RH, W10, SM, Per, Temp, WS, Rain) %>%
    mutate(
      keep_row = if_else(Type == "HighTempLowHumidity" & 
                           any(Type == "PostRainScorch"), 
                         FALSE, TRUE)
    ) %>%
    filter(keep_row) %>%
    ungroup() %>%
    dplyr:: select(-keep_row)   
  
  
   return(final_results_cleaned)
}


library(future.apply)
plan(multisession, workers = 40)

epoch_list_groups <- split(1:sl, ceiling(seq_along(1:sl) /20))

HDW_CMFD_SM_df <- NULL
for (i in 1:length(epoch_list_groups)) {
  tryCatch({
    result_list <- future_lapply(common_sites[epoch_list_groups[[i]]], function(s) {
      tryCatch({
        temp_SM_df <- WheatHDW_FC_CFMD(s)
        return(temp_SM_df)
      }, error = function(e) {
        message("Error processing site ", s, ": ", e$message)
        return(NULL)
      })
    }, future.seed = TRUE)
    
     
    result_list <- result_list[!sapply(result_list, is.null)]
    
    if(length(result_list) > 0) {
      temp_SM_df <- do.call(rbind, result_list)
      fwrite(temp_SM_df, paste0('.../Dry_hot_windy_CMFD/',i,'HDW_temp_SM_df.csv'))
      HDW_CMFD_SM_df <- rbind(HDW_CMFD_SM_df, temp_SM_df)
    }
    print(paste("Completed group", i))
  }, error = function(e) {
    message("Error in group ", i, ": ", e$message)
  })
}
fwrite(HDW_CMFD_SM_df, paste0('.../HDW_CMFD_SM_df.csv'))

#  Clean up parallel resources
future::plan(sequential)
gc()

################################## 1.1 HDW calculation based on field data##########################################

library(dplyr)
library(stringr)
library(lubridate)

WheatHDW_EXP_CFMD <- function(site_name_i) {
  # site_name_i = Fcommon_sites[1]
  # --- 1. Load required packages. ---
  library(parallel)
  library(doParallel)
  library(foreach)
  library(dplyr)
  library(lubridate)
  library(raster)
  library(sp)
  library(sf)
  library(readr)
  library(tidyr)
  library(data.table)
  
  # --- 2.The data is read only once globally and shared among all child processes. The global data is read only once and shared by all child processes.---
  message("Loading global data ...")
  
  
  EXDat_Clean_Final = readxl::read_excel(".../EXDat_Clean_Final.xlsx")
  colnames(EXDat_Clean_Final)[2:3] =c('Provience','City')
  crop_df   = EXDat_Clean_Final
   
  China_map      = st_read('.../National county-level statistics.shp')
  help_data      = readxl::read_xls('.../help_data.xls')


  Site_lon_lat_sf   = left_join(help_data[,c(4:6)], crop_df, by = c('site'))
  Site_lon_lat_sf   = Site_lon_lat_sf[!is.na(Site_lon_lat_sf$Culs),]
  site_name         =  read_csv("deepseek_csv_20250619_c8334f.txt")
  colnames(site_name)[2:3] <- c('NAME', 'SUBRG')
  site_name$SUBRG <- paste0('Subreg_', site_name$SUBRG)
  
  # Assuming site_lon_lat_sf is already prepared.
  crop_df_mer <- left_join(Site_lon_lat_sf, site_name[, 2:3], by = 'NAME')
  site_sub1 = unique(crop_df_mer$site[crop_df_mer$SUBRG=="Subreg_1"])
  site      = unique(crop_df_mer$site)
  
  ERA5L_SMS_colns = read.csv('.../ERA5L_SMS_colns.csv', fileEncoding = "GB2312")
  ERA5L_T2m_colns = read.csv('.../ERA5L_T2m_colns.csv', fileEncoding = "GB2312")
  
  Common_sites_1 <- intersect(site_sub1, names(SM_81_18)[!sapply(SM_81_18, anyNA)])
  
  Common_sites_2 <- intersect(site, intersect(ERA5L_SMS_colns[,2], ERA5L_T2m_colns[,2]))
  
  Fcommon_sites   = c(Common_sites_1,unique(crop_df_mer$site[crop_df_mer$SUBRG!="Subreg_1"])
 
 
  temp_loc <- crop_df_mer[crop_df_mer$site == site_name_i, ]
  

  
  wind_path = substr(list.files('.../Hourly Wind Speed Data/'),16,20)
  
  Fcommon_sites <- intersect(Fcommon_sites,wind_path)
  
  
  wind_file <- paste0('.../Hourly Wind Speed Data/',
                      'step_iteration_', site_name_i, '.csv')
  if (!file.exists(wind_file)) {
    warning(paste("no ws:", wind_file))
    return(NULL)
  }
  W10 <- fread(wind_file)
  
  
  T2m = read_csv('.../ERA5L_T2m.csv',
                 col_select = c('date',site_name_i))
  
  RH=  read_csv('.../CMFD_RH_hourly_spline.csv',
                col_select = c('date',site_name_i))
  RH = RH[year(RH$date) %in% 1981:2018,]
  
  SM = read_csv('.../SM_81_18_root_mean.csv',
                col_select = c('date', matches(site_name_i)))
  # 
  TWR <- cbind(T2m, RH[,2], W10$step_iteration_wspd[year(as.Date(W10$time)) %in% 1981:2018])
  colnames(TWR) <- c("date", 'T2', 'RH', 'W10')
  
  colnames(SM)[2] <- 'value'
  expanded_SM <- SM %>% rowwise() %>% do({
    tibble(datetime = seq.POSIXt(from = as.POSIXct(.$date), by = "hour", length.out = 24),
           value = .$value)
  }) %>% ungroup()
  
  TWRSM <- cbind(TWR, expanded_SM$value[year(expanded_SM$datetime) %in% 1981:2018])
  colnames(TWRSM)[5] <- 'SM'
  library(tidyr)
  detach("package:tidyr", unload = TRUE)
   
  Prec_daily <- read_csv('.../Merged_Rain_data.csv',
                         col_select = c('date',site_name_i))
  colnames(Prec_daily)[2] = 'prec'
   
  expanded_Per <- Prec_daily %>% rowwise() %>% do({
    tibble(datetime = seq.POSIXt(from = as.POSIXct(.$date), by = "hour", length.out = 24),
           value = .$prec)
  }) %>% ungroup()
  
  expanded_Per$datetime = substr(expanded_Per$datetime,1,10)
  colnames(expanded_Per)[1] = 'date'
  expanded_Per$date = as.Date(expanded_Per$date)
  TWRSM$date = as.Date(TWRSM$date)
  
  start_time = as.POSIXct("1981-01-01 00:00:00", tz = "UTC")
  end_time   = start_time + (length(TWRSM$date) - 1) * 3600  
  TWRSM$date = seq(from = start_time, by = "hour", length.out = length(TWRSM$date))
  
  start_time = as.POSIXct("1980-01-01 00:00:00", tz = "UTC")
  end_time   = start_time + (length(expanded_Per$date) - 1) * 3600  
  expanded_Per$date = seq(from = start_time, by = "hour", length.out = length(expanded_Per$date))
  
  inste_date = intersect(expanded_Per$date, TWRSM$date)
  
  TWRSMPer <- cbind(TWRSM[TWRSM$date %in% inste_date,], expanded_Per$value[expanded_Per$date %in%inste_date ])
  
  colnames(TWRSMPer)[6] <- 'Per'
  TWRSMPer$Subreg <- temp_loc$SUBRG[1]
  
  TWRSMPer$T2 = as.numeric(TWRSMPer$T2)
  TWRSMPer$RH = as.numeric(TWRSMPer$RH)
  TWRSMPer$W10 = as.numeric(TWRSMPer$W10)
  TWRSMPer$SM = as.numeric(TWRSMPer$SM)
  TWRSMPer$Per = as.numeric(TWRSMPer$Per)
   
  SM_q30_1 <- quantile(TWRSMPer$SM[year(TWRSMPer$date) <= 1992], probs = 0.6, na.rm = TRUE)
  SM_q30_2 <- quantile(TWRSMPer$SM[year(TWRSMPer$date) > 1992],  probs = 0.6, na.rm = TRUE)
  
  #  
  TWRSMPer$Date   <- as.Date(TWRSMPer$date)
  temp_loc$HDdate <- ymd(temp_loc$HDdate)
  temp_loc$MTdate <- ymd(temp_loc$MTdate)
  
  #  
  
  detect_HDW_high_temp <- function(data, subreg, year, sm, SM_q30_1, SM_q30_2) {
    if (subreg == "Subreg_1") {
      sm_thres <- ifelse(year <= 1992, SM_q30_1, SM_q30_2)
      data %>% filter(
        ((SM < sm_thres & T2 >= 31 ) | (SM >= sm_thres & T2 >= 33 )) &
          RH <= 30 & W10 >= 3 
      )
    } else if (subreg == "Subreg_2") {
      data %>% filter(T2 >= 30 & RH <= 30 & W10 >= 3  )
    } else if (subreg == "Subreg_3") {
      data %>% filter(T2 >= 32 & RH <= 30 & W10 >= 3 )
    } else if (subreg == "Subreg_4") {
      data %>% filter(T2 >= 32 & RH <= 30 & W10 >= 2 )
    } else if (subreg == "Subreg_5") {
      data %>% filter(T2 >= 31 & RH <= 30 & W10 >= 0.5 )
    } else {
      data.frame()
    }
  }
  
  detect_HDW_after_rain <- function(data) {
    # data =  weather_sub

    rain_dates <- data %>% filter(Rain >=5) %>% pull(date)  # 小雨及以上
    HDW_dates <- NULL
    if(length(rain_dates)>0){
      HDW_dates <- NULL
      for (d in 1:length(rain_dates)) {
        post3days <- data %>%
          filter(date > rain_dates[d] & date <= rain_dates[d] + days(3)) %>%
          filter(T2 >= 30& RH <= 40 & W10 >= 3)
        if (nrow(post3days) > 0) {
          HDW_dates <- rbind(HDW_dates, post3days)
        }
      }
      return(HDW_dates)
    }
  }
  
  
  detect_HDW_dry_wind <- function(data) {
    data %>% filter(T2 >= 25 & RH <= 30 & W10 >= 15)
  }
  
  
  results = NULL
  for (k in seq_len(nrow(temp_loc))) {
    site_row <- temp_loc[k, ]
    start_date <- site_row$HDdate
    end_date <- site_row$MTdate
    subregion <- site_row$SUBRG
    year_i <- year(start_date)
    Cul <- site_row$Culs
    weather_sub <- TWRSMPer %>%
      filter(Date >= start_date & date <= end_date+day(1) & Subreg == subregion) %>%
      mutate(
        Temp = T2, RH = RH, WS = W10, Rain = Per, SM = SM,
        DayPeriod = case_when(
          date <= start_date + (end_date - start_date) / 3 ~ "Early",
          date <= start_date + 2 * (end_date - start_date) / 3 ~ "Middle",
          TRUE ~ "Late"
        )
      )
    

    ht_HDW <- detect_HDW_high_temp(weather_sub, subregion, year_i, weather_sub$SM, SM_q30_1, SM_q30_2)
    if(length(ht_HDW$DayPeriod)>0){
      ht_HDW <- ht_HDW %>% mutate(Type = "HighTempLowHumidity", Period = DayPeriod)
    }else{
      ht_HDW <- data.frame(matrix(rep(0,14),ncol = 14))
      colnames(ht_HDW) = c("date", "T2", "RH", "W10", "SM", "Per", "Subreg", "Date" ,"Temp",
                           "WS",  "Rain", "DayPeriod", "Type", "Period" )
      ht_HDW$Type = "HighTempLowHumidity"}
    

    rain_HDW_dates <- detect_HDW_after_rain(weather_sub)
    rain_HDW <- weather_sub %>%
      filter(date %in% rain_HDW_dates$date) %>%
      mutate(Type = "PostRainScorch", Period = DayPeriod)
    
    if(length(rain_HDW$DayPeriod)>0){
      rain_HDW <- rain_HDW %>% mutate(Type = "PostRainScorch", Period = DayPeriod)
    }else{
      rain_HDW<- data.frame(matrix(rep(0,14),ncol = 14))
      colnames(rain_HDW) = c("date", "T2", "RH", "W10", "SM", "Per", "Subreg", "Date" ,"Temp",
                             "WS",  "Rain", "DayPeriod", "Type", "Period" )
      rain_HDW$Type = "PostRainScorch"}
    

    dry_HDW <- detect_HDW_dry_wind(weather_sub) %>%
      mutate(Type = "DryWind", Period = DayPeriod)
    
    if(length(dry_HDW$DayPeriod)>0){
      dry_HDW <- dry_HDW %>% mutate(Type = "DryWind", Period = DayPeriod)
    }else{
      dry_HDW<- data.frame(matrix(rep(0,14),ncol = 14))
      colnames(dry_HDW) = c("date", "T2", "RH", "W10", "SM", "Per", "Subreg", "Date" ,"Temp",
                            "WS",  "Rain", "DayPeriod", "Type", "Period" )
      dry_HDW$Type = "DryWind"}
    
    combined <- rbind(ht_HDW, rain_HDW, dry_HDW) %>%
      mutate(site = site_name_i, Year = year_i, Cul = Cul)
    
    results = rbind(results, combined) 
  }
  
  final_result  = results
  
  final_results_cleaned <- final_result %>%
    filter(Type %in% c("PostRainScorch", "HighTempLowHumidity", "DryWind")) %>%
    group_by(date, T2, RH, W10, SM, Per, Temp, WS, Rain, Cul) %>%
    mutate(
      keep_row = if_else(Type == "HighTempLowHumidity" & 
                           any(Type == "PostRainScorch"), 
                         FALSE, TRUE)
    ) %>%
    filter(keep_row) %>%
    ungroup() %>%
    dplyr:: select(-keep_row)  
  
  return(final_results_cleaned)
}


library(future.apply)
plan(multisession, workers =  40)

epoch_list_groups <- split(1:sl, ceiling(seq_along(1:sl) /20))

HDW_CMFD_SM_EXP <- NULL
for (i in 1:length(epoch_list_groups)) {
  tryCatch({
    result_list <- future_lapply(Fcommon_sites[epoch_list_groups[[i]]], function(s) {
      tryCatch({
        temp_SM_df <- WheatHDW_EXP_CFMD(s)
        return(temp_SM_df)
      }, error = function(e) {
        message("Error processing site ", s, ": ", e$message)
        return(NULL)
      })
    }, future.seed = TRUE)
    
    if(length(result_list) > 0) {
      temp_SM_df <- do.call(rbind, result_list)
      fwrite(temp_SM_df, paste0('...',i,'HDW_temp_SM_df.csv'))
      HDW_CMFD_SM_EXP <- rbind(HDW_CMFD_SM_EXP, temp_SM_df)
    }
    print(paste("Completed group", i))
  }, error = function(e) {
    message("Error in group ", i, ": ", e$message)
  })
}
fwrite(HDW_CMFD_SM_EXP, paste0('.../HDW_CMFD_SM_EXP.csv'))


future::plan(sequential)
gc()



##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 2 SPChanges in HDW  in wheat growing seasons########################################
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 2 SPChanges in HDW  in wheat growing seasons########################################
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 2 SPChanges in HDW  in wheat growing seasons########################################
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 2 SPChanges in HDW  in wheat growing seasons########################################
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 2.1 SPChanges in HDW  in wheat growing seasons bsed on county data################## 

library(dplyr)
library(tidyr)
library(ggplot2)
library(Kendall)
library(trend)
library(changepoint)
library(RColorBrewer)
library(ggpattern)
library(scales)
library(sf)
library(purrr)
library(tibble)
library(data.table)
setwd('.../HDW/')

HDW_CMFD_SM_df <- read.csv(".../HDW_CMFD_SM_df.csv")

HDW_CMFD_SM_df_cleaned <- HDW_CMFD_SM_df %>%
  filter(Type %in% c("PostRainScorch", "HighTempLowHumidity", "DryWind")) %>%
  group_by(date, T2, RH, W10, SM, Per, Temp, WS, Rain) %>%
  mutate(
    keep_row = if_else(Type == "HighTempLowHumidity" & 
                         any(Type == "DryWind"), 
                       FALSE, TRUE)
  ) %>%
  filter(keep_row) %>%
  ungroup() %>%
  dplyr:: select(-keep_row)  




setDT(HDW_CMFD_SM_df)

HDW_CMFD_SM_df_cleaned = HDW_CMFD_SM_df_cleaned%>%  distinct(date,Type, Period, site, Year, .keep_all = TRUE)

HDW_CMFD_SM_df_cleaned = na.omit(HDW_CMFD_SM_df_cleaned)
saveRDS(CMFD_freqs_means_sf, 'CMFD_freqs_means_sf.rds')




#Calculate the annual total duration of each type of hot-dry wind event.
annual_counts <- HDW_CMFD_SM_df_cleaned  %>%
  group_by(site, Year, Type) %>%
  summarise(hours = n(), .groups = "drop") %>%
  complete(site, Year = 1981:2018, Type, fill = list(hours = 0)) %>%
  filter(!is.na(Type))
annual_counts = annual_counts[annual_counts$Type!=0,]

summary_stats <- annual_counts %>%
  group_by(site, Type) %>%
  summarise(
    mean_hours = mean(hours, na.rm = TRUE),
    freq_years = sum(hours > 0, na.rm = TRUE) / n(),
    .groups = "drop"
  )

CMFD_freqs_means_sf = summary_stats %>% left_join(help_data_sf[,c(4:6,53)], by = c('site')) 

CMFD_freqs_means_sf = st_as_sf(CMFD_freqs_means_sf)
boxplot(CMFD_freqs_means_sf$mean_hours)
 
my_breaks <- c(seq(0,20,2),140)
my_labels <- sapply(1:(length(my_breaks) - 1), function(i) {
  paste(my_breaks[i], "~", my_breaks[i + 1])
})
 
CMFD_freqs_means_sf$Type[CMFD_freqs_means_sf$Type=='HighTempLowHumidity'] = 'HTLH'
CMFD_freqs_means_sf$Type[CMFD_freqs_means_sf$Type=="DryWind"] = 'DTWD'
CMFD_freqs_means_sf$Type[CMFD_freqs_means_sf$Type=="PostRainScorch"] = 'PRGW' 
 
HDW_CMFD_mean_gg <- ggplot() +
  geom_sf(data = CMFD_freqs_means_sf, aes(fill = mean_hours), linewidth = 0.01) +
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(9, "BuPu"),na.value = "white",
                    breaks = c(seq(0,5,0.5),seq(10,40,5),140),
                    labels = c(seq(0,5,0.5),seq(10,40,5),'>40'),
                    values = rescale(c(seq(0,5,0.5),seq(10,40,5),140)),
                    name = bquote((hours~season^-1))) +
  labs(subtitle = bquote((a)~Averaged~HDW[HD-MT])) +
  ylim(4000000, 6300000) +facet_grid(. ~ Type) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65", linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50", linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(
    plot.subtitle = element_text(size = 7, hjust = 0.02, vjust = 1.5),
    strip.text = element_text(size = 6),
    legend.key.size = unit(1.5, 'cm'),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = c(0.68, 1.35),
    legend.direction = "horizontal",
    legend.key.height = unit(0.13, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 4.5),
    legend.title = element_text(size = 5.5),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_))


HDW_CMFD_freq_gg <- ggplot() +
  geom_sf(data = CMFD_freqs_means_sf, aes(fill = freq_years*100), linewidth = 0.01) +
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(9, "BuPu"),
                    breaks = seq(0,100,10),
                    limits = c(0,100),na.value = "white",
                    values = rescale(seq(0,100,10)),
                    name = bquote('(%)')) +
  labs(subtitle = bquote((b)~HDW[HD-MT]~frequency)) +
  ylim(4000000, 6300000) +facet_grid(. ~ Type) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65", linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50", linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(
    plot.subtitle = element_text(size = 7, hjust = 0.02, vjust = 1.5),
    strip.text = element_text(size = 6),
    legend.key.size = unit(1.6, 'cm'),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = c(0.71, 1.35),
    legend.direction = "horizontal",
    legend.key.height = unit(0.15, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 4.5),
    legend.title = element_text(size = 5.5),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_))


analysis_data <- annual_counts 
  
poisson_trends <- analysis_data %>% 
  group_by(site, Type) %>% 
  do({
    model <- glm(hours ~ Year, data = ., family = poisson)
    trend <- coef(model)["Year"] 
    p_value <- summary(model)$coefficients["Year", "Pr(>|z|)"] 
   
    data.frame(trend = (exp(trend) - 1) * unique(.$mean_hour), p_value = p_value)
  })


significant_trends <- poisson_trends %>% filter(p_value < 0.05)
print(significant_trends)

CMFD_all_trends_sf = poisson_trends %>% left_join(help_data_sf[,c(4:6,53)], by = c('site')) 


library(sf)
China_map      = st_read('.../National county-level statistics.shp')
China_line     = st_read('.../China_line.shp')
Provience_line = st_read('.../Provience_line.shp')
China_sea      = st_read('.../China_sea.shp')
Chian_frame    = st_read('.../Chian_frame.shp')

summary(CMFD_all_trends_sf$trend)

CMFD_all_trends_sf <- st_as_sf(CMFD_all_trends_sf)

my_breaks <- c(seq(-5,28,2))
my_labels <- sapply(1:(length(my_breaks) - 1), function(i) {
  paste(my_breaks[i], "~", my_breaks[i + 1])
})

CMFD_all_trends_sf <- CMFD_all_trends_sf %>% mutate(sig = p_value < 0.05)

signif_centroids <- CMFD_all_trends_sf %>%filter(sig) %>%mutate(geometry = st_centroid(geometry))  

signif_centroids <- signif_centroids %>%mutate(x = st_coordinates(geometry)[, 1],y = st_coordinates(geometry)[, 2])

 CMFD_all_trends_sf$Type[ CMFD_all_trends_sf$Type=='HighTempLowHumidity'] = 'HTLH'
 CMFD_all_trends_sf$Type[ CMFD_all_trends_sf$Type=="DryWind"] = 'DTWD'
 CMFD_all_trends_sf$Type[ CMFD_all_trends_sf$Type=="PostRainScorch"] = 'PRGW'
 
 signif_centroids <- CMFD_all_trends_sf %>%
  filter(p_value <= 0.05) %>%

  st_cast("POLYGON") %>%
  
  group_by(site, Type) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
 
  mutate(point = st_point_on_surface(geometry)) %>%
  
  mutate(
    area = as.numeric(st_area(geometry)),
    size_pt = scales::rescale(area, to = c(0.3, 2.5))  # 可调
  ) %>%
  
  st_set_geometry("point") %>%
 
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  )
 
HDW_CMFD_trend_gg <- ggplot() +
  geom_sf(data = CMFD_all_trends_sf, aes(fill = trend*10),color = "grey95",linewidth = 0.1) +
  scale_fill_stepsn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")[c(1:5,8,9)]),
                    breaks =  c(seq(-4,10,1)), 
                    labels =  c(seq(-4,10,1)), 
                    limits = c(-4,10),na.value = "white",
                    values = scales::rescale( c(seq(-4,10,1))),
                    name = bquote((hours~dec^-1)))+
  geom_point(
    data = signif_centroids,
    aes(x = x, y = y, size = size_pt), 
    shape = "+",
    color = "gray35", # 不用透明度
    stroke = 0
  ) +
  labs(subtitle = bquote((c)~HDW[HD-MT]~trends),x = NULL, y = NULL ) +
  ylim(4000000, 6300000) +facet_grid(Type ~ .) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65",alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50",alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.02, vjust = 1.5),
    strip.text = element_text(size = 8),
    legend.key.size = unit(0.95, 'cm'),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = c(0.82, 1.08),
    legend.direction = "horizontal",
    legend.key.height = unit(0.15, 'cm'),
    legend.background = element_blank(),
    panel.spacing = unit(0.08, "cm"), 
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 6),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_))


library(devEMF)
library("cowplot")
emf('.../Figure/Figure 1.emf',
    units = "cm",width=15.8, height=15, emfPlus = TRUE, coordDPI = 600, pointsize = 15 )

ggdraw() +
  draw_plot(HDW_CMFD_mean_gg, x = -0.01, y = 0.50,  width =0.44, height = 0.5) +
  draw_plot(HDW_CMFD_freq_gg, x = -0.01, y = -0.005,  width =0.44, height = 0.5)+
  draw_plot(HDW_CMFD_trend_gg, x = 0.26, y = -0.005, width =.85, height = 1.01)
dev.off()

###################################################
###################################################2.2 Analysis on different period of Types
###################################################

annual_counts_period <- HDW_CMFD_SM_df_cleaned %>%
  filter(Period != 0) %>%
  group_by(site, Year, Type, Period) %>%
  summarise(hours = n(), .groups = "drop") %>%
  complete(site, Year = 1981:2018, Type, Period, fill = list(hours = 0)) %>%
  mutate(Period = ifelse(is.na(Period), 
                         rep(c("Early", "Middle", "Late"), length.out = sum(is.na(Period))), 
                         Period)) %>%
  filter(!is.na(Type))

annual_counts_period = annual_counts_period[annual_counts_period$Type!=0,]

summary_stats_period <- annual_counts_period %>%
  group_by(site, Type, Period) %>%
  summarise(
    mean_hours = mean(hours, na.rm = TRUE),
    freq_years = sum(hours > 0, na.rm = TRUE) / n(),
    .groups = "drop"
  )

CMFD_freqs_means_period_sf = summary_stats_period %>% left_join(help_data_sf[,c(4:6,53)], by = c('site')) 

CMFD_freqs_means_period_sf = st_as_sf(CMFD_freqs_means_period_sf)
boxplot(CMFD_freqs_means_period_sf$mean_hours)


CMFD_freqs_means_period_sf$Type[CMFD_freqs_means_period_sf$Type=='HighTempLowHumidity'] = 'HTLH'
CMFD_freqs_means_period_sf$Type[CMFD_freqs_means_period_sf$Type=="DryWind"] = 'DTWD'
CMFD_freqs_means_period_sf$Type[CMFD_freqs_means_period_sf$Type=="PostRainScorch"] = 'PRGW'
 
# @@@@@@@@@@@@@@@@@@@@@@@2.2.1 mean  plot
saveRDS(CMFD_freqs_means_period_sf, 'CMFD_freqs_means_period_sf')
HDW_CMFD_mean_period_gg <- ggplot() +
  geom_sf(data = CMFD_freqs_means_period_sf, aes(fill = mean_hours), linewidth = 0.01) +
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(9, "BuPu"),na.value = "white",
                    breaks = c(seq(0,5,0.5),seq(10,40,5),60),
                    #labels = c(seq(0,5,0.5),seq(10,40,5),'>40'),
                    values = rescale(c(seq(0,5,0.5),seq(10,40,5),60)),
                    name = bquote((hours~season^-1))) +
  labs(subtitle = bquote(Averaged~HDW[HD-MT])) +
  ylim(4000000, 6300000) +facet_grid(Period ~ Type) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65", linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50", linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(
    plot.subtitle = element_text(size = 7, hjust = 0.02, vjust = 1.5),
    strip.text = element_text(size = 6),
    legend.key.size = unit(1.5, 'cm'),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = c(0.68, 1.1),
    legend.direction = "horizontal",
    legend.key.height = unit(0.13, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 4.5),
    legend.title = element_text(size = 5.5),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent", colour = NA_character_))

emf('.../Figure S18.emf',
     units = "cm", width=16,height=10)
HDW_CMFD_mean_period_gg
dev.off()

# @@@@@@@@@@@@@@@@@@@@@@@2.2.2 freq  plot
HDW_CMFD_freq_period_gg <- ggplot() +
  geom_sf(data = CMFD_freqs_means_period_sf, aes(fill = freq_years*100), linewidth = 0.01) +
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(9, "BuPu"),
                    breaks = seq(0,100,10),
                    limits = c(0,100),na.value = "white",
                    values = rescale(seq(0,100,10)),
                    name = bquote('(%)')) +
  labs(subtitle = bquote(HDW[HD-MT]~frequency)) +
  ylim(4000000, 6300000) +facet_grid(Period ~ Type) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65", linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50", linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(
    plot.subtitle = element_text(size = 7, hjust = 0.02, vjust = 1.5),
    strip.text = element_text(size = 6),
    legend.key.size = unit(1.5, 'cm'),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = c(0.68, 1.1),
    legend.direction = "horizontal",
    legend.key.height = unit(0.13, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 4.5),
    legend.title = element_text(size = 5.5),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent", colour = NA_character_))

emf('.../Figure S19.emf',
     units = "cm", width=16,height=10)
HDW_CMFD_freq_period_gg
dev.off()

########@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 2.2.3 trend analysis
analysis_data_period <- annual_counts_period

analysis_data_period <- analysis_data_period %>% 
  group_by(site, Type, Period) %>% 
  mutate(mean_hour = mean(hours, na.rm = TRUE)) %>%
  ungroup()

 
poisson_trends_period <- analysis_data_period %>% 
  group_by(site, Type, Period) %>% 
  do({
    model <- glm(hours ~ Year, data = ., family = poisson)
    trend <- coef(model)["Year"]  # 获取Year_num的系数
    p_value <- summary(model)$coefficients["Year", "Pr(>|z|)"]  # 获取p值
    
    data.frame(trend = (exp(trend) - 1) * unique(.$mean_hour), p_value = p_value)
  })


CMFD_all_trends_period_sf = poisson_trends_period %>% left_join(help_data_sf[,c(4:6,53)], by = c('site')) 

summary(CMFD_all_trends_period_sf$trend)

CMFD_all_trends_period_sf <- st_as_sf(CMFD_all_trends_period_sf)



CMFD_all_trends_period_sf <- CMFD_all_trends_period_sf %>% mutate(sig = p_value < 0.05)


signif_centroids_period <- CMFD_all_trends_period_sf %>%filter(sig) %>%mutate(geometry = st_centroid(geometry))  # 注意我们只替换 geometry 列，不影响其他属性


signif_centroids_period <- signif_centroids_period %>%mutate(x = st_coordinates(geometry)[, 1],y = st_coordinates(geometry)[, 2])

CMFD_all_trends_period_sf$Type[CMFD_all_trends_period_sf$Type=='HighTempLowHumidity'] = 'HTLH'
CMFD_all_trends_period_sf$Type[CMFD_all_trends_period_sf$Type=="DryWind"] = 'DTWD'
CMFD_all_trends_period_sf$Type[CMFD_all_trends_period_sf$Type=="PostRainScorch"] = 'PRGW' 


signif_centroids_period <- CMFD_all_trends_period_sf %>% group_by(Period) %>%
  filter(p_value <= 0.05) %>%

  st_cast("POLYGON") %>%

  group_by(site, Type,Period) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%

  mutate(point = st_point_on_surface(geometry)) %>%

  mutate(
    area = as.numeric(st_area(geometry)),
    size_pt = pmin(scales::rescale(area, to = c(0.01, 0.5)), 0.5) # 可调
  ) %>%

  st_set_geometry("point") %>%

  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  )


HDW_CMFD_trend_period_gg <- ggplot() +
  geom_sf(data = CMFD_all_trends_period_sf, aes(fill = trend*10),
          color = 'white',linewidth =0.1) +
    scale_fill_stepsn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")[c(1:5,8,9)]),
                    breaks =  c(seq(-4,10,1)), 
                    labels =  c(seq(-4,10,1)), 
                    limits = c(-4,10),na.value = "white",
                    values = scales::rescale( c(seq(-4,10,1))),
                    name = bquote((hours~dec^-1)))+
          color = "gray35", alpha = 0.01)+
  geom_point(
       data = signif_centroids_period,
       aes(x = x, y = y, size = size_pt), 
       shape = "+",
       color = "gray35", 
       stroke = 0.4
     ) + scale_size(range = c(0.5,2), name = '')+guides(size = "none") + 
  labs(subtitle = bquote(HDW[HD-MT]~trends),x = NULL, y = NULL ) +
  ylim(4000000, 6300000) +facet_grid(Period ~ Type) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65",alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50",alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.02, vjust = 1.5),
         strip.text = element_text(size = 8),
         legend.key.size = unit(1.5, 'cm'),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         legend.position = c(0.68, 1.12),
         legend.direction = "horizontal",
         legend.key.height = unit(0.15, 'cm'),
         legend.background = element_blank(),
         panel.spacing = unit(0.08, "cm"), 
         legend.text = element_text(size = 5),
         legend.title = element_text(size = 6),
         strip.background = element_rect(color = 'transparent'),
         plot.background = element_rect(fill = "transparent",colour = NA_character_))


emf('.../Figure S20.emf',
     units = "cm", width=16,height=10)
HDW_CMFD_trend_period_gg
dev.off()


#############2.3 Spatial-temporal patterns of partial correlation coefficients between three types of HDW #################################
#############2.3 Spatial-temporal patterns of partial correlation coefficients between three types of HDW #################################



cor_to_long_with_p <- function(df, vars) {
  # 
  if (nrow(df) < 5) {
    return(data.frame(
      Var1 = character(),
      Var2 = character(),
      correlation = numeric(),
      p_value = numeric()
    ))
  }
  
  r <- Hmisc::rcorr(as.matrix(df[, vars]), type = "pearson")
  
  var_names <- rownames(r$r)
  results <- data.frame()
  
  for (i in 2:length(var_names)) {
    for (j in 1:(i-1)) {
      results <- rbind(results, data.frame(
        Var1 = var_names[i],
        Var2 = var_names[j],
        correlation = r$r[i, j],
        p_value = r$P[i, j]
      ))
    }
  }
  return(results)
}



site_corr_long <- wide_data %>%
  group_by(site) %>%
  summarise(
    cor_long = list(cor_to_long_with_p(cur_data(), wind_types)),
    .groups = "drop"
  ) %>%
  unnest(cor_long)

site_corr_long$Var1[site_corr_long$Var1=='HighTempLowHumidity'] = 'HTLH'
site_corr_long$Var1[site_corr_long$Var1=="DryWind"] = 'DTWD'
site_corr_long$Var1[site_corr_long$Var1=="PostRainScorch"] = 'PRGW'

site_corr_long$Var2[site_corr_long$Var2=='HighTempLowHumidity'] = 'HTLH'
site_corr_long$Var2[site_corr_long$Var2=="DryWind"] = 'DTWD'
site_corr_long$Var2[site_corr_long$Var2=="PostRainScorch"] = 'PRGW'
 
year_corr_long <- wide_data %>%
  group_by(Year) %>%
  summarise(
    cor_long = list(cor_to_long_with_p(cur_data(), wind_types)),
    .groups = "drop"
  ) %>%
  unnest(cor_long)

year_corr_long$Var1[year_corr_long$Var1=='HighTempLowHumidity'] = 'HTLH'
year_corr_long$Var1[year_corr_long$Var1=="DryWind"] = 'DTWD'
year_corr_long$Var1[year_corr_long$Var1=="PostRainScorch"] = 'PRGW'

year_corr_long$Var2[year_corr_long$Var2=='HighTempLowHumidity'] = 'HTLH'
year_corr_long$Var2[year_corr_long$Var2=="DryWind"] = 'DTWD'
year_corr_long$Var2[year_corr_long$Var2=="PostRainScorch"] = 'PRGW'
 
site_corr_long$group = paste0(site_corr_long$Var1, ' VS ', site_corr_long$Var2)




site_corr_long_sf = site_corr_long   %>% left_join(help_data_sf[,c(4:6,53)], by = c('site')) 
site_corr_long_sf <- st_as_sf(site_corr_long_sf)




HDW_site_corr_long_r_gg <- ggplot() +
  geom_sf(data = site_corr_long_sf, aes(fill = correlation),color = 'gray75',linewidth = 0.01) +
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(11, "BrBG"),
                    breaks = seq(-1,1,0.2),
                    limits = c(-1,1),na.value = "gray75",
                    values = rescale(seq(-1,1,0.2)),
                    name = 'r') +
  labs(subtitle = bquote('(a) R value of partial correlation analysis')) +
  ylim(4000000, 6300000) +facet_grid(~group) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65", linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50", linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(
    plot.subtitle = element_text(size = 7, hjust = 0.02, vjust = 1.5),
    strip.text = element_text(size = 6),
    legend.key.size = unit(1.5, 'cm'),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = c(0.68, 1.35),
    legend.direction = "horizontal",
    legend.key.height = unit(0.15, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 4.5),
    legend.title = element_text(size = 5.5),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent", colour = NA_character_))

HDW_site_corr_long_p_gg <- ggplot() +
  geom_sf(data = site_corr_long_sf, aes(fill = p_value),color = 'gray75',linewidth = 0.01) +
  scale_fill_stepsn(colors = rev(RColorBrewer::brewer.pal(11, "BrBG")[6:11]),
                    breaks = c(0,0.001,0.005,seq(0.1,1,0.1)),
                    limits = c(0,1),na.value = "gray75",
                    values = rescale(c(0,0.001,0.005,seq(0.1,1,0.1))),
                    name = 'p') +
  labs(subtitle = bquote('(b) P value of partial correlation analysis')) +
  ylim(4000000, 6300000) +facet_grid(~group) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65", linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50", linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(
    plot.subtitle = element_text(size = 7, hjust = 0.02, vjust = 1.5),
    strip.text = element_text(size = 6),
    legend.key.size = unit(1.5, 'cm'),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = c(0.68, 1.35),
    legend.direction = "horizontal",
    legend.key.height = unit(0.15, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 4.5),
    legend.title = element_text(size = 5.5),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent", colour = NA_character_))


year_corr_long$group = paste0(year_corr_long$Var1, ' VS ', year_corr_long$Var2)

 
year_corr_long <- year_corr_long %>%
  mutate(sig = ifelse(p_value < 0.05, "significant", "not_significant"))

 
HDW_year_corr_long_rp_gg <- ggplot(year_corr_long, aes(x = Year, y = correlation)) +
  geom_line(aes(color = group), size = 0.5) + 
  geom_point(aes(fill = sig), shape = 21, size = 1, color = "black") +  
  scale_fill_manual(name='',values = c("significant" = "black", "not_significant" = "white")) +
  scale_color_manual(name = '',values = RColorBrewer::brewer.pal(8, "Set2"))+
  labs(y = "Correlation", x = "Year", fill = "Significance") +
  labs(subtitle = bquote('(c)')) +theme_bw()+
  theme(
    plot.subtitle = element_text(size = 7, hjust = 0.02, vjust = 1.5),
    strip.text = element_text(size = 6),
    legend.key.size = unit(0.3, 'cm'),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    legend.position = 'right',
    #legend.direction = "horizontal",
    legend.key.height = unit(0.01, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 6),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent", colour = NA_character_))


library("cowplot")
emf('.../Figure S17.emf',
     units = "cm", width=16,height=12, res =1500, compression = 'lzw',pointsize = 11)

ggdraw() +
  draw_plot(HDW_site_corr_long_r_gg, x = 0, y = 0.61,  width =1, height = 0.43) +
  draw_plot(HDW_site_corr_long_p_gg, x = 0, y = 0.27,  width =1, height = 0.43)+
  draw_plot(HDW_year_corr_long_rp_gg, x = 0, y = -0.01, width =1, height = 0.35)
dev.off()

############################ 2.4 Attribution of trend of HDW  in wheat growing seasons ############################
############################ 2.4 Attribution of trend of HDW  in wheat growing seasons ############################
############################ 2.4 Attribution of trend of HDW  in wheat growing seasons ############################

library(dplyr)
library(broom)
HDMT_climate_hours <- read_csv("HDW TS Change in wheat growing seasons/HDMT_climate_hours.csv")

climate_hours_trend_analysis <- HDMT_climate_hours %>%
  group_by(site) %>%
  summarise(
    Trend_W10_gt_3 = tidy(lm(count_W10_gt_3 ~ Year)),
    Trend_T2_gt_30 = tidy(lm(count_T2_gt_30 ~ Year)),
    Trend_RH_lt_30 = tidy(lm(count_RH_lt_30 ~ Year))
  ) %>%
  mutate(

    trend_W10_gt_3 = Trend_W10_gt_3$estimate[2],
    trend_T2_gt_30 = Trend_T2_gt_30$estimate[2],
    trend_RH_lt_30 = Trend_RH_lt_30$estimate[2],
    p_W10_gt_3 = Trend_W10_gt_3$p.value[2],  
    p_T2_gt_30 = Trend_T2_gt_30$p.value[2],
    p_RH_lt_30 = Trend_RH_lt_30$p.value[2],
    sig_W10_gt_3 = ifelse(p_W10_gt_3 <= 0.05, T, F),
    sig_T2_gt_30 = ifelse(p_T2_gt_30 <= 0.05, T, F),
    sig_RH_lt_30 = ifelse(p_RH_lt_30 <= 0.05, T, F)
  ) %>%

  filter(Trend_W10_gt_3$term == "Year" & 
         Trend_T2_gt_30$term == "Year" & 
         Trend_RH_lt_30$term == "Year")  %>%
  select(site, trend_W10_gt_3,trend_T2_gt_30,trend_RH_lt_30,sig_W10_gt_3, sig_T2_gt_30, sig_RH_lt_30)

climate_hours_trend_long <- climate_hours_trend_analysis %>%
  pivot_longer(
    cols = c(trend_W10_gt_3,trend_T2_gt_30,trend_RH_lt_30,
             sig_W10_gt_3, sig_T2_gt_30, sig_RH_lt_30),
    names_to = c(".value", "Variable"),  # .value 表示前缀 (p_ / sig_)
    names_pattern = "(trend|sig)_(W10_gt_3|T2_gt_30|RH_lt_30)"
  ) %>%
  mutate(
    Variable = recode(Variable,
                      "W10_gt_3" = "Wind",
                      "T2_gt_30" = "Temperature",
                      "RH_lt_30" = "Relative humidity"
    )
  )



climate_hours_trend_sf = climate_hours_trend_long   %>% left_join(help_data_sf[,c(4:6,53)], by = c('site')) 
climate_hours_trend_sf <- st_as_sf(climate_hours_trend_sf)

HDMT_climate_avg <- HDMT_climate_hours %>%
  group_by(site, Period) %>%
  summarise(
    Wind = mean(count_W10_gt_3, na.rm = TRUE),
    Temperature = mean(count_T2_gt_30, na.rm = TRUE),
    `Relative humidity` = mean(count_RH_lt_30, na.rm = TRUE),
    n_years = n()
  ) %>%
  ungroup()

HDMT_climate_avg_long <- HDMT_climate_avg %>%
  pivot_longer(
    cols = c(Wind, Temperature, `Relative humidity`),
    names_to = "Variable",
    values_to = "Value"
  )


climate_hours_avg_sf = HDMT_climate_avg_long    %>% left_join(help_data_sf[,c(4:6,53)], by = c('site')) 
climate_hours_avg_sf <- st_as_sf(climate_hours_avg_sf)

climate_hours_trend_r_gg <- ggplot() +
  geom_sf(data = climate_hours_trend_sf %>% filter(!sig),
          fill = 'gray', color = 'white',linewidth =0.01) +
    ggnewscale::new_scale_fill() +
  geom_sf(data = climate_hours_trend_sf%>% filter(sig), aes(fill = trend),
          color = 'white',linewidth =0.01) +
    scale_fill_stepsn(
    colors = rev(RColorBrewer::brewer.pal(11, "PRGn")),
    breaks = c(-10, -5, -2, -1, 0, 1, 2, 5, 10),
    labels = c("-10", "-5", "-2", "-1", "0", "1", "2", "5", "10"),
    limits = c(-10, 10),na.value = "white",
    name = bquote("Significant "(hours~season^-1)),
    guide = guide_colorbar(title.position = "top",
                           barwidth = unit(5.5, "cm"),
                           barheight = unit(0.1, "cm"),
                           label.position = "bottom")) +
  labs(subtitle = bquote((b)),x = NULL, y = NULL ) +
  ylim(4000000, 6300000) +facet_grid(Variable~.) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65",alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50",alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.02, vjust = 1.5),
        strip.text = element_text(size = 7),
        legend.key.size = unit(1.0, 'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.62, 1.1),
        legend.direction = "horizontal",
        legend.key.height = unit(0.05, 'cm'),
        legend.box = "horizontal" ,
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),  
        legend.spacing.x = unit(0.01, "cm"),   
        legend.spacing.y = unit(0.005, "cm"),   
        legend.background = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        strip.background = element_rect(color = 'transparent'),        
        plot.background = element_rect(fill = "transparent", colour = NA_character_))



climate_hours_av_gg <- ggplot() +
  geom_sf(data = climate_hours_avg_sf,aes(fill = Value ), linewidth = 0.1, color = 'white') +
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(9, "RdPu"),
                    breaks =  c(seq(0,200,50),300,400,500,600),
                    labels =  c(seq(0,200,50),300,400,500,600),
                    limits = c(0,650),na.value = "white",
                    values = scales::rescale(c(seq(0,200,50),300,400,500,600)),
                    name = bquote( hours~season^-1 ))+
  labs(subtitle = bquote((a)),x = NULL, y = NULL ) +
  ylim(4000000, 6300000) +facet_grid(Variable~.) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65",alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50",alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.02, vjust = 1.5),
        strip.text = element_text(size = 7),
        legend.key.size = unit(0.8, 'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.55, 1.05),
        legend.direction = "horizontal",
        legend.key.height = unit(0.13, 'cm'),
        legend.box = "horizontal", 
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 6),
        strip.background = element_rect(color = 'transparent'),        
        plot.background = element_rect(fill = "transparent", colour = NA_character_))


emf('.../Figure/Figure S22.emf',
    units = "cm", width=14.6,height=11.5, pointsize = 11)

ggdraw() +
  draw_plot(climate_hours_av_gg, x = 0, y = -0.04,  width =0.5, height = 1) +
  draw_plot(climate_hours_trend_r_gg, x = 0.48, y = -0.04,  width =0.5, height = 1) 
dev.off()
