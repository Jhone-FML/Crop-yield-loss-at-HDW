library(readxl)
library(raster)
library(lubridate)
library(sp)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(fixest)
library(cowplot)
library(tsibble)
library(feasts)
library(stats)
library(utils)
library(dplR)
library(ggpubr)
library(distributional)
library(ggdist)
library(ggpattern)
library(data.table)
library(ggpmisc)
library(ggh4x)
library(dplyr)
library(lubridate)
library(broom)
library(devEMF)
setwd('E:/Crop yield loss at DHW/')

library(readxl)
sitstation <- read_excel("..../sitstation.xls") 
colnames(sitstation) = sitstation[2,]  
sitstation = sitstation[-c(1,2),]  
colnames(sitstation)[c(2,9,10)]  = c('site','lat', 'lon')

China_map      = st_read('.../National county-level statistics.shp')
China_line     = st_read('.../China_line.shp')
Provience_line = st_read('.../Provience_line.shp')
China_sea      = st_read('.../China_sea.shp')
Chian_frame    = st_read('.../Chian_frame.shp')
help_data      = readxl::read_xls('.../help_data.xls')




########################################  1.2 Hourly Temperature Adjustment Test
comnames = intersect(substr(unique(colnames(ERA5L_T2m))[1:900],1,6),
                     substr(list.files('..../site_91_250622/'),1,5))
CMA_w10_hourly = NULL
CMA_T2m_hourly = NULL
comnames_lonlat = NULL
for (k in 1:length(comnames)) {
  
  temdata = fread(paste0('..../site_91_250622/',comnames[k],'.csv'))
  colnames(temdata)[c(1,2)] = c("date","site")
  temp_w10_hourly = temdata[,c(1,2,14)]
  temp_T2m_hourly = temdata[,c(1,2,12)]
  colnames(temp_w10_hourly)[3] = "value"
  colnames(temp_T2m_hourly)[3] = "value"
  temp_w10_hourly$value[temp_w10_hourly$value>10000]= NA
  temp_T2m_hourly$value[temp_T2m_hourly$value>10000]= NA                    
  temp_w10_hourly = na.omit(temp_w10_hourly)
  temp_T2m_hourly = na.omit(temp_T2m_hourly)
  
  CMA_w10_hourly = rbind(CMA_w10_hourly,temp_w10_hourly)
  CMA_T2m_hourly = rbind(CMA_T2m_hourly,temp_T2m_hourly)
  temp_lonlat = temdata[1,7:8]
  comnames_lonlat = rbind(comnames_lonlat, temp_lonlat)
}
CMA_w10_hourly$site = as.character(CMA_w10_hourly$site)
CMA_T2m_hourly$site = as.character(CMA_T2m_hourly$site)

ERA5L_T2m_hourly = ERA5L_T2m[, ..comnames]

start_time = as.POSIXct("1981-01-01 00:00:00", tz = "UTC")
end_time   = start_time + (length(ERA5L_T2m$date) - 1) * 3600  

ERA5L_T2m_hourly$date = seq(from = start_time, by = "hour", length.out = length(ERA5L_T2m$date))

ERA5L_T2m_hourly$date <- with_tz(ERA5L_T2m_hourly$date,tzone = "Asia/Shanghai")
CMC_T2m_hourly$date   <-  with_tz(CMC_T2m_hourly$date,tzone = "Asia/Shanghai")

ERA5L_T2m_hourly = melt(setDT(ERA5L_T2m_hourly), id.vars = c("date"), variable.name = "site")

ERA5L_T2m_hourly_mer = CMA_T2m_hourly %>% right_join(ERA5L_T2m_hourly, by=c('date','site'))
ERA5L_T2m_hourly_mer = na.omit(ERA5L_T2m_hourly_mer)

ERA5L_T2m_hourly_mer$Group = 'ERA5L'



T2m_hourly_gg <- ggplot(ERA5L_T2m_hourly_mer, aes(value.x, value.y)) +
  geom_bin2d(bins = 100) +
  scale_fill_gradientn(colors = viridis::viridis(10), trans = "log10",
                       breaks = 10^(0:5), labels = scales::comma) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = 'gray65') +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., ..p.value.label.., sep = "~~~~")),
               formula = y~x, parse = TRUE, size = 2.5, label.x = 0.01, label.y = c(0.96, 0.99)) +
  annotate("text",  x = -Inf, y = Inf, label = "", 
           hjust = -0.5, vjust = 0.2, size = 5, fontface = "bold") +
  labs(x = "Hourly temperature of CMA (°C)", 
       y = "Hourly temperature of ERA5-Land (°C)",
       fill = "Count") #+


emf('.../Figures/Figure S4.emf',
     units = "cm", width=12,height=8, pointsize = 11)
T2m_hourly_gg
dev.off()


# ERA5L_T2m_hourly_mer

ERA5L_T2m_trend_df = ERA5L_T2m_hourly_mer %>%
  mutate(
    datetime = as.POSIXct(date),
    date = as.Date(datetime),
    hour = hour(datetime),
    month = month(date),
    day = day(date),
    year = year(date),
    site = as.character(site)
  ) %>% filter(month %in% 3:6 & hour %in% 8:18)  



# 2. 多尺度趋势分析函数
calculate_trends <- function(data, group_vars, time_var, value_var) {
  # data = ERA5L_RH_MAJJ_hourly_trend_df
  data = as.data.frame(data)
  data%>%
    group_by(across(all_of(c(group_vars, time_var)))) %>%
    summarise(
      mean_value = mean(!!sym(value_var), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(c("site", time_var)))) %>%
    filter(n() >= 3) %>%  # 至少3个时间点
    do({
      mod <- lm(mean_value ~ year, data = .)
      tidy(mod) %>% 
        filter(term != "(Intercept)") %>%
        select(estimate, p.value)
    }) %>%
    rename(trend_slope = estimate, trend_p = p.value)
}

# 3. 各尺度趋势计算
## 小时尺度 (8-18时逐小时趋势)
CMA_T2m_MAJJ_hourly_trend <- ERA5L_T2m_trend_df %>%
  calculate_trends(
    group_vars = c("site", 'year'), 
    time_var = "hour", 
    value_var = 'value.x'
  ) %>%
  mutate(time_scale = "hourly_8to18",
         Group = 'CMA')


CMA_T2m_MAJJ_monthly_trend <- ERA5L_T2m_trend_df %>%
  calculate_trends(
    group_vars = c("site", 'year'), 
    time_var = "month", 
    value_var = 'value.x'
  ) %>%
  mutate(time_scale = "month_3to6",
         month = case_when(
           month == 3 ~ "March",
           month == 4 ~ "April", 
           month == 5 ~ "May",
           month == 6 ~ "June",
           TRUE ~ as.character(month)  # 保留其他月份的数字表示
         ),
         Group = 'CMA')


ERA5L_T2m_MAJJ_hourly_trend <- ERA5L_T2m_trend_df %>%
  calculate_trends(
    group_vars = c("site", 'year'), 
    time_var = "hour", 
    value_var = 'value.y'
  ) %>%
  mutate(time_scale = "hourly_8to18",
         Group = 'ERA5L')


ERA5L_T2m_MAJJ_monthly_trend <- ERA5L_T2m_trend_df %>%
  calculate_trends(
    group_vars = c("site", 'year'), 
    time_var = "month", 
    value_var = 'value.y'
  ) %>%
  mutate(time_scale = "month_3to6",
         month = case_when(
           month == 3 ~ "March",
           month == 4 ~ "April", 
           month == 5 ~ "May",
           month == 6 ~ "June",
           TRUE ~ as.character(month)  # 保留其他月份的数字表示
         ),
         Group = 'ERA5L')

T2m_MAJJ_hourly_trend = rbind(CMA_T2m_MAJJ_hourly_trend,ERA5L_T2m_MAJJ_hourly_trend)
T2m_MAJJ_monthly_trend = rbind(CMA_T2m_MAJJ_monthly_trend,ERA5L_T2m_MAJJ_monthly_trend)


T2m_MAJJ_hourly_trend_results = T2m_MAJJ_hourly_trend %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
T2m_MAJJ_hourly_sp = T2m_MAJJ_hourly_trend_results
coordinates(T2m_MAJJ_hourly_sp) = ~lon+lat
proj4string(T2m_MAJJ_hourly_sp) <- CRS("+init=epsg:4480")
T2m_MAJJ_hourly_sf  =  st_as_sf(T2m_MAJJ_hourly_sp,coords = 1:2)

T2m_MAJJ_monthly_trend_results = T2m_MAJJ_monthly_trend %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
T2m_MAJJ_monthly_sp = T2m_MAJJ_monthly_trend_results
coordinates(T2m_MAJJ_monthly_sp) = ~lon+lat
proj4string(T2m_MAJJ_monthly_sp) <- CRS("+init=epsg:4480")
T2m_MAJJ_monthly_sf  =  st_as_sf(T2m_MAJJ_monthly_sp,coords = 1:2)

slope_range <- quantile(T2m_MAJJ_hourly_sf$trend_slope, probs = c(0.02, 0.98), na.rm = TRUE)
slope_breaks <- seq(round(slope_range[1], 2), round(slope_range[2], 2), length.out = 10)

T2m_MAJJ_hourly_sf$group = paste0(T2m_MAJJ_hourly_sf$hour,': ',T2m_MAJJ_hourly_sf$Group)
# 2. 主绘图代码



target_crs <- 4547
China_line_proj <- st_transform(China_line, crs = target_crs)
China_sea_proj <- st_transform(China_sea, crs = target_crs)
Provience_line_proj <- st_transform(Provience_line, crs = target_crs)
Chian_frame_proj <- st_transform(Chian_frame, crs = target_crs)

T2m_MAJJ_hourly_sf_proj <- st_transform(T2m_MAJJ_hourly_sf, crs = target_crs)

T2m_MAJJ_hourly_sf_proj$facet_lab = paste0(T2m_MAJJ_hourly_sf_proj$Group," : Hour ", T2m_MAJJ_hourly_sf_proj$hour) 
T2m_MAJJ_hourly_sf_proj$facet_lab  = factor(T2m_MAJJ_hourly_sf_proj$facet_lab,
                                            levels = c("CMA : Hour 8" ,   "CMA : Hour 9",    "CMA : Hour 10", "CMA : Hour 11",
                                                       "ERA5L : Hour 8",  "ERA5L : Hour 9",  "ERA5L : Hour 10", "ERA5L : Hour 11",
                                                       "CMA : Hour 12",   "CMA : Hour 13",   "CMA : Hour 14",  "CMA : Hour 15", 
                                                       "ERA5L : Hour 12", "ERA5L : Hour 13", "ERA5L : Hour 14", "ERA5L : Hour 15", 
                                                       "CMA : Hour 16", "CMA : Hour 17",  "CMA : Hour 18",
                                                       "ERA5L : Hour 16", "ERA5L : Hour 17","ERA5L : Hour 18"))

T2m_hourly_trend_gg_8_18 <- ggplot() +
  geom_sf(data = T2m_MAJJ_hourly_sf_proj[T2m_MAJJ_hourly_sf_proj$hour %in% c(8:18),],
          aes(color = trend_slope),
          size = 0.3,linewidth = 0.3,key_glyph = "rect") +
  scale_color_gradientn(
    colors = rev(brewer.pal(11, "RdYlBu")),  # 增强颜色对比度
    values = scales::rescale(c(slope_range[1], 0, slope_range[2])),  # 确保0值居中
    limits = c(slope_range[1], slope_range[2]),
    breaks = slope_breaks,
    labels = function(x) sprintf("%.2f", x),  # 固定4位小数
    name = bquote("Temperature trend (°C"~" year"^-1~")"),
    guide = guide_colorbar(
      title.position = "top",    # 标题在上方
      title.hjust = 0.5,         # 标题水平居中
      barwidth = unit(8, "cm"),  # 调整图例条宽度
      barheight = unit(0.2, "cm") # 调整图例条高度
    )) +
  facet_wrap(~facet_lab,ncol = 4)+ylim(3500000, 5950000) +xlim(-2900000, 1900000)  +theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  -9.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 7),
        legend.key.size = unit(30,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.78, 0.1),
        legend.direction = "horizontal",#c(0.95,0.70),
        panel.spacing = unit(0.03, units = "cm"), # removes space between panels
        strip.background = element_rect(color = 'transparent'),
        legend.key.height = unit(0.5,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 7.0),
        legend.title= element_text(size = 8.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))






T2m_MAJJ_monthly_sf_proj =  st_transform(T2m_MAJJ_monthly_sf, crs = target_crs)


T2m_MAJJ_monthly_sf_proj$month = factor(T2m_MAJJ_monthly_sf_proj$month, levels = c("March","April","May", "June"))

T2m_monthly_trend_gg <- ggplot() +
  geom_sf(data = T2m_MAJJ_monthly_sf_proj ,aes(color = trend_slope),
          size = 0.5,linewidth = 0.3,key_glyph = "rect") +
  scale_color_gradientn(
    colors = rev(brewer.pal(11, "RdYlBu")),  # 增强颜色对比度
    values = scales::rescale(c(slope_range[1], 0, slope_range[2])),  # 确保0值居中
    limits = c(slope_range[1], slope_range[2]),
    breaks = slope_breaks,
    labels = function(x) sprintf("%.2f", x),  # 固定4位小数
    name = bquote("Temperature trend (°C"~" year"^-1~")")) +
  ylim(3500000, 5950000) +xlim(-2900000, 1900000) +theme_bw()+
  facet_grid(Group~month)+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  -9.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 7),
        legend.key.size = unit(1.5,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'bottom',#c(0.6, 1.2),
        #legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 7.0),
        legend.title= element_text(size = 8.0),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))#+




########################################## 2 Daily Relative Humidity Test
########################################## 2 Daily Relative Humidity Test

ERA5L_RH  = fread('DHW/ERA5L_RH.csv')
ERA5L_RH_MAJJ  = ERA5L_RH [month(as.Date(ERA5L_RH $date)) %in% c(3:6),]
ERA5L_RH_MAJJ_mean  =  ERA5L_RH_MAJJ[,-1] %>% group_by(date) %>% summarise_at(vars(colnames(ERA5L_RH_MAJJ[,-1])[-900]),list(mean));
ERA5L_RH_MAJJ_mean_long = melt(setDT(ERA5L_RH_MAJJ_mean), id.vars = c("date"), variable.name = "site")
ERA5L_RH_MAJJ_mean_long$date = as.character(ERA5L_RH_MAJJ_mean_long$date)
ERA5L_RH_MAJJ_mean_long$site = as.character(ERA5L_RH_MAJJ_mean_long$site)

RH_mer       = CMA_RH_Mean %>% right_join(ERA5L_RH_MAJJ_mean_long, by=c('date','site'))
RH_mer$value.x = as.numeric(RH_mer$value.x)
RH_mer$group = 'ERA5L'

CMFD_RH_3hour_fun <- function(i) {
  library(readxl)
  library(raster)
  library(lubridate)
  library(sp)
  library(tidyverse)
  library(tidyr)
  detach("package:tidyr", unload = TRUE)
  uni_sites = substr(unique(RH_mer$site),1,6)
  stations  = read_xls('.../Data/sitstation.xls')
  colnames(stations) = stations[2,]
  stations  = stations[-1,]
  colnames(stations)[c(2,9,10)] = c('site', 'lat','lon')
  matched_stations = subset(stations, site %in% uni_sites, select = c(site, lat, lon))
  # id = expand.grid(i = 1:492, k = 1:899)
  
  CMFD_RH_files = list.files('.../Data/CMFD2.0/RHUM', full.names = TRUE)
  
  # matched_stations_sp <- matched_stations#[id$k[j],]
  # coordinates(matched_stations_sp) <- ~lon+lat
  # proj4string(matched_stations_sp) <- CRS("+init=epsg:4326")
  
  temp_rh <- brick(CMFD_RH_files[i], varname = 'rhum')
  # target_crs <- crs(temp_rh)
  # matched_stations_sp_transformed <- spTransform(matched_stations_sp, target_crs)
  
  # temp_rh_extr <- extract(temp_rh, matched_stations_sp_transformed)
  library(ncdf4)
  nc_file = nc_open(CMFD_RH_files[i])
  # 获取经纬度和温度数据
  lat = ncvar_get(nc_file, "lat")
  lon = ncvar_get(nc_file, "lon")
  relative_humidity = ncvar_get(nc_file, "rhum")
  
  # 假设你有一个站点经纬度坐标 lat1 和 lon1
  # 提取每个站点的数据
  temp_rh_extr = sapply(1:nrow(matched_stations), function(i) {
    lat1 <- matched_stations$lat[i]
    lon1 <- matched_stations$lon[i]
    
    # 找到最近的经纬度索引
    lat_idx <- which.min(abs(lat - lat1))
    lon_idx <- which.min(abs(lon - lon1))
    
    # 提取该站点的气温数据
    rhum <- relative_humidity[ lon_idx, lat_idx,]
    return(rhum)
  })
  temp_rh_extr = data.frame(temp_rh_extr)
  # 输出所有站点的数据
  nc_close(nc_file)
  colnames(temp_rh_extr) = matched_stations$site
  temp_rh_extr$date = temp_rh@z$`Date/time`
  
  
  return(temp_rh_extr)
}

library(foreach)
library(doParallel)

n.cores <- 20
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(cl = my.cluster)

# 方案1：为每个站点处理所有文件
system.time(
  CMFD_RH_3hour <- foreach(
    i = 1:480,#nrow(id),
    .combine = 'rbind',
    .packages = c("raster", "sp", "tidyverse")
  ) %dopar% {
    temp = CMFD_RH_3hour_fun(i)
    return(temp)
  }
)

parallel::stopCluster(cl = my.cluster)


write_csv2(CMFD_RH_3hour,'DHW/CMFD_RH_3hour.csv')

CMFD_RH_3hour$ymd = substr(CMFD_RH_3hour$date,1,10)

CMFD_RH_3hour$date <- ymd_hms(
  ifelse(nchar(CMFD_RH_3hour$date) == 10,  # 判断是否只有日期（如"1980-01-01"）
    paste(CMFD_RH_3hour$date, "00:00:00"),  # 补全日期的缺失时间
    CMFD_RH_3hour$date  # 已有时间的保持不变
  )
)

CMFD_RH_daily       = CMFD_RH_3hour %>%  group_by(ymd) %>% summarise_at(vars(colnames(CMFD_RH_3hour)[-c(900:901)]),list(mean));
colnames(CMFD_RH_daily)[1] = 'date'

CMFD_RH_daily_MAJJ  = CMFD_RH_daily[month(as.Date(CMFD_RH_daily$date)) %in% c(3:6),]

CMFD_RH_daily_MAJJ_long = melt(setDT(CMFD_RH_daily_MAJJ), id.vars = c("date"), variable.name = "site")


CMA_RH_Mean$site = substr(CMA_RH_Mean$site,1,6)

CMFD_RH_daily_MAJJ_mer       = CMA_RH_Mean %>% right_join(CMFD_RH_daily_MAJJ_long, by=c('date','site'))

CMFD_RH_daily_MAJJ_mer$group = 'CMFD'

RH_dialy_mer = rbind(RH_mer, CMFD_RH_daily_MAJJ_mer)
RH_dialy_mer$value.x = as.numeric(RH_dialy_mer$value.x)
RH_dialy_mer$value.y = as.numeric(RH_dialy_mer$value.y)

 

   
RH_dialy_gg <- ggplot(RH_dialy_mer, aes(value.x, value.y)) +
  geom_hex(bins = 50,alpha = 0.8) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "BuGn"),  # 改用PRGn色阶
    trans = "log10",
    breaks = c(1, 10, 100, 1000),    # 对数刻度断点
    labels = scales::comma,           # 千分位格式化
    name = "Data Density\n(log10 count)") +
  facet_wrap(~group, ncol = 2) +  # 按 group 分面
  geom_smooth(method = "lm", se = FALSE, color = "gray65") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., ..p.value.label.., sep = "~~~~")),
               formula = y~x, parse = TRUE, size = 2.0, label.x = 0.01, label.y = c(0.93, 0.93)) +
  labs( x = "Relative humidity of CMA (%)", y = "Relative humidity of reanalysis dataset (%)",
    fill = "Count")  +
  # annotate("text",  x = -Inf, y = Inf, label = c("(b)",'(c)'), 
  #          hjust = -0.5, vjust = 2, size = 5, fontface = "bold") 
  geom_text(data = data.frame(group = unique(RH_dialy_mer$group),label = c("(b)", "(a)")),
      aes(x = -Inf, y = Inf, label = label),hjust = -0.2,vjust = 1.5,size = 2.5, fontface = "bold") +
  theme(strip.text = element_text(size = 7),
        legend.key.size = unit(0.1,'cm'),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.position = 'right',
        # legend.justification = c("right", "top"),
        # legend.box.just = "right",
        legend.key.height = unit(1,'cm'),
        legend.key.width = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 6),
        legend.title= element_text(size = 7),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


ERA5L_RH_MAJJ_hourly = ERA5L_RH_MAJJ


hours <- rep(0:23, length.out = length(ERA5L_RH_MAJJ_hourly$date))


ERA5L_RH_MAJJ_hourly$datetime <- as.POSIXct(
  paste(ERA5L_RH_MAJJ_hourly$date, sprintf("%02d:00:00", hours)),
  format = "%Y-%m-%d %H:%M:%S"
)

ERA5L_RH_MAJJ_hourly_long = melt(setDT(ERA5L_RH_MAJJ_hourly[,-c(1,901)]), id.vars = c("datetime"), variable.name = "site")


library(dplyr)
library(lubridate)
library(broom)

ERA5L_RH_MAJJ_hourly_trend_df = ERA5L_RH_MAJJ_hourly_long %>%
  mutate(
    # datetime = as.POSIXct(date),
    date = as.Date(datetime),
    hour = hour(datetime),
    month = month(date),
    day = day(date),
    year = year(date),
    site = as.character(site)
  ) %>% filter(month %in% 3:6 & hour %in% 8:18)  



# 2. 多尺度趋势分析函数
calculate_trends <- function(data, group_vars, time_var, value_var) {
  # data = ERA5L_RH_MAJJ_hourly_trend_df
  data = as.data.frame(data)
  data%>%
    group_by(across(all_of(c(group_vars, time_var)))) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(c("site", time_var)))) %>%
    filter(n() >= 3) %>%  # 至少3个时间点
    do({
      mod <- lm(mean_value ~ year, data = .)
      tidy(mod) %>% 
        filter(term != "(Intercept)") %>%
        select(estimate, p.value)
    }) %>%
    rename(trend_slope = estimate, trend_p = p.value)
}

# 3. 各尺度趋势计算
## 小时尺度 (8-18时逐小时趋势)
ERA5L_RH_MAJJ_hourly_trend <- ERA5L_RH_MAJJ_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", 'year'), 
    time_var = "hour", 
    value_var = 'value'
  ) %>%
  mutate(time_scale = "hourly_8to18")


ERA5L_RH_MAJJ_monthly_trend <- ERA5L_RH_MAJJ_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", 'year'), 
    time_var = "month", 
    value_var = 'value'
  ) %>%
  mutate(time_scale = "month_3to6")

ERA5L_RH_MAJJ_monthly_trend <- ERA5L_RH_MAJJ_monthly_trend %>%
  mutate(month = case_when(
    month == 3 ~ "March",
    month == 4 ~ "April", 
    month == 5 ~ "May",
    month == 6 ~ "June",
    TRUE ~ as.character(month)  # 保留其他月份的数字表示
  )) 
  



emf('.../Figures/Figure S6.emf',
     units = "cm", width=16,height=8, pointsize = 11)
RH_dialy_gg
dev.off()

########################################## 2.2 Daily Relative Humidity Interpolating hourly
setDT(CMFD_RH_3hour)

dt_long <- melt(CMFD_RH_3hour, id.vars = c("ymd", "date"), variable.name = "site", value.name = "RH")

# 创建目标的逐小时时间序列
hour_seq <- seq(min(dt_long$date), max(dt_long$date), by = "1 hour")
sites <- unique(dt_long$site)

# 插值函数
interp_hourly <- function(df_site) {
  # 仅保留非NA的点
  df_valid <- df_site[!is.na(RH)]
  
  # 插值（采用样条插值）
  rh_interp <- spline(x = as.numeric(df_valid$date), y = df_valid$RH, xout = as.numeric(hour_seq), method = "natural")$y
  
  data.table(
    datetime = hour_seq,
    site = unique(df_site$site),
    RH = pmin(pmax(rh_interp, 0), 100)  # 限制在0-100%
  )
}

# 对每个站点进行插值
RH_hourly <- rbindlist(lapply(sites, function(s) {
  interp_hourly(dt_long[site == s])
}))

CMFD_RH_hourly_spline <- dcast(
  data = RH_hourly,
  formula = datetime ~ site,
  value.var = "RH"
)

colnames(RH_hourly_spline)[1] = 'date'

write.csv(RH_hourly_spline,'DHW/CMFD_RH_hourly_spline.csv')

CMFD_RH_MAJJ_hourly  = RH_hourly_spline[month(as.Date(RH_hourly_spline$date)) %in% c(3:6),]
CMFD_RH_MAJJ_hourly_long = melt(setDT(CMFD_RH_MAJJ_hourly), id.vars = c("date"), variable.name = "site")

CMFD_RH_MAJJ_hourly_trend_df = CMFD_RH_MAJJ_hourly_long %>%
  mutate(
    datetime = as.POSIXct(date),
    date = as.Date(datetime),
    hour = hour(datetime),
    month = month(datetime),
    day = day(datetime),
    year = year(datetime),
    site = as.character(site)
  ) %>% filter(month %in% 3:6 & hour %in% 8:18)  



# 2. 多尺度趋势分析函数
calculate_trends <- function(data, group_vars, time_var, value_var) {
  # data = ERA5L_RH_MAJJ_hourly_trend_df
  data = as.data.frame(data)
  data%>%
    group_by(across(all_of(c(group_vars, time_var)))) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(c("site", time_var)))) %>%
    filter(n() >= 3) %>%  # 至少3个时间点
    do({
      mod <- lm(mean_value ~ year, data = .)
      tidy(mod) %>% 
        filter(term != "(Intercept)") %>%
        select(estimate, p.value)
    }) %>%
    rename(trend_slope = estimate, trend_p = p.value)
}

# 3. 各尺度趋势计算
## 小时尺度 (8-18时逐小时趋势)
CMFD_RH_MAJJ_hourly_trend <- CMFD_RH_MAJJ_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", 'year'), 
    time_var = "hour", 
    value_var = 'value'
  ) %>%
  mutate(time_scale = "hourly_8to18")


CMFD_RH_MAJJ_monthly_trend <- CMFD_RH_MAJJ_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", 'year'), 
    time_var = "month", 
    value_var = 'value'
  ) %>%
  mutate(time_scale = "month_3to6")

CMFD_RH_MAJJ_monthly_trend <- CMFD_RH_MAJJ_monthly_trend %>%
  mutate(month = case_when(
    month == 3 ~ "March",
    month == 4 ~ "April", 
    month == 5 ~ "May",
    month == 6 ~ "June",
    TRUE ~ as.character(month)  # 保留其他月份的数字表示
  )) 

CMFD_RH_MAJJ_hourly_trend$Groupp = 'CMFD'
ERA5L_RH_MAJJ_hourly_trend$Groupp = 'ERA5L'

CMFD_RH_MAJJ_monthly_trend$Groupp = 'CMFD'
ERA5L_RH_MAJJ_monthly_trend$Groupp = 'ERA5L'

CMFD_RH_MAJJ_hourly_df = rbind(CMFD_RH_MAJJ_hourly_trend,ERA5L_RH_MAJJ_hourly_trend)
CMFD_RH_MAJJ_monthly_df = rbind(CMFD_RH_MAJJ_monthly_trend,ERA5L_RH_MAJJ_monthly_trend)




CMFD_RH_MAJJ_hourly_df_results = CMFD_RH_MAJJ_hourly_df %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
CMFD_RH_MAJJ_hourly_sp = CMFD_RH_MAJJ_hourly_df_results
coordinates(CMFD_RH_MAJJ_hourly_sp) = ~lon+lat
proj4string(CMFD_RH_MAJJ_hourly_sp) <- CRS("+init=epsg:4480")
CMFD_RH_MAJJ_hourly_sf  =  st_as_sf(CMFD_RH_MAJJ_hourly_sp,coords = 1:2)


CMFD_RH_MAJJ_monthly_df_results = CMFD_RH_MAJJ_monthly_df %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
CMFD_RH_MAJJ_monthly_sp = CMFD_RH_MAJJ_monthly_df_results
coordinates(CMFD_RH_MAJJ_monthly_sp) = ~lon+lat
proj4string(CMFD_RH_MAJJ_monthly_sp) <- CRS("+init=epsg:4480")
CMFD_RH_MAJJ_monthly_sf  =  st_as_sf(CMFD_RH_MAJJ_monthly_sp,coords = 1:2)


slope_range <- quantile(CMFD_RH_MAJJ_hourly_sf$trend_slope, probs = c(0.02, 0.98), na.rm = TRUE)
slope_breaks <- seq(round(slope_range[1], 2), round(slope_range[2], 2), length.out = 10)

CMFD_RH_MAJJ_hourly_sf_proj <- st_transform(CMFD_RH_MAJJ_hourly_sf, crs = target_crs)

CMFD_RH_MAJJ_hourly_sf_proj$facet_lab = paste0(CMFD_RH_MAJJ_hourly_sf_proj$Groupp," : Hour ", CMFD_RH_MAJJ_hourly_sf_proj$hour) 
CMFD_RH_MAJJ_hourly_sf_proj$facet_lab  = factor(CMFD_RH_MAJJ_hourly_sf_proj$facet_lab,
                                            levels = c("CMFD : Hour 8" ,   "CMFD : Hour 9",    "CMFD : Hour 10", "CMFD : Hour 11", 
                                                       "ERA5L : Hour 8",  "ERA5L : Hour 9",  "ERA5L : Hour 10",  "ERA5L : Hour 11", 
                                                       "CMFD : Hour 12",   "CMFD : Hour 13",   "CMFD : Hour 14",  "CMFD : Hour 15",
                                                       "ERA5L : Hour 12", "ERA5L : Hour 13", "ERA5L : Hour 14", "ERA5L : Hour 15", 
                                                       "CMFD : Hour 16", "CMFD : Hour 17",  "CMFD : Hour 18",
                                                       "ERA5L : Hour 16", "ERA5L : Hour 17","ERA5L : Hour 18"))


RH_hourly_trend_gg_8_18 <- ggplot() +
  geom_sf(data = CMFD_RH_MAJJ_hourly_sf_proj[CMFD_RH_MAJJ_hourly_sf_proj$hour %in% c(8:18),],
          aes(color = trend_slope),
          size = 0.2,linewidth = 0.3,key_glyph = "rect") +
  scale_color_gradientn(
    colors = rev(brewer.pal(11, "BrBG")),  # 增强颜色对比度
    values = scales::rescale(c(slope_range[1], 0, slope_range[2])),  # 确保0值居中
    limits = c(slope_range[1], slope_range[2]),
    breaks = slope_breaks,
    labels = function(x) sprintf("%.2f", x),  # 固定4位小数
    name = bquote("Relative humidity trend (%"~" year"^-1~")"),
    guide = guide_colorbar(
      title.position = "top",    # 标题在上方
      title.hjust = 0.5,         # 标题水平居中
      barwidth = unit(8, "cm"),  # 调整图例条宽度
      barheight = unit(0.2, "cm") # 调整图例条高度
    )) +
  facet_wrap(~facet_lab,ncol = 4)+ylim(3500000, 5950000) +xlim(-2900000, 1900000)  +theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  -9.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 7),
        legend.key.size = unit(2,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.77, 0.1),
        legend.direction = "horizontal",#c(0.95,0.70),
        panel.spacing = unit(0.03, units = "cm"), # removes space between panels
        strip.background = element_rect(color = 'transparent'),
        legend.key.height = unit(0.8,'cm'),
        legend.key.width =  unit(4,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 7.0),
        legend.title= element_text(size = 8.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))





emf('.../Figure S7.emf',
     units = "cm", width=18,height=17, pointsize = 11)
RH_hourly_trend_gg_8_18
dev.off()


CMFD_RH_MAJJ_monthly_sf_proj <- st_transform(CMFD_RH_MAJJ_monthly_sf, crs = target_crs)

CMFD_RH_MAJJ_monthly_sf_proj$month = factor(CMFD_RH_MAJJ_monthly_sf_proj$month, levels = c("March","April","May", "June"))

slope_range <- quantile(CMFD_RH_MAJJ_monthly_sf_proj$trend_slope, probs = c(0.00, 1), na.rm = TRUE)
slope_breaks <- seq(round(slope_range[1], 2), round(slope_range[2], 2), length.out = 10)

RH_monthly_trend_gg <- ggplot() +
  geom_sf(data = CMFD_RH_MAJJ_monthly_sf_proj ,aes(color = trend_slope),
          size = 0.2,linewidth = 0.3,key_glyph = "rect") +
  scale_color_gradientn(
    colors = rev(brewer.pal(11, "BrBG")),  # 增强颜色对比度
    values = scales::rescale(c(slope_range[1], 0, slope_range[2])),  # 确保0值居中
    limits = c(slope_range[1], slope_range[2]),
    breaks = slope_breaks,
    labels = function(x) sprintf("%.2f", x),  # 固定4位小数
    name = bquote("Relative humidity trend (%"~" year"^-1~")")) +
  facet_grid(Groupp~month)+ ylim(3500000, 5950000) +xlim(-2900000, 1900000) +theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  -9.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 7),
        legend.key.size = unit(1.5,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'bottom',#c(0.6, 1.2),
        #legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 7.0),
        legend.title= element_text(size = 8.0),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))#+


emf('.../Figures/Figure S8.emf',
     units = "cm", width=23,height=9, pointsize = 11)
RH_monthly_trend_gg
dev.off()


########################################## 3 Hourly Wind Speed Test
########################################## 3 Hourly Wind Speed Test 

HWS_WTH_PTH = list.files('..../Hourly Wind Speed Data/')

Com_HWS_WTH_root = paste0('step_iteration_',comnames,'.csv')

HWS_Wind_hourly = NULL

for (k in 1:length(Com_HWS_WTH_root)) {
  
  if(HWS_WTH_root[k]%in%HWS_WTH_PTH){
    Temp_Cli = fread(paste0('.../Hourly Wind Speed Data/', 
                               Com_HWS_WTH_root[k]), header=T)
    
    colnames(Temp_Cli)[c(1,2)] = c('date', 'W10m');
    
    Temp_Cli = Temp_Cli[year(Temp_Cli$date) %in% c(1981:2018),]
    
    temp_wind_hourly = data.frame(date  = as.POSIXct(Temp_Cli$date),
                                  site  = comnames[k],
                                  value = Temp_Cli$W10m)
    
    HWS_Wind_hourly     = rbind(HWS_Wind_hourly, temp_wind_hourly)
  }
  print(k)
}

HWS_Wind_hourly$date = as.POSIXct(HWS_Wind_hourly$date, tz = "UTC")

W10_hourly_mer = CMA_w10_hourly %>% right_join(HWS_Wind_hourly, by=c('date','site'))
W10_hourly_mer = na.omit(W10_hourly_mer)


ERA5L_W10_hourly = ERA5L_W10[, ..comnames]

start_time = as.POSIXct("1981-01-01 00:00:00", tz = "UTC")
end_time   = start_time + (length(ERA5L_W10$date) - 1) * 3600  # 3600秒=1小时

# 生成完整的小时序列
ERA5L_W10_hourly$date = seq(from = start_time, by = "hour", length.out = length(ERA5L_W10$date))

ERA5L_W10_hourly = melt(setDT(ERA5L_W10_hourly), id.vars = c("date"), variable.name = "site")

era5_W10_hourly_mer = CMA_w10_hourly %>% right_join(ERA5L_W10_hourly, by=c('date','site'))
era5_W10_hourly_mer = na.omit(era5_W10_hourly_mer)

W10_hourly_mer$Group = 'RNSHWD' 
era5_W10_hourly_mer$Group = 'ERA5L'


W10_hourly = rbind(W10_hourly_mer,era5_W10_hourly_mer)


W10_hourly_gg <- ggplot(W10_hourly, aes(value.x, value.y)) +
  geom_hex(bins = 500,show.legend = TRUE) +
  scale_fill_gradientn( colors =  brewer.pal(9, "RdPu"),  # 风速适合暖色调
    trans = "log10",#breaks = c(100000,10000000,50000000),
    labels = scales::comma, name = "Data Density\n(log10 count)") +
  geom_abline(slope = 1, intercept = 0,linetype = "dashed", color = "red4",linewidth = 1)+
  geom_vhlines(xintercept = 3,yintercept = 3,linetype = "dashed", color = "gray85",linewidth = 0.6,lab = 3)+
  geom_vhlines(xintercept = 6,yintercept = 6,linetype = "dashed", color = "gray85",linewidth = 1,lab = 6)+
  geom_text(data = data.frame(x = c(3, 6), y = c(3, 6), label = c("3 m/s", "6 m/s")),
            aes(x = x, y = y, label = label), vjust = -1, size = 1.8, color = "gray25")+
  coord_cartesian(xlim = c(0, 15), ylim = c(0, 15)) +  # 改用coord_cartesian避免数据裁剪
  geom_smooth(method = "lm",se = FALSE,formula = y ~ x,color = "gray65",linewidth = 0.8) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")),  # 简化标签
    formula = y ~ x,parse = TRUE,size = 3,label.x = 0.05,label.y = 0.93,color = "black") +
   facet_wrap(~Group, ncol = 2) +  
  labs(x = bquote("CMA wind speed (m "~s^-1~")"),y = bquote("Reanalysis wind speed (m "~s^-1~")")) +
  geom_text(data = distinct(W10_hourly, Group, .keep_all = TRUE),
    aes(x = -Inf, y = Inf, label = c("(b)", "(a)")), hjust = -0.25,vjust = 1.8,size = 2.5,fontface = "bold") +
  theme(strip.text = element_text(size = 7,face = "bold", margin = margin(b = 5)),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6),
    legend.title = element_text(size = 7,vjust = 1),
    legend.text = element_text(size = 7),
    legend.position = "right",
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(0.3, "cm"),
    plot.title = element_text(size = 7,face = "bold",hjust = 0.5, margin = margin(b = 10)))



emf('.../Figures/Figure S5.emf',
     units = "cm", width=16,height=7, pointsize = 11)
W10_hourly_gg 
dev.off()

# 1. 数据预处理
W10_hourly_trend_df = W10_hourly %>%
  mutate(
    datetime = as.POSIXct(date),
    date = as.Date(datetime),
    hour = hour(datetime),
    month = month(date),
    day = day(date),
    year = year(date)
  ) %>% filter(month %in% 3:6 & hour %in% 8:18)  



# 2. 多尺度趋势分析函数
calculate_trends <- function(data, group_vars, time_var, value_var) {
  # data = W10_hourly_trend_df
 data%>%
    group_by(across(all_of(c(group_vars, time_var)))) %>%
    summarise(
      mean_value = mean(!!sym(value_var), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(c("site", "Group", time_var)))) %>%
  filter(n() >= 3) %>%  # 至少3个时间点
  do({
    mod <- lm(mean_value ~ year, data = .)
    tidy(mod) %>% 
      filter(term != "(Intercept)") %>%
      select(estimate, p.value)
  }) %>%
  rename(trend_slope = estimate, trend_p = p.value)
}

# 3. 各尺度趋势计算
## 小时尺度 (8-18时逐小时趋势)
CMA_W10_hourly_trend <- W10_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", "Group",'year'), 
    time_var = "hour", 
    value_var = 'value.x'
  ) %>%
  mutate(time_scale = "hourly_8to18")

## 日尺度 (1-31天趋势)
CMA_W10_daily_trend <- W10_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", "Group", "year"), 
    time_var = "day", 
    value_var = 'value.x'
  ) %>%
  mutate(time_scale = "daily_1to31")

## 月尺度 (3-6月趋势)
CMA_W10_monthly_trend <- W10_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", "Group", "year"), 
    time_var = "month", 
    value_var = 'value.x'
  ) %>%
  mutate(time_scale = "monthly_3to6")


RNA_W10_hourly_trend <- W10_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", "Group",'year'), 
    time_var = "hour", 
    value_var = 'value.y'
  ) %>%
  mutate(time_scale = "hourly_8to18")

## 日尺度 (1-31天趋势)
RNA_W10_daily_trend <- W10_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", "Group", "year"), 
    time_var = "day", 
    value_var = 'value.y'
  ) %>%
  mutate(time_scale = "daily_1to31")

## 月尺度 (3-6月趋势)
RNA_W10_monthly_trend <- W10_hourly_trend_df %>%
  calculate_trends(
    group_vars = c("site", "Group", "year"), 
    time_var = "month", 
    value_var = 'value.y'
  ) %>%
  mutate(time_scale = "monthly_3to6")

CMA_W10_hourly_trend_results = CMA_W10_hourly_trend %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
CMA_W10_hourly_trend_sp = CMA_W10_hourly_trend_results
coordinates(CMA_W10_hourly_trend_sp) = ~lon+lat
proj4string(CMA_W10_hourly_trend_sp) <- CRS("+init=epsg:4480")
CMA_W10_hourly_trend_sf  =  st_as_sf(CMA_W10_hourly_trend_sp,coords = 1:2)
CMA_W10_hourly_trend_sf$Group = 'CMA';


CMA_W10_daily_trend_results = CMA_W10_daily_trend %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
CMA_W10_daily_trend_sp = CMA_W10_daily_trend_results
coordinates(CMA_W10_daily_trend_sp) = ~lon+lat
proj4string(CMA_W10_daily_trend_sp) <- CRS("+init=epsg:4480")
CMA_W10_daily_trend_sf  =  st_as_sf(CMA_W10_daily_trend_sp,coords = 1:2)
CMA_W10_daily_trend_sf$Group = 'CMA';


CMA_W10_monthly_trend_results = CMA_W10_monthly_trend %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
CMA_W10_monthly_trend_sp = CMA_W10_monthly_trend_results
coordinates(CMA_W10_monthly_trend_sp) = ~lon+lat
proj4string(CMA_W10_monthly_trend_sp) <- CRS("+init=epsg:4480")
CMA_W10_monthly_trend_sf  =  st_as_sf(CMA_W10_monthly_trend_sp,coords = 1:2)
CMA_W10_monthly_trend_sf$Group = 'CMA';




RNA_W10_hourly_trend_results = RNA_W10_hourly_trend %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
RNA_W10_hourly_trend_sp = RNA_W10_hourly_trend_results
coordinates(RNA_W10_hourly_trend_sp) = ~lon+lat
proj4string(RNA_W10_hourly_trend_sp) <- CRS("+init=epsg:4480")
RNA_W10_hourly_trend_sf  =  st_as_sf(RNA_W10_hourly_trend_sp,coords = 1:2)



RNA_W10_daily_trend_results = RNA_W10_daily_trend %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
RNA_W10_daily_trend_sp = RNA_W10_daily_trend_results
coordinates(RNA_W10_daily_trend_sp) = ~lon+lat
proj4string(RNA_W10_daily_trend_sp) <- CRS("+init=epsg:4480")
RNA_W10_daily_trend_sf  =  st_as_sf(RNA_W10_daily_trend_sp,coords = 1:2)



RNA_W10_monthly_trend_results = RNA_W10_monthly_trend %>% left_join(sitstation[,c(2,9,10)], by = 'site') 
RNA_W10_monthly_trend_sp = RNA_W10_monthly_trend_results
coordinates(RNA_W10_monthly_trend_sp) = ~lon+lat
proj4string(RNA_W10_monthly_trend_sp) <- CRS("+init=epsg:4480")
RNA_W10_monthly_trend_sf  =  st_as_sf(RNA_W10_monthly_trend_sp,coords = 1:2)


W10_monthly_trend_sf = rbind(RNA_W10_monthly_trend_sf,CMA_W10_monthly_trend_sf)
W10_hourly_trend_sf = rbind(RNA_W10_hourly_trend_sf,CMA_W10_hourly_trend_sf)


slope_range <- quantile(W10_hourly_trend_sf$trend_slope, probs = c(0, 1), na.rm = TRUE)
slope_breaks <- seq(round(slope_range[1], 2), round(slope_range[2], 2), length.out = 10)


W10_hourly_trend_sf_proj <- st_transform(W10_hourly_trend_sf, crs = target_crs)

W10_hourly_trend_sf_proj$facet_lab = paste0(W10_hourly_trend_sf_proj$Group," : Hour ", W10_hourly_trend_sf_proj$hour)

W10_hourly_trend_sf_proj$facet_lab  = factor(W10_hourly_trend_sf_proj$facet_lab,
                                                levels = c("ERA5L : Hour 8", "ERA5L : Hour 9", "ERA5L : Hour 10", "ERA5L : Hour 11",
                                                           "ERA5L : Hour 12",  "ERA5L : Hour 13","ERA5L : Hour 14",  "ERA5L : Hour 15",
                                                           "ERA5L : Hour 16",  "ERA5L : Hour 17",  "ERA5L : Hour 18",  "RNSHWD : Hour 8", 
                                                           "RNSHWD : Hour 9",  "RNSHWD : Hour 10", "RNSHWD : Hour 11", "RNSHWD : Hour 12",
                                                           "RNSHWD : Hour 13", "RNSHWD : Hour 14", "RNSHWD : Hour 15", "RNSHWD : Hour 16", 
                                                           "RNSHWD : Hour 17", "RNSHWD : Hour 18", "CMA : Hour 8","CMA : Hour 9", "CMA : Hour 10", 
                                                           "CMA : Hour 11",    "CMA : Hour 12","CMA : Hour 13","CMA : Hour 14",  
                                                           "CMA : Hour 15",  "CMA : Hour 16",   "CMA : Hour 17",    "CMA : Hour 18"))
W10_hourly_trend_sf_proj$hours = paste0('Hour: ',W10_hourly_trend_sf_proj$hour)
W10_hourly_trend_sf_proj$hours = factor(W10_hourly_trend_sf_proj$hours,
                                        levels = c("Hour: 8",   "Hour: 9",   "Hour: 10",  "Hour: 11",
                                                   "Hour: 12",  "Hour: 13",  "Hour: 14",  "Hour: 15",
                                                   "Hour: 16",  "Hour: 17",  "Hour: 18"))


W10_hourly_trend_gg_8_12 <- ggplot() +
  geom_sf(data = W10_hourly_trend_sf_proj[W10_hourly_trend_sf_proj$hour %in% c(8:12),],aes(color = trend_slope),
  size = 0.5,linewidth = 0.3,key_glyph = "rect") +
  scale_color_gradientn(
    colors = c(brewer.pal(11, "PRGn")),  # 增强颜色对比度
    values = scales::rescale(c(slope_range[1], 0, slope_range[2])),  # 确保0值居中
    limits = c(slope_range[1], slope_range[2]),
    breaks = slope_breaks,
    labels = function(x) sprintf("%.2f", x),  # 固定4位小数
    name = bquote("Wind speed trend (m s"^-1~" year"^-1~")")) +
  facet_grid(hours~Group)+ylim(3500000, 5950000) +xlim(-2900000, 1900000)+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  -9.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 7),
        legend.key.size = unit(1.5,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'bottom',#c(0.6, 1.2),
        panel.spacing = unit(0.03, units = "cm"), # removes space between panels
        #legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 7.0),
        legend.title= element_text(size = 8.0),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))




W10_hourly_trend_gg_13_18 <- ggplot() +
  geom_sf(data = W10_hourly_trend_sf_proj[W10_hourly_trend_sf_proj$hour %in% c(13:18),],aes(color = trend_slope),
          size = 0.5,linewidth = 0.3,key_glyph = "rect") +
  scale_color_gradientn(
    colors = c(brewer.pal(11, "PRGn")),  # 增强颜色对比度
    values = scales::rescale(c(slope_range[1], 0, slope_range[2])),  # 确保0值居中
    limits = c(slope_range[1], slope_range[2]),
    breaks = slope_breaks,
    labels = function(x) sprintf("%.2f", x),  # 固定4位小数
    name = bquote("Wind speed trend (m s"^-1~" year"^-1~")")) +
  facet_grid(hours~Group)+ylim(3500000, 5950000) +xlim(-2900000, 1900000)+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  -9.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 7),
        legend.key.size = unit(1.5,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'bottom',#c(0.6, 1.2),
        panel.spacing = unit(0.03, units = "cm"), # removes space between panels
        #legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 7.0),
        legend.title= element_text(size = 8.0),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


emf('.../Figures/Figure S0.emf',
     units = "cm", width=15,height=17, pointsize = 11)
W10_hourly_trend_gg_13_18
dev.off()




W10_monthly_trend_sf = rbind(RNA_W10_monthly_trend_sf,CMA_W10_monthly_trend_sf)

W10_monthly_trend_sf <- W10_monthly_trend_sf %>%
  mutate(month = case_when(
    month == 3 ~ "March",
    month == 4 ~ "April", 
    month == 5 ~ "May",
    month == 6 ~ "June",
    TRUE ~ as.character(month)  # 保留其他月份的数字表示
  )) %>%
  # 将月份转换为因子并设置正确顺序
  mutate(month = factor(month, levels = c("March", "April", "May", "June")))


slope_range <- quantile(W10_monthly_trend_sf$trend_slope, probs = c(0, 1), na.rm = TRUE)
slope_breaks <- seq(round(slope_range[1], 2), round(slope_range[2], 2), length.out = 10)



W10_monthly_trend_sf_proj <- st_transform(W10_monthly_trend_sf, crs = target_crs)

W10_monthly_trend_gg <- ggplot() +
  geom_sf(data = W10_monthly_trend_sf_proj,aes(color = trend_slope),
          size = 0.5,linewidth = 0.3,key_glyph = "rect") +
  scale_color_gradientn(
    colors = c(brewer.pal(11, "PRGn")),  # 增强颜色对比度
    values = scales::rescale(c(slope_range[1], 0, slope_range[2])),  # 确保0值居中
    limits = c(slope_range[1], slope_range[2]),
    breaks = slope_breaks,
    labels = function(x) sprintf("%.2f", x),  # 固定4位小数
    name = bquote("Wind speed trend (m s"^-1~" year"^-1~")")) +
  facet_grid(month~Group)+ylim(3500000, 5950000) +xlim(-2900000, 1900000)+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  -9.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 7),
        legend.key.size = unit(1.5,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'bottom',#c(0.6, 1.2),
        panel.spacing = unit(0.05, units = "cm"), 
        #legend.direction = "horizontal",#c(0.95,0.70),
        strip.background = element_rect(color = 'transparent'),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 7.0),
        legend.title= element_text(size = 8.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))#+




