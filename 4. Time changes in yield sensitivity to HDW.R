library(readxl)
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
library(corrplot)
library(devEMF)
##############################################################################
DHW_vars = c('cumulative_hours_HighTempLowHumidity_Early', 'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle',
             'cumulative_hours_PostRainScorch_Late', 'cumulative_hours_PostRainScorch_Middle')

release_years <- Wheat_exp_merged_final %>%
  group_by(Culs) %>%
  summarise(release_year = min(year)) %>%
  ungroup()

Wheat_exp_merged_final_df <- Wheat_exp_merged_final %>% left_join(release_years, by = "Culs")
Wheat_exp_merged_final_df = Wheat_exp_merged_final_df %>% filter(!is.na(cumulative_hours_HighTempLowHumidity_Early))


EXDat_mer = Wheat_exp_merged_final_df
EXDat_mer$hdw_hour = EXDat_mer$cumulative_hours_HighTempLowHumidity_Early+
                     EXDat_mer$cumulative_hours_HighTempLowHumidity_Late+
                     EXDat_mer$cumulative_hours_HighTempLowHumidity_Middle


#################################################################


EXDat_mer_DHW = EXDat_mer[EXDat_mer$hdw_hour!=0,]
EXDat_Myield_DHW = EXDat_mer_DHW %>% group_by(site,year) %>% summarise_at(vars("Yield_S"),list(mean))

EXDat_Mmer_DHW = merge(EXDat_Myield_DHW, EXDat_mer_DHW,  by = c('site','year'))

EXDat_Mmer_DHW$Y_anomly = EXDat_Mmer_DHW$Yield_S.y-EXDat_Mmer_DHW$Yield_S.x

#EXDat_Mmer_DHW = EXDat_Mmer_DHW[,c(1:8,168)]
nexpourn_vars = setdiff(unique(EXDat_mer$Culs),unique(EXDat_Mmer_DHW$Culs))

colnames(sitstation) = sitstation[2,];
sitstation = sitstation[-c(1:2),]
colnames(sitstation)[c(2,9,10)] = c('site', 'Lat', 'Lon')
sitstation$site = as.numeric(sitstation$site)

HDW_cul_exp_df <- read_excel("HDW_cul_exp_df_mhj_.xlsx")
HDW_cul_exp_df_jion = left_join(HDW_cul_exp_df,sitstation[,c(2,9,10)], 'site')
write.csv(HDW_cul_exp_df_jion, 'HDW_cul_exp_df_jion.csv')



nexpourn_vars_sites = EXDat_mer[EXDat_mer$Culs %in% nexpourn_vars&EXDat_mer$hdw_hour==0,]
nexpourn_vars_sites$group = 'No HDW exposure varieties'

expourn_lossY_sites = EXDat_Mmer_DHW[EXDat_Mmer_DHW$Y_anomly<0&EXDat_Mmer_DHW$hdw_hour!=0,]
expourn_lossY_sites$group = 'Varieties exposed to HDW with yield loss'

expourn_gainY_sites = EXDat_Mmer_DHW[EXDat_Mmer_DHW$Y_anomly>=0&EXDat_Mmer_DHW$hdw_hour!=0,,] 
expourn_gainY_sites$group = 'Varieties exposed to HDW without yield loss'

 
CUL_expourn  = length(unique(EXDat_Mmer_DHW$Culs))
CUL_nexpourn = length(unique(EXDat_mer$Culs))-CUL_expourn

CUL_lossn   = length(unique(EXDat_Mmer_DHW$Culs[EXDat_Mmer_DHW$Y_anomly<0]))
CUL_nlossn  = CUL_expourn-CUL_lossn

PC_CUL =  data.frame(value = c(CUL_expourn*100/(CUL_expourn+CUL_nexpourn),
                               CUL_nexpourn*100/(CUL_expourn+CUL_nexpourn),
                               CUL_lossn*100/(CUL_lossn+CUL_nlossn),
                               CUL_nlossn*100/(CUL_lossn+CUL_nlossn)),
                     Group = c('Percentage of varieties under DHW exposure',
                               'Percentage of varieties under DHW exposure',
                               'Percentage of varieties in yield loss attributed to DHW',
                               'Percentage of varieties in yield loss attributed to DHW'),
                     Type = c('DHW exposure', 'No DHW exposure', 'Yield loss', 'No Yield loss'))

EXDat_Mmer_DHW$Group = ifelse(EXDat_Mmer_DHW$Y_anomly<0, 'Yield loss', 'Yield gain')
EXDat_Mmer_DHW$Y_anomly_pc = EXDat_Mmer_DHW$Y_anomly*100/EXDat_Mmer_DHW$Yield_S.x

PC_CUL_gg1 =  ggplot(PC_CUL[PC_CUL$Group=='Percentage of varieties under DHW exposure',],
                     aes(x = Group, y = value, fill = Type)) +coord_polar(theta = "y") +    
  geom_col() +geom_text(aes(label = paste0(round(value,1), "")),
                        position = position_stack(vjust = 0.5),size = 2.5) +
  scale_fill_brewer(palette = "Set2",name = '') +
  theme_minimal(base_size = 16) +
  ylab(" ") +labs(title = ' ')+ #coord_flip()+
  xlab(NULL)+labs(subtitle = "Percentage of variety\n under HDW exposure (%)")+
  theme(plot.subtitle = element_text(size = 7, hjust = 0.5, vjust = -6,lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 7),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 8),
        legend.position = c(.5,0.01),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 4),
        panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

PC_CUL_gg2 =  ggplot(PC_CUL[PC_CUL$Group=='Percentage of varieties in yield loss attributed to DHW',], 
                     aes(x = Group, y = value, fill = Type)) +
  geom_col() +geom_text(aes(label = paste0(round(value,1), "")),
                        position = position_stack(vjust = 0.5),size = 2.5) +
  scale_fill_brewer(palette = "Set2",name = '') +
  theme_minimal(base_size = 16) +coord_polar(theta = "y") +
  ylab(" ") +#coord_flip()+
  xlab(NULL)+labs(subtitle = "Percentage of variety in \nyield loss attributed to HDW (%)")+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.5,                     # Horizontal adjustment
                                     vjust = -6,                      # Vertical adjustment
                                     lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 7),
        axis.text.x =  element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 8),
        legend.position = c(.5,0.01),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 4),
        panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

EX_Y_anomly_gg =  ggplot(EXDat_Mmer_DHW) + 
  geom_boxplot(aes(x = Group, y = Y_anomly_pc,fill = Group),
               width = .5, outlier.shape = NA,lwd = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'black')+
  stat_summary(aes(x = Group, y = Y_anomly_pc,label=round(..y..,1)),
               vjust = -1.5,hjust = -0.3,fun.y=mean, geom="text", size=2.5, color="red") +
  theme_minimal(base_size = 16) +xlab('')+
  scale_y_continuous(limits = c(-25,25),breaks= seq(-25,25,5))+
  ylab('Changes in yield of variety \ncluster with HDW exposure (%)')+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.3,'cm'),
        # text = element_text(size = 7),
        axis.text.x = element_text(size = 7,angle = 0,vjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.position = c(6.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 4),
        panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


sites_d_1 = nexpourn_vars_sites[,c(1,3,125)]
sites_d_2 = expourn_lossY_sites[,c(1,4,129)]
sites_n   = expourn_gainY_sites[,c(1,4,129)]


HDW_cul_exp_df =  rbind(sites_d_1, sites_d_2, sites_n)


sitstation <- read_excel("F:/Crop yield loss at DHW/sitstation.xls")
colnames(sitstation)[c(2,9,10)] = c('site','lat','lon')
HDW_cul_exp_df_jion = HDW_cul_exp_df %>% left_join(sitstation[,c(2,9,10)],by = 'site')

HDW_cul_exp_df_sp = HDW_cul_exp_df_jion; 

coordinates(HDW_cul_exp_df_sp) = ~lon+lat
proj4string(HDW_cul_exp_df_sp) <- CRS("+init=epsg:4480")
HDW_cul_exp_df_sf  =  st_as_sf(HDW_cul_exp_df_sp,coords = 1:2)


site_lable_prop <- HDW_cul_exp_df_sf %>%
  count(site, group) %>%                      
  group_by(site) %>%
  mutate(prop = n / sum(n)) %>%              
  select(-n) %>%
  pivot_wider(names_from = group, values_from = prop, values_fill = 0)

library(ggplot2)
library(ggtern)
library(patchwork)
library(RColorBrewer)
library(dplyr)

plot_spatial_ternary <- function(sf_obj, labels, china_layers = list(),
                                 xlim = c(75.5, 133.5), ylim = c(30.5, 52.8)) 
{
  # sf_obj = site_lable_prop
  # Set2  
  palette <- brewer.pal(3, "Set2")
  
  target_crs <- 4547
  China_line_proj <- st_transform(China_line, crs = target_crs)
  China_sea_proj <- st_transform(China_sea, crs = target_crs)
  Provience_line_proj <- st_transform(Provience_line, crs = target_crs)
  Chian_frame_proj <- st_transform(Chian_frame, crs = target_crs)
  cluster_cols = colnames(site_lable_prop)[-c(1:2)]
  #  
  sf_obj <- sf_obj %>%
    mutate(total = rowSums(across(all_of(cluster_cols))),
           r = !!sym(cluster_cols[1]) / total,
           g = !!sym(cluster_cols[2]) / total,
           b = !!sym(cluster_cols[3]) / total,
           #  
           col = rgb(
             r * col2rgb(palette[1])[1]/255 + g * col2rgb(palette[2])[1]/255 + b * col2rgb(palette[3])[1]/255,
             r * col2rgb(palette[1])[2]/255 + g * col2rgb(palette[2])[2]/255 + b * col2rgb(palette[3])[2]/255,
             r * col2rgb(palette[1])[3]/255 + g * col2rgb(palette[2])[3]/255 + b * col2rgb(palette[3])[3]/255
           )) %>%
    st_as_sf()
  sf_obj_proj <- st_transform(sf_obj, crs = target_crs)
  #  
  gg_map <- ggplot() +
    geom_sf(data = sf_obj_proj, aes(color = col), size = 2) +
    scale_color_identity()+
    #coord_sf(xlim = xlim, ylim = ylim) +
    theme_bw()
  
  #  
  for(layer in china_layers){
    gg_map <- gg_map + geom_sf(data = layer, color="grey65", linewidth=0.2)+
      theme_bw()+ ylim(3500000, 5950000) +xlim(-2900000, 1900000) +
      theme(legend.key.size = unit(0,'cm'),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_text(size = 0.01),
            legend.position ='none',
            legend.direction = "horizontal",#c(0.95,0.70),
            legend.key.height = unit(0.18,'cm'),
            legend.background = element_blank(),
            legend.text = element_text(size = 1),
            legend.title= element_text(size = 1),
            strip.background = element_rect(color = 'transparent'),
            plot.background = element_rect(fill = "transparent", colour = NA_character_)
      )
  }
  
  #  
  steps <- 40
  tern_grid <- expand.grid(
    c1 = seq(0, 1, length.out = steps),
    c2 = seq(0, 1, length.out = steps)
  )
  tern_grid$c3 <- 1 - tern_grid$c1 - tern_grid$c2
  tern_grid <- subset(tern_grid, c3 >= 0)
  
  #  
  tern_grid$col <- rgb(
    tern_grid$c1 * col2rgb(palette[1])[1]/255 + tern_grid$c2 * col2rgb(palette[2])[1]/255 + tern_grid$c3 * col2rgb(palette[3])[1]/255,
    tern_grid$c1 * col2rgb(palette[1])[2]/255 + tern_grid$c2 * col2rgb(palette[2])[2]/255 + tern_grid$c3 * col2rgb(palette[3])[2]/255,
    tern_grid$c1 * col2rgb(palette[1])[3]/255 + tern_grid$c2 * col2rgb(palette[2])[3]/255 + tern_grid$c3 * col2rgb(palette[3])[3]/255
  )
  
  gg_legend <- ggtern(tern_grid, aes(x = c1, y = c2, z = c3, color = col)) +
    geom_point(shape = 16, size = 2) +
    scale_color_identity() +
    theme_minimal() +  
    theme_void() +      
    xlab("") + ylab("") + zlab("") +
    
    theme(plot.margin = margin(30, 30, 30, 30, unit = "pt"),  
          tern.axis.ticks = element_blank(),    
          tern.axis.text.T = element_blank(),   
          tern.axis.text.L = element_blank(),   
          tern.axis.text.R = element_blank(),   
          axis.title = element_blank(),        
          axis.text = element_blank(),          
          axis.ticks = element_blank(),        
          plot.background = element_rect(fill = "transparent", colour = NA_character_)
    ) +
    annotate("text", x=1.05, y=0, z=0, label=labels[1], color='black', angle=-55, hjust=0.5, vjust=-0.5, size=1.5) +
    annotate("text", x=0, y=1.05, z=0, label=labels[2], color='black', angle=0,  hjust=0.5, vjust=-0.5, size=1.5) +
    annotate("text", x=0, y=0, z=1.05, label=labels[3], color='black', angle=45, hjust=0.5, vjust=-0.5, size=1.5)
  
 
  final_plot <- gg_map +
    annotation_custom(
      grob = ggplotGrob(gg_legend),
      xmin = 1000000, xmax = 2400000,  
      ymin =1800000
    )
  
  
  
  
  return(final_plot)
}



China_line_proj <- st_transform(China_line, crs = target_crs)
China_sea_proj <- st_transform(China_sea, crs = target_crs)
Provience_line_proj <- st_transform(Provience_line, crs = target_crs)
Chian_frame_proj <- st_transform(Chian_frame, crs = target_crs)

# ========= 3. =========
HDW_cul_exp_gg  <- plot_spatial_ternary(
  sf_obj = site_lable_prop,
  labels = c("Cluster 1", "Cluster 2", "Cluster 3"),
  china_layers = list(China_line_proj, China_sea_proj, Provience_line_proj, Chian_frame_proj)
)

  


emf('.../Figures/Figure S12.emf',
    units = "cm", width=14.8, height=7.6,  pointsize = 11)
ggdraw() +
  draw_plot(HDW_cul_exp_gg, x = 0, y = 0.00,  width =1, height = 1) +
  draw_plot(PC_CUL_gg1,  x = 0.08, y = 0.40,  width =0.2, height = 0.74)+
  draw_plot(PC_CUL_gg2,  x = .08, y = -0.05,  width =0.2, height = 0.7)+
  draw_plot(EX_Y_anomly_gg,  x = 0.4, y = 0.3,  width =.3, height = 0.6)
dev.off()

####################################################
####################################################
####################################################

Soil_vars = c( 'PT_JT_SM_0q~10q' , 'PT_JT_SM_10q~20q' , 'PT_JT_SM_20q~30q' ,
               'PT_JT_SM_30q~40q' , 'PT_JT_SM_40q~50q' ,  'PT_JT_SM_50q~60q' , 
               'PT_JT_SM_60q~70q' , 'PT_JT_SM_70q~80q' , 'PT_JT_SM_80q~90q' ,
               'PT_JT_SM_90q~100q' , 
               'JT_HD_SM_0q~10q' , 'JT_HD_SM_10q~20q' , 'JT_HD_SM_20q~30q' ,
               'JT_HD_SM_30q~40q' ,'JT_HD_SM_40q~50q' ,  'JT_HD_SM_50q~60q' ,
               'JT_HD_SM_60q~70q' , 'JT_HD_SM_70q~80q' , 
               'JT_HD_SM_80q~90q' , 'JT_HD_SM_90q~100q' , 
               'HD_MT_SM_0q~10q', 'HD_MT_SM_10q~20q' , 'HD_MT_SM_20q~30q' , 
               'HD_MT_SM_30q~40q' , 'HD_MT_SM_40q~50q' , 'HD_MT_SM_50q~60q' , 
               'HD_MT_SM_60q~70q' , 'HD_MT_SM_70q~80q' , 'HD_MT_SM_80q~90q' , 
               'HD_MT_SM_90q~100q')

Ta_vars = c('PJGDDNDHW' , 'JHGDDNDHW' , 'HMGDDNDHW' , 'JHEDDNDHW' , 'HMEDDNDHW' , 
            'PJFDDNDHW' , 'JHFDDNDHW')

Ta_varsS = c('PJGDD' , 'JHGDD' , 'HMGDD' , 'JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')

DHW_vars = c('cumulative_hours_DryWind_Early','cumulative_hours_DryWind_Late','cumulative_hours_DryWind_Middle',
             'cumulative_hours_HighTempLowHumidity_Early', 'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle', 'cumulative_hours_PostRainScorch_Early',
             'cumulative_hours_PostRainScorch_Late', 'cumulative_hours_PostRainScorch_Middle')

Phen_vars = c('PTdate',"JTdate","HDdate","MTdate")
Per_vars = c('PT_JT_total_rainfall', 'JT_HD_total_rainfall', 'HD_MT_total_rainfall',
             'PT_JT_total_rainfall2', 'JT_HD_total_rainfall2', 'HD_MT_total_rainfall2')

Cost_vars = c("Adj_Seed_Cost", "Adj_Fertilizer_Cost",  
             "Adj_Mechanization_Cost", "Adj_Irrigation_Cost")


vars <- c(Ta_varsS,Soil_vars,DHW_vars)

vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})

var_s1      = c(vars_escaped,'year',
                paste0('year:',c('JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')),
                paste0('year:', DHW_vars))

fmula_s1    = as.formula(paste0("Yield_Y~ ", paste0(var_s1, collapse = " + "), "|site"))

vars <- c(Ta_varsS,Soil_vars,DHW_vars,Cost_vars)

vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})

var_s2     = c(vars_escaped,'year',
               paste0('year:',c('JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')),
               paste0('year:', DHW_vars))

fmula_s2    = as.formula(paste0("Yield_Y~ ", paste0(var_s2, collapse = " + "), "|site "))


vars <- c(Ta_varsS,Per_vars,DHW_vars)

vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})

var_s3     = c(vars_escaped,'year',
               paste0('year:',c('JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')),
               paste0('year:', DHW_vars))

fmula_s3    = as.formula(paste0("Yield_Y~ ", paste0(var_s3, collapse = " + "), "|site"))


vars <- c(Ta_varsS,Per_vars,DHW_vars,Cost_vars)

vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})

var_s4     = c(vars_escaped,'year',
               paste0('year:',c('JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')),
               paste0('year:', DHW_vars))

fmula_s4    = as.formula(paste0("Yield_Y~", paste0(var_s4, collapse = " + "), "|site"))


BootReg_sen_change    = function(start.yr,end.yr, fmula, boot, data, Cost){
  
 
  bootcoefs = c()

  yrs.to.samp = start.yr:end.yr
 
  nyr = end.yr - start.yr + 1
  
  all_years <- unique(data$year[data$year >= start.yr & data$year <= end.yr])
  all_counties <- unique(data$site)
  
  Rsq       = NULL
  for (n in 1:boot) {
    
    yrsamp = sample(yrs.to.samp,size= 38,replace = T)
    tempdf = data %>% filter(year == yrsamp[1]) 
    
    for (k in 2:38) tempdf = rbind(tempdf, data %>% filter(year == yrsamp[k]))
   
    
    cropfit    = feols(fmula, data = tempdf) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)

  }
  bootcoef = data.frame(bootcoefs)

  
  PJFDD_change  = (bootcoef$Estimate[bootcoef$Var=='PJFDD:year']) * (2018-1981)
  
  # JHFDD_change  = (bootcoef$Estimate[bootcoef$Var=='JHFDDNDHW:year']) * (2018-1981)
  
  JHEDD_change  = (bootcoef$Estimate[bootcoef$Var=='JHEDD:year']) * (2018-1981)
  
  HMEDD_change  = (bootcoef$Estimate[bootcoef$Var=="HMEDD:year"]) * (2018-1981)

  HighTempLowHumidity_Early_change =  (bootcoef$Estimate[bootcoef$Var=='cumulative_hours_HighTempLowHumidity_Early:year'] )* (2018-1981)
  
  HighTempLowHumidity_Middle_change = ( bootcoef$Estimate[bootcoef$Var=='cumulative_hours_HighTempLowHumidity_Middle:year'] )* (2018-1981)
  
  HighTempLowHumidity_Late_change =  (bootcoef$Estimate[bootcoef$Var=='cumulative_hours_HighTempLowHumidity_Late:year'] )* (2018-1981)
  
  
  
  PostRainScorch_Early_change =  (bootcoef$Estimate[bootcoef$Var=='cumulative_hours_PostRainScorch_Early:year'] )* (2018-1981)
  
  PostRainScorch_Middle_change =  ( bootcoef$Estimate[bootcoef$Var=='cumulative_hours_PostRainScorch_Middle:year'] )* (2018-1981)
  
  PostRainScorch_Late_change =  ( bootcoef$Estimate[bootcoef$Var=='cumulative_hours_PostRainScorch_Late:year'] )* (2018-1981)
 
  
  Mbootcoef     = data.frame(value = c(PJFDD_change,JHEDD_change,HMEDD_change,
                                         HighTempLowHumidity_Early_change, 
                                         HighTempLowHumidity_Middle_change,
                                         HighTempLowHumidity_Late_change,
                                         PostRainScorch_Early_change, 
                                         PostRainScorch_Middle_change,
                                         PostRainScorch_Late_change),
                               varbs = c(rep(c('FDD','EDD','EDD'),each = length(PJFDD_change)),      
                                         rep(c('HTLH','HTLH','HTLH'), each = length(PJFDD_change)),
                                         rep(c('PRGW','PRGW','PRGW'), each = length(PJFDD_change))),
                               stage = c(rep(c('PT-JT','JT-HD','HD-MT'), each = length(PJFDD_change)),
                                         rep(c('Early','Middle','Late'), each = length(PJFDD_change)),
                                         rep(c('Early','Middle','Late'), each = length(PJFDD_change)))) 
    

  return(Mbootcoef)
}
set.seed(1234)

Sens_fit_1 =   BootReg_sen_change(1981, 2018, fmula_s1, 10000, Wheat_county_merged_final, Cost = F)  
Sens_fit_2 =   BootReg_sen_change(1981, 2018, fmula_s2, 10000, Wheat_county_merged_final, Cost = T)  
Sens_fit_3 =   BootReg_sen_change(1981, 2018, fmula_s3, 10000, Wheat_county_merged_final, Cost = F)  
Sens_fit_4 =   BootReg_sen_change(1981, 2018, fmula_s4, 10000, Wheat_county_merged_final, Cost = T)  

Sens_fit_1$Model = 'TSDM'
Sens_fit_2$Model = 'TSCD'
Sens_fit_3$Model = 'TPDM'
Sens_fit_4$Model = 'TPCD'

Sens_fit_1$Y_levle = 'County'
Sens_fit_2$Y_levle = 'County'
Sens_fit_3$Y_levle = 'County'
Sens_fit_4$Y_levle = 'County'

##################################################################
##################################################################
##################################################################
DHW_vars = c('cumulative_hours_HighTempLowHumidity_Early', 
'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle',
             'cumulative_hours_PostRainScorch_Late',
			 'cumulative_hours_PostRainScorch_Middle')
			 
Cost_vars = c("Adj_Seed_Cost",          "Adj_Fertilizer_Cost",                                                       
              "Adj_Mechanization_Cost", "Adj_Irrigation_Cost" )
release_years <- Wheat_exp_merged_final %>%
  group_by(Culs) %>%
  summarise(release_year = min(year)) %>%
  ungroup()

Wheat_exp_merged_final_df <- Wheat_exp_merged_final %>% left_join(release_years, by = "Culs")
Wheat_exp_merged_final_df = Wheat_exp_merged_final_df %>% filter(!is.na(cumulative_hours_PostRainScorch_Middle))

vars <- c(Ta_varsS,Soil_vars,DHW_vars)

vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})

var_s1     = c(vars_escaped,'year',
               paste0('year:',c('JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')),
               paste0('year:', DHW_vars))

fmula_s1f    = as.formula(paste0("Yield_S ~ ", paste0(var_s1, collapse = " + "), "|site"))

vars <- c(Ta_varsS,Soil_vars,DHW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})
var_s2     = c(vars_escaped,'year',
               paste0('year:',c('JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')),
               paste0('year:', DHW_vars))
fmula_s2f    = as.formula(paste0("Yield_S ~", paste0(var_s2, collapse = " + "), "|site"))


vars <- c(Ta_varsS,Per_vars,DHW_vars)

vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})

var_s3     = c(vars_escaped,'year',
               paste0('year:',c('JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')),
               paste0('year:', DHW_vars))

fmula_s3f    = as.formula(paste0("Yield_S ~", paste0(var_s3, collapse = " + "), "|site"))

vars <- c(Ta_varsS,Per_vars,DHW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})
# var_s4     = c(vars_escaped,'year','release_year',paste0('year:',c(vars_escaped,'release_year')),
#                paste0('release_year:',c(vars_escaped)))
var_s4     = c(vars_escaped,'year',
               paste0('year:',c('JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')),
               paste0('year:', DHW_vars))

fmula_s4f    = as.formula(paste0("Yield_S ~ ", paste0(var_s4, collapse = " + "), "|site"))

BootReg_sen_change    = function(start.yr,end.yr, fmula, boot, data, Cost){
     
  bootcoefs = c()
  
  yrs.to.samp <- start.yr:end.yr
  
  sites = unique(data$site)  
  
  nyr = end.yr - start.yr + 1
  
  Rsq       = NULL
  for (n in 1:boot) {

     samp_sites = sample(sites, size = length(sites), replace = TRUE)  
     tempdf = lapply(samp_sites, function(s){
       dats = data %>% filter(site == s, year %in% yrs.to.samp)
       dats$draw_site_id = s  
       dats
     }) %>% bind_rows()      
    
    cropfit    = feols(fmula, data =tempdf) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
  }
  bootcoef = data.frame(bootcoefs)
  
  PJFDD_change  = (bootcoef$Estimate[bootcoef$Var=='PJFDD:year']) * (2018-2007)
  
    
  JHEDD_change  = (bootcoef$Estimate[bootcoef$Var=='JHEDD:year']) * (2018-2007)
  
  HMEDD_change  = (bootcoef$Estimate[bootcoef$Var=="HMEDD:year"]) * (2018-2007)
  
        
  HighTempLowHumidity_Early_change =   (bootcoef$Estimate[bootcoef$Var=='cumulative_hours_HighTempLowHumidity_Early:year'] )* (2018-2007)
  
  HighTempLowHumidity_Middle_change =  ( bootcoef$Estimate[bootcoef$Var=='cumulative_hours_HighTempLowHumidity_Middle:year'] )* (2018-2007)
  
  HighTempLowHumidity_Late_change =  (bootcoef$Estimate[bootcoef$Var=='cumulative_hours_HighTempLowHumidity_Late:year'] )* (2018-2007)
  
  
  
  # PostRainScorch_Early_change =  (bootcoef$Estimate[bootcoef$Var=='cumulative_hours_PostRainScorch_Early:year'] )* (2018-2007)
  
  PostRainScorch_Middle_change =  ( bootcoef$Estimate[bootcoef$Var=='cumulative_hours_PostRainScorch_Middle:year'] )* (2018-2007)
  
  PostRainScorch_Late_change =  ( bootcoef$Estimate[bootcoef$Var=='cumulative_hours_PostRainScorch_Late:year'] )* (2018-2007)
  
  
  Mbootcoef     = data.frame(value = c(PJFDD_change,JHEDD_change,HMEDD_change,
                                         HighTempLowHumidity_Early_change, 
                                         HighTempLowHumidity_Middle_change,
                                         HighTempLowHumidity_Late_change,
                                         # PostRainScorch_Early_change, 
                                         PostRainScorch_Middle_change,
                                         PostRainScorch_Late_change),
                               varbs = c(rep(c('FDD','EDD','EDD'),each = length(PJFDD_change)),
                                         rep(c('HTLH','HTLH','HTLH'), each = length(PJFDD_change)),
                                         rep(c('PRGW','PRGW'), each = length(PJFDD_change))),
                               stage = c(rep(c('PT-JT','JT-HD','HD-MT'), each = length(PJFDD_change)),
                                         rep(c('Early','Middle','Late'), each = length(PJFDD_change)),
                                         rep(c('Middle','Late'), each = length(PJFDD_change))),
                               Breeding = c(rep('No',length(PJFDD_change)*8)))
    

  return(Mbootcoef)
}
set.seed(1234)

Sens_fit_1f =   BootReg_sen_change(2007, 2018, fmula_s1f, 10000, Wheat_exp_merged_final_df, Cost = F)  
Sens_fit_2f =   BootReg_sen_change(2007, 2018, fmula_s2f, 10000, Wheat_exp_merged_final_df, Cost = T)  
Sens_fit_3f =   BootReg_sen_change(2007, 2018, fmula_s3f, 10000, Wheat_exp_merged_final_df, Cost = F)  
Sens_fit_4f =   BootReg_sen_change(2007, 2018, fmula_s4f, 10000, Wheat_exp_merged_final_df, Cost = T)  

Sens_fit_1f$Model = 'TSDM'
Sens_fit_2f$Model = 'TSCD'
Sens_fit_3f$Model = 'TPDM'
Sens_fit_4f$Model = 'TPCD'

Sens_fit_1f$Y_levle = 'Field'
Sens_fit_2f$Y_levle = 'Field'
Sens_fit_3f$Y_levle = 'Field'
Sens_fit_4f$Y_levle = 'Field'

Sens_fit_1$Breeding='No'
Sens_fit_2$Breeding='No'
Sens_fit_3$Breeding='No'
Sens_fit_4$Breeding='No'



#############################################################################################
library(ggh4x)

Sens_fit_df = rbind(Sens_fit_1,Sens_fit_2,Sens_fit_3,Sens_fit_4,
                    Sens_fit_1f,Sens_fit_2f,Sens_fit_3f, Sens_fit_4f)

Sens_fit_df_TEF = Sens_fit_df %>% filter(varbs %in% c("EDD", "FDD")&Breeding %in% c('No'))
Sens_fit_df_TEF$cols = paste0(Sens_fit_df_TEF$varbs,'_',Sens_fit_df_TEF$stage)

Sens_fit_df_TEF$fact = paste0(Sens_fit_df_TEF$Y_levle,': ', Sens_fit_df_TEF$Model)

sens_change_TEF_gg =  ggplot(Sens_fit_df_TEF) + 
   stat_pointinterval(aes(x = cols, y = value,color = cols),size = 1.5,
                     position = position_dodge(width = 0.7, preserve = "single"))+
   scale_fill_manual(values = c("orangered3", "orangered",
                               brewer.pal(11, "PRGn")[2],brewer.pal(11, "PRGn")[2],
                               brewer.pal(11, "PRGn")[4],brewer.pal(11, "PRGn")[4],
                               brewer.pal(11, "PRGn")[9],brewer.pal(11, "PRGn")[9],
                               brewer.pal(11, "PRGn")[10],
                               brewer.pal(11, "PRGn")[10]))+
  scale_color_manual(values = c("orangered3", "orangered",
                                brewer.pal(11, "PRGn")[2],brewer.pal(11, "PRGn")[2],
                                brewer.pal(11, "PRGn")[4],brewer.pal(11, "PRGn")[4],
                                brewer.pal(11, "PRGn")[9],brewer.pal(11, "PRGn")[9],
                                brewer.pal(11, "PRGn")[10],
                                brewer.pal(11, "PRGn")[10]))+
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'black')+
  stat_summary(aes(x = cols, y = value,label=round(..y..,1), color = cols),
               hjust = 0,vjust = 1.5,fun.y=mean, geom="text", size=3) +
   theme_bw()+ ylab(bquote('Changes in yield sensitivity to weather'~(kg~ha^-1~day~'°C')))+
  scale_x_discrete('',labels = c("EDD_HD-MT"  = bquote(EDD['HD-MT']),
                                 "EDD_JT-HD"  = bquote(EDD['JT-HD']),
                                 "FDD_JT-HD"  = bquote(FDD['JT-HD']),
                                 "FDD_PT-JT"  = bquote(FDD['PT-JT'])))+#coord_flip()+
  facet_wrap(fact~., scales = "free",ncol=2) +
  theme(strip.text = element_text(size = 8),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 4),
        axis.text.x = element_text(size = 7,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.position = c(10.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 4),
        # panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


emf('.../Figures/Figure  S27.emf',
     units = "cm", width=16, height=16,  pointsize = 11)
sens_change_TEF_gg 
dev.off()



Sens_fit_df_dhw = Sens_fit_df[Sens_fit_df$varbs %in% c("DTWD", "HTLH", "PRGW"),]
Sens_fit_df_dhw$cols = paste0(Sens_fit_df_dhw$varbs,'_',Sens_fit_df_dhw$stage)

Sens_fit_df_dhw_filtered <- Sens_fit_df_dhw %>%
  group_by(varbs, Model, Y_levle, Breeding) %>%
  filter(value >= quantile(value, 0.025, na.rm = TRUE),
         value<= quantile(value, 0.975, na.rm = TRUE)) %>%
  ungroup()


Sens_fit_df_dhw_filtered $fact = paste0(Sens_fit_df_dhw_filtered $Y_levle,': ', Sens_fit_df_dhw_filtered$Model)

library(ggh4x)
sens_change_DHW_gg =  ggplot(Sens_fit_df_dhw_filtered %>%
                               filter(Breeding=='No')%>%
                               filter(!str_detect(cols, 'DTWD'))) + 
  stat_halfeye(aes(x = cols, y = value,fill = cols),adjust = .6,width = 1,
               .width = 0.0, alpha = 0.6,justification = -.10,  point_colour = NA) +
  stat_pointinterval(aes(x = cols, y = value,color = cols),size = 1.5,
                     position = position_dodge(width = 0.7, preserve = "single"))+
  scale_color_manual(name = '',values = brewer.pal(11, "Set2")[c(1,1,1,2,2,2)])+
  scale_fill_manual(name = '',values =  brewer.pal(11, "Set2")[c(1,1,1,2,2,2)])+
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'black')+
  stat_summary(aes(x = cols, y = value,label=round(..y..,1), color = cols),
               hjust = 0,vjust = 1.5,fun.y=mean, geom="text", size=3) +
  theme_bw()+ ylab(bquote('Changes in yield sensitivity to DHW'~(kg~ha^-1~hour^-1)))+
  xlab('Types of HDW across three phase in HD-MT')+
  scale_x_discrete(limits = c("HTLH_Early", "HTLH_Middle", "HTLH_Late", 
                                 "PRGW_Early", "PRGW_Middle", "PRGW_Late"),
                   labels = c("HTLH_Early"  = "HTLH-E",#bquote(HTLH['E']),
                                 "HTLH_Middle" = "HTLH-M",#bquote(HTLH['M']),
                                 "HTLH_Late"   = "HTLH-L",#bquote(HTLH['L']),
                                 "PRGW_Early"  = "PRGW-E",#bquote(PRGW['E']),
                                 "PRGW_Middle" = "PRGW-M",#bquote(PRGW['M']),
                                 "PRGW_Late"   = "PRGW-L"#bquote(PRGW['L']))
                                   ))+#coord_flip()+
  facet_wrap(fact~., scales = "free",ncol=2) +
  theme(strip.text = element_text(size = 8),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 4),
        axis.text.x = element_text(size = 7,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 10),
        legend.position = c(10.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 4),
        # panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


emf('.../Figures/Figure S26.emf',
     units = "cm", width=14.8, height=18,  pointsize = 11)
sens_change_DHW_gg
dev.off()


Sens_fit_df_dhw_sum = Sens_fit_df_dhw[Sens_fit_df_dhw$Breeding %in% 'No',]
Sens_fit_df_dhw_sum$ID = c(rep(c(1:100),36),rep(c(1:100),20)) #rep(c(1:100),56) 

summarized_df <- Sens_fit_df_dhw_sum %>%
  group_by(varbs, Model, Y_levle, Breeding, ID) %>%
  summarise(value = sum(value, na.rm = TRUE))  %>%
  ungroup()  # Optional: Removes grouping structure


summarized_df_filtered <- summarized_df %>%
  group_by(varbs, Model, Y_levle, Breeding) %>%
  filter(value >= quantile(value, 0.025, na.rm = TRUE),
         value<= quantile(value, 0.975, na.rm = TRUE)) %>%
  ungroup()

 library(ggh4x)

county_sens_change_sum_gg <- ggplot(summarized_df_filtered[summarized_df_filtered$varbs!="DTWD"&summarized_df_filtered$Y_levle=='County',]) + 
  stat_halfeye(aes(x = varbs, y = value, fill = varbs),
               adjust = 0.6, width = 0.8, .width = 0.0, alpha = 0.6,
               position = position_dodge(width = 1),
               justification = -.2, point_colour = NA) +
  stat_pointinterval(aes(x = varbs, y = value, color = varbs),justification = .2,
                     size = 0.6, position = position_dodge(width = 1),alpha =0.6) +
  scale_color_manual(name = '', values = brewer.pal(8, "Set2")) +
  scale_fill_manual(name = '', values = brewer.pal(8, "Set2")) +
  geom_hline(yintercept = 0, lwd = 0.5, linetype = "dashed", color = 'black') +
  # stat_summary(aes(x = varbs, y = value, label = round(..y..,1), color = Y_levle),
  #              fun = median, geom = "text", size = 2,
  #              position = position_dodge(width = 0.8),
  #              hjust = 0.5, vjust = 2.5) +
  # ylab(bquote(atop('Changes over time in yield sensitivity',
  #                  'to HDW'~(kg~ha^-1~hour^-1)))) +
  stat_summary(aes(x = varbs, y =value,  # 固定高度，例如 y=0 或自定义
                   label = round(..y.., 1), color = varbs),
               fun = median, geom = "label", size = 2,
               position = position_dodge(width = 0.8),
               hjust = 1.2, vjust = -1,  # 控制上下偏移
               label.size = 0.15, fill = "white", label.r = unit(0.15, "lines")) +
  ylab(bquote('Changes in yield sensitivity to HDW'~(kg~ha^-1~hour^-1))) +
  xlab('Types of HDW')+
  # scale_x_discrete('', labels = c("DTWD" = bquote(HDW['DTWD']),
  #                                 "HTLH" = bquote(HDW['HTLH']),
  #                                 "PRGW" = bquote(HDW['PRGW']))) +
  labs(subtitle = "(a)") +theme_bw() +facet_wrap(.~Model,ncol = 2) +
  #coord_cartesian(ylim = c(-1000, 1000))+
  theme(plot.subtitle = element_text(size =10,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 7),
        legend.key.size = unit(0.3, 'cm'),
        text = element_text(size = 4),
        axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.position = "none",  # 将图例移到顶部
        panel.spacing = unit(0.5, "lines"),  # 增加面板间距
        # panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


field_sens_change_sum_gg <- ggplot(summarized_df_filtered[summarized_df_filtered$varbs!="DTWD"&summarized_df_filtered$Y_levle!='County',]) + 
  stat_halfeye(aes(x = varbs, y = value, fill = varbs),
               adjust = 0.6, width = 0.8, .width = 0.0, alpha = 0.6,
               position = position_dodge(width = 1),
               justification = -.2, point_colour = NA) +
  stat_pointinterval(aes(x = varbs, y = value, color = varbs),justification = .2,
                     size = 0.6, position = position_dodge(width = 1),alpha =0.6) +
  scale_color_manual(name = '', values = brewer.pal(8, "Set2")) +
  scale_fill_manual(name = '', values = brewer.pal(8, "Set2")) +
  geom_hline(yintercept = 0, lwd = 0.5, linetype = "dashed", color = 'black') +
  # stat_summary(aes(x = varbs, y = value, label = round(..y..,1), color = Y_levle),
  #              fun = median, geom = "text", size = 2,
  #              position = position_dodge(width = 0.8),
  #              hjust = 0.5, vjust = 2.5) +
  # ylab(bquote(atop('Changes over time in yield sensitivity',
  #                  'to HDW'~(kg~ha^-1~hour^-1)))) +
  stat_summary(aes(x = varbs, y =value,  # 固定高度，例如 y=0 或自定义
                   label = round(..y.., 1), color = varbs),
               fun = median, geom = "label", size = 2,
               position = position_dodge(width = 0.8),
               hjust = 0.5, vjust = -3,  # 控制上下偏移
               label.size = 0.15, fill = "white", label.r = unit(0.15, "lines")) +
  ylab(bquote('Changes in yield sensitivity to HDW'~(kg~ha^-1~hour^-1))) +
  xlab('Types of HDW')+
  # scale_x_discrete('', labels = c("DTWD" = bquote(HDW['DTWD']),
  #                                 "HTLH" = bquote(HDW['HTLH']),
  #                                 "PRGW" = bquote(HDW['PRGW']))) +
  labs(subtitle = "(a)") +theme_bw() +facet_wrap(.~Model,ncol = 2) +
  #coord_cartesian(ylim = c(-1000, 1000))+
  theme(plot.subtitle = element_text(size =10,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 7),
        legend.key.size = unit(0.3, 'cm'),
        text = element_text(size = 4),
        axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.position = "none",  # 将图例移到顶部
        panel.spacing = unit(0.5, "lines"),  # 增加面板间距
        # panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


########################################################
############################## Province sensitivity fit
########################################################

DHW_vars = c('cumulative_hours_HighTempLowHumidity_Early', 'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle','cumulative_hours_PostRainScorch_Early',
             'cumulative_hours_PostRainScorch_Late', 'cumulative_hours_PostRainScorch_Middle')


PDHW_vars    = paste0(DHW_vars,':','Provience')

vars         <- c(Ta_varsS,Soil_vars,DHW_vars,paste0(DHW_vars,':','year'),
                  paste0(PDHW_vars,':','year'),Cost_vars)
				  
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula       = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|Provience^site"))

PBootReg    = function(fmula, boot, data, size){

  bootcoefs = c()
  
  Provience <- unique(data$Provience)
  
  Rsq       = NULL
  for (n in 1:boot) {
    
    samp = sample(Provience,size= length(Provience),replace = T)
    tempdf = data %>% filter(year == samp[1]) 
    
    for (k in 2:nyr) tempdf = rbind(tempdf, data %>% filter(Provience == samp[k]))
    
    cropfit    = feols(fmula, data = tempdf) 
    
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
  }
  bootcoef = data.frame(bootcoefs)
  
  return(bootcoef)
}

PDHW_yield_fit = PBootReg(fmula, 10000, Wheat_county_merged_final[Wheat_county_merged_final$Provience!='吉林',] , 100000)

Pro_sen_avfit  = PDHW_yield_fit 

Pro_sen_avfit$type   = str_extract(Pro_sen_avfit$Var,"(?<=_)[A-Za-z]+(?=_(Early|Middle|Late))|(?<=_)[A-Za-z]+(?=:Provience)")
Pro_sen_avfit$Period = str_extract(Pro_sen_avfit$Var,"(?<=_)(Early|Middle|Late)(?=:Provience|$)")
Pro_sen_avfit$Provience = str_extract(Pro_sen_avfit$Var, "(?<=Provience)[\u4e00-\u9fa5]+") 



# DHW_yield_fit_TEMP$type[DHW_yield_fit_TEMP$type=='DryWind'] = 'DTWD'
Pro_sen_avfit$type[Pro_sen_avfit$type=='HighTempLowHumidity'] = 'HTLH'
Pro_sen_avfit$type[Pro_sen_avfit$type=='PostRainScorch'] = 'PRGW'

Pro_sen_avfit = Pro_sen_avfit %>%filter(grepl("Provience", Var)|grepl("cumulative", Var))


Pro_sen_avfit_base = Pro_sen_avfit[is.na(Pro_sen_avfit$Provience==T),]
Pro_sen_avfit_Prov = Pro_sen_avfit[!is.na(Pro_sen_avfit$Provience==T),]

Pro_sen_avfit_join = Pro_sen_avfit_Prov %>%left_join(Pro_sen_avfit_base, by= c('type','Period','ID')) 

Pro_sen_avfit_all =data.frame(Var = Pro_sen_avfit_join$Var.x,
                              Estimate = Pro_sen_avfit_join$Estimate.x+Pro_sen_avfit_join$Estimate.y,
                              Std..Error=Pro_sen_avfit_join$Std..Error.x+Pro_sen_avfit_join$Pr...t...y,
                              Type=Pro_sen_avfit_join$type,
                              Period=Pro_sen_avfit_join$Period,
                              province=Pro_sen_avfit_join$Provience.x,
                              ID= Pro_sen_avfit_join$ID)


sum_Pro_sen_avfit <- Pro_sen_avfit_all%>%
  group_by(province, Type, ID) %>%
  summarise(value = sum(Estimate, na.rm = TRUE))  %>%
  ungroup() 

sum_Pro_sen_avch = sum_Pro_sen_avfit %>% left_join(sum_Pro_sen_change, by = c("province", "Type", "ID"))

sum_Pro_sen_stats <- sum_Pro_sen_avch %>%
  group_by(province, Type) %>%
  summarise(
    mean_value.x = mean(value.x, na.rm = TRUE),  # value.x 的平均值
    sd_value.x = sd(value.x, na.rm = TRUE),      # value.x 的标准差
    mean_value.y = mean(value.y, na.rm = TRUE),  # value.y 的平均值
    sd_value.y = sd(value.y, na.rm = TRUE),      # value.y 的标准差
    .groups = "drop"  # 避免分组警告
  )

sum_Pro_sen_stats = sum_Pro_sen_stats %>% filter(!(Type=='PRGW'&province %in% c('Gansu','Inner Mongolia',
                                                               'Ningxia','Tianjing','Xinjiang')))

Sens_Pro_mean_gg = ggplot(sum_Pro_sen_stats, aes(x = mean_value.x, y = mean_value.y)) +
  geom_errorbar(aes(ymin = mean_value.y - sd_value.y, 
                    ymax = mean_value.y + sd_value.y,color = province),
                width = 0,alpha =0.4) +
  geom_errorbarh(aes(xmin = mean_value.x - sd_value.x, 
                     xmax = mean_value.x + sd_value.x,color = province),
                 height = 0,alpha =0.4) +
  geom_point(aes(color = province), size = 2.0,alpha =0.4) +
  ggrepel::geom_text_repel(aes(label = province,color = province), size = 2.0,alpha = 0.6, box.padding = 0.2,max.overlaps = 20) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  scale_color_manual(values =  c(brewer.pal(8, "Dark2"), brewer.pal(11, "Paired")))+
  facet_wrap( ~Type, scales = "free",ncol = 2) +
  #ylab(bquote(atop('Time trends in yield sensitivity','to DHW 1981-2018 ' (kg~ha^-2~hour^-1))))+
  ylab(bquote('Changes in yield sensitivity to HDW ' (kg~ha^-1~hour^-1)))+
  xlab(bquote('Average yield sensitivity to HDW ' (kg~ha^-1~hour^-1)))+
  labs(subtitle = "(b)")+coord_cartesian(ylim = c(-50, 50)) +  # 关键修改：限制y轴范围
  theme_bw() +theme(plot.subtitle = element_text(size = 10,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 7),
        axis.text.x = element_text(size = 7,angle = 0,vjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.position = c(10.12,0.80),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 4),
        # panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


filed_wheat_data = Wheat_exp_merged_final

filed_wheat_data$HTLH = filed_wheat_data$cumulative_hours_HighTempLowHumidity_Early+
  filed_wheat_data$cumulative_hours_HighTempLowHumidity_Late+filed_wheat_data$cumulative_hours_HighTempLowHumidity_Middle

filed_wheat_data$PRGW = filed_wheat_data$cumulative_hours_PostRainScorch_Early+
  filed_wheat_data$cumulative_hours_PostRainScorch_Late+filed_wheat_data$cumulative_hours_PostRainScorch_Middle

filed_wheat_data$DTWD = filed_wheat_data$cumulative_hours_DryWind_Early+
  filed_wheat_data$cumulative_hours_DryWind_Late+filed_wheat_data$cumulative_hours_DryWind_Middle

DHW_vars = c('HTLH', 'PRGW')


varsb <- c(Ta_varsS,Soil_vars,DHW_vars,Cost_vars)
varsb <- sapply(varsb, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})
set.seed(123)

fmulab    = as.formula(paste0("Yield_S ~ ", paste0(varsb, collapse = " + ")))
FDHW_yield_fit_year = c()
for (i in unique(filed_wheat_data$year)) {
   
  temp_fitb =   BootReg(fmulab,
                        10000, 
                        filed_wheat_data[filed_wheat_data$year==i,], 
                        600)
  
  
  temp_fitb =  temp_fitb %>% filter(Var %in% c(DHW_vars))
  temp_fitb$year = i
  temp_fita$model = 'TSCD'

  FDHW_yield_fit_year = rbind(FDHW_yield_fit_year,  temp_fita,temp_fitb)
}

FDHW_yield_fit_year$Type = FDHW_yield_fit_year$Var

FDHW_yield_fit_year$group = 'HTLH'

sen_change_trend = data.frame(year = c(seq(2006,2018,1),seq(2006,2018,1),seq(2006,2018,1),seq(2006,2018,1)),
                              Estimate = c(seq(1:13)*-13.7/13, seq(1:13)*36/13,seq(1:13)*-27/13, seq(1:13)*46.2/13),
                              Type = c(rep("HTLH",13), rep("PRGW",13),rep("HTLH",13), rep("PRGW",13)),
                              group = 'PRGW',
                              model = c(rep('TSCD', 26),rep('TSDM',26)))

FHW_ychange_gg_df  = FDHW_yield_fit_year[,c(2,3,7:10)]#rbind(sen_change_trend , FDHW_yield_fit_year[,c(2,7:10)])
FHW_ychange_gg_df  = FHW_ychange_gg_df %>% filter(year>2006)
FHW_ychange_gg_df$Type[FHW_ychange_gg_df$Type=='HighTempLowHumidity'] = 'HTLH'
FHW_ychange_gg_df$Type[FHW_ychange_gg_df$Type=='PostRainScorch'] = 'PRGW'


FHW_ychange_gg = ggplot(FHW_ychange_gg_df[FHW_ychange_gg_df$Type != "DryWind" & FHW_ychange_gg_df$model == 'TSCD',], 
                        aes(x = year, y = Estimate, color = Type)) +
  # 1. 添加 95% 误差棒 (1.96 * 标准误)
  geom_errorbar(aes(ymin = Estimate -  Std..Error, ymax = Estimate +  Std..Error), 
                width = 0.3, alpha = 0.7, size = 0.5) + 
  geom_line(size = 0.5, alpha = 0.6) +  # 折线
  geom_point(size = 1.5) +              # 建议加上数据点，让误差棒的中心更清晰
  scale_color_manual(name = '', values = c(brewer.pal(8, "Dark2")[c(2,1)])) +
  scale_linetype_manual(name = '', values = c(1,5)) +
  theme_bw(base_size = 9) + 
  labs(x = "Release year of variety",
       y = bquote(beta["HDW"] ~ "(kg" ~ ha^-1 ~ hour^-1 * ")"),
       subtitle = "(b)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  theme(plot.subtitle = element_text(size = 10,                      # Font size
                                     hjust = 0.02,                   # Horizontal adjustment
                                     vjust = 1.5,                    # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 7),
        axis.text.x = element_text(size = 7, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.position = c(0.15,0.96),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 4),
        # panel.grid = element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

emf('.../Figure 4.emf',
     units = "cm", width=16, height=12,  pointsize = 11)
ggdraw() +
  draw_plot(county_sens_change_sum_gg,   x = 0, y = 0,  width = 0.6, height = 1)+
  draw_plot(sensity_change_sp_gg, x = 0.58, y = -0.02,  width = 0.42, height =1.2)
dev.off()


emf('.../Figures/Figure S25.emf',
    units = "cm", width=14.6, height=8,  pointsize = 11)
ggdraw() +
  draw_plot(field_sens_change_sum_gg,   x = 0, y = 0,  width = 0.5, height = 1)+
  draw_plot(FHW_ychange_gg, x = 0.5, y = 0,  width = 0.5, height =1)
dev.off()

