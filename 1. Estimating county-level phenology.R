library(readxl)
library(raster)
library(lubridate)
library(sp)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(ggpmisc)
library(cowplot)
library(devEMF)
setwd('E:/Crop yield loss at DHW/')

County_yield  = read_xlsx('County_yield.xlsx')
#####################
#################### model fite
########################

Y_Mean = County_yield %>% group_by(site) %>% summarise_at(vars("Yield_Y"),list(mean,sd));

Y_Mean_sf = left_join(help_data_sf, Y_Mean, by = c('site'))


Y_mean_gg = ggplot() +geom_sf(data = Y_Mean_sf,aes(fill=fn1), 
                             linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(11, "YlGn")),
                    breaks =  c(seq(0,8000,1000)),
                    labels =  c(seq(0,8000,1000)),
                    limits = c(0,8000),na.value = "white",
                    values = scales::rescale(c(seq(0,8000,1000))),
                    name = bquote((kg~ha^-1)))+
  theme_bw()+labs(subtitle = '(a) 1981-2018 Mean yield')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.2,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'left',#c(0.60, 1.08),
        #legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.60,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5.0),
        legend.title= element_text(size = 6.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


YYS_Mean = County_yield %>% group_by(year) %>% summarise_at(vars("Yield_Y"),list(mean,sd))

YYS_Mean_gg = ggplot(YYS_Mean, aes(x=year, y=fn1)) + 
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), width=.1) +
  ylab(bquote('1981-2018 yield'~(kg~ha^-1)))+xlab('Year')+
  geom_line() + geom_point()+labs(subtitle = '(b)')+
  scale_color_brewer(palette="Paired")+theme_bw()+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(2.2,'cm'),
        axis.title = element_text(size = 8.0),
        axis.text = element_text(size = 7.0),
        legend.position = c(0.60, .85),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


emf('...../Figure/Figure S1.emf',
     units = "cm", width=16,height=5, pointsize = 11)
ggdraw() +
  draw_plot(Y_mean_gg,   x = 0,   y = 0.0,  width =0.54, height = 1) +
  draw_plot(YYS_Mean_gg, x = 0.54, y = 0.0,  width =0.46, height = 1)
dev.off()

EXDat = read_xlsx('experiment data.xlsx', sheet = 'Sheet3') 

EXDat = EXDat[EXDat$site %in% County_yield$site,]

EXDat_mer = merge(EXDat[,c(1,5,6,24)], County_yield,  by=c('site','year'))
EXDat_mer = na.omit(EXDat_mer)
EXDat_mer = EXDat_mer[EXDat_mer$Yield_S!=0,]

EXDat_sp = EXDat_mer %>% group_by(site) %>% summarise_at(vars('lon','lat','Yield_S'),list(mean));

coordinates(EXDat_sp) = ~lon+lat
proj4string(EXDat_sp) <- CRS("+init=epsg:4480")
EXDat_sp_sf  =  st_as_sf(EXDat_sp,coords = 1:2)

# st_crs(PNY_Mean_sf)$proj4string = st_transform(PNY_Mean_sf, crs = st_crs(Provience_line))

# plot(wheat_are)
# plot(PNY_Mean,add = T)
EXDat_MY_gg = ggplot() +geom_sf(data = EXDat_sp_sf,aes(color=Yield_S, size = Yield_S), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = bquote((kg~ha^-1)))+
  scale_size_continuous(breaks = c(seq(4000,18000,2000)),
                        range = c(.1,3),name = bquote((kg~ha^-1)))+
  scale_color_stepsn(colors = c(brewer.pal(9, "YlGn")),
                     breaks =  c(seq(4000,18000,2000)),
                     name =bquote((kg~ha^-1)))+
  guides(color= guide_legend(c('Yield_S','Yield_S')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(a) Mean yield of field trials of 2007-2018')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.6,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.89, 0.26),
        #legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 6.0),
        legend.title= element_text(size = 7.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

EXDat_mean = EXDat_mer %>% group_by(year) %>% summarise_at(vars('Yield_S'),list(mean,sd));

EXDat_Mean_gg = ggplot(EXDat_mean, aes(x=year, y=fn1)) + 
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), width=.1) +
  ylab(bquote('2007-2018 yield'~(kg~ha^-1)))+xlab('Year')+
  geom_line() + geom_point()+labs(subtitle = '(b)')+
  scale_color_brewer(palette="Paired")+theme_bw()+
  theme(plot.subtitle = element_text(size = 8,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(2.0,'cm'),
        axis.title = element_text(size = 8.0),
        axis.text = element_text(size = 7.0),
        legend.position = c(0.60, .85),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

emf('...../Figure/Figure S10.emf',
     units = "cm", width=16,height=6, pointsize = 11)
ggdraw() +
  draw_plot(EXDat_MY_gg, x = -0.01, y = 0.0,  width =0.56, height = 1) +
draw_plot(EXDat_Mean_gg, x = 0.54,  y = 0.0,  width =0.46, height = 1)

dev.off()

China_map      = st_read('.../China map/National county-level statistics.shp')
China_line     = st_read('.../China_line.shp')
Provience_line = st_read('.../Provience_line.shp')
China_sea      = st_read('.../China_sea.shp')
Chian_frame    = st_read('.../Chian_frame.shp')
help_data      = readxl::read_xls('.../help_data.xls')


Wheat = readRDS('..../Wheat_re_df.rds')

Wheat = Wheat[c('Yield', "site", 'name', "year", "lon", "lat", "Plant", "JT_das", "HD_das", "MT_das","PTJT_GDD", "JTHD_GDD", "HDMT_GDD", "PTJT_EDD", "JTHD_EDD", "HDMT_EDD", "PTJT_FDD", "JTHD_FDD", "HDMT_FDD", "PTJT_Prec", "JTHD_Prec",  "HDMT_Prec")]

Wheat$JTdate = as.Date(Wheat$Plant)+Wheat$JT_das

Wheat$HDdate = as.Date(Wheat$Plant)+Wheat$HD_das

Wheat$MTdate = as.Date(Wheat$Plant)+Wheat$MT_das

Wheat$PTdate = as.Date(Wheat$Plant)


Wheat$JTdoy = format((Wheat$JTdate), "%m-%d")
Wheat$HDdoy = format((Wheat$HDdate), "%m-%d")
Wheat$MTdoy = format((Wheat$MTdate), "%m-%d")
Wheat$PTdoy = format((Wheat$PTdate), "%m-%d")

##################
##################
##################
Wheat$JT_DOY= yday(Wheat$JTdate)
Wheat$HD_DOY= yday(Wheat$HDdate)
Wheat$MT_DOY= yday(Wheat$MTdate)
Wheat$PT_DOY= yday(Wheat$PTdate)


PNY_Mean = Wheat %>% group_by(site,name) %>% summarise_at(vars('lon','lat',"JT_DOY", 'HD_DOY', 'MT_DOY','PT_DOY'),list(mean));

coordinates(PNY_Mean) = ~lon+lat
proj4string(PNY_Mean) <- CRS("+init=epsg:4480")
PNY_Mean_sf  =  st_as_sf(PNY_Mean,coords = 1:2)

# st_crs(PNY_Mean_sf)$proj4string = st_transform(PNY_Mean_sf, crs = st_crs(Provience_line))

# plot(wheat_are)
# plot(PNY_Mean,add = T)
PT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name!='SW',],
                     aes(color=PT_DOY, size = PT_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(260,310,5)),
                        range = c(.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(260,310,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('PT_DOY','PT_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(a) Planting')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

JT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name!='SW',],
                             aes(color=JT_DOY, size = JT_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(70,120,5)),
                        range = c(.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(70,120,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('JT_DOY','JT_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(b) Jointing')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

HD_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name!='SW',],
                             aes(color=HD_DOY, size = HD_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(100,140,5)),
                        range = c(0.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(100,140,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('HD_DOY','HD_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(c) Heading')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

MT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name!='SW',],
                             aes(color=MT_DOY, size = MT_DOY), 
                             linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(140,200,5)),
                        range = c(0.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(140,200,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('HD_DOY','HD_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '  (d) Maturity')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



emf('.../Figure/Figure S2.emf',
     units = "cm", width=16,height=9.7)

ggdraw() +
  draw_plot(PT_ST_gg, x = -0.01, y = 0.48, width =0.515, height = 0.56) +
  draw_plot(JT_ST_gg, x = 0.48,  y = 0.48, width =0.515, height = 0.56)+
  draw_plot(HD_ST_gg, x = -0.01, y = -0.02, width =0.515, height = 0.56)+
  draw_plot(MT_ST_gg, x = 0.48,  y = -0.02, width =0.515, height = 0.56)
dev.off()




SWPT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name=='SW',],
                               aes(color=PT_DOY, size = PT_DOY), 
                               linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(80,120,5)),
                        range = c(.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(80,120,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('PT_DOY','PT_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(a) Planting')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

SWJT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name=='SW',],
                               aes(color=JT_DOY, size = JT_DOY), 
                               linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(120,170,5)),
                        range = c(.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(120,170,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('JT_DOY','JT_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(b) Jointing')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

SWHD_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name=='SW',],
                               aes(color=HD_DOY, size = HD_DOY), 
                               linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(140,190,5)),
                        range = c(0.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(140,190,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('HD_DOY','HD_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(c) Heading')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

SWMT_ST_gg = ggplot() +geom_sf(data = PNY_Mean_sf[PNY_Mean_sf$name=='SW',],
                               aes(color=MT_DOY, size = MT_DOY), 
                               linewidth = 0.1) +xlim(75.5,133.5)+ylim(30.5,52.8)+
  scale_size(range = c(0,1.0), name = '(doy)')+
  scale_size_continuous(breaks = c(seq(180,240,5)),
                        range = c(0.1,1.0),name = '(doy)')+
  scale_color_stepsn(colors = c(brewer.pal(11, "YlGnBu")),
                     breaks =  c(seq(180,240,5)),
                     name ='(doy)')+
  guides(color= guide_legend(c('HD_DOY','HD_DOY')), fill = "none")+
  guides(color= guide_legend(), size=guide_legend())+
  theme_bw()+labs(subtitle = '(d) Maturity')+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  8.5,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 5),
        legend.key.size = unit(1.0,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.3),
        # legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



emf('.../Figure/Figure S3.emf',
     units = "cm", width=16,height=9.7)

ggdraw() +
  draw_plot(SWPT_ST_gg, x = -0.01, y = 0.48, width =0.515, height = 0.56) +
  draw_plot(SWJT_ST_gg, x = 0.48,  y = 0.48, width =0.515, height = 0.56)+
  draw_plot(SWHD_ST_gg, x = -0.01, y = -0.02, width =0.515, height = 0.56)+
  draw_plot(SWMT_ST_gg, x = 0.48,  y = -0.02, width =0.515, height = 0.56)
dev.off()

####################
###################step 2 calculated climate variables for RF models training
###################

stations                 = readxl::read_xls('.../sitstation.xls')
colnames(stations)       = stations[2,]         ;stations     = stations[-c(1:2),]

colnames(stations)[1:2] = c('Provience', 'site'); Wheat$site   = as.numeric(Wheat$site);

stations$site  = as.numeric(stations$site)      ; Wheat        = left_join(stations[,1:2], Wheat, by = c('site'))

Wheat = Wheat %>% distinct() %>% na.omit()      ; colnames(Wheat)[1] = 'Provience'

Wheat_margin = Wheat %>% group_by(Provience) %>% 
  summarise_at(vars('JTdoy','HDdoy','MTdoy','PTdoy'),list(max,min));

site          =  unique(Wheat$site) 


Widow_df = NULL
for(i in 1:length(site)){
  
  files = list.dirs('Data/ERA5/', full.names = T)
  temp_loc = Wheat[Wheat$site == site[i], ]
  coordinates(temp_loc) = ~  lon +lat
  
  T2m = NULL
  for (j in 2:length(files)) {
    
    path  = paste0(files[j] ,'/data_stream-oper.nc')
    t2m   = brick(path, varname = 't2m')
    temp_t2m = data.frame(t(extract(t2m, temp_loc))[,1])
    colnames(temp_t2m)[1] = 'value'
    
    temp_t2m$date = as_datetime(c(t2m@z$`valid_time (seconds since 1970-01-01)`))
    
    T2m  = rbind(T2m, temp_t2m) 
    
  }
  
  
  pathp = '.../CHM_PRE_V2_daily/'
  pathps = list.files(pathp) 
  
  Prceps = NULL
  for (k in 17:55) {
    prec_root = paste0(pathp,pathps[k])
    
    prec = brick(prec_root)
    
    temp_prec = data.frame(t(extract(prec, temp_loc))[,1])
    colnames(temp_prec)[1] = 'value'
    
    temp_prec$date = as_datetime(c(prec@z$Date))
    
    Prceps = rbind(Prceps, temp_prec)
  }
  
  temp_lox = Wheat[Wheat$site == site[i], ]
  
  temp_margin = Wheat_margin[Wheat_margin$Provience==temp_lox$Provience[1],]
  
  TP = NULL
  for (n in 1:length(temp_lox$Provience)) {
    
    if(temp_margin$JTdoy_fn1>temp_margin$JTdoy_fn2&temp_margin$JTdoy_fn1>="10-01"){
      year1 = temp_lox$year[n]-1
      year2 = temp_lox$year[n] 
    }else{
      year1 = temp_lox$year[n]
      year2 = temp_lox$year[n] 
    }
    
    JT_int =  which(T2m$date %within% interval(as.Date(paste0(year1,'-',temp_margin$JTdoy_fn1)), 
                                               as.Date(paste0(year2,'-',temp_margin$JTdoy_fn2))))
    
    HD_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn2))))
    
    MT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn2))))
    
    PT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn2))))
   
    
    JT_inp =  which(Prceps$date %within% interval(as.Date(paste0(year1,'-',temp_margin$JTdoy_fn1)), 
                                                  as.Date(paste0(year2,'-',temp_margin$JTdoy_fn2))))
    
    HD_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn2))))
    
    MT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn2))))
    
    PT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn2))))
    
   
    
    JT_t = mean(T2m$value[JT_int]-273.15)
    HD_t = mean(T2m$value[HD_int]-273.15)
    MT_t = mean(T2m$value[MT_int]-273.15)
    PT_t = mean(T2m$value[PT_int]-273.15)
   
    JT_p = sum(Prceps$value[JT_inp])
    HD_p = sum(Prceps$value[HD_inp])
    MT_p = sum(Prceps$value[MT_inp])
    PT_p = sum(Prceps$value[PT_inp])
     
    temp_TP = cbind(JT_t,HD_t,MT_t,PT_t, 
                    JT_p, HD_p,MT_p,PT_p)
    
    TP = rbind(TP, temp_TP)
    
  }
  
  temp_lox = cbind(temp_lox, TP)
  
  Widow_df = rbind(Widow_df, temp_lox)
}


#####################################phenology RF model training 
#####################################phenology RF model training 
#####################################phenology RF model training 
#####################################phenology RF model training 

library(randomForest)

Widow_df$JTDoy = yday(Widow_df$JTdate)
WWJTdoyrf <- randomForest(JTDoy~JT_p+JT_t+lat+year, Widow_df[Widow_df$name=='WW',])


Widow_df$HDDoy = yday(Widow_df$HDdate)
WWHDdoyrf <- randomForest(HDDoy~HD_p+HD_t+lat+year, Widow_df[Widow_df$name=='WW',])

Widow_df$MTDoy = yday(Widow_df$MTdate)
WWMTdoyrf <- randomForest(MTDoy~MT_p+MT_t+lat+year, Widow_df[Widow_df$name=='WW',])


Widow_df$PTDoy = yday(Widow_df$PTdate)
WWPTdoyrf <- randomForest(PTDoy~PT_p+PT_t+lat+year, Widow_df[Widow_df$name=='WW',])


# Widow_df$MKDoy = yday(Widow_df$Milk_stage)
# WWMKdoyrf <- randomForest(MKDoy~MK_p+MK_t+lat+year, Widow_df[Widow_df$name=='WW',])
# 
# Widow_df$DTDoy = yday(Widow_df$dough_stage)
# WWDTdoyrf <- randomForest(DTDoy~DT_p+DT_t+lat+year, Widow_df[Widow_df$name=='WW',])


###################SWRF
Widow_df$JTDoy = yday(Widow_df$JTdate)
SWJTdoyrf <- randomForest(JTDoy~JT_p+JT_t+lat+year, Widow_df[Widow_df$name=='SW',])


Widow_df$HDDoy = yday(Widow_df$HDdate)
SWHDdoyrf <- randomForest(HDDoy~HD_p+HD_t+lat+year, Widow_df[Widow_df$name=='SW',])

Widow_df$MTDoy = yday(Widow_df$MTdate)
SWMTdoyrf <- randomForest(MTDoy~MT_p+MT_t+lat+year, Widow_df[Widow_df$name=='SW',])


Widow_df$PTDoy = yday(Widow_df$PTdate)
SWPTdoyrf <- randomForest(PTDoy~PT_p+PT_t+lat+year, Widow_df[Widow_df$name=='SW',])

# Widow_df$MKDoy = yday(Widow_df$Milk_stage)
# SWMKdoyrf <- randomForest(MKDoy~MK_p+MK_t+lat+year, Widow_df[Widow_df$name=='SW',])
# 
# Widow_df$DTDoy = yday(Widow_df$dough_stage)
# SWDTdoyrf <- randomForest(DTDoy~DT_p+DT_t+lat+year, Widow_df[Widow_df$name=='SW',])

nameRF = randomForest(factor(name)~Provience+lat+lon+PT_p+PT_t+JT_p+JT_t+HD_p+HD_t+MT_p+MT_t+year, Widow_df,proximity=TRUE)

###############RF train plot
Ph_gg_df = data.frame(predicted = c(WWPTdoyrf$predicted, WWJTdoyrf$predicted,WWHDdoyrf$predicted,WWMTdoyrf$predicted,
                                    SWPTdoyrf$predicted, SWJTdoyrf$predicted,SWHDdoyrf$predicted,SWMTdoyrf$predicted), 
                      Obs = c(Widow_df[Widow_df$name=='WW','PTDoy'], Widow_df[Widow_df$name=='WW','JTDoy'],
                              Widow_df[Widow_df$name=='WW','HDDoy'], Widow_df[Widow_df$name=='WW','MTDoy'],
                              Widow_df[Widow_df$name=='SW','PTDoy'], Widow_df[Widow_df$name=='SW','JTDoy'],
                              Widow_df[Widow_df$name=='SW','HDDoy'], Widow_df[Widow_df$name=='SW','MTDoy']),
                      stage = c(rep(c('Planting (doy)','Jointing (doy)','Heading (doy)','Maturity (doy)'),each=length(WWPTdoyrf$predicted)),
                                rep(c('Planting (doy)','Jointing (doy)','Heading (doy)','Maturity (doy)'),each=length(SWPTdoyrf$predicted))),
                      group = c(rep('Winter wheat', length(WWPTdoyrf$predicted)*4),rep('Spring wheat', length(SWPTdoyrf$predicted)*4)))

ph_rf_gg = ggplot(data = Ph_gg_df, aes(x = Obs, y = predicted,fill = stage,color = stage)) +
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = stage))+
  geom_point(alpha = 0.3, size =0.6) +xlab('Observations') +ylab('Predictions')+
  geom_smooth(method = "lm", se=T,color = 'gray75', formula = y ~ x) +
  scale_fill_manual(name = '',values = brewer.pal(8, "Dark2"))+
  scale_color_manual(name = '',values = brewer.pal(8, "Dark2"))+
  stat_poly_line(color = 'gray75') +theme_bw()+
  stat_poly_eq(use_label(c("eq", "R2")),color = 'black',size = 2) +
  facet_wrap(stage~group, scales = 'free',ncol = 4)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1.45,'cm'),
        # axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        legend.position = 'none',
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.18,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 6),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

# HDW_type_YTrend_gg

emf('.../Figure/Figure S13.emf',
     units = "cm", width=15, height=10)
ph_rf_gg
dev.off()


###############
############### step 4 Calculated variables for predicting phenology with RF Models 
###############

wheat_are = raster('.../Crop area.tif')
plot(wheat_are)

stations       = readxl::read_xls('.../sitstation.xls') 
colnames(stations)       = stations[2,]
stations       = stations[-c(1:2),]

colnames(stations)[c(1:2,9:10)] = c('Provience', 'site','lat','lon')

stations$site  = as.numeric(stations$site)
stations       = na.omit(stations)

coordinates(stations) = ~  lon +lat
plot(stations, add = T) 

wheat_stations = data.frame(extract(wheat_are, stations)) 

wheat_sts      = cbind(wheat_stations, stations)



wheat_sts = wheat_sts[!(wheat_sts$site %in% site), ]

Incer_windF = function(i){
  library(readxl)
  library(raster)
  library(lubridate)
  library(sp)
  library(tidyverse)
  library(tidyr)
  detach("package:tidyr", unload = TRUE)
  
  site =  unique(site_lat_lon$site)
  setwd('E:/Crop yield loss at DHW/')
  
  files = list.dirs('.../Data/ERA5/', full.names = T)
  
  temp_loc = site_lat_lon[site_lat_lon$site == site[i], ]
  
  coordinates(temp_loc) = ~  lon +lat
  
  T2m = NULL
  for (j in 2:length(files)) {
    
    path  = paste0(files[j] ,'/data_stream-oper.nc')
    t2m   = brick(path, varname = 't2m')
    temp_t2m = data.frame(t(extract(t2m, temp_loc))[,1])
    colnames(temp_t2m)[1] = 'value'
    
    temp_t2m$date = as_datetime(c(t2m@z$`valid_time (seconds since 1970-01-01)`))
    
    T2m  = rbind(T2m, temp_t2m) 
    
  }
  
  pathp = '..../Data/CHM_PRE_V2_daily/'
  pathps = list.files(pathp) 
  
  Prceps = NULL
  for (k in 17:55) {
    prec_root = paste0(pathp,pathps[k])
    
    prec = brick(prec_root)
    
    temp_prec = data.frame(t(extract(prec, temp_loc))[,1])
    colnames(temp_prec)[1] = 'value'
    
    temp_prec$date = as_datetime(c(prec@z$Date))
    
    Prceps = rbind(Prceps, temp_prec)
  }
  
  temp_lox = site_lat_lon[site_lat_lon$site == site[i], ]#wheat_sts[wheat_sts$site == site[i], ]
 
  temp_lox = eval(parse(text = paste0('rbind(', paste0(rep('temp_lox',38),collapse = ','),')')))
  
  temp_lox$year = 1981:2018
  temp_margin = Wheat_margin[Wheat_margin$Provience==temp_lox$Provience[1],]
  
  TP = NULL
  for (n in 1:length(temp_lox$Provience)) {
    
    if(temp_margin$JTdoy_fn1>temp_margin$JTdoy_fn2&temp_margin$JTdoy_fn1>="10-01"){
      year1 = temp_lox$year[n]-1
      year2 = temp_lox$year[n] 
    }else{
      year1 = temp_lox$year[n]
      year2 = temp_lox$year[n] 
    }
    
    JT_int =  which(T2m$date %within% interval(as.Date(paste0(year1,'-',temp_margin$JTdoy_fn1)), 
                                               as.Date(paste0(year2,'-',temp_margin$JTdoy_fn2))))
    
    HD_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn2))))
    
    MT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn2))))
    
    PT_int =  which(T2m$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn1)), 
                                               as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn2))))
    
    
    
    JT_inp =  which(Prceps$date %within% interval(as.Date(paste0(year1,'-',temp_margin$JTdoy_fn1)), 
                                                  as.Date(paste0(year2,'-',temp_margin$JTdoy_fn2))))
    
    HD_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n],'-',temp_margin$HDdoy_fn2))))
    
    MT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n],'-',temp_margin$MTdoy_fn2))))
    
    PT_inp =  which(Prceps$date %within% interval(as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn1)), 
                                                  as.Date(paste0(temp_lox$year[n]-1,'-',temp_margin$PTdoy_fn2))))
 

    JT_t = mean(T2m$value[JT_int]-273.15)
    HD_t = mean(T2m$value[HD_int]-273.15)
    MT_t = mean(T2m$value[MT_int]-273.15)
    PT_t = mean(T2m$value[PT_int]-273.15)
    
    JT_p = sum(Prceps$value[JT_inp])
    HD_p = sum(Prceps$value[HD_inp])
    MT_p = sum(Prceps$value[MT_inp])
    PT_p = sum(Prceps$value[PT_inp])
     
    temp_TP = cbind(JT_t,HD_t,MT_t,PT_t,
                    JT_p, HD_p,MT_p,PT_p)
    
    TP = rbind(TP, temp_TP)
    
  }
  
  temp_lox = cbind(temp_lox, TP)
}

Variables_RH_py = NULL
for (i in 1:length(site_lat_lon$site)) {
  temp = Incer_windF(i)
  Variables_RH_py = rbind(Variables_RH_py, temp)
}

getwd()
write.csv(Variables_RH_py,'..../Variables_RH_py.csv')

########################################## phenology predictions
########################################## phenology predictions
########################################## phenology predictions

Variables_RH_py$name = predict(nameRF,Variables_RH_py)


Variables_RH_py$JTDoy =  ifelse(Variables_RH_py$name=='SW',
                              round(predict(SWJTdoyrf,Variables_RH_py),0),
                              round(predict(WWJTdoyrf,Variables_RH_py),0))

Variables_RH_py$HDDoy =  ifelse(Variables_RH_py$name=='SW',
                              round(predict(SWHDdoyrf,Variables_RH_py),0),
                              round(predict(WWHDdoyrf,Variables_RH_py)))

Variables_RH_py$MTDoy =  ifelse(Variables_RH_py$name=='SW',
                              round(predict(SWMTdoyrf,Variables_RH_py),0),
                              round(predict(WWMTdoyrf,Variables_RH_py),0))

Variables_RH_py$PTDoy =  ifelse(Variables_RH_py$name=='SW',
                              round(predict(SWPTdoyrf,Variables_RH_py),0),
                              round(predict(WWPTdoyrf,Variables_RH_py),0))



Variables_RH_py$JTdate = as.Date(Variables_RH_py$JTDoy, origin = as.Date(paste0(Variables_RH_py$year,'-01-01')))
Variables_RH_py$HDdate = as.Date(Variables_RH_py$HDDoy, origin = as.Date(paste0(Variables_RH_py$year,'-01-01')))
Variables_RH_py$MTdate = as.Date(Variables_RH_py$MTDoy, origin = as.Date(paste0(Variables_RH_py$year,'-01-01')))
Variables_RH_py$PTdate = ifelse(Variables_RH_py$name=='SW',
                              as.character(as.Date(Variables_RH_py$PTDoy, 
                                                   origin = as.Date(paste0(Variables_RH_py$year,'-01-01')))),
                              as.character(as.Date(Variables_RH_py$PTDoy, 
                                                   origin = as.Date(paste0(c(Variables_RH_py$year-1),'-01-01')))))



Variables_RH_py$Plant  = as.Date(as.character(Variables_RH_py$PTdate))


WWPhenology_mean = Variables_RH_py[Variables_RH_py$Variables_RH_py$name=="WW",] %>% group_by(site) %>% summarise_at(vars("PTDoy","JTDoy","HDDoy","MTDoy"),list(mean));
help_data         = cbind(help_data, China_map$geometry)
help_data_sf      = st_as_sf(help_data)
PT_mean_sf = left_join(help_data_sf, WWPhenology_mean[,c(1,2)], by = c('site'))

WWPTdoy_mean_gg =   ggplot() +geom_sf(data = PT_mean_sf,aes(fill=PTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(259,315,5)), labels =  c(seq(259,315,5)),
                    limits = c(259,315),na.value = "white",
                    values = scales::rescale(seq(259,315,5)),name = '(doy)')+
  labs(subtitle = bquote((a)~Planting))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                     # Vertical adjustment
                                     lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


JT_mean_sf = left_join(help_data_sf, WWPhenology_mean[,c(1,3)], by = c('site'))

WWJTdoy_mean_gg =   ggplot() +geom_sf(data = JT_mean_sf,aes(fill=JTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(50,132,5)), labels =  c(seq(50,132,5)),
                    limits = c(50,132),na.value = "white",
                    values = scales::rescale(seq(50,132,5)),name = '(doy)')+
  labs(subtitle = bquote((b)~Jointing))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                     # Vertical adjustment
                                     lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


HD_mean_sf = left_join(help_data_sf, WWPhenology_mean[,c(1,4)], by = c('site'))

WWHDdoy_mean_gg =   ggplot() +geom_sf(data = HD_mean_sf,aes(fill=HDDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(90,160,5)), labels =  c(seq(90,160,5)),
                    limits = c(90,160),na.value = "white",
                    values = scales::rescale(seq(90,160,5)),name = '(doy)')+
  labs(subtitle = bquote((c)~Heading))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                      # Vertical adjustment
                                     lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


MT_mean_sf = left_join(help_data_sf, WWPhenology_mean[,c(1,5)], by = c('site'))

WWMTdoy_mean_gg =   ggplot() +geom_sf(data = MT_mean_sf,aes(fill=MTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(130,205,5)), labels =  c(seq(130,205,5)),
                    limits = c(130,205),na.value = "white",
                    values = scales::rescale(seq(130,205,5)),name = '(doy)')+
  labs(subtitle = bquote((d)~Maturity))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 4.5,                     # Vertical adjustment
                                     lineheight = 3),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


emf('.../Figure/Figure S14.emf',
     units = "cm", width=16,height=9.7)
ggdraw() +
  draw_plot(WWPTdoy_mean_gg, x = -0.01, y = 0.46, width =0.515, height = 0.56) +
  draw_plot(WWJTdoy_mean_gg, x = 0.48,  y = 0.46, width =0.515, height = 0.56)+
  draw_plot(WWHDdoy_mean_gg, x = -0.01, y = -0.04, width =0.515, height = 0.56)+
  draw_plot(WWMTdoy_mean_gg, x = 0.48,  y = -0.04, width =0.515, height = 0.56)
dev.off() 


############
############
###########

SWPhenology_mean = Variables_RH_py[Variables_RH_py&Variables_RH_py$name=="SW",] %>% group_by(site) %>% summarise_at(vars("PTDoy","JTDoy","HDDoy","MTDoy"),list(mean));

SPT_mean_sf = left_join(help_data_sf, SWPhenology_mean[,c(1,2)], by = c('site'))

SWPTdoy_mean_gg =   ggplot() +geom_sf(data = SPT_mean_sf,aes(fill=PTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(60,123,5)), labels =  c(seq(60,123,5)),
                    limits = c(60,123),na.value = "white",
                    values = scales::rescale(seq(60,123,5)),name = '(doy)')+
  labs(subtitle = bquote((a)~Planting))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                     # Vertical adjustment
                                     lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


SJT_mean_sf = left_join(help_data_sf, SWPhenology_mean[,c(1,3)], by = c('site'))

SWJTdoy_mean_gg =   ggplot() +geom_sf(data = SJT_mean_sf,aes(fill=JTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(114,174,5)), labels =  c(seq(114,174,5)),
                    limits = c(114,174),na.value = "white",
                    values = scales::rescale(seq(114,174,5)),name = '(doy)')+
  labs(subtitle = bquote((b)~Jointing))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                     # Vertical adjustment
                                     lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


SHD_mean_sf = left_join(help_data_sf, SWPhenology_mean[,c(1,4)], by = c('site'))

SWHDdoy_mean_gg =   ggplot() +geom_sf(data = SHD_mean_sf,aes(fill=HDDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(130,195,5)), labels =  c(seq(130,195,5)),
                    limits = c(130,195),na.value = "white",
                    values = scales::rescale(seq(130,195,5)),name = '(doy)')+
  labs(subtitle = bquote((c)~Heading))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust =  4.5,                      # Vertical adjustment
                                     lineheight = 1),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


SMT_mean_sf = left_join(help_data_sf, SWPhenology_mean[,c(1,5)], by = c('site'))

SWMTdoy_mean_gg =   ggplot() +geom_sf(data = SMT_mean_sf,aes(fill=MTDoy), 
                                      linewidth = 0.1, color = 'gray85') +
  xlim(-2500000,2000000)+ylim(4000000,6300000)+
  scale_fill_stepsn(colors = c(RColorBrewer::brewer.pal(9, "Blues")),
                    breaks =  c(seq(165,250,5)), labels =  c(seq(165,250,5)),
                    limits = c(165,250),na.value = "white",
                    values = scales::rescale(seq(165,250,5)),name = '(doy)')+
  labs(subtitle = bquote((d)~Maturity))+theme_bw()+
  geom_sf(data=China_line, color="grey65", linewidth = 0.2)+
  geom_sf(data=China_sea, color="grey65", linewidth = 0.2)+
  geom_sf(data=Provience_line, color="grey50", linewidth = 0.2)+
  geom_sf(data=Chian_frame, color="grey65", linewidth = 0.2)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 4.5,                     # Vertical adjustment
                                     lineheight = 3),
        strip.text = element_text(size = 5),
        legend.key.size = unit(0.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.61, 1.09),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.15,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4.0),
        legend.title= element_text(size = 5.0),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


emf('.../Figure S15.emf',
     units = "cm", width=16,height=9.7)
ggdraw() +
  draw_plot(SWPTdoy_mean_gg, x = -0.01, y = 0.46, width =0.515, height = 0.56) +
  draw_plot(SWJTdoy_mean_gg, x = 0.48,  y = 0.46, width =0.515, height = 0.56)+
  draw_plot(SWHDdoy_mean_gg, x = -0.01, y = -0.04, width =0.515, height = 0.56)+
  draw_plot(SWMTdoy_mean_gg, x = 0.48,  y = -0.04, width =0.515, height = 0.56)
dev.off()


library(ggridges)
Wheat_DHW_temp = Wheat_DHW
colnames(Wheat_DHW_temp)[16] = 'year'
Wheat_DHW_temp$site  =  as.character(Wheat_DHW_temp$site)

Data_HDW_PY = Wheat_county_merged_final[,c("site","year","JTdate","HDdate", "MTdate","PTdate")] %>% 
  left_join(Wheat_DHW_temp, by = c('site', 'year'))

class()

Data_HDW_PY$HDW_occurrence = Data_HDW_PY$Date-Data_HDW_PY$PTdate
Data_HDW_PY$JT = Data_HDW_PY$JTdate-Data_HDW_PY$PTdate
Data_HDW_PY$HD = Data_HDW_PY$HDdate-Data_HDW_PY$PTdate
Data_HDW_PY$MT = Data_HDW_PY$MTdate-Data_HDW_PY$PTdate

Data_HDW_PY$group = ifelse(Data_HDW_PY$year>2008,'2009-2018','1981-2008')
Data_HDW_PY_nao = na.omit(Data_HDW_PY)



#  
df_long <- Data_HDW_PY_nao %>%
  mutate(across(c(HDW_occurrence, JT, HD, MT),
                ~ as.numeric(.))) %>%
  pivot_longer(cols = c(HDW_occurrence, JT, HD, MT),
               names_to = "Variable",
               values_to = "Days")
df_long <- df_long %>%
  mutate(
    wtype = case_when(
      Variable == "MT" & Days < 150 ~ "Spring wheat",
      Variable == "HDW_occurrence" & Days < 150 ~ "Spring wheat",
      Variable == "HD" & Days < 150 ~ "Spring wheat",
      Variable == "JT" & Days < 100 ~ "Spring wheat",
      TRUE ~ "Winter wheat"
    )
  )

#  
df_long <- df_long %>%
  mutate(Type = recode(Type,
                       "HighTempLowHumidity" = "HDWHTLH",
                       "PostRainScorch"      = "HDWPRGW",
                       "DryWind"             = "HDWDTWD"))

#  
df_long <- df_long %>%
  mutate(Type_label = case_when(
    Type == "HDWHTLH" ~ "HDW[HTLH]",
    Type == "HDWPRGW" ~ "HDW[PRGW]",
    Type == "HDWDTWD" ~ "HDW[DTWD]"
  ))
df_long$Type_label = factor(df_long$Type_label,levels = c( "HDW[HTLH]", "HDW[PRGW]", "HDW[DTWD]"))
#ggplot(weather_atl, aes(x = windSpeed, y = fct_rev(Month), fill = Month)) +
# 自定义 group 颜色（按你的实际 group 名称来改）
my_cols <- c("1981-2008" = "#1b9e77",
             "2009-2018" = "#e55f09")

ridge_scale <- 0.7

#  
x_range_df <- df_long %>%
  group_by(Variable, Type_label, wtype) %>%
  summarise(xmin = min(Days, na.rm = TRUE),
            xmax = max(Days, na.rm = TRUE),
            .groups = 'drop')

median_df <- df_long %>%
  group_by(Type_label, Variable,  group, wtype) %>%
  summarise(value = median(Days, na.rm = TRUE),
            .groups = 'drop')

median_df <- median_df %>%
  mutate(
    x_pos = case_when(
      wtype == "Spring wheat" & group == "1981-2008" ~ 140,
      wtype == "Spring wheat" & group == "2009-2018" ~ 40,
      wtype == "Winter wheat" & group == "1981-2008" ~ 280,
      wtype == "Winter wheat" & group == "2009-2018" ~ 150,
      TRUE ~ NA_real_
    ),
    hjust = case_when(
      group %in% c("1981-2008") ~ 0,
      group %in% c("2009-2018") ~ 1,
      TRUE ~ 0.5
    )
  )

 
df_long <- df_long %>%
  mutate(Variable = recode(Variable,
                           "HDW_occurrence" = "HDW occurrence",
                           "JT" = "JT",
                           "HD" = "HD",
                           "MT" = "MT"),
         Variable = factor(Variable, levels = c("HDW occurrence", "JT", "HD", "MT"))
  )


emf('.../Figures/Figure S21.emf',
     units = "cm", width=18, height= 14, pointsize = 11)

ggplot(df_long, aes(x = Days, y = fct_rev(Variable), fill = group, color = group)) +
  geom_density_ridges(alpha = 0.4, scale = ridge_scale,
                      quantile_lines = TRUE, quantiles = 2) +
  geom_text(data = median_df,
            aes(x = x_pos, y = as.numeric(fct_rev(Variable)) + 0.3,
                label = value,inherit.aes = FALSE, 
                color = group),
            size = 3,
            show.legend = FALSE)+
  facet_grid(Type_label ~ wtype, scales = "free",
             labeller = labeller(Type_label = label_parsed)) +
  xlab('Day after planting (days)')+ylab("")+
  scale_fill_manual(values = my_cols,name = '') +
  scale_color_manual(values = my_cols,name = '') +
  #guides(fill = guide_legend(title = "Group")) +
  theme_bw()+
  theme(plot.subtitle = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 8.5),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.position = c(.92, 0.04),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 7),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent", colour = NA_character_)
  )

dev.off()




