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


Wheat_county_merged_final <- Wheat_county_merged_final %>%
  
  mutate(is_1981_1988 = year >= 1981 & year <= 1988) %>%
  group_by(Provience) %>%
  arrange(year) %>%
  mutate(across(
    c(Seed_Cost, Fertilizer_Cost, Manure_Cost, Pesticide_Cost, 
      Mechanization_Cost, Irrigation_Cost,
      Adj_Seed_Cost, Adj_Fertilizer_Cost,
      Adj_Mechanization_Cost, Adj_Irrigation_Cost),
    ~ {
       
      if (all(is.na(.x))) return(.x)
      
      
      temp_vals <- .x
      
      
      temp_vals[is_1981_1988] <- NA
      
       
      filled <- zoo::na.approx(temp_vals, na.rm = FALSE)
      
      
      if (any(is.na(filled))) {
        non_na <- which(!is.na(filled))
        if (length(non_na) >= 2) {
          
          x_vals <- 1:length(filled)
          filled <- approx(x = x_vals[non_na], 
                           y = filled[non_na],
                           xout = x_vals,
                           rule = 2)$y
        }
      }
      
      
      final_vals <- ifelse(is_1981_1988 | is.na(.x), filled, .x)
      
      return(final_vals)
    }
  )) %>%
  ungroup() %>%
  select(-is_1981_1988)
 
Wheat_county_merged_final_SD = Wheat_county_merged_final %>% mutate_at(c(colnames(Wheat_county_merged_final)[c(4:11, 18:127)]), ~(scale(.) %>% as.vector))
 target_cols <- colnames(Wheat_county_merged_final)[c(4:11, 18:130)]
 

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

Ta_vars = c('PJGDDNHDW' , 'JHGDDNHDW' , 'HMGDDNHDW' , 'JHEDDNHDW' , 'HMEDDNHDW' , 
                 'PJFDDNHDW' , 'JHFDDNHDW')

Ta_varsS = c('PJGDD' , 'JHGDD' , 'HMGDD' , 'JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')

HDW_vars = c('cumulative_hours_DryWind_Early',
             'cumulative_hours_DryWind_Late','cumulative_hours_DryWind_Middle',
             'cumulative_hours_HighTempLowHumidity_Early', 'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle', 'cumulative_hours_PostRainScorch_Early',
             'cumulative_hours_PostRainScorch_Late', 'cumulative_hours_PostRainScorch_Middle')

Per_vars = c('PT_JT_total_rainfall', 'JT_HD_total_rainfall', 'HD_MT_total_rainfall',
             'PT_JT_total_rainfall2', 'JT_HD_total_rainfall2', 'HD_MT_total_rainfall2')


Cost_vars = c("Adj_Seed_Cost",          "Adj_Fertilizer_Cost",                                                       
              "Adj_Mechanization_Cost", "Adj_Irrigation_Cost" )

vars <- c(Ta_vars,Soil_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula1    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Soil_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula2    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Soil_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula3    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|site + year"))

vars <- c(Ta_vars,Per_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
fmula4    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Per_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula5    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Per_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula6    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))



# 
BootReg    = function(fmula, boot, data, size){
  
  
  bootcoefs = c()
 
  Rsq       = NULL
  for (n in 1:boot) {
    
    rsamp_data = sample(1:nrow(data), size=size, replace = T)  
    
    rsamp_data = data[rsamp_data,]
    
    cropfit    = feols(fmula, data = rsamp_data) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
 
  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean));
  
  Mbootcoef$rsq  = mean(Rsq)
  
  return(Mbootcoef)
}
 
set.seed(1234)
 
HDW_yield_fit_1 =   BootReg(fmula1, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_2 =   BootReg(fmula2, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_3 =   BootReg(fmula3, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_4 =   BootReg(fmula4, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_5 =   BootReg(fmula5, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_6 =   BootReg(fmula6, 10000, Wheat_county_merged_final_SD, 60000)  


HDW_yield_fit_1$Model  = 'TSMM'
HDW_yield_fit_2$Model  = 'TSDM'
HDW_yield_fit_3$Model  = 'TSCD'
HDW_yield_fit_4$Model  = 'TPMM'
HDW_yield_fit_5$Model  = 'TPDM'
HDW_yield_fit_6$Model  = 'TPCD'

HDW_yield_fit_1$Y_levle = 'County'
HDW_yield_fit_2$Y_levle = 'County'
HDW_yield_fit_3$Y_levle = 'County'
HDW_yield_fit_4$Y_levle = 'County'
HDW_yield_fit_5$Y_levle = 'County'
HDW_yield_fit_6$Y_levle = 'County'

HDW_yield_fit_1$HDW_sou = 'NHDW'
HDW_yield_fit_2$HDW_sou = 'NHDW'
HDW_yield_fit_3$HDW_sou = 'NHDW'
HDW_yield_fit_4$HDW_sou = 'NHDW'
HDW_yield_fit_5$HDW_sou = 'NHDW'
HDW_yield_fit_6$HDW_sou = 'NHDW'

HDW_yield_fit_df = rbind(HDW_yield_fit_1,
                         HDW_yield_fit_2,
                         HDW_yield_fit_3,
                         HDW_yield_fit_4,
                         HDW_yield_fit_5,
                         HDW_yield_fit_6)

vars <- c(Ta_varsS,Soil_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula1    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Soil_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula2    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Soil_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
fmula3    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Per_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula4    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Per_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula5    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Per_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula6    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})

set.seed(1234)

 
HDW_yield_fit_1W =   BootReg(fmula1, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_2W =   BootReg(fmula2, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_3W =   BootReg(fmula3, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_4W =   BootReg(fmula4, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_5W =   BootReg(fmula5, 10000, Wheat_county_merged_final_SD, 60000)  
HDW_yield_fit_6W =   BootReg(fmula6, 10000, Wheat_county_merged_final_SD, 60000)  
 

HDW_yield_fit_1W$Model  = 'TSMM'
HDW_yield_fit_2W$Model  = 'TSDM'
HDW_yield_fit_3W$Model  = 'TSCD'
HDW_yield_fit_4W$Model  = 'TPMM'
HDW_yield_fit_5W$Model  = 'TPDM'
HDW_yield_fit_6W$Model  = 'TPCD'

HDW_yield_fit_1W$Y_levle = 'County'
HDW_yield_fit_2W$Y_levle = 'County'
HDW_yield_fit_3W$Y_levle = 'County'
HDW_yield_fit_4W$Y_levle = 'County'
HDW_yield_fit_5W$Y_levle = 'County'
HDW_yield_fit_6W$Y_levle = 'County'

HDW_yield_fit_1W$HDW_sou = 'WHDW'
HDW_yield_fit_2W$HDW_sou = 'WHDW'
HDW_yield_fit_3W$HDW_sou = 'WHDW'
HDW_yield_fit_4W$HDW_sou = 'WHDW'
HDW_yield_fit_5W$HDW_sou = 'WHDW'
HDW_yield_fit_6W$HDW_sou = 'WHDW'

HDW_yield_fit_dfW = rbind(HDW_yield_fit_1W,
                         HDW_yield_fit_2W,
                         HDW_yield_fit_3W,
                         HDW_yield_fit_4W,
                         HDW_yield_fit_5W,
                         HDW_yield_fit_6W)




#@@@####################################
#@@@ ###################################3 fit yield response for field level
#@@@####################################


Ta_vars = c('PJGDDNHDW' , 'JHGDDNHDW' , 'HMGDDNHDW' , 'JHEDDNHDW' , 'HMEDDNHDW', 'PJFDDNHDW', 'JHFDDNHDW' )

Ta_varsS = c('PJGDD' , 'JHGDD' , 'HMGDD' , 'JHEDD' , 'HMEDD', 'PJFDD','JHFDD')

Cost_vars = c("Adj_Seed_Cost",          "Adj_Fertilizer_Cost",                                                      
              "Adj_Mechanization_Cost", "Adj_Irrigation_Cost" )

HDW_vars = c('cumulative_hours_HighTempLowHumidity_Early', 'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle', 
             'cumulative_hours_PostRainScorch_Late','cumulative_hours_PostRainScorch_Middle')

vars <- c(Ta_vars,Soil_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
fmula1x    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Soil_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula2x    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))
 
vars <- c(Ta_vars,Soil_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
fmula3x    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))
 
vars <- c(Ta_vars,Per_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula4x    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))
 
vars <- c(Ta_vars,Per_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
fmula5x    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))
 
vars <- c(Ta_vars,Per_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula6x    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

set.seed(1234)

HDW_yield_fit_1_exp =   BootReg(fmula=fmula1x, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_2_exp =   BootReg(fmula=fmula2x, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_3_exp =   BootReg(fmula=fmula3x, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_4_exp =   BootReg(fmula=fmula4x, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_5_exp =   BootReg(fmula=fmula5x, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_6_exp =   BootReg(fmula=fmula6x, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  


HDW_yield_fit_1_exp$Model  = 'TSMM'
HDW_yield_fit_2_exp$Model  = 'TSDM'
HDW_yield_fit_3_exp$Model  = 'TSCD'
HDW_yield_fit_4_exp$Model  = 'TPMM'
HDW_yield_fit_5_exp$Model  = 'TPDM'
HDW_yield_fit_6_exp$Model  = 'TPCD'

HDW_yield_fit_1_exp$Y_levle = 'Field'
HDW_yield_fit_2_exp$Y_levle = 'Field'
HDW_yield_fit_3_exp$Y_levle = 'Field'
HDW_yield_fit_4_exp$Y_levle = 'Field'
HDW_yield_fit_5_exp$Y_levle = 'Field'
HDW_yield_fit_6_exp$Y_levle = 'Field'

HDW_yield_fit_1_exp$HDW_sou = 'NHDW'
HDW_yield_fit_2_exp$HDW_sou = 'NHDW'
HDW_yield_fit_3_exp$HDW_sou = 'NHDW'
HDW_yield_fit_4_exp$HDW_sou = 'NHDW'
HDW_yield_fit_5_exp$HDW_sou = 'NHDW'
HDW_yield_fit_6_exp$HDW_sou = 'NHDW'

HDW_yield_fit_df_exp = rbind(HDW_yield_fit_1_exp,
                         HDW_yield_fit_2_exp,
                         HDW_yield_fit_3_exp,
                         HDW_yield_fit_4_exp,
                         HDW_yield_fit_5_exp,
                         HDW_yield_fit_6_exp)

###############################################
#################################################
################################################
vars <- c(Ta_varsS,Soil_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula1xn    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))
 

vars <- c(Ta_varsS,Soil_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula2xn    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))
 
vars <- c(Ta_varsS,Soil_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula3xn    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))
 
vars <- c(Ta_varsS,Per_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula4xn    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))
 
vars <- c(Ta_varsS,Per_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula5xn    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))
 
vars <- c(Ta_varsS,Per_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula6xn    = as.formula(paste0("Yield_S ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

set.seed(1234)

HDW_yield_fit_1_exp_n =   BootReg(fmula=fmula1xn, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_2_exp_n =   BootReg(fmula=fmula2xn, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_3_exp_n =   BootReg(fmula=fmula3xn, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_4_exp_n =   BootReg(fmula=fmula4xn, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_5_exp_n =   BootReg(fmula=fmula5xn, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  
HDW_yield_fit_6_exp_n =   BootReg(fmula=fmula6xn, boot=10000, data=Wheat_exp_merged_final_SD, size=10000)  


HDW_yield_fit_1_exp_n$Model  = 'TSMM'
HDW_yield_fit_2_exp_n$Model  = 'TSDM'
HDW_yield_fit_3_exp_n$Model  = 'TSCD'
HDW_yield_fit_4_exp_n$Model  = 'TPMM'
HDW_yield_fit_5_exp_n$Model  = 'TPDM'
HDW_yield_fit_6_exp_n$Model  = 'TPCD'

HDW_yield_fit_1_exp_n$Y_levle = 'Field'
HDW_yield_fit_2_exp_n$Y_levle = 'Field'
HDW_yield_fit_3_exp_n$Y_levle = 'Field'
HDW_yield_fit_4_exp_n$Y_levle = 'Field'
HDW_yield_fit_5_exp_n$Y_levle = 'Field'
HDW_yield_fit_6_exp_n$Y_levle = 'Field'

HDW_yield_fit_1_exp_n$HDW_sou = 'WHDW'
HDW_yield_fit_2_exp_n$HDW_sou = 'WHDW'
HDW_yield_fit_3_exp_n$HDW_sou = 'WHDW'
HDW_yield_fit_4_exp_n$HDW_sou = 'WHDW'
HDW_yield_fit_5_exp_n$HDW_sou = 'WHDW'
HDW_yield_fit_6_exp_n$HDW_sou = 'WHDW'

HDW_yield_fit_df_exp_n = rbind(HDW_yield_fit_1_exp_n,
                               HDW_yield_fit_2_exp_n,
                               HDW_yield_fit_3_exp_n,
                               HDW_yield_fit_4_exp_n,
                               HDW_yield_fit_5_exp_n,
                               HDW_yield_fit_6_exp_n)



########################################################
#########################################################
#########################################################
HDW_yield_fit = rbind(HDW_yield_fit_df_exp,
                      HDW_yield_fit_df_exp_n,
                      HDW_yield_fit_df,
                      HDW_yield_fit_dfW)


HDW_yield_fit_o1 =   BootReg(fmula1, 10000, Wheat_county_merged_final, 60000) 
HDW_yield_fit_o2 =   BootReg(fmula2, 10000, Wheat_county_merged_final, 60000)
HDW_yield_fit_o3 =   BootReg(fmula3, 10000, Wheat_county_merged_final, 60000)
 

dfSM      = rbind(HDW_yield_fit_o1[HDW_yield_fit_o1$Var %in% HDW_yield_fit_3$Var[14:43],],
                  HDW_yield_fit_o2[HDW_yield_fit_o2$Var %in% HDW_yield_fit_3$Var[14:43],],
                  HDW_yield_fit_o3[HDW_yield_fit_o3$Var %in% HDW_yield_fit_3$Var[14:43],])

dfSM$Model = c(rep('TSMM',30),rep('TSDM',30),rep('TSCD',30))

dfSM$stg  = substr(dfSM$Var,2,6)
 
dfSM$stg[dfSM$stg=='PT_JT'] = 'PT-JT'
dfSM$stg[dfSM$stg=='JT_HD'] = 'JT-HD'
dfSM$stg[dfSM$stg=='HD_MT'] = 'HD-MT'
 
Soil_point_gg =   ggplot(dfSM) +
  geom_hline(yintercept = 0,lwd = 0.5, linetype = "dashed", color = 'gray75')+
  geom_point(aes(x= soil, y=  Estimate,
                 shape=Model, color=Model,fill = Model),size=1,
             position = position_dodge(0.5))+
  geom_errorbar(aes(x= soil,ymin=( Estimate-Std..Error),
                    ymax=( Estimate+Std..Error),
                    color=Model),linewidth=0.25, 
                width=.45,position=position_dodge(0.5))+
  scale_fill_manual(name = '',values = brewer.pal(11, "Dark2"))+
  scale_shape_manual(name = '',values = c(1,2,3))+
  scale_color_manual(name = '',values = brewer.pal(11, "Dark2"))+
  facet_grid(stg~.)+ylab(bquote(Yield~sensitivty~to~soil~moisture~(kg~ha^-2~day^-1)))+
  scale_x_discrete('Soil moisture bins at different quantiles',labels = c(
    "SM_0q~10q"  = bquote(SM['10% quantile']),
    "SM_10q~20q" = bquote(SM['20% quantile']),
    "SM_20q~30q" = bquote(SM['30% quantile']),
    "SM_30q~40q" = bquote(SM['40% quantile']),
    "SM_40q~50q" = bquote(SM['50% quantile']),
    'SM_50q~60q' = bquote(SM['60% quantile']),
    "SM_60q~70q" = bquote(SM['70% quantile']),
    "SM_70q~80q" = bquote(SM['80% quantile']),
    'SM_80q~90q' = bquote(SM['90% quantile']),
    "SM_90q~100q" = bquote(SM['100% quantile'])))+
  # ylab("Yield loss per standard unit (%)")+
  labs(subtitle = c('(a)'))+theme_bw()+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 6),
        legend.key.size = unit(0.3,'cm'),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 5,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(0.91,0.20),
        legend.key.height = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 5),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

 

HDW_yield_fit_Per =   BootReg(fmula4, 10000, Wheat_county_merged_final, 60000)

dfper      = data.frame(HDW_yield_fit_Per[HDW_yield_fit_Per$Var %in% c(Per_vars),])

Per_Fit    = data.frame(PJ_per = c(Wheat_county_merged_final$PT_JT_total_rainfall),
                        JH_per = c(Wheat_county_merged_final$JT_HD_total_rainfall),
                        HM_per = c(Wheat_county_merged_final$HD_MT_total_rainfall))

Per_Fit$PJ_per_fit = Per_Fit$PJ_per*(dfper[5,2])+ Per_Fit$PJ_per^2*(dfper[6,2])
Per_Fit$JH_per_fit = Per_Fit$JH_per*(dfper[3,2])+ Per_Fit$JH_per^2*(dfper[4,2])
Per_Fit$HM_per_fit = Per_Fit$HM_per*(dfper[1,2])+ Per_Fit$HM_per^2*(dfper[2,2])

Per_Fit$PJ_per_fit_sd =  Per_Fit$PJ_per*(dfper[5,3])
Per_Fit$JH_per_fit_sd =  Per_Fit$JH_per*(dfper[3,3])
Per_Fit$HM_per_fit_sd =  Per_Fit$HM_per*(dfper[1,3])

Per_Fit_long1      = pivot_longer(Per_Fit[c('PJ_per','JH_per','HM_per')], 
                                  cols = c('PJ_per','JH_per','HM_per'),
                                  names_to = "STG", values_to = "Per")

Per_Fit_long2      = pivot_longer(Per_Fit[c('PJ_per_fit','JH_per_fit','HM_per_fit')], 
                                  cols = c('PJ_per_fit','JH_per_fit','HM_per_fit'),
                                  names_to = "STG", values_to = "Per")

Per_Fit_long3      = pivot_longer(Per_Fit[c('PJ_per_fit_sd','JH_per_fit_sd','HM_per_fit_sd')], 
                                  cols = c('PJ_per_fit_sd','JH_per_fit_sd','HM_per_fit_sd'),
                                  names_to = "STG", values_to = "sd")

Per_Fit_long         = Per_Fit_long1
Per_Fit_long$per_fit = Per_Fit_long2$Per
Per_Fit_long$sd      = Per_Fit_long3$sd

Per_Fit_long = Per_Fit_long[Per_Fit_long$Per<=400,]
Per_Fit_long = na.omit(Per_Fit_long)


Perp_sensity_gg = ggplot(Per_Fit_long) +  
  geom_histogram(aes(x = Per, y = ..density..,group = STG, fill = STG),
                 color= 'gray75',alpha = 0.8, size = 0.4) +
  # geom_density(aes(x = Per, group = STG, fill = STG),color = 'gray65',lwd = 1,linetype = 5,alpha = 0.3) +
  geom_line(aes(x = Per,y = 0.03+per_fit/30000,group = STG,color = STG),size = 0.6)+
  geom_ribbon(aes(x = Per, ymin = 0.03+(per_fit-sd)/30000,
                  ymax = 0.03+(per_fit+sd)/30000,fill = STG),alpha = 0.3)+
  scale_fill_manual(values = c(brewer.pal(11, "PRGn")[c(9:11)]),
                    name = '',label = c('HD-MT','JT-HD', 'PT-JT') )+
  scale_color_manual(values = c(brewer.pal(11, "PRGn")[c(9:11)]),
                     name = '',label = c('HD-MT','JT-HD', 'PT-JT') ) +
  scale_y_continuous(name = "Density of county-year precipitation",
                     sec.axis = sec_axis(~(.*30000)-30000*0.03,
                                         name= bquote(Sensitivity~of~yield~to~Prcp~(kg~ha^-2~mm^-1)))) +
  geom_hline(yintercept=0.03, color = "gray75",linetype = 5, size=0.4)+
  xlab('Cumulative precipitation (mm)')+
  labs(subtitle = c('(b)'))+theme_bw()+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 6),
        legend.key.size = unit(0.3,'cm'),
        # axis.ticks = element_blank(),
        axis.text = element_text(size = 5,angle = 0),
        axis.title = element_text(size = 6),
        legend.position = c(0.78,0.22),
        legend.key.height = unit(0.22,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 5),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))





##############################################################

HDW_vars = c('cumulative_hours_DryWind_Early',
             'cumulative_hours_DryWind_Late','cumulative_hours_DryWind_Middle',
             'cumulative_hours_HighTempLowHumidity_Early', 'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle', 'cumulative_hours_PostRainScorch_Early',
             'cumulative_hours_PostRainScorch_Late', 'cumulative_hours_PostRainScorch_Middle')


HDW_gg_df = HDW_yield_fit[HDW_yield_fit$Var %in% c(HDW_vars),]

HDW_gg_df <- HDW_gg_df %>%mutate(Type = str_extract(Var, "(?<=cumulative_hours_)[^_]+"),TimePeriod = str_extract(Var, "[^_]+$")    )

HDW_gg_df$Type[HDW_gg_df$Type=='DryWind'] = 'DTWD'
HDW_gg_df$Type[HDW_gg_df$Type=='HighTempLowHumidity'] = 'HTLH'
HDW_gg_df$Type[HDW_gg_df$Type=='PostRainScorch'] = 'PRGW'


HDW_gg_summary <- HDW_gg_df %>%
  group_by(Model, Y_levle, HDW_sou, Type) %>%
  summarise(
     
    Estimate = sum(Estimate, na.rm = TRUE),
    Std.Error = sum(Std..Error, na.rm = TRUE),
     
    t.value = mean(t.value, na.rm = TRUE),
    Pr.t = min(Pr...t.., na.rm = TRUE),
    rsq = mean(rsq, na.rm = TRUE),
    
    .groups = "drop"
  )

HDW_gg_summary$col = paste0(HDW_gg_summary$Type,'_', HDW_gg_summary$Y_levle) 

HDW_gg_summary <- HDW_gg_summary %>%mutate( Type = factor(Type, levels = c("HTLH", "PRGW", "DTWD")))

HDW_gg_summary$Model = paste0(HDW_gg_summary$Model,'_',HDW_gg_summary$HDW_sou)

 
HDW_gg_summary <- HDW_gg_summary %>%
   mutate(Model_formatted = sapply(strsplit(Model, "_"), function(x) {
       if (length(x) == 2) {paste0(x[1], "[", x[2], "]")} 
     else {Model  }}))
 
HDW_gg_summary <- HDW_gg_summary %>% mutate( signif = case_when(Pr.t < 0.001 ~ "***",Pr.t < 0.01 ~ "**",Pr.t < 0.05 ~ "*",TRUE ~ "ns"),

y_position = ifelse(Estimate > 0, 
                         (Estimate + Std.Error) * 100 + 1, 
                         (Estimate - Std.Error) * 100 - 1))
 
library(ggpattern)
 

 
 
 level_order = c('PJFDD', 'JHFDD', 'JHEDD', 'HMEDD',"hdw_hour1", "hdw_hour2")

 EX_df_get   = HDW_yield_fit[HDW_yield_fit$Var %in% c("JHEDD","HMEDD","PJFDD", "JHFDD","HMEDDNHDW","JHEDDNHDW","JHFDDNHDW","PJFDDNHDW"),]
 EX_df_mean  = EX_df_get  ;EX_df_mean$Estimate = EX_df_mean$Estimate*100
 colnames(EX_df_mean)[3] = 'sd'
 EX_df_mean$sd = EX_df_mean$sd*100
 
 library(smplot2)
 
 EX_df_mean <- EX_df_mean %>%
   mutate(Var = str_replace(Var, "NHDW$", ""))   
 
 EX_df_mean$col = paste0(EX_df_mean$Var,'_', EX_df_mean$Y_levle)
 
 CLEX_df_means_gg = ggplot( EX_df_mean, aes(x = Var, y = Estimate,
                                              color = col,fill = col))+
   geom_bar_pattern(stat = "identity",alpha = 0.7, lwd= 0.05,aes(pattern = col),
                    position = position_dodge(.9),
                    color = "black", 
                    pattern_fill = "black",
                    pattern_angle = 45,
                    pattern_density = 0.000001,
                    pattern_size = .05,
                    pattern_spacing = 0.1,
                    pattern_key_scale_factor = 0.1) +
   geom_errorbar(aes(ymin=Estimate-sd, ymax=Estimate+sd), width=.2,
                 linewidth = 0.5, position=position_dodge(.9))+
   scale_pattern_manual(values = c("none", "stripe","none", "stripe","none", "stripe",
                                   "none", "stripe","none", "stripe","none", "stripe",
                                   "none", "stripe"),name = '') +
   scale_fill_manual(values = c("orangered2","orangered2", "orangered3","orangered3", 
                                brewer.pal(11, "PRGn")[2],brewer.pal(11, "PRGn")[2],
                                brewer.pal(11, "PRGn")[4],brewer.pal(11, "PRGn")[4],
                                brewer.pal(11, "PRGn")[9],brewer.pal(11, "PRGn")[9],
                                brewer.pal(11, "PRGn")[10],
                                brewer.pal(11, "PRGn")[10]))+
   scale_color_manual(values = c("orangered2","orangered2", "orangered3","orangered3", 
                                 brewer.pal(11, "PRGn")[2],brewer.pal(11, "PRGn")[2],
                                 brewer.pal(11, "PRGn")[4],brewer.pal(11, "PRGn")[4],
                                 brewer.pal(11, "PRGn")[9],brewer.pal(11, "PRGn")[9],
                                 brewer.pal(11, "PRGn")[10],
                                 brewer.pal(11, "PRGn")[10]))+
   facet_grid(HDW_sou~Model)+
   scale_x_discrete('Standardize climate indices',labels = c(
     "PJFDD" = bquote(FDD[pT-JT]),
     "JHFDD" = bquote(FDD[JT-HD]),
     "JHEDD" = bquote(EDD[JT-HD]),
     "HMEDD" = bquote(EDD[HD-MT]),
     "hdw_hour1" = bquote(HDW[HTLH]),
     'hdw_hour2' = bquote(HDW[PRGW]),
     "hdw_hour3" = bquote(HDW[DTWD])
   ))+ylab("Yield effects by per standard unit (%)")+theme_bw()+#ylim(-30,30)+
   theme(strip.text = element_text(size = 7),
         axis.text.x = element_text(size = 8,angle = 45,vjust = 0.5),
         axis.text.y = element_text(size = 8,angle = 0,vjust = 0.5),
         axis.title = element_text(size = 9),
         legend.position = c(10.3,0.95),
         #legend.direction = "horizontal",
         legend.key.height = unit(0.12,'cm'),
         legend.background = element_blank(),
         legend.text = element_text(size = 3.6),
         legend.title= element_text(size = 4),
         strip.background = element_rect(color = 'transparent'),
         plot.background = element_rect(fill = "transparent",
                                        colour = NA_character_))
 
 
 
 
emf('.../Figure S24.emf',
      units = "cm", width=15, height=8)
 CLEX_df_means_gg
 dev.off()
 

 
HDW_SEN_sing_gg  = ggplot(HDW_gg_summary,
                          aes(x = Type, y = Estimate*100,
                              color = col,fill = col))+
  geom_bar_pattern(stat = "identity",alpha = 0.6, lwd= 0.05,aes(pattern = col),
                   position = position_dodge(.9),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.000001,
                   pattern_size = .05,
                   pattern_spacing = 0.1,
                   pattern_key_scale_factor = 0.1) +
  geom_errorbar(aes(ymin=(Estimate-Std.Error)*100, ymax=(Estimate+Std.Error)*100), width=.2,
                position=position_dodge(.9))+guides(fill=guide_legend(ncol=2),
                                                    label.position = "top")+
  geom_text(aes(y = y_position, label = signif), 
            position = position_dodge(0.9), 
            size = 2, vjust = 0.5, color = "black") +
  scale_x_discrete('Yield models',labels = c(
    "HTLH" = bquote(HDW[HTLH]),
    'PRWD' = bquote(HDW[PRGW]),
    "DTWD" = bquote(HDW[DTWD])))+ylab("Yield loss per standard unit (%)")+
  scale_pattern_manual(values = c("none","none", "stripe","none", "stripe", "none"),name = '') +
  scale_fill_manual(values =  c('orange3',"orangered4",'brown4',
                                "orangered3", "orangered2" ),
                    name = '',label = c('County level: HTLH','Field trail: HTLH',
                                        'County level: PRGW','Field trail: PRGW',
                                        'County level: DTWD') )+
  scale_color_manual(values =   c('orange3',"orangered4",'brown4',
                                  "orangered3", "orangered2"),
                     name = '',label = c('County level: HTLH','Field trail: HTLH',
                                         'County level: PRGW','Field trail: PRGW',
                                         'County level: DTWD'))+
  facet_grid(~ Model_formatted,  switch = "x",scales = "free_x",labeller = label_parsed ) +
  labs(subtitle = c('(c)'))+theme_bw()+#ylim(-30,30)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 4),
        legend.key.size = unit(0.15,'cm'),
        panel.spacing = unit(0, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 5,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 5,angle = 0,vjust = 0.5),
        axis.title = element_text(size = 6),
        legend.position = c(10.3,0.95),
        #legend.direction = "horizontal",
        legend.key.height = unit(0.12,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 3.6),
        legend.title= element_text(size = 4),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))


#############################################
HDW_vars = c('cumulative_hours_HighTempLowHumidity_Early', 'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle','cumulative_hours_PostRainScorch_Early',
             'cumulative_hours_PostRainScorch_Late', 'cumulative_hours_PostRainScorch_Middle')


PHDW_vars = paste0(HDW_vars,':','Provience')

vars <- c(Ta_varsS,Soil_vars,HDW_vars,PHDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+Provience^site"))
BootReg    = function(fmula, boot, data, size){
  
  

  bootcoefs = c()
  Rsq       = NULL
  for (n in 1:boot) {
    
    rsamp_data = sample(1:nrow(data), size=size, replace = T)  
    
    rsamp_data = data[rsamp_data,]
    
    cropfit    = feols(fmula, data = rsamp_data) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)

  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef #%>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean));
  

  
  return(Mbootcoef)
}
HDW_yield_fit_TEMP =   BootReg(fmula, 10000, Wheat_county_merged_final[Wheat_county_merged_final$Provience!='Jinlin',] , 100000)

HDW_yield_fit_TEMP$Provience = str_extract(HDW_yield_fit_TEMP$Var, "(?<=Provience)[\u4e00-\u9fa5]+") 


Provience = unique(Wheat_county_merged_final$Provience)

HDW_yield_fit_TEMP$type = str_extract(HDW_yield_fit_TEMP$Var,"(?<=_)[A-Za-z]+(?=_(Early|Middle|Late))|(?<=_)[A-Za-z]+(?=:Provience)")
HDW_yield_fit_TEMP$Period = str_extract(HDW_yield_fit_TEMP$Var,"(?<=_)(Early|Middle|Late)(?=:Provience|$)")


HDW_yield_fit_TEMP$type[HDW_yield_fit_TEMP$type=='HighTempLowHumidity'] = 'HTLH'
HDW_yield_fit_TEMP$type[HDW_yield_fit_TEMP$type=='PostRainScorch'] = 'PRWG'

HDW_yield_fit_base = HDW_yield_fit_TEMP[is.na(HDW_yield_fit_TEMP$Provience==T),]
HDW_yield_fit_Prov = HDW_yield_fit_TEMP[!is.na(HDW_yield_fit_TEMP$Provience==T),]

HDW_yield_fit_Prov_join = HDW_yield_fit_Prov %>%left_join(HDW_yield_fit_base[HDW_yield_fit_base$type %in% c('HTLH','PRWD'),], by= c('type','Period','ID')) 

HDW_yield_fit_all =data.frame(Var = HDW_yield_fit_Prov_join$Var.x,
                              Estimate = HDW_yield_fit_Prov_join$Estimate.x+HDW_yield_fit_Prov_join$Estimate.y,
                              Std..Error=HDW_yield_fit_Prov_join$Std..Error.x+HDW_yield_fit_Prov_join$Std..Error.y,
                              type=HDW_yield_fit_Prov_join$type,
                              Period=HDW_yield_fit_Prov_join$Period,
                              Provience=HDW_yield_fit_Prov_join$Provience.x,
                              ID= HDW_yield_fit_Prov_join$ID)


Prov_HDW_SEN_sum <- HDW_yield_fit_all %>%
  group_by(type, Provience, ID) %>%
  summarise(
    Estimate = sum(Estimate, na.rm = TRUE),
    Std.Error = sum(Std..Error, na.rm = TRUE),
    .groups = "drop"
  )

Prov_HDW_SEN_sum_mean = Prov_HDW_SEN_sum %>% group_by(type, Provience) %>%
  summarise(
    mean = mean(Estimate, na.rm = TRUE),
    Std = sd(Estimate, na.rm = TRUE),
    .groups = "drop"
  )

Prov_HDW_SEN_sum_mean = Prov_HDW_SEN_sum_mean %>% filter(!(type=='PRGW'&Provience %in% c('Gansu','Inner Mongolia',
                                         'Ningxia','Tianjing','Xinjiang')))

Prov_HDW_SEN_all = Prov_HDW_SEN_sum_mean %>%
  group_by(type) %>%
  summarise(
    mean = mean(mean, na.rm = TRUE),
    Std = mean(Std, na.rm = TRUE),
    .groups = "drop"
  )

Prov_HDW_SEN_all$Provience = 'All'
Prov_HDW_SEN_ALL = rbind(Prov_HDW_SEN_all,Prov_HDW_SEN_sum_mean)


Prov_HDW_SEN_DF = NULL
for(i in (1:nrow(Prov_HDW_SEN_ALL))){
  temp = Prov_HDW_SEN_ALL$mean[i]*0:10
  df   = data.frame(Values = temp, Hs = 0:10)
  df$Prov = Prov_HDW_SEN_ALL$Provience[i]
  df$varbs = Prov_HDW_SEN_ALL$type[i]
  df$sd = Prov_HDW_SEN_ALL$Std[i]*0:10
  Prov_HDW_SEN_DF = rbind(Prov_HDW_SEN_DF, df)
}


Prov_HDW_SEN_gg = ggplot(Prov_HDW_SEN_DF) +
  geom_line( linewidth = 0.2,aes(x=Hs, y=Values, color=Prov,group = Prov))+
  xlab('∆ HDW (hour)')+ ylab(expression('Yield respond'~(kg~ha^-2)))+
  scale_color_manual(values = c('Black', brewer.pal(8, "Dark2"),
                                brewer.pal(8, "Accent")), name = '')+
  geom_ribbon(aes(x = Hs, ymin =Values-sd,
                  ymax = Values+sd,fill = Prov),alpha = 0.1)+
  scale_fill_manual(values = c('Black', brewer.pal(8, "Dark2"),
                               brewer.pal(8, "Accent")), name = '')+
  # guides(color=guide_legend(ncol=4,label.position = "top",keylength = unit(0.1, "cm") ))+
  facet_grid(.~varbs, switch = "x", scales = "free_y") +
  labs(subtitle = c('(d)'))+theme_bw()+#ylim(-2000,500)+
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.3,                     # Vertical adjustment
                                     lineheight = 1,                # Line spacing
                                     margin = margin(20, 0, 0, 0)),
        strip.text = element_text(size = 4),
        legend.key.size = unit(0.02,'cm'),
        panel.spacing = unit(0.05, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 6),
        legend.position = c(0.15, .42),
        # legend.justification = c("right", "top"),
        # legend.box.just = "right",
        legend.key.height = unit(0.02,'cm'),
        legend.key.width = unit(0.1,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 4),
        legend.title= element_text(size = 5),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



emf('.../Figures/Figure 2.emf',
     units = "cm", width=16,height=12)

ggdraw() +
  draw_plot(Soil_point_gg, x = 0, y = 0.40,  width =0.6, height = 0.65) +
  draw_plot(Perp_sensity_gg, x = 0.6, y = 0.40,  width =0.4, height = 0.65)+
  draw_plot(HDW_SEN_sing_gg, x = 0, y = 0,  width =0.6, height = 0.48) +
  draw_plot(Prov_HDW_SEN_gg, x = 0.6, y = 0,  width =0.4, height = 0.48)
dev.off()



HDW_gg_df$TP = paste0(HDW_gg_df$Type,'-',HDW_gg_df$TimePeriod)

HDW_gg_df$Model = paste0(HDW_gg_df$Model,'_',HDW_gg_df$HDW_sou)
HDW_gg_df$col   = paste0(HDW_gg_df$Type,'_', HDW_gg_df$Y_levle) 

HDW_gg_df <- HDW_gg_df %>%
  mutate(Model_formatted = sapply(strsplit(Model, "_"), function(x) {
    if (length(x) == 2) {paste0(x[1], "[", x[2], "]")} 
    else {Model  }}))


HDW_gg_df <- HDW_gg_df %>%mutate( Type = factor(Type, levels = c("HTLH", "PRWD", "DTWD")))
HDW_gg_df <- HDW_gg_df %>% mutate( signif = case_when(Pr...t.. < 0.001 ~ "***",Pr...t.. < 0.01 ~ "**",Pr...t.. < 0.05 ~ "*",TRUE ~ "ns"),
                                               y_position = ifelse(Estimate > 0, 
                                                                 (Estimate + Std..Error) * 100 + 1, 
                                                                 (Estimate - Std..Error) * 100 - 1))

HDW_gg_df$TimePeriod <- factor(HDW_gg_df$TimePeriod, 
                               levels = c("Early", "Middle", "Late"))

library(ggpattern)
HDW_SEN_sing_spy_gg  = ggplot(HDW_gg_df,
                          aes(x = TimePeriod, y = Estimate*100,
                              color = col,fill = col))+
  geom_bar_pattern(stat = "identity",alpha = 0.6, lwd= 0.05,aes(pattern = col),
                   position = position_dodge(.9),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.000001,
                   pattern_size = .05,
                   pattern_spacing = 0.1,
                   pattern_key_scale_factor = 0.1) +
  geom_errorbar(aes(ymin=(Estimate-Std..Error)*100, ymax=(Estimate+Std..Error)*100), width=.2,
                position=position_dodge(.9))+guides(fill=guide_legend(ncol=2),
                                                    label.position = "top")+
  geom_text(aes(y = y_position, label = signif),
            position = position_dodge(0.9),
            size = 2, vjust = 0.5, color = "black") +
  scale_x_discrete(' ',labels = c(
    "HTLH" = bquote(HDW[HTLH]),
    'PRWD' = bquote(HDW[PRGW]),
    "DTWD" = bquote(HDW[DTWD])))+ylab("Yield loss per standard unit (%)")+
  scale_pattern_manual(values = c("none","none", "stripe","none", "stripe", "none"),name = '') +
  scale_fill_manual(values =  c('orange3',"orangered4",'brown4',
                                "orangered3", "orangered2" ),
                    name = '',label = c('County level: HTLH','Field trail: HTLH',
                                        'County level: PRGW','Field trail: PRGW',
                                        'County level: DTWD') )+
  scale_color_manual(values =   c('orange3',"orangered4",'brown4',
                                  "orangered3", "orangered2"),
                     name = '',label = c('County level: HTLH','Field trail: HTLH',
                                         'County level: PRGW','Field trail: PRGW',
                                         'County level: DTWD'))+
  facet_grid(Type+Y_levle~ Model_formatted,  scales = "free_x",labeller = label_parsed ) +
  theme_bw()+#ylim(-30,30)+
  theme(strip.text = element_text(size = 4),
        legend.key.size = unit(0.15,'cm'),
        # panel.spacing = unit(0, units = "cm"), # removes space between panels
        #strip.placement = "outside", # moves the states down
        # strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 5,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 5,angle = 0,vjust = 0.5),
        axis.title = element_text(size = 6),
        legend.position = c(10.3,0.95),
        #legend.direction = "horizontal",
        legend.key.height = unit(0.12,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 3.6),
        legend.title= element_text(size = 4),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))



emf('.../Supplementary Figure 23.emf',
     units = "cm", width=16,height=12, res = 3000, compression = 'lzw',pointsize = 11)
HDW_SEN_sing_spy_gg
dev.off()

#############################################


HDW_vars = c('cumulative_hours_DryWind_Early',
             'cumulative_hours_DryWind_Late','cumulative_hours_DryWind_Middle',
             'cumulative_hours_HighTempLowHumidity_Early', 'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle', 'cumulative_hours_PostRainScorch_Early',
             'cumulative_hours_PostRainScorch_Late', 'cumulative_hours_PostRainScorch_Middle')


vars <- c(Ta_vars,Soil_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
Ofmula2    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Soil_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
Ofmula3    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|site + year"))


vars <- c(Ta_vars,Per_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
Ofmula5    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Per_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})
Ofmula6    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))




BootReg    = function(fmula, boot, data, size){
  
  

  bootcoefs = c()
  Rsq       = NULL
  for (n in 1:boot) {
    
    rsamp_data = sample(1:nrow(data), size=size, replace = T)  
    
    rsamp_data = data[rsamp_data,]
    
    cropfit    = feols(fmula, data = rsamp_data) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)

  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean));
  
  Mbootcoef$rsq  = mean(Rsq)
  
  return(Mbootcoef)
}

set.seed(1234)

OHDW_yield_fit_2 =   BootReg(Ofmula2, 10000, Wheat_county_merged_final, 60000)  
OHDW_yield_fit_3 =   BootReg(Ofmula3, 10000, Wheat_county_merged_final, 60000)  
									 
OHDW_yield_fit_5 =   BootReg(Ofmula5, 10000, Wheat_county_merged_final, 60000)  
OHDW_yield_fit_6 =   BootReg(Ofmula6, 10000, Wheat_county_merged_final, 60000)  



OHDW_yield_fit_2$Model  = 'TSDM'
OHDW_yield_fit_3$Model  = 'TSCD'
 
OHDW_yield_fit_5$Model  = 'TPDM'
OHDW_yield_fit_6$Model  = 'TPCD'


OHDW_yield_fit_2$Y_levle = 'County'
OHDW_yield_fit_3$Y_levle = 'County'
 
OHDW_yield_fit_5$Y_levle = 'County'
OHDW_yield_fit_6$Y_levle = 'County'


OHDW_yield_fit_2$HDW_sou = 'NHDW'
OHDW_yield_fit_3$HDW_sou = 'NHDW'
 
OHDW_yield_fit_5$HDW_sou = 'NHDW'
OHDW_yield_fit_6$HDW_sou = 'NHDW'

OHDW_yield_fit_df = rbind(OHDW_yield_fit_2,
                          OHDW_yield_fit_3,
                          OHDW_yield_fit_5,
                          OHDW_yield_fit_6)


vars <- c(Ta_varsS,Soil_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
OWfmula2    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Soil_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
OWfmula3    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))


vars <- c(Ta_varsS,Per_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
OWfmula5    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Per_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
OWfmula6    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

set.seed(1234)


OWHDW_yield_fit_2W =   BootReg(OWfmula2, 10000, Wheat_county_merged_final, 60000)  
OWHDW_yield_fit_3W =   BootReg(OWfmula3, 10000, Wheat_county_merged_final, 60000)  
										  
OWHDW_yield_fit_5W =   BootReg(OWfmula5, 10000, Wheat_county_merged_final, 60000)  
OWHDW_yield_fit_6W =   BootReg(OWfmula6, 10000, Wheat_county_merged_final, 60000)  



OWHDW_yield_fit_2W$Model  = 'TSDM'
OWHDW_yield_fit_3W$Model  = 'TSCD'

OWHDW_yield_fit_5W$Model  = 'TPDM'
OWHDW_yield_fit_6W$Model  = 'TPCD'


OWHDW_yield_fit_2W$Y_levle = 'County'
OWHDW_yield_fit_3W$Y_levle = 'County'

OWHDW_yield_fit_5W$Y_levle = 'County'
OWHDW_yield_fit_6W$Y_levle = 'County'


OWHDW_yield_fit_2W$HDW_sou = 'WHDW'
OWHDW_yield_fit_3W$HDW_sou = 'WHDW'

OWHDW_yield_fit_5W$HDW_sou = 'WHDW'
OWHDW_yield_fit_6W$HDW_sou = 'WHDW'

OWHDW_yield_fit_dfW = rbind(OWHDW_yield_fit_2W,
                            OWHDW_yield_fit_3W,
                            OWHDW_yield_fit_5W,
                            OWHDW_yield_fit_6W)

OWHDW_fit_df  = rbind(OHDW_yield_fit_df, OWHDW_yield_fit_dfW)
OWHDW_fit_df_hdw = OWHDW_fit_df %>% filter(Var %in% HDW_vars)

OWHDW_fit_df_hdw <- OWHDW_fit_df_hdw %>%mutate(Type = str_extract(Var, "(?<=cumulative_hours_)[^_]+"),
                                               Period = str_extract(Var, "[^_]+$")    )

OWHDW_fit_df_hdw$Type[OWHDW_fit_df_hdw$Type=='DryWind'] = 'DTWD'
OWHDW_fit_df_hdw$Type[OWHDW_fit_df_hdw$Type=='HighTempLowHumidity'] = 'HTLH'
OWHDW_fit_df_hdw$Type[OWHDW_fit_df_hdw$Type=='PostRainScorch'] = 'PRGW'

OWHDW_fit_df_hdw_sum = OWHDW_fit_df_hdw %>% group_by(Model,Y_levle, HDW_sou, Type) %>%
  summarise(Value = sum(Estimate, na.rm = TRUE),.groups = 'drop')

CMFD_freqs_means_sf = readRDS('.../CMFD_freqs_means_sf.rds')

CMFD_freqs_means_sf_jion = CMFD_freqs_means_sf %>% left_join(OWHDW_fit_df_hdw_sum, by = 'Type')
  
CMFD_freqs_means_sf_jion$effects = CMFD_freqs_means_sf_jion$mean_hours*CMFD_freqs_means_sf_jion$Value  


CMFD_yield_loss_ymean = CMFD_freqs_means_sf_jion %>%
  group_by(year) %>%
  summarise(
    mean_effects = mean(effects, na.rm = TRUE),    
    sd_effects   = sd(effects, na.rm = TRUE)       
  )

HDW_effects_sp_gg <- ggplot() +
  geom_sf(data = CMFD_freqs_means_sf_jion, aes(fill = effects),, color = 'gray85',linewidth = 0.01) +
  scale_fill_stepsn(colors = rev(brewer.pal(9, "RdPu")),
                    breaks =  c(-4000,seq(-500,-100,100),seq(-100,0,10)),
                    labels =  c('< -500',seq(-500,-100,100),seq(-100,0,10)),
                    limits = c(-4000,0),na.value = "white",
                    values = scales::rescale(c(-4000,seq(-500,-100,100),seq(-100,0,10))),
                    name = bquote((kg~ha^-1~season^-1)))+
  labs(subtitle = bquote(Yield~loss~attributed~to~HDW))+
  ylim(4000000, 6300000) +facet_grid(Model~ Type) +
  theme_bw() +xlim(-2500000, 2000000) +
  geom_sf(data = China_line, color = "grey65", linewidth = 0.1) +
  geom_sf(data = China_sea, color = "grey65", linewidth = 0.1) +
  geom_sf(data = Provience_line, color = "grey50", linewidth = 0.1) +
  geom_sf(data = Chian_frame, color = "grey65", linewidth = 0.1) +
  theme(plot.subtitle = element_text(size = 7,                     # Font size
                                     hjust = 0.02,                     # Horizontal adjustment
                                     vjust = 1.5,                     # Vertical adjustment
                                     lineheight = 2),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1.9,'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.62, 1.074),
        legend.direction = "horizontal",#c(0.95,0.70),
        legend.key.height = unit(0.18,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 5),
        legend.title= element_text(size = 6),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent", colour = NA_character_)
  )


emd('.../Figure 3.emf',
     units = "cm", width=17,height=13, res = 1500, compression = 'lzw',pointsize = 11)
HDW_effects_sp_gg
dev.off()









#################################################################################
#################################################################################
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

Ta_vars = c('PJGDDNHDW' , 'JHGDDNHDW' , 'HMGDDNHDW' , 'JHEDDNHDW' , 'HMEDDNHDW' , 
            'PJFDDNHDW' , 'JHFDDNHDW')

Ta_varsS = c('PJGDD' , 'JHGDD' , 'HMGDD' , 'JHEDD' , 'HMEDD', 'PJFDD' , 'JHFDD')

HDW_vars = c('cumulative_hours_DryWind_Early',
             'cumulative_hours_DryWind_Late','cumulative_hours_DryWind_Middle',
             'cumulative_hours_HighTempLowHumidity_Early', 'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle', 'cumulative_hours_PostRainScorch_Early',
             'cumulative_hours_PostRainScorch_Late', 'cumulative_hours_PostRainScorch_Middle')


Per_vars = c('PT_JT_total_rainfall', 'JT_HD_total_rainfall', 'HD_MT_total_rainfall',
             'PT_JT_total_rainfall2', 'JT_HD_total_rainfall2', 'HD_MT_total_rainfall2')



Cost_vars = c("Adj_Seed_Cost",          "Adj_Fertilizer_Cost",                            
              "Adj_Manure_Cost",        "Adj_Pesticide_Cost" ,                            
              "Adj_Mechanization_Cost", "Adj_Irrigation_Cost" )

vars <- c(Ta_vars,Soil_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula1    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Soil_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula2    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Soil_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula3    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|site + year"))

vars <- c(Ta_vars,Per_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula4    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Per_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula5    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_vars,Per_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
fmula6    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))




BootReg    = function(fmula, boot, data, size){
 
  bootcoefs = c()
  Rsq       = NULL
  for (n in 1:boot) {
    
    rsamp_data = sample(1:nrow(data), size=size, replace = T)  
    
    rsamp_data = data[rsamp_data,]
    
    cropfit    = feols(fmula, data = rsamp_data) 
    
    Rsq        = c(Rsq, cropfit$sq.cor)
    
    coeftable = cropfit$coeftable
    coeftable$ID = n
    coeftable$Var = rownames(coeftable)
    bootcoefs = rbind(bootcoefs, coeftable)
    
    # sing      = cbind(sing,cropfit$coeftable$`Pr(>|t|)`)
  }
  bootcoef = data.frame(bootcoefs)
  
  Mbootcoef =  bootcoef %>% group_by(Var) %>% summarise_at(vars(colnames(bootcoef)[1:4]),list(mean));
  
  Mbootcoef$rsq  = mean(Rsq)
  
  return(Mbootcoef)
}

set.seed(1234)


HDW_yield_fit_1o =   BootReg(fmula1, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_2o =   BootReg(fmula2, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_3o =   BootReg(fmula3, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_4o =   BootReg(fmula4, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_5o =   BootReg(fmula5, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_6o =   BootReg(fmula6, 10000, Wheat_county_merged_final, 60000)  


HDW_yield_fit_1o$Model  = 'TSMM'
HDW_yield_fit_2o$Model  = 'TSDM'
HDW_yield_fit_3o$Model  = 'TSCD'
HDW_yield_fit_4o$Model  = 'TPMM'
HDW_yield_fit_5o$Model  = 'TPDM'
HDW_yield_fit_6o$Model  = 'TPCD'

HDW_yield_fit_1o$Y_levle = 'County'
HDW_yield_fit_2o$Y_levle = 'County'
HDW_yield_fit_3o$Y_levle = 'County'
HDW_yield_fit_4o$Y_levle = 'County'
HDW_yield_fit_5o$Y_levle = 'County'
HDW_yield_fit_6o$Y_levle = 'County'

HDW_yield_fit_1o$HDW_sou = 'NHDW'
HDW_yield_fit_2o$HDW_sou = 'NHDW'
HDW_yield_fit_3o$HDW_sou = 'NHDW'
HDW_yield_fit_4o$HDW_sou = 'NHDW'
HDW_yield_fit_5o$HDW_sou = 'NHDW'
HDW_yield_fit_6o$HDW_sou = 'NHDW'

HDW_yield_fit_dfo = rbind(HDW_yield_fit_1o,
                         HDW_yield_fit_2o,
                         HDW_yield_fit_3o,
                         HDW_yield_fit_4o,
                         HDW_yield_fit_5o,
                         HDW_yield_fit_6o)

vars <- c(Ta_varsS,Soil_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula1    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Soil_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula2    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Soil_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula3    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Per_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
fmula4    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Per_vars,HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula5    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(Ta_varsS,Per_vars,HDW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})
fmula6    = as.formula(paste0("Yield_Y ~ ", paste0(vars_escaped, collapse = " + "), "|year+site"))

vars <- c(HDW_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl("~", x)) {
    paste0("`", x, "`")   
  } else {
    x
  }
})



set.seed(1234)


HDW_yield_fit_1Wo =   BootReg(fmula1, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_2Wo =   BootReg(fmula2, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_3Wo =   BootReg(fmula3, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_4Wo =   BootReg(fmula4, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_5Wo =   BootReg(fmula5, 10000, Wheat_county_merged_final, 60000)  
HDW_yield_fit_6Wo =   BootReg(fmula6, 10000, Wheat_county_merged_final, 60000)  


HDW_yield_fit_1Wo$Model  = 'TSMM'
HDW_yield_fit_2Wo$Model  = 'TSDM'
HDW_yield_fit_3Wo$Model  = 'TSCD'
HDW_yield_fit_4Wo$Model  = 'TPMM'
HDW_yield_fit_5Wo$Model  = 'TPDM'
HDW_yield_fit_6Wo$Model  = 'TPCD'

HDW_yield_fit_1Wo$Y_levle = 'County'
HDW_yield_fit_2Wo$Y_levle = 'County'
HDW_yield_fit_3Wo$Y_levle = 'County'
HDW_yield_fit_4Wo$Y_levle = 'County'
HDW_yield_fit_5Wo$Y_levle = 'County'
HDW_yield_fit_6Wo$Y_levle = 'County'

HDW_yield_fit_1Wo$HDW_sou = 'WHDW'
HDW_yield_fit_2Wo$HDW_sou = 'WHDW'
HDW_yield_fit_3Wo$HDW_sou = 'WHDW'
HDW_yield_fit_4Wo$HDW_sou = 'WHDW'
HDW_yield_fit_5Wo$HDW_sou = 'WHDW'
HDW_yield_fit_6Wo$HDW_sou = 'WHDW'

HDW_yield_fit_dfWo = rbind(HDW_yield_fit_1Wo,
                          HDW_yield_fit_2Wo,
                          HDW_yield_fit_3Wo,
                          HDW_yield_fit_4Wo,
                          HDW_yield_fit_5Wo,
                          HDW_yield_fit_6Wo)

########################### Production cost merger#################################################
########################### Production cost merger#################################################
########################### Production cost merger#################################################
wheat_cost_and_benefit_1953_2006 <- read_excel("Yield Response/1953-2006 wheat cost and benifit.xlsx")
colnames(wheat_cost_and_benefit_1953_2006) = wheat_cost_and_benefit_1953_2006[2,]
wheat_cost_and_benefit_1953_2006 = wheat_cost_and_benefit_1953_2006[-c(1:2),]


wheat_cost_and_benefit_2007_2022 <- read_excel("Yield Response/2007-2022 wheat cost and benifit.xlsx")
colnames(wheat_cost_and_benefit_2007_2022) = wheat_cost_and_benefit_2007_2022[2,]
wheat_cost_and_benefit_2007_2022 = wheat_cost_and_benefit_2007_2022[-c(1:2),]

wheat_cost_and_benefit = rbind(wheat_cost_and_benefit_1953_2006[,c(1,2,3,39:53,55:59)],
                               wheat_cost_and_benefit_2007_2022[,c(1,2,3,33:47,49:53)])

#2@@@@@@@@@@@@@merge wheat_cost_and_benefit for county level
colnames(wheat_cost_and_benefit) = c("year",  "Provience",  "yield","Seed_Cost",          
                                     "Fertilizer_Cost",     "Manure_Cost",  "Pesticide_Cost",      
                                     "Agricultural_Film_Cost", "Contract_Work_Cost", "Mechanization_Cost",  
                                     "Irrigation_Cost", "Including_Water_Fee",  "Animal_Labor_Cost",   
                                     "Fuel_Power_Cost",  "Technical_Service_Fee", "Tools_Materials_Cost",
                                     "Repai_Maintenance", "Other_Direct_Costs",   "Fixed_Asset_Depreciation",
                                     "Insurance_Cost",    "Management_Fee",    "Financial_Cost",   
                                     "Marketing_Cost")
									 
wheat_cost_and_benefit = wheat_cost_and_benefit[,-c(8,18)]

check_na <- function(df) {
  na_info <- data.frame(
    column = names(df),
    na_count = sapply(df, function(x) sum(is.na(x))),
    na_percent = sapply(df, function(x) round(mean(is.na(x)) * 100, 2)),
    class = sapply(df, function(x) class(x)[1])   
  )
  
  na_info <- na_info[order(-na_info$na_count), ]
  na_info[na_info$na_count > 0, ]
}

check_na(wheat_cost_and_benefit)

library(dplyr)
library(tidyr)
library(zoo)   

intended_cost_cols <- c("Seed_Cost", "Fertilizer_Cost", "Manure_Cost", "Pesticide_Cost",
                        "Contract_Work_Cost", "Mechanization_Cost", "Irrigation_Cost", 
                        "Fuel_Power_Cost", "Technical_Service_Fee", "Tools_Materials_Cost", 
                        "Repai_Maintenance", "Fixed_Asset_Depreciation", "Insurance_Cost",
                        "Management_Fee", "Financial_Cost", "Marketing_Cost")

cost_cols <- intersect(intended_cost_cols, colnames(wheat_cost_and_benefit))


wheat_cost_cleaned <- wheat_cost_and_benefit %>% 
  mutate(year = as.numeric(year),
         yield = as.numeric(yield)) %>%
  mutate(across(all_of(cost_cols), ~as.numeric(.x)))
wheat_cost_cleaned = wheat_cost_cleaned[wheat_cost_cleaned$year%in% 1981:2018,]


full_cost_cols = c("Seed_Cost", "Fertilizer_Cost", "Manure_Cost",
                   "Pesticide_Cost",'Mechanization_Cost','Irrigation_Cost')

wheat_cost_cleaned = wheat_cost_cleaned[,c("year", "Provience",'yield',full_cost_cols)]

wheat_cost_cleaned = wheat_cost_cleaned[wheat_cost_cleaned$Provience %in%
                                          c(unique(Wheat_county_cl_sm_dhw$Provience),'means'),]


wheat_cost_fill_clean <- wheat_cost_fill_clean %>%
  mutate(year = as.integer(year),
         Provience = as.character(Provience))

library(dplyr)


wheat_cost_fill_clean <- wheat_cost_fill_clean %>%
  mutate(year = as.integer(year),
         Provience = as.character(Provience))


years_full <- 1981:2018
provs <- unique(wheat_cost_fill_clean$Provience)

full_frame <- expand.grid(
  Provience = provs,
  year = years_full,
  stringsAsFactors = FALSE
) %>% arrange(Provience, year)

df_full <- full_frame %>%
  left_join(wheat_cost_fill_clean, by = c("Provience", "year"))

df_full$Seed_Cost[df_full$year==2008] = df_full$Seed_Cost[df_full$year==2008]*0.1

wheat_cost_filled <- df_full %>% 

  mutate(is_1981_1988 = year >= 1981 & year <= 1988) %>%
  group_by(Provience) %>%
  arrange(year) %>%
  mutate(across(
    c(yield, Seed_Cost, Fertilizer_Cost, Manure_Cost, 
      Pesticide_Cost, Mechanization_Cost, Irrigation_Cost),
    ~ {
 
      if (all(is.na(.x))) return(.x)

      temp_vals <- .x
      temp_vals[is_1981_1988] <- NA

      filled <- zoo::na.approx(temp_vals, na.rm = FALSE)

      if (any(is.na(filled))) {
        non_na <- which(!is.na(filled))
        if (length(non_na) >= 2) {
          x_vals <- 1:length(filled)
          filled <- approx(
            x = x_vals[non_na], 
            y = filled[non_na], 
            xout = x_vals, 
            rule = 2
          )$y
        }
      }

      final_vals <- ifelse(is_1981_1988 | is.na(.x), filled, .x)
      return(final_vals)
    }
  )) %>%
  ungroup() %>%
  select(-is_1981_1988)


merged_data <- Wheat_county_cl_sm_dhw %>%
  left_join(wheat_cost_filled, by = c("year", "Provience"))

for (col in full_cost_cols ) {
  if (col %in% colnames(merged_data)) {
    merged_data[[paste0("Adj_", col)]] <- with(merged_data,( get(col)*15) 
    )
  } else {
    warning(paste("Column", col, "not found in merged_final"))
  }
}


trend_site <- merged_data %>%
  group_by(site) %>%
  do({
    df <- .

    df_valid <- df %>% filter(!is.na(Yield_Y), !is.na(year))

    if (nrow(df_valid) < 2) {
      data.frame(year = df$year,
                 trend_Yield_Y_site = NA)
    } else {
      model <- lm(Yield_Y ~ year, data = df_valid)
      data.frame(year = df$year,
                 trend_Yield_Y_site = predict(model, newdata = df))
    }
  }) %>% ungroup()

trend_site <- trend_site %>% distinct(site, year, trend_Yield_Y_site, .keep_all = TRUE)

trend_prov<- merged_data %>%
  group_by(Provience) %>% do({
    model <- lm(yield ~ year, data = .)
    data.frame(year = .$year,
               trend_yield_prov = predict(model, newdata = .))
  })

trend_prov<- trend_prov %>% distinct(Provience, year,  trend_yield_prov, .keep_all = TRUE)

merged_data <- merged_data %>%
  left_join(trend_site, by = c("site", "year"))

merged_data <- merged_data %>%
  left_join(trend_prov, by = c("Provience", "year"))


for (col in full_cost_cols ) {
  if (col %in% colnames(merged_data)) {
    merged_data[[paste0("Adj_", col)]] <- with(merged_data,(trend_Yield_Y_site* get(col)/(trend_yield_prov)) 
    )
  } else {
    warning(paste("Column", col, "not found in merged_final"))
  }
}


library(pinyin)
library(pinyinlite)
wheat_cost_long <- wheat_cost_filled %>%
  pivot_longer(
    cols = c(Seed_Cost, Fertilizer_Cost,
             Mechanization_Cost, Irrigation_Cost),
    names_to = "Cost_Type",
    values_to = "Cost"
  ) %>%
  mutate(Cost_Type = gsub("_Cost", "", Cost_Type)) 
library(dplyr)
library(tidyr)
library(ggplot2)


wheat_cost_long <- wheat_cost_filled %>%

  pivot_longer(
    cols = c(Seed_Cost, Fertilizer_Cost, Manure_Cost,
             Pesticide_Cost, Mechanization_Cost, Irrigation_Cost),
    names_to = "Cost_Type",
    values_to = "Cost"
  ) 


AgpC_gg = ggplot(wheat_cost_long %>% filter( Cost_Type %in% c("Seed", "Fertilizer", 'Mechanization','Irrigation')), 
                 aes(x = year, y = Cost*15, color = Cost_Type)) +
  geom_line(size = 0.6) +
  facet_wrap(~ Provience, scales = "free_y",ncol=3) +
  labs(
    x = "Year",
    y =  bquote(Wheat~Production~cost~(CNY~per~kg~ha^-1)),
    color = "Cost Type",
    #title = "Trend of Wheat Production Costs by Province"
  ) +scale_color_brewer(palette = "Set2", name = '') + 
  theme_minimal(base_size = 12) +theme_bw(base_size = 12)+
  theme(strip.text = element_text(size = 8),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 6),
        axis.text.x = element_text(size = 8,angle = 45,vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position = c(0.82,0.06),
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 9),
        legend.title= element_text(size = 8),
        # panel.grid =  element_blank(),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))

library(devEMF)
emf('F:/Crop yield loss at DHW/Figures/Figure S9.emf',
     units = "cm", width=20,height=18, pointsize = 11)
AgpC_gg
dev.off()