
# ============================
# packages
# ============================

library(fixest)
library(dplyr)
library(splines)
library(broom)
library(foreach)
library(doParallel)

set.seed(1234)

DHW_vars = c('cumulative_hours_HighTempLowHumidity_Early', 
             'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle',
             'cumulative_hours_PostRainScorch_Early',
             'cumulative_hours_PostRainScorch_Late',
             'cumulative_hours_PostRainScorch_Middle')

Boot_reg_sen_change_sem_foreach = function(start.yr, end.yr, fmula, boot, data, Cost){

  cores <- parallel::detectCores() - 40
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  yrs.to.samp <- start.yr:end.yr
  
  boot_list <- foreach(n = 1:boot, .packages = c("fixest", "dplyr", "broom")) %dopar% {
    
    yrsamp = sample(yrs.to.samp, size = (end.yr-start.yr+1), replace = TRUE)
    tempdf = data %>% filter(year == yrsamp[1]) 
    for (k in 2:length(yrsamp)) {
      tempdf = rbind(tempdf, data %>% filter(year == yrsamp[k]))
    }
    
    vars <- c("cumulative_hours_HighTempLowHumidity_Early",
              "cumulative_hours_HighTempLowHumidity_Late",
              "cumulative_hours_HighTempLowHumidity_Middle",
              "cumulative_hours_PostRainScorch_Early",
              "cumulative_hours_PostRainScorch_Late",
              "cumulative_hours_PostRainScorch_Middle")
    
    # ---- 1.  ----
    trend_results <- lapply(vars, function(v) {
      tempdf %>%
        group_by(year) %>%
        summarise(mean_val = mean(.data[[v]], na.rm = TRUE)) %>%
        ungroup() %>%
        do({
          fm <- glm(mean_val ~ year, data = ., family = poisson())
          tidy(fm)
        }) %>%
        filter(term == "year") %>%
        mutate(variable = v)
    }) %>% bind_rows()
    
    trend_results <- na.omit(trend_results) %>%
      select(variable, mean_coef = estimate)
    
    trend_results <- t(trend_results$mean_coef)
    colnames(trend_results) <- c(
      "Trends_HTLH_Early", "Trends_HTLH_Late", "Trends_HTLH_Middle",
      "Trends_PRGW_Early", "Trends_PRGW_Late", "Trends_PRGW_Middle"
    )
    
    
    # ---- 2. ----
    cost_vars <- c("Adj_Seed_Cost", "Adj_Fertilizer_Cost",                            
                   "Adj_Manure_Cost", "Adj_Pesticide_Cost",                            
                   "Adj_Mechanization_Cost", "Adj_Irrigation_Cost")
    
    cost_trends <- lapply(cost_vars, function(v) {
      tempdf %>%
        group_by(year) %>%
        summarise(mean_val = mean(.data[[v]], na.rm = TRUE)) %>%
        ungroup() %>%
        do({
          fm <- lm(mean_val ~ year, data = .)
          tidy(fm)
        }) %>%
        filter(term == "year") %>%
        mutate(variable = v)
    }) %>% bind_rows()
    
    cost_trends <- na.omit(cost_trends) %>%
      select(variable, mean_coef = estimate)
    
    cost_trends <- t(cost_trends$mean_coef)
    colnames(cost_trends) <- c(
      "Seed", "Fertilizer", "Manure", "Pesticide", "Mechanization", "Irrigation"
    )
    cropfit    = feols(fmula, data = tempdf) 
    coeftable  = cropfit$coeftable
    coeftable$Var = rownames(coeftable)
    
    list(coef = coeftable, trend = trend_results, cost = cost_trends)
  }
  
  stopCluster(cl)  # 停止并行
  
 
  bootcoefs <- do.call(rbind, lapply(boot_list, function(x) x$coef))
  Trend_results <- do.call(rbind, lapply(boot_list, function(x) x$trend))
  Cost_trends   <- do.call(rbind, lapply(boot_list, function(x) x$cost))
  
  HighTempLowHumidity_Early_change =  (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_HighTempLowHumidity_Early:year'] )#(end.yr-start.yr)
  HighTempLowHumidity_Middle_change = (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_HighTempLowHumidity_Middle:year'] )# (end.yr-start.yr)
  HighTempLowHumidity_Late_change =  (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_HighTempLowHumidity_Late:year'] )# (end.yr-start.yr)
  
  PostRainScorch_Early_change =  (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_PostRainScorch_Early:year'] )# (end.yr-start.yr)
  PostRainScorch_Middle_change = (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_PostRainScorch_Middle:year'] )# (end.yr-start.yr)
  PostRainScorch_Late_change =  (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_PostRainScorch_Late:year'] )# (end.yr-start.yr)
  
  
  Mbootcoef = data.frame(#DTWD_Early_change  = DryWind_Early_change,
    #DTWD_Middle_change = DryWind_Middle_change,
    #DTWD_Late_change   = DryWind_Late_change,
    HTLH_Early_change  = HighTempLowHumidity_Early_change,
    HTLH_Middle_change = HighTempLowHumidity_Middle_change,
    HTLH_Late_change   = HighTempLowHumidity_Late_change,
    PRGW_Early_change  = PostRainScorch_Early_change,
    PRGW_Middle_change = PostRainScorch_Middle_change,
    PRGW_Late_change   = PostRainScorch_Late_change)
  
  Mbootcoef = cbind(Mbootcoef, Trend_results, Cost_trends)
  return(Mbootcoef)
}
vars <- c(Ta_vars,Soil_vars,DHW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  
  } else {
    x
  }
})
var_s2     = c(vars_escaped,'year',
               paste0('year:',c('JHEDDNDHW' , 'HMEDDNDHW', 'PJFDDNDHW')), 
               paste0('year:',DHW_vars))
fmula_s2    = as.formula(paste0("Yield_Y~ ", paste0(var_s2, collapse = " + "), "|site "))

SEM_df_12 = Boot_reg_sen_change_sem_foreach(1981, 2018, fmula_s2, 10000, Wheat_county_merged_final, Cost = TRUE)





SEM_scaled <- SEM_df_12 %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))


pairs <- list(

  HighTempLowHumidity_Early  = c("HTLH_Early_change",  "Trends_HTLH_Early"),
  HighTempLowHumidity_Middle = c("HTLH_Middle_change", "Trends_HTLH_Middle"),
  HighTempLowHumidity_Late   = c("HTLH_Late_change",   "Trends_HTLH_Late"),
  PostRainScorch_Early  = c("PRGW_Early_change",       "Trends_PRGW_Early"),
  PostRainScorch_Middle = c("PRGW_Middle_change",      "Trends_PRGW_Middle"),
  PostRainScorch_Late   = c("PRGW_Late_change",        "Trends_PRGW_Late")
)

# 
management_vars <- c("Seed", "Fertilizer",                            
                     "Mechanization", "Irrigation")



Importance = NULL
XGMd = list()
Response = NULL
XGB_OP   = NULL
for (nm in names(pairs)) {
  
  # nm ='HighTempLowHumidity_Early'
  change_var <- pairs[[nm]][1]
  trend_var <- pairs[[nm]][2]
  
  # 
  model_formula <- paste0(change_var, " ~ ", trend_var, " + ", paste(management_vars, collapse = " + "))

  X <- model.matrix(as.formula(model_formula), data = SEM_df_12)[,-1]
  y <- SEM_df_12[,change_var]
  
  dtrain <- xgb.DMatrix(data = X, label = y)
  
  xgb_model <- xgboost(data = dtrain,
                       objective = "reg:squarederror",
                       nrounds = 500,
                       eta = 0.05,      
                       max_depth = 4,   
                       subsample = 0.8,
                       colsample_bytree = 0.8)
  
  predictions <- predict(xgb_model, X)
  
  xgb_op = data.frame(xgb_Pre = predictions, fxm_est =  y, var_hdw = change_var, level = 'County')
  
  XGB_OP = rbind(XGB_OP,xgb_op)
  XGMd = c(XGMd,xgb_model)
  importance <- xgb.importance(model = xgb_model)
  importance$ID = pairs[[nm]][1]
  Importance  = rbind(Importance, importance)
  
  final_results = NULL
 for (i in colnames(X)) {
   pdp_fert <- partial(xgb_model, 
                       pred.var = i , 
                       train = X,
                       type = "regression")
   
   colnames(pdp_fert) = c('x', 'y')
   pdp_fert$Variables = i
   pdp_fert$ID        = pairs[[nm]][1]
   final_results      = rbind(final_results, pdp_fert)
 }

  
  Response = rbind(Response, final_results)
}


library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(unikn)
seecol(pal = pal_unikn_pair)
#  
Importance1 <- Importance %>%
  mutate(Feature = as.factor(Feature),
         ID = as.factor(ID)) %>%
  group_by(ID) %>%
  mutate(percentage = Gain / sum(Gain) * 100) %>%
  ungroup()

# 


Importance1$Feature <- factor(Importance1$Feature, 
                              levels = c('Fertilizer', 'Irrigation', 'Mechanization',
                                          'Seed', 'Trends_HTLH_Early',
                                         'Trends_HTLH_Middle','Trends_HTLH_Late',
										 'Trends_PRGW_Early',
                                         'Trends_PRGW_Middle','Trends_PRGW_Late'))

# 
n_feat <- length(levels(Importance1$Feature))
col= c('#8E2043', '#BC7A8F','#E0607E', '#ECA0B2')


labels_ID <- c(
  "HTLH_Early_change"  = "HTLH-E",
  "HTLH_Middle_change" = "HTLH-M",
  "HTLH_Late_change"   = "HTLH-L",
  "PRGW_Early_change"  = "PRGW-E",
  "PRGW_Middle_change" = "PRGW-M",
  "PRGW_Late_change"   = "PRGW-L"
)

Importance1$ID <- factor(
  Importance2$ID,
  levels = c("HTLH_Early_change", "HTLH_Middle_change", "HTLH_Late_change",
             "PRGW_Early_change", "PRGW_Middle_change", "PRGW_Late_change")
)

Importance_gg = ggplot(Importance1, aes(x = 1, y = percentage, fill = Feature)) +
  geom_col(color = "white", width = 1) +# coord_polar(theta = "y") +
  xlab("") + ylab("") +facet_wrap(. ~ ID,ncol = 6,
    labeller = as_labeller(labels_ID, default = label_parsed)) +
  geom_text(
    aes(label = ifelse(percentage >= 1, paste0(round(percentage, 1), "%"), "")),
    position = position_stack(vjust = 0.5),color = 'white', size = 2.0) +
  scale_fill_manual(name = '',values = c(col, c("gray66","gray68","gray70","gray72","gray74","gray76")),
    labels = c(
      "Trends_HTLH_Early"  = "HTLH-E",#bquote(HDW[HTLH-E]),
      "Trends_HTLH_Middle" = "HTLH-M",#bquote(HDW[HTLH-M]),
      "Trends_HTLH_Late"   = "HTLH-L",#bquote(HDW[HTLH-L]),
      "Trends_PRGW_Early"  = "PRGW-E",#bquote(HDW[PRGW-E]),
      "Trends_PRGW_Middle" = "PRGW-M",#bquote(HDW[PRGW-M]),
      "Trends_PRGW_Late"   = "PRGW-L"#bquote(HDW[PRWG-L]))
        )) +
  theme_bw() +labs(subtitle = "(a) Importance of drivers in XGBoost at the county scale") +
  theme(plot.subtitle = element_text(size = 10),
        panel.spacing = unit(0.1, units = "cm"),
    strip.text = element_text(size = 7),
    legend.key.size = unit(0.3,'cm'),
    text = element_text(size = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 5),
    legend.position = 'bottom',#c(-0.6,0.9),
    legend.key.height = unit(0.2,'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 5),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent", colour = NA_character_)
  )
















facet_map <- c(
  Trends_HTLH_Early  = "HTLH-E",
  Trends_HTLH_Middle = "HTLH-M",
  Trends_HTLH_Late   = "HTLH-L",
  Trends_PRGW_Early  = "PRGW-E",
  Trends_PRGW_Middle = "PRGW-M",
  Trends_PRGW_Late   = "PRGW-L"
)

Response <- Response %>%
  mutate(Variables_parsed = ifelse(Variables %in% names(facet_map),
                                   facet_map[Variables],
                                   Variables)) 

legend_map <- c(
  HTLH_Early_change  = "HTLH-E",
  HTLH_Middle_change = "HTLH-M",
  HTLH_Late_change   = "HTLH-L",
  PRGW_Early_change  = "PRGW-E",
  PRGW_Middle_change = "PRGW-M",
  PRGW_Late_change   = "PRGW-L"
)

legend_levels <- unique(as.character(Response$ID))  # or levels(Response$ID) if it's a factor
 
legend_labels_char <- sapply(legend_levels, function(x) {
  if (x %in% names(legend_map)) legend_map[[x]] else x
}, USE.NAMES = FALSE)

 
n_legend <- length(legend_levels)
cols <- brewer.pal(8, "Set2")[c(1:6)]

 
desired_order <- c(
  "HTLH-E", "HTLH-M", "HTLH-L",
  "PRGW-E", "PRGW-M","PRGW-L",
  "Fertilizer", "Irrigation", 
  "Mechanization", "Seed"
)

# 
Response$Variables_parsed <- factor(as.character(Response$Variables_parsed),
                                    levels = desired_order)



p_1 <- ggplot(subset(Response, !Variables %in% c("Fertilizer", "Irrigation", "Manure",
                                                 "Mechanization", "Pesticide", "Seed")), 
            aes(x = x, y = y, color = ID)) +
  geom_line(size = 0.6) +facet_wrap(~ Variables_parsed, scales = "free_x", 
                                    labeller = label_parsed, ncol = 3) +
  ylab(bquote(italic(beta)['HDW × year'] ~ (kg~ha^-1~hour^-1~year^-1)))+
  xlab(bquote('Trend of HDW '(hour~year^-1))) +
  labs(subtitle = "(b) Partial dependence of the drivers at the county scale") +
  scale_color_manual(name = '',values = cols,breaks = legend_levels,
    labels = parse(text = legend_labels_char)  # parse -> expression for legend
  ) +theme_bw() +
  theme(plot.subtitle = element_text(size = 10),
    strip.text = element_text(size = 8),
    legend.key.size = unit(0.3,'cm'),
    text = element_text(size = 8.5),
    axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 8.5),
    legend.position = c(.08, 0.92),
    legend.key.height = unit(0.2,'cm'),
    legend.background = element_blank(),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    strip.background = element_rect(color = 'transparent'),
    plot.background = element_rect(fill = "transparent", colour = NA_character_)
  )



p_2 <- ggplot(subset(Response, Variables %in% c("Fertilizer", "Irrigation", "Manure",
                                                   "Mechanization", "Pesticide", "Seed")), 
              aes(x = x, y = y, color = ID)) +
  geom_line(size = 0.6) +facet_wrap(~ Variables_parsed, scales = "free_x", 
                                    labeller = label_parsed, ncol = 4) +
  ylab(bquote(italic(beta)['HDW × year'] ~ (kg~ha^-1~hour^-1~year^-1)))+
  xlab(bquote('Trend of Cost '(CNY~year^-1~kg~ha^-1))) +
  labs(subtitle = "(a) Partial dependence of the drivers at the county scale") +
  scale_color_manual(name = '', values = cols, breaks = legend_levels,
    labels = parse(text = legend_labels_char)  # parse -> expression for legend
  ) +theme_bw() +guides(color = guide_legend(ncol = 6))+
  theme(plot.subtitle = element_text(size = 10),
        strip.text = element_text(size = 8),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 8.5),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 8.5),
        legend.position = 'top',
        legend.key.height = unit(0.2,'cm'),
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent", colour = NA_character_)
  )


# ============================
# field scale estimation
# ============================

DHW_vars = c('cumulative_hours_HighTempLowHumidity_Early', 
             'cumulative_hours_HighTempLowHumidity_Late',
             'cumulative_hours_HighTempLowHumidity_Middle',
             'cumulative_hours_PostRainScorch_Late',
             'cumulative_hours_PostRainScorch_Middle')

Cost_vars = c("Adj_Seed_Cost",          "Adj_Fertilizer_Cost",                            
              "Adj_Mechanization_Cost", "Adj_Irrigation_Cost" )

Boot_reg_sen_change_sem_foreach_v2 = function(start.yr, end.yr, fmula, boot, data, Cost){
  library(doRNG)
  set.seed(12345)
  cores <- parallel::detectCores() - 40
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  yrs.to.samp <- start.yr:end.yr
  nyr = end.yr - start.yr + 1
  sites = unique(data$site) 
  Culs = unique(data$Culs) 
  boot_list <- foreach(n = 1:boot, .packages = c("fixest", "dplyr", "broom"), 
                        .options.RNG = 12345) %dorng% {
                          samp_sites = sample(sites, size = length(sites), replace = TRUE) 
                          samp_culs       = sample(Culs, size = length(Culs), replace = TRUE) 
                           tempdf = lapply(samp_sites, function(s){
                            dats = data %>% filter(site == s, year %in% yrs.to.samp, Culs %in% samp_culs)
                            dats$draw_site_id = s 
                            dats
                          }) %>% bind_rows()

    vars <- c('cumulative_hours_HighTempLowHumidity_Early', 
              'cumulative_hours_HighTempLowHumidity_Late',
              'cumulative_hours_HighTempLowHumidity_Middle',
              'cumulative_hours_PostRainScorch_Late',
              'cumulative_hours_PostRainScorch_Middle')
    
    # ---- 1. ----
    
    trend_results <- lapply(vars, function(v) {
      tempdf %>%
         group_by(year) %>%
         summarise(mean_value = mean(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
         do({
           fm <- glm(mean_value ~ year, data = ., family = poisson())
           tidy(fm)
         }) %>%
        filter(term == "year") %>%
        summarise(mean_coef = mean(estimate, na.rm = TRUE)) %>%
        mutate(variable = v)
    }) %>% bind_rows()
    trend_results = na.omit(trend_results)%>%  group_by(variable) %>% summarise(mean_coef = mean(mean_coef, na.rm = TRUE)) 
    
    trend_results <- t(trend_results$mean_coef)
    colnames(trend_results) <- c("Trends_HTLH_Early",
                                 "Trends_HTLH_Late",
                                 "Trends_HTLH_Middle",
                                  "Trends_PRGW_Late",
                                 "Trends_PRGW_Middle")
    
    # ---- 2. ----
    cost_vars <- c("Adj_Seed_Cost", "Adj_Fertilizer_Cost",                            
                   "Adj_Manure_Cost", "Adj_Pesticide_Cost",                            
                   "Adj_Mechanization_Cost", "Adj_Irrigation_Cost")
    
    cost_trends <- lapply(cost_vars, function(v) {
      tempdf %>% filter(!is.na(.data[[v]]))  %>%
        group_by(year) %>%
       summarise(mean_value = mean(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
        do({
          fm <- glm(mean_value ~ year, data = .)
          # fm <- glm(as.formula(paste0(v, " ~ year")), data = .)
          tidy(fm)
        }) %>%
        filter(term == "year") %>%
        summarise(mean_coef = mean(estimate, na.rm = TRUE)) %>%
        mutate(variable = v)
    }) %>% bind_rows()
    
    cost_trends = na.omit(cost_trends)%>%  group_by(variable) %>% summarise(mean_coef = mean(mean_coef, na.rm = TRUE)) 
    
    breeding = mean(tempdf$release_year)
    cost_trends <- t(cost_trends$mean_coef)
    colnames(cost_trends) <- c("Seed", "Fertilizer",                            
                               "Manure", "Pesticide",                            
                               "Mechanization", "Irrigation")
    cropfit    = feols(fmula, data = tempdf) 
    coeftable  = cropfit$coeftable
    coeftable$Var = rownames(coeftable)
    
    list(coef = coeftable, trend = trend_results, cost = cost_trends, breedings = breeding)
  }
  
  stopCluster(cl)  
  
  # 
  bootcoefs <- do.call(rbind, lapply(boot_list, function(x) x$coef))
  Trend_results <- do.call(rbind, lapply(boot_list, function(x) x$trend))
  Cost_trends   <- do.call(rbind, lapply(boot_list, function(x) x$cost))
  Breeding   <- do.call(rbind, lapply(boot_list, function(x) x$breedings))
  # 
 
  HTLH_Early_change =  (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_HighTempLowHumidity_Early:year'] )#  (end.yr-start.yr)
  HTLH_Middle_change = (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_HighTempLowHumidity_Middle:year'] )#  (end.yr-start.yr)
  HTLH_Late_change =  (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_HighTempLowHumidity_Late:year'] )# (end.yr-start.yr)
  
  PRGW_Middle_change = (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_PostRainScorch_Middle:year'] )# (end.yr-start.yr)
  PRGW_Late_change =  (bootcoefs$Estimate[bootcoefs$Var=='cumulative_hours_PostRainScorch_Late:year'] )#  (end.yr-start.yr)
  
  
  Mbootcoef <- bind_cols(
    HTLH_Early_change  = tibble(HTLH_Early_change),
    HTLH_Middle_change = tibble(HTLH_Middle_change),
    HTLH_Late_change   = tibble(HTLH_Late_change),
    PRGW_Middle_change = tibble(PRGW_Middle_change),
    PRGW_Late_change   = tibble(PRGW_Late_change)
  )
  
  Mbootcoef = cbind(Mbootcoef, Trend_results, Cost_trends,Breeding)
  return(Mbootcoef)
}

vars <- c(Ta_vars,Soil_vars,DHW_vars,Cost_vars)
vars_escaped <- sapply(vars, function(x) {
  if (grepl(".", x)) {
    paste0("`", x, "`")  # 包裹反引号
  } else {
    x
  }
})
var_s2     = c(vars_escaped,'year',
               # paste0('year:',c('JHEDDNDHW' , 'HMEDDNDHW', 'PJFDDNDHW')),
               paste0('year:',DHW_vars))
fmula_f2    = as.formula(paste0("Yield_S~ ", paste0(var_s2, collapse = " + "), "|site"))

SEM_field_df_1 = Boot_reg_sen_change_sem_foreach_v2(2006, 2018, fmula_f2, 10000, Wheat_exp_merged_final_df, Cost = TRUE)

############################################################
############################################################
############################################################

pairs <- list(
  HighTempLowHumidity_Early  = c("HTLH_Early_change",  "Trends_HTLH_Early"),
  HighTempLowHumidity_Middle = c("HTLH_Middle_change", "Trends_HTLH_Middle"),
  HighTempLowHumidity_Late   = c("HTLH_Late_change",   "Trends_HTLH_Late"),
  PostRainScorch_Middle = c("PRGW_Middle_change",      "Trends_PRGW_Middle"),
  PostRainScorch_Late   = c("PRGW_Late_change",        "Trends_PRGW_Late")
)


management_vars <- c("Seed", "Fertilizer",                                                        
                     "Mechanization", "Irrigation")
Breeding_vars = 'Breeding'

Importance_f = NULL
XGMd_f = list()
Response_f = NULL
XGB_OP_f   = NULL
for (nm in names(pairs)) {
  change_var <- pairs[[nm]][1]
  trend_var <- pairs[[nm]][2]
  # 
  model_formula <- paste0(change_var, " ~ ",  trend_var,' + ', paste(management_vars, collapse = " + "))
  # SEM_field_df_1 = SEM_field_df_1_filtered
  X <- model.matrix(as.formula(model_formula), data = SEM_field_df_1)[,-1]
  y <- SEM_field_df_1[,change_var]
  
  dtrain <- xgb.DMatrix(data = X, label = y)
  
  xgb_model <- xgboost(data = dtrain,
                       objective = "reg:squarederror",
                       nrounds = 500,
                       eta = 0.05,      
                       max_depth = 4,   
                       subsample = 0.8,
                       colsample_bytree = 0.8)
  
  predictions <- predict(xgb_model, X)
  
  xgb_op = data.frame(xgb_Pre = predictions, fxm_est =  y, var_hdw = change_var, level = 'Field')
  
  XGB_OP_f = rbind(XGB_OP_f,xgb_op)
  XGMd = c(XGMd,xgb_model)
  importance <- xgb.importance(model = xgb_model)
  importance$ID = pairs[[nm]][1]
  Importance_f  = rbind(Importance_f, importance)
  
  final_results = NULL
  for (i in colnames(X)) {
    pdp_fert <- partial(xgb_model, 
                        pred.var = i , 
                        train = X, 
                        type = "regression")
    
    
    colnames(pdp_fert) = c('x', 'y')
    pdp_fert$Variables = i
    pdp_fert$ID        = pairs[[nm]][1]
    final_results      = rbind(final_results, pdp_fert)
  }
  
  
  Response_f = rbind(Response_f, final_results)
}



Importance2 <- Importance_f %>% 
  group_by(ID) %>%
  mutate(percentage = Gain / sum(Gain) * 100) %>%
  ungroup()


Importance2_f$Feature <- factor(Importance2_f$Feature, 
                              levels = c('Breeding','Fertilizer', 'Irrigation', 'Mechanization',
                                          'Seed', 'Trends_HTLH_Early',
                                         'Trends_HTLH_Middle','Trends_HTLH_Late',  'Trends_PRGW_Early',
                                         'Trends_PRGW_Middle','Trends_PRGW_Late'))

n_feat <- length(levels(Importance2_f$Feature))
col= c('#8E2043', '#BC7A8F','#E0607E', '#ECA0B2')

labels_ID <- c(
  "HTLH_Early_change"  = "HTLH-E",
  "HTLH_Middle_change" = "HTLH-M",
  "HTLH_Late_change"   = "HTLH-L",
  "PRGW_Middle_change" = "PRGW-M",
  "PRGW_Late_change"   = "PRGW-L"
)

Importance2_f$ID <- factor(
  Importance2_f$ID,
  levels = c("HTLH_Early_change", "HTLH_Middle_change", "HTLH_Late_change",
             "PRGW_Middle_change", "PRGW_Late_change")
)

Importance_f_gg = ggplot(Importance2, aes(x = 1, y = percentage, fill = Feature)) +
  geom_col(color = "white", width = 1) +
  xlab("") + ylab("") +
  facet_wrap(. ~ ID, ncol =5,
    labeller = as_labeller(labels_ID, default = label_parsed)) +
  geom_text(aes(label = ifelse(percentage >= 1, paste0(round(percentage, 1), "%"), "")),
    position = position_stack(vjust = 0.5),color = 'white', size = 2.0) +
  scale_fill_manual(name = '',
    values = c(col, c("gray66","gray68","gray70","gray72","gray74","gray76")),
    labels = c(
      "Trends_HTLH_Early"  = "HTLH-E",
      "Trends_HTLH_Middle" = "HTLH-M",
      "Trends_HTLH_Late"   = "HTLH-L",
      "Trends_PRGW_Middle" = "PRGW-M",
      "Trends_PRGW_Late"   = "PRGW-L" )) +
  theme_bw() +labs(subtitle = "(c) Importance of drivers in XGBoost at the field scale") +
  theme(plot.subtitle = element_text(size = 10),
        strip.text = element_text(size = 7),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 5),
        legend.position = 'bottom',#c(0.8,0.3),
        legend.key.height = unit(0.2,'cm'),
        panel.spacing = unit(0.1, units = "cm"),
        legend.background = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 5),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent", colour = NA_character_))






facet_map <- c(
  Trends_HTLH_Early  = "HTLH-E",
  Trends_HTLH_Middle = "HTLH-M",
  Trends_HTLH_Late   = "HTLH-L",
  Trends_PRGW_Middle = "PRGW-M",
  Trends_PRGW_Late   = "PRGW-L"
)

Response_f <- Response_f %>%
  mutate(Variables_parsed = ifelse(Variables %in% names(facet_map),
                                   facet_map[Variables],
                                   Variables)) 

legend_map <- c(
  HTLH_Early_change  = "HTLH-E",
  HTLH_Middle_change = "HTLH-M",
  HTLH_Late_change   = "HTLH-L",
  PRGW_Middle_change = "PRGW-M",
  PRGW_Late_change   = "PRGW-L"
)

legend_levels <- unique(as.character(Response_f$ID))  # or levels(Response$ID) if it's a factor

legend_labels_char <- sapply(legend_levels, function(x) {
  if (x %in% names(legend_map)) legend_map[[x]] else x
}, USE.NAMES = FALSE)

n_legend <- length(legend_levels)
cols <- brewer.pal(8, "Set2")[c(1:3,5,6)]

desired_order <- c(
  "HTLH-E", "HTLH-M","HTLH-L",
  "PRGW-M","PRGW-L",'Breeding',
  "Fertilizer", "Irrigation", "Manure",
  "Mechanization", "Pesticide", "Seed"
)

Response_f$Variables_parsed <- factor(as.character(Response_f$Variables_parsed),
                                    levels = desired_order)

library(ggh4x)
custom_x_scales <- list(
  scale_x_continuous(limits = c(-0.225, 0.0),
                     breaks = seq(-0.225, 0.0, by = 0.1)),
  scale_x_continuous(limits = c(-0.15, 0.05),
                     breaks = seq(-0.15, 0.05, by = 0.075)),
  scale_x_continuous(limits = c(-0.075, 0.038),
                     breaks = seq(-0.075, 0.04, by = 0.05))
)
custom_y_scales <- list(
  scale_y_continuous(limits = c(-100, 100)), # For cyl = 4
  scale_y_continuous(limits = c(-100, 10)), # For cyl = 6
  scale_y_continuous(limits = c(-50, 50)),  # For cyl = 8
  scale_y_continuous(limits = c(-250, 250)), # For cyl = 4
  scale_y_continuous(limits = c(-50, 50)), # For cyl = 6
  scale_y_continuous(limits = c(-50, 50))  # For cyl = 8
)

p_field_1_1 <- ggplot(subset(Response_f, Variables %in% c('Trends_HTLH_Early', 
                                                          'Trends_HTLH_Middle',
                                                          'Trends_HTLH_Late'  )), 
  aes(x = x, y = y, color = ID)) +geom_line(size = 0.6) +
  facet_wrap(~ Variables_parsed, scales = "free_x",
             labeller = label_parsed, ncol = 3) +
  xlab("")+ylab("")+
  labs(subtitle = "(d) Partial dependence of the drivers at the field scale") +
  scale_color_manual( name = '',values = cols,breaks = legend_levels,
    labels = parse(text = legend_labels_char)  # parse -> expression for legend
  ) +theme_bw()+#ylim(-50,50)+
  theme(plot.subtitle = element_text(size = 10),
        strip.text = element_text(size = 8),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 8.5),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position = 'none',#c(.5, -0.07),
        legend.key.height = unit(0.2,'cm'),
        #legend.direction = 'horizontal',
        panel.spacing = unit(0.2, units = "cm"),
        legend.background = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent", colour = NA_character_)
  )

p_field_1_2 <- ggplot(subset(Response_f, Variables %in% c('Trends_PRGW_Middle',
                                                           'Trends_PRGW_Late'  )), 
                      aes(x = x, y = y, color = ID)) +geom_line(size = 0.6) +
  facet_wrap(~ Variables_parsed, scales = "free_x",
             labeller = label_parsed, ncol = 2) +
  ylab(bquote(italic(beta)['HDW × year'] ~ (kg~ha^-1~hour^-1~year^-1)))+
  xlab(bquote('Trend of HDW '(hour~year^-1))) +
  scale_color_manual( name = '',values = cols,breaks = legend_levels,
                      labels = parse(text = legend_labels_char)  # parse -> expression for legend
  ) +theme_bw()+
  theme(plot.subtitle = element_text(size = 10),
        strip.text = element_text(size = 8),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 8.5),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position = 'none',#c(.5, -0.07),
        legend.key.height = unit(0.2,'cm'),
        #legend.direction = 'horizontal',
        panel.spacing = unit(0.2, units = "cm"),
        legend.background = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent", colour = NA_character_),
        axis.title.y.left = element_text(hjust = -0.2)
  )


p_field_2 <- ggplot(subset(Response_f, Variables %in% c("Fertilizer", "Irrigation", "Manure",
                                                         "Mechanization", "Pesticide", "Seed")),
                    aes(x = x, y = y, color = ID)) +
  geom_line(size = 0.6) +facet_wrap(~ Variables_parsed, scales = "free_x",
                                    labeller = label_parsed, ncol = 4) +
  ylab(bquote(italic(beta)['HDW × year'] ~ (kg~ha^-1~hour^-1~year^-1)))+
  xlab(bquote('Trend of cost '(CNY~year^-1~kg~ha^-1))) +
  labs(subtitle = "(b) Partial dependence of the drivers at the field scale") +
  scale_color_manual( name = '',values = cols,breaks = legend_levels,
                      labels = parse(text = legend_labels_char)  # parse -> expression for legend
  ) +theme_bw() +
  theme(plot.subtitle = element_text(size = 10),
        strip.text = element_text(size = 8),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 8.5),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position = 'none',#c(.5, -0.07),
        legend.key.height = unit(0.2,'cm'),
        #legend.direction = 'horizontal',
        panel.spacing = unit(0.2, units = "cm"),
        legend.background = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent", colour = NA_character_)
  )


emf('.../Figure 5.emf', coordDPI = 600,
    units = "cm", width=20, height= 12,  pointsize = 15, family = "Arial")# 禁止栅格化
ggdraw() +
  draw_plot(Importance_gg, x = -0.02, y = 0,  width =.45, height = 1) +
  draw_plot(p_1,  x = 0.43, y = 0,   width =.57, height =1)
dev.off()

emf('.../Figure S28.emf', coordDPI = 600,
    units = "cm", width=20, height= 12,  pointsize = 10, family = "Arial")# 禁止栅格化
ggdraw() +
  draw_plot(Importance_f_gg,  x = -0.02, y = -.01,  width =.45, height = 1) +
  draw_plot(p_field_1_1,  x = 0.43, y = 0.50,   width =.565, height =0.50)+
  draw_plot(p_field_1_2,  x = 0.43, y = -0.01,   width =.57, height =0.50)
dev.off()

emf('.../Figures/Figure S29', coordDPI = 600,
    units = "cm", width=14.6, height= 14,  pointsize = 15, family = "Arial") 
ggdraw() +
  draw_plot(p_2,  x = 0, y = 0.45,   width =1, height =0.55)+
  draw_plot(p_field_2,  x = 0, y = 0,   width =1, height =.45)
dev.off()

library(ggplot2)
library(ggpmisc)

XGB_OP_df = rbind(XGB_OP, XGB_OP_f)


emf('.../Figures/Figure S30.emf',
     units = "cm", width=24, height= 18,  pointsize = 11)

ggplot(XGB_OP_df, aes(x = fxm_est, y = xgb_Pre, color = level)) +
  geom_point(alpha = 0.3, size = 1.0) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "red", linewidth = 0.8) +
  facet_wrap(level~ var_hdw, scales = "free") +
  stat_poly_line(aes(group = 1), color = "red") +
  stat_poly_eq(aes(group = 1, 
                   label = paste(after_stat(eq.label), 
                                 after_stat(rr.label), 
                                 sep = "~~~")),
               formula = y ~ x, 
               parse = TRUE,
               label.x = "right", label.y = "top",
               size = 3,color = 'black') +
  scale_color_manual(values = c("#1f77b4","#1a57b0"),) +
  labs(x = bquote('FIE estimation'~(kg~ha^-1~hour^-1)),
       y = bquote('XGB Prediction'~(kg~ha^-1~hour^-1)), 
       title = "", color = " ") +
  theme_bw() +
  theme(plot.subtitle = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.key.size = unit(0.3,'cm'),
        text = element_text(size = 8.5),
        axis.text.x = element_text(size = 9, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        legend.position = 'none',
        legend.key.height = unit(0.2,'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        strip.background = element_rect(color = 'transparent'),
        plot.background = element_rect(fill = "transparent", colour = NA_character_)
  )
dev.off()
