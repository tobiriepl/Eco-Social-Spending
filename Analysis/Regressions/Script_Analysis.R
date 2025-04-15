# data analysis
library(tidyverse)        # For data manipulation (dplyr, ggplot2, etc.)
library(readxl)           # For reading Excel files: read_excel()
library(plm)              # For panel regressions: pdata.frame(), plm()


# load data
data_pooled = read_excel("Data/Overview/data_pooled.xlsx")  #mutate(across(.cols = contains("gov_spending_tot_g"), .fns = ~ . * 100))


############### analysis

##### regressions
# converting for regressions
data_pooled_plm <- pdata.frame(data_pooled,  index = c("year"))     # "name" controls too much

#View(data_pooled_plm )

# regressions (using gloria data)
# model gov_g                                    
model_gov_g <- plm(             #              # main model: control variable: total_gov_spending_cap_g -> health_envi:  8.02***, soc: 3.21***,  edu_recr: 5.17***, hous: 17.51***, indu: 3.24***, ord_def: -8.52***, admin: 26.65***
  thresholds ~ total_gov_spending_cap_g   +   # # adjusted r2:  health_envi: 0.41 , soc: 0.35, edu_recr: 0.35, hous: 0.33  , indu: 0.33,  ord_def: 0.55,    admin: 0.34
  hous_gov_spending_tot_g ,         
  model = "within",              # alternative: # control variable: total_gov_spending_gdp_g -> health_envi: 10.41***, soc = 4.20***, edu_recr: 6.61***, hous: 17.30***, indu: 2.59*, ord_def: -10.03***, admin: 30.76***
  data = data_pooled_plm          
)                               
summary(model_gov_g)


# model hh_g                     
model_hh_g <- plm(                              
  thresholds ~ gdp_cap +      
   health_envi_hh_spending_tot_g,             #  health_envi: 7.10***, soc: -6.46, edu_recr: 2.39, hous: 4.29***, indu: -5.17***, ord_def: -21.09**, admin: 6.09***
  model = "within",                  
  data = data_pooled_plm        # adjusted r2: health_envi: 0.68, soc: 0.67, edu_recr: 0.67, hous: 0.68, indu: 0.69, ord_def: 0.67, admin: 0.68
)                                

summary(model_hh_g)





