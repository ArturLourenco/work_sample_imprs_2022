gpp_to_monthly <-  ECtowerData %>%
  group_by(rDate = as.Date(format(rDate, "%Y-%m-%d"))) %>% # group by the day
  summarise_at(c("GPP_uStar_f"), mean, na.rm = TRUE) %>%
  # mutate(GPP_uStar_f =  umolCO2.to.gC(GPP_uStar_f)) %>% # convert to gC m-2 day-1 (* 10^-6 * 12 * 86400) or umolCO2.to.gC() 
  group_by(rDate = format(rDate, "%Y-%m")) %>% # group by the day 
  summarise_at(c("GPP_uStar_f"),sum, na.rm = TRUE) %>% 
  mutate(rDate = as.Date(timeLastDayInMonth(paste(rDate,'01',sep = "-")))) %>% 
  group_by(rDate = format(rDate, "%Y")) %>% # group by the day
  summarise_at(c("GPP_uStar_f"), sum, na.rm = TRUE)

nee_to_monthly <-  ECtowerData %>%
  group_by(rDate = as.Date(format(rDate, "%Y-%m-%d"))) %>% # group by the day
  summarise_at(c("NEE_uStar_f"), mean, na.rm = TRUE) %>%
  group_by(rDate = format(rDate, "%Y-%m")) %>% # group by the day
  summarise_at(c("NEE_uStar_f"),sum, na.rm = TRUE) %>% 
  mutate(rDate = as.Date(timeLastDayInMonth(paste(rDate,'01',sep = "-")))) %>% 
  group_by(rDate = format(rDate, "%Y")) %>% # group by the day
  summarise_at(c("NEE_uStar_f"), sum, na.rm = TRUE)

rain_to_monthly <-  ECtowerData %>%
  group_by(rDate = as.Date(format(rDate, "%Y-%m-%d"))) %>% # group by the day
  summarise_at(c("Rain"), sum, na.rm = TRUE) %>%
  mutate(Rain = Rain) %>% 
  group_by(rDate = format(rDate, "%Y-%m")) %>% # group by the day
  summarise_at(c("Rain"),sum, na.rm = TRUE) %>% 
  mutate(rDate = as.Date(timeLastDayInMonth(paste(rDate,'01',sep = "-")))) %>% 
  group_by(rDate = format(rDate, "%Y")) %>% # group by the day
  summarise_at(c("Rain"), sum, na.rm = TRUE)

cor_data<- ECtowerData %>%
  select(-rDate) %>%
  replace(is.na(.), 0) %>%  
  cor() 

cor_data<- ECtowerData %>%
  select(GPP_uStar_f,NetRad,rh02,rh15, SWDR, Rain, PARd, SWUR, G, PARu, Trad) %>%
  replace(is.na(.), 0) %>%  
  cor() 

corrplot(
  cor_data,
  method = "color",
  type = "lower",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  sig.level = 0.05,
  insig = "pch",
  addrect = 3,
  diag = FALSE )  

corrplot(
  cor_data)
