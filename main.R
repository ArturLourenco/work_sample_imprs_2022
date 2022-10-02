# This is the draft code

# 1 Clean console and cache -------------------------------------------------------------------

gc(TRUE) 
rm(list = ls()) 
dev.off() 
cat("\014") 
Sys.setlocale(locale = "Portuguese")

# 2 Loading packages, external files and set work directory -------------------------------------------------------------

list.of.packages <-
  c(
    "colorRamps",
    "ggplot2",
    "zoo",
    "RColorBrewer",
    "ggrepel",
    "sf",
    "rgeos",
    "ggforce",
    "scales",
    "lubridate",
    "tidyverse",
    "rayshader",
    "timeDate",
    "corrplot",
    "bigleaf",
    "patchwork",
    "leaflet",
    "ggpmisc"
  ) # packages list

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])] # checar se h? algum pacote novo

if (length(new.packages))
  install.packages(new.packages) # installs new packages if required

lapply(list.of.packages, library, character.only = TRUE) # loading packages

setwd("G:/My Drive/Phd/MPI/MPI 2022/work_sample/work_sample_imprs_2022") # set work directory

# 3 Import dataset -------------------------------------------------------------

ECtowerData<- read_csv("IMPRS_1_ECtowerData.csv")

View(ECtowerData) # visual checking data imported


# 4 Study site -------------------------------------------------------------------------

MajadasStation<- data.frame(lat=c(39.934583,39.942692, 39.940337), long=c(-5.775889,-5.778656,-5.774675), name= c("Tower South","Tower North","Tower East")) # from http://www.europe-fluxdata.eu/home/site-details?id=ES-LM2

leaflet(data = MajadasStation) %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addMarkers(~long, ~lat, popup = ~as.character(name), label = ~as.character(name),group = "Majadas del Tietar Flux Towers") %>% 
  addLayersControl(
    overlayGroups = c("Majadas del Tietar Flux Towers"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addMiniMap() %>% 
  setView(-5.778126, 39.940683, zoom = 16)

# 5 Data preprocessing -------------------------------------------------------------

na_count <-  ECtowerData %>%
  summarise(across(everything(), ~ sum(is.na(.))))  

na_count_biomet <- head(ECtowerData[which(is.na(ECtowerData$Wspd)),13:31])

length(ECtowerData$rDate) != length(seq(as.POSIXct(first(ECtowerData$rDate)), as.POSIXct(last(ECtowerData$rDate)), by=30*60))


# 6 Half-hourly Carbon fluxes -------------------------------------------------------------------------

ECtowerData_longer <-  ECtowerData %>%
  select(c(rDate, GPP_uStar_f, NEE_uStar_f, Reco_uStar)) %>% 
  # mutate_at(c("GPP_uStar_f", "NEE_uStar_f", "Reco_uStar"), cumsum) %>%
  pivot_longer(!rDate, names_to = "variable", values_to = "value")

fill_labels <- setNames(c("NEE", "GPP", "Reco"),
                        c("NEE_uStar_f", "GPP_uStar_f", "Reco_uStar"))

ggplot(ECtowerData_longer, aes(x = rDate, y = value, group = variable, col = variable)) +
  geom_line() +
  labs(
    title = "Carbon Fluxes Variation at Half-hourly Scale",
    subtitle = "Series 2018-2019 from the experimental site Majadas del Tietar",
    y = "",
    x = "Half-hourly",
    caption = "Data Source: MPI/BGI-EcoMet."
  ) +  # title and caption
  scale_color_discrete(name = 'Variables', labels = fill_labels) +
  theme_bw() + 
  facet_wrap(~ variable, scales = "free_y",ncol = 1,  strip.position = "left", 
             labeller = as_labeller(c(NEE_uStar_f = "(umol CO2 m-2 s-1)", GPP_uStar_f = " ", Reco_uStar = " ") ) )  +
  ylab(NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside")

co2_to_monthly <-  ECtowerData %>%
  select(c(rDate, GPP_uStar_f, NEE_uStar_f, Reco_uStar)) %>% 
  group_by(rDate = as.Date(format(rDate, "%Y-%m-%d"))) %>% # group by the day
  summarise_at(c("GPP_uStar_f", "NEE_uStar_f", "Reco_uStar"), mean, na.rm = TRUE) %>%
  mutate_at(c("GPP_uStar_f", "NEE_uStar_f", "Reco_uStar"), umolCO2.to.gC) %>% 
  group_by(rDate = format(rDate, "%Y-%m")) %>%
  summarise_at(c("GPP_uStar_f", "NEE_uStar_f", "Reco_uStar"),sum, na.rm = TRUE) %>% 
  mutate(rDate = as.Date(timeLastDayInMonth(paste(rDate,'01',sep = "-"))))

co2_to_annual <- co2_to_monthly %>% 
  group_by(rDate = format(rDate, "%Y")) %>%
  summarise_at(c("GPP_uStar_f", "NEE_uStar_f", "Reco_uStar"),sum, na.rm = TRUE) %>%   t() %>% 
  as.data.frame()

co2_to_annual<- co2_to_annual[-1,]
co2_to_annual<- cbind(Var=c("GPP", "NEE", "Reco"),co2_to_annual)
colnames(co2_to_annual)<- c("Var","2018", "2019")
rownames(co2_to_annual)<- c("GPP", "NEE", "Reco")

m_abb<- data.frame(m=1:12,abb=month.abb)

co2_to_monthly_long <- co2_to_monthly %>% 
  pivot_longer(!rDate, names_to = "variable", values_to = "value")

p1<- ggplot(co2_to_monthly_long,
            aes(x = as.factor(rDate), y = value, fill = variable)) +
  geom_bar(stat="identity") + 
  labs(
    title = "Carbon Fluxes Variation at Monthly Scale",
    subtitle = "Series 2018-2019 from the experimental site Majadas del Tietar",
    y = "",
    x = "Monthly",
    caption = "Data Source: MPI/BGI-EcoMet."
  ) +  # title and caption
  scale_color_discrete(name = 'Variables', labels = fill_labels) +
  scale_x_discrete(labels = rep(m_abb$abb,2)) +
  theme_bw() + 
  facet_wrap(~ variable, scales = "free_y",ncol = 1,  strip.position = "left", 
             labeller = as_labeller(c(NEE_uStar_f = "(gC m-2 )", GPP_uStar_f = " ", Reco_uStar = " ") ) )  +
  ylab(NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside") 

ggdraw(p1) + draw_grob(tableGrob(co2_to_annual, rows=NULL), x=.8, y=0.1, width=0.2, height=0.2)


# 8 Carbon fluxes Drivers -------------------------------------------------------------------------

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

HHly_mean <-  ECtowerData %>%
  select(rDate,GPP_uStar_f,SWDR) %>% 
  group_by(rDate = format(rDate, "%H:%M")) %>%  # group by the day
  summarise(across(everything(), .f = list(mean = mean, max = max, min = min), na.rm = TRUE)) %>%
  pivot_longer(!rDate, names_to = "variable", values_to = "value") %>% 
  mutate(group = if_else(str_match(variable,"GPP_uStar_f") == "GPP_uStar_f","GPP_uStar_f","SWDR",missing = "SWDR"))

ggplot(HHly_mean, aes(x = rDate, y = value, group = group, col = variable)) +
  geom_point() +
  labs(
    title = "Half-hourly Mean, Max and Min Variation of GPP and SWDR",
    subtitle = "Series 2018-2019",
    y = "GPP (umol CO2 m-2 s-1)",
    x = "Half-hourly",
    caption = "Data Source: MPI/BGI-EcoMet."
  ) +  # title and caption
  scale_x_discrete(breaks = unique(HHly_mean$rDate)[seq(1,48,4)]) +
  scale_color_discrete(name = 'Variables') +
  theme_bw() + 
  facet_wrap(~ group,scales = "free_y",ncol = 1,  strip.position = "left", 
             labeller = as_labeller(c(Rain = "SWDR (?)", GPP_uStar_f = "GPP (umol CO2 m-2 s-1)") ) )  +
  ylab(NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside")

# linear regression plots

l1<- ggplot(data = ECtowerData %>% filter(rDate >= "2019-05-08 00:00:00" & rDate <= "2019-05-08 23:30:00")
            , aes(x = SWDR, y = GPP_uStar_f)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(
    title = "Day in the Rainy Season",
    # subtitle = "",
    y = "GPP (mmol H2O m-2 s-1)",
    x = "SWDR (?)",
    caption = "Data Source: MPI/BGI-EcoMet."
  ) +
  theme_bw() 

l2<- ggplot(data = ECtowerData %>% filter(rDate >= "2019-03-12 00:00:00" & rDate <= "2019-03-12 23:30:00")
            , aes(x = SWDR, y = GPP_uStar_f)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(
    title = "Day in the Dry Season",
    # subtitle = "",
    y = "GPP (mmol H2O m-2 s-1)",
    x = "SWDR (?)",
    caption = "Data Source: MPI/BGI-EcoMet."
  ) +
  theme_bw() 

l1 + l2 + plot_layout(nrow = 1)
