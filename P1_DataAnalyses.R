################################################################################
## The Politics of Carework
## Exploratory Analyses ESS
################################################################################

####
## 1) LOAD RELEVANT PACKAGES
####

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(scico)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ltm)
library(patchwork)
library(ordinal)
library(lme4)
library(broom.mixed)
library(marginaleffects)
library(factoextra)
library(reshape2)
library(ggeffects)
library(texreg)


##############################################################################################

####
## 2) IMPORT DATA
####

# Change working directory to data location
setwd("C:/Users/gareval1/OneDrive - UvA/Desktop/UvA/Politics of Carework/Data/European Social Survey/All Waves")

# Import dataset
load("DataP1_base.RData")

##############################################################################################
##############################################################################################
##############################################################################################

####
## 3) FIGURES WITH POLITICAL PARTICIPATION METRICS
####

### 3.1) Participation of care workers and entire workforce by country

plotdata <- function(data, variable, level) {
  
  # Allow variable to be passed unquoted or quoted
  var <- rlang::ensym(variable)
  
  # Initialize output data frame with countries
  out <- data.frame(cntry = levels(data$cntry))
  
  # Helper function to compute proportions for a given subset
  prop_fun <- function(subset_data) {
    # Create table of variable x country
    tb <- table(subset_data[[rlang::as_string(var)]], subset_data$cntry)
    # Turn into proportions over columns
    prop <- prop.table(tb, 2)
    
    # Return vector of proportions for the chosen level
    # If level is missing in some columns, return NA
    out_vec <- rep(NA, ncol(prop))
    if (level %in% rownames(prop)) {
      out_vec <- prop[level, ]
    }
    return(out_vec)
  }
  
  # 1 — care workers (among those in paid work & careworker == Yes)
  sub_care <- subset(data, pdwrk == "Marked" & pcareworker == "Yes")
  out$careworkers <- prop_fun(sub_care)
  
  # 2 — all in paid work
  sub_paid <- subset(data, pdwrk == "Marked")
  out$paidwork <- prop_fun(sub_paid)
  
  # 3 — childcare
  #sub_child <- subset(data, pdwrk == "Marked" & childcare=="Yes")
  #out$child <- prop_fun(sub_child)
  
  # 4 — home-based LTC
  #sub_home <- subset(data, pdwrk == "Marked" & homebcare=="Yes")
  #out$home <- prop_fun(sub_home)
  
  # 5 — facility-based LTC
  #sub_inst <- subset(data, pdwrk == "Marked" & instcare=="Yes")
  #out$inst <- prop_fun(sub_inst)
  
  out <- out[order(out$careworkers, decreasing = TRUE), ]
  out$cntry <- factor(out$cntry, levels = out$cntry)
  
  out_long <- out %>%
    pivot_longer(
      cols = c(paidwork#,child,home,inst
      ),
      names_to = "group",
      values_to = "value"
    )
  
  out_long$group<-factor(out_long$group,levels=c("paidwork"#,"child","home","inst"
  ))
  
  return(list(out,out_long))
}

###

country_vote<-plotdata(CPSample,"vote_bin", "Yes")

country_vote[[1]]$cntry<-recode_factor(country_vote[[1]]$cntry,
                                       "Denmark"="DK","Ukraine"="UA","Belgium"="BE","Sweden"="SE","Cyprus"="CY",
                                       "Bulgaria"="BG","Norway"="NO","Finland"="FI","Hungary"="HU","Netherlands"="NL",
                                       "Croatia"="HR","Austria"="AT","Iceland"="IS","Greece"="GR","Latvia"="LV",
                                       "Ireland"="IE","Germany"="DE","Czechia"="CZ","Spain"="ES","Portugal"="PT",
                                       "Poland"="PL","Slovakia"="SK","Lithuania"="LT","United Kingdom"="GB",
                                       "France"="FR","Estonia"="EE","Italy"="IT","Slovenia"="SI","Switzerland"="CH")
country_vote[[2]]$cntry<-recode_factor(country_vote[[2]]$cntry,
                                       "Denmark"="DK","Ukraine"="UA","Belgium"="BE","Sweden"="SE","Cyprus"="CY",
                                       "Bulgaria"="BG","Norway"="NO","Finland"="FI","Hungary"="HU","Netherlands"="NL",
                                       "Croatia"="HR","Austria"="AT","Iceland"="IS","Greece"="GR","Latvia"="LV",
                                       "Ireland"="IE","Germany"="DE","Czechia"="CZ","Spain"="ES","Portugal"="PT",
                                       "Poland"="PL","Slovakia"="SK","Lithuania"="LT","United Kingdom"="GB",
                                       "France"="FR","Estonia"="EE","Italy"="IT","Slovenia"="SI","Switzerland"="CH")

p_vote<-ggplot(country_vote[[2]], aes(x = cntry)) +
  # Use the wide dataset for bars — 1 row per country, so no stacking
  geom_col(
    data = country_vote[[1]],
    aes(y = careworkers),
    fill = viridis(1)
  ) +
  # Use long dataset for points
  geom_point(
    aes(y = value, color = group),
    size = 2
  ) +
  geom_line(
    aes(y = value, color = group, group = group),
    linewidth = 0.8) +
  geom_text(
    data = country_vote[[1]], # Again the wide dataset
    aes(y = careworkers, 
        label = scales::percent(careworkers, accuracy = 1)),
    vjust = -0.3, size = 2
  ) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,1)) +
  scale_color_manual(
    values = c(
      viridis(6)[6]#,            # yellow
      #viridis::viridis(6)[5],   # green
      #viridis::turbo(6)[5],     # red
      #viridis::viridis(6)[3]#,   # blue
    ),
    labels = c(
      paidwork   = "Workforce average" #,
      #child = "Child care",
      #home = "Home-based care",
      #inst = "Facility-based care"
    ),
    name   = "Group"
  ) +
  labs(x = "",y="",title = "Voting")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12))

###

country_mbtru<-plotdata(CPSample,"mbtru_bin","Yes")

country_mbtru[[1]]$cntry<-recode_factor(country_mbtru[[1]]$cntry,
                                        "Iceland"="IS","Denmark"="DK","Finland"="FI","Sweden"="SE","Norway"="NO",
                                        "Belgium"="BE","Ukraine"="UA","Cyprus"="CY","Slovenia"="SI","Austria"="AT",
                                        "United Kingdom"="GB","Ireland"="IE","Netherlands"="NL","Spain"="ES","Latvia"="LV",
                                        "Greece"="GR","Croatia"="HR","Czechia"="CZ","Estonia"="EE","Portugal"="PT",
                                        "Poland"="PL","Hungary"="HU","Italy"="IT","Lithuania"="LT","Switzerland"="CH",
                                        "Bulgaria"="BG","Germany"="DE","France"="FR","Slovakia"="SK")
country_mbtru[[2]]$cntry<-recode_factor(country_mbtru[[2]]$cntry,
                                        "Iceland"="IS","Denmark"="DK","Finland"="FI","Sweden"="SE","Norway"="NO",
                                        "Belgium"="BE","Ukraine"="UA","Cyprus"="CY","Slovenia"="SI","Austria"="AT",
                                        "United Kingdom"="GB","Ireland"="IE","Netherlands"="NL","Spain"="ES","Latvia"="LV",
                                        "Greece"="GR","Croatia"="HR","Czechia"="CZ","Estonia"="EE","Portugal"="PT",
                                        "Poland"="PL","Hungary"="HU","Italy"="IT","Lithuania"="LT","Switzerland"="CH",
                                        "Bulgaria"="BG","Germany"="DE","France"="FR","Slovakia"="SK")

p_union<-ggplot(country_mbtru[[2]], aes(x = cntry)) +
  # Use the wide dataset for bars — 1 row per country, so no stacking
  geom_col(
    data = country_mbtru[[1]],
    aes(y = careworkers),
    fill = viridis(1)
  ) +
  # Use long dataset for points
  geom_point(
    aes(y = value, color = group),
    size = 2
  ) +
  geom_line(
    aes(y = value, color = group, group = group),
    linewidth = 0.8) +
  geom_text(
    data = country_mbtru[[1]], # Again the wide dataset
    aes(y = careworkers, 
        label = scales::percent(careworkers, accuracy = 1)),
    vjust = -0.3, size = 2
  ) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,1)) +
  scale_color_manual(
    values = c(
      viridis(6)[6]#,            # yellow
      #viridis::viridis(6)[5],   # green
      #viridis::turbo(6)[5],     # red
      #viridis::viridis(6)[3]#,   # blue
    ),
    labels = c(
      paidwork   = "Workforce average" #,
      #child = "Child care",
      #home = "Home-based care",
      #inst = "Facility-based care"
    ),
    name   = "Group"
  ) +
  labs(x = "",y="",title = "Union membership")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12))

###

country_partinst<-plotdata(CPSample,"part_inst","Yes")

country_partinst[[1]]$cntry<-recode_factor(country_partinst[[1]]$cntry,
                                           "Iceland"="IS","Ireland"="IE","Finland"="FI","Norway"="NO","Denmark"="DK",
                                           "Austria"="AT","Belgium"="BE","Cyprus"="CY","Sweden"="SE","Germany"="DE",
                                           "Estonia"="EE","Spain"="ES","United Kingdom"="GB","Ukraine"="UA",
                                           "Switzerland"="CH","Croatia"="HR","Netherlands"="NL","Lithuania"="LT",
                                           "France"="FR","Hungary"="HU","Portugal"="PT","Slovakia"="SK","Poland"="PL",
                                           "Latvia"="LV","Czechia"="CZ","Greece"="GR","Slovenia"="SI","Bulgaria"="BG",
                                           "Italy"="IT")
country_partinst[[2]]$cntry<-recode_factor(country_partinst[[2]]$cntry,
                                           "Iceland"="IS","Ireland"="IE","Finland"="FI","Norway"="NO","Denmark"="DK",
                                           "Austria"="AT","Belgium"="BE","Cyprus"="CY","Sweden"="SE","Germany"="DE",
                                           "Estonia"="EE","Spain"="ES","United Kingdom"="GB","Ukraine"="UA",
                                           "Switzerland"="CH","Croatia"="HR","Netherlands"="NL","Lithuania"="LT",
                                           "France"="FR","Hungary"="HU","Portugal"="PT","Slovakia"="SK","Poland"="PL",
                                           "Latvia"="LV","Czechia"="CZ","Greece"="GR","Slovenia"="SI","Bulgaria"="BG",
                                           "Italy"="IT")


p_partinst<-ggplot(country_partinst[[2]], aes(x = cntry)) +
  # Use the wide dataset for bars — 1 row per country, so no stacking
  geom_col(
    data = country_partinst[[1]],
    aes(y = careworkers),
    fill = viridis(1)
  ) +
  # Use long dataset for points
  geom_point(
    aes(y = value, color = group),
    size = 2
  ) +
  geom_line(
    aes(y = value, color = group, group = group),
    linewidth = 0.8) +
  geom_text(
    data = country_partinst[[1]], # Again the wide dataset
    aes(y = careworkers, 
        label = scales::percent(careworkers, accuracy = 1)),
    vjust = -0.3, size = 2
  ) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,1)) +
  scale_color_manual(
    values = c(
      viridis(6)[6]#,            # yellow
      #viridis::viridis(6)[5],   # green
      #viridis::turbo(6)[5],     # red
      #viridis::viridis(6)[3]#,   # blue
    ),
    labels = c(
      paidwork   = "Workforce average" #,
      #child = "Child care",
      #home = "Home-based care",
      #inst = "Facility-based care"
    ),
    name   = "Group"
  ) +
  labs(x = "",y="",title = "Institutional participation")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12))

###

country_partntinst<-plotdata(CPSample,"part_ntinst","Yes")

country_partntinst[[1]]$cntry<-recode_factor(country_partntinst[[1]]$cntry,
                                             "Sweden"="SE","Iceland"="IS","Denmark"="DK","Germany"="DE","Norway"="NO",
                                             "Switzerland"="CH","United Kingdom"="GB","Finland"="FI","France"="FR",
                                             "Spain"="ES","Belgium"="BE","Austria"="AT","Croatia"="HR","Netherlands"="NL",
                                             "Ireland"="IE","Czechia"="CZ","Slovakia"="SK","Latvia"="LV","Estonia"="EE",
                                             "Cyprus"="CY","Italy"="IT","Poland"="PL","Portugal"="PT","Lithuania"="LT",
                                             "Bulgaria"="BG","Greece"="GR","Slovenia"="SI","Ukraine"="UA","Hungary"="HU")
country_partntinst[[2]]$cntry<-recode_factor(country_partntinst[[2]]$cntry,
                                             "Sweden"="SE","Iceland"="IS","Denmark"="DK","Germany"="DE","Norway"="NO",
                                             "Switzerland"="CH","United Kingdom"="GB","Finland"="FI","France"="FR",
                                             "Spain"="ES","Belgium"="BE","Austria"="AT","Croatia"="HR","Netherlands"="NL",
                                             "Ireland"="IE","Czechia"="CZ","Slovakia"="SK","Latvia"="LV","Estonia"="EE",
                                             "Cyprus"="CY","Italy"="IT","Poland"="PL","Portugal"="PT","Lithuania"="LT",
                                             "Bulgaria"="BG","Greece"="GR","Slovenia"="SI","Ukraine"="UA","Hungary"="HU")


p_partntinst<-ggplot(country_partntinst[[2]], aes(x = cntry)) +
  # Use the wide dataset for bars — 1 row per country, so no stacking
  geom_col(
    data = country_partntinst[[1]],
    aes(y = careworkers),
    fill = viridis(1)
  ) +
  # Use long dataset for points
  geom_point(
    aes(y = value, color = group),
    size = 2
  ) +
  geom_line(
    aes(y = value, color = group, group = group),
    linewidth = 0.8) +
  geom_text(
    data = country_partntinst[[1]], # Again the wide dataset
    aes(y = careworkers, 
        label = scales::percent(careworkers, accuracy = 1)),
    vjust = -0.3, size = 2
  ) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,1)) +
  scale_color_manual(
    values = c(
      viridis(6)[6]#,            # yellow
      #viridis::viridis(6)[5],   # green
      #viridis::turbo(6)[5],     # red
      #viridis::viridis(6)[3]#,   # blue
    ),
    labels = c(
      paidwork   = "Workforce average" #,
      #child = "Child care",
      #home = "Home-based care",
      #inst = "Facility-based care"
    ),
    name   = "Group"
  ) +
  labs(x = "",y="",title = "Non-institutional participation")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12))

(p_vote | p_union) /
  (p_partinst | p_partntinst) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

### 3.2) Maps showing participation rates among care workers for each type of participation

europe_map <- ne_countries(
  scale = "medium",
  continent = "Europe",
  returnclass = "sf"
)

###
## Voting
###

country_vote<-plotdata(CPSample,"vote_bin","Yes")

votemap_data <- country_vote[[1]] %>%
  mutate(cntry = as.character(cntry))

europe_map_joined <- europe_map %>%
  left_join(votemap_data, by = c("name" = "cntry"))

mapplot_vote<-ggplot(europe_map_joined) +
  geom_sf(aes(fill = careworkers), color = "white", linewidth = 0.2) +
  #scale_fill_gradient(
  #  low = viridis::viridis(6)[1],
  #  high = viridis::viridis(6)[6],
  #  na.value = "grey90",
  #  name = "Share"
  #) +
  scale_fill_viridis_c(
    limits = c(0, 1),
    #trans = "sqrt",
    option = "D",
    begin = 0,
    end = 1) +
  coord_sf(
    xlim = c(-30, 45),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  labs(
    fill="Share",title = "Voting"
  )

###
## Union membership
###

country_mbtru<-plotdata(CPSample,"mbtru_bin","Yes")

unionmap_data <- country_mbtru[[1]] %>%
  mutate(cntry = as.character(cntry))

europe_map_joined <- europe_map %>%
  left_join(unionmap_data, by = c("name" = "cntry"))

mapplot_mbtru<-ggplot(europe_map_joined) +
  geom_sf(aes(fill = careworkers), color = "white", linewidth = 0.2) +
  #scale_fill_gradient(
  #  low = viridis::viridis(6)[1],
  #  high = viridis::viridis(6)[6],
  #  na.value = "grey90",
  #  name = "Share"
  #) +
  scale_fill_viridis_c(
    limits = c(0, 1),
    #trans = "sqrt",
    option = "D",
    begin = 0,
    end = 1) +
  coord_sf(
    xlim = c(-30, 45),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  labs(
    fill="Share",title = "Union membership"
  )

###
## Institutional participation
###

country_partinst<-plotdata(CPSample,"part_inst","Yes")

partinstmap_data <- country_partinst[[1]] %>%
  mutate(cntry = as.character(cntry))

europe_map_joined <- europe_map %>%
  left_join(partinstmap_data, by = c("name" = "cntry"))

mapplot_partinst<-ggplot(europe_map_joined) +
  geom_sf(aes(fill = careworkers), color = "white", linewidth = 0.2) +
  #scale_fill_gradient(
  #  low = viridis::viridis(6)[1],
  #  high = viridis::viridis(6)[6],
  #  na.value = "grey90",
  #  name = "Share"
  #) +
  scale_fill_viridis_c(
    limits = c(0, 1),
    #trans = "sqrt",
    option = "D",
    begin = 0,
    end = 1
  ) +
  coord_sf(
    xlim = c(-30, 45),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  labs(
    fill="Share",title = "Institutional participation"
  )

###
## Non-Institutional participation
###

country_partntinst<-plotdata(CPSample,"part_ntinst","Yes")

partntinstmap_data <- country_partntinst[[1]] %>%
  mutate(cntry = as.character(cntry))

europe_map_joined <- europe_map %>%
  left_join(partntinstmap_data, by = c("name" = "cntry"))

mapplot_partntinst<-ggplot(europe_map_joined) +
  geom_sf(aes(fill = careworkers), color = "white", linewidth = 0.2) +
  #scale_fill_gradient(
  #  low = viridis::viridis(6)[1],
  #  high = viridis::viridis(6)[6],
  #  na.value = "grey90",
  #  name = "Share"
  #) +
  scale_fill_viridis_c(
    limits = c(0, 1),
    #trans = "sqrt",
    option = "D",
    begin = 0,
    end = 1
  ) +
  coord_sf(
    xlim = c(-30, 45),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  labs(fill="Share",title = "Non-institutional participation")

mapplot_abs_sqr<-(mapplot_vote | mapplot_mbtru) /
  (mapplot_partinst | mapplot_partntinst) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
  )

### 3.3) Maps comparing the likelihood of participating (care workers vs average worker) 
# for each type of participation

###
## Voting
###

country_vote<-plotdata(CPSample,"vote_bin","Yes")

votemap_data <- country_vote[[1]] %>%
  mutate(cntry = as.character(cntry))

votemap_data$diff<-(votemap_data$careworkers-votemap_data$paidwork)/votemap_data$paidwork

europe_map_joined <- europe_map %>%
  left_join(votemap_data, by = c("name" = "cntry"))

mapplot_vote<-ggplot(europe_map_joined) +
  geom_sf(aes(fill = diff), color = "white", linewidth = 0.2) +
  #scale_fill_viridis_c(option="inferno",name = NULL,
  #                     limits = c(-0.7, 0.7)) +
  scale_fill_scico(
    palette = "vik",   # diverging palette
    direction = 1,     # default direction
    limits = c(-0.7, 0.7) # fix legend and colors
  ) +
  coord_sf(
    xlim = c(-30, 45),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  labs(
    fill="Proportional difference",title = "Voting"
  )

###
## Union membership
###

country_mbtru<-plotdata(CPSample,"mbtru_bin","Yes")

unionmap_data <- country_mbtru[[1]] %>%
  mutate(cntry = as.character(cntry))

unionmap_data$diff<-(unionmap_data$careworkers-unionmap_data$paidwork)/unionmap_data$paidwork

europe_map_joined <- europe_map %>%
  left_join(unionmap_data, by = c("name" = "cntry"))

mapplot_mbtru<-ggplot(europe_map_joined) +
  geom_sf(aes(fill = diff), color = "white", linewidth = 0.2) +
  #scale_fill_viridis_c(option="inferno",name = NULL,
  #                     limits = c(-0.7, 0.7)) +
  scale_fill_scico(
    palette = "vik",   # diverging palette
    direction = 1,     # default direction
    limits = c(-0.7, 0.7) # fix legend and colors
  ) +
  coord_sf(
    xlim = c(-30, 45),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  labs(
    fill="Proportional difference",title = "Union membership"
  )

###
## Institutional participation
###

country_partinst<-plotdata(CPSample,"part_inst","Yes")

partinstmap_data <- country_partinst[[1]] %>%
  mutate(cntry = as.character(cntry))

partinstmap_data$diff<-(partinstmap_data$careworkers-partinstmap_data$paidwork)/partinstmap_data$paidwork

europe_map_joined <- europe_map %>%
  left_join(partinstmap_data, by = c("name" = "cntry"))

mapplot_partinst<-ggplot(europe_map_joined) +
  geom_sf(aes(fill = diff), color = "white", linewidth = 0.2) +
  #scale_fill_viridis_c(option="inferno",name = NULL,
  #limits = c(-0.7, 0.7)) +
  scale_fill_scico(
    palette = "vik",   # diverging palette
    direction = 1,     # default direction
    limits = c(-0.7, 0.7) # fix legend and colors
  ) +
  coord_sf(
    xlim = c(-30, 45),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  labs(
    fill="Proportional difference",title = "Institutional participation"
  )

###
## Non-Institutional participation
###

country_partntinst<-plotdata(CPSample,"part_ntinst","Yes")

partntinstmap_data <- country_partntinst[[1]] %>%
  mutate(cntry = as.character(cntry))

partntinstmap_data$diff<-(partntinstmap_data$careworkers-partntinstmap_data$paidwork)/partntinstmap_data$paidwork

europe_map_joined <- europe_map %>%
  left_join(partntinstmap_data, by = c("name" = "cntry"))

mapplot_partntinst<-ggplot(europe_map_joined) +
  geom_sf(aes(fill = diff), color = "white", linewidth = 0.2) +
  #scale_fill_viridis_c(option="inferno",name = NULL,
  #limits = c(-0.7, 0.7)) +
  scale_fill_scico(
    palette = "vik",   # diverging palette
    direction = 1,     # default direction
    limits = c(-0.7, 0.7) # fix legend and colors
  ) +
  coord_sf(
    xlim = c(-30, 45),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  labs(fill="Proportional difference",title = "Non-institutional participation")

mapplot_dif<-(mapplot_vote | mapplot_mbtru) /
  (mapplot_partinst | mapplot_partntinst) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
  )

##############################################################################################
##############################################################################################
##############################################################################################

####
## 4) MODELS WITH INDIVIDUAL LEVEL PREDICTORS
####

# Scaling numeric variables

CPSample$hinctnta_sca<-scale(CPSample$hinctnta_num)
CPSample$agea_sca<-scale(CPSample$agea)
CPSample$nkids_sca<-scale(CPSample$nkids)
# We also recode the migration variable
CPSample$migration<-recode_factor(CPSample$brncntr,"Yes"="Born in country","No"="Born abroad")

## Note: because employment sector is missing in Waves 1 to 11, the models are fitted
# using Waves 4-11.This leaves us with 5694 care workers and a minimum of 21 cws per country.

##
# 4.1. Voting
##

mvote_0<-glmer(vote_bin~gndr+migration+wrkctra+part_time+education+sector+
                   hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                 data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                 control = glmerControl(optimizer = "bobyqa"))
summary(mvote_0)

mvote_1<-glmer(vote_bin~pcareworker+wave_fe+(1|cntry),
               data=CPSample[which(CPSample$wave > 3),],weights=anweight,family=binomial(link = "logit"),                
               control = glmerControl(optimizer = "bobyqa"))
summary(mvote_1) 

mvote_2<-glmer(vote_bin~pcareworker+gndr+migration+wrkctra+part_time+education+sector+
                   hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                 data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                 control = glmerControl(optimizer = "bobyqa"))
summary(mvote_2) 

mvote_3<-glmer(vote_bin~gndr+migration+wrkctra+part_time+education+sector+
                   hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                 data=CPSample[which(CPSample$pcareworker=="Yes"),],weights=anweight,family=binomial(link = "logit"),                
                 control = glmerControl(optimizer = "bobyqa"))
summary(mvote_3)

mvote_4<-glmer(vote_bin~pcareworker*gndr+pcareworker*migration+pcareworker*wrkctra+pcareworker*part_time+
                   pcareworker*education+pcareworker*sector+hinctnta_sca+agea_sca+lwp+nkids_sca+
                   pcareworker*Care_Exp+wave_fe+
                   (1+pcareworker|cntry),
                 data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                 control = glmerControl(optimizer = "bobyqa"))
summary(mvote_4)

##
# 4.2. Unionising
##

munion_0<-glmer(mbtru_bin~gndr+migration+wrkctra+part_time+education+sector+
                    hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                  data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                  control = glmerControl(optimizer = "bobyqa"))
summary(munion_0)

munion_1<-glmer(mbtru_bin~pcareworker+wave_fe+(1|cntry),
                data=CPSample[which(CPSample$wave > 3),],weights=anweight,family=binomial(link = "logit"),                
                control = glmerControl(optimizer = "bobyqa"))
summary(munion_1) # 

munion_2<-glmer(mbtru_bin~pcareworker+gndr+migration+wrkctra+part_time+education+sector+
                    hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                  data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                  control = glmerControl(optimizer = "bobyqa"))
summary(munion_2) #

munion_3<-glmer(mbtru_bin~gndr+migration+wrkctra+part_time+education+sector+
                    hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                  data=CPSample[which(CPSample$pcareworker=="Yes"),],weights=anweight,family=binomial(link = "logit"),                
                  control = glmerControl(optimizer = "bobyqa"))
summary(munion_3)

munion_4<-glmer(mbtru_bin~pcareworker*gndr+pcareworker*migration+pcareworker*wrkctra+pcareworker*part_time+
                    pcareworker*education+pcareworker*sector+hinctnta_sca+agea_sca+lwp+nkids_sca+
                    pcareworker*Care_Exp+wave_fe+
                    (1+pcareworker|cntry),
                  data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                  control = glmerControl(optimizer = "bobyqa"))
summary(munion_4) # adding a random slope for pcareworker

##
# 4.3. Institutional participation
##

minst_0<-glmer(part_inst~gndr+migration+wrkctra+part_time+education+sector+
                   hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                 data=CPSample,weights=anweight,family=binomial(link = "logit"),
                 control = glmerControl(optimizer = "bobyqa"))
summary(minst_0) # 

minst_1<-glmer(part_inst~pcareworker+wave_fe+(1|cntry),
               data=CPSample[which(CPSample$wave > 3),],weights=anweight,family=binomial(link = "logit"),                
               control = glmerControl(optimizer = "bobyqa"))
summary(minst_1) # 

minst_2<-glmer(part_inst~pcareworker+gndr+migration+wrkctra+part_time+education+sector+
                   hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                 data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                 control = glmerControl(optimizer = "bobyqa"))
summary(minst_2) # 

minst_3<-glmer(part_inst~gndr+migration+wrkctra+part_time+education+sector+
                   hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                 data=CPSample[which(CPSample$pcareworker=="Yes"),],weights=anweight,family=binomial(link = "logit"),                
                 control = glmerControl(optimizer = "bobyqa"))
summary(minst_3)

minst_4<-glmer(part_inst~pcareworker*gndr+pcareworker*migration+pcareworker*wrkctra+pcareworker*part_time+
                   pcareworker*education+pcareworker*sector+hinctnta_sca+agea_sca+lwp+nkids_sca+
                   pcareworker*Care_Exp+wave_fe+
                   (1+pcareworker|cntry),
                 data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                 control = glmerControl(optimizer = "bobyqa"))
summary(minst_4) # adding a random slope for pcareworker

##
# 4.4. Non-institutional participation
##

mntinst_0<-glmer(part_ntinst~gndr+migration+wrkctra+part_time+education+sector+
                     hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                   data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                   control = glmerControl(optimizer = "bobyqa"))
summary(mntinst_0) # 

mntinst_1<-glmer(part_ntinst~pcareworker+wave_fe+(1|cntry),
                 data=CPSample[which(CPSample$wave > 3),],weights=anweight,family=binomial(link = "logit"),                
                 control = glmerControl(optimizer = "bobyqa"))
summary(mntinst_1) # 

mntinst_2<-glmer(part_ntinst~pcareworker+gndr+migration+wrkctra+part_time+education+sector+
                     hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                   data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                   control = glmerControl(optimizer = "bobyqa"))
summary(mntinst_2) # 

mntinst_3<-glmer(part_ntinst~gndr+migration+wrkctra+part_time+education+sector+
                     hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+(1|cntry),
                   data=CPSample[which(CPSample$pcareworker=="Yes"),],weights=anweight,family=binomial(link = "logit"),                
                   control = glmerControl(optimizer = "bobyqa"))
summary(mntinst_3)

mntinst_4<-glmer(part_ntinst~pcareworker*gndr+pcareworker*migration+pcareworker*wrkctra+pcareworker*part_time+
                     pcareworker*education+pcareworker*sector+hinctnta_sca+agea_sca+lwp+nkids_sca+
                     pcareworker*Care_Exp+wave_fe+
                     (1+pcareworker|cntry),
                   data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                   control = glmerControl(optimizer = "bobyqa"))
summary(mntinst_4) # adding a random slope for pcareworker

###

save(list=ls(),file="ResultsP1.RData")

save(mvote_0,mvote_1,mvote_2,mvote_3,mvote_4,
     munion_0,munion_1,munion_2,munion_3,munion_4,
     minst_0,minst_1,minst_2,minst_3,minst_4,
     mntinst_0,mntinst_1,mntinst_2,mntinst_3,mntinst_4,
     file="ModelsP1.RData")

##############################################################################################

########
### Plot the models coefficients:
########

### Voting:

# Compute marginal effects
ame_mvote_0 <- avg_slopes(
  mvote_0,
  variables = c(
    "gndr", "migration", "wrkctra", "part_time", "education", "sector",
    "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 0")

ame_mvote_1 <- avg_slopes(
  mvote_1,
  variables = "pcareworker"
) %>%
  mutate(model = "Model 1")

ame_mvote_2 <- avg_slopes(
  mvote_2,
  variables = c(
    "pcareworker", "gndr", "migration", "wrkctra", "part_time", "education",
    "sector", "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 2")

ame_mvote_3 <- avg_slopes(
  mvote_3,
  variables = c(
    "gndr", "migration", "wrkctra", "part_time", "education", "sector",
    "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 3")

# Create an object with the marginal effects for the 4 models
ame_vote_df <- bind_rows(
  ame_mvote_0,
  ame_mvote_1,
  ame_mvote_2,
  ame_mvote_3
)

# Combine variable and level names for multilevel factors
ame_vote_df <- ame_vote_df %>%
  mutate(
    term_plot = case_when(
      is.na(contrast) ~ term,
      TRUE ~ paste0(term, ": ", contrast)
    )
  )

# Change variable labels for the plot
ame_vote_df$term_plot<-recode_factor(as.factor(ame_vote_df$term_plot),
                                     "pcareworker: Yes - No" = "Care worker",
                                     "gndr: Female - Male" = "Female",
                                     "migration: Born abroad - Born in country" = "Born abroad",
                                     "wrkctra: Limited - Unlimited" = "Contract: fixed-term (ref: unlimited)",
                                     "wrkctra: No contract - Unlimited" = "Contract: no contract (ref: unlimited)",
                                     "part_time: Part-time - Full-time" = "Part-time work",
                                     "sector: Private - Public" = "Sector: private (ref: public)",
                                     "sector: Self-employed - Public" = "Sector: self-employed (ref: public)",
                                     "sector: Other - Public" = "Sector: other (ref: public)",
                                     "education: Vocational - Secondary or lower" = "Vocational training (ref: secondary)",
                                     "education: Tertiary - Secondary or lower" = "Tertiary education (ref: secondary)",
                                     "agea_sca: dY/dX" = "Age (std.)",
                                     "hinctnta_sca: dY/dX" = "HH income (std.)",
                                     "lwp: Lives with husband/wife/partner - Does not" = "Lives with partner",
                                     "nkids_sca: dY/dX" = "Number of children (std.)")

# Reorder variables
ame_vote_df <- ame_vote_df %>%
  mutate(
    term_plot = factor(
      term_plot,
      levels = c(
        "Number of children (std.)","Lives with partner","HH income (std.)","Age (std.)",
        "Tertiary education (ref: secondary)","Vocational training (ref: secondary)",
        "Sector: other (ref: public)","Sector: self-employed (ref: public)","Sector: private (ref: public)",
        "Part-time work","Contract: no contract (ref: unlimited)","Contract: fixed-term (ref: unlimited)",
        "Born abroad","Female","Care worker"
      )))

ame_plot_vote<-ggplot(ame_vote_df,
                      aes(x = estimate,
                          y = term_plot,
                          color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  #scale_color_brewer(palette = "Dark2") +
  scale_color_manual(
    values = c(
      viridis(6)[6],            # yellow
      viridis::viridis(6)[5],   # green
      viridis::turbo(6)[5],     # red
      viridis::viridis(6)[3]   # blue
    ),
    labels = c(
      "Model 0"   = "Covariates only",
      "Model 1" = "Care work only",
      "Model 2" = "Care work + covariates",
      "Model 3"= "Care worker sample"
    ),
    name   = "Model"
  ) + 
  labs(
    x = "Average marginal effect on P(Vote)",
    y = NULL,
    color = "Model"
  ) +
  xlim(-0.76, 0.25) +
  theme_minimal(base_size = 12)

#######################

### Union membership:

# Compute marginal effects
ame_munion_0 <- avg_slopes(
  munion_0,
  variables = c(
    "gndr", "migration", "wrkctra", "part_time", "education", "sector",
    "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 0")

ame_munion_1 <- avg_slopes(
  munion_1,
  variables = "pcareworker"
) %>%
  mutate(model = "Model 1")

ame_munion_2 <- avg_slopes(
  munion_2,
  variables = c(
    "pcareworker", "gndr", "migration", "wrkctra", "part_time", "education",
    "sector", "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 2")

ame_munion_3 <- avg_slopes(
  munion_3,
  variables = c(
    "gndr", "migration", "wrkctra", "part_time", "education", "sector",
    "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 3")

# Create an object with the marginal effects for the 4 models
ame_union_df <- bind_rows(
  ame_munion_0,
  ame_munion_1,
  ame_munion_2,
  ame_munion_3
)

# Combine variable and level names for multilevel factors
ame_union_df <- ame_union_df %>%
  mutate(
    term_plot = case_when(
      is.na(contrast) ~ term,
      TRUE ~ paste0(term, ": ", contrast)
    )
  )

# Change variable labels for the plot
ame_union_df$term_plot<-recode_factor(as.factor(ame_union_df$term_plot),
                                      "pcareworker: Yes - No" = "Care worker",
                                      "gndr: Female - Male" = "Female",
                                      "migration: Born abroad - Born in country" = "Born abroad",
                                      "wrkctra: Limited - Unlimited" = "Contract: fixed-term (ref: unlimited)",
                                      "wrkctra: No contract - Unlimited" = "Contract: no contract (ref: unlimited)",
                                      "part_time: Part-time - Full-time" = "Part-time work",
                                      "sector: Private - Public" = "Sector: private (ref: public)",
                                      "sector: Self-employed - Public" = "Sector: self-employed (ref: public)",
                                      "sector: Other - Public" = "Sector: other (ref: public)",
                                      "education: Vocational - Secondary or lower" = "Vocational training (ref: secondary)",
                                      "education: Tertiary - Secondary or lower" = "Tertiary education (ref: secondary)",
                                      "agea_sca: dY/dX" = "Age (std.)",
                                      "hinctnta_sca: dY/dX" = "HH income (std.)",
                                      "lwp: Lives with husband/wife/partner - Does not" = "Lives with partner",
                                      "nkids_sca: dY/dX" = "Number of children (std.)")
# Reorder variables
ame_union_df <- ame_union_df %>%
  mutate(
    term_plot = factor(
      term_plot,
      levels = c(
        "Number of children (std.)","Lives with partner","HH income (std.)","Age (std.)",
        "Tertiary education (ref: secondary)","Vocational training (ref: secondary)",
        "Sector: other (ref: public)","Sector: self-employed (ref: public)","Sector: private (ref: public)",
        "Part-time work","Contract: no contract (ref: unlimited)","Contract: fixed-term (ref: unlimited)",
        "Born abroad","Female","Care worker"
      )))

ame_plot_union<-ggplot(ame_union_df,
                       aes(x = estimate,
                           y = term_plot,
                           color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  #scale_color_brewer(palette = "Dark2") +
  scale_color_manual(
    values = c(
      viridis(6)[6],            # yellow
      viridis::viridis(6)[5],   # green
      viridis::turbo(6)[5],     # red
      viridis::viridis(6)[3]   # blue
    ),
    labels = c(
      "Model 0"   = "Covariates only",
      "Model 1" = "Care work only",
      "Model 2" = "Care work + covariates",
      "Model 3"= "Care worker sample"
    ),
    name   = "Model"
  ) + 
  labs(
    x = "Average marginal effect on P(Union membership)",
    y = NULL,
    color = "Model"
  ) +
  xlim(-0.76, 0.25) +
  theme_minimal(base_size = 12)

#######################

### Institutional participation:

# Compute marginal effects
ame_minst_0 <- avg_slopes(
  minst_0,
  variables = c(
    "gndr", "migration", "wrkctra", "part_time", "education", "sector",
    "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 0")

ame_minst_1 <- avg_slopes(
  minst_1,
  variables = "pcareworker"
) %>%
  mutate(model = "Model 1")

ame_minst_2 <- avg_slopes(
  minst_2,
  variables = c(
    "pcareworker", "gndr", "migration", "wrkctra", "part_time", "education",
    "sector", "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 2")

ame_minst_3 <- avg_slopes(
  minst_3,
  variables = c(
    "gndr", "migration", "wrkctra", "part_time", "education", "sector",
    "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 3")

# Create an object with the marginal effects for the 4 models
ame_inst_df <- bind_rows(
  ame_minst_0,
  ame_minst_1,
  ame_minst_2,
  ame_minst_3
)

# Combine variable and level names for multilevel factors
ame_inst_df <- ame_inst_df %>%
  mutate(
    term_plot = case_when(
      is.na(contrast) ~ term,
      TRUE ~ paste0(term, ": ", contrast)
    )
  )

# Change variable labels for the plot
ame_inst_df$term_plot<-recode_factor(as.factor(ame_inst_df$term_plot),
                                     "pcareworker: Yes - No" = "Care worker",
                                     "gndr: Female - Male" = "Female",
                                     "migration: Born abroad - Born in country" = "Born abroad",
                                     "wrkctra: Limited - Unlimited" = "Contract: fixed-term (ref: unlimited)",
                                     "wrkctra: No contract - Unlimited" = "Contract: no contract (ref: unlimited)",
                                     "part_time: Part-time - Full-time" = "Part-time work",
                                     "sector: Private - Public" = "Sector: private (ref: public)",
                                     "sector: Self-employed - Public" = "Sector: self-employed (ref: public)",
                                     "sector: Other - Public" = "Sector: other (ref: public)",
                                     "education: Vocational - Secondary or lower" = "Vocational training (ref: secondary)",
                                     "education: Tertiary - Secondary or lower" = "Tertiary education (ref: secondary)",
                                     "agea_sca: dY/dX" = "Age (std.)",
                                     "hinctnta_sca: dY/dX" = "HH income (std.)",
                                     "lwp: Lives with husband/wife/partner - Does not" = "Lives with partner",
                                     "nkids_sca: dY/dX" = "Number of children (std.)")

# Reorder variables
ame_inst_df <- ame_inst_df %>%
  mutate(
    term_plot = factor(
      term_plot,
      levels = c(
        "Number of children (std.)","Lives with partner","HH income (std.)","Age (std.)",
        "Tertiary education (ref: secondary)","Vocational training (ref: secondary)",
        "Sector: other (ref: public)","Sector: self-employed (ref: public)","Sector: private (ref: public)",
        "Part-time work","Contract: no contract (ref: unlimited)","Contract: fixed-term (ref: unlimited)",
        "Born abroad","Female","Care worker"
      )))

ame_plot_inst<-ggplot(ame_inst_df,
                      aes(x = estimate,
                          y = term_plot,
                          color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  #scale_color_brewer(palette = "Dark2") +
  scale_color_manual(
    values = c(
      viridis(6)[6],            # yellow
      viridis::viridis(6)[5],   # green
      viridis::turbo(6)[5],     # red
      viridis::viridis(6)[3]   # blue
    ),
    labels = c(
      "Model 0"   = "Covariates only",
      "Model 1" = "Care work only",
      "Model 2" = "Care work + covariates",
      "Model 3"= "Care worker sample"
    ),
    name   = "Model"
  ) + 
  labs(
    x = "Average marginal effect on P(Institutional participation)",
    y = NULL,
    color = "Model"
  ) +
  xlim(-0.76, 0.25) +
  theme_minimal(base_size = 12)

#######################

### Non-institutional participation

# Compute marginal effects
ame_mntinst_0 <- avg_slopes(
  mntinst_0,
  variables = c(
    "gndr", "migration", "wrkctra", "part_time", "education", "sector",
    "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 0")

ame_mntinst_1 <- avg_slopes(
  mntinst_1,
  variables = "pcareworker"
) %>%
  mutate(model = "Model 1")

ame_mntinst_2 <- avg_slopes(
  mntinst_2,
  variables = c(
    "pcareworker", "gndr", "migration", "wrkctra", "part_time", "education",
    "sector", "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 2")

ame_mntinst_3 <- avg_slopes(
  mntinst_3,
  variables = c(
    "gndr", "migration", "wrkctra", "part_time", "education", "sector",
    "hinctnta_sca", "agea_sca", "lwp", "nkids_sca"
  )
) %>%
  mutate(model = "Model 3")

# Create an object with the marginal effects for the 4 models
ame_ntinst_df <- bind_rows(
  ame_mntinst_0,
  ame_mntinst_1,
  ame_mntinst_2,
  ame_mntinst_3
)

# Combine variable and level names for multilevel factors
ame_ntinst_df <- ame_ntinst_df %>%
  mutate(
    term_plot = case_when(
      is.na(contrast) ~ term,
      TRUE ~ paste0(term, ": ", contrast)
    )
  )

# Change variable labels for the plot
ame_ntinst_df$term_plot<-recode_factor(as.factor(ame_ntinst_df$term_plot),
                                       "pcareworker: Yes - No" = "Care worker",
                                       "gndr: Female - Male" = "Female",
                                       "migration: Born abroad - Born in country" = "Born abroad",
                                       "wrkctra: Limited - Unlimited" = "Contract: fixed-term (ref: unlimited)",
                                       "wrkctra: No contract - Unlimited" = "Contract: no contract (ref: unlimited)",
                                       "part_time: Part-time - Full-time" = "Part-time work",
                                       "sector: Private - Public" = "Sector: private (ref: public)",
                                       "sector: Self-employed - Public" = "Sector: self-employed (ref: public)",
                                       "sector: Other - Public" = "Sector: other (ref: public)",
                                       "education: Vocational - Secondary or lower" = "Vocational training (ref: secondary)",
                                       "education: Tertiary - Secondary or lower" = "Tertiary education (ref: secondary)",
                                       "agea_sca: dY/dX" = "Age (std.)",
                                       "hinctnta_sca: dY/dX" = "HH income (std.)",
                                       "lwp: Lives with husband/wife/partner - Does not" = "Lives with partner",
                                       "nkids_sca: dY/dX" = "Number of children (std.)")
# Reorder variables
ame_ntinst_df <- ame_ntinst_df %>%
  mutate(
    term_plot = factor(
      term_plot,
      levels = c(
        "Number of children (std.)","Lives with partner","HH income (std.)","Age (std.)",
        "Tertiary education (ref: secondary)","Vocational training (ref: secondary)",
        "Sector: other (ref: public)","Sector: self-employed (ref: public)","Sector: private (ref: public)",
        "Part-time work","Contract: no contract (ref: unlimited)","Contract: fixed-term (ref: unlimited)",
        "Born abroad","Female","Care worker"
      )))

ame_plot_ntinst<-ggplot(ame_ntinst_df,
                        aes(x = estimate,
                            y = term_plot,
                            color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  #scale_color_brewer(palette = "Dark2") +
  scale_color_manual(
    values = c(
      viridis(6)[6],            # yellow
      viridis::viridis(6)[5],   # green
      viridis::turbo(6)[5],     # red
      viridis::viridis(6)[3]   # blue
    ),
    labels = c(
      "Model 0"   = "Covariates only",
      "Model 1" = "Care work only",
      "Model 2" = "Care work + covariates",
      "Model 3"= "Care worker sample"
    ),
    name   = "Model"
  ) + 
  labs(
    x = "Average marginal effect on P(Non-institutional participation)",
    y = NULL,
    color = "Model"
  ) +
  xlim(-0.76, 0.25) +
  theme_minimal(base_size = 12)

###

# All combined

(ame_plot_vote | ame_plot_union) /
  (ame_plot_inst | ame_plot_ntinst) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ame_plot_vote /
  ame_plot_union +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ame_plot_inst /
  ame_plot_ntinst +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

###

# Plot with all types of activity DVs for the care workers sample:

## Need to update the model names!
ame_mvote_all<-ame_mvote_2;ame_mvote_all$model<-rep("Vote",length(ame_mvote_all$model))
ame_munion_all<-ame_munion_2;ame_munion_all$model<-rep("Union membership",length(ame_munion_all$model))
ame_minst_all<-ame_minst_2;ame_minst_all$model<-rep("Institutional",length(ame_minst_all$model))
ame_mntinst_all<-ame_mntinst_2;ame_mntinst_all$model<-rep("Non-institutional",length(ame_mntinst_all$model))

# Create an object with the marginal effects for the 4 models
ame_all_df <- bind_rows(
  ame_mvote_all,
  ame_munion_all,
  ame_minst_all,
  ame_mntinst_all
)

# Combine variable and level names for multilevel factors
ame_all_df <- ame_all_df %>%
  mutate(
    term_plot = case_when(
      is.na(contrast) ~ term,
      TRUE ~ paste0(term, ": ", contrast)
    )
  )

# Change variable labels for the plot
ame_all_df$term_plot<-recode_factor(as.factor(ame_all_df$term_plot),
                                    "pcareworker: Yes - No" = "Care worker",
                                    "gndr: Female - Male" = "Female",
                                    "migration: EU - National" = "EU migration (ref: national)",
                                    "migration: Third country - National" = "3rd country migration (ref: national)",
                                    "wrkctra: Limited - Unlimited" = "Contract: fixed-term (ref: unlimited)",
                                    "wrkctra: No contract - Unlimited" = "Contract: no contract (ref: unlimited)",
                                    "part_time: Part-time - Full-time" = "Part-time work",
                                    "sector: Private - Public" = "Sector: private (ref: public)",
                                    "sector: Self-employed - Public" = "Sector: self-employed (ref: public)",
                                    "sector: Other - Public" = "Sector: other (ref: public)",
                                    "education: Vocational - Secondary or lower" = "Vocational training (ref: secondary)",
                                    "education: Tertiary - Secondary or lower" = "Tertiary education (ref: secondary)",
                                    "agea_sca: dY/dX" = "Age (std.)",
                                    "hinctnta_sca: dY/dX" = "HH income (std.)",
                                    "lwp: Lives with husband/wife/partner - Does not" = "Lives with partner",
                                    "nkids_sca: dY/dX" = "Number of children (std.)",
                                    "Care_Exp" = "Public care expenditure")

# Reorder variables
ame_all_df <- ame_all_df %>%
  mutate(
    term_plot = factor(
      term_plot,
      levels = c(
        "Care worker","Female","EU migration (ref: national)","3rd country migration (ref: national)",
        "Contract: fixed-term (ref: unlimited)","Contract: no contract (ref: unlimited)","Part-time work",
        "Sector: private (ref: public)","Sector: self-employed (ref: public)","Sector: other (ref: public)",
        "Vocational training (ref: secondary)","Tertiary education (ref: secondary)","Age (std.)",
        "HH income (std.)","Lives with partner","Number of children (std.)")
    )
  )

ame_all_df <- ame_all_df %>%
  mutate(
    term_plot = factor(
      term_plot,
      levels = c(
        "Number of children (std.)","Lives with partner","HH income (std.)",
        "Age (std.)","Tertiary education (ref: secondary)","Vocational training (ref: secondary)",
        "Sector: other (ref: public)","Sector: self-employed (ref: public)","Sector: private (ref: public)",
        "Part-time work","Contract: no contract (ref: unlimited)","Contract: fixed-term (ref: unlimited)",
        "3rd country migration (ref: national)","EU migration (ref: national)","Female","Care worker"
      )))

ame_all_df$model <- factor(
  ame_all_df$model,
  levels = c(
    "Vote",
    "Union membership",
    "Institutional",
    "Non-institutional"
  )
)

ame_plot_all<-ggplot(ame_all_df,
                     aes(x = estimate,
                         y = term_plot,
                         color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  #scale_color_brewer(palette = "Dark2") +
  scale_color_manual(
    values = c(
      viridis(6)[6],            # yellow
      viridis::viridis(6)[5],   # green
      viridis::turbo(6)[5],     # red
      viridis::viridis(6)[3]   # blue
    ),
    name   = "Model"
  ) + 
  labs(
    x = "Average marginal effect on P(Participation)",
    y = NULL,
    color = "Model"
  ) +
  xlim(-0.76, 0.25) +
  theme_minimal(base_size = 12)

# A reduced version with less covariates

ame_all_df_reduced<-ame_all_df[-which(ame_all_df$term %in% c("agea_sca","hinctnta_sca",
                                                             "lwp","nkids_sca")),]

ame_plot_all_reduced<-ggplot(ame_all_df_reduced,
                     aes(x = estimate,
                         y = term_plot,
                         color = model,
                         shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2.5
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  #scale_color_brewer(palette = "Dark2") +
  scale_color_manual(
    values = c(
      viridis(6)[1],            # dark blue
      viridis::viridis(6)[6],   # yellow
      viridis::turbo(6)[5],     # red
      viridis::viridis(6)[4]   # green
    ),
    name   = "Model"
  ) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(
    x = "Average marginal effect on P(Participation)",
    y = NULL,
    color = "Model",
    shape = "Model"
  ) +
  xlim(-0.76, 0.25) +
  theme_minimal(base_size = 12)

##############################################################################################

########
### Plot the interaction effects
########

###
# Voting
pprv4.1<-plot(predict_response(mvote_4,c("gndr","pcareworker")))+
  labs(title="",x="Gender",y="P(voting)",colour="Care worker")

pprv4.2<-plot(predict_response(mvote_4,c("migration","pcareworker")))+
  labs(title="",x="Migration",y="P(voting)",colour="Care worker")

pprv4.3<-plot(predict_response(mvote_4,c("wrkctra","pcareworker")))+
  labs(title="",x="Type of contract",y="P(voting)",colour="Care worker")

pprv4.4<-plot(predict_response(mvote_4,c("part_time","pcareworker")))+
  labs(title="",x="Working hours",y="P(voting)",colour="Care worker")

pprv4.5<-plot(predict_response(mvote_4,c("sector","pcareworker")))+
  labs(title="",x="Employment sector",y="P(voting)",colour="Care worker")

pprv4.6<-plot(predict_response(mvote_4,c("education","pcareworker")))+
  labs(title="",x="Educational level",y="P(voting)",colour="Care worker")

pprv4.7<-plot(predict_response(mvote_4,c("Care_Exp","pcareworker")))+
  labs(title="",x="Public investment in care",y="P(voting)",colour="Care worker")+
  guides(colour = "none")

(pprv4.1 | pprv4.2) /
  (pprv4.3 | pprv4.4) /
  (pprv4.5 | pprv4.6) /
  (pprv4.7)+
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
  )

###
# Union membership
ppru4.1<-plot(predict_response(munion_4,c("gndr","pcareworker")))+
  labs(title="",x="Gender",y="P(unionising)",colour="Care worker")

ppru4.2<-plot(predict_response(munion_4,c("migration","pcareworker")))+
  labs(title="",x="Migration",y="P(unionising)",colour="Care worker")

ppru4.3<-plot(predict_response(munion_4,c("wrkctra","pcareworker")))+
  labs(title="",x="Type of contract",y="P(unionising)",colour="Care worker")

ppru4.4<-plot(predict_response(munion_4,c("part_time","pcareworker")))+
  labs(title="",x="Working hours",y="P(unionising)",colour="Care worker")

ppru4.5<-plot(predict_response(munion_4,c("sector","pcareworker")))+
  labs(title="",x="Employment sector",y="P(unionising)",colour="Care worker")

ppru4.6<-plot(predict_response(munion_4,c("education","pcareworker")))+
  labs(title="",x="Educational level",y="P(unionising)",colour="Care worker")

ppru4.7<-plot(predict_response(munion_4,c("Care_Exp","pcareworker")))+
  labs(title="",x="Public investment in care",y="P(unionising)",colour="Care worker")+
  guides(colour = "none")

(ppru4.1 | ppru4.2) /
  (ppru4.3 | ppru4.4) /
  (ppru4.5 | ppru4.6) /
  (ppru4.7)+
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
  )

###
# Institutional participation
ppri4.1<-plot(predict_response(minst_4,c("gndr","pcareworker")))+
  labs(title="",x="Gender",y="P(institutional)",colour="Care worker")

ppri4.2<-plot(predict_response(minst_4,c("migration","pcareworker")))+
  labs(title="",x="Migration",y="P(institutional)",colour="Care worker")

ppri4.3<-plot(predict_response(minst_4,c("wrkctra","pcareworker")))+
  labs(title="",x="Type of contract",y="P(institutional)",colour="Care worker")

ppri4.4<-plot(predict_response(minst_4,c("part_time","pcareworker")))+
  labs(title="",x="Working hours",y="P(institutional)",colour="Care worker")

ppri4.5<-plot(predict_response(minst_4,c("sector","pcareworker")))+
  labs(title="",x="Employment sector",y="P(institutional)",colour="Care worker")

ppri4.6<-plot(predict_response(minst_4,c("education","pcareworker")))+
  labs(title="",x="Educational level",y="P(institutional)",colour="Care worker")

ppri4.7<-plot(predict_response(minst_4,c("Care_Exp","pcareworker")))+
  labs(title="",x="Public investment in care",y="P(institutional)",colour="Care worker")+
  guides(colour = "none")

(ppri4.1 | ppri4.2) /
  (ppri4.3 | ppri4.4) /
  (ppri4.5 | ppri4.6) /
  (ppri4.7)+
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
  )

###
# Non-institutional participation
pprn4.1<-plot(predict_response(mntinst_4,c("gndr","pcareworker")))+
  labs(title="",x="Gender",y="P(N. non-institutional)",colour="Care worker")

pprn4.2<-plot(predict_response(mntinst_4,c("migration","pcareworker")))+
  labs(title="",x="migration",y="P(N. non-institutional)",colour="Care worker")

pprn4.3<-plot(predict_response(mntinst_4,c("wrkctra","pcareworker")))+
  labs(title="",x="Type of contract",y="P(N. non-institutional)",colour="Care worker")

pprn4.4<-plot(predict_response(mntinst_4,c("part_time","pcareworker")))+
  labs(title="",x="Working hours",y="P(N. non-institutional)",colour="Care worker")

pprn4.5<-plot(predict_response(mntinst_4,c("sector","pcareworker")))+
  labs(title="",x="Employment sector",y="P(N. non-institutional)",colour="Care worker")

pprn4.6<-plot(predict_response(mntinst_4,c("education","pcareworker")))+
  labs(title="",x="Educational level",y="P(N. non-institutional)",colour="Care worker")

pprn4.7<-plot(predict_response(mntinst_4,c("Care_Exp","pcareworker")))+
  labs(title="",x="Public investment in care",y="P(N. non-institutional)",colour="Care worker")+
  guides(colour = "none")

(pprn4.1 | pprn4.2) /
  (pprn4.3 | pprn4.4) /
  (pprn4.5 | pprn4.6) /
  (pprn4.7)+
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
  )

save(list=ls(),file="ResultsP1.RData")

##############################################################################################
##############################################################################################
##############################################################################################

####
## 5) CLUSTER ANALYSIS
####

###
# 5.1) Main clustering (the one used in the article)
###

# Countries are clustered based on migrant employment, public sector employment, and public investment in ECEC and LTC

CM3.3<-CPSample %>%
  filter(pcareworker == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    migrant   = mean(brncntr == "Yes",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    #unlimited = mean(wrkctra == "Unlimited", na.rm = TRUE), # I'm dropping it cause it's highly correlated with sector
    #tertiary  = mean(education == "Tertiary", na.rm = TRUE),
    #ECEC_Exp   = mean(ECEC_Exp,na.rm=T),
    #LTC_Exp   = mean(LTC_Exp,na.rm=T),
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    .groups = "drop"
  )

CM3.3 <- CM3.3 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM3.3<-scale(na.omit(CM3.3)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM3.3, kmeans, method = "wss") # 5

set.seed(123) 
CM3.3_cluster<-kmeans(CM3.3,centers=5,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM3.3_cluster, data = CM3.3, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07","green"#,"purple"
), 
ellipse.type = "euclid", # Concentration ellipse  
star.plot = TRUE, # Add segments from centroids to items 
repel = TRUE, # Avoid label overplotting (slow)  
ggtheme = theme_minimal()  )

# Calculate centroids' positions
1-(CM3.3_cluster$centers[,1] * (0.1082085) + (0.8778724)) # Share of migrants
CM3.3_cluster$centers[,2] * (0.1885388) + (0.6317164) # Share of public sector employment
CM3.3_cluster$centers[,3] * (1.1398620) + (1.9880419) # Expenditure as % of GDP

# Reorder clusters and give them names
CM3.3_cluster_final<-CM3.3_cluster

CM3.3_cluster_final$cluster<-recode_factor(as.factor(CM3.3_cluster_final$cluster),
                                           "1"="Public care",
                                           "3"="State-funded mix",
                                           "2"="Migrant reliant",
                                           "5"="Underfunded public sector",
                                           "4"="Absent state")

fviz_cluster(CM3.3_cluster_final, data = CM3.3, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07","green"#,"purple"
), 
ellipse.type = "euclid", # Concentration ellipse  
star.plot = TRUE, # Add segments from centroids to items 
repel = TRUE, # Avoid label overplotting (slow)  
ggtheme = theme_minimal()  )

## Plot the clusters on a map

europe_map <- ne_countries(
  scale = "medium",
  continent = "Europe",
  returnclass = "sf"
)

#country_cluster<-plotdata(CPSample,"vote_bin","Yes") # Instead of this I need to just be the clusters

country_cluster<-cbind(levels(CPSample$cntry),
                              c("State-funded mix","State-funded mix","Underfunded public sector",
                                "State-funded mix","Absent state","Underfunded public sector",
                                "State-funded mix","Public care","Underfunded public sector",
                                "Migrant reliant","Public care","State-funded mix",
                                "State-funded mix","Absent state","Absent state",
                                "Underfunded public sector","State-funded mix","Public care",
                                "Migrant reliant","Underfunded public sector","Underfunded public sector",
                                "Public care","Public care","Underfunded public sector",
                                "Absent state","Public care","State-funded mix","Absent state",NA))
colnames(country_cluster)=c("cntry","regime");country_cluster<-as.data.frame(country_cluster[-29,])
country_cluster$regime<-factor(country_cluster$regime,levels=c("Public care","State-funded mix",
                                                                  "Migrant reliant","Underfunded public sector",
                                                                  "Absent state"))

europe_map_joined <- europe_map %>%
  left_join(country_cluster, by = c("name" = "cntry"))

mapplot_regimes<-ggplot(europe_map_joined) +
  # NA countries in grey
  geom_sf(
    data = subset(europe_map_joined, is.na(regime)),
    fill = "grey80",
    linewidth = 0.2, color="white"
  ) +
  # Regime countries with the desired colours
  geom_sf(
    data = subset(europe_map_joined, !is.na(regime)),
    aes(fill = regime),
    linewidth = 0.2,color="white"
  ) +
  scale_fill_manual(
    values = c(
      "Public care" = "purple",
      "State-funded mix" = "#00AFBB",
      "Migrant reliant" = "#E7B800",
      "Underfunded public sector" = "#FC4E07",
      "Absent state" = "green"
    )
  ) +
  coord_sf(
    xlim = c(-30, 45),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  labs(
    fill = "Regime",
    title = ""
  )

###
# 5.2) Robustness: alternative cluster especifications
###

# Matrix 1: Aggregated variables: share of non-citizens in care work
# share of care workers in the public sector
# share of care workers on unlimited contracts
# share of care workers with tertiary education

CM1.1 <- CPSample %>%
  filter(pcareworker == "Yes" & wave>3) %>%
  mutate(cntrywave = paste(cntry, wave, sep = "_")) %>%
  group_by(cntrywave) %>%
  summarise(
    citizen   = mean(citizen == "National",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    unlimited = mean(wrkctra == "Unlimited", na.rm = TRUE),
    tertiary  = mean(education == "Tertiary", na.rm = TRUE),
    .groups = "drop"
  )

CM1.1 <- CM1.1 %>%
  tibble::column_to_rownames("cntrywave") # Turn country-waves into identifiers

CM1.1<-scale(na.omit(CM1.1)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM1.1, kmeans, method = "wss") # Its not super clear, lets go with 5

set.seed(123) 
CM1.1_cluster<-kmeans(CM1.1,centers=5,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM1.1_cluster, data = CM1.1, palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","green"), 
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )
###
## With aggregated country data instead of country waves

CM1.2 <- CPSample %>%
  filter(pcareworker == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    citizen   = mean(citizen == "National",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    unlimited = mean(wrkctra == "Unlimited", na.rm = TRUE),
    tertiary  = mean(education == "Tertiary", na.rm = TRUE),
    .groups = "drop"
  )

CM1.2 <- CM1.2 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM1.2<-scale(na.omit(CM1.2)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM1.2, kmeans, method = "wss") # Its not super clear, lets go with 4

set.seed(123) 
CM1.2_cluster<-kmeans(CM1.2,centers=5,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM1.2_cluster, data = CM1.2, palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","green"), 
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )

##
# Using only migration and employment sector

CM1.3 <- CPSample %>%
  filter(pcareworker == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    citizen   = mean(citizen == "National",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    .groups = "drop"
  )

CM1.3 <- CM1.3 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM1.3<-scale(na.omit(CM1.3)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM1.3, kmeans, method = "wss") # 3 clusters

set.seed(123) 
CM1.3_cluster<-kmeans(CM1.3,centers=3,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM1.3_cluster, data = CM1.3, palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )

#########################

# Matrix 2: Macro variables: investment in child-care, investment in LTC (Eurostat), Gender Equality in Politics (EIGE)

# Public investment in ECEC: https://ec.europa.eu/eurostat/databrowser/view/educ_uoe_fine06__custom_20053571/default/table
# Public investment in LTC: https://ec.europa.eu/eurostat/databrowser/view/tps00214__custom_20053617/default/table

CM2.2 <- CPSample %>%
  filter(pcareworker == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    ECEC_Exp   = mean(ECEC_Exp,na.rm=T),
    LTC_Exp   = mean(LTC_Exp,na.rm=T),
    .groups = "drop"
  )

CM2.2<-CM2.2[-29,]

CM2.2 <- CM2.2 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM2.2<-scale(na.omit(CM2.2)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM2.2, kmeans, method = "wss") # 4 clusters

set.seed(123) 
CM2.2_cluster<-kmeans(CM2.2,centers=4,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM2.2_cluster, data = CM2.2, palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )

### Also adding gender equality

GEI<-read.csv2("GEI_2017-2022.csv",sep=",")

names(GEI)[names(GEI)=="Country"]="cntry"
names(GEI)[names(GEI)=="Year"]="year"
names(GEI)[names(GEI)=="VALUE"]="value"

GEI<-GEI[,c("cntry","year","value")]

GEI$value<-as.numeric(GEI$value)
GEI$cntry<-recode_factor(as.factor(GEI$cntry),"Netherlands (the)"="Netherlands")

GEI<-GEI[-which(GEI$cntry %in% c("EU27 countries (from 2020)",
                                 "Luxembourg","Malta","Romania")),]
GEI$cntry<-droplevels(GEI$cntry)

GEI$mean<-numeric(nrow(GEI))
for (i in 1:nrow(GEI)){
  GEI$mean[i]=mean(GEI$value[GEI$cntry==GEI$cntry[i]],na.rm=T)
}
GEI$sd<-numeric(nrow(GEI))
for (i in 1:nrow(GEI)){
  GEI$sd[i]=sd(GEI$value[GEI$cntry==GEI$cntry[i]],na.rm=T)
}

CPSample$GEI<-numeric(nrow(CPSample))
for (i in 1:nrow(CPSample)){
  CPSample$GEI[i]=GEI$mean[which(GEI$cntry %in% CPSample$cntry[i])][1]
}

CM2.3 <- CPSample %>%
  filter(pcareworker == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    GEI = mean(GEI,na.rm=T),
    .groups = "drop"
  )

CM2.3<-CM2.3[-c(4,13,18,23,29),]

CM2.3 <- CM2.3 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM2.3<-scale(na.omit(CM2.3)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM2.3, kmeans, method = "wss") # 4 clusters

set.seed(123) 
CM2.3_cluster<-kmeans(CM2.3,centers=4,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM2.3_cluster, data = CM2.3, palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  ) # Not really clear outcome

### Using only the domain of power (data have to be introduced manually)

CM2.4 <- CPSample %>%
  filter(pcareworker == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    .groups = "drop"
  )

CM2.4$GEI_PPower<-numeric(nrow(CM2.4))
CM2.4$GEI_PPower[CM2.4$cntry=="Austria"]=mean(c(46.2,41.9,69.5,66.4))
CM2.4$GEI_PPower[CM2.4$cntry=="Belgium"]=mean(c(49.8,55,66,79.6))
CM2.4$GEI_PPower[CM2.4$cntry=="Bulgaria"]=mean(c(33.6,30.2,42.7,38.9))
CM2.4$GEI_PPower[CM2.4$cntry=="Switzerland"]=NA
CM2.4$GEI_PPower[CM2.4$cntry=="Cyprus"]=mean(c(15.2,11.8,20.1,19.9))
CM2.4$GEI_PPower[CM2.4$cntry=="Czechia"]=mean(c(18.1,21.7,29.5,21.7))
CM2.4$GEI_PPower[CM2.4$cntry=="Germany"]=mean(c(41.5,55.4,51.2,63.5))
CM2.4$GEI_PPower[CM2.4$cntry=="Denmark"]=mean(c(65.1,52.9,58.7,74.9))
CM2.4$GEI_PPower[CM2.4$cntry=="Estonia"]=mean(c(21.9,28.3,27.8,45.9))
CM2.4$GEI_PPower[CM2.4$cntry=="Spain"]=mean(c(59,58.6,77.2,77.7))
CM2.4$GEI_PPower[CM2.4$cntry=="Finland"]=mean(c(77.2,76.3,79.4,78.4))
CM2.4$GEI_PPower[CM2.4$cntry=="France"]=mean(c(51.2,66.5,76.4,79.7))
CM2.4$GEI_PPower[CM2.4$cntry=="United Kingdom"]=NA
CM2.4$GEI_PPower[CM2.4$cntry=="Greece"]=mean(c(24.3,22.7,20.6,30.6))
CM2.4$GEI_PPower[CM2.4$cntry=="Croatia"]=mean(c(25.6,24,31,35))
CM2.4$GEI_PPower[CM2.4$cntry=="Hungary"]=mean(c(7.3,7.1,14.6,10.9))
CM2.4$GEI_PPower[CM2.4$cntry=="Ireland"]=mean(c(18.9,24.1,31.8,36.2))
CM2.4$GEI_PPower[CM2.4$cntry=="Iceland"]=NA
CM2.4$GEI_PPower[CM2.4$cntry=="Italy"]=mean(c(18.7,31.3,42.1,41.2))
CM2.4$GEI_PPower[CM2.4$cntry=="Lithuania"]=mean(c(20.7,24.8,33.5,48.5))
CM2.4$GEI_PPower[CM2.4$cntry=="Latvia"]=mean(c(23.1,24.5,26.5,37.6))
CM2.4$GEI_PPower[CM2.4$cntry=="Netherlands"]=mean(c(53.7,56.1,61.7,67.9))
CM2.4$GEI_PPower[CM2.4$cntry=="Norway"]=NA
CM2.4$GEI_PPower[CM2.4$cntry=="Poland"]=mean(c(20.7,32.3,31.3,37.2))
CM2.4$GEI_PPower[CM2.4$cntry=="Portugal"]=mean(c(27.7,31.3,50.4,50.3))
CM2.4$GEI_PPower[CM2.4$cntry=="Sweden"]=mean(c(85.2,88.6,91.8,88.3))
CM2.4$GEI_PPower[CM2.4$cntry=="Slovenia"]=mean(c(30.1,51.1,36.6,54.3))
CM2.4$GEI_PPower[CM2.4$cntry=="Slovakia"]=mean(c(18.1,15.8,22.2,20.9))
CM2.4$GEI_PPower[CM2.4$cntry=="Ukraine"]=NA

CM2.4<-CM2.4[-c(4,13,18,23,29),]

CM2.4 <- CM2.4 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM2.4<-scale(na.omit(CM2.4)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM2.4, kmeans, method = "wss") # 4 clusters

set.seed(123) 
CM2.4_cluster<-kmeans(CM2.4,centers=4,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM2.4_cluster, data = CM2.4, palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  ) # Not really clear outcome

#########################

# Matrix 3: Combination of the previous two

CM3.2<-CPSample %>%
  filter(pcareworker == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    citizen   = mean(citizen == "National",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    #unlimited = mean(wrkctra == "Unlimited", na.rm = TRUE), # I'm dropping it cause it's highly correlated with sector
    #tertiary  = mean(education == "Tertiary", na.rm = TRUE),
    #ECEC_Exp   = mean(ECEC_Exp,na.rm=T),
    #LTC_Exp   = mean(LTC_Exp,na.rm=T),
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    .groups = "drop"
  )

CM3.2 <- CM3.2 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM3.2<-scale(na.omit(CM3.2)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM3.2, kmeans, method = "wss") # Its not super clear, lets go with 5

set.seed(123) 
CM3.2_cluster<-kmeans(CM3.2,centers=5,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM3.2_cluster, data = CM3.2, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07","green"#,"purple"
), 
ellipse.type = "euclid", # Concentration ellipse  
star.plot = TRUE, # Add segments from centroids to items 
repel = TRUE, # Avoid label overplotting (slow)  
ggtheme = theme_minimal()  )

# Calculate centroids' positions
1-(CM3.2_cluster$centers[,1] * (0.07401856) + (0.9411721)) # Share of non-citizens
CM3.2_cluster$centers[,2] * (0.18853876) + (0.6317164) # Share of public sector employment
CM3.2_cluster$centers[,3] * (1.13986203) + (1.9880419) # Expenditure as % of GDP

## And the same but with country of birth instead of citizenship
# (this is the clustering we ended up using)

CM3.3<-CPSample %>%
  filter(pcareworker == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    migrant   = mean(brncntr == "Yes",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    #unlimited = mean(wrkctra == "Unlimited", na.rm = TRUE), # I'm dropping it cause it's highly correlated with sector
    #tertiary  = mean(education == "Tertiary", na.rm = TRUE),
    #ECEC_Exp   = mean(ECEC_Exp,na.rm=T),
    #LTC_Exp   = mean(LTC_Exp,na.rm=T),
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    .groups = "drop"
  )

CM3.3 <- CM3.3 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM3.3<-scale(na.omit(CM3.3)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM3.3, kmeans, method = "wss") # 5

set.seed(123) 
CM3.3_cluster<-kmeans(CM3.3,centers=5,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM3.3_cluster, data = CM3.3, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07","green"#,"purple"
), 
ellipse.type = "euclid", # Concentration ellipse  
star.plot = TRUE, # Add segments from centroids to items 
repel = TRUE, # Avoid label overplotting (slow)  
ggtheme = theme_minimal()  )

# Calculate centroids' positions
1-(CM3.3_cluster$centers[,1] * (0.1082085) + (0.8778724)) # Share of migrants
CM3.3_cluster$centers[,2] * (0.1885388) + (0.6317164) # Share of public sector employment
CM3.3_cluster$centers[,3] * (1.1398620) + (1.9880419) # Expenditure as % of GDP

# Reorder clusters and give them names
CM3.3_cluster_final<-CM3.3_cluster

CM3.3_cluster_final$cluster<-recode_factor(as.factor(CM3.3_cluster_final$cluster),
                                           "1"="Public care",
                                           "3"="State-funded mix",
                                           "2"="Migrant reliant",
                                           "5"="Underfunded public sector",
                                           "4"="Absent state")

fviz_cluster(CM3.3_cluster_final, data = CM3.3, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07","green"#,"purple"
), 
ellipse.type = "euclid", # Concentration ellipse  
star.plot = TRUE, # Add segments from centroids to items 
repel = TRUE, # Avoid label overplotting (slow)  
ggtheme = theme_minimal()  )

#########################

# Matrix 4: Political participation items

CM4 <- CPSample %>%
  filter(pcareworker == "Yes" & wave>3) %>%
  mutate(cntrywave = paste(cntry, wave, sep = "_")) %>%
  group_by(cntrywave) %>%
  summarise(
    vote   = mean(vote_bin == "Yes",  na.rm = TRUE),
    union    = mean(mbtru_bin  == "Yes",    na.rm = TRUE),
    institutional = mean(part_inst == "Yes", na.rm = TRUE),
    non_institutional  = mean(part_ntinst == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

CM4 <- CM4 %>%
  tibble::column_to_rownames("cntrywave") # Turn country-waves into identifiers

CM4<-scale(na.omit(CM4)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM4, kmeans, method = "wss") # 5 seems the best value

set.seed(123) 
CM4_cluster<-kmeans(CM4,centers=5,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM4_cluster, data = CM4, palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","green"), 
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )

## Only with countries and not waves

CM4.2 <- CPSample %>%
  filter(pcareworker == "Yes" & wave>3) %>%
  group_by(cntry) %>%
  summarise(
    vote   = mean(vote_bin == "Yes",  na.rm = TRUE),
    union    = mean(mbtru_bin  == "Yes",    na.rm = TRUE),
    institutional = mean(part_inst == "Yes", na.rm = TRUE),
    non_institutional  = mean(part_ntinst == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

CM4.2 <- CM4.2 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM4.2<-scale(na.omit(CM4.2)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM4.2, kmeans, method = "wss") # 6 seems the best value, 4 returns a more clear picture

set.seed(123) 
CM4.2_cluster<-kmeans(CM4.2,centers=4,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM4.2_cluster, data = CM4.2, palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )

## Using the difference in averages with the entire workforce

CM4.2 <- CPSample %>%
  filter(pcareworker == "Yes" & wave>3) %>%
  group_by(cntry) %>%
  summarise(
    vote   = mean(vote_bin == "Yes",  na.rm = TRUE),
    union    = mean(mbtru_bin  == "Yes",    na.rm = TRUE),
    institutional = mean(part_inst == "Yes", na.rm = TRUE),
    non_institutional  = mean(part_ntinst == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

CM4.3 <- CPSample %>%
  filter(pcareworker == "No" & wave>3) %>%
  group_by(cntry) %>%
  summarise(
    vote   = mean(vote_bin == "Yes",  na.rm = TRUE),
    union    = mean(mbtru_bin  == "Yes",    na.rm = TRUE),
    institutional = mean(part_inst == "Yes", na.rm = TRUE),
    non_institutional  = mean(part_ntinst == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

CM4.3$vote<-(CM4.2$vote-CM4.3$vote)/CM4.3$vote
CM4.3$union<-(CM4.2$union-CM4.3$union)/CM4.3$union
CM4.3$institutional<-(CM4.2$institutional-CM4.3$institutional)/CM4.3$institutional
CM4.3$non_institutional<-(CM4.2$non_institutional-CM4.3$non_institutional)/CM4.3$non_institutional

CM4.3 <- CM4.3 %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM4.3<-scale(na.omit(CM4.3)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM4.3, kmeans, method = "wss") # 5 seems the best value

set.seed(123) 
CM4.3_cluster<-kmeans(CM4.3,centers=5,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM4.3_cluster, data = CM4.3, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07","green"),
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )

# Unscale the centroids:
CM4.3_cluster$centers[,1] * (0.1228704) + (-0.0562409) # Likelihoods of voting
CM4.3_cluster$centers[,2] * (0.3369132) + (0.0820918) # Likelihoods of unionising
CM4.3_cluster$centers[,3] * (0.1839734) + (-0.2580498) # Likelihoods of institutional participation
CM4.3_cluster$centers[,4] * (0.2839761) + (-0.1395385) # Likelihoods of non-institutional participation

###

# Matrix 5: migration, public sector, investment in both sectors, and political participation
CM5.1 <- CPSample %>%
  filter(pcareworker == "Yes" & wave>3) %>%
  group_by(cntry) %>%
  summarise(
    citizen   = mean(citizen == "National",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    #ECEC_Exp   = mean(ECEC_Exp,na.rm=T),
    #LTC_Exp   = mean(LTC_Exp,na.rm=T),
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    vote   = mean(vote_bin == "Yes",  na.rm = TRUE),
    union    = mean(mbtru_bin  == "Yes",    na.rm = TRUE),
    institutional = mean(part_inst == "Yes", na.rm = TRUE),
    non_institutional  = mean(part_ntinst == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

CM5.1 <- CM5.1%>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM5.1<-scale(na.omit(CM5.1)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM5.1, kmeans, method = "wss") # 4 or 5

set.seed(123) 
CM5.1_cluster<-kmeans(CM5.1,centers=4,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM5.1_cluster, data = CM5.1, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )

# Unscale the centroids
1-(CM5.1_cluster$centers[,1] * (0.07401856) + (0.9411721)) # Share of migrant labour
CM5.1_cluster$centers[,2] * (0.18853876) + (0.6317164) # Share of public sector employment
CM5.1_cluster$centers[,3] * (0.39303840) + (0.7290802) # Investment in Care Policies
CM5.1_cluster$centers[,5] * (0.11856181) + (0.7053256) # Share who vote
CM5.1_cluster$centers[,6] * (0.27623060) + (0.2839622) # Share who unionise
CM5.1_cluster$centers[,5] * (0.05291543) + (0.1358758) # Share in institutional participation
CM5.1_cluster$centers[,6] * (0.14620227) + (0.2647200) # Share in non-institutional participation

## Using diferences between care workers and entire workforce for participation items
CM5.1 <- CPSample %>%
  filter(pcareworker == "Yes" & wave>3) %>%
  group_by(cntry) %>%
  summarise(
    citizen   = mean(citizen == "National",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    #ECEC_Exp   = mean(ECEC_Exp,na.rm=T),
    #LTC_Exp   = mean(LTC_Exp,na.rm=T),
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    vote   = mean(vote_bin == "Yes",  na.rm = TRUE),
    union    = mean(mbtru_bin  == "Yes",    na.rm = TRUE),
    institutional = mean(part_inst == "Yes", na.rm = TRUE),
    non_institutional  = mean(part_ntinst == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

CM5.2 <- CPSample %>%
  filter(pcareworker == "No" & wave>3) %>%
  group_by(cntry) %>%
  summarise(
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    vote   = mean(vote_bin == "Yes",  na.rm = TRUE),
    union    = mean(mbtru_bin  == "Yes",    na.rm = TRUE),
    institutional = mean(part_inst == "Yes", na.rm = TRUE),
    non_institutional  = mean(part_ntinst == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

CM5.2$citizen<-CM5.1$citizen
CM5.2$public<-CM5.1$public
CM5.2$vote<-(CM5.1$vote-CM5.2$vote)/CM5.2$vote
CM5.2$union<-(CM5.1$union-CM5.2$union)/CM5.2$union
CM5.2$institutional<-(CM5.1$institutional-CM5.2$institutional)/CM5.2$institutional
CM5.2$non_institutional<-(CM5.1$non_institutional-CM5.2$non_institutional)/CM5.2$non_institutional

CM5.2 <- CM5.2%>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM5.2<-scale(na.omit(CM5.2)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM5.2, kmeans, method = "wss") # 4 or 5

set.seed(123) 
CM5.2_cluster<-kmeans(CM5.2,centers=4,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM5.2_cluster, data = CM5.2, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )

CM5.2_cluster$centers[,1] * (1.13804483) + (1.98896514) # Investment in Care Policies
CM5.2_cluster$centers[,2] * (0.11879539) + (-0.06340597) # Share who vote
CM5.2_cluster$centers[,3] * ( 0.33433322) + (0.06804278) # Share who unionise
CM5.2_cluster$centers[,4] * (0.18254433) + (-0.26573742) # Share in institutional participation
CM5.2_cluster$centers[,5] * (0.23500598) + (-0.10880770) # Share in non-institutional participation
1-(CM5.2_cluster$centers[,6] * (0.07401856) + (0.94117208)) # Share of migrant labour
CM5.2_cluster$centers[,7] * (0.18853876) + (0.63171636) # Share of public sector employment

## Using differences between care workers and entire workforce for care regime characteristics and participation items

CM5.1 <- CPSample %>%
  filter(pcareworker == "Yes" & wave>3) %>%
  group_by(cntry) %>%
  summarise(
    citizen   = mean(citizen == "National",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    #ECEC_Exp   = mean(ECEC_Exp,na.rm=T),
    #LTC_Exp   = mean(LTC_Exp,na.rm=T),
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    vote   = mean(vote_bin == "Yes",  na.rm = TRUE),
    union    = mean(mbtru_bin  == "Yes",    na.rm = TRUE),
    institutional = mean(part_inst == "Yes", na.rm = TRUE),
    non_institutional  = mean(part_ntinst == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

CM5.3 <- CPSample %>%
  filter(pcareworker == "No" & wave>3) %>%
  group_by(cntry) %>%
  summarise(
    citizen   = mean(citizen == "National",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    Care_Exp   = mean((ECEC_Exp+LTC_Exp),na.rm=T),
    vote   = mean(vote_bin == "Yes",  na.rm = TRUE),
    union    = mean(mbtru_bin  == "Yes",    na.rm = TRUE),
    institutional = mean(part_inst == "Yes", na.rm = TRUE),
    non_institutional  = mean(part_ntinst == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

CM5.3$citizen<-(CM5.1$citizen-CM5.3$citizen)/CM5.3$citizen
CM5.3$public<-(CM5.1$public-CM5.3$public)/CM5.3$public
CM5.3$vote<-(CM5.1$vote-CM5.3$vote)/CM5.3$vote
CM5.3$union<-(CM5.1$union-CM5.3$union)/CM5.3$union
CM5.3$institutional<-(CM5.1$institutional-CM5.3$institutional)/CM5.3$institutional
CM5.3$non_institutional<-(CM5.1$non_institutional-CM5.3$non_institutional)/CM5.3$non_institutional

CM5.3 <- CM5.3%>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM5.3<-scale(na.omit(CM5.3)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM5.3, kmeans, method = "wss") # 4 or 5

set.seed(123) 
CM5.3_cluster<-kmeans(CM5.3,centers=5,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM5.3_cluster, data = CM5.3, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07","green"),
             ellipse.type = "euclid", # Concentration ellipse  
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)  
             ggtheme = theme_minimal()  )

CM5.3_cluster$centers[,1] * (0.05697557) + (-0.01128775) # Dif in share of citizens
CM5.3_cluster$centers[,2] * (0.52152497) + (1.25935879) # Dif in share of public sector employment
CM5.3_cluster$centers[,3] * (1.13804483) + (1.98896514) # Dif investment in Care
CM5.3_cluster$centers[,5] * (0.11879539) + (-0.06340597) # Dif in share who vote
CM5.3_cluster$centers[,6] * (0.33433322) + (0.06804278) # Dif in share who unionise
CM5.3_cluster$centers[,5] * (0.18254433) + (-0.26573742) # Dif in share in institutional participation
CM5.3_cluster$centers[,6] * (0.23500598) + (-0.10880770) # Dif in share in non-institutional participation

save(list=ls(),file="ResultsP1.RData")

##############################################################################################
##############################################################################################
##############################################################################################

####
## 6) MODELS USING THE CLUSTERS
####

CPSample$clusterfinal<-factor(nrow(CPSample),levels=c("Public care",
                                                      "State-funded mix",
                                                      "Migrant reliant",
                                                      "Underfunded public sector",
                                                      "Absent state"))
CPSample$clusterfinal[which(CPSample$cntry %in% c("Finland","Denmark","Iceland",
                                              "Norway","Sweden","Netherlands"))]="Public care"
CPSample$clusterfinal[which(CPSample$cntry %in% c("Switzerland","Austria","Germany",
                                                  "France","Belgium","Ireland",
                                                  "United Kingdom","Slovenia"))]="State-funded mix"
CPSample$clusterfinal[which(CPSample$cntry %in% c("Italy","Spain"))]="Migrant reliant"
CPSample$clusterfinal[which(CPSample$cntry %in% c("Poland","Latvia","Estonia","Lithuania",
                                              "Czechia","Bulgaria","Hungary"))]="Underfunded public sector"
CPSample$clusterfinal[which(CPSample$cntry %in% c("Greece","Portugal","Cyprus","Croatia",
                                                  "Slovakia"))]="Absent state"

mclusterfinal_vote<-glmer(vote_bin~pcareworker*clusterfinal+
                            gndr+education+hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+
                            (1+pcareworker|cntry),
                          data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                          control = glmerControl(optimizer = "bobyqa"))
summary(mclusterfinal_vote) 

mclusterfinal_union<-glmer(mbtru_bin~pcareworker*clusterfinal+
                             gndr+education+hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+
                             (1+pcareworker|cntry),
                           data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                           control = glmerControl(optimizer = "bobyqa"))
summary(mclusterfinal_union) 

mclusterfinal_inst<-glmer(part_inst~pcareworker*clusterfinal+
                            gndr+education+hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+
                            (1+pcareworker|cntry),
                          data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                          control = glmerControl(optimizer = "bobyqa"))
summary(mclusterfinal_inst) 

mclusterfinal_ntinst<-glmer(part_ntinst~pcareworker*clusterfinal+
                              gndr+education+hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+
                              (1+pcareworker|cntry),
                            data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                            control = glmerControl(optimizer = "bobyqa"))
summary(mclusterfinal_ntinst) 

# Voting
plot_vote_clusterfinalint<-plot(predict_response(mclusterfinal_vote, c("clusterfinal", "pcareworker")))+
  scale_colour_manual(
    values = c("#F8766D", "#00BFC4")
  ) +
  scale_y_continuous(
    limits = c(0.6, 0.9),
    labels = scales::number_format(accuracy = 0.1)  # shows 0.0–1.0
  ) +
  labs(title="",x="Cluster",y="P(voting)",colour="Care worker")

plot_union_clusterfinalint<-plot(predict_response(mclusterfinal_union, c("clusterfinal", "pcareworker")))+
  scale_colour_manual(
    values = c("#F8766D", "#00BFC4")
  ) +
  scale_y_continuous(
    #limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.1)  # shows 0.0–1.0
  ) +
  labs(title="",x="Cluster",y="P(Unionising)",colour="Care worker")

plot_inst_clusterfinalint<-plot(predict_response(mclusterfinal_inst, c("clusterfinal", "pcareworker")))+
  scale_colour_manual(
    values = c("#F8766D", "#00BFC4")
  ) +
  scale_y_continuous(
    limits = c(0.09, 0.3),
    labels = scales::number_format(accuracy = 0.1)  # shows 0.0–1.0
  ) +
  labs(title="",x="Cluster",y="P(Institutional)",colour="Care worker")

plot_ntinst_clusterfinalint<-plot(predict_response(mclusterfinal_ntinst, c("clusterfinal", "pcareworker")))+
  scale_colour_manual(
    values = c("#F8766D", "#00BFC4")
  ) +
  scale_y_continuous(
    #limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.1)  # shows 0.0–1.0
  ) +
  labs(title="",x="Cluster",y="P(Non-institutional)",colour="Care worker")

plot_vote_clusterfinalint/ 
  plot_union_clusterfinalint/
  plot_inst_clusterfinalint/ 
  plot_ntinst_clusterfinalint +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",)

plot_vote_clusterfinalint/ 
  plot_union_clusterfinalint+
  plot_layout(guides = "collect") &
  theme(legend.position = "right",)

plot_inst_clusterfinalint/ 
  plot_ntinst_clusterfinalint +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",)

### First and second difference tests

# First and second difference tests for interaction effects
vote_1dif1<-comparisons(
    mclusterfinal_vote,
    variables = "clusterfinal",
    by = "pcareworker",
    type = "response",
    re.form = NA)

vote_1dif2<-comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  re.form = NA)

vote_dif2<-rbind(
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b2 - b1 = 0",
  re.form = NA),
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b3 - b1 = 0",
  re.form = NA),
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b4 - b1 = 0",
  re.form = NA),
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b5 - b1 = 0",
  re.form = NA),
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b3 - b2 = 0",
  re.form = NA),
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b4 - b2 = 0",
  re.form = NA),
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b5 - b2 = 0",
  re.form = NA),
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b4 - b3 = 0",
  re.form = NA),
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b5 - b3 = 0",
  re.form = NA),
  comparisons(
  mclusterfinal_vote,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  hypothesis = "b5 - b4 = 0",
  re.form = NA))

save(list=ls(),file="ResultsP1Final.RData")
  
##

union_1dif1<-comparisons(
  mclusterfinal_union,
  variables = "clusterfinal",
  by = "pcareworker",
  type = "response",
  re.form = NA)

union_1dif2<-comparisons(
  mclusterfinal_union,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  re.form = NA)

union_dif2<-rbind(
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b2 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b3 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b4 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b3 - b2 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b4 - b2 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b2 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b4 - b3 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b3 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_union,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b4 = 0",
    re.form = NA))

##

inst_1dif1<-comparisons(
  mclusterfinal_inst,
  variables = "clusterfinal",
  by = "pcareworker",
  type = "response",
  re.form = NA)

inst_1dif2<-comparisons(
  mclusterfinal_inst,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  re.form = NA)

inst_dif2<-rbind(
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b2 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b3 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b4 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b3 - b2 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b4 - b2 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b2 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b4 - b3 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b3 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_inst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b4 = 0",
    re.form = NA))

##

ntinst_1dif1<-comparisons(
  mclusterfinal_ntinst,
  variables = "clusterfinal",
  by = "pcareworker",
  type = "response",
  re.form = NA)

ntinst_1dif2<-comparisons(
  mclusterfinal_ntinst,
  variables = "pcareworker",
  by = "clusterfinal",
  type = "response",
  re.form = NA)

ntinst_dif2<-rbind(
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b2 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b3 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b4 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b1 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b3 - b2 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b4 - b2 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b2 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b4 - b3 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b3 = 0",
    re.form = NA),
  comparisons(
    mclusterfinal_ntinst,
    variables = "pcareworker",
    by = "clusterfinal",
    type = "response",
    hypothesis = "b5 - b4 = 0",
    re.form = NA))

##############################################################################################
##############################################################################################
##############################################################################################

####
## 7) ROBUSTNESS
####

##
# 7.1) Separate items for institutional and non-institutional participation
##

mclusterfinal_contplt<-glmer(contplt~pcareworker*clusterfinal+
                          gndr+education+hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+
                          (1+pcareworker|cntry),
                        data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                        control = glmerControl(optimizer = "bobyqa"))
summary(mclusterfinal_contplt)

mclusterfinal_wrkprty<-glmer(wrkprty~pcareworker*clusterfinal+
                          gndr+education+hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+
                          (1+pcareworker|cntry),
                        data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                        control = glmerControl(optimizer = "bobyqa"))
summary(mclusterfinal_wrkprty)

mclusterfinal_pbldmn<-glmer(pbldmn~pcareworker*clusterfinal+
                         gndr+education+hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+
                         (1+pcareworker|cntry),
                       data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                       control = glmerControl(optimizer = "bobyqa"))
summary(mclusterfinal_pbldmn)

mclusterfinal_sgnptit<-glmer(sgnptit~pcareworker*clusterfinal+
                          gndr+education+hinctnta_sca+agea_sca+lwp+nkids_sca+wave_fe+
                          (1+pcareworker|cntry),
                        data=CPSample,weights=anweight,family=binomial(link = "logit"),                
                        control = glmerControl(optimizer = "bobyqa"))
summary(mclusterfinal_sgnptit)

plot_contplt_clusterint<-plot(predict_response(mclusterfinal_contplt, c("clusterfinal", "pcareworker")))+
  scale_colour_manual(
    values = c("#F8766D", "#00BFC4")
  ) +
  scale_y_continuous(
    #limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.1)  # shows 0.0–1.0
  ) +
  labs(title="",x="Cluster",y="P(contacting)",colour="Care worker")

plot_wrkprty_clusterint<-plot(predict_response(mclusterfinal_wrkprty, c("clusterfinal", "pcareworker")))+
  scale_colour_manual(
    values = c("#F8766D", "#00BFC4")
  ) +
  scale_y_continuous(
    #limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.1)  # shows 0.0–1.0
  ) +
  labs(title="",x="Cluster",y="P(working party)",colour="Care worker")

plot_pbldmn_clusterint<-plot(predict_response(mclusterfinal_pbldmn, c("clusterfinal", "pcareworker")))+
  scale_colour_manual(
    values = c("#F8766D", "#00BFC4")
  ) +
  scale_y_continuous(
    #limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.1)  # shows 0.0–1.0
  ) +
  labs(title="",x="Cluster",y="P(demonstrating)",colour="Care worker")

plot_sgnptit_clusterint<-plot(predict_response(mclusterfinal_sgnptit, c("clusterfinal", "pcareworker")))+
  scale_colour_manual(
    values = c("#F8766D", "#00BFC4")
  ) +
  scale_y_continuous(
    #limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.1)  # shows 0.0–1.0
  ) +
  labs(title="",x="Cluster",y="P(signing)",colour="Care worker")

plot_contplt_clusterint/ 
plot_wrkprty_clusterint/
plot_pbldmn_clusterint/ 
plot_sgnptit_clusterint +
plot_layout(guides = "collect") &
theme(legend.position = "right",) 

##
# 7.2) Separate clusters and models for child care and LTC
##

# 7.3.1) Clusters for child care

CM_childcare<-CPSample %>%
  filter(childcare == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    migrant   = mean(brncntr == "Yes",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    ECEC_Exp   = mean((ECEC_Exp),na.rm=T),
    .groups = "drop"
  )

CM_childcare <- CM_childcare %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM_childcare<-scale(na.omit(CM_childcare)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM_childcare, kmeans, method = "wss") # 4

set.seed(123) 
CM_childcare_cluster<-kmeans(CM_childcare,centers=4,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM_childcare_cluster, data = CM_childcare, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07"#,"green"#,"purple"
), 
ellipse.type = "euclid", # Concentration ellipse  
star.plot = TRUE, # Add segments from centroids to items 
repel = TRUE, # Avoid label overplotting (slow)  
ggtheme = theme_minimal()  )

# Calculate centroids' positions
1-(CM_childcare_cluster$centers[,1] * (0.08571069) + (0.9163991)) # Share of migrants
CM_childcare_cluster$centers[,2] * (0.23064184) + (0.5770504) # Share of public sector employment
CM_childcare_cluster$centers[,3] * (0.41565543) + (0.7373683) # Expenditure as % of GDP

# 7.3.1) Clusters for LTC

CPSample$LTC<-factor(nrow(CPSample),levels=c("No","Yes"))
CPSample$LTC[which(CPSample$homebcare=="Yes" & CPSample$instcare=="Yes")]="Yes"
CPSample$LTC[which(CPSample$homebcare=="Yes" & CPSample$instcare=="No")]="Yes"
CPSample$LTC[which(CPSample$homebcare=="No" & CPSample$instcare=="Yes")]="Yes"
CPSample$LTC[which(CPSample$homebcare=="No" & CPSample$instcare=="No")]="No"

CM_LTC<-CPSample %>%
  filter(LTC == "Yes", wave > 3) %>%
  group_by(cntry) %>%
  summarise(
    migrant   = mean(brncntr == "Yes",  na.rm = TRUE),
    public    = mean(sector  == "Public",    na.rm = TRUE),
    ECEC_Exp   = mean((LTC_Exp),na.rm=T),
    .groups = "drop"
  )

CM_LTC <- CM_LTC %>%
  tibble::column_to_rownames("cntry") # Turn country-waves into identifiers

CM_LTC<-scale(na.omit(CM_LTC)) # Omit NAs and scale variables for clustering

# Figure out number of clusters
fviz_nbclust(CM_LTC, kmeans, method = "wss") # 6

set.seed(123) 
CM_LTC_cluster<-kmeans(CM_LTC,centers=6,iter.max=10,nstart=25) # The nstart parameter indicates that R will try 25 random initial assignments
# And select the best one

fviz_cluster(CM_LTC_cluster, data = CM_LTC, palette = c("purple", "#00AFBB", "#E7B800", "#FC4E07","green","darkred"
), 
ellipse.type = "euclid", # Concentration ellipse  
star.plot = TRUE, # Add segments from centroids to items 
repel = TRUE, # Avoid label overplotting (slow)  
ggtheme = theme_minimal()  )

# Calculate centroids' positions
1-(CM_LTC_cluster$centers[,1] * (0.1274966) + (0.8620703)) # Share of migrants
CM_LTC_cluster$centers[,2] * (0.2000936) + (0.6423910) # Share of public sector employment
CM_LTC_cluster$centers[,3] * (0.9396368) + (1.3013559) # Expenditure as % of GDP

save(list=ls(),file="ResultsP1.RData")
