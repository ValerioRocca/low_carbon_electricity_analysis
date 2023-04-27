#----
# 1.7. World hydro electricity generation growth time series, 1975-2020, base
# 2000 = 100 (note: not 1985!! Because african countries have missing values)
#----
electricity_generation_by_type = function(type){
  
  place = select(main, year, x = type) %>%
    mutate_all(~replace_na(.,0)) %>%
    cbind(., tag = main$tag) %>%
    filter(year >= 2000, year <= 2020) %>%
    group_by(year, tag) %>%
    summarize(var = sum(x)) %>%
    spread(key = tag, value = var) %>%
    apply(., 2, function(x) x*100/x[1]) %>%
    as.data.frame() %>%
    mutate(year = year*2000/100) %>%
    gather(key = "tag", value = place_var, -year)
  
  place_plot = ggplot(place, aes(year, place_var, color = tag)) +
    geom_line(size=1)+
    scale_color_viridis(discrete = T)
  
  return(place_plot)
}

electricity_generation_by_type("solar_electricity")








#----


# Evolution of the electricity generation by hydropower from 1985 = 100



# Time series of the share of renewable generation by macroregion
share_lc_elec_macro = main[c("year","hydro_electricity", "electricity_generation")] %>%
  mutate_all(~replace_na(.,0)) %>%
  cbind(., tag = main$tag) %>%
  filter(year >= 1985, year <= 2020) %>%
  group_by(year, tag) %>%
  summarize(world_hydro_share = sum(hydro_electricity)/sum(electricity_generation))

ggplot(share_lc_elec_macro, aes(year, world_hydro_share, color = tag)) +
  geom_line(size=1)+
  scale_x_continuous(limits = c(1985,2020)) +
  scale_color_viridis(discrete = T)

# Evolution of the electricity share by hydropower from 1985 = 100
share_lc_elec_macro = main[c("year","hydro_electricity", "electricity_generation")] %>%
  mutate_all(~replace_na(.,0)) %>%
  cbind(., tag = main$tag) %>%
  filter(year >= 1985, year <= 2020, tag != "sub_african") %>%
  group_by(year, tag) %>%
  summarize(world_hydro_share = sum(hydro_electricity)/sum(electricity_generation)) %>%
  spread(key = tag, value = world_hydro_share) %>%
  apply(., 2, function(x) x*100/x[1]) %>%
  as.data.frame() %>%
  mutate(year = year*1985/100) %>%
  gather(key = "tag", value = "hydro_elec_macro", -year)

ggplot(share_lc_elec_macro, aes(year, hydro_elec_macro, color = tag)) +
  geom_line(size=1)+
  scale_x_continuous(limits = c(1985,2020)) +
  scale_y_continuous(limits = c(25, 150)) +
  scale_color_viridis(discrete = T) +
  geom_hline(yintercept = 100, lineType = "dashed", color = "red", size = 0.5)




#..... (eventually repeat for other low carbons)



# Total low carbon electricity generation in the developed economies, grouped by Type
total_lc_elec_grouped = main[c("year", "hydro_electricity",
                               "nuclear_electricity", "solar_electricity", "wind_electricity",
                               "other_renewable_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  cbind(., tag = main$tag) %>%
  filter(year >= 1975, year <= 2020, tag == "developed") %>%
  group_by(year) %>%
  summarize(Nuclear = sum(nuclear_electricity),
            Hydro = sum(hydro_electricity),
            Wind = sum(wind_electricity),
            Solar = sum(solar_electricity),
            Other = sum(other_renewable_electricity)) %>%
  gather(key = "Type",
         value = "world_elect",
         -year)
total_lc_elec_grouped$Type = factor(total_lc_elec_grouped$Type,
                                    levels = c("Nuclear", "Hydro", "Solar", 
                                               "Wind", "Other"))

ggplot(total_lc_elec_grouped, aes(year, world_elect, fill = Type)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_x_continuous(limits = c(1975,2020)) +
  scale_fill_viridis(discrete = T)



# Share of low carbon electricity generation in the developed economies, grouped by Type
share_lc_elec_grouped = main[c("year","hydro_electricity", "wind_electricity",
                               "nuclear_electricity", "solar_electricity", 
                               "other_renewable_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  cbind(., tag = main$tag) %>%
  filter(tag == "developed") %>%
  group_by(year) %>%
  summarize(Nuclear = sum(nuclear_electricity),
            Hydro = sum(hydro_electricity),
            Wind = sum(wind_electricity),
            Solar = sum(solar_electricity),
            Other = sum(other_renewable_electricity)) %>%
  select(Nuclear, Hydro, Wind, Solar,
         Other, Nuclear) %>%
  mutate(year = 1900:2022, row_total = rowSums(.)) %>%
  mutate(across(Nuclear:Other, ~ . / row_total * 100)) %>%
  select(-row_total) %>%
  gather(key = "Type",
         value = "world_elect",
         -year)
share_lc_elec_grouped$Type = factor(share_lc_elec_grouped$Type,
                                    levels = c("Nuclear", "Hydro", "Solar", 
                                               "Wind", "Other"))

ggplot(share_lc_elec_grouped, aes(year, world_elect, fill = Type)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_x_continuous(limits = c(1975,2020)) +
  scale_fill_viridis(discrete = T)


# Growth of low carbon electricity generation in the developed economies, grouped by type (5Y)
growth_lc_elec_grouped = main[c("year", "hydro_electricity",
                                "nuclear_electricity", "solar_electricity", "wind_electricity",
                                "other_renewable_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  cbind(., tag = main$tag) %>%
  filter(tag == "advanced") %>%
  group_by(year) %>%
  summarize(Nuclear = sum(nuclear_electricity),
            Hydro = sum(hydro_electricity),
            Wind = sum(wind_electricity),
            Solar = sum(solar_electricity),
            Other = sum(other_renewable_electricity)) %>%
  mutate(Nuclear = (Nuclear - dplyr::lag(Nuclear,5)) / dplyr::lag(Nuclear,5),
         Hydro = (Hydro - dplyr::lag(Hydro,5)) / dplyr::lag(Hydro,5),
         Wind = (Wind - dplyr::lag(Wind,5)) / dplyr::lag(Wind,5),
         Solar = (Solar - dplyr::lag(Solar,5)) / dplyr::lag(Solar,5),
         Other = (Other - dplyr::lag(Other,5)) / dplyr::lag(Other,5),) %>%
  select(-Nuclear, -Hydro, -Wind, -Solar, -Other) %>%
  gather(key = "Type",
         value = "world_elect",
         -year)
growth_lc_elec_grouped$Type = factor(growth_lc_elec_grouped$Type,
                                     levels = c("Nuclear", "Hydro", "Solar", 
                                                "Wind", "Other"))

ggplot(growth_lc_elec_grouped, aes(year, world_elect, colour = Type)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(1995,2020)) +
  scale_y_continuous(limits = c(-1.5,13))

ggplot(filter(growth_lc_elec_grouped, Type != "Solar"), 
       aes(year, world_elect, colour = Type)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(1995,2020)) +
  scale_y_continuous(limits = c(-1,4))

