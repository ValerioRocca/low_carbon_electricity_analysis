# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Remove variables from main dataframe
main = select(total_energy_data, -c("gdp", "biofuel_cons_change_pct",
            "biofuel_cons_change_twh", "biofuel_cons_per_capita",
            "biofuel_elec_per_capita", "coal_cons_change_pct",
            "coal_cons_change_twh", "coal_cons_per_capita",
            "coal_elec_per_capita","coal_prod_change_pct",
            "coal_prod_change_twh", "coal_prod_per_capita",
            "energy_cons_change_pct", "energy_per_capita", "energy_per_gdp",
            "fossil_cons_change_pct", "fossil_cons_change_twh",
            "fossil_elec_per_capita", "gas_cons_change_pct",
            "gas_cons_change_twh", "gas_elec_per_capita","gas_prod_change_pct",
            "gas_prod_change_twh", "gas_prod_per_capita",
            "hydro_cons_change_pct", "hydro_cons_change_twh",
            "hydro_elec_per_capita", "fossil_energy_per_capita",
            "hydro_energy_per_capita","low_carbon_cons_change_pct",
            "low_carbon_cons_change_twh", "low_carbon_elec_per_capita",
            "low_carbon_energy_per_capita", "net_elec_imports_share_demand",
            "nuclear_cons_change_pct", "nuclear_cons_change_twh",
            "nuclear_elec_per_capita", "nuclear_energy_per_capita",
            "oil_prod_per_capita", "gas_energy_per_capita",
            "oil_elec_per_capita", "oil_prod_change_pct",
            "oil_prod_change_twh", "other_renewable_exc_biofuel_electricity",
            "other_renewables_cons_change_pct",
            "other_renewables_cons_change_twh",
            "other_renewables_elec_per_capita",
            "other_renewables_elec_per_capita_exc_biofuel",
            "other_renewables_energy_per_capita", "other_renewables_share_elec_exc_biofuel",
            "per_capita_electricity","renewables_cons_change_pct",
            "renewables_cons_change_twh", "renewables_elec_per_capita",
            "renewables_energy_per_capita", "solar_cons_change_pct",
            "solar_cons_change_twh", "solar_elec_per_capita",
            "wind_cons_change_pct", "wind_cons_change_twh",
            "solar_energy_per_capita", "wind_elec_per_capita",
            "wind_energy_per_capita", "oil_cons_change_pct",
            "oil_cons_change_twh", "oil_energy_per_capita",
            "biofuel_consumption", "biofuel_electricity",
            "biofuel_share_elec","biofuel_share_energy"))

# Separate biofuel consumption from other renewables; rename the variables

#main = mutate(main,
#              other_renewable_consumption = other_renewable_consumption - biofuel_consumption,
#              other_renewable_electricity = other_renewable_exc_biofuel_electricity,
#              other_renewables_share_elec = other_renewables_share_elec_exc_biofuel,
#              other_renewables_share_energy = other_renewables_share_energy - biofuel_share_energy)
 
#main = select(main, -c("other_renewable_exc_biofuel_electricity",
#                                    "other_renewables_share_elec_exc_biofuel"))

# Remove variables without an ISO code

main = main[main$iso_code!='',]

# Merge other datasetsq

main = left_join(main,
                 select(country_areas, -c("Entity")),
                 by=c("iso_code" = "Code",
                      "year" = "Year"))
rm(country_areas)

main = left_join(main,
                 select(human_development_index, -c("Entity")),
                 by=c("iso_code" = "Code",
                      "year" = "Year"))
rm(human_development_index)

main = left_join(main,
                 select(urbanisation_rate, -c("Entity")),
                 by = c("iso_code" = "Code",
                        "year" = "Year"))
rm(urbanisation_rate)

death_rates_from_air_pollution = death_rates_from_air_pollution[, c(2,3,5)]
colnames(death_rates_from_air_pollution) = c("Code", "Year", "particulate_pollution")
main = left_join(main,
                 death_rates_from_air_pollution,
                 by = c("iso_code" = "Code",
                        "year" = "Year"))
rm(death_rates_from_air_pollution)

main = left_join(main,
                 select(share.of.land.area.used.for.agriculture, -c("Entity")),
                 by = c("iso_code" = "Code",
                        "year" = "Year"))
rm(share.of.land.area.used.for.agriculture)


coal.proved.reserves = coal.proved.reserves[coal.proved.reserves$Year!="2020",]
colnames(coal.proved.reserves) = c("Entity", "Code", "Year", "coal_reserves_2021")
main = left_join(main,
                 select(coal.proved.reserves, -c("Entity","Year")),
                 by = c("iso_code" = "Code"))
rm(coal.proved.reserves)

colnames(oil.proved.reserves.cia) = c("V1","V2", "oil_reserves_2012")
oil.proved.reserves.cia$oil_reserves_2012 = as.numeric(gsub(",","",oil.proved.reserves.cia$oil_reserves_2012))
main = left_join(main,
                 select(oil.proved.reserves.cia, -c("V1")),
                 by = c("country" = "V2"))
rm(oil.proved.reserves.cia)

colnames(uranium.proved.reserves.oecd) = c("V1","V2", "uranium_reserves_2019", "V4")
uranium.proved.reserves.oecd$uranium_reserves_2019 = as.numeric(gsub(",","",uranium.proved.reserves.oecd$uranium_reserves_2019))
uranium.proved.reserves.oecd$V1 = sub(".","",uranium.proved.reserves.oecd$V1)
main = left_join(main,
                 select(uranium.proved.reserves.oecd, -c("V2", "V4")),
                 by = c("country" = "V1"))
rm(uranium.proved.reserves.oecd)

main = left_join(main,
                 select(natural.gas.proved.reserves, -c("Entity",)),
                 by = c("iso_code" = "Code",
                        "year" = "Year"))
rm(natural.gas.proved.reserves)


govern_exp = government_total_expenditure_constant_2015_dollars[,4:66]
rm(government_total_expenditure_constant_2015_dollars)
colnames(govern_exp) = c("code", 1960:2021)
govern_exp = gather(govern_exp,
                    key = "year",
                    value = "government_expenditure",
                    -code)
govern_exp$year = as.integer(govern_exp$year)

main = left_join(main,
                 govern_exp,
                 by = c("iso_code" = "code",
                        "year" = "year"))
rm(govern_exp)

gdp = gdp_constant_2015_dollars[,4:66]
rm(gdp_constant_2015_dollars)
colnames(gdp) = c("code", 1960:2021)
gdp = gather(gdp,
             key = "year",
             value = "gdp",
             -code)
gdp$year = as.integer(gdp$year)

main = left_join(main,
                 gdp,
                 by = c("iso_code" = "code",
                        "year" = "year"))
rm(gdp)

rm(total_energy_data)

# Change variables names
colnames(main) = c(colnames(main[,1:59]), "land_area", "hdi", "urbaniz_rate",
                           "particulate_pollution", "agri_land_rate",
                           "coal_reserves_2021", "oil_reserves_2012",
                           "uranium_reserves_2019","gas_reserves",
                           "government_expenditure", "gdp")

# Change ".." to NA
main = mutate(main, government_expenditure = na_if(government_expenditure, ".."),
               gdp = na_if(gdp, ".."))
main$government_expenditure = as.numeric(main$government_expenditure)
main$gdp = as.numeric(main$gdp)




# ANALISI DESCRITTIVA DEL DATASET

# Andamento nel consumo delle rinnovabili nel mondo
# ... diviso per fonte
# ... diviso per geografia economica e/o territoriale
# Focus su qualche outlier e/o Italia

# Dati: electricity consumption + share electricity +
#       consumo energia primaria + share energia primaria


# Total and share in the world from renewables and low carbon
# ------

# Total renewable electricity
total_ren_elec = main[c("year","renewables_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(world_renew_elect = sum(renewables_electricity))

# Share renewable electricity
share_ren_elec = main[c("year","renewables_share_elec")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(world_renew_elect_share = sum(renewables_share_elec))
# ----

# Total and share of low carbon electricity by type
# ----

# Total low carbon electricity generation in the world grouped by type
total_lc_elec_grouped = main[c("year", "hydro_electricity",
    "nuclear_electricity", "solar_electricity", "wind_electricity",
    "other_renewable_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(world_nuclear_elec = sum(nuclear_electricity),
            world_hydro_elec = sum(hydro_electricity),
            world_wind_elec = sum(wind_electricity),
            world_solar_elec = sum(solar_electricity),
            world_other_elec = sum(other_renewable_electricity),
            world_nuclear_elec = sum(nuclear_electricity)) %>%
  gather(key = "type",
         value = "world_elect",
         -year)
total_lc_elec_grouped$type = factor(total_lc_elec_grouped$type,
  levels = c("world_nuclear_elec", "world_hydro_elec", "world_solar_elec", 
    "world_wind_elec", "world_other_elec"))

# Share of low carbon electricity generation in the world grouped by type
share_lc_elec_grouped = main[c("year","biofuel_electricity", "hydro_electricity",
                               "nuclear_electricity", "solar_electricity", "wind_electricity",
                               "other_renewable_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(world_nuclear_elec = sum(nuclear_electricity),
            world_hydro_elec = sum(hydro_electricity),
            world_wind_elec = sum(wind_electricity),
            world_solar_elec = sum(solar_electricity),
            world_biofuel_elec = sum(biofuel_electricity),
            world_other_elec = sum(other_renewable_electricity)) %>%
  select(world_nuclear_elec, world_hydro_elec, world_wind_elec, world_solar_elec,
         world_biofuel_elec, world_other_elec, world_nuclear_elec) %>%
  mutate(year = 1900:2022, row_total = rowSums(.)) %>%
  mutate(across(world_nuclear_elec:world_other_elec, ~ . / row_total * 100)) %>%
  select(-row_total) %>%
  gather(key = "type",
         value = "world_elect",
         -year)
share_lc_elec_grouped$type = factor(share_lc_elec_grouped$type,
      levels = c("world_nuclear_elec", "world_hydro_elec", "world_solar_elec", 
                 "world_wind_elec", "world_biofuel_elec", "world_other_elec"))
  

# Graphs
#----
# Total world electricity production from renewables
ggplot(total_ren_elec, aes(year, world_renew_elect)) +
  geom_area()+
  scale_x_continuous(limits = c(1975,2020))

# Total world electricity production from low carbon, grouped by type
ggplot(total_lc_elec_grouped, aes(year, world_elect, fill = type)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_x_continuous(limits = c(1975,2020)) +
  scale_fill_viridis(discrete = T)

# Share of world electricity production from low carbon, grouped by type
ggplot(share_lc_elec_grouped, aes(year, world_elect, fill = type)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_x_continuous(limits = c(1975,2020)) +
  scale_fill_viridis(discrete = T)









# Share of world electricity production from renewables
prova = main[c("year","electricity_generation","renewables_share_elec")] %>%
  mutate_all(~replace_na(.,0)) %>%
  mutate(country = main$iso_code) %>%
  group_by(year) %>%
  mutate(elect_rate = electricity_generation/sum(electricity_generation)) %>%
  summarise(renew_share_elec = sum(renewables_share_elec * elect_rate))

ggplot(prova, aes(year, renew_share_elec)) +
  geom_line() +
  scale_x_continuous(limits = c(1985,2020)) +
  scale_y_continuous(limits = c(15, 30))
  


# Total electricity production in the world from low carbon
prova_2 = main[c("year","low_carbon_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(world_lc_elect = sum(low_carbon_electricity))

ggplot(prova_2, aes(year, world_lc_elect,)) +
  geom_line(col = "red") +
  geom_line(aes(year, prova$world_renew_elect), col = "blue") +
  scale_x_continuous(limits = c(1975,2020))



