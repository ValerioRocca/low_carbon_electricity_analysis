#### Libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(car) # for box-cox
#library(maps) # for world map
library(rworldmap) #for world map
library(glmnet)


#### Preparation and Cleaning ####

# 1) Remove variables from main dataframe
main = select(total_energy_data,
              -c("gdp",
            "biofuel_cons_change_pct",
            "biofuel_cons_change_twh",
            "biofuel_cons_per_capita",
            "biofuel_elec_per_capita",
            "coal_cons_change_pct",
            "coal_cons_change_twh",
            "coal_cons_per_capita",
            "coal_elec_per_capita",
            "coal_prod_change_pct",
            "coal_prod_change_twh",
            "coal_prod_per_capita",
            "coal_consumption",
            "coal_share_energy",
            "energy_cons_change_pct",
            "energy_per_capita",
            "energy_per_gdp",
            "electricity_share_energy",
            "fossil_cons_change_pct",
            "fossil_cons_change_twh",
            "fossil_elec_per_capita",
            "fossil_fuel_consumption",
            "fossil_share_energy",
            "gas_cons_change_pct",
            "gas_cons_change_twh",
            "gas_elec_per_capita",
            "gas_prod_change_pct",
            "gas_prod_change_twh",
            "gas_prod_per_capita",
            "gas_consumption",
            "gas_share_energy",
            "hydro_cons_change_pct",
            "hydro_cons_change_twh",
            "hydro_elec_per_capita",
            "fossil_energy_per_capita",
            "hydro_energy_per_capita",
            "hydro_consumption",
            "hydro_share_energy",
            "low_carbon_cons_change_pct",
            "low_carbon_cons_change_twh",
            "low_carbon_elec_per_capita",
            "low_carbon_energy_per_capita",
            "low_carbon_consumption",
            "low_carbon_share_energy",
            "net_elec_imports_share_demand",
            "nuclear_cons_change_pct",
            "nuclear_cons_change_twh",
            "nuclear_elec_per_capita",
            "nuclear_energy_per_capita",
            "nuclear_consumption",
            "nuclear_share_energy",
            "oil_prod_per_capita",
            "gas_energy_per_capita",
            "oil_elec_per_capita",
            "oil_prod_change_pct",
            "oil_prod_change_twh",
            "oil_consumption",
            "oil_share_energy",
            "other_renewable_exc_biofuel_electricity",
            "other_renewables_cons_change_pct",
            "other_renewables_cons_change_twh",
            "other_renewables_elec_per_capita",
            "other_renewables_elec_per_capita_exc_biofuel",
            "other_renewables_energy_per_capita",
            "other_renewables_share_elec_exc_biofuel",
            "other_renewable_consumption",
            "other_renewables_share_energy",
            "per_capita_electricity",
            "renewables_cons_change_pct",
            "renewables_cons_change_twh",
            "renewables_elec_per_capita",
            "renewables_energy_per_capita",
            "renewables_consumption",
            "renewables_share_energy",
            "solar_cons_change_pct",
            "solar_cons_change_twh",
            "solar_elec_per_capita",
            "solar_consumption",
            "solar_share_energy",
            "wind_cons_change_pct",
            "wind_cons_change_twh",
            "wind_consumption",
            "wind_share_energy",
            "solar_energy_per_capita",
            "wind_elec_per_capita",
            "wind_energy_per_capita",
            "oil_cons_change_pct",
            "oil_cons_change_twh",
            "oil_energy_per_capita",
            "biofuel_consumption",
            "biofuel_electricity",
            "biofuel_share_elec",
            "biofuel_share_energy"))

# Remove units without an ISO code
main = main[main$iso_code!='',]

# 2) Merge other datasets

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
colnames(main) = c(colnames(main[,1:36]), "land_area", "hdi", "urbaniz_rate",
                           "particulate_pollution", "agri_land_rate",
                           "coal_reserves_2021", "oil_reserves_2012",
                           "uranium_reserves_2019","gas_reserves", "gdp")

# Change ".." to NA
main = mutate(main, gdp = na_if(gdp, ".."))
main$gdp = as.numeric(main$gdp)


#### ANALISI DESCRITTIVA DEL DATASET ####


# 1. Analysis on the world electricity generation from LC sources ----
# 1.1. World renewable and LC electricity generation time series, 1965-2020 ----
place = main[c("year","renewables_electricity", "low_carbon_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(Renewables = sum(renewables_electricity),
            Low_carbon = sum(low_carbon_electricity) - sum(renewables_electricity)) %>%
  gather(key = "Type",
         value = "elect",
         -year)

ggplot(place, aes(year, elect, fill = Type)) +
  geom_area(alpha=0.6, colour="white") +
  scale_x_continuous(limits = c(1965,2020), breaks = seq(1965, 2020, by = 5)) +
  ylim(0, 10500) +
  scale_fill_manual(name = "Source", labels = c("Nuclear", "Renewables"),
                    values = c("#B2FF00", "#0060FA")) +
  labs(title = "World electricity generation from renewables and nuclear",
       x = "Year",
       y = "Terawatt-hours")


# In realtà nella prima slide della presentazione mettici la produzione totale:
place = main[c("year", "low_carbon_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(sum_lc = sum(low_carbon_electricity))

ggplot(place, aes(year, sum_lc)) +
  geom_area(alpha = 0.7, colour="white") +
  scale_x_continuous(limits = c(1965,2020), breaks = seq(1965, 2020, by = 5)) +
  labs(title = "World electricity generation from low carbon sources",
       x = "Year",
       y = "Terawatt-hours")

# Electricity from LC in 1965, today and avg growth rate
place = filter(place, year == 1965 | year == 2020)
place
(place[2,2]/place[1,2]) / (place[2,1]-place[1,1])
  

# E poi parla di quali sono i paesi che generano più elettricità:

# Prima cosa: quali sono i paesi che generano più elettricità nel 2020?
place = main[c("year", "low_carbon_electricity")] %>% mutate_all(~replace_na(.,0))
place = cbind(iso_code = main$iso_code, place) %>%
  filter(year == 2020) %>%
  arrange(desc(low_carbon_electricity))

as_tibble(place)
place_2 = place$iso_code[1:9]

place = main[c("year", "low_carbon_electricity")] %>% mutate_all(~replace_na(.,0))
place = cbind(iso_code = main$iso_code, place) 
for(i in 1:nrow(place)){
  place$iso_code[i] = ifelse(place$iso_code[i] %in% place_2, place$iso_code[i], "OTH")
}
place = group_by(place, iso_code, year) %>%
  summarize(sum_lc = sum(low_carbon_electricity))

place$iso_code = factor(place$iso_code,
                        levels = c("CHN", "USA", "BRA", "CAN", "FRA", "RUS",
                                   "IND", "DEU", "JPN", "OTH"))

ggplot(place, aes(year, sum_lc, fill = iso_code)) +
  geom_area(alpha=0.6, colour="white") +
  scale_x_continuous(limits = c(1965,2020), breaks = seq(1965, 2020, by = 5)) +
  labs(title = "World electricity generation from low carbon sources, grouped by countries",
       x = "Year",
       y = "Terawatt-hours") +
  scale_fill_viridis_d(name = "Source", option = "E")



#----
# 1.2. World LC electricity generation growth time series, 1975-2020, 5 years
#      moving average
#----

# In realtà qui mettici time series della produzione da nucleare e da rinnovabili

place = main[c("year", "renewables_electricity", "nuclear_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(sum_ren = sum(renewables_electricity),
            sum_nucl = sum(nuclear_electricity)) %>%
  gather(key = "Source", value = "value", -year) 

ggplot(place, aes(year, value, color = Source)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#B2FF00", "#0060FA")) +
  scale_x_continuous(limits = c(1965,2020), breaks = seq(1965, 2020, by = 5))





#----
# 1.3. World LC electricity generation time series, grouped by type, 1975-2020
#----
place = main[c("year", "hydro_electricity",
                               "nuclear_electricity", "solar_electricity", "wind_electricity",
                               "other_renewable_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(Nuclear = sum(nuclear_electricity),
            Hydro = sum(hydro_electricity),
            Wind = sum(wind_electricity),
            Solar = sum(solar_electricity),
            Other = sum(other_renewable_electricity)) %>%
  gather(key = "Type",
         value = "elect",
         -year)
place$Type = factor(place$Type, levels = c("Nuclear", "Hydro", "Solar", 
                                           "Wind", "Other"))

ggplot(place, aes(year, elect, fill = Type)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_x_continuous(limits = c(1965,2020), breaks = seq(1965, 2020, by = 5)) +
  #xlim(1975,2020)??
  scale_y_continuous(limits = c(0,10500)) +
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  labs(title = "World electricity generation from low carbon sources, grouped by source",
       x = "Year",
       y = "Terawatt-hours")

#----
# 1.4. World LC electricity generation time series, 1975-2020, 5 years
#      moving average, grouped by type
#----
place = main[c("year", "hydro_electricity", "nuclear_electricity",
               "solar_electricity", "wind_electricity",
               "other_renewable_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
  filter(year <= 2020) %>%
  group_by(year) %>%
  summarize(Nuclear = sum(nuclear_electricity),
            Hydro = sum(hydro_electricity),
            Wind = sum(wind_electricity),
            Solar = sum(solar_electricity),
            Other = sum(other_renewable_electricity)) %>%
  mutate(Nuclear = (Nuclear - dplyr::lag(Nuclear,5)), #/ dplyr::lag(Nuclear,5),
         Hydro = (Hydro - dplyr::lag(Hydro,5)), #/ dplyr::lag(Hydro,5),
         Wind = (Wind - dplyr::lag(Wind,5)), #/ dplyr::lag(Wind,5),
         Solar = (Solar - dplyr::lag(Solar,5)), #/ dplyr::lag(Solar,5),
         Other = (Other - dplyr::lag(Other,5))) %>% #/ dplyr::lag(Other,5),) %>%
  gather(key = "Type",
         value = "world_elect",
         -year)
place$Type = factor(place$Type, levels = c("Nuclear", "Hydro", "Solar", 
                                                "Wind", "Other"))

ggplot(place, aes(year, world_elect, colour = Type)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(1995,2020)) +
  #xlim(1995,2020)??
  scale_y_continuous() +
  scale_color_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B"))


#----
# 1.5. World LC electricity generation share w.r.t LC electricity time series,
#      1975-2020, grouped by type
#----
place = main[c("year","hydro_electricity", "wind_electricity", 
               "nuclear_electricity", "solar_electricity", 
               "other_renewable_electricity")] %>%
  mutate_all(~replace_na(.,0)) %>%
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
         value = "elect",
         -year)
place$Type = factor(place$Type, levels = c("Nuclear", "Hydro", "Solar", 
                                           "Wind", "Other"))

ggplot(place, aes(year, elect, fill = Type)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_x_continuous(limits = c(1965,2020), breaks = seq(1965, 2020, by = 5)) +
  
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B"))
#----
# 1.6. World LC electricity generation share w.r.t total electricity generation
#      time series, 1975-2020, grouped by type
#----
place = main[c("year","hydro_electricity", "wind_electricity", 
               "nuclear_electricity", "solar_electricity", 
               "other_renewable_electricity", "electricity_generation")] %>%
  mutate_all(~replace_na(.,0)) %>%
  group_by(year) %>%
  summarize(Nuclear = sum(nuclear_electricity)/sum(electricity_generation),
            Hydro = sum(hydro_electricity)/sum(electricity_generation),
            Wind = sum(wind_electricity)/sum(electricity_generation),
            Solar = sum(solar_electricity)/sum(electricity_generation),
            Other = sum(other_renewable_electricity)/sum(electricity_generation))%>%
  mutate(year = 1900:2022, row_total = rowSums(.)) %>%
  mutate(across(Nuclear:Other, ~ . / row_total * 100 * 1000)) %>%
  select(-row_total) %>%
  gather(key = "Type",
         value = "elect",
         -year)
place$Type = factor(place$Type, levels = c("Nuclear", "Hydro", "Solar", 
                                           "Wind", "Other"))

ggplot(place, aes(year, elect, fill = Type)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_x_continuous(limits = c(1965,2020), breaks = seq(1965, 2020, by = 5))  +
  #xlim(1985,2020)??
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  labs(title = "World electricity generation from low carbon sources (sum = 100), grouped by source",
       x = "Year",
       y = "% of terawatt-hours")

#----


# Grouping of countries in regions
#----
# Countries are divided on the basis of socio, economical and/or geographical
# metrics. The divisions are:
# 1) Developed countries (Western Europe, US, CANZUK, Israel, Japan, Korea,
#    Taiwan, HK, Macao)
# 2) Latin America and the Caribbeans (including French Guyana and other
#    dependencies)
# 3) Eastern Europe: ex-Warsaw pact (excluding -stan countries)
# 4) Sub-Saharan Africa
# 5) Middle east and North Africa
# 6) Emerging and Developing Asia

# a) Extract the unique ISO codes
unique(main$iso_code)

# b) Create a vector for each group, containing the ISO-codes
developed_countries = c("AUS", "AUT", "BEL", "CAN", "CYP", "DNK", "FRO", "FIN",
                        "FRA", "DEU", "GRC", "GRL", "HKG", "ISL", "IRL", "ISR",
                        "ITA", "JPN", "LUX", "MAC", "MLT", "NLD", "NZL", "NOR",
                        "PRT", "SPM", "KOR", "ESP", "SWE", "CHE", "TWN", "GBR",
                        "USA", "REU", "GIB")

latin_countries = c("ATG", "ARG", "ABW", "BHS", "BRB", "BLZ", "BMU", "BOL",
                    "BRA", "CYM", "CHL", "COL", "CRI", "CUB", "DMA", "DOM",
                    "ECU", "SLV", "FLK", "GUF", "GRD", "GLP", "GTM", "GUY",
                    "HTI", "HND", "JAM", "MTQ", "MEX", "MSR", "NIC", "PAN",
                    "PRY", "PER", "PRI", "KNA", "LCA", "VCT", "SUR", "TTO",
                    "TCA", "VIR", "URY", "VEN", "VGB", "ANT")

east_europe_countries = c("ALB", "ARM", "AZE", "BLR", "BIH", "BGR", "HRV",
                          "CZE", "EST", "GEO", "HUN", "LVA", "LTU", "MDA",
                          "MNE", "MKD", "POL", "ROU", "RUS", "SRB", "SVK",
                          "SVN", "UKR")

sub_african_countries = c("AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR",
                          "CAF", "TCD", "COM", "COG", "CIV", "COD", "DJI",
                          "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA",
                          "GIN", "GNB", "KEN", "LSO", "LBR", "MDG", "MWI",
                          "MLI", "MRT", "MUS", "MOZ", "NAM", "NER", "NGA",
                          "RWA", "STP", "SEN", "SLE", "SOM", "ZAF", "SSD",
                          "SDN", "TZA", "TGO", "UGA", "ZMB", "ZWE", "SHN")

middle_east_countries = c("DZA", "BHR", "EGY", "IRN", "IRQ", "JOR", "KWT",
                          "LBN", "LBY", "MAR", "OMN", "PSE", "QAT", "SAU",
                          "SYR", "TUR", "ARE", "YEM", "TUN")

asian_countries = c("AFG", "ASM", "BGD", "BTN", "BRN", "KHM", "CHN", "COK",
                    "FJI", "PYF", "GUM", "IND", "IDN", "KAZ", "KIR", "KGZ",
                    "LAO", "MYS", "MDV", "FSM", "MNG", "MMR", "NRU", "NPL",
                    "NCL", "PRK", "MNP", "PAK", "PNG", "PHL", "WSM", "VNM",
                    "SYC", "SGP", "SLB", "LKA", "TJK", "THA", "TLS", "TON",
                    "TKM", "TUV", "UZB", "VUT", "NIU")


tag = rep(0, nrow(main))

for(i in 1:length(tag)){
  if(main$iso_code[i] %in% developed_countries){
    tag[i] = "developed"
  }
  else{
    if(main$iso_code[i] %in% latin_countries){
      tag[i] = "latin"
    }
    else{
      if(main$iso_code[i] %in% east_europe_countries){
        tag[i] = "east_europe"
      }
      else{
        if(main$iso_code[i] %in% sub_african_countries){
          tag[i] = "sub_african"
        }
        else{
          if(main$iso_code[i] %in% middle_east_countries){
            tag[i] = "middle_east"
          }
          else{
            if(main$iso_code[i] %in% asian_countries){
              tag[i] = "asian"
            }
          }
        }
      }
    }
  }
}

# c) Add the vector to the table
main = cbind(main, tag)

# MOMENTANEO: elimina i paesi senza ISO (poi integrare con quello di Jack)
main = filter(main, iso_code != "ATA", iso_code != "ESH")

# Plot della mappa del mondo
df_asian = data.frame(region = "Asia", tag = asian_countries)
df_east = data.frame(region = "Eastern Europe", tag = east_europe_countries)
df_middle = data.frame(region = "Middle East", tag = middle_east_countries)
df_dev = data.frame(region = "Developed", tag = developed_countries)
df_africa = data.frame(region ="Africa", tag = sub_african_countries)
df_latin = data.frame(region = "Latin America", tag = latin_countries)
df_world = rbind(df_asian, df_east, df_middle, df_dev, df_africa, df_latin)


map = joinCountryData2Map(df_world, joinCode = "ISO3",
                          nameJoinColumn = "tag")

mapCountryData(map, nameColumnToPlot = "region", catMethod = "categorical",
               missingCountryCol = gray(.8),
               colourPalette = c("#35B779", "#FDE725", "#21908C", "#440154",
                                 "#8FD744", "#30678D"),
               mapTitle = "World grouping in macroregions")

#----
# 1.7. Ratio between LC electricity generation in 2020 and total electricity
#      generation in 2020 (y) VS ratio between average LC electricity generation
#      in 2011-2020 w.r.t total electricity production in 2011-2020 (x)
#
#      Why this calculation: we're interested in studying not only which area
#      produces more electricity from LC, but what's the trend. On y we have
#      the ratio and not the growth in renewables as the developing countries
#      a growth in the renewables production doesn't necessarily mean a push
#      for the renewables; there can be a general push for more electricity
#      generation
#----

place = select(main, year, low_carbon_electricity, renewables_electricity,
               electricity_generation) %>%
  mutate_all(~replace_na(.,0)) %>%
  cbind(., tag = main$tag) %>%
  filter(year == 2010 | year == 2020 ) %>%
  gather(key = "type", value = "lc_generation", -year, -electricity_generation, -tag) %>%
  group_by(year, tag, type) %>%
  summarize(perc_elec_from_lc = sum(lc_generation)*100 /sum(electricity_generation)) %>%
  spread(key = year, value = perc_elec_from_lc)
colnames(place) = c("tag", "Source", "ten", "twenty")

place[place$Source  == "low_carbon_electricity", "Source"] = "LC"
place[place$Source  == "renewables_electricity", "Source"] = "REN"

ggplot(place, aes(ten, twenty, col = Source)) +
  geom_point(size = 3) +
  geom_line(aes(group = tag), color = "black", linetype = "dashed") +
  geom_text(aes(label = tag), hjust = 0, vjust = 1.5) +
  scale_color_manual(values = c("#59E80C", "#141BDB")) +
  labs(title = "Share of electricity generation from LC and renewable sources, grouped by macroregion",
       x = "% electricity generation 2010",
       y = "% electricity generation 2020") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_abline(intercept = 0, slope = 1.5, linetype = "dashed", color = "red")


#----
# 1.8. World LC electricity generation share w.r.t total electricity generation
#      time series, 1985-2020, grouped by type and area
#----

place = main[c("year","hydro_electricity", "wind_electricity", 
               "nuclear_electricity", "solar_electricity", 
               "other_renewable_electricity", "electricity_generation")] %>%
  mutate_all(~replace_na(.,0)) %>%
  cbind(., tag = main$tag) %>%
  group_by(year, tag) %>%
  summarize(Nuclear = sum(nuclear_electricity)/sum(electricity_generation),
            Hydro = sum(hydro_electricity)/sum(electricity_generation),
            Wind = sum(wind_electricity)/sum(electricity_generation),
            Solar = sum(solar_electricity)/sum(electricity_generation),
            Other = sum(other_renewable_electricity)/sum(electricity_generation)) %>%
#place_2 = select(place, year, tag)
#place = ungroup(place) %>%
#  select(-year, -tag) %>%
  # mutate(row_total = rowSums(.)) %>%
  # mutate(across(Nuclear:Other, ~ . / row_total * 100 * 1000)) %>%
  # select(-row_total) %>%
#  cbind(.,place_2) %>%
  gather(key = "Type",
         value = "elect",
         -year, -tag)
place$Type = factor(place$Type, levels = c("Nuclear", "Hydro", "Solar", 
                                           "Wind", "Other"))
for(i in 1:nrow(place)){
  if(place$year[i] < 2000 & place$tag[i] == "sub_african"){
    place$elect[i] = 0
  }
}

place[place$tag  == "asian", "tag"] = "Asian"
place[place$tag  == "developed", "tag"] = "Developed"
place[place$tag  == "east_europe", "tag"] = "Eastern Europe"
place[place$tag  == "latin", "tag"] = "Latin America & Caribbeans"
place[place$tag  == "middle_east", "tag"] = "Middle East & Northern Africa"
place[place$tag  == "sub_african", "tag"] = "Sub-Saharan Africa"

ggplot(place, aes(year, elect, fill = Type)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_x_continuous(limits = c(1985,2020)) +
  #xlim(1985,2020)??
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  facet_wrap(~ tag, nrow = 2) +
  labs(title = "Share of electricity generation from LC and renewable sources, grouped by source and macroregion",
       x = "Year",
       y = "% of terawatt-hours")

# Focus on renewables excluding hydroelectric
place = filter(place, Type == "Solar" | Type == "Wind" | Type == "Other")

ggplot(place, aes(year, elect, fill = Type)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_x_continuous(limits = c(1985,2020)) +
  #xlim(1985,2020)??
  scale_fill_manual(values = c("#0060FA", "#141BDB", "#00296B")) +
  facet_wrap(~ tag, nrow = 2)

place = main[c("year","wind_electricity", "solar_electricity", 
               "other_renewable_electricity", "electricity_generation")] %>%
  mutate_all(~replace_na(.,0)) %>%
  cbind(., tag = main$tag) %>%
  group_by(year, tag) %>%
  filter(tag == "sub_african" & (year == 2014 )) %>%
  summarize(Wind = sum(wind_electricity)/sum(electricity_generation),
            Solar = sum(solar_electricity)/sum(electricity_generation),
            Other = sum(other_renewable_electricity)/sum(electricity_generation))

# Why does aub africa have that behaviour from 2014 to 2015? (EVENTUALMENTE DA CONTROLLARE)
# Electricty generation per capita divided by macroregion
place = main[c("year","electricity_generation", "population")] %>%
  mutate_all(~replace_na(.,0)) %>%
  cbind(., tag = main$tag) %>%
  group_by(tag,year) %>%
  summarize(gen_per_capita = sum(electricity_generation)/sum(population))
place$tag = factor(place$tag, levels = c("developed", "east_europe",
                                         "middle_east", "asian", "latin",
                                         "sub_african"))

# DA MODIFICARE METTENDO I LABEL GIUSTI ED ORDINATI
ggplot(place, aes(year, gen_per_capita, color = tag)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1985,2020), breaks = seq(1985, 2020, by = 5)) +
  scale_color_manual(values = c("#EA5555", "#F39C3C", "#ECD03F", "#6EB35E", "#4996C8",
                                "#774ED8"))

#----
# 1.9. (electricity from LC and renewables in 2020 / electricity generation in
#       2020) VS (same in 2010) for countries w/> 500.000 population
#----
# Objective: look at clusters

place = filter(main, ((year == 2010 | year == 2020) & population > 500000)) %>%
  select(year, low_carbon_electricity, renewables_electricity,
         electricity_generation, tag, iso_code) 
place = filter(place, iso_code != names(which(table(place$iso_code) == 1))[1],
           iso_code != names(which(table(place$iso_code) == 1))[2],
           iso_code != names(which(table(place$iso_code) == 1))[3])
# To remove countries <= 500k pop
  for(i in 1:length(place$year)){
    if(place$year[i] == "2020"){
      place$year[i] = "post"
    }
    else{
      place$year[i] = "pre"
    }
  }
place = gather(place, key = type, value = var, -tag, -iso_code, -year) %>%
  spread(key = year, value = var)
i = 1
while(i < nrow(place)){
  place[i+1,4] = place[i+1,4]*100 / place[i,4]
  place[i+1,5] = place[i+1,5]*100 / place[i,5]
  place[i+2,4] = place[i+2,4]*100 / place[i,4]
  place[i+2,5] = place[i+2,5]*100 / place[i,5]
  i = i+3
}
place = filter(place, type != "electricity_generation")

# w/low carbon
ggplot(filter(place, type == "low_carbon_electricity"),
       aes(pre, post, col = tag)) +
  #geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = iso_code))

# Filter, selecting the most consuming countries
vec = c("USA", "CHN", "IND", "	RUS", "	DEU", "CAN", "BRA", "JPN", "KOR")
place = filter(place, iso_code %in% vec)
ggplot(filter(place, type == "low_carbon_electricity"),
       aes(pre, post, col = tag)) +
  #geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = iso_code))

#----
# 1.10. Which are the countries nearer the full renewable/LC target?
#       Plot electricity from LC and renewables / electricity demand (y) VS
#       electricity consumption per capita (x) for countries w/> 500.000 population
#       Why also per capita: because we expect the countries with lower x
#       to 1) have an higher renewables % and 2) not continue this trend in
#       the future or making it more difficult.
#       We also scale x (sqrt and then from 0 to 1)
#
place = filter(main, (year == 2020 & population > 500000 & iso_code != "REU")) %>%
  transform(elec_demand_per_capita = (sqrt(electricity_demand / population) - min(sqrt(electricity_demand / population))) / (max(sqrt(electricity_demand / population)) - min(sqrt(electricity_demand / population))),
         green_score_renew = renewables_electricity / electricity_demand,
         green_score_lc = low_carbon_electricity / electricity_demand) %>%
  select(iso_code, elec_demand_per_capita, green_score_renew, green_score_lc)

# W.r.t LC
ggplot()+
  geom_point(data = filter(place,
                           iso_code != "LAO",
                           iso_code != "BTN",
                           iso_code != "PRY",
                           iso_code != "NOR"),
             mapping = aes(elec_demand_per_capita, green_score_lc,
                           color = green_score_lc)) +
  geom_text(data = filter(place,
                          iso_code == "LAO" |
                            iso_code == "BTN" |
                            iso_code == "PRY" |
                            iso_code == "NOR"),
            mapping = aes(elec_demand_per_capita, green_score_lc,
                          label = iso_code,
                          color = green_score_lc)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_color_gradient(low = "#ECD03F", high = "#59E80C", na.value = "#59E80C",
                       limits = c(0,1),
                       guide = "none") +
  labs(title = "Green score for world countries, 2020",
       x = "Scaled electricity demand per capita",
       y = "Green score")

# How can Paraguay, Bhutan and Laos achieve such an high score? What's the ren.
#  mix of Norway?
place = filter(main, (year == 2020 & (iso_code == "LAO" | iso_code == "BTN" |
                                        iso_code == "PRY" | iso_code == "NOR"))) %>%
  select(iso_code, solar_share_elec, wind_share_elec, hydro_share_elec,
         nuclear_share_elec, other_renewables_share_elec) %>%
  gather(key = "Source", value = "value", -iso_code)

place[place$Source  == "nuclear_share_elec", "Source"] = "Nuclear"
place[place$Source  == "hydro_share_elec", "Source"] = "Hydro"
place[place$Source  == "solar_share_elec", "Source"] = "Solar"
place[place$Source  == "wind_share_elec", "Source"] = "Wind"
place[place$Source  == "other_renewables_share_elec", "Source"] = "Other"

place$Source = factor(place$Source, levels = c("Nuclear", "Hydro", "Solar", "Wind",
                                               "Other"))

ggplot(place, aes(x = iso_code, y = value, fill = Source)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.6, colour = "white") +
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  labs(title = "Electricity generation mix in selected countries, 2020",
       x = "",
       y = "% gen. from LC sources")


# Study if there's a correlation between the usage of some low carbon and the
# green score

place = filter(main, year == 2020) %>%
  transform(green_score_lc = (low_carbon_electricity / electricity_demand)) %>%
  select(iso_code, green_score_lc, nuclear_share_elec, hydro_share_elec,
         solar_share_elec, wind_share_elec, other_renewables_share_elec,
         green_score_lc) %>%
  filter(complete.cases(.)) %>%
  gather(key = "Source", value = "value", -iso_code, -green_score_lc) %>%
  filter(value > 1)

place[place$Source  == "nuclear_share_elec", "Source"] = "Nuclear"
place[place$Source  == "hydro_share_elec", "Source"] = "Hydro"
place[place$Source  == "solar_share_elec", "Source"] = "Solar"
place[place$Source  == "wind_share_elec", "Source"] = "Wind"
place[place$Source  == "other_renewables_share_elec", "Source"] = "Other"

place$Source = factor(place$Source, levels = c("Nuclear", "Hydro", "Solar", "Wind",
                                               "Other"))

# Nuclear share
# Exclude countries without nuclear electricity production
# Exclude outliers
ggplot(filter(place, iso_code != "LAO", iso_code != "BTN", iso_code != "PRY"),
       aes(value, green_score_lc, color = value)) +
  geom_point() +
  geom_smooth(method = "loess", span = 2, se = FALSE, size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_color_gradient(low = "grey30", high = "#59E80C", na.value = "#59E80C",
                       limits = c(0,40),
                       guide = "none") +
  facet_wrap(~Source, nrow = 2) +
  labs(title = "Green score VS LC generation by source, 2020",
       x = "% gen. from LC source",
       y = "Green score")








# Boxplot of the green scores, separated by area (not showing strong outliers)
place = filter(main, year == 2020) %>%
  transform(green_score_lc = (low_carbon_electricity / electricity_demand)) %>%
  select(iso_code, tag, green_score_lc)

ggplot(place, aes(x = tag, y = green_score_lc)) +
  geom_boxplot(aes(fill = tag)) +
  ylim(0,1.25) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red")



# Barplot which shows the green score (y) with bars stacked on the base of
#    source for each country, divided by area (pop > 500 000)
place = filter(main, year == 2020, !is.na(other_renewables_share_elec),
               population >= 500000) %>%
  transform(ratio = (low_carbon_electricity / (electricity_demand * low_carbon_share_elec))) %>%
  transform(green_solar = ratio * solar_share_elec,
            green_wind = ratio * wind_share_elec,
            green_hydro = ratio * hydro_share_elec,
            green_nuclear = ratio * nuclear_share_elec,
            green_other = ratio * other_renewables_share_elec) %>%
  select(iso_code, tag, green_solar, green_wind,
         green_hydro, green_nuclear, green_other) %>%
  # There are NaN values obtain because of division by zero. We want them to be 0
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .))) %>%
  # There are NA values. We want to remove them
  gather(key = "Source", value = "value", -iso_code, -tag)

place[place$Source  == "green_nuclear", "Source"] = "Nuclear"
place[place$Source  == "green_hydro", "Source"] = "Hydro"
place[place$Source  == "green_solar", "Source"] = "Solar"
place[place$Source  == "green_wind", "Source"] = "Wind"
place[place$Source  == "green_other", "Source"] = "Other"

place$Source = factor(place$Source, levels = c("Nuclear", "Hydro", "Solar", "Wind",
                                               "Other"))

# Developed
ggplot(filter(place, tag == "developed"),
       aes(x = reorder(iso_code, value), y = value, fill = Source)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.6, colour = "white") +
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Green score in developed countries, by source, 2020",
       x = "",
       y = "Green score")

# Eastern europe
ggplot(filter(place, tag == "east_europe"),
       aes(x = reorder(iso_code, value), y = value, fill = Source)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.6, colour = "white") +
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Green score in Eastern European countries, by source, 2020",
       x = "",
       y = "Green score")

# Latin
ggplot(filter(place, tag == "latin", iso_code != "PRY"),
       aes(x = reorder(iso_code, value), y = value, fill = Source)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.6, colour = "white") +
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Green score in Latin America & Caribbean, by source, 2020",
       subtitle = "Note: Paraguay removed being an outlier",
       x = "",
       y = "Green score")

# Asian
ggplot(filter(place, tag == "asian", iso_code != "LAO", iso_code != "BTN"),
       aes(x = reorder(iso_code, value), y = value, fill = Source)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.6, colour = "white") +
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Green score in Asia, by source, 2020",
       subtitle = "Note: Laos and Bhutan removed being outliers",
       x = "",
       y = "Green score")

# Sub-saharian
ggplot(filter(place, tag == "sub_african"),
       aes(x = reorder(iso_code, value), y = value, fill = Source)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.6, colour = "white") +
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Green score in Sub-Saharan countries, by source, 2020",
       x = "",
       y = "Green score")


# Middle-east
ggplot(filter(place, tag == "middle_east"),
       aes(x = reorder(iso_code, value), y = value, fill = Source)) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.6, colour = "white") +
  scale_fill_manual(values = c("#B2FF00", "#05B6FF", "#0060FA", "#141BDB", "#00296B")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Green score in Middle East & North Africa, by source, 2020",
       x = "",
       y = "Green score")



#----


#### Cleaning, Selecting and Preparing Data for Modelling ####
#keep only years between 2000 and 2019
prova = main[main$year== 2000| main$year==2016,] %>%
  #substitute NA with 0 when it refers to that, remove columns missing data for non top countries
  mutate(oil_reserves_2012 = coalesce(oil_reserves_2012, 0),
         uranium_reserves_2019 = coalesce(uranium_reserves_2019, 0),
         gas_reserves = coalesce(gas_reserves, 0),
         coal_reserves_2021 = coalesce(coal_reserves_2021, 0))
#remove Countries that have all important fields NA or 0
prova = prova[(prova$country != "Antarctica") &
                (prova$country != "Netherlands Antilles") &
                (prova$country != "Gibraltar") &
                (prova$country != "Western Sahara") &
                (prova$country != "Niue") &
                (prova$country != "Saint Helena") &
                (prova$country != "Bermuda") &
                (prova$country != "French Guiana") &
                (prova$country != "Guadeloupe") &
                (prova$country != "Micronesia (country)") &
                (prova$country != "Northern Mariana Islands") &
                (prova$country != "Reunion") &
                (prova$country != "Tuvalu") &
                (prova$country != "Martinique") &
                (prova$country != "British Virgin Islands") &
                (prova$country != "Cook Islands") &
                (prova$country != "Faeroe Islands") &
                (prova$country != "Falkland Islands") &
                (prova$country != "Montserrat") &
                (prova$country != "New Caledonia") &
                (prova$country != "Saint Pierre and Miquelon") &
                (prova$country != "Aruba") &
                (prova$country != "Cayman Islands") &
                (prova$country != "French Polynesia") &
                (prova$country != "Turks and Caicos Islands") &
                (prova$country != "American Samoa") &
                (prova$country != "Greenland") &
                (prova$country != "Guam") &
                (prova$country != "Nauru") &
                (prova$country != "United States Virgin Islands")
              ,]
# Palestine missing data in 2000,
# productions missing 2017 to 2020,
# some energy_cons_change_twh missing can throw to 0,
# some primary_energy_consumption missing, can copy from other year maybe?,
# agri_land_rate missing for 2019 can copy 2018,
# missing data for taiwan, north korea, macao

#pro capite (million) transformation
cols_procapite <- c(6,7,9,10,11,12,14,15,17,18,20,22,23,25,26,28,29,31,33,35,37,40,42,43,44,45,46)
prova <- prova %>% mutate(across(all_of(cols_procapite), .fns = ~.*1000000/population))
cols_log <- c(4, 37, 40, 42, 43, 44, 45, 46)
prova <- prova %>% mutate(across(all_of(cols_log), .fns = ~ log(.+1)))
#find na values for data cleaning
#colSums(is.na(prova))/3760*100

#prova[is.na(prova$energy_cons_change_twh),]$energy_consumption

#View(prova[is.na(prova$coal_share_elec),c("country","year","coal_share_elec")])
#View(prova[!(is.na(prova$coal_share_elec)|prova$coal_share_elec==0),c("country","year","coal_share_elec")])

#count NA by year

#nacount = prova %>%
#  group_by(year) %>%
#  summarize(na_perc = sum(is.na(particulate_pollution))/n())
#plot(nacount)
#colnames(prova)





        
       






#correlation plot
cor_prova = cor(prova[,4:46], use="pairwise.complete.obs")
corrplot(cor_prova, method="color", tl.cex = .2)

summary(prova)
colnames(prova)
p=prova
#filter p by tag
#p=p[p$tag=="developed",]
p=p[complete.cases(p),]


#normalizing columns
normalize <- function(x) {
  return((x- min(x)) /(max(x)-min(x)))
}

normcols=c(4:46)
# year-wise normalization
#for (i in unique(p$year)){
#  p[p$year==i,normcols] <- lapply(p[p$year==i,normcols], normalize)
#}
#overall normalization
p[normcols] <- lapply(p[normcols], normalize)

#normalize year
p["year"] <- lapply(p["year"], normalize)

#### chosing dep and indep variables without using tag as dummy variable ####
objective = "carbon_intensity_elec"
dependent = colnames(p)[c(2,4,37,38,39,41,42,43,44,45,46)]

#### Stepwise LM ####
#custom formula
#my_formula <- as.formula(
#  paste(paste(objective), " ~ ", paste(dependent, collapse = " + ")))
my_formula <- as.formula(
  paste("qlogis((", paste(objective), "/100.001)+0.000005) ~ ", paste(dependent, collapse = " + ")))
#my_formula <- as.formula(
#  paste(paste(objective), " ~ ", paste(dependent, collapse = " + "), " + I(",  paste(dependent, collapse = "^2) + I("), "^2)"))
#my_formula <- as.formula(
#  paste("qlogis((", paste(objective), "/100.001)+0.000005) ~ ", paste(dependent, collapse = " + "), " + I(",  paste(dependent, collapse = "^2) + I("), "^2)"))
my_formula


#creating model
model <- lm(my_formula, p)
summary(model)
step_model=step(model,direction=c("both"), trace=FALSE, k=log(nrow(p)))
summary(step_model)
#visualization of residuals
step_resid=resid(step_model)
plot(p$gdp, step_resid, 
     ylab="Residuals", 
     main="Residuals plot") 
abline(0, 0)

#### Setup for Lasso and Ridge ####
#y = p[,objective]
y = qlogis((p[,objective]/100.001)+0.000005)
x = data.matrix(select(p,dependent))
#x = cbind(data.matrix(select(p,dependent)),data.matrix(select(p,dependent)^2))
#colnames(x)<-c(colnames(select(p,dependent)),paste(colnames(select(p,dependent)),"^2"))

#### Lasso ####
lasso <- glmnet(x, y, alpha = 1)
plot(lasso, xvar = "lambda", label=TRUE)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

#analyze best model
best_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_lasso)
print(best_lasso)

#### Ridge ####
ridge <- glmnet(x, y, alpha = 0)
plot(ridge, xvar="dev", label=TRUE)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

#analyze best model
best_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_ridge)
print(best_ridge)



#### chosing indep variables using tag as dummy variable ####
dependent = colnames(p)[c(2,4,37,38,39,41,42,43,44,45,46,47)]

#### Stepwise LM ####
#custom formula
#my_formula <- as.formula(
#  paste(paste(objective), " ~ ", paste(dependent, collapse = " + ")))
my_formula <- as.formula(
  paste("qlogis((", paste(objective), "/100.001)+0.000005) ~ ", paste(dependent, collapse = " + ")))
#my_formula <- as.formula(
#  paste(paste(objective), " ~ ", paste(dependent, collapse = " + "), " + I(",  paste(dependent, collapse = "^2) + I("), "^2)"))
#my_formula <- as.formula(
#  paste("qlogis((", paste(objective), "/100.001)+0.000005) ~ ", paste(dependent, collapse = " + "), " + I(",  paste(dependent, collapse = "^2) + I("), "^2)"))
my_formula


#creating model
model <- lm(my_formula, p)
summary(model)
step_model=step(model,direction=c("both"), trace=FALSE, k=log(nrow(p)))
summary(step_model)
#visualization of residuals
step_resid=resid(step_model)
plot(p$population, step_resid, 
     ylab="Residuals", 
     main="Residuals plot") 
abline(0, 0)

#### Setup for Lasso and Ridge ####
#y = p[,objective]
y = qlogis((p[,objective]/100.001)+0.000005)
x = data.matrix(select(p,dependent))
#x = cbind(data.matrix(select(p,dependent)),data.matrix(select(p,dependent)^2))
#colnames(x)<-c(colnames(select(p,dependent)),paste(colnames(select(p,dependent)),"^2"))

#### Lasso ####
lasso <- glmnet(x, y, alpha = 1)
plot(lasso, xvar = "lambda", label=TRUE)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

#analyze best model
best_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_lasso)
print(best_lasso)

#### Ridge ####
ridge <- glmnet(x, y, alpha = 0)
plot(ridge, xvar="dev", label=TRUE)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

#analyze best model
best_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_ridge)
print(best_ridge)
