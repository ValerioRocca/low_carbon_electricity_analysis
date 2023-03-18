# Libraries
library(dplyr)
library(tidyr)

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
            "oil_prod_change_twh", "other_renewable_electricity",
            "other_renewables_cons_change_pct",
            "other_renewables_cons_change_twh",
            "other_renewables_elec_per_capita",
            "other_renewables_elec_per_capita_exc_biofuel",
            "other_renewables_energy_per_capita", "other_renewables_share_elec",
            "per_capita_electricity","renewables_cons_change_pct",
            "renewables_cons_change_twh", "renewables_elec_per_capita",
            "renewables_energy_per_capita", "solar_cons_change_pct",
            "solar_cons_change_twh", "solar_elec_per_capita",
            "wind_cons_change_pct", "wind_cons_change_twh",
            "solar_energy_per_capita", "wind_elec_per_capita",
            "wind_energy_per_capita", "oil_cons_change_pct",
            "oil_cons_change_twh", "oil_energy_per_capita"))

# Separate biofuel consumption from other renewables; rename the variables

main = mutate(main,
              other_renewable_consumption = other_renewable_consumption - biofuel_consumption,
              other_renewable_electricity = other_renewable_exc_biofuel_electricity,
              other_renewables_share_elec = other_renewables_share_elec_exc_biofuel,
              other_renewables_share_energy = other_renewables_share_energy - biofuel_share_energy)
 
main = select(main, -c("other_renewable_exc_biofuel_electricity",
                                    "other_renewables_share_elec_exc_biofuel"))

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
main = left_join(main,
                 select(oil.proved.reserves.cia, -c("V1")),
                 by = c("country" = "V2"))
rm(oil.proved.reserves.cia)

colnames(uranium.proved.reserves.oecd) = c("V1","V2", "uranium_reserves_2019", "V4")
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
