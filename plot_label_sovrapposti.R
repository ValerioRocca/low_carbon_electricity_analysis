# a. Share of electricity generation (2020 vs 2010)
# Creation of the dataset
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
place[place$Source  == "renewables_electricity", "Source"] = "Ren."

# Creation of the plot
gg1 = ggplot(place, aes(ten, twenty, col = Source)) +
  geom_point(size = 3) +
  geom_line(aes(group = tag), color = "black", linetype = "dashed") +
  geom_text(aes(label = tag), position = position_jitter(width = 0.5, height = 0.5)) +
  scale_color_manual(values = c("#59E80C", "#141BDB")) +
  labs(title = "Share of electricity generation from LC and renewable sources",
       subtitle = "2010 vs 2020, grouped by macroregion",
       x = "Share of electricity generation 2010",
       y = "Share of elec. gen. 2020") +
  xlim(0,70) +
  ylim(0,70) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_abline(intercept = 0, slope = 1.5, linetype = "dashed", color = "red")