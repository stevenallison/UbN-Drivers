
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(gridExtra)
library(gtable)
library(grid)
library(ggh4x)
library(stringr)

GDP <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "GDP") %>%
  pivot_longer(cols = !Year, names_to = "Variable")

Econ <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Credit ag") %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  merge(GDP, all=T)

Population <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Population") %>%
  mutate(value = value/1e6) %>%
  mutate(Variable = factor(Variable,levels = c("Total","Urban","Rural")))

Cropland <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Crop expansion") %>%
  select(-Mha) %>%
  rename("Cropland (Mha)" = "Cropland area") %>%
  pivot_longer(cols = !Year, names_to = "Variable")

Farm.Clearing <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Farm clearing") %>%
  select("Year", "Forest cleared") %>%
  rename("Trees cut (billion board ft)" = "Forest cleared") %>%
  pivot_longer(cols = !Year, names_to = "Variable")

Ag <- merge(Cropland, Farm.Clearing, all = T)

Land.cover <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Land cover") %>%
  select(Year,
         "Primary forest (Mha)" = land_cover_primf,
         "Secondary forest (Mha)" = land_cover_secdf,
         "Urban (Mha)" = land_cover_urban,
         "Pasture/Rangeland (Mha)" = land_cover_Graze_Pasture,
         "Cropland LUH2 (Mha)" = land_cover_Crops) %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  mutate(value = value/10000) %>%
  filter(Year > 1799) %>%
  merge(Ag, all=T)

Timber <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Timber") %>%
  mutate(value = case_when(Source == "Howard and Liang 2019" ~ value*12,
                           T~value)) %>%
  mutate(Variable = factor(Variable,levels = c("Fuelwood","Industrial wood","Lumber","Other products")))

Wood.Grazing <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Grazing") %>%
  select(Variable, Year, value) %>%
  merge(Timber %>%
          filter(Source == "Magerl et al. 2022") %>%
          select(Variable, Year, value), all=T) %>%
  mutate(Variable = factor(Variable,
                           levels = c("Fuelwood","Industrial wood","Forest grazing","Pasture grazing")))

Aqua <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Aquaculture") %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  mutate(Variable = factor(Variable,
                           levels = c("Molluscs", "Diadromous fishes", "Marine fishes"))) %>%
  na.omit()

Coal <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Coal production") %>%
  pivot_longer(cols = !Year, names_to = "Variable")

Fossil <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Oil&Gas") %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  merge(Coal, all=T)

Invasives <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Invasives") %>%
  select(!"Species first recorded per 5 years") %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  na.omit() %>%
  mutate(Variable = factor(Variable, levels = c("Vascular plants (United States)",
                                                "Vascular plants (Hawaiian Islands)",
                                                "Aquatic plants (United States)",
                                                "Aquatic animals (United States)")))

Nitrogen <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Fertilizer nitrogen") %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  mutate(Source = "FAO") %>%
  mutate(value = value/1e6)

Phosphorus <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Fertilizer phosphorus") %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  mutate(Source = "FAO") %>%
  mutate(value = value/1e6)

Plastic <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Plastic") %>%
  select(Year, "Plastic pollution") %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  mutate(Source = "Kan") %>%
  mutate(value = value/1000)

Pollution <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Pesticides") %>%
  mutate(value = value/1e6) %>%
  merge(Nitrogen, all=T) %>%
  merge(Phosphorus, all=T) %>%
  merge(Plastic, all=T) %>%
  mutate(Source = factor(Source, levels = c("FAO", "USDA", "Kan"))) %>%
  mutate(Variable = factor(Variable, levels = c("Nitrogen fertilizer",
                                                "Phosphate fertilizer",
                                                "Herbicides",
                                                "Insecticides",
                                                "Fungicides",
                                                "Plastic pollution")))
                                                

Pop.plot <- ggplot(Population, aes(x=Year, y=value, color = Variable)) +
  ylab("Population (millions)") +
  geom_line() +
  theme_linedraw() +
  theme(legend.title = element_blank()) +
  labs(title = "a)")

Econ.plot <- ggplot(Econ, aes(x=Year, y=value, color = Variable)) +
  xlim(1945, 2025) +
  ylab("US Dollars") +
  geom_line() +
  theme_linedraw() +
  theme(legend.title = element_blank()) +
  labs(title = "b)")

grob.Econ <- ggplotGrob(Econ.plot)
grob.Pop <- ggplotGrob(Pop.plot)
grob.PopEcon <- rbind(grob.Pop, grob.Econ, size = "first")
grob.PopEcon$widths <- unit.pmax(grob.Econ$widths, grob.Pop$widths)
pdf("Population.pdf", width = 9, height = 5)
grid.draw(grob.PopEcon)
dev.off()

land_labels <- Land.cover %>%
  group_by(Variable) %>%
  summarize(y = 0.93*(max(value) - min(value)) + min(value)) %>%
  mutate(x = 1638) %>%
  mutate(panel_text = c("a)", "b)", "c)", "d)", "e)", "f)", "g)"))

Land.plot <- ggplot(Land.cover, aes(x=Year, y=value)) +
  geom_line() +
  theme_linedraw() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(y = NULL) +
  facet_wrap(~Variable, scales="free_y", ncol=1, strip.position="left",
             labeller = label_wrap_gen(width = 19)) +
  geom_label(aes(x = x, y = y, label = panel_text), data = land_labels) +
  scale_x_continuous(expand = c(0.01, 0.01))

pdf("Landcover.pdf", width = 9, height = 9)
Land.plot
dev.off()

Aqua.plot <- ggplot(Aqua, aes(x=Year, y=value, color = Variable)) +
  labs(y = str_wrap("Production (tonnes)", width = 20)) +
  geom_line() +
  theme_linedraw() +
  theme(legend.title = element_blank()) +
  labs(title = "a)")

Timber.plot <- ggplot(Timber, aes(x=Year, y=value, color = Variable)) +
  xlim(1630, 2020) +
  labs(y = str_wrap("Timber extracted (billion board feet)", width = 20)) +
  geom_line(data = Timber %>% filter(Source != "Magerl et al. 2022")) +
  theme_linedraw() +
  theme(legend.title = element_blank()) +
  labs(title = "b)")

Wood.Grazing.plot <- ggplot(Wood.Grazing, aes(x=Year, y=value, color = Variable)) +
  xlim(1630, 2020) +
  ylim(0, 150) +
  labs(y = str_wrap("Extracted production (Tg C per year)", width = 20)) +
  geom_line() +
  theme_linedraw() +
  theme(legend.title = element_blank()) +
  labs(title = "c)")

grob.Timber <- ggplotGrob(Timber.plot)
grob.Wood.Grazing <- ggplotGrob(Wood.Grazing.plot)
grob.Aqua <- ggplotGrob(Aqua.plot)
grob.Harvest <- rbind(grob.Aqua, grob.Timber, grob.Wood.Grazing, size = "first")
grob.Harvest$widths <- unit.pmax(grob.Timber$widths, grob.Wood.Grazing$widths, grob.Aqua$widths)
pdf("Harvest.pdf", width = 9, height = 5)
grid.draw(grob.Harvest)
dev.off()

fossil_labels <- Fossil %>%
  group_by(Variable) %>%
  summarize(y = 0.93*(max(value) - min(value)) + min(value)) %>%
  mutate(x = 1950) %>%
  mutate(panel_text = c("a)", "b)", "c)", "d)"))

Fossil.plot <- ggplot(Fossil, aes(x=Year, y=value)) +
  geom_line() +
  theme_linedraw() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(y = NULL) +
  facet_wrap(~Variable, scales="free_y", ncol=1, strip.position="left",
             labeller = label_wrap_gen(width = 30)) +
  geom_label(aes(x = x, y = y, label = panel_text), data = fossil_labels) +
  scale_x_continuous(expand = c(0.01, 0.01))

pdf("Fossil.pdf", width = 9, height = 7)
Fossil.plot
dev.off()

invasives_labels <- Invasives %>%
  group_by(Variable) %>%
  summarize(y = 0.93*(max(value) - min(value)) + min(value)) %>%
  mutate(x = 1803) %>%
  mutate(panel_text = c("a)", "b)", "c)", "d)"))

Invasives.plot <- ggplot(Invasives, aes(x=Year, y=value)) +
  geom_line() +
  theme_linedraw() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(y = "Species first recorded per 5 years") +
  facet_wrap(~Variable, scales="free_y", ncol=1, strip.position="left",
             labeller = label_wrap_gen(width = 20)) +
  geom_label(aes(x = x, y = y, label = panel_text), data = invasives_labels) +
  scale_x_continuous(expand = c(0.01, 0.01))

pdf("Invasives.pdf", width = 9, height = 7)
Invasives.plot
dev.off()

pollution_labels <- Pollution %>%
  group_by(Variable) %>%
  summarize(y = 0.9*(max(value) - min(value)) + min(value)) %>%
  mutate(x = 1951) %>%
  mutate(Source = NA) %>%
  mutate(panel_text = c("a)", "b)", "c)", "d)", "e)", "f)"))

Pollution.plot <- ggplot(Pollution, aes(x=Year, y=value, color=Source)) +
  scale_color_manual(values = c("black", "red", "blue")) +
  geom_line() +
  theme_linedraw() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(y = "Pollution source (million tonnes)") +
  labs(color = "Data from") +
  facet_wrap(~Variable, scales="free_y", ncol=1, strip.position="left",
             labeller = label_wrap_gen(width = 15)) +
  geom_label(aes(x = x, y = y, label = panel_text), data = pollution_labels, color = "black") +
  scale_x_continuous(expand = c(0.01, 0.01))

pdf("Pollution.pdf", width = 9, height = 7)
Pollution.plot
dev.off()

