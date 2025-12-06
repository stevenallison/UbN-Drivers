
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
  pivot_longer(cols = GDPpc, names_to = "Variable")

Population <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Population") %>%
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
  mutate(Variable = factor(Variable,levels = c("Fuelwood","Industrial wood","Forest grazing","Pasture grazing")))


Coal <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Coal production") %>%
  pivot_longer(cols = !Year, names_to = "Variable")

Fossil <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Oil&Gas") %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  merge(Coal, all=T)

Nitrogen <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Fertilizer nitrogen") %>%
  pivot_longer(cols = !Year, names_to = "Variable")

Phosphorus <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Fertilizer phosphorus") %>%
  pivot_longer(cols = !Year, names_to = "Variable")

Pollution <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Pesticides") %>%
  merge(Nitrogen, all=T) %>%
  merge(Phosphorus, all=T)


GDP.plot <- ggplot(GDP, aes(x=Year, y=value)) +
  xlim(1945, 2025) +
  ylab("GDPpc ($US)") +
  geom_line() +
  theme_linedraw()

Pop.plot <- ggplot(Population, aes(x=Year, y=value, color = Variable)) +
  xlim(1945, 2025) +
  ylim(0, 3.5e+08) +
  ylab("Population size") +
  geom_line() +
  theme_linedraw() +
  theme(legend.title = element_blank())

grob.GDP <- ggplotGrob(GDP.plot)
grob.Pop <- ggplotGrob(Pop.plot)
grob.PopGDP <- rbind(grob.GDP, grob.Pop, size = "first")
grob.PopGDP$widths <- unit.pmax(grob.GDP$widths, grob.Pop$widths)
pdf("Population.pdf", width = 9, height = 5)
grid.draw(grob.PopGDP)
dev.off()

Land.plot <- ggplot(Land.cover, aes(x=Year, y=value)) +
  geom_line() +
  theme_linedraw() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(y = NULL) +
  facet_wrap(~Variable, scales="free_y", ncol=1, strip.position="left",
             labeller = label_wrap_gen(width = 19))

#facet_wrap2(~Variable, scales="free_y", ncol=1, strip.position="left",
#             labeller = as_labeller(Ag.labels)) +
#    facetted_pos_scales(y = list(scale_y_continuous(limits = c(0, 160)),
#                                 scale_y_continuous(limits = c(0, 250))))

pdf("Landcover.pdf", width = 9, height = 9)
Land.plot
dev.off()

Timber.plot <- ggplot(Timber, aes(x=Year, y=value, color = Variable)) +
  xlim(1630, 2020) +
  labs(y = str_wrap("Timber extracted (billion board feet)", width = 20)) +
  geom_line(data = Timber %>% filter(Source != "Magerl et al. 2022")) +
  theme_linedraw() +
  theme(legend.title = element_blank())

Wood.Grazing.plot <- ggplot(Wood.Grazing, aes(x=Year, y=value, color = Variable)) +
  xlim(1630, 2020) +
  ylim(0, 150) +
  labs(y = str_wrap("Extracted production (Tg C per year)", width = 20)) +
  geom_line() +
  theme_linedraw() +
  theme(legend.title = element_blank())

grob.Timber <- ggplotGrob(Timber.plot)
grob.Wood.Grazing <- ggplotGrob(Wood.Grazing.plot)
grob.LandExtract <- rbind(grob.Timber, grob.Wood.Grazing, size = "first")
grob.LandExtract$widths <- unit.pmax(grob.Timber$widths, grob.Wood.Grazing$widths)
pdf("LandExtract.pdf", width = 9, height = 5)
grid.draw(grob.LandExtract)
dev.off()

Fossil.plot <- ggplot(Fossil, aes(x=Year, y=value)) +
  geom_line() +
  theme_linedraw() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(y = NULL) +
  facet_wrap(~Variable, scales="free_y", ncol=1, strip.position="left",
             labeller = label_wrap_gen(width = 30))

pdf("Fossil.pdf", width = 9, height = 7)
Fossil.plot
dev.off()

Pollution.plot <- ggplot(Pollution, aes(x=Year, y=value)) +
  geom_line() +
  theme_linedraw() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(y = NULL) +
  facet_wrap(~Variable, scales="free_y", ncol=1, strip.position="left",
             labeller = label_wrap_gen(width = 20))

pdf("Pollution.pdf", width = 9, height = 7)
Pollution.plot
dev.off()

