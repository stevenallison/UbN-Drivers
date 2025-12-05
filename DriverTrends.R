
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(gridExtra)
library(gtable)
library(grid)
library(ggh4x)

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
         "Primary forest (?Unit)" = land_cover_primf,
         "Secondary forest (?Unit)" = land_cover_secdf,
         "Urban (?Unit)" = land_cover_urban,
         "Pasture/Rangeland (?Unit)" = land_cover_Graze_Pasture,
         "Cropland (?Unit)" = land_cover_Crops) %>%
  pivot_longer(cols = !Year, names_to = "Variable") %>%
  filter(Year > 1799) %>%
  merge(Ag, all=T)

Timber <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Timber")


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
grid.newpage()
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
  facet_wrap(~Variable, scales="free_y", ncol=1, strip.position="left", labeller = label_wrap_gen(width = 20))

#facet_wrap2(~Variable, scales="free_y", ncol=1, strip.position="left",
#             labeller = as_labeller(Ag.labels)) +
#    facetted_pos_scales(y = list(scale_y_continuous(limits = c(0, 160)),
#                                 scale_y_continuous(limits = c(0, 250))))

pdf("Landcover.pdf", width = 9, height = 9)
Land.plot
dev.off()

