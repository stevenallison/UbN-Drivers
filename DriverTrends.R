
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(gridExtra)
library(gtable)
library(grid)

GDP <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "GDP") %>%
  pivot_longer(cols = GDPpc, names_to = "Variable")

Population <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Population") %>%
  mutate(Variable = factor(Variable,levels = c("Total","Urban","Rural")))


GDP.plot <- ggplot(GDP, aes(x=Year, y=value)) +
  xlim(1945, 2025) +
  ylab("GDPpc ($US)") +
  geom_line() +
  theme_linedraw()

Pop.plot <- ggplot(Population, aes(x=Year, y=value, color = Variable)) +
  xlim(1945, 2025) +
  ylim(0,3.5e+08) +
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
