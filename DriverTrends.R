
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


a <- ggplot(GDP, aes(x=Year, y=value)) +
  xlim(1945, 2025) +
  ylab("GDPpc ($US)") +
  geom_line() +
  theme_linedraw()

b <- ggplot(Population, aes(x=Year, y=value, color = Variable)) +
  xlim(1945, 2025) +
  ylim(0,3.5e+08) +
  ylab("Population size") +
  geom_line() +
  theme_linedraw() +
  theme(legend.title = element_blank())

grob.a <- ggplotGrob(a)
grob.b <- ggplotGrob(b)
grob.c <- rbind(grob.a, grob.b, size = "first")
grob.c$widths <- unit.pmax(grob.a$widths, grob.b$widths)
grid.newpage()
pdf("Population.pdf", width = 9, height = 5)
grid.draw(grob.c)
dev.off()

pop <- read.csv("FAOSTAT_data_en_10-23-2025population.csv")
pop <- pop[,which(colnames(pop)%in%c("Year","Element","Value"))]
names(pop) [1] <- "Item"
fert <- read.csv("FAOSTAT_data_en_10-23-2025fertilizers.csv")
fert <- fert[,which(colnames(fert)%in%c("Year","Item","Value"))]
pest <- read.csv("FAOSTAT_data_en_10-23-2025pesticides.csv")
pest <- pest[,which(colnames(pest)%in%c("Year","Item","Value"))]
subs <- read.csv("FAOSTAT_data_en_10-23-2025subsidies.csv")
subs <- subs[,which(colnames(subs)%in%c("Year","Item","Value"))]

df <- rbind(pop,fert,pest,subs,pcgdp)

write.csv(df,"NNAtrends.csv",row.names=F)

png("NNAdrivers.png",width=5,height=7,units="in",res=300)
ggplot(df, aes(x=Year,y=Value)) + geom_line() + facet_wrap(~Item,scales="free_y",ncol=2) + theme_bw()
dev.off()