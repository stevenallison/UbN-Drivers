
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(gridExtra)
library(gtable)

GDP <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "GDP") %>%
  pivot_longer(cols = GDPpc, names_to = "Variable")

Population <- drive_get("Trends in Drivers Data") %>% 
  read_sheet(sheet = "Population")


a <- ggplot(GDP, aes(x=Year, y=value)) +
  geom_line() +
  theme_linedraw()

b <- ggplot(Population, aes(x=Year, y=value)) +
         geom_line() +
         facet_wrap(~Variable, scales="free_y", ncol=1) +
         theme_linedraw()

g <- arrangeGrob(a, b, ncol=1, nrow=2)
ggsave("Population.pdf", g, width = 9, height = 5)

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