library(tidyverse)
library(lubridate)
library(grid)
library(rworldmap)
library(mapproj)
library(patchwork)
library(devEMF)

reg = data.frame(geo=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","UK","LI","IS","NO"),ctry=c("AUT","BEL","BLG","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR","LIE","ISL","NOR"),country=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Rep.","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom","Lichenstein","Iceland","Norway"),regija=c("Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","HR","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","Zemlje SIE","Ostale zemlje EU-a","Zemlje SIE","Zemlje SIE","Zemlje SIE","Ostale zemlje EU-a","Ostale zemlje EU-a","Ostale zemlje EU-a","EEA","EEA","EEA"))
# Tema
gtema <- theme_minimal() + theme(panel.background = element_rect(fill="white",linetype = 0),plot.background = element_rect(fill="white",linetype = 0),legend.box.background = element_rect(fill="white",linetype = 0),text = element_text(colour = "#000000"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(gtema)

# Paleta boja
boje_fill <- scale_fill_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))
boje_col <- scale_color_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))

# 1. Primjer - ESRB Warningsi ####

# Get the world map
worldMap <- getMap()
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME %in% reg$country) 
# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})
europeCoords <- do.call("rbind", europeCoords)
# podaci
warnings <- data.frame(
  country = c("United Kingdom", "Sweden", "Netherlands", "Luxembourg", "Finland", "Denmark", "Austria",
              "Norway", "Iceland", "France", "Germany", "Czech Republic",
              "Slovakia", "Liechtenstein", "Hungary", "Croatia", "Bulgaria"),
  year = c(rep("2016", 7), rep("2019", 5), rep("2021", 5))
)
warnings <- warnings %>% full_join(reg,by="country")
# crtanje grafikona
europeanUnionTable <- data.frame(country = warnings$country, value = warnings$year)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]
# grafikon
emf(file = "01_warnings_mapa.emf",width = 6,height = 6)
ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value), colour = "black", size = 0.1) + theme(panel.grid.minor = element_line(colour = NA), panel.background = element_rect(fill = NA, colour = NA), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank(), legend.position = "right") + boje_fill + plot_annotation(title=str_wrap("ESRB Warnings related to medium-term vulnerabilities in RRE sector",80),caption = "Sources: ESRB and Hanfa")
dev.off()
rm(worldMap,europeanUnionTable,europeCoords,indEU)


# 2. Primjer - Zemlje koje imaju FSR ####

# Get the world map
worldMap <- getMap()
# Select only the index of states member of the E.U.
indEU <- 1:243
# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})
europeCoords <- do.call("rbind", europeCoords)
# podaci
fsrs <- data.frame(
  country = c("Australia", "Austria", "Belgium", "Canada", "Cyprus","Croatia", "Denmark", "European Central Bank", "European Central Bank", "Finland", "France", "Germany", "Greece", "Iceland", "Ireland", "Italy", "Japan", "Luxembourg", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States", "United States", "Argentina", "Brazil", "Chile", "China", "Colombia", "Czech Republic", "Egypt", "Estonia", "Hong Kong", "Hungary", "India", "Indonesia", "Israel", "Kazakhstan", "Latvia", "Lithuania", "Malaysia", "Mexico", "Morocco", "Nigeria", "Pakistan", "Peru", "Philippines", "Poland", "Romania", "Russia", "Singapore", "Slovakia", "Slovenia", "South Africa", "South Korea", "Taiwan", "Thailand", "Turkey", "Ukraine", "Uruguay"),
  yesno = rep("Yes", 64))

#warnings <- warnings %>% full_join(reg,by="country")
# crtanje grafikona
europeanUnionTable <- data.frame(country = fsrs$country, value = fsrs$yesno)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]
# grafikon
emf(file = "02_fsrs_mapa.emf",width = 8,height = 6)
ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value), colour = "black", size = 0.1) + theme(panel.grid.minor = element_line(colour = NA), panel.background = element_rect(fill = NA, colour = NA), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank(), legend.position = "right") + boje_fill + plot_annotation(title=str_wrap("Countries that regularly publish Financial stability reports",80),caption = "Sources: Center for Financial Stability and Hanfa")
dev.off()
rm(worldMap,europeanUnionTable,europeCoords,indEU)
