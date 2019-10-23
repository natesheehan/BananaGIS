library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)
library(sp)
library(magrittr)
library(classInt)
library(leaflet)
# read in the csv
bananaImportCsv <- read_csv("~/University/GIS/project/ukBananaImportTidyData.csv") 
fiveYearBananaCsv <- read_csv("~/University/GIS/project/5YearBananaImport.csv") 

barplot(fiveYearBananaCsv$cars, main="Cars", xlab="Days",  
        ylab="Total", names.arg=c("Mon","Tue","Wed","Thu","Fri"), 
        border="blue", density=c(10,20,30,40,50))


data("World")
fullMap <- merge(World, 
                 bananaImportCsv, 
                 by.x="sovereignt", 
                 by.y="Country")

fiveYearMap <- merge(World, 
                     fiveYearBananaCsv, 
                     by.x="sovereignt", 
                     by.y="Exporters")

install.packages("shinyjs")

colnames(fullMap)[colnames(fullMap)=="Share in United Kingdom's imports (%)"] <- "ukShare"
colnames(fullMap)[colnames(fullMap)=="Growth in imported quantity between 2014-2018 "] <- "ukGrowth"

colours<- brewer.pal(5, "Blues")

breaks<-classIntervals(fullMap$ukShare, 
                       n=5, 
                       style="jenks")

breaks<-classIntervals(fiveYearBananaCsv$imported, 
                       n=5, 
                       style="jenks")

graphics::plot(breaks, 
               pal=colours)

summary(breaks)
breaks <- as.numeric(breaks$brks)
#create a new sp object from the earlier sf object 
#with all of our data in THEN Transform it to WGS84 
#THEN convert it to SP.  

fullMapSP <- fullMap %>%
  st_transform(crs = 4326) %>%
  as("Spatial")

#colour palette using colorBin colour mapping
pal <- colorBin(palette = "RdYlBu", 
                domain = fullMapSP$ukShare,
                #create bins using the breaks object from earlier
                bins = breaks)
# now, add some polygons colour them using your colour palette,
#overlay the, on top of a nice backdrop and add a legend. 
#Note the use of the magrittr pipe operator (%>%)
#check the documentation to understand how this is working…
googleMaps = paste("https://earth.google.com/web/",fullMapSP$name,sep="")


leaflet(fullMapSP) %>%
  addPolygons(stroke = FALSE, 
              fillOpacity = 0.5, 
              smoothFactor = 1,
              color = ~pal(ukShare),
              popup = paste("Country:", fullMapSP$name, "<br>",
                            #   "Visit:", paste("<a href='https://earth.google.com/web/",fullMapSP$name)'"',sep="","<br>",
                            #   "<a href='https://earth.google.com/web/'>Visit</a><br>",
                            "Share of UK imports:", fullMapSP$ukShare, "%", "<br>",
                            "Quantity Exported: ", fullMapSP$Quantity.imported.in.2018, " tons", "<br>",
                            "Value of imported bananas:",'$', fullMapSP$Value.imported.in.2018..USD.thousand.,"0000","<br>",
                            "5 Year growth in exported value: ", fullMapSP$Growth.in.imported.quantity.between.2014.2018.....p.a.., "%", "<br",
                            "Avg distance to the UK: ", fullMapSP$Average.distance.between.partner.countries.and.all.their.importing.markets..km., "km","<br>" )
  ) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addLegend("topright", 
            pal= pal, 
            values = ~ukShare, 
            title = "2018 UK  Banana Importation Share", 
            labFormat = labelFormat(prefix = "%"),
            opacity = 1
  )     



