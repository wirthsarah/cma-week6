library("readr")
library("sf")
library("dplyr")
library("tmap")
library("ggplot2")
library("terra")
library("lubridate")

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",") |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

# Task 1 ----
fanel <-read_sf(dsn="Feldaufnahmen_Fanel.gpkg")
  #What information does the dataset contain? Information about the dimensions of fields and the fruit that is cultivated on it
  #What is the geometry type of the dataset (possible types are: Point, Lines and Polygons)? Polygons
  #What are the data types of the other columns? numeric for the FieldID and character for Frucht
  #What is the coordinate system of the dataset? CH1903+ LV95/ EPSG 2056
  
# Task 2 ----
wildschwein_sommer <- wildschwein_BE |> filter(DatetimeUTC>="2015-05-01 02:00:00",DatetimeUTC<"2015-07-01 02:00:00")

tmap_mode("plot")

tm_shape(fanel)+
  tm_polygons(col="Frucht", alpha=0.6, border.col = "black")+
  tm_layout(" ",
            legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)+
  tm_shape(wildschwein_sommer)+
  tm_dots(col="TierName", border.col="black")


wildschwein_sommer_Feld <-st_join(wildschwein_sommer, fanel, left=TRUE)

tm_shape(wildschwein_sommer_Feld)+
  tm_dots(col="Frucht", size=0.3)+
  tm_shape(wildschwein_sommer_Feld)+
  tm_dots(col="TierName", size=0.05)
          
#findings: each individual has more or less its own region. Rosa spends some time in the forest but also lots of time on the fields. Ruth spends less time in the fields and more in the forest. Sabi rather rarely leaves the forest.

# Task 3 ----
wildschwein_sommer_Feld$hour <- hour(wildschwein_sommer_Feld$DatetimeUTC)

wildschwein_summary <-wildschwein_sommer_Feld |> count(TierName,hour,Frucht)
max<-wildschwein_sommer_Feld |> count(Frucht)#Wald, Feuchtgebiet, Gerste, NA, Rueben


wildschwein_summary$Frucht[!(wildschwein_summary$Frucht %in% c("Wald","Feuchtgebiet","Gerste","Rueben"))]<-"other"

ggplot(data=wildschwein_summary, aes(x=hour, fill=factor(Frucht,levels=c("Rueben","Gerste","other","Feuchtgebiet","Wald"))))+
  geom_bar(position="fill")+
  facet_wrap(~TierName)+
  labs(fill="area")+
  scale_y_continuous("Percentage", labels= scales::percent)+
  xlab("Time of day in hours")


ggplot(data=filter(wildschwein_summary,TierName=="Ruth"), aes(x=hour, fill=factor(Frucht,levels=c("Rueben","Gerste","other","Feuchtgebiet","Wald"))))+
  geom_bar(position="fill")+
 coord_polar()+
  labs(fill="area")+
  xlab("")+
  ylab("")

# Task 4 ----
vegehoehe <- terra::rast("vegetationshoehe_LFI.tif")

tm_shape(vegehoehe)+
  tm_raster()

# Task 5 ----
vegehoehe_ex<- extract(vegehoehe,wildschwein_sommer_Feld)

wildschwein_sommer_Feld<-cbind(wildschwein_sommer_Feld, vegehoehe_ex)

wildschwein_sommer_Feld$hoehekat[(wildschwein_sommer_Feld$vegetationshoehe_LFI >0 & wildschwein_sommer_Feld$vegetationshoehe_LFI<=10)]<-"0 to 10"

wildschwein_sommer_Feld$hoehekat[(wildschwein_sommer_Feld$vegetationshoehe_LFI >10 & wildschwein_sommer_Feld$vegetationshoehe_LFI<=20)]<-"10 to 20"

wildschwein_sommer_Feld$hoehekat[(wildschwein_sommer_Feld$vegetationshoehe_LFI >20 & wildschwein_sommer_Feld$vegetationshoehe_LFI<=30)]<-"20 to 30"

wildschwein_sommer_Feld$hoehekat[(wildschwein_sommer_Feld$vegetationshoehe_LFI >30 & wildschwein_sommer_Feld$vegetationshoehe_LFI<40)]<-"30 to 40"

wildschwein_sommer_Feld$hoehekat[(wildschwein_sommer_Feld$vegetationshoehe_LFI >40 & wildschwein_sommer_Feld$vegetationshoehe_LFI<50)]<-"40 to 50"

wildschwein_sommer_Feld$hoehekat[(is.na(wildschwein_sommer_Feld$hoehekat))] <-"other"
wildschwein_sommer_Feld <-wildschwein_sommer_Feld |> count(TierName,hour,hoehekat)


ggplot(data=wildschwein_sommer_Feld, aes(x=hour, fill=factor(hoehekat,levels=c("0 to 10","10 to 20","20 to 30","30 to 40","40 to 50","other"))))+
  geom_bar(position="fill")+
  facet_wrap(~TierName)+
  labs(fill="area")+
  scale_y_continuous("Percentage", labels= scales::percent)+
  xlab("vegetation height")
