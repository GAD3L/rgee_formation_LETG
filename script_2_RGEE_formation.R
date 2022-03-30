#liberer session
rm(list = ls())

## Import et initialisation rgee
library(rgee)
ee_Initialize()

###################################################
## 6 - Classification non-supervisee : Kmeans 

# Telechargement d'une image d'entrainement
sinop <- ee$Geometry$Point(-55.51,-11.87)
S2 = ee$ImageCollection("COPERNICUS/S2")

sinopS2 = S2$filterDate("2020-01-15","2020-12-31")$filterBounds(sinop)$filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE",20))$first() # selection d'une image 

# Definition d'une r?gion d'?chantillonage sur l'image
region <- ee$Geometry$Rectangle(-55.92, -12.32,-55.45, -11.86)

# Jeu d'entrainement
training <- sinopS2$sample(
  region = region,
  scale = 10,
  numPixels = 5000
)

# Initie le modele et on l'entraine
clusterer <- ee$Clusterer$wekaKMeans(2)$train(training)

# application du model Kmeans ? l'image
resultimage <- sinopS2$cluster(clusterer)

## Visualisation 1 image
# Montre la region d'entrainement
Map$setCenter(-55.55,-11.94,8)
mapRegion <- Map$addLayer(
  eeObject = ee$Image()$paint(region, 0, 2),
  name = "region"
)

# Visualisation 
geoviz_class = list(min = 0, max = 1, palette = c("yellow", "green"))
geoviz_imageS2 = list(bands = c("B4", "B3", "B2"),min=0, max = 2000)

mapResult <- Map$addLayer(
  eeObject = resultimage,
  visParams = geoviz_class,
  name = "Kmeans 2020"
)

mapsinopS2 <- Map$addLayer(
  eeObject = sinopS2,
  visParams = geoviz_imageS2,
  name = "image S2 2020"
)

map1 <- mapsinopS2 + mapRegion
map1

map <- mapResult | mapsinopS2
# Ajout d'une legende 
map%>% addLegend(
  position = "bottomright",
  colors = c("green","yellow"),
  labels = c("0","1"), 
  opacity = 1,
  title = "L?gende"
)


ee_print(resultimage)


## Export du resultat 

task_img <- ee_image_to_drive(
  image = resultimage,
  folder = "rgee_formation",
  fileFormat = "GEO_TIFF",
  fileNamePrefix = "Kmeans2020_sinop"
)

task_img$start()
ee_monitoring(task_img)

###################################################
## 7 - Classification supervisee : RF

# Telechargement d'une image S2

S2 = ee$ImageCollection("COPERNICUS/S2_SR")

S2_2020 = S2$filterDate('2020-01-01','2020-12-31')$filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE",10))

frontiere_MT <- ee$FeatureCollection('FAO/GAUL/2015/level1')$filter(ee$Filter$eq('ADM1_NAME','Mato Grosso'))$geometry() # Utilisation des donn?es FAO pour d?finir le Mato Grosso

S2_2020_MT = S2_2020$median()$clip(frontiere_MT)$select('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12')


# Import de donnees terrain 
library(sf)
data_terrain <- st_read("F:/RGEE/Formation_RGEE/terrain/Terrain.shp") 

terrain <- sf_as_ee(data_terrain)

ee_print(terrain)


# Visualisation des donnees terrain
Map$setCenter(-54.65,-11.47,6)

mapImage <- Map$addLayer(
  eeObject = S2_2020_MT,
  visParams = list(
    bands = c("B4", "B3", "B2"),
    min = 0,
    max = 2500
  ),
  name = "S2_2020_sinop"
)

mapterrain <- Map$addLayer(
  eeObject = terrain,
  name = "Terrain"
)

mapImage + mapterrain


# Extraction a partir des donnees terrain (en plus, pas necessaire pour RF)
extarct_data_terrain <- ee_extract(S2_2020_MT,data_terrain)


# RandomForest

# Utilsation de ces bandes
bands = c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12')

# Jeu d'entrainement
training = S2_2020_MT$sampleRegions(
  collection = terrain,
  properties = list("lab"),
  scale = 10
)

# Creation d'un classifier randomforest
classifier = ee$Classifier$smileRandomForest(
  numberOfTrees = 100
)

# Entrainement du classifier.
trained = classifier$train(training, "lab",bands)

#Apllication a l'image
classified = S2_2020_MT$classify(trained)
# ee_print(classified)


# Visualisation de l'image resultat
geoviz_image = list(bands = c("B4", "B3", "B2"),min=0, max = 2500)
geoviz_class = list(min = 0, max = 1, palette = c("yellow", "green"))

Map$setCenter(-55.51,-11.87, 6)

mapclassified <- Map$addLayer(
  eeObject = classified,
  visParams = geoviz_class,
  name = "classif_2020"
)

mapImage <- Map$addLayer(
  eeObject = S2_2020_MT,
  visParams = geoviz_image,
  name = "S2_2020_sinop"
)

mapclassified | mapImage

## Extraction comme raster (550sec)

img_raster <- ee_as_raster(
  classified,
  region = ee$Geometry$Rectangle(-55.82,-11.99,-55.21,-11.56),
  via = "drive",
  container = "rgee_formation",
  scale = 10,
  maxPixels = 1e+09
)

###################################################
## 8 - Serie temporelle 

# Telechargement collection MODIS
col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')$filterDate('2020-01-01','2020-12-31')

ee_print(col)

# Import de donnee Terrain
library(sf)
data_terrain <- st_read("F:/RGEE/Formation_RGEE/terrain/Terrain.shp")

extract <- ee_extract(col,data_terrain)
extract

data_terrain_ee <- sf_as_ee(data_terrain)

# Creation d'un point EE
pt <- ee$Geometry$Point(-1.5427,48.5247)
pt_sf <- ee_as_sf(pt)

print(pt_sf)

pt_NDVI <- ee_extract(col,pt_sf)
pt_NDVI

# Export en csv 
write.csv(pt_NDVI,file = "F:/RGEE/Formation_RGEE/pt_NDVI.csv") #selection du fichier ou il sera enregitré

# Visualisation
col_first <- col$first()

mapNDVI <- Map$addLayer(
  eeObject = col_first,
  visParams = list(
    min = -2000,
    max = 10000,
    palette=c(
      'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
      '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
      '012E01', '011D01', '011301'
    )
  ),
  name = "NDVI"
)

mapdataterrain <- Map$addLayer(
  eeObject = data_terrain_ee,
  name = "data_terrain"
)

mappt <- Map$addLayer(
  eeObject = pt,
  name = "pt"
)

mapNDVI + mapdataterrain + mappt


