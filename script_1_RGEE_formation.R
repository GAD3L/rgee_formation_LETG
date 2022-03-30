
###################################################
## 1 - Installation de rgee
# 
# library(rgee)
# ee_install() #initialise earth engine par une connexion et cr?ation d'un nouvel environnement python
# ee_Initialize()
# 
# ## restart R
# ee_check() ## V?rifier que tout est OK 

# ee_install_upgrade()

## Packages RGEE

library(rgee)
ee_Initialize() # Initialisation de la connexion avec GEE

###################################################
## 2 - Telechargement des donnees et visualisation 

# Telechargement d'une collection
L5_SR <- ee$ImageCollection('LANDSAT/LT05/C01/T1_SR')
# ee_print(L5_SR)

# Definition d'un point d'int?r?t
sinop <- ee$Geometry$Point(-55.51,-11.87)
# ee_print(sinop)

# Selection des images de 2010 sur sinop
L5_2010_sinop <- L5_SR$filterDate("2010-01-01","2010-12-31")$filterBounds(sinop)

ee_print(L5_2010_sinop)
L5_2010_sinop$getInfo()

#library(rgeeExtra)
first_L5_2010_sinop <- L5_2010_sinop[[1]]


# Selection de la premiere image de la collection
first_L5_2010_sinop <- L5_2010_sinop$first()
L5_2010_sinop$first()$getInfo()

ee_print(L5_2010_sinop)

ee_print(first_L5_2010_sinop)

## Visualisation de l'image 

Map$setCenter (-54.65,-11.47,8) # Centre de la map sur le point d'int?ret

mapImage <- Map$addLayer(
  eeObject = first_L5_2010_sinop,
  visParams = list(
    bands = c("B3", "B2", "B1"),
    min = 0,
    max = 2500
  ),
  name = "RGB"
)

mapImage


###################################################
## 3 - Masque des nuages sur L5 

# Fonction qui masque les nuages sur les images L5
cloudMaskL457 <- function(image) {
  qa = image$select('pixel_qa')
  cloud = qa$bitwiseAnd(5)$And(qa$bitwiseAnd(7))$Or(qa$bitwiseAnd(3));
  mask2 = image$mask()$reduce(ee$Reducer$min())
  return (image$updateMask(cloud)$updateMask(mask2))
}

imagenocloud = cloudMaskL457(first_L5_2010_sinop) 

# Visualisation
Map$setCenter (-54.65,-11.47,8)

mapImagenocloud <- Map$addLayer(
  eeObject = imagenocloud,
  visParams = list(
    bands = c("B3", "B2", "B1"),
    min = 0,
    max = 2500
  ),
  name = "RGBnocloud"
)

mapImage | mapImagenocloud

###################################################
## 4 - Calcul du NDVI et export

NDVI<-imagenocloud$expression(
  '((TM4 - TM3) / (TM4 + TM3))',
  list('TM4'=imagenocloud$select('B4'),
       'TM3'=imagenocloud$select('B3')))

ee_print(NDVI)

mapNDVI <- Map$addLayer(
  eeObject = NDVI,
  visParams = list(
    min = 0,
    max = 1,
    palette=c(
      'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
      '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
      '012E01', '011D01', '011301'
    )
  ),
  name = "NDVI"
)

mapNDVI | mapImagenocloud

# Ajout du NDVI comme une bande de l'image 

imagenocloud <- imagenocloud$select('B1','B2','B3','B4','B5','B6','B7') # selection des bandes L5
addNDVI <- imagenocloud$addBands(NDVI) # ajout de l'image NDVI comme nouvelle bande 
imageNDVI = addNDVI$rename('B1','B2','B3','B4','B5','B6','B7','NDVI') # Rename de la nouvelle bande 
ee_print(imageNDVI)

## Export selon un shapefile
## Import de shapefile
library(sf)
data_terrain <- st_read("F:/RGEE/Formation_RGEE/data_terrain/data_terrain.shp") 

# Visualisation

# Passage de sf as eeobject
terrain <- sf_as_ee(data_terrain) # passage du shp en ee objet
ee_print(terrain)

Map$setCenter (-54.65,-11.47,8)

mapImagenocloud <- Map$addLayer(
  eeObject = imagenocloud,
  visParams = list(
    bands = c("B3", "B2", "B1"),
    min = 0,
    max = 2500
  ),
  name = "RGB"
)

mapdataterrain <- Map$addLayer(
  eeObject = terrain,
  name = "data_terrain",
)

mapImagenocloud + mapdataterrain


## Utilisation de ee_extract

NDVI_extract <- ee_extract(imageNDVI,data_terrain,fun = ee$Reducer$mean()) #extraction d'une valeur moyenne par polygone pour chaque bande de l'image
str(NDVI_extract)

write.csv(NDVI_extract,file = "F:/RGEE/Formation_RGEE/NDVI.csv")


###################################################
## 5 - Creation d'une fonction NDVI et de classes sur le NDVI

addNDVI <- function(img){
  NDVI<-img$expression('(TM4 - TM3) / (TM4 + TM3 + 0.001)',
    list('TM4'=img$select('B4'),'TM3'=img$select('B3')))
  addNDVI = img$addBands(NDVI$rename('NDVI'))
  return(addNDVI)
}

ClassNDVI <- function (img){
  NDVI_classe<-img$expression("(b('NDVI') > 0.7) ? 3 :
  (b('NDVI') >= 0.36) ? 2 : (b('NDVI') < 0.36) ? 1 : 0")
  addNDVI_Classe = img$addBands(NDVI_classe$rename('NDVI_classe'))
  return(addNDVI_Classe)
}

## Application de la fonction sur une image
image_NDVI <- addNDVI(first_L5_2010_sinop)
ee_print(image_NDVI)

image_classNDVI <- ClassNDVI(image_NDVI) 
ee_print(image_classNDVI)

## Visualisation 
mapNDVI <- Map$addLayer(
  eeObject = image_NDVI$select('NDVI'),
  visParams = list(
    min = 0,
    max = 1,
    palette=c(
      'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
      '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
      '012E01', '011D01', '011301'
    )
  ),
  name = "NDVI"
)

mapClassNDVI <-  Map$addLayer(
  image_classNDVI$select('NDVI_classe'),
  list(min=0, max=3, 
  palette=c('#000000', '#BEF52D', '#61F712', '#1C9B05')),
  'NDVI_classe')

mapNDVI + mapClassNDVI

## Meme chose sur l'ensemble du Mato Grosso
#  Mato Grosso
frontiere_MT <- ee$FeatureCollection('FAO/GAUL/2015/level1')$filter(ee$Filter$eq('ADM1_NAME','Mato Grosso'))$geometry()

L5_2010_MT <- L5_SR$filterDate("2010-01-01","2010-12-31")$filterBounds(frontiere_MT)

composite_MT <- L5_2010_MT$median()$clip(frontiere_MT)

# Application des fonctions
composite_MT_NDVI <- addNDVI(composite_MT)

composite_MT_class_NDVI <- ClassNDVI(composite_MT_NDVI)

# Visualisation 
mapNDVI <- Map$addLayer(
  eeObject = composite_MT_NDVI$select('NDVI'),
  visParams = list(
    min = 0,
    max = 1,
    palette=c(
      'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
      '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
      '012E01', '011D01', '011301'
    )
  ),
  name = "NDVI"
)

mapClassNDVI <-  Map$addLayer(
  composite_MT_class_NDVI$select('NDVI_classe'),
  list(min=0, max=3, 
       palette=c('#000000', '#BEF52D', '#61F712', '#1C9B05')),
  'NDVI_classe')

mapImage <- Map$addLayer(
  eeObject = composite_MT,
  visParams = list(
    bands = c("B3", "B2", "B1"),
    min = 0,
    max = 2500
  ),
  name = "composite_MT"
)

mapNDVI + mapClassNDVI + mapImage



