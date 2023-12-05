#################################################################
#### Atelier workshop : manipuler les données vecteur avec R ####
#### 1er décembre 2023                                       ####
#### Julia Paul-Venturine (PSE, LIEPP)                       ####
#################################################################


#### Préambule ####

# chemins

rm(list=ls())
gc()

ordi <- getwd()


# charger les libraires

library(sf)
library(sp)
library(tidyr)
library(tidyverse)

# charger le dossier

path <- "votre dossier local"
setwd(path)


#### Manipulation de données vecteur ####

# A : créer notre location iedes

iedes_point <- st_point(c(2.469865, 48.835492), dim = "XY") # sfg object
iedes_point

iedes_geom <- st_sfc(iedes_point, crs = "EPSG:4326")    # sfc object
iedes_data = data.frame(                           # data.frame object
  objet = "worksop",
  date = as.Date("2023-12-01")
)
iedes_sf <- st_sf(iedes_data, geometry = iedes_geom) 

iedes_sf


# B: Sections cadastrales

# lire le fichier : st_read() ou read_sf()

sec19 <- st_read("cadastre_oct2019/sections.shp") 
class(sec19)
head(sec19)

sec19_tbl <- read_sf("cadastre_oct2019/sections.shp")
class(sec19_tbl)
head(sec19_tbl)


# variables

ls(sec19)

class(sec19$commune)
class(sec19$geometry)

# sous échantillon

sec19_nogent <- sec19 %>% filter(commune=="94052")

# plot

plot(sec19_nogent)
plot(sec19_nogent[["geometry"]])

# aire des sections cadastrales

sec19_dep <- sec19 %>% 
  mutate(dep = substr(commune, 1,2)) %>%
  mutate(area_sec = st_area(geometry))

# Error in `stopifnot()`:
#   ℹ In argument: `area_dep = st_area(geometry)`.
# ℹ In group 65: `dep = "64"`.
# Caused by error in `wk_handle.wk_wkb()`:
#   ! Loop 0 is not valid: Edge 538 crosses edge 540

sec19_valid <- st_make_valid(sec19)

sec19_dep_valid <- sec19_valid %>% 
  mutate(dep = substr(commune, 1,2)) %>%
  group_by(dep) %>%
  mutate(area_sec = st_area(geometry))

View(sec19_dep_valid)

# filtrer

sec19_vdm <- sec19_dep_valid %>%
  filter(dep=="94")

# union   
plot(sec19_vdm[["geometry"]])

vdm <- sec19_vdm %>%
  summarize(geometry = st_union(geometry))

plot(vdm[["geometry"]])

# plot


# nettoyer les lignes résiduelles liées à l'union : st_buffer

vdm_clean <- st_buffer(vdm, 0)
plot(vdm_clean[["geometry"]])


# C : les usines

library(geojsonsf)
# GeoJSON to Simple Feature Converter

pprt_control <- geojson_sf("ICPE/icpe.geojson") 

pprt_sh <- pprt_control %>% 
  filter(seveso=="SH" &
           lib_precis!="Centroïde Commune" &
           lib_regime=="Soumis à Autorisation") 


# préparer le match : vérifier les CRS

head(pprt_sh)

st_crs(pprt_sh)
st_crs(sec19_vdm_clean)


# intersection

pprt_sh_vdm <- st_intersection(pprt_sh, sec19_vdm_clean)

# vecteur logique 

intersects <- st_intersects(sec19_vdm_clean, pprt_sh)
class(intersects)

# et si on n'avait pas le bon crs ?

# on reprojette 
pprt_sh_2154 <- st_transform(pprt_sh, 2154)
st_crs(pprt_sh_2154)

erreur <- st_intersection(sec19_vdm_clean, pprt_sh_2154)

# Erreur dans geos_op2_geom("intersection", x, y, ...) : 
#   st_crs(x) == st_crs(y) n'est pas TRUE

plot(pprt_sh_vdm[["geometry"]], add = TRUE, col = "red")
plot(iedes_sf[["geometry"]], add = TRUE, col = "blue")

# calculer les distances
distances <- st_distance(iedes_sf, pprt_sh_vdm)

# Trouver le nom de l'établissement
pprt_sh_vdm$nom_ets[which(distances==min(distances))]

# autres informations
pprt_sh_vdm[distances==min(distances),]


# zones tampons : simulation des plans de prévention des risques

pprt_vdm_simul <- st_buffer(pprt_sh_vdm, 2000)
plot(pprt_vdm_simul, add = TRUE, col = "green")

ppr_vdm <- st_union(pprt_vdm_simul)
plot(ppr_vdm, add = TRUE, col = "orange")
