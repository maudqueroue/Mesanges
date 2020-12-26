rm(list=ls())

# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 

# Load Corine Land Cover
shp_CLC <- sf::st_read(dsn   = here::here("data","CLC12_FR_RGF_SHP"),
                   layer = "CLC12_FR_RGF") %>%
           sf::st_transform(shp,crs = 2154)

# Les differents points avec leur longitude et latitude et type CLC
load(here::here("output","data_EPS.RData"))
CLC_EPS <- data_EPS %>%
              dplyr::rename(
                long = 'longitude_wgs84',
                lat  = 'latitude_wgs84') %>%
              dplyr::distinct(point, .keep_all=T) %>%
              dplyr::select("point", "long", "lat") %>%
              dplyr::filter(!is.na(long)) %>%
              dplyr::filter(!is.na(lat))  %>%
              tibble::add_column(CLC_1 = NA) %>%
              tibble::add_column(CLC_2 = NA)         
rm(data_EPS)

# Creation de la couche de points
dsf_EPS <-  Mesanges::give_point(CLC_EPS)


# On recupere les premiers code CLC
for (i in 1:5){#nrow(CLC_EPS)) {
  CLC_EPS[i,] <- Mesanges::give_CLC(dsf_EPS[i,], CLC_EPS[i,], shp_CLC)
}

save(CLC_EPS, file  = here::here("output","CLC_EPS.RData"))



