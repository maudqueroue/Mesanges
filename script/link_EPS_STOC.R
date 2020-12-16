rm(list=ls())

# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 

#EPS
load(here::here("output","CLC_EPS.RData"))
CLC_EPS <- dplyr::as_tibble(CLC_EPS) %>%
               dplyr::rename(
               lat = 'latitude',
               long = 'longitude',
               CLC_1 = 'CLC',
               CLC_2 = 'CLC_nd')

# STOC
load(here::here("output","CLC_STOC.RData"))


# Creation des couche de points
dsf_STOC <-  Mesanges::give_point(CLC_STOC)
dsf_EPS <-  Mesanges::give_point(CLC_EPS)


# Point par par station STOC
point_by_site <- list()
for (i in 1:nrow(CLC_STOC)) {
  point_by_site[[i]] <- Mesanges::give_point_by_site(dsf_STOC[i,], dsf_EPS, 25)
}


###### Carte

# Load Corine Land Cover
shp_CLC <- sf::st_read(dsn   = here::here("data","CLC12_FR_RGF_SHP"),
                       layer = "CLC12_FR_RGF") %>%
  sf::st_transform(shp,crs = 2154)

Mesanges::plot_carte(9, dsf_STOC, dsf_EPS, CLC_EPS, 25, shp_CLC)

