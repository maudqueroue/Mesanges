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

CLC_STOC <- read.table(here::here("data","coord_STOC.csv"),head=T,sep=";") %>%
               dplyr::rename(
               long = 'Lon',
               lat  = 'Lat') %>%
               tibble::add_column(CLC_1 = NA) %>%
               tibble::add_column(CLC_2 = NA)


# Creation de la couche de points
dsf_STOC <-  Mesanges::give_point(CLC_STOC)


# On recupere les premiers code CLC
for (i in 1:nrow(CLC_STOC)) {
  CLC_STOC[i,] <- Mesanges::give_CLC(dsf_STOC[i,], CLC_STOC[i,], shp_CLC)
}

# On sauve le fichier
save(CLC_STOC, file  = here::here("output","CLC_STOC.RData"))
# write.csv(CLC_STOC,here::here("output","CLC_STOC.csv"))

# Carte 
Mesanges::plot_carte_STOC(378, CLC_STOC, shp_CLC)

