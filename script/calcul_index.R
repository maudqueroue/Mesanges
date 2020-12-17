##########################
#   Index PASSEREAUX     #
#   Mesange Charbonniere #
##########################
setwd("~/These/MNHN/Mesanges")
rm(list=ls())

# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 

# Les donnees EPS
load(here::here("output","data_EPS.RData"))
data_EPS <- data_EPS %>%
  dplyr::filter(data_EPS$annee != 1914) %>%
  dplyr::rename(
    long = 'longitude_wgs84',
    lat  = 'latitude_wgs84') %>%
  dplyr::filter(!is.na(long)) %>%
  dplyr::filter(!is.na(lat)) %>%
  replace(is.na(.), 0)

# Type d'habitat pour chaque station STOC
load(here::here("output","CLC_STOC.RData"))

# Type d'habitat pour chaque station EPS
load(here::here("output","CLC_EPS.RData"))
CLC_EPS <- dplyr::as_tibble(CLC_EPS)

# Les points d'ecoute (juste leurs identifiants) pour chaque station STOC dans un rayon de 25km
load(here::here("output","point_by_site.RData"))

# On lie les tables CLC_EPS et data_EPS
EPS <- dplyr::left_join(data_EPS, CLC_EPS, by = "point") %>%
  dplyr::rename(CLC_EPS = 'CLC')  %>%
  dplyr::select(-CLC_nd)
        
#mettre avec lat et long CLC_EPS nouveau

# Table finale
# selectionner les point concernant la station,
# ajouter une colonne avec le nom de la station
# lier les nouveaux tableaux

data <- data.frame("carre" = NA, "annee" = NA, "point" = NA, long= as.numeric("NA"), lat =as.numeric("NA"), PARCAE = as.numeric("NA"), PARMAJ = as.numeric("NA"), SYLATR = as.numeric("NA"), SYLBOR = as.numeric("NA"), longitude = "NA", latitude = "NA", CLC_EPS = "NA", ID_PROG = "NA", CLC_STOC = "NA")

for (i in 1:nrow(CLC_STOC)) {
  EPS_by_STOC <- EPS %>%
                dplyr::filter(EPS$point %in% point_by_site[[i]]) %>%
                tibble::add_column(ID_PROG = as.character(CLC_STOC$ID_PROG[i])) %>%
                tibble::add_column(CLC_STOC = as.character(CLC_STOC$CLC_1[i]))
  
  data <- dplyr::bind_rows(data,EPS_by_STOC)
}

rm(point_by_site, EPS_by_STOC, data_EPS, CLC_EPS, CLC_STOC, EPS, i)


# Type d habitat
hab_fav <- c("111","242","311","313","322")
hab_defav <- c("112", "121", "122", "123", "124", "131", "132", "133",
               "141", "142", "211", "212", "213", "221", "222", "223", 
               "231", "241", "243", "244", "312", "321", "323", "324",
               "331", "332", "333", "334", "335", "411", "412", "421",
               "422", "423", "511", "512", "521", "522", "523")


# Avec les donnees de PARMAJ
#############################

setwd("~/These/MNHN/CMR_parmaj")
load("hvie_ID_PROG_parmaj_tot.RData")
ID_PROG_parmaj <- hvie_ID_PROG_parmaj$ID_PROG
# On ne garde que les sites utilises dans les donnees parmaj
data_parmaj <- data %>%
                dplyr::filter(data$ID_PROG %in% ID_PROG_parmaj)

rm(hvie_ID_PROG_parmaj, ID_PROG_parmaj)

# On retrie selon le type d'habitat
#--------------------------------------------

# 1. On garde que les stations STOC dans l'habitat 1 : habitat favorable
# 2. On garde uniquement les point EPS dans l'habitat 1

#### Data habitat favorable 
data_parmaj_1 <- data_parmaj %>%
  dplyr::filter(data_parmaj$CLC_STOC %in% hab_fav & data_parmaj$CLC_EPS %in% hab_fav)  

# Quelle station n ont pas assez de points ?
data_1 <- data_parmaj_1 %>%
  dplyr::group_by(ID_PROG) %>%
  dplyr::count()

# Nom des stations concernees
ID_PROG_supr <- data_1$ID_PROG[which(data_1$n < 10)]

# On supprime ces stations
data_parmaj_1 <- data_parmaj_1 %>%
  dplyr::filter(!(data_parmaj_1$ID_PROG %in% ID_PROG_supr))

rm(data_1, ID_PROG_supr)

# Calcul de l'index
index_parmaj_hab1_mean <- Mesanges::index(data_parmaj_1, data_parmaj_1$PARMAJ)[[1]]
index_parmaj_hab1_sd <- Mesanges::index(data_parmaj_1, data_parmaj_1$PARMAJ)[[2]]

setwd("~/These/MNHN/Mesanges")
save(index_parmaj_hab1_mean, file  = here::here("output","index_parmaj_hab1_mean.RData"))
save(index_parmaj_hab1_sd, file  = here::here("output","index_parmaj_hab1_sd.RData"))

# Plot index
Mesanges::plot_index(index_parmaj_hab1_mean, index_parmaj_hab1_sd, "indianred4", "parmaj", "favorable" )


#### Data habitat defavorable 
data_parmaj_2 <- data_parmaj %>%
  dplyr::filter(data_parmaj$CLC_STOC %in% hab_defav & data_parmaj$CLC_EPS %in% hab_defav)  

# Quelle station n ont pas assez de points ?
  data_2 <- data_parmaj_2 %>%
  dplyr::group_by(ID_PROG) %>%
  dplyr::count()

# Nom des stations concernees
ID_PROG_supr <- data_2$ID_PROG[which(data_2$n < 10)]

# On supprime ces stations
data_parmaj_2 <- data_parmaj_2 %>%
  dplyr::filter(!(data_parmaj_2$ID_PROG %in% ID_PROG_supr))

rm(data_2, ID_PROG_supr)

# Calcul de l'index
index_parmaj_hab2_mean <- Mesanges::index(data_parmaj_2, data_parmaj_2$PARMAJ)[[1]]
index_parmaj_hab2_sd <- Mesanges::index(data_parmaj_2, data_parmaj_2$PARMAJ)[[2]]
setwd("~/These/MNHN/Mesanges")
save(index_parmaj_hab2_mean, file  = here::here("output","index_parmaj_hab2_mean.RData"))
save(index_parmaj_hab2_sd, file  = here::here("output","index_parmaj_hab2_sd.RData"))

# Plot index
Mesanges::plot_index(index_parmaj_hab2_mean, index_parmaj_hab2_sd, "indianred4", "parmaj", "defavorable" )


rm(data_parmaj)


# Avec les donnees de PARCAE
#############################

setwd("~/These/MNHN/CMR_parcae")
load("hvie_ID_PROG_parcae_tot.RData")
ID_PROG_parcae <- hvie_ID_PROG_parcae$ID_PROG
# On ne garde que les sites utilis?s dans les donn?es parmaj
data_parcae <- data %>%
  dplyr::filter(data$ID_PROG %in% ID_PROG_parcae)
rm(hvie_ID_PROG_parcae, ID_PROG_parcae,data)

# On retrie selon le type d'habitat
#--------------------------------------------

# 1. On garde que les stations STOC dans l'habitat 1 : habitat favorable
# 2. On garde uniquement les point EPS dans l'habitat 1

#### Data habitat favorable 
data_parcae_1 <- data_parcae %>%
  dplyr::filter(data_parcae$CLC_STOC %in% hab_fav & data_parcae$CLC_EPS %in% hab_fav)  

# Quelle station n ont pas assez de points ?
data_1 <- data_parcae_1 %>%
  dplyr::group_by(ID_PROG) %>%
  dplyr::count()

# Nom des stations concernees
ID_PROG_supr <- data_1$ID_PROG[which(data_1$n < 10)]

# On supprime ces stations
data_parcae_1 <- data_parcae_1 %>%
  dplyr::filter(!(data_parcae_1$ID_PROG %in% ID_PROG_supr))

rm(data_1, ID_PROG_supr)

# Calcul de l'index
index_parcae_hab1_mean <- Mesanges::index(data_parcae_1, data_parcae_1$PARCAE)[[1]]
index_parcae_hab1_sd <- Mesanges::index(data_parcae_1, data_parcae_1$PARCAE)[[2]]
setwd("~/These/MNHN/Mesanges")
save(index_parcae_hab1_mean, file  = here::here("output","index_parcae_hab1_mean.RData"))
save(index_parcae_hab1_sd, file  = here::here("output","index_parcae_hab1_sd.RData"))

# Plot index
Mesanges::plot_index(index_parcae_hab1_mean, index_parcae_hab1_sd, "indianred4", "parcae", "favorable" )


#### Data habitat defavorable 
data_parcae_2 <- data_parcae %>%
  dplyr::filter(data_parcae$CLC_STOC %in% hab_defav & data_parcae$CLC_EPS %in% hab_defav)  

# Quelle station n ont pas assez de points ?
data_2 <- data_parcae_2 %>%
  dplyr::group_by(ID_PROG) %>%
  dplyr::count()

# Nom des stations concernees
ID_PROG_supr <- data_2$ID_PROG[which(data_2$n < 10)]

# On supprime ces stations
data_parcae_2 <- data_parcae_2 %>%
  dplyr::filter(!(data_parcae_2$ID_PROG %in% ID_PROG_supr))

rm(data_2, ID_PROG_supr)

# Calcul de l'index
index_parcae_hab2_mean <- Mesanges::index(data_parcae_2, data_parcae_2$PARCAE)[[1]]
index_parcae_hab2_sd <- Mesanges::index(data_parcae_2, data_parcae_2$PARCAE)[[2]]
setwd("~/These/MNHN/Mesanges")
save(index_parcae_hab2_mean, file  = here::here("output","index_parcae_hab2_mean.RData"))
save(index_parcae_hab2_sd, file  = here::here("output","index_parcae_hab2_sd.RData"))

# Plot index
Mesanges::plot_index(index_parcae_hab2_mean, index_parcae_hab2_sd, "indianred4", "parcae", "defavorable" )


rm(data_parcae)