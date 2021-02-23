##########################
#   Index PASSEREAUX     #
##########################
rm(list=ls())
color<-c("#FF8830","#A6B06D","#589482","#8C2423")

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

# Les points d'ecoute (juste leurs identifiants) pour chaque station STOC dans un rayon de 25km
load(here::here("output","EPS_by_STOC.RData"))

# On lie les tables CLC_EPS et data_EPS
EPS <- dplyr::left_join(data_EPS, CLC_EPS, by = c("point", "lat", "long")) %>%
  dplyr::rename(CLC_EPS = 'CLC_hab')  %>%
  dplyr::select(-CLC_1) %>%
  dplyr::select(-CLC_2)
        
# Table finale
# selectionner les point concernant la station,
# ajouter une colonne avec le nom de la station
# lier les nouveaux tableaux

data <- data.frame("carre" = NA, "annee" = NA, "point" = NA, long= as.numeric("NA"), lat =as.numeric("NA"), PARCAE = as.numeric("NA"), PARMAJ = as.numeric("NA"), SYLATR = as.numeric("NA"), SYLBOR = as.numeric("NA"), CLC_EPS = as.numeric("NA"), ID_PROG = "NA", CLC_STOC = "NA")
# warnings "NA" pas importants
for (i in 1:nrow(CLC_STOC)) {
  data_add <- EPS %>%
                dplyr::filter(EPS$point %in% EPS_by_STOC[[i]]) %>%
                tibble::add_column(ID_PROG = as.character(CLC_STOC$ID_PROG[i])) %>%
                tibble::add_column(CLC_STOC = as.character(CLC_STOC$CLC_hab[i]))
  
  data <- dplyr::bind_rows(data, data_add)
}

rm(data_add, EPS_by_STOC, data_EPS, CLC_EPS, CLC_STOC, EPS, i)


# Type d habitat
hab_fav <- c("311","313")
hab_defav <- c("111","112", "121", "122", "123", "124", "131", "132", 
               "133","141", "142", "211", "212", "213", "221", "222", 
               "223", "231", "241", "242", "243", "244", "312", "321", 
               "322", "323", "324", "331", "332", "333", "334", "335", 
               "411", "412", "421", "422", "423", "511", "512", "521", 
               "522", "523")


# Avec les donnees de PARMAJ
#############################

load(here::here("output","hvie_ID_PROG_parmaj_tot.RData"))
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

# On supprime les points qui n'ont jamais contacté de mesanges
data_parmaj_1 <- Mesanges::supr_point(data_parmaj_1, "PARMAJ")

# Calcul de l'index
index_parmaj_hab1_mean <- Mesanges::index(data_parmaj_1, data_parmaj_1$PARMAJ)[[1]]
index_parmaj_hab1_var <- Mesanges::index(data_parmaj_1, data_parmaj_1$PARMAJ)[[2]]

# save
save(index_parmaj_hab1_mean, file  = here::here("output","index_parmaj_hab1_mean.RData"))
save(index_parmaj_hab1_var, file  = here::here("output","index_parmaj_hab1_var.RData"))

par(mfrow=c(2,2))
# Plot index
Mesanges::plot_index(index_parmaj_hab1_mean, sqrt(index_parmaj_hab1_var), color[3], "parmaj", "favorable" )

#### Data habitat defavorable 
data_parmaj_2 <- data_parmaj %>%
  dplyr::filter(data_parmaj$CLC_STOC %in% hab_defav & data_parmaj$CLC_EPS %in% hab_defav)  

# On supprime les points qui n'ont jamais contacté de mesanges
data_parmaj_2 <- Mesanges::supr_point(data_parmaj_2, "PARMAJ")

# Calcul de l'index
index_parmaj_hab2_mean <- Mesanges::index(data_parmaj_2, data_parmaj_2$PARMAJ)[[1]]
index_parmaj_hab2_var <- Mesanges::index(data_parmaj_2, data_parmaj_2$PARMAJ)[[2]]
# save
save(index_parmaj_hab2_mean, file  = here::here("output","index_parmaj_hab2_mean.RData"))
save(index_parmaj_hab2_var, file  = here::here("output","index_parmaj_hab2_var.RData"))

# Plot index
Mesanges::plot_index(index_parmaj_hab2_mean, sqrt(index_parmaj_hab2_var), color[3], "parmaj", "defavorable" )


rm(data_parmaj)


# Avec les donnees de PARCAE
#############################
load(here::here('output',"hvie_ID_PROG_parcae_tot.RData"))
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

# On supprime les points qui n'ont jamais contacté de mesanges
data_parcae_1 <- Mesanges::supr_point(data_parcae_1, "PARCAE")

# Calcul de l'index
index_parcae_hab1_mean <- Mesanges::index(data_parcae_1, data_parcae_1$PARCAE)[[1]]
index_parcae_hab1_var <- Mesanges::index(data_parcae_1, data_parcae_1$PARCAE)[[2]]

# save 
save(index_parcae_hab1_mean, file  = here::here("output","index_parcae_hab1_mean.RData"))
save(index_parcae_hab1_var, file  = here::here("output","index_parcae_hab1_var.RData"))

# Plot index
Mesanges::plot_index(index_parcae_hab1_mean, sqrt(index_parcae_hab1_var), color[2], "parcae", "favorable" )
#Mesanges::plot_glm(data_parcae_1, data_parcae_1$PARCAE)

#### Data habitat defavorable 
data_parcae_2 <- data_parcae %>%
  dplyr::filter(data_parcae$CLC_STOC %in% hab_defav & data_parcae$CLC_EPS %in% hab_defav)  

# On supprime les points qui n'ont jamais contacté de mesanges
data_parcae_2 <- Mesanges::supr_point(data_parcae_2, "PARCAE")

# Calcul de l'index
index_parcae_hab2_mean <- Mesanges::index(data_parcae_2, data_parcae_2$PARCAE)[[1]]
index_parcae_hab2_var <- Mesanges::index(data_parcae_2, data_parcae_2$PARCAE)[[2]]
# save
save(index_parcae_hab2_mean, file  = here::here("output","index_parcae_hab2_mean.RData"))
save(index_parcae_hab2_var, file  = here::here("output","index_parcae_hab2_var.RData"))

# Plot index
Mesanges::plot_index(index_parcae_hab2_mean, sqrt(index_parcae_hab2_var), color[2], "parcae", "defavorable" )

rm(data_parcae)


