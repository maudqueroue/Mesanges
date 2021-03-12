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

load(here::here("output","hvie_ID_PROG_parmaj_tot_tt.RData"))
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

index_parmaj_1 <- index_diff(data_parmaj_1, "PARMAJ")
plot(index_parmaj_1, type='l')
n_parmaj_1 <- calcul_n(data_parmaj_1,"PARMAJ",index_parmaj_1)

save(index_parmaj_1, file=here::here("output","index_parmaj_1.RData"))
save(n_parmaj_1, file=here::here("output","n_parmaj_1.RData"))

#### Data habitat defavorable 
data_parmaj_2 <- data_parmaj %>%
  dplyr::filter(data_parmaj$CLC_STOC %in% hab_defav & data_parmaj$CLC_EPS %in% hab_defav) 
# On supprime les points qui n'ont jamais contacté de mesanges
data_parmaj_2 <- Mesanges::supr_point(data_parmaj_2, "PARMAJ")

index_parmaj_2 <- index_diff(data_parmaj_2, "PARMAJ")
plot(index_parmaj_2, type='l')
n_parmaj_2 <- calcul_n(data_parmaj_2,"PARMAJ",index_parmaj_2)

save(index_parmaj_2, file=here::here("output","index_parmaj_2.RData"))
save(n_parmaj_2, file=here::here("output","n_parmaj_2.RData"))

### Pour toute l'espece

data_parmaj <- Mesanges::supr_point(data_parmaj, "PARMAJ")

index_parmaj <- index_diff(data_parmaj, "PARMAJ")
plot(index_parmaj, type='l')


# Avec les donnees de PARCAE
#############################
load(here::here('output',"hvie_ID_PROG_parcae_tot_tt.RData"))
ID_PROG_parcae <- hvie_ID_PROG_parcae$ID_PROG
# On ne garde que les sites utilis?s dans les donn?es parmaj
data_parcae <- data %>%
  dplyr::filter(data$ID_PROG %in% ID_PROG_parcae)
rm(hvie_ID_PROG_parcae, ID_PROG_parcae)


#### Data habitat favorable 
data_parcae_1 <- data_parcae %>%
  dplyr::filter(data_parcae$CLC_STOC %in% hab_fav & data_parcae$CLC_EPS %in% hab_fav)  
# On supprime les points qui n'ont jamais contacté de mesanges
data_parcae_1 <- Mesanges::supr_point(data_parcae_1, "PARCAE")

index_parcae_1 <- index_diff(data_parcae_1, "PARCAE")
plot(index_parcae_1, type='l')
n_parcae_1 <- calcul_n(data_parcae_1,"PARCAE",index_parmaj_1)

save(index_parcae_1, file=here::here("output","index_parcae_1.RData"))
save(n_parcae_1, file=here::here("output","n_parcae_1.RData"))

#### Data habitat defavorable 
data_parcae_2 <- data_parcae %>%
  dplyr::filter(data_parcae$CLC_STOC %in% hab_defav & data_parcae$CLC_EPS %in% hab_defav) 
# On supprime les points qui n'ont jamais contacté de mesanges
data_parcae_2 <- Mesanges::supr_point(data_parcae_2, "PARCAE")

index_parcae_2 <- index_diff(data_parcae_2, "PARCAE")
plot(index_parcae_2, type='l')
n_parcae_2 <- calcul_n(data_parcae_2,"PARCAE",index_parmaj_2)

save(index_parcae_2, file=here::here("output","index_parcae_2.RData"))
save(n_parcae_2, file=here::here("output","n_parcae_2.RData"))

### Pour toute l'espece

data_parcae <- Mesanges::supr_point(data_parcae, "PARCAE")

index_parcae <- index_diff(data_parcae, "PARCAE")
plot(index_parcae, type='l')


# Avec les donnees de SYLBOR
#############################
load(here::here('output',"hvie_ID_PROG_sylbor_tot_tt.RData"))
ID_PROG_sylbor <- hvie_ID_PROG_sylbor$ID_PROG
# On ne garde que les sites utilis?s dans les donn?es sylbor
data_sylbor <- data %>%
  dplyr::filter(data$ID_PROG %in% ID_PROG_sylbor)
rm(hvie_ID_PROG_sylbor, ID_PROG_sylbor)


# On supprime les points qui n'ont jamais contacté de sylbor
data_sylbor <- Mesanges::supr_point(data_sylbor, "SYLBOR")
index_sylbor <- index_diff(data_sylbor, "SYLBOR")
plot(index_sylbor, type='l')
n_sylbor <- calcul_n(data_sylbor,"SYLBOR",index_sylbor)

save(index_sylbor, file=here::here("output","index_sylbor.RData"))
save(n_sylbor, file=here::here("output","n_sylbor.RData"))

# Avec les donnees de SYLATR
#############################
load(here::here('output',"hvie_ID_PROG_sylatr_tot_tt.RData"))
ID_PROG_sylatr <- hvie_ID_PROG_sylatr$ID_PROG
# On ne garde que les sites utilis?s dans les donn?es sylatr
data_sylatr <- data %>%
  dplyr::filter(data$ID_PROG %in% ID_PROG_sylatr)
rm(hvie_ID_PROG_sylatr, ID_PROG_sylatr)


# On supprime les points qui n'ont jamais contacté de sylatr
data_sylatr <- Mesanges::supr_point(data_sylatr, "SYLATR")
index_sylatr <- index_diff(data_sylatr, "SYLATR")
plot(index_sylatr, type='l')
n_sylatr <- calcul_n(data_sylatr,"SYLATR",index_sylatr)

save(index_sylatr, file=here::here("output","index_sylatr.RData"))
save(n_sylatr, file=here::here("output","n_sylatr.RData"))


