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

data <- data_parmaj_1  
  
years <- seq(min(data$annee),max(data$annee),1)
K <- length(years)
ID_PROG <- unique(data$ID_PROG)
nb_S <- length(ID_PROG)
fluct <- as.data.frame(matrix(NA,nb_S,K)) 

  for (s in 1:nb_S) {
    a <- ID_PROG[s]
    data_site <- subset(data,data$ID_PROG==a)
    ID_point <- unique(data_site$point)
    N <- length(ID_point)
    hvie_point <- as.data.frame(matrix(NA,N,K))
    hvie_point$ID <- ID_point
      
    for (i in 1:N) {
    
          data_point <- subset(data_site, data_site$point == hvie_point$ID[i])
        
        for (j in 1:K) {
          data_point_annee <- subset(data_point,data_point$annee==years[j])
          
            
            if (nrow(data_point_annee)==0) {hvie_point[i,j] <- NA}
            if (nrow(data_point_annee)>=1) {hvie_point[i,j] <- data_point_annee$PARMAJ[1]}
            rm(data_point_annee)
          
        }
      
      rm(data_point)
    }
 #   rm(data_site, ID_point,N, hvie_point)
    
    for (t in 2:K) {
      sub_data <- subset(hvie_point,is.na(hvie_point[,(t-1)])==F & is.na(hvie_point[,t])==F)
      fluct[s,t] <- sum(sub_data[,t]) / sum(sub_data[,(t-1)]) 
    }
    
    rm(hvie_point,sub_data,data_site,ID_point,N)
  }
    
    #return(fluct)
  #}

fluct[fluct=="NaN"] <- NA
fluct[fluct==0] <- NA
fluct[fluct=="Inf"] <- NA


ff <- apply(fluct,2,mean,na.rm=T)

ens <- NULL
ens[1] <- 1
for (i in 2:K) {
  ens[i] <- ens[i-1] * ff[i]
}

plot(ens)
