
######                                       ######
###### SCRIPT DONNEES MODELE PARCAE          ######
######                                       ######

# Jeu de donnees entrant : histoire de vie des sites
#                          Premier tri donnees MNHN

# Jeu de donnees sortant : hvie_parcae_tot
#                          hvie_ID_prog_parcae_tot   


rm(list=ls())

# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 

# Data
load(here::here("output","data_STOC.RData"))

# 1. Selection de l'espece
#------------------------
# On ne garde que les parcae
parcae <- subset(data_STOC,data_STOC$ESPECE=="PARCAE")
rm(data_STOC)

#Informations 
# Annees
years <- seq(min(unique(substr(parcae$DATE,7,10))),max(unique(substr(parcae$DATE,7,10))),1)
K <- length(years)
#Identifiant des parcae 
ID_parcae <- unique(parcae$BAGUE)
N <- length(ID_parcae)

# 2. Redefinition de l'age 
#-----------------------
unique(parcae$AGE)

parcae <- Mesanges::new_age(parcae)

unique(parcae$new_AGE)

# 3. Creation des histoires de vie
# -----------------------

# hvie
hvie_parcae <- as.data.frame(matrix(NA,length(ID_parcae),K))
hvie_parcae$ID <- ID_parcae
colnames(hvie_parcae) <- c(years,"ID")

# Calcul hvie
hvie_parcae <- Mesanges::hvie_tt(parcae, hvie_parcae)
N <- dim(hvie_parcae)[1]

# On transforme en numerique : Adulte = 2, Jeune = 1
hvie_parcae[hvie_parcae=="A"] <- 2
hvie_parcae[hvie_parcae=="P"] <- 1
hvie_parcae[hvie_parcae=="C"] <- 3
hvie_parcae[hvie_parcae=="AP"] <- 4


# 4. On regarde s'il n'y a pas des individus vu jeunes plus tard que la premiere occasion de capture
#----------------------------------

hvie_parcae <- Mesanges::check_age(hvie_parcae)
N <- dim(hvie_parcae)[1]


# 5. On regarde s'il reste des incertains
#------------------------------------------

# si vus une seule fois incertains, on retire la donnees.
check <- which(hvie_parcae[,1:K]==3) # 29

# Si il est capture qu'une seule annee, on supprime
hvie_parcae <- Mesanges::check_3(hvie_parcae, check)

# Qui doit on trier au cas par cas :
check <- which(hvie_parcae[,1:K]==3) # 2

# cas pas cas
ligne <- check[1]%%N
colonne <- ceiling(check[1]/N)
bague <- hvie_parcae$ID[ligne] # ....4145741
subset(parcae,parcae$BAGUE==bague)
hvie_parcae[ligne,colonne] <- 1

ligne <- check[2]%%N
colonne <- ceiling(check[2]/N)
bague <- hvie_parcae$ID[ligne] # ....4145839
subset(parcae,parcae$BAGUE==bague)
hvie_parcae[ligne,colonne] <- 1

# Est-ce qu'on a fini ? 
check <- which(hvie_parcae[,1:K]==3) # Oui

rm(colonne,ligne,bague,check)

# 6. On regarde ceux pour qui on a pas pu trancher entre Adulte et poussin
#----------------------------
check <- which(hvie_parcae[,1:K]==4) # 27

# On retranscrit en numerique
parcae$new_AGE[parcae$new_AGE=="P"]<-1
parcae$new_AGE[parcae$new_AGE=="A"]<-2

# S'il ont ete vus qu'une seule annee, on prend le statut defini lors du bagage
# Si pas de baguage la seule annee ou il y a ete vu, on le regarde au cas par cas
# Si individus vus plusieurs annees on regarde au cas par cas
hvie_parcae <- Mesanges::check_4(parcae, hvie_parcae, check)

# Qui doit on trier au cas par cas :
check <- which(hvie_parcae[,1:K]==4) # 1 individu

#1
ligne <- check[1]%%N
colonne <- ceiling(check[1]/N)
bague <- hvie_parcae$ID[ligne] #"....4301563"
subset(parcae,parcae$BAGUE==bague)
hvie_parcae[ligne,]
hvie_parcae[ligne,colonne] <- 2
hvie_parcae[ligne,]

# On a fini ?
check <- which(hvie_parcae[,1:K]==4) #oui

# On supprime les oiseaux qui ne sont finalement pas conserves
# avec uniquement des 0 dans les hvie
hvie_parcae <- Mesanges::supp_ind(hvie_parcae)

# On remet à jour N et ID_parcae
N <- dim(hvie_parcae)[1]
ID_parcae <- hvie_parcae$ID

rm(check,ligne,colonne,bague)

# 7. Gestion des transients
# #-----------------------

hvie_parcae <- Mesanges::transient_tt(parcae, hvie_parcae)
# warnings : pas de probleme

# On supprime les oiseaux qui ne sont finalement pas conserves
# avec uniquement des 0 dans les hvie
hvie_parcae <- Mesanges::supp_ind(hvie_parcae)

# On remet à jour N et ID_parcae
N <- dim(hvie_parcae)[1]
ID_parcae <- hvie_parcae$ID

# 8. Covariable individuelle 
#---------

# Il nous faut le nombre de captures total (-1 pour transience)
# et le nombre d'annees de capture
# pour chaque individus des hvie
hvie_parcae <- Mesanges::cov_ind_tt(parcae, hvie_parcae)


# Quels individus sont problematiques
which(hvie_parcae$nb_years_capt==0) #0
which(hvie_parcae$nb_capt==0) #0
which(is.na(hvie_parcae$nb_years_capt)==T) #0
which(is.na(hvie_parcae$nb_capt)==T) #0

#Calcul de la covariable :
hvie_parcae <- Mesanges::calcul_cov_ind(hvie_parcae)

which(hvie_parcae$cov_ind<0) # ca parait bon

# 9. Hvie site
#-------------------------------------------------

load(here::here('output',"hvie_ID_PROG.RData"))

#Quels sont les sites concernant les parcae
hvie_ID_PROG_parcae <- hvie_ID_PROG %>%
  dplyr::filter(hvie_ID_PROG$ID_PROG %in% unique(hvie_parcae$ID_PROG))

rm(hvie_ID_PROG)

# 10. type d'habitat
#----------------------------------------

# On ajoute les infos contenu dans CLC_STOC aux hvies 
load(here::here("output","CLC_STOC.RData"))
hvie_ID_PROG_parcae <- dplyr::left_join(hvie_ID_PROG_parcae, CLC_STOC, by = c("ID_PROG")) 

rm(CLC_STOC)  

# type d'habitat
hvie_ID_PROG_parcae <- Mesanges::cov_hab(hvie_ID_PROG_parcae)

# 11. Creation vecteur pour lier hvie individus et hvie site
#---------

hvie_parcae <- Mesanges::link_hvie(hvie_parcae, hvie_ID_PROG_parcae)

# 12. Save
#--------------
save(hvie_parcae,file=here::here('output',"hvie_parcae_tot_tt.RData"))
save(hvie_ID_PROG_parcae,file=here::here('output',"hvie_ID_PROG_parcae_tot_tt.RData"))


# Analyses supplementaires 
#---------------------------
rm(list=ls())
load(here::here('output',"hvie_parcae_tot_tt.RData"))
load(here::here('output',"hvie_ID_PROG_parcae_tot_tt.RData"))

info_PROG <- hvie_ID_PROG_parcae[,-c(1:19)] %>%
  dplyr::mutate(ID_PROG=as.factor(ID_PROG)) 

hvie_parcae <- hvie_parcae %>%
  dplyr::mutate(ID_PROG=as.factor(ID_PROG))

rm(hvie_ID_PROG_parcae)

data <- dplyr::left_join(hvie_parcae, info_PROG, by = c("ID_PROG")) 


data_1 <- data %>%
  dplyr::filter(cov_hab == 1)

data_2 <- data %>%
  dplyr::filter(cov_hab == 2)


# data 1
N <- nrow(data_1)
K <- 19

nb_capt <- NULL
juv <- NULL
ad <- NULL
ad_juv <- NULL
ad_1 <- NULL
juv_1 <- NULL

for(i in 1:N){
  nb_capt[i] <- length(which(data_1[i,1:K]>0))
  
  juv[i] <- length(which(data_1[i,1:K]==1))
  ad[i] <- length(which(data_1[i,1:K]==2))
  
  if (nb_capt[i] >1) {
    if(ad[i]>0 & juv[i]>0){
      ad_juv[i] <- 1 }
    else {ad_juv[i] <- 0}
  }
  
  if(nb_capt[i] == 1) {
    ad_1[i] <- ad[i]
    juv_1[i] <- juv[i]
  }
  else {ad_1[i] <- 0
  juv_1[i] <- 0}
}

length(which(nb_capt>1))
length(which(juv>0))
length(which(ad>0))
length(which(ad_1>0))
length(which(juv_1>0))
length(which(ad_juv==1))

# data 2
N <- nrow(data_2)

nb_capt <- NULL
juv <- NULL
ad <- NULL
ad_juv <- NULL
ad_1 <- NULL
juv_1 <- NULL

for(i in 1:N){
  nb_capt[i] <- length(which(data_2[i,1:K]>0))
  
  juv[i] <- length(which(data_2[i,1:K]==1))
  ad[i] <- length(which(data_2[i,1:K]==2))
  
  if (nb_capt[i] >1) {
    if(ad[i]>0 & juv[i]>0){
      ad_juv[i] <- 1 }
    else {ad_juv[i] <- 0}
  }
  
  if(nb_capt[i] == 1) {
    ad_1[i] <- ad[i]
    juv_1[i] <- juv[i]
  }
  else {ad_1[i] <- 0
  juv_1[i] <- 0}
}

length(which(nb_capt>1))
length(which(juv>0))
length(which(ad>0))
length(which(ad_1>0))
length(which(juv_1>0))
length(which(ad_juv==1))

