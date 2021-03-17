
######                                       ######
###### SCRIPT DONNEES MODELE PARMAJ          ######
######                                       ######

# Jeu de donnees entrant : histoire de vie des sites
#                          Premier tri donnees MNHN

# Jeu de donnees sortant : hvie_parmaj_tot
#                          hvie_ID_prog_parmaj_tot   


rm(list=ls())

# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 

# Data
load(here::here("output","data_STOC.RData"))

# 1. Selection de l'espece
#------------------------
# On ne garde que les parmaj
parmaj <- subset(data_STOC,data_STOC$ESPECE=="PARMAJ")
rm(data_STOC)

#Informations 
# Annees
years <- seq(min(unique(substr(parmaj$DATE,7,10))),max(unique(substr(parmaj$DATE,7,10))),1)
K <- length(years)
#Identifiant des parmaj 
ID_parmaj <- unique(parmaj$BAGUE)
N <- length(ID_parmaj)

# 2. Redefinition de l'age 
#-----------------------
unique(parmaj$AGE)

parmaj <- Mesanges::new_age(parmaj)

unique(parmaj$new_AGE)

# 3. Creation des histoires de vie
# -----------------------

# hvie
hvie_parmaj <- as.data.frame(matrix(NA,length(ID_parmaj),K))
hvie_parmaj$ID <- ID_parmaj
colnames(hvie_parmaj) <- c(years,"ID")

# Calcul hvie
hvie_parmaj <- Mesanges::hvie_tt(parmaj, hvie_parmaj)
N <- dim(hvie_parmaj)[1]

# On transforme en numerique : Adulte = 2, Jeune = 1
hvie_parmaj[hvie_parmaj=="A"] <- 2
hvie_parmaj[hvie_parmaj=="P"] <- 1
hvie_parmaj[hvie_parmaj=="C"] <- 3
hvie_parmaj[hvie_parmaj=="AP"] <- 4


# 4. On regarde s'il n'y a pas des individus vu jeunes plus tard que la premiere occasion de capture
#----------------------------------

hvie_parmaj <- Mesanges::check_age(hvie_parmaj)
N <- dim(hvie_parmaj)[1]


# 5. On regarde s'il reste des incertains
#------------------------------------------

# si vus une seule fois incertains, on retire la donnees.
check <- which(hvie_parmaj[,1:K]==3) # 84

# Si il est capture qu'une seule annee, on supprime
hvie_parmaj <- Mesanges::check_3(hvie_parmaj, check)

# Qui doit on trier au cas par cas :
check <- which(hvie_parmaj[,1:K]==3) # 2

# cas pas cas
ligne <- check[1]%%N
colonne <- ceiling(check[1]/N)
bague <- hvie_parmaj$ID[ligne] # ....4935116
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,colonne] <- 1

ligne <- check[2]%%N
colonne <- ceiling(check[2]/N)
bague <- hvie_parmaj$ID[ligne] #....6178095
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,colonne] <- 1

# Est-ce qu'on a fini ? 
check <- which(hvie_parmaj[,1:K]==3) # Oui

rm(colonne,ligne,bague,check)

# 6. On regarde ceux pour qui on a pas pu trancher entre Adulte et poussin
#----------------------------
check <- which(hvie_parmaj[,1:K]==4) # 59

# On retranscrit en numerique
parmaj$new_AGE[parmaj$new_AGE=="P"]<-1
parmaj$new_AGE[parmaj$new_AGE=="A"]<-2

# S'il ont ete vus qu'une seule annee, on prend le statut defini lors du bagage
# Si pas de baguage la seule annee ou il y a ete vu, on le regarde au cas par cas
# Si individus vus plusieurs annees on regarde au cas par cas
hvie_parmaj <- Mesanges::check_4(parmaj, hvie_parmaj, check)

# Qui doit on trier au cas par cas :
check <- which(hvie_parmaj[,1:K]==4) # 1 individu

#1
ligne <- check[1]%%N
colonne <- ceiling(check[1]/N)
bague <- hvie_parmaj$ID[ligne] #....7258709
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 1
hvie_parmaj[ligne,]

# On a fini ?
check <- which(hvie_parmaj[,1:K]==4) #oui

# On supprime les oiseaux qui ne sont finalement pas conserves
# avec uniquement des 0 dans les hvie
hvie_parmaj <- Mesanges::supp_ind(hvie_parmaj)

# On remet à jour N et ID_parmaj
N <- dim(hvie_parmaj)[1]
ID_parmaj <- hvie_parmaj$ID

rm(check,ligne,colonne,bague)


n_juv <- NULL
n_ad <- NULL
n_couple <- NULL


for(i in 1:19) {
  n_juv[i] <- length(which(hvie_parmaj[,i]==1))
  n_ad[i] <- length(which(hvie_parmaj[,i]==2))
  n_couple[i] <- (n_juv[i]/n_ad[i])*2
}

# 7. Gestion des transients
# #-----------------------

hvie_parmaj <- Mesanges::transient_tt(parmaj, hvie_parmaj)
# warnings : pas de probleme

# On supprime les oiseaux qui ne sont finalement pas conserves
# avec uniquement des 0 dans les hvie
hvie_parmaj <- Mesanges::supp_ind(hvie_parmaj)

# On remet à jour N et ID_parmaj
N <- dim(hvie_parmaj)[1]
ID_parmaj <- hvie_parmaj$ID

# 8. Covariable individuelle 
#---------

# Il nous faut le nombre de captures total (-1 pour transience)
# et le nombre d'annees de capture
# pour chaque individus des hvie
hvie_parmaj <- Mesanges::cov_ind_tt(parmaj, hvie_parmaj)


# Quels individus sont problematiques
which(hvie_parmaj$nb_years_capt==0) #0
which(hvie_parmaj$nb_capt==0) #0
which(is.na(hvie_parmaj$nb_years_capt)==T) #0
which(is.na(hvie_parmaj$nb_capt)==T) #0

#Calcul de la covariable :
hvie_parmaj <- Mesanges::calcul_cov_ind(hvie_parmaj)

which(hvie_parmaj$cov_ind<0) # ca parait bon

# 9. Hvie site
#-------------------------------------------------

load(here::here('output',"hvie_ID_PROG.RData"))

#Quels sont les sites concernant les parmaj
hvie_ID_PROG_parmaj <- hvie_ID_PROG %>%
  dplyr::filter(hvie_ID_PROG$ID_PROG %in% unique(hvie_parmaj$ID_PROG))

rm(hvie_ID_PROG)

# 10. type d'habitat
#----------------------------------------

# On ajoute les infos contenu dans CLC_STOC aux hvies 
load(here::here("output","CLC_STOC.RData"))
hvie_ID_PROG_parmaj <- dplyr::left_join(hvie_ID_PROG_parmaj, CLC_STOC, by = c("ID_PROG")) 

rm(CLC_STOC)  

# type d'habitat
hvie_ID_PROG_parmaj <- Mesanges::cov_hab(hvie_ID_PROG_parmaj)

# 11. Creation vecteur pour lier hvie individus et hvie site
#---------

hvie_parmaj <- Mesanges::link_hvie(hvie_parmaj, hvie_ID_PROG_parmaj)

# 12. Save
#--------------
save(hvie_parmaj,file=here::here('output',"hvie_parmaj_tot_tt.RData"))
save(hvie_ID_PROG_parmaj,file=here::here('output',"hvie_ID_PROG_parmaj_tot_tt.RData"))


# Analyses supplementaires 
#---------------------------
rm(list=ls())
load(here::here('output',"hvie_parmaj_tot_tt.RData"))
load(here::here('output',"hvie_ID_PROG_parmaj_tot_tt.RData"))

info_PROG <- hvie_ID_PROG_parmaj[,-c(1:19)] %>%
  dplyr::mutate(ID_PROG=as.factor(ID_PROG)) 

hvie_parmaj <- hvie_parmaj %>%
  dplyr::mutate(ID_PROG=as.factor(ID_PROG))

rm(hvie_ID_PROG_parmaj)

data <- dplyr::left_join(hvie_parmaj, info_PROG, by = c("ID_PROG")) 


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

mean(data_1$cov_ind)
mean(data_2$cov_ind)

n_juv <- NULL
n_ad <- NULL
n_couple <- NULL


for(i in 1:19) {
  n_juv[i] <- length(which(hvie_parmaj[,i]==1))
  n_ad[i] <- length(which(hvie_parmaj[,i]==2))
  n_couple[i] <- (n_juv[i]/n_ad[i])*2
}
