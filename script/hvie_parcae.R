
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
hvie_parcae_tot <- Mesanges::hvie(parcae, hvie_parcae)
hvie_parcae <- hvie_parcae_tot[[1]]

# check
d <- hvie_parcae_tot[[2]]
d2 <- hvie_parcae_tot[[3]]
check <- which(d>1)
d2[check]

#Quels sont les statuts
length(which(d2=="AP"))
length(which(d2=="PA"))
length(which(d2=="PC"))
length(which(d2=="CP"))
length(which(d2=="AC"))
length(which(d2=="CA"))

rm(d,d2,check,hvie_parcae_tot)

#########################

# On transforme en numeric : Adulte = 2, Jeune = 1
hvie_parcae[hvie_parcae=="A"] <- 2
hvie_parcae[hvie_parcae=="P"] <- 1
hvie_parcae[hvie_parcae=="C"] <- 3
hvie_parcae[hvie_parcae=="AP"] <- 4

# 4. Gestion des transients
#-----------------------
#hvie_parcae_1 <- Mesanges::transient(parcae, hvie_parcae)
hvie_parcae <- Mesanges::transient_new(parcae, hvie_parcae)

# On supprime les individus qui ne sont pas vu autrement que transients
hvie_parcae <- Mesanges::supp_ind(hvie_parcae)

# On remet à jour N et ID_parcae
N <- dim(hvie_parcae)[1]
ID_parcae <- hvie_parcae$ID


# 5. On regarde s'il reste des incertains
#------------------------------------------
check <- which(hvie_parcae==3)

# 1 à faire au cas pas cas
# --> 
ligne <- check[1]%%N
colonne <- ceiling(check[1]/N)
bague <- hvie_parcae$ID[ligne] # "....3387269"
subset(parcae,parcae$BAGUE==bague)
hvie_parcae[ligne,colonne] <- 0

# Est-ce qu'on a fini ? 
check <- which(hvie_parcae==3) # Oui

rm(colonne,ligne,bague,check)

# 6. On regarde ceux pour qui on a pas pu trancher entre Adulte et poussin
#----------------------------
check <- which(hvie_parcae==4)

# On retranscrit en numerique
parcae$new_AGE[parcae$new_AGE=="P"]<-1
parcae$new_AGE[parcae$new_AGE=="A"]<-2

# S'il ont ete vus qu'une seule annee, on prend le statut defini lors du bagage
# Si pas de baguage la seule annee ou il y a ete vu, on le regarde au cas par cas
# Si individus vus plusieurs annees on regarde au cas par cas
hvie_parcae <- Mesanges::check_4(parcae, hvie_parcae, check)

# Qui doit on trier au cas par cas :
check <- which(hvie_parcae==4) # 3 individus

#1
ligne <- check[1]%%N
colonne <- ceiling(check[1]/N)
bague <- hvie_parcae$ID[ligne] #"....4301563"
subset(parcae,parcae$BAGUE==bague)
hvie_parcae[ligne,]
hvie_parcae[ligne,colonne] <- 2
hvie_parcae[ligne,]

#2
ligne <- check[2]%%N
colonne <- ceiling(check[2]/N)
bague <- hvie_parcae$ID[ligne] # "....3840277"
subset(parcae,parcae$BAGUE==bague)
hvie_parcae[ligne,]
hvie_parcae[ligne,colonne] <- 1
hvie_parcae[ligne,]

#3
ligne <- check[3]%%N
colonne <- ceiling(check[3]/N)
bague <- hvie_parcae$ID[ligne] #"....7638994"
subset(parcae,parcae$BAGUE==bague)
hvie_parcae[ligne,]
hvie_parcae[ligne,colonne] <- 2
hvie_parcae[ligne,]

# On a fini ?
check <- which(hvie_parcae==4) #oui

# On supprime les oiseaux qui ne sont finalement pas conserves
# avec uniquement des 0 dans les hvie
hvie_parcae <- Mesanges::supp_ind(hvie_parcae)

# On remet à jour N et ID_parcae
N <- dim(hvie_parcae)[1]
ID_parcae <- hvie_parcae$ID

rm(check,ligne,colonne,bague)


# 7. Stations STOC
# -------------------------

# A quelle station appartient chaque oiseau de la matrice des histoires de vie

hvie_parcae <- Mesanges::ind_site(parcae, hvie_parcae)

# combien d'individus vu sur deux sites differents
ind_double_site <- which(hvie_parcae$nb_site>1) # 12 individus

# Gestion au cas par cas 
# Pour tous les individus vus deux fois on separe leurs histoires de capture selon les sites

#1#....4774276 
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[1]])
hvie_parcae[ind_double_site[1],]
# hvie station 97 : vu une fois jeune 
hvie_parcae$ID_PROG[ind_double_site[1]] <- 97
hvie_parcae$nb_site[ind_double_site[1]] <- 1
# hvie station 96 : vu une fois jeune 
hvie_new <- hvie_parcae[ind_double_site[1],]
hvie_new$ID_PROG <- 96 
hvie_parcae <- rbind(hvie_parcae,hvie_new)
rm(hvie_new)

#2#....5311094  
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[2]])
hvie_parcae[ind_double_site[2],]
# hvie station 13 : premiere annee de capture vu jeune : on garde
hvie_parcae$ID_PROG[ind_double_site[2]] <- 13
hvie_parcae$nb_site[ind_double_site[2]] <- 1
# hvie station 898 : deuxieme annee de capture vu jeune : impossible il est adulte
hvie_parcae[ind_double_site[2],9] <- 0
hvie_parcae[ind_double_site[2],] 

#3# ....4897437 
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[3]])
hvie_parcae[ind_double_site[3],]
# vu deux annees, adulte, sur sites differents avec meme coordonnees
# hvie station 65  : vu une fois adulte : transient
# hvie station 64 : vu une fois adulte : transient 
hvie_parcae[ind_double_site[3],1:19] <- rep(0,19)
hvie_parcae[ind_double_site[3],]

#4# ....5582026 
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[4]])
hvie_parcae[ind_double_site[4],]
# hvie station 150 : vu une fois jeune
hvie_parcae$ID_PROG[ind_double_site[4]] <- 150
hvie_parcae$nb_site[ind_double_site[4]] <- 1
# hvie station 149 : vu une fois jeune
hvie_new <- hvie_parcae[ind_double_site[4],]
hvie_new$ID_PROG <- 149 
hvie_parcae <- rbind(hvie_parcae,hvie_new)
rm(hvie_new)

#5# ....6175994 
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[5]])
hvie_parcae[ind_double_site[5],] 
# hvie station 106 : premiere capture jeune
hvie_parcae$ID_PROG[ind_double_site[5]] <- 106
hvie_parcae$nb_site[ind_double_site[5]] <- 1
# hvie station 194 : deuxième annee de capture adulte : transient
hvie_parcae[ind_double_site[5],11] <- 0
hvie_parcae[ind_double_site[5],] 

#6# ....6470371 
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[6]])
hvie_parcae[ind_double_site[6],] 
# hvie station 96 : premiere occasion de capture, vu une fois, jeune, on garde
hvie_parcae$ID_PROG[ind_double_site[6]] <- 96
hvie_parcae$nb_site[ind_double_site[6]] <- 1
hvie_parcae[ind_double_site[6],]
# hvie station 199 : vu une fois , adulte : transient
# hvie station 97 : vu une fois , adulte : transient
hvie_parcae[ind_double_site[6],11] <- 0
hvie_parcae[ind_double_site[6],]

#7# ....6470015 
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[7]])
hvie_parcae[ind_double_site[7],] 
# hvie station 96 : premiere occasion de capture jeune, vu une fois 
hvie_parcae$ID_PROG[ind_double_site[7]] <- 96
hvie_parcae$nb_site[ind_double_site[7]] <- 1
hvie_parcae[ind_double_site[7],12] <- 0
# hvie station 97 : vu deux fois, adulte
# on garde 97 car vu plus de fois
hvie_new <- hvie_parcae[ind_double_site[7],]
hvie_new$ID_PROG <- 97 
hvie_new[,10] <- 0
hvie_new[,12] <- 2
hvie_parcae <- rbind(hvie_parcae,hvie_new)
rm(hvie_new)

#8# ....6552574  
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[8]])
hvie_parcae[ind_double_site[8],] 
# vu deux annees differentes, jeune et adulte, sur deux site different avec meme coordonnees
# hvie station 276 : premiere occasion de capture jeune 
hvie_parcae$ID_PROG[ind_double_site[8]] <- 276
hvie_parcae$nb_site[ind_double_site[8]] <- 1
# hvie station 277 : une seule capture adulte : transient
hvie_parcae[ind_double_site[8],14] <- 0 
hvie_parcae[ind_double_site[8],] 

#9# ....7572587
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[9]])
hvie_parcae[ind_double_site[9],] 
# hvie station 685 : vu une fois jeune
hvie_parcae$ID_PROG[ind_double_site[9]] <- 685
hvie_parcae$nb_site[ind_double_site[9]] <- 1
# hvie station 684 : vu une fois jeune
hvie_new <- hvie_parcae[ind_double_site[9],]
hvie_new$ID_PROG <- 684 
hvie_parcae <- rbind(hvie_parcae,hvie_new)
rm(hvie_new)

#10# ....7195128 
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[10]])
hvie_parcae[ind_double_site[10],] 
# hvie station 199 : premiere occasion de capture jeune : on garde 
hvie_parcae$ID_PROG[ind_double_site[10]] <- 199
hvie_parcae$nb_site[ind_double_site[10]] <- 1
# hvie station 97 : vu une fois, adulte, transient
hvie_parcae[ind_double_site[10],15] <- 0 
hvie_parcae[ind_double_site[10],] 

#11# ....8008308 
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[11]])
hvie_parcae[ind_double_site[11],] 
# hvie station 199 : premiere occasion de capture, jeune 
hvie_parcae$ID_PROG[ind_double_site[11]] <- 199
hvie_parcae$nb_site[ind_double_site[11]] <- 1
hvie_parcae[ind_double_site[11],18] <- 0 
# hvie station 97 : vu deux fois, adulte
hvie_new <- hvie_parcae[ind_double_site[11],]
hvie_new$ID_PROG <- 97 
hvie_new[,17] <- 0
hvie_new[,18] <- 2
hvie_parcae <- rbind(hvie_parcae,hvie_new)
rm(hvie_new)

#12# ....8748761
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[12]])
hvie_parcae[ind_double_site[12],] 
# hvie station 298 : vu une fois
hvie_parcae$ID_PROG[ind_double_site[12]] <- 298
hvie_parcae$nb_site[ind_double_site[12]] <- 1
# hvie station 287 : vu une fois
hvie_new <- hvie_parcae[ind_double_site[12],]
hvie_new$ID_PROG <- 287 
hvie_parcae <- rbind(hvie_parcae,hvie_new)
rm(hvie_new) 

# On supprime les oiseaux qui ne sont finalement pas conserves
# avec uniquement des 0 dans les hvie
hvie_parcae <- Mesanges::supp_ind(hvie_parcae)

# On remet à jour N et ID_parmaj
N <- dim(hvie_parcae)[1]
ID_parcae <- hvie_parcae$ID

# combien d'individus vu sur deux sites differents
ind_double_site <- which(hvie_parcae$nb_site>1) # Plus aucun ! c'est tres bien
rm(ind_double_site)

# On peut supprimer la colonne nb_site qui n'est plus utile
hvie_parcae <- hvie_parcae %>%
  dplyr::select(!nb_site)

# 8. Covariable individuelle 
#---------

# Il nous faut le nombre de captures total (-1 pour transience)
# et le nombre d'annees de capture
# pour chaque individus des hvie

#hvie_parcae_1 <- Mesanges::cov_ind(parcae, hvie_parcae)
hvie_parcae <- Mesanges::cov_ind_new(parcae, hvie_parcae)


# Quels individus sont problematiques
which(hvie_parcae$nb_years_capt==0) #0
which(hvie_parcae$nb_capt==0) #0
which(is.na(hvie_parcae$nb_years_capt)==T) #0
ind_pb <- which(is.na(hvie_parcae$nb_capt)==T) #17

# cas par cas

#1
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[1]])
# ....4774276
hvie_parcae[ind_pb[1],]
hvie_parcae$nb_capt[ind_pb[1]] <- 1

#2
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[2]])
#....5311094
hvie_parcae[ind_pb[2],]
hvie_parcae$nb_capt[ind_pb[2]] <- 1

#3
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[3]])
#....5582026
hvie_parcae[ind_pb[3],]
hvie_parcae$nb_capt[ind_pb[3]] <- 1

#4
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[4]])
# ....6175994
hvie_parcae[ind_pb[4],]
hvie_parcae$nb_capt[ind_pb[4]] <- 1

#5
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[5]])
# ....6470371
hvie_parcae[ind_pb[5],]
hvie_parcae$nb_capt[ind_pb[5]] <- 1

#6
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[6]])
# ....6470015
hvie_parcae[ind_pb[6],]
hvie_parcae$nb_capt[ind_pb[6]] <- 1

#7
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[7]])
# ....6552574
hvie_parcae[ind_pb[7],]
hvie_parcae$nb_capt[ind_pb[7]] <- 1

#8
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[8]])
# ....7572587
hvie_parcae[ind_pb[8],]
hvie_parcae$nb_capt[ind_pb[8]] <- 1

#9
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[9]])
#....7195128
hvie_parcae[ind_pb[9],]
hvie_parcae$nb_capt[ind_pb[9]] <- 1

#10
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[10]])
#....8008308
hvie_parcae[ind_pb[10],]
hvie_parcae$nb_capt[ind_pb[10]] <- 1

#11
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[11]])
# ....8748761
hvie_parcae[ind_pb[11],]
hvie_parcae$nb_capt[ind_pb[11]] <- 1

#12
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[12]])
# ....4774276
hvie_parcae[ind_pb[12],]
hvie_parcae$nb_capt[ind_pb[12]] <- 1

#13
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[13]])
# ....5582026
hvie_parcae[ind_pb[13],]
hvie_parcae$nb_capt[ind_pb[13]] <- 1

#14
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[14]])
# ....6470015
hvie_parcae[ind_pb[14],]
hvie_parcae$nb_capt[ind_pb[14]] <- 1

#15
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[15]])
# ....7572587
hvie_parcae[ind_pb[15],]
hvie_parcae$nb_capt[ind_pb[15]] <- 1

#16
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[16]])
# ....8008308
hvie_parcae[ind_pb[16],]
hvie_parcae$nb_capt[ind_pb[16]] <- 1

#17
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[17]])
# ....8748761
hvie_parcae[ind_pb[17],]
hvie_parcae$nb_capt[ind_pb[17]] <- 1

#Calcul de la covariable :
hvie_parcae <- Mesanges::calcul_cov_ind(hvie_parcae)

which(hvie_parcae$cov_ind<0) # ca parait bon

rm(ind_pb)

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
save(hvie_parcae,file=here::here('output',"hvie_parcae_tot.RData"))
save(hvie_ID_PROG_parcae,file=here::here('output',"hvie_ID_PROG_parcae_tot.RData"))



# Analyses supplementaires 
#---------------------------
rm(list=ls())
load(here::here('output',"hvie_parcae_tot.RData"))
load(here::here('output',"hvie_ID_PROG_parcae_tot.RData"))

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

