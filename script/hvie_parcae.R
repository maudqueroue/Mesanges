
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
# On ne garde que les parmaj
parcae <- subset(data_STOC,data_STOC$ESPECE=="PARCAE")
rm(data_STOC)

#Informations 
# Annees
years <- seq(min(unique(substr(parcae$DATE,7,10))),max(unique(substr(parcae$DATE,7,10))),1)
K <- length(years)
#Identifiant des parmaj 
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

hvie_parcae <- Mesanges::transient(parcae, hvie_parcae)

# On supprime les individus qui ne sont pas vu autrement que transients
hvie_parcae <- Mesanges::supp_ind(hvie_parcae)

# On remet à jour N et ID_parmaj
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

# On remet à jour N et ID_parmaj
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
# hvie station 96 : premiere occasion de capture donc retiree pour transient
#On garde hvie pour station 97
hvie_parcae[ind_double_site[1],]
hvie_parcae$ID_PROG[ind_double_site[1]] <- 97
hvie_parcae$nb_site[ind_double_site[1]] <- 1
hvie_parcae[ind_double_site[1],] 

#2#....5311094
# Si on le considere comme deux individus differents : a retirer pour transience
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[2]])
# hvie station 13  : uniquement vu une fois : transient ? 
# hvie station 898 : uniquement vu une fois : transient ? 
hvie_parcae[ind_double_site[2],]
hvie_parcae[ind_double_site[2],1:K] <- rep(0,19)
hvie_parcae[ind_double_site[2],]

#3# ....4897437
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[3]])
# hvie station 64  : premiere occasion de capture donc retiree pour transient
# hvie station 65 : vu une fois et meme coord
hvie_parcae[ind_double_site[3],]
hvie_parcae$ID_PROG[ind_double_site[3]] <- 65
hvie_parcae$nb_site[ind_double_site[3]] <- 1
hvie_parcae[ind_double_site[3],]

#4# ....5582026 
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[4]])
# hvie station 150  : uniquement vu une fois : transient ? 
# hvie station 149 : uniquement vu une fois : transient ? 
hvie_parcae[ind_double_site[4],]
hvie_parcae[ind_double_site[4],1:K] <- rep(0,19)
hvie_parcae[ind_double_site[4],]

#5# ....6175994
# Si on le considere comme deux individus differents : a retirer pour transience
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[5]])
# hvie station 194  : uniquement vu une fois : transient ? 
# hvie station 106 : uniquement vu une fois : transient ? 
hvie_parcae[ind_double_site[5],] 
hvie_parcae[ind_double_site[5],1:K] <- rep(0,19)
hvie_parcae[ind_double_site[5],] 

#6# ....6470371
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[6]])
# hvie station 97 : vu deux fois
# hvie station 199 : vu une fois et meme coord
hvie_parcae[ind_double_site[6],] 
hvie_parcae$ID_PROG[ind_double_site[6]] <- 97
hvie_parcae$nb_site[ind_double_site[6]] <- 1
hvie_parcae[ind_double_site[6],] 

#7# ....6470015
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[7]])
# hvie station 96 : premiere occasion de capture donc retiree pour coherence transient 
# Exactement meme coordonnees et vu deux fois donc pas transient sur 97
hvie_parcae[ind_double_site[7],] 
hvie_parcae$ID_PROG[ind_double_site[7]] <- 97
hvie_parcae$nb_site[ind_double_site[7]] <- 1
hvie_parcae[ind_double_site[7],] 

#8# ....6552574
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[8]])
# hvie station 276 : premiere occasion de capture donc retiree pour coherence transient 
# Exactement meme coordonnees donc pas transient sur 277
hvie_parcae[ind_double_site[8],] 
hvie_parcae$ID_PROG[ind_double_site[8]] <- 277
hvie_parcae$nb_site[ind_double_site[8]] <- 1
hvie_parcae[ind_double_site[8],] 

#9# ....7572587
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[9]])
# hvie station 685 : vu uen fois : transient ?
# hvie station 684 : vu une fois : transient ?
hvie_parcae[ind_double_site[9],] 
hvie_parcae[ind_double_site[9],1:K] <- rep(0,19)
hvie_parcae[ind_double_site[9],] 

#10# ....7195128
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[10]])
# hvie station 199 : premiere occasion de capture donc retiree pour coherence transient 
# Exactement meme coordonnees donc pas transient sur 97
hvie_parcae[ind_double_site[10],] 
hvie_parcae$ID_PROG[ind_double_site[10]] <- 97
hvie_parcae$nb_site[ind_double_site[10]] <- 1
hvie_parcae[ind_double_site[10],] 

#11# ....8008308
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[11]])
# hvie station 199 : premiere occasion de capture donc retiree pour coherence transient 
# vu deux fois sur 97
hvie_parcae[ind_double_site[11],] 
hvie_parcae$ID_PROG[ind_double_site[11]] <- 97
hvie_parcae$nb_site[ind_double_site[11]] <- 1
hvie_parcae[ind_double_site[11],] 

#12# ....8748761
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_double_site[12]])
# hvie station 298 : vu uen fois : transient ?
# hvie station 287 : vu une fois : transient ?
hvie_parcae[ind_double_site[12],] 
hvie_parcae[ind_double_site[12],1:K] <- rep(0,19)
hvie_parcae[ind_double_site[12],] 

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

hvie_parcae <- Mesanges::cov_ind(parcae, hvie_parcae)

# Quels individus sont problematiques
which(hvie_parcae$nb_years_capt==0) #0
which(hvie_parcae$nb_capt==0) #0
which(is.na(hvie_parcae$nb_years_capt)==T) #0
ind_pb <- which(is.na(hvie_parcae$nb_capt)==T)

# cas par cas

#1
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[1]])
# ....4774276
hvie_parcae$nb_capt[ind_pb[1]] <- 1

#2
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[2]])
#....4897437
hvie_parcae$nb_capt[ind_pb[2]] <- 1

#3
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[3]])
# ....6470371
hvie_parcae$nb_capt[ind_pb[3]] <- 2

#4
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[4]])
# ....6470015
hvie_parcae$nb_capt[ind_pb[4]] <- 2

#5
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[5]])
# ....6552574
hvie_parcae$nb_capt[ind_pb[5]] <- 1

#6
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[6]])
#....7195128
hvie_parcae$nb_capt[ind_pb[6]] <- 1

#7
subset(parcae,parcae$BAGUE==hvie_parcae$ID[ind_pb[7]])
# ....8008308
hvie_parcae$nb_capt[ind_pb[7]] <- 2

#Calcul de la covariable :
hvie_parcae <- Mesanges::calcul_cov_ind(hvie_parcae)

which(hvie_parcae$cov_ind<0) # ca parait bon

rm(ind_pb)

# 9. Hvie site
#-------------------------------------------------

load(here::here('output',"hvie_ID_PROG.RData"))

#Quels sont les sites concernant les parmaj
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


