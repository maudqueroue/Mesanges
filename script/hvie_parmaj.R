
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
hvie_parmaj_tot <- Mesanges::hvie(parmaj, hvie_parmaj)
hvie_parmaj <- hvie_parmaj_tot[[1]]

# check
d <- hvie_parmaj_tot[[2]]
d2 <- hvie_parmaj_tot[[3]]
check <- which(d>1)
d2[check]

#Quels sont les statuts
length(which(d2=="AP"))
length(which(d2=="PA"))
length(which(d2=="PC"))
length(which(d2=="CP"))
length(which(d2=="AC"))
length(which(d2=="CA"))

rm(d,d2,check,hvie_parmaj_tot)

#########################

# On transforme en numeric : Adulte = 2, Jeune = 1
hvie_parmaj[hvie_parmaj=="A"] <- 2
hvie_parmaj[hvie_parmaj=="P"] <- 1
hvie_parmaj[hvie_parmaj=="C"] <- 3
hvie_parmaj[hvie_parmaj=="AP"] <- 4

# 4. Gestion des transients
#-----------------------
#hvie_parmaj_1 <- Mesanges::transient(parmaj, hvie_parmaj)
hvie_parmaj <- Mesanges::transient_new(parmaj, hvie_parmaj)

# On supprime les individus qui ne sont pas vu autrement que transients
hvie_parmaj <- Mesanges::supp_ind(hvie_parmaj)

# On remet à jour N et ID_parmaj
N <- dim(hvie_parmaj)[1]
ID_parmaj <- hvie_parmaj$ID


# 5. On regarde s'il reste des incertains
#------------------------------------------
check <- which(hvie_parmaj==3)

# Si il est capture qu'une seule annee, on supprime
hvie_parmaj <- Mesanges::check_3(hvie_parmaj, check)

# On regarde s'il reste des incertains :
check <- which(hvie_parmaj==3)  

# 2 à faire au cas pas cas

#1.
ligne <- check[1]%%N
colonne <- ceiling(check[1]/N)
bague <- hvie_parmaj$ID[ligne] #"....4935116"
subset(parmaj,parmaj$BAGUE==bague)
# On ne peut pas conclure sur son age a la première capture : on met 0
hvie_parmaj[ligne,colonne] <- 0

#2.
ligne <- check[2]%%N
colonne <- ceiling(check[2]/N)
bague <- hvie_parmaj$ID[ligne] #"....5186424"
subset(parmaj,parmaj$BAGUE==bague)
# On peut conclure sur son age car deuxieme annee de capture : on met 2
hvie_parmaj[ligne,colonne] <- 2

# Est-ce qu'on a fini ? 
check <- which(hvie_parmaj==3) # Oui

rm(colonne,ligne,bague,check)

# 6. On regarde ceux pour qui on a pas pu trancher entre Adulte et poussin
#----------------------------
check <- which(hvie_parmaj==4)

# On retranscrit en numerique
parmaj$new_AGE[parmaj$new_AGE=="P"]<-1
parmaj$new_AGE[parmaj$new_AGE=="A"]<-2

# S'il ont ete vus qu'une seule annee, on prend le statut defini lors du bagage
# Si pas de baguage la seule annee ou il y a ete vu, on le regarde au cas par cas
# Si individus vus plusieurs annees on regarde au cas par cas
hvie_parmaj <- Mesanges::check_4(parmaj, hvie_parmaj, check)

# Qui doit on trier au cas par cas :
check <- which(hvie_parmaj==4) # 9 individus

# 1
ligne <- check[1]%%N
colonne <- ceiling(check[1]/dim(hvie_parmaj)[1])
bague <- hvie_parmaj$ID[ligne] #"....5616189"
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 1

#2.
ligne <- check[2]%%N
colonne <- ceiling(check[2]/dim(hvie_parmaj)[1])
bague <- hvie_parmaj$ID[ligne] #....5582454
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 2

#3.
ligne <- check[3]%%N
colonne <- ceiling(check[3]/dim(hvie_parmaj)[1])
bague <- hvie_parmaj$ID[ligne] #....6837244
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 1

#4.
ligne <- check[4]%%N
colonne <- ceiling(check[4]/dim(hvie_parmaj)[1])
bague <- hvie_parmaj$ID[ligne] #....6576415 
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 2

#5.
ligne <- check[5]%%N
colonne <- ceiling(check[5]/dim(hvie_parmaj)[1])
bague <- hvie_parmaj$ID[ligne] #....7686263
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 2

#6.
ligne <- check[6]%%N
colonne <- ceiling(check[6]/dim(hvie_parmaj)[1])
bague <- hvie_parmaj$ID[ligne] #....6743149
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 1

#7.
ligne <- check[7]%%N
colonne <- ceiling(check[7]/dim(hvie_parmaj)[1])
bague <- hvie_parmaj$ID[ligne] #....7258709
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 1

#8.
ligne <- check[8]%%N
colonne <- ceiling(check[8]/dim(hvie_parmaj)[1])
bague <- hvie_parmaj$ID[ligne] #....7612498
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 2

#9.
ligne <- check[9]%%N
colonne <- ceiling(check[9]/dim(hvie_parmaj)[1])
bague <- hvie_parmaj$ID[ligne] #....8178504
subset(parmaj,parmaj$BAGUE==bague)
hvie_parmaj[ligne,]
hvie_parmaj[ligne,colonne] <- 2

# On a fini ?
check <- which(hvie_parmaj==4) #oui

# On supprime les oiseaux qui ne sont finalement pas conserves
# avec uniquement des 0 dans les hvie
hvie_parmaj <- Mesanges::supp_ind(hvie_parmaj)

# On remet à jour N et ID_parmaj
N <- dim(hvie_parmaj)[1]
ID_parmaj <- hvie_parmaj$ID

rm(check,ligne,colonne,bague)


# 7. Stations STOC
# -------------------------

# A quelle station appartient chaque oiseau de la matrice des histoires de vie

hvie_parmaj <- Mesanges::ind_site(parmaj, hvie_parmaj)
  
# Gestion des oiseaux vus sur deux sites differents

# combien d'individus vu sur deux sites differents
ind_double_site <- which(hvie_parmaj$nb_site>1) # 18 individus

# Gestion au cas par cas 
# Pour tous les individus vus deux fois on separe leurs histoires de capture selon les sites

#-------1#....4514091
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[1]])
hvie_parmaj[ind_double_site[1],]

# hvie station 97 : vu une fois jeune
hvie_parmaj$ID_PROG[ind_double_site[1]] <- 97
hvie_parmaj$nb_site[ind_double_site[1]] <- 1
hvie_parmaj[ind_double_site[1],] 

# hvie station 96 : vu deux fois jeune
hvie_new <- hvie_parmaj[ind_double_site[1],]
hvie_new$ID_PROG <- 96  
hvie_parmaj <- rbind(hvie_parmaj,hvie_new)
rm(hvie_new)

#---------2#....4585897
# Si on le considere comme deux individus differents : a retirer pour transience
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[2]])
hvie_parmaj[ind_double_site[2],]

# hvie station 13  : premiere capture, jeune
hvie_parmaj$ID_PROG[ind_double_site[2]] <- 13
hvie_parmaj$nb_site[ind_double_site[2]] <- 1

# hvie station 898 : vu une seule fois adulte : a retirer pour transience
hvie_parmaj[ind_double_site[2],9] <- 0 
hvie_parmaj[ind_double_site[2],] 

#---------3# JA...283154
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[3]])
hvie_parmaj[ind_double_site[3],]

# hvie station 13  : uniquement vu une fois adulte : transient ? 
# hvie station 898 : uniquement vu une fois adulte : transient ? 
# On retire l'individu pour respecter la transience
hvie_parmaj[ind_double_site[3],1:K] <- rep(0,19)
hvie_parmaj[ind_double_site[3],]

#----------4# ....5309934 
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[4]])
hvie_parmaj[ind_double_site[4],]
# hvie station 192 : erreur d'identification de la station 
# On garde le nom 162
hvie_parmaj$ID_PROG[ind_double_site[4]] <- 162
hvie_parmaj$nb_site[ind_double_site[4]] <- 1
hvie_parmaj[ind_double_site[4],] 

#-----------5# ....5608045
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[5]])
hvie_parmaj[ind_double_site[5],] 

# hvie station 150  : uniquement vu une fois jeune
hvie_parmaj$ID_PROG[ind_double_site[5]] <- 150
hvie_parmaj$nb_site[ind_double_site[5]] <- 1
hvie_parmaj[ind_double_site[5],] 

# hvie station 149 : uniquement vu une fois jeune : on garde
hvie_new <- hvie_parmaj[ind_double_site[5],]
hvie_new$ID_PROG <- 149  
hvie_parmaj <- rbind(hvie_parmaj,hvie_new)
rm(hvie_new)  

#-----------6# ....5241231
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[6]])
hvie_parmaj[ind_double_site[6],] 

# hvie station 60 : une capture jeune : on garde
hvie_parmaj$ID_PROG[ind_double_site[6]] <- 60
hvie_parmaj$nb_site[ind_double_site[6]] <- 1

# hvie station 269 : une capture :adulte : transient 
hvie_parmaj[ind_double_site[6],10] <- 0 
hvie_parmaj[ind_double_site[6], ] 

#--------7 # ....5241274
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[7]])
hvie_parmaj[ind_double_site[7],] 
# hvie station 269  : uniquement vu une fois adulte : transient ? 
# hvie station 60 : uniquement vu une fois adulte : transient ? 
# On retire l'individu pour respecter la transience
hvie_parmaj[ind_double_site[7],1:K] <- rep(0,19)
hvie_parmaj[ind_double_site[7],]

#--------8# ....5353624
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[8]])
hvie_parmaj[ind_double_site[8],] 
# hvie station 269 : premiere occasion de capture, jeune 
hvie_parmaj$ID_PROG[ind_double_site[8]] <- 269
hvie_parmaj$nb_site[ind_double_site[8]] <- 1
hvie_parmaj[ind_double_site[8], 12] <- 0

# hvie station 610 : vu deux fois, adulte : on garde
hvie_new <- hvie_parmaj[ind_double_site[8],]
hvie_new$ID_PROG <- 610 
hvie_new[,10] <- 0
hvie_new[,12] <- 2
hvie_parmaj <- rbind(hvie_parmaj,hvie_new)
rm(hvie_new)

#--------9# ....6503619
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[9]])
hvie_parmaj[ind_double_site[9],] 
# hvie station 97 : vu une fois jeune : on garde 
hvie_parmaj$ID_PROG[ind_double_site[9]] <- 97
hvie_parmaj$nb_site[ind_double_site[9]] <- 1
# hvie station 199 : vu une fois jeune : on garde
hvie_new <- hvie_parmaj[ind_double_site[9],]
hvie_new$ID_PROG <- 199 
hvie_parmaj <- rbind(hvie_parmaj,hvie_new)
rm(hvie_new) 

#--------10# ....6552416
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[10]])
hvie_parmaj[ind_double_site[10],] 
# hvie station 154 : erreur d'identification de la station 
# On garde le nom 269
hvie_parmaj$ID_PROG[ind_double_site[10]] <- 269
hvie_parmaj$nb_site[ind_double_site[10]] <- 1
hvie_parmaj[ind_double_site[10],] 

#-------11# ....2026801
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[11]])
hvie_parmaj[ind_double_site[11],] 
# hvie station 269 : capture jeune : on garde
hvie_parmaj$ID_PROG[ind_double_site[11]] <- 269
hvie_parmaj$nb_site[ind_double_site[11]] <- 1
# hvie station 610 : capture adulte : transient
hvie_parmaj[ind_double_site[11],13] <- 0
hvie_parmaj[ind_double_site[11],] 

#-------12# ....6715876
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[12]])
hvie_parmaj[ind_double_site[12],] 
# hvie station 192 : erreur d'identification de la station 
# On garde le nom 162
hvie_parmaj$ID_PROG[ind_double_site[12]] <- 162
hvie_parmaj$nb_site[ind_double_site[12]] <- 1
hvie_parmaj[ind_double_site[12],] 

#--------13# ....6863092
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[13]])
hvie_parmaj[ind_double_site[13],] 
# hvie station 192 : premiere occasion de capture jeune : on garde
hvie_parmaj$ID_PROG[ind_double_site[13]] <- 192
hvie_parmaj$nb_site[ind_double_site[13]] <- 1
hvie_parmaj[ind_double_site[13],15] <- 0 

# hvie 849 : vu plusieurs fois adulte :on garde
# hvie station 199 : vu une fois jeune : on garde
hvie_new <- hvie_parmaj[ind_double_site[13],]
hvie_new$ID_PROG <- 849 
hvie_new[, 13] <- 0
hvie_new[, 15] <- 2
hvie_parmaj <- rbind(hvie_parmaj,hvie_new)
rm(hvie_new)

#---------14# ....7521243
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[14]])
hvie_parmaj[ind_double_site[14],] 
# hvie station 242 : erreur d'identification de la station
# On garde le nom 83
hvie_parmaj$ID_PROG[ind_double_site[14]] <- 83
hvie_parmaj$nb_site[ind_double_site[14]] <- 1
hvie_parmaj[ind_double_site[14],] 

#----------15# ....7686263
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[15]])
hvie_parmaj[ind_double_site[15],] 
# hvie station 136 : Une seule capture, jeune 
hvie_parmaj$ID_PROG[ind_double_site[15]] <- 136
hvie_parmaj$nb_site[ind_double_site[15]] <- 1
hvie_parmaj[ind_double_site[15],16] <- 0 
# hvie station 845 : car vu plusieurs fois, deux annees differentes, adulte
hvie_new <- hvie_parmaj[ind_double_site[15],]
hvie_new$ID_PROG <- 845 
hvie_new[, 15] <- 0
hvie_new[, 16] <- 2
hvie_parmaj <- rbind(hvie_parmaj,hvie_new)
rm(hvie_new)

#--------16# ....6482156
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[16]])
hvie_parmaj[ind_double_site[16],]
# hvie station 667 : Une seule capture adulte : transient
# hvie station station 246 : deux captures jeune puis adulte
hvie_parmaj$ID_PROG[ind_double_site[16]] <- 246
hvie_parmaj$nb_site[ind_double_site[16]] <- 1
# On supprime 667 pour cohérence transient
hvie_parmaj[ind_double_site[16],17] <- 0
hvie_parmaj[ind_double_site[16],]

#------17# ....6747503
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[17]])
hvie_parmaj[ind_double_site[17],] 
# vu deux site differents, jeune puis adulte, deux annnes differentes
# hvie station 915 : Une seule capture adulte : transient
# hvie station 149 : Vu plusieurs fois jeune : on garde
hvie_parmaj$ID_PROG[ind_double_site[17]] <- 149
hvie_parmaj$nb_site[ind_double_site[17]] <- 1
# mais attention pour coherence transient on ne garde pas la derniere capture 915
hvie_parmaj[ind_double_site[17],15] <- 0
hvie_parmaj[ind_double_site[17],] 

#----------18# ....7612498
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_double_site[18]])
hvie_parmaj[ind_double_site[18],]
# hvie station 848 : Une seule capture adulte : transient
# hvie station 639 : Vu plusieurs fois : on garde
hvie_parmaj$ID_PROG[ind_double_site[18]] <- 639
hvie_parmaj$nb_site[ind_double_site[18]] <- 1
# mais attention pour coherence transient on ne garde pas la derniere capture 848
hvie_parmaj[ind_double_site[18],18] <- 0
hvie_parmaj[ind_double_site[18],]

# On supprime les oiseaux qui ne sont finalement pas conserves
# avec uniquement des 0 dans les hvie
hvie_parmaj <- Mesanges::supp_ind(hvie_parmaj)

# On remet à jour N et ID_parmaj
N <- dim(hvie_parmaj)[1]
ID_parmaj <- hvie_parmaj$ID

# combien d'individus vu sur deux sites differents
ind_double_site <- which(hvie_parmaj$nb_site>1) # Plus aucun ! c'est tres bien
rm(ind_double_site)

# On peut supprimer la colonne nb_site uqi n'est plus utile
hvie_parmaj <- hvie_parmaj %>%
  dplyr::select(!nb_site)


# 8. Covariable individuelle 
#---------

# Il nous faut le nombre de captures total (-1 pour transience)
# et le nombre d'annees de capture
# pour chaque individus des hvie

hvie_parmaj <- Mesanges::cov_ind_new(parmaj, hvie_parmaj)

# Quels individus sont problematiques
which(hvie_parmaj$nb_years_capt==0) #0
which(hvie_parmaj$nb_capt==0) #0
which(is.na(hvie_parmaj$nb_years_capt)==T) #0
ind_pb <- which(is.na(hvie_parmaj$nb_capt)==T) #22

# cas par cas

#1
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[1]])
#....4514091
hvie_parmaj[ind_pb[1],]
hvie_parmaj$nb_capt[ind_pb[1]] <- 1

#2
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[2]])
#....4585897
hvie_parmaj[ind_pb[2],]
hvie_parmaj$nb_capt[ind_pb[2]] <- 1

#3
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[3]])
#....5309934
hvie_parmaj[ind_pb[3],]
hvie_parmaj$nb_capt[ind_pb[3]] <- 1

#4
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[4]])
#....5608045
hvie_parmaj[ind_pb[4],]
hvie_parmaj$nb_capt[ind_pb[4]] <- 1

#5
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[5]])
# ....5241231
hvie_parmaj[ind_pb[5],]
hvie_parmaj$nb_capt[ind_pb[5]] <- 1

#6
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[6]])
# ....5353624
hvie_parmaj[ind_pb[6],]
hvie_parmaj$nb_capt[ind_pb[6]] <- 1

#7
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[7]])
#....6503619
hvie_parmaj[ind_pb[7],]
hvie_parmaj$nb_capt[ind_pb[7]] <- 1

#8
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[8]])
#....6552416
hvie_parmaj[ind_pb[8],]
hvie_parmaj$nb_capt[ind_pb[8]] <- 1

#9
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[9]])
# ....2026801
hvie_parmaj[ind_pb[9],]
hvie_parmaj$nb_capt[ind_pb[9]] <- 1

#10
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[10]])
#....6715876
hvie_parmaj[ind_pb[10],]
hvie_parmaj$nb_capt[ind_pb[10]] <- 1

#11
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[11]])
#....6863092
hvie_parmaj[ind_pb[11],]
hvie_parmaj$nb_capt[ind_pb[11]] <- 1

#12
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[12]])
#....7521243
hvie_parmaj[ind_pb[12],]
hvie_parmaj$nb_capt[ind_pb[12]] <- 6

#13
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[13]])
#....7686263
hvie_parmaj[ind_pb[13],]
hvie_parmaj$nb_capt[ind_pb[13]] <- 1

#14
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[14]])
#....6482156
hvie_parmaj[ind_pb[14],]
hvie_parmaj$nb_capt[ind_pb[14]] <- 2

#15
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[15]])
# ....6747503
hvie_parmaj[ind_pb[15],]
hvie_parmaj$nb_capt[ind_pb[15]] <- 2

#16
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[16]])
#....7612498
hvie_parmaj[ind_pb[16],]
hvie_parmaj$nb_capt[ind_pb[16]] <- 2

#17
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[17]])
#....4514091
hvie_parmaj[ind_pb[17],]
hvie_parmaj$nb_capt[ind_pb[17]] <- 2

#18
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[18]])
#....5608045
hvie_parmaj[ind_pb[18],]
hvie_parmaj$nb_capt[ind_pb[18]] <- 1

#19
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[19]])
#....5353624
hvie_parmaj[ind_pb[19],]
hvie_parmaj$nb_capt[ind_pb[19]] <- 1

#20
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[20]])
#....6503619
hvie_parmaj[ind_pb[20],]
hvie_parmaj$nb_capt[ind_pb[20]] <- 1

#21
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[21]])
#....6863092
hvie_parmaj[ind_pb[21],]
hvie_parmaj$nb_capt[ind_pb[21]] <- 2

#22
subset(parmaj,parmaj$BAGUE==hvie_parmaj$ID[ind_pb[22]])
#....7686263
hvie_parmaj[ind_pb[22],]
hvie_parmaj$nb_capt[ind_pb[22]] <- 1

#Calcul de la covariable :
hvie_parmaj <- Mesanges::calcul_cov_ind(hvie_parmaj)

which(hvie_parmaj$cov_ind<0) # ca parait bon

rm(ind_pb)

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
save(hvie_parmaj,file=here::here('output',"hvie_parmaj_tot.RData"))
save(hvie_ID_PROG_parmaj,file=here::here('output',"hvie_ID_PROG_parmaj_tot.RData"))



# Analyses supplementaires 
#---------------------------
rm(list=ls())
load(here::here('output',"hvie_parmaj_tot.RData"))
load(here::here('output',"hvie_ID_PROG_parmaj_tot.RData"))

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
