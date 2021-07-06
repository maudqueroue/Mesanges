rm(list=ls())

# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 


# Carte France
#-------------------

Fr <- sf::st_read(dsn = here::here('data','France'), 
                  layer = "DEPARTEMENT",
                  quiet = TRUE) %>% 
  tibble::add_column(PAYS = "FR") %>% 
  sf::st_transform(4326) %>% 
  dplyr::group_by(PAYS)%>% 
  dplyr::summarize()

#STOC
#-----------------
# Load sites
load(here::here('output','hvie_ID_PROG_tits.RData'))

# Les differents points avec leur longitude et latitude et type CLC
CLC_STOC <- read.table(here::here("output","CLC_STOC.csv"),head=T,sep=";") %>%
  dplyr::select('ID_PROG',"lat",'long')

# On relie les stations avec leur coordonnées
data_STOC <- dplyr::left_join(hvie_ID_PROG_tits, CLC_STOC, by = c("ID_PROG")) %>%
  dplyr::select('lat','long','cov_hab')
data_STOC$cov_hab <- as.factor(data_STOC$cov_hab)

# EPS
#--------------
load(here::here('output','data_tits_HQ.RData'))
load(here::here('output','data_tits_LQ.RData'))

data_tits_HQ <- data_tits_HQ %>%
  dplyr::distinct(point, .keep_all=TRUE)
data_tits_HQ$cov_hab <- rep(3,nrow(data_tits_HQ))

data_tits_LQ <- data_tits_LQ %>%
  dplyr::distinct(point,.keep_all=TRUE)
data_tits_LQ$cov_hab <- rep(4,nrow(data_tits_LQ))

data_EPS <- rbind(data_tits_LQ,data_tits_HQ) 

data_EPS <- data_EPS %>%
  dplyr::select('lat','long','cov_hab')

data_EPS$cov_hab <- as.factor(data_EPS$cov_hab)

#data complet 
#---------------
data <- rbind(data_EPS,data_STOC)

# Carte
#----------------
ggplot2::ggplot(Fr) +
ggplot2::xlab("") + 
ggplot2::ylab("") +
ggplot2::xlim(-4.5, 7.8) +
ggplot2::ylim(42.5,51) +
ggplot2::geom_sf() +
ggplot2::geom_point(data = data, ggplot2::aes(x= long, y=lat, color=cov_hab, shape =cov_hab, size=cov_hab)) +
ggplot2::scale_color_manual(values=c('#C75146','#56B4E9','#540804','#2F4858'),name='',labels=c('Count points high-quality','Count points low-quality','CR sites high-quality','CR sites low-quality')) +
ggplot2::scale_shape_manual(values=c(19,19,17,17),name='',labels=c('Count points high-quality','Count points low-quality','CR sites high-quality','CR sites low-quality')) +
ggplot2::scale_size_manual(values=c(0.9,0.9,3,3),name='',labels=c('Count points high-quality','Count points low-quality','CR sites high-quality','CR sites low-quality')) +
ggplot2::theme_minimal()


# Informations donnees tits
#-----------------------------------
load(here::here('output','data_tits_HQ.RData'))
load(here::here('output','data_tits_LQ.RData'))

# Nombre de points par habitat
length(unique(data_tits_HQ$point))
length(unique(data_tits_LQ$point))
#Nombre de satation de capture recapture par habitat
nrow(subset(data_STOC,data_STOC$cov_hab==1))
nrow(subset(data_STOC,data_STOC$cov_hab==2))

load(here::here('output','hvie_parmaj.RData'))
load(here::here('output','hvie_parcae.RData'))

#Infos
# temp <- 1:19
# f_bt <- NULL
# l_bt <- NULL

# for (i in 1:nrow(hvie_parcae)){
#   f_bt[i] <- min(temp[hvie_parcae[i,1:19]>0])
#   l_bt[i] <- max(temp[hvie_parcae[i,1:19]>0])
#   if((l_bt[i]-f_bt[i])>1){
#     hvie_parcae[i,(f_bt[i]+1):l_bt[i]] <- 2
#   }
#   else{next}
# }
# 
# f_gt <- NULL
# l_gt <- NULL
# 
# for (i in 1:nrow(hvie_parmaj)){
#   f_gt[i] <- min(temp[hvie_parmaj[i,1:19]>0])
#   l_gt[i] <- max(temp[hvie_parmaj[i,1:19]>0])
#   if((l_gt[i]-f_gt[i])>1){
#     hvie_parmaj[i,(f_gt[i]+1):l_gt[i]] <- 2
#   }
#   else{next}
# }

hvie_ID_PROG_tits <- hvie_ID_PROG_tits %>%
  dplyr::select('ID_PROG','cov_hab')
hvie_ID_PROG_tits$ID_PROG <- as.character(hvie_ID_PROG_tits$ID_PROG)
hvie_parcae <- dplyr::left_join(hvie_parcae,hvie_ID_PROG_tits,by=c('ID_PROG'))
hvie_parmaj <- dplyr::left_join(hvie_parmaj,hvie_ID_PROG_tits,by=c('ID_PROG'))

#jeu de données tits par habitat
hvie_parcae_HQ <- subset(hvie_parcae, hvie_parcae$cov_hab==1)
hvie_parcae_LQ <- subset(hvie_parcae, hvie_parcae$cov_hab==2)
hvie_parmaj_HQ <- subset(hvie_parmaj, hvie_parmaj$cov_hab==1)
hvie_parmaj_LQ <- subset(hvie_parmaj, hvie_parmaj$cov_hab==2)

#Nb individus
length(unique(hvie_parcae_HQ$ID))
length(unique(hvie_parcae_LQ$ID))
length(unique(hvie_parmaj_HQ$ID))
length(unique(hvie_parmaj_LQ$ID))

# BT HQ 
#-------------------
#Infos
temp <- 1:19
AGE_bt_HQ <- NULL
nbrecapt_bt_HQ <- NULL

for (i in 1:nrow(hvie_parcae_HQ)){
  AGE_bt_HQ[i] <- hvie_parcae_HQ[i,min(temp[hvie_parcae_HQ[i,1:19]>0])]
  nbrecapt_bt_HQ[i] <- length(which(hvie_parcae_HQ[i,1:19]>0))-1
}

# Calcul nb recapture
length(which(nbrecapt_bt_HQ>0))
# info complet
info_bt_HQ <- cbind(AGE_bt_HQ,nbrecapt_bt_HQ)
# Calcul nb jeunes recaptured
length(which(info_bt_HQ[,1]=="1" & info_bt_HQ[,2]>0))
# Calcul nb adults recaptured
sum(nbrecapt_bt_HQ) - length(which(info_bt_HQ[,1]=="1" & info_bt_HQ[,2]>0))
# Propotion jeunes first capture
length(which(AGE_bt_HQ=="1"))/length(AGE_bt_HQ)

# BT LQ 
#-------------------
#Infos
temp <- 1:19
AGE_bt_LQ <- NULL
nbrecapt_bt_LQ <- NULL

for (i in 1:nrow(hvie_parcae_LQ)){
  AGE_bt_LQ[i] <- hvie_parcae_LQ[i,min(temp[hvie_parcae_LQ[i,1:19]>0])]
  nbrecapt_bt_LQ[i] <- length(which(hvie_parcae_LQ[i,1:19]>0))-1
}

# Calcul nb recapture
length(which(nbrecapt_bt_LQ>0))
# info complet
info_bt_LQ <- cbind(AGE_bt_LQ,nbrecapt_bt_LQ)
# Calcul nb jeunes recaptured
length(which(info_bt_LQ[,1]=="1" & info_bt_LQ[,2]>0))
# Calcul nb adults recaptured
sum(nbrecapt_bt_LQ) - length(which(info_bt_LQ[,1]=="1" & info_bt_LQ[,2]>0))
# Propotion jeunes first capture
length(which(AGE_bt_LQ=="1"))/length(AGE_bt_LQ)

# GT HQ 
#-------------------
#Infos
temp <- 1:19
AGE_gt_HQ <- NULL
nbrecapt_gt_HQ <- NULL

for (i in 1:nrow(hvie_parmaj_HQ)){
  AGE_gt_HQ[i] <- hvie_parmaj_HQ[i,min(temp[hvie_parmaj_HQ[i,1:19]>0])]
  nbrecapt_gt_HQ[i] <- length(which(hvie_parmaj_HQ[i,1:19]>0))-1
}

# Calcul nb recapture
length(which(nbrecapt_gt_HQ>0))
# info complet
info_gt_HQ <- cbind(AGE_gt_HQ,nbrecapt_gt_HQ)
# Calcul nb jeunes recaptured
length(which(info_gt_HQ[,1]=="1" & info_gt_HQ[,2]>0))
# Calcul nb adults recaptured
sum(nbrecapt_gt_HQ) - length(which(info_gt_HQ[,1]=="1" & info_gt_HQ[,2]>0))
# Propotion jeunes first capture
length(which(AGE_gt_HQ=="1"))/length(AGE_gt_HQ)


# GT LQ 
#-------------------
#Infos
temp <- 1:19
AGE_gt_LQ <- NULL
nbrecapt_gt_LQ <- NULL

for (i in 1:nrow(hvie_parmaj_LQ)){
  AGE_gt_LQ[i] <- hvie_parmaj_LQ[i,min(temp[hvie_parmaj_LQ[i,1:19]>0])]
  nbrecapt_gt_LQ[i] <- length(which(hvie_parmaj_LQ[i,1:19]>0))-1
}

# Calcul nb recapture
length(which(nbrecapt_gt_LQ>0))
# info complet
info_gt_LQ <- cbind(AGE_gt_LQ,nbrecapt_gt_LQ)
# Calcul nb jeunes recaptured
length(which(info_gt_LQ[,1]=="1" & info_gt_LQ[,2]>0))
# Calcul nb adults recaptured
sum(nbrecapt_gt_LQ) - length(which(info_gt_LQ[,1]=="1" & info_gt_LQ[,2]>0))
# Propotion jeunes first capture
length(which(AGE_gt_LQ=="1"))/length(AGE_gt_LQ)


#EPS
#----------

sum(data_tits_HQ$PARCAE)
sum(data_tits_HQ$PARMAJ)
sum(data_tits_LQ$PARCAE)
sum(data_tits_LQ$PARMAJ)

sum(data_tits_HQ$PARCAE)/nrow(data_tits_HQ)
sum(data_tits_LQ$PARCAE)/nrow(data_tits_LQ)
sum(data_tits_HQ$PARMAJ)/nrow(data_tits_HQ)
sum(data_tits_LQ$PARMAJ)/nrow(data_tits_LQ)

# Mean nb capt STOC
#--------------------
load(here::here('output','hvie_ID_PROG_tits.RData'))
hvie_site <- as.matrix(hvie_ID_PROG_tits[1:19])
hvie_site[which(hvie_site>0)] <- 1
hvie_site <- cbind(hvie_site,hvie_ID_PROG_tits)
hvie_site_HQ <- subset(hvie_site,hvie_site$cov_hab=="1")
hvie_site_LQ <- subset(hvie_site,hvie_site$cov_hab=="2")

nb_capt_site_HQ <- apply(hvie_site_HQ[,1:19],2,sum)
nb_capt_site_LQ <- apply(hvie_site_LQ[,1:19],2,sum)

parcae_HQ <- as.matrix(hvie_parcae_HQ[,1:19])
parcae_HQ <- apply(parcae_HQ,2,as.numeric)
parcae_HQ[which(parcae_HQ>0)] <- 1
nb_capt_parcae_HQ <- apply(parcae_HQ,2,sum)
mean(nb_capt_parcae_HQ/nb_capt_site_HQ)

parcae_LQ <- as.matrix(hvie_parcae_LQ[,1:19])
parcae_LQ <- apply(parcae_LQ,2,as.numeric)
parcae_LQ[which(parcae_LQ>0)] <- 1
nb_capt_parcae_LQ <- apply(parcae_LQ,2,sum)
mean(nb_capt_parcae_LQ/nb_capt_site_LQ)

parmaj_HQ <- as.matrix(hvie_parmaj_HQ[,1:19])
parmaj_HQ <- apply(parmaj_HQ,2,as.numeric)
parmaj_HQ[which(parmaj_HQ>0)] <- 1
nb_capt_parmaj_HQ <- apply(parmaj_HQ,2,sum)
mean(nb_capt_parmaj_HQ/nb_capt_site_HQ)

parmaj_LQ <- as.matrix(hvie_parmaj_LQ[,1:19])
parmaj_LQ <- apply(parmaj_LQ,2,as.numeric)
parmaj_LQ[which(parmaj_LQ>0)] <- 1
nb_capt_parmaj_LQ <- apply(parmaj_LQ,2,sum)
mean(nb_capt_parmaj_LQ/nb_capt_site_LQ)

# Mean nb capt EPS
#--------------------

load(here::here('output','data_tits_HQ.RData'))
load(here::here('output','data_tits_LQ.RData'))

EPS_HQ_bt <- NULL
EPS_HQ_gt <- NULL
EPS_LQ_bt <- NULL
EPS_LQ_gt <- NULL

year <- seq(2001,2019,1)
for(i in 1 :19){
  sub_point_EPS_HQ <- subset(data_tits_HQ,data_tits_HQ$annee==year[i])
  sub_point_EPS_LQ <- subset(data_tits_LQ,data_tits_LQ$annee==year[i])
  EPS_HQ_bt[i] <- sum(sub_point_EPS_HQ$PARCAE)/nrow(sub_point_EPS_HQ)
  EPS_LQ_bt[i] <- sum(sub_point_EPS_LQ$PARCAE)/nrow(sub_point_EPS_LQ)
  EPS_HQ_gt[i] <- sum(sub_point_EPS_HQ$PARMAJ)/nrow(sub_point_EPS_HQ)
  EPS_LQ_gt[i] <- sum(sub_point_EPS_LQ$PARMAJ)/nrow(sub_point_EPS_LQ)
  rm(sub_point_EPS_HQ,sub_point_EPS_LQ)
}

mean(EPS_LQ_bt)
