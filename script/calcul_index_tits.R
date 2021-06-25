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

rm(data_add, EPS_by_STOC, CLC_EPS, CLC_STOC, EPS, i)


# Type d habitat
HQ <- c("311","313")
LQ <- c("111","112", "121", "122", "123", "124", "131", "132", 
        "133","141", "142", "211", "212", "213", "221", "222", 
        "223", "231", "241", "242", "243", "244", "312", "321", 
        "322", "323", "324", "331", "332", "333", "334", "335", 
        "411", "412", "421", "422", "423", "511", "512", "521", 
        "522", "523")


# Avec les donnees de TITS
#############################

load(here::here("output","hvie_ID_PROG_tits.RData"))
ID_PROG_tits <- hvie_ID_PROG_tits$ID_PROG
# On ne garde que les sites utilises dans les donnees parmaj
data_tits <- data %>%
  dplyr::filter(data$ID_PROG %in% ID_PROG_tits)

rm(hvie_ID_PROG_tits, ID_PROG_tits, data_EPS)

#### Data habitat HQ 
data_tits_HQ <- data_tits %>%
  dplyr::filter(data_tits$CLC_STOC %in% HQ & data_tits$CLC_EPS %in% HQ)

# On supprime les points qui n'ont jamais contacté de mesanges
data_tits_HQ <- Mesanges::supr_point(data_tits_HQ, "TITS")
# on supprime les doublons
data_tits_HQ <- data_tits_HQ %>% dplyr::distinct(carre,annee,point,.keep_all = TRUE)


#### Data habitat LQ 
data_tits_LQ <- data_tits %>%
  dplyr::filter(data_tits$CLC_STOC %in% LQ & data_tits$CLC_EPS %in% LQ)

# On supprime les points qui n'ont jamais contacté de mesanges
data_tits_LQ <- Mesanges::supr_point(data_tits_LQ, "TITS")
# on supprime les doublons
data_tits_LQ <- data_tits_LQ %>% dplyr::distinct(carre,annee,point,.keep_all = TRUE)

save(data_tits_HQ,file=here::here('output','data_tits_HQ.RData'))
save(data_tits_LQ,file=here::here('output','data_tits_LQ.RData'))

##############################################################################################
# CREATION INDEX
##############################################################################################

### PARMAJ 
index_parmaj_HQ_point <- Mesanges::index_new(data_tits_HQ, "PARMAJ")
save(index_parmaj_HQ_point,file=here::here("output","index_parmaj_HQ_point.RData"))
load(here::here("output","index_parmaj_HQ_point.RData"))
Mesanges::plot_index(index_parmaj_HQ_point[[1]], sqrt(index_parmaj_HQ_point[[2]]), color[3], "parmaj", "favorable - point" )

index_parmaj_LQ_point <- Mesanges::index_new(data_tits_LQ, "PARMAJ")
save(index_parmaj_LQ_point,file=here::here("output","index_parmaj_LQ_point.RData"))
load(here::here("output","index_parmaj_LQ_point.RData"))
Mesanges::plot_index(index_parmaj_LQ_point[[1]], sqrt(index_parmaj_LQ_point[[2]]), color[1], "parmaj", "defavorable - point" )

#### PARCAE
index_parcae_HQ_point <- Mesanges::index_new(data_tits_HQ, "PARCAE")
save(index_parcae_HQ_point,file=here::here("output","index_parcae_HQ_point.RData"))
load(here::here("output","index_parcae_HQ_point.RData"))
Mesanges::plot_index(index_parcae_HQ_point[[1]], sqrt(index_parcae_HQ_point[[2]]), color[2], "parcae", "favorable - point" )

index_parcae_LQ_point <- Mesanges::index_new(data_tits_LQ, "PARCAE")
save(index_parcae_LQ_point,file=here::here("output","index_parcae_LQ_point.RData"))
load(here::here("output","index_parcae_LQ_point.RData"))
Mesanges::plot_index(index_parcae_LQ_point[[1]], sqrt(index_parcae_LQ_point[[2]]), color[4], "parcae", "defavorable - point" )

##############################################################################################
# PLOT INDEX
##############################################################################################

load(here::here("output","index_parcae_HQ_point.RData"))
ind_bt1 <- list()
ind_bt1$gammat_bt1 <- t(apply(index_parcae_HQ_point[[3]],c(2,3), mean, na.rm = TRUE))
ind_bt1$l_025_bt1  <- apply(ind_bt1$gammat_bt1,2,quantile,probs=0.025)
ind_bt1$l_25_bt1   <- apply(ind_bt1$gammat_bt1,2,quantile,probs=0.25)
ind_bt1$l_50_bt1   <- apply(ind_bt1$gammat_bt1,2,quantile,probs=0.50)
ind_bt1$l_75_bt1   <- apply(ind_bt1$gammat_bt1,2,quantile,probs=0.75)
ind_bt1$l_975_bt1  <- apply(ind_bt1$gammat_bt1,2,quantile,probs=0.975)

load(here::here("output","index_parcae_LQ_point.RData"))
ind_bt2 <- list()
ind_bt2$gammat_bt2 <- t(apply(index_parcae_LQ_point[[3]],c(2,3), mean, na.rm = TRUE))
ind_bt2$l_025_bt2  <- apply(ind_bt2$gammat_bt2,2,quantile,probs=0.025)
ind_bt2$l_25_bt2   <- apply(ind_bt2$gammat_bt2,2,quantile,probs=0.25)
ind_bt2$l_50_bt2   <- apply(ind_bt2$gammat_bt2,2,quantile,probs=0.50)
ind_bt2$l_75_bt2   <- apply(ind_bt2$gammat_bt2,2,quantile,probs=0.75)
ind_bt2$l_975_bt2  <- apply(ind_bt2$gammat_bt2,2,quantile,probs=0.975)

load(here::here("output","index_parmaj_HQ_point.RData"))
ind_gt1 <- list()
ind_gt1$gammat_gt1 <- t(apply(index_parmaj_HQ_point[[3]],c(2,3), mean, na.rm = TRUE))
ind_gt1$l_025_gt1  <- apply(ind_gt1$gammat_gt1,2,quantile,probs=0.025)
ind_gt1$l_25_gt1   <- apply(ind_gt1$gammat_gt1,2,quantile,probs=0.25)
ind_gt1$l_50_gt1   <- apply(ind_gt1$gammat_gt1,2,quantile,probs=0.50)
ind_gt1$l_75_gt1   <- apply(ind_gt1$gammat_gt1,2,quantile,probs=0.75)
ind_gt1$l_975_gt1  <- apply(ind_gt1$gammat_gt1,2,quantile,probs=0.975)

load(here::here("output","index_parmaj_LQ_point.RData"))
ind_gt2 <- list()
ind_gt2$gammat_gt2 <- t(apply(index_parmaj_LQ_point[[3]],c(2,3), mean, na.rm = TRUE))
ind_gt2$l_025_gt2  <- apply(ind_gt2$gammat_gt2,2,quantile,probs=0.025)
ind_gt2$l_25_gt2   <- apply(ind_gt2$gammat_gt2,2,quantile,probs=0.25)
ind_gt2$l_50_gt2   <- apply(ind_gt2$gammat_gt2,2,quantile,probs=0.50)
ind_gt2$l_75_gt2   <- apply(ind_gt2$gammat_gt2,2,quantile,probs=0.75)
ind_gt2$l_975_gt2  <- apply(ind_gt2$gammat_gt2,2,quantile,probs=0.975)

color<-c("#FF8830","#A6B06D","#589482","#8C2423")
color_bt <- color[3]
color_gt <- color[2]

color.transparent_bt <- adjustcolor(color_bt, alpha.f = 0.4)
color.transparent_2_bt<- adjustcolor(color_bt, alpha.f = 0.2)
color.transparent_gt <- adjustcolor(color_gt, alpha.f = 0.4)
color.transparent_2_gt<- adjustcolor(color_gt, alpha.f = 0.2)


K=18
par(mfrow=c(1,2))

plot(ind_bt1$l_50_bt1,type='l',axes=F,lwd =2,col="ivory4",ylim=c(0,2.5),ylab="Mean number of tits per count point",xlab="",main="High Quality Habitats")
xx<- c(1:K,K:1)
yy <- c(ind_bt1$l_025_bt1[1:K],ind_bt1$l_975_bt1[K:1])
polygon(xx,yy,col=color.transparent_2_bt, border=NA)
xx<- c(1:K,K:1)
yy <- c(ind_gt1$l_025_gt1[1:K],ind_gt1$l_975_gt1[K:1])
polygon(xx,yy,col=color.transparent_2_gt, border=NA)
xx<- c(1:K,K:1)
yy <- c(ind_bt1$l_25_bt1[1:K],ind_bt1$l_75_bt1[K:1])
polygon(xx,yy,col=color.transparent_bt, border=NA)
xx<- c(1:K,K:1)
yy <- c(ind_gt1$l_25_gt1[1:K],ind_gt1$l_75_gt1[K:1])
polygon(xx,yy,col=color.transparent_gt, border=NA)
lines(ind_bt1$l_50_bt1,lwd =2, col=color_bt)
lines(ind_gt1$l_50_gt1,lwd =2, col=color_gt)
axis(1, at=c(1:K),labels=c(seq(2002,2019,1)))
axis(side =2, cex.axis=1, las=2)


plot(ind_bt2$l_50_bt2,type='l',axes=F,lwd =2,col="ivory4",ylim=c(0,2.5),ylab="Mean number of tits per count point",xlab="",main="Low Quality Habitats")
xx<- c(1:K,K:1)
yy <- c(ind_bt2$l_025_bt2[1:K],ind_bt2$l_975_bt2[K:1])
polygon(xx,yy,col=color.transparent_2_bt, border=NA)
xx<- c(1:K,K:1)
yy <- c(ind_gt2$l_025_gt2[1:K],ind_gt2$l_975_gt2[K:1])
polygon(xx,yy,col=color.transparent_2_gt, border=NA)
xx<- c(1:K,K:1)
yy <- c(ind_bt2$l_25_bt2[1:K],ind_bt2$l_75_bt2[K:1])
polygon(xx,yy,col=color.transparent_bt, border=NA)
xx<- c(1:K,K:1)
yy <- c(ind_gt2$l_25_gt2[1:K],ind_gt2$l_75_gt2[K:1])
polygon(xx,yy,col=color.transparent_gt, border=NA)
lines(ind_bt2$l_50_bt2,lwd =2, col=color_bt)
lines(ind_gt2$l_50_gt2,lwd =2, col=color_gt)
axis(1, at=c(1:K),labels=c(seq(2002,2019,1)))
axis(side =2, cex.axis=1, las=2)
legend('topright',legend=c("Blue tit", "Great tit"),lty=1,lwd=2,col=color[3:2], bty='n')

