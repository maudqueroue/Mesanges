######                                       ######
###### SCRIPT PREMIER TRI DONNEES PASSEREAUX ######
######                                       ######

# Jeu de donnees entrant : donnees brut MNHN

# Jeu de donnees sortant : histoire de vie des sites
#                          Premier tri donnees MNHN         


rm(list=ls())

# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 

# DATA MNHN

data_brut <- read.csv2(here::here('Data',"Data_MNHN.csv")) %>%
  dplyr::select('ACTION','BAGUE','DATE','HEURE','ESPECE','SEXE','AGE',"THEME.SESSION","ID_PROG","LAT","LON","DEPT","LOCALITE")

data <- data_brut 

rm(data_brut)
#-- 534407 

####### ATTENTION MODIF DATA #####
# -> station 43 et 623 ? lier
# -> station 42 et 285 ? lier 
# -> station 200 et 615 ? lier
data$ID_PROG[which(data$ID_PROG==43)]<-623
data$ID_PROG[which(data$ID_PROG==42)]<-285
data$ID_PROG[which(data$ID_PROG==200)]<-615


# enlever les lignes sans nom de site
data <- data %>% 
  dplyr::filter(!is.na(ID_PROG)) 


#1 ON NE GARDE QUE LES DONNES STOC et STOC ROZO
# -----------------------------------------------------------------------

unique(data$THEME.SESSION)

data <- data %>%
  dplyr::filter(data$THEME.SESSION == "STOC" | data$THEME.SESSION == "STOC ROZO")

unique(data$THEME.SESSION)
# -- 503904

# 2 QUELLES ANNEES GARDE T ON ?
#------------------------------------------------------------------------

# On ne garde que les annees apres 2001 pour etre coherent avec les donnees EPS
unique(substr(data$DATE,7,10))

data <- data %>%
  dplyr::filter(substr(data$DATE,7,10) > 2000) 

unique(substr(data$DATE,7,10))
#-- 422610


#3. Gestion date 
#------------------------------------------------------
data$annee <- substr(data$DATE,7,10)

unique(substr(data$DATE,4,5))

data <- data %>%
  dplyr::filter(substr(data$DATE,4,5) %in% c("04", "05", "06", "07")) 

unique(substr(data$DATE,4,5))

data$DATE <- lubridate::dmy(data$DATE)

# on range dans l'ordre croissant des dates
data <- data%>%
  dplyr::arrange(DATE)

#4. Gestion age 
#------------------------------------------------------
# on redefini l'age soit adulte soit poussin
data <- Mesanges::new_age(data)
# on supprime les incertains
data <- data[-which(data$new_AGE=="C"),]
data <- data[-which(is.na(data$new_AGE)==TRUE),]

unique(data$new_AGE)


#5. QUELLES STATIONS GARDER EN LATITUDE
#---------------------------------------------------

CLC_STOC <- read.table(here::here("data","coord_STOC.csv"),head=T,sep=";") %>%
  dplyr::rename(
    long = 'Lon',
    lat  = 'Lat')

ID_PROG <- CLC_STOC %>%
  dplyr::filter(CLC_STOC$lat>46)

ID_to_keep <- unique(ID_PROG$ID_PROG)

data <- data %>%
  dplyr::filter(data$ID_PROG %in% ID_to_keep)

rm(ID_to_keep,CLC_STOC,ID_PROG)

#5. garde que indiv dans jeu de données CMR
#----------------------------------------------
load(here::here('output','hvie_sylatr_tot_tt.RData'))
ID_sylatr <- unique(hvie_sylatr$ID)

data_sylatr <- data %>%
  dplyr::filter(data$BAGUE %in% ID_sylatr)
unique(data_sylatr$ESPECE)


load(here::here('output','hvie_sylbor_tot_tt.RData'))
ID_sylbor <- unique(hvie_sylbor$ID)

data_sylbor <- data %>%
  dplyr::filter(data$BAGUE %in% ID_sylbor)
unique(data_sylbor$ESPECE)

# on crée un tableau commun
data <- rbind(data_sylatr,data_sylbor)

rm(ID_sylatr,ID_sylbor,hvie_sylatr,hvie_sylbor)

#7. On garde premiere capture par an  
#------------------------------------------------------

data_ind <- data %>% dplyr::distinct(annee,BAGUE,ESPECE,.keep_all = TRUE)

######## tricherie date pour plot
data_ind$fDATE <- data_ind$DATE

for(i in 1:nrow(data_ind)){
  data_ind$fDATE[i] <- update(data_ind$fDATE[i],year = 2001)
}

### Date en chiffre a parti d'avril
num_Date <- seq(as.Date("2001-04-01"), as.Date("2001-07-31"), by="days")

data_ind$nDATE <- NA
for (i in 1:nrow(data_ind)){
  data_ind$nDATE[i] <- which(num_Date==data_ind$fDATE[i])
}

# Data par age
data_A <- subset(data_ind, data_ind$new_AGE=='A')
data_P <- subset(data_ind, data_ind$new_AGE=='P')

#### plot 
library(ggplot2)

ggplot(data_A,aes(fDATE)) +
  geom_density(aes(fill = ESPECE),alpha=0.7) +
  facet_wrap(~ annee)
ggplot(data_P,aes(fDATE)) +
  geom_density(aes(fill = ESPECE),alpha=0.7) +
  facet_wrap(~ annee)


info_A <- data_A %>% 
  dplyr::group_by(annee,ESPECE) %>%
  dplyr::summarise(mean=mean(nDATE),med=median(nDATE),sd=sd(nDATE),q25=quantile(nDATE, probs=0.25),q75=quantile(nDATE, probs = 0.75)) 

info_P <- data_P %>% 
  dplyr::group_by(annee,ESPECE) %>%
  dplyr::summarise(mean=mean(nDATE),med=median(nDATE),sd=sd(nDATE),q25=quantile(nDATE, probs=0.25),q75=quantile(nDATE, probs = 0.75)) 

ggplot(info_A, aes(x=med, y=annee, color=ESPECE)) +
    geom_errorbar(aes(xmin = q25, xmax = q75), position = position_dodge(0.3), width = 0.3,size=1.5) +
    geom_point(aes(color = ESPECE), position = position_dodge(0.3), size=3) +
    theme_minimal()+
    scale_color_manual(values = c("#00AFBB", "#E7B800"))

ggplot(info_P, aes(x=med, y=annee, color=ESPECE)) +
  geom_errorbar(aes(xmin = q25, xmax = q75), position = position_dodge(0.3), width = 0.3,size=1.5) +
  geom_point(aes(color = ESPECE), position = position_dodge(0.3), size=3) +
  theme_minimal()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))














#8. On regarde combien de capture adulte/poussin a chaque occasion de capture par site
#------------------------------------------------------
sylatr <- data_ind %>%
  dplyr::group_by(ID_PROG,DATE,new_AGE) %>%
  dplyr::summarise(n=dplyr::n()) %>%
  tidyr::pivot_wider(names_from = new_AGE, values_from = n) 


sylatr[is.na(sylatr)] <- 0


#9. On calcule les proportions et les proportion cumulees pour faire un indice de pheno
#------------------------------------------------------

sylatr$annee <- substr(sylatr$DATE,1,4)

ID_site <- unique(sylatr$ID_PROG)
years <- unique(sylatr$annee)

sylatr$prop_P <- NA
sylatr$prop_cum_P <- NA
sylatr$prop_A <- NA
sylatr$prop_cum_A <- NA

for(i in 1:length(ID_site)){
  for(j in 1:length(years)){
    ss <- subset(sylatr,sylatr$ID_PROG==ID_site[i] & sylatr$annee==years[j])
    ss$prop_P <- NA
    ss$prop_A <- NA
    if(nrow(ss)>1){
      nb_tot_P <- sum(ss$P)
      nb_tot_A <- sum(ss$A)
      for(k in 1:nrow(ss)){
        ss$prop_P[k] <- ss$P[k]/nb_tot_P
        ss$prop_A[k] <- ss$A[k]/nb_tot_A
      }
      ss$prop_cum_P[1] <- ss$prop_P[1]
      ss$prop_cum_A[1] <- ss$prop_A[1]
      
      for(k in 2:nrow(ss)){
        ss$prop_cum_P[k] <- ss$prop_cum_P[k-1]+ss$prop_P[k]
        ss$prop_cum_A[k] <- ss$prop_cum_A[k-1]+ss$prop_A[k]
        
      }
      sylatr$prop_P[which(sylatr$ID_PROG==ID_site[i] & sylatr$annee==years[j])] <- ss$prop_P
      sylatr$prop_cum_P[which(sylatr$ID_PROG==ID_site[i] & sylatr$annee==years[j])] <- ss$prop_cum_P
      sylatr$prop_A[which(sylatr$ID_PROG==ID_site[i] & sylatr$annee==years[j])] <- ss$prop_A
      sylatr$prop_cum_A[which(sylatr$ID_PROG==ID_site[i] & sylatr$annee==years[j])] <- ss$prop_cum_A
      
    }
    else{next}
  }
}


###############################
###############################

ggplot(dataA,aes(fDATE)) +
  geom_density(aes(fill = annee),alpha=0.7)


dm(data_ind$DATE)

datay1A <- subset(datay1,datay1$A>0)
datay1P <- subset(datay1, datay1$P>0)

datay2 <- subset(sylatr,sylatr$annee==years[2])



plot(datay$DATE,datay$prop_A)
plot(datay1$DATE,datay1$A)
plot(datay2$DATE,datay2$A)

plot(datay1$DATE,datay1$P)
plot(datay2$DATE,datay2$P)

fit <- lm(datay2$P~datay2$DATE)

ggplot2::ggplot(datay1P,ggplot2::aes(DATE,P)) +
  ggplot2::stat_smooth(method="lm",formula = y ~ poly(x,2), size=1) +
  ggplot2::geom_point()

################ TRI POUR SORTIR AGE MAL identifié

# on redefini l'age soit adulte soit poussin
data <- Mesanges::new_age(data)
# on supprime les incertains
data <- data[-which(data$new_AGE=="C"),]
data <- data[-which(is.na(data$new_AGE)==TRUE),]


# on garde une ligne par individu par jour par age
data_test <- data %>% dplyr::distinct(DATE,ID_PROG,new_AGE,BAGUE,.keep_all = TRUE)
# on garde un eligne par individu par jour
data_test_2 <- data %>% dplyr::distinct(DATE,ID_PROG,BAGUE,.keep_all = TRUE)

# quel individu sont vu la meme journee dans deux ages differents 
ligne_pb <- dplyr::setdiff(data_test,data_test_2) %>%
  dplyr::select('BAGUE','DATE',"ID_PROG")

# on supprime les individus qui sont vus dans deux age differents
data_keep <- data_test_2  %>%
  dplyr::select('BAGUE','DATE',"ID_PROG")

data_new <- dplyr::setdiff(data_keep,ligne_pb)

# On a enfin un tableau avec que des individus vu dansun seul age par jour
sylatr <- dplyr::left_join(data_new,data_test_2) %>%
  dplyr::relocate('ACTION', .before = "BAGUE") %>%
  dplyr::relocate("ID_PROG", .before = "LAT")


rm(data_new,ligne_pb,data_test,data_test_2,data_keep)


data_sp <- data_test %>%
  tidyr::pivot_wider(names_from = new_AGE, values_from = n) 



  data_test <- sylatr %>%
    dplyr::group_by(ID_PROG,DATE,new_AGE) %>%
    dplyr::summarise(n=dplyr::n())

unique(sylatr$new_AGE)
