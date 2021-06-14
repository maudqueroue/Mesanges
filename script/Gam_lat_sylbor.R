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
  dplyr::select('BAGUE','DATE','ESPECE','AGE',"THEME.SESSION","ID_PROG")

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


#1 ON NE GARDE QUE LES FAUVETTES SYLBOR et SYLATR
# -----------------------------------------------------------------------

data <- data %>%
  dplyr::filter(data$ESPECE == "SYLBOR")

unique(data$ESPECE)
# -- 503904


#3. Gestion date 
#------------------------------------------------------
data$annee <- substr(data$DATE,7,10)

unique(substr(data$DATE,4,5))

data <- data %>%
  dplyr::filter(substr(data$DATE,4,5) %in% c("04", "05", "06", "07")) 

unique(substr(data$DATE,4,5))


#5.Mise en forme data pour GAM
#---------------------------------------------------
data <- data %>% 
  dplyr::group_by(ID_PROG,annee) %>%
  dplyr::summarise(n=dplyr::n())


#5.Information Latitude
#---------------------------------------------------

CLC_STOC <- read.table(here::here("data","coord_STOC.csv"),head=T,sep=";") %>%
  dplyr::rename(
    long = 'Lon',
    lat  = 'Lat')

data <- dplyr::left_join(data, CLC_STOC, by = c("ID_PROG")) 
data <- data[-which(is.na(data$lat)==TRUE),]

# Gam latitude
#----------------------------------------------
library(gamm4)

model <- gamm4(n~s(lat),family=poisson,data=data)

plot.gam(model$gam)

