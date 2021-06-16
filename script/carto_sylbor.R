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


# Data STOC
#------------------

#data
load(here::here("output","data_STOC.RData"))

#ajout colonne annee
data_STOC$year <- substr(data_STOC$DATE,7,10)

# On garde que sylbor
sylbor_STOC <- subset(data_STOC,data_STOC$ESPECE=="SYLBOR") 

# enlever les lignes sans nom de site ou annee
sylbor_STOC <- sylbor_STOC %>% 
  dplyr::filter(!is.na(ID_PROG)) %>%
  dplyr::filter(!is.na(year))

# Cb de sylbor par an par annee
sylbor_STOC <- sylbor_STOC %>%
  dplyr::group_by(ID_PROG,year) %>%
  dplyr::summarise(n=dplyr::n())

# Les differents points avec leur longitude et latitude et type CLC
CLC_STOC <- read.table(here::here("output","CLC_STOC.csv"),head=T,sep=";") %>%
  dplyr::select('ID_PROG',"lat",'long')

# On relie les stations avec leur coordonnées
CLC_STOC <- dplyr::left_join(sylbor_STOC, CLC_STOC, by = c("ID_PROG")) %>%
  dplyr::filter(!is.na(long)) %>%
  dplyr::filter(!is.na(lat))

# Creation de la couche de points
dsf_STOC <-  CLC_STOC %>%
  sf::st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  sf::st_transform(crs = 4326)

# Plot carte
ggplot2::ggplot(Fr) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = dsf_STOC, ggplot2::aes(color=n)) +
  ggplot2::coord_sf(crs = 4326, datum = sf::st_crs(4326)) +
  ggplot2::guides(fill = FALSE) +
  ggplot2::ggtitle("France") +
  ggplot2::theme(title = ggplot2::element_text(size = 16))+
  ggplot2::scale_color_gradientn(colours = rainbow(6))


# Data EPS
#------------------

load(here::here("output","data_EPS.RData"))

data_EPS <- data_EPS[,c('annee','carre','point','SYLBOR')]

# on garde les point avec de la sybor
sylbor_EPS <- subset(data_EPS,data_EPS$SYLBOR>0)

lien_carre_point <- sylbor_EPS %>%
  dplyr::distinct(carre,.keep_all = TRUE) 
lien_carre_point <- lien_carre_point[,c('carre','point')]

sylbor_EPS <- sylbor_EPS %>%
  dplyr::group_by(annee,carre)  %>%
  dplyr::summarise(n=dplyr::n())

# Les differents points avec leur longitude et latitude et type CLC
load(here::here("output","CLC_EPS.RData"))

# On eneleve les point sans lat ou long
CLC_EPS <- CLC_EPS %>%
  dplyr::select("point", "long", "lat") %>%
  dplyr::filter(!is.na(long)) %>%
  dplyr::filter(!is.na(lat)) 
     
# On relie les point avec leur coordonnées
sylbor_EPS <- dplyr::left_join(sylbor_EPS, lien_carre_point, by = c("carre"))
  
CLC_EPS <- dplyr::left_join(sylbor_EPS, CLC_EPS, by = c("point")) %>%
  dplyr::filter(!is.na(long)) %>%
  dplyr::filter(!is.na(lat)) 

# Creation de la couche de points
dsf_EPS <-  CLC_EPS %>%
  sf::st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  sf::st_transform(crs = 4326)

# Plot EPS
ggplot2::ggplot(Fr) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = dsf_EPS, ggplot2::aes(color=n)) +
  ggplot2::coord_sf(crs = 4326, datum = sf::st_crs(4326)) +
  ggplot2::guides(fill = FALSE) +
  ggplot2::ggtitle("France") +
  ggplot2::theme(title = ggplot2::element_text(size = 16))+
  ggplot2::scale_color_gradientn(colours = rainbow(6))

# Gam latitude
#----------------------------------------------
library(gamm4)

model <- gamm4(n~s(lat),family=poisson,data=CLC_EPS)

plot.gam(model$gam)
