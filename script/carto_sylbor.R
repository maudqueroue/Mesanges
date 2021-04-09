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
  sf::st_transform(2154) %>% 
  dplyr::group_by(PAYS)%>% 
  dplyr::summarize()


# Data STOC
#------------------

#data
load(here::here("output","data_STOC.RData"))

#ajout colonne annee
data_STOC$year <- substr(data_STOC$DATE,7,10)

# Nom de ID PROG
ID_STOC <- unique(data_STOC$ID_PROG)

# On garde que sylbor
sylbor_STOC <- subset(data_STOC,data_STOC$ESPECE=="SYLBOR")

# Cb de sylbor en moyenne par an
sylbor_STOC <- sylbor_STOC %>%
  dplyr::group_by(ID_PROG,year) %>%
  dplyr::summarise(n=dplyr::n()) %>%
  dplyr::group_by(ID_PROG) %>%
  dplyr::summarise(n=mean(n)) 

# On enleve donnee quand moins de 5/an
sylbor_STOC$n[which(sylbor_STOC$n<5)] <- NA

# Les differents points avec leur longitude et latitude et type CLC
CLC_STOC <- read.table(here::here("data","coord_STOC.csv"),head=T,sep=";") %>%
  dplyr::rename(
    long = 'Lon',
    lat  = 'Lat')

# On garde que les points utilisés dans le STOC 
CLC_STOC <- CLC_STOC %>%
  dplyr::filter(CLC_STOC$ID_PROG %in% ID_STOC)

# On relie les point avec le nb moyen de sylbor vu par an 
CLC_STOC <- dplyr::left_join(CLC_STOC, sylbor_STOC, by = c("ID_PROG")) 

# Creation de la couche de points
dsf_STOC <-  CLC_STOC %>%
  sf::st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  sf::st_transform(crs = 2154)

# Plot carte
ggplot2::ggplot(Fr) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = dsf_STOC, ggplot2::aes(color=n)) +
  ggplot2::coord_sf(crs = 2154, datum = sf::st_crs(2154)) +
  ggplot2::guides(fill = FALSE) +
  ggplot2::ggtitle("France") +
  ggplot2::theme(title = ggplot2::element_text(size = 16))+
  ggplot2::scale_color_gradientn(colours = rainbow(6))


# Data EPS
#------------------

load(here::here("output","data_EPS.RData"))

# Nom des points
ID_EPS <- unique(data_EPS$point)

# on garde les point avec de la sybor
sylbor_EPS <- subset(data_EPS,data_EPS$SYLBOR>0)

# Cb de sylbor en moyenne par an
sylbor_EPS <- sylbor_EPS %>%
  dplyr::group_by(point) %>%
  dplyr::summarise(n=sum(SYLBOR)) 

# Les differents points avec leur longitude et latitude et type CLC
load(here::here("output","data_EPS.RData"))

CLC_EPS <- data_EPS %>%
  dplyr::rename(
    long = 'longitude_wgs84',
    lat  = 'latitude_wgs84') %>%
  dplyr::distinct(point, .keep_all=T) %>%
  dplyr::select("point", "long", "lat") %>%
  dplyr::filter(!is.na(long)) %>%
  dplyr::filter(!is.na(lat))
     

CLC_EPS <- CLC_EPS %>%
  dplyr::filter(CLC_EPS$point %in% ID_EPS)

CLC_EPS <- dplyr::left_join(CLC_EPS, sylbor_EPS, by = c("point")) 

# Creation de la couche de points
dsf_EPS <-  CLC_EPS %>%
  sf::st_as_sf(coords = c("long","lat"), crs = 4326) %>%
  sf::st_transform(crs = 2154)

# Plot EPS
ggplot2::ggplot(Fr) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = dsf_EPS, ggplot2::aes(color=n)) +
  ggplot2::coord_sf(crs = 2154, datum = sf::st_crs(2154)) +
  ggplot2::guides(fill = FALSE) +
  ggplot2::ggtitle("France") +
  ggplot2::theme(title = ggplot2::element_text(size = 16))+
  ggplot2::scale_color_gradientn(colours = rainbow(6))

