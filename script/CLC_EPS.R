rm(list=ls())

# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 

# Load Corine Land Cover
shp_CLC <- sf::st_read(dsn   = here::here("data","CLC12_FR_RGF_SHP"),
                   layer = "CLC12_FR_RGF") %>%
           sf::st_transform(shp,crs = 2154)

# Les differents points avec leur longitude et latitude et type CLC
load(here::here("output","data_EPS.RData"))
CLC_EPS <- data_EPS %>%
              dplyr::rename(
                long = 'longitude_wgs84',
                lat  = 'latitude_wgs84') %>%
              dplyr::distinct(point, .keep_all=T) %>%
              dplyr::select("point", "long", "lat") %>%
              dplyr::filter(!is.na(long)) %>%
              dplyr::filter(!is.na(lat))  %>%
              tibble::add_column(CLC_1 = NA) %>%
              tibble::add_column(CLC_2 = NA)         
rm(data_EPS)

# Creation de la couche de points
dsf_EPS <-  Mesanges::give_point(CLC_EPS)


# On recupere les premiers code CLC
for (i in 1:5){#nrow(CLC_EPS)) {
  CLC_EPS[i,] <- Mesanges::give_CLC(dsf_EPS[i,], CLC_EPS[i,], shp_CLC)
}

save(CLC_EPS, file  = here::here("output","CLC_EPS.RData"))

# Carte
load(here::here("output","CLC_EPS.RData"))
Mesanges::plot_carte_point(379, CLC_EPS, shp_CLC)

#########################
##############################
# Analyse effet habitat
# Type d'habitat pour chaque station EPS
load(here::here("output","CLC_EPS.RData"))

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

# On lie les tables CLC_EPS et data_EPS
EPS <- dplyr::left_join(data_EPS, CLC_EPS, by = c("point", "lat", "long")) %>%
  dplyr::rename(CLC_EPS = 'CLC_1')  %>%
  dplyr::select(-CLC_2)

rm(data_EPS, CLC_EPS)

# Creation de data pour glm
data <- EPS %>%
  dplyr::select("point","PARCAE","PARMAJ","CLC_EPS") %>%
  dplyr::rename(
    A_parmaj = 'PARMAJ',
    B_parcae = 'PARCAE') %>%
  dplyr::filter(!is.na(point)) %>% 
  tidyr::pivot_longer(cols = c("B_parcae","A_parmaj"), names_to= "sp") %>%
  dplyr::mutate(sp=as.factor(sp)) %>%
  dplyr::mutate(CLC_EPS=as.factor(CLC_EPS)) 

#--------------------------------------------------  
# EFFET HABITAT PARMAJ  
model.glm <- glm(PARMAJ ~ 0 + as.factor(CLC_EPS) , data = EPS, family = 'poisson')
sum_glm <- summary(model.glm)

effet_CLC <- matrix(NA,4000,2)

nCLC <- length(unique(data$CLC_EPS))
code <- as.vector(sort(unique(data$CLC_EPS)))

for(i in 1:nCLC){
  effet_CLC[((i-1)*100+1):(i*100),1] <- rnorm(100,sum_glm$coefficients[i,1],sum_glm$coefficients[i,2])
  effet_CLC[((i-1)*100+1):(i*100),2] <- rep(code[i],100)
}

effet_CLC <- as.data.frame(effet_CLC)
colnames(effet_CLC) <- c("abondance","CLC")

effet_CLC %>%
  ggplot(aes(x = as.factor(CLC), y =as.numeric(abondance),fill=CLC)) +
  geom_boxplot(width=0.6,position=position_dodge(0.8),alpha=0.4,outlier.shape = NA) +
  scale_y_continuous(limits=c(-4,2))+
  labs(title="Effet habitat PARMAJ",x="Habitat", y = "Effet")+
  theme_minimal()


#--------------------------------------------------------
# EFFET HABITAT PARCAE
model.glm <- glm(PARCAE ~ 0 + as.factor(CLC_EPS) , data = EPS, family = 'poisson')
sum_glm <- summary(model.glm)

effet_CLC <- matrix(NA,4000,2)

nCLC <- length(unique(data$CLC_EPS))
code <- as.vector(sort(unique(data$CLC_EPS)))

for(i in 1:nCLC){
  effet_CLC[((i-1)*100+1):(i*100),1] <- rnorm(100,sum_glm$coefficients[i,1],sum_glm$coefficients[i,2])
  effet_CLC[((i-1)*100+1):(i*100),2] <- rep(code[i],100)
}

effet_CLC <- as.data.frame(effet_CLC)
colnames(effet_CLC) <- c("abondance","CLC")

effet_CLC %>%
  ggplot(aes(x = as.factor(CLC), y =as.numeric(abondance),fill=CLC)) +
  geom_boxplot(width=0.6,position=position_dodge(0.8),alpha=0.4,outlier.shape = NA) +
  scale_y_continuous(limits=c(-4,2))+
  labs(title="Effet habitat PARCAE",x="Habitat", y = "Effet")+
  theme_minimal()

#---------------------------------------

# EFFET ESPECE
model.glm <- glm(value ~ 0 + sp * CLC_EPS , data = data, family = 'poisson')
sum_glm <- summary(model.glm)

effet_sp <- matrix(NA,200,2)
effet_sp[1:100,1] <- rnorm(100,sum_glm$coefficients[1,1],sum_glm$coefficients[1,2])
effet_sp[1:100,2] <- rep("parmaj",100)
effet_sp[101:200,1] <- rnorm(100,sum_glm$coefficients[2,1],sum_glm$coefficients[2,2])
effet_sp[101:200,2] <- rep("parcae",100)
effet_sp <- as.data.frame(effet_sp)
colnames(effet_sp) <- c("abondance","sp")

library(ggplot2)
effet_sp %>%
  ggplot(aes(x = sp, y =as.numeric(abondance),fill=sp)) +
  geom_boxplot(width=0.6,position=position_dodge(0.8),alpha=0.4,outlier.shape = NA) +
  scale_fill_manual(values=c("#8C2423","#589482")) +
  scale_y_continuous(limits=c(-1,0))+
  labs(title="Effet mesange",x="Espece", y = "Effet")+
  theme_minimal()


#-------------------------------------------------------
# EFFET HABITAT
model.glm <- glm(value ~ 0 + CLC_EPS * sp , data = data, family = 'poisson')
sum_glm <- summary(model.glm)

effet_CLC <- matrix(NA,4000,2)

nCLC <- length(unique(data$CLC_EPS))
code <- as.vector(sort(unique(data$CLC_EPS)))

for(i in 1:nCLC){
  effet_CLC[((i-1)*100+1):(i*100),1] <- rnorm(100,sum_glm$coefficients[i,1],sum_glm$coefficients[i,2])
  effet_CLC[((i-1)*100+1):(i*100),2] <- rep(code[i],100)
}

effet_CLC <- as.data.frame(effet_CLC)
colnames(effet_CLC) <- c("abondance","CLC")

effet_CLC %>%
  ggplot(aes(x = as.factor(CLC), y =as.numeric(abondance),fill=CLC)) +
  geom_boxplot(width=0.6,position=position_dodge(0.8),alpha=0.4,outlier.shape = NA) +
  scale_y_continuous(limits=c(-4,2))+
  labs(title="Effet habitat",x="Habitat", y = "Effet")+
  theme_minimal()

#-----------------------------------------------------
# EFFET HABIAT x ESPECE

effet_CLC_sp <- matrix(NA,3900,2)

for(i in 1:(nCLC-1)){
  effet_CLC_sp[((i-1)*100+1):(i*100),1] <- rnorm(100,sum_glm$coefficients[(41+i),1],sum_glm$coefficients[(41+i),2])
  effet_CLC_sp[((i-1)*100+1):(i*100),2] <- rep(code[i+1],100)
}

effet_CLC_sp <- as.data.frame(effet_CLC_sp)
colnames(effet_CLC_sp) <- c("abondance","CLC")

effet_CLC_sp %>%
  ggplot(aes(x = as.factor(CLC), y =as.numeric(abondance),fill=CLC)) +
  geom_boxplot(width=0.6,position=position_dodge(0.8),alpha=0.4,outlier.shape = NA) +
  scale_y_continuous(limits=c(-2,1.5))+
  labs(title="Effet habitat*Bleue",x="Habitat", y = "Effet")+
  theme_minimal()





