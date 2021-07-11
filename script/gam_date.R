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

# on supprime data sans date, sans ID_PROG, sans espece
data <- data %>% 
  dplyr::filter(!is.na(ID_PROG))%>% 
  dplyr::filter(!is.na(DATE)) %>% 
  dplyr::filter(!is.na(ESPECE))


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

data$annee <- substr(data$DATE,7,10)

# 2 QUELS MOIS GARDE T ON ?
#------------------------------------------------------------------------
unique(substr(data$DATE,4,5))

data <- data %>%
  dplyr::filter(substr(data$DATE,4,5) %in% c("04", "05", "06", "07")) 

unique(substr(data$DATE,4,5))

#4. Gestion age 
#------------------------------------------------------
# on redefini l'age soit adulte soit poussin
data <- Mesanges::new_age(data)
# on supprime les incertains
data <- data[-which(data$new_AGE=="C"),]
data <- data[-which(is.na(data$new_AGE)==TRUE),]

unique(data$new_AGE)

# On reformate les données
#------------------------------------------------------------------------

#on enleve les doublons : oiseaux capturé deux fois meme journée

data_STOC <- data %>% 
  dplyr::distinct(DATE,ID_PROG,BAGUE,ESPECE,new_AGE,.keep_all = TRUE)

data_STOC <- data %>% 
  dplyr::group_by(ID_PROG,DATE,ESPECE,annee,new_AGE) %>%
  dplyr::summarise(n=dplyr::n()) %>%
  tidyr::pivot_wider(names_from = ESPECE, values_from = n) 

# on range dans l'ordre croissant des dates
data_STOC$DATE <- lubridate::dmy(data_STOC$DATE)
data_STOC  <- data_STOC %>%
  dplyr::arrange(DATE)

# NA deviennent 0
data_STOC[is.na(data_STOC)] <- 0


# Pour sylatr 

data_sylatr <- data_STOC %>%
  dplyr::select('ID_PROG','DATE','annee','new_AGE','SYLATR')

######## tricherie date pour plot
data_sylatr$fDATE <- data_sylatr$DATE

for(i in 1:nrow(data_sylatr)){
  data_sylatr$fDATE[i] <- update(data_sylatr$fDATE[i],year = 2001)
}

### Date en chiffre a parti de mars
num_Date <- seq(as.Date("2001-03-01"), as.Date("2001-07-31"), by="days")

data_sylatr$nDATE <- NA
for (i in 1:nrow(data_sylatr)){
  data_sylatr$nDATE[i] <- which(num_Date==data_sylatr$fDATE[i])
}

data_sylatr <- data_sylatr %>%
  dplyr::select(!'fDATE')

# Age en colonne 
data_sylatr <- data_sylatr %>% 
  tidyr::pivot_wider(names_from = new_AGE, values_from = c('SYLATR')) 
data_sylatr[is.na(data_STOC)] <- 0

#5. QUELLES STATIONS GARDER ?
#---------------------------------------------------
load(here::here('output','hvie_ID_PROG_syl.RData'))

ID_PROG <-  unique(hvie_ID_PROG_syl$ID_PROG)

data_sylatr <- data_sylatr[which((data_sylatr$ID_PROG %in% ID_PROG)==TRUE),]


# Pour sylbor

data_sylbor <- data_STOC %>%
  dplyr::select('ID_PROG','DATE','annee','new_AGE','SYLBOR')

######## tricherie date pour plot
data_sylbor$fDATE <- data_sylbor$DATE

for(i in 1:nrow(data_sylbor)){
  data_sylbor$fDATE[i] <- update(data_sylbor$fDATE[i],year = 2001)
}

data_sylbor$nDATE <- NA
for (i in 1:nrow(data_sylbor)){
  data_sylbor$nDATE[i] <- which(num_Date==data_sylbor$fDATE[i])
}

data_sylbor <- data_sylbor %>%
  dplyr::select(!'fDATE')

# Age en colonne 
data_sylbor <- data_sylbor %>% 
  tidyr::pivot_wider(names_from = new_AGE, values_from = c('SYLBOR')) 
data_sylbor[is.na(data_STOC)] <- 0

#5. QUELLES STATIONS GARDER ?
#---------------------------------------------------
data_sylbor <- data_sylbor[which((data_sylbor$ID_PROG %in% ID_PROG)==TRUE),]


rm(hvie_ID_PROG_syl,ID_PROG,data,i,num_Date, data_STOC)



##################################################################
# GAM SYLATR
##################################################################

library(gamm4)

# Préparation des valeurs d'incrémentation et du nombre d'années
IntervalleTemps <- seq(-20,+15,1)  
l <- length(IntervalleTemps)
annees <- as.vector(unique(data_sylatr$annee))
Nombre_annees <- length(annees)-1
data_annee_ref <- subset(data_sylatr, annee=="2019")
out3 <- data.frame('annee'=rep(0,Nombre_annees),"Decalage_pheno"=rep(0,Nombre_annees),"min"=rep(0,Nombre_annees),"a2"=rep(0,Nombre_annees))
a <- rep(0,Nombre_annees)
a2 <- rep(0,Nombre_annees)

# Boucle générale sur les années
for (i in 1:Nombre_annees) {
  data_annee_test <- subset(data_sylatr, annee==annees[i])
  data_annee <- rbind(data_annee_test, data_annee_ref)
  out1 <- data.frame("annee"=rep(0,l),"DT"=rep(0,l),"AIC"=rep(0,l),"AIC2"=rep(0,l))
  
  # Boucle sur les décalages phénologiques testés
  for (h in 1:l){
    Decalage_test <- IntervalleTemps[h]
    data_annee$date <- rep(NA, nrow(data_annee))
    
    ### Boucle permettant de créer les nouvelles dates décalées de SYLBOR en rajoutant le décalage 
    # phénologique testé sans toucher à SYLATR qui sert de référence. 
    # Ainsi, une valeur de décalage phénologique négative qu'on avance les données de SYLBOR 
    for (j in 1:nrow(data_annee)){
      if (data_annee$annee[j]=="2019"){
        data_annee$date[j] <- data_annee$nDATE[j]
      }
      else {
        data_annee$date[j] <- data_annee$nDATE[j]+Decalage_test
      }
    }
    
    ### Ajustement du modèle gamm à changer en fonction de vos données 
    # (ici, le modèle ne contient pas d'effet site). 
    # Attention, si le modèle change, il est probable que les indices des valeurs à récupérer 
    # dans la matrice de résultats du modèle changent aussi. 
    # Cela concerne la ligne  sortie[t,3]=(-logLik(model$mer))[[1]]
    model <- gamm4(cbind(P,A)~s(date),random=~(1|ID_PROG),family=binomial,data=data_annee)
    out1$annee[h] <- annees[i]
    out1$DT[h]    <- Decalage_test
    out1$AIC[h]   <- summary(model$mer)$AICtab[1]
    out1$AIC2[h]   <- (-logLik(model$mer))[[1]]
    data_annee    <- data_annee[,1:((ncol(data_annee))-1)]
    
    # plus utile
    rm(model,Decalage_test,j)
  }
  
  ### Partie du programme qui sélectionne les 10 valeurs d'AIC autour du minimum 
  # pour ensuite opérer une régression quadratique et estimer le décalage phénologique
  out2 <- data.frame('annee' =rep(0,11),'AIC'=rep(0,11),'DT'=rep(0,11))
  
  # Recherche du minimum d'AIC et du décalage correspondant
  a[i] <- which(out1$AIC==min(out1$AIC))
  a2[i] <- which(out1$AIC2==min(out1$AIC2))
  # # Sélection des 10 valeurs d'AIC et des décalages correspondant en fonction de la position du minimum dans la série de décalages testés. 
  out2$AIC   <- signif(out1$AIC[(a[i]-5):(a[i]+5)],digit=6)
  out2$DT    <- out1$DT[(a[i]-5):(a[i]+5)]
  out2$annee <- annees[i]
  # 
  # ### Partie du programme qui opère une régression quadratique pour estimer le décalage phénologique entre les données de sylbor et sylatr
  DT_carre <- out2$DT * out2$DT
  Regression_quadratique <- summary(glm(out2$AIC ~ DT_carre + out2$DT)) 
  # 
  out3$annee[i] <- annees[i]			
  out3$Decalage_pheno[i] <- (-Regression_quadratique[[12]][3]/(2*Regression_quadratique[[12]][2]))		
  out3$min[i] <- IntervalleTemps[a[i]]
  out3$a2[i] <- IntervalleTemps[a2[i]]
  
  
  rm(data_annee,out1,out2,Regression_quadratique,DT_carre,h,data_annee_test)
}

DP_sylatr <- out3$Decalage_pheno
save(DP_sylatr,file = here::here('output',"DP_sylatr.RData"))


############ Comparaison decalage pheno et decalage arrivée fauvettes à tête noire

load(here::here('output','DP.RData'))
load(here::here('output','DP_sylatr.RData'))

par(mfrow=c(1,1))
color<-c("#FF8830","#A6B06D","#589482","#8C2423")
plot(DP[1:19], c(DP_sylatr[1:18],0), lwd=2, pch=19, col=color[1], axes=F, xlab= "decalage phenologique", ylab= "decalage arrivee tete noire")
axis(1)
axis(2, las=2)

##################################################################
# GAM SYLBOR
##################################################################

###### Par curiosité décalage arrivée des fauvettes des jardins  

library(gamm4)

# Préparation des valeurs d'incrémentation et du nombre d'années
IntervalleTemps <- seq(-20,+15,1)  
l <- length(IntervalleTemps)
annees <- as.vector(unique(data_sylbor$annee))
Nombre_annees <- length(annees)-1
data_annee_ref <- subset(data_sylbor, annee=="2019")
out3 <- data.frame('annee'=rep(0,Nombre_annees),"Decalage_pheno"=rep(0,Nombre_annees),"min"=rep(0,Nombre_annees),"a2"=rep(0,Nombre_annees))
a <- rep(0,Nombre_annees)
a2 <- rep(0,Nombre_annees)

# Boucle générale sur les années
for (i in 1:Nombre_annees) {
  data_annee_test <- subset(data_sylbor, annee==annees[i])
  data_annee <- rbind(data_annee_test, data_annee_ref)
  out1 <- data.frame("annee"=rep(0,l),"DT"=rep(0,l),"AIC"=rep(0,l),"AIC2"=rep(0,l))
  
  # Boucle sur les décalages phénologiques testés
  for (h in 1:l){
    Decalage_test <- IntervalleTemps[h]
    data_annee$date <- rep(NA, nrow(data_annee))
    
    ### Boucle permettant de créer les nouvelles dates décalées de SYLBOR en rajoutant le décalage 
    # phénologique testé sans toucher à SYLATR qui sert de référence. 
    # Ainsi, une valeur de décalage phénologique négative qu'on avance les données de SYLBOR 
    for (j in 1:nrow(data_annee)){
      if (data_annee$annee[j]=="2019"){
        data_annee$date[j] <- data_annee$nDATE[j]
      }
      else {
        data_annee$date[j] <- data_annee$nDATE[j]+Decalage_test
      }
    }
    
    ### Ajustement du modèle gamm à changer en fonction de vos données 
    # (ici, le modèle ne contient pas d'effet site). 
    # Attention, si le modèle change, il est probable que les indices des valeurs à récupérer 
    # dans la matrice de résultats du modèle changent aussi. 
    # Cela concerne la ligne  sortie[t,3]=(-logLik(model$mer))[[1]]
    model <- gamm4(cbind(P,A)~s(date),random=~(1|ID_PROG),family=binomial,data=data_annee)
    out1$annee[h] <- annees[i]
    out1$DT[h]    <- Decalage_test
    out1$AIC[h]   <- summary(model$mer)$AICtab[1]
    out1$AIC2[h]   <- (-logLik(model$mer))[[1]]
    data_annee    <- data_annee[,1:((ncol(data_annee))-1)]
    
    # plus utile
    rm(model,Decalage_test,j)
  }
  
  ### Partie du programme qui sélectionne les 10 valeurs d'AIC autour du minimum 
  # pour ensuite opérer une régression quadratique et estimer le décalage phénologique
  out2 <- data.frame('annee' =rep(0,11),'AIC'=rep(0,11),'DT'=rep(0,11))
  
  # Recherche du minimum d'AIC et du décalage correspondant
  a[i] <- which(out1$AIC==min(out1$AIC))
  a2[i] <- which(out1$AIC2==min(out1$AIC2))
  # # Sélection des 10 valeurs d'AIC et des décalages correspondant en fonction de la position du minimum dans la série de décalages testés. 
  out2$AIC   <- signif(out1$AIC[(a[i]-5):(a[i]+5)],digit=6)
  out2$DT    <- out1$DT[(a[i]-5):(a[i]+5)]
  out2$annee <- annees[i]
  # 
  # ### Partie du programme qui opère une régression quadratique pour estimer le décalage phénologique entre les données de sylbor et sylatr
  DT_carre <- out2$DT * out2$DT
  Regression_quadratique <- summary(glm(out2$AIC ~ DT_carre + out2$DT)) 
  # 
  out3$annee[i] <- annees[i]			
  out3$Decalage_pheno[i] <- (-Regression_quadratique[[12]][3]/(2*Regression_quadratique[[12]][2]))		
  out3$min[i] <- IntervalleTemps[a[i]]
  out3$a2[i] <- IntervalleTemps[a2[i]]
  
  
  rm(data_annee,out1,out2,Regression_quadratique,DT_carre,h,data_annee_test)
}

DP_sylbor<- out3$Decalage_pheno
save(DP_sylbor,file = here::here('output',"DP_sylbor.RData"))


par(mfrow=c(1,1))
color<-c("#FF8830","#A6B06D","#589482","#8C2423")
plot(DP[1:19], c(DP_sylbor[1:18],0), lwd=2, pch=19, col=color[1], axes=F, xlab= "decalage phenologique", ylab= "decalage arrivee jardin")
axis(1)
axis(2, las=2)
