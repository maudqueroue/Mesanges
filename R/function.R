#' Extract code CLC
#'
#' @param  dsf 
#'
#' @return 
#' @export
#'
give_CLC <- function(dsf, ID, shp) {
  
  coord_buffer_EPS <- sf::st_buffer(dsf,dist=units::set_units(200, m))
  
  poly <- shp %>%
    dplyr::slice(sf::st_intersects(coord_buffer_EPS$geometry, shp)[[1]])
  
  if(nrow(poly)>1) {
    
    poly <- poly %>% 
      dplyr::mutate(
        sf::st_area(sf::st_intersection(coord_buffer_EPS$geometry, poly))
      ) %>%
      dplyr::rename(area='sf::st_area(sf::st_intersection(coord_buffer_EPS$geometry, poly))')
    
    area_CLC <- poly %>%
      dplyr::group_by(CODE_12) %>%
      dplyr::summarise(area_tot = sum(area)) %>%
      dplyr::arrange(dplyr::desc(area_tot))
    
    ID$CLC_1 <- area_CLC$CODE_12[1]
    ID$CLC_2 <- area_CLC$CODE_12[2]
    
  }
  
  if(nrow(poly)==1) {
    ID$CLC_1 <- poly$CODE_12[1]
  }
  return(ID)
}

#' Extract code CLC
#'
#' @param  CLC 
#'
#' @return 
#' @export
#'
give_point <- function(ID) {
  dsf <- ID %>%
           dplyr::select("long","lat") %>%
           sf::st_as_sf(coords = c("long","lat"), crs = 4326) %>%
           sf::st_transform(crs = 2154)
}

#' Give point by site
#'
#' @param  dsf_STOC, dsf_EPS, dist_km  
#'
#' @return 
#' @export
#'
give_point_by_site <- function(dsf_STOC_i, dist_km, CLC_EPS) {
  dsf_EPS <-  Mesanges::give_point(CLC_EPS)
  coord_buffer_STOC <- sf::st_buffer(dsf_STOC_i, dist=units::set_units(dist_km, km))
  int <- sf::st_intersects(coord_buffer_STOC$geometry, dsf_EPS)
  pts <- CLC_EPS[int[[1]],"point"]
  return(pts)
}

#' Plot carte
#'
#' @param  nb_STOC, dsf_STOC, dsf_EPS, dist_km, shp  
#'
#' @return 
#' @export
#'
plot_carte <- function(nb_STOC, dsf_STOC, CLC_EPS, dist_km, shp) {
  
  coord_buffer_STOC <- sf::st_buffer(dsf_STOC[nb_STOC,],dist=units::set_units(dist_km, km))
  
  # Coord Point STOC
  x <- as.vector(as(dsf_STOC[nb_STOC,], "Spatial")@coords)[1]
  y <- as.vector(as(dsf_STOC[nb_STOC,], "Spatial")@coords)[2]
  
  dsf_EPS <-  Mesanges::give_point(CLC_EPS)
  
  points <- Mesanges::give_point_by_site(dsf_STOC[nb_STOC,], dist_km, CLC_EPS)
  
  CLC_EPS_red <- CLC_EPS %>%
    dplyr::filter(CLC_EPS$point %in% points)
  
  dsf_EPS_red <-  Mesanges::give_point(CLC_EPS_red)
  
  #Polygones interessants
  int <- sf::st_intersects(coord_buffer_STOC$geometry, shp_CLC)
  
  polygone <- shp_CLC %>%
    dplyr::filter(shp_CLC$ID %in% as.character(shp_CLC$ID[int[[1]]]))
  rm(int)
  
  print(ggplot2::ggplot() +
          ggplot2::geom_sf(data = polygone, ggplot2::aes(fill=CODE_12)) +
          ggplot2::geom_sf(data = coord_buffer_STOC, ggplot2::aes(fill=NA),colour="white",alpha=0.1) +
          ggplot2::geom_sf(data = dsf_EPS_red) +
          ggplot2::geom_sf(data = dsf_STOC[nb_STOC,],colour="white",size=2) +
          ggplot2::coord_sf(
            xlim = c((x-25100),(x+25100)),
            ylim = c((y-25100),(y+25100)),
            datum = sf::st_crs(2154),
            expand=FALSE)
  )
}

#' Plot carte
#'
#' @param  nb_STOC, dsf_STOC, shp  
#'
#' @return 
#' @export
#'
plot_carte_point <- function(nb, CLC, shp) {
  
  dsf <-  Mesanges::give_point(CLC)
  
  coord_buffer <- sf::st_buffer(dsf[nb,],dist=units::set_units(200, m))
  
  # Coord Point STOC
  x <- as.vector(as(dsf[nb,], "Spatial")@coords)[1]
  y <- as.vector(as(dsf[nb,], "Spatial")@coords)[2]
  
  #Polygones interessants
  int <- sf::st_intersects(coord_buffer$geometry, shp)
  
  polygone <- shp %>%
    dplyr::filter(shp$ID %in% as.character(shp$ID[int[[1]]]))
  rm(int)
  
  print(ggplot2::ggplot() +
          ggplot2::geom_sf(data = polygone, ggplot2::aes(fill=CODE_12))+ 
          ggplot2::geom_sf(data = coord_buffer, ggplot2::aes(fill=NA),colour="white",alpha=0.1) +
          ggplot2::geom_sf(data = dsf[nb,],colour="white",size=5) +
          ggplot2::coord_sf(
            xlim = c((x-300),(x+300)),
            ylim = c((y-300),(y+300)),
            datum = sf::st_crs(2154),
            expand=FALSE)
  )
}


#' Calcul index
#'
#' @param  data  
#'
#' @return 
#' @export
#'
index <- function (data, data_sp) { 
  
  # Nombre de stations STOC
  nsites <- length(unique(data$ID_PROG))
  # Nombre d'annees
  nyears <- length(unique(data$annee))
  
  # ajoute nb de repetitions par ID_PROG et annee
  data <- dplyr::add_count(data, as.factor(ID_PROG), as.factor(annee), name = "nrep")

    # effet fixe annee et site 
  model.glm <- glm(as.numeric(data_sp) ~ 0 + as.factor(ID_PROG) + as.factor(annee) + offset(log(nrep)), data = data, family = 'poisson')

  # Calcul de l'index en supposant que alpha et beta suivent des normales et en faisant du Monte Carlo, on simule un grand nombre de valeurs.
  
  alphaihat <- model.glm$coefficients[1:nsites]
  se_alphaihat <- summary(model.glm)$coefficients[,'Std. Error'][1:nsites]
  
  nbMC <- 100
  aik <- matrix(NA, nrow = nsites, ncol = nbMC)
  for (i in 1:nsites){
    for (j in 1:nbMC){
      aik[i,j] <- rnorm(1, mean = alphaihat[i], sd = se_alphaihat[i])
    }
  }
  
  betathat <- model.glm$coefficients[(nsites+1):length(model.glm$coefficients)]
  se_betathat <- summary(model.glm)$coefficients[,'Std. Error'][(nsites+1):length(model.glm$coefficients)]
  
  btk <- matrix(NA, nrow = nyears - 1, ncol = nbMC)
  for (i in 1:(nyears - 1)){
    for (j in 1:nbMC){
      btk[i,j] <- rnorm(1, mean = betathat[i], sd = se_betathat[i])
    }
  }
  
offset <- log(matrix(tidyr::complete(dplyr::count(data, ID_PROG, annee), 
                              ID_PROG, 
                              annee, 
                              fill = list(n = NA))$n, 
                     ncol = nyears))
  
  #gammaitk
  gammaitk <- array(NA, dim = c(nsites, nyears - 1, nbMC))
  for (t in 1:(nyears - 1)){
    for (i in 1:nsites){
      for (k in 1:nbMC){
        gammaitk[i,t,k] <- exp(offset[i,t + 1] + btk[t,k] + aik[i,k])
      }
      
    }
  }

#gammat
gammat <- t(apply(gammaitk,c(2,3), sum, na.rm = TRUE))
  
  # On calcule la moyenne des log et leur variance 
  logyt <- apply(log(gammat), 2, mean)
  
  sigma2tt <- (log(gammat) - matrix(rep(logyt, nbMC), nrow = nbMC, byrow = T))^2
  sigma2t <- apply(sigma2tt, 2, mean)
  
  
  index_mean <- logyt
  index_var <- sigma2t
  
  out <- list(index_mean, index_var)
  return(out)
}


plot_index <- function(mean, sd, col, sp, hab) {
  
  df <- data.frame(years = 2:19,
                    logy = mean,
                    sigma = sd)
  df %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = as.numeric(years), y = logy) +
    ggplot2::geom_line(col = col,size = 2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = logy - 2 * sigma,
                    ymax = logy + 2 * sigma), alpha = 0.25, fill = col) +
    ggplot2::labs(title = paste("Variations annuelles index log y [t], pour ",sp," dans habitat ", hab, sep=""),
         y = 'log(yt)') +
    ggplot2::scale_x_continuous(name="years", breaks=seq(1,19,3),labels=seq(2001,2019,3))+
    ggplot2::theme_minimal()
}



#' Calcul nouvel age
#'
#' @param  data  
#'
#' @return 
#' @export
#'
new_age <- function (data) { 
  
data <- data %>%
  tibble::add_column(new_AGE = NA)

# Juv
data$new_AGE[which(data$AGE=="PUL")]<- "P"
data$new_AGE[which(data$AGE=="1A")] <- "P"
data$new_AGE[which(data$AGE=="1A?")]<- "P"

# Ad
data$new_AGE[which(data$AGE=="+1A")]<- "A"
data$new_AGE[which(data$AGE=="+1?")]<- "A"
data$new_AGE[which(data$AGE=="2A")] <- "A"
data$new_AGE[which(data$AGE=="2A?")]<- "A"
data$new_AGE[which(data$AGE=="+2A")]<- "A"
data$new_AGE[which(data$AGE=="+2?")]<- "A"
data$new_AGE[which(data$AGE=="+3A")]<- "A"
data$new_AGE[which(data$AGE=="4A")] <- "A"
data$new_AGE[which(data$AGE=="+4A")]<- "A"
data$new_AGE[which(data$AGE=="+6A")]<- "A"

# Incertains
data$new_AGE[which(data$AGE=="")]   <- "C"
data$new_AGE[which(data$AGE=="VOL")]<- "C"

return(data)
}

#' Calcul hvie
#'
#' @param  data  
#'
#' @return 
#' @export
#'
hvie <- function(data, hvie_sp) {
  K <- 19
  N <- nrow(hvie_sp)
  
  #Pour avoir des informations
  d <- matrix(NA,nrow(hvie_sp),K)
  d2 <- matrix(NA,nrow(hvie_sp),K)
  
  for (i in 1:N) {
    a <- subset(data,data$BAGUE==hvie_sp$ID[i])
    
    for (j in 1:K) {
      b <- subset(a,substr(a$DATE,7,10)==years[j])
      
      if (nrow(b)==0) {hvie_sp[i,j] <- 0}
      if (nrow(b)==1) {hvie_sp[i,j] <- b$new_AGE[1]}
      if (nrow(b)>1)  {
        
        c <- unique(b$new_AGE)
        d[i,j] <- length(c)
        d2[i,j] <- paste(c,collapse="")
        
        if(length(which(c=="A"))>0  & length(which(c=="P"))>0 ) {hvie_sp[i,j]  <- "AP"}
        if(length(which(c=="A"))>0  & length(which(c=="P"))==0) {hvie_sp[i,j]  <- "A" }
        if(length(which(c=="A"))==0 & length(which(c=="P"))>0 ) {hvie_sp[i,j]  <- "P" }
        if(length(which(c=="A"))==0 & length(which(c=="P"))==0 ){hvie_sp[i,j]  <- "C" }
        
        rm(c)
      }
      rm(b)
    }
    rm(a)
  }
  return(list(hvie_sp,d,d2))
}

#' Calcul transient
#'
#' @param  data  
#'
#' @return 
#' @export
#'
transient <- function(data, hvie_sp) {
  K <- 19
  N <- nrow(hvie_sp)
  
  transient <- matrix(NA,N,K)
  
  for (i in 1:N) {
    a <- subset(data,data$BAGUE==hvie_sp$ID[i])
    
    for (j in 1:K) {
      b <- subset(a,substr(a$DATE,7,10)==years[j])
      transient[i,j] <- nrow(b)
      rm(b)
    }
    rm(a)
  }
  
  rm(i,j)
  
  # Premi?re occasion vu 1 seule fois ? = transient
  e <- NULL
  for (i in 1:N){
    temp <- 1:K
    e <- c(e,min(temp[transient[i,]>=1]))}
  
  # On supprime la premi?re occasion de capture sauf si il a ?t? vu deux fois
  for(i in 1:N){
    if(transient[i,e[i]]==1) {hvie_sp[i,e[i]]<-0}
    if(transient[i,e[i]]>1) {next}
  }
  
  return(hvie_sp)
}


#' Calcul transient
#'
#' @param  data  
#'
#' @return 
#' @export
#'
transient_new <- function(data, hvie_sp) {
K <- 19
N <- nrow(hvie_sp)

  transient <- matrix(NA,N,K)
  
  for (i in 1:N) {
    a <- subset(data,data$BAGUE==hvie_sp$ID[i])
    
    for (j in 1:K) {
      b <- subset(a,substr(a$DATE,7,10)==years[j])
      transient[i,j] <- nrow(b)
      rm(b)
    }
    rm(a)
  }
  
  rm(i,j)
  
  # Premi?re occasion vu 1 seule fois ? = transient
  e <- NULL
  for (i in 1:N){
    temp <- 1:K
    e <- c(e,min(temp[transient[i,]>=1]))}
  
  # On supprime la premi?re occasion de capture sauf si il a ?t? vu deux fois
  for(i in 1:N){
    if(transient[i,e[i]]==1) {
      if (hvie_sp[i,e[i]]==1) {next}
      else {hvie_sp[i,e[i]]<-0}
    }
    if(transient[i,e[i]]>1) {next}
  }
  
  return(hvie_sp)
}


#' Suppression individu hvie = 0
#'
#' @param  data  
#'
#' @return 
#' @export
#'
supp_ind <- function(hvie_sp) {
# On verifie le nombre d'individus
N <- dim(hvie_sp)[1] 

# On supprime les oiseaux qui ne sont vu que transients
hvie_sp$sum_hvie <- rep(NA,N)
for(i in 1:N){
  hvie_sp$sum_hvie[i] <-sum(as.numeric(hvie_sp[i,1:K])) 
}

# On supprime les choses inutiles
hvie_sp <- hvie_sp %>%
  dplyr::filter(!hvie_sp$sum_hvie==0) %>%
  dplyr::select(!sum_hvie)

return(hvie_sp)
}


#' Gestion des incertains : Si il est capture qu'une seule annee, on supprime
#'
#' @param  data  
#'
#' @return 
#' @export
#'
check_3 <- function(hvie_sp, check) {
  N <- dim(hvie_sp)[1] 
  for(i in 1:length(check)){
    ligne <- check[i]%%N
    colonne <- ceiling(check[i]/N)
    if(length(which(as.numeric(hvie_sp[ligne,1:K])>0))==1){hvie_sp[ligne,colonne] <- 0}
    else{next}
    rm(ligne,colonne)
  }
  return(hvie_sp)
}


#' Gestion quand plusieurs age la même annee
#'
#' @param  data  
#'
#' @return 
#' @export
#'
check_4 <- function(data, hvie_sp, check) {
  
# S'ils ont ete vus qu'une seule annee, on prend le statut defini lors du bagage
# Si pas de baguage la seule annee ou il y a ete vu, on le regarde au cas par cas
# Si individus vus plusieurs annees on regarde au cas par cas
  N <- dim(hvie_sp)[1]
  
  for(i in 1:length(check)){
    ligne <- check[i]%%N
    colonne <- ceiling(check[i]/N)
    bague <- hvie_sp$ID[ligne]
    a <- subset(data,data$BAGUE==bague)
    if(length(unique(substr(a$DATE,7,10)))==1){
      b <- subset(a, a$ACTION=="B")
      if(nrow(b)>0) {hvie_sp[ligne,colonne] <-b$new_AGE[1]}
      else {next}
    }
    else{next}
    rm(ligne, colonne, bague, a, b)
  }
  return(hvie_sp)  
} 


#' a quel site appartient chaque oiseau
#'
#' @param  data  
#'
#' @return 
#' @export
#'
ind_site <- function(data, hvie_sp) {
  
  N <- dim(hvie_sp)[1]
  hvie_sp$ID_PROG <- rep(NA,N)
  hvie_sp$nb_site <- rep(NA,N)
  
  for (i in 1:N){
    # On ne garde dans les donnees correspondant a l'oiseau i
    a <- subset(data, data$BAGUE==hvie_sp$ID[i])
    # On r?cup?re les ou l'identifiant(s) de la station
    hvie_sp$ID_PROG[i] <- paste(unique(a$ID_PROG),collapse="/")
    #On regarde s'il y a differents station pour un meme oiseaux
    hvie_sp$nb_site[i] <- length(unique(a$ID_PROG))
    rm(a)
  }
  
  return(hvie_sp)
}

#' covariable individuelle
#'
#' @param  data  
#'
#' @return 
#' @export
#'
cov_ind <- function(data, hvie_sp) {  

# Il nous faut le nombre de captures total (-1 pour transience)
# et le nombre d'annees de capture
# pour chaque individus des hvie

  N <- dim(hvie_sp)[1]
  hvie_sp$nb_capt <- rep(NA,N)
  hvie_sp$nb_years_capt <- rep(NA,N)
  
  for (i in 1:N) {
    a <- subset(data, data$BAGUE==hvie_sp$ID[i])
    hvie_sp$nb_years_capt[i] <- length(which(as.numeric(hvie_sp[i,1:K])>0))
    if(length(unique(a$ID_PROG))==1) {
      hvie_sp$nb_capt[i] <- (nrow(a)-1)
    }
    else {next}
  }
  
  return(hvie_sp)
}

#' covariable individuelle
#'
#' @param  data  
#'
#' @return 
#' @export
#'
cov_ind_new <- function(data, hvie_sp) {  
  
  # Il nous faut le nombre de captures total (-1 pour transience)
  # et le nombre d'annees de capture
  # pour chaque individus des hvie
  
  N <- dim(hvie_sp)[1]
  hvie_sp$nb_capt <- rep(NA,N)
  hvie_sp$nb_years_capt <- rep(NA,N)
  
  for (i in 1:N) {
    a <- subset(data, data$BAGUE==hvie_sp$ID[i])
    hvie_sp$nb_years_capt[i] <- length(which(as.numeric(hvie_sp[i,1:K])>0))
    if(length(unique(a$ID_PROG))==1) {
      if(length(which(as.numeric(hvie_sp[i,1:K])==1))>0){
        hvie_sp$nb_capt[i] <- nrow(a)}
      else {
      hvie_sp$nb_capt[i] <- (nrow(a)-1)
      }
    }
    else {next}
  }
  return(hvie_sp)
}

#' calcul covariable individuelle
#'
#' @param  data  
#'
#' @return 
#' @export
#'
calcul_cov_ind <- function(hvie_sp) {
  
  N <- dim(hvie_sp)[1]
  hvie_sp$cov_ind <- rep(NA,N)
  
  for(i in 1:N) {
    hvie_sp$cov_ind[i] <- log(1+((hvie_sp$nb_capt[i]-hvie_sp$nb_years_capt[i])/hvie_sp$nb_years_capt[i]))
  }
  hvie_sp <- hvie_sp %>%
    dplyr::select(!nb_capt) %>%
    dplyr::select(!nb_years_capt) 
  return(hvie_sp)
}

#' covariable habitat
#'
#' @param  data  
#'
#' @return 
#' @export
#'
cov_hab <- function(hvie_PROG) {
  hab_fav <- c("311","313")
  S <- dim(hvie_PROG)[1]
  hvie_PROG$cov_hab <- rep(2,S)
  hvie_PROG$cov_hab[hvie_PROG$CLC_hab %in% hab_fav] <- 1 
  
  hvie_PROG <- hvie_PROG %>%
    dplyr::select(-CLC_1) %>%  
    dplyr::select(-CLC_2) %>%  
    dplyr::select(-CLC_bagueur) %>% 
    dplyr::select(-CLC_hab) %>% 
    dplyr::select(-lat) %>%  
    dplyr::select(-long) 
  
  return(hvie_PROG)
}


#' lien hvie_PROG et hvie_IND
#'
#' @param  data  
#'
#' @return 
#' @export
#'
link_hvie <- function(hvie_sp, hvie_PROG) {

# On cree le vecteur avec le nouveau numero de site de 1 au nombre total de sites
  N <- dim(hvie_sp)[1]
  
  hvie_sp$vector_ID_PROG <- rep(NA,N)

  for(i in 1:N){
    hvie_sp$vector_ID_PROG[i] <- which(hvie_PROG$ID_PROG==hvie_sp$ID_PROG[i])
  }
  return(hvie_sp)
}  


#' lien hvie_PROG et hvie_IND
#'
#' @param  data  
#'
#' @return 
#' @export
#'
plot_glm <- function (data, data_sp) { 
  
  # Nombre de stations STOC
  nsites <- length(unique(data$ID_PROG))
  # Nombre d'annees
  nyears <- length(unique(data$annee))
  
  # effet fixe annee et site 
  model.glm <- glm(as.numeric(data_sp) ~ 0 + as.factor(ID_PROG) + as.factor(annee), data = data, family = 'poisson')
  
  pred <- predict(model.glm, type = 'response', se = TRUE)
  data$fit.glm <- rep(NA, nrow(data))
  data$fit.glm[!is.na(data_sp)] <- pred$fit
  data$sefit.glm <- rep(NA, nrow(data))
  data$sefit.glm[!is.na(data_sp)] <- pred$se.fit
  
  # On represente graphiquement les pr?dictions pour chacun des sites
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = as.numeric(annee), y = as.numeric(data_sp))) +
    ggplot2::facet_wrap(ggplot2::vars(ID_PROG), scales = "free_y") +
    ggplot2::geom_point(alpha = 0.25) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = fit.glm - 2 * sefit.glm,
                    ymax = fit.glm + 2 * sefit.glm), alpha = 0.25, fill = "red") +
    ggplot2::geom_line(ggplot2::aes(y = fit.glm,group=1), color = "red") +
    ggplot2::labs(y= "index", x="annee",title = 'Ajustement du modele Poisson')
}


#' Selection habitat
#'
#' @param  data  
#'
#' @return 
#' @export
#'
select_habitat_STOC <- function (CLC, data_bagueur) { 
  
  # Add CLC bagueur to CLC_STOC
  CLC <- dplyr::left_join(CLC, data_bagueur, by = c("ID_PROG")) 
  
  CLC <- CLC %>%
    tibble::add_column(CLC_hab = NA)
  
  
  # Choix de l'habitat à conserver
  
  for (i in 1 :nrow(CLC)) {
    
    # Si habitat primaire forestier, on garde 
    if(CLC$CLC_1[i] == "311" | CLC$CLC_1[i] == "312" | CLC$CLC_1[i] == "313"){
      CLC$CLC_hab[i] <- CLC$CLC_1[i] }
    
    # Si l'habitat primaire n'est pas forestier, on regarde l'habitat secondaire
    if(is.na(CLC$CLC_hab[i])==TRUE) {
      
      # On regarde que si l'habitat secondaire est disponible
      if(is.na(CLC$CLC_2[i])==FALSE) {
        
        if(CLC$CLC_2[i] == "311" | CLC$CLC_2[i] == "312" | CLC$CLC_2[i] == "313"){
          CLC$CLC_hab[i] <- CLC$CLC_2[i] }
        
      }
      # Si l'habitat secondaire n'est pas forestier, on regarde l'habitat décrit par le bagueur 
      if(is.na(CLC$CLC_hab[i])==TRUE) {
        
        # On regarde que si le bagueur a défini l'habitat de la station
        if(is.na(CLC$CLC_bagueur[i])==FALSE) {
          
          if(CLC$CLC_bagueur[i] == "311" | CLC$CLC_bagueur[i] == "312" | CLC$CLC_bagueur[i] == "313"){
            CLC$CLC_hab[i] <- CLC$CLC_bagueur[i] }
          
        }
        
        # Si aucun habitat n'est forestier, on garde l'habitat primaire
        if(is.na(CLC$CLC_hab[i])==TRUE) {
          CLC$CLC_hab[i] <- CLC$CLC_1[i] }
        
      }
    }
  }  
  
  return(CLC)
  
}