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
give_point_by_site <- function(dsf_STOC, dsf_EPS, dist_km) {
  coord_buffer_STOC <- sf::st_buffer(dsf_STOC, dist=units::set_units(dist_km, km))
  int <- sf::st_intersects(coord_buffer_STOC$geometry, dsf_EPS)
  point_by_site <- CLC_EPS[int[[1]],"point"]
  pts <- point_by_site %>% dplyr::pull(point)
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
  
  points <- Mesanges::give_point_by_site(dsf_STOC[nb_STOC,], dsf_EPS, dist_km)
  
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
          ggplot2::geom_sf(data = dsf_STOC[nb_STOC,],colour="white",size=7) +
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
plot_carte_STOC <- function(nb_STOC, CLC_STOC, shp) {
  
  dsf_STOC <-  Mesanges::give_point(CLC_STOC)
  
  coord_buffer_STOC <- sf::st_buffer(dsf_STOC[nb_STOC,],dist=units::set_units(200, m))
  
  # Coord Point STOC
  x <- as.vector(as(dsf_STOC[nb_STOC,], "Spatial")@coords)[1]
  y <- as.vector(as(dsf_STOC[nb_STOC,], "Spatial")@coords)[2]
  
  #Polygones interessants
  int <- sf::st_intersects(coord_buffer_STOC$geometry, shp_CLC)
  
  polygone <- shp_CLC %>%
    dplyr::filter(shp_CLC$ID %in% as.character(shp_CLC$ID[int[[1]]]))
  rm(int)
  
  print(ggplot2::ggplot() +
          ggplot2::geom_sf(data = polygone, ggplot2::aes(fill=CODE_12))+ 
          ggplot2::geom_sf(data = coord_buffer_STOC, ggplot2::aes(fill=NA),colour="white",alpha=0.1) +
          ggplot2::geom_sf(data = dsf_STOC[nb_STOC,],colour="white",size=5) +
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
  
  # effet fixe annee et site 
  model.glm <- glm(as.numeric(data_sp) ~ 0 + as.factor(ID_PROG) + as.factor(annee), data = data, family = 'poisson')
  
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
  
  # On calcule le gamma
  sumi_expaik <- apply(exp(aik), 2, sum)                                                              
  
  #gammat
  gammat <- matrix(NA, nrow = nbMC, ncol = (nyears - 1))
  for (i in 1:(nyears - 1)){
    for (j in 1:nbMC){
      gammat[j,i] <- sumi_expaik[j] * exp(btk[i,j])
    }
  }
  
  # On calcule la moyenne des log et leur variance 
  logyt <- apply(log(gammat), 2, mean)
  
  sigma2tt <- (log(gammat) - matrix(rep(logyt, nbMC), nrow = nbMC, byrow = T))^2
  sigma2t <- apply(sigma2tt, 2, mean)
  
  
  index_mean <- logyt
  index_sd <- sigma2t
  
  out <- list(index_mean, index_sd)
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
