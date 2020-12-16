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
plot_carte <- function(nb_STOC, dsf_STOC, dsf_EPS, CLC_EPS, dist_km, shp) {
  
  
  coord_buffer_STOC <- sf::st_buffer(dsf_STOC[nb_STOC,],dist=units::set_units(dist_km, km))
  
  # Coord Point STOC
  x <- as.vector(as(dsf_STOC[nb_STOC,], "Spatial")@coords)[1]
  y <- as.vector(as(dsf_STOC[nb_STOC,], "Spatial")@coords)[2]
  
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