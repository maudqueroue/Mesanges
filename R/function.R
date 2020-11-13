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
#' @param  dsf 
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