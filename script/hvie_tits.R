######                                       ######
###### SCRIPT DONNEES MODELE PARCAE          ######
######                                       ######

# Jeu de donnees entrant : histoire de vie des sites
#                          Premier tri donnees MNHN

# Jeu de donnees sortant : hvie_parcae_tot
#                          hvie_ID_prog_parcae_tot   

rm(list=ls())

# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 

# hvie tits
#--------------------
load(here::here('output',"hvie_parcae.RData"))
load(here::here('output',"hvie_parmaj.RData"))

# Hvie site
#--------------------
load(here::here('output',"hvie_ID_PROG.RData"))

#Quels sont les sites concernant les parcae
hvie_ID_PROG_parcae <- hvie_ID_PROG %>%
  dplyr::filter(hvie_ID_PROG$ID_PROG %in% unique(hvie_parcae$ID_PROG))

#Quels sont les sites concernant les parmaj
hvie_ID_PROG_parmaj <- hvie_ID_PROG %>%
  dplyr::filter(hvie_ID_PROG$ID_PROG %in% unique(hvie_parmaj$ID_PROG))

# Sites en commun pour les deux
#--------------------
hvie_ID_PROG_tits <- hvie_ID_PROG_parmaj %>%
  dplyr::filter(hvie_ID_PROG_parmaj$ID_PROG %in% hvie_ID_PROG_parcae$ID_PROG)
hvie_ID_PROG_tits <- hvie_ID_PROG_tits[-which(hvie_ID_PROG_tits$ID_PROG==56),]


rm(hvie_ID_PROG, hvie_ID_PROG_parcae, hvie_ID_PROG_parmaj)

# On filtre pour ne garder qu eles hvie communes aux deux espèces
#--------------------
ID_PROG_tits <- unique(hvie_ID_PROG_tits$ID_PROG)

hvie_parcae <- hvie_parcae %>%
  dplyr::filter(hvie_parcae$ID_PROG %in% ID_PROG_tits)
hvie_parmaj <- hvie_parmaj %>%
  dplyr::filter(hvie_parmaj$ID_PROG %in% ID_PROG_tits)

# type d'habitat
#--------------------
# On ajoute les infos contenu dans CLC_STOC aux hvies 
load(here::here("output","CLC_STOC.RData"))
hvie_ID_PROG_tits <- dplyr::left_join(hvie_ID_PROG_tits, CLC_STOC, by = c("ID_PROG")) 

rm(CLC_STOC)  

# type d'habitat
hvie_ID_PROG_tits <- Mesanges::cov_hab(hvie_ID_PROG_tits)

# 11. Creation vecteur pour lier hvie individus et hvie site
#---------
hvie_parcae <- Mesanges::link_hvie(hvie_parcae, hvie_ID_PROG_tits)
hvie_parmaj <- Mesanges::link_hvie(hvie_parmaj, hvie_ID_PROG_tits)

save(hvie_ID_PROG_tits,file=here::here('output',"hvie_ID_PROG_tits.RData"))
save(hvie_parmaj,file=here::here('output',"hvie_parmaj.RData"))
save(hvie_parcae,file=here::here('output',"hvie_parcae.RData"))

