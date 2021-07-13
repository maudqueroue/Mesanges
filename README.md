# Mesanges
Mon travail sur les mésanges :baby_chick: (mise à jour à faire)

## Le plan à suivre :clipboard:

### Tri des données Points d'écoute :loud_sound:

#### 1.	Tri_data_EPS : Tri des données des points d’écoute (EPS) _(rapide)_
*	Fichiers entrés (2) : STOC_EPS (data) et EPS_carres (data)
*	Reconstitution des points d’écoute pour les données manquantes
*	Fichiers sortis (1) : data_EPS (output)

### Récupération des habitats Corine Land Cover :national_park:

#### 2.	CLC_EPS : Récupération des habitats CLC pour les points d’écoute (EPS) _(LONG ! à faire sur cluster : plusieurs jours)_
* Fichiers entrés (2) : couche corine land cover CLC12_FR_RGF_SHP (data) et data_EPS (output)
* Récupération du type d’habitat pour chaque point d’écoute et figure
* Fichiers sortis (1) : CLC_EPS (output) 

#### 3.	CLC_STOC : Récupération des habitats CLC pour les stations de capture (STOC) _(LONG ! quelques heures)_
* Fichiers entrés (2) : couche corine land cover CLC12_FR_RGF_SHP (data) et coord_STOC (data)
* Récupération du type d’habitat pour chaque station de capture, figure CLC 
* Fichiers sortis (1) : CLC_STOC (output) 

### Comment lier les stations STOC et les points d'écoute :loud_sound::bird::deciduous_tree:

#### 4.	Link_EPS_STOC : Lien entre les stations de capture (STOC) et les points d’écoute (EPS) _(rapide)_
*	Fichiers entrés (3) : couche corine land cover CLC12_FR_RGF_SHP (data) et CLC_EPS (output) et CLC_STOC (output)
*	Récupération du nom des points d’écoute autour de chaque station STOC et figure
* Fichiers sortis (1) : EPS_by_STOC (output)

### Création des histoires de capture à partir des données STOC :hatching_chick::baby_chick::hatched_chick:

#### 5. Tri_data_STOC : Tri des données des stations de capture (STOC) _(rapide)_
* Fichiers entrés (1) : Data_MNHN (data)
* Tri des données et création des histoires de vie des sites
* Fichiers sortis (2) : data_STOC (ouput) et hvie_ID_PROG (output)

#### 6.	hvie_sp : Histoire de capture par espèce _(rapide)_
* Fichiers entrés (3) : data_STOC (output), hvie_ID_PROG (output), CLC_STOC (output) 
* Création des histoires de capture pour les individus avec les covariables correspondantes
* Ajout d’information dans les histoires de vie des sites pour les sites capturant l’espèce
*	Fichiers sortis (2) : hvie_sp_tot (output) et hvie_ID_PROG_sp_tot (output)

### Calcul des index :chart_with_upwards_trend:

#### 7. Calcul_index : Calcul des index pour chaque sp selon l’habitat _(rapide)_
* Fichiers entrés (6) : data_EPS (output) et CLC_STOC (output) et CLC_EPS (output) et EPS_by_STOC (output) et hvie_ID_PROG_sp (output)
* Création d’un fichier répertoriant pour chaque station de capature (STOC), les sp comptées dans les points d’écoute (EPS) dans le buffer de 25km tout en gardant     l’information sur les covariables habitat
*	Création des index selon l’espèce et le type d’habitat.
* Fichiers sortis (8) : index_sp_hab_mean et index_sp_hab_sd

### Les modèles :computer::bar_chart:

#### 8. CMR_sp : Modèle CMR pour chaque sp _(LONG ! à faire sur cluster, quelques heures)_
*	Fichiers entrés (2) : hvie_sp_tot (output) et hvie_ID_PROG_sp_tot (ouput) 
* Fichiers sortis : out_CMR_sp (1) _(pas sur github car fichiers lourds)_

#### 9.	IPM_sp : Modèle intégré pour chaque sp _(LONG ! à faire sur cluster, plusieurs heures)_
*	Fichiers entrés (6) : hvie_sp_tot (output) et hvie_ID_PROG_sp_tot (ouput) et index_sp_hab_mean (output) et index_sp_hab_sd (output)
* Fichiers sortis : out_IPM_sp (1) _(pas sur github car fichiers lourds)_

