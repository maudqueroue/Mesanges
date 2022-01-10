##########################
#     IPM PASSEREAUX     #
#      Mesanges M2       #
##########################

rm(list=ls())
#Package
library(nimble)
library(nimbleEcology)
library(tidyverse)


#################
# Fonction 
#################

dCJS_vv_sum <- nimbleFunction(
  # It is assumed that the individual has already been captured.
  # Therefore, the first entry in x represents the first possible recapture event.
  # probSurvive[t] represents survival from t-1 to t.
  # probCapture[t] represents capture probability at time t.
  run = function(x = double(1),    ## standard name for the "data"
                 probSurvive = double(1),
                 probCapture = double(1),
                 mult = double(0), #! NEWLY ADDED: argument stating number of occurences of same capture history in entire dataset 
                 len = double(0, default = 0),
                 log = integer(0, default = 0) ## required log argument
  ) {
    if (len != 0) {
      if (len != length(x)) stop("Argument len must match length of data, or be 0.")
    }
    if (length(probSurvive) < length(x) - 1)
      stop("Length of probSurvive must be at least length of data minus 1.")
    if (length(x) != length(probCapture)) stop("Length of probCapture does not match length of data.")
    if (x[1] != 1) stop("dCJS requires specifying first capture. x[1] must equal 1.")
    
    ## Note the calculations used here are actually in hidden Markov model form.
    probAliveGivenHistory <- 1
    ## logProbData will be the final answer
    logProbData <- 0
    if (len == 0) {  ## l<1 should not occur, but just in case:
      len <- length(x)
    }
    for (t in 2:len) {
      ## probAlive is P(Alive(t) | x(1)...x(t-1))
      ## probAliveGivenHistory is (Alive(t-1) | x(1)...x(t-1))
      probAlive <- probAliveGivenHistory * probSurvive[t - 1]
      if (!is.na(x[t])) {
        if (x[t] == 1) {
          ## ProbThisObs = P(x(t) | x(1)...x(t-1))
          probThisObs <- probAlive * probCapture[t]
          probAliveGivenHistory <- 1
        } else {
          probAliveNotSeen <- probAlive * (1 - probCapture[t])
          probThisObs <- probAliveNotSeen + (1 - probAlive)
          probAliveGivenHistory <- probAliveNotSeen / probThisObs
        }
        logProbData <- logProbData + log(probThisObs) * mult #! NEWLY ADDED: "mult"
      }
    }
    if (log) {
      return(logProbData)
    }
    return(exp(logProbData))
    returnType(double())
  }
)

rCJS_vv_sum <- nimbleFunction(
  run = function(n = integer(),
                 probSurvive = double(1),
                 probCapture = double(1),
                 mult = double(0), #! NEWLY ADDED: argument stating number of occurences of same capture history in entire dataset 
                 len = double(0, default = 0)) {
    if (n != 1) stop("rCJS only works for n = 1")
    if (len < 2)
      stop("len must be greater than 1.")
    if(length(probSurvive) != len - 1)
      stop("Length of probSurvive is not the same as len - 1.")
    if(length(probCapture) != len)
      stop("Length of probCapture is not the same as len.")
    ans <- numeric(length = len, init = FALSE)
    ans[1] <- 1
    alive <- 1
    if (len <= 0) return(ans)
    for (i in 2:len) {
      if (alive)
        alive <- rbinom(1, size = 1, prob = probSurvive[i - 1])
      if (alive) {
        ans[i] <- rbinom(1, size = 1, prob = probCapture[i])
      } else {
        ans[i] <- 0
      }
    }
    return(ans)
    returnType(double(1))
  }
)

#################
#     Data      #
#################

#Histoire de vie individus
# Les histoires de vie sont 1=juvenile, 2=adulte, 0= pas vu  
load(here::here('output','hvie_parcae.RData'))
load(here::here('output','hvie_parmaj.RData'))

# Histoire de vie des sites
# nombre de capture secondaires par an
load(here::here('output','hvie_ID_PROG_tits.RData'))

# Nb ind
N_bt <- dim(hvie_parcae)[1]
N_gt <- dim(hvie_parmaj)[1]

# Nb years
K <- 19

#############################
#  Regroupement d'individus #
#############################

# on renomme les colonnes
hvie_parcae <- hvie_parcae %>%
  dplyr::rename(
    a_2001 = '2001',
    a_2002 = '2002',
    a_2003 = '2003',
    a_2004 = '2004',
    a_2005 = '2005',
    a_2006 = '2006',
    a_2007 = '2007',
    a_2008 = '2008',
    a_2009 = '2009',
    a_2010 = '2010',
    a_2011 = '2011',
    a_2012 = '2012',
    a_2013 = '2013',
    a_2014 = '2014',
    a_2015 = '2015',
    a_2016 = '2016',
    a_2017 = '2017',
    a_2018 = '2018',
    a_2019 = '2019')

# Nouvelle colonne : nb_ind_groupe : nb d individu partageant la meme histoire de capture
hvie_parcae <- hvie_parcae %>% 
  dplyr::group_by(a_2001,a_2002,a_2003,a_2004,a_2005,a_2006,a_2007,a_2008,a_2009,
                  a_2010,a_2011,a_2012,a_2013,a_2014,a_2015,a_2016,a_2017,a_2018,
                  a_2019,censure,ID_PROG,cov_ind,vector_ID_PROG) %>%
  dplyr::summarise(nb_ind_groupe=dplyr::n())


# on renomme les colonnes
hvie_parmaj <- hvie_parmaj %>%
  dplyr::rename(
    a_2001 = '2001',
    a_2002 = '2002',
    a_2003 = '2003',
    a_2004 = '2004',
    a_2005 = '2005',
    a_2006 = '2006',
    a_2007 = '2007',
    a_2008 = '2008',
    a_2009 = '2009',
    a_2010 = '2010',
    a_2011 = '2011',
    a_2012 = '2012',
    a_2013 = '2013',
    a_2014 = '2014',
    a_2015 = '2015',
    a_2016 = '2016',
    a_2017 = '2017',
    a_2018 = '2018',
    a_2019 = '2019')

# Nouvelle colonne : nb_ind_groupe : nb d individu partageant la meme histoire de capture
hvie_parmaj <- hvie_parmaj %>% 
  dplyr::group_by(a_2001,a_2002,a_2003,a_2004,a_2005,a_2006,a_2007,a_2008,a_2009,
                  a_2010,a_2011,a_2012,a_2013,a_2014,a_2015,a_2016,a_2017,a_2018,
                  a_2019,censure,ID_PROG,cov_ind,vector_ID_PROG) %>%
  dplyr::summarise(nb_ind_groupe=dplyr::n())

########################################################
# On enleve les hvie pour les individus avec f[i] = l[i]
########################################################

# Histoire de vie des mesanges en 0 et 1 (pas vu/vu)
#-----------------------------
# On remplace par un tous les 1 et 2 en 1 (=vu)
# il ne reste que des 1 (vu) ou des 0 (pas vu)
mydata_bt <- as.matrix(hvie_parcae[,1:K])
mydata_bt <- apply(mydata_bt,2,as.numeric)
mydata_bt[which(mydata_bt>0)] <- 1

mydata_gt <- as.matrix(hvie_parmaj[,1:K])
mydata_gt <- apply(mydata_gt,2,as.numeric)
mydata_gt[which(mydata_gt>0)] <- 1


# Nb individu
#-------------------------------
N_bt <- nrow(mydata_bt)
N_gt <- nrow(mydata_gt)

# Compute the date of first capture for each individual:
##-------------------------------
f_bt <- NULL
for (i in 1:N_bt){
  temp <- 1:K
  f_bt <- c(f_bt,min(temp[mydata_bt[i,]==1]))}

f_gt <- NULL
for (i in 1:N_gt){
  temp <- 1:K
  f_gt <- c(f_gt,min(temp[mydata_gt[i,]==1]))}


# Compute the last date of capture for each individual with censure:
#-------------------------------
l_bt <- NULL
for (i in 1:N_bt){
  temp <- 1:K
  if (hvie_parcae$censure[i]==1){l_new <- K}
  else {l_new <- max(temp[mydata_bt[i,]==1])}
  l_bt <- c(l_bt,l_new)
  rm(l_new)}

l_gt <- NULL
for (i in 1:N_gt){
  temp <- 1:K
  if (hvie_parmaj$censure[i]==1){l_new <- K}
  else {l_new <- max(temp[mydata_gt[i,]==1])}
  l_gt <- c(l_gt,l_new)
  rm(l_new)}


# Pour quels individus l[i]=f[i]
#----------------------------
id_bt <- NULL
for(i in 1:N_bt){
  if(l_bt[i]==f_bt[i]) {
    id_bt[i] <- "id"
  }
  else{id_bt[i] <- "ok"}
}

ID_suppr_bt <- which(id_bt=="id")

id_gt <- NULL
for(i in 1:N_gt){
  if(l_gt[i]==f_gt[i]) {
    id_gt[i] <- "id"
  }
  else{id_gt[i] <- "ok"}
}

ID_suppr_gt <- which(id_gt=="id")


# On les supprime
#---------------------
hvie_parcae <- hvie_parcae[-ID_suppr_bt,]
hvie_parmaj <- hvie_parmaj[-ID_suppr_gt,]


# Choses inutiles
#----------------------
rm(ID_suppr_bt,l_bt,f_bt,mydata_bt,N_bt,id_bt,
   ID_suppr_gt,l_gt,f_gt,mydata_gt,N_gt,id_gt,
   temp,i)


###############################
# Formatage des donnees
###############################


# Histoire de vie des mesanges en 0 et 1 (pas vu/vu)
#-----------------------------
# On remplace par un tous les 1 et 2 en 1 (=vu)
# il ne reste que des 1 (vu) ou des 0 (pas vu)
mydata_bt <- as.matrix(hvie_parcae[,1:K])
mydata_bt <- apply(mydata_bt,2,as.numeric)
mydata_bt[which(mydata_bt>0)] <- 1

mydata_gt <- as.matrix(hvie_parmaj[,1:K])
mydata_gt <- apply(mydata_gt,2,as.numeric)
mydata_gt[which(mydata_gt>0)] <- 1


# Covariable age 
#----------------------
# On cree la matrice des covariables pour l'age 
cov_age_bt <- as.matrix(hvie_parcae[,1:K])
cov_age_bt <- apply(cov_age_bt,2,as.numeric)

cov_age_gt <- as.matrix(hvie_parmaj[,1:K])
cov_age_gt <- apply(cov_age_gt,2,as.numeric)


# Dans quel site a ete capturee chaque mesange
#---------------------------
# Vecteur donnant le site dans lequel a ete capture chaque mesange 
ind_site_bt <- hvie_parcae$vector_ID_PROG
ind_site_gt <- hvie_parmaj$vector_ID_PROG


# vecteur covariable individuelle recapture
# --------------------------
cov_ind_bt <- hvie_parcae$cov_ind
cov_ind_gt <- hvie_parmaj$cov_ind


# Histoire de capture des sites 
#-----------------------------
# On remplace les histoires de vies des sites en 0 et 1
# quand le site est actif = 1, pas actif = 0

hvie_site <- as.matrix(hvie_ID_PROG_tits[1:K])
hvie_site[which(hvie_site>0)] <- 1

# Covariable habitat 
#------------------------
#covariable habitat par site
cov_hab <- hvie_ID_PROG_tits$cov_hab

# Nb individu partageant la m?me observation
#--------------------------------
nb_ind_bt <- hvie_parcae$nb_ind_groupe
nb_ind_gt <- hvie_parmaj$nb_ind_groupe


# Index 
#-------------------------
load('index_parcae_HQ_point.RData')
load('index_parcae_LQ_point.RData')

load('index_parmaj_HQ_point.RData')
load('index_parmaj_LQ_point.RData')

#Moyenne
counts_bt <- matrix(NA,2,19)
counts_bt[1,2:19] <- index_parcae_HQ_point[[1]]
counts_bt[2,2:19] <- index_parcae_LQ_point[[1]]

counts_gt <- matrix(NA,2,19)
counts_gt[1,2:19] <- index_parmaj_HQ_point[[1]]
counts_gt[2,2:19] <- index_parmaj_LQ_point[[1]]

#sd
sd.counts_bt <- matrix(NA,2,19) 
sd.counts_bt[1,2:19] <- sqrt(index_parcae_HQ_point[[2]])
sd.counts_bt[2,2:19] <- sqrt(index_parcae_LQ_point[[2]])

sd.counts_gt <- matrix(NA,2,19) 
sd.counts_gt[1,2:19] <- sqrt(index_parmaj_HQ_point[[2]])
sd.counts_gt[2,2:19] <- sqrt(index_parmaj_LQ_point[[2]])


#  Dimensions   
#------------------
# Nb of individuals
N_bt <- dim(mydata_bt)[1]
N_gt <- dim(mydata_gt)[1]


# Compute the date of first capture for each individual:
#-------------------------------
f_bt <- NULL
for (i in 1:N_bt){
  temp <- 1:K
  f_bt <- c(f_bt,min(temp[mydata_bt[i,]==1]))}

f_gt <- NULL
for (i in 1:N_gt){
  temp <- 1:K
  f_gt <- c(f_gt,min(temp[mydata_gt[i,]==1]))}


# Compute the last date of capture for each individual with censure:
#-------------------------------
l_bt <- NULL
for (i in 1:N_bt){
  temp <- 1:K
  if (hvie_parcae$censure[i]==1){l_new <- K}
  else {l_new <- max(temp[mydata_bt[i,]==1])}
  l_bt <- c(l_bt,l_new)
  rm(l_new)}

l_gt <- NULL
for (i in 1:N_gt){
  temp <- 1:K
  if (hvie_parmaj$censure[i]==1){l_new <- K}
  else {l_new <- max(temp[mydata_gt[i,]==1])}
  l_gt <- c(l_gt,l_new)
  rm(l_new)}


# Covariable age 
#------------------------
# un individu ne peut etre juvenile que la premiere annee de capture
# Donc apres la premiere occasion de capture f, on a forcement que des 2 (=adulte). 

for (i in 1:N_bt){
  for(j in 1:K){
    if (j > f_bt[i] & j<=l_bt[i]) {cov_age_bt[i,j] <- 2}
    else{next}
  }
}

for (i in 1:N_gt){
  for(j in 1:K){
    if (j > f_gt[i] & j<=l_gt[i]) {cov_age_gt[i,j] <- 2}
    else{next}
  }
}


cov_age_bt[which(cov_age_bt==0)] <- NA
cov_age_gt[which(cov_age_gt==0)] <- NA

# Choses inutiles 
#-----------------
rm(index_parcae_HQ_point,index_parcae_LQ_point,hvie_ID_PROG_tits,hvie_parcae,
   index_parmaj_HQ_point,index_parmaj_LQ_point,hvie_parmaj,
   i,j,temp)

################################
# MODEL
################################

code <- nimbleCode({
  
  
  ###############
  #      BT     #
  ############### 
  
  # Initial population sizes
  # -------------------------------
  
  # Juveniles
  nN1_bt[1] ~ dnorm(8000, sd = 2500)
  nN1_bt[2] ~ dnorm(15000, sd = 5000)
  # Adults
  nNad_bt[1] ~ dnorm(2000, sd = 700)
  nNad_bt[2] ~ dnorm(4000, sd = 1500)
  
  for(h in 1:2){
    #Juveniles
    N1_bt[h,1] <- round(nN1_bt[h])
    # Adults
    Nad_bt[h,1] <- round(nNad_bt[h])
    #Total
    Ntot_bt[h,1] <- N1_bt[h,1] + Nad_bt[h,1]
  }#h
  
  
  # Productivity
  # -------------------------------
  
  for (h in 1:2){
    # intercept
    mu.fec_bt[h] ~ dnorm(log(6), sd = 0.5)
    # sd pour effet aleatoire temps
    sigma.fec_bt[h] ~ dunif(0,1)
  }#h
  
  for(t in 2:K){
    log(fec_bt[1,t]) <- mu.fec_bt[1] + DDIA_fec_bt[1] * (Nad_bt[1,t]/2000) + DDIE_fec_bt[1] * (Nad_gt[1,t]/3000) + eps.fec_bt[1,t]
    log(fec_bt[2,t]) <- mu.fec_bt[2] + DDIA_fec_bt[2] * (Nad_bt[2,t]/4000) + DDIE_fec_bt[2] * (Nad_gt[2,t]/5000) + eps.fec_bt[2,t]
    #Random effect time
    eps.fec_bt[1,t] ~ dnorm(0, sd = sigma.fec_bt[1])
    eps.fec_bt[2,t] ~ dnorm(0, sd = sigma.fec_bt[2])
  }#t
  
  # Survival / Recapture
  # -------------------------------
  
  #--- Survival 
  # selon habitat et age
  for(u in 1:2) {
    for (h in 1:2) {
      # intercept
      mean.phi_bt[h,u] <- exp(mu.phi_bt[h,u]) /(1 + exp(mu.phi_bt[h,u]))
      mu.phi_bt[h,u] ~ dnorm(0, sd= 1)
      # sd pour effet aleatoire temps
      sigma.phi_bt[h,u] ~ dunif(0,5)
    }#h
  }#u
  
  #--- Recapture
  # Effet cov ind recapture
  gamma.i.p_bt   ~  dnorm(0, sd = 1)
  # mean
  mean.p_bt <- exp(mu.p_bt) /(1 + exp(mu.p_bt)) 
  mu.p_bt ~ dnorm(0, sd = 1)
  # sd pour effet aleatoire temps
  sigma.p_bt ~ dunif(0,5)
  
  # Loop on time 
  for (t in 1:K){ 
    
    # Survival
    logit(eta.phi_bt[1,1,t]) <- mu.phi_bt[1,1] + DDIA_phiJ_bt[1] * (Ntot_bt[1,t]/10000) + DDIE_phiJ_bt[1] * (Ntot_gt[1,t]/19000) + eps.phi_bt[1,1,t]
    logit(eta.phi_bt[1,2,t]) <- mu.phi_bt[1,2] + eps.phi_bt[1,2,t]
    logit(eta.phi_bt[2,1,t]) <- mu.phi_bt[2,1] + DDIA_phiJ_bt[2] * (Ntot_bt[2,t]/11000) + DDIE_phiJ_bt[2] * (Ntot_gt[2,t]/22000) + eps.phi_bt[2,1,t]
    logit(eta.phi_bt[2,2,t]) <- mu.phi_bt[2,2] + eps.phi_bt[2,2,t]
    #Random effect time
    eps.phi_bt[1,1,t] ~ dnorm(0, sd = sigma.phi_bt[1,1])
    eps.phi_bt[1,2,t] ~ dnorm(0, sd = sigma.phi_bt[1,2])
    eps.phi_bt[2,1,t] ~ dnorm(0, sd = sigma.phi_bt[2,1])
    eps.phi_bt[2,2,t] ~ dnorm(0, sd = sigma.phi_bt[2,2])  
    
    # Recapture
    eta.p_bt[t] <- mu.p_bt + eps.p_bt[t]
    # Random effect time
    eps.p_bt[t] ~ dnorm(0, sd = sigma.p_bt)
  }#t
  
  
  # Loop on individuals
  
  for (i in 1:N_bt){
    for (t in f_bt[i]:l_bt[i]){ 
      # Survie
      phi_bt[i,t] <- eta.phi_bt[cov_hab[ind_site_bt[i]],cov_age_bt[i,t],t]
      p_bt[i,t]   <-  1 / (1 + exp(-(eta.p_bt[t] + gamma.i.p_bt * cov_ind_bt[i]))) * (1-step(-hvie_site[ind_site_bt[i],(t)])) 
    } #t
  } #i 
  
  
  # System process
  # ------------------------------- 
  
  for (h in 1:2) {
    for (t in 2:K) {
      Nad_juv_bt[h,t] ~ dbin(eta.phi_bt[h,1,t-1],round(N1_bt[h,t-1]))
      Nad_ad_bt[h,t] ~ dbin(eta.phi_bt[h,2,t-1],round(Nad_bt[h,t-1]))
      
      # Nombre d'adultes qui sont deja sur le site
      Nres_bt[h,t] <- Nad_juv_bt[h,t] + Nad_ad_bt[h,t]
      
      Nim_bt[h,t] <- round((Nad_juv_bt[h,t] + Nad_ad_bt[h,t])*0.5)
      
      # Ajout du nombre d'immigrant
      Nad_bt[h,t] <- Nres_bt[h,t] + Nim_bt[h,t]
      
      # Jeunes
      meanN1_bt[h,t] <- round(Nad_bt[h,t]*fec_bt[h,t]*0.5)
      N1_bt[h,t] ~ dpois(meanN1_bt[h,t])
      
      # Total
      Ntot_bt[h,t] <- N1_bt[h,t] + Nad_bt[h,t]
    } #t
  } #h
  
  # Count likelihood
  # ------------------------------- 
  for(h in 1:2){
    for (t in 2:K){
      logNad_bt[h,t] <- log(Nad_bt[h,t])
      counts_bt[h,t] ~ dnorm(logNad_bt[h,t], sd = sd.counts_bt[h, t])
    } #t
  } #h
  
  
  # CR likelihood
  # ------------------------------- 
  for (i in 1:N_bt) {
    mydata_bt[i,f_bt[i]:l_bt[i]] ~ dCJS_vv_sum(probSurvive = phi_bt[i, f_bt[i]:l_bt[i]],
                                               probCapture = p_bt[i, f_bt[i]:l_bt[i]],
                                               mult = nb_ind_bt[i],
                                               len = l_bt[i]-f_bt[i]+1)
  } #i 
  
  
  ###############
  #      GT     #
  ###############
  
  
  # Initial population sizes
  # -------------------------------
  
  #Juveniles
  nN1_gt[1] ~ dnorm(8000, sd = 2500)
  nN1_gt[2] ~ dnorm(17000, sd = 5500)
  # Adults
  nNad_gt[1] ~ dnorm(3000, sd = 1000)
  nNad_gt[2] ~ dnorm(5000, sd = 1500)
  
  for(h in 1:2){
    #Juveniles
    N1_gt[h,1] <- round(nN1_gt[h])
    # Adults
    Nad_gt[h,1] <- round(nNad_gt[h])
    #Total
    Ntot_gt[h,1] <- N1_gt[h,1] + Nad_gt[h,1]
  }#h
  
  
  # Productivity
  # -------------------------------
  
  for (h in 1:2){
    # intercept
    mu.fec_gt[h] ~ dnorm(log(6), sd = 0.5)
    # sd pour effet aleatoire temps
    sigma.fec_gt[h] ~ dunif(0,1)
  }#h
  
  for(t in 2:K){
    log(fec_gt[1,t]) <- mu.fec_gt[1] + DDIA_fec_gt[1] * (Nad_gt[1,t]/3000) + DDIE_fec_gt[1] * (Nad_bt[1,t]/2000) + eps.fec_gt[1,t]
    log(fec_gt[2,t]) <- mu.fec_gt[2] + DDIA_fec_gt[2] * (Nad_gt[2,t]/5000) + DDIE_fec_gt[2] * (Nad_bt[2,t]/4000) + eps.fec_gt[2,t]
    #Random effect time
    eps.fec_gt[1,t] ~ dnorm(0, sd = sigma.fec_gt[1])
    eps.fec_gt[2,t] ~ dnorm(0, sd = sigma.fec_gt[2])
  }#t
  
  # Survival / Recapture
  # -------------------------------
  
  #--- Survival
  # selon habitat et age
  for(u in 1:2) {
    for (h in 1:2) {
      # mean
      mean.phi_gt[h,u] <- exp(mu.phi_gt[h,u]) /(1 + exp(mu.phi_gt[h,u]))
      mu.phi_gt[h,u] ~ dnorm(0, sd = 1)
      # sd pour effet aleatoire temps
      sigma.phi_gt[h,u] ~ dunif(0,5)
    }#h
  }#u
  
  #--- Recapture
  # Effet cov ind recapture
  gamma.i.p_gt   ~  dnorm(0,1)
  # mean
  mean.p_gt <- exp(mu.p_gt) /(1 + exp(mu.p_gt))
  mu.p_gt ~ dnorm(0, sd = 1)
  # sd pour effet aleatoire temps
  sigma.p_gt ~ dunif(0,5)
  
  # Loop on time
  for (t in 1:K){
    
    # Survival
    logit(eta.phi_gt[1,1,t]) <- mu.phi_gt[1,1] + DDIA_phiJ_gt[1] * (Ntot_gt[1,t]/11000) + DDIE_phiJ_gt[1] * (Ntot_bt[1,t]/10000) + eps.phi_gt[1,1,t]
    logit(eta.phi_gt[1,2,t]) <- mu.phi_gt[1,2] + eps.phi_gt[1,2,t]
    logit(eta.phi_gt[2,1,t]) <- mu.phi_gt[2,1] + DDIA_phiJ_gt[2] * (Ntot_gt[2,t]/22000) + DDIE_phiJ_gt[2] * (Ntot_bt[2,t]/19000) + eps.phi_gt[2,1,t]
    logit(eta.phi_gt[2,2,t]) <- mu.phi_gt[2,2] + eps.phi_gt[2,2,t]
    #Random effect time
    eps.phi_gt[1,1,t] ~ dnorm(0, sd = sigma.phi_gt[1,1])
    eps.phi_gt[1,2,t] ~ dnorm(0, sd = sigma.phi_gt[1,2])
    eps.phi_gt[2,1,t] ~ dnorm(0, sd = sigma.phi_gt[2,1])
    eps.phi_gt[2,2,t] ~ dnorm(0, sd = sigma.phi_gt[2,2])
    
    # Recapture
    eta.p_gt[t] <- mu.p_gt + eps.p_gt[t]
    # Random effect time
    eps.p_gt[t] ~ dnorm(0, sd = sigma.p_gt)
  }#t
  
  
  # Loop on individuals
  
  for (i in 1:N_gt){
    for (t in f_gt[i]:l_gt[i]){
      # Survie
      phi_gt[i,t] <- eta.phi_gt[cov_hab[ind_site_gt[i]],cov_age_gt[i,t],t]
      p_gt[i,t]   <-  1 / (1 + exp(-(eta.p_gt[t] + gamma.i.p_gt * cov_ind_gt[i]))) * (1-step(-hvie_site[ind_site_gt[i],(t)]))
    } #t
  } #i
  
  
  # System process
  # -------------------------------
  
  for (h in 1:2) {
    for (t in 2:K) {
      Nad_juv_gt[h,t] ~ dbin(eta.phi_gt[h,1,t-1],round(N1_gt[h,t-1]))
      Nad_ad_gt[h,t] ~ dbin(eta.phi_gt[h,2,t-1],round(Nad_gt[h,t-1]))
      
      # Nombre d'adultes qui sont deja sur le site
      Nres_gt[h,t] <- Nad_juv_gt[h,t] + Nad_ad_gt[h,t]
      
      Nim_gt[h,t] <- round((Nad_juv_gt[h,t] + Nad_ad_gt[h,t])*0.5)
      
      # Ajout du nombre d'immigrant
      Nad_gt[h,t] <- Nres_gt[h,t] + Nim_gt[h,t]
      
      # Jeunes
      meanN1_gt[h,t] <- round(Nad_gt[h,t]*fec_gt[h,t]*0.5)
      N1_gt[h,t] ~ dpois(meanN1_gt[h,t])
      
      # Total
      Ntot_gt[h,t] <- N1_gt[h,t] + Nad_gt[h,t]
    } #t
  } #h
  
  # Count likelihood
  # -------------------------------
  for(h in 1:2){
    for (t in 2:K){
      logNad_gt[h,t] <- log(Nad_gt[h,t])
      counts_gt[h,t] ~ dnorm(logNad_gt[h,t], sd = sd.counts_gt[h, t])
    } #t
  } #h
  
  
  # # CR likelihood
  # # ------------------------------- 
  for (i in 1:N_gt) {
    mydata_gt[i,f_gt[i]:l_gt[i]] ~ dCJS_vv_sum(probSurvive = phi_gt[i, f_gt[i]:l_gt[i]],
                                               probCapture = p_gt[i, f_gt[i]:l_gt[i]],
                                               mult = nb_ind_gt[i],
                                               len = l_gt[i]-f_gt[i]+1)
  } #i
  
  
  ###############
  #  All tits   #
  ###############
  
  # Effet DD
  #---------------------------
  
  for(h in 1:2){
    #bt
    DDIA_phiJ_bt[h] ~ dnorm(0, sd=2)
    DDIA_fec_bt[h] ~ dnorm(0, sd=0.5)
    DDIE_phiJ_bt[h] ~ dnorm(0, sd=2)
    DDIE_fec_bt[h] ~ dnorm(0, sd=0.5)
    #gt
    DDIA_phiJ_gt[h] ~ dnorm(0, sd=2)
    DDIA_fec_gt[h] ~ dnorm(0, sd=0.5)
    DDIE_phiJ_gt[h] ~ dnorm(0, sd=2)
    DDIE_fec_gt[h] ~ dnorm(0, sd=0.5)
  }#h
  
  for(t in 1:K){
    for(h in 1:2) {
      Nad_ts[h,t] <- Nad_bt[h,t] + Nad_gt[h,t]
      Ntot_ts[h,t] <- Ntot_bt[h,t] + Ntot_gt[h,t]
    }#h
  }#t
  
})

########
# DATA #
########

# Form the list of data
const = list(K=K,
             N_bt=N_bt,
             sd.counts_bt = sd.counts_bt,
             f_bt=f_bt,
             l_bt=l_bt,
             ind_site_bt = ind_site_bt,
             cov_age_bt=cov_age_bt,
             cov_ind_bt=cov_ind_bt,
             nb_ind_bt=nb_ind_bt,
             N_gt=N_gt,
             sd.counts_gt = sd.counts_gt,
             f_gt=f_gt,
             l_gt=l_gt,
             ind_site_gt = ind_site_gt,
             cov_age_gt=cov_age_gt,
             cov_ind_gt=cov_ind_gt,
             nb_ind_gt=nb_ind_gt,
             hvie_site=hvie_site,
             cov_hab=cov_hab)

data = list(mydata_bt = mydata_bt,
            counts_bt = counts_bt,
            mydata_gt = mydata_gt,
            counts_gt = counts_gt)

#########
# INITS #
#########

init   <- list(mu.phi_bt = matrix(0,2,2),
               mu.p_bt = 0,
               mu.fec_bt = c(6,6),
               sigma.phi_bt = matrix(0.2,2,2),
               sigma.p_bt = 0.2,
               sigma.fec_bt = c(0.2,0.2),
               gamma.i.p_bt = 0,
               nN1_bt = c(8000,15000),
               nNad_bt = c(2000,4000),
               Nad_juv_bt = matrix(c(NA,rep(700,18),NA,rep(700,18)),2,19,byrow=T),
               Nad_ad_bt = matrix(c(NA,rep(1300,18),NA,rep(1300,18)),2,19,byrow=T),
               N1_bt = matrix(c(rep(6000,19),rep(14000,19)),2,19,byrow=T),
               mu.phi_gt = matrix(0,2,2),
               mu.p_gt = 0,
               mu.fec_gt = c(6,6),
               sigma.phi_gt = matrix(0.2,2,2),
               sigma.p_gt = 0.2,
               sigma.fec_gt = c(0.2,0.2),
               gamma.i.p_gt = 0,
               nN1_gt = c(8000,17000),
               nNad_gt = c(3000,5000),
               Nad_juv_gt = matrix(c(NA,rep(800,18),NA,rep(1600,18)),2,19,byrow=T),
               Nad_ad_gt = matrix(c(NA,rep(1100,18),NA,rep(2000,18)),2,19,byrow=T),
               N1_gt = matrix(c(rep(10000,19),rep(23000,19)),2,19,byrow=T),
               DDIA_phiJ_gt = c(0,0),
               DDIA_phiJ_bt = c(0,0),
               DDIA_fec_gt = c(0,0),
               DDIA_fec_bt = c(0,0),
               DDIE_phiJ_gt = c(0,0),
               DDIE_phiJ_bt = c(0,0),
               DDIE_fec_gt = c(0,0),
               DDIE_fec_bt = c(0,0))

inits <- list(init,init)

# Specify the parameters to be monitored
parameters <- c("eta.phi_bt","eta.p_bt","fec_bt",
                "sigma.phi_bt","sigma.p_bt","sigma.fec_bt",
                "eps.phi_bt","eps.p_bt","eps.fec_bt",
                "mean.phi_bt","mu.phi_bt",
                "mean.p_bt","mu.p_bt",
                "mu.fec_bt",
                "gamma.i.p_bt",
                "Ntot_bt",
                "N1_bt", "nN1_bt","Nad_ad_bt","nNad_bt","Nad_juv_bt","Nad_bt","Nim_bt",'Nres_bt',
                "eta.phi_gt","eta.p_gt","fec_gt",
                "sigma.phi_gt","sigma.p_gt","sigma.fec_gt",
                "eps.phi_gt","eps.p_gt","eps.fec_gt",
                "mean.phi_gt","mu.phi_gt",
                "mean.p_gt"  ,"mu.p_gt"  ,
                "mu.fec_gt",
                "gamma.i.p_gt",
                "Ntot_gt",
                "N1_gt", "nN1_gt","Nad_ad_gt","nNad_gt","Nad_juv_gt","Nad_gt","Nim_gt",'Nres_gt',
                'Nad_ts',"Ntot_ts",
                "DDIA_phiJ_gt", "DDIA_phiJ_bt","DDIA_fec_gt", "DDIA_fec_bt","DDIE_phiJ_gt", "DDIE_phiJ_bt","DDIE_fec_gt","DDIE_fec_bt")


#################
# fit the model #
#################

m <- nimbleModel(code, 
                 const, 
                 data, 
                 init, 
                 check = FALSE, 
                 calculate = FALSE)
m$calculate()

# Inits simulation
simNodes <- c('eps.phi_bt','eps.p_bt',"eps.fec_bt", 
              'eps.phi_gt','eps.p_gt',"eps.fec_gt")

simNodeScalar <- m$expandNodeNames(simNodes)

allNodes <- m$getNodeNames()

nodesSorted <- allNodes[allNodes %in% simNodeScalar]

for(n in nodesSorted) {
  m$simulate(n)
  depNodes <- m$getDependencies(n)
  m$calculate(depNodes)
}

rm(depNodes,n,nodesSorted,allNodes,simNodes,simNodeScalar)

m$calculate()

n.thin = 20
n.burnin = 100000
n.keep.exact = 20000
n.keep = n.keep.exact * n.thin
n.iter = n.burnin + n.keep
n.chains = 2

# Build model
Rmcmc<-buildMCMC(conf, enableWAIC = TRUE)

# Compile model
Cmodel<-compileNimble(m)

# Copile MCMC
Cmcmc<-compileNimble(Rmcmc, project=m)


# Run model
out <- runMCMC(Cmcmc, 
               niter = n.iter, 
               nburnin = n.burnin, 
               nchains = n.chains , 
               inits=inits,
               progressBar = TRUE, 
               summary = FALSE, 
               samplesAsCodaMCMC = TRUE,
               WAIC =TRUE) 

save(out,file='out_tits_M2.RData')

