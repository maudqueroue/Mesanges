##########################
#     CMR PASSEREAUX     #
#      Mesange Bleue     #
##########################

rm(list=ls())

library(nimble)
# Packages necessaires
devtools::install_deps(upgrade="never")

# Load fonctions importantes
devtools::load_all() 

#################
#     Data      #
#################

setwd("~/These/MNHN/Mesanges/output")

#Histoire de vie individus
load("hvie_parcae_tot.RData")

# Histoire de vie des sites
load("hvie_ID_PROG_parcae_tot.RData")

# Nb ind
N <- dim(hvie_parcae)[1]
# Nb site
S <- dim(hvie_ID_PROG_parcae)[1]
# Nb years
K <- 19

#############################
# DIMINUTION JEU DE DONNEES #
#############################

# # Combien de sites garder ?
# nsites <- 50
# 
# #On choisi al?atoirement n sites
# ID <- sort(sample(seq(1,S,1),nsites))
# 
# #On garde  que les sites s?lectionn?s
# hvie_ID_PROG_parcae <- hvie_ID_PROG_parcae[ID,]
# S <- dim(hvie_ID_PROG_parcae)[1]
# 
# #On garde que les individus des sites s?lectionn?es
# hvie_to_keep <- which((hvie_parcae$vector_ID_PROG_parcae %in% ID)==TRUE)
# hvie_parcae <- hvie_parcae[hvie_to_keep,]
# N <- dim(hvie_parcae)[1]
# 
# for(i in 1:N){
#   hvie_parcae$vector_ID_PROG_parcae[i] <- which(hvie_ID_PROG_parcae$ID_PROG==hvie_parcae$ID_PROG[i])
# }
# 
# rm(hvie_to_keep,nsites,ID)

##############################
##############################

# Les histoires de vie sont 1=juvenile, 2=adulte, 0= pas vu  

# On cr?e x la matrice des covariables pour l'?ge 
cov_age <- as.matrix(hvie_parcae[,1:K])
cov_age <- apply(cov_age,2,as.numeric)

# On remplace par un tous les 1 et 2 pour former les donn?es en 0 et 1
mydata <- as.matrix(hvie_parcae[,1:K])
mydata <- apply(mydata,2,as.numeric)
mydata[which(mydata>0)] <- 1

# Vecteur donnant le site dans lequel a ?t? captur? chaque m?sange 
ind_site <- hvie_parcae$vector_ID_PROG

#vecteur covariable individuelle recpature 
cov_ind <- hvie_parcae$cov_ind

# On remplace les histoires de vies des sites (nombre de captures secondaires par an) par des 1
# quand le site est actif, sinon ce sont des 0.

hvie_site <- as.matrix(hvie_ID_PROG_parcae[1:K])
hvie_site[which(hvie_site>0)] <- 1

# Covariable habitat
cov_hab <- hvie_ID_PROG_parcae$cov_hab

rm(hvie_ID_PROG_parcae,hvie_parcae,K,N,S)


#################
#  Dimensions   #
#################

# Nb of individuals
N <- dim(mydata)[1]

# Nb of capture events
K <- dim(mydata)[2]

# Compute the date of first capture for each individual:
f <- NULL
for (i in 1:N){
  temp <- 1:K
  f <- c(f,min(temp[mydata[i,]==1]))}

# On remplace les 0 dans la covariables ?ge : un individu ne peut ?tre juv?nile que la premi?re ann?e
# Donc apr?s la premi?re occasion de capture f, on a forcement que des 2. 

for (i in 1:N){
  for(j in 1:K){
    if (j > f[i]) {cov_age[i,j] <- 2}
    else{next}
  }
}

#################
#     Model     #
#################

code <- nimbleCode({
  
  ##### Priors and constraints ##### 
  
  # Calcul survie/recapture selon individu
  
  for (i in 1:N){
    for (t in f[i]:(K-1)){ 
      # Survie
      phi[i,t] <-  (1 / (1 + exp(-eta.phi[cov_hab[ind_site[i]],cov_age[i,t],t])))
      # Recapture
      p[i,t]   <-  1 / (1 + exp(-(eta.p[t] + gamma.i.p * cov_ind[i]))) * (1-step(-hvie_site[ind_site[i],(t+1)])) 
          } #t
  } #i 
  
  
  # Calcul survie/recapture selon habitat et age
  for (t in 1:(K-1)){ 
    # Recapture
    eta.p[t] <- mu.p + eps.p[t]
    # Random effect time
    eps.p[t] ~ dnorm(0, tau.p)
    
    for(h in 1:2){
      for (u in 1:2){
        # Survie
        eta.phi[h,u,t] <- mu.phi + gamma.h.phi[h] + gamma.u.phi[u] + eps.phi[h,u,t]
        #Random effect time
        eps.phi[h,u,t] ~ dnorm(0, tau.phi[h,u])
      }#u
    }#h
  }#t
  
  # Survie
  # mean 
  mean.phi <- exp(mu.phi) /(1 + exp(mu.phi))
  mu.phi ~ dnorm(0,1)
  
  # tau pour effet al?atoire temps
  for(u in 1:2) {
    for (h in 1:2) { 
      sigma.phi[h,u] ~ dunif(0.1,5)
      tau.phi[h,u] <- pow(sigma.phi[h,u],-2)
    }#h
  }#u
  
  # Proba recapture
  # mean
  mean.p <- exp(mu.p) /(1 + exp(mu.p)) 
  mu.p ~ dnorm(0,1)
  
  # tau pour effet al?atoire temps
  sigma.p ~ dunif(0.1,5)
  tau.p <- pow(sigma.p,-2)

  
  # Effet age et habitat
  for (v in 1:2){    
    gamma.u.phi[v] ~  dnorm(0,1)
    gamma.h.phi[v] ~  dnorm(0,1)
  }#v
  
  # Effet cov ind
  gamma.i.p   ~  dnorm(0,1)
  
  ##### Likelihood ##### 
  for (i in 1:N) {
    # Define latent state at first capture 
    z[i,f[i]] <- 1
    for (t in (f[i]+1):K){ 
      # State process
      z[i,t] ~ dbern(mu1[i,t]) 
      mu1[i,t] <- phi[i,t-1] * z[i,t-1] 
      # Observation process 
      mydata[i,t] ~ dbern(mu2[i,t]) 
      mu2[i,t] <- p[i,t-1] * z[i,t] } #t
  } #i 
}) # end model


########
# DATA #
########

# Function to create a matrix with information about known latent state z 
known.state.cjs <- function(ch){
  state <- ch
  for (i in 1:dim(ch)[1]){
    n1 <- min(which(ch[i,]==1))
    n2 <- max(which(ch[i,]==1))
    state[i,n1:n2] <- 1
    state[i,n1] <- NA
  }
  state[state==0] <- NA
  return(state)
}

# Form the list of data
const = list(N=N,
             K=K,
             f=f,
             hvie_site=hvie_site,
             ind_site = ind_site,
             cov_age=cov_age,
             cov_ind=cov_ind,
             cov_hab=cov_hab)

data = list(z=known.state.cjs(mydata),
            mydata=mydata)

#########
# INITS #
#########

# Generate inits for the latent states:
cjs.init.z <- function(ch,f){ 
  for (i in 1:dim(ch)[1]){ 
    if (sum(ch[i,])==1) next 
    n2 <- max(which(ch[i,]==1)) 
    ch[i,f[i]:n2] <- NA }
  for (i in 1:dim(ch)[1]){ 
    ch[i,1:f[i]] <- NA 
  }
  return(ch) 
}

init1  <- list(z=cjs.init.z(mydata,f),
               mu.phi=rnorm(1,0,1),
               mu.p=rnorm(1,0,1),
               sigma.phi=matrix(runif(4,0.1,5),2,2),
               sigma.p=runif(1,0.1,5),
               gamma.h.phi=rnorm(2,0,1),
               gamma.u.phi=rnorm(2,0,1),
               gamma.i.p=rnorm(1,0,1))

init2  <- list(z=cjs.init.z(mydata,f),
               mu.phi=rnorm(1,0,1),
               mu.p=rnorm(1,0,1),
               sigma.phi=matrix(runif(4,0.1,5),2,2),
               sigma.p=runif(1,0.1,5),
               gamma.h.phi=rnorm(2,0,1),
               gamma.u.phi=rnorm(2,0,1),
               gamma.i.p=rnorm(1,0,1))

inits <- list(init1,init2)

# Specify the parameters to be monitored
parameters <- c("eta.phi","eta.p","sigma.phi","tau.phi","eps.phi",
                "mean.phi","mu.phi",
                "mean.p"  ,"mu.p"  ,"sigma.p"  ,"tau.p"  ,"eps.p",
                "gamma.u.phi","gamma.h.phi","gamma.i.p")

#################
# fit the model #
#################

m <- nimbleModel(code, const, data, init1, check = FALSE, calculate = FALSE)
m$calculate()

# Inits simulation
simNodes <- c('eps.phi','eps.p')

simNodeScalar <- m$expandNodeNames(simNodes)

allNodes <- m$getNodeNames()

nodesSorted <- allNodes[allNodes %in% simNodeScalar]

for(n in nodesSorted) {
  m$simulate(n)
  depNodes <- m$getDependencies(n)
  m$calculate(depNodes)
}

m$calculate()

n.thin = 5
n.burnin = 50 #5000
n.keep.exact = 100 #10000
n.keep = n.keep.exact * n.thin
n.iter = n.burnin + n.keep
n.chains = 2

conf <- configureMCMC(m, 
                      thin=n.thin, 
                      monitors = parameters, 
                      autoBlock = FALSE)

# Build model
Rmcmc<-buildMCMC(conf)

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
               samplesAsCodaMCMC = TRUE) 

save(out,file="out_CMR_parcae.RData")


# ######################################################
# # Analyse output
# ######################################################

# rm(list=ls())
# dev.off()
# # package
# library(R2jags)
# library(coda)
# library(nimble)
# library(basicMCMCplots)
# library(boot)
# 
# # Repertoire
# setwd("~/These/MNHN/CMR_parcae/CMR_parcae_nimble")
# 
# # Data
# load("hvie_parcae_tot.RData")
# 
# # Dimension
# # Nb of individuals
# N <- dim(hvie_parcae)[1]
# # Nb of capture events
# K <- 19
# 
# # output
# load("out_CMR_parcae_nimble.RData")
# out_mat <- as.matrix(out)
# 
# #########################
# # Test de gelman
# #########################
# 
# var <- c("eta.phi[1, 1, ","eta.phi[1, 2, ","eta.phi[2, 1, ","eta.phi[2, 2, ","eps.phi[1, 1, ","eps.phi[1, 2, ","eps.phi[2, 1, ","eps.phi[2, 2, ","eta.p[","eps.p[")
# 
# 
# for(v in 1:length(var)) {
#   gelman <- NULL
#   for (t in 1:(K-1)) {
#     gelman[t] <-gelman.diag(out[,c(paste(var[v],t,"]", sep=""))], confidence = 0.95, transform=TRUE, autoburnin=TRUE)$psrf[1]
#   }
#   plot(gelman,pch=19,col="indianred4",main=paste(var[v]))
#   abline(h=1.1,lty=2)
#   abline(h=1.5,lty=2,col="red")
# }
# 
# var <- c("mean.phi","mu.phi",
#          "sigma.phi[1, 1]","tau.phi[1, 1]","sigma.phi[1, 2]","tau.phi[1, 2]","sigma.phi[2, 1]","tau.phi[2, 1]","sigma.phi[2, 2]","tau.phi[2, 2]",
#          "mean.p"  ,"mu.p"  ,"sigma.p",  "tau.p",
#          "gamma.u.phi[1]","gamma.u.phi[2]","gamma.h.phi[1]","gamma.h.phi[2]","gamma.i.p")
# 
# gelman <- NULL
# for(v in 1:length(var)) {
#   gelman[v] <-gelman.diag(out[,c(paste(var[v], sep=""))], confidence = 0.95, transform=TRUE, autoburnin=TRUE)$psrf[1]
#   print(paste(var[v],"  :   ",gelman[v],sep=""))
# }
# 
# which(gelman > 1.1)
# for(v in (which(gelman > 1.1))) {
#   chainsPlot(out, paste(var[v], sep=""))
#   legend("topleft",legend=paste("gelman : ",round(gelman[v],4),sep=""))
# }
# 
# #########################
# # Figures param?tres
# #########################
# 
# plot_parameters <- function(var,color,x_lab,y_lab,title) {
# 
#   K = K-1
# 
#   l_025 <- NULL
#   l_25  <- NULL
#   l_50  <- NULL
#   l_75  <- NULL
#   l_975 <- NULL
# 
#   for (i in 1:K){
#     l_025[i]   <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.025)))
#     l_25[i]    <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.25)))
#     l_50[i]    <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.5)))
#     l_75[i]    <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.75)))
#     l_975[i]   <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.95)))
#   }
# 
# 
#   color.transparent <- adjustcolor(color, alpha.f = 0.4)
#   color.transparent_2 <- adjustcolor(color, alpha.f = 0.2)
# 
# 
#   plot(l_50,type='l',axes=F,lwd =2,col="ivory4",ylim=c(0,1),ylab= y_lab,xlab=x_lab,main=title)
# 
#   xx<- c(1:K,K:1)
#   yy <- c(l_025[1:K],l_975[K:1])
#   polygon(xx,yy,col=color.transparent_2, border=NA)
# 
#   xx<- c(1:K,K:1)
#   yy <- c(l_25[1:K],l_75[K:1])
#   polygon(xx,yy,col=color.transparent, border=NA)
# 
#   lines(l_50,lwd =2, col=color)
# 
#   axis(1, at=c(1:K),labels=c(seq(2001,2018,1)))
#   axis(side =2, cex.axis=1, las=2)
# }
# 
# 
# par(mfrow=c(2,2))
# color<-c("#FF8830","#A6B06D","#589482","#8C2423")
# plot_parameters("eta.phi[1, 1, ",color[1],"ann?es","survie","survie juv")
# plot_parameters("eta.phi[1, 2, ",color[2],"ann?es","survie","survie ad")
# plot_parameters("eta.phi[2, 1, ",color[1],"ann?es","survie","survie juv")
# plot_parameters("eta.phi[2, 2, ",color[2],"ann?es","survie","survie ad")
# plot_parameters("eta.p[",color[3],"ann?es","detection","detection")
# 
