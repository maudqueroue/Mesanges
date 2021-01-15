##########################
#     IPM PASSEREAUX     #
#   Mesange Charbonniere #
##########################

rm(list=ls())
#Package
library(nimble)

# ----------------------------------------------------------------

#################
#     Data      #
#################
#setwd("~/These/MNHN/Mesanges/output")
#setwd("~/These/MNHN/Mesanges_cluster/IPM_parmaj_new")
setwd("//hpcm.cluster.calcul.hpc/Shared-2/QUEROUE/IPM_parmaj")

#Histoire de vie individus
load('hvie_parmaj_tot.RData')

# Histoire de vie des sites
load('hvie_ID_PROG_parmaj_tot.RData')

# Nb ind
N <- dim(hvie_parmaj)[1]
# Nb site
S <- dim(hvie_ID_PROG_parmaj)[1]
# Nb years
K <- 19

# Les histoires de vie sont 1=juvenile, 2=adulte, 0= pas vu  

# On cree la matrice des covariables pour l'age 
cov_age <- as.matrix(hvie_parmaj[,1:K])
cov_age <- apply(cov_age,2,as.numeric)

# On remplace par un tous les 1 et 2 pour former les donnees en 0 et 1
mydata <- as.matrix(hvie_parmaj[,1:K])
mydata <- apply(mydata,2,as.numeric)
mydata[which(mydata>0)] <- 1

# Vecteur donnant le site dans lequel a ete capture chaque mesange 
ind_site <- hvie_parmaj$vector_ID_PROG

#vecteur covariable individuelle recpature 
cov_ind <- hvie_parmaj$cov_ind

# On remplace les histoires de vies des sites (nombre de captures secondaires par an) par des 1
# quand le site est actif, sinon ce sont des 0.

hvie_site <- as.matrix(hvie_ID_PROG_parmaj[1:K])
hvie_site[which(hvie_site>0)] <- 1

# Covariable habitat
cov_hab <- hvie_ID_PROG_parmaj$cov_hab

# Log habitat
load('index_parmaj_hab1_mean.RData')
load('index_parmaj_hab1_var.RData')
load('index_parmaj_hab2_mean.RData')
load('index_parmaj_hab2_var.RData')

counts <- matrix(NA,2,19)
counts[1,2:19] <- index_parmaj_hab1_mean
counts[2,2:19] <- index_parmaj_hab2_mean

sd.counts <- matrix(NA,2,19) 
sd.counts[1,2:19] <- sqrt(index_parmaj_hab1_var)
sd.counts[2,2:19] <- sqrt(index_parmaj_hab2_var)

rm(index_parmaj_hab1_mean,index_parmaj_hab1_var,index_parmaj_hab2_mean,index_parmaj_hab2_var,hvie_ID_PROG_parmaj,hvie_parmaj)


# Compute the date of first capture for each individual:
f <- NULL
for (i in 1:N){
  temp <- 1:K
  f <- c(f,min(temp[mydata[i,]==1]))}

# On remplace les 0 dans la covariables age : un individu ne peut etre juvenile que la premiere annee
# Donc apres la premiere occasion de capture f, on a forcement que des 2. 

for (i in 1:N){
  for(j in 1:K){
    if (j > f[i]) {cov_age[i,j] <- 2}
    else{next}
  }
}

################################
# MODEL
################################

code <- nimbleCode({
  
  
  ###############
  # PART COUNTS #
  ############### 
  
  # Initial population sizes
  # -------------------------------
  
  for(h in 1:2){
    
    #Juveniles
    nN1[h] ~ dnorm(400, sd = 50)
    N1[h,1] <- round(nN1[h])
    
    # Adults
    nNad[h] ~ dnorm(180, sd = 30)
    Nad[h,1] <- round(nNad[h])
  }
  
  # Priors
  # -------------------------------
  
  # survival
  for (h in 1:2) {
    for(u in 1:2) {
      for (t in 1:(K-1)) {
        n_eta.phi[h,u,t] <- 1/(1+exp(-eta.phi[h,u,t])) 
      }
    }
  }
  
  
  # productivity
  for (h in 1:2){ 
    for(t in 1:K){
      # mettre un prior informatif ?
      # ecrire autrement ? 
      fec[h,t] ~ dunif(0,20)
    }
  }
  
  # System process
  # ------------------------------- 
  
  for (h in 1:2) {
    for (t in 2:K) {
      Nad_juv[h,t] ~ dbin(n_eta.phi[h,1,t-1],round(N1[h,t-1]))
      Nad_ad[h,t] ~ dbin(n_eta.phi[h,2,t-1],round(Nad[h,t-1]))
      
      Nad[h,t] <- Nad_juv[h,t] + Nad_ad[h,t]
      
      # Jeunes
      meanN1[h,t] <- round(Nad[h,t-1]*(fec[h,t-1]/2))
      N1[h,t] ~ dpois(meanN1[h,t])
    }
  }
  
  # Observation process
  # ------------------------------- 
  for(h in 1:2){
    for (t in 2:K){
      logNad[h,t] <- log(Nad[h,t])
      counts[h,t] ~ dnorm(logNad[h,t], sd = sd.counts[h, t])
    }
  }
  
  
  ########################## 
  # PART CAPTURE RECAPTURE #
  ########################## 
  
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
    eps.p[t] ~ dnorm(0, sd = sigma.p)
    
    for(h in 1:2){
      for (u in 1:2){
        # Survie
        eta.phi[h,u,t] <- mu.phi + gamma.h.phi[h] + gamma.u.phi[u] + eps.phi[h,u,t]
        #Random effect time
        eps.phi[h,u,t] ~ dnorm(0, sd = sigma.phi[h,u])
      }#u
    }#h
  }#t
  
  # Survie
  # mean 
  mean.phi <- exp(mu.phi) /(1 + exp(mu.phi))
  mu.phi ~ dnorm(0,1)
  
  # sd pour effet aleatoire temps
  for(u in 1:2) {
    for (h in 1:2) { 
      sigma.phi[h,u] ~ dunif(0.1,5)
    }#h
  }#u
  
  # Proba recapture
  # mean
  mean.p <- exp(mu.p) /(1 + exp(mu.p)) 
  mu.p ~ dnorm(0,1)
  
  # sd pour effet aleatoire temps
  sigma.p ~ dunif(0.1,5)
  
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
  
})

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
             sd.counts = sd.counts,
             f=f,
             hvie_site=hvie_site,
             ind_site = ind_site,
             cov_age=cov_age,
             cov_ind=cov_ind,
             cov_hab=cov_hab)

data = list( z = known.state.cjs(mydata),
             mydata = mydata,
             counts = counts)

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
               gamma.i.p=rnorm(1,0,1),
               Nad = matrix(rep(c(115,180), 19, each=1),2,19))


init2  <- list(z=cjs.init.z(mydata,f),
               mu.phi=rnorm(1,0,1),
               mu.p=rnorm(1,0,1),
               sigma.phi=matrix(runif(4,0.1,5),2,2),
               sigma.p=runif(1,0.1,5),
               gamma.h.phi=rnorm(2,0,1),
               gamma.u.phi=rnorm(2,0,1),
               gamma.i.p=rnorm(1,0,1),
               Nad = matrix(rep(c(115,180), 19, each=1),2,19))


inits <- list(init1,init2)

# Specify the parameters to be monitored
parameters <- c("n_eta.phi","eta.phi","eta.p","sigma.phi","eps.phi",
                "mean.phi","mu.phi",
                "mean.p"  ,"mu.p"  ,"sigma.p" ,"eps.p",
                "gamma.u.phi","gamma.h.phi","gamma.i.p",
                "fec","Nad","N1", "nN1", "nNad","Nad_ad","Nad_juv")


#################
# fit the model #
#################
m <- nimbleModel(code, const, data, init1, check = FALSE, calculate = FALSE)
m$calculate()

# Inits simulation
simNodes <- c('eps.phi','eps.p',"Nad","N1","nN1","nNad","fec","Nad_ad","Nad_juv")

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
n.burnin = 50000
n.keep.exact = 20000
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


save(out,file='out_IPM_parmaj.RData')


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
# setwd("~/These/MNHN/Mesanges_cluster/IPM_parmaj")
# 
# # Data
# load('hvie_parmaj_tot.RData')
# 
# # Dimension
# # Nb of individuals
# N <- dim(hvie_parmaj)[1]
# # Nb of capture events
# K <- 19
# 
# # output
# load('out_IPM_parmaj_long.RData')
# out_mat <- as.matrix(out)
# 
# #########################
# # Test de gelman
# #########################
# 
# # var dependante du temps
# var <- c("n_eta.phi[1, 1, ", "n_eta.phi[1, 2, ", "n_eta.phi[2, 1, ", "n_eta.phi[2, 2, ",
#          "eta.phi[1, 1, ","eta.phi[1, 2, ","eta.phi[2, 1, ","eta.phi[2, 2, ",
#          "eps.phi[1, 1, ","eps.phi[1, 2, ","eps.phi[2, 1, ","eps.phi[2, 2, ",
#          "eta.p[","eps.p[", "fec[1, ",
#          "fec[2, ", "Nad[1, ", "Nad[2, ", "N1[1, ", "N1[2, ")
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
# # var independante du temps
# var <- c("mean.phi","mu.phi",
#          "nN1[1]","nNad[1]","nN1[2]","nNad[2]",
#          "sigma.phi[1, 1]","sigma.phi[1, 2]","sigma.phi[2, 1]","sigma.phi[2, 2]",
#          "mean.p"  ,"mu.p"  ,"sigma.p",
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
# # Figures parametres
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
# plot_parameters("eta.phi[1, 1, ",color[1],"annees","survie","survie juv")
# plot_parameters("eta.phi[1, 2, ",color[2],"annees","survie","survie ad")
# plot_parameters("eta.phi[2, 1, ",color[1],"annees","survie","survie juv")
# plot_parameters("eta.phi[2, 2, ",color[2],"annees","survie","survie ad")
# plot_parameters("eta.p[",color[3],"annees","detection","detection")
# 
# 
# 
# plot_states <- function(var,color,y_lim,x_lab,y_lab,title) {
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
#     l_025[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.025))
#     l_25[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.25))
#     l_50[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.5))
#     l_75[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.75))
#     l_975[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.95))
#   }
#   
#   
#   color.transparent <- adjustcolor(color, alpha.f = 0.4)
#   color.transparent_2 <- adjustcolor(color, alpha.f = 0.2)
#   
#   
#   plot(l_50,type='l',axes=F,lwd =2,col="ivory4",ylim=y_lim,ylab= y_lab,xlab=x_lab,main=title)
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
# par(mfrow=c(2,2))
# color<-c("#FF8830","#A6B06D","#589482","#8C2423")
# plot_states("Nad[1, ",color[2], c(0,200),"annees","number","number of adult hab1")
# plot_states("Nad[2, ",color[2],c(0,200), "annees","number","number of adult hab2")
# plot_states("N1[1, ",color[2],c(0,400), "annees","number","number of juv hab1")
# plot_states("N1[2, ",color[2],c(0,400), "annees","number","number of juv hab2")
# 
# plot_states("fec[1, ",color[2],c(0,30),"annees","fecundity","fecundity hab 1")
# plot_states("fec[2, ",color[2],c(0,30),"annees","fecundity","fecundity hab 2")
# 
# #####################
# # Check avec les donnees
# 
# check_state <- function(var,Nb,col,x_lab,y_lab,title) {
#   
#   K=19
#   
#   l_025 <- NULL
#   l_25  <- NULL
#   l_50  <- NULL
#   l_75  <- NULL
#   l_975 <- NULL
#   
#   for (i in 1:K){
#     l_025[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.025))
#     l_25[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.25))
#     l_50[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.50))
#     l_75[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.75))
#     l_975[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.975))
#   }
#   
#   color.transparent <- adjustcolor(col, alpha.f = 0.4)
#   color.transparent_2 <- adjustcolor(col, alpha.f = 0.2)
#   
#   
#   plot(l_50,type='l',axes=F,lwd =2,col="ivory4",ylab= y_lab,xlab=x_lab,main=title,ylim=c(0,200))
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
#   lines(2:K,Nb,type="l",lwd=3,lty=3, col=1)
#   axis(1, at=c(1:K),labels=c(seq(2001,2019,1)))
#   axis(side =2, cex.axis=1, las=2)
# }
# 
# par(mfrow=c(1,1))
# load('index_parmaj_hab1_mean.RData')
# load('index_parmaj_hab2_mean.RData')
# 
# check_state("Nad[1, ", exp(index_parmaj_hab1_mean), col = color[2], x_lab="", y_lab="",title="")
# check_state("Nad[2, ", exp(index_parmaj_hab2_mean), col = color[2], x_lab="", y_lab="",title="")
# 
# boxplot_maud <- function(var,x,color,x_lim,y_lim,x_lab,y_lab,ad) {
#   
#   l_025   <- as.numeric(quantile(var,probs= 0.025))
#   l_10    <- as.numeric(quantile(var,probs= 0.10))
#   l_50    <- as.numeric(quantile(var,probs= 0.50))
#   l_90    <- as.numeric(quantile(var,probs= 0.90))
#   l_975   <- as.numeric(quantile(var,probs= 0.975))
#   
#   if(ad==F){
#     plot(c(x,x+0.4),c(l_50,l_50),type='l',axes=F,lwd =2,col="black", xlim=x_lim, ylim=y_lim, ylab=y_lab, xlab=x_lab)
#     abline(h=0,lty=2,col="ivory3")
#     arrows((x+0.2),l_025,(x+0.2),l_975,length=0.06, angle=90, code=3,col="black")
#     xx<- c(x,(x+0.4),(x+0.4),x)
#     yy <- c(l_10,l_10,l_90,l_90)
#     polygon(xx,yy,col=color, border=NA)
#     lines(c(x,x+0.4),c(l_50,l_50),lwd =2, col="black")
#   }
#   
#   if(ad==T){
#     par(new=TRUE)
#     arrows((x+0.2),l_025,(x+0.2),l_975,length=0.06, angle=90, code=3,col="black")
#     xx<- c(x,(x+0.4),(x+0.4),x)
#     yy <- c(l_10,l_10,l_90,l_90)
#     polygon(xx,yy,col=color, border=NA)
#     lines(c(x,x+0.4),c(l_50,l_50),lwd =2, col="black")
#   }
#   
# }
# 
# 
# 
# boxplot_maud(out_mat[,"gamma.u.phi[1]"],1, color[3],x_lim=c(0,6),y_lim=c(-3,3),"Parametre demo","Effet",F)
# boxplot_maud(out_mat[,"gamma.u.phi[2]"],2, color[3],x_lim=c(0,6),y_lim=c(-3,3),"Parametre demo","Effet",T)
# boxplot_maud(out_mat[,"gamma.h.phi[1]"],3, color[3],x_lim=c(0,6),y_lim=c(-3,3),"Parametre demo","Effet",T)
# boxplot_maud(out_mat[,"gamma.h.phi[2]"],4, color[3],x_lim=c(0,6),y_lim=c(-3,3),"Parametre demo","Effet",T)
# boxplot_maud(out_mat[,"gamma.i.p"],     5, color[3],x_lim=c(0,6),y_lim=c(-3,3),"Parametre demo","Effet",T)
# axis(1,at=seq(1.2,5.2,1),labels=c("juv","ad","hab1","hab2","cov_ind"))
# axis(side =2, cex.axis=1, las=2)
# 
