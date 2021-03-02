##########################
#     IPM PASSEREAUX     #
# Fauvette des jardins   #
##########################

rm(list=ls())
#Package
library(nimble)

# ----------------------------------------------------------------

#################
#     Data      #
#################
setwd("~/These/MNHN/Mesanges/output")
#setwd("//hpcm.cluster.calcul.hpc/Shared-2/QUEROUE/IPM_sylatr_tt")
#setwd("~/These/MNHN/Mesanges_cluster/IPM_sylatr_tt")


#Histoire de vie individus
load('hvie_sylatr_tot_tt.RData')

# Histoire de vie des sites
load('hvie_ID_PROG_sylatr_tot_tt.RData')

# Nb ind
N <- dim(hvie_sylatr)[1]
# Nb site
S <- dim(hvie_ID_PROG_sylatr)[1]
# Nb years
K <- 19

#############################
# DIMINUTION JEU DE DONNEES #
#############################

# # Combien de sites garder ?
nsites <- 50

#On choisi aleatoirement n sites
ID <- sort(sample(seq(1,S,1),nsites))

#On garde  que les sites selectionnes
hvie_ID_PROG_sylatr <- hvie_ID_PROG_sylatr[ID,]
S <- dim(hvie_ID_PROG_sylatr)[1]

#On garde que les individus des sites selectionnes
hvie_to_keep <- which((hvie_sylatr$vector_ID_PROG %in% ID)==TRUE)
hvie_sylatr <- hvie_sylatr[hvie_to_keep,]
N <- dim(hvie_sylatr)[1]

for(i in 1:N){
  hvie_sylatr$vector_ID_PROG[i] <- which(hvie_ID_PROG_sylatr$ID_PROG==hvie_sylatr$ID_PROG[i])
}

rm(hvie_to_keep,ID,nsites)

####################
####################

# Les histoires de vie sont 1=juvenile, 2=adulte, 0= pas vu  

# On cree la matrice des covariables pour l'age 
cov_age <- as.matrix(hvie_sylatr[,1:K])
cov_age <- apply(cov_age,2,as.numeric)

# On remplace par un tous les 1 et 2 pour former les donnees en 0 et 1
mydata <- as.matrix(hvie_sylatr[,1:K])
mydata <- apply(mydata,2,as.numeric)
mydata[which(mydata>0)] <- 1

# Vecteur donnant le site dans lequel a ete capture chaque mesange 
ind_site <- hvie_sylatr$vector_ID_PROG

#vecteur covariable individuelle recpature 
cov_ind <- hvie_sylatr$cov_ind

# On remplace les histoires de vies des sites (nombre de captures secondaires par an) par des 1
# quand le site est actif, sinon ce sont des 0.

hvie_site <- as.matrix(hvie_ID_PROG_sylatr[1:K])
hvie_site[which(hvie_site>0)] <- 1

# Log habitat
load('index_sylatr_mean.RData')
load('index_sylatr_var.RData')

counts <- NULL
counts[2:19] <- index_sylatr_mean

sd.counts <- NULL
sd.counts[2:19] <- sqrt(index_sylatr_var)

# Compute the date of first capture for each individual:
f <- NULL
for (i in 1:N){
  temp <- 1:K
  f <- c(f,min(temp[mydata[i,]==1]))}

# Compute the last date of capture for each individual with censure:
l <- NULL
for (i in 1:N){
  temp <- 1:K
  if (hvie_sylatr$censure[i]==1){l_new <- K}
  else {l_new <- max(temp[mydata[i,]==1])}
  l <- c(l,l_new)
  rm(l_new)}


# On remplace les 0 dans la covariables ?ge : un individu ne peut ?tre juv?nile que la premi?re ann?e
# Donc apr?s la premi?re occasion de capture f, on a forcement que des 2. 

for (i in 1:N){
  for(j in 1:K){
    if (j > f[i] & j<=l[i]) {cov_age[i,j] <- 2}
    else{next}
  }
}

cov_age[which(cov_age==0)] <- NA

rm(index_sylatr_mean,index_sylatr_var,i,hvie_ID_PROG_sylatr,hvie_sylatr,S,j,temp)

################################
# MODEL
################################

code <- nimbleCode({
  
  
  ###############
  # PART COUNTS #
  ############### 
  
  # Initial population sizes
  # -------------------------------
  
  #Juveniles
  nN1 ~ dnorm(1500, sd = 400)
  
  # Adults
  nNad ~ dnorm(800, sd = 200)
  
  #Juveniles
  N1[1] <- round(nN1)
  # Adults
  Nad[1] <- round(nNad)
  
  
  # Priors
  # -------------------------------
  
  # survival
  
  for(u in 1:2) {
    for (t in 1:(K-1)) {
      n_eta.phi[u,t] <- 1/(1+exp(-eta.phi[u,t])) 
    }
  }
  
  
  
  # productivity
  
  mu.fec ~ dnorm(log(8), sd = 0.1)
  
  for(t in 2:K){
    # mettre un prior informatif ?
    # ecrire autrement ? 
    
    eps.fec[t] ~ dnorm(0, sd = 0.3)
    log(fec[t]) <- mu.fec + eps.fec[t]
  }
  
  
  # System process
  # ------------------------------- 
  
  
  for (t in 2:K) {
    Nad_juv[t] ~ dbin(n_eta.phi[1,t-1],round(N1[t-1]))
    Nad_ad[t] ~ dbin(n_eta.phi[2,t-1],round(Nad[t-1]))
    
    # Nombre d'adultes qui sont deja sur le site
    Nres[t] <- Nad_juv[t] + Nad_ad[t] 
    
    Nim[t] <- round((Nad_juv[t] + Nad_ad[t])*0.3)
    
    # Ajout du nombre d'immigrant
    Nad[t] <- Nres[t] + Nim[t]
    
    # Jeunes
    meanN1[t] <- round(Nad[t]*fec[t]*0.5)
    N1[t] ~ dpois(meanN1[t])
    
  }
  
  # Observation process
  # ------------------------------- 
  for (t in 2:K){
    logNad[t] <- log(Nad[t])
    counts[t] ~ dnorm(logNad[t], sd = (sd.counts[t] * kappa))
  }
  
  
  kappa ~ dlnorm(log(3),0.3)
  
  ########################## 
  # PART CAPTURE RECAPTURE #
  ########################## 
  
  # Calcul survie/recapture selon individu
  
  for (i in 1:N){
    for (t in f[i]:(l[i]-1)){ 
      # Survie
      logit(phi[i,t]) <- eta.phi[cov_age[i,t],t]
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
    
    
    for (u in 1:2){
      # Survie
      eta.phi[u,t] <- mu.phi + gamma.u.phi[u] + eps.phi[u,t]
      #Random effect time
      eps.phi[u,t] ~ dnorm(0, sd = sigma.phi[u])
    }#u
  }#t
  
  # Survie
  # mean 
  mean.phi <- exp(mu.phi) /(1 + exp(mu.phi))
  mu.phi ~ dnorm(0,1)
  
  # sd pour effet aleatoire temps
  for(u in 1:2) {
    sigma.phi[u] ~ dunif(0.1,5)
  }#u
  
  # Proba recapture
  # mean
  mean.p <- exp(mu.p) /(1 + exp(mu.p)) 
  mu.p ~ dnorm(0,1)
  
  # sd pour effet aleatoire temps
  sigma.p ~ dunif(0.1,5)
  
  # Effet age et habitat
  gamma.u.phi[1] <- 0
  gamma.u.phi[2] ~  dnorm(0,1)
  
  
  # Effet cov ind
  gamma.i.p   ~  dnorm(0,1)
  
  ##### Likelihood ##### 
  for (i in 1:N) {
    # Define latent state at first capture 
    z[i,f[i]] <- 1
    for (t in (f[i]+1):l[i]){ 
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
             l=l,
             hvie_site=hvie_site,
             ind_site = ind_site,
             cov_age=cov_age,
             cov_ind=cov_ind)

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
               sigma.phi=c(1,1),
               sigma.p=runif(1,0.1,5),
               gamma.u.phi=c(NA,0),
               gamma.i.p=rnorm(1,0,1),
               Nad_juv = rep(150,19),
               Nad_ad = rep(200,19),
               mu.fec = log(8),
               kappa = 2,
               eps.fec = rep(0,19))


init2  <- list(z=cjs.init.z(mydata,f),
               mu.phi=rnorm(1,0,1),
               mu.p=rnorm(1,0,1),
               sigma.phi=c(1,1),
               sigma.p=runif(1,0.1,5),
               gamma.u.phi=c(NA,0),
               gamma.i.p=rnorm(1,0,1),
               Nad_juv = rep(150,19),
               Nad_ad = rep(200,19),
               mu.fec = log(8),
               kappa = 2,
               eps.fec = rep(0,19))


inits <- list(init1,init2)

# Specify the parameters to be monitored
parameters <- c("n_eta.phi","eta.phi","eta.p","sigma.phi","eps.phi", "kappa",
                "mean.phi","mu.phi",
                "mean.p"  ,"mu.p"  ,"sigma.p" ,"eps.p",
                "gamma.u.phi","gamma.i.p","mu.fec","eps.fec",
                "fec","N1", "nN1", "nNad","Nad_ad","Nad_juv","Nad","Nim",'Nres')


#################
# fit the model #
#################
m <- nimbleModel(code, const, data, init1, check = FALSE, calculate = FALSE)
m$calculate()

# Inits simulation
simNodes <- c('eps.phi','eps.p',"nN1","nNad","N1")

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
n.burnin = 1000
n.keep.exact = 2000
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

save(out,file='out_IPM_sylatr.RData')

# ######################################################
# # Analyse output
# ######################################################

# rm(list=ls())
# 
# # package
library(R2jags)
library(coda)
library(nimble)
library(basicMCMCplots)
library(boot)
# 
# # Repertoire
# setwd("~/These/MNHN/Mesanges_cluster/IPM_sylatr_tt")
# 
# # Data
# load("hvie_sylatr_tot_tt.RData")
# 
# # Dimension
# # Nb of individuals
# N <- dim(hvie_sylatr)[1]
# # Nb of capture events
# K <- 19
# 
# # output
# load("out_IPM_sylatr.RData")
out_mat <- as.matrix(out)
# 
#########################
# Test de gelman
#########################

# var dependante du temps
var <- c("n_eta.phi[1, ", "n_eta.phi[2, ", "eps.fec[", 
         "eta.phi[1, ","eta.phi[1, ","eta.phi[2, ","eta.phi[2, ",
         "eps.phi[1, ","eps.phi[1, ","eps.phi[2, ","eps.phi[2, ",
         "eta.p[","eps.p[", "fec[", "Nim[", "Nres[", "Nad[")


for(v in 1:length(var)) {
  gelman <- NULL
  for (t in 2:(K-1)) {
    gelman[t] <-gelman.diag(out[,c(paste(var[v],t,"]", sep=""))], confidence = 0.95, transform=TRUE, autoburnin=TRUE)$psrf[1]
    
  }
  plot(gelman,pch=19,col="indianred4",main=paste(var[v]))
  abline(h=1.1,lty=2)
  abline(h=1.5,lty=2,col="red")
}

# var independante du temps
var <- c("mean.phi","mu.phi","mu.fec","kappa",
         "nN1","nNad",
         "sigma.phi[1]","sigma.phi[2]",
         "mean.p"  ,"mu.p"  ,"sigma.p",
         "gamma.u.phi[1]","gamma.u.phi[2]","gamma.i.p")

gelman <- NULL
for(v in 1:length(var)) {
  gelman[v] <-gelman.diag(out[,c(paste(var[v], sep=""))], confidence = 0.95, transform=TRUE, autoburnin=TRUE)$psrf[1]
  chainsPlot(out, paste(var[v], sep=""))
  legend("topleft",legend=paste("gelman : ",round(gelman[v],4),sep=""))
  print(paste(var[v],"  :   ",gelman[v],sep=""))
}


#########################
# Figures parametres
#########################

plot_parameters <- function(var,color,x_lab,y_lab,title) {
  
  K = K-1
  
  l_025 <- NULL
  l_25  <- NULL
  l_50  <- NULL
  l_75  <- NULL
  l_975 <- NULL
  
  for (i in 1:K){
    l_025[i]   <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.025)))
    l_25[i]    <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.25)))
    l_50[i]    <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.5)))
    l_75[i]    <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.75)))
    l_975[i]   <- inv.logit(as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.975)))
  }
  
  
  color.transparent <- adjustcolor(color, alpha.f = 0.4)
  color.transparent_2 <- adjustcolor(color, alpha.f = 0.2)
  
  
  plot(l_50,type='l',axes=F,lwd =2,col="ivory4",ylim=c(0,1),ylab= y_lab,xlab=x_lab,main=title)
  
  xx<- c(1:K,K:1)
  yy <- c(l_025[1:K],l_975[K:1])
  polygon(xx,yy,col=color.transparent_2, border=NA)
  
  xx<- c(1:K,K:1)
  yy <- c(l_25[1:K],l_75[K:1])
  polygon(xx,yy,col=color.transparent, border=NA)
  
  lines(l_50,lwd =2, col=color)
  
  axis(1, at=c(1:K),labels=c(seq(2001,2018,1)))
  axis(side =2, cex.axis=1, las=2)
}


par(mfrow=c(2,2))
color<-c("#FF8830","#A6B06D","#589482","#8C2423")
plot_parameters("eta.phi[1, ",color[1],"annees","survie","survie juv")
plot_parameters("eta.phi[2, ",color[4],"annees","survie","survie ad")

par(mfrow=c(1,1))
plot_parameters("eta.p[","ivory4","annees","detection","detection")




plot_states <- function(var,deb,color,y_lim,x_lab,y_lab,title) {
  
  K = 19
  
  l_025 <- NULL
  l_25  <- NULL
  l_50  <- NULL
  l_75  <- NULL
  l_975 <- NULL
  
  if(deb==2){
    for (i in 2:K){
      l_025[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.025))
      l_25[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.25))
      l_50[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.5))
      l_75[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.75))
      l_975[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.975))
    }
  }
  
  if(deb==1){
    for (i in 1:K){
      l_025[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.025))
      l_25[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.25))
      l_50[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.5))
      l_75[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.75))
      l_975[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],0.975))
    }
  }
  
  color.transparent <- adjustcolor(color, alpha.f = 0.4)
  color.transparent_2 <- adjustcolor(color, alpha.f = 0.2)
  
  
  plot(l_50,type='l',axes=F,lwd =2,col="ivory4",ylim=y_lim,ylab= y_lab,xlab=x_lab,main=title)
  
  xx<- c(1:K,K:1)
  yy <- c(l_025[1:K],l_975[K:1])
  polygon(xx,yy,col=color.transparent_2, border=NA)
  
  xx<- c(1:K,K:1)
  yy <- c(l_25[1:K],l_75[K:1])
  polygon(xx,yy,col=color.transparent, border=NA)
  
  lines(l_50,lwd =2, col=color)
  
  axis(1, at=c(1:K),labels=c(seq(2001,2019,1)))
  axis(side =2, cex.axis=1, las=2)
}

par(mfrow=c(1,1))
plot_states("fec[", 2, color[1],c(0,20),"annees","fecundity","fecundity")


#####################
# Check avec les donnees

check_state <- function(var,Nb,col,x_lab,y_lab,title) {
  
  K=19
  
  l_025 <- NULL
  l_25  <- NULL
  l_50  <- NULL
  l_75  <- NULL
  l_975 <- NULL
  
  for (i in 1:K){
    l_025[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.025))
    l_25[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.25))
    l_50[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.50))
    l_75[i]    <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.75))
    l_975[i]   <- as.numeric(quantile(out_mat[,paste(var,i,"]",sep="")],probs= 0.975))
  }
  
  color.transparent <- adjustcolor(col, alpha.f = 0.4)
  color.transparent_2 <- adjustcolor(col, alpha.f = 0.2)
  
  
  plot(l_50,type='l',axes=F,lwd =2,col="ivory4",ylab= y_lab,xlab=x_lab,main=title,ylim=c(0,1000))
  
  xx<- c(1:K,K:1)
  yy <- c(l_025[1:K],l_975[K:1])
  polygon(xx,yy,col=color.transparent_2, border=NA)
  
  xx<- c(1:K,K:1)
  yy <- c(l_25[1:K],l_75[K:1])
  polygon(xx,yy,col=color.transparent, border=NA)
  
  lines(l_50,lwd =2, col=col)
  lines(2:K,Nb,type="l",lwd=3,lty=3, col=1)
  axis(1, at=c(1:K),labels=c(seq(2001,2019,1)))
  axis(side =2, cex.axis=1, las=2)
}

par(mfrow=c(1,1))
load('index_sylatr_mean.RData')


color<-c("#FF8830","#A6B06D","#589482","#8C2423")
par(mfrow=c(2,2))
plot_states("N1[", 1, color[1],c(0,2500), "annees","nombre","Nb juv")
check_state("Nad[", exp(index_sylatr_mean), col = color[2], x_lab="annees", y_lab="nombre",title="Nb Ad")

boxplot_maud <- function(var,x,color,x_lim,y_lim,x_lab,y_lab,ad) {
  
  l_025   <- as.numeric(quantile(var,probs= 0.025))
  l_10    <- as.numeric(quantile(var,probs= 0.10))
  l_50    <- as.numeric(quantile(var,probs= 0.50))
  l_90    <- as.numeric(quantile(var,probs= 0.90))
  l_975   <- as.numeric(quantile(var,probs= 0.975))
  
  if(ad==F){
    plot(c(x,x+0.4),c(l_50,l_50),type='l',axes=F,lwd =2,col="black", xlim=x_lim, ylim=y_lim, ylab=y_lab, xlab=x_lab)
    abline(h=0,lty=2,col="ivory3")
    arrows((x+0.2),l_025,(x+0.2),l_975,length=0.06, angle=90, code=3,col="black")
    xx<- c(x,(x+0.4),(x+0.4),x)
    yy <- c(l_10,l_10,l_90,l_90)
    polygon(xx,yy,col=color, border=NA)
    lines(c(x,x+0.4),c(l_50,l_50),lwd =2, col="black")
  }
  
  if(ad==T){
    par(new=TRUE)
    arrows((x+0.2),l_025,(x+0.2),l_975,length=0.06, angle=90, code=3,col="black")
    xx<- c(x,(x+0.4),(x+0.4),x)
    yy <- c(l_10,l_10,l_90,l_90)
    polygon(xx,yy,col=color, border=NA)
    lines(c(x,x+0.4),c(l_50,l_50),lwd =2, col="black")
  }
  
}


par(mfrow=c(1,1))
boxplot_maud(out_mat[,"gamma.u.phi[1]"],1, color[3],x_lim=c(0,6),y_lim=c(-2,5),"Parametre demo","Effet",F)
boxplot_maud(out_mat[,"gamma.u.phi[2]"],2, color[3],x_lim=c(0,6),y_lim=c(-2,5),"Parametre demo","Effet",T)
boxplot_maud(out_mat[,"gamma.i.p"],     5, color[3],x_lim=c(0,6),y_lim=c(-2,5),"Parametre demo","Effet",T)
axis(1,at=c(1.2,2.2,5.2),labels=c("juv","ad","cov_ind"))
axis(side =2, cex.axis=1, las=2)



plot_states("Nad_juv[", 2, color[1],c(0,800), "annees","nombre","Nb juv")
plot_states("Nad_ad[", 2, color[1],c(0,800), "annees","nombre","Nb ad")
