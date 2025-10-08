
# Instructions

#1. Put this script, aux_data.RData and ffnn.bin files in the same folder 

#2. downoad and install Rtools from: https://cran.r-project.org/bin/windows/Rtools/

#3. downoad and install JAGS from:https://sourceforge.net/projects/mcmc-jags/

#4. downoad and install necessary packages, by running the following lines, after removing the hashtag (#)
# list.of.packages <-c("shiny","shinydashboard","shinyWidgets","DT","shinyBS","mvtnorm","conicfit","htm2txt",
#                      "stringi","imputeTS","neuralnet","coda","tibble","gplots","ggpubr","ggplot2","R2jags",
#                      "shinyFiles","fs","data.table","randtests","shinycssloaders","datamods")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)



#5. Press the "Run App" button from the Rstudio menu to start the app


#################################### Packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyBS) 
library(mvtnorm)
library(conicfit)
library(htm2txt)
library(stringi)
library(imputeTS)
library(neuralnet)
library(coda)
library(tibble)
library(gplots)
library(ggpubr)
library(ggplot2)
library(R2jags)
library(shinyFiles)
library(fs)
library(data.table)
library(randtests) # REPLACEMENT of snpar
library(shinycssloaders)
library(tidyr)
library(dplyr)
library(datamods)
#library(plotly)

 setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
 path=getwd()
 #source("ABC_aux_functions.R")
 '%!in%' <- function(x,y)!('%in%'(x,y))
linebreaks <- function(n){HTML(strrep(br(), n))}
load("aux_data.RData")
options(scipen = 100)

nn_file<- "ffnn.bin" # file containing neural networks trained to estimate B/k priors
#only_catch=test_CATCH_data%>%group_by(Stock)%>%dplyr::summarise(catch=sum(ct,na.rm = T),bt=sum(bt,na.rm=T))
only_catch=c("Aust_pec_Namaqua","Belo_bel_TunisianPGSidra","Boop_boo_AzoresCanariesMadeira", "Boop_boo_GulfGuineaIs",        
"Bore_sai_Kara","Brac_aur_GulfGuineaW","Brev_pec_RioGrande","Bros_bro_NGrandBanks")
test_CATCH_data=test_CATCH_data[test_CATCH_data$Stock %!in% only_catch,]
test_CATCH_ID_All=test_CATCH_ID_All[test_CATCH_ID_All$Stock %!in% only_catch,]

test_CATCH_ID= test_CATCH_ID_All[,c("Continent","Region","Subregion","Group","ScientificName" ,"Stock")]

template_CATCH_data_clnms =c("Stock","yr","ct","bt","ct_smthd","bt_iterp","bt_smthd","ecreep","bt_cv","Stock_objID")
template_CATCH_data = data.frame(matrix(nrow = 0, ncol = length(template_CATCH_data_clnms)))
colnames(template_CATCH_data) = template_CATCH_data_clnms

template_CATCH_ID_clnms = c("Continent","Region","Subregion","Stock","Group","Name","ScientificName","Source",
                            "StartYear","EndYear","Resilience","r.low","r.hi","stb.low","stb.hi","int.yr",
                            "intb.low","intb.hi","endb.low","endb.hi","btype","e.creep","Comments","MSY.prior",
                            "q.low","q.hi","Cr_sel","Stock_objID")
template_CATCH_ID = data.frame(matrix(nrow = 0, ncol = length(template_CATCH_ID_clnms)))
colnames(template_CATCH_ID) = template_CATCH_ID_clnms

test_CATCH_ID=tibble::as_tibble(test_CATCH_ID)


################################## FUNCTIONS
################################## FUNCTIONS
################################## FUNCTIONS
################################## FUNCTIONS
################################## FUNCTIONS
################################## FUNCTIONS

runs.sig3 <- function(x,type=NULL,mixing="less") {
  if(is.null(type)) type="resid"
  if(type=="resid"){
    mu = 0}else{mu = mean(x, na.rm = TRUE)}
  alternative=c("two.sided","left.sided","right.sided")[which(c("two.sided", "less","greater")%in%mixing)]
  # Average moving range
  mr  <- abs(diff(x - mu))
  amr <- mean(mr, na.rm = TRUE)
  # Upper limit for moving ranges
  ulmr <- 3.267 * amr
  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  # Calculate standard deviation, Montgomery, 6.33
  stdev <- amr / 1.128
  # Calculate control limits
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  if(nlevels(factor(sign(x)))>1){
    # Make the runs test non-parametric
    runstest = randtests::runs.test(x,threshold = 0,alternative = alternative)
    if(is.na(runstest$p.value)) p.value =0.001
    pvalue = round(runstest$p.value,3)} else {
      pvalue = 0.001
    }
  return(list(sig3lim=c(lcl,ucl),p.runs= pvalue))
}

mvn   <- function(n,mean.log.r,sd.log.r,mean.log.k,sd.log.k) {
  cor.log.rk   <- -0.76 # empirical value of log r-k correlation in 250 stocks analyzed with BSM (without r-k correlation), used only in graph
  cov.log.rk <- cor.log.rk*sd.log.r*sd.log.k # covariance with empirical correlation and prior variances  covar.log.rk = matrix(NA, ncol=2,nrow=2)   # contract covariance matrix
  covar.log.rk      <- base::matrix(NA, ncol=2,nrow=2) # covariance matrix
  covar.log.rk[1,1] <- sd.log.r^2                # position [1,1] is variance of log.r
  covar.log.rk[2,2] <- sd.log.k^2               # position [2,2] is variance of log.k
  covar.log.rk[1,2] = covar.log.rk[2,1] = cov.log.rk     # positions [1,2] and [2,1] are correlations
  mu.log.rk  <- (c(mean.log.r,mean.log.k))      # vector of log.means
  mvn.log.rk <- mvtnorm::rmvnorm(n,mean=mu.log.rk,sigma=covar.log.rk,method="svd")
  return(mvn.log.rk)
}

get_beta <- function(mu,CV,Min=0,Prior="x"){
  a = seq(0.0001,1000,0.001)
  b= (a-mu*a)/mu
  s2 = a*b/((a+b)^2*(a+b+1))
  sdev = sqrt(s2)
  # find beta parameter a
  CV.check = (sdev/mu-CV)^2
  a = a[CV.check==min(CV.check)]
  # find beta parameter b
  b = (a-mu*a)/mu
  # x = seq(Min,1,0.001)
  # pdf = dbeta(x,a,b)
  # if(Plot==TRUE){
  #   plot(x,pdf,type="l",xlim=range(x[pdf>0.01]),xlab=paste(Prior),ylab="",yaxt="n")
  #   polygon(c(x,rev(x)),c(rep(0,length(x)),rev(ifelse(pdf==Inf,100000,pdf))),col="grey")
  # }
  return(c(a,b))
}

beta.prior = function(b.prior){
  bk.beta = matrix(0,nrow = 3,ncol=3)
  for(i in 1:3){
    sd.bk = (as.numeric(b.prior[2,i])-as.numeric(b.prior[1,i]))/(4*0.98)
    mu.bk = mean(as.numeric(b.prior[1:2,i]))
    cv.bk = sd.bk/mu.bk
    bk.beta[1:2,i] = get_beta(mu.bk,cv.bk)
  }
  return(bk.beta)
}

bsm   <- function(ct,btj,nyr,prior.r,prior.k,startbio,q.priorj,
                  init.q,init.r,init.k,pen.bk,pen.F,b.yrs,b.prior,CV.C,CV.cpue,nbk,rk.cor,n.chains,cmsyjags) {
  #><> convert b.prior ranges into beta priors
  bk.beta = beta.prior(b.prior)
  
  if(cmsyjags==TRUE ){ nbks=3 } else {nbks = nbk} # Switch between CMSY + BSM
  
  # Data to be passed on to JAGS
  jags.data        <- c('ct','btj','nyr', 'prior.r', 'prior.k', 'startbio', 'q.priorj',
                        'init.q','init.r','init.k','pen.bk','pen.F','b.yrs','bk.beta','CV.C','CV.cpue','nbks','rk.cor')
  # Parameters to be returned by JAGS #><> HW add key quantaties
  jags.save.params <- c('r','k','q', 'P','ct.jags','cpuem','proc.logB','B','F','BBmsy','FFmsy','ppd.logrk')
  
  # JAGS model ----
  Model = "model{
    # to reduce chance of non-convergence, Pmean[t] values are forced >= eps
    eps<-0.01
    #><> Add Catch.CV
    for(t in 1:nyr){
      ct.jags[t] ~ dlnorm(log(ct[t]),pow(CV.C,-2))
    }

    penm[1]  <- 0 # no penalty for first biomass
    Pmean[1] <- log(alpha)
    P[1]     ~ dlnorm(Pmean[1],itau2)

    for (t in 2:nyr) {
      Pmean[t] <- ifelse(P[t-1] > 0.25,
        log(max(P[t-1] + r*P[t-1]*(1-P[t-1]) - ct.jags[t-1]/k,eps)),  # Process equation
        log(max(P[t-1] + 4*P[t-1]*r*P[t-1]*(1-P[t-1]) - ct.jags[t-1]/k,eps))) # linear decline of r at B/k < 0.25
      P[t]     ~ dlnorm(Pmean[t],itau2) # Introduce process error
      penm[t]  <- ifelse(P[t]<(eps+0.001),log(q*k*P[t])-log(q*k*(eps+0.001)),
                   # ifelse(P[t]>1,ifelse((ct[t]/max(ct))>0.2,log(q*k*P[t])-log(q*k*(0.99)),0),0)) # penalty if Pmean is outside viable biomass
                    ifelse(P[t]>1.1,log(q*k*P[t])-log(q*k*(0.99)),0))
    }

    # Get Process error deviation
    for(t in 1:nyr){
      proc.logB[t] <- log(P[t]*k)-log(exp(Pmean[t])*k)}

    # ><> b.priors with penalties
    # Biomass priors/penalties are enforced as follows
    for(i in 1:nbks){
    bk.mu[i] ~ dbeta(bk.beta[1,i],bk.beta[2,i])
    bk.beta[3,i] ~ dnorm(bk.mu[i]-P[b.yrs[i]],10000)
    }

    for (t in 1:nyr){
      Fpen[t]   <- ifelse(ct[t]>(0.9*k*P[t]),ct[t]-(0.9*k*P[t]),0) # Penalty term on F > 1, i.e. ct>B
      pen.F[t]  ~ dnorm(Fpen[t],1000)
      pen.bk[t] ~ dnorm(penm[t],10000)
      cpuem[t]  <- log(q*P[t]*k);
      #><> HW 2023
      btj[t]     ~ dlnorm(cpuem[t],pow(sigma2[t],-1));
    }

  # priors
  log.alpha               <- log((startbio[1]+startbio[2])/2) # needed for fit of first biomass
  sd.log.alpha            <- (log.alpha-log(startbio[1]))/4
  tau.log.alpha           <- pow(sd.log.alpha,-2)
  alpha                   ~  dlnorm(log.alpha,tau.log.alpha)

  # set realistic prior for q
  log.qm              <- mean(log(q.priorj))
  sd.log.q            <- (log.qm-log(q.priorj[1]))/2
  tau.log.q           <- pow(sd.log.q,-2)
  q                   ~  dlnorm(log.qm,tau.log.q)

  # define process (tau) and observation (sigma) variances as inversegamma priors
  itau2 ~ dgamma(4,0.01)
  tau2  <- 1/itau2
  tau   <- pow(tau2,0.5)

  isigma2 ~ dgamma(2,0.01)
  #><> HW 2023 
  for(t in 1:nyr){
  sigma2[t] <- 1/isigma2+pow(CV.cpue[t],2) # Add minimum realistic CPUE CV
  sigma[t]  <- pow(sigma2[t],0.5)
  }
  
  log.rm              <- mean(log(prior.r))
  sd.log.r            <- abs(log.rm - log(prior.r[1]))/2
  tau.log.r           <- pow(sd.log.r,-2)

  # bias-correct lognormal for k
  log.km              <- mean(log(prior.k))
  sd.log.k            <- abs(log.km-log(prior.k[1]))/2
  tau.log.k           <- pow(sd.log.k,-2)

  # Construct Multivariate lognormal (MVLN) prior
  mu.rk[1] <- log.rm
  mu.rk[2] <- log.km

  # Prior for correlation log(r) vs log(k)
  #><>MSY: now directly taken from mvn of ki = 4*msyi/ri
  rho <- rk.cor

  # Construct Covariance matrix
  cov.rk[1,1] <- sd.log.r * sd.log.r
  cov.rk[1,2] <- rho
  cov.rk[2,1] <- rho
  cov.rk[2,2] <- sd.log.k * sd.log.k

  # MVLN prior for r-k
  log.rk[1:2] ~ dmnorm(mu.rk[],inverse(cov.rk[,]))
  r <- exp(log.rk[1])
  k <- exp(log.rk[2])

  #><>MSY get posterior predictive distribution for rk
  ppd.logrk[1:2] ~ dmnorm(mu.rk[],inverse(cov.rk[,]))

  # ><>HW: Get B/Bmsy and F/Fmsy directly from JAGS
  Bmsy <- k/2
  Fmsy <- r/2
  for (t in 1:nyr){
  B[t] <- P[t]*k # biomass
  F[t] <- ct.jags[t]/B[t]
  BBmsy[t] <- P[t]*2 #true for Schaefer
  FFmsy[t] <- ifelse(BBmsy[t]<0.5,F[t]/(Fmsy*2*BBmsy[t]),F[t]/Fmsy)
  }
} "    # end of JAGS model
  
  # Write JAGS model to file ----
  cat(Model, file="r2jags.bug")
  
  #><>MSY: change to lognormal inits (better)
  j.inits <- function(){list("log.rk"=c(rnorm(1,mean=log(init.r),sd=0.2),rnorm(1,mean=log(init.k),sd=0.1)),
                             "q"=rlnorm(1,mean=log(init.q),sd=0.2),"itau2"=1000,"isigma2"=1000)}
  # run model ----
  jags_outputs <- jags.parallel(data=jags.data,
                                working.directory=NULL, inits=j.inits,
                                parameters.to.save=jags.save.params,
                                model.file="r2jags.bug", n.chains = n.chains,
                                n.burnin = 30000, n.thin = 10,
                                n.iter = 60000)
  return(jags_outputs)
}

traceEllipse<-function(rs,ks,prior.r,prior.k){
  log.rs<-log(rs)
  log.ks<-log(ks)
  #prepare data for ellipse fitting
  cloud.data <- as.matrix(data.frame(x = log.rs, y = log.ks))
  ellip <- conicfit::EllipseDirectFit(cloud.data)
  #estimate ellipse characteristics
  atog<-conicfit::AtoG(ellip)
  ellipG <- atog$ParG
  ell.center.x<-ellipG[1]
  ell.center.y<-ellipG[2]
  ell.axis.a<-ellipG[3]
  ell.axis.b<-ellipG[4]
  ell.tilt.angle.deg<-180/pi*ellipG[5]
  ell.slope<-tan(ellipG[5])
  xy.ell<-conicfit::calculateEllipse(ell.center.x,
                                     ell.center.y,
                                     ell.axis.a,
                                     ell.axis.b,
                                     ell.tilt.angle.deg)
  #draw ellipse
  ell.intercept.1 = ell.center.y-ell.center.x*ell.slope
  #draw ellipse main axis
  ell.demiaxis.c.sqr<-(0.25*ell.axis.a*ell.axis.a)-(0.25*ell.axis.b*ell.axis.b)
  if (ell.demiaxis.c.sqr<0)
    ell.demiaxis.c.sqr<-ell.axis.a/2
  else
    ell.demiaxis.c<-sqrt(ell.demiaxis.c.sqr)
  sin.c<-ell.demiaxis.c*sin(ellipG[5])
  cos.c<-ell.demiaxis.c*cos(ellipG[5])
  ell.foc.y<-ell.center.y-sin.c
  ell.foc.x<-ell.center.x-cos.c
  return (c(exp(ell.foc.x),exp(ell.foc.y)))
}

fbsb <- function(sp_name) {
  sp_name <- gsub("\\s+", " ", sp_name)
  sp_name=trimws(sp_name, "b")
  sp_name=gsub(" ","-",sp_name)
  page_a=paste0("https://www.fishbase.se/summary/",sp_name)
  page_b=paste0("https://www.fishbase.ca/summary/",sp_name)
  page_c=paste0("https://www.sealifebase.se/summary/",sp_name)
  page_d=paste0("https://www.sealifebase.ca/summary/",sp_name)
  
  thepage_a=tryCatch(readLines(page_a,warn = FALSE), error=function(err) 1)
  thepage_b=tryCatch(readLines(page_b,warn = FALSE), error=function(err) 1)
  thepage_c=tryCatch(readLines(page_c,warn = FALSE), error=function(err) 1)
  thepage_d=tryCatch(readLines(page_d,warn = FALSE), error=function(err) 1)
  
  if (length(thepage_a)<30 & length(thepage_b)<30 & length(thepage_c)<30 & length(thepage_d)<30 ) {
    plain.text1=NA
    plain.text2=NA
    r_range=NA
    Resilience=NA
    page_=NA } else {
      pages=list(thepage_a,thepage_b,thepage_c,thepage_d)
      thepage_picker=c(length(thepage_a),length(thepage_b), length(thepage_c),length(thepage_d))
      thepage=pages[[which.max(thepage_picker)]]
      
      pages_=list(page_a,page_b,page_c,page_d)
      page_=pages_[[which.max(thepage_picker)]]
      
      x=grep('Prior r',thepage)
      y=grep('Resilience',thepage)
      datalines1 =thepage[x]
      datalines1=datalines1[nchar(datalines1)== max(nchar(datalines1))]
      datalines2 =thepage[y]
      
      if (sum(nchar(paste0(htm2txt::htm2txt(datalines2))))==10) {
        datalines2=thepage[thepage %like% "population doubling time"]
      }
      
      # returns TRUE
      if (identical(datalines1, character(0))) {
        plain.text1=NA
        r_range=list(c(NA,NA))
      } else if(grepl("[0-9]", datalines1)) {
        plain.text1 <-htm2txt::htm2txt(datalines1)
        plain.text1=gsub("\t","",plain.text1)
        r_range=gsub(".*=","",plain.text1)
        r_range=gsub(",.*","",r_range)
        r_range=gsub(" ","",r_range)
        r_range=strsplit(r_range, "-")
      } else {
        plain.text1=NA
        r_range=list(c(NA,NA))}
      plain.text2 <-htm2txt::htm2txt(datalines2)
      plain.text2=gsub("\t","",plain.text2)
      plain.text2=gsub(".*:", "", plain.text2)
      plain.text2=stringi::stri_trans_general(plain.text2, "ascii")
      plain.text2=gsub("[^[:alnum:][:blank:]?&,.;)(=/\\-]", "", plain.text2)
      plain.text2=gsub("B","",plain.text2)
      plain.text2=gsub("  ", "", plain.text2)
      Resilience=gsub(",.*","",plain.text2)
      Resilience=gsub(" ","",Resilience)}
  results=c(NA,NA,NA,NA,NA,NA)
  results[1]=plain.text1
  results[2]=ifelse(identical(plain.text2, character(0)),NA, plain.text2)
  results[3]=as.numeric(r_range[[1]][1])
  results[4]=as.numeric(r_range[[1]][2])
  results[5]=ifelse(identical(Resilience, character(0)),NA, Resilience)
  results[6]=page_
  
  return(results)
}


MSY_calculator=function(catch.obj) {
  nyr          <- length(catch.obj$yr) # number of years in the time series
  max.yr.i     <- which.max(catch.obj$ct_smthd)
  sd.ct        <- sd(catch.obj$ct_smthd,na.rm = T)
  mean.ct      <- mean(catch.obj$ct_smthd,na.rm = T)
  min_max      <-  catch.obj$ct_smthd[which.min(catch.obj$ct_smthd)]/catch.obj$ct_smthd[ which.max(catch.obj$ct_smthd)]
  ct.sort     <- sort(catch.obj$ct)
  if(max.yr.i>(nyr-4) || ((sd.ct/mean.ct) < 0.1 && min_max > 0.66)) {
    MSY.pr <- mean(ct.sort[(nyr-2):nyr]) } else {
      MSY.pr <- 0.75*mean(ct.sort[(nyr-4):nyr]) } # else, use fraction of mean of 5 highest catches as MSY prior
  
  sd.log.msy.pr <- 0.3 # rounded upward to account for reduced variability in selected stocks
  log.msy.pr    <- log(MSY.pr)
  prior.msy     <- c(exp(log.msy.pr-1.96*sd.log.msy.pr),exp(log.msy.pr+1.96*sd.log.msy.pr))
  init.msy      <- MSY.pr
  MSY.pr     <- exp(log.msy.pr)#*1000
  
}
ANN.priors=function(Catch.obj,MSY ) {
  nn_file<- "ffnn.bin"
  #   req(input$Priors_but2)
  min_max= Catch.obj$ct_smthd[which.min(Catch.obj$ct_smthd)]/ Catch.obj$ct_smthd[which.max(Catch.obj$ct_smthd)]
  
  nyr          <- length(Catch.obj$yr) # number of years in the time series
  
  if(min_max > 0.7) { # if catch is about flat, use middle year as int.yr
    int.yr    <- as.integer(mean(c(input$yr_slct[1], input$yr_slct[2])))
  } else { # only consider catch 5 years away from end points and within last 30 years # 50
    yrs.int       <- Catch.obj$yr[Catch.obj$yr>(Catch.obj$yr[nyr]-30) & Catch.obj$yr>Catch.obj$yr[4] & Catch.obj$yr<Catch.obj$yr[nyr-4]]
    ct.int        <- Catch.obj$ct_smthd[Catch.obj$yr>(Catch.obj$yr[nyr]-30) & Catch.obj$yr>Catch.obj$yr[4] & Catch.obj$yr<Catch.obj$yr[nyr-4]]
    min.ct.int    <- min(ct.int)
    min.ct.int.yr <- yrs.int[which.min(ct.int)]
    max.ct.int    <- max(ct.int)
    max.ct.int.yr <- yrs.int[which.max(ct.int)]
    #if min year is after max year, use min year for int year
    if(min.ct.int.yr > max.ct.int.yr) { int.yr <- min.ct.int.yr } else {
      min.ct.after.max <- min(ct.int[yrs.int >= max.ct.int.yr])
      if((min.ct.after.max/max.ct.int) < 0.75) {
        int.yr <- yrs.int[yrs.int > max.ct.int.yr & ct.int==min.ct.after.max]
      } else {int.yr <- min.ct.int.yr}
    }
    
  }
  
  load(file = nn_file) # load neural network file
  nn=list(nn.startbio=nn.startbio,
          nn.intbio= nn.intbio,
          nn.endbio=nn.endbio,
          nn.values=data.frame(
            "slope.first.max"=slope.first.max,
            "slope.first.min"=slope.first.min,
            "slope.last.max"=slope.last.max,
            "slope.last.min"=slope.last.min,
            "yr.norm.max"=yr.norm.max,
            "yr.norm.min"=yr.norm.min
          ))
  
  
  ct.raw=Catch.obj$ct
  ct=Catch.obj$ct_smthd
  mean.ct      <- mean(Catch.obj$ct_smthd,na.rm = T)
  max.ct       <- Catch.obj$ct_smthd[which.max(Catch.obj$ct_smthd)]
  
  # get additional properties of catch time series
  mean.ct.end       = mean(ct.raw[(nyr-4):nyr]) # mean of catch in last 5 years
  mean.ct_MSY.end   = mean.ct.end/MSY
  # Get slope of catch in last 10 years
  ct.last           = ct[(nyr-9):nyr]/mean(ct) # last catch standardized by mean catch
  yrs.last          = seq(1:10)
  fit.last          = lm(ct.last ~ yrs.last)
  slope.last        = as.numeric(coefficients(fit.last)[2])
  slope.last.nrm    = (slope.last - nn[["nn.values"]]$slope.last.min)/(nn[["nn.values"]]$slope.last.max - nn[["nn.values"]]$slope.last.min) # normalized slope 0-1
  # Get slope of catch in first 10 years
  ct.first         = ct[1:10]/mean.ct # catch standardized by mean catch
  yrs.first         <- seq(1:10)
  fit.first         <- lm(ct.first ~ yrs.first)
  slope.first       <- as.numeric(coefficients(fit.first)[2])
  slope.first.nrm   <- (slope.first -  nn[["nn.values"]]$slope.first.min)/( nn[["nn.values"]]$slope.first.max -  nn[["nn.values"]]$slope.first.min) # normalized slope 0-1
  ct_max.1          =ct[1]/max.ct
  ct_MSY.1          =ct[1]/MSY
  mean.ct_MSY.start = mean(ct[1:5])/MSY
  ct_MSY.int        =ct[which(Catch.obj$yr==int.yr)]/MSY
  ct_max.end        = ct[nyr]/max.ct
  ct_MSY.end        = ct[nyr]/MSY
  max.ct.i          = which.max(ct)/nyr
  int.ct.i        = which( Catch.obj$yr==int.yr)/nyr
  min.ct.i          = which.min(ct)/nyr
  yr.norm           <- (nyr - nn[["nn.values"]]$yr.norm.min)/(nn[["nn.values"]]$yr.norm.max - nn[["nn.values"]]$yr.norm.min) # normalize nyr 0-1
  # classify catch patterns as Flat, LH, LHL, HL, HLH or OTH
  if(min_max >=0.45 & ct_max.1 >= 0.45 & ct_max.end >= 0.45) { Flat <- 1 } else Flat <- 0
  if(min_max<0.25 & ct_max.1<0.45 & ct_max.end>0.45) { LH <- 1 } else LH <- 0
  if(min_max<0.25 & ct_max.1 < 0.45 & ct_max.end < 0.25) { LHL <- 1 } else LHL <- 0
  if(min_max<0.25 & ct_max.1 > 0.5 & ct_max.end < 0.25) { HL <- 1 } else HL <- 0
  if(min_max<0.25 & ct_max.1 >= 0.45 & ct_max.end >= 0.45) { HLH <- 1 } else HLH <- 0
  if(sum(c(Flat,LHL,LH,HL,HLH))<1) { OTH <- 1 } else OTH <- 0
  
  
  
  ANN_CLs=data.frame(bk.MSY = c(0.256 , 0.721 ), # based on all ct/MSY.pr data for 400 stocks # data copied from Plot_ct_MSY_13.R output
                     CL.1   = c( 0.01 , 0.203 ),
                     CL.2   = c( 0.2 , 0.431 ),
                     CL.3   = c( 0.8 , -0.45 ),
                     CL.4   = c( 1.02 , -0.247 ))
  ct_MSY.lim   <- 1.21  # ct/MSY.pr ratio above which B/k prior is assumed constant
  
  if(mean.ct_MSY.start >= ct_MSY.lim) {
    startbio    <- ANN_CLs$bk.MSY
  } else { # else run neural network to determine whether B/k is above or below 0.5
    nninput.start  <- as.data.frame(cbind(Flat,LH,LHL,
                                          HL,HLH,OTH,
                                          min_max,max.ct.i,min.ct.i,
                                          yr.norm, #ct_MSY.1,
                                          mean.ct_MSY.start,slope.first.nrm,
                                          mean.ct_MSY.end,slope.last.nrm)) #gm.prior.r
    
    pr.nn.startbio <- neuralnet::compute(nn[["nn.startbio"]], nninput.start)
    pr.nn_indices.startbio <- max.col(pr.nn.startbio$net.result)
    ct_MSY.start.use     <- ifelse(ct_MSY.1 < mean.ct_MSY.start,ct_MSY.1,mean.ct_MSY.start)
    if(pr.nn_indices.startbio==1) { # if nn predicts B/k below 0.5
      startbio      <- c(ANN_CLs$CL.1[1]+ANN_CLs$CL.1[2]*ct_MSY.start.use,
                         ANN_CLs$CL.2[1]+ANN_CLs$CL.2[2]*ct_MSY.start.use) } else {
                           startbio      <- c(ANN_CLs$CL.3[1]+ANN_CLs$CL.3[2]*ct_MSY.start.use,
                                              ANN_CLs$CL.4[1]+ANN_CLs$CL.4[2]*ct_MSY.start.use) }
  } # end of neural network loop
  
  if(ct_MSY.int >= ct_MSY.lim) {
    intbio=ANN_CLs$bk.MSY
  } else { # else run neural network to determine whether B/k is above or below 0.5
    nninput.int    <- as.data.frame(cbind(Flat,LH,LHL,
                                          HL,HLH,OTH, # shapes
                                          min_max,max.ct.i,min.ct.i,yr.norm, # general
                                          int.ct.i,ct_MSY.int,                   # int
                                          mean.ct_MSY.end,slope.last.nrm,        # end
                                          mean.ct_MSY.start,slope.first.nrm))     # start
    
    pr.nn.intbio   <-neuralnet::compute(nn[["nn.intbio"]], nninput.int)
    pr.nn_indices.intbio <- max.col(pr.nn.intbio$net.result)
    if(pr.nn_indices.intbio==1){ # if nn predicts B/k below 0.5
      intbio      <- c(ANN_CLs$CL.1[1]+ANN_CLs$CL.1[2]*ct_MSY.int,ANN_CLs$CL.2[1]+ANN_CLs$CL.2[2]*ct_MSY.int) } else {
        intbio    <- c(ANN_CLs$CL.3[1]+ANN_CLs$CL.3[2]*ct_MSY.int,ANN_CLs$CL.4[1]+ANN_CLs$CL.4[2]*ct_MSY.int)}
  } # end of nn loop
  
  if(mean.ct_MSY.end >= ct_MSY.lim) {
    endbio    =ANN_CLs$bk.MSY
  } else { # else run neural network to determine whether B/k is above or below 0.5
    nninput.end    <- as.data.frame(cbind(Flat,LH,LHL,
                                          HL,HLH,OTH,
                                          ct_MSY.int,min_max, max.ct.i,  # arbitrary best sequence
                                          int.ct.i, min.ct.i,yr.norm,
                                          mean.ct_MSY.start, slope.first.nrm,
                                          mean.ct_MSY.end, slope.last.nrm))
    pr.nn.endbio   <- neuralnet::compute(nn[["nn.endbio"]], nninput.end)
    pr.nn_indices.endbio <- max.col(pr.nn.endbio$net.result)
    ct_MSY.use    <- ifelse(ct_MSY.end < mean.ct_MSY.end,
                            ct_MSY.end,mean.ct_MSY.end)
    if(pr.nn_indices.endbio==1){ # if nn predicts B/k below 0.5
      endbio      <- c(ANN_CLs$CL.1[1]+ANN_CLs$CL.1[2]*ct_MSY.use,ANN_CLs$CL.2[1]+ANN_CLs$CL.2[2]*ct_MSY.use) } else {
        endbio    <- c(ANN_CLs$CL.3[1]+ANN_CLs$CL.3[2]*ct_MSY.use,ANN_CLs$CL.4[1]+ANN_CLs$CL.4[2]*ct_MSY.use)}
  } # end of nn loop
  
  strprrs=c(startbio,NA)
  int_prrs=c(intbio,int.yr)
  endprrs=c(endbio,NA)
  
  empBKpriors=data.frame(start=strprrs,int=int_prrs,end=endprrs)   ########SWITCHER OFF
  empBKpriors=round(empBKpriors,4)
  
  
}


rk_priors=function(rlow,rhigh,MSY,Stock_objID) {
  n            <- 5000 # number of points in multivariate cloud in graph panel (b)
  log.msy.pr=log(as.numeric(MSY))
  # turn numerical ranges into log-normal distributions
  mean.log.r=mean(log(c(as.numeric(rlow),as.numeric(rhigh))))
  sd.log.r=(log(as.numeric(rhigh))-log(as.numeric(rlow)))/(2*1.96)  # assume range covers 4 SD
  sd.log.msy.pr <- 0.3 # rounded upward to account for reduced variability in selected stocks
  # generate msy and r independently
  ri1     <- rlnorm(n,mean.log.r,sd.log.r)
  msyi1  <- rlnorm(n,log.msy.pr,sd.log.msy.pr)
  ki1     <- msyi1*4/ri1
  #><>MSY: get log median and covariance
  cov_rk <- cov(cbind(log(ri1),log(ki1)))
  rk.cor <- cov_rk[2,1] #MSY: correlation rho input to JAGS
  mean.log.k <- median(log(ki1))
  sd.log.k.pr <- sd(log(ki1))
  # # quick check must be the same
  sd.log.k = sqrt(cov_rk[2,2])
  prior.k  <- exp(mean.log.k-1.96*sd.log.k.pr)
  prior.k[2]  <- exp(mean.log.k+1.96*sd.log.k.pr)
  msy.init <- exp(mean.log.k)
  mvn.log.rk <- mvn(n=n,mean.log.r=mean.log.r,sd.log.r=sd.log.r,mean.log.k=mean.log.k,sd.log.k=sd.log.k)
  #><>MSY rk based on empirical mvn
  ri.emp    <- exp(mvn.log.rk[,1])
  ki1.emp    <- exp(mvn.log.rk[,2])
  init.r      <- as.numeric(rlow)+0.8*(as.numeric(rhigh)-as.numeric(rlow))
  init.k      <- prior.k[1]+0.1*(prior.k[2]-prior.k[1])
  rk_priors=list(rkpriors=data.frame(Stock_objID,
                                     prior.k.low=prior.k[1],
                                     prior.k.hi=prior.k[2],
                                     init.k=init.k,
                                     init.r=init.r,
                                     rk.cor=rk.cor),
                 rkplots=data.frame(ri1=ri1,ki1=ki1),
                 rkplotsemp=data.frame(ri.emp=ri.emp,ki1.emp=ki1.emp),
                 mean.log.k=mean.log.k,
                 sd.log.k=sd.log.k)
  return(rk_priors)
}



q.priors=function(ABC.obj,rk.obj,METHOD) {
  if (METHOD == "CMSY") {
    q.init=1
    q.prior= c(0.99,1.01)
  }else if (METHOD == "BSM") { 
    if(ABC.obj[["input"]][["Input_parameters"]]$btype=="biomass") {
      q.biomass.pr <- c(0.9,1.1) # if btype=="biomass" this is the prior range for q
      q.prior <- q.biomass.pr 
      
      q.init  <- mean(q.prior)
    } else { # if btype is CPUE
      # get mean of 3 highest bt values
      bt=ABC.obj[["input"]][["Input_data"]]$bt[!is.na(ABC.obj[["input"]][["Input_data"]]$bt_smthd)]
      bt.sort <- sort(bt)
      mean.max.bt <- mean(bt.sort[(length(bt.sort)-2):length(bt.sort)],na.rm = T)
      # Estimate q.prior[2] from max cpue = q * k, q.prior[1] from max cpue = q * 0.25 * k
      q.1           <- mean.max.bt/rk.obj[["rkpriors"]]$prior.k.hi
      q.2           <- mean.max.bt/(0.25*rk.obj[["rkpriors"]]$prior.k.low)
      q.prior       <- c(q.1,q.2)
      q.init        <- mean(q.prior)}
  }
  
  return(c(q.prior,q.init))
}  


ABC_fit= function(ABC.obj,rk.obj,q.obj,METHOD) {
  
  q_prrs=q.obj[1:2]
  init.q_=q.obj[3]
  # if (METHOD == "CMSY") {
  #   init.q_=1
  #   q_prrs= c(0.99,1.01)
  # }else if (METHOD == "BSM") { 
  #   q_prrs=c(as.numeric(ABC.obj[["input"]][["Input_parameters"]]$q_low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$q_high))
  #   if(mean(ABC.obj[["input"]][["Input_data"]]$bt_smthd,na.rm=T)<100) {
  #     q_prrs <- q_prrs/1000
  #     }
  #   
  #   init.q_=mean(c(as.numeric(ABC.obj[["input"]][["Input_parameters"]]$q_low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$q_high)),na.rm=T)
  #   if(mean(ABC.obj[["input"]][["Input_data"]]$bt_smthd,na.rm=T)<100) {
  #     init.q_ <- init.q_/1000
  #     }
  # }
  
  ####nbk
  if (ABC.obj[["input"]][["Input_parameters"]]$nbk=="Only start")  {
    nbk=1 } else if (ABC.obj[["input"]][["Input_parameters"]]$nbk=="Start & intermediate") {
      nbk=2  } else {
        nbk=3
      }
  
  pen.bk = pen.F = rep(0,length(ABC.obj[["input"]][["Input_data"]]$ct_smthd))
  
  
  b.yrs = c(1,length(as.integer(ABC.obj[["input"]][["Input_parameters"]]$StartYear):
                       as.integer(ABC.obj[["input"]][["Input_parameters"]]$int.yr)),
            length(as.integer(ABC.obj[["input"]][["Input_parameters"]]$StartYear):
                     as.integer(ABC.obj[["input"]][["Input_parameters"]]$EndYear)))
  
  b.prior_ = rbind(matrix(c(as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.hi),
                            as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.hi),
                            as.numeric(ABC.obj[["input"]][["Input_parameters"]]$endb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$endb.hi)),2,3),rep(0,3)) # last row includes the 0 penalty
  if (METHOD == "CMSY") { 
    bt.start <- mean(c(rk.obj[["rkpriors"]]$prior.k.low*
                         as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.low),
                       rk.obj[["rkpriors"]]$prior.k.hi*
                         as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.hi))) # derive proxy for first bt value
    
    btj_ <- c(bt.start,rep(NA,length(ABC.obj[["input"]][["Input_data"]]$ct_smthd)-1)) }else if (METHOD == "BSM") { 
      btj_=ABC.obj[["input"]][["Input_data"]]$bt_smthd
    }
  
  if (METHOD == "CMSY") { 
    CV.cpue_=rep(0.2,length(ABC.obj[["input"]][["Input_data"]]$ct_smthd))  }else if (METHOD == "BSM") { 
      
      if (all(is.na(ABC.obj[["input"]][["Input_data"]]$bt_cv)) &  !is.na(ABC.obj[["input"]][["Input_parameters"]]$CV_cpue)) {
        CV.cpue_      <- rep(ABC.obj[["input"]][["Input_parameters"]]$CV_cpue,length(ABC.obj[["input"]][["Input_data"]]$ct_smthd))
      } else if  (all(is.na(ABC.obj[["input"]][["Input_data"]]$bt_cv))) {
        CV.cpue_      <- rep(0.2,length(ABC.obj[["input"]][["Input_data"]]$ct_smthd)) } else {
          CV.cpue_<- ABC.obj[["input"]][["Input_data"]]$bt_cv
        }
      
      CV.cpue_[is.na(CV.cpue_)]=0.2
    }
  
  #CV.cpue_[is.na(CV.cpue_)]=0.2
  
  jags_outputs <- bsm(ct=as.numeric(ABC.obj[["input"]][["Input_data"]]$ct_smthd),
                      btj <-btj_,
                      nyr=length(ABC.obj[["input"]][["Input_data"]]$ct_smthd),
                      prior.r=as.numeric(c(ABC.obj[["input"]][["Input_parameters"]]$r.low,ABC.obj[["input"]][["Input_parameters"]]$r.hi)),
                      prior.k=as.numeric(c(rk.obj[["rkpriors"]]$prior.k.low, rk.obj[["rkpriors"]]$prior.k.hi)),
                      startbio=as.numeric(c(ABC.obj[["input"]][["Input_parameters"]]$stb.low,ABC.obj[["input"]][["Input_parameters"]]$stb.hi)),
                      q.priorj=as.numeric(q_prrs),#c(0.99,1.01),# # since no abundance data are available in this run,
                      init.q=init.q_ ,           # q could be omitted and is set here to (practically) 1  ,
                      init.r=rk.obj[["rkpriors"]]$init.r,
                      init.k=rk.obj[["rkpriors"]]$init.k,
                      pen.bk=pen.bk,
                      pen.F=pen.F,
                      b.yrs=b.yrs,
                      b.prior=b.prior_,
                      CV.C=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$CV_catch),# 0.15,  #><>MSY: Add Catch CV
                      CV.cpue=CV.cpue_,#input$CPUE_CV,  #><>MSY: Add minimum realistic cpue CV
                      nbk=nbk,
                      rk.cor=rk.obj[["rkpriors"]]$rk.cor,
                      n.chains     <- 2, # number of chains to be used in JAGS, default = 2
                      cmsyjags=F)$BUGSoutput$sims.list
  jags_outputs[["r"]]=as.numeric(coda::mcmc(jags_outputs[["r"]]))
  jags_outputs[["k"]]=as.numeric(coda::mcmc(jags_outputs[["k"]]))
  jags_outputs[["q"]]=as.numeric(coda::mcmc(jags_outputs[["q"]]))
  jags_outputs[["ppd.r"]]<- exp(as.numeric(coda::mcmc(jags_outputs[["ppd.logrk"]][,1])))
  jags_outputs[["ppd.k"]]<- exp(as.numeric(coda::mcmc(jags_outputs[["ppd.logrk"]][,2])))
  return(jags_outputs)  
}



extract_ABC_fit= function(fit_obj,q.obj,ABC.obj) {
  
  ABC.obj[["input"]][["Input_parameters"]]$q_low=q.obj[1]
  ABC.obj[["input"]][["Input_parameters"]]$q_high=q.obj[2]
  
  ABC.obj[["output"]][["output_posteriors"]]$r_post=as.numeric(quantile(fit_obj[["r"]],c(0.5,0.025,0.975))) #median, 95% CIs
  ABC.obj[["output"]][["output_posteriors"]]$k_post<- as.numeric(quantile(fit_obj[["k"]],c(0.5,0.025,0.975)))
  ABC.obj[["output"]][["output_posteriors"]]$MSY_post<-as.numeric( quantile(fit_obj[["r"]]*fit_obj[["k"]]/4,c(0.5,0.025,0.975)))
  ABC.obj[["output"]][["output_posteriors"]]$q_post<-as.numeric( quantile(fit_obj[["q"]],c(0.5,0.025,0.975)))
  ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post<- as.numeric(quantile(fit_obj[["r"]]/2,c(0.5,0.025,0.975)))
  ABC.obj[["output"]][["output_posteriors"]]$Bmsy_post<- as.numeric(quantile(fit_obj[["k"]]/2,c(0.5,0.025,0.975)))

  ABC.obj[["output"]][["output_timeseries"]][, c("Catch","Catch_low","Catch_high")]=t(apply(fit_obj$ct.jags,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  ABC.obj[["output"]][["output_timeseries"]][, c("FFmsy","FFmsy_low","FFmsy_high")]=t(apply(fit_obj$FFmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  ABC.obj[["output"]][["output_timeseries"]][, c("f","f_low","f_high")]=t(apply(fit_obj$F,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  ABC.obj[["output"]][["output_timeseries"]][, c("BBmsy","BBmsy_low","BBmsy_high")]=t(apply(fit_obj$BBmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  ABC.obj[["output"]][["output_timeseries"]][, c("B","B_low","B_high")]= t(apply(fit_obj$B,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  ABC.obj[["output"]][["output_timeseries"]][, c("bk","bk_low","bk_high")]=  t(apply(fit_obj$P,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  
  if(tail(ABC.obj[["output"]][["output_timeseries"]]$BBmsy,1)<0.5){
    ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1]=
      ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post[1]*2*tail(ABC.obj[["output"]][["output_timeseries"]]$BBmsy,1)
    ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2]=
      ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post[2]*2*tail(ABC.obj[["output"]][["output_timeseries"]]$BBmsy,1)
    ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3]=
      ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post[3]*2*tail(ABC.obj[["output"]][["output_timeseries"]]$BBmsy,1)
    
    ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post_correction_note= rep("Fmsy was corrected downward to account for reduced recruitment.",3)
  } else {
    ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1]=ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post[1]
    ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2]=ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post[2]
    ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3]=ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post[3]
    ABC.obj[["output"]][["output_posteriors"]]$Fmsy_post_correction_note= rep("No correction to FFmsy",3)
  }
  return(ABC.obj)
}

ggrk.plot= function(ABC.obj,rk.obj,fit_obj,METHOD) {
  
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  
  my_y_title <-bquote(atop(Possible~"r-k"~pairs~"for"~bold(.(ABC.obj[["input"]][["Stock_info"]]$Stock))))
  
  pick_rk<-ggplot2::ggplot() +
    ggplot2::geom_point(data=rk.obj[["rkplots"]],ggplot2::aes(x=ri1,y=ki1),color="grey",size=0.7,alpha=0.4)+
    ggplot2::scale_x_continuous(trans='log',limits=c(0.95*quantile(rk.obj[["rkplots"]]$ri1,0.001),1.2*quantile(rk.obj[["rkplots"]]$ri1,0.999)),labels = function(x) round(as.numeric(x),2)) +
    ggplot2::scale_y_continuous(trans='log',limits=c(0.95*quantile(rk.obj[["rkplots"]]$ki1,0.001),1.2*quantile(rk.obj[["rkplots"]]$ki1,0.999)),labels = function(x) round(as.numeric(x))) +
    ggplot2::theme_classic()+
    ggplot2::labs(y="k (tonnes)", x="r (1/year)",title=my_y_title)+
    ggplot2::geom_rect(ggplot2::aes(xmin = as.numeric( ABC.obj[["input"]][["Input_parameters"]]$r.low),
                                    xmax = as.numeric( ABC.obj[["input"]][["Input_parameters"]]$r.hi),
                                    ymin =  as.numeric(rk.obj[["rkpriors"]]$prior.k.low),
                                    ymax =  as.numeric(rk.obj[["rkpriors"]]$prior.k.hi)),linetype="dotted",fill=NA,colour = "gray") +
    ggplot2::geom_point(data=data.frame(rs=fit_obj[["r"]],ks=fit_obj[["k"]]),ggplot2::aes(x=rs,y=ks),color="gray18",size=0.7,alpha=0.2)+
    ggplot2::geom_point(data= ABC.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=r_post[1],y=k_post[1]),color=clr,size=0.7)+
    ggplot2::geom_segment(data= ABC.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=ifelse(r_post[2]>0,r_post[2],0.001),y=k_post[1],xend=r_post[3],yend=k_post[1]),col=clr,size=0.7)+
    ggplot2::geom_segment(data= ABC.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=r_post[1],y=ifelse(k_post[2]>0,k_post[2],0.001),xend=r_post[1],yend=k_post[3]),col=clr,size=0.7)+
    theme(text = element_text(size = 10)) 
  
  return(pick_rk)
  
}

ggrk_2.plot= function(ABC.obj,rk.obj,fit_obj,METHOD) {
  
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  
  my_y_title <-bquote(atop(Analysis~of~viable~"r-k"~pairs~"for"~bold(.(ABC.obj[["input"]][["Stock_info"]]$Stock))))
  
  pick_rk<-ggplot2::ggplot() +
    ggplot2::geom_point(data=rk.obj[["rkplots"]],ggplot2::aes(x=ri1,y=ki1),color="grey",size=0.7,alpha=0.4)+
    ggplot2::scale_x_continuous(trans='log',limits=c( 0.9*as.numeric( ABC.obj[["output"]][["output_posteriors"]]$r_post[2]),as.numeric( 1.1*ABC.obj[["output"]][["output_posteriors"]]$r_post[3])),labels = function(x) round(as.numeric(x),2)) +
    ggplot2::scale_y_continuous(trans='log',limits=c(as.numeric(rk.obj[["rkpriors"]]$prior.k.low), as.numeric(rk.obj[["rkpriors"]]$prior.k.hi)),labels = function(x) round(as.numeric(x))) +
    ggplot2::theme_classic()+
    ggplot2::labs(y="k (tonnes)", x="r (1/year)",title=my_y_title)+
    ggplot2::geom_point(data=data.frame(rs=fit_obj[["r"]],ks=fit_obj[["k"]]),ggplot2::aes(x=rs,y=ks),color="gray18",size=0.7,alpha=0.2)+
    ggplot2::geom_point(data= ABC.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=r_post[1],y=k_post[1]),color=clr,size=0.7)+
    ggplot2::geom_segment(data= ABC.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=ifelse(r_post[2]>0,r_post[2], as.numeric( ABC.obj[["input"]][["Input_parameters"]]$r.low)),y=k_post[1],xend=r_post[3],yend=k_post[1]),col=clr,size=0.7)+
    ggplot2::geom_segment(data= ABC.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=r_post[1],y=ifelse(k_post[2]>0,k_post[2],0.001),xend=r_post[1],yend=k_post[3]),col=clr,size=0.7)+
    theme(text = element_text(size = 10)) 
  
  return(pick_rk)
  
}



ggcatch.plot= function(ABC.obj,METHOD,Management=F) {
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  if (Management==F) {
    my_y_title <-bquote(atop(Catch~"for"~bold(.(ABC.obj[["input"]][["Stock_info"]]$Stock))))
  } else {
    my_y_title="Catch"
  }
  temp_data=as.data.frame(ABC.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  
  pic_catch=ggplot2::ggplot()+
    ggplot2::geom_segment(aes(y=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                              yend=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                              x = min(temp_data$yr), xend =  max( temp_data$yr) ,linetype="B"),size=0.7)+
    ggplot2::geom_ribbon(ggplot2::aes(x=temp_data$yr,ymin=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[2],
                                      ymax=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[3],linetype="B"), alpha=0.2)+
    ggplot2::geom_line(data = temp_data,ggplot2::aes(x=yr,y=Catch,color="A"),size=0.7)+
    ggplot2::geom_ribbon(data = temp_data,ggplot2::aes(x=yr,ymin=Catch_low, ymax=Catch_high,fill="A"), alpha=0.2)+#y=ct.jags_low,
    ggplot2::theme_classic()+
    ggplot2::scale_linetype_manual(labels=c("MSY and 95% CI"),values=c("dashed"))+
    ggplot2::scale_color_manual(labels=c("Catch and 95% CI"),values=c(clr))+
    ggplot2::scale_fill_manual(labels=c("Catch and 95% CI"),values=c(clr))+ggplot2::theme(
      legend.position = "bottom")+
    ggplot2::labs(x="Year",y="Catch",title=my_y_title,fill="",color="",linetype="")+
    ggplot2::scale_y_continuous(limits=c(0,NA)) +
    theme(text = element_text(size = 10)) 

  if (Management==T) {
    pic_catch=ggplot2::ggplot()+
      ggplot2::geom_segment(aes(y=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                yend=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                x = min(temp_data$yr), xend =  max( temp_data$yr) ,linetype="B"),size=0.7)+
      ggplot2::geom_ribbon(ggplot2::aes(x=temp_data$yr,ymin=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[2],
                                        ymax=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[3],linetype="B"), alpha=0.2)+
      ggplot2::geom_line(data = temp_data,ggplot2::aes(x=yr,y=Catch,color="A"),size=0.7)+
      ggplot2::geom_ribbon(data = temp_data,ggplot2::aes(x=yr,ymin=Catch_low, ymax=Catch_high,fill="A"), alpha=0.2)+#y=ct.jags_low,
      ggplot2::theme_classic()+
      ggplot2::scale_linetype_manual(labels=c("MSY"),values=c("dashed"))+
      ggplot2::scale_color_manual(labels=c("Catch"),values=c(clr))+
      ggplot2::scale_fill_manual(labels=c("Catch"),values=c(clr))+
      ggplot2::labs(x="Year",y="Catch",title=my_y_title,fill="",color="",linetype="")+
      ggplot2::scale_y_continuous(limits=c(0,NA)) +
      theme(text = element_text(size = 10)) 
  }
  return(pic_catch)
}

ggbk.plot= function(ABC.obj,METHOD,Management=F) {
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  
  if (Management==F) {
    my_y_title <-bquote(atop(Stock~size~"for"~bold(.(ABC.obj[["input"]][["Stock_info"]]$Stock))))
  } else {
    my_y_title="Stock size"
  }
  
  temp_data=as.data.frame(ABC.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  Bmsy= expression(B[MSY])
  Blim= expression(B[lim])
  
  pic_bk=ggplot2::ggplot()+
    ggplot2::geom_line(data=temp_data,ggplot2::aes(x=yr,y=bk,color=clr),size=0.7)+
    ggplot2::geom_ribbon(data=temp_data,ggplot2::aes(x=yr,ymin=bk_low, ymax=bk_high,fill=clr),alpha=0.2)+
    ggplot2::theme_classic()+
    ggplot2::geom_hline( aes(yintercept=0.5,linetype="A"),color="black",size=0.7)+
    ggplot2::geom_hline( aes(yintercept=0.25,linetype="B"),color="red",size=0.7)+
    ggplot2::geom_segment(ggplot2::aes(x=min(as.numeric(temp_data$yr)),
                                       y=as.numeric(ABC.obj[["input"]][["Input_parameters"]][["stb.low"]]),
                                       xend=min(as.numeric(temp_data$yr)),
                                       yend=as.numeric(ABC.obj[["input"]][["Input_parameters"]][["stb.hi"]]),
                                       linetype="C"),col="purple",size=1)+
    ggplot2::scale_color_manual(labels=c("Biomass and 95% CI"),values=c(clr))+
    ggplot2::scale_fill_manual(labels=c("Biomass and 95% CI"),values=c(clr))+
    ggplot2::scale_linetype_manual(labels=c(Bmsy, Blim,"B/k priors"),values=c(2,3,1),
                                   guide = guide_legend(override.aes = list(color = c("black", "red","purple"),
                                                                            size = c(0.7, 0.7,1)))
    )+
    ggplot2::labs(y="Relative biomass B/k", x="Year",title=my_y_title,fill="",color="",shape="",linetype="")+ggplot2::theme(
      legend.position = "bottom")+
    ggplot2::scale_y_continuous(limits=c(0,NA)) +
    theme(text = element_text(size = 10)) 
  
  
  if (ABC.obj[["input"]][["Input_parameters"]][["nbk"]]=="Start & intermediate") {
    
    pic_bk=pic_bk+ggplot2::geom_segment(ggplot2::aes(x=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$int.yr),
                                                     y=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.low), 
                                                     xend=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$int.yr),
                                                     yend=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.hi),
                                                     linetype="C"),col="purple",size=1)
    
  } else if (ABC.obj[["input"]][["Input_parameters"]][["nbk"]]=="All three (recommended)") {
    pic_bk=pic_bk+ggplot2::geom_segment(ggplot2::aes(x=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$int.yr),
                                                     y=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.low), 
                                                     xend=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$int.yr),
                                                     yend=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.hi),
                                                     linetype="C"),col="purple",size=1)+
      ggplot2::geom_segment(ggplot2::aes(x=max(as.numeric(temp_data$yr)),
                                         y=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$endb.low), 
                                         xend=max(as.numeric(temp_data$yr)),
                                         yend=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$endb.hi),
                                         linetype="C"),col="purple",size=1)
  }
  
  if (Management==T) {
    pic_bk=ggplot2::ggplot()+
      ggplot2::geom_line(data=temp_data,ggplot2::aes(x=yr,y=bk,color=clr),size=0.7)+
      ggplot2::geom_ribbon(data=temp_data,ggplot2::aes(x=yr,ymin=bk_low, ymax=bk_high,fill=clr),alpha=0.2)+
      ggplot2::theme_classic()+
      ggplot2::geom_hline( aes(yintercept=0.5,linetype="A"),color="black",size=0.7)+
      ggplot2::geom_hline( aes(yintercept=0.25,linetype="B"),color="red",size=0.7)+
      ggplot2::scale_color_manual(labels=c("Biomass"),values=c(clr))+
      ggplot2::scale_fill_manual(labels=c("Biomass"),values=c(clr))+
      ggplot2::scale_linetype_manual(labels=c(Bmsy, Blim),values=c(2,3),
                                     guide = guide_legend(override.aes = list(color = c("black", "red"),
                                                                              size = c(0.7, 0.7))) )+
      ggplot2::labs(y="Relative biomass B/k", x="Year",title=my_y_title,fill="",color="",shape="",linetype="")+
      ggplot2::scale_y_continuous(limits=c(0,NA)) +
      theme(text = element_text(size = 10)) 
  }
  
  return(pic_bk)
}

ggFFmsy.plot= function(ABC.obj,METHOD,Management=F) {
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  if (Management==F) {
    my_y_title <-bquote(atop(Exploitation~"for"~bold(.(ABC.obj[["input"]][["Stock_info"]]$Stock))))
  } else {
    my_y_title="Exploitation"
  }
  
  temp_data=as.data.frame(ABC.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  ly= expression(F/F[MSY])
  A_= expression('' ~ F/F[MSY] ~ "and"~"95%"~ "CI")
  
  pic_FFmsy=ggplot2::ggplot()+
    ggplot2::geom_line(data=temp_data,ggplot2::aes(x=yr,y=FFmsy,color=clr),size=0.7)+
    ggplot2::geom_ribbon(data=temp_data,ggplot2::aes(x=yr,ymin=FFmsy_low, ymax=FFmsy_high,fill=clr),
                         alpha=0.2)+
    ggplot2::theme_classic()+
    ggplot2::geom_hline(yintercept=1, linetype="dashed")+
    ggplot2::scale_color_manual(labels=c(A_),values=c(clr))+
    ggplot2::scale_fill_manual(labels=c(A_),values=c(clr))+
    ggplot2::labs(y=ly, x="Year",title=my_y_title,fill="",color="",shape="",linetype="")+
    ggplot2::scale_y_continuous(limits=c(0,NA))+
    ggplot2::theme(legend.position = "bottom")+
    theme(text = element_text(size = 10)) 
  
  if (Management==T) {
    A_= expression(F/F[MSY])
    
    pic_FFmsy=ggplot2::ggplot()+
      ggplot2::geom_line(data=temp_data,ggplot2::aes(x=yr,y=FFmsy,color=clr),size=0.7)+
      ggplot2::geom_ribbon(data=temp_data,ggplot2::aes(x=yr,ymin=FFmsy_low, ymax=FFmsy_high,fill=clr),
                           alpha=0.2)+
      ggplot2::theme_classic()+
      ggplot2::geom_hline(yintercept=1, linetype="dashed")+
      ggplot2::scale_color_manual(labels=c(A_),values=c(clr))+
      ggplot2::scale_fill_manual(labels=c(A_),values=c(clr))+
      ggplot2::labs(y=ly, x="Year",title=my_y_title,fill="",color="",shape="",linetype="")+
      ggplot2::scale_y_continuous(limits=c(0,NA))+
      theme(text = element_text(size = 10)) 
  }
  return(pic_FFmsy)
}

ggparabola.plot= function(CLA.obj,METHOD) {
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  my_y_title <-bquote(atop(Equilibrium~curve~"for"~bold(.(CLA.obj[["input"]][["Stock_info"]]$Stock))))
  temp_data=as.data.frame(CLA.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  x=seq(from=0,to=2,by=0.001)
  y.c  <- ifelse(x>0.25,1,ifelse(x>0.125,4*x,exp(-10*(0.125-x))*4*x)) # correction for low recruitment below half and below quarter of Bmsy
  y=(4*x-(2*x)^2)*y.c
  temp_parabola=data.frame(x=x,y=y)
  max.parabola2=max(c(1.5,1.2*(ct.cmsy=as.numeric(CLA.obj[["output"]][["output_timeseries"]][["Catch"]])/
                                 as.numeric(CLA.obj[["output"]][["output_posteriors"]][["MSY_post"]][1]))),na.rm=T)
  
  MSY.cmsy=as.numeric(CLA.obj[["output"]][["output_posteriors"]][["MSY_post"]][1])
  
  pic_parabola=ggplot2::ggplot()+
    ggplot2::geom_line(data=temp_parabola,ggplot2::aes(x=x,y=y),size=0.7)+
    ggplot2::scale_y_continuous(limits=c(0,max.parabola2)) +
    ggplot2::scale_x_continuous(limits=c(0,1)) +
    ggplot2::geom_segment(data=temp_data,ggplot2::aes(x=bk,xend =lead(bk),y=Catch/MSY.cmsy,yend=lead(Catch/MSY.cmsy),color=clr), size=0.7,arrow=arrow(type="closed", length = unit(0.1, "inches")))+
    ggplot2::geom_point(data=temp_data,ggplot2::aes(x=bk[1],y=Catch[1]/MSY.cmsy,shape="0"),color=clr, size=2)+
    ggplot2::geom_point(data=temp_data,ggplot2::aes(x=bk[nyr],y=Catch[nyr]/MSY.cmsy,shape="2"),color=clr, size=2)+
    ggplot2::scale_color_manual(labels="Catch/MSY",values=c(clr))+
    ggplot2::scale_shape_manual(name="",labels=c("start year", "end year"),values=c(0,2))+
    ggplot2::theme(legend.position = "bottom")+
    ggplot2::labs(y="Catch / MSY", x="Relative biomass B/k",color="",title=my_y_title)+
    ggplot2::theme_classic()+ggplot2::theme(legend.position = "bottom")+
    theme(text = element_text(size = 10)) 
  
  return(pic_parabola)
} 

ggkobe.plot= function(ABC.obj,fit_obj,METHOD,Management=F) {
  
  int.yr=ABC.obj[["input"]][["Input_parameters"]][["int.yr"]]
  temp_data=as.data.frame(ABC.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  
  nint.yr=match(int.yr,temp_data$yr)
  # if (all(is.na(BSM_2_rsq_products_a()))) {
  y.F_Fmsy = fit_obj$FFmsy[,nyr]
  x.b_bmsy = fit_obj$BBmsy[,nyr]
  
  F.Fmsy=as.numeric(temp_data$FFmsy)
  B.Bmsy=as.numeric(temp_data$BBmsy)
  
  #><>HW better performance if FFmsy = x for larger values
  kernelF <- gplots::ci2d(x.b_bmsy,y.F_Fmsy,nbins=201,factor=2.2,ci.levels=c(0.50,0.80,0.75,0.90,0.95),show="none")
  max.y    <-max(c(2,1.1*F.Fmsy),na.rm =T)
  max.x   <- max(c(2,1.1*B.Bmsy),na.rm =T)
  
  lx= expression(B/B[MSY])
  ly= expression(F/F[MSY])
  
  kern95=kernelF$contours$"0.95"
  kern95$x[kern95$x>max.x]=max.x
  kern95$y[kern95$y>max.y]=max.y
  kern95[kern95<0]=0
  kern80=kernelF$contours$"0.8"
  kern80$x[kern80$x>max.x]=max.x
  kern80$y[kern80$y>max.y]=max.y
  kern80[kern80<0]=0
  
  kern50=kernelF$contours$"0.5"
  kern50$x[kern50$x>max.x]=max.x
  kern50$y[kern50$y>max.y]=max.y
  kern50[kern50<0]=0
  
  Pr.green = paste0(round(sum(ifelse(x.b_bmsy>1 & y.F_Fmsy<1,1,0))/length(x.b_bmsy)*100,1),"%")
  Pr.red =  paste0(round(sum(ifelse(x.b_bmsy<1 & y.F_Fmsy>1,1,0))/length(x.b_bmsy)*100,1),"%")
  Pr.yellow =  paste0(round(sum(ifelse(x.b_bmsy<1 & y.F_Fmsy<1,1,0))/length(x.b_bmsy)*100,1),"%")
  Pr.orange =  paste0(round(sum(ifelse(x.b_bmsy>1 & y.F_Fmsy>1,1,0))/length(x.b_bmsy)*100,1),"%")
  
  labls= data.frame(x=as.character(c(Pr.yellow,Pr.green,Pr.red,Pr.orange,"95% C.I.","80% C.I.","50% C.I.")),
                    y=as.factor(c("yellow", "green", "red", "orange","slategray1", "slategray2", "slategray3")))
  rects=data.frame(colr=c("yellow", "green", "red", "orange"),xmin=c(0,1,0,1),
                   xmax=c(1,max.x,1,max.x),ymin=c(0,0,1,1),ymax=c(1,1,max.y,max.y))
  
  points=data.frame(shp=c(1,2,3),x=c(B.Bmsy[1],B.Bmsy[nint.yr],B.Bmsy[nyr]),
                    y=c(F.Fmsy[1],F.Fmsy[nint.yr],F.Fmsy[nyr]))
  
  
  if (Management==F) {
    my_y_title <-bquote(atop(Kobe~plot~"for"~bold(.(ABC.obj[["input"]][["Stock_info"]]$Stock))))
  } else {
    my_y_title="Kobe plot"
  }
  
  picKobe=ggplot2::ggplot()+
    ggplot2::scale_x_continuous(limits=c(0,max.x),breaks=seq(0,max.x,0.5)) + 
    ggplot2::theme_classic()+
    ggplot2::scale_y_continuous(limits=c(0,max.y),breaks=seq(0,max.y,0.5)) +
    ggplot2::labs(x=lx,y=ly,title = my_y_title)+
    ggplot2::geom_rect(data=rects,ggplot2::aes(xmin = xmin , xmax = xmax, ymin = ymin, ymax = ymax,fill =colr))  +
    ggplot2::geom_polygon(data=kern95,ggplot2::aes(x=x,y=y,fill=labls$x[5]), alpha = 0.65)+
    ggplot2::geom_polygon(data=kern80,ggplot2::aes(x=x,y=y,fill=labls$x[6]), alpha = 0.65)+
    ggplot2::geom_polygon(data=kern50,ggplot2::aes(x=x,y=y,fill=labls$x[7]), alpha = 0.65)+
    ggplot2::geom_point(ggplot2::aes(x=B.Bmsy,y=F.Fmsy),color="black",size=1.5)+
    ggplot2::geom_path(ggplot2::aes(x=B.Bmsy,y=F.Fmsy),color="black")+
    ggplot2::geom_point(data=points,ggplot2::aes(x=x,y=y,shape=factor(shp)),color="black",fill="white",size=2.7)+
    ggplot2::scale_shape_manual(name="",
                                labels  =c(as.character(ABC.obj[["input"]][["Input_parameters"]][["StartYear"]]),
                                           as.character(ABC.obj[["input"]][["Input_parameters"]][["int.yr"]]),
                                           as.character(ABC.obj[["input"]][["Input_parameters"]][["EndYear"]])),
                                values=c(22,21,24))+
    ggplot2::scale_fill_manual(name="",
                               labels  =c("50% C.I.","80% C.I.","95% C.I.",Pr.green,Pr.orange,Pr.red,Pr.yellow),
                               values=c(
                                 "slategray3", "slategray2","slategray1", "green","orange","red","yellow"))+
    theme(text = element_text(size = 10)) 
  
  if(Management==T) {
    
    picKobe=ggplot2::ggplot()+
      ggplot2::scale_x_continuous(limits=c(0,max.x),breaks=seq(0,max.x,0.5)) + 
      ggplot2::theme_classic()+
      ggplot2::scale_y_continuous(limits=c(0,max.y),breaks=seq(0,max.y,0.5)) +
      ggplot2::labs(x=lx,y=ly,title = my_y_title)+
      ggplot2::geom_rect(data=rects,ggplot2::aes(xmin = xmin , xmax = xmax, ymin = ymin, ymax = ymax),fill =rects$colr)  +
      ggplot2::geom_polygon(data=kern95,ggplot2::aes(x=x,y=y,fill=labls$x[5]), alpha = 0.65)+
      ggplot2::geom_polygon(data=kern80,ggplot2::aes(x=x,y=y,fill=labls$x[6]), alpha = 0.65)+
      ggplot2::geom_polygon(data=kern50,ggplot2::aes(x=x,y=y,fill=labls$x[7]), alpha = 0.65)+
      ggplot2::geom_point(ggplot2::aes(x=B.Bmsy,y=F.Fmsy),color="black",size=1.5)+
      ggplot2::geom_path(ggplot2::aes(x=B.Bmsy,y=F.Fmsy),color="black")+
      ggplot2::geom_point(data=points,ggplot2::aes(x=x,y=y,shape=factor(shp)),color="black",fill="white",size=2.7)+
      ggplot2::scale_shape_manual(name="",
                                  labels  =c(as.character(ABC.obj[["input"]][["Input_parameters"]][["StartYear"]]),
                                             as.character(ABC.obj[["input"]][["Input_parameters"]][["int.yr"]]),
                                             as.character(ABC.obj[["input"]][["Input_parameters"]][["EndYear"]])),
                                  values=c(22,21,24))+
      ggplot2::scale_fill_manual(name="",
                                 labels  =c("50% C.I.","80% C.I.","95% C.I."),
                                 values=c(
                                   "slategray3", "slategray2","slategray1"))+
      theme(text = element_text(size = 10)) #+ggplot2::theme(
    #   legend.position = "bottom")
  }
  return(picKobe)
}

ggprorposterior.plot= function(ABC.obj,fit_obj,rk.obj,METHOD) {
  mean.log.r=mean(log(c(as.numeric(ABC.obj[["input"]][["Input_parameters"]][["r.low"]]),as.numeric(ABC.obj[["input"]][["Input_parameters"]][["r.hi"]]))))
  sd.log.r=(log(as.numeric(ABC.obj[["input"]][["Input_parameters"]][["r.hi"]]))-log(as.numeric(ABC.obj[["input"]][["Input_parameters"]][["r.low"]])))/(2*1.96)  # assume range covers 4 SD
  mean.log.k= rk.obj[["mean.log.k"]]
  sd.log.k= rk.obj[["sd.log.k"]]
  rk <- exp(mvn(n=5000,mean.log.r=mean.log.r,sd.log.r=sd.log.r,
                mean.log.k=mean.log.k,sd.log.k=sd.log.k))
  pp.lab = "r"
  rpr = sort(rk[,1])
  post = fit_obj[["r"]]
  prior <-dlnorm(sort(rpr),meanlog = mean.log.r, sdlog = sd.log.r) #><>HW now pdf
  nmc = length(post)
  pdf = stats::density(post,adjust=2)
  A_post=data.frame(x=pdf$x,y=pdf$y)
  B_prior=data.frame(x= c((rpr),rev(prior)),y=c(prior,rep(0,length(sort(prior)))))
  PPVR = round((sd(post)/mean(post))^2/(sd(prior)/mean(prior))^2,2)
  
  pic_1<-  ggplot2::ggplot()+
    ggplot2::geom_polygon(data=A_post,ggplot2::aes(x=x,y=y,fill="Posterior"),col="black")+
    ggplot2::geom_polygon(data=B_prior,ggplot2::aes(x=x,y=y,fill="Prior"),col="black",alpha=0.5)+
    ggplot2::labs(x="r",fill=paste("PPVR =",PPVR),y="")+
    ggplot2::scale_fill_manual(values = c("red","blue"))+ggplot2::theme_classic()+
    ggplot2::theme(legend.position = c(.85,.8))+
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())+
    theme(text = element_text(size = 10)) #+
  
  rpr = sort(rk[,2])
  post = fit_obj[["k"]]
  prior <-dlnorm(sort(rpr),meanlog = mean.log.k, sdlog = sd.log.k) #><>HW now pdf
  # generic ><>HW streamlined GP to check
  nmc = length(post)
  pdf = stats::density(post,adjust=2)
  PPVR = round((sd(post)/mean(post))^2/(sd(prior)/mean(prior))^2,2)
  A_post=data.frame(x=pdf$x,y=pdf$y)
  B_prior=data.frame(x= c((rpr),rev(prior)),y=c(prior,rep(0,length(sort(prior)))))
  pic_2=ggplot2::ggplot()+
    ggplot2::geom_polygon(data=A_post,ggplot2::aes(x=x,y=y,fill="Posterior"),col="black")+
    ggplot2::geom_polygon(data=B_prior,ggplot2::aes(x=x,y=y,fill="Prior"),col="black",alpha=0.5)+
    ggplot2::labs(x="k",fill=paste("PPVR =",PPVR),y="")+
    ggplot2::scale_fill_manual(values = c("red","blue"))+ggplot2::theme_classic()+
    ggplot2::theme(legend.position = c(.85,.8))+
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())+
    theme(text = element_text(size = 10)) #+
  
  # MSY
  rpr = sort(as.numeric(rk[,1])*as.numeric(rk[,2])/4)
  post =  as.numeric(fit_obj[["r"]])* as.numeric(fit_obj[["k"]])/4
  prior <-dlnorm(sort(rpr),meanlog = mean(log(rpr)), sdlog = sd(log(rpr))) #><>HW now pdf
  prand <- rlnorm(2000,meanlog = mean(log(rpr)), sdlog = sd(log(rpr)))
  # generic ><>HW streamlined GP to check
  nmc = length(post)
  pdf = stats::density(post,adjust=2)
  PPVR = round((sd(post)/mean(post))^2/(sd(prand)/mean(prand))^2,2)
  A_post=data.frame(x=pdf$x,y=pdf$y)
  B_prior=data.frame(x= c((rpr),rev(prior)),y=c(prior,rep(0,length(sort(prior)))))
  
  pic_3=ggplot2::ggplot()+
    ggplot2::geom_polygon(data=A_post,ggplot2::aes(x=x,y=y,fill="Posterior"),col="black")+
    ggplot2::geom_polygon(data=B_prior,ggplot2::aes(x=x,y=y,fill="Prior"),col="black",alpha=0.5)+
    ggplot2::labs(x="MSY",fill=paste("PPVR =",PPVR),y="")+
    ggplot2::scale_fill_manual(values = c("red","blue"))+ggplot2::theme_classic()+
    ggplot2::theme(legend.position = c(.85,.8))+
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())+
    theme(text = element_text(size = 10)) 
  
  #><> bk beta priors
  int.yr= ABC.obj[["input"]][["Input_parameters"]][["int.yr"]]
  StartYear=ABC.obj[["input"]][["Input_parameters"]]$StartYear
  EndYear= ABC.obj[["input"]][["Input_parameters"]]$EndYear
  
  b.prior = rbind(matrix(c(ABC.obj[["input"]][["Input_parameters"]]$stb.low,ABC.obj[["input"]][["Input_parameters"]]$stb.hi,
                           ABC.obj[["input"]][["Input_parameters"]]$intb.low,ABC.obj[["input"]][["Input_parameters"]]$intb.hi,
                           ABC.obj[["input"]][["Input_parameters"]]$endb.low,ABC.obj[["input"]][["Input_parameters"]]$endb.hi),2,3),rep(0,3)) # last row includes the 0 penalty
  
  bk.beta = (beta.prior(b.prior))
  all.bk.CMSY  = fit_obj$P
  startbio=c(ABC.obj[["input"]][["Input_parameters"]]$stb.low,ABC.obj[["input"]][["Input_parameters"]]$stb.hi)
  
  # bk1
  post = all.bk.CMSY[,1]
  nmc = length(post)
  rpr = seq(0.5*as.numeric(startbio[1]),as.numeric(startbio[2])*1.5,0.005)
  pdf = stats::density(post,adjust=2)
  prand <- sort(rbeta(2000,bk.beta[1,1], bk.beta[2,1]))
  prior <-dbeta(sort(prand),bk.beta[1,1], bk.beta[2,1]) #><>HW now pdf
  
  A_post=data.frame(x=pdf$x,y=pdf$y)
  B_prior=data.frame(x= c(prand,rev(prand)),y=c(prior,rep(0,length(sort(prior)))))
  PPVR = round((sd(post)/mean(post))^2/(sd(prand)/mean(prand))^2,2)
  
  pic_4=ggplot2::ggplot()+
    ggplot2::geom_polygon(data=A_post,ggplot2::aes(x=x,y=y,fill="Posterior"),col="black")+
    ggplot2::geom_polygon(data=B_prior,ggplot2::aes(x=x,y=y,fill="Prior"),col="black",alpha=0.5)+
    ggplot2::labs(x=paste0("B/k ",StartYear),fill=paste("PPVR =",PPVR),y="")+
    ggplot2::scale_fill_manual(values = c("red","blue"))+ggplot2::theme_classic()+
    ggplot2::theme(legend.position = c(.85,.8))+
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())+
    theme(text = element_text(size = 10)) 
  
  # bk2
  intbio=c(ABC.obj[["input"]][["Input_parameters"]]$intb.low,ABC.obj[["input"]][["Input_parameters"]]$intb.hi)
  indx=(as.numeric(int.yr)-as.numeric(StartYear))+1
  post = all.bk.CMSY[,indx]
  rpr = seq(0.5*as.numeric(intbio[1]),as.numeric(intbio[2])*1.5,0.005)
  pdf = stats::density(post,adjust=2)
  prand <- sort(rbeta(2000,bk.beta[1,2], bk.beta[2,2]))
  prior <-dbeta(sort(prand),bk.beta[1,2], bk.beta[2,2]) #><>HW now pdf
  PPVR = round((sd(post)/mean(post))^2/(sd(prand)/mean(prand))^2,2)
  A_post=data.frame(x=pdf$x,y=pdf$y)
  B_prior=data.frame(x= c(prand,rev(prand)),y=c(prior,rep(0,length(sort(prior)))))
  
  pic_5=ggplot2::ggplot()+
    ggplot2::geom_polygon(data=A_post,ggplot2::aes(x=x,y=y,fill="Posterior"),col="black")+
    ggplot2::geom_polygon(data=B_prior,ggplot2::aes(x=x,y=y,fill="Prior"),col="black",alpha=0.5)+
    ggplot2::labs(x=paste0("B/k ",int.yr),fill=paste("PPVR =",PPVR),y="")+
    ggplot2::scale_fill_manual(values = c("red","blue"))+ggplot2::theme_classic()+
    ggplot2::theme(legend.position = c(.85,.8))+
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())+
    theme(text = element_text(size = 10)) 
  
  # bk3
  endbio=c(ABC.obj[["input"]][["Input_parameters"]]$endb.low,ABC.obj[["input"]][["Input_parameters"]]$endb.hi)
  indx=(as.numeric(EndYear)-as.numeric(StartYear))+1
  post = all.bk.CMSY[,indx]
  rpr = seq(0.5*as.numeric(endbio[1]),as.numeric(endbio[2])*1.5,0.005)
  pdf = stats::density(post,adjust=2)
  prand <- sort(rbeta(2000,bk.beta[1,3], bk.beta[2,3]))
  prior <-dbeta(sort(prand),bk.beta[1,3], bk.beta[2,3]) #><>HW now pdf
  PPVR = round((sd(post)/mean(post))^2/(sd(prand)/mean(prand))^2,2)
  A_post=data.frame(x=pdf$x,y=pdf$y)
  B_prior=data.frame(x= c(prand,rev(prand)),y=c(prior,rep(0,length(sort(prior)))))
  
  pic_6=ggplot2::ggplot()+
    ggplot2::geom_polygon(data=A_post,ggplot2::aes(x=x,y=y,fill="Posterior"),col="black")+
    ggplot2::geom_polygon(data=B_prior,ggplot2::aes(x=x,y=y,fill="Prior"),col="black",alpha=0.5)+
    ggplot2::labs(x=paste0("B/k ",EndYear),fill=paste("PPVR =",PPVR),y="")+
    ggplot2::scale_fill_manual(values = c("red","blue"))+ggplot2::theme_classic()+
    ggplot2::theme(legend.position = c(.85,.8))+
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())+
    theme(text = element_text(size = 10)) #+
  
  if (ABC.obj[["input"]][["Input_parameters"]][["nbk"]]=="All three (recommended)") {
    temp_F=ggpubr::ggarrange(pic_1,pic_2,pic_3,
                             pic_4,pic_5,pic_6,
                             labels=c("A","B","C","D","E","F"),
                             ncol = 3,nrow = 2)
  } else if (ABC.obj[["input"]][["Input_parameters"]][["nbk"]]=="Start & intermediate") {
    temp_F=ggpubr::ggarrange(pic_1,pic_2,pic_3,
                             pic_4,pic_5,
                             labels=c("A","B","C","D","E"),
                             ncol = 3,nrow = 2)
  } else {
    temp_F=ggpubr::ggarrange(pic_1,pic_2,pic_3,
                             pic_4,
                             labels=c("A","B","C","D"),
                             ncol = 3,nrow = 2)
  }
  return(temp_F)
}


gg_management.plot= function(ABC.obj,fit_obj,METHOD) {
  pic_1manag=ggcatch.plot(ABC.obj,METHOD,Management=T)
  pic_2manag=ggbk.plot(ABC.obj,METHOD,Management=T)
  pic_3manag=ggFFmsy.plot(ABC.obj,METHOD,Management=T)
  pic_4manag=ggkobe.plot(ABC.obj,fit_obj,METHOD,Management=T)
  
  temp_object=ggpubr::ggarrange(pic_1manag,pic_2manag,pic_3manag,pic_4manag,
                                labels=c("A","B","C","D"),
                                ncol = 2,nrow = 2)
  return(temp_object)
}

 
 
 gg_summary.plot= function(ABC.obj,rk.obj,fit_obj,METHOD) {
   pic_1summary=ggcatch.plot(ABC.obj,METHOD,Management=F)
   pic_2summary=ggrk.plot(ABC.obj,rk.obj,fit_obj,METHOD)+ggtitle("Finding viable r-k")
   pic_3summary=ggrk_2.plot(ABC.obj,rk.obj,fit_obj,METHOD)+ggtitle("Analysis of viable r-k")
   pic_4summary=ggbk.plot(ABC.obj,METHOD,Management=F)+ggtitle("Stock size")
   pic_5summary=ggFFmsy.plot(ABC.obj,METHOD,Management=F)+ggtitle("Exploitation rate")
   pic_6summary=ggparabola.plot(ABC.obj,METHOD)+ggtitle("Equilibrium curve")
   temp_object=ggpubr::ggarrange(pic_1summary,pic_2summary,pic_3summary,pic_4summary,pic_5summary,pic_6summary,
                                 labels=c("A","B","C","D","E","F"),
                                 ncol = 3,nrow = 2)
   return(temp_object)
 }

 
 


ggpdiagnostics.plot= function(ABC.obj,fit_obj,METHOD) {
  if (METHOD=="CMSY") {
    #  clr="blue"
    #my_y_title <-bquote(atop(Catch~"for"~the~stock~bold(.(ABC.obj[["input"]][["Stock_info"]]$Stock))~of~italic(.(ABC.obj[["input"]][["Stock_info"]]$ScientificName))))
    temp_data=as.data.frame(ABC.obj[["output"]][["output_timeseries"]])
    temp_data$yr=as.integer(row.names(temp_data))
    nyr=length(temp_data$yr)
    
    temp_data_in=as.data.frame(ABC.obj[["input"]][["Input_data"]])
    # temp_data_in$yr=as.integer(row.names(temp_data_in))
    
    pic_1=ggplot2::ggplot()+
      ggplot2::geom_segment(aes(y=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                yend=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                x = min(temp_data$yr), xend =  max( temp_data$yr) ,linetype="A",col="A"),size=0.7)+
      ggplot2::geom_line(data = temp_data,ggplot2::aes(x=yr,y=Catch,linetype="B",col="B"),size=0.7)+
      ggplot2::geom_point(data = temp_data_in,ggplot2::aes(x=yr,y=ct_smthd,shape="Observed",fill="D"),size=2,shape=21)+
      ggplot2::geom_ribbon(data = temp_data,  ggplot2::aes(x=yr,ymin=Catch_low, ymax=Catch_high),fill="darkslategray", alpha=0.3)+#y=ct.jags_low,
      ggplot2::theme_classic()+
      ggplot2::scale_linetype_manual(labels=c("MSY","Predicted and 95% CI"),values=c("dashed","solid"))+
      # ggplot2::scale_color_manual(labels=c("Catch and 95% CI"),values=c(clr))+
      ggplot2::scale_fill_manual(labels=c("Observed"),values=c("#F8766D"))+
      ggplot2::scale_color_manual(labels=c("MSY","Predicted and 95% CI"),values=c("black","black"))+
      
      ggplot2::labs(x="Year",y="Catch",fill="",shape="",linetype="",title="Catch fit",color="")+
      ggplot2::scale_y_continuous(limits=c(0,NA))+
      ggplot2::theme(legend.position = 'bottom',
                     legend.spacing.x = ggplot2::unit(0.2, 'cm'))+
      theme(text = element_text(size = 10)) 
    temp_object=pic_1
    
  } else if (METHOD=="BSM") {
    #  clr="blue"
    
    #my_y_title <-bquote(atop(Catch~"for"~the~stock~bold(.(ABC.obj[["input"]][["Stock_info"]]$Stock))~of~italic(.(ABC.obj[["input"]][["Stock_info"]]$ScientificName))))
    temp_data=as.data.frame(ABC.obj[["output"]][["output_timeseries"]])
    temp_data$yr=as.integer(row.names(temp_data))
    nyr=length(temp_data$yr)
    
    temp_data_in=as.data.frame(ABC.obj[["input"]][["Input_data"]])
    # temp_data_in$yr=as.integer(row.names(temp_data_in))
    
    pic_1=ggplot2::ggplot()+
      ggplot2::geom_segment(aes(y=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                yend=ABC.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                x = min(temp_data$yr), xend =  max( temp_data$yr) ,linetype="A",col="A"),size=0.7)+
      ggplot2::geom_line(data = temp_data,ggplot2::aes(x=yr,y=Catch,linetype="B",col="B"),size=0.7)+
      ggplot2::geom_point(data = temp_data_in,ggplot2::aes(x=yr,y=ct_smthd,shape="Observed",fill="D"),size=2,shape=21)+
      ggplot2::geom_ribbon(data = temp_data,  ggplot2::aes(x=yr,ymin=Catch_low, ymax=Catch_high),fill="darkslategray", alpha=0.3)+#y=ct.jags_low,
      ggplot2::theme_classic()+
      ggplot2::scale_linetype_manual(labels=c("MSY","Predicted and 95% CI"),values=c("dashed","solid"))+
      # ggplot2::scale_color_manual(labels=c("Catch and 95% CI"),values=c(clr))+
      ggplot2::scale_fill_manual(labels=c("Observed"),values=c("#F8766D"))+
      ggplot2::scale_color_manual(labels=c("MSY","Predicted and 95% CI"),values=c("black","black"))+
      
      ggplot2::labs(x="Year",y="Catch",fill="",shape="",linetype="",title="Catch fit",color="")+
      ggplot2::scale_y_continuous(limits=c(0,NA))+
      ggplot2::theme(legend.position = 'bottom',
                     legend.spacing.x = ggplot2::unit(0.2, 'cm'))+
      theme(text = element_text(size = 10)) 
     pred.cpue = apply(exp(fit_obj$cpuem),2,quantile,c(0.5,0.025,0.975),na.rm=T)
    temp_data_pred.cpue=data.frame(
      yr= temp_data$yr,
      bt_smthd=temp_data_in$bt_smthd,
      pred.cpue= pred.cpue[1,],
      lcl.pred.cpue= pred.cpue[2,],
      ucl.pred.cpue= pred.cpue[3,])
    
    pic_2= ggplot2::ggplot(data = temp_data_pred.cpue, ggplot2::aes(x=yr))+
      ggplot2::geom_line(ggplot2::aes(y=pred.cpue,linetype="Predicted"),size=1)+
      ggplot2::geom_point(ggplot2::aes(y=bt_smthd,shape="Observed"),size=2,fill="#F8766D")+
      ggplot2::geom_ribbon(ggplot2::aes(ymin=lcl.pred.cpue, ymax=ucl.pred.cpue,fill="95% CI"), alpha=0.2)+
      ggplot2::scale_y_continuous(limits = c(0,max(temp_data_pred.cpue$ucl.pred.cpue)))+
      ggplot2::theme_classic()+
      ggplot2::labs(x="Year",y="CPUE",fill="",shape="",linetype="",title="CPUE fit")+
      ggplot2::scale_linetype_manual(values=c("solid"))+
      ggplot2::scale_shape_manual(values=c(21))+
      ggplot2::scale_fill_manual(values=c("darkslategray"))+
      ggplot2::theme(legend.position = 'bottom',
                     legend.spacing.x = ggplot2::unit(0.2, 'cm'))+
      theme(text = element_text(size = 10)) 
    
    pred.pe = apply(fit_obj$proc.logB,2,quantile,c(0.5,0.025,0.975),na.rm=T)
    
    temp_data_pred.pe=data.frame(
      yr= temp_data$yr,
      pred.pe= pred.pe[1,],
      lcl.pred.pe= pred.pe[2,],
      ucl.pred.pe= pred.pe[3,])
    floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
    ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
    
    pic_3= ggplot2::ggplot(data = temp_data_pred.pe, ggplot2::aes(x=yr))+
      ggplot2::geom_line(ggplot2::aes(y=pred.pe),size=1)+
      ggplot2::geom_ribbon(ggplot2::aes(ymin=lcl.pred.pe, ymax=ucl.pred.pe),fill="darkslategray", alpha=0.2)+
      ggplot2::geom_hline(yintercept=0, linetype="dashed", size=1)+
      ggplot2::scale_y_continuous(limits = c(floor_dec(min(temp_data_pred.pe$lcl.pred.pe),1),
                                             ceiling_dec(max(temp_data_pred.pe$ucl.pred.pe),1)))+ggplot2::theme_classic()+
      ggplot2::labs(x="Year",y="Deviation log(B)",title="Process variation")+
      theme(text = element_text(size = 10)) 
    
    
    # get residuals
    resid_1 = (log(temp_data_in$bt_smthd)-log(pred.cpue[1,]))[is.na(temp_data_in$bt_smthd)==F]
    res.yr = temp_data_in$yr[is.na(temp_data_in$bt_smthd)==F]
    runstest = runs.sig3(resid_1)
    RMSE = sqrt(mean(resid_1^2))
    minyr=min(temp_data_in$yr,na.rm = T)
    maxyr=max(temp_data_in$yr,na.rm = T)
    if(RMSE>0.1){lims = runstest$sig3lim} else {lims=c(-1,1)}
    cols = c(rgb(1,0,0,0.5),rgb(0,1,0,0.5))[ifelse(runstest$p.runs<0.05,1,2)]
    
    tempresid=data.frame(yr=temp_data_in$yr[!is.na(temp_data_in$bt_smthd)],xend=0,resid=resid_1)
    pic_4=ggplot2::ggplot(tempresid,ggplot2::aes(x=yr,y = resid)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = min(temp_data_in$yr,na.rm=T),
                                      xmax = max(temp_data_in$yr,na.rm=T),ymin = lims[1],ymax = lims[2]),
                         fill=cols,colour = cols,alpha=0.2)+
      ggplot2::geom_hline(yintercept=0, linetype="dashed", size=1)+ggplot2::theme_classic()+
      ggplot2::geom_segment(ggplot2::aes(xend=yr,y = xend, yend =resid )) +
      ggplot2::geom_point(size=2,shape=21,fill="white")+
      ggplot2::labs(x="Year",y=expression(log(cpue[obs])-log(cpue[pred])),title="Residual diagnostics")+
      theme(text = element_text(size = 10)) 
    
    temp_object=ggpubr::ggarrange(pic_1,pic_2,pic_3,pic_4,
                                  labels=c("A","B","C","D"),
                                  ncol = 2,nrow = 2)
  }
  return(temp_object)
}


retro_fit= function(ABC.obj,rk.obj,q.obj,ret_steps,METHOD) {
  q_prrs=q.obj[1:2]
  init.q_=q.obj[3]
  
  retro_years=vector()
  retro.obj_data=list()
  for (i in 1:ret_steps) {
    retro_years[i]=ABC.obj[["input"]][["Input_parameters"]][["EndYear"]]-i
    retro.obj_data[[i]]=ABC.obj[["input"]][["Input_data"]][ABC.obj[["input"]][["Input_data"]]$yr<= retro_years[i],]
  }
  
  ####nbk
  if (ABC.obj[["input"]][["Input_parameters"]]$nbk=="Only start")  {
    nbk=1 } else if (ABC.obj[["input"]][["Input_parameters"]]$nbk=="Start & intermediate") {
      nbk=2  } else {
        nbk=3
      }
  
  xx=data.frame(year=c(ABC.obj[["input"]][["Input_parameters"]][["StartYear"]],
                       ABC.obj[["input"]][["Input_parameters"]][["int.yr"]],
                       ABC.obj[["input"]][["Input_parameters"]][["EndYear"]]),
                low=c(as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.low),
                      as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.low),
                      as.numeric(ABC.obj[["input"]][["Input_parameters"]]$endb.low)),
                high=c(as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.hi),
                       as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.hi),
                       as.numeric(ABC.obj[["input"]][["Input_parameters"]]$endb.hi))
  )
  xy=data.frame(year=seq(ABC.obj[["input"]][["Input_parameters"]][["StartYear"]],ABC.obj[["input"]][["Input_parameters"]][["EndYear"]],1))
  xy_=merge(x=xy,y=xx, all.x = TRUE)
  
  
  xy_$low=imputeTS::na_interpolation(xy_$low, option = "linear")
  xy_$high=imputeTS::na_interpolation(xy_$high, option = "linear")
  
  ggplot()+geom_line(data=xy_,aes(x=year,y=low),linetype="dashed")+
    geom_line(data=xy_,aes(x=year,y=high),linetype="dashed")+
    geom_segment(data=xy_,aes(x=1950 ,xend =1950 ,y=low[1],yend =high[1]),size=2)+
    geom_segment(data=xy_,aes(x=1990  ,xend =1990  ,y=low[41],yend =high[41]),size=2)+
    geom_segment(data=xy_,aes(x=1995  ,xend =1995  ,y=low[46],yend =high[46]),size=2)
  
  ###########################################
  b.prior_=list()
  
  as.numeric(xy_$low[xy_$year==(ABC.obj[["input"]][["Input_parameters"]]$int.yr-1)])
  
  nbks_=list()
  for (i in 1:ret_steps) {
    if (nbk==3 & max(retro.obj_data[[i]]$yr)>ABC.obj[["input"]][["Input_parameters"]]$int.yr ) {
      nbks_[[i]]=3
      b.prior_[[i]] = rbind(matrix(c(as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.hi),
                                     as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.hi),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)])),2,3),rep(0,3)) # last row includes the 0 penalty
      
    } else if (max(retro.obj_data[[i]]$yr)<=ABC.obj[["input"]][["Input_parameters"]]$int.yr ) {
      nbks_[[i]]=nbk
      b.prior_[[i]] = rbind(matrix(c(as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.hi),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)]),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)])),2,3),rep(0,3)) # last row includes the 0 penalty
    } else {
      nbks_[[i]]=nbk
      b.prior_[[i]] = rbind(matrix(c(as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.hi),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)]),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)])),2,3),rep(0,3)) # last row includes the 0 penalty
      
    }
  }
  
  pen.bk = pen.F = rep(0,length(ABC.obj[["input"]][["Input_data"]]$ct_smthd))
  
  b.yrs_=list()
  for (i in 1:ret_steps) {
    
    if (nbks_[[i]]==3) {
      
      b.yrs_[[i]] = c(1,length(as.integer(ABC.obj[["input"]][["Input_parameters"]]$StartYear):
                                 as.integer(ABC.obj[["input"]][["Input_parameters"]]$int.yr)),
                      length(as.integer(ABC.obj[["input"]][["Input_parameters"]]$StartYear):
                               as.integer(max(retro.obj_data[[i]]$yr))))
    } else {
      
      b.yrs_[[i]] = c(1,length(as.integer(ABC.obj[["input"]][["Input_parameters"]]$StartYear):
                                 as.integer(max(retro.obj_data[[i]]$yr))),
                      length(as.integer(ABC.obj[["input"]][["Input_parameters"]]$StartYear):
                               as.integer(max(retro.obj_data[[i]]$yr))))
      
    }}
  
  btj_=list()
  for (i in 1:ret_steps) {
    if (METHOD == "CMSY") { 
      bt.start <- mean(c(rk.obj[["rkpriors"]]$prior.k.low*
                           as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.low),
                         rk.obj[["rkpriors"]]$prior.k.hi*
                           as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.hi))) # derive proxy for first bt value
      
      btj_[[i]]<- c(bt.start,rep(NA,length(retro.obj_data[[i]]$ct_smthd)-1)) }else if (METHOD == "BSM") { 
        btj_[[i]]=retro.obj_data[[i]]$bt_smthd
      }
  }
  
  CV.cpue_=list()
  for (i in 1:ret_steps) {
    
    if (METHOD == "CMSY") { 
      CV.cpue_[[i]]=rep(0.2,length(retro.obj_data[[i]]$ct_smthd))  }else if (METHOD == "BSM") { 
        
        if (all(is.na(retro.obj_data[[i]]$bt_cv)) &  !is.na(ABC.obj[["input"]][["Input_parameters"]]$CV_cpue)) {
          CV.cpue_[[i]]      <- rep(ABC.obj[["input"]][["Input_parameters"]]$CV_cpue,length(retro.obj_data[[i]]$ct_smthd))
        } else if  (all(is.na(ABC.obj[["input"]][["Input_data"]]$bt_cv))) {
          CV.cpue_[[i]]      <- rep(0.2,length(retro.obj_data[[i]]$ct_smthd)) } else {
            CV.cpue_[[i]]<- retro.obj_data[[i]]$bt_cv
          }
        
        CV.cpue_[[i]][is.na(CV.cpue_[[i]])]=0.2
      }
  }
  
  retro_jags_outputs=list()
  for (j in 1:ret_steps) {
    retro_jags_outputs[[j]] <- bsm(ct=as.numeric(retro.obj_data[[j]]$ct_smthd),
                                   btj <-btj_[[j]],
                                   nyr=length(retro.obj_data[[j]]$ct_smthd),
                                   prior.r=as.numeric(c(ABC.obj[["input"]][["Input_parameters"]]$r.low,ABC.obj[["input"]][["Input_parameters"]]$r.hi)),
                                   prior.k=as.numeric(c(rk.obj[["rkpriors"]]$prior.k.low, rk.obj[["rkpriors"]]$prior.k.hi)),
                                   startbio=as.numeric(c(ABC.obj[["input"]][["Input_parameters"]]$stb.low,ABC.obj[["input"]][["Input_parameters"]]$stb.hi)),
                                   q.priorj=as.numeric(q_prrs),#c(0.99,1.01),# # since no abundance data are available in this run,
                                   init.q=init.q_ ,           # q could be omitted and is set here to (practically) 1  ,
                                   init.r=rk.obj[["rkpriors"]]$init.r,
                                   init.k=rk.obj[["rkpriors"]]$init.k,
                                   pen.bk=pen.bk,
                                   pen.F=pen.F,
                                   b.yrs=b.yrs_[[j]],
                                   b.prior= b.prior_[[j]],
                                   CV.C=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$CV_catch),# 0.15,  #><>MSY: Add Catch CV
                                   CV.cpue=CV.cpue_[[j]],#input$CPUE_CV,  #><>MSY: Add minimum realistic cpue CV
                                   nbk=nbks_[[j]],
                                   rk.cor=rk.obj[["rkpriors"]]$rk.cor,
                                   n.chains     <- 2, # number of chains to be used in JAGS, default = 2
                                   cmsyjags=F)$BUGSoutput$sims.list
    retro_jags_outputs[[j]][["r"]]=as.numeric(coda::mcmc(retro_jags_outputs[[j]][["r"]]))
    retro_jags_outputs[[j]][["k"]]=as.numeric(coda::mcmc(retro_jags_outputs[[j]][["k"]]))
    retro_jags_outputs[[j]][["q"]]=as.numeric(coda::mcmc(retro_jags_outputs[[j]][["q"]]))
    retro_jags_outputs[[j]][["ppd.r"]]<- exp(as.numeric(coda::mcmc(retro_jags_outputs[[j]][["ppd.logrk"]][,1])))
    retro_jags_outputs[[j]][["ppd.k"]]<- exp(as.numeric(coda::mcmc(retro_jags_outputs[[j]][["ppd.logrk"]][,2])))
  }
  
  retro_outcomes=list()
  for (i in 1:ret_steps) {
    
    retro_outcomes[[i]]=ABC.obj[["output"]][["output_timeseries"]]
    retro_outcomes[[i]]=retro_outcomes[[i]][1:(nrow(retro_outcomes[[i]])-i),]
    retro_outcomes[[i]][, c("Catch","Catch_low","Catch_high")]=t(apply( retro_jags_outputs[[i]]$ct.jags,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("FFmsy","FFmsy_low","FFmsy_high")]=t(apply( retro_jags_outputs[[i]]$FFmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("f","f_low","f_high")]=t(apply( retro_jags_outputs[[i]]$F,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("BBmsy","BBmsy_low","BBmsy_high")]=t(apply( retro_jags_outputs[[i]]$BBmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("B","B_low","B_high")]= t(apply( retro_jags_outputs[[i]]$B,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("bk","bk_low","bk_high")]=  t(apply( retro_jags_outputs[[i]]$P,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]]$ID=max(as.integer(row.names(retro_outcomes[[i]])))
  }
  ABC.obj[["retro_output"]]=retro_outcomes
  
  return(ABC.obj)
}

retro_fit2= function(ABC.obj,rk.obj,q.obj,ret_steps,METHOD) {
  q_prrs=q.obj[1:2]
  init.q_=q.obj[3]
  
  retro_years=vector()
  retro.obj_data=list()
  for (i in 1:ret_steps) {
    retro_years[i]=ABC.obj[["input"]][["Input_parameters"]][["EndYear"]]-i
    retro.obj_data[[i]]=ABC.obj[["input"]][["Input_data"]][ABC.obj[["input"]][["Input_data"]]$yr<= retro_years[i],]
  }
  
  nbk_=list()
  for (i in 1:ret_steps) {
    if (ABC.obj[["input"]][["Input_parameters"]]$nbk=="Only start")  {
      nbk_[[i]]=1 } else if (max(retro.obj_data[[i]]$yr)>ABC.obj[["input"]][["Input_parameters"]]$int.yr   ) {
        nbk_[[i]]=2} else {
          nbk_[[i]]=1
        }}
  
  pen.bk = pen.F = rep(0,length(ABC.obj[["input"]][["Input_data"]]$ct_smthd))
  
  
  b.yrs = c(1,length(as.integer(ABC.obj[["input"]][["Input_parameters"]]$StartYear):
                       as.integer(ABC.obj[["input"]][["Input_parameters"]]$int.yr)),
            length(as.integer(ABC.obj[["input"]][["Input_parameters"]]$StartYear):
                     as.integer(ABC.obj[["input"]][["Input_parameters"]]$EndYear)))
  
  b.prior_ = rbind(matrix(c(as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.hi),
                            as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$intb.hi),
                            as.numeric(ABC.obj[["input"]][["Input_parameters"]]$endb.low),as.numeric(ABC.obj[["input"]][["Input_parameters"]]$endb.hi)),2,3),rep(0,3)) # last row includes the 0 penalty
  
  btj_=list()
  for (i in 1:ret_steps) {
    if (METHOD == "CMSY") { 
      bt.start <- mean(c(rk.obj[["rkpriors"]]$prior.k.low*
                           as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.low),
                         rk.obj[["rkpriors"]]$prior.k.hi*
                           as.numeric(ABC.obj[["input"]][["Input_parameters"]]$stb.hi))) # derive proxy for first bt value
      
      btj_[[i]]<- c(bt.start,rep(NA,length(retro.obj_data[[i]]$ct_smthd)-1)) }else if (METHOD == "BSM") { 
        btj_[[i]]=retro.obj_data[[i]]$bt_smthd
      }
  }
  
  CV.cpue_=list()
  for (i in 1:ret_steps) {
    
    if (METHOD == "CMSY") { 
      CV.cpue_[[i]]=rep(0.2,length(retro.obj_data[[i]]$ct_smthd))  }else if (METHOD == "BSM") { 
        
        if (all(is.na(retro.obj_data[[i]]$bt_cv)) &  !is.na(ABC.obj[["input"]][["Input_parameters"]]$CV_cpue)) {
          CV.cpue_[[i]]      <- rep(ABC.obj[["input"]][["Input_parameters"]]$CV_cpue,length(retro.obj_data[[i]]$ct_smthd))
        } else if  (all(is.na(ABC.obj[["input"]][["Input_data"]]$bt_cv))) {
          CV.cpue_[[i]]      <- rep(0.2,length(retro.obj_data[[i]]$ct_smthd)) } else {
            CV.cpue_[[i]]<- retro.obj_data[[i]]$bt_cv
          }
        
        CV.cpue_[[i]][is.na(CV.cpue_[[i]])]=0.2
      }
  }
  #CV.cpue_[is.na(CV.cpue_)]=0.2
  retro_jags_outputs=list()
  for (i in 1:ret_steps) {
    retro_jags_outputs[[i]] <- bsm(ct=as.numeric(retro.obj_data[[i]]$ct_smthd),
                                   btj <-btj_[[i]],
                                   nyr=length(retro.obj_data[[i]]$ct_smthd),
                                   prior.r=as.numeric(c(ABC.obj[["input"]][["Input_parameters"]]$r.low,ABC.obj[["input"]][["Input_parameters"]]$r.hi)),
                                   prior.k=as.numeric(c(rk.obj[["rkpriors"]]$prior.k.low, rk.obj[["rkpriors"]]$prior.k.hi)),
                                   startbio=as.numeric(c(ABC.obj[["input"]][["Input_parameters"]]$stb.low,ABC.obj[["input"]][["Input_parameters"]]$stb.hi)),
                                   q.priorj=as.numeric(q_prrs),#c(0.99,1.01),# # since no abundance data are available in this run,
                                   init.q=init.q_ ,           # q could be omitted and is set here to (practically) 1  ,
                                   init.r=rk.obj[["rkpriors"]]$init.r,
                                   init.k=rk.obj[["rkpriors"]]$init.k,
                                   pen.bk=pen.bk,
                                   pen.F=pen.F,
                                   b.yrs=b.yrs,
                                   b.prior=b.prior_,
                                   CV.C=as.numeric(ABC.obj[["input"]][["Input_parameters"]]$CV_catch),# 0.15,  #><>MSY: Add Catch CV
                                   CV.cpue=CV.cpue_[[i]],#input$CPUE_CV,  #><>MSY: Add minimum realistic cpue CV
                                   nbk=nbk_[[i]],
                                   rk.cor=rk.obj[["rkpriors"]]$rk.cor,
                                   n.chains     <- 2, # number of chains to be used in JAGS, default = 2
                                   cmsyjags=F)$BUGSoutput$sims.list
    retro_jags_outputs[[i]][["r"]]=as.numeric(coda::mcmc(retro_jags_outputs[[i]][["r"]]))
    retro_jags_outputs[[i]][["k"]]=as.numeric(coda::mcmc(retro_jags_outputs[[i]][["k"]]))
    retro_jags_outputs[[i]][["q"]]=as.numeric(coda::mcmc(retro_jags_outputs[[i]][["q"]]))
    retro_jags_outputs[[i]][["ppd.r"]]<- exp(as.numeric(coda::mcmc(retro_jags_outputs[[i]][["ppd.logrk"]][,1])))
    retro_jags_outputs[[i]][["ppd.k"]]<- exp(as.numeric(coda::mcmc(retro_jags_outputs[[i]][["ppd.logrk"]][,2])))
  }
  
  
  retro_outcomes=list()
  for (i in 1:ret_steps) {
    
    retro_outcomes[[i]]=ABC.obj[["output"]][["output_timeseries"]]
    retro_outcomes[[i]]=retro_outcomes[[i]][1:(nrow(retro_outcomes[[i]])-i),]
    retro_outcomes[[i]][, c("Catch","Catch_low","Catch_high")]=t(apply( retro_jags_outputs[[i]]$ct.jags,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("FFmsy","FFmsy_low","FFmsy_high")]=t(apply( retro_jags_outputs[[i]]$FFmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("f","f_low","f_high")]=t(apply( retro_jags_outputs[[i]]$F,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("BBmsy","BBmsy_low","BBmsy_high")]=t(apply( retro_jags_outputs[[i]]$BBmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("B","B_low","B_high")]= t(apply( retro_jags_outputs[[i]]$B,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("bk","bk_low","bk_high")]=  t(apply( retro_jags_outputs[[i]]$P,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]]$ID=max(as.integer(row.names(retro_outcomes[[i]])))
  }
  ABC.obj[["retro_output"]]=retro_outcomes
  
  return(ABC.obj)
}


ggplot.retro=function(ABC.object) {
  retros=ABC.object[["retro_output"]]
  for (i in 1:length(retros)) {
    retros[[i]]$yr=as.integer(row.names(retros[[i]]))
  }
  retros=rbindlist(retros)
  
  ft_mod= ABC.object[["output"]][["output_timeseries"]]
  ft_mod$ID=max(as.integer(rownames(ft_mod)))
  ft_mod$yr=as.integer(row.names(ft_mod))
  retros=rbind(retros,ft_mod)
  max.y_ffmsy <- max(c(1.2,1.1*retros$FFmsy),na.rm=T)
  max.y_bbmsy <- max(c(1.2, 1.1*retros$BBmsy ),na.rm=T)
  
  my_y_title1 <-bquote(atop("F/Fmsy Retrospective"~"for"~the~stock~bold(.(ABC.object[["input"]][["Stock_info"]]$Stock))~of~italic(.(ABC.object[["input"]][["Stock_info"]]$ScientificName))))
  my_y_title2 <-bquote(atop("B/Bmsy Retrospective"~"for"~the~stock~bold(.(ABC.object[["input"]][["Stock_info"]]$Stock))~of~italic(.(ABC.object[["input"]][["Stock_info"]]$ScientificName))))
  
  picffmsy=  ggplot2::ggplot()+
    ggplot2::geom_line(data=retros, ggplot2::aes(x=yr,y=FFmsy,col=as.factor(ID)),size=1)+
    ggplot2::geom_point(data=retros, ggplot2::aes(x=yr,y=FFmsy,col=as.factor(ID),shape=as.factor(ID)),size=2)+
    ggplot2::scale_y_continuous(limits=c(0,max.y_ffmsy))+
    
    ggplot2::geom_hline(ggplot2::aes(yintercept=1),linetype="dashed",size=1)+
    ggplot2::theme_classic()+
    ggplot2::labs(y="F/Fmsy",x="Year",fill="",color="",linetype="",shape="",title=my_y_title1)+
    ggplot2::theme(legend.position = "bottom")
  
  picbbmsy=  ggplot2::ggplot()+
    ggplot2::geom_line(data=retros, ggplot2::aes(x=yr,y=BBmsy,col=as.factor(ID)),size=1)+
    ggplot2::geom_point(data=retros, ggplot2::aes(x=yr,y=BBmsy,col=as.factor(ID),shape=as.factor(ID)),size=2)+
    ggplot2::scale_y_continuous(limits=c(0,max.y_bbmsy))+
    #ggplot2::scale_x_continuous(breaks =seq(yr_[1],yr_[nyr],3))+
    # ggplot2::scale_color_manual(values=c("blue"),labels=c("F/Fmsy"))+
    # ggplot2::scale_fill_manual(values=c("blue"),labels=c("F/Fmsy"))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=1),linetype="dashed",size=1)+
    ggplot2::theme_classic()+
    ggplot2::labs(y="B/Bmsy",x="Year",fill="",color="",linetype="",shape="",title=my_y_title2)+
    ggplot2::theme(legend.position = "bottom")#+
  # ggplot2::scale_color_manual(values=c("green"),labels=c(0, unique(bbmsy_retro$ID) ))
  
  pic_all=ggpubr::ggarrange(picffmsy,picbbmsy,
                            labels=c("A","B"),
                            ncol = 2,nrow = 1)
  return(pic_all)
}


ABC.forward=function(ABC_fit,ABC_res,nyears=5,status.quo_years=1,interim.quant = c("Catch", "F")[2],quant = c("Catch", "F")[2],Manual_Scenarios=NULL){
  year = seq(ABC_res[["input"]][["Input_parameters"]][["StartYear"]],
             ABC_res[["input"]][["Input_parameters"]][["EndYear"]],1)
  initial=NULL
  ######## Extract r and k 
  rk=data.frame(K=ABC_fit[["k"]],r=ABC_fit[["r"]])
  ######## Extract B and k 
  B=as.data.frame(ABC_fit[["B"]])
  colnames(B)=year
  B_=gather(B,key=year,value = B)
  data.frame(H=c(ABC_fit[["F"]]),Catch=c(ABC_fit[["ct.jags"]]),P=c(ABC_fit[["P"]]))
  trj=cbind(B_,data.frame(H=c(ABC_fit[["F"]]),Catch=c(ABC_fit[["ct.jags"]]),P=c(ABC_fit[["P"]]),FFmsy=c(ABC_fit[["FFmsy"]])))
  
  if (is.null(Manual_Scenarios)){
    Scenarios1=c("Status_quo","Stop_Fishing","25%_Fmsy","50%_Fmsy","80%_Fmsy","Fmsy")
    Scenarios2=c("Status_quo","Stop_Fishing","25%_Status_quo","50%_Status_quo","80%_Status_quo")
    
    #Fxs_=c(0,"Current_F",0.2,0.4,0.6,0.8,1,1.2,"Current_Catch")
    Fxs1=c(0,0.25,0.5,0.8,1)
    Fxs2=c(0,0.25,0.5,0.8)
    
  } else if (is.numeric(Manual_Scenarios)) {
    Fxs1=Manual_Scenarios
    Fxs2=Manual_Scenarios
    Manual_Scenarios_=Manual_Scenarios*100
    Scenarios1=paste0(Manual_Scenarios_,"%_Fmsy")
    Scenarios2=paste0(Manual_Scenarios_,"%_Status_quo")
    Scenarios1[Scenarios1=="0%_Fmsy"]="Stop_Fishing"
    Scenarios1[Scenarios1=="100%_Fmsy"]="Fmsy"
    Scenarios2[Scenarios2=="0%_Status_quo"]="Stop_Fishing"
    #Scenarios2[Scenarios1=="100%_Status_quo"]="msy"
    Scenarios1=c("Status_quo",Scenarios1)
    Scenarios2=c("Status_quo",Scenarios2)
  } #else {
  #   stop("Provide a vector with numeric values for F/Fmsy or Catch/msy depending on the 'quant' choise")
  # }
  # Y = ABC_res[["output"]][["output_posteriors"]][["r_post"]][1] * tail(ABC_res[["output"]][["output_timeseries"]][["B"]],1) * (1 - tail(ABC_res[["output"]][["output_timeseries"]][["B"]],1) /ABC_res[["output"]][["output_posteriors"]][["k_post"]][1])
  ##### status quo
  year = seq(ABC_res[["input"]][["Input_parameters"]][["StartYear"]],ABC_res[["input"]][["Input_parameters"]][["EndYear"]],1)
  yrend = max(year)
  n = length(year)
  
  if (quant == "Catch") {
    status.quo = mean(ABC_res[["input"]][["Input_data"]][["ct"]][(n - (status.quo_years-1)):n])
    if (is.null(initial)) {
      initial = status.quo
    }
  }
  if (quant == "F") {
    status.quo =   mean(ABC_res[["output"]][["output_timeseries"]][["f"]][(n - (status.quo_years-1)):n])
    if (is.null(initial)) {
      initial = status.quo
    }
  }
  if (interim.quant == "Catch") {
    #  interim_var = mean(ABC_res[["input"]][["Input_data"]][["ct"]][(n - (status.quo_years-1)):n])
    interim_var = mean(ABC_res[["output"]][["output_timeseries"]][["Catch"]][(n - (status.quo_years-1)):n])
  }
  if (interim.quant == "F") {
    interim_var =   mean(ABC_res[["output"]][["output_timeseries"]][["f"]][(n - (status.quo_years-1)):n])
  }
  
  fmsy=ifelse(unique(ABC_res[["output"]][["output_posteriors"]][["Fmsy_post_correction_note"]])=="Fmsy was corrected downward to account for reduced recruitment.",
              ABC_res[["output"]][["output_posteriors"]][["Fmsy_post_corrected"]][1],
              ABC_res[["output"]][["output_posteriors"]][["Fmsy_post"]][1])
  
  #r_=ABC_res[["output"]][["output_posteriors"]][["r_post"]][1]
  #bmsy=ABC_res[["output"]][["output_posteriors"]][["Bmsy_post"]][1]
  #msy=fmsy*bmsy
  
  
  if (quant == "F") {
    vals = c(initial, Fxs1 * fmsy)
    Scenaria=data.frame(Scenario=Scenarios1,F_vals=vals)}
  if (quant == "Catch") {
    vals = c(status.quo, Fxs2 *status.quo)
    Scenaria=data.frame(Scenario=Scenarios2,F_vals=vals)}
  
  ######## Projection data.frames
  projection_years=seq(yrend,yrend+nyears,1)
  Last_year = data.frame(trj[trj$year == yrend, ])
  H =B=P=C=FFmsy= matrix(NA, nrow = nrow(Last_year), ncol = length(projection_years))
  H[,1]=Last_year$H
  B[,1]=Last_year$B
  C[,1]= Last_year$Catch
  P[,1]= Last_year$P
  FFmsy[,1]=Last_year$FFmsy
  
  if (interim.quant == "F") {
    H[,2]= interim_var
  }
  if (interim.quant == "Catch") {
    C[,2]= interim_var}
  
  H=as.data.frame(H)
  #colnames(H)=projection_years
  B=as.data.frame(B)
  #colnames(B)=projection_years
  C=as.data.frame(C)
  #colnames(C)=projection_years
  P=as.data.frame(P)
  FFmsy=as.data.frame(FFmsy)
  
  colnames(P)=colnames(H)=colnames(B)= colnames(C)= colnames(FFmsy)=projection_years
  Scen_list=list()
  for (j in 1:nrow(Scenaria)) {
    H_=H
    FFmsy_=FFmsy
    
    if (quant == "F") {
      H_[, 3:ncol(H_)] =Scenaria[j,2]
    }
    C_=C
    
    if (quant == "Catch") {
      C_[, 3:ncol(C_)] = Scenaria[j,2]}
    P_=P
    B_=B
    for (i in 2:length(projection_years)) {
      # P_[, i] <-ifelse(median(P_[, i - 1])>0.25, (P_[, i - 1] + rk$r * P_[, i - 1] * (1 - P_[, i - 1]) - C_[, i - 1]/rk$K),
      #                  (P_[, i - 1] + rk$r * 4* P_[, i - 1]^2 * (1 - P_[, i - 1]) - C_[, i - 1]/rk$K))
      
      if (median(P_[, i - 1])>0.25) {
        P_[, i] <- P_[, i - 1] + rk$r * P_[, i - 1] * (1 - P_[, i - 1]) - C_[, i - 1]/rk$K} else {
          P_[, i] <-  P_[, i - 1] + rk$r * 4* P_[, i - 1]^2 * (1 - P_[, i - 1]) - C_[, i - 1]/rk$K
        }
      
      P_[, i][ P_[, i]<0]=0
      B_[, i] = P_[, i] * rk$K
      B_[, i][ B_[, i]<0]=0
      if (quant == "F") {
        if (interim.quant == "Catch") {
          if (i==2) { 
            C_[, i] =    C_[, i] 
            H_[, i] = C_[, i]/B_[, i]
            
          } else {
            C_[, i] = H_[, i] * B_[, i]
            
          }
        }
        C_[, i] = H_[, i] * B_[, i]
        C_[, i][ C_[, i]<0]=0
        #  FFmsy_[, i] =   ifelse( P_[, i]<0.25, H_[, i]/(rk$r*2* P_[, i]),H_[, i]*2/rk$r)
        if (median(P_[, i])>0.25) {
          FFmsy_[, i] = H_[, i]*2/rk$r } else {
            FFmsy_[, i] =  H_[, i]/(rk$r*2* P_[, i])
          }
        FFmsy_[, i][ FFmsy_[, i]<0]=0
        FFmsy_[, i][is.infinite( FFmsy_[, i])]=0
        FFmsy_[, i][is.nan( FFmsy_[, i])]=0
      }
      if (quant == "Catch") {
        if (interim.quant == "F") {
          if (i==2) { 
            H_[, i] =  H_[, i] 
            C_[, i]= H_[, i]*B_[, i]
          } else { 
            H_[, i] = C_[, i]/B_[, i]
          }
        }
        
        H_[, i] = C_[, i]/B_[, i]
        H_[, i][ H_[, i]<0]=0
        H_[, i][is.infinite( H_[, i])]=0
        # FFmsy_[, i] =   ifelse( P_[, i]<0.25, H_[, i]/(rk$r*2* P_[, i]),H_[, i]*2/rk$r)
        if (median(P_[, i])>0.25) {
          FFmsy_[, i] = H_[, i]*2/rk$r } else {
            FFmsy_[, i] =  H_[, i]/(rk$r*2* P_[, i])
          }
        
        FFmsy_[, i][ FFmsy_[, i]<0]=0
        FFmsy_[, i][is.infinite( FFmsy_[, i])]=0
        FFmsy_[, i][is.nan( FFmsy_[, i])]=0
        
      }
    }
    B_a=gather(B_,key=year,value = B)
    H_a=gather(H_,key=year,value = H)
    C_a=gather(C_,key=year,value = Catch)
    P_a=gather(P_,key=year,value =P)
    FFmsy_a=gather(FFmsy_,key=year,value = FFmsy)
    proj=cbind(B_a,H_a[,2],C_a[,2],P_a[,2],FFmsy_a[,2])
    proj$Scenario=Scenaria[j,1]
    Scen_list[[j]]=proj
  }
  Scen_lst=rbindlist(Scen_list)
  colnames(Scen_lst)=c("year","B","H", "Catch","P","FFmsy", "Scenario")
  
  trj$Scenario="fit"
  fin_df=Scen_lst
  
  fit_=ABC_res[["output"]][["output_timeseries"]]
  fit_$Scenario="fit"
  fit_$year=as.integer(row.names(fit_))
  
  fit_=fit_[, c("year","Scenario","Catch","Catch_low","Catch_high", "FFmsy","FFmsy_low",
                "FFmsy_high", "f","f_low","f_high","BBmsy","BBmsy_low",
                "BBmsy_high", "B","B_low","B_high","bk","bk_low","bk_high")]
  
  fin_df_2=fin_df%>%group_by( year,Scenario)%>%summarise(
    Catch2=median(Catch),
    Catch_low=quantile(Catch,0.025),
    Catch_high =quantile(Catch,0.975),
    FFmsy=median(FFmsy),#/fmsy,
    FFmsy_low=quantile(FFmsy,0.025),#/fmsy,
    FFmsy_high =quantile(FFmsy,0.975),#/fmsy,
    f=median(H),
    f_low=quantile(H,0.025),
    f_high =quantile(H,0.975),
    BBmsy=median(P)/2,
    BBmsy_low=quantile(P,0.025)/2,
    BBmsy_high =quantile(P,0.975)/2,
    B_=median(B),
    B_low=quantile(B,0.025),
    B_high =quantile(B,0.975),
    bk=median(P),
    bk_low=quantile(P,0.025),
    bk_high =quantile(P,0.975)
  )%>%ungroup()%>%as.data.frame()
  
  colnames(fin_df_2)=colnames(fit_)
  f_df=rbind(fit_,fin_df_2)
  f_df$year=as.integer(f_df$year)
  return(f_df)
}

################################## FUNCTIONS
################################## FUNCTIONS
################################## FUNCTIONS
################################## FUNCTIONS
################################## FUNCTIONS
################################## FUNCTIONS





################################################################
#########################   SHINY UI    ########################
################################################################ 

  shinyUI <- shinydashboard::dashboardPage(#skin = "purple",
    shinydashboard::dashboardHeader(title = "BSM"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Welcome", tabName = "main_page", icon = shiny::icon("door-open")),
        shinydashboard::menuItem("1. Data input", tabName = "Dt_in_page", icon = shiny::icon("file-import"),
                                 shinydashboard::menuItem('Load data',
                                                          tabName = 'Upld_dt_page',
                                                          icon = shiny::icon('upload')),
                                 shinydashboard::menuItem('Explore existing stocks',
                                                          tabName = 'Expl_dt_page',
                                                          icon = shiny::icon('list'))),
        shinydashboard::menuItem("2. Prepare data", tabName = 'Pdata', icon = shiny::icon("wrench")),
        shinydashboard::menuItem("3. Priors", tabName = 'priors', icon = shiny::icon("magnifying-glass")),
        shinydashboard::menuItem("4. Run the model", tabName = 'run_model', icon = shiny::icon("play")),
        shinydashboard::menuItem("5. Run model forecast", tabName = 'run_forecast', icon = shiny::icon("arrow-trend-up")),
        shinydashboard::menuItem(" -  Additional info", tabName = 'addit_info', icon = shiny::icon("info")),
        tags$hr(style = "border-top: 3px solid #000000;"),
        shinyWidgets::actionBttn(inputId = "quit", label = "Quit",style = "jelly", color = "danger")
      ) #sidebarMenu
    ), #dashboardSidebar
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        ################################################################
        ######################    PAGE 1 INTRO    ######################
        ################################################################
        shinydashboard::tabItem(tabName = "main_page",
                                shiny::fluidRow(
                                  column(width=8,offset=0.5,shinydashboard::box(title = "BSM",  solidHeader = TRUE,status="primary",
                                                                                shiny::h4( "BSM is a stock assessment method using a full Bayesian implementation of a modified Schaefer model to predict stock status and exploitation from catch and abundance data. An Artificial Intelligence neural network assists in selecting appropriate priors for relative stock size based on patterns in catch time series."),
                                                                                width=10))),
                                shiny::fluidRow(
                                  column(width=8,shinydashboard::box(title = "Step-by-Step process", solidHeader = TRUE,status="primary",
                                                                     shiny::h4( "1. Either select one of the existing stocks to assess or load your own data"),
                                                                     shiny::h4( "2. Prepare your data"),
                                                                     shiny::h4( "3. Explore and decide about priors"),
                                                                     shiny::h4( "4. Run the model and export the results"),width=10),offset=5))#,
                                # headerPanel(
                                # HTML('<p><shiny::img src="Picture2.png"/></p>')
                                # shiny::fluidRow( #system.file("image","Picture2.png",package = "triton")
                                #   shinydashboard::box(img(src="Picture2.png", height="90%", width="90%"),
                                #       background = "light-blue",
                                #                       align="center", width = 12)
                                # )
        ),
        ################################################################
        ###################### PAGE 2 UPLOAD DATA ######################
        ################################################################
        shinydashboard::tabItem(tabName = "Upld_dt_page", #tabItem2
                                shiny::fluidRow(
                                  shinydashboard::valueBoxOutput("adv_up1",width =12)
                                ),
                                shiny::fluidRow(
                                  column(width = 4,
                                         shiny::fluidRow( shinydashboard::tabBox(title = shiny::h4("Input stock information"),id="Inp_Stck_tabset",
                                                                                 side="right",selected = "Species info",
                                                                                 tabPanel("Region info",
                                                                                          shinyWidgets::pickerInput(inputId = "Area_Cont",label = "Continent",
                                                                                                                    choices =c("Africa","Antarctica","Asia","Europe",
                                                                                                                               "North America","South America","Oceania"),
                                                                                                                    options = list(title = "Select continent")
                                                                                          ),
                                                                                          textInput("Area_Reg", "Region"),
                                                                                          textInput("Area_SbReg", "Sub-region"),
                                                                                          textInput("Area_comm", "Area comments")),
                                                                                 tabPanel("Species info",
                                                                                          textInput("Sp_Scient", "Scientific"),
                                                                                          textInput("Sp_Comm", "Common name"),
                                                                                          shinyWidgets::pickerInput(inputId = "Sp_Group",label = "Species group",
                                                                                                                    choices = c("Demersal fish","Small pelagic","Large pelagic",
                                                                                                                                "Cephalopod","Crustacean","Other"),
                                                                                                                    options = list(title = "Select species group"))
                                                                                 ),
                                                                                 width = 12)),
                                         shiny::fluidRow(align="center",
                                                         conditionalPanel(condition ="input.Sp_Scient!=''",
                                                                          shinyWidgets::actionBttn(inputId="Upld_butt_1",label =" Start the process",
                                                                                                   style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                   no_outline=F,block=F,color="primary")))
                                  ),
                                  column(width = 4,
                                         shinydashboard::valueBoxOutput("Sps_Info",width = 12)
                                  ),
                                  column(width = 4,
                                         shiny::fluidRow(
                                           shinydashboard::valueBoxOutput("Area_Info",width = 12)
                                         )
                                  )
                                ),
                                shiny::fluidRow(
                                  conditionalPanel(condition ="input.Upld_butt_1",
                                                   tags$br(),
                                                   shinydashboard::valueBoxOutput("adv_up2",width =12))
                                ),
                                shiny::fluidRow(
                                         conditionalPanel(condition ="input.Upld_butt_1",
                                                          shinydashboard::box(
                                                            shinyWidgets::awesomeRadio(inputId = "btype",label = "Select CPUE/Biomass type",
                                                                                       choices = c(CPUE="CPUE",Biomass="biomass"),
                                                                                       selected = "CPUE",inline = TRUE,
                                                                                       checkbox = F),
                                                            width=4)
                                                          )
                                ),
                                shiny::fluidRow(
                                  column(width=4,
                                         conditionalPanel(condition ="input.Upld_butt_1",
                                                          shinydashboard::box(title = "Data load",  collapsible = TRUE,
                                                                              fileInput("file1", "Choose CSV File",multiple = TRUE,
                                                                                        accept = c("text/csv","text/comma-separated-values,text/plain",
                                                                                                   ".csv")),
                                                                              checkboxInput("header", "Header row present?", TRUE),
                                                                              radioButtons("sep", "Column separator",
                                                                                           choices = c(Comma = ",",Semicolon = ";",
                                                                                                       Tab = "\t"),selected = ",",
                                                                                           inline = T),
                                                                              radioButtons("quote", "Text marker",choices = c(None = "",
                                                                                                                              '"' = '"',"'" = "'"),
                                                                                           selected = '"',inline = T),
                                                                              shinyWidgets::pickerInput(inputId = "Yr_col",label = "Year column",
                                                                                                        choices = c(""),options = list(
                                                                                                          title = "Define the year column")),
                                                                              shinyWidgets::pickerInput(inputId = "ct_col",label = "Catch column",
                                                                                                        choices = c(""),options = list(
                                                                                                          title = "Define the Catch column")),
                                                                               shinyWidgets::pickerInput(inputId = "bt_col",label = "CPUE/Biomass column",
                                                                                                                         choices = c(""),options = list(
                                                                                                                           title = "Define the Biomass column")),
                                                                              shinyWidgets::awesomeRadio(inputId = "do_u_have_cv",label = "Can you provide CV for CPUE/Biomass?",
                                                                                                         choices = c(Yes="Yes",No="No"),
                                                                                                         selected = "No",inline = TRUE,
                                                                                                         checkbox = F),
                                                                              helpText("Note: if Yes, then in your input .csv you should have a column with cpue cv."),
                                                                              conditionalPanel(condition ="input.do_u_have_cv=='Yes'",
                                                                                               shinyWidgets::pickerInput(inputId = "cv_col",label = "CV column",
                                                                                                                         choices = c(""),options = list(
                                                                                                                           title = "Define the CV column"))        
                                                                                               ),
                                                                              #)
                                                                              width=12),
                                                          conditionalPanel(condition ="input.Yr_col!='' & input.ct_col!=''& input.bt_col!=''",# (input.bt_col!=''| input.is_there_biomass=='Catch')
                                                                           shinyWidgets::materialSwitch(
                                                                             inputId = "Upld_butt_3",
                                                                             label = " Continue",
                                                                             status = "primary"
                                                                           )))),
                                  column(width=8,
                                         shiny::fluidRow(
                                           conditionalPanel(condition ="input.Upld_butt_1",
                                                            shinydashboard::box(title = "Loaded data", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                                                                                DT::dataTableOutput("contents"),width=12)))
                                  )
                                ),
                                shiny::fluidRow(
                                  conditionalPanel(condition ="input.Upld_butt_3",
                                                   tags$br(),
                                                   shinydashboard::valueBoxOutput("adv_up4",width =12))
                                ),
                                shiny::fluidRow(
                                  column(width = 4,
                                         conditionalPanel(condition ="input.Upld_butt_3",
                                                          shinydashboard::box(title = "Stock Creation", solidHeader = TRUE,status = "primary",background = "light-blue",
                                                                              textInput("Crtd_StckID_input", shiny::h4("Input a stockID"), ""),
                                                                              shinyWidgets::actionBttn(inputId="button_2",label =" Create Stock Object",
                                                                                                       style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                       no_outline=F,block=F,color="primary"),
                                                                              width = 12))
                                  ),
                                  column(width = 4,
                                         conditionalPanel(condition = "input.Upld_butt_3",
                                                          shinydashboard::box(title = "Catch Plot", solidHeader = TRUE,status = "primary",
                                                                              shinycssloaders::withSpinner(shiny::plotOutput("Catch_plot2")),width = 12,height=300))),
                                  column(width = 4,
                                         conditionalPanel(condition = "input.Upld_butt_3",
                                                          shinydashboard::box(title = "CPUE Plot", solidHeader = TRUE,status = "primary",
                                                                              shinycssloaders::withSpinner(shiny::plotOutput("Biomass_plot2")),width = 12,height=300)))),
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.button_2",
                                                   shinydashboard::valueBoxOutput("Stock_infobox_1",width = 12)))
        ),
        ################################################################
        ###################### PAGE 3 EXPLORE DATA #####################
        ################################################################
        shinydashboard::tabItem(tabName = "Expl_dt_page",
                                shiny::fluidRow(
                                  shinydashboard::valueBoxOutput("adv_expl1",width =12)
                                ),
                                shiny::fluidRow(
                                  column(width = 4,
                                         shinydashboard::box(title = "Explore existing stocks", solidHeader = TRUE,
                                                             datamods::select_group_ui(
                                                               id = "my_filters",
                                                               inline = FALSE,
                                                               params = list(
                                                                 Continent = list(inputId = "Continent", title = "Continent", placeholder = 'select'),
                                                                 Region = list(inputId = "Region", title = "Region", placeholder = 'select'),
                                                                 Subregion = list(inputId = "Subregion", title = "Sub-region", placeholder = 'select'),
                                                                 Group = list(inputId = "Group", title = "Group", placeholder = 'select'),
                                                                 ScientificName = list(inputId = "ScientificName", title = "Scientific name", placeholder = 'select'))
                                                             ),width = 12),
                                         shinydashboard::box(title = "Explore existing stocks", solidHeader = TRUE,status = "primary",background = "light-blue",
                                                             textInput("txt1", shiny::h4("Select stock:"), ""),
                                                             shinyWidgets::actionBttn(inputId="button_1",label =" Create stock object",
                                                                                      style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                      no_outline=F,block=F,color="primary"),
                                                             width=12),
                                         conditionalPanel(condition = "input.button_1",
                                                          shinydashboard::valueBoxOutput("Stock_infobox_2",width = 12))
                                  ),
                                  column(width = 8,
                                         shiny::fluidRow(shinydashboard::box(title = "Existing stocks", solidHeader = TRUE,status = "primary",
                                                                             DT::dataTableOutput("Exist_Stcks_tble"),width = 12)),
                                         shiny::fluidRow(
                                           shinydashboard::valueBoxOutput("txtout1",width =6),
                                           shinydashboard::valueBoxOutput("txtout2",width =6)),
                                         shiny::fluidRow(
                                           shinydashboard::valueBoxOutput("txtout3",width =6),
                                           shinydashboard::valueBoxOutput("txtout4",width =6)),
                                         shiny::fluidRow(
                                           shinydashboard::box(title = "Catch plot", solidHeader = TRUE,
                                                               shinycssloaders::withSpinner(shiny::plotOutput("Catch_plot")),width = 6,height=300),
                                           shinydashboard::box(title = "CPUE plot", solidHeader = TRUE,
                                                               shinycssloaders::withSpinner(shiny::plotOutput("Biomass_plot")),width = 6,height=300)
                                         )))
        ),
        ################################################################
        ###################### PAGE 4 Prepare Data  ####################
        ################################################################
        
        tabItem(tabName = "Pdata",
                fluidRow( h3("Explore and prepare the data")),
                fluidRow(
                  conditionalPanel(condition = "input.button_1 |input.button_2",
                                   shinydashboard::box(collapsible = F,
                                                       shinyWidgets::actionBttn(inputId="prepare_data",label ="Start",
                                                                                style = "unite",size = "md",icon = icon("paper-plane"),
                                                                                no_outline=F,block=F,color="primary"),
                                                       shinyBS::bsTooltip("prepare_data", title="Press to proceed to the next step",
                                                                          placement = "bottom", trigger = "hover",
                                                                          options = NULL),width = 4
                                   )),
                  conditionalPanel(condition = "input.prepare_data",valueBoxOutput("Stock_infobox_prepare",width = 8))
                  ),
                fluidRow(  conditionalPanel(condition = "input.prepare_data", 
                                            h3("Prepare catch data"))),
                fluidRow(
                  column(width=4,
                         conditionalPanel(condition = "input.prepare_data", 
                                          shinydashboard::box(collapsible = F, 
                                                              tags$b("Change start and end year to those with reliable catch data"),
                                                              shinyWidgets::prettySwitch(
                                                                inputId = "upMaterial1",
                                                                label = "Edit",
                                                                slim = T,
                                                                value = FALSE,
                                                                status = "info" ),
                                                              conditionalPanel(condition = "input.upMaterial1",
                                                                               sliderInput("yr_slct", "",
                                                                                           min = 1900, max = 2026, value = c(1900, 2026),step=1,sep = "")),
                                                              tags$b("Smooth catch data if variability is unreasonably high"),
                                                              shinyWidgets::prettySwitch(
                                                                inputId = "upMaterial2",
                                                                slim = T,
                                                                label = "Edit",
                                                                value = FALSE,status = "info" ),
                                                              conditionalPanel(condition = "input.upMaterial2",
                                                                               sliderInput(inputId = "bw_inpt", label = "",
                                                                                           min = 1, max = 6, value =1, step =1)),
                                                              tags$b("Change CV to reflect uncertainty in catch"),
                                                              helpText("Note: Minimum of 0.1 recommended"),
                                                               shinyWidgets::prettySwitch(
                                                                inputId = "upMaterialccv",
                                                                slim = T,
                                                                label = "Edit",
                                                                value = FALSE,status = "info" ),
                                                              conditionalPanel(condition = "input.upMaterialccv",
                                                               sliderInput(inputId = "Catch_CV", label = "",
                                                                          min = 0.05, max = 0.5, value =0.15, step =0.05)),
                                                              # tags$b("Change the prior for MSY"),
                                                              # shinyWidgets::prettySwitch(
                                                              #   inputId = "upMaterial3",
                                                              #   slim = T,
                                                              #   label = "Edit",
                                                              #   value = FALSE,status = "info" ),
                                                              # conditionalPanel(condition = "input.upMaterial3",
                                                              #                  textOutput("calc_MSY"),
                                                              #                  sliderInput(inputId = "MSY_prr", label = "",
                                                              #                              min = 1, max = 10, value =3, sep="",step =0.01)),
                                                              width =12))
                  ),
                         column(width=8,    
                                conditionalPanel(condition = "input.prepare_data", 
                                                 shinydashboard::box(collapsible = F, 
                                                                     shinycssloaders::withSpinner(shiny::plotOutput(outputId = "Check_Catch_plot")),
                                                                     width =12))
                         )
                 ),
                
                fluidRow( 
                  conditionalPanel(condition = "input.prepare_data", 
                                   h3("Prepare CPUE data"))),
                fluidRow(
                  column(width=4,
                         conditionalPanel(condition = "input.prepare_data", 
                                          shinydashboard::box(collapsible = F, 
                                                              tags$b("Change start and end year to those with reliable CPUE data"),
                                                              shinyWidgets::prettySwitch(
                                                                inputId = "upMaterial4",
                                                                slim = T,
                                                                label = "Edit",
                                                                value = FALSE,status = "info" ),
                                                              conditionalPanel(condition = "input.upMaterial4",
                                                                               sliderInput("bio_yr_slct", "",
                                                                                           min = 1900, max = 2026, value = c(2005,2020),step=1,sep = "")),
                                                              #helpText("Note: Select start year such that you trust that the data are reasonable."),
                                                                                    tags$b("Correct CPUE data for effort creep"),
                                                              shinyWidgets::prettySwitch(
                                                                inputId = "ecreepyes",
                                                                label = "Edit",
                                                                slim = T,
                                                                value = FALSE,
                                                                status = "info" ),
                                                              conditionalPanel(condition = "input.ecreepyes",
                                                                               uiOutput("help_ecreep"),
                                                                               sliderInput("ecreep_year", "Set starting year for ecreep",
                                                                                           min = 1900, max = 2026, value = 2000,step=1,sep = ""),      
                                                                               helpText("Note: Choose the year from which the ecreep effect starts. Consider that the first sonars became available in the 1970s, the first GPS units in the 1980s, cheap fish finders and chart plotters after 2000."),
                                                                               sliderInput("ecreepslider", "% value",
                                                                                           min = 0, max = 5, value = c(2),step=0.5,sep = "")
                                                              ),
                                                              tags$b("Smooth CPUE data if variability is unreasonably high"),
                                                              shinyWidgets::prettySwitch(
                                                                inputId = "upMaterial_bt",
                                                                slim = T,
                                                                label = "Edit",
                                                                value = FALSE,status = "info" ),
                                                              conditionalPanel(condition = "input.upMaterial_bt",
                                                                               sliderInput(inputId = "bw_inpt_bt", label = "",
                                                                                           min = 1, max = 6, value =1, step =1)),
                                                              conditionalPanel(condition = "input.do_u_have_cv=='No'",
                                                                tags$b("Add minimum realistic CV for CPUE"),
                                                              shinyWidgets::prettySwitch(
                                                                inputId = "upMaterialbcv",
                                                                slim = T,
                                                                label = "Edit",
                                                                value = FALSE,status = "info" ),
                                                              conditionalPanel(condition = "input.upMaterialbcv",
                                                                               sliderInput(inputId = "CPUE_CV", label = "",
                                                                                           min = 0.05, max = 0.5, value =0.2, step =0.05))),
                                                              width =12))   
                  ),
                  column(width=8,    
                         conditionalPanel(condition = "input.prepare_data", 
                                          shinydashboard::box(collapsible = F, 
                                                              shinycssloaders::withSpinner(shiny::plotOutput(outputId = "Check_Biomass_plot")),
                                                              width =12),
                                          linebreaks(2),
                                          shinydashboard::valueBoxOutput("When_happy",width = 12)
                                          )
                  )
                  )
                ),
        
        ################################################################
        ###################### PAGE 5 PRIOR ENTRY  #####################
        ################################################################
        shinydashboard::tabItem(tabName = "priors",
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.prepare_data",
                                                   shinydashboard::valueBoxOutput("Stock_infobox_patience",width = 12))),
                                shiny::fluidRow( shiny::h3("Select resilience - r priors")),
                                  shiny::fluidRow( column(width = 4,
                                  conditionalPanel(condition = "input.prepare_data",
                                                   shinydashboard::box(collapsible = F,
                                                                       # uiOutput("cond_ifmany"),
                                                                       shinyWidgets::actionBttn(inputId="procc_rpriors",label ="Start",
                                                                                                style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                no_outline=F,block=F,color="primary"),
                                                                       shinyBS::bsTooltip("procc_rpriors", title="Press to proceed to the next step",
                                                                                          placement = "bottom", trigger = "hover",
                                                                                          options = NULL),width = 12
                                                   ))),
                                column(width = 4,
                                       conditionalPanel(condition = "input.procc_rpriors",
                                                        shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("Stock_infobox_3",width = 12)))
                                ),       
                                column(width = 4,
                                       conditionalPanel(condition ="input.procc_rpriors",
                                                        #shinydashboard::valueBoxOutput("Stock_infobox_patience",width = 4),
                                                        shinydashboard::box(collapsible = F,align="center",background="green",
                                                                            # uiOutput("cond_ifmany"),
                                                                            shiny::h4("Press 'Connect' to establish connection to FishBase or SeaLifeBase to get information on resilience and r priors for your stock"),
                                                                            shinyWidgets::actionBttn(inputId="con_fbase",label ="Connect",
                                                                                                     style = "unite",size = "md",icon = shiny::icon("info"),
                                                                                                     no_outline=F,block=F,color="success"),
                                                                            shinyBS::bsTooltip("con_fbase", title="Press to proceed to the next step",
                                                                                               placement = "bottom", trigger = "hover",
                                                                                               options = NULL),width = 12
                                                        )))), 
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.procc_rpriors",
                                                   shinydashboard::box(collapsible = F,
                                                                       tags$b("Change r prior"),
                                                                       shinyWidgets::prettySwitch(
                                                                         inputId = "Acc_FBpriors",
                                                                         label = "Edit",
                                                                          slim = T,
                                                                         value = T,status = "info" ),
                                                                       helpText("Note: The Resilience classification from FishBase/SeaLifeBase provides a general and often best first prior range for r and is used as default. You can manually change that range if better data are available. Note that High resilience only applies to species which reproduce within their first year."),
                                                                       conditionalPanel(condition = "input.Acc_FBpriors",
                                                                                        selectInput("resilience_in","Resilience",
                                                                                                    choices=c("","Very low","Low","Medium","High"),
                                                                                                    selected = ""),
                                                                                        helpText("Note: Very low: 0.015-0.01. Low: 0.05-0.5. Medium: 0.2-0.8. High: 0.6-1.5."),
                                                                                        sliderInput("priors_rrange", "r priors range",
                                                                                                    min = 0, max = 1.5, value = c(0.2, 0.8),step=0.01,sep = ""),
                                                                                        helpText("Note: r low and r high priors are an optional set of parameters to specify the range of intrinsic growth rate for the selected species. If no values are provided, the range will be based on Resilience. If values are given, the Resilience choise will be ignored.")
                                                                       ),
                                                                       shinyWidgets::materialSwitch(
                                                                         inputId = "Acc_rpriors",
                                                                         label = " Accept r prior and continue",
                                                                         status = "primary"),
                                                                       width=4)),
                                  conditionalPanel(condition = "input.con_fbase",
                                                   shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("FB_resil",width =5))),
                                  conditionalPanel(condition = "input.procc_rpriors",
                                                   shinydashboard::valueBoxOutput("Res_rpriors",width = 3))
                                ),
                                fluidRow(
                                  conditionalPanel(condition = "input.Acc_rpriors",
                                                   h3("Select other priors. Start by setting prior of relative stock size (B/k) for a selected year"))),
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.Acc_rpriors",
                                                   shinydashboard::box(collapsible = F,
                                                                       linebreaks(1),
                                                                       tags$b("Option to load B/k priors from LBB"),
                                                                       shinyWidgets::prettySwitch(
                                                                         inputId = "Load_LBB",
                                                                         slim = T,
                                                                         label = "Load independent priors from LBB",
                                                                         value = FALSE,status = "info" ),
                                                                       conditionalPanel(condition ="input.Load_LBB",
                                                                                        shinydashboard::box(title = "Data load",  collapsible = TRUE,
                                                                                                            fileInput("file_LBB", "Choose .csv File",multiple = TRUE,
                                                                                                                      accept = c("text/csv","text/comma-separated-values,text/plain",
                                                                                                                                 ".csv")),
                                                                                                            width=12)),width=4),
                                                   uiOutput("LBB_message")
                                                   
                                  )),
                                shiny::fluidRow(
                                  tags$head(
                                    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
                                  ),
                                  column(width=4,
                                         conditionalPanel(condition = "input.Acc_rpriors",
                                                          shinydashboard::box(collapsible = F,
                                                                              tags$b("Change B/k prior for start year"),
                                                                              shinyWidgets::prettySwitch(
                                                                                inputId = "upMat1",
                                                                                slim = T,
                                                                                label = "Edit",
                                                                                value = FALSE,status = "info" ),
                                                                              conditionalPanel(condition = "input.upMat1",
                                                                                               selectInput("expert_bk_start","",#B/k start year based on expertise knowledge
                                                                                                           choices= c("","Unexploited, 0.75-1.0", "Sustainable, 0.4-0.8", "Medium, 0.2-0.6", "Low, 0.01-0.4","Very low, 0.01-0.2"),
                                                                                                           selected = ""),
                                                                                               #  helpText("Note: Very low: 0.01-0.2. Low: 0.01-0.4. Medium: 0.2-0.6. Sustainable: 0.4-0.8. Unexploited: 0.75-1.0."),
                                                                                             #  textOutput("calc_Bk_start"),
                                                                                               sliderInput("man_Bk_start_A", "",
                                                                                                           min = 0, max = 1.25, value = c(0.4, 0.8),step=0.01,sep = "")),
                                                                              tags$b("Change B/k prior for intermediate year"),
                                                                              shinyWidgets::prettySwitch(
                                                                                inputId = "upMat2",
                                                                                slim = T,
                                                                                label = "Edit",
                                                                                value = FALSE,status = "info" ),
                                                                              conditionalPanel(condition = "input.upMat2",
                                                                                                                   sliderInput("man_Bk_int_year_A", "Select year for intermediate B/k prior",
                                                                                                           min = 1900, max = 2026, value = 2020,step=1,sep = ""),
                                                                                               selectInput("expert_bk_ind","",#B/k intermediate year based on expertise knowledge
                                                                                                           choices= c("","Unexploited, 0.75-1.0", "Sustainable, 0.4-0.8", "Medium, 0.2-0.6", "Low, 0.01-0.4","Very low, 0.01-0.2"),
                                                                                                           selected = ""),
                                                                                               #  helpText("Note: Very low: 0.01-0.2. Low: 0.01-0.4. Medium: 0.2-0.6. Sustainable: 0.4-0.8. Unexploited: 0.75-1.0."),
                                                                                              # textOutput("calc_Bk_int"),
                                                                                               sliderInput("man_Bk_int_A", "",
                                                                                                           min = 0, max = 1.25, value = c(0.4, 0.8),step=0.01,sep = "")),
                                                                              tags$b("Change B/k prior for end year"),
                                                                              shinyWidgets::prettySwitch(
                                                                                inputId = "upMat3",
                                                                                label = "Edit",
                                                                                slim = T,
                                                                                value = FALSE,status = "info" ),
                                                                              conditionalPanel(condition = "input.upMat3",
                                                                                               selectInput("expert_bk_end","",#B/k end year based on expertise knowledge
                                                                                                           choices= c("","Unexploited, 0.75-1.0", "Sustainable, 0.4-0.8", "Medium, 0.2-0.6", "Low, 0.01-0.4","Very low, 0.01-0.2"),
                                                                                                           selected = ""),
                                                                                               # helpText("Note: Very low: 0.01-0.2. Low: 0.01-0.4. Medium: 0.2-0.6. Sustainable: 0.4-0.8. Unexploited: 0.75-1.0."),
                                                                                              # textOutput("calc_Bk_end"),
                                                                                               sliderInput("man_Bk_end_A", "",
                                                                                                           min = 0, max = 1.25, value = c(0.4, 0.8),step=0.01,sep = "")),
                                                                              width=12))
                                  ),
                                  column(width=8,
                                         conditionalPanel(condition = "input.Acc_rpriors",
                                                          shiny::h4(tagList("Select B/k priors based on expert knowledge ", actionLink(inputId = "open_helpTable", label = "(Guidance)"))),
                                                          shinycssloaders::withSpinner(shiny::plotOutput(outputId = "Check_priors_plot"))
                                         ))),
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.Acc_rpriors",
                                                   shinyWidgets::materialSwitch(
                                                     inputId = "Priors_but2",
                                                     label = "Continue to B/k prior selection",
                                                     status = "primary"))),
                                fluidRow(
                                  conditionalPanel(condition = "input.Priors_but2",
                                                   shiny::h4("Compare your initial prior settings with the pattern proposed by a neural network (ANN) based on the catch pattern. Note that the ANN pattern may not apply to your stock and that especially the first year may be wrong because ANN cannot distinguish between early low catches stemming from low effort (= nearly unexploited B/k) or previous overfishing (= very low B/k)."))),
                                shiny::fluidRow(conditionalPanel(condition = "input.Priors_but2",
                                                                 shiny::h5("Select starting and ending year, explore Catch plot and MSY priors."))),
                                shiny::fluidRow(
                                  column(width=4,
                                         shiny::fluidRow(width=12,
                                                         conditionalPanel(condition = "input.Priors_but2",
                                                                          shinydashboard::box(collapsible = F,
                                                                                              conditionalPanel(condition = "input.Priors_but2",
                                                                                                               tags$b("Change B/k prior for start year"),
                                                                                                               shinyWidgets::prettySwitch(
                                                                                                                 inputId = "upMaterial5",
                                                                                                                 slim = T,
                                                                                                                 label = "Edit",
                                                                                                                 value = FALSE,status = "info" ),
                                                                                                               conditionalPanel(condition = "input.upMaterial5",
                                                                                                                                sliderInput("man_Bk_start", "",
                                                                                                                                            min = 0, max = 1.25, value = c(0.4, 0.8),step=0.01,sep = "")),
                                                                                                               tags$b("Change B/k prior for intermediate year"),
                                                                                                               shinyWidgets::prettySwitch(
                                                                                                                 inputId = "upMaterial6",
                                                                                                                 slim = T,
                                                                                                                 label = "Edit",
                                                                                                                 value = FALSE,status = "info" ),
                                                                                                               conditionalPanel(condition = "input.upMaterial6",
                                                                                                                                sliderInput("man_Bk_int_year", "Select year for intermediate B/k prior",
                                                                                                                                            min = 1900, max = 2026, value = 2020,step=1,sep = ""),
                                                                                                                                sliderInput("man_Bk_int", "",
                                                                                                                                            min = 0, max = 1.25, value = c(0.4, 0.8),step=0.01,sep = "")),
                                                                                                               tags$b("Change B/k prior for end year"),
                                                                                                               shinyWidgets::prettySwitch(
                                                                                                                 inputId = "upMaterial7",
                                                                                                                 label = "Edit",
                                                                                                                 slim = T,
                                                                                                                 value = FALSE,status = "info" ),
                                                                                                               conditionalPanel(condition = "input.upMaterial7",
                                                                                                                                sliderInput("man_Bk_end", "",
                                                                                                                                            min = 0, max = 1.25, value = c(0.4, 0.8),step=0.01,sep = "")),
                                                                                                               # materialSwitch(
                                                                                                               #   inputId = "show_biomass",
                                                                                                               #   label = "Show CPUE in the plot", 
                                                                                                               #   value = FALSE,
                                                                                                               #   status = "primary"
                                                                                                               # ),
                                                                                                               awesomeRadio(
                                                                                                                 inputId = "nbk_",
                                                                                                                 label = "Choose the B/k priors to be used in the model", 
                                                                                                                 choices = c("Only start", "Start & intermediate", "All three (recommended)"),
                                                                                                                 selected = "All three (recommended)", 
                                                                                                                 inline = FALSE
                                                                                                               )
                                                                                                               ),
                                                                                              width =12),
                                                                          shinydashboard::box(collapsible = F,
                                                                                              # conditionalPanel(condition = "input.Save_priors", 
                                                                                              #helpText("Note: Copy the working directory address and paste it here."),
                                                                                              #helpText("example: 'C:\ABC_outputs'"),
                                                                                              linebreaks(1),
                                                                                              tags$b("Set working directory"),
                                                                                              
                                                                                              shinyDirButton("directory", "Select folder", "Please select a folder",multiple=FALSE),
                                                                                              #textInput("dir.name", "Set working directory"),
                                                                                              linebreaks(2),
                                                                                              uiOutput("dir_name"),
                                                                                              linebreaks(2),
                                                                                              helpText("Name your stock object. Note that you can create several stock objects with different parameterization, store them and run the model for each one in the 'Run model' tab."),
                                                                                              # helpText(HTML(paste0("<b>","Always press 'Save priors' to update possible changes","</b>"))),
                                                                                              textInput("obj.name", "Name stock object"),
                                                                                              shinyWidgets::actionBttn(inputId="Save_priors",label ="Save input so far",
                                                                                                                       style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                                       no_outline=F,block=F,color="danger"),
                                                                                              shinyBS::bsTooltip("Save_priors", title="Press 'YES' to save priors in your stock object. If changes have been made with 'save priors' button set to 'YES', press 'NO' and then 'YES'.",
                                                                                                                 placement = "bottom", trigger = "hover",
                                                                                                                 options = NULL),
                                                                                              linebreaks(2),
                                                                                              conditionalPanel(condition = "input.Save_priors",
                                                                                                               uiOutput("created_objects")),
                                                                                              width = 12)
                                                         )),
                                         shiny::fluidRow(width=12,
                                                         conditionalPanel(condition = "input.Save_priors",
                                                                          shinydashboard::valueBoxOutput("You_can_proceed_to_run",width = 12))
                                         )),
                                  column(width=8,
                                         conditionalPanel(condition = "input.Priors_but2",
                                                          shinycssloaders::withSpinner(shiny::plotOutput(outputId = "Biomass_plot_3"))
                                                          #tableOutput("TESTNEURAL"), ######################
                                         ),
                                         conditionalPanel(condition = "input.Priors_but2",
                                                          conditionalPanel(condition = "input.Save_priors",
                                                                           shinydashboard::box(collapsible = F,
                                                                                               htmlOutput("param_sofar"),
                                                                                               tags$hr(style = "border-top: 3px solid #000000;"),
                                                                                               verbatimTextOutput("middle_outputs"),
                                                                                               tags$hr(style = "border-top: 3px solid #000000;") ,width=12)
                                                          ))
                                         )                                         
                                ),
                                shinyBS::bsModal(id='popup_Page_helpTable',
                                                 title ='Guidance for selecting B/k range based on descriptions of stock status or fishery for the selected year',
                                                 trigger="open_helpTable",
                                                 size = "large",
                                                 shiny::tableOutput('helpTable')
                                )
                                ),
        ############ ############ ############
        ############ RUN MODEL UI ############
        ############ ############ ############
        shinydashboard::tabItem(tabName = "run_model",
                                shiny::fluidRow(uiOutput("Zavarakatranemia")),
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.Save_priors",
                                                   shinydashboard::box(collapsible = F,align="center", background = "light-blue",
                                                                       column(width=4,align="center",
                                                                              awesomeRadio(
                                                                                inputId = "Id049",
                                                                                label = "Select 'A' to run the current stock object or select 'B' to choose one of the saved object versions. You can examine the content of the chosen object by pressing the 'See selected object' button", 
                                                                                choices = c("A", "B"),
                                                                                selected = "A",
                                                                                inline = TRUE, 
                                                                                checkbox = TRUE
                                                                              )),
                                                                       column(width=4,align="center",
                                                                              
                                                                              conditionalPanel(condition = "input.Id049=='B'",
                                                                                               uiOutput("select_diff_obj"),
                                                                              )),
                                                                       column(width=4,align="center",
                                                                              conditionalPanel(condition = "input.Id049=='B'",
                                                                                               shinyWidgets::actionBttn(inputId="see_obj",label =" See selected object",
                                                                                                                        style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                                        no_outline=F,block=F,color="primary"))  
                                                                       ) , width=12))),
                                shiny::fluidRow(
                                  column(width=4,align="center",
                                         uiOutput("cond_ifmany_run"),
                                         conditionalPanel(condition = "input.Save_priors",
                                                          shinyWidgets::actionBttn(inputId="Start_run",label =" Start",
                                                                                   style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                   no_outline=F,block=F,color="primary")),
                                         shinyBS::bsTooltip("Start_run", title="Press to run the model",
                                                            placement = "bottom", trigger = "hover",
                                                            options = NULL)),
                                  column(width=8,
                                         conditionalPanel(condition = "input.Save_priors",
                                                          shinydashboard::valueBoxOutput("Run_infobox_patient",width=6)),
                                         )),
                                tags$br(),
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.Save_priors",
                                     shinyWidgets::progressBar(
                                    id = "prog_bar",value = 0,total = 100,title = "",
                                    display_pct = TRUE,striped = T,# size="sm",
                                    status="success"))
                                ),
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.Start_run",
                                                   shinydashboard::box(collapsible = F,align="center", background = "light-blue",
                                                                       uiOutput("working_directory"),width = 12))
                                ),
                                shiny::fluidRow(
                                  column(width=4,
                                         conditionalPanel(condition = "input.Start_run",
                                                          tags$b("Model outputs"),
                                                          tags$hr(style = "border-top: 3px solid #000000;"),
                                                          uiOutput("ModOutp_a1"),
                                                          uiOutput("ModOutp_a2"),
                                                          uiOutput("ModOutp_a3"),
                                                          uiOutput("ModOutp_a4"),
                                                          uiOutput("ModOutp_a5"),
                                                          uiOutput("ModOutp_a6"),
                                                          uiOutput("ModOutp_a7"),
                                                          uiOutput("ModOutp_a8"),
                                                         # uiOutput("ModOutp_a9"),
                                                          uiOutput("ModOutp_a10"),
                                                          uiOutput("ModOutp_a11"),
                                                          tags$hr(style = "border-top: 1px solid #999797;"),
                                                          tags$b("Results of BSM analysis"),
                                                          uiOutput("ModOutp_b1"),
                                                          uiOutput("ModOutp_b2"),
                                                          uiOutput("ModOutp_b3"),
                                                          uiOutput("ModOutp_b6"),
                                                          uiOutput("ModOutp_b4"),
                                                          uiOutput("ModOutp_b5"),
                                                          tags$hr(style = "border-top: 1px solid #999797;"),
                                                          tags$b("Results for Management"),
                                                          uiOutput("ModOutp_23"),
                                                          uiOutput("ModOutp_25"),
                                                          uiOutput("ModOutp_26"),
                                                          uiOutput("ModOutp_27"),
                                                          uiOutput("ModOutp_28"),
                                                          uiOutput("ModOutp_29"),
                                                          uiOutput("ModOutp_30"),
                                                          tags$hr(style = "border-top: 3px solid #000000;"),
                                                          shinydashboard::box(collapsible = F,
                                                                              tags$b("Retrospective analysis"),
                                                                              linebreaks(1),
                                                                              shinyWidgets::prettySwitch(
                                                                                inputId = "Add_retro",
                                                                                label = "Edit",
                                                                                slim = T,
                                                                                value = FALSE,status = "info" ),
                                                                              conditionalPanel(condition = "input.Add_retro",
                                                                                               sliderInput("retro_range", "Retrospective years",
                                                                                                           min = 1, max =10, value = c(3),step=1,sep = ""),
                                                                                               helpText("Select number of retrospective years"),
                                                                                               shinyWidgets::actionBttn(inputId="Retrospective",label ="Retrospective",
                                                                                                                        style = "unite",size = "md",icon = shiny::icon("chart-area"),
                                                                                                                        no_outline=F,block=F,color="primary"),
                                                                                               shinyWidgets::progressBar(
                                                                                                 id = "prog_bar_retro",value = 0,total = 100,title = "",
                                                                                                 display_pct = TRUE,striped = T,# size="sm",
                                                                                                 status="success")
                                                                              ),
                                                                              tags$hr(style = "border-top: 3px solid #000000;"),
                                                                              tags$b("Plot management graphs"),
                                                                              linebreaks(1),
                                                                              shinyWidgets::actionBttn(inputId="ppplot5",label ="Management",
                                                                                                       style = "unite",size = "sm",icon = shiny::icon("chart-area"),
                                                                                                       no_outline=F,block=F,color="primary"),
                                                                              tags$hr(style = "border-top: 3px solid #000000;"),
                                                                              tags$b("Plot prior and posterior distributions"),
                                                                              linebreaks(1),
                                                                              shinyWidgets::actionBttn(inputId="PriorPosterior_trigger",label ="Prior/Posterior",
                                                                                                       style = "unite",size = "sm",icon = shiny::icon("chart-area"),
                                                                                                       no_outline=F,block=F,color="primary"),
                                                                              tags$hr(style = "border-top: 3px solid #000000;"),
                                                                              tags$b("Diagnostics"),
                                                                              linebreaks(1),
                                                                              shinyWidgets::actionBttn(inputId="diag_trigger",label ="Diagnostics",
                                                                                                       style = "unite",size = "sm",icon = shiny::icon("chart-area"),
                                                                                                       no_outline=F,block=F,color="primary"),
                                                                              tags$hr(style = "border-top: 3px solid #000000;"),
                                                                              tags$b("Save a graph"),
                                                                              shinyWidgets::materialSwitch(
                                                                                inputId = "DL_picA",
                                                                                label = "Save",
                                                                                value = FALSE,status = "info" ),
                                                                              conditionalPanel(condition = "input.DL_picA",
                                                                                               tags$b("Select graph"),
                                                                                               shinyWidgets::pickerInput(inputId = "Run_select_pic",
                                                                                                                         label = "",choices =c("A","B","C","D","E","F","G","H","I","J"),#,"J"
                                                                                                                         options = list(title = "Select graph"),
                                                                                                                         choicesOpt = list(
                                                                                                                           subtext = c(
                                                                                                                             "Finding viable r-k",
                                                                                                                             "Catch and MSY",
                                                                                                                             "Stock size",
                                                                                                                             "Exploitation rate",
                                                                                                                             "Equilibrium curve",
                                                                                                                             "Kobe plot",
                                                                                                                             "Management graphs",
                                                                                                                             "BSM prior, posterior distributions",
                                                                                                                             "Diagnostics",
                                                                                                                             "Retrospective (run retro first)"
                                                                                                                             ))),
                                                                                               tags$b("Select pic format"),
                                                                                               shinyWidgets::awesomeRadio(
                                                                                                 inputId = "format_picA",label = "pic format",
                                                                                                 choices = c("jpg", "png", "tiff","pdf"),selected = "jpg",
                                                                                                 inline = TRUE,checkbox = TRUE),
                                                                                               tags$b("Set width and height (cm)"),
                                                                                               sliderInput("width_pic_A", "width (cm)",
                                                                                                           min = 4, max = 30, value = 5,step=0.1,sep = ""),
                                                                                               sliderInput("height_pic_A", "height (cm)",
                                                                                                           min = 4, max = 30, value = 5,step=0.1,sep = ""),
                                                                                               tags$b("Define dpi"),
                                                                                               sliderInput("dpi_pic_A", "dpi",
                                                                                                           min = 150, max = 900, value = 300,step=50,sep = ""),
                                                                                               shinyWidgets::downloadBttn("download_pic_A", "Save graph",
                                                                                                                          color = "primary",style = "unite",
                                                                                                                          no_outline=F,block=F)),
                                                                              tags$hr(style = "border-top: 3px solid #000000;"),
                                                                              shinyWidgets::actionBttn(inputId="button_summary",label =" Create assessment summary",
                                                                                                       style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                       no_outline=F,block=F,color="primary"),
                                                                              
                                                                              ,width=12))),
                                  column(width = 8,align="center",
                                         shiny::fluidRow(
                                          # column(width = 6,
                                         column(width = 12,align="center",offset=2,
                                               conditionalPanel(condition = "input.Start_run",
                                                                   shinydashboard::box(collapsible = F,
                                                                                       shinycssloaders::withSpinner(shiny::plotOutput("rk_space")),
                                                                      shiny::h5("The graph shows as light grey dots the explored log r-k space. Black dots are viable r-k pairs found to be compatible with catch and CPUE data and the priors, with the most probable r-k pair indicted by the red cross (with CI).  The dotted rectangle indicates the prior r-k range."),
                                                                      width = 8)))),#),height = 250
                                         shiny::fluidRow(
                                           conditionalPanel(condition = "input.Start_run",
                                                            shinydashboard::box(collapsible = F,
                                                                                shinycssloaders::withSpinner(shiny::plotOutput("Catch_plot_final")),
                                                                                shiny::h5("The plot shows as blue curve the catch and as dashed horizontal line the estimate of MSY, both with indication of 95% CI. The dots indicate the catch data provided to the model."),
                                                                                width = 12))),
                                          shiny::fluidRow(
                                           conditionalPanel(condition = "input.Start_run",
                                           shinydashboard::box(collapsible = F,
                                                               shinycssloaders::withSpinner(shiny::plotOutput("Pred_biom_plot")),
                                                               shiny::h5("Plot of estimated biomass (red), with dotted lines indicating the 2.5th and 97.5th percentiles. Vertical purple lines indicate the prior biomass ranges and the grey dots indicate the scaled CPUE. The dashed horizontal line indicates Bmsy and the dotted line indicates Blim."),
                                                               width = 12))),#,height = 320
                                         shiny::fluidRow(
                                           conditionalPanel(condition = "input.Start_run",
                                           shinydashboard::box(collapsible = F,
                                                               shiny::plotOutput("Pred_FFmsy_plot"),
                                                               shiny::h5("Exploitation (F/Fmsy) as estimated (red), where the dashed horizontal line indicates Fmsy. The grey dots are the observed harvest rate scaled to results."),
                                                               width = 12))),#,height = 320
                                        # column(width = 12,align="center",#,offset=1
                                                shiny::fluidRow(
                                                  conditionalPanel(condition = "input.Start_run",
                                                  shinydashboard::box(collapsible = F,
                                                                      shinycssloaders::withSpinner(shiny::plotOutput("Parabola")),
                                                                      shiny::h5("Surplus production equilibrium parabola, with reduced productivity at small stock sizes. Overlaid are the corresponding modeled results for relative catch and stock size (red)."),
                                                                      width = 12))),
                                                shiny::fluidRow(
                                                  column(width = 12,align="center",offset=1,
                                                   conditionalPanel(condition = "input.Start_run",
                                                  shinydashboard::box(collapsible = F,
                                                                      shinycssloaders::withSpinner(shiny::plotOutput("kobe_plot")),
                                                                      shiny::h5("The Kobe plot combines the time series of stock size (B/Bmsy on the X-axis) and exploitation (F/Fmsy on the Y-axis). The colors identify combinations of stock size and exploitation as: Green = sustainable; Yellow = overfished; Orange = subject to overfishing; Red = overfished and subject to overfishing. The black line shows the time series of stock status and exploitation, and the shaded areas give the plausible confidence intervals for the last year as detailed in the legend."),
                                                                      width = 10))))#)
                                  )
                                ), 
                                shinyBS::bsModal(id='modalExample',
                                                 title ='Prior & posterior distributions',
                                                 trigger="PriorPosterior_trigger",
                                                 size = "large",
                                                 shinycssloaders::withSpinner(shiny::plotOutput('PriorPosterior')),
                                                 shiny::h5("Comparison of prior and posterior distributions for r, k, MSY and B/k. PPVR gives the ratio of posterior to prior variance: the smaller the ratio the stronger the update of the prior by the new data.")
                                ),
                                shinyBS::bsModal(id='popup_Page_1',
                                                 title ='Retrospective Graphs, It takes some time to do all the necessary calculations, so be patient!',
                                                 trigger="Retrospective",
                                                 size = "large",
                                                 shinycssloaders::withSpinner(shiny::plotOutput('retro_pics')),
                                                 shiny::h4("Retrospective analysis compares the latest predicted exploitation and stock status with those where one to several years have been removed from the time series. This shows how dependent the latest predictions are on the latest data (the larger the deviations, the stronger the dependence and the need for precaution).")
                                ),
                                shinyBS::bsModal(id='modalExample3',
                                                 title ='Diagnostics',
                                                 trigger="diag_trigger",
                                                 size = "large",
                                                 shinycssloaders::withSpinner(shiny::plotOutput('Diagnostics.plot')),
                                                 shiny::h5("Diagnostics showing (A) the fit of the predicted to the observed catch, (B) the fit of predicted to observed CPUE, (C) the deviation from observed to predicted biomass, and (D) an analysis of the log-CPUE residuals, which should preferably be randomly distributed, and which may have a red warning background if some residuals exceed value +/- 0.3, suggesting that stronger smoothing of cpue may be appropriate.")
                                                 ),
                                shinyBS::bsModal(id='modalExample4',
                                                 title ='Selected object',
                                                 trigger="see_obj",
                                                 size = "large",
                                                 shiny::tableOutput('see_object_table')),
                                shinyBS::bsModal(id='modalExample5',
                                                 title ='Management',
                                                 trigger="ppplot5",
                                                 size = "large",
                                                 shinycssloaders::withSpinner(shiny::plotOutput('Alexandros5')))),
        ################################################################
        ###################    PAGE 6 RUN FORECAST    ##################
        ################################################################
        shinydashboard::tabItem(tabName = "run_forecast",
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.Start_run",
                                                   column(width=4,
                                                          shinydashboard::box(title = "Forecast parameters", solidHeader = TRUE,status="primary",
                                                                              numericInput(inputId="N_prj_years", label="Number of projection years", value=5, min = 1, max = NA, step = 1),
                                                                              awesomeRadio(
                                                                                inputId = "interim_par",
                                                                                label = "Assumption for the interim year: Select the parameter to be equal with the last year of the timeseries.", 
                                                                                choices = c("F", "Catch"),
                                                                                selected = "F",
                                                                                status = "warning",
                                                                                inline = T,
                                                                                checkbox = T
                                                                              ), 
                                                                              
                                                                               awesomeRadio(
                                                                                inputId = "ForC",
                                                                                label = "Scenarios build on ", 
                                                                                choices = c("F", "Catch"),
                                                                                selected = "F",
                                                                                status = "primary",
                                                                                inline = T,
                                                                                checkbox = T
                                                                              ),
                                                                              numericInput(inputId="Sq_years", label="Number of years for estimating Status quo values", value=1, min = 1, max = 5, step = 1),
                                                                              width = 12)
                                                          # )
                                                   )),
                                  column(width = 8,#offset=2,
                                         conditionalPanel(condition = "input.Start_run",
                                                          shinydashboard::valueBoxOutput("Run_forecast_info1",width=6)
                                         )
                                         )),
                                shiny::fluidRow(
                                  column(width=4,align="center",
                                         conditionalPanel(condition = "input.Start_run",
                                                          shinyWidgets::actionBttn(inputId="Start_forecast",label =" Start",
                                                                                   style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                   no_outline=F,block=F,color="primary")),
                                         shinyBS::bsTooltip("Start_forecast", title="Press to run the forecast",
                                                            placement = "bottom", trigger = "hover",
                                                            options = NULL)),
                                  column(width=8,
                                         conditionalPanel(condition = "input.Start_forecast",
                                                          shinydashboard::box(title = "Assumptions for the interim year",align="center", solidHeader = TRUE,status="primary",collapsible = F,  
                                                                              textOutput("interim_text"),
                                                                              shinycssloaders::withSpinner(tableOutput("interim_assumptions")),
                                                                              width = 12)  
                                                          )
                                  )),
                                
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.Start_forecast",
                                  column(width=4,
                                         shinydashboard::box(title = "Manual scenarios", solidHeader = TRUE,status="primary",
                                    shinyWidgets::materialSwitch(
                                    inputId = "Manual_scenarios",
                                    label = "Insert scenarios manually",
                                    status = "primary"),
                                  conditionalPanel(condition = "input.Manual_scenarios",
                                                   
                                  numericInput(inputId="Scen_years", label="Number of scenarios", value=5, min = 1, max = 10, step = 1),
                                  tags$hr(style = "border-top: 1px solid #999797;"),
                                  tags$b("Select the multiplier for Fishing Mortality (Multiplier x Fmsy) or Catch (Multiplier x Status quo Catch)"),
                                  uiOutput("scenaria")
                                   )
                                  ,width = 12)
                                 # )
                                  ))),
                                shiny::fluidRow(
                                  column(width = 8,offset=4,
                                 shinydashboard::box(title = "Annual scenarios", solidHeader = TRUE,status="primary",align="center",
                                 shinycssloaders::withSpinner(tableOutput("advice_outcomes"))
                                 ,width = 12))
                                  ),
                                shiny::fluidRow(
                                  column(width = 4,
                                         conditionalPanel(condition = "input.Start_forecast",
                                         shinyWidgets::materialSwitch(
                                           inputId = "for_CI",
                                           label = "Show CI in figures",
                                           status = "primary"))
                                         ),
                                  column(width = 8,
                                         conditionalPanel(condition = "input.Start_forecast",
                                                          shinydashboard::box(collapsible = F,
                                                                              shinycssloaders::withSpinner(shiny::plotOutput("FFMSY_forecast")),
                                                                              shiny::h5("TEXT"),
                                                                              width = 12),
                                                          shinydashboard::box(collapsible = F,
                                                                              shinycssloaders::withSpinner(shiny::plotOutput("catch_forecast")),
                                                                              shiny::h5("TEXT"),
                                                                              width = 12),
                                                          shinydashboard::box(collapsible = F,
                                                                              shinycssloaders::withSpinner(shiny::plotOutput("bk_forecast")),
                                                                              shiny::h5("TEXT"),
                                                                              width = 12)
                                                          )
                                )
                                )
        ),
      
        shinydashboard::tabItem(tabName = "addit_info",
                                #tags$hr(style = "border-top: 2px solid #000000;"),
                                shiny::fluidRow(shiny::h3(tags$b("References:")),
                                                #shiny::h4(tagList("Froese R, Winker H, Coro G, Demirel N, Tsikliras AC, Dimarchopoulou D, Scarcella G, Palomares D, Dureuil M, Pauly D, 2020.", br(), "Estimating stock status from relative abundance and resilience. ICES Journal of Marine Science, 77(2): 527-538. https://doi.org/10.1093/icesjms/fsz230"))
                                ),
                                tags$hr(style = "border-top: 2px solid #000000;"),
                                shiny::fluidRow(shiny::h3(tags$b("Version:")),
                                                shiny::h4("v1.01")),
                                tags$hr(style = "border-top: 2px solid #000000;"),
                                shiny::fluidRow(shiny::h3(tags$b("Issue tracking:")),
                                                shiny::h4("touloumisk@inale.gr")),
                                tags$hr(style = "border-top: 2px solid #000000;"),
                                #tags$hr(style = "border-top: 2px solid #000000;"),
                                #tableOutput("test_orf"), ######################
                                shiny::fluidRow(shiny::h3(tags$b("ABC app team:"))),
                                tags$hr(style = "border-top: 2px solid #000000;")
        )
      )
    )
  )
  
  ############ ############ ############
  ############    SERVER    ############
  ############ ############ ############
  shinyServer=function(input, output, session){
    
  #  shinyBS::toggleModal(session, "modalExample", toggle = "toggle")
    
    Stock_obj <- reactiveValues()
    Stock_obj$Stocks=data.frame(ID=1:200,stock=rep(NA,200))
    Stock_obj$Catch_ID <- template_CATCH_ID
    Stock_obj$Catch_data <- template_CATCH_data
    shinyWidgets::useSweetAlert()
    
    ###### Create advise info boxes
    # url_fshbs= a("FishBase", href="https://www.fishbase.se/search.php",style = "color:black")
    # url_slfbs= a("SeaLifeBase", href="https://sealifebase.se/search.php",style = "color:black")
    # 
    
      output$adv_up1= shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Create stock object - step 1."),shiny::h5("Provide some basic information about your stock (Species info and Region info).",br(),
                                                             "In the blue boxes at the right side of the screen, we will show your inputs.",br(),
                                                             "Make sure that the scientific name matches exactly the scientific name used in FishBase or SeaLifeBase, because this name will be used to extract information for the priors.") , #",url_fshbs," or ",url_slfbs, " #If your species isn't in the dropdown list, type its name and press Add [Species Name]....
        icon = shiny::icon("dice-one"),
        color = "light-blue") })
    
    output$adv_up2=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Create stock object - step 2."),
        shiny::h5("Define the type of abundance data available. ",#Even if you use biomass from previous models, it is often better to treat it as CPUE.
                  "Load your catch and CPUE from a comma-delimited (.csv) file.",
                  "That file should contain at least one column for Year, catch and, cpue/biomass.",
                  "Use the options in the box to help with loading the data.",br(),
                  "Input a stockID and press 'Create stock object' button."),
        icon = shiny::icon("dice-two"),
        color = "light-blue") })
    
    output$adv_up4=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Create stock object - step 3."),shiny::h5("The third step is give a unique name to your stock." ,br(),
                                                             "If everything is ok, press 'Create stock object' button, to finish data enter."),
        icon = shiny::icon("dice-three"),
        color = "light-blue") })
    
    output$adv_expl1=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Explore stock object"),shiny::h5("In this section you can explore stocks stored in our database.",br(),
                                                    "Select a stock from the table and then press the 'Create stock object' button.",br(),
                                                    "You can search the database from 'Explore existing stocks' box at the left side of the screen.") ,
        icon = shiny::icon("envelope"),
        color = "light-blue") })
    
    ###### Update select tools

    observe({
      updateTextInput(session, "Sp_Comm", value = species_DB$FBname[species_DB$Species==input$Sp_Scient])
    })
    
    Common_name=reactive({
      xx=input$Sp_Comm
    })
    
    ###CREATE box(title = "Selected Species Info"
    output$Sps_Info=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Species info"),shiny::h5(HTML(paste0("Scientific name: ", tags$b(tags$em(input$Sp_Scient)),br(),br(),
                                                        "Common name: ",tags$b(input$Sp_Comm),br(),br(),
                                                        "Species group: ", tags$b(input$Sp_Group) ))),
        icon = shiny::icon("file-alt"),
        color = "light-blue") })
    ###CREATE box(title = Selected Region Info"
    output$Area_Info=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Region info"),shiny::h5(HTML(paste0("Continent: ", tags$b(input$Area_Cont),br(),br(),
                                                       "Region: ",tags$b(input$Area_Reg),br(),br(),
                                                       "Sub-region: ", tags$b(input$Area_SbReg),br(),br(),
                                                       "Comments: ", tags$b(input$Area_comm)))),
        icon = shiny::icon("file-alt"),
        color = "light-blue")})
    
    ###CREATE UPLOAD DATA
    inp_data <- reactive({
      req(input$file1)
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,#
                    quote = input$quote) #
      is.num <- sapply(df, is.numeric)
      df[is.num] <- lapply(df[is.num], round,2)
      observe({
        temp_df<- df
        shinyWidgets::updatePickerInput(session, "Yr_col", choices = names(temp_df),
                                        selected ="")
        shinyWidgets::updatePickerInput(session, "ct_col", choices = names(temp_df),
                                        selected ="")
        shinyWidgets::updatePickerInput(session, "bt_col", choices = names(temp_df),
                                        selected ="")
         shinyWidgets::updatePickerInput(session, "cv_col", choices = names(temp_df),
                                        selected ="")
      })
      return(df)
    })
    output$contents <-  DT::renderDataTable({ inp_data()
      DT::datatable( inp_data(),
                     options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
    })
    ######DES AYTO
    observeEvent(input$button_2,{
      if(input$prepare_data > 0 ) {
        shinyWidgets::sendSweetAlert(
          session = session,title = "Warning",
          text = "If you have already run an analysis for a stock and you want to run a new one for another stock, it is advised to reload the app and start from scratch.", type = "warning")
      }
      
      if(input$Crtd_StckID_input=="") {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Input a stock ID!",
          type = "error")
      }  else {
        if(input$button_2 > 0) {
          newLine1 <- isolate(c(input$Area_Cont,
                                input$Area_Reg,
                                input$Area_SbReg,
                                input$Crtd_StckID_input,
                                input$Sp_Group,
                                Common_name(),
                                input$Sp_Scient,
                                NA,
                                min(as.integer(inp_data()[,input$Yr_col]),na.rm = T),
                                max(as.integer(inp_data()[,input$Yr_col]),na.rm = T),
                                rep(NA,10),
                                "None",#ifelse(input$is_there_biomass=="Catch & Biomass",input$btype,)
                                rep(NA,5),
                                "created"
          ))
          if (all(is.na(Stock_obj$Stocks$stock))){
            Stock_obj$Stocks$stock[1]=newLine1[4]
            Stock_obj$Catch_ID[1,]=c(newLine1,1)
          } else {# if(newLine1[4] %!in% Stock_obj$Stocks$stock) {
            Stock_obj$Stocks$stock[length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)])+1]=newLine1[4]
            Stock_obj$Catch_ID[length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)]),]=c(newLine1,length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)]))
          }
        }
      }
    })
    
    observeEvent(input$button_2,{
      if(input$Crtd_StckID_input=="") {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Input a stock ID!",
          type = "error")
      }  else {
        if(input$button_2 > 0) {
          # if(input$do_u_have_cv=="Yes" & all(is.na(as.numeric(inp_data()[,input$cv_col])))) {
          #   shinyWidgets::sendSweetAlert(
          #     session = session,title = "Warning",
          #     text = "Your dataset cv column is empty. As a result the cpue CV is set automatically to 0.2. If you want to have the option to change the cpue CV, choose 'No' after the 'Can you provide CV for CPUE/Biomass?' question", type = "warning")
          # }
          if (input$do_u_have_cv=="Yes"){
            newLine_catch_data <- isolate(data.frame(
              Stock= rep(input$Crtd_StckID_input,length(inp_data()[,input$Yr_col])),
              yr=as.integer(inp_data()[,input$Yr_col]),
              ct=as.numeric(inp_data()[,input$ct_col]),
              bt= as.numeric(inp_data()[,input$bt_col]),
              ct_smthd= rep(NA,length(inp_data()[,input$Yr_col])),
              bt_iterp= rep(NA,length(inp_data()[,input$Yr_col])),
              bt_smthd= rep(NA,length(inp_data()[,input$Yr_col])),
              ecreep= rep(NA,length(inp_data()[,input$Yr_col])),
              bt_cv=  as.numeric(inp_data()[,input$cv_col]),
              Stock_objID=rep(Stock_obj$Catch_ID$Stock_objID[Stock_obj$Catch_ID$Stock==input$Crtd_StckID_input],length(inp_data()[,input$Yr_col]))
            ))} else if (input$do_u_have_cv=="No") {
              newLine_catch_data <-isolate(data.frame(
                Stock= rep(input$Crtd_StckID_input,length(inp_data()[,input$Yr_col])),
                yr=as.integer(inp_data()[,input$Yr_col]),
                ct=as.numeric(inp_data()[,input$ct_col]),
                bt= as.numeric(inp_data()[,input$bt_col]),
                ct_smthd= rep(NA,length(inp_data()[,input$Yr_col])),
                bt_iterp= rep(NA,length(inp_data()[,input$Yr_col])),
                bt_smthd= rep(NA,length(inp_data()[,input$Yr_col])),
                ecreep= rep(NA,length(inp_data()[,input$Yr_col])),
                bt_cv=  rep(NA,length(inp_data()[,input$Yr_col])),
                Stock_objID=rep(Stock_obj$Catch_ID$Stock_objID[Stock_obj$Catch_ID$Stock==input$Crtd_StckID_input],length(inp_data()[,input$Yr_col]))
              ))
            } 
          }
        Stock_obj$Catch_data <- rbind(Stock_obj$Catch_data, newLine_catch_data)
        }
    })
    
    observeEvent(input$Upld_butt_1,{   #TRICK to erase things
     # shinyWidgets::updateMaterialSwitch(session, "Upld_butt_2", value = F)
      shinyWidgets::updateMaterialSwitch(session, "Upld_butt_3", value =F)
    })
    
    
    observeEvent(input$button_1,{   #TRICK to erase things
      shinyWidgets::updatePrettySwitch(session, "upMaterial4", value = F)
      shinyWidgets::updatePrettySwitch(session, "ecreepyes", value = F)
      shinyWidgets::updatePrettySwitch(session, "upMaterial_bt", value = F)
      shinyWidgets::updatePrettySwitch(session, "upMaterial1", value = F)
      shinyWidgets::updatePrettySwitch(session, "upMaterialccv", value = F)
      shinyWidgets::updatePrettySwitch(session, "upMaterial2", value = F)
      shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
      shinyWidgets::updateMaterialSwitch(session, "Priors_but2", value = F)
    })
    
    
    observeEvent(input$button_2,{   #TRICK to erase things
      shinyWidgets::updatePrettySwitch(session, "upMaterial4", value = F)
      shinyWidgets::updatePrettySwitch(session, "ecreepyes", value = F)
      shinyWidgets::updatePrettySwitch(session, "upMaterial_bt", value = F)
      shinyWidgets::updatePrettySwitch(session, "upMaterial1", value = F)
      shinyWidgets::updatePrettySwitch(session, "upMaterialccv", value = F)
      shinyWidgets::updatePrettySwitch(session, "upMaterial2", value = F)
      shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
      shinyWidgets::updateMaterialSwitch(session, "Priors_but2", value = F)
    })
    
    output$Stock_infobox_1=shinydashboard::renderValueBox({
      shinydashboard::valueBox(shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the stock object ", input$Crtd_StckID_input,br(),
                                                                           " of the species ",tags$em(input$Sp_Scient), ".",br(),
                                                                           "You can know proceed to the 'Prepare data' tab."))),
                               shiny::icon("thumbs-up", lib = "glyphicon"),
                               color = "light-blue") })
    
    output$Catch_plot2= shiny::renderPlot({
      ggplot2::ggplot(data=Stock_obj$Catch_data[Stock_obj$Catch_data$Stock==input$Crtd_StckID_input,], ggplot2::aes(x=yr, y=ct)) +
        ggplot2::geom_line(color="blue",size=1)+ggplot2::labs(y="Catch", x="Year")+
        ggplot2::geom_point(color="black",size=2)+ggplot2::theme_classic()+
        ggplot2::scale_y_continuous(limits=c(0,NA))
    }, height = 200)

    output$Biomass_plot2= shiny::renderPlot({
      ggplot2::ggplot(data=Stock_obj$Catch_data[Stock_obj$Catch_data$Stock==input$Crtd_StckID_input,], ggplot2::aes(x=yr, y=bt)) +
        ggplot2::geom_line(color="blue",size=1)+ggplot2::labs(y="CPUE", x="Year")+
        ggplot2::geom_point(color="black",size=2)+ggplot2::theme_classic()+
        ggplot2::scale_y_continuous(limits=c(0,NA))
    }, height = 200)

    
    ###### EXPLORE EXISTING STOCKS
    res_mod <- datamods::select_group_server(   #callModule(module = 
      id = "my_filters",
     # inline = FALSE,
      data_r = test_CATCH_ID,
      vars_r = c("Continent", "Region", "Subregion", "Group", "ScientificName")
    )
    
    output$Exist_Stcks_tble <- DT::renderDataTable({
      DT::datatable(res_mod()[,c("Continent", "Region","ScientificName","Stock")],
                    selection = 'single', options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
    })
    
    observe({
      updateTextInput(session, "txt1",value=res_mod()[input$Exist_Stcks_tble_rows_selected,"Stock"])
    })
    
    output$txtout1=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Info"),shiny::h5(test_CATCH_ID_All$Name[test_CATCH_ID_All$Stock==input$txt1]) ,
        icon = shiny::icon("info-circle"),
        color = "aqua") })
    
    output$txtout2=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Previous user-provided settings"),shiny::h5(if (input$txt1=="") {
          "Select a stock"} else {HTML(paste(tags$b("Start, End year: "), test_CATCH_ID_All$StartYear[test_CATCH_ID_All$Stock==input$txt1], "-",
                                             test_CATCH_ID_All$EndYear[test_CATCH_ID_All$Stock==input$txt1],tags$br(),
                                             tags$b("Resilience, r priors: "),test_CATCH_ID_All$Resilience[test_CATCH_ID_All$Stock==input$txt1],
                                             ", ",test_CATCH_ID_All$r.low[test_CATCH_ID_All$Stock==input$txt1],"-",
                                             test_CATCH_ID_All$r.hi[test_CATCH_ID_All$Stock==input$txt1],tags$br(),
                                             if( !is.na(test_CATCH_ID_All$stb.low[test_CATCH_ID_All$Stock==input$txt1]) | !is.na(test_CATCH_ID_All$stb.hi[test_CATCH_ID_All$Stock==input$txt1])) {
                                               paste(tags$b("start B/k priors: "), test_CATCH_ID_All$stb.low[test_CATCH_ID_All$Stock==input$txt1], "-",
                                                     test_CATCH_ID_All$stb.hi[test_CATCH_ID_All$Stock==input$txt1],tags$br())},
                                             if( !is.na(test_CATCH_ID_All$intb.low[test_CATCH_ID_All$Stock==input$txt1]) | !is.na(test_CATCH_ID_All$intb.hi[test_CATCH_ID_All$Stock==input$txt1]) |
                                                 !is.na(test_CATCH_ID_All$int.yr[test_CATCH_ID_All$Stock==input$txt1])) {
                                               paste(tags$b("Intermediate B/k priors: "), test_CATCH_ID_All$intb.low[test_CATCH_ID_All$Stock==input$txt1], "-",
                                                     test_CATCH_ID_All$intb.hi[test_CATCH_ID_All$Stock==input$txt1], ", ",  test_CATCH_ID_All$int.yr[test_CATCH_ID_All$Stock==input$txt1],tags$br())},
                                             if( !is.na(test_CATCH_ID_All$endb.low[test_CATCH_ID_All$Stock==input$txt1]) | !is.na(test_CATCH_ID_All$endb.hi[test_CATCH_ID_All$Stock==input$txt1])) {
                                               paste(tags$b("End B/k priors: "),test_CATCH_ID_All$endb.low[test_CATCH_ID_All$Stock==input$txt1], "-",
                                                     test_CATCH_ID_All$endb.hi[test_CATCH_ID_All$Stock==input$txt1], sep="")}
          ))
          }),
        icon = shiny::icon("info-circle"),
        color = "aqua") })
    
    output$txtout3=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Source"),shiny::h5(test_CATCH_ID_All$Source[test_CATCH_ID_All$Stock==input$txt1]) ,
        icon = shiny::icon("info-circle"),
        color = "aqua") })
    
    output$txtout4=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Additional info"),shiny::h5(test_CATCH_ID_All$Comments[test_CATCH_ID_All$Stock==input$txt1]) ,
        icon = shiny::icon("info-circle"),
        color = "aqua") })
    
    output$Catch_plot= shiny::renderPlot({
      validate(
        need(input$txt1 %in% test_CATCH_data$Stock, 'Choose a valid Stock from the last column of the above Data Table')
      )
      ggplot2::ggplot(data=test_CATCH_data[test_CATCH_data$Stock==input$txt1 & test_CATCH_data$ct>0,], ggplot2::aes(x=yr, y=ct, group=1)) +
        ggplot2::geom_line(color="blue",size=1)+ggplot2::labs(y="Catch", x="Year")+
        ggplot2::geom_point(color="black",size=2)+ggplot2::theme_classic()+
        ggplot2::scale_y_continuous(limits=c(0,NA)) 
    }, height = 200)
    
    output$Biomass_plot= shiny::renderPlot({
      validate(
        need(input$txt1 %in% test_CATCH_data$Stock, 'Choose a valid Stock from the last column of the above Data Table')
      )
      p2= ggplot2::ggplot(data=test_CATCH_data[test_CATCH_data$Stock==input$txt1 & test_CATCH_data$bt>0,], ggplot2::aes(x=yr, y=bt, group=1)) +
        ggplot2::geom_line(color="blue",size=1)+ggplot2::labs(y="CPUE", x="Year")+
        ggplot2::geom_point(color="black",size=2)+ggplot2::theme_classic()+
        ggplot2::scale_y_continuous(limits=c(0,NA)) 
      p2
    }, height = 200)
    
    observeEvent(input$button_1,{
      if(input$procc_rpriors > 0 ) {
        shinyWidgets::sendSweetAlert(
          session = session,title = "Warning",
          text = "If you have already run an analysis for a stock and you want to run a new one for another stock, it is advised to reload the app and start from scratch.", type = "warning")
      }
      if(input$button_1 > 0) {
        if(input$txt1 %!in% test_CATCH_ID_All$Stock) {
          shinyWidgets::sendSweetAlert(
            session = session,title = "Error...",
            text = "Oups !", type = "error")
        } else {
          newLine1 <- isolate(c(as.vector(test_CATCH_ID_All[test_CATCH_ID_All$Stock==input$txt1,1:26]),"selected"))
          if (all(is.na(Stock_obj$Stocks$stock))){
            Stock_obj$Stocks$stock[1]=input$txt1
            Stock_obj$Catch_ID[1,]=c(newLine1,1)
          } else {#if(input$txt1 %!in% Stock_obj$Stocks$stock)
            Stock_obj$Stocks$stock[length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)])+1]=newLine1[4]
            Stock_obj$Catch_ID[length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)]),]=c(newLine1,length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)]))
          }
        }
      }
    })
    #output$TESTNEURAL=renderTable({k_priors()[["rkpriors"]]})
    
    observeEvent(input$button_1,{
      if(input$button_1 > 0) {
        if(input$txt1 %!in% test_CATCH_ID_All$Stock) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error...",
            text = "Oups !",
            type = "error"
          )
        } else {
          newLine_catch_data <- isolate(cbind(
            test_CATCH_data[test_CATCH_data$Stock==input$txt1,1:9],
            Stock_objID=rep(Stock_obj$Catch_ID$Stock_objID[Stock_obj$Catch_ID$Stock_objID==max(Stock_obj$Catch_ID$Stock_objID)],
                            nrow(test_CATCH_data[test_CATCH_data$Stock==input$txt1,])))
          )
          Stock_obj$Catch_data <- rbind(Stock_obj$Catch_data, newLine_catch_data)
          # }
        }}
    })
    #output$see_stockID=renderTable({Stock_obj$Catch_ID})
    
    output$Stock_infobox_2=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the stock object ", test_CATCH_ID_All[test_CATCH_ID_All$Stock==input$txt1,"Stock"],
                                                    " of the species ",tags$em(test_CATCH_ID_All[test_CATCH_ID_All$Stock==input$txt1,"ScientificName"]), ".",br(),
                                                    "You can know proceed to the 'Prepare data' tab."))),
        shiny::icon("thumbs-up", lib = "glyphicon"),
        color = "light-blue") })
    
    # AEK=reactive({
    #    controp=tryCatch(Selected_stck1()[["Catch_ID"]][1,"ScientificName"], error=function(err) NA)
    #  })
  
    ################################################################
    ###################### PAGE 4 PREPARE DATA  #####################
    ################################################################
    
    #########SELECT STOCK TO WORK WITH PRIORS
    
    Final_stock=eventReactive(input$prepare_data,{
      final_stock=list(Catch_ID=Stock_obj$Catch_ID[Stock_obj$Catch_ID$Stock_objID==max(Stock_obj$Catch_ID$Stock_objID),],
                       Catch_Data=Stock_obj$Catch_data[Stock_obj$Catch_data$Stock_objID==max(Stock_obj$Catch_ID$Stock_objID),])
      final_stock[["Catch_ID"]]["ScientificName"]=gsub("_"," ",final_stock[["Catch_ID"]]["ScientificName"])
      final_stock[["Catch_Data"]]["bt"][final_stock[["Catch_Data"]]["bt"]==0]=NA
      final_stock[["Catch_Data"]]=final_stock[["Catch_Data"]][!is.na(final_stock[["Catch_Data"]]$yr),]
          return(final_stock)
    })
    
    Final_stock <- Final_stock %>% debounce(1000)
    
    output$Stock_infobox_prepare=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Stock object"),shiny::h5(HTML(paste0("You have selected the stock ",
                                                        tags$b(Final_stock()[["Catch_ID"]][1,"Stock"]), " of the species ",
                                                        tags$b(tags$em(Final_stock()[["Catch_ID"]][1,"ScientificName"]))
        ))),
        icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "light-blue") })
    
    observe({   #TRICK to erase things
      req(Final_stock())
      mnyr=min(Final_stock()[["Catch_Data"]][c("yr","ct")]$yr,na.rm = T)
      mxyr= max(Final_stock()[["Catch_Data"]][c("yr","ct")]$yr,na.rm = T)
      # end.yr    =Final_stock()[["Catch_Data"]][c("yr","bt")]$yr[max(which(Final_stock()[["Catch_Data"]][c("yr","bt")]$bt>0))]
      updateSliderInput(session, "yr_slct", value = c(mnyr,mxyr),min = mnyr, max =mxyr)
    })
    
    
    observe({   #TRICK to erase things
      req(Final_stock())
      req(input$yr_slct)
      
      mnyr=input$yr_slct[1]
      start.yr.bio  =Final_stock()[["Catch_Data"]][c("yr","bt")]$yr[which(Final_stock()[["Catch_Data"]][c("yr","bt")]$bt>0)[1]]
      if (mnyr>=start.yr.bio) {
        AAA=mnyr} else {
          AAA=start.yr.bio
        }
      end.yr    =Final_stock()[["Catch_Data"]][c("yr","bt")]$yr[max(which(Final_stock()[["Catch_Data"]][c("yr","bt")]$bt>0))]
      updateSliderInput(session, "bio_yr_slct", value = c(AAA,end.yr) ,min = AAA, max =end.yr)
    })
    
    
    observe({
         updateSliderInput(session, "ecreep_year", value =input$bio_yr_slct[1],min =input$bio_yr_slct[1], max =input$bio_yr_slct[2]-1)
    })
    
  #  url_palom= a("Link", href="https://doi.org/10.5751/ES-11136-240331")
    output$help_ecreep=renderUI({ shiny::h5(tagList("Note: Over time, fishers become more efficient at catching fish; a 2% increase in catchabilty per year is common (Palomares and Pauly 2019), https://doi.org/10.5751/ES-11136-240331",))})# url_palom
    
    Catch_obj=reactive({
      req(Final_stock())
      catch_obj=Final_stock()[["Catch_Data"]][,c("Stock_objID","Stock","yr","ct")]
      minyr=min(catch_obj$yr[!is.na(catch_obj$ct)])
      maxyr=max(catch_obj$yr[!is.na(catch_obj$ct)])
      ct_obj=data.frame(yr=seq(minyr,maxyr,1))
      ct_obj=merge(x = ct_obj, y = catch_obj, all.x = TRUE)
      ct_obj=ct_obj[ct_obj$yr>=input$yr_slct[1],]
      ct_obj=ct_obj[ct_obj$yr<=input$yr_slct[2],]
      ct_obj$ct_smthd = ksmooth(x=ct_obj$yr,y=ct_obj$ct,kernel="normal",n.points=length(ct_obj$yr),bandwidth=input$bw_inpt)$y
      return(ct_obj)
    })
    
    bio_obj=reactive({
      req(Final_stock())
      #  req(input$procc_rpriors==T)
      if (all(is.na(Final_stock()[["Catch_Data"]]["bt"]))) {
        temp_bio_obj=Final_stock()[["Catch_Data"]][c("yr","bt","bt_iterp","bt_smthd","ecreep","bt_cv")]
        temp_bio_obj$bt_iterp=NA
        temp_bio_obj$bt_smthd=NA
        temp_bio_obj$ecreep=NA
      } else {
        temp_bio_obj=Final_stock()[["Catch_Data"]][c("yr","bt","bt_iterp","bt_smthd","ecreep","bt_cv")]
        start.yr=input$bio_yr_slct[1]
        end.yr<-input$bio_yr_slct[2]
        temp_bio_obj=temp_bio_obj[temp_bio_obj$yr>=start.yr & temp_bio_obj$yr<=end.yr,]
        temp_bio_obj=temp_bio_obj[temp_bio_obj$yr <= input$yr_slct[2],]
        temp_bio_obj$bt_iterp=imputeTS::na_interpolation(temp_bio_obj$bt, option="linear") #"spline" , "stine"
        temp_bio_obj$bt_smthd=ksmooth(x=temp_bio_obj$yr,y=temp_bio_obj$bt_iterp,kernel="normal",n.points=length(temp_bio_obj$yr),bandwidth=input$bw_inpt_bt)$y ####set bdwd
     
      if (input$ecreepyes==F) {
        temp_bio_obj=temp_bio_obj} else {
          temp_ecreep=temp_bio_obj[c("yr","bt","ecreep")]
          maxyr=max(as.integer(temp_ecreep$yr[!is.na(temp_ecreep$bt)]))
          temp_ecreep$ecreep=temp_ecreep$bt

          for(i in 1:(maxyr- as.integer(input$ecreep_year))) {
            temp_ecreep$ecreep[temp_ecreep$yr==(input$ecreep_year+i)]  <-
              temp_ecreep$bt[temp_ecreep$yr==(input$ecreep_year+i)]*(1-input$ecreepslider/100)^i # equation for decay in %; first cpue without correction
          } 
          temp_bio_obj$ecreep=temp_ecreep$ecreep
          temp_bio_obj$bt_iterp=imputeTS::na_interpolation(temp_bio_obj$ecreep, option="linear") #"spline" , "stine"
          temp_bio_obj$bt_smthd=ksmooth(x=temp_bio_obj$yr,y=temp_bio_obj$bt_iterp,kernel="normal",n.points=length(temp_bio_obj$yr),bandwidth=input$bw_inpt_bt)$y ####set bdwd
        } }

      return(temp_bio_obj)
    })
   
   bio_obj <- bio_obj %>% debounce(500)
     
    output$When_happy=shinydashboard::renderValueBox({
      shinydashboard::valueBox( shiny::h4("Message"),shiny::h4("When you are happy with data preparation, move on to the '3. Priors' tab where you can also save the modified data."),
                                icon = shiny::icon("envelope"),color = "light-blue")})
    
       MSY_calculated=reactive({
      req(Catch_obj())
      msy=MSY_calculator(Catch_obj())
    })
       
    output$Check_Catch_plot= shiny::renderPlot({
      lower_cv=(1- input$Catch_CV)*Catch_obj()$ct_smthd
      upper_cv=(1+ input$Catch_CV)*Catch_obj()$ct_smthd
      if (input$upMaterialccv==F) {  
        p_1= ggplot2::ggplot(data=Catch_obj(),ggplot2::aes(x=yr)) +
          ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(Catch_obj()$ct,upper_cv),na.rm = T)))+
          ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))+
          ggplot2::geom_line(ggplot2::aes(y=ct_smthd, color="A"),size=1)+
          ggplot2::geom_line(ggplot2::aes(y=ct,color="B"),size=1)+
         # geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "A"),linetype="dashed", alpha=0.3)+
          ggplot2::geom_point(ggplot2::aes(y=ct),color="black",size=1)+
          ggplot2::theme_classic()+
          ggplot2::scale_color_manual(labels=c("Smoothed catch","Reported catch"),values=c("red","blue"))+
          ggplot2::scale_fill_manual(labels=c("CI of (smoothed) catch based on CV"),values=c("red"))+
          ggplot2::theme(legend.position="bottom")+ggplot2::labs(y="Catch", x="Year",title="Catch data",color="",fill="") 
        } else {
      p_1= ggplot2::ggplot(data=Catch_obj(),ggplot2::aes(x=yr)) +
        ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(Catch_obj()$ct,upper_cv),na.rm = T)))+
        ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))+
        ggplot2::geom_line(ggplot2::aes(y=ct_smthd, color="A"),size=1)+
        ggplot2::geom_line(ggplot2::aes(y=ct,color="B"),size=1)+
        ggplot2::geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "A"),linetype="dashed", alpha=0.3)+
        ggplot2::geom_point(ggplot2::aes(y=ct),color="black",size=1)+
        ggplot2::theme_classic()+
        ggplot2::scale_color_manual(labels=c("Smoothed catch","Reported catch"),values=c("red","blue"))+
        ggplot2::scale_fill_manual(labels=c("CI of (smoothed) catch based on CV"),values=c("red"))+
        ggplot2::theme(legend.position="bottom")+ggplot2::labs(y="Catch", x="Year",title="Catch plot",color="",fill="")
     } 
      print(p_1)
    })#, height = 340

    output$Check_Biomass_plot= shiny::renderPlot({
       validate(
   need(!all(is.na(bio_obj()$bt)), 'No CPUE data available')
   )
      bio_obj_temp= bio_obj()  
      if (input$do_u_have_cv=="No") {
        lower_cv=(1- input$CPUE_CV)*bio_obj()$bt_smthd
        upper_cv=(1+ input$CPUE_CV)*bio_obj()$bt_smthd
      } else  if (input$do_u_have_cv=="Yes"){
        bio_obj_temp$lower_cv=(1- as.numeric(bio_obj()$bt_cv))*bio_obj()$bt_smthd
        bio_obj_temp$upper_cv=(1+ as.numeric(bio_obj()$bt_cv))*bio_obj()$bt_smthd
        lower_cv=bio_obj_temp$lower_cv
        upper_cv=bio_obj_temp$upper_cv
      }
        
     if (input$do_u_have_cv=="No" & input$upMaterialbcv==F) { 
       p_1= ggplot2::ggplot(data=bio_obj(),ggplot2::aes(x=yr)) +
         ggplot2::geom_line(ggplot2::aes(y=bt_smthd, color="B"),size=1)+
         ggplot2::geom_line(ggplot2::aes(y=bt,color="A"),size=1)+
         ggplot2::labs(y="CPUE", x="Year",title="CPUE data",color="",fill="")+
         ggplot2::geom_point(ggplot2::aes(y=bt),color="black",size=1)+ggplot2::theme_classic()+
         ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE"),values=c("blue","red"))+
         ggplot2::theme(legend.position="bottom") +
         ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(bio_obj()$bt,upper_cv),na.rm = T)))+
         ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))
       if (input$ecreepyes==T ) {#|input$ecreepslider>0
         validate(
           need(!all(is.na(bio_obj()$ecreep)), 'Processing...')
         )
         validate(
           need(input$ecreep_year>=min(bio_obj()$yr), 'Processing...')
         )
           p_1= ggplot2::ggplot(data=bio_obj(),ggplot2::aes(x=yr)) +
           ggplot2::geom_line(ggplot2::aes(y=bt_smthd, color="B"),size=1)+
           # geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "cl on smoothed CPUE based on CV"),linetype="dashed", alpha=0.3)+
           ggplot2::geom_line(ggplot2::aes(y=bt,color="A"),size=1)+
           ggplot2::theme_classic()+
           ggplot2::theme(legend.position="bottom") +
           ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(bio_obj()$bt,upper_cv),na.rm = T)))+
           ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))+
           ggplot2::geom_line(ggplot2::aes(x=yr,y=ecreep, color="C"),size=1)+
           ggplot2::geom_point(ggplot2::aes(x=input$ecreep_year,y=bio_obj()$bt[bio_obj()$yr==input$ecreep_year],shape="E-creep effect start year"),size=4)+
           #ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(upper_cv,bio_obj()$bt), na.rm = T)))+
            ggplot2::labs(y="CPUE", x="Year",title="CPUE data",color="",fill="",shape="")+
           ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE","E-creeped CPUE" ),
                                       values=c("blue","red","green"))}
       
       }  else if (input$do_u_have_cv=="Yes" | input$upMaterialbcv==T) {#  
        p_1= ggplot2::ggplot(data=bio_obj(),ggplot2::aes(x=yr)) +
          ggplot2::geom_line(ggplot2::aes(y=bt_smthd, color="B"),size=1)+
          geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "cl on smoothed CPUE based on CV"),linetype="dashed", alpha=0.3)+
          ggplot2::geom_line(ggplot2::aes(y=bt,color="A"),size=1)+
          ggplot2::geom_point(ggplot2::aes(y=bt),color="black",size=1)+ggplot2::theme_classic()+
          # ggplot2::geom_label(ggplot2::aes(x=min(yr)+4,y=1.2,label =paste0("q_low=", sprintf('%0.3f',q_priors()[1]), ", q_high=",sprintf('%0.3f',q_priors()[2]))),col="#F37B59", alpha=0.2)+
          ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE"),values=c("blue","red"))+
          ggplot2::theme(legend.position="bottom") +
          ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(bio_obj()$bt,upper_cv),na.rm = T)))+
                    ggplot2::labs(y="CPUE", x="Year",title="CPUE data",color="",fill="",shape="")+
          ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))
      if (input$ecreepyes==T ) {#|input$ecreepslider>0
        validate(
          need(!all(is.na(bio_obj()$ecreep)), 'Processing...')
        )
        validate(
          need(input$ecreep_year>=min(bio_obj()$yr), 'Processing...')
        )
        p_1=ggplot2::ggplot(data=bio_obj(),ggplot2::aes(x=yr)) +
          ggplot2::geom_line(ggplot2::aes(y=bt_smthd, color="B"),size=1)+
          geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "cl on smoothed CPUE based on CV"),linetype="dashed", alpha=0.3)+
          ggplot2::geom_line(ggplot2::aes(y=bt,color="A"),size=1)+
          ggplot2::geom_point(ggplot2::aes(y=bt),color="black",size=1)+ggplot2::theme_classic()+
          # ggplot2::geom_label(ggplot2::aes(x=min(yr)+4,y=1.2,label =paste0("q_low=", sprintf('%0.3f',q_priors()[1]), ", q_high=",sprintf('%0.3f',q_priors()[2]))),col="#F37B59", alpha=0.2)+
          #ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE"),values=c("blue","red"))+
          ggplot2::theme(legend.position="bottom") +
         # ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(bio_obj()$bt,upper_cv),na.rm = T)))+
          ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))+
          ggplot2::geom_line(ggplot2::aes(x=yr,y=ecreep, color="C"),size=1)+
          ggplot2::geom_point(ggplot2::aes(x=input$ecreep_year,y=bio_obj()$bt[bio_obj()$yr==input$ecreep_year],shape="E-creep effect start year"),size=4)+
          ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(upper_cv,bio_obj()$bt), na.rm = T)))+
                    ggplot2::labs(y="CPUE", x="Year",title="CPUE data",color="",fill="",shape="")+
           ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE","E-creeped CPUE" ),
                                      values=c("blue","red","green"))}
       }
      print(p_1)
    })

    ################################################################
    ###################### PAGE 4 PRIOR ENTRY  #####################
    ################################################################
    
    #########SELECT STOCK TO WORK WITH PRIORS
    output$Stock_infobox_patience=shinydashboard::renderValueBox({
      shinydashboard::valueBox( shiny::h4("Start priors processing"),shiny::h5("Press 'Start' to enter resilience and r priors. You will also have the option to press 'Connect' button to connect to FishBase or SeaLifeBase
                                                                             for extraction of prior information. You will then have the 
                                                                             option to edit that information. Be patient since establishing the 
                                                                             connection may take a moment. Wait until the 'FishBase/SeaLifeBase info' box appears. If there is no Internet connection, a message will appear and you can enter the r-prior manually.
                                                                             Don't forget to press the 'Save input so far' button when you're done."),
                                icon = shiny::icon("envelope"),color = "light-blue")})
    
    output$Stock_infobox_3=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Stock object"),shiny::h5(HTML(paste0("You have selected the stock ",
                                                        tags$b(Final_stock()[["Catch_ID"]][1,"Stock"]), " of the species ",
                                                        tags$b(tags$em(Final_stock()[["Catch_ID"]][1,"ScientificName"])),"."
        ))),
        icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "light-blue") })

    Fishbase_text=eventReactive(input$con_fbase,{
      req(Final_stock())
      results= fbsb(Final_stock()[["Catch_ID"]][1,"ScientificName"])
      return(results)
    })
    
    url_r=eventReactive(input$con_fbase,{
      url= a("Link", href=Fishbase_text()[6],style = "color:black") })
    
    output$FB_resil=shinydashboard::renderValueBox({
      if (is.na(Fishbase_text()[6])) {
        shinydashboard::valueBox(
          shiny::h4("FishBase/SeaLifeBase info"),"We couldn't find information in FishBase/SeaLifeBase. Check species name spelling or consider to search manually. If no information on resilience or r-range is available in FishBase/SeaLifeBase, using Medium resilience for 'regular' species and Low resilience for large or deep-water species is a reasonable starting point. Very low resilience is found in very large species and High resilience is found in species which reproduce in their first year. The general receiving e-mail of FishBase and SeaLifeBase for comments, feedbacks and data sending to be added into the databases is info@quatics.org.",
          icon = shiny::icon("info-circle"),
          color = "aqua")
      } else  if (!isTruthy(input$resilience_in)) {
        shinydashboard::valueBox(
          shiny::h4("FishBase/SeaLifeBase info"),tagList(HTML(paste0("According to FishBase/SeaLifeBase, ", tags$em(Final_stock()[["Catch_ID"]][1,"ScientificName"]), " has prior r range:", tags$b(Fishbase_text()[1]),tags$br(), "<font color=\"#cc0000\"><b>"," For more info press: ",url_r(), "</b></font>",tags$br(), "Since no information on resilience is available in FishBase/SeaLifeBase, using Medium resilience for 'regular' species and Low resilience for large or deep-water species is a reasonable starting point. Very low resilience is found in very large species and High resilience is found in species which reproduce in their first year." ))),
          icon = shiny::icon("info-circle"),
          color = "aqua") } else {
            shinydashboard::valueBox(
              shiny::h4("FishBase/SeaLifeBase info"),tagList(HTML(paste0("According to FishBase/SeaLifeBase, ", tags$em(Final_stock()[["Catch_ID"]][1,"ScientificName"]), " has "," Resilience: ",
                                                             tags$b(Fishbase_text()[2]), " Prior r range:", tags$b(Fishbase_text()[1]),tags$br(), "<font color=\"#cc0000\"><b>"," For more info press: ",url_r(), "</b></font>"))),
              icon = shiny::icon("info-circle"),
              color = "aqua")
          }
    })
  
        observe({
      updateSelectInput(session, "resilience_in", selected=Fishbase_text()[5])
    })
    
    temp_rpriors=reactive({
      resili=input$resilience_in
      if (resili=="Very low"){
        rlow=0.015
        rhigh=0.1} else if (resili=="Low"){
          rlow=0.05
          rhigh=0.5} else if (resili=="Medium"){
            rlow=0.2
            rhigh=0.8} else if (resili=="High"){
              rlow=0.6
              rhigh=1.5
            } else {
              rlow=input$priors_rrange[1]
              rhigh= input$priors_rrange[2]}
      frpriors=c(resili,rlow,rhigh)
    })
    
    observe({
      updateSliderInput(session, "priors_rrange", value=c(temp_rpriors()[2],temp_rpriors()[3]))
    })
    
    output$Res_rpriors=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Selected r priors"),shiny::h5(HTML(paste0("Resilience:    ", temp_rpriors()[1],br(),
                                                             "r prior range: ",input$priors_rrange[1], "-",input$priors_rrange[2]))),
        icon = shiny::icon("file-alt"),
        color = "light-blue") })
    
    final_rpriors=eventReactive(input$Acc_rpriors==T,{
      req(temp_rpriors())
      fprrs=temp_rpriors()
      fprrs=c(as.numeric(input$priors_rrange[1]),as.numeric(input$priors_rrange[2]))
      return(fprrs)
    })
    
    LBB_data <- reactive({
      req(input$file_LBB)
      # if(isTruthy(input$file_LBB) ) {
      #   shinyWidgets::sendSweetAlert(
      #     session = session,title = "Info",
      #     text = "Note: use abundance predictions (B/k) from length-frequency analysis (LBB) to inform your prior settings. For example, set the intermediate prior year in the middle of a section with similar LBB B/k predictions and adjust the prior range to roughly match the LBB range.", type = "info")
      # }
        df <- read.csv(input$file_LBB$datapath,
                     header =T,
                     sep = ",",#
      quote = input$quote) #
      is.num <- sapply(df, is.numeric)
      df[is.num] <- lapply(df[is.num], round,2)
      return(df)
    })
    
    output$LBB_message <- renderUI({
      if (isTruthy(input$file_LBB) ) {
        shinydashboard::valueBox(
          shiny::h4("LBB info"),"Note: Use abundance predictions (B/k) from length-frequency analysis (LBB) to inform your prior settings. For example, set the intermediate prior year in the middle of a section with similar LBB B/k predictions and adjust your prior range to roughly match the LBB range.",
          icon = shiny::icon("info-circle"),
          color = "aqua",width = 8)
      }
    })
    
    temp_expert_bk_start=reactive({
      expert_bk_start_=input$expert_bk_start
      if (expert_bk_start_=="Unexploited, 0.75-1.0"){
        ebk_stat_low=0.75
        ebk_stat_high=1} else if (expert_bk_start_=="Sustainable, 0.4-0.8"){
          ebk_stat_low=0.4
          ebk_stat_high=0.8} else if (expert_bk_start_=="Medium, 0.2-0.6"){
            ebk_stat_low=0.2
            ebk_stat_high=0.6} else if (expert_bk_start_=="Low, 0.01-0.4"){
              ebk_stat_low=0.01
              ebk_stat_high=0.4 } else if (expert_bk_start_=="Very low, 0.01-0.2"){
                ebk_stat_low=0.01
                ebk_stat_high=0.2 } else {
                  ebk_stat_low=input$man_Bk_start_A[1]
                  ebk_stat_high= input$man_Bk_start_A[2]}
      expert_bk_start_priors=c(expert_bk_start_,ebk_stat_low,ebk_stat_high)
    })
    
    temp_expert_bk_ind=reactive({
      expert_bk_ind_=input$expert_bk_ind
      if (expert_bk_ind_=="Unexploited, 0.75-1.0"){
        ebk_stat_low=0.75
        ebk_stat_high=1} else if (expert_bk_ind_=="Sustainable, 0.4-0.8"){
          ebk_stat_low=0.4
          ebk_stat_high=0.8} else if (expert_bk_ind_=="Medium, 0.2-0.6"){
            ebk_stat_low=0.2
            ebk_stat_high=0.6} else if (expert_bk_ind_=="Low, 0.01-0.4"){
              ebk_stat_low=0.01
              ebk_stat_high=0.4 } else if (expert_bk_ind_=="Very low, 0.01-0.2"){
                ebk_stat_low=0.01
                ebk_stat_high=0.2 } else {
                  ebk_stat_low=input$man_Bk_int_A[1]
                  ebk_stat_high= input$man_Bk_int_A[2]}
      expert_bk_ind_priors=c(expert_bk_ind_,ebk_stat_low,ebk_stat_high)
    })
    
    temp_expert_bk_end=reactive({
      expert_bk_end_=input$expert_bk_end
      if (expert_bk_end_=="Unexploited, 0.75-1.0"){
        ebk_end_low=0.75
        ebk_end_high=1} else if (expert_bk_end_=="Sustainable, 0.4-0.8"){
          ebk_end_low=0.4
          ebk_end_high=0.8} else if (expert_bk_end_=="Medium, 0.2-0.6"){
            ebk_end_low=0.2
            ebk_end_high=0.6} else if (expert_bk_end_=="Low, 0.01-0.4"){
              ebk_end_low=0.01
              ebk_end_high=0.4 } else if (expert_bk_end_=="Very low, 0.01-0.2"){
                ebk_end_low=0.01
                ebk_end_high=0.2 } else {
                  ebk_end_low=input$man_Bk_end_A[1]
                  ebk_end_high= input$man_Bk_end_A[2]}
      expert_bk_end_priors=c(expert_bk_end_,ebk_end_low,ebk_end_high)
    })
    
    observe({
      updateSliderInput(session, "man_Bk_start_A", value = c( as.numeric(temp_expert_bk_start()[2]),  as.numeric(temp_expert_bk_start()[3]))
                       )
    })
    
    observe({
      updateSliderInput(session, "man_Bk_int_A", value = c(   as.numeric(temp_expert_bk_ind()[2]),  as.numeric(temp_expert_bk_ind()[3]))
                        )
    })
    
    observe({
      updateSliderInput(session, "man_Bk_end_A", value = c(   as.numeric(temp_expert_bk_end()[2]),  as.numeric(temp_expert_bk_end()[3]))
                       )
    })
    
    observe({
      updateSliderInput(session, "man_Bk_int_year_A", value =as.integer(floor((Catch_obj()[c("yr","ct_smthd")]$yr[which(Catch_obj()[c("yr","ct_smthd")]$ct_smthd>0)[1]]+max(as.integer(Catch_obj()$yr),na.rm = T))/2)),min = min(Catch_obj()$yr), max =max(Catch_obj()$yr))
    })

    observe({
      start.yr =Catch_obj()[c("yr","ct_smthd")]$yr[which(Catch_obj()[c("yr","ct_smthd")]$ct_smthd>0)[1]]
      end.yr    =max(as.integer(Catch_obj()$yr),na.rm = T)
      updateSliderInput(session, "bk_priors_year", value =start.yr,min =start.yr, max =end.yr)
    })
    
    #LBB_data <- LBB_data %>% debounce(1000)
    
    output$Check_priors_plot= shiny::renderPlot({
      validate(
        need(isTruthy(input$resilience_in), "You han't set resilience for the stock neither FishBase/SeaLifeBase provided this information. Press 'Change r prior' and select a 'resilience' option from the dropdown list to be able to continue.")
      )
      req(Catch_obj())
      start_yr =Catch_obj()[c("yr","ct_smthd")]$yr[which(Catch_obj()[c("yr","ct_smthd")]$ct_smthd>0)[1]]
      end_yr    =max(as.integer(Catch_obj()$yr),na.rm = T)
      
      start=input$man_Bk_start_A
      ind=input$man_Bk_int_A
      end=input$man_Bk_end_A
      
      temp=data.frame(yr=c(start_yr,input$man_Bk_int_year_A,end_yr),ln=c(mean(start),mean(ind),mean(end)))
      
      if (!isTruthy(input$file_LBB)) {
        p2= ggplot2::ggplot() +
          ggplot2::geom_errorbar(aes(x=start_yr, ymin=start[1], ymax=start[2],color="A"), width=0.4, size=1.5)+
          ggplot2::geom_errorbar(aes(x=input$man_Bk_int_year_A, ymin=ind[1], ymax=ind[2],color="A"), width=0.4, size=1.5)+
          ggplot2::geom_errorbar(aes(x=end_yr, ymin=end[1], ymax=end[2],color="A"), width=0.4, size=1.5)+
          #  ggplot2::scale_x_continuous(breaks=seq(start_yr,end_yr,2))+
          ggplot2::scale_y_continuous(breaks=seq(0,1.25,0.1),limits = c(0,1.25))+
          ggplot2::geom_hline( ggplot2::aes(yintercept=0.5,linetype="B"),color = "black")+
          ggplot2::geom_line(data=temp, ggplot2::aes(x=yr,y=ln,color="A"),size=1)+
          scale_color_manual(values=c("blue"),labels=c("Expert priors"))+
          ggplot2::scale_linetype_manual(values=c("dashed"),labels=c("Bmsy"))+
          ggplot2::labs(y="B/k", x="Year",color="",linetype="")+
          ggplot2::theme_classic()+
          ggplot2::theme(text = ggplot2::element_text(size = 16))+
          ggplot2::theme(legend.position="bottom")
      } else {
        LBB_data_=as.data.frame(LBB_data())
        LBB_data_$BB0.ucl.ts[LBB_data_$BB0.ucl.ts>1]=1
        LBB_data_$BB0.lcl.ts[LBB_data_$BB0.lcl.ts<0]=0
        p2=ggplot2::ggplot() +
          geom_ribbon(data=LBB_data_,ggplot2::aes(x=Year, ymin =BB0.lcl.ts, ymax =BB0.ucl.ts,fill = "A"), alpha=0.3)+
          geom_line(data=LBB_data_,ggplot2::aes(x=Year, y =BB0.ts,color="A"))+#,linetype="dashed"
          geom_point(data=LBB_data_,ggplot2::aes(x=Year, y =BB0.ts,color="A"))+
          ggplot2::geom_errorbar(aes(x=start_yr, ymin=start[1], ymax=start[2],linetype="C"),color="blue", width=1, size=1)+
          ggplot2::geom_errorbar(aes(x=input$man_Bk_int_year_A, ymin=ind[1], ymax=ind[2],linetype="C"),color="blue", width=1, size=1)+
          ggplot2::geom_errorbar(aes(x=end_yr, ymin=end[1], ymax=end[2],linetype="C"),color="blue", width=1, size=1)+
          ggplot2::geom_line(data=temp, ggplot2::aes(x=yr,y=ln,linetype="C"),color="blue",size=1)+
          ggplot2::geom_hline( ggplot2::aes(yintercept=0.5,linetype="D"),color = "black")+
          #  ggplot2::scale_x_continuous(breaks=seq(start_yr,end_yr,2))+
          ggplot2::scale_y_continuous(breaks=seq(0,1.25,0.1),limits = c(0,1.25))+
          ggplot2::scale_color_manual(values=c("green"),labels=c("LBB B/k priors"))+
          ggplot2::scale_linetype_manual(values=c("solid","dashed"),labels=c("Expert priors","Bmsy"))+
          ggplot2::scale_fill_manual(values=c("green"),labels=c("LBB B/k priors"))+
          ggplot2::labs(y="B/k", x="Year",col="",fill="",linetype="")+
          ggplot2::theme_classic()+
          ggplot2::theme(text = ggplot2::element_text(size = 16))+
          ggplot2::theme(legend.position="bottom")
      }
      p2
    })
    
    output$helpTable=renderTable({data.frame("Status"=c("Very low","Low","Medium","Sustainable","Unexploited"),
                                             "B/k range"=c("0.01-0.2","0.01-0.4", "0.2-0.6",  "0.4-0.8", "0.75-1.0"),
                                             "Descriptions of stock status or fishery"=
                                               c("strongly overfished; severely depleted; collapsed; closed; abandoned; unprofitable; minimal catch; truncated size/age structure; strongly reduced recruitment; only sporadic recruitment; low abundance in much of previous area",
                                                 "overfished; depleted; outside safe biological limits; reduced recruitment; reduced catch; increased cost of fishing; increased effort; reduced profits; reduction of mean size in the catch and in surveys; disappearance of fish from areas where they used to be",
                                                 "fully exploited; maximally sustainably fished; high catch; high fishing effort; high cost of fishing but still reasonable profits; first signs of decline in average size and reduced abundance in some areas; occasional low recruitment",
                                                 "pretty good catch; good catch per effort; high profits; many large fish; healthy size/age structure; high abundance throughout area; regular recruitment; healthy fishery",
                                                 " underdeveloped fishery; underfished; low market demand; only occasional catches; only bycatch; not vulnerable to common gears")
    )})
    
    ANN_priors=reactive({
    req(MSY_calculated())
      req(Catch_obj())
    ANN.priors(Catch_obj(),MSY_calculated())
    })
    
    observe({
      updateSliderInput(session, "man_Bk_start", value = c( input$man_Bk_start_A[1], input$man_Bk_start_A[2]))
    })
    
    observe({
      updateSliderInput(session, "man_Bk_int_year", value = input$man_Bk_int_year_A, min = input$yr_slct[1], max =input$yr_slct[2])
    })
    
    observe({
      updateSliderInput(session, "man_Bk_int", value = c(  input$man_Bk_int_A[1], input$man_Bk_int_A[2]))
    })
    
    observe({
      updateSliderInput(session, "man_Bk_end", value = c( input$man_Bk_end_A[1], input$man_Bk_end_A[2]))
    })
    
    output$You_can_proceed_to_run=shinydashboard::renderValueBox({
      shinydashboard::valueBox( shiny::h4("You can now proceed to'Run the model' tab"),shiny::h5(" "),
                                icon = shiny::icon("envelope"),color = "light-blue")})
    
    output$Biomass_plot_3= shiny::renderPlot({
      req(bio_obj())
      req(Catch_obj())
      temp_catch_obj=Catch_obj()
      temp_bio_obj=bio_obj()
      f_bio_obj=merge(x = temp_catch_obj, y = temp_bio_obj, by = "yr", all.x = TRUE)
      start_exp=input$man_Bk_start
      ind_exp=input$man_Bk_int
      end_exp=input$man_Bk_end
      start_ANN=c(as.numeric(ANN_priors()[1,1]),as.numeric(ANN_priors()[2,1]))
      ind_ANN=c(as.numeric(ANN_priors()[1,2]),as.numeric(ANN_priors()[2,2]))
      end_ANN=c(as.numeric(ANN_priors()[1,3]),as.numeric(ANN_priors()[2,3]))
      
      temp_ANN=data.frame(yr=c(min(f_bio_obj$yr),as.numeric(ANN_priors()[3,2]),max(f_bio_obj$yr)),ln=c(mean(start_ANN),mean(ind_ANN),mean(end_ANN)))
      my_y_title <-bquote(atop(Compare~expert~and~ANN~priors~"for"~the~stock~bold(.(Final_stock()[["Catch_ID"]][1,"Stock"]))~of~italic(.(Final_stock()[["Catch_ID"]][1,"ScientificName"]))))
      
      if (input$nbk_=="All three (recommended)") {
        temp_exp=data.frame(yr=c(min(f_bio_obj$yr),input$man_Bk_int_year,max(f_bio_obj$yr)),ln=c(mean(start_exp),mean(ind_exp),mean(end_exp)))
        p_1= ggplot2::ggplot(data=f_bio_obj,ggplot2::aes(x=yr)) +
          ggplot2::theme_classic()+
          ggplot2::scale_x_continuous(limits = c(min(f_bio_obj$yr,na.rm=T)-1,max(f_bio_obj$yr,na.rm=T)+1))+
          ggplot2::geom_segment(ggplot2::aes(x=min(yr),y=as.numeric(ANN_priors()[1,1]), xend=min(yr),yend=as.numeric(ANN_priors()[2,1]),col="A"),size=1)+
          ggplot2::geom_segment(ggplot2::aes(x=max(yr),y=as.numeric(ANN_priors()[1,3]), xend=max(yr),yend=as.numeric(ANN_priors()[2,3]),col="A"),size=1)+
          ggplot2::geom_segment(ggplot2::aes(x=as.numeric(ANN_priors()[3,2]),y=as.numeric(ANN_priors()[1,2]), xend=as.numeric(ANN_priors()[3,2]),yend=as.numeric(ANN_priors()[2,2]),col="A"),size=1)+
          ggplot2::geom_line(data=temp_ANN, ggplot2::aes(x=yr,y=ln,col="A"),size=1)+
          ggplot2::geom_errorbar(aes(x=min(yr), ymin=input$man_Bk_start[1], ymax=input$man_Bk_start[2],color="B"),  size=1)+
          ggplot2::geom_errorbar(aes(x=input$man_Bk_int_year, ymin=input$man_Bk_int[1], ymax=input$man_Bk_int[2],color="B"), size=1)+
          ggplot2::geom_errorbar(aes(x=max(yr), ymin=input$man_Bk_end[1], ymax=input$man_Bk_end[2],color="B"), size=1)+
          ggplot2::geom_line(data=temp_exp, ggplot2::aes(x=yr,y=ln,color="B"),size=1)+
          ggplot2::scale_color_manual(values=c("gray","blue"),labels=c("ANN estimated priors","Expert priors"))+
          ggplot2::scale_y_continuous(breaks=seq(0,1.25,0.1),limits = c(0,1.25))+
          ggplot2::theme(legend.position="bottom")+
          ggplot2::labs(y="B/k", x="Year",title=my_y_title,color="")
      } else if  (input$nbk_=="Start & intermediate") {
        temp_exp=data.frame(yr=c(min(f_bio_obj$yr),input$man_Bk_int_year),ln=c(mean(start_exp),mean(ind_exp)))
        
        p_1= ggplot2::ggplot(data=f_bio_obj,ggplot2::aes(x=yr)) +
          ggplot2::theme_classic()+
          ggplot2::scale_x_continuous(limits = c(min(f_bio_obj$yr,na.rm=T)-1,max(f_bio_obj$yr,na.rm=T)+1))+
          ggplot2::geom_segment(ggplot2::aes(x=min(yr),y=as.numeric(ANN_priors()[1,1]), xend=min(yr),yend=as.numeric(ANN_priors()[2,1]),col="A"),size=1)+
          ggplot2::geom_segment(ggplot2::aes(x=max(yr),y=as.numeric(ANN_priors()[1,3]), xend=max(yr),yend=as.numeric(ANN_priors()[2,3]),col="A"),size=1)+
          ggplot2::geom_segment(ggplot2::aes(x=as.numeric(ANN_priors()[3,2]),y=as.numeric(ANN_priors()[1,2]), xend=as.numeric(ANN_priors()[3,2]),yend=as.numeric(ANN_priors()[2,2]),col="A"),size=1)+
          ggplot2::geom_line(data=temp_ANN, ggplot2::aes(x=yr,y=ln,col="A"),size=1)+
          ggplot2::geom_errorbar(aes(x=min(yr), ymin=input$man_Bk_start[1], ymax=input$man_Bk_start[2],color="B"),size=1)+
          ggplot2::geom_errorbar(aes(x=input$man_Bk_int_year, ymin=input$man_Bk_int[1], ymax=input$man_Bk_int[2],color="B"), size=1)+
          ggplot2::geom_line(data=temp_exp, ggplot2::aes(x=yr,y=ln,color="B"),size=1)+
          ggplot2::scale_color_manual(values=c("gray","blue"),labels=c("ANN estimated priors","Expert priors"))+
          ggplot2::theme(legend.position="bottom")+
          ggplot2::scale_y_continuous(breaks=seq(0,1.25,0.1),limits = c(0,1.25))+
          ggplot2::labs(y="B/k", x="Year",title=my_y_title,color="")
      } else {
        p_1= ggplot2::ggplot(data=f_bio_obj,ggplot2::aes(x=yr)) +
          ggplot2::theme_classic()+
          ggplot2::scale_x_continuous(limits = c(min(f_bio_obj$yr,na.rm=T)-1,max(f_bio_obj$yr,na.rm=T)+1))+
          ggplot2::geom_segment(ggplot2::aes(x=min(yr),y=as.numeric(ANN_priors()[1,1]), xend=min(yr),yend=as.numeric(ANN_priors()[2,1]),col="A"),size=1)+
          ggplot2::geom_segment(ggplot2::aes(x=max(yr),y=as.numeric(ANN_priors()[1,3]), xend=max(yr),yend=as.numeric(ANN_priors()[2,3]),col="A"),size=1)+
          ggplot2::geom_segment(ggplot2::aes(x=as.numeric(ANN_priors()[3,2]),y=as.numeric(ANN_priors()[1,2]), xend=as.numeric(ANN_priors()[3,2]),yend=as.numeric(ANN_priors()[2,2]),col="A"),size=1)+
          ggplot2::geom_line(data=temp_ANN, ggplot2::aes(x=yr,y=ln,col="A"),size=1)+
          ggplot2::geom_errorbar(aes(x=min(yr), ymin=input$man_Bk_start[1], ymax=input$man_Bk_start[2],color="B"), size=1)+
          ggplot2::scale_color_manual(values=c("gray","blue"),labels=c("ANN estimated priors","Expert priors"))+
          ggplot2::theme(legend.position="bottom")+
          ggplot2::scale_y_continuous(breaks=seq(0,1.25,0.1),limits = c(0,1.25))+
          ggplot2::labs(y="B/k", x="Year",title=my_y_title,color="")
      }
      print(p_1)
    })
    
    ABC_object_=eventReactive(input$Save_priors,{
      req(Final_stock())
      req(Catch_obj())
      req(bio_obj())
      
      temp_catch_obj=as.data.frame(merge(x = Catch_obj(), y = ,bio_obj(), by = "yr", all.x = TRUE))
      fstck_Catch_Data=temp_catch_obj[,c( "Stock","yr","ct",
                                          "bt","ct_smthd","bt_iterp",
                                          "bt_smthd","ecreep","bt_cv","Stock_objID")]
      
      if (isTruthy(input$CPUE_CV) & all(is.na(fstck_Catch_Data$bt_cv))) {
        fstck_Catch_Data$bt_cv=input$CPUE_CV
      }
      
      if (isTruthy(input$CPUE_CV) & all(is.na(fstck_Catch_Data$bt_cv))) {
        CV_cpue_=input$CPUE_CV
      } else if (!isTruthy(input$CPUE_CV) & unique(fstck_Catch_Data$bt_cv)==0.2) {
        CV_cpue_="no cpue"
      }  else if (isTruthy(input$CPUE_CV) & !all(is.na(fstck_Catch_Data$bt_cv))) {
        CV_cpue_="provided by the user"
      }   
         
      nyr=length(fstck_Catch_Data$ct_smthd)
      
      stock_info_clnms =c("Stock","ScientificName","Name","Group","Continent","Region","Subregion","Comments","Source")
      
      inputs_clnms =c("StartYear","EndYear","Smooth_K_catch","CV_catch","StartYear_bt","EndYear_bt","Ecreep_cpue",
                      "Ecreep_Year","Ecreep_value","Smooth_cpue","Smooth_K_cpue","CV_cpue","Resilience","r.low","r.hi","FBSLB_info_r","FBSLB_info_Resilience","FBSLB_page","MSY_prior","q_low","q_high",
                      "ANN_stb.low","ANN_stb.hi","ANN_int.yr", "ANN_intb.low","ANN_intb.hi","ANN_endb.low","ANN_endb.hi","nbk", "stb.low","stb.hi",
                      "int.yr", "intb.low","intb.hi","endb.low","endb.hi","btype")
      
      data_clnms =c("Stock","yr","ct","bt","ct_smthd","bt_iterp","bt_smthd","ecreep","bt_cv", "Stock_objID")
      
      input_info = data.frame(matrix(nrow = 1, ncol = length(stock_info_clnms)))
      colnames(input_info)=stock_info_clnms
      
      input_param = data.frame(matrix(nrow = 1, ncol = length(inputs_clnms)))
      colnames(input_param)=inputs_clnms
      
      input_data = data.frame(matrix(nrow =1, ncol = length(data_clnms))) #nrow(fstck_Catch_Data)
      colnames(input_data) = data_clnms
      
      
      posteriors_clmns=c("r_post","k_post","MSY_post","q_post","Fmsy_post","Bmsy_post","Fmsy_post_corrected","Fmsy_post_correction_note")
      output_posteriors_ = data.frame(matrix(nrow =3, ncol = length(posteriors_clmns))) #nrow(fstck_Catch_Data)
      colnames(output_posteriors_)=posteriors_clmns
      
      row.names(output_posteriors_)=c("median","95% CIlow","95% CI.high")
      
      timeseries_clmns=c("Catch","Catch_low","Catch_high","FFmsy","FFmsy_low","FFmsy_high","f","f_low","f_high","BBmsy","BBmsy_low","BBmsy_high",
                         "B","B_low","B_high", "bk","bk_low","bk_high")
      output_timeseries_ = data.frame(matrix(nrow =length(fstck_Catch_Data$yr), ncol = length(timeseries_clmns))) #nrow(fstck_Catch_Data)
      colnames(output_timeseries_)=timeseries_clmns
      
      row.names(output_timeseries_)=fstck_Catch_Data$yr
      
      
      ABC_object=list(input=list(Stock_info=input_info,Input_parameters=input_param,Input_data=fstck_Catch_Data),
                      output=list(output_posteriors=output_posteriors_,output_timeseries=output_timeseries_))
      
      if (isTruthy(input$con_fbase)) {
        FT=Fishbase_text()} else {
          FT=rep(NA,6)
        }
      
      ########Stock_info
      ABC_object[["input"]][["Stock_info"]]$Stock=Final_stock()[["Catch_ID"]]$Stock
      ABC_object[["input"]][["Stock_info"]]$ScientificName =Final_stock()[["Catch_ID"]]$ScientificName
      ABC_object[["input"]][["Stock_info"]]$Name =Final_stock()[["Catch_ID"]]$Name
      ABC_object[["input"]][["Stock_info"]]$Group  =Final_stock()[["Catch_ID"]]$Group
      ABC_object[["input"]][["Stock_info"]]$Continent  =Final_stock()[["Catch_ID"]]$Continent
      ABC_object[["input"]][["Stock_info"]]$Region  =Final_stock()[["Catch_ID"]]$Region
      ABC_object[["input"]][["Stock_info"]]$Subregion  =Final_stock()[["Catch_ID"]]$Subregion
      ABC_object[["input"]][["Stock_info"]]$Comments  =Final_stock()[["Catch_ID"]]$Comments
      ABC_object[["input"]][["Stock_info"]]$Source =Final_stock()[["Catch_ID"]]$Source
      
      # ########Input_parameters
      ABC_object[["input"]][["Input_parameters"]]$StartYear=input$yr_slct[1]
      ABC_object[["input"]][["Input_parameters"]]$EndYear=input$yr_slct[2]
      ABC_object[["input"]][["Input_parameters"]]$Smooth_K_catch=input$bw_inpt
      ABC_object[["input"]][["Input_parameters"]]$CV_catch=input$Catch_CV
      ABC_object[["input"]][["Input_parameters"]]$StartYear_bt=input$bio_yr_slct[1]
      ABC_object[["input"]][["Input_parameters"]]$EndYear_bt=input$bio_yr_slct[2]
      ABC_object[["input"]][["Input_parameters"]]$Ecreep_cpue=input$ecreepyes
      ABC_object[["input"]][["Input_parameters"]]$Ecreep_Year=input$ecreep_year
      ABC_object[["input"]][["Input_parameters"]]$Ecreep_value=input$ecreepslider
      ABC_object[["input"]][["Input_parameters"]]$Smooth_cpue=input$upMaterialbcv
      ABC_object[["input"]][["Input_parameters"]]$Smooth_K_cpue=input$bw_inpt_bt
      ABC_object[["input"]][["Input_parameters"]]$CV_cpue=CV_cpue_
      ABC_object[["input"]][["Input_parameters"]]$Resilience=input$resilience_in
      ABC_object[["input"]][["Input_parameters"]]$r.low=as.numeric(final_rpriors()[1])
      ABC_object[["input"]][["Input_parameters"]]$r.hi=as.numeric(final_rpriors()[2])
      
      FBSLB_info_r=gsub(",","-",FT[1])
      FBSLB_info_r=gsub(";",".",FBSLB_info_r)
      
      FBSLB_info_Resilience=gsub(",","-",FT[2])
      FBSLB_info_Resilience=gsub(";",".",FBSLB_info_Resilience)
      
      ABC_object[["input"]][["Input_parameters"]]$FBSLB_info_r=FBSLB_info_r
      ABC_object[["input"]][["Input_parameters"]]$FBSLB_info_Resilience=FBSLB_info_Resilience
      ABC_object[["input"]][["Input_parameters"]]$FBSLB_page=FT[6]
      ABC_object[["input"]][["Input_parameters"]]$q_low=NA
      ABC_object[["input"]][["Input_parameters"]]$q_high=NA
      
      ABC_object[["input"]][["Input_parameters"]]$MSY_prior=MSY_calculated()
      ABC_object[["input"]][["Input_parameters"]]$ANN_stb.low=as.numeric(ANN_priors()[1,1])
      ABC_object[["input"]][["Input_parameters"]]$ANN_stb.hi=as.numeric(ANN_priors()[2,1])
      ABC_object[["input"]][["Input_parameters"]]$ANN_int.yr=as.numeric(ANN_priors()[3,2])
      ABC_object[["input"]][["Input_parameters"]]$ANN_intb.low=as.numeric(ANN_priors()[1,2])
      ABC_object[["input"]][["Input_parameters"]]$ANN_intb.hi=as.numeric(ANN_priors()[2,2])
      ABC_object[["input"]][["Input_parameters"]]$ANN_endb.low=as.numeric(ANN_priors()[1,3])
      ABC_object[["input"]][["Input_parameters"]]$ANN_endb.hi=as.numeric(ANN_priors()[2,3])
      
      ABC_object[["input"]][["Input_parameters"]]$nbk=input$nbk_
      ABC_object[["input"]][["Input_parameters"]]$stb.low=input$man_Bk_start[1]
      ABC_object[["input"]][["Input_parameters"]]$stb.hi=input$man_Bk_start[2]
      ABC_object[["input"]][["Input_parameters"]]$int.yr=input$man_Bk_int_year
      ABC_object[["input"]][["Input_parameters"]]$intb.low=input$man_Bk_int[1]
      ABC_object[["input"]][["Input_parameters"]]$intb.hi=input$man_Bk_int[2]
      ABC_object[["input"]][["Input_parameters"]]$endb.low=input$man_Bk_end[1]
      ABC_object[["input"]][["Input_parameters"]]$endb.hi=input$man_Bk_end[2]
      ABC_object[["input"]][["Input_parameters"]]$btype=input$btype
      ########Input_data
      #  ABC_object[["input"]][["Input_data"]]=fstck_Catch_Data
      
      return(ABC_object)
    })
    
    middle_outplts=eventReactive(input$Save_priors==T,{
      if (input$nbk_=="Only start") {
        outpts_m=data.frame(range=c(paste0(ABC_object_()[["input"]][["Input_parameters"]]$StartYear, "-", ABC_object_()[["input"]][["Input_parameters"]]$EndYear),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$StartYear_bt, "-", ABC_object_()[["input"]][["Input_parameters"]]$EndYear_bt),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$r.low, "-",ABC_object_()[["input"]][["Input_parameters"]]$r.hi),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$stb.low, "-",ABC_object_()[["input"]][["Input_parameters"]]$stb.hi)
                                    #paste0(format(ABC_object_()[["input"]][["Input_parameters"]]$q_low,digits=3), "-",format(ABC_object_()[["input"]][["Input_parameters"]]$q_high,digits=3))
        ))
        row.names(outpts_m)=c("Analysis","cpue", "Prior for r","Start B/k prior")#,"q prior","Interm. B/k prior","End B/k prior"
      } else if (input$nbk_=="Start & intermediate") {
        outpts_m=data.frame(range=c(paste0(ABC_object_()[["input"]][["Input_parameters"]]$StartYear, "-", ABC_object_()[["input"]][["Input_parameters"]]$EndYear),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$StartYear_bt, "-", ABC_object_()[["input"]][["Input_parameters"]]$EndYear_bt),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$r.low, "-",ABC_object_()[["input"]][["Input_parameters"]]$r.hi),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$stb.low, "-",ABC_object_()[["input"]][["Input_parameters"]]$stb.hi),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$intb.low, "-",ABC_object_()[["input"]][["Input_parameters"]]$intb.hi)
                                   # paste0(format(ABC_object_()[["input"]][["Input_parameters"]]$q_low,digits=3), "-",format(ABC_object_()[["input"]][["Input_parameters"]]$q_high,digits=3))
                                     ),
        year=c("","","","",ABC_object_()[["input"]][["Input_parameters"]]$int.yr))#,"",""
        row.names(outpts_m)=c("Analysis","cpue","Prior for r","Start B/k prior","Interm. B/k prior")#,"q prior"
      } else {
        outpts_m=data.frame(range=c(paste0(ABC_object_()[["input"]][["Input_parameters"]]$StartYear, "-", ABC_object_()[["input"]][["Input_parameters"]]$EndYear),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$StartYear_bt, "-", ABC_object_()[["input"]][["Input_parameters"]]$EndYear_bt),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$r.low, "-",ABC_object_()[["input"]][["Input_parameters"]]$r.hi),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$stb.low, "-",ABC_object_()[["input"]][["Input_parameters"]]$stb.hi),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$intb.low, "-",ABC_object_()[["input"]][["Input_parameters"]]$intb.hi),
                                    paste0(ABC_object_()[["input"]][["Input_parameters"]]$endb.low, "-",ABC_object_()[["input"]][["Input_parameters"]]$endb.hi)
                                   # paste0(format(ABC_object_()[["input"]][["Input_parameters"]]$q_low,digits=3), "-",format(ABC_object_()[["input"]][["Input_parameters"]]$q_high,digits=3))
                                             ),
        year=c("","","","",ABC_object_()[["input"]][["Input_parameters"]]$int.yr,""))#,""
        row.names(outpts_m)=c("Analysis","cpue","Prior for r","Start B/k prior","Interm. B/k prior","End B/k prior")#,"q prior"
      }            
      return(outpts_m) 
    })
    
    output$middle_outputs <-renderPrint({middle_outplts()
    })
    
    output$param_sofar <-renderText({paste("Parameterization so far for the stock",
                                           tags$b(ABC_object_()[["input"]][["Stock_info"]]$Stock), "of", 
                                           tags$em(ABC_object_()[["input"]][["Stock_info"]]$ScientificName))
    })
    
    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
    
    shinyFiles::shinyDirChoose(input,id="directory", roots =volumes, session = session,  allowDirCreate = T)#restrictions = system.file(package = "base"),
    
    dir.name=reactive({
      if (isTruthy(input$directory)) {
     dr= parseDirPath(volumes, input$directory)
      } else {
        dr="."  
        }
    })
    
   output$dir_name=renderUI({
   req(dir.name())
     if (dir.name()!=".") {
       shiny::h5(tagList("You have selected/created the folder: ",tags$b(dir.name())))
     } else {
       shiny::h5(tagList("You haven't selected/created a folder."))#As a result, the working directory is set to., tags$b(getwd( ))
     }
       })
    
    observe({
      req(Final_stock())
      updateTextInput(session, "obj.name",value=paste0(Final_stock()[["Catch_ID"]]$Stock, "_V1") ) #, placeholder=paste0("e.g.  ",Final_stock()[["Catch_ID"]]$Stock, "_V1")
    })
    
    object_NAME=reactive({
      req(ABC_object_())
      if (input$obj.name=="") {
        name=paste0(ABC_object_()[["input"]][["Stock_info"]]$Stock, "_V1")
      } else {
        name=input$obj.name
      }
      return(name)
    })
    
    observeEvent(input$Save_priors, {
      req(object_NAME())
      ABC_object <- ABC_object_()
      dir.create( paste0(dir.name(),"/BSM"))
      dir=paste0(paste0(dir.name(),"/BSM"))
      
      save(ABC_object,file =paste0(dir,"/", object_NAME(), ".RData"))
      Save_done <- showNotification(paste("Message:",  "The stock object with the input parameterization has been saved in ", paste0(dir.name(),"/BSM/",object_NAME(), ".RData")), duration = 10)
      
    })
    
    objects <- reactiveVal(data.frame(A.A=rep(NA,100),Created_Stocks=rep(NA,100)))
    count <- reactiveVal(0)
    
    observeEvent(input$Save_priors, {
      count(count() + 1)
    })
    
    observeEvent(input$Save_priors,{
      # start with current data
      objects() %>%
        add_row(
          A.A = isolate(as.integer(count())),
          Created_Stocks = isolate(object_NAME())
        ) %>%
        # update data value
        objects()
    }
    )
    
    output$created_objects <- renderUI({
      renderTable({as.data.frame( objects()[!is.na( objects()$A.A),])})
    })
    
    
    output$select_diff_obj <- renderUI({
      filenames <- list.files(paste0(dir.name(),"/BSM"), pattern="*.RData", full.names=F)
      filenames=gsub(".RData","",filenames)
      shinyWidgets::updatePickerInput(session, "Id081", choices = filenames,
                                      selected =filenames[1])
      pickerInput(
        inputId = "Id081",
        label = "Select stock object", 
        choices = filenames,
        selected =filenames[1])
      
    })
    
    fstc=reactive({
      if (input$Id049=="A") {
        sdsd=ABC_object_() } else if (input$Id049=="B") {
          asa=paste0(dir.name(),"/BSM/",input$Id081,".RData")
          load(asa)
          sdsd=ABC_object
        }
      return(sdsd)
    })
    
    fstc <- fstc %>% debounce(500)
    
    
    observeEvent(input$see_obj,{
      req(fstc())
      xxx=as.data.frame(t(cbind(fstc()[["input"]][["Stock_info"]],fstc()[["input"]][["Input_parameters"]])))
      xxx$parameter=row.names(xxx)
      colnames(xxx)[1]="value"
      xxx=xxx[,c(2,1)]
      output$see_object_table=renderTable(xxx)
    })
    
    observe({
      output$Zavarakatranemia=renderUI({ shiny::h4(tagList("Run the model and get the results for the stock ",  tags$b(fstc()[["input"]][["Stock_info"]][["Stock"]]), "of",  tags$em(fstc()[["input"]][["Stock_info"]][["ScientificName"]])))})
    })
    
    ###########################
    ###### RUN THE MODEL ######
    ###########################
    
    output$Run_infobox_patient=shinydashboard::renderValueBox({
      shinydashboard::valueBox( shiny::h4("Run the model"),shiny::h5("Press 'Start' button to run the model.
              It takes some time to do all the necessary calculations, so be patient!"),
                                icon = shiny::icon("envelope"),
                                color = "light-blue")
    })
    
    r.k_priors= eventReactive(input$Start_run,{
      # req(ABC_object_()) ####mhpvs na to kanw based on stock object?
      req(fstc())
      rk_priors_=rk_priors(fstc()[["input"]][["Input_parameters"]]$r.low,fstc()[["input"]][["Input_parameters"]]$r.hi,fstc()[["input"]][["Input_parameters"]]$MSY_prior,fstc()[["input"]][["Stock_info"]][["Stock"]])
      return(rk_priors_)
    })
    

    q_priors= eventReactive(input$Start_run,{
      req(fstc())
      req(r.k_priors())
      q.priors_=q.priors(fstc(),r.k_priors(),"BSM")
    return(q.priors_)
    })
    ##################### RUN THE MODEL 
    
    observeEvent(input$Start_run, {
      req(r.k_priors()) ####mhpvs na to kanw based on stock object?
      rk_priors_done <- showNotification(paste("Message:", "r-k priors have been calculated"), duration = 5)
    })
    
    observeEvent(input$Start_run, {
      req(q_priors()) ####mhpvs na to kanw based on stock object?
      q_priors_done <- showNotification(paste("Message:", "q priors have been calculated"), duration = 5)
    })

    BSM_run=eventReactive(input$Start_run,{
      ABC_fit(ABC_object_(),r.k_priors(),q_priors(),"BSM")
    })
    
    observeEvent(input$Start_run, {
      req(BSM_run())
      Model_run <- showNotification(paste("Message: ", "Model run completed"), duration = 5)
    })
    
    ##################### EXTRACT OUTCOMES TO STOCK OBJECT
    ABC_object_final=eventReactive(input$Start_run,{
      ABC_object_final_=extract_ABC_fit(BSM_run(),q_priors(),ABC_object_())
    })
    
    observeEvent(input$Start_run, {
      req(ABC_object_final())
      Model_run <- showNotification(paste("Message: ", "Model outcomes extracted"), duration = 5)
    })
    
    observeEvent(input$Start_run,{   #SEE AFTER 5
      req(ABC_object_final())
      start.yr =ABC_object_final()[["input"]][["Input_parameters"]][["StartYear"]]
      end.yr    =ABC_object_final()[["input"]][["Input_parameters"]][["EndYear"]]
      nyr=end.yr-start.yr+1
      nyr_mid=ceiling(nyr/2)
      updateSliderInput(session, "retro_range", value =ifelse(nyr_mid<3,nyr_mid,3),  max =nyr_mid) 
    })
    
    run_pictures <- reactiveValues()
    observeEvent(input$Start_run,{
      run_pictures$pic_A=ggrk.plot(ABC_object_final(),r.k_priors(),BSM_run(),"BSM") 
      pic_A_ready <- showNotification(paste("Message: ", "r-k paired graph ready"), duration = 5)
    })
    
    output$rk_space= shiny::renderPlot({
      run_pictures$pic_A})#,height = 230
 
    ####################### Stock info
    ####################### Stock info
    ####################### Stock info
    ####################### Stock info
    
    output$ModOutp_a1=renderUI({HTML(paste0(tags$b("Species: "),tags$em(ABC_object_final()[["input"]][["Stock_info"]]$ScientificName), ", stock:",  ABC_object_final()[["input"]][["Stock_info"]]$Stock))})
    output$ModOutp_a2=renderUI({HTML(paste0(tags$b("Common name: "),tags$em(ABC_object_final()[["input"]][["Stock_info"]]$Name)))})
    
    output$ModOutp_a3=renderUI({HTML(paste0(tags$b("Region: "),ABC_object_final()[["input"]][["Stock_info"]]$Region, ", ", ABC_object_final()[["input"]][["Stock_info"]]$Subregion))})
    output$ModOutp_a4=renderUI({HTML(paste0(tags$b("Catch data used from years: "),ABC_object_final()[["input"]][["Input_parameters"]]$StartYear,
                                            "-", ABC_object_final()[["input"]][["Input_parameters"]]$EndYear,", abundance: ", ABC_object_final()[["input"]][["Input_parameters"]]$btype))})
    
    output$ModOutp_a5=renderUI({HTML(paste0(tags$b("Prior initial rel. biomass: "),format(round(as.numeric(ABC_object_final()[["input"]][["Input_parameters"]]$stb.low),2),digits = 3), "-", format(as.numeric(ABC_object_final()[["input"]][["Input_parameters"]]$stb.hi),digits = 3)))})
    
    output$ModOutp_a6=renderUI({
      if (input$nbk_!="Only start") {
        HTML(paste0(tags$b("Prior intermediate rel. biomass: "),format(as.numeric(ABC_object_final()[["input"]][["Input_parameters"]]$intb.low),digits = 3), "-", format(as.numeric(ABC_object_final()[["input"]][["Input_parameters"]]$intb.hi),digits = 3), ", ", ABC_object_final()[["input"]][["Input_parameters"]]$int.yr))
      }
    })
    output$ModOutp_a7=renderUI({
      if (input$nbk_=="All three (recommended)") {
        HTML(paste0(tags$b("Prior final rel. biomass: "),format(as.numeric(ABC_object_final()[["input"]][["Input_parameters"]]$endb.low),digits = 3), "-", format(as.numeric(ABC_object_final()[["input"]][["Input_parameters"]]$endb.hi),digits = 3)))
      }
    })
    
    output$ModOutp_a8=renderUI({HTML(paste0(tags$b("Prior range for r: "),ABC_object_final()[["input"]][["Input_parameters"]]$r.low, "-", ABC_object_final()[["input"]][["Input_parameters"]]$r.hi))})
    #output$ModOutp_a9=renderUI({HTML(paste0(tags$b("Derived prior range for k: "),format(round(r.k_priors()[["rkpriors"]]$prior.k.low),digits = 3), "-", format(round(r.k_priors()[["rkpriors"]]$prior.k.hi),digits = 3)))})
    output$ModOutp_a10=renderUI({HTML(paste0(tags$b("Derived prior MSY: "),format(round(as.numeric(ABC_object_final()[["input"]][["Input_parameters"]]$MSY_prior)),digits = 3)))})
    output$ModOutp_a11=renderUI({HTML(paste0(tags$b("Derived prior range for q: "),format(ABC_object_final()[["input"]][["Input_parameters"]]$q_low,digits = 3), "-", format(ABC_object_final()[["input"]][["Input_parameters"]]$q_high,digits = 3)))})
    
    ####################### Model outputs
    ####################### Model outputs
    ####################### Model outputs
    ####################### Model outputs
    output$ModOutp_b1=renderUI({HTML(paste0(tags$b("r= "),format(ABC_object_final()[["output"]][["output_posteriors"]]$r_post[1],digits = 3),
                                            ", ",tags$b("95% CL ="),format(ABC_object_final()[["output"]][["output_posteriors"]]$r_post[2],digits = 3), "-",
                                            format(ABC_object_final()[["output"]][["output_posteriors"]]$r_post[3],digits = 3)))})
    output$ModOutp_b2=renderUI({HTML(paste0(tags$b("k= "),format(round(ABC_object_final()[["output"]][["output_posteriors"]]$k_post[1]),digits = 3),
                                            ", ",tags$b("95% CL ="),format(round(ABC_object_final()[["output"]][["output_posteriors"]]$k_post[2]),digits = 3), "-",
                                            format(round(ABC_object_final()[["output"]][["output_posteriors"]]$k_post[3]),digits = 3)))})
    
    output$ModOutp_b3=renderUI({HTML(paste0(tags$b("MSY= "),format(round(ABC_object_final()[["output"]][["output_posteriors"]]$MSY_post[1]),digits = 3),
                                            ", ",tags$b("95% CL ="),format(round(ABC_object_final()[["output"]][["output_posteriors"]]$MSY_post[2]),digits = 3), "-",
                                            format(round(ABC_object_final()[["output"]][["output_posteriors"]]$MSY_post[3]),digits = 3)))})
    
    output$ModOutp_b4=renderUI({HTML(paste0(tags$b("Relative biomass in last year = "),format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$bk, n=1),digits = 3),
                                            "k, 2.5th perc = ",format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$bk_low, n=1),digits = 3),", 97.5th perc = ",
                                            format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$bk_high, n=1),digits = 3)))})
    
    output$ModOutp_b5=renderUI({HTML(paste0(tags$b("Exploitation F/(r/2) in last year = "),format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$FFmsy, n=1),digits = 3),
                                            ", 2.5th perc = ",format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$FFmsy_low, n=1),digits = 3),", 97.5th perc = ",
                                            format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$FFmsy_high, n=1),digits = 3)))})
    
    output$ModOutp_b6=renderUI({HTML(paste0(tags$b("q= "),format(ABC_object_final()[["output"]][["output_posteriors"]]$q_post[1],digits = 3),
                                            ", ",tags$b("95% CL ="),format(ABC_object_final()[["output"]][["output_posteriors"]]$q_post[2],digits = 3), "-",
                                            format(ABC_object_final()[["output"]][["output_posteriors"]]$q_post[3],digits = 3)))})
    
    output$ModOutp_23=renderUI({
      if(ABC_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_correction_note[1]=="Fmsy was corrected downward to account for reduced recruitment."){
        HTML(paste0(tags$b("Fmsy ="),format(ABC_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1],digits = 3),
                    ", 95% CL =",format(ABC_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2],digits = 3),"-",
                    format(ABC_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3],digits = 3)," - Fmsy was corrected downward to account for reduced recruitment."))} else {
                      HTML(paste0(tags$b("Fmsy ="),format(ABC_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1],digits = 3),
                                  ", 95% CL =",format(ABC_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2],digits = 3),"-",
                                  format(ABC_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3],digits = 3)))
                    }
    })
    
    output$ModOutp_25=renderUI({
      HTML(paste0(tags$b("MSY ="),format(round(ABC_object_final()[["output"]][["output_posteriors"]]$MSY_post[1]),digits = 2),
                  ", 95% CL =",format(round(ABC_object_final()[["output"]][["output_posteriors"]]$MSY_post[2]),digits = 2),"-",
                  format(round(ABC_object_final()[["output"]][["output_posteriors"]]$MSY_post[3]),digits = 2)))#}
    })
    
    output$ModOutp_26=renderUI({
      HTML(paste0(tags$b("Bmsy ="),format(round(ABC_object_final()[["output"]][["output_posteriors"]]$Bmsy_post[1]),digits = 2),
                  ", 95% CL =",format(round(ABC_object_final()[["output"]][["output_posteriors"]]$Bmsy_post[2]),digits = 2),"-",
                  format(round(ABC_object_final()[["output"]][["output_posteriors"]]$Bmsy_post[3]),digits = 2)))#}
    })
    
    output$ModOutp_27=renderUI({
      HTML(paste0(tags$b("Biomass in last year ="),format(round(tail(ABC_object_final()[["output"]][["output_timeseries"]]$B,1)),digits = 2),
                  ", 2.5th perc =",format(round(tail(ABC_object_final()[["output"]][["output_timeseries"]]$B_low,1)),digits = 2),", 97.5 perc =",
                  format(round(tail(ABC_object_final()[["output"]][["output_timeseries"]]$B_high,1)),digits = 2)))#}
    })
    
    output$ModOutp_28=renderUI({
      HTML(paste0(tags$b("B/Bmsy in last year  ="),format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$BBmsy,1),digits = 3),
                  ", 2.5th perc =",format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$BBmsy_low,1),digits = 3),", 97.5 perc =",
                  format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$BBmsy_high,1),digits = 3)))#}
    })
    
    output$ModOutp_29=renderUI({
      HTML(paste0(tags$b("Fishing mortality in last year  ="),format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$f,1),digits = 3),
                  ", 2.5th perc =",format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$f_low,1),digits = 3),", 97.5 perc =",
                  format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$f_high,1),digits = 3)))#}
    })
    
    output$ModOutp_30=renderUI({
      HTML(paste0(tags$b("Exploitation F/Fmsy in last year  ="),format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$FFmsy,1),digits = 3),
                  ", 2.5th perc =",format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$FFmsy_low,1),digits = 3),", 97.5 perc =",
                  format(tail(ABC_object_final()[["output"]][["output_timeseries"]]$FFmsy_high,1),digits = 3)))#}
    })
    
    ############ Catch plot
    observeEvent(input$Start_run,{
      run_pictures$pic_B=ggcatch.plot(ABC_object_final(),METHOD = "BSM") 
      pic_B_ready <- showNotification(paste("Message: ", "Catch graph ready"), duration = 5)
    })
    
    output$Catch_plot_final= shiny::renderPlot({
      run_pictures$pic_B})
    
    observeEvent(input$Start_run,{
      run_pictures$pic_C=ggbk.plot(ABC_object_final(),"BSM") 
      
      pic_C_ready <- showNotification(paste("Message: ", "B/k graph ready"), duration = 5)
    })
    
    output$Pred_biom_plot= shiny::renderPlot({
      run_pictures$pic_C
    })
    
    ###### FFmsy PLOT
    observeEvent(input$Start_run,{
      run_pictures$pic_D=ggFFmsy.plot(ABC_object_final(),"BSM") 
      pic_D_ready <- showNotification(paste("Message: ", "F/Fmsy graph ready"), duration = 5)
      
    })
    
    output$Pred_FFmsy_plot= shiny::renderPlot({
      run_pictures$pic_D
    })
    
    ###### PARABOLA PLOT
    observeEvent(input$Start_run,{
      run_pictures$pic_E=ggparabola.plot(ABC_object_final(),"BSM") 
      
      pic_D_ready <- showNotification(paste("Message: ", "Stock equillibrium graph ready"), duration = 5)
    })
    
    output$Parabola= shiny::renderPlot({
      run_pictures$pic_E
    })
    
    observeEvent(input$Start_run, {
      for (i in 1:100) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = "prog_bar",
          value = i, total = 100,
          title = paste("Process", trunc(i/10)))
        Sys.sleep(0.05)
      }
    })
    
    ###### KOBE PLOT
    observeEvent(input$Start_run,{
      run_pictures$pic_F=ggkobe.plot(ABC_object_final(),BSM_run() ,"BSM") 
      pic_F_ready <- showNotification(paste("Message: ", "Kobe plot graph ready"), duration = 5)
    })
    
    output$kobe_plot= shiny::renderPlot({
      run_pictures$pic_F
    })
    
    observeEvent(input$Start_run,{
      run_pictures$pic_G= gg_management.plot(ABC_object_final(),BSM_run(),"BSM")
      pic_G_ready <- showNotification(paste("Message: ", "Management graph ready"), duration = 5)
      
    })
    
    output$Alexandros5= shiny::renderPlot({
      run_pictures$pic_G
    })
    
    observeEvent(input$Start_run,{
      run_pictures$pic_H= ggprorposterior.plot(ABC_object_final(),BSM_run(),r.k_priors(),"BSM") 
      pic_G_ready <- showNotification(paste("Message: ", "Prior-Posterior graph ready"), duration = 5)
    })
    
    output$PriorPosterior <- shiny::renderPlot({
      run_pictures$pic_H
    })
    
    observeEvent(input$Start_run,{
      run_pictures$pic_I= ggpdiagnostics.plot(ABC_object_final(),BSM_run(),"BSM")
       pic_I_ready <- showNotification(paste("Message: ", "Diagnostics graph ready"), duration = 5)
    })
    
    output$Diagnostics.plot <- shiny::renderPlot({
      run_pictures$pic_I
    })
    
    ABC_retro=eventReactive(input$Retrospective,{
      req(ABC_object_final())
      xx= retro_fit(ABC_object_final(),r.k_priors(),q_priors(),input$retro_range,"BSM")
    })
    
    observeEvent(input$Retrospective,{
      req(ABC_retro())
      run_pictures$pic_J=ggplot.retro(ABC_retro())
    })
    
    output$retro_pics= shiny::renderPlot({
      run_pictures$pic_J
    })#,height = 300
    
    observeEvent(input$Retrospective, {
      for (i in 1:100) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = "prog_bar_retro",
          value = i, total = 100,
          title = paste("Process", trunc(i/10)))
        Sys.sleep(0.1)
      }
    })
    
    observeEvent(input$Start_run, {
      req(ABC_object_final())
      req(fstc())
      req( run_pictures$pic_A)
      
      ABC_object <- ABC_object_final()
      if (input$Id049=="A") {
        nm=object_NAME()} else if (input$Id049=="B") {
          nm=input$Id081}
      
      dir.create(paste0(dir.name(),"/BSM/outputs"))
      dir.create(paste0(dir.name(),"/BSM/outputs/",nm))
      device_="tiff"
      
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","rk_pic."),device_),plot=run_pictures$pic_A, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Catch_and_MSY."),device_),plot=run_pictures$pic_B, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Stock_size."),device_),plot=run_pictures$pic_C, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Exploitation_rate."),device_),plot=run_pictures$pic_D, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Equilibrium_curve."),device_),plot=run_pictures$pic_E, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Kobe_plot."),device_),plot=run_pictures$pic_F, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Management_graphs."),device_),plot=run_pictures$pic_G, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","prior_posterior_distributions."),device_),plot=run_pictures$pic_H, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      save(ABC_object,  file =paste0(dir.name(),"/BSM/outputs/",nm,"/","ABC_obj_",gsub(" ", "_",fstc()[["input"]][["Stock_info"]][["ScientificName"]]), "_",Sys.Date(), ".RData"))
      write.csv(cbind(fstc()[["input"]][["Stock_info"]],fstc()[["input"]][["Input_parameters"]]), paste0(dir.name(),"/BSM/outputs/",nm,"/input_parameters.csv"), row.names = F)
      write.csv(ABC_object[["output"]][["output_timeseries"]], paste0(dir.name(),"/BSM/outputs/",nm,"/output_timeseries.csv"), row.names = TRUE)
      write.csv(ABC_object[["output"]][["output_posteriors"]], paste0(dir.name(),"/BSM/outputs/",nm,"/output_posteriors.csv"), row.names = TRUE)
      
      Save_done <- showNotification(paste("Message: ", "All the outcomes are saved in your working directory"), duration = 10)
      
    })
    
    observeEvent(input$button_summary, {
      if (input$Id049=="A") {
        nm=object_NAME()} else if (input$Id049=="B") {
          nm=input$Id081}
      device_="png"
      
      xxx= gg_summary.plot(ABC_object_final(),r.k_priors(),BSM_run(),"BSM")
      
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Summary_pic."),device_),plot=xxx, device =device_, width = 25, height =18, units = "cm",  dpi = 300)
      Save_done <- showNotification(paste("Message: ", "Summary outcomes are saved in your working directory"), duration = 5)
    })
    
    observeEvent(input$Retrospective, {
      req( run_pictures$pic_J)
      if (input$Id049=="A") {
        nm=object_NAME()} else if (input$Id049=="B") {
          nm=input$Id081}
      device_="tiff"
      ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","retrospective_pic."),device_),plot=run_pictures$pic_J, device =device_, width = 18, height =9, units = "cm",  dpi = 300)
      Save_done <- showNotification(paste("Message: ", "Retrospective outcomes are saved in your working directory"), duration = 5)
    })
    
    observe({
      req(ABC_object_final())
      if (input$Id049=="A") {
        nm=object_NAME()} else if (input$Id049=="B") {
          nm=input$Id081}
      output$working_directory=renderUI({ shiny::h4(tagList("All your outcomes are stored in the directory: ",  tags$b(paste0(dir.name(),"/BSM/outputs/",nm))))})
    })

    output$download_pic_A <- downloadHandler(
      filename = function(){
        paste("pic_",input$Run_select_pic,Sys.Date(),".", input$format_picA,sep="")},
      content = function(file) {
        ggplot2::ggsave(file,plot=run_pictures[[paste0("pic_",input$Run_select_pic)]],
                        device =  input$format_picA, width =  input$width_pic_A, height = input$height_pic_A, units = "cm",  dpi = as.integer(input$dpi_pic_A))
      })

    ###########################
    ###### RUN FORECAST  ######
    ###########################
    
    output$Run_forecast_info1=shinydashboard::renderValueBox({
      shinydashboard::valueBox( shiny::h4("Run model forecast"),shiny::h5("Select the basic parameters for forcasting and press 'Start' button to run forecast.
              The first time, forecast runs with default scenarios After that, you can manually set the scenarios and the corresponding parameterization."),
                                icon = shiny::icon("envelope"),
                                color = "light-blue")
    })
    
    output$scenaria <- renderUI({
     # req(Active_CVs())
      # shiny::validate(need( as.integer(length(Active_CVs()$onoff[Active_CVs()$onoff==1]))>0, "Select at least one index!"))
      # Active_CVs_=Active_CVs()
      
      # CPUEs <- as.integer(length(Active_CVs_$onoff[Active_CVs_$onoff==1])) # default 2
      # CPUEs_names <-Active_CVs_$df_clnms[Active_CVs_$onoff==1] # default 2
      # 
      lapply(1:input$Scen_years, function(i) {
        numericInput(inputId =paste0("Scen_",i), label =paste("Scenario",i),
                    min = 0, max = 10, value = 0.2*as.integer(i), step = 0.1)
      })
    })

    switch_on=eventReactive(input$Start_forecast,{
    sss=T
    })
    
    
    ABC_FW=reactive({
      req(switch_on())
      if (input$Manual_scenarios==F) {
        ABC_FW_=ABC.forward(BSM_run(),ABC_object_final(),nyears=input$N_prj_years,status.quo_years=input$Sq_years,interim.quant =input$interim_par,quant =input$ForC,Manual_Scenarios=NULL )} else {
          scen_names= c(paste0("Scen_",seq(1,input$Scen_years,1)))
          mscen <- sapply(1:input$Scen_years, function(i) {
            as.numeric(input[[ scen_names[i]]])
          })
          ABC_FW_=ABC.forward(BSM_run(),ABC_object_final(),status.quo_years=input$Sq_years,interim.quant =input$interim_par,quant =input$ForC,Manual_Scenarios=mscen )
        }
      return(ABC_FW_)
    })
    
    ABC_FW <- ABC_FW %>% debounce(500)
    
    observeEvent(input$Start_forecast, {
      req(ABC_FW())
      
      if (input$Id049=="A") {
        nm=object_NAME()} else if (input$Id049=="B") {
          nm=input$Id081}
      # device_="tiff"
      # 
      # ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","rk_pic."),device_),plot=run_pictures$pic_A, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      # ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Catch_and_MSY."),device_),plot=run_pictures$pic_B, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      # ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Stock_size."),device_),plot=run_pictures$pic_C, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      # ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Exploitation_rate."),device_),plot=run_pictures$pic_D, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      # 
      # ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Equilibrium_curve."),device_),plot=run_pictures$pic_E, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      # ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Kobe_plot."),device_),plot=run_pictures$pic_F, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      # ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","Management_graphs."),device_),plot=run_pictures$pic_G, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      # ggsave(filename=paste0(paste0(dir.name(),"/BSM/outputs/",nm,"/","prior_posterior_distributions."),device_),plot=run_pictures$pic_H, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      # save(ABC_object,  file =paste0(dir.name(),"/BSM/outputs/",nm,"/","ABC_obj_",gsub(" ", "_",fstc()[["input"]][["Stock_info"]][["ScientificName"]]), "_",Sys.Date(), ".RData"))
      # write.csv(cbind(fstc()[["input"]][["Stock_info"]],fstc()[["input"]][["Input_parameters"]]), paste0(dir.name(),"/BSM/outputs/",nm,"/input_parameters.csv"), row.names = F)
      # write.csv(ABC_object[["output"]][["output_timeseries"]], paste0(dir.name(),"/BSM/outputs/",nm,"/output_timeseries.csv"), row.names = TRUE)
      write.csv(ABC_FW(), paste0(dir.name(),"/BSM/outputs/",nm,"/output_FORWARDs.csv"), row.names = TRUE)
      
      #  write.csv(ABC_object[["output"]][["output_posteriors"]], paste0(dir.name(),"/BSM/outputs/",nm,"/output_posteriors.csv"), row.names = TRUE)
      
      Save_done <- showNotification(paste("Message: ", "The forecast outcomes are saved in your working directory"), duration = 10)
      
    })
    
    
    
    output$interim_text <-renderText({
      req(ABC_FW())
      Interim_year=max(ABC_FW()$year[ABC_FW()$Scenario=="fit"],na.rm = T)+1
      #ABC_FW()=unique(ABC_FW()[ABC_FW()$year==Interim_year,-2])
      
        if (input$ForC=="Catch") {
          if (input$Sq_years==1) {
            
            paste("Catch for ",Interim_year, "was equal to ", (Interim_year-1), "Catch." ) } else {
              paste("Catch for  ",Interim_year, "was equal to the mean of  ", (Interim_year-input$Sq_years), "-",(Interim_year-1), "Catches." )
            }} else   {
        if (input$Sq_years==1) {
          paste("Fishing mortality for ",Interim_year, "was equal to ", (Interim_year-1), "Fishing mortality." ) } else {
            paste("Fishing mortality for ",Interim_year, "was equal to the mean of  ", (Interim_year-input$Sq_years), "-",(Interim_year-1), "Fishing mortalities." )
          }} 
        })
    
    output$interim_assumptions= renderTable({
      req(ABC_FW())
      Interim_sum=ABC_FW()[,c("year","Scenario","Catch","Catch_low","Catch_high", "FFmsy","FFmsy_low","FFmsy_high","bk","bk_low","bk_high"  , "B","B_low","B_high")]
      
      Interim_year=max(Interim_sum$year[Interim_sum$Scenario=="fit"],na.rm = T)+1
      Interim_sum=unique(Interim_sum[Interim_sum$year==Interim_year,-2])
      Interim_sum$Catch=round( Interim_sum$Catch)
      Interim_sum$Catch_low =round( Interim_sum$Catch_low )
      Interim_sum$Catch_high=round( Interim_sum$Catch_high)
      
      Interim_sum$FFmsy =round( Interim_sum$FFmsy,2)
      Interim_sum$FFmsy_low =round( Interim_sum$FFmsy_low,2)
      Interim_sum$FFmsy_high=round( Interim_sum$FFmsy_high,2)
      
      Interim_sum$bk =round( Interim_sum$bk,2)
      Interim_sum$bk_low =round( Interim_sum$bk_low,2)
      Interim_sum$bk_high=round( Interim_sum$bk_high,2)
      
      Interim_sum$B =as.integer(round( Interim_sum$B))
      Interim_sum$B_low=as.integer(round( Interim_sum$B_low))
      Interim_sum$B_high=as.integer(round( Interim_sum$B_high))
      
      
      if (input$ForC!="Catch") {
        xx=data.table(Var=c("F/Fmsy","Catch","B/k","Biomass"),
                      Value=as.character(c(Interim_sum$FFmsy,Interim_sum$Catch,Interim_sum$bk, Interim_sum$B )),
                      Lower_limit=c("",Interim_sum$Catch_low,Interim_sum$bk_low,Interim_sum$B_low),
                      Upper_limit=c("",Interim_sum$Catch_high,Interim_sum$bk_high,Interim_sum$B_high))
  colnames(xx)=c("","Value","Lower limit","Upper limit")
        
            }
      
      if (input$ForC=="Catch") {
        xx=data.table(Var=c("F/Fmsy","Catch","B/k","Biomass"),
                      Value=as.character(c(Interim_sum$FFmsy,Interim_sum$Catch,Interim_sum$bk, Interim_sum$B )),
                      Lower_limit=c(Interim_sum$FFmsy_low,"",Interim_sum$bk_low,Interim_sum$B_low),
                      Upper_limit=c(Interim_sum$FFmsy_high,"",Interim_sum$bk_high,Interim_sum$B_high))
        colnames(xx)=c("","Value","Lower limit","Upper limit")
        
         }
      
      #ABC_FW()=unique(ABC_FW()[ABC_FW()$year==Interim_year,-2])
      return(xx)
    })
    

    output$advice_outcomes= renderTable({
      req(ABC_FW())
      Interim_sum=ABC_FW()[,c("year","Scenario","Catch","Catch_low","Catch_high",  "f","B","B_low","B_high")]
      
      last_year=Interim_sum[Interim_sum$Scenario=="fit" & Interim_sum$year==max(as.integer(Interim_sum$year[Interim_sum$Scenario=="fit"])),]
      Interim_years=max(Interim_sum$year[Interim_sum$Scenario=="fit"],na.rm = T)+2
      Interim_sum=Interim_sum[Interim_sum$year %in% c(Interim_years,(Interim_years+1)),]
      Interim_sum$Catch=as.integer(round( Interim_sum$Catch))
      Interim_sum$Catch_low =as.integer(round( Interim_sum$Catch_low ))
      Interim_sum$Catch_high=as.integer(round( Interim_sum$Catch_high))
      Interim_sum$f=round( Interim_sum$f,3)
      Interim_sum$B =as.integer(round( Interim_sum$B,0))
      Interim_sum$B_low =round( Interim_sum$B_low,0)
      Interim_sum$B_high=round( Interim_sum$B_high,0)
      
      f_table1=Interim_sum[Interim_sum$year==min(as.integer(Interim_sum$year)),]
      f_table2=Interim_sum[Interim_sum$year==max(as.integer(Interim_sum$year)),]
      
      diff_perc_catch=round(100*(f_table1$Catch-last_year$Catch)/last_year$Catch,2)
      diff_perc_B=round(100*(f_table2$B-last_year$B)/last_year$B,2)
      f_table1$'Catch range'=paste0(f_table1$`Catch_low`,"-",f_table1$`Catch_high`)
      f_table2$'B range'=paste0(f_table2$B_low,"-",f_table2$B_high)
      
      
      # colnames(f_table1)= c("Scenario","1997 Catch","1997 Catch range",
      #                      "1997 Fishing mortality","1998 Biomass",
      #                      "1998 Biomass range","Catch change [%]","Biomass change [%]")
      
      colnames(f_table1)[c(3,6,10)]=c("Catch","Fishing mortality","Catch range")
      colnames(f_table2)[c(7,10)]=c("Biomass","Biomass range")
      colnames(f_table1)[c(3,6,10)]=paste(min(as.integer(Interim_sum$year)),colnames(f_table1)[c(3,6,10)])
      colnames(f_table2)[c(7,10)]=paste(max(as.integer(Interim_sum$year)),colnames(f_table2)[c(7,10)])
      
      #f_table1=f_table1[,c(2,3,10,6)]
      #f_table2=f_table2[,c(6,9)]
      f_table=cbind(f_table1[,c(2,3,10,6)],f_table2[,c(7,10)],diff_perc_catch,diff_perc_B)
      f_table=f_table[,c(1:5,7,8)]
      
      colnames(f_table)[6]= paste(min(as.integer(Interim_sum$year)),"Catch change [%]")
      colnames(f_table)[7]= paste(max(as.integer(Interim_sum$year)),"Biomass change [%]")
      return(f_table)
    })
    
    output$FFMSY_forecast= shiny::renderPlot({
      req(ABC_FW())
      to_sim=ABC_FW()
      interim_year=to_sim$year[to_sim$Scenario=="fit"]
      interim_year=max(as.integer(interim_year))+1
      if (input$for_CI==F) {
      ########### F/FMSY
      AA= ggplot(data=to_sim)+geom_hline(aes(yintercept =1,linetype=""))+
        geom_line(data=to_sim[to_sim$Scenario=="fit",],aes(x=year,y=FFmsy ,size="fitted"),size=1,col="blue")+
       # ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario=="fit",],ggplot2::aes(x=year,ymin =FFmsy_low , ymax =FFmsy_high),fill = "blue",linetype="dashed", alpha=0.3)+
        geom_line(data=to_sim[to_sim$Scenario!="fit",],aes(x=year,y=FFmsy ,col=Scenario),size=1)+
       # ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario!="fit",],ggplot2::aes(x=year,ymin =FFmsy_low , ymax =FFmsy_high,fill = Scenario),linetype="dashed", alpha=0.3)+
        geom_point(data=to_sim[to_sim$year==interim_year,],aes(x=year,y=FFmsy,shape="interim year" ),size=2)+
        geom_vline(xintercept = interim_year-1,linetype="dotted")+
        geom_vline(xintercept = interim_year+1,linetype="dotted")+
        scale_linetype_manual(labels=c("Fmsy"),values = c("dashed"))+
        scale_size_manual(labels="fitted",values = 1)+
        theme_classic()+labs(x="Year",y="F/Fmsy",linetype="",color="Scenarios",size="",fill="Scenarios",shape="")+
        scale_y_continuous(limits = c(0,1.1*max(to_sim$FFmsy)),expand=c(0,0))} else {
          
          AA= ggplot(data=to_sim)+geom_hline(aes(yintercept =1,linetype=""))+
            geom_line(data=to_sim[to_sim$Scenario=="fit",],aes(x=year,y=FFmsy ,size="fitted"),size=1,col="blue")+
            ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario=="fit",],ggplot2::aes(x=year,ymin =FFmsy_low , ymax =FFmsy_high),fill = "blue",linetype="dashed", alpha=0.3)+
            geom_line(data=to_sim[to_sim$Scenario!="fit",],aes(x=year,y=FFmsy ,col=Scenario),size=1)+
            ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario!="fit",],ggplot2::aes(x=year,ymin =FFmsy_low , ymax =FFmsy_high,fill = Scenario),linetype="dashed", alpha=0.3)+
            geom_point(data=to_sim[to_sim$year==interim_year,],aes(x=year,y=FFmsy,shape="interim year" ),size=2)+
            geom_vline(xintercept = interim_year-1,linetype="dotted")+
            geom_vline(xintercept = interim_year+1,linetype="dotted")+
            scale_linetype_manual(labels=c("Fmsy"),values = c("dashed"))+
            scale_size_manual(labels="fitted",values = 1)+
            theme_classic()+labs(x="Year",y="F/Fmsy",linetype="",color="Scenarios",size="",fill="Scenarios",shape="")+
            scale_y_continuous(limits = c(0,1.1*max(to_sim$FFmsy_high)),expand=c(0,0))
        }
      return(AA)
    })
    
    output$bk_forecast= shiny::renderPlot({
      req(ABC_FW())
      req(ABC_object_final())
      ABCOBJ=ABC_object_final()

      BMSY=ABCOBJ[["output"]][["output_posteriors"]][["Bmsy_post"]][1]
      BMSY_low=ABCOBJ[["output"]][["output_posteriors"]][["Bmsy_post"]][2]
      BMSY_high=ABCOBJ[["output"]][["output_posteriors"]][["Bmsy_post"]][3]
      
      to_sim=ABC_FW()
      interim_year=to_sim$year[to_sim$Scenario=="fit"]
      interim_year=max(as.integer(interim_year))+1
      to_sim$BMSY_low=BMSY_low
      to_sim$BMSY_high=BMSY_high
      if (input$for_CI==F) {
        AA=   ggplot(data=to_sim)+
          geom_line(data=to_sim[to_sim$Scenario=="fit",],aes(x=year,y=B ),linetype="solid",size=1,col="blue")+
          geom_hline(aes(yintercept = BMSY,linetype="AAA"),size=1,col="black")+
          geom_hline(aes(yintercept = BMSY/2,linetype="BBB"),size=1,col="black")+
          #  ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario=="fit",],ggplot2::aes(x=year,ymin =bk_low , ymax =bk_high),fill = "blue",linetype="dashed", alpha=0.3)+
         # ggplot2::geom_ribbon(data=to_sim,ggplot2::aes(x=year,ymin =BMSY_low , ymax =BMSY_high,linetype="AAA"),alpha=0.2)+
          geom_line(data=to_sim[to_sim$Scenario!="fit",],aes(x=year,y=B ,col=Scenario),size=1)+
          geom_point(data=to_sim[to_sim$year==interim_year,],aes(x=year,y=B,shape="interim year" ),size=2)+
          geom_vline(xintercept = interim_year-1,linetype="dotted")+
          geom_vline(xintercept = interim_year+1,linetype="dotted")+
          scale_fill_manual(labels=c("Bmsy"),values = c("gray"))+
          scale_linetype_manual(labels=c("Bmsy","Blim"),values = c("dashed","dotted"))+
          scale_size_manual(labels="fitted",values = 1)+
          theme_classic()+labs(x="Year",y="Biomass",linetype="",color="Scenarios",shape="",fill="")+
          scale_y_continuous(limits = c(0,NA ),expand=c(0,0))+ 
          scale_x_continuous(expand=c(0,0))+ 
          guides(linetype = guide_legend(override.aes = list(fill = c("gray","white")#,
                                                             #     color = c("black","black","blue"))
          )))
      } else {
            
            AA= ggplot(data=to_sim)+
              geom_line(data=to_sim[to_sim$Scenario=="fit",],aes(x=year,y=B ),linetype="solid",size=1,col="blue")+
              geom_hline(aes(yintercept = BMSY,linetype="AAA"),size=1,col="black")+
              geom_hline(aes(yintercept = BMSY/2,linetype="BBB"),size=1,col="black")+
              ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario=="fit",],ggplot2::aes(x=year,ymin =B_low, ymax =B_high),fill = "blue",linetype="dashed", alpha=0.3)+
              ggplot2::geom_ribbon(data=to_sim,ggplot2::aes(x=year,ymin =BMSY_low , ymax =BMSY_high,linetype="AAA"),alpha=0.2)+
              geom_line(data=to_sim[to_sim$Scenario!="fit",],aes(x=year,y=B ,col=Scenario),size=1)+
              geom_point(data=to_sim[to_sim$year==interim_year,],aes(x=year,y=B,shape="interim year" ),size=2)+
              geom_vline(xintercept = interim_year-1,linetype="dotted")+
              geom_vline(xintercept = interim_year+1,linetype="dotted")+
              scale_fill_manual(labels=c("Bmsy"),values = c("gray"))+
              scale_linetype_manual(labels=c("Bmsy","Blim"),values = c("dashed","dotted"))+
              scale_size_manual(labels="fitted",values = 1)+
              theme_classic()+labs(x="Year",y="Biomass",linetype="",color="Scenarios",shape="",fill="")+
              scale_y_continuous(limits = c(0,NA ),expand=c(0,0))+ 
              scale_x_continuous(expand=c(0,0))+ 
              guides(linetype = guide_legend(override.aes = list(fill = c("gray","white")#,
                                                                 #     color = c("black","black","blue"))
              )))
          }
      return(AA)
    })
    
    output$catch_forecast= shiny::renderPlot({
      req(ABC_FW())
      to_sim=ABC_FW()
      interim_year=to_sim$year[to_sim$Scenario=="fit"]
      interim_year=max(as.integer(interim_year))+1
      
      if (input$for_CI==F) {
        AA=  ggplot(data=to_sim)+#geom_hline(aes(yintercept = msy,linetype=""))+
          # ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario=="fit",],ggplot2::aes(x=year,ymin =Catch_low , ymax =Catch_high),fill = "blue",linetype="dashed", alpha=0.3)+
          geom_line(data=to_sim[to_sim$Scenario=="fit",],aes(x=year,y=Catch   ,size="fitted"),col="blue")+
          #  ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario!="fit",],ggplot2::aes(x=year,ymin =Catch_low , ymax =Catch_high,fill = Scenario),linetype="dashed", alpha=0.3)+
          geom_line(data=to_sim[to_sim$Scenario!="fit",],aes(x=year,y=Catch   ,col=Scenario),size=1)+
          geom_vline(xintercept = interim_year-1,linetype="dotted")+
          geom_vline(xintercept = interim_year+1,linetype="dotted")+
          geom_point(data=to_sim[to_sim$year==interim_year,],aes(x=year,y=Catch,shape="interim year" ),size=2)+
           scale_linetype_manual(labels=c("bmsy"),values = c("dashed"))+
          scale_size_manual(labels="fitted",values = 1)+
          theme_bw()+labs(x="Year",y="Catch",linetype="",color="Scenarios",size="")+
          scale_y_continuous(limits = c(0,1.1*max(to_sim$Catch  )),expand=c(0,0))} else {
            
            AA= ggplot(data=to_sim)+#geom_hline(aes(yintercept = msy,linetype=""))+
              ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario=="fit",],ggplot2::aes(x=year,ymin =Catch_low , ymax =Catch_high),fill = "blue",linetype="dashed", alpha=0.3)+
              geom_line(data=to_sim[to_sim$Scenario=="fit",],aes(x=year,y=Catch   ,size="fitted"),col="blue")+
              ggplot2::geom_ribbon(data=to_sim[to_sim$Scenario!="fit",],ggplot2::aes(x=year,ymin =Catch_low , ymax =Catch_high,fill = Scenario),linetype="dashed", alpha=0.3)+
              geom_line(data=to_sim[to_sim$Scenario!="fit",],aes(x=year,y=Catch   ,col=Scenario),size=1)+
              geom_vline(xintercept = interim_year-1,linetype="dotted")+
              geom_vline(xintercept = interim_year+1,linetype="dotted")+
              geom_point(data=to_sim[to_sim$year==interim_year,],aes(x=year,y=Catch,shape="interim year" ),size=2)+
               scale_linetype_manual(labels=c("bmsy"),values = c("dashed"))+
              scale_size_manual(labels="fitted",values = 1)+
              theme_bw()+labs(x="Year",y="Catch",linetype="",color="Scenarios",size="")+
              scale_y_continuous(limits = c(0,1.1*max(to_sim$Catch_high  )),expand=c(0,0))
          }
      
      return(AA)
      
    })
    
    
    observe({
      if (input$quit == 1) stopApp()
    })
  
  }
  
  shinyApp(ui=shinyUI, server=shinyServer)

