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
  if (mu==1) {
    mu=0.99
  }
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

# beta.prior = function(b.prior){
#   bk.beta = matrix(0,nrow = 3,ncol=3)
#   for(i in 1:3){
#     sd.bk = (as.numeric(b.prior[2,i])-as.numeric(b.prior[1,i]))/(4*0.98)
#     mu.bk = mean(as.numeric(b.prior[1:2,i]))
#     cv.bk = sd.bk/mu.bk
#     bk.beta[1:2,i] = get_beta(mu.bk,cv.bk)
#   }
#   return(bk.beta)
# }

beta.prior = function(b.prior,nbk=3){
  bk.beta = matrix(0,nrow = 3,ncol=nbk)
  for(i in 1:nbk){
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
  bk.beta = beta.prior(b.prior,nbk)
  
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


MSY.calculator=function(catch.obj) {
  nyr          <- length(catch.obj$ct_data$yr) # number of years in the time series
  max.yr.i     <- which.max(catch.obj$ct_data$ct_smthd)
  sd.ct        <- sd(catch.obj$ct_data$ct_smthd,na.rm = T)
  mean.ct      <- mean(catch.obj$ct_data$ct_smthd,na.rm = T)
  min_max      <-  catch.obj$ct_data$ct_smthd[which.min(catch.obj$ct_data$ct_smthd)]/catch.obj$ct_data$ct_smthd[ which.max(catch.obj$ct_data$ct_smthd)]
  ct.sort     <- sort(catch.obj$ct_data$ct)
  if(max.yr.i>(nyr-4) || ((sd.ct/mean.ct) < 0.1 && min_max > 0.66)) {
    MSY.pr <- mean(ct.sort[(nyr-2):nyr]) } else {
      MSY.pr <- 0.75*mean(ct.sort[(nyr-4):nyr]) } # else, use fraction of mean of 5 highest catches as MSY prior
  
  sd.log.msy.pr <- 0.3 # rounded upward to account for reduced variability in selected stocks
  log.msy.pr    <- log(MSY.pr)
  prior.msy     <- c(exp(log.msy.pr-1.96*sd.log.msy.pr),exp(log.msy.pr+1.96*sd.log.msy.pr))
  init.msy      <- MSY.pr
  MSY.pr     <- exp(log.msy.pr)#*1000
  
  return(MSY.pr)
}

ANN.priors=function(Catch.obj,MSY ) {
  nn_file<- "ffnn.bin"
  #   req(input$Priors_but2)
  min_max= Catch.obj$ct_data$ct_smthd[which.min(Catch.obj$ct_data$ct_smthd)]/ Catch.obj$ct_data$ct_smthd[which.max(Catch.obj$ct_data$ct_smthd)]
  nyr          <- length(Catch.obj$ct_data$yr) # number of years in the time series
  if(min_max > 0.7) { # if catch is about flat, use middle year as int.yr
    int.yr    <- as.integer(mean(c(input$yr_slct[1], input$yr_slct[2])))
  } else { # only consider catch 5 years away from end points and within last 30 years # 50
    yrs.int       <- Catch.obj$ct_data$yr[Catch.obj$ct_data$yr>(Catch.obj$ct_data$yr[nyr]-30) & Catch.obj$ct_data$yr>Catch.obj$ct_data$yr[4] & Catch.obj$ct_data$yr<Catch.obj$ct_data$yr[nyr-4]]
    ct.int        <- Catch.obj$ct_data$ct_smthd[Catch.obj$ct_data$yr>(Catch.obj$ct_data$yr[nyr]-30) & Catch.obj$ct_data$yr>Catch.obj$ct_data$yr[4] & Catch.obj$ct_data$yr<Catch.obj$ct_data$yr[nyr-4]]
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
            "yr.norm.min"=yr.norm.min)
  )
  
  ct.raw=Catch.obj$ct_data$ct
  ct=Catch.obj$ct_data$ct_smthd
  mean.ct      <- mean(Catch.obj$ct_data$ct_smthd,na.rm = T)
  max.ct       <- Catch.obj$ct_data$ct_smthd[which.max(Catch.obj$ct_data$ct_smthd)]
  
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
  ct_max.1          = ct[1]/max.ct
  ct_MSY.1          = ct[1]/MSY
  mean.ct_MSY.start = mean(ct[1:5])/MSY
  ct_MSY.int        = ct[which(Catch.obj$ct_data$yr==int.yr)]/MSY
  ct_max.end        = ct[nyr]/max.ct
  ct_MSY.end        = ct[nyr]/MSY
  max.ct.i          = which.max(ct)/nyr
  int.ct.i          = which( Catch.obj$ct_data$yr==int.yr)/nyr
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
  empBKpriors=round(empBKpriors,4)}

rk.priors=function(Catch.obj,r.priors,  n=5000,Stock_objID=1,Plot=T) {
  ##### Stock_objID ####### facilitates the shiny app, ignore it any way
  
  resiliences=c("Very low","Low","Medium","High")
  
  if (r.priors[1] %in% resiliences) {
    
    if (r.priors=="Very low"){
      rlow=0.015
      rhigh=0.1} else if (r.priors=="Low"){
        rlow=0.05
        rhigh=0.5} else if (r.priors=="Medium"){
          rlow=0.2
          rhigh=0.8} else if (r.priors=="High"){
            rlow=0.6
            rhigh=1.5
            
          }
    rpriors=c(r.priors,rlow,rhigh)
  } else if (is.numeric(r.priors) & length(r.priors)==2) {
    rlow=r.priors[1]
    rhigh= r.priors[2]
    rpriors=c(NA,rlow,rhigh)
  } else {
    stop("For r priors, select either one of the following: 
    'Very low','Low','Medium','High',
         or a vector with two values, r lower and upper limits." )         
  }
  
  MSY=MSY.calculator(Catch.obj)
  rlow=as.numeric(rpriors[2])
  rhigh=as.numeric(rpriors[3])
  #n            <- 5000 # number of points in multivariate cloud in graph panel (b)
  
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
  rk_priors=list(rpriors=rpriors,
                 rkpriors=data.frame(Stock_objID,
                                     prior.k.low=prior.k[1],
                                     prior.k.hi=prior.k[2],
                                     init.k=init.k,
                                     init.r=init.r,
                                     rk.cor=rk.cor),
                 rkplots=data.frame(ri1=ri1,ki1=ki1),
                 rkplotsemp=data.frame(ri.emp=ri.emp,ki1.emp=ki1.emp),
                 mean.log.k=mean.log.k,
                 sd.log.k=sd.log.k)
  
  pick_rk<-
    ggplot2::ggplot() +
    ggplot2::geom_point(data=rk_priors[["rkplots"]],ggplot2::aes(x=ri1,y=ki1),color="black",size=0.7,alpha=0.4)+
    ggplot2::scale_x_continuous(trans='log',limits=c(0.95*quantile(rk_priors[["rkplots"]]$ri1,0.001),1.2*quantile(rk_priors[["rkplots"]]$ri1,0.999)),labels = function(x) round(as.numeric(x),2)) +
    ggplot2::scale_y_continuous(trans='log',limits=c(0.95*quantile(rk_priors[["rkplots"]]$ki1,0.001),1.2*quantile(rk_priors[["rkplots"]]$ki1,0.999)),labels = function(x) round(as.numeric(x))) +
    ggplot2::geom_text(aes(x=0.9*quantile(rk_priors[["rkplots"]]$ri1,0.999) ,y=1.1*quantile(rk_priors[["rkplots"]]$ki1,0.999)),label=paste("k priors: ", round(prior.k[1]), " - ",round(prior.k[2]), "t"))+
    ggplot2::theme_classic()+
    ggplot2::labs(y="k (tonnes)", x="r (1/year)")+#,title=my_y_title
    ggplot2::geom_rect(ggplot2::aes(xmin = as.numeric(rk_priors$rpriors[2]),
                                    xmax = as.numeric( rk_priors$rpriors[3]),
                                    ymin =  as.numeric(rk_priors[["rkpriors"]]$prior.k.low),
                                    ymax =  as.numeric(rk_priors[["rkpriors"]]$prior.k.hi)),linetype="solid",fill=NA,colour = "black") 
  if (Plot==T) {
    print(pick_rk)
  }
  rk_priors=list(rpriors=rpriors,
                 msy_prior=MSY,
                 rkpriors=data.frame(Stock_objID,
                                     prior.k.low=prior.k[1],
                                     prior.k.hi=prior.k[2],
                                     init.k=init.k,
                                     init.r=init.r,
                                     rk.cor=rk.cor),
                 rkplots=data.frame(ri1=ri1,ki1=ki1),
                 rkplotsemp=data.frame(ri.emp=ri.emp,ki1.emp=ki1.emp),
                 mean.log.k=mean.log.k,
                 sd.log.k=sd.log.k,
                 pic=pick_rk)               
  return(rk_priors) 
}

q.priors=function(Biomass_object,rk_object) {
  if (is.null(Biomass_object)) {
    q.init=1
    q.prior= c(0.99,1.01)
  }else if (!is.null(Biomass_object)) { 
    if(Biomass_object$bt_param$Biom_type=="biomass") {
      q.biomass.pr <- c(0.9,1.1) # if btype=="biomass" this is the prior range for q
      q.prior <- q.biomass.pr 
      q.init  <- mean(q.prior)
    } else { # if btype is CPUE
      # get mean of 3 highest bt values
      bt=Biomass_object$bt_data$bt[!is.na(Biomass_object$bt_data$bt_smthd)]
      bt.sort <- sort(bt)
      mean.max.bt <- mean(bt.sort[(length(bt.sort)-2):length(bt.sort)],na.rm = T)
      # Estimate q.prior[2] from max cpue = q * k, q.prior[1] from max cpue = q * 0.25 * k
      q.1           <- mean.max.bt/rk_object[["rkpriors"]]$prior.k.hi
      q.2           <- mean.max.bt/(0.25*rk_object[["rkpriors"]]$prior.k.low)
      q.prior       <- c(q.1,q.2)
      q.init        <- mean(q.prior)}
  }
  return(c(q.prior,q.init))
} 

Catch_obj=function(inp,start_yr=NA, end_yr=NA,smoother_bw=1,Catch_CV=0.15,Plot=T,ShowCV=T ) {
  minyr=min(inp$yr[!is.na(inp$ct)])
  maxyr=max(inp$yr[!is.na(inp$ct)])
  ct_obj=data.frame(yr=seq(minyr,maxyr,1))
  ct_obj=merge(x = ct_obj, y = inp, all.x = TRUE)
  if( is.na(start_yr)) {
    ct_obj=ct_obj[ct_obj$yr>=minyr,]
  } else {
    ct_obj=ct_obj[ct_obj$yr>=start_yr,]
  }
  if( is.na(end_yr)) {
    ct_obj=ct_obj[ct_obj$yr<=maxyr,]
  } else {
    ct_obj=ct_obj[ct_obj$yr<=end_yr,]
  }
  ct_obj$ct_smthd = ksmooth(x=ct_obj$yr,y=ct_obj$ct,kernel="normal",n.points=length(ct_obj$yr),bandwidth=smoother_bw)$y
  
  lower_cv=(1- Catch_CV)*ct_obj$ct_smthd
  upper_cv=(1+ Catch_CV)*ct_obj$ct_smthd
  if (ShowCV==F) {  
    p1= ggplot2::ggplot(data=ct_obj,ggplot2::aes(x=yr)) +
      ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(ct_obj$ct,upper_cv),na.rm = T)))+
      ggplot2::scale_x_continuous(limits = c(min(ct_obj$yr,na.rm=T)-1,max(ct_obj$yr,na.rm=T)+1))+
      ggplot2::geom_line(ggplot2::aes(y=ct_smthd, color="A"),size=1)+
      ggplot2::geom_line(ggplot2::aes(y=ct,color="B"),size=1)+
      # geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "A"),linetype="dashed", alpha=0.3)+
      ggplot2::geom_point(ggplot2::aes(y=ct),color="black",size=1)+
      ggplot2::theme_classic()+
      ggplot2::scale_color_manual(labels=c("Smoothed catch","Reported catch"),values=c("red","blue"))+
      ggplot2::scale_fill_manual(labels=c("CI of (smoothed) catch based on CV"),values=c("red"))+
      ggplot2::theme(legend.position="bottom")+ggplot2::labs(y="Catch", x="Year",title="Catch data",color="",fill="") 
  } else {
    p1=  ggplot2::ggplot(data=ct_obj,ggplot2::aes(x=yr)) +
      ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(ct_obj$ct,upper_cv),na.rm = T)))+
      ggplot2::scale_x_continuous(limits = c(min(ct_obj$yr,na.rm=T)-1,max(ct_obj$yr,na.rm=T)+1))+
      ggplot2::geom_line(ggplot2::aes(y=ct_smthd, color="A"),size=1)+
      ggplot2::geom_line(ggplot2::aes(y=ct,color="B"),size=1)+
      ggplot2::geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "A"),linetype="dashed", alpha=0.3)+
      ggplot2::geom_point(ggplot2::aes(y=ct),color="black",size=1)+
      ggplot2::theme_classic()+
      ggplot2::scale_color_manual(labels=c("Smoothed catch","Reported catch"),values=c("red","blue"))+
      ggplot2::scale_fill_manual(labels=c("CI of (smoothed) catch based on CV"),values=c("red"))+
      ggplot2::theme(legend.position="bottom")+ggplot2::labs(y="Catch", x="Year",title="Catch plot",color="",fill="")
  } 
  if (Plot==T) {
    print(p1) }
  
  start_yr= min(ct_obj$yr[!is.na(ct_obj$ct)])
  end_yr= max(ct_obj$yr[!is.na(ct_obj$ct)])
  ct_obj=ct_obj[,c("yr","ct","ct_smthd")]
  ct_obj_=list(ct_data=ct_obj,ct_param=data.frame(start_yr=start_yr,end_yr=end_yr,Catch_CV=Catch_CV,smoother_bw=smoother_bw),pic=p1)
  return(ct_obj_)
}




Bio_obj=function(inp,start_yr=NA, end_yr=NA,smoother_bw=1,ecreep=F,ecreep_yr=NA,ecreepvalue=2,Biom_type=c("CPUE","Biomass")[1],CPUE_CV=0.2,Plot=T,ShowCV=T) {
  if (all(is.na(inp$bt))) {
    print("There is no biomass index in the dataset")
  }
  if (is.na(start_yr)) {
    minyr=min(inp$yr[!is.na(inp$bt)])
  } else {
    minyr=start_yr
  }
  
  if (is.na(end_yr)) {
    maxyr=max(inp$yr[!is.na(inp$bt)])
  } else {
    maxyr=end_yr
  }
  temp_bio_obj=inp[inp$yr>=minyr & inp$yr<=maxyr,]
  temp_bio_obj=temp_bio_obj[!is.na(temp_bio_obj$yr),]
  temp_bio_obj=temp_bio_obj[!is.na(temp_bio_obj$ct),]
  temp_bio_obj$bt_iterp=imputeTS::na_interpolation(temp_bio_obj$bt, option="linear") #"spline" , "stine"
  temp_bio_obj$bt_smthd=ksmooth(x=temp_bio_obj$yr,y=temp_bio_obj$bt_iterp,kernel="normal",n.points=length(temp_bio_obj$yr),bandwidth=smoother_bw)$y ####set bdwd
  temp_bio_obj$ecreep=NA
  
  if (ecreep==F) {
    temp_bio_obj=temp_bio_obj
    temp_bio_obj$ecreep=NA
  } else {
    temp_ecreep=temp_bio_obj[c("yr","bt")]
    temp_ecreep$ecreep=NA
    maxyr=max(as.integer(temp_ecreep$yr[!is.na(temp_ecreep$bt)]))
    temp_ecreep$ecreep=temp_ecreep$bt
    
    if (is.na(ecreep_yr) ) {
      ecreep_year=min(temp_ecreep$yr)
    } else {
      ecreep_year=ecreep_yr
    }
    for(i in 1:(maxyr- as.integer(ecreep_year))) {
      temp_ecreep$ecreep[temp_ecreep$yr==(ecreep_year+i)]  <-
        temp_ecreep$bt[temp_ecreep$yr==(ecreep_year+i)]*(1-ecreepvalue/100)^i # equation for decay in %; first cpue without correction
    } 
    temp_bio_obj$ecreep=temp_ecreep$ecreep
    temp_bio_obj$bt_iterp=imputeTS::na_interpolation(temp_bio_obj$ecreep, option="linear") #"spline" , "stine"
    temp_bio_obj$bt_smthd=ksmooth(x=temp_bio_obj$yr,y=temp_bio_obj$bt_iterp,kernel="normal",n.points=length(temp_bio_obj$yr),bandwidth=smoother_bw)$y ####set bdwd
  } 
  temp_bio_obj$lower_cv=(1- CPUE_CV)*temp_bio_obj$bt_smthd
  temp_bio_obj$upper_cv=(1+ CPUE_CV)*temp_bio_obj$bt_smthd
  
  if (ShowCV==F) {  
    p_1= ggplot2::ggplot(data=temp_bio_obj,ggplot2::aes(x=yr)) +
      ggplot2::geom_line(ggplot2::aes(y=bt_smthd, color="B"),size=1)+
      ggplot2::geom_line(ggplot2::aes(y=bt,color="A"),size=1)+
      ggplot2::labs(y="CPUE", x="Year",title="CPUE data",color="",fill="")+
      ggplot2::geom_point(ggplot2::aes(y=bt),color="black",size=1)+ggplot2::theme_classic()+
      ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE"),values=c("blue","red"))+
      ggplot2::theme(legend.position="bottom") +
      ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(temp_bio_obj$bt,temp_bio_obj$upper_cv),na.rm = T)))#+
    # ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))
    if (ecreep==T ) {#|input$ecreepslider>0
      
      p_1= ggplot2::ggplot(data=temp_bio_obj,ggplot2::aes(x=yr)) +
        ggplot2::geom_line(ggplot2::aes(y=bt_smthd, color="B"),size=1)+
        # geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "cl on smoothed CPUE based on CV"),linetype="dashed", alpha=0.3)+
        ggplot2::geom_line(ggplot2::aes(y=bt,color="A"),size=1)+
        ggplot2::theme_classic()+
        ggplot2::theme(legend.position="bottom") +
        ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(temp_bio_obj$bt,temp_bio_obj$upper_cv),na.rm = T)))+
        #ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))+
        ggplot2::geom_line(ggplot2::aes(x=yr,y=ecreep, color="C"),size=1)+
        # ggplot2::annotate("point", x=ecreep_year,y=temp_bio_obj$bt[temp_bio_obj$yr==ecreep_year],shape="E-creep effect start year",size=4)+
        ggplot2::geom_point(ggplot2::aes(x=ecreep_year,y=bt[yr==ecreep_year],shape="E-creep effect start year"),size=4)+
        #ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(upper_cv,temp_bio_obj$bt), na.rm = T)))+
        ggplot2::labs(y="CPUE", x="Year",title="CPUE data",color="",fill="",shape="")+
        ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE","E-creeped CPUE" ),
                                    values=c("blue","red","green"))
    }
    
  } else {
    p_1= ggplot2::ggplot(data=temp_bio_obj,ggplot2::aes(x=yr)) +
      ggplot2::geom_line(ggplot2::aes(y=bt_smthd, color="B"),size=1)+
      geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "cl on smoothed CPUE based on CV"),linetype="dashed", alpha=0.3)+
      ggplot2::geom_line(ggplot2::aes(y=bt,color="A"),size=1)+
      ggplot2::geom_point(ggplot2::aes(y=bt),color="black",size=1)+
      ggplot2::theme_classic()+
      # ggplot2::geom_label(ggplot2::aes(x=min(yr)+4,y=1.2,label =paste0("q_low=", sprintf('%0.3f',q_priors()[1]), ", q_high=",sprintf('%0.3f',q_priors()[2]))),col="#F37B59", alpha=0.2)+
      ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE"),values=c("blue","red"))+
      ggplot2::theme(legend.position="bottom") +
      ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(temp_bio_obj$bt,temp_bio_obj$upper_cv),na.rm = T)))+
      ggplot2::labs(y="CPUE", x="Year",title="CPUE data",color="",fill="",shape="")#+
    # ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))
    if (ecreep==T ) {  
      
      p_1=ggplot2::ggplot(data=temp_bio_obj,ggplot2::aes(x=yr)) +
        ggplot2::geom_line(ggplot2::aes(y=bt_smthd, color="B"),size=1)+
        geom_ribbon(ggplot2::aes(ymin =lower_cv, ymax =upper_cv,fill = "cl on smoothed CPUE based on CV"),linetype="dashed", alpha=0.3)+
        ggplot2::geom_line(ggplot2::aes(y=bt,color="A"),size=1)+
        ggplot2::geom_point(ggplot2::aes(y=bt),color="black",size=1)+
        ggplot2::theme_classic()+
        # ggplot2::geom_label(ggplot2::aes(x=min(yr)+4,y=1.2,label =paste0("q_low=", sprintf('%0.3f',q_priors()[1]), ", q_high=",sprintf('%0.3f',q_priors()[2]))),col="#F37B59", alpha=0.2)+
        #ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE"),values=c("blue","red"))+
        ggplot2::theme(legend.position="bottom") +
        # ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(temp_bio_obj$bt,upper_cv),na.rm = T)))+
        #  ggplot2::scale_x_continuous(limits = c(min(Catch_obj()$yr,na.rm=T)-1,max(Catch_obj()$yr,na.rm=T)+1))+
        ggplot2::geom_line(ggplot2::aes(x=yr,y=ecreep, color="C"),size=1)+
        ggplot2::geom_point(ggplot2::aes(x=ecreep_year,y=bt[yr==ecreep_year],shape="E-creep effect start year"),size=4)+
        ggplot2::scale_y_continuous(limits = c(0,1.1*max(c(temp_bio_obj$upper_cv,temp_bio_obj$bt), na.rm = T)))+
        ggplot2::labs(y="CPUE", x="Year",title="CPUE data",color="",fill="",shape="")+
        ggplot2::scale_color_manual(labels=c("Raw CPUE","Smoothed CPUE","E-creeped CPUE" ),
                                    values=c("blue","red","green"))
    }
    
  }
  if (Plot==T) {  
    suppressWarnings(print(p_1))}
  
  start_yr= min(temp_bio_obj$yr[!is.na(temp_bio_obj$bt_smthd)])
  end_yr= max(temp_bio_obj$yr[!is.na(temp_bio_obj$bt_smthd)])
  
  
  exists("ecreep_year")
  temp_bio_obj=temp_bio_obj[,c("yr","bt","bt_iterp","bt_smthd","ecreep")]
  bt_obj_=list(bt_data=temp_bio_obj,bt_param=data.frame(bio_start_yr=start_yr,bio_end_yr=end_yr,CPUE_CV=CPUE_CV,smoother_bw=smoother_bw,ecreep=ecreep,ecreep_yr=NA,ecreepvalue=ecreepvalue,Biom_type=Biom_type ),pic=p_1)
  if ( exists("ecreep_year")){
    bt_obj_[["bt_param"]]$ecreep_yr = ecreep_year
  }
  return(bt_obj_)
  #return(temp_bio_obj)
}



Biom_priors=function(Catch.obj,nbk=3,start_bio,int_bio,int_bio_year,end_bio, Plot=T) {  #ANN_priors=T,
  
  Stock_states=c("Very low","Low","Medium","Sustainable","Unexploited")
  
  if (start_bio[1] %in%  Stock_states) {
    if (start_bio=="Unexploited"){
      ebk_stat_low=0.75
      ebk_stat_high=1} else if (start_bio=="Sustainable"){
        ebk_stat_low=0.4
        ebk_stat_high=0.8} else if (start_bio=="Medium"){
          ebk_stat_low=0.2
          ebk_stat_high=0.6} else if (start_bio=="Low"){
            ebk_stat_low=0.01
            ebk_stat_high=0.4 } else if (start_bio=="Very low"){
              ebk_stat_low=0.01
              ebk_stat_high=0.2 
            } 
    expert_bk_start_priors=c(start_bio,ebk_stat_low,ebk_stat_high)
  } else if (is.numeric(start_bio) & length(start_bio)==2) {
    ebk_stat_low=start_bio[1]
    ebk_stat_high=start_bio[2]
    expert_bk_start_priors=c(NA,ebk_stat_low,ebk_stat_high)
  } else {
    stop("For start_bio priors, put either one of the following: 
    'Very low','Low','Medium','Sustainable','Unexploited',
         or a vector with two values, lower and upper limits." )
  }
  
  if (nbk>1) {
    if (int_bio[1] %in%  Stock_states) {
      if (start_bio=="Unexploited"){
        ebk_stat_low=0.75
        ebk_stat_high=1} else if (int_bio=="Sustainable"){
          ebk_stat_low=0.4
          ebk_stat_high=0.8} else if (int_bio=="Medium"){
            ebk_stat_low=0.2
            ebk_stat_high=0.6} else if (int_bio=="Low"){
              ebk_stat_low=0.01
              ebk_stat_high=0.4 } else if (int_bio=="Very low"){
                ebk_stat_low=0.01
                ebk_stat_high=0.2 
              } 
      expert_bk_ind_priors=c(int_bio,ebk_stat_low,ebk_stat_high)
    } else if (is.numeric(int_bio) & length(int_bio)==2) {
      ebk_stat_low=int_bio[1]
      ebk_stat_high=int_bio[2]
      expert_bk_ind_priors=c(NA,ebk_stat_low,ebk_stat_high)
    } else {
      stop("For int_bio priors, put either one of the following: 
    'Very low','Low','Medium','Sustainable','Unexploited',
         or a vector with two values, lower and upper limits." )
    }
  } else {
    expert_bk_ind_priors=c(NA,NA,NA)
  }
  
  if (nbk>2) {
    if (end_bio[1] %in%  Stock_states) {
      if (end_bio=="Unexploited"){
        ebk_stat_low=0.75
        ebk_stat_high=1} else if (end_bio=="Sustainable"){
          ebk_stat_low=0.4
          ebk_stat_high=0.8} else if (end_bio=="Medium"){
            ebk_stat_low=0.2
            ebk_stat_high=0.6} else if (end_bio=="Low"){
              ebk_stat_low=0.01
              ebk_stat_high=0.4 } else if (end_bio=="Very low"){
                ebk_stat_low=0.01
                ebk_stat_high=0.2 
              } 
      expert_bk_end_priors=c(end_bio,ebk_stat_low,ebk_stat_high)
    } else if (is.numeric(end_bio) & length(end_bio)==2) {
      ebk_stat_low=end_bio[1]
      ebk_stat_high=end_bio[2]
      expert_bk_end_priors=c(NA,ebk_stat_low,ebk_stat_high)
    } else {
      stop("For end_bio priors, put either one of the following: 
    'Very low','Low','Medium','Sustainable','Unexploited',
         or a vector with two values, lower and upper limits." )
    }   
  } else {
    expert_bk_end_priors=c(NA,NA,NA)
  }  
  strprrs=as.numeric(c(expert_bk_start_priors[c(2,3)],NA))
  intprrs=as.numeric(c(expert_bk_ind_priors[c(2,3)],int_bio_year))
  endprrs=as.numeric(c(expert_bk_end_priors[c(2,3)],NA))
  
  MAN_Priors=data.frame(start=strprrs,int=intprrs,end=endprrs)   ########SWITCHER OFF
  # if(ANN_priors==T){
  msy=MSY.calculator(Catch.obj)
  ANN_priors=ANN.priors(Catch.obj,msy)
  # }
  bk_priors=list(MAN_Priors=MAN_Priors,ANN_priors=ANN_priors,params=data.frame(nbk=nbk,msy=msy))
  pic=ggbkpriors.plot(Catch.obj,bk_priors)
  if (Plot==T) {
    print(pic)
  }
  bk_priors=list(MAN_Priors=MAN_Priors,ANN_priors=ANN_priors,params=data.frame(nbk=nbk,msy=msy),pic=pic)
  return(bk_priors)
}


ggbkpriors.plot=function(Catch.obj,bk_priors) {
  #my_y_title <-bquote(atop(Compare~expert~and~ANN~priors~"for"~the~stock~bold(.(Final_stock()[["Catch_ID"]][1,"Stock"]))~of~italic(.(Final_stock()[["Catch_ID"]][1,"ScientificName"]))))
  nbk=bk_priors$params$nbk
  Cbj=Catch.obj$ct_data
  int_year_man=bk_priors$MAN_Priors$int[3]
  int_year_ANN=bk_priors$ANN_priors$int[3]
  
  ANNpriors=bk_priors$ANN_priors
  temp_ANN=data.frame(yr=c(min(Cbj$yr),int_year_ANN,max(Cbj$yr)),ln=c(mean(bk_priors$ANN_priors$start[1:2]),
                                                                      mean(bk_priors$ANN_priors$int[1:2]),
                                                                      mean(bk_priors$ANN_priors$end[1:2])))
  p_1= ggplot2::ggplot(data=Cbj,ggplot2::aes(x=yr)) +
    ggplot2::theme_classic()+
    ggplot2::scale_x_continuous(limits = c(min(Cbj$yr,na.rm=T)-1,max(Cbj$yr,na.rm=T)+1))+
    ggplot2::geom_segment(ggplot2::aes(x=min(yr),y=as.numeric(ANNpriors[1,1]), xend=min(yr),yend=as.numeric(ANNpriors[2,1]),col="A"),size=1)+
    ggplot2::geom_segment(ggplot2::aes(x=max(yr),y=as.numeric(ANNpriors[1,3]), xend=max(yr),yend=as.numeric(ANNpriors[2,3]),col="A"),size=1)+
    ggplot2::geom_segment(ggplot2::aes(x=as.numeric(ANNpriors[3,2]),y=as.numeric(ANNpriors[1,2]), xend=as.numeric(ANNpriors[3,2]),yend=as.numeric(ANNpriors[2,2]),col="A"),size=1)+
    ggplot2::geom_line(data=temp_ANN, ggplot2::aes(x=yr,y=ln,col="A"),size=1)+
    ggplot2::geom_errorbar(aes(x=min(yr), ymin=bk_priors$MAN_Priors$start[1], ymax=bk_priors$MAN_Priors$start[2],color="B"),  size=1)+
    ggplot2::scale_color_manual(values=c("gray","blue"),labels=c("ANN estimated priors","Expert priors"))+
    ggplot2::theme(legend.position="bottom")+
    ggplot2::scale_y_continuous(breaks=seq(0,1.25,0.1),limits = c(0,1.25))+
    # ggplot2::labs(y="B/k", x="Year",title=my_y_title,color="")
    ggplot2::labs(y="B/k", x="Year",color="") 
  
  if  (nbk==2) {
    temp_exp=data.frame(yr=c(min(Cbj$yr),int_year_man),ln=c(mean(bk_priors$MAN_Priors$start[1:2]),
                                                            mean(bk_priors$MAN_Priors$int[1:2])))
    
    p_1=p_1+ggplot2::geom_errorbar(aes(x=int_year_man, ymin=bk_priors$MAN_Priors$int[1], ymax=bk_priors$MAN_Priors$int[2],color="B"), size=1)+
      ggplot2::geom_line(data=temp_exp, ggplot2::aes(x=yr,y=ln,color="B"),size=1) }else if(nbk==3) {
        
        temp_exp=data.frame(yr=c(min(Cbj$yr),int_year_man,max(Cbj$yr)),ln=c(mean(bk_priors$MAN_Priors$start[1:2]),
                                                                            mean(bk_priors$MAN_Priors$int[1:2]),
                                                                            mean(bk_priors$MAN_Priors$end[1:2])))
        
        p_1=p_1+ggplot2::geom_errorbar(aes(x=int_year_man, ymin=bk_priors$MAN_Priors$int[1], ymax=bk_priors$MAN_Priors$int[2],color="B"), size=1)+   
          ggplot2::geom_errorbar(aes(x=max(yr), ymin=bk_priors$MAN_Priors$end[1], ymax=bk_priors$MAN_Priors$end[2],color="B"), size=1)+
          ggplot2::geom_line(data=temp_exp, ggplot2::aes(x=yr,y=ln,color="B"),size=1)
      }
  return(p_1)
}

CLA_stock_maker=function(Stock="Stock",Species=NA,Catch_object,Biomass_object=NULL,rk_object,bk_object,fbslb=NULL,MAN_ANN_bk="MAN") {
  if (is.null(Biomass_object)) {
    inp=Catch_object$ct_data
    inp$bt=NA
    inp$bt_iterp=NA
    inp$bt_smthd=NA
    inp$ecreep=NA 
    inp$bt_cv =NA 
    inp$Stock_objID=NA
    
  } else {
    inp1=Catch_object$ct_data
    inp2=Biomass_object$bt_data
    inp=merge(x = inp1, y = inp2, all.x = TRUE)
    
    inp$bt_cv= Biomass_object$bt_param$CPUE_CV
    inp$Stock_objID=NA}
  inp$Stock=Stock
  # colnames(inp)=c( "yr","ct","ct_smthd",
  #                 "bt","bt_iterp",
  #                 "bt_smthd","ecreep","bt_cv","Stock_objID","Stock")
  
  inp=inp[,c("Stock", "yr","ct","bt","ct_smthd","bt_iterp",
             "bt_smthd","ecreep","bt_cv","Stock_objID")]
  
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
  output_timeseries_ = data.frame(matrix(nrow =length(inp$yr), ncol = length(timeseries_clmns))) #nrow(fstck_Catch_Data)
  colnames(output_timeseries_)=timeseries_clmns
  row.names(output_timeseries_)=inp$yr
  CLA_object=list(input=list(Stock_info=input_info,Input_parameters=input_param,Input_data=inp),
                  output=list(output_posteriors=output_posteriors_,output_timeseries=output_timeseries_))
  
  CLA_object[["input"]][["Stock_info"]]$Stock=Stock
  CLA_object[["input"]][["Stock_info"]]$ScientificName =Species
  CLA_object[["input"]][["Stock_info"]]$Name =NA                  ########### seee this
  CLA_object[["input"]][["Stock_info"]]$Group  =NA
  CLA_object[["input"]][["Stock_info"]]$Continent  =NA
  CLA_object[["input"]][["Stock_info"]]$Region  =NA
  CLA_object[["input"]][["Stock_info"]]$Subregion  =NA
  CLA_object[["input"]][["Stock_info"]]$Comments  =NA
  CLA_object[["input"]][["Stock_info"]]$Source =NA
  
  CLA_object[["input"]][["Input_parameters"]]$StartYear=Catch_object$ct_param$start_yr
  CLA_object[["input"]][["Input_parameters"]]$EndYear=Catch_object$ct_param$end_yr
  CLA_object[["input"]][["Input_parameters"]]$CV_catch=Catch_object$ct_param$Catch_CV
  CLA_object[["input"]][["Input_parameters"]]$Smooth_K_catch=Catch_object$ct_param$smoother_bw
  
  CLA_object[["input"]][["Input_parameters"]]$StartYear_bt=ifelse(!is.null(Biomass_object), Biomass_object$bt_param$bio_start_yr,NA)
  CLA_object[["input"]][["Input_parameters"]]$EndYear_bt=ifelse(!is.null(Biomass_object),Biomass_object$bt_param$bio_end_yr,NA)
  CLA_object[["input"]][["Input_parameters"]]$CV_cpue=ifelse(!is.null(Biomass_object),Biomass_object$bt_param$CPUE_CV,NA)
  CLA_object[["input"]][["Input_parameters"]]$Smooth_K_cpue=ifelse(!is.null(Biomass_object),Biomass_object$bt_param$smoother_bw,NA)
  CLA_object[["input"]][["Input_parameters"]]$Smooth_cpue=ifelse(!is.null(Biomass_object),ifelse(Biomass_object$bt_param$smoother_bw==1,F,T),NA)
  CLA_object[["input"]][["Input_parameters"]]$Ecreep_cpue=ifelse(!is.null(Biomass_object),Biomass_object$bt_param$ecreep,NA)
  CLA_object[["input"]][["Input_parameters"]]$Ecreep_Year=ifelse(!is.null(Biomass_object),Biomass_object$bt_param$ecreep_yr,NA)
  CLA_object[["input"]][["Input_parameters"]]$Ecreep_value=ifelse(!is.null(Biomass_object),Biomass_object$bt_param$ecreepvalue,NA)
  CLA_object[["input"]][["Input_parameters"]]$btype=ifelse(!is.null(Biomass_object),Biomass_object$bt_param$Biom_type,NA)
  CLA_object$input$Input_parameters$Resilience= rk_object$rpriors[1]
  CLA_object$input$Input_parameters$r.low= as.numeric(rk_object$rpriors[2])
  CLA_object$input$Input_parameters$r.hi= as.numeric(rk_object$rpriors[3])
  CLA_object$input$Input_parameters$FBSLB_info_r=ifelse(is.null(fbslb),NA,fbslb[1])
  CLA_object$input$Input_parameters$FBSLB_info_Resilience=ifelse(is.null(fbslb),NA,fbslb[2])
  CLA_object$input$Input_parameters$FBSLB_page=ifelse(is.null(fbslb),NA,fbslb[6])
  CLA_object$input$Input_parameters$MSY_prior=rk_object$msy_prior
  
  qpriors=q.priors(Biomass_object,rk_object) 
  CLA_object$input$Input_parameters$q_low = qpriors[1]
  CLA_object$input$Input_parameters$q_high = qpriors[2]
  CLA_object$input$Input_parameters$ANN_stb.low = bk_object$ANN_priors$start[1]
  CLA_object$input$Input_parameters$ANN_stb.hi = bk_object$ANN_priors$start[2]
  CLA_object$input$Input_parameters$ANN_int.yr = bk_object$ANN_priors$int[3]
  CLA_object$input$Input_parameters$ANN_intb.low = bk_object$ANN_priors$int[1]
  CLA_object$input$Input_parameters$ANN_intb.hi= bk_object$ANN_priors$int[2]
  CLA_object$input$Input_parameters$ANN_endb.low= bk_object$ANN_priors$end[1]
  CLA_object$input$Input_parameters$ANN_endb.hi= bk_object$ANN_priors$end[2]
  CLA_object$input$Input_parameters$nbk= bk_object$params$nbk
  CLA_object$input$Input_parameters$stb.low= ifelse(MAN_ANN_bk=="MAN",bk_object$MAN_Priors$start[1],bk_object$ANN_priors$start[1])
  CLA_object$input$Input_parameters$stb.hi= ifelse(MAN_ANN_bk=="MAN",bk_object$MAN_Priors$start[2],bk_object$ANN_priors$start[2])
  CLA_object$input$Input_parameters$int.yr= ifelse(MAN_ANN_bk=="MAN",bk_object$MAN_Priors$int[3], bk_object$ANN_priors$int[3])
  CLA_object$input$Input_parameters$intb.low= ifelse(MAN_ANN_bk=="MAN",bk_object$MAN_Priors$int[1], bk_object$ANN_priors$int[1])
  CLA_object$input$Input_parameters$intb.hi= ifelse(MAN_ANN_bk=="MAN",bk_object$MAN_Priors$int[2], bk_object$ANN_priors$int[2])
  CLA_object$input$Input_parameters$endb.low= ifelse(MAN_ANN_bk=="MAN",bk_object$MAN_Priors$end[1], bk_object$ANN_priors$end[1])
  CLA_object$input$Input_parameters$endb.hi= ifelse(MAN_ANN_bk=="MAN",bk_object$MAN_Priors$end[2], bk_object$ANN_priors$end[2])
  CLA_object$input$Input_parameters$btype= ifelse(is.null(Biomass_object),"None",Biomass_object$bt_param$Biom_type) 
  
  CLA_object$rk_object=rk_object
  
  return(CLA_object)
}                          



CLA.fit= function(CLA.obj,METHOD=c("CMSY","BSM")[2],n.chains=2) {
  if (METHOD=="BSM" & CLA.obj$input$Input_parameters$btype=="None") {
    stop("You can not run BSM method if you don't have CPUE/Biomass index. Put METHOD='CMSY' instead.")
  } else if (METHOD!="BSM" & METHOD!="CMSY") {
    stop("Choose either METHOD='CMSY' if you don't have or want to use CPUE/Biomass index or METHOD='BSM' to run the model")
  }
  
  q_prrs= c(CLA.obj$input$Input_parameters$q_low,CLA.obj$input$Input_parameters$q_high)
  init.q_=mean(q_prrs)
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
  
  # if (ABC.obj[["input"]][["Input_parameters"]]$nbk=="Only start")  {
  #   nbk=1 } else if (ABC.obj[["input"]][["Input_parameters"]]$nbk=="Start & intermediate") {
  #     nbk=2  } else {
  #       nbk=3
  #     }
  nbk= CLA.obj$input$Input_parameters$nbk
  
  pen.bk = pen.F = rep(0,length(CLA.obj[["input"]][["Input_data"]]$ct_smthd))
  b.yrs = c(1,length(as.integer(CLA.obj[["input"]][["Input_parameters"]]$StartYear):
                       as.integer(CLA.obj[["input"]][["Input_parameters"]]$int.yr)),
            length(as.integer(CLA.obj[["input"]][["Input_parameters"]]$StartYear):
                     as.integer(CLA.obj[["input"]][["Input_parameters"]]$EndYear)))
  
  if (is.na(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low))) {
    intb.low=0.4 } else {
      intb.low=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low)
    }
  if (is.na(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi))) {
    intb.hi=0.5 } else {
      intb.hi=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi)
    }
  
  if (is.na(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.low))) {
    endb.low=0.4 } else {
      endb.low=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.low)
    }
  if (is.na(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.hi))) {
    endb.hi=0.5 } else {
      endb.hi=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.hi)
    }
  
  b.prior_ = rbind(matrix(c(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi),
                            intb.low,intb.hi,
                            endb.low,endb.hi),2,3),rep(0,3)) # last row includes the 0 penalty
  
  
  if (METHOD == "CMSY") { 
    bt.start <- mean(c(CLA.obj$rk_object[["rkpriors"]]$prior.k.low*
                         as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),
                       CLA.obj$rk_object[["rkpriors"]]$prior.k.hi*
                         as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi))) # derive proxy for first bt value
    
    btj_ <- c(bt.start,rep(NA,length(CLA.obj[["input"]][["Input_data"]]$ct_smthd)-1)) }else if (METHOD == "BSM") { 
      btj_=CLA.obj[["input"]][["Input_data"]]$bt_smthd
    }
  
  if (METHOD == "CMSY") { 
    CV.cpue_=rep(0.2,length(CLA.obj[["input"]][["Input_data"]]$ct_smthd))  }else if (METHOD == "BSM") { 
      
      if (all(is.na(CLA.obj[["input"]][["Input_data"]]$bt_cv)) &  !is.na(CLA.obj[["input"]][["Input_parameters"]]$CV_cpue)) {
        CV.cpue_      <- rep(CLA.obj[["input"]][["Input_parameters"]]$CV_cpue,length(CLA.obj[["input"]][["Input_data"]]$ct_smthd))
      } else if  (all(is.na(CLA.obj[["input"]][["Input_data"]]$bt_cv))) {
        CV.cpue_      <- rep(0.2,length(CLA.obj[["input"]][["Input_data"]]$ct_smthd)) } else {
          CV.cpue_<- CLA.obj[["input"]][["Input_data"]]$bt_cv
        }
      CV.cpue_[is.na(CV.cpue_)]=0.2
    }
  
  #CV.cpue_[is.na(CV.cpue_)]=0.2
  
  jags_outputs <- bsm(ct=as.numeric(CLA.obj[["input"]][["Input_data"]]$ct_smthd),
                      btj <-btj_,
                      nyr=length(CLA.obj[["input"]][["Input_data"]]$ct_smthd),
                      prior.r=as.numeric(c(CLA.obj[["input"]][["Input_parameters"]]$r.low,CLA.obj[["input"]][["Input_parameters"]]$r.hi)),
                      prior.k=as.numeric(c(CLA.obj$rk_object[["rkpriors"]]$prior.k.low, CLA.obj$rk_object[["rkpriors"]]$prior.k.hi)),
                      startbio=as.numeric(c(CLA.obj[["input"]][["Input_parameters"]]$stb.low,CLA.obj[["input"]][["Input_parameters"]]$stb.hi)),
                      q.priorj=as.numeric(q_prrs),#c(0.99,1.01),# # since no abundance data are available in this run,
                      init.q=init.q_ ,           # q could be omitted and is set here to (practically) 1  ,
                      init.r=CLA.obj$rk_object[["rkpriors"]]$init.r,
                      init.k=CLA.obj$rk_object[["rkpriors"]]$init.k,
                      pen.bk=pen.bk,
                      pen.F=pen.F,
                      b.yrs=b.yrs,
                      b.prior=b.prior_,
                      CV.C=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$CV_catch),# 0.15,  #><>MSY: Add Catch CV
                      CV.cpue=CV.cpue_,#input$CPUE_CV,  #><>MSY: Add minimum realistic cpue CV
                      nbk=nbk,
                      rk.cor=CLA.obj$rk_object[["rkpriors"]]$rk.cor,
                      n.chains     <- n.chains, # number of chains to be used in JAGS, default = 2
                      cmsyjags=F)$BUGSoutput$sims.list
  jags_outputs[["r"]]=as.numeric(coda::mcmc(jags_outputs[["r"]]))
  jags_outputs[["k"]]=as.numeric(coda::mcmc(jags_outputs[["k"]]))
  jags_outputs[["q"]]=as.numeric(coda::mcmc(jags_outputs[["q"]]))
  jags_outputs[["ppd.r"]]<- exp(as.numeric(coda::mcmc(jags_outputs[["ppd.logrk"]][,1])))
  jags_outputs[["ppd.k"]]<- exp(as.numeric(coda::mcmc(jags_outputs[["ppd.logrk"]][,2])))
  return(jags_outputs)  
}



extract.CLA.fit= function(fit.obj,CLA.obj) {
  
  CLA.obj[["output"]][["output_posteriors"]]$r_post=as.numeric(quantile(fit.obj[["r"]],c(0.5,0.025,0.975))) #median, 95% CIs
  CLA.obj[["output"]][["output_posteriors"]]$k_post<- as.numeric(quantile(fit.obj[["k"]],c(0.5,0.025,0.975)))
  CLA.obj[["output"]][["output_posteriors"]]$MSY_post<-as.numeric( quantile(fit.obj[["r"]]*fit.obj[["k"]]/4,c(0.5,0.025,0.975)))
  CLA.obj[["output"]][["output_posteriors"]]$q_post<-as.numeric( quantile(fit.obj[["q"]],c(0.5,0.025,0.975)))
  CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post<- as.numeric(quantile(fit.obj[["r"]]/2,c(0.5,0.025,0.975)))
  CLA.obj[["output"]][["output_posteriors"]]$Bmsy_post<- as.numeric(quantile(fit.obj[["k"]]/2,c(0.5,0.025,0.975)))
  CLA.obj[["output"]][["output_timeseries"]][, c("Catch","Catch_low","Catch_high")]=t(apply(fit.obj$ct.jags,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  CLA.obj[["output"]][["output_timeseries"]][, c("FFmsy","FFmsy_low","FFmsy_high")]=t(apply(fit.obj$FFmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  CLA.obj[["output"]][["output_timeseries"]][, c("f","f_low","f_high")]=t(apply(fit.obj$F,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  CLA.obj[["output"]][["output_timeseries"]][, c("BBmsy","BBmsy_low","BBmsy_high")]=t(apply(fit.obj$BBmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  CLA.obj[["output"]][["output_timeseries"]][, c("B","B_low","B_high")]= t(apply(fit.obj$B,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  CLA.obj[["output"]][["output_timeseries"]][, c("bk","bk_low","bk_high")]=  t(apply(fit.obj$P,2,quantile,c(0.5,0.025,0.975),na.rm=T))
  
  if(tail(CLA.obj[["output"]][["output_timeseries"]]$BBmsy,1)<0.5){
    CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1]=
      CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post[1]*2*tail(CLA.obj[["output"]][["output_timeseries"]]$BBmsy,1)
    CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2]=
      CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post[2]*2*tail(CLA.obj[["output"]][["output_timeseries"]]$BBmsy,1)
    CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3]=
      CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post[3]*2*tail(CLA.obj[["output"]][["output_timeseries"]]$BBmsy,1)
    CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_correction_note= rep("Fmsy was corrected downward to account for reduced recruitment.",3)
  } else {
    CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1]=CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post[1]
    CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2]=CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post[2]
    CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3]=CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post[3]
    CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_correction_note= rep("No correction to FFmsy",3)
  }
  return(CLA.obj)
}

ggrk.plot= function(CLA.obj,fit.obj,METHOD) {
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  my_y_title <-bquote(atop(Possible~"r-k"~pairs~"for"~bold(.(CLA.obj[["input"]][["Stock_info"]]$Stock))))
  pick_rk<-ggplot2::ggplot() +
    ggplot2::geom_point(data=CLA.obj$rk_object[["rkplots"]],ggplot2::aes(x=ri1,y=ki1),color="grey",size=0.7,alpha=0.4)+
    ggplot2::scale_x_continuous(trans='log',limits=c(ifelse(0.95*quantile(CLA.obj$rk_object[["rkplots"]]$ri1,0.001)>
                                                              CLA.obj[["output"]][["output_posteriors"]]$r_post[2],CLA.obj[["output"]][["output_posteriors"]]$r_post[2],
                                                            0.95*quantile(CLA.obj$rk_object[["rkplots"]]$ri1,0.001)),1.2*quantile(CLA.obj$rk_object[["rkplots"]]$ri1,0.999)),labels = function(x) round(as.numeric(x),2)) +
    ggplot2::scale_y_continuous(trans='log',limits=c(0.95*quantile(CLA.obj$rk_object[["rkplots"]]$ki1,0.001),1.2*quantile(CLA.obj$rk_object[["rkplots"]]$ki1,0.999)),labels = function(x) round(as.numeric(x))) +
    ggplot2::theme_classic()+
    ggplot2::labs(y="k (tonnes)", x="r (1/year)",title=my_y_title)+
    ggplot2::geom_rect(ggplot2::aes(xmin = as.numeric( CLA.obj[["input"]][["Input_parameters"]]$r.low),
                                    xmax = as.numeric( CLA.obj[["input"]][["Input_parameters"]]$r.hi),
                                    ymin =  as.numeric(CLA.obj$rk_object[["rkpriors"]]$prior.k.low),
                                    ymax =  as.numeric(CLA.obj$rk_object[["rkpriors"]]$prior.k.hi)),linetype="dotted",fill=NA,colour = "gray") +
    ggplot2::geom_point(data=data.frame(rs=fit.obj[["r"]],ks=fit.obj[["k"]]),ggplot2::aes(x=rs,y=ks),color="gray18",size=0.7,alpha=0.2)+
    ggplot2::geom_point(data= CLA.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=r_post[1],y=k_post[1]),color=clr,size=1)+
    ggplot2::geom_segment(data= CLA.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=ifelse(r_post[2]>0,r_post[2],0.001),y=k_post[1],xend=r_post[3],yend=k_post[1]),col=clr,size=0.7)+
    ggplot2::geom_segment(data= CLA.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=r_post[1],y=ifelse(k_post[2]>0,k_post[2],0.001),xend=r_post[1],yend=k_post[3]),col=clr,size=0.7)+
    theme(text = element_text(size = 10)) 
  return(pick_rk)
}



ggcatch.plot= function(CLA.obj,METHOD,Management=F) {
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  if (Management==F) {
    my_y_title <-bquote(atop(Catch~"for"~bold(.(CLA.obj[["input"]][["Stock_info"]]$Stock))))
  } else {
    my_y_title="Catch"
  }
  temp_data=as.data.frame(CLA.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  pic_catch=ggplot2::ggplot()+
    ggplot2::geom_segment(aes(y=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                              yend=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                              x = min(temp_data$yr), xend =  max( temp_data$yr) ,linetype="B",col="B"),size=1)+
    ggplot2::geom_ribbon(ggplot2::aes(x=temp_data$yr,ymin=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[2],
                                      ymax=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[3],linetype="B",fill="B"), alpha=0.2)+
    ggplot2::geom_line(data = temp_data,ggplot2::aes(x=yr,y=Catch,color="A",linetype="A"),size=0.7)+
    # ggplot2::geom_point(data = temp_data,ggplot2::aes(x=yr,y=Catch,color="A"))+
    ggplot2::geom_ribbon(data = temp_data,ggplot2::aes(x=yr,ymin=Catch_low, ymax=Catch_high,fill="A"), alpha=0.2)+#y=ct.jags_low,
    ggplot2::theme_classic()+
    ggplot2::scale_linetype_manual(labels=c("Catch and 95% CI","MSY and 95% CI"),values=c("solid", "dashed"))+
    ggplot2::scale_color_manual(labels=c("Catch and 95% CI","MSY and 95% CI"),values=c("black","darkgray"))+
    ggplot2::scale_fill_manual(labels=c("Catch and 95% CI","MSY and 95% CI"),values=c("black","darkgray"))+
    ggplot2::theme(
      legend.position = "bottom")+
    ggplot2::labs(x="Year",y="Catch",title=my_y_title,fill="",color="",linetype="")+
    ggplot2::scale_y_continuous(limits=c(0,NA),expand = c(0,0)) +
    ggplot2::scale_x_continuous(expand = expansion(mult = c(0, 0.02)))# +
  # theme(text = element_text(size = 10)) 
  
  if (Management==T) {
    pic_catch=ggplot2::ggplot()+
      ggplot2::geom_segment(aes(y=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                yend=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                x = min(temp_data$yr), xend =  max( temp_data$yr) ,linetype="B",col="B"),size=1)+
      ggplot2::geom_ribbon(ggplot2::aes(x=temp_data$yr,ymin=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[2],
                                        ymax=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[3],linetype="B",fill="B"), alpha=0.2)+
      ggplot2::geom_line(data = temp_data,ggplot2::aes(x=yr,y=Catch,color="A",linetype="A"),size=0.7)+
      #ggplot2::geom_point(data = temp_data,ggplot2::aes(x=yr,y=Catch,color="A"))+
      ggplot2::geom_ribbon(data = temp_data,ggplot2::aes(x=yr,ymin=Catch_low, ymax=Catch_high,fill="A"), alpha=0.2)+#y=ct.jags_low,
      ggplot2::theme_classic()+
      ggplot2::scale_linetype_manual(labels=c("Catch","MSY"),values=c("solid", "dashed"))+
      ggplot2::scale_color_manual(labels=c("Catch","MSY"),values=c("black","darkgray"))+
      ggplot2::scale_fill_manual(labels=c("Catch","MSY"),values=c("black","darkgray"))+
      ggplot2::theme_classic()+
      # ggplot2::scale_linetype_manual(labels=c("MSY"),values=c("dashed"))+
      # ggplot2::scale_color_manual(labels=c("Catch"),values=c(clr))+
      # ggplot2::scale_fill_manual(labels=c("Catch"),values=c(clr))+
      ggplot2::labs(x="Year",y="Catch",title=my_y_title,fill="",color="",linetype="")+
      ggplot2::scale_y_continuous(limits=c(0,NA),expand = c(0,0)) +
      ggplot2::scale_x_continuous(expand = expansion(mult = c(0, 0.02)))# +
    # theme(text = element_text(size = 10)) 
    
  }
  return(pic_catch)
}



ggbk.plot= function(CLA.obj,METHOD,Management=F) {
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  
  if (Management==F) {
    my_y_title <-bquote(atop(Stock~size~"for"~bold(.(CLA.obj[["input"]][["Stock_info"]]$Stock))))
  } else {
    my_y_title="Stock size"
  }
  
  temp_data=as.data.frame(CLA.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  Bmsy= expression(B[MSY])
  Blim= expression(B[lim])
  
  pic_bk=ggplot2::ggplot()+
    ggplot2::geom_line(data=temp_data,ggplot2::aes(x=yr,y=bk,color="blue"),size=0.7)+
    ggplot2::geom_ribbon(data=temp_data,ggplot2::aes(x=yr,ymin=bk_low, ymax=bk_high,fill="blue"),alpha=0.2)+
    ggplot2::theme_classic()+
    ggplot2::geom_hline( aes(yintercept=0.5,linetype="A"),color="black",size=0.7)+
    ggplot2::geom_hline( aes(yintercept=0.25,linetype="B"),color="blue",size=0.7)+
    ggplot2::geom_segment(ggplot2::aes(x=min(as.numeric(temp_data$yr)),
                                       y=as.numeric(CLA.obj[["input"]][["Input_parameters"]][["stb.low"]]),
                                       xend=min(as.numeric(temp_data$yr)),
                                       yend=as.numeric(CLA.obj[["input"]][["Input_parameters"]][["stb.hi"]]),
                                       linetype="C"),col="purple",size=1)+
    ggplot2::scale_color_manual(labels=c("Biomass and 95% CI"),values=c("blue"))+
    ggplot2::scale_fill_manual(labels=c("Biomass and 95% CI"),values=c("blue"))+
    ggplot2::scale_linetype_manual(labels=c(Bmsy, Blim,"B/k priors"),values=c(2,3,1),
                                   guide = guide_legend(override.aes = list(color = c("black", "blue","purple"),
                                                                            size = c(0.7, 0.7,1))))+
    ggplot2::labs(y="Relative biomass B/k", x="Year",title=my_y_title,fill="",color="",shape="",linetype="")+ggplot2::theme(
      legend.position = "bottom")+
    ggplot2::scale_y_continuous(limits=c(0,NA),expand = c(0,0)) +
    ggplot2::scale_x_continuous(expand = expansion(mult = c(0.02, 0.02)))# +
  #  theme(text = element_text(size = 10)) 
  
  if (CLA.obj[["input"]][["Input_parameters"]][["nbk"]]==2) {
    
    pic_bk=pic_bk+ggplot2::geom_segment(ggplot2::aes(x=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$int.yr),
                                                     y=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low), 
                                                     xend=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$int.yr),
                                                     yend=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi),
                                                     linetype="C"),col="purple",size=1)
  } else if (CLA.obj[["input"]][["Input_parameters"]][["nbk"]]==3) {
    pic_bk=pic_bk+ggplot2::geom_segment(ggplot2::aes(x=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$int.yr),
                                                     y=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low), 
                                                     xend=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$int.yr),
                                                     yend=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi),
                                                     linetype="C"),col="purple",size=1)+
      ggplot2::geom_segment(ggplot2::aes(x=max(as.numeric(temp_data$yr)),
                                         y=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.low), 
                                         xend=max(as.numeric(temp_data$yr)),
                                         yend=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.hi),
                                         linetype="C"),col="purple",size=1)
  }
  
  if (Management==T) {
    pic_bk=ggplot2::ggplot()+
      ggplot2::geom_line(data=temp_data,ggplot2::aes(x=yr,y=bk,color="blue"),size=0.7)+
      ggplot2::geom_ribbon(data=temp_data,ggplot2::aes(x=yr,ymin=bk_low, ymax=bk_high,fill="blue"),alpha=0.2)+
      ggplot2::theme_classic()+
      ggplot2::geom_hline( aes(yintercept=0.5,linetype="A"),color="black",size=0.7)+
      ggplot2::geom_hline( aes(yintercept=0.25,linetype="B"),color="blue",size=0.7)+
      ggplot2::scale_color_manual(labels=c("Biomass"),values=c("blue"))+
      ggplot2::scale_fill_manual(labels=c("Biomass"),values=c("blue"))+
      ggplot2::scale_linetype_manual(labels=c(Bmsy, Blim),values=c(2,3),
                                     guide = guide_legend(override.aes = list(color = c("black", "blue"),size = c(0.7, 0.7))) )+
      ggplot2::labs(y="Relative biomass B/k", x="Year",title=my_y_title,fill="",color="",shape="",linetype="")+
      ggplot2::scale_y_continuous(limits=c(0,NA),expand = c(0,0)) +
      ggplot2::scale_x_continuous(expand = expansion(mult = c(0, 0.02)))# +
    #  theme(text = element_text(size = 10)) 
  }
  return(pic_bk)
}



ggFFmsy.plot= function(CLA.obj,METHOD,Management=F) {
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  if (Management==F) {
    my_y_title <-bquote(atop(Exploitation~"for"~bold(.(CLA.obj[["input"]][["Stock_info"]]$Stock))))
  } else {
    my_y_title="Exploitation"
  }
  
  temp_data=as.data.frame(CLA.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  ly= expression(F/F[MSY])
  A_= expression('' ~ F/F[MSY] ~ "and"~"95%"~ "CI")
  
  pic_FFmsy=ggplot2::ggplot()+
    ggplot2::geom_line(data=temp_data,ggplot2::aes(x=yr,y=FFmsy,color=clr),size=0.7)+
    ggplot2::geom_ribbon(data=temp_data,ggplot2::aes(x=yr,ymin=FFmsy_low, ymax=FFmsy_high,fill=clr),alpha=0.2)+
    ggplot2::theme_classic()+
    ggplot2::geom_hline(yintercept=1, linetype="dashed")+
    ggplot2::scale_color_manual(labels=c(A_),values=c(clr))+
    ggplot2::scale_fill_manual(labels=c(A_),values=c(clr))+
    ggplot2::labs(y=ly, x="Year",title=my_y_title,fill="",color="",shape="",linetype="")+
    ggplot2::theme(legend.position = "bottom")+
    ggplot2::scale_y_continuous(limits=c(0,NA),expand = c(0,0)) +
    ggplot2::scale_x_continuous(expand = expansion(mult = c(0, 0.02)))# +
  # theme(text = element_text(size = 10)) 
  
  if (Management==T) {
    A_= expression(F/F[MSY])
    
    pic_FFmsy=ggplot2::ggplot()+
      ggplot2::geom_line(data=temp_data,ggplot2::aes(x=yr,y=FFmsy,color=clr),size=0.7)+
      ggplot2::geom_ribbon(data=temp_data,ggplot2::aes(x=yr,ymin=FFmsy_low, ymax=FFmsy_high,fill=clr),alpha=0.2)+
      ggplot2::theme_classic()+
      ggplot2::geom_hline(yintercept=1, linetype="dashed")+
      ggplot2::scale_color_manual(labels=c(A_),values=c(clr))+
      ggplot2::scale_fill_manual(labels=c(A_),values=c(clr))+
      ggplot2::labs(y=ly, x="Year",title=my_y_title,fill="",color="",shape="",linetype="")+
      ggplot2::scale_y_continuous(limits=c(0,NA),expand = c(0,0)) +
      ggplot2::scale_x_continuous(expand = expansion(mult = c(0, 0.02)))# +
    # theme(text = element_text(size = 10)) 
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
    ggplot2::scale_y_continuous(limits=c(0,NA),expand = c(0,0))# +
   # ggplot2::scale_x_continuous(expand = expansion(mult = c(0, 0.02)))# +
  # theme(text = element_text(size = 10)) 
  
  return(pic_parabola)
} 


ggkobe.plot= function(CLA.obj,fit.obj,METHOD,Management=F) {
  
  int.yr=CLA.obj[["input"]][["Input_parameters"]][["int.yr"]]
  temp_data=as.data.frame(CLA.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  
  nint.yr=match(int.yr,temp_data$yr)
  # if (all(is.na(BSM_2_rsq_products_a()))) {
  y.F_Fmsy = fit.obj$FFmsy[,nyr]
  x.b_bmsy = fit.obj$BBmsy[,nyr]
  
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
    my_y_title <-bquote(atop(Kobe~plot~"for"~bold(.(CLA.obj[["input"]][["Stock_info"]]$Stock))))
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
                                labels  =c(as.character(CLA.obj[["input"]][["Input_parameters"]][["StartYear"]]),
                                           as.character(CLA.obj[["input"]][["Input_parameters"]][["int.yr"]]),
                                           as.character(CLA.obj[["input"]][["Input_parameters"]][["EndYear"]])),
                                values=c(22,21,24))+
    ggplot2::scale_fill_manual(name="",
                               labels  =c("50% C.I.","80% C.I.","95% C.I.",Pr.green,Pr.orange,Pr.red,Pr.yellow),
                               values=c(
                                 "slategray3", "slategray2","slategray1", "green","orange","red","yellow"))#+
   # theme(text = element_text(size = 10)) 
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
                                  labels  =c(as.character(CLA.obj[["input"]][["Input_parameters"]][["StartYear"]]),
                                             as.character(CLA.obj[["input"]][["Input_parameters"]][["int.yr"]]),
                                             as.character(CLA.obj[["input"]][["Input_parameters"]][["EndYear"]])),
                                  values=c(22,21,24))+
      ggplot2::scale_fill_manual(name="",
                                 labels  =c("50% C.I.","80% C.I.","95% C.I."),
                                 values=c("slategray3", "slategray2","slategray1"))#+
   #   theme(text = element_text(size = 10)) #+ggplot2::theme(
    #   legend.position = "bottom")
  }
  return(picKobe)
}

ggmanagement.plot= function(CLA.obj,fit_obj,METHOD) {
  pic_1manag=ggcatch.plot(CLA.obj,METHOD,Management=T)
  pic_2manag=ggbk.plot(CLA.obj,METHOD,Management=T)
  pic_3manag=ggFFmsy.plot(CLA.obj,METHOD,Management=T)
  pic_4manag=ggkobe.plot(CLA.obj,fit_obj,METHOD,Management=T)
  temp_object=ggpubr::ggarrange(pic_1manag,pic_2manag,pic_3manag,pic_4manag,
                                labels=c("A","B","C","D"),
                                ncol = 2,nrow = 2)
  return(temp_object)
}




ggprorposterior.plot= function(CLA.obj,fit.obj,METHOD) {
  mean.log.r=mean(log(c(as.numeric(CLA.obj[["input"]][["Input_parameters"]][["r.low"]]),as.numeric(CLA.obj[["input"]][["Input_parameters"]][["r.hi"]]))))
  sd.log.r=(log(as.numeric(CLA.obj[["input"]][["Input_parameters"]][["r.hi"]]))-log(as.numeric(CLA.obj[["input"]][["Input_parameters"]][["r.low"]])))/(2*1.96)  # assume range covers 4 SD
  mean.log.k= CLA.obj$rk_object[["mean.log.k"]]
  sd.log.k= CLA.obj$rk_object[["sd.log.k"]]
  
  nbk=CLA.obj$input$Input_parameters$nbk
  
  rk <- exp(mvn(n=5000,mean.log.r=mean.log.r,sd.log.r=sd.log.r,
                mean.log.k=mean.log.k,sd.log.k=sd.log.k))
  pp.lab = "r"
  rpr = sort(rk[,1])
  post = fit.obj[["r"]]
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
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),axis.text.y = ggplot2::element_blank())+
    theme(text = element_text(size = 10)) #+
  
  rpr = sort(rk[,2])
  post = fit.obj[["k"]]
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
  post =  as.numeric(fit.obj[["r"]])* as.numeric(fit.obj[["k"]])/4
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
  int.yr= CLA.obj[["input"]][["Input_parameters"]][["int.yr"]]
  StartYear=CLA.obj[["input"]][["Input_parameters"]]$StartYear
  EndYear= CLA.obj[["input"]][["Input_parameters"]]$EndYear
  
  if (is.na(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low))) {
    intb.low=0.4 } else {
      intb.low=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low)
    }
  if (is.na(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi))) {
    intb.hi=0.5 } else {
      intb.hi=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi)
    }
  
  if (is.na(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.low))) {
    endb.low=0.4 } else {
      endb.low=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.low)
    }
  if (is.na(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.hi))) {
    endb.hi=0.5 } else {
      endb.hi=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.hi)
    }
  
  
  b.prior = rbind(matrix(c(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi),
                           intb.low,intb.hi,
                           endb.low,endb.hi),2,3),rep(0,3)) # last row includes the 0 penalty
  
  bk.beta = (beta.prior(b.prior))
  all.bk.CMSY  = fit.obj$P
  startbio=c(CLA.obj[["input"]][["Input_parameters"]]$stb.low,CLA.obj[["input"]][["Input_parameters"]]$stb.hi)
  
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
  if (nbk>1) {
    intbio=c(CLA.obj[["input"]][["Input_parameters"]]$intb.low,CLA.obj[["input"]][["Input_parameters"]]$intb.hi)
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
  }
  
  if (nbk>2) {
    
    # bk3
    endbio=c(CLA.obj[["input"]][["Input_parameters"]]$endb.low,CLA.obj[["input"]][["Input_parameters"]]$endb.hi)
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
  }
  if (nbk==3) {
    temp_F=ggpubr::ggarrange(pic_1,pic_2,pic_3,
                             pic_4,pic_5,pic_6,
                             labels=c("A","B","C","D","E","F"),
                             ncol = 3,nrow = 2)
  } else if (nbk==2) {
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

ggpdiagnostics.plot= function(CLA.obj,fit_obj,METHOD) {
  if (METHOD=="CMSY") {
    #  clr="blue"
    #my_y_title <-bquote(atop(Catch~"for"~the~stock~bold(.(CLA.obj[["input"]][["Stock_info"]]$Stock))~of~italic(.(CLA.obj[["input"]][["Stock_info"]]$ScientificName))))
    temp_data=as.data.frame(CLA.obj[["output"]][["output_timeseries"]])
    temp_data$yr=as.integer(row.names(temp_data))
    nyr=length(temp_data$yr)
    
    temp_data_in=as.data.frame(CLA.obj[["input"]][["Input_data"]])
    # temp_data_in$yr=as.integer(row.names(temp_data_in))
    
    pic_1=ggplot2::ggplot()+
      ggplot2::geom_segment(aes(y=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                yend=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
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
    
    #my_y_title <-bquote(atop(Catch~"for"~the~stock~bold(.(CLA.obj[["input"]][["Stock_info"]]$Stock))~of~italic(.(CLA.obj[["input"]][["Stock_info"]]$ScientificName))))
    temp_data=as.data.frame(CLA.obj[["output"]][["output_timeseries"]])
    temp_data$yr=as.integer(row.names(temp_data))
    nyr=length(temp_data$yr)
    
    temp_data_in=as.data.frame(CLA.obj[["input"]][["Input_data"]])
    # temp_data_in$yr=as.integer(row.names(temp_data_in))
    
    pic_1=ggplot2::ggplot()+
      ggplot2::geom_segment(aes(y=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                                yend=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
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
      ggplot2::scale_y_continuous(limits = c(floor_dec(min(temp_data_pred.pe$lcl.pred.pe),1),ceiling_dec(max(temp_data_pred.pe$ucl.pred.pe),1)))+
      ggplot2::theme_classic()+
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

retro.fit= function(CLA.obj,ret_steps,METHOD) {
  q_prrs=c(CLA.obj$input$Input_parameters$q_low, CLA.obj$input$Input_parameters$q_high)
  init.q_=mean(c(CLA.obj$input$Input_parameters$q_low, CLA.obj$input$Input_parameters$q_high))
  retro_years=vector()
  retro.obj_data=list()
  for (i in 1:ret_steps) {
    retro_years[i]=CLA.obj[["input"]][["Input_parameters"]][["EndYear"]]-i
    retro.obj_data[[i]]=CLA.obj[["input"]][["Input_data"]][CLA.obj[["input"]][["Input_data"]]$yr<= retro_years[i],]
  }
  ####nbk
  # if (CLA.obj[["input"]][["Input_parameters"]]$nbk=="Only start")  {
  #   nbk=1 } else if (CLA.obj[["input"]][["Input_parameters"]]$nbk=="Start & intermediate") {
  #     nbk=2  } else {
  #       nbk=3
  #     }
  if (CLA.obj[["input"]][["Input_parameters"]]$nbk==1)  {
    nbk=1 } else if (CLA.obj[["input"]][["Input_parameters"]]$nbk==2) {
      nbk=2  } else {
        nbk=3
      }
  
  
  
  xx=data.frame(year=c(CLA.obj[["input"]][["Input_parameters"]][["StartYear"]],
                       CLA.obj[["input"]][["Input_parameters"]][["int.yr"]],
                       CLA.obj[["input"]][["Input_parameters"]][["EndYear"]]),
                low=c(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),
                      as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low),
                      as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.low)),
                high=c(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi),
                       as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi),
                       as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.hi))
  )
  xy=data.frame(year=seq(CLA.obj[["input"]][["Input_parameters"]][["StartYear"]],CLA.obj[["input"]][["Input_parameters"]][["EndYear"]],1))
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
  
  as.numeric(xy_$low[xy_$year==(CLA.obj[["input"]][["Input_parameters"]]$int.yr-1)])
  
  nbks_=list()
  for (i in 1:ret_steps) {
    if (nbk==3 & max(retro.obj_data[[i]]$yr)>CLA.obj[["input"]][["Input_parameters"]]$int.yr ) {
      nbks_[[i]]=3
      b.prior_[[i]] = rbind(matrix(c(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi),
                                     as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low),as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)])),2,3),rep(0,3)) # last row includes the 0 penalty
      
    } else if (max(retro.obj_data[[i]]$yr)<=CLA.obj[["input"]][["Input_parameters"]]$int.yr ) {
      nbks_[[i]]=nbk
      b.prior_[[i]] = rbind(matrix(c(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)]),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)])),2,3),rep(0,3)) # last row includes the 0 penalty
    } else {
      nbks_[[i]]=nbk
      b.prior_[[i]] = rbind(matrix(c(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)]),
                                     as.numeric(xy_$low[xy_$year== max(retro.obj_data[[i]]$yr)]),as.numeric(xy_$high[xy_$year== max(retro.obj_data[[i]]$yr)])),2,3),rep(0,3)) # last row includes the 0 penalty
      
    }
  }
  
  pen.bk = pen.F = rep(0,length(CLA.obj[["input"]][["Input_data"]]$ct_smthd))
  
  b.yrs_=list()
  for (i in 1:ret_steps) {
    
    if (nbks_[[i]]==3) {
      
      b.yrs_[[i]] = c(1,length(as.integer(CLA.obj[["input"]][["Input_parameters"]]$StartYear):
                                 as.integer(CLA.obj[["input"]][["Input_parameters"]]$int.yr)),
                      length(as.integer(CLA.obj[["input"]][["Input_parameters"]]$StartYear):
                               as.integer(max(retro.obj_data[[i]]$yr))))
    } else {
      
      b.yrs_[[i]] = c(1,length(as.integer(CLA.obj[["input"]][["Input_parameters"]]$StartYear):
                                 as.integer(max(retro.obj_data[[i]]$yr))),
                      length(as.integer(CLA.obj[["input"]][["Input_parameters"]]$StartYear):
                               as.integer(max(retro.obj_data[[i]]$yr))))
      
    }}
  
  btj_=list()
  for (i in 1:ret_steps) {
    if (METHOD == "CMSY") { 
      bt.start <- mean(c(CLA.obj$rk_object[["rkpriors"]]$prior.k.low*
                           as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),
                         CLA.obj$rk_object[["rkpriors"]]$prior.k.hi*
                           as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi))) # derive proxy for first bt value
      
      btj_[[i]]<- c(bt.start,rep(NA,length(retro.obj_data[[i]]$ct_smthd)-1)) }else if (METHOD == "BSM") { 
        btj_[[i]]=retro.obj_data[[i]]$bt_smthd
      }
  }
  
  CV.cpue_=list()
  for (i in 1:ret_steps) {
    
    if (METHOD == "CMSY") { 
      CV.cpue_[[i]]=rep(0.2,length(retro.obj_data[[i]]$ct_smthd))  }else if (METHOD == "BSM") { 
        
        if (all(is.na(retro.obj_data[[i]]$bt_cv)) &  !is.na(CLA.obj[["input"]][["Input_parameters"]]$CV_cpue)) {
          CV.cpue_[[i]]      <- rep(CLA.obj[["input"]][["Input_parameters"]]$CV_cpue,length(retro.obj_data[[i]]$ct_smthd))
        } else if  (all(is.na(CLA.obj[["input"]][["Input_data"]]$bt_cv))) {
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
                                   prior.r=as.numeric(c(CLA.obj[["input"]][["Input_parameters"]]$r.low,CLA.obj[["input"]][["Input_parameters"]]$r.hi)),
                                   prior.k=as.numeric(c(CLA.obj$rk_object[["rkpriors"]]$prior.k.low, CLA.obj$rk_object[["rkpriors"]]$prior.k.hi)),
                                   startbio=as.numeric(c(CLA.obj[["input"]][["Input_parameters"]]$stb.low,CLA.obj[["input"]][["Input_parameters"]]$stb.hi)),
                                   q.priorj=as.numeric(q_prrs),#c(0.99,1.01),# # since no abundance data are available in this run,
                                   init.q=init.q_ ,           # q could be omitted and is set here to (practically) 1  ,
                                   init.r=CLA.obj$rk_object[["rkpriors"]]$init.r,
                                   init.k=CLA.obj$rk_object[["rkpriors"]]$init.k,
                                   pen.bk=pen.bk,
                                   pen.F=pen.F,
                                   b.yrs=b.yrs_[[j]],
                                   b.prior= b.prior_[[j]],
                                   CV.C=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$CV_catch),# 0.15,  #><>MSY: Add Catch CV
                                   CV.cpue=CV.cpue_[[j]],#input$CPUE_CV,  #><>MSY: Add minimum realistic cpue CV
                                   nbk=nbks_[[j]],
                                   rk.cor=CLA.obj$rk_object[["rkpriors"]]$rk.cor,
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
    
    retro_outcomes[[i]]=CLA.obj[["output"]][["output_timeseries"]]
    retro_outcomes[[i]]=retro_outcomes[[i]][1:(nrow(retro_outcomes[[i]])-i),]
    retro_outcomes[[i]][, c("Catch","Catch_low","Catch_high")]=t(apply( retro_jags_outputs[[i]]$ct.jags,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("FFmsy","FFmsy_low","FFmsy_high")]=t(apply( retro_jags_outputs[[i]]$FFmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("f","f_low","f_high")]=t(apply( retro_jags_outputs[[i]]$F,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("BBmsy","BBmsy_low","BBmsy_high")]=t(apply( retro_jags_outputs[[i]]$BBmsy,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("B","B_low","B_high")]= t(apply( retro_jags_outputs[[i]]$B,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]][, c("bk","bk_low","bk_high")]=  t(apply( retro_jags_outputs[[i]]$P,2,quantile,c(0.5,0.025,0.975),na.rm=T))
    retro_outcomes[[i]]$ID=max(as.integer(row.names(retro_outcomes[[i]])))
  }
  CLA.obj[["retro_output"]]=retro_outcomes
  
  return(CLA.obj)
}


ggretro.plot=function(retro.object) {
  retros=retro.object[["retro_output"]]
  for (i in 1:length(retros)) {
    retros[[i]]$yr=as.integer(row.names(retros[[i]]))
  }
  retros=rbindlist(retros)
  
  ft_mod= retro.object[["output"]][["output_timeseries"]]
  ft_mod$ID=max(as.integer(rownames(ft_mod)))
  ft_mod$yr=as.integer(row.names(ft_mod))
  retros=rbind(retros,ft_mod)
  max.y_ffmsy <- max(c(1.2,1.1*retros$FFmsy),na.rm=T)
  max.y_bbmsy <- max(c(1.2, 1.1*retros$BBmsy ),na.rm=T)
  
  my_y_title1 <-bquote(atop("F/Fmsy Retrospective"~"for"~the~stock~bold(.(retro.object[["input"]][["Stock_info"]]$Stock))~of~italic(.(retro.object[["input"]][["Stock_info"]]$ScientificName))))
  my_y_title2 <-bquote(atop("B/Bmsy Retrospective"~"for"~the~stock~bold(.(retro.object[["input"]][["Stock_info"]]$Stock))~of~italic(.(retro.object[["input"]][["Stock_info"]]$ScientificName))))
  
  picffmsy=  ggplot2::ggplot()+
    ggplot2::geom_line(data=retros, ggplot2::aes(x=yr,y=FFmsy,col=as.factor(ID)),size=1)+
    #  ggplot2::geom_point(data=retros, ggplot2::aes(x=yr,y=FFmsy,col=as.factor(ID),shape=as.factor(ID)),size=2)+
    ggplot2::scale_y_continuous(limits=c(0,max.y_ffmsy))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=1),linetype="dashed",size=1)+
    ggplot2::theme_classic()+
    ggplot2::labs(y="F/Fmsy",x="Year",fill="",color="",linetype="",shape="",title=my_y_title1)+
    ggplot2::theme(legend.position = "bottom")
  
  picbbmsy=  ggplot2::ggplot()+
    ggplot2::geom_line(data=retros, ggplot2::aes(x=yr,y=BBmsy,col=as.factor(ID)),size=1)+
    #   ggplot2::geom_point(data=retros, ggplot2::aes(x=yr,y=BBmsy,col=as.factor(ID),shape=as.factor(ID)),size=2)+
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


CLA.forward=function(fit.obj,CLA.obj,nyears=5,status.quo_years=1,interim.quant = c("Catch", "F")[2],quant = c("Catch", "F")[2],Manual_Scenarios=NULL){
  year = seq(CLA.obj[["input"]][["Input_parameters"]][["StartYear"]],
             CLA.obj[["input"]][["Input_parameters"]][["EndYear"]],1)
  initial=NULL
  ######## Extract r and k 
  rk=data.frame(K=fit.obj[["k"]],r=fit.obj[["r"]])
  ######## Extract B and k 
  B=as.data.frame(fit.obj[["B"]])
  colnames(B)=year
  B_=gather(B,key=year,value = B)
  data.frame(H=c(fit.obj[["F"]]),Catch=c(fit.obj[["ct.jags"]]),P=c(fit.obj[["P"]]))
  trj=cbind(B_,data.frame(H=c(fit.obj[["F"]]),Catch=c(fit.obj[["ct.jags"]]),P=c(fit.obj[["P"]]),FFmsy=c(fit.obj[["FFmsy"]])))
  
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
  # Y = CLA.obj[["output"]][["output_posteriors"]][["r_post"]][1] * tail(CLA.obj[["output"]][["output_timeseries"]][["B"]],1) * (1 - tail(CLA.obj[["output"]][["output_timeseries"]][["B"]],1) /CLA.obj[["output"]][["output_posteriors"]][["k_post"]][1])
  ##### status quo
  year = seq(CLA.obj[["input"]][["Input_parameters"]][["StartYear"]],CLA.obj[["input"]][["Input_parameters"]][["EndYear"]],1)
  yrend = max(year)
  n = length(year)
  
  if (quant == "Catch") {
    status.quo = mean(CLA.obj[["input"]][["Input_data"]][["ct"]][(n - (status.quo_years-1)):n])
    if (is.null(initial)) {
      initial = status.quo
    }
  }
  if (quant == "F") {
    status.quo =   mean(CLA.obj[["output"]][["output_timeseries"]][["f"]][(n - (status.quo_years-1)):n])
    if (is.null(initial)) {
      initial = status.quo
    }
  }
  if (interim.quant == "Catch") {
    #  interim_var = mean(CLA.obj[["input"]][["Input_data"]][["ct"]][(n - (status.quo_years-1)):n])
    interim_var = mean(CLA.obj[["output"]][["output_timeseries"]][["Catch"]][(n - (status.quo_years-1)):n])
  }
  if (interim.quant == "F") {
    interim_var =   mean(CLA.obj[["output"]][["output_timeseries"]][["f"]][(n - (status.quo_years-1)):n])
  }
  
  fmsy=ifelse(unique(CLA.obj[["output"]][["output_posteriors"]][["Fmsy_post_correction_note"]])=="Fmsy was corrected downward to account for reduced recruitment.",
              CLA.obj[["output"]][["output_posteriors"]][["Fmsy_post_corrected"]][1],
              CLA.obj[["output"]][["output_posteriors"]][["Fmsy_post"]][1])
  
  #r_=CLA.obj[["output"]][["output_posteriors"]][["r_post"]][1]
  #bmsy=CLA.obj[["output"]][["output_posteriors"]][["Bmsy_post"]][1]
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
  
  fit_=CLA.obj[["output"]][["output_timeseries"]]
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
  forward_df=rbind(fit_,fin_df_2)
  forward_df$year=as.integer(forward_df$year)
  params=data.frame(nyears=nyears,status.quo_years=status.quo_years,interim.quant=interim.quant,quant=quant)
  f_list=list(params=params,forward_df=forward_df)
  return(f_list)
}


ggforecast.FFMSY= function(cla.fwd,CI=F) {
  to_sim=cla.fwd[["forward_df"]]
  interim_year=to_sim$year[to_sim$Scenario=="fit"]
  interim_year=max(as.integer(interim_year))+1
  if (CI==F) {
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
}


ggforecast.bk=function(CLA.obj,CLA.fwd,CI=F) {
  BMSY=CLA.obj[["output"]][["output_posteriors"]][["Bmsy_post"]][1]
  BMSY_low=CLA.obj[["output"]][["output_posteriors"]][["Bmsy_post"]][2]
  BMSY_high=CLA.obj[["output"]][["output_posteriors"]][["Bmsy_post"]][3]
  to_sim=CLA.fwd[["forward_df"]]
  interim_year=to_sim$year[to_sim$Scenario=="fit"]
  interim_year=max(as.integer(interim_year))+1
  to_sim$BMSY_low=BMSY_low
  to_sim$BMSY_high=BMSY_high
  if (CI==F) {
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
}


ggforecast.catch=function(cla.proj,CI=F) {
  to_sim=cla.proj[["forward_df"]]
  interim_year=to_sim$year[to_sim$Scenario=="fit"]
  interim_year=max(as.integer(interim_year))+1
  
  if (CI==F) {
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
}

CLA_stock_maker_empty=function() {
  inp_clnms=c("Stock", "yr","ct","bt","ct_smthd","bt_iterp",
              "bt_smthd","ecreep","bt_cv","Stock_objID")
  inp = data.frame(matrix(nrow = 1, ncol = length(inp_clnms)))
  colnames(inp)=inp_clnms
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
  output_timeseries_ = data.frame(matrix(nrow =length(inp$yr), ncol = length(timeseries_clmns))) #nrow(fstck_Catch_Data)
  colnames(output_timeseries_)=timeseries_clmns
  # row.names(output_timeseries_)=inp$yr
  CLA_object=list(input=list(Stock_info=input_info,Input_parameters=input_param,Input_data=inp),
                  output=list(output_posteriors=output_posteriors_,output_timeseries=output_timeseries_),
                  rk_object=NA)
  return(CLA_object)
}                          

CLA_stock_maker_empty_shiny=function(inpt,Stock,yrcol,ctcol,btcol,cvcol=NA,do_u_have_cv="No") {
  inp_clnms=c("Stock", "yr","ct","bt","ct_smthd","bt_iterp",
              "bt_smthd","ecreep","bt_cv","Stock_objID")
  inp = data.frame(matrix(nrow = nrow(inpt), ncol = length(inp_clnms)))
  colnames(inp)=inp_clnms
  inp$Stock=Stock
  inp$yr=as.integer(inpt[,yrcol])
  inp$ct=as.numeric(inpt[,ctcol])
  inp$bt= as.numeric(inpt[,btcol])
  if (do_u_have_cv=="Yes"){
    inp$bt_cv=  as.numeric(inpt[,cvcol]) } 
  stock_info_clnms =c("Stock","ScientificName","Name","Group","Continent","Region","Subregion","Comments","Source")
  inputs_clnms =c("StartYear","EndYear","Smooth_K_catch","CV_catch","StartYear_bt","EndYear_bt","Ecreep_cpue",
                  "Ecreep_Year","Ecreep_value","Smooth_cpue","Smooth_K_cpue","CV_cpue","Resilience","r.low","r.hi","FBSLB_info_r","FBSLB_info_Resilience","FBSLB_page","MSY_prior","q_low","q_high",
                  "ANN_stb.low","ANN_stb.hi","ANN_int.yr", "ANN_intb.low","ANN_intb.hi","ANN_endb.low","ANN_endb.hi","nbk", "stb.low","stb.hi",
                  "int.yr", "intb.low","intb.hi","endb.low","endb.hi","btype")
  
  data_clnms =c("Stock","yr","ct","bt","ct_smthd","bt_iterp","bt_smthd","ecreep","bt_cv", "Stock_objID")
  
  input_info = data.frame(matrix(nrow = 1, ncol = length(stock_info_clnms)))
  colnames(input_info)=stock_info_clnms
  input_info$Stock=Stock
  
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
  output_timeseries_ = data.frame(matrix(nrow =length(inp$yr), ncol = length(timeseries_clmns))) #nrow(fstck_Catch_Data)
  colnames(output_timeseries_)=timeseries_clmns
  # row.names(output_timeseries_)=inp$yr
  
  CLA_object=list(input=list(Stock_info=input_info,Input_parameters=input_param,Input_data=inp),
                  output=list(output_posteriors=output_posteriors_,output_timeseries=output_timeseries_),
                  rk_object=NA)
  return(CLA_object)
}   


interim.summary=function(cla.proj) {
  
  Interim_year=max(cla.proj[["forward_df"]]$year[cla.proj[["forward_df"]]$Scenario=="fit"],na.rm = T)+1
  
  if ( cla.proj[["params"]][["quant"]]=="Catch") {
    if (cla.proj[["params"]][["status.quo_years"]]==1) {
      
      txt=  paste("Catch for ",Interim_year, "was equal to ", (Interim_year-1), "Catch." ) } else {
        txt=     paste("Catch for  ",Interim_year, "was equal to the mean of  ", (Interim_year-cla.proj[["params"]][["status.quo_years"]]), "-",(Interim_year-1), "Catches." )
      }} else   {
        if (cla.proj[["params"]][["status.quo_years"]]==1) {
          txt=    paste("Fishing mortality for ",Interim_year, "was equal to ", (Interim_year-1), "Fishing mortality." ) } else {
            txt=     paste("Fishing mortality for ",Interim_year, "was equal to the mean of  ", (Interim_year-cla.proj[["params"]][["status.quo_years"]]), "-",(Interim_year-1), "Fishing mortalities." )
          }} 
  cat(txt)
  cat("\n")
  
  Interim_sum=cla.proj[["forward_df"]][,c("year","Scenario","Catch","Catch_low","Catch_high", "FFmsy","FFmsy_low","FFmsy_high","bk","bk_low","bk_high"  , "B","B_low","B_high")]
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
  
  if (cla.proj[["params"]][["quant"]]!="Catch") {
    xx=data.table(Var=c("F/Fmsy","Catch","B/k","Biomass"),
                  Value=as.character(c(Interim_sum$FFmsy,Interim_sum$Catch,Interim_sum$bk, Interim_sum$B )),
                  Lower_limit=c("",Interim_sum$Catch_low,Interim_sum$bk_low,Interim_sum$B_low),
                  Upper_limit=c("",Interim_sum$Catch_high,Interim_sum$bk_high,Interim_sum$B_high))
    colnames(xx)=c("","Value","Lower limit","Upper limit")
  }
  if (cla.proj[["params"]][["quant"]]=="Catch") {
    xx=data.table(Var=c("F/Fmsy","Catch","B/k","Biomass"),
                  Value=as.character(c(Interim_sum$FFmsy,Interim_sum$Catch,Interim_sum$bk, Interim_sum$B )),
                  Lower_limit=c(Interim_sum$FFmsy_low,"",Interim_sum$bk_low,Interim_sum$B_low),
                  Upper_limit=c(Interim_sum$FFmsy_high,"",Interim_sum$bk_high,Interim_sum$B_high))
    colnames(xx)=c("","Value","Lower limit","Upper limit")
  }
  xx=as.data.frame(xx)
  # row.names(xx)=xx[,1]
  # xx=xx[,2:4]
  print(xx,row.names = FALSE)
  sum.list=list(info=txt,proj.summary=xx)
  return(sum.list)
  
}


advice.summary=function(cla.proj) {
  Interim_sum=cla.proj[["forward_df"]][,c("year","Scenario","Catch","Catch_low","Catch_high",  "f","B","B_low","B_high")]
  
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
  f_table=as.data.frame(f_table)
  colnames(f_table)[6]= paste(min(as.integer(Interim_sum$year)),"Catch change [%]")
  colnames(f_table)[7]= paste(max(as.integer(Interim_sum$year)),"Biomass change [%]")
  
  print(f_table,row.names = F)
  return(f_table)
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
    ggplot2::scale_y_continuous(trans='log',limits=c( 0.9*as.numeric( ABC.obj[["output"]][["output_posteriors"]]$k_post[2]), as.numeric( 1.1*ABC.obj[["output"]][["output_posteriors"]]$k_post[3])),labels = function(x) round(as.numeric(x))) +
    ggplot2::theme_classic()+
    ggplot2::labs(y="k (tonnes)", x="r (1/year)",title=my_y_title)+
    ggplot2::geom_point(data=data.frame(rs=fit_obj[["r"]],ks=fit_obj[["k"]]),ggplot2::aes(x=rs,y=ks),color="gray18",size=0.7,alpha=0.2)+
    ggplot2::geom_point(data= ABC.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=r_post[1],y=k_post[1]),color=clr,size=0.7)+
    ggplot2::geom_segment(data= ABC.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=ifelse(r_post[2]>0,r_post[2], as.numeric( ABC.obj[["input"]][["Input_parameters"]]$r.low)),y=k_post[1],xend=r_post[3],yend=k_post[1]),col=clr,size=0.7)+
    ggplot2::geom_segment(data= ABC.obj[["output"]][["output_posteriors"]],ggplot2::aes(x=r_post[1],y=ifelse(k_post[2]>0,k_post[2],0.001),xend=r_post[1],yend=k_post[3]),col=clr,size=0.7)+
    theme(text = element_text(size = 10)) 
  
  return(pick_rk)
  
}

gg_summary.plot= function(ABC.obj,fit_obj,METHOD) {
  
  rk.obj= ABC.obj[["rk_object"]]
  pic_1summary=ggcatch.plot(ABC.obj,METHOD,Management=F)+geom_point(data=ABC.obj[["input"]][["Input_data"]],
                                                                    ggplot2::aes(x=yr,y=ct),shape=21,fill="gray")+
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)))
  pic_2summary=ggrk.plot(ABC.obj,fit_obj,METHOD)+ggtitle("Finding viable r-k")
  #pic_3summary=ggrk_2.plot(ABC.obj,rk.obj,fit_obj,METHOD)+ggtitle("Analysis of viable r-k")
  pic_3summary=  ggkiel.plot(ABC.obj,METHOD)+ggtitle("Biomass and Catch plot")
  pic_4summary=ggbk.plot(ABC.obj,METHOD,Management=F)+ggtitle("Stock size")+geom_point(data=ABC.obj[["input"]][["Input_data"]],
                                                                                       ggplot2::aes(x=yr,y=ABC.obj[["input"]][["Input_data"]][["bt"]]/
                                                                                                      (ABC.obj[["output"]][["output_posteriors"]][["q_post"]][1]*
                                                                                                         ABC.obj[["output"]][["output_posteriors"]][["k_post"]][1])),shape=21,fill="gray")+
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)))
  pic_5summary=ggFFmsy.plot(ABC.obj,METHOD,Management=F)+ggtitle("Exploitation rate")
  pic_6summary=ggparabola.plot(ABC.obj,METHOD)+ggtitle("Equilibrium curve")
  temp_object=ggpubr::ggarrange(pic_1summary,pic_2summary,pic_6summary,pic_4summary,pic_5summary,pic_3summary,
                                labels=c("A","B","C","D","E","F"),
                                ncol = 3,nrow = 2)
  return(temp_object)
}




CLA.summary =function(CLA.obj) {
  
  cat("Model outputs\n")
  cat("\n")
  cat("------------------------------------\n")
  cat("\n")
  cat("Stock summary\n")
  cat("\n")
  
  cat("Species: ",CLA.obj[["input"]][["Stock_info"]]$ScientificName, ", stock:",
      CLA.obj[["input"]][["Stock_info"]]$Stock,"\n")
  cat("Common name: ",CLA.obj[["input"]][["Stock_info"]]$Name,"\n")
  cat("Region: ",CLA.obj[["input"]][["Stock_info"]]$Region, ", ", CLA.obj[["input"]][["Stock_info"]]$Subregion,"\n")
  cat("Catch data used from years: ",CLA.obj[["input"]][["Input_parameters"]]$StartYear,
      "-", CLA.obj[["input"]][["Input_parameters"]]$EndYear,", abundance: ",
      CLA.obj[["input"]][["Input_parameters"]]$btype,"\n")
  cat("Prior initial rel. biomass: ",format(round(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),2),digits = 3), "-", format(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi),digits = 3),"\n")
  if (CLA.obj$input$Input_parameters$nbk!=1) {
    cat("Prior intermediate rel. biomass: ",format(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low),digits = 3),
        "-", format(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi),digits = 3), ", ", 
        CLA.obj[["input"]][["Input_parameters"]]$int.yr,"\n")}
  
  if (CLA.obj$input$Input_parameters$nbk==3) {
    cat("Prior final rel. biomass: ",format(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.low),digits = 3),
        "-", format(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.hi),digits = 3),"\n")}
  
  cat("Prior range for r: ",CLA.obj[["input"]][["Input_parameters"]]$r.low, "-", CLA.obj[["input"]][["Input_parameters"]]$r.hi,"\n")
  cat("Derived prior range for k: ",format(round(CLA.obj$rk_object[["rkpriors"]]$prior.k.low),digits = 3), "-", format(round(CLA.obj$rk_object[["rkpriors"]]$prior.k.hi),digits = 3),"\n")
  cat("Derived prior MSY: ",format(round(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$MSY_prior)),digits = 3),"\n")
  cat("Derived prior range for q: ",format(CLA.obj[["input"]][["Input_parameters"]]$q_low,digits = 3), "-", format(CLA.obj[["input"]][["Input_parameters"]]$q_high,digits = 3),"\n")
  
  cat("------------------------------------\n")
  cat("\n")
  cat("Result of the analysis \n")
  cat("\n")
  
  cat("r= ",format(CLA.obj[["output"]][["output_posteriors"]]$r_post[1],digits = 3),
      ", ","95% CL =",format(CLA.obj[["output"]][["output_posteriors"]]$r_post[2],digits = 3), "-",
      format(CLA.obj[["output"]][["output_posteriors"]]$r_post[3],digits = 3),"\n")
  
  cat("k= ",format(round(CLA.obj[["output"]][["output_posteriors"]]$k_post[1]),digits = 3),
      ", ","95% CL =",format(round(CLA.obj[["output"]][["output_posteriors"]]$k_post[2]),digits = 3), "-",
      format(round(CLA.obj[["output"]][["output_posteriors"]]$k_post[3]),digits = 3),"\n")
  
  cat("MSY= ",format(round(CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1]),digits = 3),
      ", ","95% CL =",format(round(CLA.obj[["output"]][["output_posteriors"]]$MSY_post[2]),digits = 3), "-",
      format(round(CLA.obj[["output"]][["output_posteriors"]]$MSY_post[3]),digits = 3),"\n")
  
  cat("Relative biomass in last year = ",format(tail(CLA.obj[["output"]][["output_timeseries"]]$bk, n=1),digits = 3),
      "k, 2.5th perc = ",format(tail(CLA.obj[["output"]][["output_timeseries"]]$bk_low, n=1),digits = 3),", 97.5th perc = ",
      format(tail(CLA.obj[["output"]][["output_timeseries"]]$bk_high, n=1),digits = 3),"\n")
  
  cat("Exploitation F/(r/2) in last year = ",format(tail(CLA.obj[["output"]][["output_timeseries"]]$FFmsy, n=1),digits = 3),
      ", 2.5th perc = ",format(tail(CLA.obj[["output"]][["output_timeseries"]]$FFmsy_low, n=1),digits = 3),", 97.5th perc = ",
      format(tail(CLA.obj[["output"]][["output_timeseries"]]$FFmsy_high, n=1),digits = 3),"\n")
  
  cat("q= ",format(CLA.obj[["output"]][["output_posteriors"]]$q_post[1],digits = 3),
      ", ","95% CL =",format(CLA.obj[["output"]][["output_posteriors"]]$q_post[2],digits = 3), "-",
      format(CLA.obj[["output"]][["output_posteriors"]]$q_post[3],digits = 3),"\n")
  
  cat("------------------------------------\n")
  cat("\n")
  cat("Result for Management \n")
  cat("\n")
  
  if(CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_correction_note[1]=="Fmsy was corrected downward to account for reduced recruitment."){
    cat("Fmsy =",format(CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1],digits = 3),
        ", 95% CL =",format(CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2],digits = 3),"-",
        format(CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3],digits = 3)," - Fmsy was corrected downward to account for reduced recruitment.","\n")} else {
          cat("Fmsy =",format(CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1],digits = 3),
              ", 95% CL =",format(CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2],digits = 3),"-",
              format(CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3],digits = 3),"\n")
        }
  
  # cat("MSY =",format(round(CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1]),digits = 2),
  #               ", 95% CL =",format(round(CLA.obj[["output"]][["output_posteriors"]]$MSY_post[2]),digits = 2),"-",
  #               format(round(CLA.obj[["output"]][["output_posteriors"]]$MSY_post[3]),digits = 2),"\n")
  
  cat("Bmsy =",format(round(CLA.obj[["output"]][["output_posteriors"]]$Bmsy_post[1]),digits = 2),
      ", 95% CL =",format(round(CLA.obj[["output"]][["output_posteriors"]]$Bmsy_post[2]),digits = 2),"-",
      format(round(CLA.obj[["output"]][["output_posteriors"]]$Bmsy_post[3]),digits = 2),"\n")
  
  
  
  cat("Biomass in last year =",format(round(tail(CLA.obj[["output"]][["output_timeseries"]]$B,1)),digits = 2),
      ", 2.5th perc =",format(round(tail(CLA.obj[["output"]][["output_timeseries"]]$B_low,1)),digits = 2),", 97.5 perc =",
      format(round(tail(CLA.obj[["output"]][["output_timeseries"]]$B_high,1)),digits = 2),"\n")
  
  
  cat("B/Bmsy in last year  =",format(tail(CLA.obj[["output"]][["output_timeseries"]]$BBmsy,1),digits = 3),
      ", 2.5th perc =",format(tail(CLA.obj[["output"]][["output_timeseries"]]$BBmsy_low,1),digits = 3),", 97.5 perc =",
      format(tail(CLA.obj[["output"]][["output_timeseries"]]$BBmsy_high,1),digits = 3),"\n")
  
  
  cat("Fishing mortality in last year  =",format(tail(CLA.obj[["output"]][["output_timeseries"]]$f,1),digits = 3),
      ", 2.5th perc =",format(tail(CLA.obj[["output"]][["output_timeseries"]]$f_low,1),digits = 3),", 97.5 perc =",
      format(tail(CLA.obj[["output"]][["output_timeseries"]]$f_high,1),digits = 3),"\n")
  
  
  cat("Exploitation F/Fmsy in last year  =",format(tail(CLA.obj[["output"]][["output_timeseries"]]$FFmsy,1),digits = 3),
      ", 2.5th perc =",format(tail(CLA.obj[["output"]][["output_timeseries"]]$FFmsy_low,1),digits = 3),", 97.5 perc =",
      format(tail(CLA.obj[["output"]][["output_timeseries"]]$FFmsy_high,1),digits = 3),"\n")
  # summary_list <- list(
  #   Stock_summary=data.frame(
  #   Species=CLA.obj[["input"]][["Stock_info"]]$ScientificName,
  #   Stock= CLA.obj[["input"]][["Stock_info"]]$Stock,
  #   Common_name=CLA.obj[["input"]][["Stock_info"]]$Name,
  #   Region=CLA.obj[["input"]][["Stock_info"]]$Region,
  #   Subregion= CLA.obj[["input"]][["Stock_info"]]$Subregion,
  #   StartYear=CLA.obj[["input"]][["Input_parameters"]]$StartYear,
  #   EndYear= CLA.obj[["input"]][["Input_parameters"]]$EndYear,
  #   btype=CLA.obj[["input"]][["Input_parameters"]]$btype,
  #   bk_prior_start_low=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.low),
  #   bk_prior_start_high=as.numeric(CLA.obj[["input"]][["Input_parameters"]]$stb.hi),
  #   bk_prior_int_low=ifelse(CLA.obj$input$Input_parameters$nbk!=1,as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low)
  #     if () {
  #       cat("Prior intermediate rel. biomass: ",format(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.low),digits = 3),
  #           "-", format(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$intb.hi),digits = 3), ", ", 
  #           CLA.obj[["input"]][["Input_parameters"]]$int.yr,"\n")}
  #     
  #     if (CLA.obj$input$Input_parameters$nbk==3) {
  #       cat("Prior final rel. biomass: ",format(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.low),digits = 3),
  #           "-", format(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$endb.hi),digits = 3),"\n")}
  #     
  #     cat("Prior range for r: ",CLA.obj[["input"]][["Input_parameters"]]$r.low, "-", CLA.obj[["input"]][["Input_parameters"]]$r.hi,"\n")
  #     cat("Derived prior range for k: ",format(round(CLA.obj$rk_object[["rkpriors"]]$prior.k.low),digits = 3), "-", format(round(CLA.obj$rk_object[["rkpriors"]]$prior.k.hi),digits = 3),"\n")
  #     cat("Derived prior MSY: ",format(round(as.numeric(CLA.obj[["input"]][["Input_parameters"]]$MSY_prior)),digits = 3),"\n")
  #     cat("Derived prior range for q: ",format(CLA.obj[["input"]][["Input_parameters"]]$q_low,digits = 3), "-", format(CLA.obj[["input"]][["Input_parameters"]]$q_high,digits = 3),"\n")
  #     
  #     
  
  
  #     
  #     
  #   ),
  #   Results=1,
  #   Management=1
  # )
  
  #return(summary_list)
}


ggkiel.plot= function(CLA.obj,METHOD) {
  if (METHOD=="CMSY") {
    clr="blue"} else if (METHOD=="BSM") {
      clr="red"
    }
  
  temp_data=as.data.frame(CLA.obj[["output"]][["output_timeseries"]])
  temp_data$yr=as.integer(row.names(temp_data))
  nyr=length(temp_data$yr)
  
  
  temp_data$FMSY_Catch=temp_data$B*CLA.obj[["output"]][["output_posteriors"]][["Fmsy_post"]][1]
  temp_data$FMSY_Catch[temp_data$BBmsy<0.5]= temp_data$B[temp_data$BBmsy<0.5]*temp_data$BBmsy[temp_data$BBmsy<0.5]*2*CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post[1]
  
  temp_data$FMSY_Catch_low=temp_data$B*CLA.obj[["output"]][["output_posteriors"]][["Fmsy_post"]][2]
  temp_data$FMSY_Catch_low[temp_data$BBmsy<0.5]= temp_data$B[temp_data$BBmsy<0.5]*temp_data$BBmsy[temp_data$BBmsy<0.5]*2*CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post[2]
  
  
  temp_data$FMSY_Catch_high=temp_data$B*CLA.obj[["output"]][["output_posteriors"]][["Fmsy_post"]][3]
  temp_data$FMSY_Catch_high[temp_data$BBmsy<0.5]= temp_data$B[temp_data$BBmsy<0.5]*temp_data$BBmsy[temp_data$BBmsy<0.5]*2*CLA.obj[["output"]][["output_posteriors"]]$Fmsy_post[3]
  
  temp_data$FMSY_Catch[ temp_data$FMSY_Catch>CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1]]=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1]
  
  my_y_title <-bquote(atop(Biomass~and~Catch~plot~"for"~bold(.(CLA.obj[["input"]][["Stock_info"]]$Stock))))
  
  pic_biocatch=ggplot2::ggplot()+
    ggplot2::geom_segment(aes(y=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                              yend=CLA.obj[["output"]][["output_posteriors"]]$MSY_post[1],
                              x = min(temp_data$yr), xend =  max( temp_data$yr),linetype="dotted" ),size=1)+
    ggplot2::geom_segment(aes(y=CLA.obj[["output"]][["output_posteriors"]]$Bmsy_post [1],
                              yend=CLA.obj[["output"]][["output_posteriors"]]$Bmsy_post [1],
                              x = min(temp_data$yr), xend =  max( temp_data$yr),linetype="dashed" ),size=1)+
    ggplot2::geom_line(data = temp_data,ggplot2::aes(x=yr,y=Catch,color="A"),size=0.7)+
    ggplot2::geom_ribbon(data = temp_data,ggplot2::aes(x=yr,ymin=Catch_low, ymax=Catch_high,fill="A"), alpha=0.2)+#y=ct.jags_low,
    ggplot2::geom_line(data = temp_data,ggplot2::aes(x=yr,y=B,color="B"),size=0.7)+
    ggplot2::geom_ribbon(data = temp_data,ggplot2::aes(x=yr,ymin=B_low, ymax=B_high,fill="B"), alpha=0.2)+#y=ct.jags_low,
    ggplot2::geom_line(data = temp_data,ggplot2::aes(x=yr,y=FMSY_Catch,color="C"),size=1.2,linetype="dashed")+
    ggplot2::geom_ribbon(data = temp_data,ggplot2::aes(x=yr,ymin=FMSY_Catch_low, ymax=FMSY_Catch_high,fill="C"), alpha=0)+#y=ct.jags_low,
    
    ggplot2::theme_classic()+
    ggplot2::scale_linetype_manual(labels=c("BMSY","MSY"),values=c("dashed","dotted" ))+
    ggplot2::scale_color_manual(labels=c("Catch and 95% CI","Biomass and 95% CI","Catch at Fmsy"),values=c("black","blue","green"))+
    ggplot2::scale_fill_manual(labels=c("Catch and 95% CI","Biomass and 95% CI","Catch at Fmsy"),values=c("black","blue","green"))+
    ggplot2::theme(
      legend.position = "bottom")+
    ggplot2::labs(x="Year",y="Catch",title=my_y_title,fill="",color="",linetype="")+
    ggplot2::scale_y_continuous(limits=c(0,NA)) +
    theme(text = element_text(size = 10)) 
  return(pic_biocatch)
}
