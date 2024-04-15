
# Instructions

#1. Put this script and AMSY_data.RData in the same folder 

#2. downoad and install Rtools from: https://cran.r-project.org/bin/windows/Rtools/

#3. downoad and install JAGS from:https://sourceforge.net/projects/mcmc-jags/

#4. downoad and install necessary packages, by running the following lines, after removing the hashtag (#)
#list.of.packages <- c("stringr","shinyWidgets", "DT","shiny","shinydashboard","shinyBS","mvtnorm","gplots","htm2txt","ggplot2","data.table","ggpubr","shinycssloaders","shinyFiles","fs","tibble")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#5. Press the "Run App" button from the Rstudio menu to start the app


#################################### Packages
library(stringr)
library(shinyWidgets)
library(DT)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(mvtnorm)
library(gplots)
library(htm2txt)
library(ggplot2)
library(data.table)
library(ggpubr)
library(shinycssloaders)
library(shinyFiles)
library(fs)
library(tibble)

#################################### Working directory
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  # path=getwd()

############## ############## ############## 
##############        UI      ############## 
############## ############## ############## 
  load("AMSY_data.RData")
   
  linebreaks <- function(n){HTML(strrep(br(), n))}
options(digits=3) # displays all numbers with three significant digits as default
options(scipen=100) # displays all numbers with three significant digits as default

FB_specs=species_DB$Species[order(species_DB$Species)]

'%!in%'=Negate('%in%')
template_cdat = data.frame(matrix(nrow = 0, ncol = ncol(cdat)))
colnames(template_cdat) = colnames(cdat)
template_CPUE_ID = data.frame(matrix(nrow = 0, ncol = ncol(cinfo)+2))
colnames(template_CPUE_ID) =c(colnames(cinfo),"Cr_sel","Stock_objID")
test_CPUE_ID=cinfo[,c("Stock", "ScientificName", "Resilience", "Bk.pr")] 

#source("AMSY_aux_functions.R")


#----------------------------------------------
#  FUNCTIONS ----
#----------------------------------------------
# 
#----------------------------------------------

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

AMSY.run=function(AMSY.obj) {
  res=AMSY.obj[["ID"]]$Resilience
  cdat_=AMSY.obj[["Data"]]
  ri1    <- exp(AMSY.obj[["mvnlogrk"]][,1])
  kqi1   <- exp(AMSY.obj[["mvnlogrk"]][,2])
  prior.r=c(AMSY.obj[["ID"]]$r.low, AMSY.obj[["ID"]]$r.hi)
  prior.kq=c(AMSY.obj[["ID"]]$prior.kq.low,AMSY.obj[["ID"]]$prior.kq.hi)
  mean.log.r=mean(log(prior.r))
  sd.log.r=(log(prior.r[2])-log(prior.r[1]))/4  # 
  mean.log.kq <- mean(log(prior.kq))
  sd.log.kq   <- (log(prior.kq[2])-log(prior.kq[1]))/4 # assume range covers 4 SD
  nyr=length(cdat_$yr)
  Bk.yr.i=AMSY.obj[["ID"]]$bk_prior_yr
  res.i        <- which(c("Very low","Low","Medium","High")%in%res) # determines process error strength
  sigma.r      <- c(0.05,0.07,0.1,0.15) # very low, low, medium, high # overall process error for productivity or r
  max.cpue     <- sort(cdat_$CPUE_used)[length(cdat_$CPUE_used)-1]
  prior.Bk=as.numeric(c(AMSY.obj[["ID"]]$bk_prior_low,  AMSY.obj[["ID"]]$bk_prior_high))
  MCA1 <-  SchaeferCPUE(yr=cdat_$yr,
                        cpue=cdat_$CPUE_used,
                        cpue.raw = cdat_$CPUE,
                        ri=ri1, kqi=kqi1, sigR=sigma.r[res.i],nyr=nyr,max.cpue=max.cpue,res=res,Bk.yr.i=Bk.yr.i,prior.Bk=prior.Bk, filter=TRUE) #><> correct PE input
  MCA_pre=check_SchaeferCPUE(MCA1,cdat_$CPUE_used,
                             cdat_$CPUE,cdat_$yr,sigma.r[res.i],AMSY.obj[["mvnlogrk"]],
                             mean.log.r=mean.log.r,sd.log.r=sd.log.r,mean.log.kq=mean.log.kq,sd.log.kq=sd.log.kq,res=res,Bk.yr.i=Bk.yr.i,prior.Bk=prior.Bk)
  MCA=MCA_pre[[1]]
  #  MCA=as.data.frame(MCA)
  return(MCA)
}

AMSY.products=function(AMSY.obj,AMSY_fit.obj) {
  
  MCA=AMSY_fit.obj
  cdat_=AMSY.obj[["Data"]]
  yr=cdat_$yr
  nyr=length(yr)
  rv       <- MCA[,"rv"]
  kqv      <- MCA[,"kqv"]
  MSYqv     <- rv * kqv/ 4
  MSYq.est  <- median(MSYqv)
  MSYq.lcl  <- as.numeric(quantile(MSYqv,0.025))
  MSYq.ucl  <- as.numeric(quantile(MSYqv,0.975))
  n.v        <- length(MSYqv)
  kqv.est    <- median(kqv)
  kqv.lcl  <- as.numeric(quantile(kqv,0.025))
  kqv.ucl  <- as.numeric(quantile(kqv,0.975))
  
  rv.est     <- 4*MSYq.est/kqv.est    # rv corresponding to median(kqv)
  rv.lcl   <- as.numeric(quantile(rv,0.025))
  rv.ucl   <- as.numeric(quantile(rv,0.975))
  
  cqt.sel           <- matrix(nrow=length(rv),ncol=nyr-1)
  colnames(cqt.sel) <- c(yr[1:nyr-1])
  for(j in 1:(nyr-1)) {
    cqt.sel[,j]     <- MCA[,j+2]}
  cqt.median        <- apply(cqt.sel,2,median)
  cqt.lcl           <- apply(cqt.sel,2,quantile,probs=0.025)
  cqt.ucl           <- apply(cqt.sel,2,quantile,probs=0.975)
  
  cpuet.sel           <- matrix(nrow=length(rv),ncol=nyr)
  colnames(cpuet.sel) <- c(yr[1:nyr])
  for(j in 1:nyr) {
    cpuet.sel[,j]     <- MCA[,j+2+nyr-1]}
  cpuet.median        <- apply(cpuet.sel,2,median)
  cpuet.lcl           <- apply(cpuet.sel,2,quantile,probs=0.025)
  cpuet.ucl           <- apply(cpuet.sel,2,quantile,probs=0.975)
  
  BBmsy.end       <- cpuet.median[nyr]/(kqv.est/2)
  BBmsy.end.lcl   <- cpuet.lcl[nyr]/(kqv.est/2)
  BBmsy.end.ucl   <- cpuet.ucl[nyr]/(kqv.est/2)
  
  
  Ft            <- cqt.median[1:(nyr-1)]/cpuet.median[1:(nyr-1)]
  FFmsy         <- Ft/(0.5*rv.est)
  FFmsy.end     <- FFmsy[nyr-1]
  Ft.lcl        <- cqt.lcl[1:(nyr-1)]/cpuet.median[1:(nyr-1)]
  FFmsy.lcl     <- Ft.lcl/(0.5*rv.est)
  FFmsy.end.lcl <- FFmsy.lcl[nyr-1]
  Ft.ucl        <- cqt.ucl[1:(nyr-1)]/cpuet.median[1:(nyr-1)]
  FFmsy.ucl     <- Ft.ucl/(0.5*rv.est)
  FFmsy.end.ucl <- FFmsy.ucl[nyr-1]
  
  outp_1=data.frame(rv.est=rv.est,rv.lcl=rv.lcl,rv.ucl=rv.ucl,kqv.est=kqv.est,
                    kqv.lcl=kqv.lcl,kqv.ucl=kqv.ucl,BBmsy.end=BBmsy.end,
                    BBmsy.end.lcl=BBmsy.end.lcl,BBmsy.end.ucl=BBmsy.end.ucl,
                    FFmsy.end=FFmsy.end,FFmsy.end.lcl=FFmsy.end.lcl,FFmsy.end.ucl=FFmsy.end.ucl,
                    MSYq.est=MSYq.est,MSYq.lcl=MSYq.lcl,MSYq.ucl=MSYq.ucl,n.v=n.v)
  
  cqt_=data.frame(cqt.median=cqt.median,cqt.lcl=cqt.lcl,cqt.ucl=cqt.ucl)
  cpuet_=data.frame(cpuet.median=cpuet.median,cpuet.lcl=cpuet.lcl,cpuet.ucl=cpuet.ucl)
  FFmsy_=data.frame(Ft=Ft,Ft.lcl=Ft.lcl,Ft.ucl=Ft.ucl,FFmsy=FFmsy,FFmsy.lcl=FFmsy.lcl,FFmsy.ucl=FFmsy.ucl)
  outp=list(outp_1=outp_1,cqt_=cqt_,cpuet_=cpuet_,FFmsy_=FFmsy_,cpuet.sel=cpuet.sel,cqt.sel=cqt.sel)
  
  AMSY.obj[["Outputs"]]=outp
  
  return(AMSY.obj)
}

SchaeferCPUE<-function(yr, cpue,cpue.raw, ri, kqi, sigR,nyr,max.cpue,res,Bk.yr.i,prior.Bk, filter){
  n.trial      <- 30 # times each year is calculated with new random error terms for r and cpue
  sigma.cpue   <- 0.3 # observation error for cpue
  min.viable   <- 20 # minimum number of viable r-kq pairs to be accepted for analysis
  max.viable   <- 5000 # maximum number of viable r-kq pairs to reduce processing time; set to 20000 if filter==FALSE
  # create matrix for results
  mdat  <- matrix(ncol = (2*nyr+1))
  colnames(mdat) <- c("rv","kqv",paste("c",yr[1:(nyr-1)],sep=""),paste("b",yr[1:nyr],sep=""))
  
  for(i in 1:length(ri)) { # for all submitted r-kq pairs
    
    for(trial in 1:n.trial) { # rerun every r-kq pair several times because error terms per year are random
      # max one succesful run across all years is returned per trial
      cqt         <- vector()
      cpuet       <- vector()
      FFmsy       <- vector()
      break.flag  <- FALSE
      
      for(t in 1:(nyr-1))  {  # for all years except the last one, for which catch cannot be calculated
        # assign random error terms to surplus production and to cpue
        err      <- exp(rnorm(1,0,sigR)) # set annual error for productivity
        cpuet[t] <- cpue[t]*exp(rnorm(1,0,sigma.cpue)) # assign error to cpue
        if(cpuet[t]<=0) {cpuet[t] <- 0.01*kqi[i]} # make sure cpuet is not zero or negative
        
        # calculate catch
        if(cpuet[t]/kqi[i] >= 0.25) {
          cqt[t] <- (cpuet[t] + cpuet[t] * ri[i] * (1-cpuet[t]/kqi[i]))*err - cpue[t+1]  } else {
            cqt[t] <- (cpuet[t] + cpuet[t] * ri[i] * (1-cpuet[t]/kqi[i])*(4*cpuet[t]/kqi[i]))*err - cpue[t+1] } # reduce r linearly below 0.25 kq
        
        # use moving average to smooth hectic catch predictions
        if(t == 2) {cqt[t] <- mean(c(cqt[t-1],cqt[t])) }
        if(t > 2) {cqt[t] <- mean(c(cqt[t-2],cqt[t-1],cqt[t])) }
        
        # calculate MSYq and F/Fmsy
        MSYq     <- ri[i]*kqi[i]/4
        FFmsy[t] <- 2*cqt[t]/(ri[i]*cpue[t])
        
        if(filter==TRUE) {
          ## Test compatibility of r-kq pairs with general prior popdyn knowledge
          ## If one test fails, break the loop and go to the next trial
          # (1) Exclude r-kq pair if catch is negative (cqt[t] < 0)
          mult.kqi    <- ifelse(res=="Very low",-0.06,ifelse(res=="Low",-0.02,0)) #relax rule for Very low and Low resilience
          if(cqt[t] < mult.kqi*kqi[i]) {break.flag<-TRUE;break}
          
          # (2) Exclude r-kq pair if catch exceeds biomass (cqt[t] > cpue[t])
          # if lowest cpue is close to zero, skip this test
          if(min(cpue.raw) > 0.1*max.cpue) {
            # in highly productive species, catch may exceed average annual biomass
            mult.cpue  <- ifelse(res=="High",1.4,ifelse(res=="Medium",1,ifelse(res=="Low",0.5,0.25)))
            if(cqt[t] > (mult.cpue*cpuet[t])) {break.flag<-TRUE;break} }
          # (3) Exclude r-kq pair if catch exceeds MSY
          # some overshooting of MSY is possible
          mult.msy <- ifelse(res=="Very low",10,ifelse(res=="Low",5,ifelse(res=="Medium",3,2)))
          if(cqt[t] > mult.msy*MSYq)        {break.flag<-TRUE;break}
          # (4) Exclude r-k pairs if F/Fmsy is highly unrealistic (negative or much too high)
          FFlow  <- ifelse(res=="Very low",-25,-3)
          FFhi   <- ifelse(res=="Very low",12,5)
          if(t > 1 && (FFmsy[t-1] < FFlow || FFmsy[t-1] > FFhi)) {break.flag<-TRUE;break}
          # (5) if relative cpue in the year of the B/k prior is outside of the prior range, discard trial
          #  relax rule if lower B/k prior range is <= 0.01
          if(prior.Bk[1] <= 0.01) { prior.Bk[1] <- 0.0001}
          if(t==Bk.yr.i && (cpuet[Bk.yr.i]/kqi[i] < prior.Bk[1] || cpuet[Bk.yr.i]/kqi[i] > prior.Bk[2])) {break.flag<-TRUE;break }
        } # end of condition for filtering
      } # end of t-loop through years
      # if t-loop was broken and flag==TRUE do not test further, do not plot points, do not store results
      if(break.flag==TRUE) { next }
      # assign error to last cpue and repeat filter (7) if applicable to last year
      cpuet[nyr] <- cpue[nyr]*exp(rnorm(1,0,sigma.cpue))
      if(cpuet[nyr]<=0) {cpuet[nyr] <- 0.01*kqi[i]} # make sure cpuet is not zero or negative
      if(filter==TRUE && Bk.yr.i==nyr && (cpuet[nyr]/kqi[i] < prior.Bk[1] || cpuet[nyr]/kqi[i] > prior.Bk[2])) { next }
      # If all tests are passed, add viable r-kq pair and predicted catch to matrix
      mdat     <- rbind(mdat,c(ri[i],kqi[i],cqt[1:(nyr-1)],cpuet[1:(nyr)]))
    } # end of trial-loop for trials per r-kq pair
    if(length(mdat[,1])>max.viable) { break} # end searching for viable pairs if n > max.viable
  } # end of i-loop through r-kq pairs
  
  mdat <- na.omit(mdat)
  return(mdat)
  
} # end of SchaeferCPUE function

check_SchaeferCPUE=function(MCA1,cpue,cpue.raw,yr,sigR,mvn.log.rk,mean.log.r,sd.log.r,mean.log.kq,sd.log.kq,res,Bk.yr.i,prior.Bk) {
  n.viable <- length(MCA1[,"rv"])
  min.viable   <- 20 # minimum number of viable r-kq pairs to be accepted for analysis
  max.viable   <- 5000 # maximum number of viable r-kq pairs to reduce processing time; set to 20000 if filter==FALSE
  n.p          <- 50000 # number of r-kq pairs to be analysed; will be doubled if too few viable pairs are found
  nyr=length(yr)
  max.cpue     <- sort(cpue)[length(cpue)-1]
  sigR_=sigR
  if(n.viable<min.viable) {
    #  cat("Too few r-kq pairs after filtering, repeating analysis with 2 times more pairs, extended prior ranges, and increased smoothing:\n")
    mvn.log.rk2 <- mvn(n=2*n.p,mean.log.r=mean.log.r,sd.log.r=1.2*sd.log.r,mean.log.kq=mean.log.kq,sd.log.kq=1.2*sd.log.kq)
    ri2  <- exp(mvn.log.rk2[,1])
    kqi2 <- exp(mvn.log.rk2[,2])
    cpue <- ksmooth(x=yr,y=cpue.raw,kernel="normal",n.points=length(yr),bandwidth=5)$y
    #############Changed sigR
    MCA2 <-  SchaeferCPUE(yr=yr, cpue=cpue,cpue.raw, ri=ri2, kqi=kqi2, sigR=sigR_,nyr=nyr,max.cpue=max.cpue,res=res,Bk.yr.i=Bk.yr.i,prior.Bk=prior.Bk, filter=TRUE)
    MCA  <- rbind(MCA1,MCA2)
    n.viable <- length(MCA[,"rv"])
    mvn.log.rk=mvn.log.rk2
  } else {MCA <- MCA1
  mvn.log.rk=mvn.log.rk
  }#cat("Viable r-kq pairs =",n.viable,"\n")
  
  if((n.viable)<10) {
    # cat("Too few r-kq pairs after filtering, doing analysis without filters:\n")
    MCA <-  SchaeferCPUE(yr=yr, cpue=cpue,cpue.raw, ri=ri2, kqi=kqi2, sigR=sigR_,nyr=nyr,max.cpue=max.cpue,res=res,Bk.yr.i=Bk.yr.i,prior.Bk=prior.Bk, filter=FALSE)
    # text(x=prior.r[1],y=0.9*prior.kq[1],"no filters")
  }
  MCA_pre=list(MCA,mvn.log.rk)
  return(MCA_pre)
}

#-------------------------------------------------------------
# Function to create multivariate-normal distribution for r-k
#-------------------------------------------------------------
mvn   <- function(n,mean.log.r,sd.log.r,mean.log.kq,sd.log.kq) {
  cor.log.rk   <- -0.607 #-0.871 # empirical value of log r-k correlation in 140 stocks analyzed with BSM
  cov.log.rk <- cor.log.rk*sd.log.r*sd.log.kq # covariance with empirical correlation and prior variances  covar.log.rk = matrix(NA, ncol=2,nrow=2)   # contract covariance matrix
  covar.log.rk      <- matrix(NA, ncol=2,nrow=2) # covariance matrix
  covar.log.rk[1,1] <- sd.log.r^2                # position [1,1] is variance of log.r
  covar.log.rk[2,2] <- sd.log.kq^2               # position [2,2] is variance of log.k
  covar.log.rk[1,2] = covar.log.rk[2,1] = cov.log.rk     # positions [1,2] and [2,1] are correlations
  mu.log.rk  <- (c(mean.log.r,mean.log.kq))      # vector of log.means
  mvn.log.rk <- mvtnorm::rmvnorm(n,mean=mu.log.rk,sigma=covar.log.rk,method="svd")
  return(mvn.log.rk)
}


ggplot.mvr=function(AMSY.prod,AMSY.fit.obj) {
  
  outp= AMSY.prod[["Outputs"]][["outp_1"]]
  MCA=AMSY.fit.obj
  prior.r=c(AMSY.prod[["ID"]][["r.low"]],AMSY.prod[["ID"]][["r.hi"]])
  prior.kq=c(AMSY.prod[["ID"]][["prior.kq.low"]],AMSY.prod[["ID"]][["prior.kq.hi"]])
  
  
  ri1    <- exp(AMSY.prod[["mvnlogrk"]][,1])  
  kqi1   <- exp(AMSY.prod[["mvnlogrk"]][,2])  
  my_y_title <-bquote(atop(Finding~viable~"r-k"~pairs~"for"~the~stock~bold(.(AMSY.prod[["ID"]]$Stock))~of~italic(.(AMSY.prod[["ID"]]$Species))))
  pic= ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(x=ri1,y=kqi1,col="A"),alpha=0.7)+
    ggplot2::geom_point(ggplot2::aes(x=MCA[,"rv"],y=MCA[,"kqv"],col="B"),alpha=0.7)+
    ggplot2::scale_x_continuous(trans='log',limits= c(0.95*quantile(ri1,0.001),1.2*quantile(ri1,0.999)),labels = function(x) round(as.numeric(x),2))+
    ggplot2::scale_y_continuous(trans='log',limits=c(0.95*quantile(kqi1,0.001),1.2*quantile(kqi1,0.999)),labels = function(x) round(as.numeric(x),2))+
    ggplot2::theme_classic()+ 
    ggplot2::geom_rect(aes(xmin = prior.r[1], xmax = prior.r[2], ymin =prior.kq[1], ymax = prior.kq[2]), 
                       color = "blue",linetype="dotted",alpha=0,size=1)+
    ggplot2::labs(x="r", y="k (cpue units)", title=my_y_title,color="")+
    ggplot2::geom_segment(ggplot2::aes(x = outp$rv.lcl, y = outp$kqv.est, xend = outp$rv.ucl, yend = outp$kqv.est), color = "red",size=1)+
    ggplot2::geom_segment(ggplot2::aes(x = outp$rv.est, y = outp$kqv.lcl, xend = outp$rv.est, yend = outp$kqv.ucl), color = "red",size=1)+
    ggplot2::geom_point(ggplot2::aes(x=outp$rv.est,y=outp$kqv.est,col="C"),size=2)+
    ggplot2::scale_color_manual(values=c("gray60","black","red"),labels=c("all r-k","viable r-k","central r-k \nwith CI"))
  
  return(pic)
  
}

ggplot.catch= function(AMSY.prod) {
  
  cdat_=AMSY.prod[["Data"]]
  yr=cdat_$yr
  nyr=length(yr)
  cqt_=AMSY.prod[["Outputs"]][["cqt_"]]
  outp=AMSY.prod[["Outputs"]][["outp_1"]]
  
  max.y  <- max(c(cqt_$cqt.ucl/outp$MSYq.est,1.1), na.rm=T)
  ribb_min=cqt_$cqt.lcl[2:(nyr-1)]/outp$MSYq.est
  ribb_min[ribb_min<0]=0
  my_y_title <-bquote(atop(Predicted~"Catch/MSY"~"for"~the~stock~bold(.(AMSY.prod[["ID"]]$Stock))~of~italic(.(AMSY.prod[["ID"]]$Species))))
  
  pic=ggplot2::ggplot()+
    ggplot2::geom_ribbon(ggplot2::aes(x=yr[2:(nyr-1)],ymin = ribb_min,
                                      ymax = cqt_$cqt.ucl[2:(nyr-1)]/outp$MSYq.est, fill = "A"),alpha=0.1)+
    ggplot2::geom_line(ggplot2::aes(x=yr[1:(nyr-1)],y=cqt_$cqt.median[1:(nyr-1)]/outp$MSYq.est,col="A"),size=1)+
    ggplot2::geom_point(ggplot2::aes(x=yr[1:(nyr-1)],y=cqt_$cqt.median[1:(nyr-1)]/outp$MSYq.est,fill="A"),shape=21,size=2)+
    ggplot2::scale_y_continuous(limits=c(0,max.y))+
    ggplot2::scale_x_continuous(breaks =seq(yr[1],yr[nyr],3))+
    ggplot2::scale_color_manual(values=c("red"),labels=c("Catch/MSY"))+
    ggplot2::scale_fill_manual(values=c("red"),labels=c("Catch/MSY"))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=1,linetype="MSY"),size=1)+
    ggplot2::theme_classic()+
    ggplot2::labs(y="Catch/MSY",x="Year",fill="",color="",linetype="",title=my_y_title)+
    ggplot2::scale_linetype_manual(values = c("dashed"),labels=c("MSY"))+
    ggplot2::theme(legend.position = "bottom")
  
  return(pic)
  
}

ggplot.FFmsy=function(AMSY.prod) {
  cdat_=AMSY.prod[["Data"]]
  yr=cdat_$yr
  nyr=length(yr)
  FFmsy_=AMSY.prod[["Outputs"]][["FFmsy_"]]
  max.y <- max(c(1.2,FFmsy_$FFmsy.ucl),na.rm=T)
  ribb_min=FFmsy_$FFmsy.lcl[2:(nyr-1)]
  ribb_min[ribb_min<0]=0
  my_y_title <-bquote(atop(Predicted~"F/Fmsy"~"for"~the~stock~bold(.(AMSY.prod[["ID"]]$Stock))~of~italic(.(AMSY.prod[["ID"]]$Species))))
  
  pic= ggplot2::ggplot()+
    ggplot2::geom_ribbon(ggplot2::aes(x=yr[2:(nyr-1)],ymin = ribb_min,
                                      ymax =FFmsy_$FFmsy.ucl[2:(nyr-1)], fill = "A"),alpha=0.1)+
    ggplot2::geom_line(ggplot2::aes(x=yr[1:(nyr-1)],y=FFmsy_$FFmsy,col="A"),size=1)+
    ggplot2::geom_point(ggplot2::aes(x=yr[1:(nyr-1)],y=FFmsy_$FFmsy,fill="A"),shape=21,size=2)+
    ggplot2::scale_y_continuous(limits=c(0,max.y))+
    ggplot2::scale_x_continuous(breaks =seq(yr[1],yr[nyr],3))+
    ggplot2::scale_color_manual(values=c("blue"),labels=c("F/Fmsy"))+
    ggplot2::scale_fill_manual(values=c("blue"),labels=c("F/Fmsy"))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=1),linetype="dashed",size=1)+
    ggplot2::theme_classic()+
    ggplot2::labs(y="F/Fmsy",x="Year",fill="",color="",linetype="",title=my_y_title)+
    ggplot2::scale_linetype_manual(values = c("dashed"),labels=c("MSY"))+
    ggplot2::theme(legend.position = "bottom")
  
  return(pic)
  
}

ggplot.BBmsy=function(AMSY.prod) {
  cdat_=AMSY.prod[["Data"]]
  yr=cdat_$yr
  nyr=length(yr)
  
  cpuet_=AMSY.prod[["Outputs"]][["cpuet_"]]
  outp=AMSY.prod[["Outputs"]][["outp_1"]]
  Bkt        <- cpuet_$cpuet.median/(0.5*outp$kqv.est)
  max.y      <- max(c(Bkt, outp$kqv.ucl/outp$kqv.est,cdat_$CPUE/(outp$kqv.est/2),cpuet_$cpuet.ucl/(0.5*outp$kqv.est)),na.rm=T)
  
  ribb_min=cpuet_$cpuet.lcl[2:nyr]/(0.5*outp$kqv.est)
  ribb_min[ribb_min<0]=0
  my_y_title <-bquote(atop("B/Bmsy"~"for"~the~stock~bold(.(AMSY.prod[["ID"]]$Stock))~of~italic(.(AMSY.prod[["ID"]]$Species))))
  
  pic=ggplot2::ggplot()+
    ggplot2::geom_ribbon(ggplot2::aes(x=yr[2:(nyr)],ymin = ribb_min,
                                      ymax =cpuet_$cpuet.ucl[2:nyr]/(0.5*outp$kqv.est), fill = "A"),alpha=0.1)+
    ggplot2::geom_line(ggplot2::aes(x=yr,y=Bkt,col="A"),size=1)+
    ggplot2::geom_point(ggplot2::aes(x=yr,y=Bkt,fill="A"),shape=21,size=2)+
    ggplot2::scale_y_continuous(limits=c(0,max.y))+
    ggplot2::scale_x_continuous(breaks =seq(yr[1],yr[nyr],3))+
    ggplot2::scale_color_manual(values=c("green"),labels=c("B/Bmsy"))+
    ggplot2::scale_fill_manual(values=c("green"),labels=c("B/Bmsy"))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=1),linetype="dashed",size=1)+
    ggplot2::geom_hline(ggplot2::aes(yintercept=outp$kqv.lcl/outp$kqv.est),linetype="dotted",size=1)+
    ggplot2::geom_hline(ggplot2::aes(yintercept=outp$kqv.ucl/outp$kqv.est),linetype="dotted",size=1)+
    ggplot2::geom_hline(ggplot2::aes(yintercept=0.5),linetype="longdash",color="red",size=1)+
    ggplot2::theme_classic()+
    ggplot2::labs(y="B/Bmsy",x="Year",fill="",color="",linetype="",title=my_y_title)+
    ggplot2::scale_linetype_manual(values = c("dashed"),labels=c("MSY"))+
    ggplot2::theme(legend.position = "bottom")
  
  return(pic)
}

ggplot.kobe=function(AMSY.prod) {
  cpuet.sel=AMSY.prod[["Outputs"]][["cpuet.sel"]]
  outp_1=AMSY.prod[["Outputs"]][["outp_1"]]
  FFmsy_=AMSY.prod[["Outputs"]][["FFmsy_"]]
  cpuet_=AMSY.prod[["Outputs"]][["cpuet_"]]
  cqt.sel=AMSY.prod[["Outputs"]][["cqt.sel"]]
  cdat_=AMSY.prod[["Data"]]
  
  nyr=length(cdat_$yr)
  bbmsy = (cpuet.sel[,nyr]/(0.5*outp_1$kqv.est))
  ffmsy = ((apply(cqt.sel[,(nyr-4):(nyr-1)],1,median)/cpuet.sel[,nyr])/(0.5*outp_1$rv.est))
  
  log.bbmsy = log(bbmsy[ffmsy>0]) # Prevents NA warning
  log.ffmsy = log(ffmsy[ffmsy>0]) # Prevents NA warning
  
  # get mean after all the CMSY subsetting (can't match with biomass sbmsetting)
  mu.kobe = c(median(log.ffmsy),median(log.bbmsy))
  # Get covariance of the 2 vectors
  cov.kobe = cov(cbind(log.ffmsy,log.bbmsy))
  # Generate 10000 new random deviates from a MVN
  log.kobe.mvn = mvtnorm::rmvnorm(10000 ,mean = mu.kobe,sigma = cov.kobe)
  kobe.mvn = exp(log.kobe.mvn)
  # Generate 10000 new random deviates from a MVN
  x.F_Fmsy =exp(log.kobe.mvn[,1])
  y.b_bmsy =exp(log.kobe.mvn[,2])
  
  kernelF <- gplots::ci2d(y.b_bmsy,x.F_Fmsy,nbins=151,factor=2.2,ci.levels=c(0.50,0.80,0.75,0.90,0.95),show="none",col=1,xlab= ifelse(harvest.label=="Fmsy",expression(paste(F/F[MSY])),expression(paste(H/H[MSY]))),ylab=expression(paste(B/B[MSY])))
  Bkt        <- cpuet_$cpuet.median/(0.5*outp_1$kqv.est)
  FFmsy=FFmsy_$FFmsy
  
  max.y   <- max(c(2, quantile(x.F_Fmsy,0.96),1.1*FFmsy,na.rm =T))
  max.x    <- max(c(2,quantile(y.b_bmsy,0.999),1.1*Bkt,na.rm =T))
  
  # -------------------------------------
  ## KOBE plot building
  # -------------------------------------
  #Create plot
  Pr.green = paste0(round(sum(ifelse(y.b_bmsy>1 & x.F_Fmsy<1,1,0))/length(y.b_bmsy)*100,1),"%")
  Pr.red =  paste0(round(sum(ifelse(y.b_bmsy<1 & x.F_Fmsy>1,1,0))/length(y.b_bmsy)*100,1),"%")
  Pr.yellow =  paste0(round(sum(ifelse(y.b_bmsy<1 & x.F_Fmsy<1,1,0))/length(y.b_bmsy)*100,1),"%")
  Pr.orange =  paste0(round(sum(ifelse(y.b_bmsy>1 & x.F_Fmsy>1,1,0))/length(y.b_bmsy)*100,1),"%")
  mid.year= floor(nyr/2)
  labls= data.frame(x=as.character(c(Pr.yellow,Pr.green,Pr.red,Pr.orange,"95% C.I.","80% C.I.","50% C.I.")),
                    y=as.factor(c("yellow", "green", "red", "orange","slategray1", "slategray2", "slategray3")))
  rects=data.frame(colr=c("yellow", "green", "red", "orange"),xmin=c(0,1,0,1),
                   xmax=c(1,max.x,1,max.x),ymin=c(0,0,1,1),ymax=c(1,1,max.y,max.y))
  points=data.frame(shp=c(1,2,3),x=c(Bkt[1],Bkt[mid.year],Bkt[nyr]),
                    y=c(FFmsy[1],FFmsy[mid.year], median(x.F_Fmsy)))
  
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
  my_y_title <-bquote(atop(Kobe~plot~"for"~the~stock~bold(.(AMSY.prod[["ID"]]$Stock))~of~italic(.(AMSY.prod[["ID"]]$Species))))
  
  picKobe=ggplot2::ggplot()+
    ggplot2::scale_x_continuous(limits=c(0,max.x),breaks=seq(0,max.x,0.5)) + ggplot2::theme_classic()+
    ggplot2::scale_y_continuous(limits=c(0,max.y),breaks=seq(0,max.y,0.5)) +ggplot2::labs(x=lx,y=ly,title = my_y_title)+
    ggplot2::geom_rect(data=rects,ggplot2::aes(xmin = xmin , xmax = xmax, ymin = ymin, ymax = ymax,fill =colr))  +
    ggplot2::geom_polygon(data=kern95,ggplot2::aes(x=x,y=y,fill=labls$x[5]), alpha = 0.65)+
    ggplot2::geom_polygon(data=kern80,ggplot2::aes(x=x,y=y,fill=labls$x[6]), alpha = 0.65)+
    ggplot2::geom_polygon(data=kern50,ggplot2::aes(x=x,y=y,fill=labls$x[7]), alpha = 0.65)+
    ggplot2::scale_fill_manual(name="",
                               labels  =c("50% C.I.","80% C.I.","95% C.I.",Pr.green,
                                          Pr.orange,Pr.red,
                                          Pr.yellow),
                               values=c(
                                 "slategray3", "slategray2",
                                 "slategray1", "green",
                                 "orange","red",
                                 "yellow"))+
    ggplot2::geom_point(ggplot2::aes(x=Bkt,y= c(FFmsy,median(x.F_Fmsy))),color="black",size=2.7)+
    ggplot2::geom_path(ggplot2::aes(x=Bkt,y= c(FFmsy,median(x.F_Fmsy))),color="black")+
    ggplot2::geom_point(data=points,ggplot2::aes(x=x,y=y,shape=factor(shp)),color="black",fill="white",size=2.7)+
    ggplot2::scale_shape_manual(name="",
                                labels  =c(as.character(cdat_$yr[1]),
                                           as.character(cdat_$yr[mid.year]),
                                           as.character(cdat_$yr[nyr])),
                                values=c(22,21,24))
  
  return(picKobe)
}


AMSY_retro=function(AMSY.object,retro_range) {
  cdat_retr=list() 
  nyr=length(AMSY.object[["Data"]]$yr)
  res=AMSY.object[["ID"]]$Resilience
  
  
  for (i in 1:retro_range) {
    cdat_retr[[i]]=AMSY.object[["Data"]][AMSY.object[["Data"]]$yr<=AMSY.object[["Data"]]$yr[nyr]-i,  ]}
  
  if (AMSY.object[["ID"]][["Ecreep_YN"]]==T) {
    for (j in 1:retro_range) {
      cdat_retr[[j]]$ecreep<- cdat_retr[[j]]$CPUE
      maxyr=max(as.integer( cdat_retr[[j]]$yr))
      inputecreep_yr=AMSY.object[["ID"]]$Start_yr_ecreep
      if (maxyr==inputecreep_yr | maxyr<inputecreep_yr) {
        inputecreep_yr=maxyr-1
      }
      for(m in 1:(maxyr-inputecreep_yr)) {
        cdat_retr[[j]]$ecreep[cdat_retr[[j]]$yr==(inputecreep_yr+m)]  <-  cdat_retr[[j]]$CPUE[cdat_retr[[j]]$yr==(inputecreep_yr+m)] *(1-AMSY.object[["ID"]]$Ecreep/100)^m # equation for decay in %; first cpue without correction
        cdat_retr[[j]]$CPUE_used= cdat_retr[[j]]$ecreep}
    } 
  }
  
  if(AMSY.object[["ID"]]$Smooth_YN==T ) {
    for (i in 1:retro_range) {  
      ####### SEE about change in d.cpue.raw_retros
      bw          <- AMSY.object[["ID"]]$Smooth_bw#log(2)/exp(mean(log(final_rpriors()))) # use population doubling time as bandwidth
      #bw          <- ifelse(bw < 3,3,bw) # enforce minimum bandwidth of 3
      cdat_retr[[i]]$CPUE_smthd<- ksmooth(x=cdat_retr[[i]]$yr,y=cdat_retr[[i]]$CPUE_used,kernel="normal",n.points=length(cdat_retr[[i]]$yr),bandwidth=bw)$y 
      cdat_retr[[i]]$CPUE_used= cdat_retr[[i]]$CPUE_smthd
    }
  }
  
  nyr_retros=list()
  
  for (i in 1:retro_range) {  
    nyr_retros[[i]]<- length(cdat_retr[[i]]$yr[!is.na(cdat_retr[[i]]$CPUE)])
  }
  max.cpue_retros=list()
  min.cpue_retros=list()
  for (i in 1:retro_range) {
    max.cpue_retros[[i]]<- sort(cdat_retr[[i]]$CPUE_used[!is.na(cdat_retr[[i]]$CPUE_used)])[length(cdat_retr[[i]]$CPUE_used[!is.na(cdat_retr[[i]]$CPUE_used)])-1]
    min.cpue_retros[[i]]<- min(cdat_retr[[i]]$CPUE_used[!is.na(cdat_retr[[i]]$CPUE_used)],na.rm = T)
  }
  minmax.cpue_retros=list(max.cpue_retros=max.cpue_retros,min.cpue_retros=min.cpue_retros)
  
  prior.r=c(AMSY.object[["ID"]]$r.low,AMSY.object[["ID"]]$r.hi)
  Bk.yr.i=AMSY.object[["ID"]]$bk_prior_yr
  priorkq_retro=list()
  
  prior.Bk=as.numeric( c(AMSY.object[["ID"]]$bk_prior_low,AMSY.object[["ID"]]$bk_prior_high))
  mean.prior.Bk   <- mean(prior.Bk)
  rr.prior.Bk     <- (mean.prior.Bk-prior.Bk[1])/mean.prior.Bk
  mean.log.r=mean(log(prior.r))
  sd.log.r=(log(prior.r[2])-log(prior.r[1]))/4  # assume range covers 4 SD
  
  
  for (i in 1:retro_range) {
    max.cpue=minmax.cpue_retros[["max.cpue_retros"]][[i]]
    
    # us relative range of B/k as relative range for kq
    prior.kq.low.1  <- (1-rr.prior.Bk)*cdat_retr[[i]]$CPUE_used[cdat_retr[[i]]$yr==Bk.yr.i]/mean.prior.Bk
    prior.kq.hi.1   <- (1+rr.prior.Bk)*cdat_retr[[i]]$CPUE_used[cdat_retr[[i]]$yr==Bk.yr.i]/mean.prior.Bk
    
    # kq must be > max cpue unless near unexploited
    prior.kq.low.2  <- ifelse(prior.kq.low.1 < max.cpue,ifelse(mean.prior.Bk >= 0.85,0.9*max.cpue,max.cpue),prior.kq.low.1)
    # increase lower kq prior if cpue is small and flat
    if((max(cdat_retr[[i]]$CPUE_used[!is.na(cdat_retr[[i]]$CPUE_used)])/min(cdat_retr[[i]]$CPUE_used[!is.na(cdat_retr[[i]]$CPUE_used)]))<2) {
      prior.kq.low <- ifelse(mean.prior.Bk < 0.3,2*prior.kq.low.2,
                             ifelse(mean.prior.Bk < 0.6,1.5*prior.kq.low.2,prior.kq.low.2))
    } else {prior.kq.low <- prior.kq.low.2 }
    
    # kq.hi at least 30-50% larger than kq.low, depending on Bk prior
    if(mean.prior.Bk >= 0.6) {
      prior.kq.hi.2  <- ifelse(prior.kq.hi.1 < (1.3*prior.kq.low),1.3*prior.kq.low,prior.kq.hi.1) } else {
        prior.kq.hi.2  <- ifelse(prior.kq.hi.1 < (1.5*prior.kq.low),1.5*prior.kq.low,prior.kq.hi.1) }
    
    # if upper prior kq is too hi, limit to 3 times lower range
    prior.kq.hi   <- ifelse(prior.kq.hi.2 > (3*prior.kq.low),3*prior.kq.low,prior.kq.hi.2)
    prior.kq_          <- c(prior.kq.low,prior.kq.hi,prior.kq.low.1,prior.kq.hi.1)
    priorkq_retro[[i]]=prior.kq_
  }
  
  n.p          <- 50000 # number of r-kq pairs to be analysed; will be doubled if too few viable pairs are found
  n.trial      <- 30 # times each year is calculated with new random error terms for r and cpue
  mean.log.r=mean(log(prior.r))
  sd.log.r=(log(prior.r[2])-log(prior.r[1]))/4  # assume range covers 4 SD
  mvn.log.rk=list()
  for (i in 1:retro_range) {
    prior.kq=priorkq_retro[[i]][c(1,2)]
    #------------------------------------------------------------------
    # Sampling of r-k space
    #------------------------------------------------------------------
    # turn numerical ranges into log-normal distributions
    mean.log.kq <- mean(log(prior.kq))
    sd.log.kq   <- (log(prior.kq[2])-log(prior.kq[1]))/4 # assume range covers 4 SD
    mvn.log.rk[[i]] <- mvn(n=n.p,mean.log.r=mean.log.r,sd.log.r=sd.log.r,
                           mean.log.kq=mean.log.kq,sd.log.kq=sd.log.kq)
    # ri1    <- exp(mvn.log.rk[,1])    #############################perito!!!!!!!!!!!!!!!!!!!!!!!?
    # kqi1   <- exp(mvn.log.rk[,2])     #############################perito!!!!!!!!!!!!!!!!!!!!!!!?
  }
  
  MCA=list()
  for (i in 1:retro_range) {
    mvnlogrk= mvn.log.rk[[i]]
    ri1    <- exp(mvnlogrk[,1])
    kqi1    <- exp(mvnlogrk[,2])
    prior.kq=priorkq_retro[[i]][c(1,2)]
    mean.log.kq <- mean(log(prior.kq))
    sd.log.kq   <- (log(prior.kq[2])-log(prior.kq[1]))/4 # assume range covers 4 SD
    
    nyr=nyr_retros[[i]]
    Bk.yr.i=AMSY.object[["ID"]]$bk_prior_yr
    res.i        <- which(c("Very low","Low","Medium","High")%in%res) # determines process error strength
    sigma.r      <- c(0.05,0.07,0.1,0.15) # very low, low, medium, high # overall process error for productivity or r
    max.cpue=minmax.cpue_retros[["max.cpue_retros"]][[i]]
    
    MCA1 <-  SchaeferCPUE(yr=cdat_retr[[i]]$yr,
                          cpue=cdat_retr[[i]]$CPUE_used,
                          cpue.raw = cdat_retr[[i]]$CPUE,
                          ri=ri1, kqi=kqi1, sigR=sigma.r[res.i],nyr=nyr,max.cpue=max.cpue,res=res,Bk.yr.i=Bk.yr.i,prior.Bk=prior.Bk, filter=TRUE) #><> correct PE input
    
    MCA_pre=check_SchaeferCPUE(MCA1,cdat_retr[[i]]$CPUE_used,
                               cdat_retr[[i]]$CPUE,cdat_retr[[i]]$yr,sigma.r[res.i],mvnlogrk,
                               mean.log.r=mean.log.r,sd.log.r=sd.log.r,mean.log.kq=mean.log.kq,sd.log.kq=sd.log.kq,res=res,Bk.yr.i=Bk.yr.i,prior.Bk=prior.Bk)
    MCA[[i]]=MCA_pre
  }
  
  FFmsy.retrospective=list() #retrospective analysis
  BBmsy.retrospective=list() #retrospective analysis
  years.retrospective=list()#retrospective analysis
  
  for (i in 1:retro_range) {
    nyr<-nyr_retros[[i]]
    yr_=cdat_retr[[i]]$yr[!is.na(cdat_retr[[i]]$CPUE)]
    MCA_A=MCA[[i]][[1]]
    rv       <- MCA_A[,"rv"]
    kqv      <- MCA_A[,"kqv"]
    MSYqv     <- rv * kqv / 4
    MSYq.est  <- median(MSYqv)
    kqv.est    <- median(kqv)
    
    rv.est     <- 4*MSYq.est/kqv.est    # rv corresponding to median(kqv)
    cqt.sel           <- matrix(nrow=length(rv),ncol=nyr-1)
    colnames(cqt.sel) <- c(yr_[1:nyr-1])
    for(j in 1:(nyr-1)) {
      cqt.sel[,j]     <- MCA_A[,j+2]}
    cqt.median        <- apply(cqt.sel,2,median)
    cpuet.sel           <- matrix(nrow=length(rv),ncol=nyr)
    colnames(cpuet.sel) <- c(yr_[1:nyr])
    for(j in 1:(nyr)) {
      cpuet.sel[,j]     <- MCA_A[,j+2+nyr-1]}
    cpuet.median        <- apply(cpuet.sel,2,median)
    
    Ft            <- cqt.median[1:(nyr-1)]/cpuet.median[1:(nyr-1)]
    FFmsy         <- Ft/(0.5*rv.est)
    Bkt        <- cpuet.median/(0.5*kqv.est)
    
    FFmsy.retrospective[[i]]<-data.frame(yr=yr_[1:nyr-1],FFmsy=FFmsy,ID=rep(i,length(FFmsy))) #retrospective analysis
    BBmsy.retrospective[[i]]<-data.frame(yr=yr_,Bkt=Bkt,ID=rep(i,length(Bkt)))  #retrospective analysis
  }
  FFmsy.retrospective=data.table::rbindlist(FFmsy.retrospective)
  BBmsy.retrospective=data.table::rbindlist(BBmsy.retrospective)
  
  retro_outcomes=list(FFmsy.retrospective=FFmsy.retrospective,BBmsy.retrospective=BBmsy.retrospective)
  
  AMSY.object[["Retro"]]=retro_outcomes
  AMSY.object[["ID"]]$Retro_range=retro_range
  return(AMSY.object)
}

ggplot.retro=function(AMSY.object) {
  cdat_=AMSY.object[["Data"]]
  ffmsy_retro=AMSY.object[["Retro"]][["FFmsy.retrospective"]]
  yr_=cdat_$yr
  nyr=length(yr_)
  FFmsy_= AMSY.object[["Outputs"]][["FFmsy_"]]
  max.y_ffmsy <- max(c(1.2,FFmsy_$FFmsy.ucl),na.rm=T)
  cpuet_=AMSY.object[["Outputs"]][["cpuet_"]]
  outp=AMSY.object[["Outputs"]][["outp_1"]]
  
  bbmsy_retro=AMSY.object[["Retro"]][["BBmsy.retrospective"]]
  Bkt<- cpuet_$cpuet.median/(0.5*outp$kqv.est)
  
  def_year=data.frame(yr=yr_ ,Bkt=Bkt,ID=rep(0,length(yr_)))
  bbmsy_retro=rbind(def_year,bbmsy_retro)
  
  max.y_bbmsy <- max(c(1.2,1.1*Bkt, 1.1*bbmsy_retro$Bkt ),na.rm=T)
  
  
  my_y_title1 <-bquote(atop("F/Fmsy Retrospective"~"for"~the~stock~bold(.(AMSY.object[["ID"]]$Stock))~of~italic(.(AMSY.object[["ID"]]$Species))))
  my_y_title2 <-bquote(atop("B/Bmsy Retrospective"~"for"~the~stock~bold(.(AMSY.object[["ID"]]$Stock))~of~italic(.(AMSY.object[["ID"]]$Species))))
  
  
  def_year_fmsy=data.frame(yr=yr_[1:(nyr-1)] ,FFmsy=FFmsy_$FFmsy,ID=rep(0,length(yr_[1:(nyr-1)])))
  ffmsy_retro=rbind(def_year_fmsy,ffmsy_retro)
  
  picffmsy=  ggplot2::ggplot()+
    # ggplot2::geom_line(ggplot2::aes(x=yr_[1:(nyr-1)],y=FFmsy_$FFmsy),col="black",size=1)+
    ggplot2::geom_line(data=ffmsy_retro, ggplot2::aes(x=yr,y=FFmsy,col=as.factor(ID)),size=1)+
    ggplot2::geom_point(data=ffmsy_retro, ggplot2::aes(x=yr,y=FFmsy,col=as.factor(ID),shape=as.factor(ID)),size=2)+
    ggplot2::scale_y_continuous(limits=c(0,max.y_ffmsy))+
    ggplot2::scale_x_continuous(breaks =seq(yr_[1],yr_[nyr],3))+
    # ggplot2::scale_color_manual(values=c("blue"),labels=c("F/Fmsy"))+
    # ggplot2::scale_fill_manual(values=c("blue"),labels=c("F/Fmsy"))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=1),linetype="dashed",size=1)+
    ggplot2::theme_classic()+
    ggplot2::labs(y="F/Fmsy",x="Year",fill="",color="",linetype="",shape="",title=my_y_title1)+
    ggplot2::theme(legend.position = "bottom")
  
  picbbmsy=  ggplot2::ggplot()+
    # ggplot2::geom_line(ggplot2::aes(x=yr_,y=Bkt,col=0),size=1)+
    ggplot2::geom_line(data=bbmsy_retro, ggplot2::aes(x=yr,y=Bkt,col=as.factor(ID)),size=1)+
    ggplot2::geom_point(data=bbmsy_retro, ggplot2::aes(x=yr,y=Bkt,col=as.factor(ID),shape=as.factor(ID)),size=2)+
    ggplot2::scale_y_continuous(limits=c(0,max.y_bbmsy))+
    ggplot2::scale_x_continuous(breaks =seq(yr_[1],yr_[nyr],3))+
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
}

#---------------------------------------------
# END OF FUNCTIONS
#---------------------------------------------

################################################################
######################          UI        ######################
################################################################

shinyUI <- shinydashboard::dashboardPage(skin = "green",
  shinydashboard::dashboardHeader(title = "AMSY"),
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
                                column(width=8,offset=0.5,shinydashboard::box(title = "AMSY ",  solidHeader = TRUE,status="success",
                                                                              uiOutput("intro_link"),
                                                                              width=10))),
                              shiny::fluidRow(
                                column(width=8,shinydashboard::box(title = "Step-by-step process", solidHeader = TRUE,status="success",
                                                                   shiny::h4( "1. Load your data or select one of the provided stocks"),
                                                                   shiny::h4( "2. Prepare the data"),
                                                                   shiny::h4( "3. Explore and decide about priors"),
                                                                   shiny::h4( "4. Run the model and export the results"),width=10),offset=5))#,
                              # headerPanel(
                              # HTML('<p><shiny::img src="Picture2.png"/></p>')
                              # shiny::fluidRow( #system.file("image","Picture2.png",package = "triton")
                              #   shinydashboard::box(img(src="Picture2.jpg", height="80%", width="80%"),align="center",
                              #                       background = "green", width = 12)
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
                                                                               tabPanel("Species info",
                                                                                        # selectizeInput("Sp_Scient", label = "Scientific name", choices = NULL,
                                                                                        #                options = list(create = T,maxOptions  = 10,placeholder = "Species name",
                                                                                        #                               searchConjunction = 'AND')),
                                                                                        textInput("Sp_Scient", "Scientific"),
                                                                                        textInput("Sp_Comm", "Common name"),
                                                                                        textInput("Stock_comment", "Comments")
                                                                               ),
                                                                               width = 12)),
                                ),
                                column(width = 8,
                                       shinydashboard::valueBoxOutput("Sps_Info",width = 8)
                                ),
                              ),
                              shiny::fluidRow(
                                                 tags$br(),
                                                 shinydashboard::valueBoxOutput("adv_up2",width =12)
                              ),
                                shiny::fluidRow(
                                column(width=4,
                                                        shinydashboard::box(title = "Data load",  collapsible = TRUE,
                                                                            fileInput("file1", "Choose csv file",multiple = TRUE,
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
                                                                            shinyWidgets::pickerInput(inputId = "CPUE_col",label = "CPUE column",
                                                                                                      choices = c(""),options = list(
                                                                                                        title = "Define the CPUE column")),
                                                                            width=12)),
                                column(width=8,
                                       shiny::fluidRow(
                                                          shinydashboard::box(title = "Loaded data", solidHeader = TRUE,status = "success",collapsible = TRUE,
                                                                              DT::dataTableOutput("contents"),width=12))
                                )
                              ),
                              shiny::fluidRow(
                                                 tags$br(),
                                                 shinydashboard::valueBoxOutput("adv_up4",width =12)
                              ),
                              shiny::fluidRow(
                                column(width = 4,
                                                        shinydashboard::box(title = "Stock creation", solidHeader = TRUE,status = "success",background = "green",
                                                                            textInput("Crtd_StckID_input", shiny::h4("Input a stockID"), ""),
                                                                            shinyWidgets::actionBttn(inputId="button_2",label =" Create stock object",
                                                                                                     style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                     no_outline=F,block=F,color="success"),
                                                                            width = 12)
                                ),
                              
                                column(width = 8,
                                        conditionalPanel(condition = "input.button_2",
                                                        shinydashboard::box(title = "CPUE plot", solidHeader = TRUE,status = "success",
                                                                            shiny::plotOutput("Biomass_plot2"),width = 12,height=400))
                                   )),
                              shiny::fluidRow(
                                conditionalPanel(condition = "input.button_2",
                                                 shinydashboard::valueBoxOutput("Stock_infobox_1",width = 8)))
      ),
      ################################################################
      ###################### PAGE 3 EXPLORE DATA #####################
      ################################################################
      tabItem(tabName = "Expl_dt_page",
              # useShinyjs(),
                 fluidRow(
                valueBoxOutput("adv_expl1",width =12)
              ),
              fluidRow(
                column(width = 4,
                       box(title = "Explore existing stocks", solidHeader = TRUE,status = "success",background = "green",
                           textInput("txt1", h4("Select stock:"), ""),
                           shinyWidgets::actionBttn(inputId="button_1",label =" Create stock object",
                                      style = "unite",size = "md",icon = icon("paper-plane"),
                                      no_outline=F,block=F,color="success"),
                           width=12),
                       valueBoxOutput("txtout1",width =12),
                       conditionalPanel(condition = "input.button_1",
                                        valueBoxOutput("Stock_infobox_2",width = 12))
                ),
                column(width = 8,
                       fluidRow(box(title = "Existing stocks", solidHeader = TRUE,status = "success",
                                    DT::dataTableOutput("Exist_Stcks_tble"),width = 12)),
                       fluidRow(
                         box(title = "CPUE plot", solidHeader = TRUE,
                             plotOutput("Biomass_plot"),width = 12,height=400)
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
                                                                                 no_outline=F,block=F,color="success"),
                                                        shinyBS::bsTooltip("prepare_data", title="Press to proceed to the next step",
                                                                           placement = "bottom", trigger = "hover",
                                                                           options = NULL),width = 4
                                    )),
                   conditionalPanel(condition = "input.prepare_data",
                                    valueBoxOutput("Stock_infobox_prepare",width = 8))
              ),
              fluidRow(
                column(width=4,
                        conditionalPanel(condition = "input.prepare_data", 
                                         shinydashboard::box(collapsible = F,
                                                            tags$b("Change start or end year for the analyses"),
                                                            shinyWidgets::prettySwitch(
                                                              inputId = "upMaterial1",
                                                              label = "Edit",
                                                              slim = T,
                                                              value = FALSE,
                                                              status = "info" ),
                                                            conditionalPanel(condition = "input.upMaterial1",
                                                                             sliderInput("CPUE_yr_slct", "",
                                                                                         min = 1900, max = 2023, value = c(1950, 2023),step=1,sep = "")),
                                                            helpText("Note: Select start and end year such that you trust that the data are reasonable."),
                                                            tags$b("Add effort creep"),
                                                            shinyWidgets::prettySwitch(
                                                              inputId = "ecreepyes",
                                                              label = "Edit",
                                                              slim = T,
                                                              value = FALSE,
                                                              status = "info" ),
                                                            conditionalPanel(condition = "input.ecreepyes",
                                                                             uiOutput("help_ecreep"),
                                                                             sliderInput("ecreep_year", "Set starting year for e-creep",
                                                                                         min = 1900, max = 2023, value = 2000,step=1,sep = ""),      
                                                                             helpText("Note: Choose the year from which the ecreep effect starts. Consider that the first sonars became available in the 1970s, the first GPS units in the 1980s, cheap fish finders and chart plotters after 2000."),
                                                                             sliderInput("ecreepslider", "% value",
                                                                                         min = 0, max = 5, value = c(2),step=0.5,sep = "")
                                                            ),
                                                            tags$b("CPUE smoothing"),
                                                            shinyWidgets::prettySwitch(
                                                              inputId = "smoothcpue",
                                                              slim = T,
                                                              label = "Smooth CPUE",
                                                              value = FALSE,status = "info" ),
                                                            helpText("Note: Set to 'Smooth' if variability of CPUE data seems unrealistically high."),
                                                            uiOutput("smooth_recom"),
                                                            conditionalPanel(condition = "input.smoothcpue",
                                                                             sliderInput(inputId = "bw_inpt", label = "",
                                                                                         min = 1, max = 6, value =3, step =1)),
                                           width =12))
                       ),
                column(width = 8,
                       conditionalPanel(condition = "input.prepare_data",
                                        shinycssloaders::withSpinner(shiny::plotOutput(outputId = "Check_pdata_plot")),
                                        linebreaks(2),
                                        shinydashboard::valueBoxOutput("When_happy",width = 12)
                                        )
                       )
                )
      ),
      
       ################################################################
      ###################### PAGE 5 PRIOR ENTRY  #####################
      ################################################################
      tabItem(tabName = "priors",
              fluidRow(
                conditionalPanel(condition = "input.button_1 |input.button_2",
                                 shinydashboard::valueBoxOutput("Stock_infobox_patience",width = 8))),
              fluidRow( h3("A. Confirm stock selection")),
               fluidRow(
                conditionalPanel(condition = "input.button_1 |input.button_2",
                                 shinydashboard::box(collapsible = F,
                                    shinyWidgets::actionBttn(inputId="procc_rpriors",label ="Connect",
                                                style = "unite",size = "md",icon = icon("paper-plane"),
                                                no_outline=F,block=F,color="success"),
                                    shinyBS::bsTooltip("procc_rpriors", title="Press to proceed to the next step",
                                               placement = "bottom", trigger = "hover",
                                               options = NULL),
                                    width = 4)
                                 ),
                conditionalPanel(condition = "input.procc_rpriors",
                                 shinycssloaders::withSpinner(valueBoxOutput("Stock_infobox_3",width = 8)
                                                              )
                                 )
                ),
              fluidRow(
                conditionalPanel(condition = "input.procc_rpriors",h3("B. Select r prior"))),
              fluidRow(
                conditionalPanel(condition = "input.procc_rpriors",
                                 shinydashboard::box(collapsible = F,
                                     tags$b("Change r prior"),
                                     shinyWidgets::prettySwitch(
                                       inputId = "Acc_FBpriors",
                                       label = "Edit",
                                       slim = T,
                                       value = FALSE,status = "info" ),
                                     helpText("Note: The Resilience classification from FishBase/SeaLifeBase provides a general and often best first prior range for r and is used as default. You can manually change that range if better data are available."),
                                     conditionalPanel(condition = "input.Acc_FBpriors",
                                                      selectInput("resilience_in","Resilience",
                                                                  choices=c("","Very low","Low","Medium","High"),
                                                                  selected = ""),
                                                      helpText("Note: Very low: 0.015-0.01. Low: 0.05-0.5. Medium: 0.2-0.8. High: 0.6-1.5."),
                                                      sliderInput("priors_rrange", "r prior range",
                                                                  min = 0, max = 1.5, value = c(0.2, 0.8),step=0.01,sep = ""),
                                                      helpText("Note: r low and r high priors are an optional set of parameters to specify the range of intrinsic growth rate for the selected species. If no values are provided, the range will be based on Resilience. If values are given, the Resilience choise will be ignored.")
                                                      ),
                                     shinyWidgets::materialSwitch(
                                       inputId = "Acc_rpriors",
                                       label = " Accept r prior and continue",
                                       status = "success"),
                                     width=4)),
                conditionalPanel(condition = "input.procc_rpriors",
                                shinydashboard::valueBoxOutput("FB_resil",width =5),
                                 shinydashboard::valueBoxOutput("Res_rpriors",width = 3))
              ),
              fluidRow(
                conditionalPanel(condition = "input.Acc_rpriors",
                                          h3("C. Select other priors. Start by setting prior of relative stock size (B/k) for a selected year"))),
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
                                                                                                 width=12)
                                                                             )
                                                            ,width=4)
                                 )
                ),
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
                                                              inputId = "upMaterial5",
                                                              slim = T,
                                                              label = "Edit",
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.upMaterial5",
                                                                             selectInput("expert_bk_start","B/k start year based on expertise knowledge",
                                                                                         choices= c("","Unexploited, 0.75-1.0", "Sustainable, 0.4-0.8", "Medium, 0.2-0.6", "Low, 0.01-0.4","Very low, 0.01-0.2"),
                                                                                         selected = ""),
                                                                           #  helpText("Note: Very low: 0.01-0.2. Low: 0.01-0.4. Medium: 0.2-0.6. Sustainable: 0.4-0.8. Unexploited: 0.75-1.0."),
                                                                             textOutput("calc_Bk_start"),
                                                                             sliderInput("man_Bk_start", "",
                                                                                         min = 0, max = 1.5, value = c(0.4, 0.8),step=0.01,sep = "")),
                                                            tags$b("Change B/k prior for intermediate year"),
                                                            shinyWidgets::prettySwitch(
                                                              inputId = "upMaterial6",
                                                              slim = T,
                                                              label = "Edit",
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.upMaterial6",
                                                                             selectInput("expert_bk_ind","B/k intermediate year based on expertise knowledge",
                                                                                         choices= c("","Unexploited, 0.75-1.0", "Sustainable, 0.4-0.8", "Medium, 0.2-0.6", "Low, 0.01-0.4","Very low, 0.01-0.2"),
                                                                                         selected = ""),
                                                                           #  helpText("Note: Very low: 0.01-0.2. Low: 0.01-0.4. Medium: 0.2-0.6. Sustainable: 0.4-0.8. Unexploited: 0.75-1.0."),
                                                                             textOutput("calc_Bk_int"),
                                                                             sliderInput("man_Bk_int_year", "Int. year",
                                                                                         min = 1900, max = 2023, value = 2020,step=1,sep = ""),
                                                                             sliderInput("man_Bk_int", "",
                                                                                         min = 0, max = 1.5, value = c(0.4, 0.8),step=0.01,sep = "")),
                                                            tags$b("Change B/k prior for end year"),
                                                            shinyWidgets::prettySwitch(
                                                              inputId = "upMaterial7",
                                                              label = "Edit",
                                                              slim = T,
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.upMaterial7",
                                                                             selectInput("expert_bk_end","B/k end year based on expertise knowledge",
                                                                                         choices= c("","Unexploited, 0.75-1.0", "Sustainable, 0.4-0.8", "Medium, 0.2-0.6", "Low, 0.01-0.4","Very low, 0.01-0.2"),
                                                                                         selected = ""),
                                                                            # helpText("Note: Very low: 0.01-0.2. Low: 0.01-0.4. Medium: 0.2-0.6. Sustainable: 0.4-0.8. Unexploited: 0.75-1.0."),
                                                                             textOutput("calc_Bk_end"),
                                                                             sliderInput("man_Bk_end", "",
                                                                                         min = 0, max = 1.5, value = c(0.4, 0.8),step=0.01,sep = "")),
                                                            width=12))
                ),
                column(width=8,
                       conditionalPanel(condition = "input.Acc_rpriors",
                                        shiny::h4(tagList("Select B/k priors based on expert knowledge ", actionLink(inputId = "open_helpTable", label = "(Guidance)"))),
                                        shiny::plotOutput(outputId = "Check_priors_plot")
                       ))),
              
              shiny::fluidRow(
                conditionalPanel(condition = "input.Acc_rpriors",
                shinyWidgets::materialSwitch(
                inputId = "go_to_d",
                label = "Continue to B/k prior selection",
                status = "success"))),
              fluidRow(
                conditionalPanel(condition = "input.go_to_d",
                                 h3("D. Select single B/k prior to be used by AMSY"))),
                fluidRow(
                column(width=4,
                       fluidRow(width=12,
                                conditionalPanel(condition = "input.Acc_rpriors",
                                conditionalPanel(condition = "input.go_to_d",
                                                 shinydashboard::box(collapsible = F,
                                                                                      radioGroupButtons(
                                                                                        inputId = "Id004",
                                                                                        label = "Select a B/k prior that makes the CPUE time series best overlap with the independent expert perception of stock development. Do NOT go back to change the Expert priors unless independent knowledge justifies a change.", 
                                                                                        choices = c("Select one of the B/k priors", "Set B/k prior manually"),
                                                                                        status = "primary"
                                                                                      ),
                                                                                      conditionalPanel(condition = "input.Id004=='Select one of the B/k priors'",   
                                                                                                       shinyWidgets::awesomeRadio(inputId = "select_123",label = "",
                                                                                                                                  choices = c(First="First",Second="Second",Third="Third"), #Expertise="Expertise",
                                                                                                                                  selected = "First",inline = F,
                                                                                                                                  checkbox = F)
                                                                                      ),
                                                                                      conditionalPanel(condition = "input.Id004=='Set B/k prior manually'",                    
                                                                                                    sliderInput("bk_priors_year", "Set B/k prior year",
                                                                                                  min = 1900, max = 2023, value = 2000,step=1,sep = ""),      
                                                                                      selectInput("bk_priors","Set priors of relative stock size (B/k) for the selected year",
                                                                                                  choices= c("","Unexploited, 0.75-1.0", "Sustainable, 0.4-0.8", "Medium, 0.2-0.6", "Low, 0.01-0.4","Very low, 0.01-0.2"),
                                                                                                  selected =  ""),
                                                                                   shinyWidgets::prettySwitch(
                                                                                        inputId = "manual_bk_priors",
                                                                                        label = "Set B/k prior range manually",
                                                                                        slim = T,
                                                                                        value = FALSE,
                                                                                        status = "info" ),
                                                                                      conditionalPanel(condition = "input.manual_bk_priors",
                                                                                                       sliderInput("bk_priors_rrange", "B/k prior range",
                                                                                                                   min = 0, max = 1, value = c(0.2, 0.4),step=0.01,sep = ""),
                                                                                                       ))
                                                                     ,width =12),
                                                 shinydashboard::box(collapsible = F,
                                                                     # conditionalPanel(condition = "input.Save_priors", 
                                                                     #helpText("Note: Copy the working directory address and paste it here."),
                                                                     #helpText("example: 'C:\ABC_outputs'"),
                                                                     linebreaks(1),
                                                                     tags$b("Set working directory"),
                                                                     shinyDirButton("directory", "Select folder", "Please select a folder",multiple=FALSE),
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
                                )))),
                column(width=8,
                       conditionalPanel(condition = "input.Acc_rpriors",
                       conditionalPanel(condition = "input.go_to_d",
                                        plotOutput(outputId = "CPUE_MAIN_PLOT"))),
                       linebreaks(2),
                       conditionalPanel(condition = "input.Acc_rpriors",
                       conditionalPanel(condition = "input.go_to_d",
                         conditionalPanel(condition = "input.Save_priors",
                                        shinydashboard::box(collapsible = F,
                                                            htmlOutput("param_sofar"),
                                                            tags$hr(style = "border-top: 3px solid #000000;"),
                                                            verbatimTextOutput("middle_outputs"),
                                                            tags$hr(style = "border-top: 3px solid #000000;") ,width=12)
                                        )))
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
                                                 shinydashboard::box(collapsible = F,align="center", background = "green",
                                                                     column(width=4,align="center",
                                                                            awesomeRadio(
                                                                              inputId = "Id049",
                                                                              label = "Select 'A' to run the current stock object or select 'B' to choose one of the saved object versions. You can examine the content of the chosen object by pressing the 'See selected object' button", 
                                                                              choices = c("A", "B"),
                                                                              selected = "A",
                                                                              inline = TRUE, 
                                                                              checkbox = TRUE)
                                                                            ),
                                                                     column(width=4,align="center",
                                                                            conditionalPanel(condition = "input.Id049=='B'",
                                                                                             uiOutput("select_diff_obj")
                                                                                             )
                                                                            ),
                                                                     column(width=4,align="center",
                                                                            conditionalPanel(condition = "input.Id049=='B'",
                                                                                             shinyWidgets::actionBttn(inputId="see_obj",label =" See selected object",
                                                                                                                      style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                                      no_outline=F,block=F,color="primary"))  
                                                                     ),width=12)
                                                 )
                                ), 
                                shiny::fluidRow(
                                column(width=4,align="center",
                                       conditionalPanel(condition = "input.Save_priors",
                                                        shinyWidgets::actionBttn(inputId="Start_run",label =" Start",
                                                                                 style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                 no_outline=F,block=F,color="success")),
                                       shinyBS::bsTooltip("Start_run", title="Press to run the model",
                                                          placement = "bottom", trigger = "hover",
                                                          options = NULL)),
                                column(width=8,
                                       conditionalPanel(condition = "input.Save_priors",
                                                        shinydashboard::valueBoxOutput("Run_infobox_patient",width=6)),
                                       conditionalPanel(condition = "input.Start_run",
                                                        shinydashboard::valueBoxOutput("Run_infobox_1",width=6)))),
                              tags$br(),
                              shiny::fluidRow(
                                conditionalPanel(condition = "input.Start_run",
                                shinyWidgets::progressBar(
                                  id = "prog_bar",value = 0,total = 100,title = "",
                                  display_pct = TRUE,striped = T,# size="sm",
                                  status="success"))
                              ),
                              shiny::fluidRow(
                                conditionalPanel(condition = "input.Start_run",
                                                 shinydashboard::box(collapsible = F,align="center", background = "green",
                                                                     uiOutput("working_directory"),width = 12))
                              ),
                              shiny::fluidRow(
                                conditionalPanel(condition = "input.Start_run",
                                                 shinydashboard::box(collapsible = F,
                                                  tags$b("Results of AMSY analysis:"),
                                                 tags$hr(style = "border-top: 3px solid #000000;"),
                                                htmlOutput("stock_species"),
                                                 verbatimTextOutput("present_outputs"),
                                                     tags$hr(style = "border-top: 3px solid #000000;") ,width=12)
                                                )
                                ),
                              shiny::fluidRow(
                                column(width=4,
                                       conditionalPanel(condition = "input.Start_run",
                                                        shinydashboard::box(collapsible = F,
                                                                            tags$b("Retrospective analysis"),
                                                                            linebreaks(1),
                                                                            tags$hr(style = "border-top: 3px solid #000000;"),
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
                                                                                                                      no_outline=F,block=F,color="success"),
                                                                                             shinyWidgets::progressBar(
                                                                                               id = "prog_bar_retro",value = 0,total = 100,title = "",
                                                                                               display_pct = TRUE,striped = T,# size="sm",
                                                                                               status="success")
                                                                            ),width=12),  
                                                        shinydashboard::box(collapsible = F,
                                                                            # linebreaks(1),
                                                                            tags$b("Save a graph"),
                                                                            linebreaks(1),
                                                                            tags$hr(style = "border-top: 3px solid #000000;"),
                                                                            shinyWidgets::materialSwitch(
                                                                              inputId = "DL_picA",
                                                                              label = "Save",
                                                                              value = FALSE,status = "info" ),
                                                                            conditionalPanel(condition = "input.DL_picA",
                                                                                             tags$b("Select graph"),
                                                                                             shinyWidgets::pickerInput(inputId = "Run_select_pic",
                                                                                                                       label = "",choices =c("A","B","C","D","E","F"),
                                                                                                                       options = list(title = "Select graph"),
                                                                                                                       choicesOpt = list(
                                                                                                                         subtext = c(
                                                                                                                           "Analysis of viable r-k",
                                                                                                                           "Relative Catch plot",
                                                                                                                           "FFmsy plot",
                                                                                                                           "BBmsy plot",
                                                                                                                           "Kobe plot",
                                                                                                                           "Retrospective pics"
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
                                                                                                                        color = "success",style = "unite",
                                                                                                                        no_outline=F,block=F)
                                                                            ),width=12)
                                       )
                                ),
                                column(width=8, 
                                       conditionalPanel(condition = "input.Start_run",
                                                        shinydashboard::box(collapsible = F,
                                                                            align="center",
                                                                            shinycssloaders::withSpinner(shiny::plotOutput("rk_space")),
                                                                            shiny::h5("Plot of r-k prior space with the cloud of gray points representing the multivariate log-normal distribution of r and k corresponding to a correlation of -0.607, derived from full Schaefer models applied to 140 real stocks. The dotted blue rectangle indicates the prior ranges for r and k. The red cross indicates the estimated central r-k pair with plausible 95% confidence limits."),
                                                                            width = 12),#,height = 420
                                                      shinydashboard::box(collapsible = F,
                                                                          align="center",
                                                                          shinycssloaders::withSpinner(shiny::plotOutput("Catch_outp")),
                                                                          shiny::h5("The red curve shows the predicted catch relative to MSY, which is indicated by the dashed horizontal line. The pink area indicates plausible 95% confidence limits."),
                                                                          width = 12),
                                                        shinydashboard::box(collapsible = F,
                                                                            align="center",
                                                                            shinycssloaders::withSpinner(shiny::plotOutput("FFmsy_outp")),
                                                                            shiny::h5("The blue curve shows the predicted exploitation (F/Fmsy) with the light blue area indicating plausible 95% confidence limits. The dashed horizontal line indicates Fmsy."),
                                                                            width = 12),
                                                        shinydashboard::box(collapsible = F,
                                                                            align="center",
                                                                            shinycssloaders::withSpinner(shiny::plotOutput("BBmsy_outp")),
                                                                            shiny::h5("The green curve shows the predicted relative stock size (B/Bmsy) with the light green area indicating plausible 95% confidence limits. The dashed black horizontal line indicates Bmsy with dotted black lines as plausible confidence limits. The dashed red line indicates half of Bmsy and thus a stock size below which recruitment may be impaired."),
                                                                            width = 12),
                                                        shinydashboard::box(collapsible = F,
                                                                            align="center",
                                                                            shinycssloaders::withSpinner(shiny::plotOutput("kobe_plot")),
                                                                            shiny::h5("The Kobe plot combines the time series of stock size (B/Bmsy on the X-axis) and exploitation (F/Fmsy on the Y-axis). The colors identify combinations of stock size and exploitation as: Green = sustainable; Yellow = overfished; Orange = subject to overfishing; Red = overfished and subject to overfishing. The black line shows the time series of stock status and exploitation, and the shaded areas give the plausible confidence intervals for the last year as detailed in the legend."),
                                                                            width = 12)))),
                              shinyBS::bsModal(id='popup_Page_1',
                                               title ='Retrospective Graphs, It takes some time to do all the necessary calculations, so be patient!',
                                               trigger="Retrospective",
                                               size = "large",
                                               shinycssloaders::withSpinner(shiny::plotOutput('retro_pics')),
                                               shiny::h4("Retrospective analysis compares the latest predicted exploitation and stock status with those where one to several years have been removed from the time series. This shows how dependent the latest predictions are on the latest data (the larger the deviations, the stronger the dependence and the need for precaution).")
                                               ),
                              shinyBS::bsModal(id='modalExample3',
                                               title ='Selected object',
                                               trigger="see_obj",
                                               size = "large",
                                               shiny::tableOutput('see_object_table')),
            ),
      shinydashboard::tabItem(tabName = "addit_info",
                              #tags$hr(style = "border-top: 2px solid #000000;"),
                              shiny::fluidRow(shiny::h3(tags$b("References:")),
                                              shiny::h4(tagList("Froese R, Winker H, Coro G, Demirel N, Tsikliras AC, Dimarchopoulou D, Scarcella G, Palomares D, Dureuil M, Pauly D, 2020.", br(), "Estimating stock status from relative abundance and resilience. ICES Journal of Marine Science, 77(2): 527-538. https://doi.org/10.1093/icesjms/fsz230"))),
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

 # shinyBS::toggleModal(session, "popup_Page_1", toggle = "toggle")
  #url_froese= shiny::a("Link", href="https://doi.org/10.1093/icesjms/fsz230")
  
  output$intro_link=renderUI({ shiny::h4(tagList("AMSY is a Bayesian MCMC tool to estimate time series of relative Biomass (B/Bmsy), fishing pressure (F/Fmsy) and relative catch (C/MSY) from a time series of abundance indices such as catch per unit of effort (CPUE), which is used as a short name from here on. Other required input are priors for resilience/productivity and for relative stock size at any year of the time series (Froese et al. 2020), https://doi.org/10.1093/icesjms/fsz230"))})
  
   Stock_obj <- reactiveValues()
   Stock_obj$Stocks=data.frame(ID=1:200,Stock=rep(NA,200))
   Stock_obj$CPUE_ID <- template_CPUE_ID
   Stock_obj$CPUE_data <- template_cdat
  shinyWidgets::useSweetAlert()

  ###### Create advise info boxes
  # url_fshbs= a("FishBase", href="https://www.fishbase.se/search.php",style = "color:black")
  # url_slfbs= a("SeaLifeBase", href="https://sealifebase.se/search.php",style = "color:black")

  output$adv_up1= shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Create stock object - step 1"),shiny::h5(tagList("Provide the scientific and common name for your species. Make sure that the scientific name matches exactly the scientific name used in FishBase or SeaLifeBase, because this name will be used to extract information for the priors."  )) , #If your species isn't in the dropdown list, type its name and press Add [Species Name].... # ",url_fshbs," or ",url_slfbs, "
      icon = shiny::icon("dice-one"),
      color ="green" )#"light-blue"
    })


  output$adv_up2=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Create stock object - step 2"),
      shiny::h5("Load your cpue, abundance or index data from a comma-delimited (.csv) file.",
                         "That file should contain at least one column for Year and one column for the cpue, abundance or index data.",
                         "Use the options in the box to help with loading the data."),
      icon = shiny::icon("dice-two"),
      color ="green" )#"light-blue"
  })

  output$adv_up4=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Create stock object - step 3"),shiny::h5("Provide a unique name for your stock. Then press the 'Create stock object' button."),
      icon = shiny::icon("dice-three"),
      color ="green" )#"light-blue"
  })
 
  ###### Update select tools
  # updateSelectizeInput(session, "Sp_Scient",choices = FB_specs,server = T,selected="")
  
  observe({
    updateTextInput(session, "Sp_Comm", value = species_DB$FBname[species_DB$Species==input$Sp_Scient])
  }) 
  
  ###CREATE box(title = "Selected Species Info"
  output$Sps_Info=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Species info"),shiny::h4(HTML(paste0("Scientific name: ", tags$b(tags$em(input$Sp_Scient)),br(),br(),
                                                      "Common name: ",tags$b(input$Sp_Comm),br(),br(),
                                                      "Comments: ",tags$b(input$Stock_comment)
                                                     ))),
      icon = shiny::icon("file-alt"),
      color ="green" )#"light-blue"
  })
  
  output$adv_expl1=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Explore stock object"),shiny::h5("In this section you can explore stocks stored in our database.",br(),
                                                  "Select a stock from the table and then press the 'Create stock object' button.") ,
      icon = shiny::icon("envelope"),
      color ="green" )#"light-blue"
  })

  ###CREATE UPLOAD DATA
  inp_data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round,2)
    observe({
      temp_df<- df
      shinyWidgets::updatePickerInput(session, "Yr_col", choices = names(temp_df),
                                      selected ="")
      shinyWidgets::updatePickerInput(session, "CPUE_col", choices = names(temp_df),
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
    if(input$procc_rpriors > 0 ) {
      shinyWidgets::sendSweetAlert(
        session = session,title = "Warning",
        text = "If you have already run an analysis for a stock and you want to run another, please press 'Quit' and reload with 'Run App'.", type = "warning")
    }
      if(input$Crtd_StckID_input=="") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Input a stock ID!",
        type = "error")
    }  else {
      if(input$button_2 > 0) {
        newLine1 <- isolate(c(input$Crtd_StckID_input,
                              input$Sp_Comm,
                              #NA,
                              input$Stock_comment,
                              input$Sp_Scient,
                              rep(NA,10),
                              "created"
        ))
        if (all(is.na(Stock_obj$Stocks$Stock))){
          Stock_obj$Stocks$Stock[1]=newLine1[1]
          Stock_obj$CPUE_ID[1,]=c(newLine1,1)
        } else {# if(newLine1[4] %!in% Stock_obj$Stocks$stock) {
          Stock_obj$Stocks$Stock[length(Stock_obj$Stocks$Stock[!is.na(Stock_obj$Stocks$Stock)])+1]=newLine1[1]
          Stock_obj$CPUE_ID[length(Stock_obj$Stocks$Stock[!is.na(Stock_obj$Stocks$Stock)]),]=c(newLine1,length(Stock_obj$Stocks$Stock[!is.na(Stock_obj$Stocks$Stock)]))
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
            newLine_CPUE_data <- isolate(data.frame(
            Stock= rep(input$Crtd_StckID_input,length(inp_data()[,input$Yr_col])),
            yr=as.integer(inp_data()[,input$Yr_col]),
            CPUE=as.numeric(inp_data()[,input$CPUE_col]),
            CPUE_smthd= rep(NA,length(inp_data()[,input$Yr_col])),
            ecreep= rep(NA,length(inp_data()[,input$Yr_col])),
            CPUE_used= rep(NA,length(inp_data()[,input$Yr_col])),
            Stock_objID=rep(Stock_obj$CPUE_ID$Stock_objID[Stock_obj$CPUE_ID$Stock==input$Crtd_StckID_input],length(inp_data()[,input$Yr_col]))
            ))
      }
      Stock_obj$CPUE_data <- rbind(Stock_obj$CPUE_data, newLine_CPUE_data)
    }
  })
  
  output$Stock_infobox_1=shinydashboard::renderValueBox({
    shinydashboard::valueBox(shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the stock object ", input$Crtd_StckID_input,br(),
                                                                         " of the species ",tags$em(input$Sp_Scient), ".",br(),
                                                                         "You can know proceed to the 'Prepare data' tab."))),
                             shiny::icon("thumbs-up", lib = "glyphicon"),
                             color = "green") })


  output$Biomass_plot2= shiny::renderPlot({
    ggplot2::ggplot(data=Stock_obj$CPUE_data[Stock_obj$CPUE_data$Stock==input$Crtd_StckID_input,], ggplot2::aes(x=yr, y=CPUE)) +
      ggplot2::geom_line(color="blue",linewidth=1)+
      ggplot2::labs(y="CPUE", x="Year")+
      ggplot2::geom_point(color="black",size=2)+
      ggplot2::theme_classic()+
      ggplot2::scale_y_continuous(limits = c(0,1.1*max(Stock_obj$CPUE_data[Stock_obj$CPUE_data$Stock==input$Crtd_StckID_input,"CPUE"],na.rm = T)))+
      ggplot2::theme(text = ggplot2::element_text(size = 16))   
      }, height = 300)

  # ###### EXPLORE EXISTING STOCKS
  res_mod <- callModule(
    module = shinyWidgets::selectizeGroupServer,
    id = "my_filters",
    inline = FALSE,
    data = test_CPUE_ID,
    vars = c("Stock", "ScientificName", "Resilience", "Bk.pr")
  )

    output$Exist_Stcks_tble <- DT::renderDataTable({
    DT::datatable(res_mod()[,c("Stock", "ScientificName", "Resilience", "Bk.pr")],
                  selection = 'single', options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
  })

  observe({
    updateTextInput(session, "txt1",value=res_mod()[input$Exist_Stcks_tble_rows_selected,"Stock"])
  })

  output$txtout1=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Info"),shiny::h5(if (input$txt1=="") {
        "Select a stock"} else {HTML(paste(cinfo$Name[cinfo$Stock==input$txt1]))
        }),
      icon = shiny::icon("info-circle"),
      color = "aqua") })                                      
                                           

  output$Biomass_plot= shiny::renderPlot({
    validate(
      need(input$txt1 %in% cdat$Stock, 'Choose a valid Stock from the last column of the above Data Table')
    )
    p2= ggplot2::ggplot(data=cdat[cdat$Stock==input$txt1 & cdat$CPUE>0,],ggplot2::aes(x=yr, y=CPUE, group=1)) +
      ggplot2::geom_line(color="blue",size=1)+
      ggplot2::labs(y="CPUE", x="Year")+
      ggplot2::geom_point(color="black",size=2)+
      ggplot2::theme_classic()+
      ggplot2::scale_y_continuous(limits = c(0,1.1*max(cdat$CPUE),na.rm=T))+
      ggplot2::theme(text = ggplot2::element_text(size = 16))   
    p2
  }, height = 300)
  # 
  observeEvent(input$button_1,{
    if(input$procc_rpriors > 0 ) {
      shinyWidgets::sendSweetAlert(
        session = session,title = "Warning",
        text = "If you have already run an analysis for a stock and you want to run a new one for another stock, it is advised to reload the app and start from scratch.", type = "warning")
    }
     if(input$button_1 > 0) {
      if(input$txt1 %!in% cinfo$Stock) {
        shinyWidgets::sendSweetAlert(
          session = session,title = "Error...",
          text = "Select a stock from the table!", type = "error")
      } else {
        newLine1 <- isolate(c(as.vector(cinfo[cinfo$Stock==input$txt1,1:14]),
                              "selected"))
        if (all(is.na(Stock_obj$Stocks$Stock))){
          Stock_obj$Stocks$Stock[1]=input$txt1
          Stock_obj$CPUE_ID[1,]=c(newLine1,1)
        } else {#if(input$txt1 %!in% Stock_obj$Stocks$stock)
          Stock_obj$Stocks$Stock[length(Stock_obj$Stocks$Stock[!is.na(Stock_obj$Stocks$Stock)])+1]=newLine1[1]
          Stock_obj$CPUE_ID[length(Stock_obj$Stocks$Stock[!is.na(Stock_obj$Stocks$Stock)]),]=c(newLine1,length(Stock_obj$Stocks$Stock[!is.na(Stock_obj$Stocks$Stock)]))
        }
      }
    }
  })

  observeEvent(input$button_1,{
    if(input$button_1 > 0) {
      if(input$txt1 %!in% cinfo$Stock) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Select a stock from the table!",
          type = "error"
        )
      } else {
        newLine_CPUE_data <- isolate(cbind(
          cdat[cdat$Stock==input$txt1,1:6],
          Stock_objID=rep(Stock_obj$CPUE_ID$Stock_objID[Stock_obj$CPUE_ID$Stock_objID==max(Stock_obj$CPUE_ID$Stock_objID)],
                          nrow(cdat[cdat$Stock==input$txt1,])))
        )
        Stock_obj$CPUE_data <- rbind(Stock_obj$CPUE_data, newLine_CPUE_data)
        # }
      }}
  })
  # 
  output$Stock_infobox_2=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the stock object ", cinfo[cinfo$Stock==input$txt1,"Stock"],
                                                  " of the species ",tags$em(cinfo[cinfo$Stock==input$txt1,"ScientificName"]), ".",br(),
                                                  "You can know proceed to the 'Prepare data' tab."))),
      shiny::icon("thumbs-up", lib = "glyphicon"),
      color = "green") })

  # ################################################################
  # ###################### PAGE 4 Prepare Data  #####################
  # ################################################################
  # 
  Final_stock=eventReactive(input$prepare_data,{
    req(Stock_obj)
    final_stock=list(CPUE_ID=Stock_obj$CPUE_ID[Stock_obj$CPUE_ID$Stock_objID==max(Stock_obj$CPUE_ID$Stock_objID,na.rm = T),],
                     CPUE_Data=Stock_obj$CPUE_data[Stock_obj$CPUE_data$Stock_objID==max(Stock_obj$CPUE_ID$Stock_objID,na.rm = T),])
    final_stock[["CPUE_ID"]]["ScientificName"]=final_stock[["CPUE_ID"]]["ScientificName"]
    final_stock[["CPUE_Data"]]["CPUE"][final_stock[["CPUE_Data"]]["CPUE"]==0]=NA
    return(final_stock)
  })

  output$Stock_infobox_prepare=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Stock object"),shiny::h5(HTML(paste0("You have selected the stock ",
                                                      tags$b(Final_stock()[["CPUE_ID"]][1,"Stock"]), " of the species ",
                                                      tags$b(tags$em(Final_stock()[["CPUE_ID"]][1,"ScientificName"]))
      ))),
      icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "green") })
  
  
  observe({
    req(Final_stock())
    start.yr =Final_stock()[["CPUE_Data"]][c("yr","CPUE")]$yr[which(Final_stock()[["CPUE_Data"]][c("yr","CPUE")]$CPUE>0)[1]]
    end.yr    =max(as.integer(Final_stock()[["CPUE_Data"]]$yr),na.rm = T)
    updateSliderInput(session, "CPUE_yr_slct", value = c(start.yr,end.yr),min = start.yr, max =end.yr)
  })
  
  #url_palom= a("Link", href="https://doi.org/10.5751/ES-11136-240331")
  output$help_ecreep=renderUI({ shiny::h5(tagList("Note: Over time, fishers become more efficient at catching fish; a 2% increase in catchabilty per year is common (Palomares and Pauly 2019), https://doi.org/10.5751/ES-11136-240331"))})
  
  observe({
    req(Final_stock())
    end.yr    =max(as.integer(Final_stock()[["CPUE_Data"]]$yr),na.rm = T)
    updateSliderInput(session, "ecreep_year", value =input$CPUE_yr_slct[1],min =input$CPUE_yr_slct[1], max =end.yr-1)
    
  })
  
  CPUE_obj=reactive({
    req(Final_stock())
    cp_obj=Final_stock()[["CPUE_Data"]]
    minyr=min(as.integer(cp_obj$yr[!is.na(cp_obj$CPUE)]))
    maxyr=max(as.integer(cp_obj$yr[!is.na(cp_obj$CPUE)]))
    cpue_obj=data.frame(yr=seq(minyr,maxyr,1))
    cpue_obj=merge(x = cpue_obj, y = cp_obj, all.x = TRUE)
    cpue_obj=cpue_obj[cpue_obj$yr>=input$CPUE_yr_slct[1],]
    cpue_obj=cpue_obj[cpue_obj$yr<=input$CPUE_yr_slct[2],]
    cpue_obj$CPUE_used=cpue_obj$CPUE
    
    if (input$ecreepyes==T) {
      # apply correction for effort-creep to commercial(!) CPUE if indicated by user
      cpue_obj$ecreep<- cpue_obj$CPUE
      for(i in 1:(maxyr- input$ecreep_year)) {
        cpue_obj$ecreep[cpue_obj$yr==(input$ecreep_year+i)]  <- cpue_obj$CPUE[cpue_obj$yr==(input$ecreep_year+i)]*(1-input$ecreepslider/100)^i # equation for decay in %; first cpue without correction
        cpue_obj$CPUE_used=cpue_obj$ecreep } } else {
          cpue_obj$CPUE_used=cpue_obj$CPUE
        }
    ########## Smooth see cmsy
    nyr=length(cpue_obj$yr)
    d.cpue.raw   <- max(diff( cpue_obj$CPUE[!is.na(cpue_obj$CPUE)] )/ cpue_obj$CPUE[!is.na(cpue_obj$CPUE)][1:(nyr-1)],na.rm = T)
    
    # if(input$smoothcpue==T |d.cpue.raw > 1.5) {   
    if(input$smoothcpue==T) {
      #smooth.flag <- TRUE
      bw          <- input$bw_inpt#log(2)/exp(mean(log(final_rpriors()))) # use population doubling time as bandwidth
      #bw          <- ifelse(bw < 3,3,bw) # enforce minimum bandwidth of 3
      cpue_obj$CPUE_smthd<- ksmooth(x=cpue_obj$yr,y=cpue_obj$CPUE_used,kernel="normal",n.points=length(cpue_obj$yr),bandwidth=bw)$y
      cpue_obj$CPUE_used=cpue_obj$CPUE_smthd}
    return(cpue_obj)
  })
  
  
  d.cpue.raw=reactive({
    req(CPUE_obj())
    cpue_obj=CPUE_obj()
    nyr=length(cpue_obj$yr)
    d.cpue.raw_   <- max(diff( cpue_obj$CPUE[!is.na(cpue_obj$CPUE)] )/ cpue_obj$CPUE[!is.na(cpue_obj$CPUE)][1:(nyr-1)],na.rm = T)
    return(d.cpue.raw_)
  })
  
  
  observe({
    if (d.cpue.raw()>1.5) {
      output$smooth_recom=renderUI({ shiny::h5(HTML(paste0( "<font color=\"#cc0000\"><b>",tags$b("Smoothing is recommended for this stock!"))))})} else {
        output$smooth_recom=renderUI({ shiny::h5(HTML(paste0( "")))})
      }
  })

  output$Check_pdata_plot= shiny::renderPlot({
    validate(
      need(input$CPUE_yr_slct[1]<=input$ecreep_year, 'The start year for ecreep should be later than start year of the analysis.')
    )
    req(CPUE_obj())
    nyr=length(CPUE_obj()$yr)
    my_y_title <-bquote(atop(CPUE~"for"~the~stock~bold(.(Final_stock()[["CPUE_ID"]][1,"Stock"]))~of~italic(.(Final_stock()[["CPUE_ID"]][1,"ScientificName"]))))
    
    if ((input$ecreepyes==F | input$ecreepslider==0) & input$smoothcpue!=T ) {
      
      p_1=ggplot2::ggplot()+
        ggplot2::geom_line(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE,color="A"),size=1)+
        ggplot2::geom_point(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE),fill="#F8766D",shape=21,size=2)+
        # ggplot2::scale_y_continuous(limits = c(0,max(1.1*c(CPUE_obj()$CPUE,prior.kq[2]))))+
        ggplot2::theme_classic()+
        ggplot2::scale_color_manual(values=c("#F8766D"),labels=c("Raw CPUE"))+
        ggplot2::scale_fill_manual(values=c("#F8766D"),labels=c("Raw CPUE"))+
        ggplot2::labs(y="CPUE",x="Year",fill="",color="",title =my_y_title)+
        ggplot2::theme(legend.position="bottom")
    } else if ((input$ecreepyes==T | input$ecreepslider>0) & input$smoothcpue!=T  ){
      p_1=ggplot2::ggplot()+
        ggplot2::geom_line(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE,color="A"),size=1)+
        ggplot2::geom_point(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE),fill="#F8766D",shape=21,size=2)+
        #  ggplot2::scale_y_continuous(limits = c(0,max(1.1*c(CPUE_obj()$CPUE,CPUE_obj()$ecreep,prior.kq[2]),na.rm=T)))+
        ggplot2::geom_line(data=CPUE_obj(),ggplot2::aes(x=yr,y=ecreep,color="B"),linewidth=1)+
        ggplot2::geom_point(data=CPUE_obj(),ggplot2::aes(x=yr,y=ecreep),fill="#00BA38",shape=21,size=1.5)+
        ggplot2::geom_point(ggplot2::aes(x=input$ecreep_year,y=CPUE_obj()$CPUE_used[CPUE_obj()$yr==input$ecreep_year],shape="E-creep effect start year"),size=4)+
        ggplot2::theme_classic()+
        ggplot2::scale_color_manual(values=c("#F8766D","#00BA38"),labels=c("Raw CPUE","CPUE corrected for e-creep"))+
        ggplot2::scale_fill_manual(values=c("#F8766D","#00BA38"),labels=c("Raw CPUE","CPUE corrected for e-creep"))+
        ggplot2::labs(y="CPUE",x="Year",fill="",color="",shape="",title =my_y_title)+
        ggplot2::theme(legend.position="bottom")
      
    } else if ((input$ecreepyes==F | input$ecreepslider==0) & input$smoothcpue==T ){
      p_1=ggplot2::ggplot()+
        ggplot2::geom_line(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE,color="A"),size=1)+
        ggplot2::geom_point(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE),fill="#F8766D",shape=21,size=2)+
        #  ggplot2::scale_y_continuous(limits = c(0,max(1.1*c(CPUE_obj()$CPUE,CPUE_obj()$CPUE_smthd,prior.kq[2]),na.rm=T)))+
        ggplot2::geom_line(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE_smthd,color="B"),linewidth=1)+
        ggplot2::geom_point(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE_smthd),fill="#619CFF",shape=21,size=1.5)+
        ggplot2::theme_classic()+
        ggplot2::scale_color_manual(values=c("#F8766D","#619CFF"),labels=c("Raw CPUE","Smoothed"))+
        ggplot2::scale_fill_manual(values=c("#F8766D","#619CFF"),labels=c("Raw CPUE","Smoothed"))+
        ggplot2::labs(y="CPUE",x="Year",fill="",color="",title = my_y_title)+
        ggplot2::theme(legend.position="bottom")
      
    } else if ((input$ecreepyes==T | input$ecreepslider>0) & input$smoothcpue==T ){
      p_1=ggplot2::ggplot()+
        ggplot2::geom_line(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE,color="A"),size=1)+
        ggplot2::geom_point(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE),fill="#F8766D",shape=21,size=2)+
        #   ggplot2::scale_y_continuous(limits = c(0,max(1.1*c(CPUE_obj()$CPUE,CPUE_obj()$CPUE_used,prior.kq[2]),na.rm=T)))+
        ggplot2::geom_line(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE_used,color="B"),linewidth=1)+
        ggplot2::geom_point(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE_used),fill="#619CFF",shape=21,size=1.5)+
        ggplot2::geom_point(ggplot2::aes(x=input$ecreep_year,y=CPUE_obj()$CPUE_used[CPUE_obj()$yr==input$ecreep_year],shape="E-creep effect start year"),size=4)+
        ggplot2::theme_classic()+
        ggplot2::scale_color_manual(values=c("#F8766D","purple"),labels=c("Raw CPUE","Smoothed & corrected for e-creep"))+
        ggplot2::scale_fill_manual(values=c("#F8766D","purple"),labels=c("Raw CPUE","Smoothed & corrected for e-creep"))+
        ggplot2::labs(y="CPUE",x="Year",fill="",color="",shape="",title = my_y_title)+
        ggplot2::theme(legend.position="bottom")
    }
    print(p_1)
  }, height = 400)

  output$When_happy=shinydashboard::renderValueBox({
    shinydashboard::valueBox( shiny::h4("Message"),shiny::h4("When you are happy with data preparation, move on to the '3. Priors' tab. "),
                              icon = shiny::icon("envelope"),color = "green")})
  
  # ################################################################
  # ###################### PAGE 5 PRIOR ENTRY  #####################
  # ################################################################
  # 
  #########SELECT STOCK TO WORK WITH PRIORS
  observeEvent(input$button_1,{   #TRICK to erase things
    shinyWidgets::updatePrettySwitch(session, "Acc_FBpriors", value = F)
    shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
    shinyWidgets::updateMaterialSwitch(session, "go_to_d", value = F)
   # reset("FB_resil")
    })
  
  observeEvent(input$button_2,{   #TRICK to erase things
    shinyWidgets::updatePrettySwitch(session, "Acc_FBpriors", value = F)
    shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
    shinyWidgets::updateMaterialSwitch(session, "go_to_d", value = F)
   # reset("FB_resil")
     })
  

  observeEvent(input$procc_rpriors,{   #TRICK to erase things
    shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
  }) 

    output$Stock_infobox_patience=shinydashboard::renderValueBox({
    shinydashboard::valueBox( shiny::h4("Start priors processing"),shiny::h5("Press 'Connect' button to connect to FishBase or SeaLifeBase 
                                                                             for extraction of prior information. You will then have the 
                                                                             option to edit that information. Be patient since the 
                                                                             connection may take a moment. 
                                                                             Don't forget to press the 'Save the priors'
                                                                             button when you're done."),
                              icon = shiny::icon("envelope"),color = "green")})

  output$Stock_infobox_3=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Stock object"),shiny::h5(HTML(paste0("You have selected the stock ",
                                                      tags$b(Final_stock()[["CPUE_ID"]][1,"Stock"]), " of the species ",
                                                      tags$b(tags$em(Final_stock()[["CPUE_ID"]][1,"ScientificName"]))
      ))),
      icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "green") })
  # 

  
# 
  Fishbase_text=eventReactive(input$procc_rpriors,{
    req(Final_stock())
    results= fbsb(Final_stock()[["CPUE_ID"]][1,"ScientificName"])
    return(results)
  })
 
  # url_r=eventReactive(input$procc_rpriors,{
  #   url= a("Link", href=Fishbase_text()[6],style = "color:black") })

  output$FB_resil=shinydashboard::renderValueBox({
    if (is.na(Fishbase_text()[6])) {
      shinydashboard::valueBox(
        shiny::h4("FishBase/SeaLifeBase info"),"We couldn't find information in FishBase/SeaLifeBase. Check species name spelling or consider to search manually. If no information on resilience or r-range is available in FishBase/SeaLifeBase, using Medium resilience for 'regular' species and Low resilience for large or deep-water species is a reasonable starting point. Very low resilience is found in very large species and High resilience is found in species which reproduce in their first year.",
        icon = shiny::icon("info-circle"),
        color = "aqua")
    } else  if (!isTruthy(input$resilience_in)) {
      shinydashboard::valueBox(
        shiny::h4("FishBase info"),tagList(HTML(paste0("According to FishBase/SeaLifeBase, ", tags$em(Final_stock()[["CPUE_ID"]][1,"ScientificName"]), " has prior r range:", tags$b(Fishbase_text()[1]),tags$br(), "<font color=\"#cc0000\"><b>"," For more info visit: ",Fishbase_text()[6], "</b></font>",tags$br(), "Since no information on resilience is available in FishBase/SeaLifeBase, using Medium resilience for 'regular' species and Low resilience for large or deep-water species is a reasonable starting point. Very low resilience is found in very large species and High resilience is found in species which reproduce in their first year." ))),
        icon = shiny::icon("info-circle"),
        color = "aqua") } else {
          shinydashboard::valueBox(
            shiny::h4("FishBase info"),tagList(HTML(paste0("According to FishBase/SeaLifeBase, ", tags$em(Final_stock()[["CPUE_ID"]][1,"ScientificName"]), " has "," Resilience: ",
                                                           tags$b(Fishbase_text()[2]), " Prior r range:", tags$b(Fishbase_text()[1]),tags$br(), "<font color=\"#cc0000\"><b>"," For more info visit: ",Fishbase_text()[6], "</b></font>"))),
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
      color = "green") })
  
  final_rpriors=eventReactive(input$Acc_rpriors==T,{
    req(temp_rpriors())
    fprrs=temp_rpriors()
    fprrs=c(as.numeric(input$priors_rrange[1]),as.numeric(input$priors_rrange[2]))
    return(fprrs)
  })
  
  
  
###########################
  ########################## Adding priors pre selecting
  
  LBB_data <- reactive({
    req(input$file_LBB)
    df <- read.csv(input$file_LBB$datapath,
                   header =T,
                   sep = ","#,
    ) #quote = input$quote
    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round,2)
   
    return(df)
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
                ebk_stat_low=input$man_Bk_start[1]
                ebk_stat_high= input$man_Bk_start[2]}
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
                ebk_stat_low=input$man_Bk_int[1]
                ebk_stat_high= input$man_Bk_int[2]}
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
                ebk_end_low=input$man_Bk_end[1]
                ebk_end_high= input$man_Bk_end[2]}
    expert_bk_end_priors=c(expert_bk_end_,ebk_end_low,ebk_end_high)
  })
  
  observe({
    updateSliderInput(session, "man_Bk_start", value = c( as.numeric(temp_expert_bk_start()[2]),  as.numeric(temp_expert_bk_start()[3])),
                      min = 0, max =1)
    })
  
  observe({
    updateSliderInput(session, "man_Bk_int", value = c(   as.numeric(temp_expert_bk_ind()[2]),  as.numeric(temp_expert_bk_ind()[3])),
                      min = 0, max =1)
  })
  
  observe({
    updateSliderInput(session, "man_Bk_end", value = c(   as.numeric(temp_expert_bk_end()[2]),  as.numeric(temp_expert_bk_end()[3])),
                      min = 0, max =1)
  })
  
  observe({
    updateSliderInput(session, "man_Bk_int_year", value =as.integer(floor((CPUE_obj()[c("yr","CPUE_used")]$yr[which(CPUE_obj()[c("yr","CPUE_used")]$CPUE_used>0)[1]]+max(as.integer(CPUE_obj()$yr),na.rm = T))/2)),min = min(CPUE_obj()$yr), max =max(CPUE_obj()$yr))
      })
  
  observe({
    start.yr =CPUE_obj()[c("yr","CPUE_used")]$yr[which(CPUE_obj()[c("yr","CPUE_used")]$CPUE_used>0)[1]]
    end.yr    =max(as.integer(CPUE_obj()$yr),na.rm = T)
    updateSliderInput(session, "bk_priors_year", value =start.yr,min =start.yr, max =end.yr)
  })
  
  #LBB_data <- LBB_data %>% debounce(1000)

  output$Check_priors_plot= shiny::renderPlot({
    validate(
      need(isTruthy(input$resilience_in), "You han't set resilience for the stock neither FishBase/SeaLifeBase provided this information. Press 'Change r prior' and select a 'resilience' option from the dropdown list to be able to continue.")
    )
    req(CPUE_obj())
      start_yr =CPUE_obj()[c("yr","CPUE_used")]$yr[which(CPUE_obj()[c("yr","CPUE_used")]$CPUE_used>0)[1]]
    end_yr    =max(as.integer(CPUE_obj()$yr),na.rm = T)
    
     start=input$man_Bk_start
     ind=input$man_Bk_int
    end=input$man_Bk_end
    
temp=data.frame(yr=c(start_yr,input$man_Bk_int_year,end_yr),ln=c(mean(start),mean(ind),mean(end)))

    if (!isTruthy(input$file_LBB)) {
      p2= ggplot2::ggplot() +
        ggplot2::geom_errorbar(aes(x=start_yr, ymin=start[1], ymax=start[2]),color="blue",  size=1)+
        ggplot2::geom_errorbar(aes(x=input$man_Bk_int_year, ymin=ind[1], ymax=ind[2]),color="blue",  size=1)+
        ggplot2::geom_errorbar(aes(x=end_yr, ymin=end[1], ymax=end[2]),color="blue", size=1)+
        #  ggplot2::scale_x_continuous(breaks=seq(start_yr,end_yr,2))+
        ggplot2::scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0,1.1))+
        ggplot2::geom_hline( ggplot2::aes(yintercept=0.5,color = "Bmsy"),linetype="dashed")+
        ggplot2::geom_line(data=temp, ggplot2::aes(x=yr,y=ln),color="blue",size=1)+
        scale_color_manual(values=c("black"),labels=c("Bmsy"))+
        ggplot2::labs(y="B/k", x="Year",color="")+
        ggplot2::theme_classic()+
        ggplot2::theme(text = ggplot2::element_text(size = 16))   
    } else {
      LBB_data_=as.data.frame(LBB_data())
      
      LBB_data_$BK.ucl[LBB_data_$BK.ucl>1]=1
      LBB_data_$Bk.lcl[LBB_data_$Bk.lcl<0]=0
      p2=ggplot2::ggplot() +
        geom_line(data=LBB_data_,ggplot2::aes(x=Year, y =Bk,color="A"),linetype="dashed")+
        geom_point(data=LBB_data_,ggplot2::aes(x=Year, y =Bk,color="A"))+
      geom_ribbon(data=LBB_data_,ggplot2::aes(x=Year, ymin =Bk.lcl, ymax =BK.ucl,color="A",fill = "A"),linetype="dashed", alpha=0.3)+
        ggplot2::geom_errorbar(aes(x=start_yr, ymin=start[1], ymax=start[2]),color="blue",  size=1)+
        ggplot2::geom_errorbar(aes(x=input$man_Bk_int_year, ymin=ind[1], ymax=ind[2]),color="blue",size=1)+
        ggplot2::geom_errorbar(aes(x=end_yr, ymin=end[1], ymax=end[2]),color="blue", size=1)+
        ggplot2::geom_hline( ggplot2::aes(yintercept=0.5,linetype = "Bmsy"),color="black")+
        #  ggplot2::scale_x_continuous(breaks=seq(start_yr,end_yr,2))+
        ggplot2::scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0,1.1))+
        ggplot2::scale_color_manual(values=c("green"),labels=c("LBB B/k priors"))+
        ggplot2::scale_linetype_manual(values=c("dashed"),labels=c("Bmsy"))+
        ggplot2::scale_fill_manual(values=c("green"),labels=c("LBB B/k priors"))+
        ggplot2::geom_line(data=temp, ggplot2::aes(x=yr,y=ln),color="blue",size=1)+
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
 

 bk_priors_used=reactive({
   bkpr=input$bk_priors
   if (input$manual_bk_priors==F & bkpr!="")  {
     if (bkpr== "Unexploited, 0.75-1.0"){
       bkprlow=0.75
       bkprhigh=1} else if (bkpr=="Sustainable, 0.4-0.8"){
         bkprlow=0.4
         bkprhigh=0.8} else if (bkpr=="Medium, 0.2-0.6"){
           bkprlow=0.2
           bkprhigh=0.6} else if (bkpr=="Low, 0.01-0.4"){
             bkprlow=0.01
             bkprhigh=0.4} else if (bkpr=="Very low, 0.01-0.2"){
               bkprlow=0.01
               bkprhigh=0.2
             }} else {
               bkprlow=input$bk_priors_rrange[1]
               bkprhigh= input$bk_priors_rrange[2]}
   frpriors=c(bkpr,bkprlow,bkprhigh)
 })
 
 
 Deside_on_Bkpriors=reactive({
   req(CPUE_obj())
   if (input$Id004=="Select one of the B/k priors") {
     if(input$select_123=="First") {
       ffpriors=c(input$man_Bk_start,min(CPUE_obj()$yr))} else if (input$select_123=="Second") {
         ffpriors=c(input$man_Bk_int,input$man_Bk_int_year)} else if (input$select_123=="Third") {
           ffpriors=c(input$man_Bk_end,max(CPUE_obj()$yr))}
   } else  {
     ffpriors=c(as.numeric(bk_priors_used()[c(2,3)]),input$bk_priors_year)
       }
       return(ffpriors)
   })
 
# Deside_on_Bkpriors <- Deside_on_Bkpriors %>% debounce(1000)

 ######## OLD, the mou, method
 prior.kq=reactive({
   req(CPUE_obj())
   req(Deside_on_Bkpriors())
   req(final_rpriors())
   cdat_=CPUE_obj()
   prior.r=final_rpriors()
   Bk.yr.i=Deside_on_Bkpriors()[3]
   max.cpue     <- sort(cdat_$CPUE_used[!is.na(cdat_$CPUE_used)])[length(cdat_$CPUE_used[!is.na(cdat_$CPUE_used)])-1]

   prior.Bk=as.numeric(Deside_on_Bkpriors()[1:2])
   # us relative range of B/k as relative range for kq
   mean.prior.Bk   <- mean(prior.Bk)
   rr.prior.Bk     <- (mean.prior.Bk-prior.Bk[1])/mean.prior.Bk

   prior.kq.low.1  <- (1-rr.prior.Bk)*cdat_$CPUE_used[cdat_$yr==Bk.yr.i]/mean.prior.Bk
   prior.kq.hi.1   <- (1+rr.prior.Bk)*cdat_$CPUE_used[cdat_$yr==Bk.yr.i]/mean.prior.Bk

   # mean.log.r=mean(log(prior.r))
   # sd.log.r=(log(prior.r[2])-log(prior.r[1]))/4  # assume range covers 4 SD

   # kq must be > max cpue unless near unexploited
   prior.kq.low.2  <- ifelse(prior.kq.low.1 < max.cpue,ifelse(mean.prior.Bk >= 0.85,0.9*max.cpue,max.cpue),prior.kq.low.1)
   # increase lower kq prior if cpue is small and flat
   if((max(cdat_$CPUE_used[!is.na(cdat_$CPUE_used)])/min(cdat_$CPUE_used[!is.na(cdat_$CPUE_used)]))<2) {
     prior.kq.low <- ifelse(mean.prior.Bk < 0.3,2*prior.kq.low.2,
                            ifelse(mean.prior.Bk < 0.6,1.5*prior.kq.low.2,prior.kq.low.2))
   } else {prior.kq.low <- prior.kq.low.2 }

   # kq.hi at least 30-50% larger than kq.low, depending on Bk prior
   if(mean.prior.Bk >= 0.6) {
     prior.kq.hi.2  <- ifelse(prior.kq.hi.1 < (1.3*prior.kq.low),1.3*prior.kq.low,prior.kq.hi.1) } else {
       prior.kq.hi.2  <- ifelse(prior.kq.hi.1 < (1.5*prior.kq.low),1.5*prior.kq.low,prior.kq.hi.1) }

   # if upper prior kq is too hi, limit to 3 times lower range
   prior.kq.hi   <- ifelse(prior.kq.hi.2 > (3*prior.kq.low),3*prior.kq.low,prior.kq.hi.2)
   prior.kq_          <- c(prior.kq.low,prior.kq.hi,prior.kq.low.1,prior.kq.hi.1)

 })

 mvnlogrk=eventReactive(input$Save_priors==T,{
   n.p          <- 50000 # number of r-kq pairs to be analysed; will be doubled if too few viable pairs are found
   n.trial      <- 30 # times each year is calculated with new random error terms for r and cpue
   req(prior.kq())
   req(final_rpriors())
   
    prior.r=final_rpriors()
    prior.kq=prior.kq()[c(1,2)]
    mean.log.r=mean(log(prior.r))
    sd.log.r=(log(prior.r[2])-log(prior.r[1]))/4  # assume range covers 4 SD
    #------------------------------------------------------------------
 # Sampling of r-k space
 #------------------------------------------------------------------
 # turn numerical ranges into log-normal distributions
 
 mean.log.kq <- mean(log(prior.kq))
 sd.log.kq   <- (log(prior.kq[2])-log(prior.kq[1]))/4 # assume range covers 4 SD
 
 mvn.log.rk <- mvn(n=n.p,mean.log.r=mean.log.r,sd.log.r=sd.log.r,mean.log.kq=mean.log.kq,sd.log.kq=sd.log.kq)
 # ri1    <- exp(mvn.log.rk[,1])    #############################perito!!!!!!!!!!!!!!!!!!!!!!!?
 # kqi1   <- exp(mvn.log.rk[,2])     #############################perito!!!!!!!!!!!!!!!!!!!!!!!?
 return(mvn.log.rk)
 })

 output$CPUE_MAIN_PLOT= shiny::renderPlot({
   validate(
     need(input$CPUE_yr_slct[1]<=input$bk_priors_year, 'The year on which B/k priors apply should be later than start year of the analysis.')
   )
   
   validate(
     need(isTruthy(input$resilience_in), "You han't set resilience for the stock neither FishBase/SeaLifeBase provided this information. Press 'Change r prior' and select a 'resilience' option from the dropdown list to be able to continue.")
   )
   
   
req(CPUE_obj())
   req(Final_stock())
req( bk_priors_used())
req( Deside_on_Bkpriors())
req(prior.kq())
   prior.kq=prior.kq()[c(1,2)]
    prior.kq.low=prior.kq[1]
    prior.kq.hi=prior.kq[2]
    nyr=length(CPUE_obj()$yr)
#d.cpue.raw   <- max(diff( CPUE_obj()$CPUE[!is.na(CPUE_obj()$CPUE)] )/ CPUE_obj()$CPUE[!is.na(CPUE_obj()$CPUE)][1:(nyr-1)],na.rm = T)

my_y_title <-bquote(atop(CPUE~and~expert~priors~"for"~the~stock~bold(.(Final_stock()[["CPUE_ID"]][1,"Stock"]))~of~italic(.(Final_stock()[["CPUE_ID"]][1,"ScientificName"]))))

start_yr =min(as.integer(CPUE_obj()$yr),na.rm = T)
end_yr    =max(as.integer(CPUE_obj()$yr),na.rm = T)

if (input$Id004=="Select one of the B/k priors") {
  if(input$select_123=="First") {
    start_f=mean(input$man_Bk_start)
    div_f=CPUE_obj()$CPUE_used[1]/start_f
    } else if (input$select_123=="Second") {
      start_f=mean(input$man_Bk_int)
      div_f=CPUE_obj()$CPUE_used[CPUE_obj()$yr==Deside_on_Bkpriors()[3]]/start_f} else if (input$select_123=="Third") {
        start_f=mean(input$man_Bk_end)
        div_f=CPUE_obj()$CPUE_used[CPUE_obj()$yr==Deside_on_Bkpriors()[3]]/start_f}
} else  {
  start_f=mean( Deside_on_Bkpriors()[c(1,2)])
  div_f=CPUE_obj()$CPUE_used[CPUE_obj()$yr==Deside_on_Bkpriors()[3]]/start_f
}

start=div_f*input$man_Bk_start
ind=input$man_Bk_int*div_f
end=input$man_Bk_end*div_f

desided= Deside_on_Bkpriors()[c(1,2)]*div_f
temp=data.frame(yr=c(start_yr,input$man_Bk_int_year,end_yr),ln=c(mean(start),mean(ind),mean(end)))
max.y=max(1.05*c(CPUE_obj()$CPUE_used,start,ind,end),na.rm = T)

   p_1=ggplot2::ggplot()+
    ggplot2::geom_line(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE_used,color="A"),size=1)+
    ggplot2::geom_point(data=CPUE_obj(),ggplot2::aes(x=yr,y=CPUE_used),fill="#F8766D",shape=21,size=2)+
     ggplot2::geom_errorbar(aes(x=start_yr, ymin=start[1], ymax=start[2],color="B"),  size=1)+
     ggplot2::geom_errorbar(aes(x=input$man_Bk_int_year, ymin=ind[1], ymax=ind[2],color="B"), size=1)+
     ggplot2::geom_errorbar(aes(x=end_yr, ymin=end[1], ymax=end[2],color="B"),  size=1)+
     ggplot2::geom_line(data=temp, ggplot2::aes(x=yr,y=ln),color="blue",size=1)+
     ggplot2::geom_errorbar(aes(x=Deside_on_Bkpriors()[3], ymin=desided[1], ymax=desided[2],color="C"), size=1)+
    ggplot2::theme_classic()+
    ggplot2::scale_color_manual(values=c("#F8766D","blue","green"),labels=c("CPUE","Expert priors","Selected B/k prior"))+
   # ggplot2::scale_fill_manual(values=c("#F8766D"),labels=c("CPUE","k prior range for selected year","Bmsy prior range"))+
    ggplot2::scale_y_continuous(limits = c(0,max.y))+
     ggplot2::labs(y="CPUE",x="Year",fill="",color="",title =my_y_title)+
    ggplot2::theme(legend.position="bottom")
  
   print(p_1)
 }, height = 400)

 output$param_sofar <-renderText({paste("Parameterization so far for the stock",  tags$b(Final_stock()[["CPUE_ID"]][1,"Stock"]), "of",  tags$em(Final_stock()[["CPUE_ID"]][1,"ScientificName"]))
 })
 
 
 
 AMSY_object=eventReactive(input$Save_priors,{
   req(CPUE_obj())
   req(Final_stock())
   req(Fishbase_text())

   FBSLB_info_r=gsub(",","-",Fishbase_text()[1])
   FBSLB_info_r=gsub(";",".",FBSLB_info_r)
   
   FBSLB_info_Resilience=gsub(",","-",Fishbase_text()[2])
   FBSLB_info_Resilience=gsub(";",".",FBSLB_info_Resilience)
   
   AMSY.object=list()
   AMSY.object[["Data"]]=CPUE_obj()
   AMSY.object[["ID"]]=data.frame(
     Stock=Final_stock()[["CPUE_ID"]][1,"Stock"],
     Species=Final_stock()[["CPUE_ID"]][1,"ScientificName"],
     Common_name=Final_stock()[["CPUE_ID"]][1,"Name"],
     Comments=Final_stock()[["CPUE_ID"]][1,"EnglishName"],
     Start_yr=input$CPUE_yr_slct[1],
     End_yr=input$CPUE_yr_slct[2],
     Ecreep_YN=input$ecreepyes,
     Start_yr_ecreep=input$ecreep_year,
     Ecreep=input$ecreepslider,
     Smooth_YN=input$smoothcpue,
     Smooth_bw=input$bw_inpt,
     Resilience=input$resilience_in,
     r.low=as.numeric(final_rpriors()[1]),
     r.hi=as.numeric(final_rpriors()[2]),
     FBSLB_info_r=FBSLB_info_r,
     FBSLB_info_Resilience=FBSLB_info_Resilience,
     FBSLB_page=Fishbase_text()[6],
     bk_prior_yr=Deside_on_Bkpriors()[3],
     bk_prior_low=Deside_on_Bkpriors()[1],
     bk_prior_high=Deside_on_Bkpriors()[2],
     prior.kq.low=prior.kq()[1],
     prior.kq.hi=prior.kq()[2]
   )
   AMSY.object[["mvnlogrk"]]=mvnlogrk()
   

return(AMSY.object)
  })
 
 
 
 
 
 middle_outplts=eventReactive(input$Save_priors==T,{
   req(AMSY_object())
   
   AMSY.object=AMSY_object()
   
   outpts_m=data.frame(value=c(paste0(AMSY.object[["ID"]]$Start_yr, "-", AMSY.object[["ID"]]$End_yr),
                               paste0(format(round(min(AMSY.object[["Data"]]$CPUE_used),2),digits = 3), "-", format(round(max(AMSY.object[["Data"]]$CPUE_used),2),digits = 3)),
                               AMSY.object[["ID"]]$Smooth_YN,
                               paste0(input$ecreepyes, ifelse(input$ecreepyes==T, paste(", value=", AMSY.object[["ID"]]$Ecreep, "%"),"")),
                               paste0(format(AMSY.object[["ID"]]$Resilience,digits = 3)),
                               paste0(format(AMSY.object[["ID"]]$r.low,digits = 3), "-",format(AMSY.object[["ID"]]$r.hi,digits = 3)),
                               paste0(format(round(quantile(exp(AMSY.object[["mvnlogrk"]][,1]),0.01),2),digits = 3),"-",format(round(quantile(exp(AMSY.object[["mvnlogrk"]][,1]),0.99),2),digits = 3)),
                               paste0(format(2*as.numeric(AMSY.object[["ID"]]$bk_prior_low),digits = 3),"-",format(2*as.numeric( AMSY.object[["ID"]]$bk_prior_high),digits = 3)),
                               # paste0(format(bk_priors_used()[1],digits = 3)),
                               paste0(format(AMSY.object[["ID"]]$bk_prior_low,digits = 3),"-",format(AMSY.object[["ID"]]$bk_prior_high,digits = 3)),
                               paste0(format(round(AMSY.object[["ID"]]$prior.kq.low,2),digits = 3),"-",format(round( AMSY.object[["ID"]]$prior.kq.hi,2),digits = 3))))
   
   row.names(outpts_m)=c("CPUE data for years","CPUE range",
                         "Smooth","Ecreep",
                         "Resilience", "Prior for r",
                         "Multivariate prior range for r",
                         paste0("B/Bmsy prior for ",  AMSY.object[["ID"]]$bk_prior_yr),
                         paste0("B/k prior for ",  AMSY.object[["ID"]]$bk_prior_yr),
                         #paste0("Prior for ",Deside_on_Bkpriors()[3]," stock status"),
                         "Prior range for k (in cpue units)")
   return(outpts_m) 
   
 })
 
 output$middle_outputs <-renderPrint({middle_outplts()
 })
 
#######   
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
   updateTextInput(session, "obj.name",value=paste0(Final_stock()[["CPUE_ID"]]["Stock"], "_V1") ) #, placeholder=paste0("e.g.  ",Final_stock()[["Catch_ID"]]$Stock, "_V1")
 })
 
 object_NAME=reactive({
   req(AMSY_object())
   if (input$obj.name=="") {
     name=paste0(AMSY_object()[["ID"]]$Stock, "_V1")
   } else {
     name=input$obj.name
   }
   return(name)
 })
 
 
 observeEvent(input$Save_priors, {
   req(object_NAME())
   AMSY.object <- AMSY_object()
   dir.create( paste0(dir.name(),"/AMSY"))
   dir=paste0(paste0(dir.name(),"/AMSY"))
   save(AMSY.object,file =paste0(dir,"/", object_NAME(), ".RData"))
   Save_done <- showNotification(paste("Message:",  "The stock object with the input parameterization has been saved in ", paste0(dir.name(),"/AMSY/",object_NAME(), ".RData")), duration = 10)
 })
 
 objects <- reactiveVal(data.frame(A.A=rep(NA,100),Created_Stocks=rep(NA,100)))
 count <- reactiveVal(0)
 
 observeEvent(input$Save_priors, {
   count(count() + 1)
 })
 
 observeEvent(input$Save_priors,{
   #start with current data
   objects() %>%
     add_row(
       A.A = isolate(as.integer(count())),
       Created_Stocks = isolate(object_NAME())
     ) %>%
     # update data value
     objects()
   # objects()=rbind( objects(),data.frame( A.A = isolate(as.integer(count())),
   #                                        Created_Stocks = isolate(object_NAME())))
 }
 )
 
 output$created_objects <- renderUI({
   renderTable({as.data.frame( objects()[!is.na( objects()$A.A),])})
 })
 
 
 output$select_diff_obj <- renderUI({
   filenames <- list.files(paste0(dir.name(),"/AMSY"), pattern="*.RData", full.names=F)
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
     sdsd=AMSY_object() } else if (input$Id049=="B") {
       asa=paste0(dir.name(),"/AMSY/",input$Id081,".RData")
       load(asa)
       sdsd=AMSY.object
     }
   return(sdsd)
 })
 
 fstc <- fstc %>% debounce(500)
 
 
 observeEvent(input$see_obj,{
   req(fstc())
   xxx=as.data.frame(t(fstc()[["ID"]]))
   xxx$parameter=row.names(xxx)
   colnames(xxx)[1]="value"
   xxx=xxx[,c(2,1)]
   output$see_object_table=renderTable(xxx)
 })
 
 observe({
   output$Zavarakatranemia=renderUI({ shiny::h4(tagList("Run the model and get the results for the stock ",  tags$b(fstc()[["ID"]]$Stock), "of",  tags$em(fstc()[["ID"]]$Species)))})
 })
 ######
   
   
   
   observeEvent(input$Save_priors,{
     output$Zavarakatranemia=renderUI({ shiny::h3(tagList("Run the model and get the results for the stock ",  tags$b(Final_stock()[["CPUE_ID"]][1,"Stock"]), "of",  tags$em(Final_stock()[["CPUE_ID"]][1,"ScientificName"])))})
   })
 
  output$Run_infobox_patient=shinydashboard::renderValueBox({
    shinydashboard::valueBox( shiny::h4("Run the model"),shiny::h5("Press 'Start' button to run the model.
              It takes some time to do all the necessary calculations, so be patient!"),
                              icon = shiny::icon("envelope"),
                              color = "green")
  })

  
    SchaeferCPUE_run=eventReactive(input$Start_run,{
    req(fstc())
    amsyrun=AMSY.run(fstc())
  })

    SchaeferCPUE_products_A=eventReactive(input$Start_run,{
      req(fstc())
      req(SchaeferCPUE_run())
      prds=AMSY.products(fstc(),SchaeferCPUE_run())
    })
  
    observeEvent(input$Start_run,{   #SEE AFTER 5
      req(SchaeferCPUE_products_A())
      start.yr =SchaeferCPUE_products_A()[["ID"]][["Start_yr"]]
      end.yr    =SchaeferCPUE_products_A()[["ID"]][["End_yr"]]
      nyr=end.yr-start.yr+1
      nyr_mid=ceiling(nyr/2)
      #updateSliderInput(session, "bk_priors_year", value =start.yr,min =start.yr, max =end.yr)
      updateSliderInput(session, "retro_range", value =ifelse(nyr_mid<3,nyr_mid,3),  max =nyr_mid) 
    })
    
     
      run_pictures <- reactiveValues()

      
    observeEvent(input$Start_run,{
      run_pictures$pic_A=ggplot.mvr(SchaeferCPUE_products_A(),SchaeferCPUE_run()) 
      pic_A_ready <- showNotification(paste("Message: ", "r-k paired graph ready"), duration = 5)
    })
    
    output$rk_space= shiny::renderPlot({
      run_pictures$pic_A})#,height = 230
    
    
    
    outpts=eventReactive(input$Start_run,{
      req(SchaeferCPUE_products_A())
      
      outpts_=data.frame(value=c(as.character(format(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$n.v,digits=3)),
                                 as.character(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$kqv.est,2),digits=3)),
                                 as.character(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$MSYq.est,2),digits=3)),
                                 as.character(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$rv.est,2),digits=3)),
                                 as.character(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$rv.est/2,2),digits=3)),
                                 as.character(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$FFmsy.end,2),digits=3)),
                                 as.character(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$BBmsy.end,2),digits=3))),
                         range=c("",
                                 paste0(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$kqv.lcl,2),digits=3),"-",format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$kqv.ucl,2),digits=3)),
                                 paste0(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$MSYq.lcl,2),digits=3),"-",format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$MSYq.ucl,2),digits=3)),
                                 paste0(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$rv.lcl,2),digits=3),"-",format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$rv.ucl,2),digits=3)),
                                 paste0(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$rv.lcl/2,2),digits=3),"-",format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$rv.ucl/2,2),digits=3)),
                                 paste0(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$FFmsy.end.lcl,2),digits=3),"-",format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$FFmsy.end.ucl,2),digits=3)),
                                 paste0(format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$BBmsy.end.lcl,2),digits=3),"-",format(round(SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]$BBmsy.end.ucl,2),digits=3))
                         ),
                         year=c("","(in cpue units)","(in cpue units)","","",paste0("(",as.character(SchaeferCPUE_products_A()[["Data"]]$yr[length(SchaeferCPUE_products_A()[["Data"]]$yr)-1]),")"),paste0("(",as.character(SchaeferCPUE_products_A()[["Data"]]$yr[length(SchaeferCPUE_products_A()[["Data"]]$yr)]),")"))
      ) 
      row.names(outpts_)=c("viable r-k pairs","median k", "median MSY","r (4 MSY/k)","Fmsy (r/2)","F/Fmsy","B/Bmsy")
      colnames(outpts_)=c("value","plausible 95% confidence limits","")
      return(outpts_) 
    })
  
  
  output$present_outputs <-renderPrint({outpts()
  })
  

  
  output$stock_species <-renderText({paste("Analysis for the stock",  tags$b(SchaeferCPUE_products_A()[["ID"]][["Stock"]]), "of",  tags$em(SchaeferCPUE_products_A()[["ID"]][["Species"]]))
  })

  
  observeEvent(input$Start_run,{
    req(SchaeferCPUE_products_A())
    run_pictures$pic_B= ggplot.catch(SchaeferCPUE_products_A())
  })
  
  output$Catch_outp= shiny::renderPlot({
    run_pictures$pic_B
  })#,height = 300
  

  observeEvent(input$Start_run,{
    req(SchaeferCPUE_products_A())
    run_pictures$pic_C= ggplot.FFmsy(SchaeferCPUE_products_A())
    
  })
  
  output$FFmsy_outp= shiny::renderPlot({
    run_pictures$pic_C
  })#,height = 300
  
 
  observeEvent(input$Start_run,{
    req(SchaeferCPUE_products_A())
    run_pictures$pic_D= ggplot.BBmsy(SchaeferCPUE_products_A())
  })
  
  output$BBmsy_outp= shiny::renderPlot({
    run_pictures$pic_D
  })#,height = 300
  
 
  
  observeEvent(input$Start_run,{
    req(SchaeferCPUE_products_A())
    run_pictures$pic_E=ggplot.kobe(SchaeferCPUE_products_A())
  })
  
  output$kobe_plot= shiny::renderPlot({
    run_pictures$pic_E
  })
  
  
  
  observeEvent(input$Start_run, {
    for (i in 1:100) {
      shinyWidgets::updateProgressBar(
        session = session,
        id = "prog_bar",
        value = i, total = 100,
        title = paste("Process", trunc(i/10)))
      Sys.sleep(0.1)
    }
  })
 
 
  
  Schaefer_prod_retro=eventReactive(input$Retrospective,{
    req(SchaeferCPUE_products_A())
    xx= AMSY_retro(SchaeferCPUE_products_A(),input$retro_range)
      
  })
  
  observeEvent(input$Retrospective,{
    req(Schaefer_prod_retro())
    run_pictures$pic_F=ggplot.retro(Schaefer_prod_retro())
  })
  
  output$retro_pics= shiny::renderPlot({
    run_pictures$pic_F
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
    req(SchaeferCPUE_products_A())
    req( run_pictures$pic_A)
    
    AMSY_object <- SchaeferCPUE_products_A()
    if (input$Id049=="A") {
      nm=object_NAME()} else if (input$Id049=="B") {
        nm=input$Id081}
    
    dir.create(paste0(dir.name(),"/AMSY/outputs"))
    dir.create(paste0(dir.name(),"/AMSY/outputs/",nm))
    device_="tiff"
    
      cdat_=SchaeferCPUE_products_A()[["Data"]]
      yr=cdat_$yr
      nyr=length(yr)

      cqt_=SchaeferCPUE_products_A()[["Outputs"]][["cqt_"]]
      outp=SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]

      ribb_min1=cqt_$cqt.lcl[2:(nyr-1)]/outp$MSYq.est
      obj_1_ribbon=data.frame(Year=yr[2:(nyr-1)],Catch_MSY_low = ribb_min1,Catch_MSY_high = cqt_$cqt.ucl[2:(nyr-1)]/outp$MSYq.est)
      obj_1_data=data.frame(Year=yr[1:(nyr-1)],Catch_MSY = cqt_$cqt.median[1:(nyr-1)]/outp$MSYq.est)

      FFmsy_=SchaeferCPUE_products_A()[["Outputs"]][["FFmsy_"]]
      max.y <- max(c(1.2,FFmsy_$FFmsy.ucl),na.rm=T)
      ribb_min2=FFmsy_$FFmsy.lcl[2:(nyr-1)]

      obj_2_ribbon=data.frame(Year=yr[2:(nyr-1)],FFmsy_low = ribb_min2,FFmsy_high = FFmsy_$FFmsy.ucl[2:(nyr-1)])
      obj_2_data=data.frame(Year=yr[1:(nyr-1)],FFmsy = FFmsy_$FFmsy)

      cpuet_=SchaeferCPUE_products_A()[["Outputs"]][["cpuet_"]]
      outp=SchaeferCPUE_products_A()[["Outputs"]][["outp_1"]]

      Bkt        <- cpuet_$cpuet.median/(0.5*outp$kqv.est)
      ribb_min3=cpuet_$cpuet.lcl[2:nyr]/(0.5*outp$kqv.est)
      obj_3_ribbon=data.frame(Year=yr[2:(nyr)],BBmsy_low = ribb_min3,BBmsy_high = cpuet_$cpuet.ucl[2:nyr]/(0.5*outp$kqv.est))
      obj_3_data=data.frame(Year=yr,BBmsy = Bkt)
      df_list <- list(obj_1_ribbon, obj_1_data, obj_2_ribbon,obj_2_data,obj_3_ribbon,obj_3_data)
      #merge all data frames together
      df_fin=Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
      df_fin=cbind(df_fin,CPUE_obj()[,2:ncol(CPUE_obj())])
      df_fin=df_fin[,c("Year","Stock","CPUE","CPUE_smthd",	"ecreep",	"CPUE_used","Catch_MSY","Catch_MSY_low",	"Catch_MSY_high","FFmsy","FFmsy_low",	"FFmsy_high","BBmsy",	"BBmsy_low",	"BBmsy_high")]
     colnames(df_fin)=c("Year","Stock","CPUE_Raw","CPUE_smoothed",	"ecreep",	"CPUE_used_in_analysis","Catch_MSY","Catch_MSY_low",	"Catch_MSY_high","FFmsy","FFmsy_low",	"FFmsy_high","BBmsy",	"BBmsy_low",	"BBmsy_high")
     
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","rk_pic."),device_),plot=run_pictures$pic_A, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","Relative_Catch."),device_),plot=run_pictures$pic_B, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","FFmsy_plot."),device_),plot=run_pictures$pic_C, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","BBmsy_plot."),device_),plot=run_pictures$pic_D, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","Kobe_plot."),device_),plot=run_pictures$pic_E, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    # ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","Kobe_plot."),device_),plot=run_pictures$pic_F, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    # ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","Management_graphs."),device_),plot=run_pictures$pic_G, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    # ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","prior_posterior_distributions."),device_),plot=run_pictures$pic_H, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    save(AMSY_object,  file =paste0(dir.name(),"/AMSY/outputs/",nm,"/","AMSY_obj_",gsub(" ", "_",AMSY_object[["ID"]][["Stock"]]), "_",Sys.Date(), ".RData"))
    write.csv(AMSY_object[["Data"]], paste0(dir.name(),"/AMSY/outputs/",nm,"/input_timeseries.csv"), row.names = TRUE)
    write.csv(AMSY_object[["Outputs"]][["outp_1"]], paste0(dir.name(),"/AMSY/outputs/",nm,"/outputs.csv"), row.names = TRUE)
    write.csv(df_fin, paste0(dir.name(),"/AMSY/outputs/",nm,"/output_timeseries.csv"), row.names = TRUE)
    # write.csv(AMSY_object[["Outputs"]][["cpuet_"]], paste0(dir.name(),"/AMSY/outputs/",nm,"/cpuet.csv"), row.names = TRUE)
    # write.csv(AMSY_object[["Outputs"]][["cqt_"]], paste0(dir.name(),"/AMSY/outputs/",nm,"/cqt.csv"), row.names = TRUE)

    Save_done <- showNotification(paste("Message: ", "All the outcomes are saved in your working directory"), duration = 10)
    
  })
  
  observeEvent(input$Retrospective, {
    req( run_pictures$pic_F)
    if (input$Id049=="A") {
      nm=object_NAME()} else if (input$Id049=="B") {
        nm=input$Id081}
    device_="tiff"
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","retrospective_pic."),device_),plot=run_pictures$pic_F, device =device_, width = 18, height =9, units = "cm",  dpi = 300)
    Save_done <- showNotification(paste("Message: ", "Retrospective outcomes are saved in your working directory"), duration = 5)
  })
 
  
  
  observe({
    req(SchaeferCPUE_products_A())
    if (input$Id049=="A") {
      nm=object_NAME()} else if (input$Id049=="B") {
        nm=input$Id081}
    output$working_directory=renderUI({ shiny::h4(tagList("All your outcomes are stored in the directory: ",  tags$b(paste0(dir.name(),"/AMSY/outputs/",nm))))})
  })
  
   
  output$download_pic_A <- downloadHandler(
    filename = function(){
      paste("pic_",input$Run_select_pic,Sys.Date(),".", input$format_picA,sep="")},
    content = function(file) {
      ggplot2::ggsave(file,plot=run_pictures[[paste0("pic_",input$Run_select_pic)]],
                      device =  input$format_picA, width =  input$width_pic_A, height = input$height_pic_A, units = "cm",  dpi = as.integer(input$dpi_pic_A))
  })
  

observe({
  if (input$quit == 1) stopApp()
})

}

shinyApp(shinyUI, shinyServer)