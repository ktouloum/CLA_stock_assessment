
# Instructions

#1. Put this script and LBB_aux_data.Rdata in the same folder 

#2  Within this folder, create a subfolder named "www". Put inside lbb.png and lbb2.png

#3. downoad and install Rtools from: https://cran.r-project.org/bin/windows/Rtools/

#4. downoad and install JAGS from:https://sourceforge.net/projects/mcmc-jags/

#5. downoad and install necessary packages, by running the following lines, after removing the hashtag (#)
#list.of.packages <- c("R2jags","shinyFiles","fs","ggridges","rvest","Hmisc","crayon","data.table","shinycssloaders","coda","htm2txt","stringi","tibble")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#6. Press the "Run App" button from the Rstudio menu to start the app


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyBS) 
library(ggpubr)
library(ggplot2)
library(R2jags)
#library(dplyr)
library(shinyFiles)
library(fs)
library(ggridges)
library(rvest)
library(Hmisc)
library(crayon) # to display bold and italics in console
library(data.table)
library(shinycssloaders)
library(coda)
library(htm2txt)
library(stringi)
library(tibble)


# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# path=getwd()

#source("LBB_aux_functions.R")
load("LBB_aux_data.Rdata")

#----------------------------------------------
#  FUNCTIONS ----
#----------------------------------------------
# 
#----------------------------------------------
linebreaks <- function(n){HTML(strrep(br(), n))}
'%!in%' <- function(x,y)!('%in%'(x,y))

fbsb_MAX <- function(sp_name) {
  # sp_name="Merluccius merluccius"
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
    speccode=NA
    Max_Linfo=NA
    Maturity=NA
  } else { 
    pages=list(thepage_a,thepage_b,thepage_c,thepage_d)
    thepage_picker=c(length(thepage_a),length(thepage_b), length(thepage_c),length(thepage_d))
    thepage=pages[[which.max(thepage_picker)]]
    
    pages_=list(page_a,page_b,page_c,page_d)
    page_=pages_[[which.max(thepage_picker)]]
    
    x=grep('speccode',thepage)
    y=grep('Max length',thepage)
    datalines1 =thepage[x[1]]
    datalines1=datalines1[nchar(datalines1)== max(nchar(datalines1))]
    datalines2 =thepage[y]
    # returns TRUE
    if (identical(datalines1, character(0))) {
      plain.text1=NA
      r_range=list(c(NA,NA))
    } else if(grepl("[0-9]", datalines1)) {
      plain.text1 <-gsub(".*speccode","",datalines1) 
      plain.text1=gsub("'.*", "", plain.text1)       
      plain.text1=gsub("=", "", plain.text1)       
      speccode=plain.text1
    } else {
      plain.text1=NA
      r_range=list(c(NA,NA))}
    plain.text2 <-htm2txt::htm2txt(datalines2)
    plain.text2=gsub("\t","",plain.text2)
    plain.text2=stringi::stri_trans_general(plain.text2, "ascii")
    plain.text2=gsub("  ", "", plain.text2)
    plain.text2=gsub("\\s*\\([^\\)]+\\)","",as.character(plain.text2 ))
    plain.text2=gsub(";;", ";", plain.text2)
    speccode=plain.text1
    
    Max_Linfo=gsub(".*\n","",plain.text2) 
    Maturity=gsub("\n.*","",plain.text2)   } 
  results=c(NA,NA,NA)
  results[1]=speccode
  results[2]=Max_Linfo
  results[3]=Maturity
  return(results)
}

fbsb_table_1 <- function(spec.code) {
  Bpage_a=paste0("https://www.fishbase.se/popdyn/PopCharList.php?ID=",spec.code)
  Bpage_b=paste0("https://www.fishbase.ca/popdyn/PopCharList.php?ID=",spec.code)
  Bpage_c=paste0("https://www.sealifebase.se/popdyn/PopCharList.php?ID=",spec.code)
  Bpage_d=paste0("https://www.sealifebase.ca/popdyn/PopCharList.php?ID=",spec.code)
  
  x1=tryCatch(rvest::read_html(Bpage_a), error=function(err) 1)
  x2=tryCatch(rvest::read_html(Bpage_b), error=function(err) 1)
  x3=tryCatch(rvest::read_html(Bpage_c), error=function(err) 1)
  x4=tryCatch(rvest::read_html(Bpage_d), error=function(err) 1)
  
  responseA =tryCatch(as.data.frame(html_table(x1)[1]), error=function(err) 1)#as.data.frame(html_table(x1)[3])
  responseB = tryCatch(as.data.frame(html_table(x2)[1]), error=function(err) 1) #as.data.frame(html_table(x2)[3])
  responseC =tryCatch(as.data.frame(html_table(x3)[1]), error=function(err) 1) # as.data.frame(html_table(x3)[3])
  responseD =tryCatch(as.data.frame(html_table(x4)[1]), error=function(err) 1)# as.data.frame(html_table(x4)[3])
  
  pages=list(responseA,responseB,responseC,responseD)
  
  thepage_picker=c(sum(nchar(pages[[1]])),sum(nchar(pages[[2]])), sum(nchar(pages[[3]])),sum(nchar(pages[[4]])))
  Table1=pages[[which.max(thepage_picker)]]
  Table1=Table1[,-c(2,4)]
  colnames(Table1)[2]="Lmax.cm"
  Table1$Locality=paste0(Table1$Locality, ", ",Table1$Country)
  Table1=Table1[,c(2,1,4)]
  Table1=Table1[!is.na(Table1$Lmax.cm),]
  
  return(Table1)
}

fbsb_table_2 <- function(spec.code) {
  
  Cpage_a=paste0("https://www.fishbase.se/popdyn/PopGrowthList.php?ID=",spec.code)
  Cpage_b=paste0("https://www.fishbase.ca/popdyn/PopGrowthList.php?ID=",spec.code)
  Cpage_c=paste0("https://www.sealifebase.se/popdyn/PopGrowthList.php?ID=",spec.code)
  Cpage_d=paste0("https://www.sealifebase.ca/popdyn/PopGrowthList.php?ID=",spec.code)
  
  x1=tryCatch(rvest::read_html(Cpage_a), error=function(err) 1)
  x2=tryCatch(rvest::read_html(Cpage_b), error=function(err) 1)
  #rvest::read_html(Cpage_b)
  x3=tryCatch(rvest::read_html(Cpage_c), error=function(err) 1)
  x4=tryCatch(rvest::read_html(Cpage_d), error=function(err) 1)
  #x4=rvest::read_html(Cpage_d)
  
  responseA =tryCatch(as.data.frame(html_table(x1)[3]), error=function(err) 1)#as.data.frame(html_table(x1)[3])
  responseB = tryCatch(as.data.frame(html_table(x2)[3]), error=function(err) 1) #as.data.frame(html_table(x2)[3])
  responseCa =tryCatch(as.data.frame(html_table(x3)[2]), error=function(err) 1) # as.data.frame(html_table(x3)[3])
  responseCb =tryCatch(as.data.frame(html_table(x3)[3]), error=function(err) 1) # as.data.frame(html_table(x3)[3])
  if (ncol(as.data.frame(responseCa))>ncol(as.data.frame(responseCb))) {
    responseC=responseCa
  } else {
    responseC=responseCb
  }
  
  responseDa =tryCatch(as.data.frame(html_table(x4)[2]), error=function(err) 1)# as.data.frame(html_table(x4)[3])
  responseDb =tryCatch(as.data.frame(html_table(x4)[3]), error=function(err) 1)# as.data.frame(html_table(x4)[3])
  if (ncol(as.data.frame(responseDa))>ncol(as.data.frame(responseDb))) {
    responseD=responseDa
  } else {
    responseD=responseDb
  }
  
  pages=list(responseA,responseB,responseC,responseD)
  
  thepage_picker=c(sum(nchar(pages[[1]])),sum(nchar(pages[[2]])), sum(nchar(pages[[3]])),sum(nchar(pages[[4]])))
  Table2=pages[[which.max(thepage_picker)]]
  if (ncol(Table2)>0 & nrow(Table2)>0) {
    Table2=Table2[,c("Loo.cm.","Length.Type",  "Sex" ,"Lm" ,"Country","Locality","Questionable", "Captive")]
    Table2=Table2[Table2$Questionable!="Yes",] 
    Table2=Table2[Table2$Captive!="Yes",]
    Table2=Table2[,1:6]
    colnames(Table2)[1]="Linf"
    colnames(Table2)[2]="L.type"
    if ( nrow(Table2)>0) {
      Table2$Locality=paste0(Table2$Locality, ", ",Table2$Country)
      Table2=Table2[,c(1:4,6)]
      Table2=Table2[!is.na(Table2$Linf), ]} else {
        Table2=data.frame( Fishbase.SeaLifeBase="No information found")
      }
    
  } else {
    Table2=data.frame( Fishbase.SeaLifeBase="No information found")
  }
  
  return(Table2)
}


fbsb_table_mat <- function(spec.code) {
  Bpage_a=paste0("https://www.fishbase.se/Reproduction/MaturityList.php?ID=",spec.code)
  Bpage_b=paste0("https://www.fishbase.ca/Reproduction/MaturityList.php?ID=",spec.code)
  Bpage_c=paste0("https://www.sealifebase.se/Reproduction/MaturityList.php?ID=",spec.code)
  Bpage_d=paste0("https://www.sealifebase.ca/Reproduction/MaturityList.php?ID=",spec.code)
  
  x1=tryCatch(rvest::read_html(Bpage_a), error=function(err) 1)
  x2=tryCatch(rvest::read_html(Bpage_b), error=function(err) 1)
  x3=tryCatch(rvest::read_html(Bpage_c), error=function(err) 1)
  x4=tryCatch(rvest::read_html(Bpage_d), error=function(err) 1)
  
  responseA =tryCatch(as.data.frame(html_table(x1)[2]), error=function(err) 1)#as.data.frame(html_table(x1)[3])
  responseB = tryCatch(as.data.frame(html_table(x2)[2]), error=function(err) 1) #as.data.frame(html_table(x2)[3])
  responseC =tryCatch(as.data.frame(html_table(x3)[2]), error=function(err) 1) # as.data.frame(html_table(x3)[3])
  responseD =tryCatch(as.data.frame(html_table(x4)[2]), error=function(err) 1)# as.data.frame(html_table(x4)[3])
  
  pages=list(responseA,responseB,responseC,responseD)
  
  thepage_picker=c(sum(nchar(pages[[1]])),sum(nchar(pages[[2]])), sum(nchar(pages[[3]])),sum(nchar(pages[[4]])))
  Table1=pages[[which.max(thepage_picker)]]
  Table1=Table1[Table1$Lm.cm.!="",]
  Table1=Table1[!is.na(Table1$Lm.cm.),]
  Table1$Length.range=paste0(Table1$Length.cm.,Table1$Length.cm..1,Table1$Length.cm..2)
  Table1$Length.range[Table1$Length.range== "NA-NA"]=""
  Table1$age.range=paste0(Table1$Age.range.y.,Table1$Age.range.y..1,Table1$Age.range.y..2)
  Table1$age.range[Table1$age.range== "NA-NA"]=""
  Table1$Locality=paste(Table1$Country,Table1$Locality,sep = ", ")
  Table1$Lm.cm.=gsub(" TL","", Table1$Lm.cm.)
  Table1=Table1[,c( "Lm.cm.","Length.range",  "age.range", "tm.y.",   "Sex.of.fish", "Locality"     )]
  colnames(Table1)=c("Lm.cm","Length.range", "Age.range","tm.y","Sex","Locality")
  return(Table1)
}


fix_LFD= function(LBB.obj,cr.sel,munit) {
  LBB_data_=as.data.frame(LBB.obj)
  LBB_data_$CatchNo=as.numeric( LBB_data_$CatchNo)
  LBB_data_$Length=as.numeric( LBB_data_$Length)
  LBB_data_$Year=as.integer( LBB_data_$Year)
  LBB_data_$CatchNo[is.na(LBB_data_$CatchNo)]=0
  if(cr.sel=="created" ) {
    munit=munit} else {
      munit="mm"    }
  if (munit=="mm") {
    LBB_data_$Length= LBB_data_$Length/10
  }
  LBB_data_=LBB_data_[LBB_data_$CatchNo>0,]
  lb_data=LBB_data_[, c("Stock" ,"Year",	"Length"	,	"CatchNo") ]
  
  return(lb_data)
}


##############################################################
#  Functions
##############################################################
#--------------------------------------------------------
# Exploited B/B0 ratio from B&H equations, for variable F
#--------------------------------------------------------
# assuming that reported lengths are the lower bounds of length classes
# get lowest exploited (>= 0.01 F) length class and class width

BH <- function(AllLength,Linf,MK,FK,GausSel,selpar1,selpar2) {
  if(GausSel==F) {
    r.Lc     <- selpar1
    r.alpha  <- selpar2 
    Lx       <- AllLength[AllLength >= Linf*(r.Lc-4.59/r.alpha)][1]
  } else if(GausSel==T) {
    r.GLmean <- selpar1
    r.SD     <- selpar2
    Lx       <- AllLength[AllLength >= Linf*(r.GLmean-3*r.SD)][1]
  }
  class.width  <- median(diff(sort(unique(AllLength))))
  FM <- FK/MK
  
  # Linf=120;Lx=22.5;r.Lc=0.2917;r.alpha=60;MK=1.5385;FK=0.7692;FM=0.5;ZK=2.3077 
  # uncomment above row for comparison of Y'R= 0.0332, B/B0=0.467 with CodLightSim
  r            <- vector() # auxilliary reduction factor
  G            <- vector() # product of reduction factors
  SL.bh        <- vector() # selection at length
  YR1.2        <- vector() # relative yield per recruit per length class
  CPUER1.2     <- vector() # relative CPUE per recruit per length class
  B1.2         <- vector() # relative unexploited biomass per recruit by length class
  L.bh         <- seq(from=Lx, to=Linf, by=class.width) # lengths to be considered
  r.L.bh       <-  L.bh / Linf # standardized lengths
  
  # calculate selection, Y'/R and CPUE'/R for every length class
  for(o in 1 : length(r.L.bh)) { 
    if(GausSel==F) {
      if(o<length(r.L.bh)) { SL.bh[o] <- mean(c(1/(1+exp(-r.alpha*(r.L.bh[o]-r.Lc))), # mean selection in length class
                                                1/(1+exp(-r.alpha*(r.L.bh[o+1]-r.Lc)))))
      } else SL.bh[o] <- 1/(1+exp(-r.alpha*(r.L.bh[o]-r.Lc)))
    } else if(GausSel==T) { # gill net selection 
      if(o<length(r.L.bh)) { SL.bh[o] <- mean(c(exp(-((r.L.bh[o]-r.GLmean)^2/(2*r.SD^2))), # mean selection in length class
                                                exp(-((r.L.bh[o+1]-r.GLmean)^2/(2*r.SD^2)))))
      } else SL.bh[o] <- exp(-((r.L.bh[o]-r.GLmean)^2/(2*r.SD^2)))
    } # end of calculation of selectivity loop
    
    if(o<length(r.L.bh)) {
      r[o]       <- (1-r.L.bh[o+1])^(FK*SL.bh[o])/(1-r.L.bh[o])^(FK*SL.bh[o]) 
      G[o]       <- prod(r[1:o]) }
    if(o==1) {
      YR1.2[o] <-(FM*SL.bh[o]/(1+FM*SL.bh[o])*(1-r.L.bh[o])^MK*(1-3*(1-r.L.bh[o])/(1+1/
                                                                                     (MK+FK*SL.bh[o]))+3*(1-r.L.bh[o])^2/(1+2/(MK+FK*SL.bh[o]))-
                                                                  (1-r.L.bh[o])^3/(1+3/(MK+FK*SL.bh[o])))) -
        (FM*SL.bh[o]/(1+FM*SL.bh[o])*(1-r.L.bh[o+1])^MK*(1-3*(1-r.L.bh[o+1])/(1+1/
                                                                                (MK+FK*SL.bh[o]))+3*(1-r.L.bh[o+1])^2/(1+2/(MK+FK*SL.bh[o]))-
                                                           (1-r.L.bh[o+1])^3/(1+3/(MK+FK*SL.bh[o]))))*G[o] 
    } else if(o==length(r.L.bh)) {
      YR1.2[o] <- (FM*SL.bh[o]/(1+FM*SL.bh[o])*(1-r.L.bh[o])^MK*(1-3*(1-r.L.bh[o])/(1+1/
                                                                                      (MK+FK*SL.bh[o]))+3*(1-r.L.bh[o])^2/(1+2/(MK+FK*SL.bh[o]))-
                                                                   (1-r.L.bh[o])^3/(1+3/(MK+FK*SL.bh[o])))) * G[o-1] 
    } else {
      YR1.2[o] <- (FM*SL.bh[o]/(1+FM*SL.bh[o])*(1-r.L.bh[o])^MK*(1-3*(1-r.L.bh[o])/(1+1/
                                                                                      (MK+FK*SL.bh[o]))+3*(1-r.L.bh[o])^2/(1+2/(MK+FK*SL.bh[o]))-
                                                                   (1-r.L.bh[o])^3/(1+3/(MK+FK*SL.bh[o])))) * G[o-1] -
        (FM*SL.bh[o]/(1+FM*SL.bh[o])*(1-r.L.bh[o+1])^MK*(1-3*(1-r.L.bh[o+1])/(1+1/
                                                                                (MK+FK*SL.bh[o]))+3*(1-r.L.bh[o+1])^2/(1+2/(MK+FK*SL.bh[o]))-
                                                           (1-r.L.bh[o+1])^3/(1+3/(MK+FK*SL.bh[o]))))*G[o]              
    } # end of loop to calculate yield per length class
    
    CPUER1.2[o] <- YR1.2[o] / FM # CPUE/R = Y/R divided by F/M
    
    if(o<length(r.L.bh)) {
      B1.2[o] <- ((1-r.L.bh[o])^MK*(1-3*(1-r.L.bh[o])/(1+1/MK)+3*(1-r.L.bh[o])^2/
                                      (1+2/MK)-(1-r.L.bh[o])^3/(1+3/MK)) -
                    (1-r.L.bh[o+1])^MK*(1-3*(1-r.L.bh[o+1])/(1+1/MK)+3*(1-r.L.bh[o+1])^2/
                                          (1+2/MK)-(1-r.L.bh[o+1])^3/(1+3/MK)))*SL.bh[o]
    } else {
      B1.2[o] <- ((1-r.L.bh[o])^MK*(1-3*(1-r.L.bh[o])/(1+1/MK)+3*(1-r.L.bh[o])^2/
                                      (1+2/MK)-(1-r.L.bh[o])^3/(1+3/MK)))*SL.bh[o]
    }
  } # end of B&H loop through length classes
  BB0   <- sum(CPUER1.2)/sum(B1.2)
  YR    <- sum(YR1.2)
  if(BB0 < 0.25) YR <- YR * BB0 / 0.25 # reduce YR if recruitment and thus productivity is reduced
  return(list(BB0,YR,CPUER1.2,B1.2,YR1.2,r.L.bh))
  
} # end of BH function

#------------------------------------------------------------
# Function to aggregate data by year
#------------------------------------------------------------
AG  <- function(dat) { # where dat contains dat$Year, dat$Length in cm, dat$CatchNo
  
  # aggregate normalized annual LFs by weighing with square root of sample size
  # get sum of frequencies per year
  sum.Ny  <- aggregate(Freq~Year,dat,sum)$Freq  
  # get the sqrt of the sum of frequencies for every year
  sqrt.Ny <- sqrt(sum.Ny) 
  # get highest frequency in each year
  max.Ny <- aggregate(Freq~Year,dat,max)$Freq
  # get Number of Length bins in each year
  binsN <- aggregate(Freq~Year,dat,length)$Freq    
  # create vectors for sqrt.Ni and sum.Ni to weigh LF data
  sqrt.Ni = rep(sqrt.Ny,binsN)
  sum.Ni = rep(sum.Ny,binsN)
  #Do weighing
  # Divide all years by sum.Ni and multiply by sqrt.Ni
  LF.w = dat$Freq/sum.Ni*sqrt.Ni  
  # Aggregate
  LF = aggregate(LF.w, by=list(dat$Length),FUN=sum)
  # Add correct column names
  colnames(LF) <- c("Length","Freq")         
  return(LF)
} #end of aggregate function

#-----------------------------------------------------------
# Function to plot LBB-fit for a single year
#-----------------------------------------------------------
# expects lengths relative to Linf (L/Linf)


#-------------------------------------------------------
# Function to apply preceding 3-years moving average
#-------------------------------------------------------
ma    <- function(x){
  x.1    <- stats::filter(x,rep(1/3,3),sides=1)
  x.1[1] <- x[1]
  x.1[2] <- (x[1]+x[2])/2
  return(x.1)
}

LBB.AG  <- function(df,Sel.yrs,width,L.width=1,L.cut) {#,munit
  Stock=unique(df$Stock)
  df=df[df$Year %in% Sel.yrs,]
  L.width=as.numeric(L.width)
  if (L.width==1) {
    df=df
  } else if (L.width>1) {
    width=width*L.width
    max_LC=unique(max(df$Length[df$CatchNo>0],na.rm=T))
    classes=seq(0,max_LC,width)
    clasnms=classes#+wdth/2
    clasnms=clasnms[1:length(clasnms)-1]
    ###LENGTH_CLASS & make the necessary dataframes, we do not take into account the season and depth since sample is small
    #Selectivity$LC=floor(Selectivity$Total.Length..cm.)+0.5
    df$LC=as.numeric(as.character(cut(df$Length,breaks =classes,labels = clasnms,right = F )))
    
    df=aggregate(CatchNo   ~Stock +Year +LC , data = df, sum)
    colnames(df)= c("Stock",   "Year","Length",  "CatchNo")
  }
  df=df[df$CatchNo>0,]
  df$CatchNo[df$Length<=L.cut]=0
  # if (munit=="mm") {
  #   df$Length=df$Length/10
  # }
  #--------------------------------------------------------------------------------------
  # Use aggregated LF data for estimation of Linf (and overall Z/K)
  #--------------------------------------------------------------------------------------
  df_        <- as.data.frame(df[,c("Year","Length","CatchNo")])
  names(df_) <- c("Year","Length","Freq")
  
  LF.all    <- AG(dat=df_) # function to aggregate data by year
  # standardize to max Freq
  LF.all$Freq = LF.all$Freq/max(LF.all$Freq) 
  # remove leading empty records
  LF.all     <- LF.all[which(LF.all$Freq>0)[1] : length(LF.all$Length),]
  # remove trailing empty records
  LF.all     <- LF.all[1 : which(LF.all$Length==max(LF.all$Length[LF.all$Freq>0])),]
  
  return(LF.all)
}


L.start= function(LF.all,Linf.user) {
  # If no Linf is provided by the user (preferred), determine Linf from fully selected LF:
  # Freq=Nstart*exp(ZK*(log(1-L/Linf)-log(1-Lstart/Linf)))
  # Nstart is canceled out when dividing both sides by their sums
  # ---------------------------------------------------------
  # determine start values of selection ogive to find first fully selected length class Lstart
  L10         <- LF.all$Length[which(LF.all$Freq>0.1)[1]] # use length at 10% of peak frequency as proxy for L10
  L90         <- LF.all$Length[which(LF.all$Freq>0.9)[1]] # use length at 90% of peak frequency as proxy for L90
  if(L90!=L10) {    #################### KT 2024 ADDED AFTER GELLIS ISSUES 
    Lc.st       <- (L10 + L90)/2 #####ADD LATER ifelse(is.na(dat.ID$Lc.user)==TRUE,(L10 + L90)/2,dat.ID$Lc.user)  # use mean of L10 and L90 as proxy for Lc, else user input
  alpha.st    <- -log(1/LF.all$Freq[which(LF.all$Freq>0.1)[1]])/(L10-Lc.st) # use rearranged logistic curve to estimate slope alpha
  
  # determine start values for Linf and Z/K 
  Linf.st     <- Linf.user      #####ADD LATER   ifelse(is.na(dat.ID$Linf.user)==F,dat.ID$Linf.user,Lmax.med) # use Linf.user or median Lmax across years as start value for Linf in nls analysis
  Lmean.st    <- sum(LF.all$Length[LF.all$Length>=Lc.st]*LF.all$Freq[LF.all$Length>=Lc.st])/
    sum(LF.all$Freq[LF.all$Length>=Lc.st])
  
  # get vectors with fully selected length classes for Linf estimation
    
  ####################CHANGE BASED ON THE CHANGES IN DATA PREPARATION
  
  Lstart     <- (alpha.st*Lc.st-log(1/0.95-1))/alpha.st   # Length where selection probability is 0.95  
  # test if there are enough (>=4) length classes for estimation of aggregated Linf and ZK 
  Lstart.i   <- which(LF.all>=Lstart)[1]
  if (is.na(Lstart.i)) {
    Lstart.i   <- which(LF.all>=round(Lstart))[1]
  }
  Lmax.i     <- length(LF.all$Length)
  peak.i     <- which.max(LF.all$Freq)
  if(Lstart.i<(peak.i+1)) Lstart <- LF.all$Length[peak.i+1] # make sure fully selected length starts after peak 
  if((Lmax.i-Lstart.i)<4) Lstart <- LF.all$Length[Lstart.i-1] # make sure enough length classes are available
  xxx=c(Lstart,Lc.st)} else {       ### KT 2024  !!!!!ADD A NOTE THAT IF ELSE IS TRUE, trawl like selectivity is not possible to run
    Lstart=L10+1
    Lc.st=L10-1
    xxx=c(Lstart,Lc.st)}   

  return(xxx)
}

Run.nls= function(LF.all,Linf.user,Linf.user.CV,Lstart) {
  # If no Linf is provided by the user (preferred), determine Linf from fully selected LF:
  # Freq=Nstart*exp(ZK*(log(1-L/Linf)-log(1-Lstart/Linf)))
  # Nstart is canceled out when dividing both sides by their sums
  # ---------------------------------------------------------
  # determine start values of selection ogive to find first fully selected length class Lstart
  L10         <- LF.all$Length[which(LF.all$Freq>0.1)[1]] # use length at 10% of peak frequency as proxy for L10
  L90         <- LF.all$Length[which(LF.all$Freq>0.9)[1]] # use length at 90% of peak frequency as proxy for L90
  Lc.st       <- Lstart[2]#(L10 + L90)/2 #####ADD LATER ifelse(is.na(dat.ID$Lc.user)==TRUE,(L10 + L90)/2,dat.ID$Lc.user)  # use mean of L10 and L90 as proxy for Lc, else user input
  alpha.st    <- -log(1/LF.all$Freq[which(LF.all$Freq>0.1)[1]])/(L10-Lc.st) # use rearranged logistic curve to estimate slope alpha
  
  # determine start values for Linf and Z/K 
  Linf.st     <- Linf.user      #####ADD LATER   ifelse(is.na(dat.ID$Linf.user)==F,dat.ID$Linf.user,Lmax.med) # use Linf.user or median Lmax across years as start value for Linf in nls analysis
  Lmean.st    <- sum(LF.all$Length[LF.all$Length>=Lc.st]*LF.all$Freq[LF.all$Length>=Lc.st])/
    sum(LF.all$Freq[LF.all$Length>=Lc.st])
  #MK.st       <- MK.user #####ADD LATER ifelse(is.na(dat.ID$MK.user)==TRUE, 1.5,dat.ID$MK.user) # default 1.5
  ZK.st       <- (Linf.st-Lmean.st)/(Lmean.st-Lc.st)       # the Holt equation
  #FK.st       <- ifelse((ZK.st-MK.st)>0,ZK.st-MK.st,0.3)   # prevent M/K being larger than Z/K
  
  # do not include Lmax to allow Linf < Lmax and to avoid error in nls when Linf-L becomes negative
  L.L         <- LF.all$Length[LF.all$Length >= Lstart[1]  & LF.all$Length < Linf.st]
  L.Freq      <- LF.all$Freq[LF.all$Length>=L.L[1]& LF.all$Length < Linf.st]
  
  # standardize frequencies by dividing by sum of observed frequencies, needed to drop NLstart from equation
  sum.L.Freq  <- sum(L.Freq)
  L.Freq      <- L.Freq/sum.L.Freq
  
  Linf.nls    <- Linf.user 
  Linf.nls.sd <- Linf.user.CV*Linf.user
  ZK.mod      <- tryCatch(nls(L.Freq ~ exp(ZK*(log(1-L.L/Linf.nls)-log(1-L.L[1]/Linf.nls)))/
                                sum(exp(ZK*(log(1-L.L/Linf.nls)-log(1-L.L[1]/Linf.nls)))),
                              start=list(ZK=ZK.st),
                              lower=c(0.7*ZK.st),
                              upper=c(1.3*ZK.st), 
                              algorithm = "port"), error=function(err) "ERROR")
  
  ZK.nls       <-ifelse(unique(ZK.mod!= "ERROR"), as.numeric(coef(ZK.mod)[1]),NA)
  ZK.nls.sd    <-ifelse(unique(ZK.mod!= "ERROR"), as.numeric(coef(summary(ZK.mod))[,2][1]),NA)
  ZK.nls.lcl   <-ifelse(unique(ZK.mod!= "ERROR"),ZK.nls-1.96*ZK.nls.sd,NA)
  ZK.nls.ucl   <-ifelse(unique(ZK.mod!= "ERROR"), ZK.nls+1.96*ZK.nls.sd,NA)
  
  df_fornls=data.frame(L.L=L.L,L.Freq=L.Freq)
  outcomes=list()
  outcomes[["df_fornls"]]=df_fornls
  outcomes[["parameters"]]=c(ZK.st,
                             Linf.nls,
                             ZK.nls,
                             ZK.nls.sd,
                             ZK.nls.lcl,
                             ZK.nls.ucl,
                             Linf.nls.sd,
                             L10)
  outcomes[["model"]]=c(ZK.mod)
  
  
  return(outcomes)
}  



LBB.obj.create   <- function(df,Species,Name,Sel.yrs,width,L.width=1,L.cut,Run.nls.obj,MK.user,MK.user.CV,Lstart.obj,GausSel,semelp,MergeLF,Lm50.user,Lmean.user,LmeanCV.user) {#,munit
  Stock=unique(df$Stock)
  
  df=df[df$Year %in% Sel.yrs,]
  
  L.width=as.numeric(L.width)
  
  if (L.width==1) {
    df=df
  } else if (L.width>1) {
    width=width*L.width
    max_LC=unique(max(df$Length[df$CatchNo>0],na.rm=T))
    classes=seq(0,max_LC,width)
    clasnms=classes#+wdth/2
    clasnms=clasnms[1:length(clasnms)-1]
    ###LENGTH_CLASS & make the necessary dataframes, we do not take into account the season and depth since sample is small
    #Selectivity$LC=floor(Selectivity$Total.Length..cm.)+0.5
    df$LC=as.numeric(as.character(cut(df$Length,breaks =classes,labels = clasnms,right = F )))
    
    df=aggregate(CatchNo   ~Stock +Year +LC , data = df, sum)
    colnames(df)= c("Stock",   "Year","Length",  "CatchNo")
  }
  #df$CatchNo[df$Length<=L.cut]=0
  # if (munit=="mm") {
  #   df$Length=df$Length/10
  # }
  max_length=max(df$Length[df$CatchNo>0],na.rm=T)
  
  df_=df[df$CatchNo>0,]
  df_$max_l=ave(df_$Length,df_$Year,FUN=max)
  df_= unique(df_[,c("Year","max_l")])
  
  median_max_length=median(df_$max_l,na.rm = T)
  
  df=df[,2:4]
  df=df[df$CatchNo>0,]
  
  LBB_object=list()
  
  LBB_object[["input.data"]]=df
  LBB_object[["model.param"]]=data.frame(Stock=Stock,Species=Species, Name=Name,max_length=max_length,median_max_length=median_max_length,
                                         width=width,L.width=L.width,L.cut=L.cut,MK.user=MK.user,MK.user.CV=MK.user.CV,
                                         Linf.nls=Run.nls.obj[["parameters"]][2],Linf.nls.sd= Run.nls.obj[["parameters"]][7],
                                         Lstart=Lstart.obj[1],Lc.st=Lstart.obj[2],L10= Run.nls.obj[["parameters"]][8],
                                         ZK.nls=Run.nls.obj[["parameters"]][3],ZK.nls.sd=Run.nls.obj[["parameters"]][4],GausSel=GausSel,Semelparous=semelp,
                                         MergeLF=MergeLF,Lm50.user=Lm50.user,Lmean.user=Lmean.user,LmeanCV.user=LmeanCV.user)
  nYears=length(unique(df$Year))
  Lfit       <- matrix(list(),nYears,3)
  # LBB_object[["model.outcomes"]]=Ldat
  LBB_object[["Lfit"]]=Lfit
  
  return(LBB_object)
  
}

LBB.fit   <- function(LBB.obj) {
  jagsFit<-c() #modification by GP to select the best year
  i = 0 # start counter
  df_ALL=LBB.obj[["input.data"]]
  df_ALL$CatchNo[df_ALL$Length<=LBB.obj[["model.param"]]$L.cut]=0
  df_ALL=df_ALL[df_ALL$Length<= LBB.obj[["model.param"]]$Linf.nls,]
  
  df_mat=LBB.obj[["input.data"]]
  
  
  MK.st=LBB.obj[["model.param"]]$MK.user
  MK.CV=LBB.obj[["model.param"]]$MK.user.CV
  Lc.st=LBB.obj[["model.param"]]$Lc.st
  L10=LBB.obj[["model.param"]]$L10
  Linf.nls=LBB.obj[["model.param"]]$Linf.nls
  Linf.nls.sd=LBB.obj[["model.param"]]$Linf.nls.sd
  ZK.nls=LBB.obj[["model.param"]]$ZK.nls
  GausSel=LBB.obj[["model.param"]]$GausSel
  MergeLF=LBB.obj[["model.param"]]$MergeLF
  Lm50.user=LBB.obj[["model.param"]]$Lm50.user
  Lmean.user=LBB.obj[["model.param"]]$Lmean.user
  LmeanCV.user=LBB.obj[["model.param"]]$LmeanCV.user
  jagsfitSLN=list()
  Years=sort(unique(df_ALL$Year))
  Ldat=LBB.obj[["model.outcomes"]]
  nYears=length(unique(df_ALL$Year))
  Lfit       <- matrix(list(),nYears,3) 
  for(Year in Years) {
    i = i+1 # i is the index of Years, which may contain gaps 
    
    # if MergeLF==TRUE and if this is the second or heigher year and no simulation, aggregate LF with previous year LF
    if(MergeLF==TRUE) {
      if(i==1) {AG.yr <- c(Year,Years[2])} else { # if first year, aggregate with second year
        AG.yr <- c(Years[i-1],Year) }
    } else AG.yr <- Year
    
    df=df_ALL[df_ALL$Year %in% AG.yr,]
    names(df) <- c("Year","Length","Freq")
    LF.y      <- AG(dat=df) # function to aggregate data by year and across years
    LF.y$Freq <- LF.y$Freq/sum(LF.y$Freq) # standardize frequencies
    
    # remove empty leading and trailing records
    LF.y        <- LF.y[which(LF.y$Freq>0)[1] : length(LF.y$Length),]
    LF.y        <- LF.y[1 : which.max(LF.y$Length[LF.y$Freq>0]),]
    # get vectors
    L.y         <- LF.y$Length
    r.Freq.y    <- LF.y$Freq
    
    # fill remaining zero frequencies with very small number, to avoid error
    r.Freq.y[r.Freq.y==0] <- min(r.Freq.y[r.Freq.y>0],na.rm=T)/100
    
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    
    df.mat=df_mat[df_mat$Year %in% AG.yr,]
    names(df.mat) <- c("Year","Length","Freq")
    LF.ymat      <- AG(dat=df.mat) # function to aggregate data by year and across years
    LF.ymat$Freq <- LF.ymat$Freq/sum(LF.ymat$Freq) # standardize frequencies
    
    # remove empty leading and trailing records
    LF.ymat        <- LF.ymat[which(LF.ymat$Freq>0)[1] : length(LF.ymat$Length),]
    LF.ymat        <- LF.ymat[1 : which.max(LF.ymat$Length[LF.ymat$Freq>0]),]
    # get vectors
    L.y.mat        <- LF.ymat$Length
    r.Freq.y.mat    <- LF.ymat$Freq
    
    # fill remaining zero frequencies with very small number, to avoid error
    r.Freq.y.mat[r.Freq.y.mat==0] <- min(r.Freq.y.mat[r.Freq.y.mat>0],na.rm=T)/100
    
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    ####################### FOR MATURITY
    
    
    # enter data for this year into data frame
    # 
    
    #-------------------------------------------------------------------------
    # Estimate annual parameters Lc, alpha, M/K, F/K from LF curve with trawl-type selection
    #-------------------------------------------------------------------------
    # determine priors 
    n.L         <- length(L.y)
    Linf.pr     <- Linf.nls
    Linf.sd.pr  <- ifelse(Linf.nls.sd/Linf.nls<0.01,Linf.nls.sd,0.01*Linf.nls) # restict prior CV of Linf to < 0.01
    MK.pr       <- MK.st
    MK.sd.pr    <- MK.CV ##ADD LATER ifelse(is.na(dat.ID$MK.user)==TRUE,0.15,0.075)
    Pile        <- 0 ##ADD LATER dat.ID$Pile
    
    # apply trawl-like selection 
    if(GausSel==FALSE){ # apply trawl-like selection 
      Lc.pr        <- 1.02*Lc.st ##ADD LATERifelse(is.na(dat.ID$Lc.user)==TRUE,1.02*Lc.st,dat.ID$Lc.user) # with 1.02 multiplier to account for systematic small underestimation
     # if (is.na(Lc.pr)) {
     #   Lc.pr=1.02
     #   
     # }
       Lc.sd.pr     <- 0.1*Lc.pr ##ADD LATERifelse(is.na(dat.ID$Lc.user)==TRUE,0.1*Lc.pr,0.05*Lc.pr) # assume narrower SD if Lc is given by user
      r.max.Freq   <- max(r.Freq.y,na.rm=T) 
      r.alpha.pr   <- -log(r.max.Freq/r.Freq.y[which(r.Freq.y>(0.1*r.max.Freq))[1]])/(L10/Linf.nls-Lc.st/Linf.nls) # relative alpha for standardized data
      r.alpha.sd.pr<- 0.025*r.alpha.pr 
      FK.pr        <- ifelse((ZK.nls-MK.st) > 0,ZK.nls-MK.st,0.3) # if Z/K <= M/K assume low F/K = 0.3 
      
      sink("SLNMod.jags")
      cat("
  model {
  r.alpha.d_tau  <- pow(r.alpha.sd.pr, -2) 
  r.alpha.d      ~ dnorm(r.alpha.pr,r.alpha.d_tau) 

   Lc.d_tau  <- pow(Lc.sd.pr,-2)
   Lc.d      ~ dnorm(Lc.pr,Lc.d_tau) #       

   MK.d_tau  <-pow(MK.sd.pr, -2) # strong prior on M/K
   MK.d      ~ dnorm(MK.pr, MK.d_tau)

   Linf.tau  <- pow(Linf.sd.pr,-2) 
   Linf.d    ~ dnorm(Linf.pr,Linf.tau)
    
   FK.d       ~ dlnorm(log(FK.pr),4) # wide prior range for F/K

   SL[1]       ~ dlogis(0,1000)
   Freq.pred[1]<-0
   xN[1]       <-1

   p.low    <- ifelse(Pile==1,0.99,0)   
   p.hi     <- ifelse(Pile==0,0.01,1)
   pile.fac ~ dunif(p.low,p.hi)


   for(j in 2:n.L) {
    SL[j] <- 1/(1+exp(-r.alpha.d*(((L.y[j]+L.y[j-1])/2)/Linf.d-Lc.d/Linf.d))) # selection at mid-length of bin

    xN[j] <- xN[j-1]*((Linf.d-L.y[j])/(Linf.d-L.y[j-1]))^(MK.d+FK.d*SL[j]) # predicted numbers without pile-up
     
    cN[j] <- (xN[j-1]-xN[j])/(MK.d+FK.d*SL[j]) # predicted relative frequency with pile-up correction

    dN[j] <- cN[j]-xN[j] # difference between corrected and uncorrected frequencies
    
    uN[j] <- xN[j] + dN[j]*pile.fac # gradual application of correction with pile.fac between 0 and 1

		Freq.pred[j]<-uN[j]*SL[j] # relative frequencies of vulnerable individuals
		
    # normalize frequencies by dividing by sum of frequencies; multiply with 10 to avoid small numbers and with 1000 for effective sample size
    r.Freq.pred[j]<- Freq.pred[j]/sum(Freq.pred)*10*1000
  }	
  
  #><> LIKELIHOOD FUNCTION
  #><> Fit observed to predicted LF data using a Dirichlet distribution (more robust in JAGS)
  r.Freq.y[2:n.L] ~ ddirch(r.Freq.pred[2:n.L])  
 
  } # END OF MODEL
    ",fill = TRUE)
      sink()
      
      MODEL = "SLNMod.jags"
      
      # list of data to pass to JAGS plus list of parameters to estimate   
      jags.data <- list ("r.Freq.y","L.y","n.L","Linf.pr","Linf.sd.pr","Lc.pr","Lc.sd.pr","r.alpha.pr","r.alpha.sd.pr","MK.pr","MK.sd.pr",
                         "FK.pr","Pile")
      jags.params <- c("r.alpha.d","Lc.d","SL","xN","FK.d","MK.d","Linf.d","pile.fac","Freq.pred")
      
      jagsfitSLN<- jags.parallel(data=jags.data, working.directory=NULL, inits=NULL, #[[i]] 
                                 parameters.to.save=jags.params, 
                                 model.file=paste(MODEL), 
                                 n.burnin=300, n.thin=10, n.iter=600, n.chains=3)
      
      jagsFit<-c(jagsFit,jagsfitSLN$BUGSoutput$pD) #modification by GP to select the best year according to the Deviance information criterion
      
      # use median and percentiles
      Ldat$Year[i]     <- Year
      Ldat$Lc[i]      <- median(jagsfitSLN$BUGSoutput$sims.list$Lc.d)
      Ldat$Lc.lcl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$Lc.d,0.025)
      Ldat$Lc.ucl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$Lc.d,0.975)
      Ldat$Lmean[i]   <- sum(L.y[L.y>=Ldat$Lc[i]]*r.Freq.y[L.y>=Ldat$Lc[i]])/sum(r.Freq.y[L.y>=Ldat$Lc[i]])
      Ldat$Lmean.lcl[i]   <- sum(L.y[L.y>=Ldat$Lc.lcl[i]]*r.Freq.y[L.y>=Ldat$Lc.lcl[i]])/sum(r.Freq.y[L.y>=Ldat$Lc.lcl[i]])
      Ldat$Lmean.ucl[i]   <- sum(L.y[L.y>=Ldat$Lc.ucl[i]]*r.Freq.y[L.y>=Ldat$Lc.ucl[i]])/sum(r.Freq.y[L.y>=Ldat$Lc.ucl[i]])
      Ldat$r.alpha[i] <- median(jagsfitSLN$BUGSoutput$sims.list$r.alpha.d)
      Ldat$r.alpha.lcl[i]<- quantile(jagsfitSLN$BUGSoutput$sims.list$r.alpha.d,0.025)
      Ldat$r.alpha.ucl[i]<- quantile(jagsfitSLN$BUGSoutput$sims.list$r.alpha.d,0.975)
      Ldat$MK[i]      <- median(jagsfitSLN$BUGSoutput$sims.list$MK.d)
      Ldat$MK.lcl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$MK.d,0.025)
      Ldat$MK.ucl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$MK.d,0.975)
      Ldat$FK[i]      <- median(jagsfitSLN$BUGSoutput$sims.list$FK.d)
      Ldat$FK.lcl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$FK.d,0.025)
      Ldat$FK.ucl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$FK.d,0.975)
      FMi             <- jagsfitSLN$BUGSoutput$sims.list$FK.d/jagsfitSLN$BUGSoutput$sims.list$MK.d
      Ldat$FM[i]      <- median(FMi)
      Ldat$FM.lcl[i]  <- quantile(FMi,0.025)
      Ldat$FM.ucl[i]  <- quantile(FMi,0.975)
      ZKi             <- jagsfitSLN$BUGSoutput$sims.list$MK.d + jagsfitSLN$BUGSoutput$sims.list$FK.d
      Ldat$ZK[i]      <- median(ZKi)
      Ldat$ZK.lcl[i]  <- quantile(ZKi,0.025)
      Ldat$ZK.ucl[i]  <- quantile(ZKi,0.975)
      Ldat$r.Lopt[i]  <- 3/(3+Ldat$MK[i])
      Ldat$Linf[i]    <- median((jagsfitSLN$BUGSoutput$sims.list$Linf.d))
      Ldat$Linf.lcl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$Linf.d,0.025)
      Ldat$Linf.ucl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$Linf.d,0.975)
      
    }
    
    if(GausSel==TRUE) {
      # determine priors
      # assume length at peak Freq as mean and distance to length at 80% of peak as SD of mean
      GLmean.st <-Lmean.user  # L.y[which.max(r.Freq.y)] ####################### KT CHANGE!!!!!!!
      # assume SD of Gaussian selection as distance between length at peak and length at 50% of peak
      Lc.pr     <- L.y[which(r.Freq.y >= (0.5*max(r.Freq.y)))][1]
      SD.st      <-LmeanCV.user# max(GLmean.st-Lc.pr,0.25*GLmean.st)
      
      # cat("Running Jags model to fit SL and N distributions for gillnet-like selection\n")
      
      n.L <- length(L.y)
      
      jags.data <- list ("n.L","GLmean.st","L.y","SD.st","ZK.nls","r.Freq.y","Linf.pr","Linf.sd.pr","MK.pr")
      jags.params <- c("GLmean.d","SD.d","SL","xN","FK.d","MK.d","Linf.d","Freq.pred")
      
      #---------------------------
      # JAGS model L-based with integral
      #---------------------------
      sink("SLNMod.jags")
      cat("
      model {
      GLmean.tau <- pow(0.1*GLmean.st,-2) 
      GLmean.d   ~ dnorm(GLmean.st,GLmean.tau)
      
      SD.tau    <- pow(0.2*SD.st,-2)
      SD.d      ~ dnorm(SD.st,SD.tau)
      
      MK.d_tau  <-pow(0.15,-2)
      MK.d      ~ dnorm(MK.pr,MK.d_tau)

      Linf.tau  <- pow(Linf.sd.pr,-2)
      Linf.d    ~ dnorm(Linf.pr,Linf.tau)
      
      FK        <- (ZK.nls-1.5) # ZK overestimated in gillnet selection, used as upper range
      FK.d      ~ dunif(0,FK)  

      SL[1]~ dlogis(0,1000)
      Freq.pred[1]<-0
      xN[1]<-1
      
      for(j in 2:n.L) {
        SL[j]<- exp(-((L.y[j]-GLmean.d)^2/(2*SD.d^2)))

        xN[j]<-xN[j-1]*exp((MK.d+FK.d*SL[j])*(log(1-L.y[j]/Linf.d)-log(1-L.y[j-1]/Linf.d)))
      
        cN[j] <- (xN[j-1]-xN[j])/(MK.d+FK.d*SL[j])

        Freq.pred[j]<-cN[j]*SL[j]
      
        #><> add effective sample size (try 100 typical for LF data)
        r.Freq.pred[j]<- Freq.pred[j]/sum(Freq.pred)*10000
      }	
      
      #><> LIKELIHOOD FUNCTION
      #><> Fit observed to predicted LF data using a Dirichlet distribution (more robust in JAGS)
      r.Freq.y[2:n.L]~ddirch(r.Freq.pred[2:n.L])  

   } # END OF MODEL
      ",fill = TRUE)
      sink()
      
      MODEL = "SLNMod.jags"
      #jagsfitSLN <- jags(jags.data, inits=NULL, jags.params, paste(MODEL), n.chains = Nchains , n.thin =Nthin , n.iter =Niter , n.burnin = Nburnin)
      
      jagsfitSLN <- jags.parallel(data=jags.data, working.directory=NULL, inits=NULL, 
                                  parameters.to.save=jags.params, 
                                  model.file=paste(MODEL), 
                                  n.burnin=300, n.thin=10, n.iter=1000, n.chains=3)
      
      jagsFit<-c(jagsFit,jagsfitSLN$BUGSoutput$pD) #modification by GP to select the best year according to the Deviance information criterion
      
      # use median and percentiles
      Ldat$Year[i]     <- Year
      Ldat$GLmean[i]    <- median(jagsfitSLN$BUGSoutput$sims.list$GLmean.d)
      Ldat$GLmean.lcl[i]<- quantile(jagsfitSLN$BUGSoutput$sims.list$GLmean.d,0.025)
      Ldat$GLmean.ucl[i]<- quantile(jagsfitSLN$BUGSoutput$sims.list$GLmean.d,0.975)
      Ldat$SD[i]        <- median(jagsfitSLN$BUGSoutput$sims.list$SD.d)
      Ldat$SD.lcl[i]    <- quantile(jagsfitSLN$BUGSoutput$sims.list$SD.d,0.025)
      Ldat$SD.ucl[i]    <- quantile(jagsfitSLN$BUGSoutput$sims.list$SD.d,0.975)
      Ldat$MK[i]        <- median(jagsfitSLN$BUGSoutput$sims.list$MK.d)
      Ldat$MK.lcl[i]    <- quantile(jagsfitSLN$BUGSoutput$sims.list$MK.d,0.025)
      Ldat$MK.ucl[i]    <- quantile(jagsfitSLN$BUGSoutput$sims.list$MK.d,0.975)
      Ldat$FK[i]        <- median(jagsfitSLN$BUGSoutput$sims.list$FK.d)
      Ldat$FK.lcl[i]    <- quantile(jagsfitSLN$BUGSoutput$sims.list$FK.d,0.025)
      Ldat$FK.ucl[i]    <- quantile(jagsfitSLN$BUGSoutput$sims.list$FK.d,0.975)
      FMi               <- jagsfitSLN$BUGSoutput$sims.list$FK.d/jagsfitSLN$BUGSoutput$sims.list$MK.d
      Ldat$FM[i]        <- median(FMi)
      Ldat$FM.lcl[i]    <- quantile(FMi,0.025)
      Ldat$FM.ucl[i]    <- quantile(FMi,0.975)
      ZKi               <- jagsfitSLN$BUGSoutput$sims.list$MK.d + jagsfitSLN$BUGSoutput$sims.list$FK.d
      Ldat$ZK[i]        <- median(ZKi)
      Ldat$ZK.lcl[i]    <- quantile(ZKi,0.025)
      Ldat$ZK.ucl[i]    <- quantile(ZKi,0.975)
      Ldat$r.Lopt[i]    <- 3/(3+Ldat$MK[i])
      Ldat$Linf[i]      <- median((jagsfitSLN$BUGSoutput$sims.list$Linf.d))
      Ldat$Linf.lcl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$Linf.d,0.025)
      Ldat$Linf.ucl[i]  <- quantile(jagsfitSLN$BUGSoutput$sims.list$Linf.d,0.975)
      
    } # end of gillnet loop
    
    
    # get MSFD D3.3 indicators
    Ldat$L95[i]      <- wtd.quantile(x=L.y.mat,weights=r.Freq.y.mat,probs=c(0.95))
    Ldat$perc.mat[i] <-ifelse(Lm50.user>0,sum(r.Freq.y.mat[L.y.mat>Lm50.user])/sum(r.Freq.y.mat),NA)
    
    # create and store vectors for plotting fit to years
    r.L.y     <- L.y[L.y < Ldat$Linf[i]] / Ldat$Linf[i] 
    r.Freq.y  <- r.Freq.y[L.y < Ldat$Linf[i]]
    Freq.pred <- vector()
    for(k in 1:length(r.L.y)){
      Freq.pred[k] <- median(jagsfitSLN$BUGSoutput$sims.list$Freq.pred[,k])
    }
    Lfit[i,1][[1]] <- r.L.y
    Lfit[i,2][[1]] <- r.Freq.y
    Lfit[i,3][[1]] <- Freq.pred
  }
  LBB.obj[["Lfit"]]=Lfit
  LBB.obj[["model.outcomes"]]=as.data.frame(Ldat)
  LBB.obj[["model.param"]]$FK.pr=ifelse(GausSel==F,FK.pr,NA)
  LBB.obj[["model.param"]]$Lc.pr=Lc.pr
  LBB.obj[["model.param"]]$Lc.sd.pr=ifelse(GausSel==F,Lc.sd.pr,NA)
  LBB.obj[["model.param"]]$r.alpha.pr=ifelse(GausSel==F,r.alpha.pr,NA)
  LBB.obj[["model.param"]]$r.alpha.sd.pr=ifelse(GausSel==F,r.alpha.sd.pr,NA)
  LBB.obj[["jagsFit"]]=jagsFit
  
  return(LBB.obj)
  
}

Bad.years=function(LBB.fit.obj) {
  Years=LBB.fit.obj[["model.outcomes"]]$Year
  GausSel=LBB.fit.obj[["model.param"]]$GausSel
  by.table=data.frame(Year=Years, G.B=rep(NA,length(Years)),action=rep(NA,length(Years))) 
  if (GausSel==F) {
    for (i in 1:length(Years)) {
      if((LBB.fit.obj[["model.outcomes"]]$ZK[i]>25 || LBB.fit.obj[["model.outcomes"]]$ZK[i] < 0.9 || (LBB.fit.obj[["model.outcomes"]]$ZK[i]/median(LBB.fit.obj[["model.outcomes"]]$MK,na.rm=TRUE)) < 0.9 ||
          LBB.fit.obj[["model.outcomes"]]$r.Lopt[i] > 1 || LBB.fit.obj[["model.outcomes"]]$r.Lopt[i] < 0.3 || (LBB.fit.obj[["model.outcomes"]]$Lc[i]/median(LBB.fit.obj[["model.outcomes"]]$Lc,na.rm=TRUE)) > 1.8 ||
          (LBB.fit.obj[["model.outcomes"]]$Lc[i]/median(LBB.fit.obj[["model.outcomes"]]$Lc,na.rm=TRUE)) < 0.4 || LBB.fit.obj[["model.outcomes"]]$MK[i] <0)) {
        if(LBB.fit.obj[["model.outcomes"]]$MK[i] <0) {
          by.table[i,2]="NO"
          by.table[i,3]= paste("Unable to determine proper Lc, remove peaks of early juveniles 
            by setting Lcut.user or removing such years or set MK.user=1.5 or remove",Years[i])
        } else {
          by.table[i,2]="NO"
          by.table[i,3]= paste("Year",Years[i],"data unsuitable for LBB analysis.")}
      } else {
        by.table[i,2]="YES"
        by.table[i,3]= paste("Year",Years[i],"data suitable for LBB analysis.")       
      }
    }
  } else {
    by.table[,2]="YES"
    by.table[,3]= paste("Data suitable for LBB analysis.")       
  }
  return(by.table)
}


BH.fit <- function(LBB.fit.obj) {
  i = 0 # start counter
  df_ALL=LBB.fit.obj[["input.data"]]
  Years=sort(unique(df_ALL$Year))
  Ldat=LBB.fit.obj[["model.outcomes"]]
  nYears=length(unique(df_ALL$Year))
  GausSel=LBB.fit.obj[["model.param"]]$GausSel
  semelp=LBB.fit.obj[["model.param"]]$Semelparous
  
  #Lfit       <- matrix(list(),nYears,3) 
  for(Year in Years) {
    i = i+1 # i is the index of Year
    
    df=df_ALL[df_ALL$Year==Year,]
    names(df) <- c("Year","Length","Freq")
    # Error propagation, assuming that fractional uncertainties add in quadrature  
    
    rel.lcl <- sqrt(((Ldat$FM[i]-Ldat$FM.lcl[i])/Ldat$FM[i])^2+((Ldat$MK[i]-Ldat$MK.lcl[i])/Ldat$MK[i])^2+((Ldat$FK[i]-Ldat$FK.lcl[i])/Ldat$FK[i])^2+((Ldat$Linf[i]-Ldat$Linf.lcl[i])/Ldat$Linf[i])^2)
    rel.ucl <- sqrt(((Ldat$FM.ucl[i]-Ldat$FM[i])/Ldat$FM[i])^2+((Ldat$MK.ucl[i]-Ldat$MK[i])/Ldat$MK[i])^2+((Ldat$FK.ucl[i]-Ldat$FK[i])/Ldat$FK[i])^2+((Ldat$Linf.ucl[i]-Ldat$Linf[i])/Ldat$Linf[i])^2)   
    
    
    BH.list  <- BH(AllLength=unique(df$Length),Linf=Ldat$Linf[i],MK=Ldat$MK[i],FK=Ldat$FK[i],GausSel=GausSel,
                   selpar1=ifelse(GausSel==T,Ldat$GLmean[i]/Ldat$Linf[i],Ldat$Lc[i]/Ldat$Linf[i]),#Ldat$Lc[i]/Ldat$Linf[i],  
                   selpar2= ifelse(GausSel==T,Ldat$SD[i]/Ldat$Linf[i],Ldat$r.alpha[i]))#Ldat$r.alpha[i]) 
    
    if(semelp==F) {
      Ldat$BB0[i]  <- as.numeric(BH.list[1])
      Ldat$YR[i]   <- as.numeric(BH.list[2]) } else { 
        
        # for semelparous species, exclude yield and biomass above Lopt
        Ldat$BB0[i] <- sum(unlist(BH.list[3])[unlist(BH.list[6])<1.1*Ldat$r.Lopt[i]]) /
          sum(unlist(BH.list[4])[unlist(BH.list[6])<1.1*Ldat$r.Lopt[i]])
        Ldat$YR[i]  <- sum(unlist(BH.list[5])[unlist(BH.list[6])<1.1*Ldat$r.Lopt[i]])
      }
    
    Ldat$BB0.lcl[i] <- Ldat$BB0[i]-Ldat$BB0[i]*rel.lcl
    Ldat$BB0.ucl[i] <- Ldat$BB0[i]+Ldat$BB0[i]*rel.ucl
    Ldat$YR.lcl[i] <- Ldat$YR[i]-Ldat$YR[i]*rel.lcl
    Ldat$YR.ucl[i] <- Ldat$YR[i]+Ldat$YR[i]*rel.ucl
  }
  LBB.fit.obj[["model.outcomes"]]=Ldat
  
  return(LBB.fit.obj)
}

median.rfpoints=function(BH.fit.obj) {
  AllLength=unique(BH.fit.obj[["input.data"]][["Length"]])
  semelp=BH.fit.obj[["model.param"]]$Semelparous
  GausSel=BH.fit.obj[["model.param"]]$GausSel
  outcomes=data.frame(
    # get some reference points as median of time series
    Linf.med     = median( BH.fit.obj[["model.outcomes"]]$Linf),
    Linf.lcl    = median( BH.fit.obj[["model.outcomes"]]$Linf.lcl),
    Linf.ucl     = median( BH.fit.obj[["model.outcomes"]]$Linf.ucl),
    MK.med       = median(BH.fit.obj[["model.outcomes"]]$MK),
    MK.lcl      = median(BH.fit.obj[["model.outcomes"]]$MK.lcl),
    MK.ucl       =median(BH.fit.obj[["model.outcomes"]]$MK.ucl),
    FK.med      = median(BH.fit.obj[["model.outcomes"]]$FK),
    FK.lcl      = median(BH.fit.obj[["model.outcomes"]]$FK.lcl),
    FK.ucl       = median(BH.fit.obj[["model.outcomes"]]$FK.ucl),
    FM.med       = median(BH.fit.obj[["model.outcomes"]]$FM),
    FM.lcl      = median(BH.fit.obj[["model.outcomes"]]$FM.lcl),
    FM.ucl       =median(BH.fit.obj[["model.outcomes"]]$FM.ucl),
    ZK.med      = median(BH.fit.obj[["model.outcomes"]]$ZK),
    ZK.lcl       =median(BH.fit.obj[["model.outcomes"]]$ZK.lcl),
    ZK.ucl      = median(BH.fit.obj[["model.outcomes"]]$ZK.ucl),
    r.Lopt.med   = median(BH.fit.obj[["model.outcomes"]]$r.Lopt),
    BB0.med     = median(BH.fit.obj[["model.outcomes"]]$BB0),
    BB0.lcl      = median(BH.fit.obj[["model.outcomes"]]$BB0.lcl),
    BB0.ucl      = median(BH.fit.obj[["model.outcomes"]]$BB0.ucl),
    YR.med       = median(BH.fit.obj[["model.outcomes"]]$YR),
    YR.lcl       = median(BH.fit.obj[["model.outcomes"]]$YR.lcl),
    YR.ucl       = median(BH.fit.obj[["model.outcomes"]]$YR.ucl)
  )
  
  outcomes$Lopt.med     = outcomes$r.Lopt.med*outcomes$Linf.med
  # outcomes$Lc_opt.med   =  outcomes$Linf.med*(2+3* outcomes$FM.med)/((1+ outcomes$FM.med)*(3+ outcomes$MK.med))
  outcomes$Lc_opt.med   <- ifelse(semelp==F,outcomes$Linf.med*(2+3* outcomes$FM.med)/((1+ outcomes$FM.med)*(3+ outcomes$MK.med)),NA) 
  
  if(GausSel==F) {
    outcomes$Lc.med      = median(BH.fit.obj[["model.outcomes"]]$Lc)
    outcomes$r.alpha.med  <- median(BH.fit.obj[["model.outcomes"]]$r.alpha) } else {
      outcomes$GLmean.med   <- median(BH.fit.obj[["model.outcomes"]]$GLmean)
      outcomes$SD.med       <- median(BH.fit.obj[["model.outcomes"]]$SD) }
  
  BFM1B0.list  <- BH(AllLength=AllLength,Linf= outcomes$Linf.med,MK= outcomes$MK.med,FK= outcomes$MK.med,GausSel=GausSel,
                     selpar1=ifelse(GausSel==T, outcomes$r.Lopt.med,5/(2*(3+ outcomes$MK.med))),
                     selpar2=ifelse(GausSel==T, outcomes$SD.med/ outcomes$Linf.med, outcomes$r.alpha.med))
  if(semelp==F) {
    outcomes$BFM1B0       <- as.numeric(BFM1B0.list[1])
    outcomes$YRFM1        <- as.numeric(BFM1B0.list[2]) } else { 
      
      # for semelparous species which die at/after Lopt, the concept of Lc_opt and YR at F=M and Lc=Lc_opt does not apply
      # instead, Bmsy is assumed at 0.5 B0, following the Schaefer model which makes no assumptions about size or age structure
      outcomes$BFM1B0 <- 0.5
      outcomes$YRFM1  <- NA
    }
  
  if(semelp==T) {LmeanFM <- NA} else {
    
    if(GausSel==F) {
      LmeanFM      <- (2*  outcomes$Lc.med*  outcomes$MK.med+  outcomes$Linf.med)/(2*  outcomes$MK.med+1)} else {
        LmeanFM    <- (2*   BH.fit.obj[["model.param"]][["Lc.pr"]]*  outcomes$MK.med+  outcomes$Linf.med)/(2*  outcomes$MK.med+1)   } 
  }
  outcomes$LmeanFM        <-LmeanFM
  
  BH.fit.obj[["median.ref.points"]]=outcomes
  return(BH.fit.obj)
}

ploting.ts=function(BH.fit.obj,smooth.ts) {
  nYears=length(BH.fit.obj[["model.outcomes"]]$Year)
  GausSel=BH.fit.obj[["model.param"]]$GausSel
  
  if(smooth.ts==TRUE && nYears>=3) {
    if(GausSel==F) {
      ploting_ts= data.frame(
        Year           =BH.fit.obj[["model.outcomes"]]$Year,
        Linf.ts        = as.vector(ma(BH.fit.obj[["model.outcomes"]]$Linf)),
        Lmean.ts       = as.vector(ma(BH.fit.obj[["model.outcomes"]]$Lmean)),
        Lmean.lcl.ts   = as.vector(ma(BH.fit.obj[["model.outcomes"]]$Lmean.lcl)),
        Lmean.ucl.ts   = as.vector(ma(BH.fit.obj[["model.outcomes"]]$Lmean.ucl)),
        Lc.ts          = as.vector(ma(BH.fit.obj[["model.outcomes"]]$Lc)),
        Lc.lcl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$Lc.lcl)),
        Lc.ucl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$Lc.ucl)),
        r.alpha.ts     = as.vector(ma(BH.fit.obj[["model.outcomes"]]$r.alpha)),
        r.alpha.lcl.ts = as.vector(ma(BH.fit.obj[["model.outcomes"]]$r.alpha.lcl)),
        r.alpha.ucl.ts = as.vector(ma(BH.fit.obj[["model.outcomes"]]$r.alpha.ucl)),
        r.Lopt.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$r.Lopt)),
        L95.ts         = as.vector(ma(BH.fit.obj[["model.outcomes"]]$L95)),
        perc.mat.ts    = as.vector(ma(BH.fit.obj[["model.outcomes"]]$perc.mat)),
        FK.ts          = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FK)),
        FK.lcl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FK.lcl)),
        FK.ucl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FK.ucl)),
        FM.ts          = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FM)),
        FM.lcl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FM.lcl)),
        FM.ucl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FM.ucl)),
        ZK.ts          = as.vector(ma(BH.fit.obj[["model.outcomes"]]$ZK)),
        ZK.lcl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$ZK.lcl)),
        ZK.ucl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$ZK.ucl)),
        YR.ts          = as.vector(ma(BH.fit.obj[["model.outcomes"]]$YR)),
        YR.lcl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$YR.lcl)),
        YR.ucl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$YR.ucl)),
        BB0.ts         = as.vector(ma(BH.fit.obj[["model.outcomes"]]$BB0)),
        BB0.lcl.ts     = as.vector(ma(BH.fit.obj[["model.outcomes"]]$BB0.lcl)),
        BB0.ucl.ts     = as.vector(ma(BH.fit.obj[["model.outcomes"]]$BB0.ucl)))
    }
    if(GausSel==T) {
      ploting_ts= data.frame( 
        Year           =BH.fit.obj[["model.outcomes"]]$Year,
        Linf.ts        = as.vector(ma(BH.fit.obj[["model.outcomes"]]$Linf)),
        r.Lopt.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$r.Lopt)),
        L95.ts         = as.vector(ma(BH.fit.obj[["model.outcomes"]]$L95)),
        perc.mat.ts    = as.vector(ma(BH.fit.obj[["model.outcomes"]]$perc.mat)),
        FK.ts          = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FK)),
        FK.lcl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FK.lcl) ),
        FK.ucl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FK.ucl)),
        FM.ts          = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FM)),
        FM.lcl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FM.lcl) ),
        FM.ucl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$FM.ucl)),
        ZK.ts          = as.vector(ma(BH.fit.obj[["model.outcomes"]]$ZK)),
        ZK.lcl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$ZK.lcl) ),
        ZK.ucl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$ZK.ucl)),
        YR.ts          = as.vector(ma(BH.fit.obj[["model.outcomes"]]$YR)),
        YR.lcl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$YR.lcl)),
        YR.ucl.ts      = as.vector(ma(BH.fit.obj[["model.outcomes"]]$YR.ucl)),
        BB0.ts         = as.vector(ma(BH.fit.obj[["model.outcomes"]]$BB0)),
        BB0.lcl.ts     = as.vector(ma(BH.fit.obj[["model.outcomes"]]$BB0.lcl)),
        BB0.ucl.ts     = as.vector(ma(BH.fit.obj[["model.outcomes"]]$BB0.ucl)),
        GLmean.ts      =  as.vector(ma(BH.fit.obj[["model.outcomes"]]$GLmean)),
        GLmean.lcl.ts  =  as.vector(ma(BH.fit.obj[["model.outcomes"]]$GLmean.lcl)),
        GLmean.ucl.ts  =  as.vector(ma(BH.fit.obj[["model.outcomes"]]$GLmean.ucl)),
        SD.ts          =  as.vector(ma(BH.fit.obj[["model.outcomes"]]$SD)))
    }
  } else {
    if(GausSel==F) {
      ploting_ts= data.frame(
        Year           =BH.fit.obj[["model.outcomes"]]$Year,
        Linf.ts        = BH.fit.obj[["model.outcomes"]]$Linf,
        Lmean.ts       = BH.fit.obj[["model.outcomes"]]$Lmean,
        Lmean.lcl.ts       = BH.fit.obj[["model.outcomes"]]$Lmean.lcl,
        Lmean.ucl.ts       = BH.fit.obj[["model.outcomes"]]$Lmean.ucl,
        Lc.ts          = BH.fit.obj[["model.outcomes"]]$Lc,
        Lc.lcl.ts      = BH.fit.obj[["model.outcomes"]]$Lc.lcl,
        Lc.ucl.ts      = BH.fit.obj[["model.outcomes"]]$Lc.ucl,
        r.alpha.ts     = BH.fit.obj[["model.outcomes"]]$r.alpha,
        r.alpha.lcl.ts = BH.fit.obj[["model.outcomes"]]$r.alpha.lcl,
        r.alpha.ucl.ts = BH.fit.obj[["model.outcomes"]]$r.alpha.ucl,
        r.Lopt.ts      = BH.fit.obj[["model.outcomes"]]$r.Lopt,
        L95.ts         = BH.fit.obj[["model.outcomes"]]$L95,
        perc.mat.ts    = BH.fit.obj[["model.outcomes"]]$perc.mat,
        FK.ts          = BH.fit.obj[["model.outcomes"]]$FK,
        FK.lcl.ts      = BH.fit.obj[["model.outcomes"]]$FK.lcl,
        FK.ucl.ts      = BH.fit.obj[["model.outcomes"]]$FK.ucl,
        FM.ts          = BH.fit.obj[["model.outcomes"]]$FM,
        FM.lcl.ts      = BH.fit.obj[["model.outcomes"]]$FM.lcl,
        FM.ucl.ts      = BH.fit.obj[["model.outcomes"]]$FM.ucl,
        ZK.ts          = BH.fit.obj[["model.outcomes"]]$ZK,
        ZK.lcl.ts      = BH.fit.obj[["model.outcomes"]]$ZK.lcl,
        ZK.ucl.ts      = BH.fit.obj[["model.outcomes"]]$ZK.ucl,
        YR.ts          = BH.fit.obj[["model.outcomes"]]$YR,
        YR.lcl.ts      = BH.fit.obj[["model.outcomes"]]$YR.lcl,
        YR.ucl.ts      = BH.fit.obj[["model.outcomes"]]$YR.ucl,
        BB0.ts         = BH.fit.obj[["model.outcomes"]]$BB0,
        BB0.lcl.ts     = BH.fit.obj[["model.outcomes"]]$BB0.lcl,
        BB0.ucl.ts     = BH.fit.obj[["model.outcomes"]]$BB0.ucl)
    }
    if(GausSel==T) {
      ploting_ts= data.frame(
        Year           =BH.fit.obj[["model.outcomes"]]$Year,
        Linf.ts        = BH.fit.obj[["model.outcomes"]]$Linf,
        r.Lopt.ts      = BH.fit.obj[["model.outcomes"]]$r.Lopt,
        L95.ts         = BH.fit.obj[["model.outcomes"]]$L95,
        perc.mat.ts    = BH.fit.obj[["model.outcomes"]]$perc.mat,
        FK.ts          = BH.fit.obj[["model.outcomes"]]$FK,
        FK.lcl.ts      = BH.fit.obj[["model.outcomes"]]$FK.lcl,
        FK.ucl.ts      = BH.fit.obj[["model.outcomes"]]$FK.ucl,
        FM.ts          = BH.fit.obj[["model.outcomes"]]$FM,
        FM.lcl.ts      = BH.fit.obj[["model.outcomes"]]$FM.lcl,
        FM.ucl.ts      = BH.fit.obj[["model.outcomes"]]$FM.ucl,
        ZK.ts          = BH.fit.obj[["model.outcomes"]]$ZK,
        ZK.lcl.ts      = BH.fit.obj[["model.outcomes"]]$ZK.lcl,
        ZK.ucl.ts      = BH.fit.obj[["model.outcomes"]]$ZK.ucl,
        YR.ts          = BH.fit.obj[["model.outcomes"]]$YR,
        YR.lcl.ts      = BH.fit.obj[["model.outcomes"]]$YR.lcl,
        YR.ucl.ts      = BH.fit.obj[["model.outcomes"]]$YR.ucl,
        BB0.ts         = BH.fit.obj[["model.outcomes"]]$BB0,
        BB0.lcl.ts     = BH.fit.obj[["model.outcomes"]]$BB0.lcl,
        BB0.ucl.ts     = BH.fit.obj[["model.outcomes"]]$BB0.ucl,
        GLmean.ts      =  BH.fit.obj[["model.outcomes"]]$GLmean,
        GLmean.lcl.ts  =  BH.fit.obj[["model.outcomes"]]$GLmean.lcl,
        GLmean.ucl.ts  =  BH.fit.obj[["model.outcomes"]]$GLmean.ucl,
        SD.ts          =  BH.fit.obj[["model.outcomes"]]$SD)
    }
  }
  return(ploting_ts)
}

ggplot.lc= function(BH_rfpoints,ts.obj) {
  #----------------------------------------------
  #  Plot time series of Lc and Lmean
  #----------------------------------------------
  GausSel=BH_rfpoints[["model.param"]]$GausSel
  semelp=BH_rfpoints[["model.param"]]$Semelparous
  Lm50.user=BH_rfpoints[["model.param"]]$Lm50.user
  
  ref.points=BH_rfpoints[["median.ref.points"]]
  if (GausSel==F) {
    max.y=1.1*( max( c(ref.points$Lc_opt.med,ref.points$Lopt.med,ref.points$LmeanFM,ts.obj$Lmean.ts),na.rm = T)) 
    
    if (semelp==F) {
      if (Lm50.user==0) {
        x=ggplot()+geom_line(data=ts.obj, aes(x=Year, y=Lmean.ts,col="A"),size=2)+
          geom_point(data=ts.obj, aes(x=Year, y=Lmean.ts,col="A"),size=2)+
          geom_hline(aes(yintercept= ref.points$Lopt.med,linetype="B"),size=1,color="darkgreen")+
          geom_hline(aes(yintercept= ref.points$LmeanFM,linetype="C"),size=1,color="darkgreen")+
          geom_hline(aes(yintercept= ref.points$Lc_opt.med,linetype="D"),size=1,color="darkgreen")+
          scale_y_continuous(limits = c(0,max.y))+
          scale_color_manual(values=c("black"),labels=c("Lmean"))+
          scale_linetype_manual(values=c("solid","dotted","dashed"),labels=c("Lopt","F=M","Lc_opt"))+
          theme_bw()+ theme(legend.position="bottom")+labs(x="Year",y="Length (cm)",colour="",linetype="")} else {
            
            x=ggplot()+geom_line(data=ts.obj, aes(x=Year, y=Lmean.ts,col="A"),size=1)+
              geom_point(data=ts.obj, aes(x=Year, y=Lmean.ts,col="A"),size=2)+
              geom_hline(aes(yintercept= ref.points$Lopt.med,linetype="B"),size=1,color="darkgreen")+
              geom_hline(aes(yintercept= ref.points$LmeanFM,linetype="C"),size=1,color="darkgreen")+
              geom_hline(aes(yintercept= ref.points$Lc_opt.med,linetype="D"),size=1,color="darkgreen")+
              geom_hline(aes(yintercept= Lm50.user,linetype="E"),size=1,color="darkgreen")+
              scale_y_continuous(limits = c(0,max.y))+
              scale_color_manual(values=c("black"),labels=c("Lmean"))+
              scale_linetype_manual(values=c("solid","dotted","dashed","dotdash"),labels=c("Lopt","F=M","Lc_opt","Lm"))+
              theme_bw()+ theme(legend.position="bottom")+labs(x="Year",y="Length (cm)",colour="",linetype="")  
          }
    } else {
      if (Lm50.user==0) {
        
        x=ggplot()+geom_line(data=ts.obj, aes(x=Year, y=Lmean.ts,col="A"),size=2)+
          geom_point(data=ts.obj, aes(x=Year, y=Lmean.ts,col="A"),size=2)+
          geom_hline(aes(yintercept= ref.points$Lopt.med,linetype="C"),size=1,color="darkgreen")+
          scale_y_continuous(limits = c(0,max.y))+
          scale_color_manual(values=c("black"),labels=c("Lmean"))+
          scale_linetype_manual(values=c("dashed","solid","dotted"),labels=c("Lc_opt","Lopt","F=M"))+
          theme_bw()+ theme(legend.position="bottom")+labs(x="Year",y="Length (cm)",colour="",linetype="")} else {
            
            x=ggplot()+geom_line(data=ts.obj, aes(x=Year, y=Lmean.ts,col="A"),size=1)+
              geom_point(data=ts.obj, aes(x=Year, y=Lmean.ts,col="A"),size=2)+
              geom_hline(aes(yintercept= ref.points$Lopt.med,linetype="C"),size=1,color="darkgreen")+
              geom_hline(aes(yintercept= Lm50.user,linetype="E"),size=1,color="darkgreen")+
              scale_y_continuous(limits = c(0,max.y))+
              scale_color_manual(values=c("black"),labels=c("Lmean"))+
              scale_linetype_manual(values=c("dashed","solid","dotted","dotdash"),labels=c("Lc_opt","Lopt","F=M","Lm"))+
              theme_bw()+ theme(legend.position="bottom")+labs(x="Year",y="Length (cm)",colour="",linetype="")
          }
    }
  } else {
    median(BH_rfpoints[["model.outcomes"]]$r.Lopt)
    max.y=1.1*( max( c( median(BH_rfpoints[["model.outcomes"]]$r.Lopt)*ref.points$Linf.med,ts.obj$GLmean.ts),na.rm = T)) 
    
    if (Lm50.user==0) {
      x=ggplot()+geom_line(data=ts.obj, aes(x=Year, y=GLmean.ts,col="A"),size=2)+
        geom_hline(aes(yintercept= ref.points$Lopt.med,linetype="C"),size=1,color="darkgreen")+
        scale_y_continuous(limits = c(0,max.y))+
        geom_hline(aes(yintercept= ref.points$LmeanFM,linetype="D"),size=1,color="darkgreen")+
        scale_color_manual(values=c("black"),labels=c("Lmean"))+
        scale_linetype_manual(values=c("solid","dotted"),labels=c("Lopt","F=M"))+
        theme_bw()+ theme(legend.position="bottom")+labs(x="Year",y="Length (cm)",colour="",linetype="")} else {
          
          x=ggplot()+geom_line(data=ts.obj, aes(x=Year, y=GLmean.ts,col="A"),size=2)+
            geom_hline(aes(yintercept= ref.points$Lopt.med,linetype="C"),size=1,color="darkgreen")+
            scale_y_continuous(limits = c(0,max.y))+
            geom_hline(aes(yintercept= ref.points$LmeanFM,linetype="D"),size=1,color="darkgreen")+
            geom_hline(aes(yintercept= Lm50.user,linetype="E"),size=1,color="darkgreen")+
            scale_color_manual(values=c("black"),labels=c("Lmean"))+
            scale_linetype_manual(values=c("solid","dotted","dotdash"),labels=c("Lopt","F=M","Lm"))+
            theme_bw()+ theme(legend.position="bottom")+labs(x="Year",y="Length (cm)",colour="",linetype="")  
        }
  }
  return(x)
}  

ggplot.fm= function(ts.obj) {
  ts.obj$FM.lcl.ts[ts.obj$FM.lcl.ts<0]=0
  x=ggplot()+
    geom_ribbon(data=ts.obj, aes(x=Year,ymin=FM.lcl.ts, ymax=FM.ucl.ts,fill="A"),alpha=0.3)+
    geom_line(data=ts.obj, aes(x=Year, y=FM.ts,col="A"),size=1)+
    geom_point(data=ts.obj, aes(x=Year, y=FM.ts,col="A"),size=2)+
    geom_hline(aes(yintercept= 1,linetype="B"),size=1,col="darkgreen")+
    scale_color_manual(values=c("#F8766D"),labels=c("F/M"))+
    scale_fill_manual(values=c("#F8766D"),labels=c("F/M"))+
    scale_linetype_manual(values=c("solid"),labels=c("F=M"))+
    theme_bw()+ theme(legend.position="bottom")+
    scale_y_continuous(limits = c(0,NA))+
    labs(x="Year",y="F/M",colour=" ",fill=" ",linetype="")
  
  return(x)
}

ggplot.bb0= function(BH.fit.obj,ts.obj) {
  ref.points=BH.fit.obj[["median.ref.points"]]
  semelp=BH.fit.obj[["model.param"]]$Semelparous
  GausSel=BH.fit.obj[["model.param"]]$GausSel
  
  if (semelp==T) {
    txt_="proxy Bmsy for semelparous"} else {
      txt_="B F=M,Lc=opt"
    }
  
  ts.obj$BB0.lcl.ts[ts.obj$BB0.lcl.ts<0]=0
  if (GausSel==F) {
    x=ggplot()+
      geom_ribbon(data=ts.obj, aes(x=Year,ymin=BB0.lcl.ts, ymax=BB0.ucl.ts,fill="A"),alpha=0.3)+
      geom_line(data=ts.obj, aes(x=Year, y=BB0.ts,col="A"),size=1)+
      geom_point(data=ts.obj, aes(x=Year, y=BB0.ts,col="A"),size=2)+
      geom_hline(aes(yintercept= 1,linetype="B"),size=1,col="darkgreen")+
      geom_hline(aes(yintercept= ref.points$BFM1B0,linetype="C"),size=1,color="darkgreen")+
      geom_hline(aes(yintercept= ref.points$BFM1B0/2,linetype="D"),size=1,color="red")+
      scale_color_manual(values=c("#619CFF"),labels=c("B/B0"))+
      scale_fill_manual(values=c("#619CFF"),labels=c("B/B0"))+
      scale_linetype_manual(values=c("solid","dashed","dotted"),labels=c("B0",txt_,"proxy 0.5 Bmsy"))+
      theme_bw()+ theme(legend.position="bottom")+
      labs(x="Year",y="B/B0",colour="",fill="",linetype=" ")+
      scale_y_continuous(limits = c(0,NA))+
      guides(linetype = guide_legend(override.aes = list(color = c("darkgreen","darkgreen","#F8766D"))))
  } else {
    x=ggplot()+
      geom_ribbon(data=ts.obj, aes(x=Year,ymin=BB0.lcl.ts, ymax=BB0.ucl.ts,fill="A"),alpha=0.3)+
      geom_line(data=ts.obj, aes(x=Year, y=BB0.ts,col="A"),size=1)+
      geom_point(data=ts.obj, aes(x=Year, y=BB0.ts,col="A"),size=2)+
      geom_hline(aes(yintercept= 1,linetype="B"),size=1,col="darkgreen")+
      #geom_hline(aes(yintercept= ref.points$BB0.med,linetype="C"),size=1,color="darkgreen")+
      geom_hline(aes(yintercept= 0.5,linetype="D"),size=1,color="darkgreen")+
      geom_hline(aes(yintercept= 0.25,linetype="E"),size=1,color="red")+
      scale_color_manual(values=c("#619CFF"),labels=c("B/B0"))+
      scale_fill_manual(values=c("#619CFF"),labels=c("B/B0"))+
      scale_linetype_manual(values=c("solid","dashed","dotted"),labels=c("B0","proxy 0.5 Bmsy","Blim"))+
      theme_bw()+ theme(legend.position="bottom")+
      labs(x="Year",y="B/B0",colour="",fill="",linetype=" ")+
      scale_y_continuous(limits = c(0,NA))+
      guides(linetype = guide_legend(override.aes = list(color = c("darkgreen","darkgreen","#F8766D"))))  
  }
  return(x)
}

ggplot.histMSFD= function(BH.fit.obj) {
  df=BH.fit.obj[["input.data"]]
  df=df[df$Year==max(df$Year),]
  colnames(df)[3]="Freq"
  #df=AG(df)
  colnames(df)
  ggplot()+geom_bar(data=df,aes(x=Length,y=Freq),color="black",fill="#F8766D",  stat = "identity")+
    scale_x_continuous(limits = c(0,NA))+labs(y="Catches")+
    theme_bw()
  
}
#----------------------------------------------
#  FUNCTIONS ----
#----------------------------------------------
# 
#----------------------------------------------




template_LBB_data_clnms =c(colnames(test_LBB_data))
template_LBB_data = data.frame(matrix(nrow = 0, ncol = length(template_LBB_data_clnms)))
colnames(template_LBB_data) = template_LBB_data_clnms

template_LBB_ID_clnms = colnames(test_LBB_ID)
template_LBB_ID = data.frame(matrix(nrow = 0, ncol = length(template_LBB_ID_clnms)))
colnames(template_LBB_ID) = template_LBB_ID_clnms

shinyUI <- shinydashboard::dashboardPage(#skin = "purple",
  shinydashboard::dashboardHeader(title = "LBB"),
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
      shinydashboard::menuItem("2. Priors", tabName = 'priors', icon = shiny::icon("magnifying-glass")),
      shinydashboard::menuItem("3. Prepare data", tabName = 'Pdata', icon = shiny::icon("wrench")),
      shinydashboard::menuItem("4. Run the LBB model", tabName = 'run_model', icon = shiny::icon("play")),
      shinydashboard::menuItem("5. Run the Yield/Recruit model", tabName = 'run_model2', icon = shiny::icon("square-poll-vertical")),
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
                                column(width=8,offset=0.5,shinydashboard::box(title = "LBB",  solidHeader = TRUE,status="primary",
                                                                              shiny::h4( "LBB is a Bayesian approach to estimate relevant parameters from length-frequency data that are representative of the lengths caught by commercial gears. It then uses yield-per-recruit equations to estimate relative stock size (B/B0)"),
                                                                              width=10))),
                              shiny::fluidRow(
                                column(width=8,shinydashboard::box(title = "Step-by-Step process", solidHeader = TRUE,status="primary",
                                                                   shiny::h4( "1. Either select one of the existing stocks to assess or load your own data"),
                                                                   shiny::h4( "2. Explore and decide about priors"),
                                                                   shiny::h4( "3. Prepare your data"),
                                                                   shiny::h4( "4. Run the LBB model"),
                                                                   shiny::h4( "5. Run the the Yield/Recruit model and export the results"),width=10),offset=5)),
  
                               # headerPanel(
                              # HTML('<p><shiny::img src="Picture2.png"/></p>')
                              shiny::fluidRow(align="center",  #system.file("image","Picture2.png",package = "triton")
                              # img(src="lbb.jpg", height="90%", width="90%")
                                shinydashboard::box(img(
                                  src = "lbb.png",
                                  alt = "Short description of what's in the pic",
                                  width = 700, height = 400
                                ),
                                shiny::h5( HTML(paste0("Conceptual framework for the analysis of length frequency data from the commercial fishery, here modelled with life history traits
of Atlantic cod (", tags$em("Gadus morhua"),"). The upper green curve shows the decline in cohort numbers without fishing, with a question mark indicating that
mortality and thus numbers of individuals not yet vulnerable to the gear are unknown and not relevant for the method. The blue curve
descending from Lx shows the decline with fishing, the upper yellow humped curve shows the fish vulnerable to the gear, and the lower red humped
curve is the catch in numbers resulting from a given fishing effort. The vertical dashed lines indicate the length (Lx) where fish become
vulnerable to the gear, the length (Lc) where 50% of the individuals are retained by the gear, the length (Lopt) where the unexploited cohort
would have maximum biomass, and the asymptotic length (Linf)."))),background = "light-blue",
                                    align="center", width = 12)
                               ),
                              shiny::fluidRow(align="center",  #system.file("image","Picture2.png",package = "triton")
                                # img(src="lbb.jpg", height="90%", width="90%")
                                shinydashboard::box(img(
                                  src = "lbb2.png",
                                  alt = "Short description of what's in the pic",
                                  width = 700, height = 400
                                ),
                                shiny::h5( HTML(paste0("Schematic representation of the length frequency distribution in commercial catch with trawl-like selectivity, with indication of the sections that are subject to no gear selection (dotted red curve), partial gear selection (solid red curve), and full gear selection (dashed red curve). LBB fits the red curve to length-frequency data and estimates Linf, Lc, Lopt, and Z/K (total mortality rate relative to somatic growth rate) from the red curve. With these parameters, Beverton and Holt's Yield/Recruit equations can be used to estimate relative stock size B/B0." ))),background = "light-blue",
                                align="center", width = 12)
                              ), 
                              
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
                                                                                        textInput("Sp_Scient", "Scientific"),
                                                                                        textInput("Sp_Comm", "Common name"),
                                                                                        tags$b("Semelparous species"),
                                                                                        linebreaks(1),
                                                                                           prettyToggle(
                                                                                          inputId = "semelparous",
                                                                                          label_on = "Yes", 
                                                                                          label_off = "No",
                                                                                          value = FALSE
                                                                                        ),
                                                                            helpText("Set to 'Yes' if species is semelparous (i.e. eels, cephalopods, salmon), otherwise 'No'."),
                                                                                        ),
                                                                               width = 12)),
                                ),
                                column(width = 6,
                                       shinydashboard::valueBoxOutput("Sps_Info",width = 12)
                                )
                              ),
                              shiny::fluidRow(
                                conditionalPanel(condition ="input.Upld_butt_1",
                                                 tags$br(),
                                                 shinydashboard::valueBoxOutput("adv_up2",width =12))
                              ),
                              shiny::fluidRow(
                                column(width=4,
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
                                                                            shinyWidgets::pickerInput(inputId = "Length_col",label = "Length column",
                                                                                                      choices = c(""),options = list(
                                                                                                        title = "Define the Length column")),
                                                                            shinyWidgets::pickerInput(inputId = "N_col",label = "CatchNo column",
                                                                                                      choices = c(""),options = list(
                                                                                                        title = "Define the CatchNo column")),
                                                                            shinyWidgets::pickerInput(inputId = "unit_col",label = "State length unit used",
                                                                                                      choices = c("mm","cm"),selected="",options = list(
                                                                                                        title = "mm or cm")),
                                                                            conditionalPanel(condition ="input.unit_col!=''" ,# (input.bt_col!=''| input.is_there_biomass=='Catch')
                                                                             numericInput(inputId="length_width",label="State width of length class",value=10,min = 1)),
                                                                            #helpText("Note: if Yes, then in your input .csv you should have a column with cpue cv."),
                                                                            width=12),
                                                        conditionalPanel(condition ="input.Yr_col!='' & input.Length_col!=''& input.N_col!='' & input.unit_col!=''" ,# (input.bt_col!=''| input.is_there_biomass=='Catch')
                                                                         shinyWidgets::materialSwitch(
                                                                           inputId = "Upld_butt_3",
                                                                           label = " Continue",
                                                                           status = "success"
                                                                         ))),
                                column(width=8,
                                       shiny::fluidRow(
                                                          shinydashboard::box(title = "Loaded data", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                                                                              DT::dataTableOutput("contents"),width=12))
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
                                                                                                     no_outline=F,block=F,color="success"),
                                                                            width = 12))
                                ),
                                column(width = 8,
                                       conditionalPanel(condition = "input.Upld_butt_3",
                                                        shinydashboard::box(title = "Raw data plot", solidHeader = TRUE,status = "primary",
                                                                            shiny::plotOutput("LBB_plot1"),width = 12))),#,height=300
                              ),
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
                                       shinydashboard::box(title = "Explore existing stocks", solidHeader = TRUE,status = "primary",background = "light-blue",
                                                           tags$b("Semelparous species"),
                                                           linebreaks(1),
                                                           prettyToggle(
                                                             inputId = "semelparous2",
                                                             label_on = "Yes", 
                                                             label_off = "No",
                                                             value = FALSE
                                                           ),
                                                           tags$b("Set to 'Yes' if species is semelparous (e.g. eels, cephalopods, salmon), otherwise 'No'."),
                                                           #helpText("Set to 'Yes' if species is semelparous (i.e. eels, cephalopods, salmon), otherwise 'No'."),
                                                           textInput("txt1", shiny::h4("Select stock:"), ""),
                                                           shinyWidgets::actionBttn(inputId="button_1",label =" Create stock object",
                                                                                    style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                    no_outline=F,block=F,color="success"),
                                                           width=12),
                                       conditionalPanel(condition = "input.button_1",
                                                        shinydashboard::valueBoxOutput("Stock_infobox_2",width = 12))
                                ),
                                column(width = 8,
                                       shiny::fluidRow(shinydashboard::box(title = "Existing stocks", solidHeader = TRUE,status = "primary",
                                                                           DT::dataTableOutput("Exist_Stcks_tble"),width = 12)),
                                       shiny::fluidRow(
                                         shinydashboard::valueBoxOutput("txtout1",width =12)),
                                       shiny::fluidRow(
                                         shinydashboard::box(title = "LFD plot", solidHeader = TRUE,
                                                             shiny::plotOutput("LBB_plot2"),width = 12)#,,height=300
                                       )))
      ),
      
      ################################################################
      ###################### PAGE 5 PRIOR ENTRY  #####################
      ################################################################
      shinydashboard::tabItem(tabName = "priors",
                              # shiny::fluidRow(
                              #   conditionalPanel(condition = "input.button_1 |input.button_2",
                              #                    shinydashboard::valueBoxOutput("Stock_infobox_patience",width = 12))),
                              shiny::fluidRow( 
                                conditionalPanel(condition = "input.button_1 |input.button_2",uiOutput("Stock_infobox_3",width = 12))),
                               shiny::fluidRow(
                                 column(width = 4,
                                conditionalPanel(condition ="input.button_1 |input.button_2",
                                                 shinydashboard::box(collapsible = F,
                                                                     # uiOutput("cond_ifmany"),
                                                                     shinyWidgets::actionBttn(inputId="procc_rpriors",label ="start",
                                                                                              style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                              no_outline=F,block=F,color="primary"),
                                                                     shinyBS::bsTooltip("procc_rpriors", title="Press to proceed to the next step",
                                                                                        placement = "bottom", trigger = "hover",
                                                                                        options = NULL),width = 12)
                                                 )
                                ),
                                column(width = 4,offset = 4,
                                conditionalPanel(condition ="input.procc_rpriors",
                                                 #shinydashboard::valueBoxOutput("Stock_infobox_patience",width = 4),
                                                 shinydashboard::box(collapsible = F,align="center",background="green",
                                                                     # uiOutput("cond_ifmany"),
                                                                     shiny::h4("Press 'Connect' to establish connection to FishBase or SeaLifeBase to get information on Lmax and Linf for your stock"),
                                                                     shinyWidgets::actionBttn(inputId="con_fbase",label ="Connect",
                                                                                              style = "unite",size = "md",icon = shiny::icon("info"),
                                                                                              no_outline=F,block=F,color="success"),
                                                                     shinyBS::bsTooltip("con_fbase", title="Press to proceed to the next step",
                                                                                        placement = "bottom", trigger = "hover",
                                                                                        options = NULL),width = 12
                                                 )))
                              ),
                              shiny::fluidRow(
                                conditionalPanel(condition = "input.con_fbase",shiny::h3("Info from FishBase/SeaLifeBase (wait for connection...)"))),
                              shiny::fluidRow(
                                conditionalPanel(condition = "input.con_fbase",
                                                 shinydashboard::valueBoxOutput("FB_resil",width =12),
                                                 shinydashboard::box(title = "List of population characteristics", solidHeader = TRUE,status = "success",collapsible = TRUE,
                                                                     shinycssloaders::withSpinner(DT::dataTableOutput("inf_fishBase_1")),width=5),
                                                                     shinydashboard::box(title = "List of growth parameters", solidHeader = TRUE,status = "success",collapsible = TRUE,
                                                                                         shinycssloaders::withSpinner(DT::dataTableOutput("inf_fishBase_2")),width=7))),
                                shiny::fluidRow(
                                  conditionalPanel(condition = "input.procc_rpriors",
                                 shinydashboard::box(collapsible = F,
                                 uiOutput("someinfo"),
                                  numericInput(inputId="Linf.user",label="Set Linf prior in cm",value=0,min = 0),
                               # shiny::h4("Linf unit: "),
                                # shinyWidgets::pickerInput(inputId = "Linf_unit",label = "Length mesurement unit",
                                #                           choices = c("mm","cm"),selected="",options = list(
                                #                             title = "mm or cm")),
                                sliderInput(inputId = "Linf_CV", label = "Set Linf CV",
                                            min = 0.01, max = 1, value =0.1, step =0.01)
                                ,width = 6)),#)
                                conditionalPanel(condition = "input.procc_rpriors",
                                                 shinydashboard::box(collapsible = F,
                                                                     uiOutput("someinfomk"),
                                                                     sliderInput(inputId = "MK.user", label = "Set M/K prior",
                                                                                 min = 0.5, max = 2.5, value =1.5, step =0.1),
                                             # helpText("Some guidance!!!!!!probably:"),
                                              
                                                                     sliderInput(inputId = "MK_CV", label = "Set M/K CV",
                                                                                 min = 0.01, max = 0.5, value =0.05, step =0.01)
                                                                     ,width = 6))),
                              
                              shiny::fluidRow(
                                column(width = 4,offset = 8,
                                conditionalPanel(condition ="input.procc_rpriors",
                                            #     shinydashboard::valueBoxOutput("Stock_infobox_maturity",width = 4),
                                                 shinydashboard::box(collapsible = F,align="center",background="green",
                                                                     # uiOutput("cond_ifmany"),
                                                                     shiny::h4("Press 'Connect' to establish connection to FishBase or SeaLifeBase to get information on Lm for your stock"),
                                                                     shinyWidgets::actionBttn(inputId="con_fbase_mat",label ="Connect",
                                                                                              style = "unite",size = "md",icon = shiny::icon("info"),
                                                                                              no_outline=F,block=F,color="success"),
                                                                     shinyBS::bsTooltip("con_fbase_mat", title="Press to proceed to the next step",
                                                                                        placement = "bottom", trigger = "hover",
                                                                                        options = NULL),width = 12
                                                 )))         
                              ),
                              shiny::fluidRow(
                                conditionalPanel(condition = "input.con_fbase_mat",shiny::h3("Info from FishBase/SeaLifeBase (wait for connection...)"))),
                              
                              
                              shiny::fluidRow(
                                column(width = 6,
                                       conditionalPanel(condition = "input.procc_rpriors",
                                                 shinydashboard::box(collapsible = F,
                                                                     tags$b("Optional:"), " You can set the length where 50% of the individuals of the larger sex reach maturity (have fully functional gonads with indication of active spawning during the spawning season). This will then be used to calculate the proportion of mature individuals in the catch. You can use info from FishBase/SeaLifeBase for your area or for adjacent or similar areas to select a reasonable Lm value for your stock.",
                                                                     linebreaks(2),
                                                                      numericInput(inputId="Lm50.user",label="Set Lm in cm",value=0,min = 0),
                                                                     width =12),
                                        shinydashboard::valueBoxOutput("success_priors",width =12))
                                       ),
                                column(width = 6,
                                       conditionalPanel(condition = "input.con_fbase_mat", shinydashboard::box(title = "List of maturity parameters", solidHeader = TRUE,status = "success",collapsible = TRUE,
                                                           shinycssloaders::withSpinner(DT::dataTableOutput("inf_maturity")),width=12)
                                       )
                                       
                                          )
                                )
                              ),
      ################################################################
      ###################### PAGE 4 Prepare Data  ####################
      ################################################################
      tabItem(tabName = "Pdata",
              fluidRow( h3("Explore and prepare the data")),
              fluidRow(
                conditionalPanel(condition = "input.procc_rpriors",
                                 shinydashboard::box(collapsible = F,
                                                     shinyWidgets::actionBttn(inputId="prepare_data",label ="Start",
                                                                              style = "unite",size = "md",icon = icon("paper-plane"),
                                                                              no_outline=F,block=F,color="primary"),
                                                     shinyBS::bsTooltip("prepare_data", title="Press to proceed to the next step",
                                                                        placement = "bottom", trigger = "hover",
                                                                        options = NULL),
                                                     linebreaks(2),
                                                     shiny::h4(tagList("Notes on Data preparation: ", actionLink(inputId = "Notes_LBB", label = "Link"))),
                                                     # shinyWidgets::actionBttn(inputId="Notes_LBB",label ="Notes on Data preparation",
                                                     #                          style = "unite",size = "sm",icon = shiny::icon("info"),
                                                     #                          no_outline=F,block=F,color="warning"),
                                                     width = 4
                                 )),
                conditionalPanel(condition = "input.prepare_data",valueBoxOutput("Stock_infobox_prepare",width = 8))
              ),
              fluidRow(  conditionalPanel(condition = "input.prepare_data", 
                                          h3("Prepare LBB data"))),
              fluidRow(
                column(width=4,
                       conditionalPanel(condition = "input.prepare_data", 
                                        shinydashboard::box(collapsible = F, 
                                                            tags$b("Note: from here on all lengths will be in cm"),
                                                            materialSwitch(
                                                              inputId = "Change_view",
                                                              label = "Select graph type", 
                                                              value = FALSE,
                                                              status = "success"
                                                            ),
                                                            materialSwitch(
                                                              inputId = "Scale_switch",
                                                              label = "Absolute or relative scaling", 
                                                              value = TRUE,
                                                              status = "success"
                                                            ),
                                                            linebreaks(1),
                                                            sliderInput(inputId = "LFDs_per_page", label = "LFDs per page",
                                                            min = 1, max = 10, value =4, step =1),
                                                            linebreaks(1),
                                                              awesomeRadio(
                                                              inputId = "LBB_pages",
                                                              label = "Navigate through the plots", 
                                                              choices = "page 1",
                                                              selected = "page 1",#yr[1],
                                                              inline = FALSE, 
                                                              status = "success"
                                                            ),width = 12)),
                       conditionalPanel(condition = "input.prepare_data", 
                        shinydashboard::box(collapsible = F, 
                                           tags$b("Change length class width (multiples)"),
                                             shinyWidgets::prettySwitch(
                                                              inputId = "Changewidthswitch",
                                                              slim = T,
                                                              label = "Edit",
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.Changewidthswitch",
                                                            sliderInput(inputId = "Change_width", label = "Change length class width",
                                                                        min =1, max = 10, value =1, step =1)),
                                                            #helpText("Note: ......"),
                                           tags$b("Select if you want to exclude catch at small lenghts."),
                                              shinyWidgets::prettySwitch(
                                                              inputId = "Lcutswitch",
                                                              slim = T,
                                                              label = "Edit",
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.Lcutswitch",
                                                             sliderInput(inputId = "L.cut", label = "L cut",
                                                                        min =0, max = 500, value =0, step =1)),
                                           linebreaks(1),
                                           tags$b("Click swich to merge LFs in subsequent years (the first year is identical with the second)."),
                                           switchInput(
                                             inputId = "MergeLF",
                                             onStatus = "success", 
                                             value = FALSE,
                                             size='small',
                                             offStatus = "danger"),
                                           conditionalPanel(condition = "input.MergeLF", 
                                           awesomeRadio(
                                             inputId = "LBB_pages_merg",
                                             label = "Navigate through the plots", 
                                             choices = "page 1",
                                             selected = "page 1",#yr[1],
                                             inline = FALSE, 
                                             status = "success"
                                           )),
                                                            width =12))),
                column(width=8,    
                       conditionalPanel(condition = "input.prepare_data", 
                                        shinydashboard::box(collapsible = F, 
                                                            shinycssloaders::withSpinner(shiny::plotOutput(outputId = "Check_LBB_plot")),
                                                           # uiOutput("Year_Selection"),
                                                            width =12),
                                        shinydashboard::box(collapsible = F, 
                                         uiOutput("Year_Selection"),
                                         width =12),
                                        conditionalPanel(condition = "input.MergeLF", 
                                                         shinydashboard::box(collapsible = F, 
                                                                             shinycssloaders::withSpinner(shiny::plotOutput(outputId = "MergeLF_plot")),
                                                                             tags$b("Probably we need some text explaining what we do here, why y axis scale changed etc..."),
                                                                             width =12))                   
                                        ))),
                fluidRow(
                  column(width=4,    
                         conditionalPanel(condition = "input.prepare_data", 
                                          shinydashboard::box(collapsible = F,
                                                              radioGroupButtons(
                                                                inputId = "GausSel",
                                                                label = "Define the selection pattern of the gear used",
                                                                # choices = c("Trawl-like selection", 
                                                                #             "Gillnet-like selection"),
                                                                choiceNames = c("Trawl-like selectivity", 
                                                                                "Gillnet-like selectivity"),
                                                                choiceValues = c(FALSE, 
                                                                                 TRUE),
                                                                selected =FALSE,
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                  yes = tags$i(class = "fa fa-circle", 
                                                                               style = "color: green"),
                                                                  no = tags$i(class = "fa fa-circle-o", 
                                                                              style = "color: red"))
                                                              ), 
                                                              shiny::h4(tagList("Selectivity patterns: ", actionLink(inputId = "Selectivity_patt", label = "Link"))),
                                                              
                                                              width = 12)),
                         conditionalPanel(condition = "input.prepare_data",  
                                          shinydashboard::box(collapsible = F,
                                          #  tags$b("Display aggregated data with priors"),
                                          # linebreaks(1),
                                          shinyWidgets::actionBttn(inputId="make_ag_lfd",label ="Display aggregated data",
                                                                   style = "unite",size = "md",icon = icon("paper-plane"),
                                                                   no_outline=F,block=F,color="primary"), 
                                          linebreaks(1),
                                          conditionalPanel(condition = "input.make_ag_lfd",  
                                          conditionalPanel(condition = "input.GausSel=='FALSE'",  
                                                              tags$b("Select the length where full selectivity starts"),
                                                              helpText("Note: set L.full such that blue line best matches decline of right-hand data points."),
                                                              shinyWidgets::prettySwitch(
                                                                inputId = "Lstartuserswitch",
                                                                slim = T,
                                                                label = "Edit",
                                                                value = FALSE,status = "info" ),
                                                              conditionalPanel(condition = "input.Lstartuserswitch",
                                                                               sliderInput(inputId = "Lstart.user", label = "",
                                                                                           min = 0, max = 500, value =1, step =1)),
                         tags$b("If necessary, change the Lc.prior manually to the length where 50% of the individuals are retained by the gear"),
                        # helpText("Note: set L.full such that blue line best matches decline of right-hand data points."),
                         shinyWidgets::prettySwitch(
                           inputId = "Lcuserswitch",
                           slim = T,
                           label = "Edit",
                           value = FALSE,status = "info" ),
                         conditionalPanel(condition = "input.Lcuserswitch",
                                          sliderInput(inputId = "Lc.user", label = "",
                                                      min = 0, max = 500, value =1, step =1)),width=12)),
                        conditionalPanel(condition = "input.GausSel=='TRUE'",  
                                                             tags$b("Set Lmean such that blue line best matches the left-hand data points"),
                                                            # helpText("Note: set L.mean such that blue line best matches the increase of left-hand data points."),
                                                             shinyWidgets::prettySwitch(
                                                               inputId = "Lmeanswitch",
                                                               slim = T,
                                                               label = "Edit",
                                                               value = FALSE,status = "info" ),
                                                             conditionalPanel(condition = "input.Lmeanswitch",
                                                                              sliderInput(inputId = "Lmean.user", label = "",
                                                                                          min = 0, max = 500, value =1, step =1)),
                                                            tags$b("Set sd for Lmean"),
                                                             shinyWidgets::prettySwitch(
                                                              inputId = "Lmeanswitch_CV",
                                                              slim = T,
                                                              label = "Edit",
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.Lmeanswitch_CV",
                                                                             sliderInput(inputId = "LmeanCV.user", label = "",
                                                                                         min = 0, max = 500, value =1, step =1))
                                                            
                                                             ), width=12)
                        ),
                         conditionalPanel(condition = "input.make_ag_lfd", 
                                          shinydashboard::box(collapsible = F,
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
                                                               width = 12))#,
                         # conditionalPanel(condition = "input.Save_priors",
                         #                  shinydashboard::valueBoxOutput("You_can_proceed_to_run",width = 12))
                         ),
                  column(width=8,    
                         conditionalPanel(condition = "input.make_ag_lfd", 
                                          shinydashboard::box(collapsible = F, 
                                                              shinycssloaders::withSpinner(shiny::plotOutput(outputId = "Check_AG_LBB_plot")),
                                                              width =12),
                                          conditionalPanel(condition = "input.Save_priors", 
                                          shinydashboard::valueBoxOutput("When_happy",width = 12))
                                          ))),
              shinyBS::bsModal(id='modalExample',
                               title ='Notes on data preparation for LBB',
                               trigger="Notes_LBB",
                               size = "large",
                              # shiny::plotOutput('PriorPosterior'),
                              shiny::h5(tagList(
                                "The LBB model tries to estimate key population parameters (Linf, Lc/Linf, M/K, F/K) from annual length frequency data (LFD) that are representative of the main gear used in the fisheries and that proportionally cover all length classes present in the exploited part of the stock. Such LFD plot should show on the left side an S-shaped curve where the middle represents the length where about 50% of the individuals encountering the gear are retained (=selected) by it. The height of that S-shaped curve should be near the highest peak in the data and about one length class to the right of the peak should approximate 100% selection. If there is a smaller peak to left, that may indicate only partially selected recruits and these can be excluded from the analysis by moving the L_cut line beyond that peak. Also, years where the highest peak appears substantially earlier or later than in adjacent years, or where there are two clearly separated peaks of very similar height, may represent unsuitable sampling regimes and such years should be excluded from the analysis as part of data preparation.",
                                tags$br(), tags$br(),
                                "Under trawl-like selection, the right hand side of the LFD plot should show a linear or exponentially declining curve, which may contain 'waves' that reflect different year classes. As part of data preparation, the raw data are standardized and aggregated across all years. Use the L_full line to select a point at or near the right side of the peak to obtain a rough but satisfactory fit to the right-hand LFD. Overall, the LFD plot should look like a right-skewed normal distribution. If it strongly deviates from such shape, consider to delete the respective year from the analysis during data preparation.",
                                tags$br(), tags$br(),
                                "If the dominant gear is instead a gillnet with a single fixed mesh size, select 'Gillnet-like selectivity' (Gaussian selectivity). If the fishery operates with mixed gears including gill nets with several different mesh sizes, and if the data stem from a joint landing site or from gear-specific length frequencies where the catch numbers are weighted by the relative annual effort of the respective gears, then trawl-like selectivity may apply.",
                                tags$br(), tags$br(),
                                "LBB can work with annual species, which mature and die within one year, if there is a well-defined main spawning season. The subsequent months then indicate the age of the cohort, and LFDs taken at that month are a snapshot of the distribution around the mean length reached at that age. However, such often perfect LFDs do not reflect selectivity and mortality and are unfit for LBB analysis. Instead, monthly or bimonthly LFDs have to be standardized (divided by relative effort) and combined to form a single LFD curve representative of the cohort throughout its exploited lifespan."
                              )) 
                                          ),
              shinyBS::bsModal(id='modalExample2',
                               title ='Selectivity patterns',
                               trigger="Selectivity_patt",
                               size = "large",
                               shinycssloaders::withSpinner(shiny::plotOutput('Selectivity_plots')),
                               shiny::h5("A Trawl-like selection: The dashed blue curve indicates the proportion of individuals retained by the trawl if they encounter the trawl, as a function of individual body length: from about 30 cm onward, all individuals are retained. The black curve is an example of observed length frequencies, with the left-hand side showing the increased selectivity, from zero to one, and the right-hand side showing the decline in numbers caused by natural mortality plus fishing mortality."),
                               shiny::h5("B Gillnet-like selection: The dashed blue curve indicates the proportion of individuals retained by the gillnet if they encounter it, as a function of individual body length. Note that all fish of about 30 cm length will be retained, but some smaller ones as well larger ones will escape. The black curve is an example of observed length frequencies, with the left-hand side showing the increased selectivity, from zero to one, and the right-hand side showing the decline in numbers caused by natural mortality plus fishing mortality and by reduced selectivity of the gear.")
                               
                                 ),
              
              
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
                                column(width=4,#align="center",
                                      # uiOutput("cond_ifmany_run"),
                                       conditionalPanel(condition = "input.Save_priors",
                                                        shinydashboard::box(collapsible = F, 
                                                        shinyWidgets::actionBttn(inputId="Start_run",label =" Start",
                                                                                 style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                 no_outline=F,block=F,color="success") , width=12))),
                                column(width=8,
                                       conditionalPanel(condition = "input.Save_priors",
                                                        shinydashboard::valueBoxOutput("Run_infobox_patient",width=12)),
                                )),
                              tags$br(),
                              shiny::fluidRow(
                                conditionalPanel(condition = "input.Save_priors",
                                                 shinyWidgets::progressBar(
                                                   id = "prog_bar",value = 0,total = 100,title = "",
                                                   display_pct = TRUE,striped = T,# size="sm",
                                                   status="success"))
                              ),
                              fluidRow(
                                column(width=4,
                                       conditionalPanel(condition = "input.Start_run", 
                                                        shinydashboard::box(collapsible = F, 
                                                                            #tags$b("See the LFDs"),
                                                                            linebreaks(1),
                                                                            sliderInput(inputId = "LFDs_per_page2", label = "LFDs per page",
                                                                                        min = 1, max = 10, value =4, step =1),
                                                                            linebreaks(1),
                                                                            awesomeRadio(
                                                                              inputId = "LBB_pages2",
                                                                              label = "Navigate through the plots", 
                                                                              choices = "page 1",
                                                                              selected = "page 1",#yr[1],
                                                                              inline = FALSE, 
                                                                              status = "success"
                                                                            ),width = 12))),  
                              
                                column(width = 8,align="center",
                                                conditionalPanel(condition = "input.Start_run",
                                                                 shinydashboard::box(collapsible = F,
                                                                                     shinycssloaders::withSpinner(shiny::plotOutput("LFDs_2")),
                                                                                     shiny::h5("Annual LBB fits (red curve) with indication of median (bold green) and annual (dashed green) Linf and Lopt estimates. Examine carefully all fits to the annual length frequency data. Z/K indicates the steepness of the decline on the right hand side, with Z/K = 3 suggesting that fishing mortality is close to natural mortality. Consider excluding years with bad fits or unrealistic data (under data preparation)."),
                                                                                     width = 12),
                                                                 shinydashboard::valueBoxOutput("success_LBB",width =12)
                                                                 )
                                       )
                               
                              ),
                              shinyBS::bsModal(id='modalExample3',
                                               title ='Selected object',
                                               trigger="see_obj",
                                               size = "large",
                                               shiny::tableOutput('see_object_table'))
                              ),
      ############ ############ ############
      ############ RUN MODEL2 UI ############
      ############ ############ ############
      shinydashboard::tabItem(tabName = "run_model2",
                              shiny::fluidRow(uiOutput("Zavarakatranemia2")),
                              shiny::fluidRow(
                                column(width=4,align="center",
                                       conditionalPanel(condition = "input.Start_run",
                                                        shinydashboard::box(collapsible = F,
                                                        shinyWidgets::actionBttn(inputId="BH_run_button",label ="Start",
                                                                                 style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                 no_outline=F,block=F,color="success"),width = 12))#,
                                ),
                                column(width=8,
                                       conditionalPanel(condition = "input.BH_run_button",
                                                        shinydashboard::valueBoxOutput("Run_BH_patient",width=12)),
                                )),
                              shiny::fluidRow(
                                conditionalPanel(condition = "input.BH_run_button",
                                                 shinydashboard::box(collapsible = F,align="center", background = "light-blue",
                                                                     uiOutput("working_directory"),width = 12))
                              ),
                              shiny::fluidRow(
                                column(width=4,
                                       conditionalPanel(condition = "input.BH_run_button",
                                                        linebreaks(2),
                                                        materialSwitch(
                                                          inputId = "smooth_ts_button",
                                                          label = "Smooth time series",
                                                          value = FALSE, 
                                                          status = "success"
                                                        ),
                                                        tags$b("Model outputs"),
                                                        tags$hr(style = "border-top: 3px solid #000000;"),
                                                        uiOutput("ModOutp_a1a"),
                                                        uiOutput("ModOutp_a1b"),
                                                        uiOutput("ModOutp_a1c"),
                                                        uiOutput("ModOutp_a2a"),
                                                        uiOutput("ModOutp_a2b"),
                                                        uiOutput("ModOutp_a3a"),
                                                        uiOutput("ModOutp_a3b"),
                                                        uiOutput("ModOutp_a4"),
                                                        uiOutput("ModOutp_a5a"),
                                                        uiOutput("ModOutp_a5b"),
                                                        tags$hr(style = "border-top: 1px solid #999797;"),
                                                        tags$b("General reference points (median across years):"),
                                                        uiOutput("ModOutp_a6"),
                                                        uiOutput("ModOutp_a7"),
                                                        uiOutput("ModOutp_a8a"),
                                                        uiOutput("ModOutp_a8b"),
                                                        uiOutput("ModOutp_a8c"),
                                                        
                                                        uiOutput("ModOutp_a9"),
                                                        uiOutput("ModOutp_a10"),
                                                        uiOutput("ModOutp_a11"),
                                                        uiOutput("ModOutp_a12"),
                                                        uiOutput("ModOutp_a13"),
                                                        uiOutput("ModOutp_a14"),
                                                        uiOutput("ModOutp_a15"),
                                                        uiOutput("ModOutp_a16a"),
                                                        uiOutput("ModOutp_a16b"),
                                                        
                                                        tags$hr(style = "border-top: 1px solid #999797;"),
                                                        uiOutput("ModOutp_a17"),
                                                        uiOutput("ModOutp_a17b"),
                                                        uiOutput("ModOutp_a18"),
                                                        uiOutput("ModOutp_a19"),
                                                        uiOutput("ModOutp_a20"),
                                                        uiOutput("ModOutp_a21"),
                                                        uiOutput("ModOutp_a22"),
                                                        uiOutput("ModOutp_a23"),
                                                        uiOutput("ModOutp_a24"),
                                                        uiOutput("ModOutp_a25"),
                                                        uiOutput("ModOutp_a26"),
                                                        uiOutput("ModOutp_a27"),
                                                        uiOutput("ModOutp_a28"),
                                                        uiOutput("ModOutp_a29"),
                                                        uiOutput("ModOutp_a30"),
                                                        tags$hr(style = "border-top: 1px solid #999797;"),
                                                        #uiOutput("ModOutp_a31"),
                                                        #tags$hr(style = "border-top: 1px solid #999797;"),
                                                        shinydashboard::box(collapsible = F,
                                                                            linebreaks(1),
                                                                            tags$b("Save a graph"),
                                                                            shinyWidgets::materialSwitch(
                                                                              inputId = "DL_picA",
                                                                              label = "Save",
                                                                              value = FALSE,status = "info" ),
                                                                            conditionalPanel(condition = "input.DL_picA",
                                                                                             tags$b("Select graph"),
                                                                                             shinyWidgets::pickerInput(inputId = "Run_select_pic",
                                                                                                                       label = "",choices =c("A","B","C","D"),#,"K","I","J"
                                                                                                                       options = list(title = "Select graph"),
                                                                                                                       choicesOpt = list(
                                                                                                                         subtext = c(
                                                                                                                           "Aggregated LFD",
                                                                                                                           "Lc time series",
                                                                                                                           "FM time series",
                                                                                                                           "BB0 time series"
                                                                                                                           #  "Catch diagnostics",
                                                                                                                           #"r-k diagnostics",
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
                                                                                                                        no_outline=F,block=F))
                                                                            ,width=12))),
                                column(width = 8,align="center",
                                       shiny::fluidRow(
                                         conditionalPanel(condition = "input.BH_run_button",
                                                          shinydashboard::box(collapsible = F,
                                                                              shinycssloaders::withSpinner(shiny::plotOutput("lc_plot")),
                                                                              uiOutput("legend_1"),
                                                                              width = 12))),#),height = 250
                                       shiny::fluidRow(
                                         conditionalPanel(condition = "input.BH_run_button",
                                                          shinydashboard::box(collapsible = F,
                                                                              shinycssloaders::withSpinner(shiny::plotOutput("fm_plot")),
                                                                              shiny::h5("Fishing mortality (F) relative to natural mortality (M), which can be seen as a proxy for maximum sustainable fishing mortality (Fmsy). F/M > 1 thus suggests overfishing F > Fmsy."),
                                                                              width = 12))),
                                       shiny::fluidRow(
                                         conditionalPanel(condition = "input.BH_run_button",
                                                          shinydashboard::box(collapsible = F,
                                                                              shinycssloaders::withSpinner(shiny::plotOutput("bb0_plot")),
                                                                              shiny::h5("Annual biomass (B) relative to unexploited biomass (B0), with the dashed green line indicating a proxy reference point for the smallest biomass that can support maximum sustainable yields (Bmsy). Half of Bmsy is widely recognized as the border to stock sizes that are too small to support regular reproduction."),
                                                                              width = 12)))#,
                                       # shiny::fluidRow(
                                       #   conditionalPanel(condition = "input.BH_run_button",
                                       #                    shinydashboard::box(collapsible = F,
                                       #                                        shinycssloaders::withSpinner(shiny::plotOutput("hist_plot")),
                                       #                                        shiny::h5("TEXT"),
                                       #                                        width = 12)))
                                       
                                       
                                       #,height = 320
                                       
                                )
                              )),
      shinydashboard::tabItem(tabName = "addit_info",
                              #tags$hr(style = "border-top: 2px solid #000000;"),
                              shiny::fluidRow(shiny::h3(tags$b("References:")),
                                              shiny::h4(tagList("Froese R, Winker H, Coro G, Demirel N, Tsikliras AC, Dimarchopoulou D, Scarcella G, Probst W N, Dureuil M, Pauly D, 2018. A new approach for estimating stock status from length frequency data. ICES Journal of Marine Science, 75(6), 2004-2015. https://doi.org/10.1093/icesjms/fsy078"))),                              tags$hr(style = "border-top: 2px solid #000000;"),
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
  
  shinyWidgets::useSweetAlert()
  #  shinyBS::toggleModal(session, "modalExample", toggle = "toggle")
  
  Stock_obj <- reactiveValues()
  Stock_obj$Stocks=data.frame(ID=1:200,stock=rep(NA,200))
  Stock_obj$LBB_ID <- template_LBB_ID
  Stock_obj$LBB_data <- template_LBB_data
  shinyWidgets::useSweetAlert()
  
  
  ###### Update select tools
  
  observe({
    updateTextInput(session, "Sp_Comm", value = species_DB$FBname[species_DB$Species==input$Sp_Scient])
  })
  
  
  Common_name=reactive({
    xx=input$Sp_Comm
  })
  
  output$Sps_Info=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Species info"),shiny::h5(HTML(paste0("Scientific name: ", tags$b(tags$em(input$Sp_Scient)),br(),br(),
                                                      "Common name: ",tags$b(input$Sp_Comm),br(),br()
                                                      ))),
      icon = shiny::icon("file-alt"),
      color = "light-blue") })
  
  
  ###CREATE UPLOAD DATA
  inp_data_LBB <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,#,
                   quote = input$quote ) #
    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round,2)
    observe({
      temp_df<- df
      shinyWidgets::updatePickerInput(session, "Yr_col", choices = names(temp_df),
                                      selected ="")
      shinyWidgets::updatePickerInput(session, "Length_col", choices = names(temp_df),
                                      selected ="")
      shinyWidgets::updatePickerInput(session, "N_col", choices = names(temp_df),
                                      selected ="")
    })
    return(df)
  })
  output$contents <-  DT::renderDataTable({ inp_data_LBB()
    DT::datatable( inp_data_LBB(),
                   options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
  })
  # 
  observeEvent(input$button_2,{
    # if(input$prepare_data > 0 ) {
    #   shinyWidgets::sendSweetAlert(
    #     session = session,title = "Warning",
    #     text = "If you have already run an analysis for a stock and you want to run a new one for another stock, it is advised to reload the app and start from scratch.", type = "warning")
    # }
    if(input$Crtd_StckID_input=="") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Input a stock ID!",
        type = "error")
    }  else {
      if(input$button_2 > 0) {
        newLine1 <- isolate(c(NA,
                              input$Sp_Scient,
                              Common_name(),
                              input$Crtd_StckID_input,
                              rep(NA,17),
                               "created"
        ))
        if (all(is.na(Stock_obj$Stocks$stock))){
          Stock_obj$Stocks$stock[1]=newLine1[4]
          Stock_obj$LBB_ID[1,]=c(newLine1,1)
        } else {# if(newLine1[4] %!in% Stock_obj$Stocks$stock) {
          Stock_obj$Stocks$stock[length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)])+1]=newLine1[4]
          Stock_obj$LBB_ID[length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)]),]=c(newLine1,length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)]))
        }
      }
    }
  })
# 
  observeEvent(input$button_2,{
    if(input$Crtd_StckID_input=="") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Input a stock ID!",
        type = "error")
    }  else {
      if(input$button_2 > 0) {
        newLine_lbb_data <- isolate(data.frame(
          Stock= rep(input$Crtd_StckID_input,length(inp_data_LBB()[,input$Yr_col])),
          Year=as.integer(inp_data_LBB()[,input$Yr_col]),
          Length=inp_data_LBB()[,input$Length_col],
          CatchNo=inp_data_LBB()[,input$N_col],
          Stock_objID=rep(Stock_obj$LBB_ID$Stock_objID[Stock_obj$LBB_ID$Stock==input$Crtd_StckID_input],length(inp_data_LBB()[,input$Yr_col]))
        ))
        # }
      }
      Stock_obj$LBB_data <- rbind(Stock_obj$LBB_data, newLine_lbb_data)
    }
  })
  
  observeEvent(input$unit_col!="",{
    updateNumericInput(session, "length_width",label = paste0("Used length class width in ", input$unit_col))
    
  })
  
  
  # observeEvent(input$Upld_butt_1,{   #TRICK to erase things
  #   shinyWidgets::updateMaterialSwitch(session, "Upld_butt_3", value =F)
  # })
  # 
  # 
  # observeEvent(input$button_1,{   #TRICK to erase things
  #   shinyWidgets::updatePrettySwitch(session, "upMaterial1", value = F)
  #   shinyWidgets::updatePrettySwitch(session, "upMaterialccv", value = F)
  #   shinyWidgets::updatePrettySwitch(session, "upMaterial2", value = F)
  #   shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
  #   shinyWidgets::updateMaterialSwitch(session, "Priors_but2", value = F)
  # })
  # 
  # observeEvent(input$button_2,{   #TRICK to erase things
  #   shinyWidgets::updatePrettySwitch(session, "upMaterial1", value = F)
  #   shinyWidgets::updatePrettySwitch(session, "upMaterialccv", value = F)
  #   shinyWidgets::updatePrettySwitch(session, "upMaterial2", value = F)
  #   shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
  #   shinyWidgets::updateMaterialSwitch(session, "Priors_but2", value = F)
  # })
  # 
  output$Stock_infobox_1=shinydashboard::renderValueBox({
    shinydashboard::valueBox(shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the stock object ", input$Crtd_StckID_input,br(),
                                                                         " of the species ",tags$em(input$Sp_Scient), ".",br(),
                                                                         "You can know proceed to the 'Priors' tab."))),
                             shiny::icon("thumbs-up", lib = "glyphicon"),
                             color = "light-blue") })
  # 
  output$LBB_plot1= shiny::renderPlot({
    ggplot(Stock_obj$LBB_data[Stock_obj$LBB_data$Stock==input$Crtd_StckID_input,],
           aes(x=Length, y=as.factor(Year), height = CatchNo, group = as.factor(Year))) + 
      scale_x_continuous(limits = c(0,NA))+
      geom_density_ridges(stat = "identity", scale = 1,alpha=0.7,fill="#F8766D")+
      ggplot2::theme_classic()+ggplot2::labs(y="Year", x="Length",fill="")
    #
      #ggplot2::scale_y_continuous(limits=c(0,NA))
  })#, height = 200
  # 
  # 

  ###### EXPLORE EXISTING STOCKS
  # res_mod <- callModule(
  #   module = shinyWidgets::selectizeGroupServer,
  #   id = "my_filters",
  #   inline = FALSE,
  #   data = test_LBB_ID,
  #   vars = c("Continent", "Region", "Subregion", "Group", "ScientificName")
  # )
  
  output$Exist_Stcks_tble <- DT::renderDataTable({
    DT::datatable(test_LBB_ID[,c( "Species" , "Stock" ,"Years.user")],
                  selection = 'single', options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
  })
  
  observe({
    updateTextInput(session, "txt1",value=test_LBB_ID[input$Exist_Stcks_tble_rows_selected,"Stock"])
    updatePrettyToggle(session,"semelparous2",  value =ifelse(test_LBB_ID[input$Exist_Stcks_tble_rows_selected,"Species"] %in% 
                                                              c("Illex coindetii", "Octopus vulgaris", "Sepia officinalis"),TRUE,FALSE)   # = getDefaultReactiveDomain(),
    )
    
  })
  
  output$txtout1=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Info"),shiny::h5(test_LBB_ID$Comment[test_LBB_ID$Stock==input$txt1]) ,
      icon = shiny::icon("info-circle"),
      color = "aqua") })
 
 
  
  output$LBB_plot2= shiny::renderPlot({
    validate(
      need(input$txt1 %in% test_LBB_data$Stock, 'Choose a valid Stock from the last column of the above Data Table')
    )
    
    ggplot(data=test_LBB_data[test_LBB_data$Stock==input$txt1,], aes(x=Length, y=as.factor(Year), height = CatchNo, group = as.factor(Year))) + 
      geom_density_ridges(stat = "identity", scale = 1,alpha=0.7,fill="#F8766D")+ggplot2::theme_classic()+ggplot2::labs(y="Year", x="LC",fill="")
    
    # 
    # ggplot2::ggplot(data=test_LBB_data[test_LBB_data$Stock==input$txt1,], ggplot2::aes(x=yr, y=ct, group=1)) +
    #   ggplot2::geom_line(color="blue",size=1)+ggplot2::labs(y="Catch", x="Year")+
    #   ggplot2::geom_point(color="black",size=2)+ggplot2::theme_classic()+
    #   ggplot2::scale_y_continuous(limits=c(0,NA)) 
  })#, height = 200
  
  
  observeEvent(input$button_1,{
    if(input$button_1 > 0) {
      if(input$txt1 %!in% test_LBB_ID$Stock) {
        shinyWidgets::sendSweetAlert(
          session = session,title = "Error...",
          text = "Oups !", type = "error")
      } else {
        newLine1 <- isolate(c(as.vector(test_LBB_ID[test_LBB_ID$Stock==input$txt1,1:21]),"selected"))
        if (all(is.na(Stock_obj$Stocks$stock))){
          Stock_obj$Stocks$stock[1]=input$txt1
          Stock_obj$LBB_ID[1,]=c(newLine1,1)
        } else {#if(input$txt1 %!in% Stock_obj$Stocks$stock)
          Stock_obj$Stocks$stock[length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)])+1]=newLine1[4]
          Stock_obj$LBB_ID[length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)]),]=c(newLine1,length(Stock_obj$Stocks$stock[!is.na(Stock_obj$Stocks$stock)]))
        }
      }
    }
  })

  observeEvent(input$button_1,{
    if(input$button_1 > 0) {
      if(input$txt1 %!in% test_LBB_ID$Stock) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Oups !",
          type = "error"
        )
      } else {
        newLine_LBB_data <- isolate(cbind(
          test_LBB_data[test_LBB_data$Stock==input$txt1,1:4],
          Stock_objID=rep(Stock_obj$LBB_ID$Stock_objID[Stock_obj$LBB_ID$Stock_objID==max(Stock_obj$LBB_ID$Stock_objID)],
                          nrow(test_LBB_data[test_LBB_data$Stock==input$txt1,])))
        )
        Stock_obj$LBB_data <- rbind(Stock_obj$LBB_data, newLine_LBB_data)
      }}
  })

  output$Stock_infobox_2=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the stock object ", test_LBB_ID[test_LBB_ID$Stock==input$txt1,"Stock"],
                                                  " of the species ",tags$em(test_LBB_ID[test_LBB_ID$Stock==input$txt1,"Species"]), ".",br(),
                                                  "You can know proceed to the 'Priors' tab."))),
      shiny::icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue") })

  ################################################################
  ###################### PRIORS  #####################
  ################################################################
  
  #########SELECT STOCK TO WORK WITH PRIORS
output$Stock_infobox_3=renderUI({
  shiny::h3(HTML(paste0("Set priors for the stock: ", tags$b(Stock_obj$LBB_ID$Stock),",  ","  ", "     species: ", tags$em(Stock_obj$LBB_ID$Species))
  ))
})
  
  
   # output$Stock_infobox_patience=shinydashboard::renderValueBox({
   #  shinydashboard::valueBox(shiny::h4( ),shiny::h4("Press 'Connect' to establish connection to FishBase or SeaLifeBase to get information on Lmax and Linf for your stock"),
   #                           color = "green") })
   # 
   # 
   # output$Stock_infobox_maturity=shinydashboard::renderValueBox({
   #   shinydashboard::valueBox(shiny::h4( ),shiny::h4("Press 'Connect' to establish connection to FishBase or SeaLifeBase to get information on Lm for your stock"),
   #                            color = "green") })
   # 
   # 
   
   # output$Stock_infobox_3=shinydashboard::renderValueBox({
   #  shinydashboard::valueBox(shiny::h4( ),shiny::h4(HTML(paste0("Stock: ", tags$b(Stock_obj$LBB_ID$Stock),",  ","  ", "     Species: ", tags$b(tags$em(Stock_obj$LBB_ID$Species)))
   #                                                       )),
   #                           color = "light-blue") })
  
  

  
 
  Final_stock=eventReactive(input$procc_rpriors,{
    final_stock=list(LBB_ID=Stock_obj$LBB_ID[Stock_obj$LBB_ID$Stock_objID==max(Stock_obj$LBB_ID$Stock_objID),],
                     LBB_Data=Stock_obj$LBB_data[Stock_obj$LBB_data$Stock_objID==max(Stock_obj$LBB_ID$Stock_objID),])
      return(final_stock)
  })
  
  output$someinfo=renderUI({
    req(Final_stock())
    cr.sel=Final_stock()[["LBB_ID"]]$Cr_sel
     F_stock=Final_stock()[["LBB_Data"]]
    
    if(cr.sel=="created" ) {
      munit=input$unit_col} else {
        munit="mm"}
    if (munit=="mm") {
      F_stock$Length= F_stock$Length/10
    }
    F_stock=F_stock[F_stock$CatchNo>0,]
    F_stock$max_L=ave(F_stock$Length,F_stock$Year, FUN = max)
    F_stock_max=max(F_stock$max_L)
    F_stock_median=median(F_stock$max_L)

      shiny::h5(tagList(tags$b("Largest length in your data is Lmax = ", F_stock_max, " cm, median annual Lmax = ",F_stock_median ," cm. "), 
                        "Use this info plus data in FishBase/SeaLifeBase from your area or from adjacent or similar areas to select a reasonable Linf prior for your stock.",tags$br(),
                        "If the species is semelparous (dies after a single reproductive event, such as salmon, squid, octopus, or eels), set Linf prior to about 1.5 times the median of annual maximum length values (around ",1.5*F_stock_median,  " cm for this stock). Sometimes individuals of semelparous species do not reproduce and continue to live and grow. Such (rare) outliers of maximum length may be indicative of or close to Linf."
                        ))
  })
  
  
  output$someinfomk=renderUI({
    shiny::h5(tagList("Set the ratio between natural mortality rate (M) relative to somatic growth rate (M/K). A value of about 1.5 is typical for adults of species that grow throughout their life, reaching maximum size at maximum age." 
                             ))
  })
  
  Fishbase_text=eventReactive(input$procc_rpriors,{
    req(Final_stock())
    results= fbsb_MAX(Final_stock()[["LBB_ID"]][1,"Species"])
    return(results)
  })
  
  # url_r=eventReactive(input$procc_rpriors,{
  #   url= a("Link", href=Fishbase_text()[6],style = "color:black") })
  # 
  output$FB_resil=shinydashboard::renderValueBox({
    if (is.na(Fishbase_text()[1])) {
      shinydashboard::valueBox(
        shiny::h4("FishBase/SeaLifeBase info"),"We couldn't find information in FishBase/SeaLifeBase. Check species name spelling or consider to search manually. If no information on resilience or r-range is available in FishBase/SeaLifeBase, using Medium resilience for 'regular' species and Low resilience for large or deep-water species is a reasonable starting point. Very low resilience is found in very large species and High resilience is found in species which reproduce in their first year. The general receiving e-mail of FishBase and SeaLifeBase for comments, feedbacks and data sending to be added into the databases is info@quatics.org.",
        icon = shiny::icon("info-circle"),
        color = "green")
    }  else {
          shinydashboard::valueBox(
            shiny::h4("FishBase/SeaLifeBase info"),tagList(HTML(paste0("According to FishBase/SeaLifeBase, ", tags$em(Final_stock()[["LBB_ID"]]["Species"]), " has: ",
                                                           tags$b(Fishbase_text()[2])))),#, " Prior r range:", tags$b(Fishbase_text()[1]),tags$br(), "<font color=\"#cc0000\"><b>"," For more info press: ",url_r(), "</b></font>"
            icon = shiny::icon("info-circle"),
            color = "green")
        }
  })
  
  
  Fishbase_Table1=eventReactive(input$procc_rpriors,{
    req(Final_stock())
    req(Fishbase_text())
    
    results= fbsb_table_1(Fishbase_text()[1])
    return(results)
  })
  
 # output$inf_fishBase_1=renderTable({Fishbase_Table1()})
  output$inf_fishBase_1 <- DT::renderDataTable({
    DT::datatable(Fishbase_Table1(),
                  selection = 'single',rownames=FALSE, options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
  })
  
  Fishbase_Table2=eventReactive(input$procc_rpriors,{
    req(Final_stock())
    req(Fishbase_text())
    
    results= fbsb_table_2(Fishbase_text()[1])
    return(results)
  })
  
  #output$inf_fishBase_2=renderTable({Fishbase_Table2()})
  output$inf_fishBase_2 <- DT::renderDataTable({
    DT::datatable(Fishbase_Table2(),
                  selection = 'single',rownames=FALSE, options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
  })

  #Final_stock <- Final_stock %>% debounce(1000)
  Fishbase_maturity=eventReactive(input$procc_rpriors,{
    req(Final_stock())
    req(Fishbase_text())
    
    results= fbsb_table_mat(Fishbase_text()[1])
    return(results)
  })
  
  #output$inf_fishBase_2=renderTable({Fishbase_Table2()})
  output$inf_maturity <- DT::renderDataTable({
    DT::datatable(Fishbase_maturity(),
                  selection = 'single',rownames=FALSE, options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
  })
  
  
  
  
  
  output$success_priors=shinydashboard::renderValueBox({
   if(isTruthy(input$Linf.user)) {
     shinydashboard::valueBox(
      shiny::h4("Info"),
            if (input$Linf.user==0) {
              shiny::h4(HTML(paste0("Set priors to continue."              )))} else {
      shiny::h4(HTML(paste0("If you are happy with your prior selection you can move onward to 'Prepare data'."
      )))},
      icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "light-blue")
   } else {
     shinydashboard::valueBox(
       shiny::h4("Info"),
      
         shiny::h4(HTML(paste0("Set priors to continue."))),
       icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "light-blue")
     
     
     }
     })
 
  output$Stock_infobox_prepare=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Stock object"),shiny::h5(HTML(paste0("You have selected the stock ",
                                                      tags$b(Final_stock()[["LBB_ID"]][1,"Stock"]), " of the species ",
                                                      tags$b(tags$em(Final_stock()[["LBB_ID"]][1,"Species"]))
      ))),
      icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "light-blue") })
 
  LBB_data=eventReactive(input$prepare_data,{
    req(Final_stock())
    lb_data_= fix_LFD(Final_stock()[["LBB_Data"]],Final_stock()[["LBB_ID"]]$Cr_sel,input$unit_col)
    # lb_data_= Final_stock()[["LBB_Data"]]
    # lb_data_$Length= lb_data_$Length/10
      return(lb_data_)
  })

  LBB_data <- LBB_data %>% debounce(600)
  
  # output$test_orf=renderTable({LBB_data()})

  observe({
    
    req(LBB_data())
    LBB_data_=LBB_data()
    LBB_data_=LBB_data_[LBB_data_$Year %in% input$Sel_yrs,]
    max_LC=unique(max(LBB_data_$Length[LBB_data_$CatchNo>0],na.rm=T))
    max_LC=max_LC-1
    updateSliderInput(session,"L.cut",max=max_LC)
    
  })

  # output$Year_Selection=renderUI({
  #   req(Final_stock())
  #   #req(input$LFDs_per_page)
  #   years=unique(Final_stock()[["LBB_Data"]]$Year)
  #   pickerInput(
  #     "Sel_yrs",
  #     label = "Select years to include in the analysis",
  #     choices=years,
  #     selected = years,
  #     multiple = T,
  #     inline = FALSE
  #   )
  # })
  
  output$Year_Selection=renderUI({
    req(Final_stock())
    #req(input$LFDs_per_page)
    years=unique(Final_stock()[["LBB_Data"]]$Year)
    checkboxGroupButtons(
      inputId = "Sel_yrs",
      label = "Select years to include in the analysis",
      choices = years,
      selected = years,
      # status = "primary",
      # checkIcon = list(
      #    yes = icon("ok", 
      #               lib = "glyphicon"),
      #    no = icon("remove",
      #              lib = "glyphicon"))
      individual = TRUE,
      checkIcon = list(
        yes = tags$i(class = "fa fa-circle", 
                     style = "color: green"),
        no = tags$i(class = "fa fa-circle-o", 
                    style = "color: red"))
    )
})
  
  
 observe({
     req(LBB_data())
   req(input$LFDs_per_page)
    LBB_data_=LBB_data()
   #LBB_data_=LBB_data_[LBB_data_$Year %in% input$Sel_yrs,]
   years=unique(LBB_data_$Year)
   pages=ceiling(length(years)/input$LFDs_per_page)
     updateAwesomeRadio(session,"LBB_pages",choices=paste0("page ",seq(1,pages,1)))
 
 })
  
  pic_LBB_see=reactive({
    validate(
      need(isTruthy(input$Sel_yrs), 'Processing...')
    )
     req(Final_stock())
     req(LBB_data())
    # req(input$LBB_pages)
     req(input$Sel_yrs)
    LBB_data_=LBB_data()
    #LBB_data_=LBB_data_[LBB_data_$Year %in% input$Sel_yrs,]
    g.b_=data.frame(Year=as.integer(sort(unique(LBB_data_$Year))),g.b=rep("A",length(unique(LBB_data_$Year))))
    g.b_$g.b[g.b_$Year %!in% input$Sel_yrs]="B"
    
    if(Final_stock()[["LBB_ID"]]$Cr_sel=="created" ) {
      wdth=input$length_width} else {
        wdth=as.numeric(Final_stock()[["LBB_ID"]]$Lc.user)/10
      }
    
    if (input$Change_width==1) {
      LBB_data_=LBB_data_
    } else if (input$Change_width>1) {
      wdth=wdth*input$Change_width
      max_LC=unique(max(LBB_data_$Length[LBB_data_$CatchNo>0],na.rm=T))
      classes=seq(0,max_LC,wdth)
      clasnms=classes#+wdth/2
      clasnms=clasnms[1:length(clasnms)-1]
      LBB_data_$LC=as.numeric(as.character(cut(LBB_data_$Length,breaks =classes,labels = clasnms,right = F )))
      LBB_data_=aggregate(CatchNo   ~Stock +Year +LC , data = LBB_data_, sum)
    colnames(LBB_data_)= c("Stock",   "Year","Length",  "CatchNo")
    LBB_data_=LBB_data_[order(LBB_data_$Year),]
    }

    years=unique(LBB_data_$Year)
    pages=ceiling(length(years)/input$LFDs_per_page)    
    years_=split(years, ceiling(seq_along(years)/input$LFDs_per_page))
    LBB_data_$CatchNo[LBB_data_$Length<=input$L.cut]=0
     LBB_data_$max_catch=ave(LBB_data_$CatchNo,LBB_data_$Year,FUN = max)
     LBB_data_$CatchNo_scaled= LBB_data_$CatchNo/ LBB_data_$max_catch
    # LBB_data_=LBB_data_[LBB_data_$Year %in% input$Sel_yrs,]
    # LBB_data_$CatchNo_scaled[LBB_data_$Length<=input$L.cut]=0
     Total_data2=aggregate(CatchNo   ~Year , data = LBB_data_, sum)
    # LBB_data_=LBB_data_%>%left_join(g.b_)
     LBB_data_=base::merge(x = LBB_data_, y = g.b_, all.x = TRUE)
     
     LBB_data_=LBB_data_[order(LBB_data_$Year),]
        pics=list()
        LBB_data_t=  LBB_data_
        min_LBB_data=min(LBB_data_t$Length,na.rm=T)
        max_LBB_data=max(LBB_data_t$Length,na.rm=T)
        lengthss=seq(min_LBB_data,max_LBB_data,wdth)
        yearss=unique(LBB_data_t$Year)
        
        LBB_data_show=expand.grid(Year=yearss,Length=lengthss)
      LBB_data_show=base::merge(x = LBB_data_show, y = LBB_data_t, all.x = TRUE)
       # LBB_data_show=left_join(LBB_data_show,LBB_data_t)
           LBB_data_show$CatchNo[is.na(LBB_data_show$CatchNo)]=0
        
    if(input$Change_view==FALSE) { 
if(input$Scale_switch==FALSE) {

    pics[[1]]=ggplot(data=LBB_data_show[LBB_data_show$Year %in% years_[[1]],], aes(x=Length, y=as.factor(Year), height = CatchNo, group = as.factor(Year)),fill="#F8766D") + 
      geom_density_ridges(stat = "identity", scale = 1,alpha=0.7)+ggplot2::theme_classic()+ggplot2::labs(y="Year", x="LC")
    
    for (i in 1:pages) {
      pics[[i]]= ggplot(data=LBB_data_show[LBB_data_show$Year %in% years_[[i]],], aes(x=Length, y=CatchNo))+
        geom_rect(data=LBB_data_[LBB_data_$Year %in% years_[[i]],],aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=g.b), alpha=0.1, show.legend = FALSE) +
        geom_line(col="#F8766D")+
        geom_area(fill="#F8766D") +
        # geom_point(shape=21,fill="#F8766D")+
        scale_fill_manual(values = c("A"="white","B"="#F8766D"))+
        geom_vline(aes(xintercept = input$L.cut,),size=1)+
        geom_text(aes(y =max_catch/4, x = input$L.cut, label = paste0("L.cut")),col="red")+  #  position=position_nudge(y= .5)
        geom_rect(aes(xmin=input$Linf.user-input$Linf_CV*input$Linf.user, 
                      xmax=input$Linf.user+input$Linf_CV*input$Linf.user, ymin=-Inf, ymax=Inf, ),fill="lightgray", color="lightgray", alpha=0.05) +
         geom_vline(aes(xintercept = input$Linf.user),col="gray",size=1)+
        geom_text(aes(y =max_catch/4, x = input$Linf.user, label = paste0("Linf")),col="black")+  
        geom_text(data = Total_data2[Total_data2$Year %in% years_[[i]],],
                  aes(y =Inf, x = Inf,hjust=c(1),vjust=c(1),label = paste0("N=",round(CatchNo))),col="black")+
        #position=position_nudge(y= .5)
        labs(fill="",x="Length (cm)")+
        facet_wrap(~Year, ncol=1)+theme_bw()#, scales="free_y"
      
        }
    
} else if(input$Scale_switch==TRUE) {
  
  pics[[1]]=ggplot(data=LBB_data_show[LBB_data_show$Year %in% years_[[1]],], aes(x=Length, y=as.factor(Year), height = CatchNo_scaled, group = as.factor(Year))) + 
    geom_density_ridges(stat = "identity", scale = 1,fill="#F8766D",alpha=0.7)+
    ggplot2::theme_classic()+
    ggplot2::labs(y="Year", x="Length class (cm)",fill="")

  for (i in 1:pages) {
    pics[[i]]=ggplot(data=LBB_data_show[LBB_data_show$Year %in% years_[[i]],], aes(x=Length, y=CatchNo))+
      geom_rect(data=LBB_data_[LBB_data_$Year %in% years_[[i]],],aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=g.b), alpha=0.1, show.legend = FALSE) +
      geom_line(col="#F8766D")+
      #geom_point(shape=21,fill="#F8766D")+
      geom_area(fill="#F8766D") +
      geom_vline(aes(xintercept = input$L.cut,),size=1)+
      scale_fill_manual(values = c("A"="white","B"="#F8766D"))+
      geom_text(aes(y =max_catch/4, x = input$L.cut, label = paste0("L.cut")),col="red")+ # position=position_nudge(y= .9)
      geom_rect(aes(xmin=input$Linf.user-input$Linf_CV*input$Linf.user, 
                    xmax=input$Linf.user+input$Linf_CV*input$Linf.user, ymin=-Inf, ymax=Inf, ),fill="lightgray", color="lightgray", alpha=0.05) +
      geom_vline(aes(xintercept = input$Linf.user),col="gray",size=1)+
      geom_text(aes(y =max_catch/4, x = input$Linf.user, label = paste0("Linf")),col="black")+
      geom_text(data = Total_data2[Total_data2$Year %in% years_[[i]],],
                aes(y =Inf, x = Inf,hjust=c(1),vjust=c(1),label = paste0("N=",round(CatchNo))),col="black")+
      #position=position_nudge(y= .9)
      labs(fill="",x="Length (cm)")+
      facet_wrap(~Year, scales="free_y", ncol=1)+theme_bw()

  }
  
}} else {
  
  if(input$Scale_switch==FALSE) {
    
    pics[[1]]=ggplot(data=LBB_data_[LBB_data_$Year %in% years_[[1]],], aes(x=Length, y=CatchNo))+
      geom_line()+
      geom_point(shape=21,fill="#F8766D")+
      facet_wrap(~Year, ncol=2)+theme_bw()#, scales="free_y"
   

    for (i in 1:pages) {

        pics[[i]]= ggplot(data=LBB_data_[LBB_data_$Year %in% years_[[i]],], aes(x=Length, y=CatchNo))+
                  geom_rect(data=LBB_data_[LBB_data_$Year %in% years_[[i]],],aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=g.b), alpha=0.1, show.legend = FALSE) +
          geom_line()+
        geom_point(shape=21,fill="#F8766D")+
          scale_fill_manual(values = c("A"="white","B"="#F8766D"))+
          geom_vline(aes(xintercept = input$L.cut,),size=1)+
        geom_text(aes(y =max_catch/4, x = input$L.cut, label = paste0("L.cut")),col="red")+#      position=position_nudge(y= .5)
          geom_rect(aes(xmin=input$Linf.user-input$Linf_CV*input$Linf.user, 
                        xmax=input$Linf.user+input$Linf_CV*input$Linf.user, ymin=-Inf, ymax=Inf, ),fill="lightgray", color="lightgray", alpha=0.05) +
          geom_vline(aes(xintercept = input$Linf.user),col="gray",size=1)+
          geom_text(aes(y =max_catch/4, x = input$Linf.user, label = paste0("Linf")),col="black")+
          geom_text(data = Total_data2[Total_data2$Year %in% years_[[i]],],
                    aes(y =Inf, x = Inf,hjust=c(1),vjust=c(1),label = paste0("N=",round(CatchNo))),col="black")+
          labs(x="Length (cm)")+
        facet_wrap(~Year, ncol=2)+theme_bw()#, scales="free_y"
         }
    
  } else if(input$Scale_switch==TRUE) {
    
    pics[[1]]=ggplot(data=LBB_data_[LBB_data_$Year %in% years_[[1]],], aes(x=Length, y=CatchNo))+
      geom_line()+
      geom_point(shape=21,fill="#F8766D")+
      facet_wrap(~Year, scales="free_y", ncol=2)+theme_bw()
    for (i in 1:pages) {
      pics[[i]]=ggplot(data=LBB_data_[LBB_data_$Year %in% years_[[i]],], aes(x=Length, y=CatchNo))+
        geom_rect(data=LBB_data_[LBB_data_$Year %in% years_[[i]],],aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=g.b), alpha=0.1, show.legend = FALSE) +
        geom_line()+
        geom_point(shape=21,fill="#F8766D")+

        geom_vline(aes(xintercept = input$L.cut,),size=1)+
        scale_fill_manual(values = c("A"="white","B"="#F8766D"))+
        geom_text(aes(y =max_catch/4, x = input$L.cut, label = paste0("L.cut")),col="red")+ #   position=position_nudge(y= .9)
        geom_rect(aes(xmin=input$Linf.user-input$Linf_CV*input$Linf.user, 
                      xmax=input$Linf.user+input$Linf_CV*input$Linf.user, ymin=-Inf, ymax=Inf, ),fill="lightgray", color="lightgray", alpha=0.05) +
        geom_vline(aes(xintercept = input$Linf.user),col="gray",size=1)+
        geom_text(aes(y =max_catch/4, x = input$Linf.user, label = paste0("Linf")),col="black")+
        geom_text(data = Total_data2[Total_data2$Year %in% years_[[i]],],
                  aes(y =Inf, x = Inf,hjust=c(1),vjust=c(1),label = paste0("N=",round(CatchNo))),col="black")+
        labs(x="Length (cm)")+
        facet_wrap(~Year, scales="free_y", ncol=2)+theme_bw()
    }}
}  
    p_1=pics[[as.numeric(gsub("page ","",input$LBB_pages))]]
    if (input$Lm50.user>0) {
      p_1=p_1+geom_vline(aes(xintercept = input$Lm50.user),col="black",size=1)+
        geom_text(aes(y =max_catch/4, x = input$Lm50.user, label = paste0("Lm")),col="blue")
    }
    
    p_2=pics
    p_all=list(p_1,p_2)
  })

  output$Check_LBB_plot= shiny::renderPlot({
    print(pic_LBB_see()[[1]])
  })#, height = 340

  
  output$success_LBB=shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        shiny::h4("Info"),
        shiny::h4(HTML(paste0("If you are happy with the LBB fits, proceed to 5. Run the Yield/Recruit model."))),
        icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "light-blue")
  })
  
  
  
  observe({
    req(LBB_data())
    req(input$LFDs_per_page)
    LBB_data_=LBB_data()
    #LBB_data_=LBB_data_[LBB_data_$Year %in% input$Sel_yrs,]
    years=unique(LBB_data_$Year)
    pages=ceiling(length(years)/input$LFDs_per_page)
    updateAwesomeRadio(session,"LBB_pages_merg",choices=paste0("page ",seq(1,pages,1)))
    
  })

  pic_MergeLF_plot=reactive({
    validate(
      need(isTruthy(input$Sel_yrs), 'Processing...')
    )
    req(Final_stock())
    req(LBB_data())
    req(input$Sel_yrs)
    LBB_data_=LBB_data()
    g.b_=data.frame(Year=as.integer(sort(unique(LBB_data_$Year))),g.b=rep("A",length(unique(LBB_data_$Year))))
    g.b_$g.b[g.b_$Year %!in% input$Sel_yrs]="B"
    
    if (input$Change_width==1) {
      LBB_data_=LBB_data_
    } else if (input$Change_width>1) {
      
      if(Final_stock()[["LBB_ID"]]$Cr_sel=="created" ) {
        wdth=input$length_width} else {
          wdth=as.numeric(Final_stock()[["LBB_ID"]]$Lc.user)/10
        }
      
      
      wdth=wdth*input$Change_width
      max_LC=unique(max(LBB_data_$Length[LBB_data_$CatchNo>0],na.rm=T))
      classes=seq(0,max_LC,wdth)
      clasnms=classes#+wdth/2
      clasnms=clasnms[1:length(clasnms)-1]
      LBB_data_$LC=as.numeric(as.character(cut(LBB_data_$Length,breaks =classes,labels = clasnms,right = F )))
      LBB_data_=aggregate(CatchNo   ~Stock +Year +LC , data = LBB_data_, sum)
      colnames(LBB_data_)= c("Stock",   "Year","Length",  "CatchNo")
      LBB_data_=LBB_data_[order(LBB_data_$Year),]
    }
    years=unique(LBB_data_$Year)
    pages=ceiling(length(years)/input$LFDs_per_page)    
    years_=split(years, ceiling(seq_along(years)/input$LFDs_per_page))
    LBB_data_$CatchNo[LBB_data_$Length<=input$L.cut]=0
    
    i = 0 # start counter
    sss2=list()
    for(Year in years) {
      i = i+1 # i is the index of Years, which may contain gaps 
      # if MergeLF==TRUE and if this is the second or heigher year and no simulation, aggregate LF with previous year LF
      if(input$MergeLF==TRUE) {
        if(i==1) {AG.yr <- c(Year,years[2])} else { # if first year, aggregate with second year
          AG.yr <- c(years[i-1],Year) }
      } else AG.yr <- Year
      
      # aggregate data within the year (sometimes there are more than one sample per year)
      df        <- data.frame(LBB_data_[LBB_data_$Year%in%AG.yr,])
      df=df[,-1]
      names(df) <- c("Year","Length","Freq")
      LF.y      <- AG(dat=df) # function to aggregate data by year and across years
      LF.y$Freq <- LF.y$Freq/sum(LF.y$Freq) # standardize frequencies
      LF.y$Year=Year
      LF.y=LF.y[,c("Year","Length","Freq" )]
      sss2[[i]]=LF.y
      
    }
    LBB_data_=rbindlist(sss2)
    LBB_data_$max_catch=ave(LBB_data_$Freq,LBB_data_$Year,FUN = max)
    #LBB_data_=LBB_data_%>%left_join(g.b_)
    LBB_data_=base::merge(x = LBB_data_, y = g.b_, all.x = TRUE)
    
    LBB_data_=LBB_data_[order(LBB_data_$Year),]
    
    pics=list()
    
    if(input$Change_view==FALSE) { 
      
      pics[[1]]=ggplot(data=LBB_data_[LBB_data_$Year %in% years_[[1]],], aes(x=Length, y=as.factor(Year), height = Freq, group = as.factor(Year)),fill="#F8766D") + 
        geom_density_ridges(stat = "identity", scale = 1,alpha=0.7)+ggplot2::theme_classic()+ggplot2::labs(y="Year", x="LC")
      
      for (i in 1:pages) {
        pics[[i]]= ggplot(data=LBB_data_[LBB_data_$Year %in% years_[[i]],], aes(x=Length, y=Freq))+
          geom_rect(data=LBB_data_[LBB_data_$Year %in% years_[[i]],],aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=g.b), alpha=0.1) +
          geom_line(col="#F8766D")+
          #geom_point(shape=21,fill="#F8766D")+
          geom_area(fill="#F8766D") +
          scale_fill_manual(values = c("A"="white","B"="#F8766D"))+
          geom_vline(aes(xintercept = input$L.cut,),size=1)+
          geom_text(aes(y =max_catch/4, x = input$L.cut, label = paste0("L.cut")),col="red")+
          geom_rect(aes(xmin=input$Linf.user-input$Linf_CV*input$Linf.user, 
                        xmax=input$Linf.user+input$Linf_CV*input$Linf.user, ymin=-Inf, ymax=Inf, ),fill="lightgray", color="lightgray", alpha=0.05) +
          geom_vline(aes(xintercept = input$Linf.user),col="gray",size=1)+
          geom_text(aes(y =max_catch/4, x = input$Linf.user, label = paste0("Linf")),col="black")+#,position=position_nudge(y= .1)
          labs(fill="",x="Length (cm)")+
          facet_wrap(~Year, scales="free_y", ncol=1)+theme_bw()
      }
      
    } else {
      pics[[1]]=ggplot(data=LBB_data_[LBB_data_$Year %in% years_[[1]],], aes(x=Length, y=Freq))+
        geom_line()+
        geom_point(shape=21,fill="#F8766D")+
        facet_wrap(~Year, ncol=2)+theme_bw()#, scales="free_y"

      for (i in 1:pages) {
        
        pics[[i]]= ggplot(data=LBB_data_[LBB_data_$Year %in% years_[[i]],], aes(x=Length, y=Freq))+
          geom_rect(data=LBB_data_[LBB_data_$Year %in% years_[[i]],],aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=g.b), alpha=0.1) +
          geom_line()+
          geom_point(shape=21,fill="#F8766D")+
          scale_fill_manual(values = c("A"="white","B"="#F8766D"))+
          geom_vline(aes(xintercept = input$L.cut,),size=1)+
          geom_text(aes(y=max_catch/4, x = input$L.cut, label = paste0("L.cut")),col="red")+
          geom_rect(aes(xmin=input$Linf.user-input$Linf_CV*input$Linf.user, 
                        xmax=input$Linf.user+input$Linf_CV*input$Linf.user, ymin=-Inf, ymax=Inf, ),fill="lightgray", color="lightgray", alpha=0.05) +
          geom_vline(aes(xintercept = input$Linf.user),col="gray",size=1)+
          geom_text(aes(y =max_catch/4, x = input$Linf.user, label = paste0("Linf")),col="black")+
          labs(x="Length (cm)")+
          facet_wrap(~Year, scales="free_y", ncol=2)+theme_bw()
      }
    }
    p_1=pics[[as.numeric(gsub("page ","",input$LBB_pages_merg))]]
    p_2=pics
    p_all=list(p_1,p_2)
  })
  
  output$MergeLF_plot= shiny::renderPlot({
    print(pic_MergeLF_plot()[[1]])
  })#, height = 340
  
  
  output$When_happy=shinydashboard::renderValueBox({
    shinydashboard::valueBox( shiny::h4("You can now proceed to'Run the model' tab"),shiny::h5(" "),
                              icon = shiny::icon("envelope"),color = "light-blue")})
  
  sem_iter=reactive({
    req(Final_stock())
    cr.sel=Final_stock()[["LBB_ID"]]$Cr_sel
    if(cr.sel=="created" ) {
      sem.iter=input$semelparous} else {
        sem.iter=input$semelparous2}
    return(sem.iter)
  })
  
  Run_AG= eventReactive(input$make_ag_lfd,{
    req(LBB_data())
    req(Final_stock())
    LBB_data_=LBB_data()
    LBB_data_=LBB_data_[LBB_data_$CatchNo>0,]
    
    if(Final_stock()[["LBB_ID"]]$Cr_sel=="created" ) {
      wdth=input$length_width
 } else {    # munit=input$unit_col
        wdth=as.numeric(Final_stock()[["LBB_ID"]]$Lc.user)/10
       # munit="mm"              
     }
 AG_=LBB.AG(LBB_data_,input$Sel_yrs,wdth,input$Change_width,input$L.cut) #,munit
      
 return(AG_)
  }) 
  
  Start_L=reactive({
    req(Run_AG())
   AA= L.start(Run_AG(),input$Linf.user)
    return(AA)
  })
  
  observe({
    req(Run_AG())
    req(Final_stock())
    if(Final_stock()[["LBB_ID"]]$Cr_sel=="created" ) {
      wdth=input$length_width
    } else {    # munit=input$unit_col
      wdth=as.numeric(Final_stock()[["LBB_ID"]]$Lc.user)/10
      # munit="mm"              
    }
    updateSliderInput(session,"Lstart.user",value=Start_L()[1], max=input$Linf.user,step=0.1)
    #  }
  })
  
  observe({
    req(Run_AG())
    req(Final_stock())
    if(Final_stock()[["LBB_ID"]]$Cr_sel=="created" ) {
      wdth=input$length_width
    } else {    # munit=input$unit_col
      wdth=as.numeric(Final_stock()[["LBB_ID"]]$Lc.user)/10
      # munit="mm"              
    }
    updateSliderInput(session,"Lc.user",value=Start_L()[2], max=input$Linf.user,step=0.1)
    #  }
  })
  
  observe({
    req(Run_AG())
    LBB_AG=Run_AG()
    LBB_AG$Length[which.max(LBB_AG$Freq)]
     wt    <- wtd.mean(LBB_AG$Length,LBB_AG$Freq)
   # updateSliderInput(session,"Lmean.user",value=wt, max=input$Linf.user,step=0.1)
    updateSliderInput(session,"Lmean.user",value=wt,min=round(0.5*wt,1) ,max=round(wt+0.5*wt,1),step=0.1)
    
    #  }
  })
 
   observe({
    req(Run_AG())
    LBB_AG=Run_AG()
    LBB_AG$Length[which.max(LBB_AG$Freq)]
    var   <- wtd.var(LBB_AG$Length,LBB_AG$Freq)
    std   <- sqrt(var)
    
    
    updateSliderInput(session,"LmeanCV.user",value=std,min=round(0.5*std,1), max=round(2*std,1),step=0.1)
    #  }
  })
  
  
  Run_nls=reactive({
    req(Run_AG())
  sss=Run.nls(Run_AG(),input$Linf.user,input$Linf_CV,c(input$Lstart.user,input$Lc.user))#Start_L()[2]
    return(sss)
  }) 
  
  
  run_pictures <- reactiveValues()
  

 pic_AG_see=reactive({
   req(Run_AG())
   req(Start_L())
   req(Run_nls())
   
  LF_ALL= Run_AG()
   df1=Run_nls()[["df_fornls"]]
   ZK.nls= Run_nls()[["parameters"]][3]
    Lstart.i    <- which(LF_ALL$Length>=input$Lstart.user)[1]
    Lstart.Freq <- mean(c(LF_ALL$Freq[(Lstart.i-1):(Lstart.i+1)]))
   
   x=df1[,1]
   y=Lstart.Freq*exp(ZK.nls*(log(1-x/input$Linf.user)-log(1-x[1]/input$Linf.user)))
     SS=data.frame(x=x,y=y)
   
     
     if(input$GausSel==F) {
   pic_AG=ggplot(data=LF_ALL, aes(x=Length, y=Freq))+
    # geom_line()+
     geom_point(size=3,shape=21,fill="#F8766D")+
     geom_line(color="#F8766D")+
      theme_bw()+
     geom_line(data=SS,aes(x=x,y=y),color="blue",size=2)+
     geom_vline(aes(xintercept = Start_L()[1]),color="gray",size=1)+
     geom_vline(aes(xintercept = Start_L()[2]),color="gray",linetype="dashed",size=1)+
       geom_vline(aes(xintercept = input$Lc.user),color="#00BA38",linetype="dashed",size=1)+
     geom_text(aes(y = 0.2, x = input$Lc.user, label = paste0("Lc.prior")),size=6,col="#00BA38")+
      geom_vline(aes(xintercept = input$Lstart.user),color="#00BA38",size=1)+
     geom_text(aes(y = 0.2, x = input$Lstart.user, label = paste0("L.full")),size=6,col="#00BA38")+
     geom_vline(aes(xintercept =input$Linf.user),color="#619CFF",size=1)+
   geom_text(aes(y = 0.2, x = input$Linf.user, label = paste0("Linf")),size=6,col="#619CFF")
   
   if (input$Linf.user-input$Lstart.user<5) {
     pic_AG=ggplot(data=LF_ALL, aes(x=Length, y=Freq))+
       geom_rect(mapping=aes(xmin=input$Lstart.user, xmax=input$Linf.user,
                                          ymin=0, ymax=Inf ),fill="red", color="red", alpha=0.2)+# geom_line()+
       geom_point(size=3,shape=21,fill="#F8766D")+
       geom_line(color="#F8766D")+
       theme_bw()+
       geom_line(data=SS,aes(x=x,y=y),color="blue",size=2)+
       geom_vline(aes(xintercept = Start_L()[1]),color="gray",size=1)+
       geom_vline(aes(xintercept = Start_L()[2]),color="gray",linetype="dashed",size=1)+
       geom_vline(aes(xintercept = input$Lc.user),color="#00BA38",linetype="dashed",size=1)+
       geom_text(aes(y = 0.2, x = input$Lc.user, label = paste0("Lc.prior")),size=6,col="#00BA38")+
        geom_vline(aes(xintercept = input$Lstart.user),color="#00BA38",size=1)+
       geom_text(aes(y = 0.2, x = input$Lstart.user, label = paste0("L.full")),size=6,col="#00BA38")+
       geom_vline(aes(xintercept =input$Linf.user),color="#619CFF",size=1)+
       geom_text(aes(y = 0.2, x = input$Linf.user, label = paste0("Linf")),size=6,col="#619CFF")
   }} else {
     wt    <- wtd.mean(LF_ALL$Length,LF_ALL$Freq)
     var   <- wtd.var(LF_ALL$Length,LF_ALL$Freq)
     std   <- sqrt(var)
     min_lf=min(LF_ALL$Length)
     max_lf=max(LF_ALL$Length)
     lfs=seq(min_lf,max_lf,0.1)
deaf_gaus=data.frame(L=lfs,y=dnorm(lfs,mean=wt,sd=std)/max(dnorm(lfs,mean=wt,sd=std)))
deaf_gaus=deaf_gaus[deaf_gaus$L<=1.05*wt,]    

new_gaus=data.frame(L=lfs,y=dnorm(lfs,mean=input$Lmean.user,
                                            sd=input$LmeanCV.user)/max(dnorm(lfs,mean=input$Lmean.user,sd=input$LmeanCV.user)))
new_gaus=new_gaus[new_gaus$L<=1.05*input$Lmean.user,]    

       pic_AG=ggplot(data=LF_ALL, aes(x=Length, y=Freq))+
       # geom_line()+
       geom_point(size=3,shape=21,fill="#F8766D")+
       geom_line(color="#F8766D")+
       theme_bw()+
       geom_line(data=deaf_gaus,aes(x=L,y=y),color="gray",size=2)+
       geom_line(data=new_gaus,aes(x=L,y=y),color="blue",size=2)+
         geom_vline(aes(xintercept = input$Lmean.user),color="#00BA38",size=1)+
         geom_text(aes(y = 0.2, x = input$Lmean.user, label = paste0("Lmean")),size=6,col="#00BA38")+
         geom_vline(aes(xintercept =input$Linf.user),color="#619CFF",size=1)+
       geom_text(aes(y = 0.2, x = input$Linf.user, label = paste0("Linf")),size=6,col="#619CFF")

   }
   return(pic_AG)
   
 }) 
 
   output$Check_AG_LBB_plot= shiny::renderPlot({
     print(pic_AG_see())
   })

   
   output$Selectivity_plots= shiny::renderPlot({
     l=seq(0,180,0.1)
     k=54
     s1=0.06
     s2=0.10
     
     nm=vector()
     for (i in 1:length(l)) {
       nm[i]=exp((-(l[i]-k)^2)/2*s1^2)
     }
     nm2=vector()
     for (i in 1:length(l)) {
       nm2[i]=exp((-(l[i]-k)^2)/2*s2^2)
     }
     
     
     n_df=data.frame(L=l/180,nm=nm)
     n_df2=n_df[n_df$L<=n_df$L[n_df$nm==max(n_df$nm)],]
     n_df3=n_df[n_df$L>n_df$L[n_df$nm==max(n_df$nm)],]
     
     n_dfb=data.frame(L=l/180,nm=nm2)
     n_dfb2=n_dfb[n_dfb$L<=n_dfb$L[n_dfb$nm==max(n_dfb$nm)],]
     n_dfb3=n_dfb[n_dfb$L>n_dfb$L[n_dfb$nm==max(n_dfb$nm)],]
     
     
     
     p1=ggplot()+geom_vline(xintercept=n_df$L[n_df$nm==max(n_df$nm)],col= "#F8766D",linetype="dashed")+
       geom_line(data=n_df2,aes(x=L,y=nm),linetype="solid",size=1.5,col="black")+
       geom_line(data=n_dfb3,aes(x=L,y=nm),linetype="solid",size=1.5,col="black")+
       geom_line(data=n_df2,aes(x=L,y=nm),linetype="dashed",size=1.5,col="#619CFF")+
       
       geom_line(data=n_df3,aes(x=L,y=nm),linetype="dashed",size=1.5,col="#619CFF")+
       geom_line(data=n_df3,aes(x=L,y=nm),linetype="solid",size=1.5,col="#619CFF",alpha=0.1)+theme_bw()+
       labs(x="Relative length (L/Linf)",y="Relative Frequency", title = "Gillnet-like selection")+
       scale_y_continuous(limits = c(0,1.2))+
       scale_x_continuous(limits = c(0,1))+
       theme(axis.text.y=element_blank(),
             axis.ticks.y=element_blank() )#,
     
     k=60
     l=seq(0,200,0.1)
     s1=0.06
     s2=0.025
     y=vector()
     for (i in 1:length(l)) {
       if (l[i]<k) {
         y[i]=exp((-(l[i]-k)^2)/2*s1^2)} else
           y[i]=exp((-(l[i]-k)^2)/2*s2^2)
     }
     
     n_df=data.frame(L=l/200,y=y)
     # n_df$cumy=cumsum(n_df$y)
     # n_df$cumy=n_df$cumy/max(n_df$cumy)
     
     n_df2=n_df[n_df$L<n_df$L[n_df$y==max(n_df$y)],]
     n_df3=n_df[n_df$L>=n_df$L[n_df$y==max(n_df$y)],]
     
     p2=ggplot()+geom_vline(xintercept=n_df$L[n_df$y==max(n_df$y)],col= "#F8766D",linetype="dashed")+
       geom_line(data=n_df,aes(x=L,y=y),size=1.5,col="black")+theme_bw()+
       geom_segment(aes(x =n_df$L[n_df$y==max(n_df$y)], y = 1, xend = 1, yend = 1),size=1.5,col="#619CFF",linetype="dashed")+
       # geom_line(data=n_df3,aes(x=L,y=y),linetype="solid",size=2,col="#619CFF",alpha=0.4)+theme_bw()+
       # geom_line(data=n_df3,aes(x=L,y=y),linetype="solid",size=2,col="#619CFF")+
       geom_line(data=n_df2,aes(x=L,y=y),size=1.5,col="#619CFF",linetype="dashed")+theme_bw()+
       labs(x="Relative length (L/Linf)",y="Relative Frequency", title = "Trawl-like selection")+
       scale_y_continuous(limits = c(0,1.2))+
       theme(axis.text.y=element_blank(),
             axis.ticks.y=element_blank()#,
             # axis.text.x=element_blank()
       )
     temp_F=ggpubr::ggarrange(p2,p1,
                              labels=c("A","B"),
                              ncol =2,nrow = 1)
     print(temp_F)
     })
   
 
   observe({
     req(pic_AG_see())
     run_pictures$pic_A=pic_AG_see()
   })
   
  ######################### save stock object
  LBB_object=eventReactive(input$Save_priors,{
    req(LBB_data())
    req(Final_stock())

    if(Final_stock()[["LBB_ID"]]$Cr_sel=="created" ) {
      wdth=input$length_width
     } else {# munit=input$unit_col
        wdth=as.numeric(Final_stock()[["LBB_ID"]]$Lc.user)/10
       # munit="mm"              
     }
    
    Species=Final_stock()[["LBB_ID"]]$Species
    Name=Final_stock()[["LBB_ID"]]$Name
    
     # LBB.obj=LBB.pre.fit(LBB_data(),input$Sel_yrs,wdth,input$Change_width,input$L.cut,input$Linf.user,input$Linf_CV,input$MK.user,input$MK_CV,Lstart.user_)
      LBB.obj=LBB.obj.create(LBB_data(),Species,Name,input$Sel_yrs,wdth,input$Change_width,input$L.cut,Run_nls(),
                             input$MK.user,input$MK_CV,Start_L(),input$GausSel,sem_iter(),input$MergeLF,input$Lm50.user,input$Lmean.user,input$LmeanCV.user)#,munit
  return(LBB.obj)
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
    req(LBB_data())
    updateTextInput(session, "obj.name",value=paste0(unique(LBB_data()$Stock), "_V1") ) #, placeholder=paste0("e.g.  ",Final_stock()[["Catch_ID"]]$Stock, "_V1")
  })
  # 
  object_NAME=reactive({
    req(LBB_object())
    if (input$obj.name=="") {
      name=paste0(LBB_object()[["model.param"]]$Stock, "_V1")
    } else {
      name=input$obj.name
    }
    return(name)
  })
  # 
  observeEvent(input$Save_priors, {
    req(object_NAME())
    LBB.object <- LBB_object()
    dir.create( paste0(dir.name(),"/LBB"))
    dir=paste0(paste0(dir.name(),"/LBB"))

    save(LBB.object,file =paste0(dir,"/", object_NAME(), ".RData"))
    Save_done <- showNotification(paste("Message:",  "The stock object with the input parameterization has been saved in ", paste0(dir.name(),"/LBB/",object_NAME(), ".RData")), duration = 5)

  })
  # 
  objects <- reactiveVal(data.frame(A.A=rep(NA,100),Created_Stocks=rep(NA,100)))
  count <- reactiveVal(0)

  observeEvent(input$Save_priors, {
    count(count() + 1)
  })

  observeEvent(input$Save_priors,{
    # start with current data
    objects() %>%
     tibble::add_row(
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
    filenames <- list.files(paste0(dir.name(),"/LBB"), pattern="*.RData", full.names=F)
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
      sdsd=LBB_object() } else if (input$Id049=="B") {
        asa=paste0(dir.name(),"/LBB/",input$Id081,".RData")
        load(asa)
        sdsd=LBB.object
      }
    return(sdsd)
  })
  
  fstc <- fstc %>% debounce(500)
  
  observeEvent(input$see_obj,{
    req(fstc())
    xxx=as.data.frame(t( fstc()[["model.param"]]))
    xxx$parameter=row.names(xxx)
    colnames(xxx)[1]="value"
    xxx=xxx[,c(2,1)]
    output$see_object_table=renderTable(xxx)
  })
  
  observe({
    output$Zavarakatranemia=renderUI({ shiny::h4(tagList("Run the LBB model and get the results for the stock ",  tags$b(fstc()[["model.param"]]$Stock), "of",  tags$em(fstc()[["model.param"]]$Species)))})
  })
  ############### SPECIES NAME

  output$Run_infobox_patient=shinydashboard::renderValueBox({
    shinydashboard::valueBox( shiny::h4("Run the LBB model"),shiny::h5("Press 'Start' to run the LBB model to fit LF curves (red) to the annual length frequencies. 
              If there is a year with a particularly bad fit, go back to '3. Prepare data' and exclude that year. It takes some time to do all the necessary calculations, so be patient!"),
                              icon = shiny::icon("envelope"),
                              color = "light-blue")
  })

  LBB_run=eventReactive(input$Start_run,{
  x=LBB.fit(fstc())
  return(x)
  })
  
  observeEvent(input$Start_run, {
    req(LBB_run())
    Model_run <- showNotification(paste("Message: ", "Model run completed"), duration = 5)
  })
  
  Bad_years=eventReactive(input$Start_run,{
    req(LBB_run())
    b.y=Bad.years(LBB_run())
    return(b.y)
  })

  observe({
    req(LBB_run())
    req(input$LFDs_per_page2)
    LBB_data_=LBB_run()
    years=LBB_data_[["model.outcomes"]]$Year
    pages=ceiling(length(years)/input$LFDs_per_page2)
    updateAwesomeRadio(session,"LBB_pages2",choices=paste0("page ",seq(1,pages,1)))
  })
  
  pic_LBB_2=reactive({
    req(LBB_run())
    req(Bad_years())
    b.y=Bad_years()
    LBB_run_=LBB_run()
    LBB_data_=LBB_run_[["input.data"]]
    Lfit=LBB_run_[["Lfit"]]
    LBB_outcomes_= LBB_run_[["model.outcomes"]]
    years=sort(unique(LBB_data_$Year))
    pages=ceiling(length(years)/input$LFDs_per_page2)    
    years_=split(years, ceiling(seq_along(years)/input$LFDs_per_page2))
    
    DATA=list()
    for (i in 1:length(years)) {
      DATA[[i]]=data.frame(Year=rep(years[i],length(Lfit[i,1][[1]])),
                           r.L.y=Lfit[i,1][[1]],
                           r.Freq.y=Lfit[i,2][[1]],
                           Freq.pred=Lfit[i,3][[1]] )
    }
    r.opt.obj=data.frame(Year=LBB_run_[["model.outcomes"]]$Year,r.Lopt= LBB_run_[["model.outcomes"]]$r.Lopt)
    r.opt.obj_med=median(r.opt.obj$r.Lopt)
    DATA_= data.table::rbindlist(DATA)
    #DATA_=DATA_%>%left_join(b.y)
    DATA_=base::merge(x = DATA_, y = b.y, all.x = TRUE)
    
    DATA_$frq_y=ave(DATA_$Freq.pred,DATA_$Year,FUN=sum )
    DATA_$max_r.Freq.y=ave(DATA_$r.Freq.y,DATA_$Year,FUN=max )
    DATA_$frq_y=DATA_$Freq.pred/DATA_$frq_y
    #DATA_=DATA_%>%left_join(r.opt.obj)
    DATA_=base::merge(x = DATA_, y = r.opt.obj, all.x = TRUE)
    
    #LBB_outcomes_=LBB_outcomes_%>%left_join(DATA_)
    LBB_outcomes_=base::merge(x = LBB_outcomes_, y = DATA_, all.x = TRUE)
    
    pics=list()
     GausSel=LBB_run()[["model.param"]]$GausSel

    if (GausSel==F) {
    for (i in 1:pages) {
      pics[[i]]=   ggplot(data=DATA_[DATA_$Year %in% years_[[i]],])+
        geom_rect(data=DATA_[DATA_$Year %in% years_[[i]],],aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=G.B), alpha=0.05) +
        geom_line(aes(x=r.L.y,y= frq_y),color="red",size=1)+
        geom_point(aes(x=r.L.y,y= r.Freq.y),shape=21,fill="#619CFF",size=2)+
        geom_vline(aes(xintercept =1),col="darkgreen",size=1)+
        geom_text(aes(y = 0,x = 1, label = paste0("Linf")), nudge_y=0.01,col="darkgreen")+
        geom_vline(aes(xintercept =r.opt.obj_med),col="darkgreen",size=1)+
        geom_text(aes(y = 0,x = r.opt.obj_med, label = paste0("Lopt")), nudge_y=0.01,col="darkgreen")+
        geom_vline(aes(xintercept =r.Lopt),col="black",linetype="dashed")+
       # geom_text(aes(y = 0,x = r.Lopt, label = paste0("Lopt")), nudge_y=0.03,col="darkgreen")+
        geom_text(data=LBB_outcomes_[LBB_outcomes_$Year %in% years_[[i]],], 
                  aes(y =max_r.Freq.y/2,x =.1, label = paste0("Linf=",format(Linf,digits=3))), nudge_y=0.01,col="black")+
        geom_text(data=LBB_outcomes_[LBB_outcomes_$Year %in% years_[[i]],], 
                  aes(y =max_r.Freq.y/4,x =.1, label = paste0("Z/K=",format(MK+FK,digits=3))), nudge_y=0.01,col="black")+
            scale_x_continuous(limits = c(0,1),breaks = seq(0,1,0.25))+
       scale_fill_manual(values = c("YES"="white","NO"="#F8766D"),labels=c("",""))+
        labs(x="Length/Linf",y="Relative frequency",fill="")+#
        facet_wrap(~Year, scales="free_y",ncol=2)+theme_bw()
    }
    } else {
      for (i in 1:pages) {
        pics[[i]]=   ggplot(data=DATA_[DATA_$Year %in% years_[[i]],])+
          geom_rect(data=DATA_[DATA_$Year %in% years_[[i]],],aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=G.B), alpha=0.05) +
          geom_line(aes(x=r.L.y,y= frq_y),color="red",size=1)+
          geom_point(aes(x=r.L.y,y= r.Freq.y),shape=21,fill="#619CFF",size=2)+
          geom_vline(aes(xintercept =1),col="darkgreen",size=1)+
          geom_text(aes(y = 0,x = 1, label = paste0("Linf")), nudge_y=0.01,col="darkgreen")+
          geom_vline(aes(xintercept =r.opt.obj_med),col="darkgreen",size=1)+
          geom_text(aes(y = 0,x = r.opt.obj_med, label = paste0("Lopt")), nudge_y=0.01,col="darkgreen")+
          geom_vline(aes(xintercept =r.Lopt),col="black",linetype="dashed")+
         # geom_text(aes(y = 0,x = r.Lopt, label = paste0("Lopt")), nudge_y=0.03,col="darkgreen")+
          geom_text(data=LBB_outcomes_[LBB_outcomes_$Year %in% years_[[i]],], 
                    aes(y =max_r.Freq.y/2,x =.1, label = paste0("Linf=",format(Linf,digits=3))), nudge_y=0.01,col="black")+
          # geom_text(data=LBB_outcomes_[LBB_outcomes_$Year %in% years_[[i]],], 
          #           aes(y =max_r.Freq.y/4,x =.1, label = paste0("Z/K=",format(MK+FK,digits=3))), nudge_y=0.01,col="black")+
          scale_x_continuous(limits = c(0,1),breaks = seq(0,1,0.25))+
          scale_fill_manual(values = c("YES"="white","NO"="#F8766D"),labels=c("",""))+
          labs(x="Length/Linf",y="Relative frequency",fill="")+#
          facet_wrap(~Year, scales="free_y",ncol=2)+theme_bw()
      }
    }
    p_1=pics[[as.numeric(gsub("page ","",input$LBB_pages2))]]
    p_2=pics
    pis=list(p_1,p_2)
      })
  
  output$LFDs_2= shiny::renderPlot({
    print(pic_LBB_2()[[1]])
  })#, height = 340
  
  observeEvent(input$Start_run, {
    for (i in 1:100) {
      shinyWidgets::updateProgressBar(
        session = session,
        id = "prog_bar",
        value = i, total = 100,
        title = paste("Process", trunc(i/10)))
      Sys.sleep(0.01)
    }
  })

  observe({
    output$Zavarakatranemia2=renderUI({ shiny::h4(tagList("Run the Yield/Recruit model and get the results for the stock ",  tags$b(fstc()[["model.param"]]$Stock), "of",  tags$em(fstc()[["model.param"]]$Species)))})
  })
  
  output$Run_BH_patient=shinydashboard::renderValueBox({
    shinydashboard::valueBox( shiny::h4("Run the Yield/Recruit model"),shiny::h5("Press 'Start' to estimate relative biomass and fisheries reference points from Yield/Recruit equations."),
                              icon = shiny::icon("envelope"),
                              color = "light-blue")
  })
    BH_run=eventReactive(input$BH_run_button,{
   # req(sem_iter())
      x=  BH.fit(LBB_run())
    return(x)
  })
  
  observeEvent(input$BH_run_button, {
    req(BH_run())
    Model_run <- showNotification(paste("Message: ", "BH Model run completed"), duration = 5)
  })
  
  BH_rfpoints=eventReactive(input$BH_run_button,{
    req(BH_run())
    x=  median.rfpoints(BH_run())
    return(x)
  })
 
   observeEvent(input$BH_run_button, {
    req(BH_rfpoints())
    Model_run <- showNotification(paste("Message: ", "Reference points calculation completed"), duration = 5)
  })
  
  smooth_ts=reactive({
    req(BH_run())
    x=  ploting.ts(BH_rfpoints(),input$smooth_ts_button)
    return(x)
  })
  
  observeEvent(input$BH_run_button, {
    req(smooth_ts())
    Model_run <- showNotification(paste("Message: ", "Smooth time series calculation completed"), duration = 5)
  })
  
  
  output$legend_1=renderUI({
    if (BH_run()[["model.param"]]$GausSel==T) {
      shiny::h5("Mean length in catch (Lmean) relative to Lopt and L_F=M (and Lm, if provided).")
    } else {
      shiny::h5("Mean length in catch (Lmean) relative to Lopt, Lc_opt and L_F=M (and Lm, if provided).")
    }
  })
  
  lc.plot=reactive({
    req(smooth_ts())
    req(BH_rfpoints())
   x= ggplot.lc(BH_rfpoints(),smooth_ts())
    return(x)
  })

    output$lc_plot= shiny::renderPlot({
    print(lc.plot())
  })#, height = 340

    observe({
      req(lc.plot())
      run_pictures$pic_B=lc.plot()
    })
    
  fm.plot=reactive({
    req(smooth_ts())
    x= ggplot.fm(smooth_ts())
    return(x)
  })
  
  output$fm_plot= shiny::renderPlot({
    print(fm.plot())
  })#, height = 340
  
  observe({
    req(fm.plot())
    run_pictures$pic_C=fm.plot()
  })
  
  bb0.plot=reactive({
    req(smooth_ts())
    req(BH_rfpoints())
    x= ggplot.bb0(BH_rfpoints(),smooth_ts())
    return(x)
  })

  output$bb0_plot= shiny::renderPlot({
    print(bb0.plot())
  })#, height = 340
  
  observe({
    req(bb0.plot())
    run_pictures$pic_D=bb0.plot()
  })
  
  
 hist.plot=reactive({
    req(BH_run())
    x= ggplot.histMSFD(BH_run())
    return(x)
  })
  
  output$hist_plot= shiny::renderPlot({
    print(hist.plot())
  })#, height = 340
  
  observe({
    req(hist.plot())
    run_pictures$pic_E=hist.plot()
  })
  
  
  
  
  ####################### Stock info
  ####################### Stock info
  ####################### Stock info
  ####################### Stock info

  output$ModOutp_a1a=renderUI({HTML(paste0(tags$b("LBB results for "),tags$em(BH_rfpoints()[["model.param"]]$Species), tags$b(ifelse(sem_iter()==T, " (Semelparous species)",
                                                                                                                              ""))
                                          ))
    })
  
  
  output$ModOutp_a1b=renderUI({HTML(paste0(tags$b("Selection: "), tags$b(ifelse(input$GausSel==T, "Gillnet-like selectivity (Gaussian)","Trawl-like selectivity"))
  ))
  })
  
  
  
  output$ModOutp_a1c=renderUI({HTML(paste0(tags$b("Stock: "),  BH_rfpoints()[["model.param"]]$Stock,", ",min(BH_rfpoints()[["model.outcomes"]]$Year),"-",
                                          max(BH_rfpoints()[["model.outcomes"]]$Year)
  ))
  })
  
  
  output$ModOutp_a2a=renderUI({HTML(paste0(tags$b("Linf prior = "),BH_rfpoints()[["model.param"]]$Linf.nls," cm, SD=",
                                          format(BH_rfpoints()[["model.param"]]$Linf.nls.sd,digits=2),
                                          " cm "
                                          ))})

  output$ModOutp_a2b=renderUI({HTML(paste0(tags$b("Lmax = "), BH_rfpoints()[["model.param"]]$max_length," cm, median annual Lmax=",
                                          BH_rfpoints()[["model.param"]]$median_max_length,
                                          " cm "
                                          ))})
  
  
  
  output$ModOutp_a3a=renderUI({
    if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
      
    HTML(paste0(tags$b("Z/K prior = "),format(BH_rfpoints()[["model.param"]]$ZK.nls,digits=2),", SD=",
                                          format(BH_rfpoints()[["model.param"]]$ZK.nls.sd,digits=2)
                                          ))}
      })
  
  output$ModOutp_a3b=renderUI({HTML(paste0(tags$b("M/K prior = "),BH_rfpoints()[["model.param"]]$MK.user,
                                           ", SD=",BH_rfpoints()[["model.param"]]$MK.user.CV))})
  
  
  output$ModOutp_a4=renderUI({
    if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
    HTML(paste0(tags$b("F/K prior = "),format(BH_rfpoints()[["model.param"]]$FK.pr,digits=2),
                                           " (wide range with tau=4 in log-normal distribution)" 
                                           ))
    }
    }) 
  
  output$ModOutp_a5a=renderUI({
    if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
    HTML(paste0(tags$b("Lc prior  = "),format(BH_rfpoints()[["model.param"]]$Lc.pr,digits=2),
                                          ", SD=",format(BH_rfpoints()[["model.param"]]$Lc.sd.pr,digits=2),
                                          ", alpha prior=",format(BH_rfpoints()[["model.param"]]$r.alpha.pr,digits=2),
                                          ", SD=",format(BH_rfpoints()[["model.param"]]$r.alpha.sd.pr,digits=2)
  ))}
      }) 

  output$ModOutp_a5b=renderUI({
      HTML(ifelse(BH_rfpoints()[["model.param"]]$Lm50.user>0,paste(tags$b(" Lm50 = "),BH_rfpoints()[["model.param"]]$Lm50.user, " cm"), ""))
  }) 
  
  output$ModOutp_a6=renderUI({HTML(paste0(tags$b("Linf = "),format( BH_rfpoints()[["median.ref.points"]]$Linf.med,digits=3)," (",
                                          format(BH_rfpoints()[["median.ref.points"]]$Linf.lcl,digits=3),"-",
                                          format(BH_rfpoints()[["median.ref.points"]]$Linf.ucl,digits=3),") cm"
  ))})
  
  output$ModOutp_a7=renderUI({HTML(paste0(tags$b("Lopt = "),format( BH_rfpoints()[["median.ref.points"]]$Lopt.med,digits=2)," cm,",
                                          " Lopt/Linf=",
                                          format(BH_rfpoints()[["median.ref.points"]]$r.Lopt.med,digits=2)
  ))})
  
  output$ModOutp_a8a=renderUI({
    if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
      
     HTML(paste0(tags$b("Lc_opt = "),format( BH_rfpoints()[["median.ref.points"]]$Lc_opt.med,digits=2)," cm"
  ))}
    })
  
  output$ModOutp_a8b=renderUI({
    if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
    HTML(paste0(tags$b("Lc_opt/Linf = "),format(BH_rfpoints()[["median.ref.points"]]$Lc_opt.med/
                                                  BH_rfpoints()[["median.ref.points"]]$Linf.med,digits=2)
  ))}
      }) 
  
  output$ModOutp_a8c=renderUI({
    HTML(paste0(tags$b("Lmean if F=M = "),format(BH_rfpoints()[["median.ref.points"]]$LmeanFM,digits=2)," cm"
  ))
    })
  
  output$ModOutp_a9=renderUI({HTML(paste0(tags$b("M/K = "),format( BH_rfpoints()[["median.ref.points"]]$MK.med,digits=3),
                                          " (",
                                          format(BH_rfpoints()[["median.ref.points"]]$MK.lcl,digits=3),
                                          "-", format(BH_rfpoints()[["median.ref.points"]]$MK.ucl,digits=3),")"
  ))})
  
  output$ModOutp_a10=renderUI({HTML(paste0(tags$b("F/M = "),format( BH_rfpoints()[["median.ref.points"]]$FM.med,digits=3),
                                          " (",
                                          format(BH_rfpoints()[["median.ref.points"]]$FM.lcl,digits=3),
                                          "-", format(BH_rfpoints()[["median.ref.points"]]$FM.ucl,digits=3),")"
  ))})
  
  output$ModOutp_a11=renderUI({HTML(paste0(tags$b("F/K = "),format( BH_rfpoints()[["median.ref.points"]]$FK.med,digits=3),
                                           " (",
                                           format(BH_rfpoints()[["median.ref.points"]]$FK.lcl,digits=3),
                                           "-", format(BH_rfpoints()[["median.ref.points"]]$FK.ucl,digits=3),")"
  ))})
 
   output$ModOutp_a12=renderUI({HTML(paste0(tags$b("Z/K = "),format( BH_rfpoints()[["median.ref.points"]]$ZK.med,digits=3),
                                           " (",
                                           format(BH_rfpoints()[["median.ref.points"]]$ZK.lcl,digits=3),
                                           "-", format(BH_rfpoints()[["median.ref.points"]]$ZK.ucl,digits=3),")"
  ))})
  
   output$ModOutp_a13=renderUI({HTML(paste0(tags$b("B/B0 = "),format( BH_rfpoints()[["median.ref.points"]]$BB0.med,digits=2),
                                            " (",
                                            format(BH_rfpoints()[["median.ref.points"]]$BB0.lcl,digits=2),
                                            "-", format(BH_rfpoints()[["median.ref.points"]]$BB0.ucl,digits=2),")"
   ))})  
   
   output$ModOutp_a14=renderUI({
     if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
        HTML(paste0(tags$b("B/B0 F=M Lc=Lc_opt = "),format( BH_rfpoints()[["median.ref.points"]]$BFM1B0,digits=2)
        ))}
     })  
   
   output$ModOutp_a15=renderUI({
     if (BH_rfpoints()[["median.ref.points"]]$BB0.lcl < -0.4 || BH_rfpoints()[["median.ref.points"]]$BB0.ucl > 2) {
       HTML(paste0(tags$b("WARNING: Uncertainty in B/B0 estimate is much too wide, data are unsuitable for stock assessment!")
       ))
     }
   })  
   
   output$ModOutp_a16a=renderUI({
     if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
       HTML(paste0(tags$b("Y/R' = "),format( BH_rfpoints()[["median.ref.points"]]$YR.med,digits=2)," (",
                                            format(BH_rfpoints()[["median.ref.points"]]$YR.lcl,digits=2),
                                            "-", format(BH_rfpoints()[["median.ref.points"]]$YR.ucl,digits=2),")",
                                            ifelse(BH_rfpoints()[["median.ref.points"]]$BB0.med < 0.25," - (reduced: B/B0<0.25)","")
       ))} else {
         HTML(paste0(tags$b("Y/R' = "),format( BH_rfpoints()[["median.ref.points"]]$YR.med,digits=2)," (",
                     format(BH_rfpoints()[["median.ref.points"]]$YR.lcl,digits=2),
                     "-", format(BH_rfpoints()[["median.ref.points"]]$YR.ucl,digits=2),")",
                     ifelse(BH_rfpoints()[["median.ref.points"]]$BB0.med < 0.25," - (reduced: B/B0<0.25)","")
                     ))
   }
     })  
   
   output$ModOutp_a16b=renderUI({
     if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
       HTML(paste0(tags$b("Y/R' F=M Lc=Lc_opt = "), format(BH_rfpoints()[["median.ref.points"]]$YRFM1,digits=2)
       ))}
   })  
   
   
   output$ModOutp_a17=renderUI({
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     smooth.ts=input$smooth_ts_button
     HTML(paste0(tags$b("Estimates for",EndYear,ifelse(smooth.ts==TRUE,"(mean of last 3 years with data):",":"))
   ))})  
   
   output$ModOutp_a17b=renderUI({
     
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
       HTML(paste0(tags$b("Lmean = "), format(smooth_ts()$Lmean.ts[last],digits=4),  " (",
                   format(smooth_ts()$Lmean.lcl.ts[last],digits=4),
                   "-", format(smooth_ts()$Lmean.ucl.ts[last],digits=4),") cm"))
     }
   })  
  
   output$ModOutp_a18=renderUI({
       EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
      last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
       if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
         HTML(paste0(tags$b("Lc50 = "), format(smooth_ts()$Lc.ts[last],digits=3),
                  " (",
                  format(smooth_ts()$Lc.lcl.ts[last],digits=3),
                  "-", format(smooth_ts()$Lc.ucl.ts[last],digits=3),") cm"))
     }
        })  
  
   output$ModOutp_a19=renderUI({
     #if(dat.ID$GausSel==F){
           
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
  
     if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
    
     HTML(paste0(tags$b("Lc50/Linf = "), format(smooth_ts()$Lc.ts[last]/smooth_ts()$Linf.ts[last],digits=2),
                 " (",
                 format(smooth_ts()$Lc.lcl.ts[last]/smooth_ts()$Linf.ts[last],digits=2),
                 "-", format(smooth_ts()$Lc.ucl.ts[last]/smooth_ts()$Linf.ts[last],digits=2),")"
     ))
     } else {
       HTML(paste0(tags$b("Lmean = "), format(smooth_ts()$GLmean.ts[last],digits=3),
                  ", SD =",
                   format(smooth_ts()$SD.ts[last],digits=3)
       ))
     }
     })  
   
   output$ModOutp_a20=renderUI({
     #if(dat.ID$GausSel==F){
  
       EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
        if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
       
   
     HTML(paste0(tags$b("Lc95 = "), format((smooth_ts()$r.alpha.ts[last]/smooth_ts()$Linf.ts[last]*smooth_ts()$Lc.ts[last]-log(1/0.95-1))/(smooth_ts()$r.alpha.ts[last]/smooth_ts()$Linf.ts[last]),digits=3),
                 ", alpha=",
                 format(smooth_ts()$r.alpha.ts[last]/smooth_ts()$Linf.ts[last],digits=3),
                 " (", format(smooth_ts()$r.alpha.lcl.ts[last]/smooth_ts()$Linf.ts[last],digits=3),"-",
                 format(smooth_ts()$r.alpha.ucl.ts[last]/smooth_ts()$Linf.ts[last],digits=3),")"
     ))
     } else {
       HTML(paste0(tags$b("Lmean/Lopt= "), format(smooth_ts()$GLmean.ts[last]/(smooth_ts()$r.Lopt.ts[last]*smooth_ts()$Linf.ts[last]),digits=2)
       ))
     }
     })  
   
   output$ModOutp_a21=renderUI({
     #if(dat.ID$GausSel==F){
     if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
       
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     HTML(paste0(tags$b("Lmean/Lopt= "), format(smooth_ts()$Lmean.ts[last]/(smooth_ts()$r.Lopt.ts[last]*smooth_ts()$Linf.ts[last]),digits=2)
     ))
     }
     })  
   
   
   output$ModOutp_a22=renderUI({
     #if(dat.ID$GausSel==F){
     if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
       
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     HTML(paste0(tags$b("Lc50/Lc_opt = "), format(smooth_ts()$Lc.ts[last]/BH_rfpoints()[["median.ref.points"]]$Lc_opt.med,digits=2)
     ))
     }
     })  
   
   
   output$ModOutp_a23=renderUI({
     #if(dat.ID$GausSel==F){
     if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
       
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     HTML(paste0(tags$b("L95th = "),format(smooth_ts()$L95.ts[last],digits=3)," cm"
     ))
     }
     })  
   
   output$ModOutp_a24=renderUI({
     #if(dat.ID$GausSel==F){
     if (BH_rfpoints()[["model.param"]]$GausSel==F ) {
       
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     HTML(paste0(tags$b("L95th/Linf = "),format(smooth_ts()$L95.ts[last]/smooth_ts()$Linf.ts[last],digits=2)
     ))
     }
     })  
   
   output$ModOutp_a25=renderUI({
     #if(dat.ID$GausSel==F){

     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     HTML(paste0(tags$b("Mature = "),format(smooth_ts()$perc.mat[last]*100,digits=2),"%"
     ))
     
     })  

   output$ModOutp_a26=renderUI({
     #if(dat.ID$GausSel==F){
     
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     HTML(paste0(tags$b("F/M = "), format(smooth_ts()$FM.ts[last],digits=2),
                 " (",
                 format(smooth_ts()$FM.lcl.ts[last],digits=2),
                 "-", format(smooth_ts()$FM.ucl.ts[last],digits=2),")"
     ))})  
   
   output$ModOutp_a27=renderUI({
     #if(dat.ID$GausSel==F){
     
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     HTML(paste0(tags$b("F/K = "), format(smooth_ts()$FK.ts[last],digits=2),
                 " (",
                 format(smooth_ts()$FK.lcl.ts[last],digits=2),
                 "-", format(smooth_ts()$FK.ucl.ts[last],digits=2),")"
     ))})  
   
   output$ModOutp_a28=renderUI({
     #if(dat.ID$GausSel==F){
     
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     HTML(paste0(tags$b("Z/K = "), format(smooth_ts()$ZK.ts[last],digits=2),
                 " (",
                 format(smooth_ts()$ZK.lcl.ts[last],digits=2),
                 "-", format(smooth_ts()$ZK.ucl.ts[last],digits=2),")"
     ))})  
  # } else if(dat.ID$GausSel==T){
  #   cat("GLmean/Linf=",format(GLmean.ts[last]/Linf.ts[last],digits=2),",SD/Linf =",SD.ts[last]/Linf.ts[last],"\n")
  #   cat("GLmean     =",GLmean.ts[last],",SD =",SD.ts[last],"\n")
  # }

   output$ModOutp_a29=renderUI({
     #if(dat.ID$GausSel==F){
     
     EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     HTML(paste0(tags$b("Y/R' = "), format(smooth_ts()$YR.ts[last],digits=2),
                 " (",
                 format(smooth_ts()$YR.lcl.ts[last],digits=2),
                 "-", format(smooth_ts()$YR.ucl.ts[last],digits=2),")",
                 ifelse(BH_rfpoints()[["median.ref.points"]]$BB0.med < 0.25,"(reduced because B/B0 < 0.25)","")
     ))})  
  
   

   #output$test_orf=renderTable({as.data.frame(BH_run()[["jagsFit"]])})
   
   output$ModOutp_a30=renderUI({
     req(BH_run())
     req(BH_rfpoints())
      EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
     last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
     
     
     HTML(paste0(tags$b("B/B0 ("), tags$b(EndYear), tags$b(")")," = " , format(smooth_ts()$BB0.ts[last],digits=2),
                 " (",
                 format(smooth_ts()$BB0.lcl.ts[last],digits=2),
                 "-", format(smooth_ts()$BB0.ucl.ts[last],digits=2),") "
                      ))})  
   
   
   # output$ModOutp_a31=renderUI({
   #   req(BH_run())
   #   req(BH_rfpoints())
   #   
   #   jagsFit=BH_run()[["jagsFit"]]
   #   bestfityr = which(jagsFit == min(jagsFit))
   #   
   #   EndYear= max(BH_rfpoints()[["model.outcomes"]]$Year)
   #   last            <- which(BH_rfpoints()[["model.outcomes"]]$Year==EndYear)
   #   Years=BH_rfpoints()[["model.outcomes"]]$Year
   #   
   #   HTML(paste0(tags$b("B/B0, best LF fit year ("), tags$b(Years[bestfityr]), tags$b(")")," = " , format(smooth_ts()$BB0.ts[bestfityr],digits=2),
   #               " (",
   #               format(smooth_ts()$BB0.lcl.ts[bestfityr],digits=2),
   #               "-", format(smooth_ts()$BB0.ucl.ts[bestfityr],digits=2),") "
   #   ))})  
   
   
  #  
  # bestfityr = which(jagsFit == min(jagsFit))
  # cat("B/B0      = ",format(BB0.ts[last],digits=2)," (",format(BB0.lcl.ts[last],digits=2),"-",
  #     format(BB0.ucl.ts[last],digits=2),"),",
  #     " best LF fit year ",Years[bestfityr],"=",format(BB0.ts[bestfityr],nsmall=2),
  #     " (",format(BB0.lcl.ts[bestfityr],digits=2),"-",format(BB0.ucl.ts[bestfityr],digits=2),")",sep="","\n")

  # # print B/B0 for selected year
  # if(is.na(Year.sel)==F) {
  #   BB0.sl     <- BB0.ts[Years==Year.sel]
  #   BB0.lcl.sl <- BB0.lcl.ts[Years==Year.sel]
  #   BB0.ucl.sl <- BB0.ucl.ts[Years==Year.sel]
  # }
  # cat("B/Bmsy    = ",format(BB0.ts[last]/BFM1B0,digits=2)," (",format(BB0.lcl.ts[last]/BFM1B0,digits=2),"-",
  #     format(BB0.ucl.ts[last]/BFM1B0,digits=2),")",
  #     ifelse(is.na(Year.sel)==F,
  #            bold(paste(", selected B/B0 ",Year.sel," = ",format(BB0.sl,digits=2)," (",format(BB0.lcl.sl,digits=2),"-",
  #                       format(BB0.ucl.sl,digits=2),")",sep="")),""),sep="","\n")
  # if(dat.ID$Comment != "" && is.na(dat.ID$Comment)==F) cat(dat.ID$Comment,"\n")
  # 
  # # point out questionable or impossible results
  # # negative rates
  # if(Ldat$MK[last] < 0 | Ldat$FK[i] < 0) cat("Data unsuitable for LF analysis, negative mortality rates are impossible\n")
  # # Biomass larger than unexploited
  # if(Ldat$BB0[last] >1.1) cat(red("Data unsuitable for LF analysis, biomass exceeds carrying capacity"),"\n")

  
  # output$test_orf=renderTable({as.data.frame(BH_run()[["jagsFit"]])})
  # output$test_orf2=renderTable({LBB_object()[[2]]})
  # output$test_orf3=renderTable({BH_rfpoints()})
  # output$test_orf4=renderTable({LBB_object()[[4]]})
  # 
  
  
   observeEvent(input$BH_run_button, {
     req( BH_rfpoints())
     req( smooth_ts())
     req( pic_LBB_2())
   
     req(fstc())
     req( run_pictures$pic_A)
     LBB_object <- BH_rfpoints()
     if (input$Id049=="A") {
       nm=object_NAME()} else if (input$Id049=="B") {
         nm=input$Id081}
     
     dir.create(paste0(dir.name(),"/LBB/outputs"))
     dir.create(paste0(dir.name(),"/LBB/outputs/",nm))
     device_="tiff"

     for (i in 1:length(pic_LBB_2()[[2]])) {
       ggsave(filename=paste0(paste0(dir.name(),"/LBB/outputs/",nm,"/","lfds_",i,"."),device_),plot=pic_LBB_2()[[2]][[i]], device =device_, width = 16, height =10, units = "cm",  dpi = 300)
     }
     
     
     ggsave(filename=paste0(paste0(dir.name(),"/LBB/outputs/",nm,"/","AG_LFD."),device_),plot=run_pictures$pic_A, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
     ggsave(filename=paste0(paste0(dir.name(),"/LBB/outputs/",nm,"/","LC_ts."),device_),plot=run_pictures$pic_B, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
     ggsave(filename=paste0(paste0(dir.name(),"/LBB/outputs/",nm,"/","FM_ts."),device_),plot=run_pictures$pic_C, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
     ggsave(filename=paste0(paste0(dir.name(),"/LBB/outputs/",nm,"/","BB0_ts."),device_),plot=run_pictures$pic_D, device =device_, width = 16, height =10, units = "cm",  dpi = 300)

     
     # ggsave(filename=paste0(paste0(dir.name(),"/LBB/outputs/",nm,"/","Equilibrium_curve."),device_),plot=run_pictures$pic_E, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
     # ggsave(filename=paste0(paste0(dir.name(),"/LBB/outputs/",nm,"/","Kobe_plot."),device_),plot=run_pictures$pic_F, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
     # ggsave(filename=paste0(paste0(dir.name(),"/LBB/outputs/",nm,"/","Management_graphs."),device_),plot=run_pictures$pic_G, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
     # ggsave(filename=paste0(paste0(dir.name(),"/LBB/outputs/",nm,"/","prior_posterior_distributions."),device_),plot=run_pictures$pic_H, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
     save(LBB_object,  file =paste0(dir.name(),"/LBB/outputs/",nm,"/","LBB_obj_",gsub(" ", "_",fstc()[["model.param"]][["Stock"]]), "_",Sys.Date(), ".RData"))
     #write.csv(cbind(fstc()[["input"]][["Stock_info"]],fstc()[["input"]][["Input_parameters"]]), paste0(dir.name(),"/LBB/outputs/",nm,"/input_parameters.csv"), row.names = F)
     write.csv(smooth_ts(), paste0(dir.name(),"/LBB/outputs/",nm,"/LBB_outputs.csv"), row.names = F)
     write.csv(LBB_object[["median.ref.points"]], paste0(dir.name(),"/LBB/outputs/",nm,"/output_ref.points.csv"), row.names = F)
     
     Save_done <- showNotification(paste("Message: ", "All the outcomes are saved in your working directory"), duration = 5)
     
   })
   
   
   observe({
     req(BH_rfpoints())
     if (input$Id049=="A") {
       nm=object_NAME()} else if (input$Id049=="B") {
         nm=input$Id081}
     output$working_directory=renderUI({ shiny::h4(tagList("All your outcomes are stored in the directory: ",  tags$b(paste0(dir.name(),"/LBB/outputs/",nm))))})
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



shinyApp(shinyUI, shinyServer)#, ...