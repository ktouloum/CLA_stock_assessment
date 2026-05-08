


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path=getwd()
#################################### Packages


library(ggplot2)
library(R2jags)
library(dplyr)
library(data.table)
library(tidyr)

AMSY_DATA=readRDS("AMSY_data.rds")

AMSY_DATA=AMSY_DATA[["anb-78ab"]]$DATA

AMSY_DATA=AMSY_DATA[,c("yr","bt")]
write.csv(AMSY_DATA,"AMSY_DATA_check_test.csv",row.names = F)

source("AMSY_aux_functions.R")




CPUE_obj=Bio_obj(AMSY_DATA,CPUE_CV=0.3) 
CPUE_obj

fishbas=fbsb("Lophius budegassa")
fishbas


priorkq=prior_kq(CPUE_obj[["bt_data"]][,c("yr","bt_smthd")],2000,0.3,0.5)
  
mvnlogrk=mvnlog.rk(0.28,0.64,priorkq,n.p=50000,n.trial=30)
  

AMSY.object=  AMSY_stkobj_maker("stock_1","Lophius budegassa",
                                "Angler fish",
                                CPUE_obj,mvnlogrk,"Low",c(0.28,0.64),2000,
                                0.3,0.5,priorkq,fbslb=fishbas)
                               
amsyrun=AMSY.run(AMSY.object)



  prds=AMSY.products(AMSY.object,amsyrun)


  ggplot.mvr(prds,amsyrun) 

  ggplot.catch(prds)

ggplot.FFmsy(prds)
  
ggplot.BBmsy(prds)

ggplot.kobe(prds)

retr=AMSY_retro(prds,3)
ggplot.retro(retr)

Retro.object=retr
