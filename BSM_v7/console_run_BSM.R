


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path=getwd()
#################################### Packages


library(ggplot2)
library(R2jags)
library(dplyr)
library(data.table)
library(tidyr)


source("CLA_functions2.R")
stckobl=test_CATCH_data[test_CATCH_data$Stock=="Atka_mackerel_Bering_Sea_Aleutian_Islands", ]


colnames(stckobl)



#### 1st step: we load a data.frame with at least three columns: "yr"=Year, "ct"= Catch, "bt"=CPUE/Biomass index. 
#### Use exactly these column names
#### Note for later: add Bt_CV
inp2=read.csv("C:/Users/toulo/Desktop/Dab.csv",header = T, stringsAsFactors = F)
### if you don't have something available right now, run the following lines to use something as an example
inp=readRDS("example.DATA.rds")
# head(inp)
# inp2=inp2[,c(7,30,23)]
# colnames(inp2)=colnames(inp)
# inp=inp2
#### 2nd step: create a "Catch object". You can select start year, end year, 
#### if you want to smooth the data series (smoother_bw=1 or means no smooth which is the default. integer value
#### above 1 smooths the curve accordingly). Catch_CV (default is 0.15, you can change), Plot=T means you see the 
#### coressponding plot, Plot=F means you dont see it, and ShowCV=T means that cv will be shown in the plot

Cobj=Catch_obj(inp ) 
Cobj=Catch_obj(inp,smoother_bw=3 ) 
Cobj=Catch_obj(inp,Catch_CV=0.25 ) 
Cobj
Cobj=Catch_obj(inp ) 

#### 3rd step: create a "Biomass object". You can select start year, end year, 
#### if you want to smooth the data series (smoother_bw=1 or means no smooth which is the default. integer value
#### above 1 smooths the curve accordingly). CPUE_CV (default is 0.2, you can change),
#### ecreep=T, means that you add technological creep to your CPUE index,
#### ecreep_yr is the year where the ecreep start to affect the time series,
#### ecreepvalue is the % value of ecreep per year (default value is 2 which means 2%)
#### Biom_type takes value either "CPUE" or "Biomass", Plot=T means you see the 
#### coressponding plot, Plot=F means you dont see it, and ShowCV=T means that cv will be shown in the plot



Bobj=Bio_obj(inp,smoother_bw=3,ecreep=T,ecreep_yr=1990,ecreepvalue=10,Biom_type=c("CPUE","Biomass")[1],CPUE_CV=0.2,Plot=T,ShowCV=F) 



Bobj=Bio_obj(inp) 
Bobj



#### 4th step: Deal with r priors. you can connect with fishbase or Sealifebase to get info for your species
#### with fbsb function. Please write the scientific name of the species correctly


fishbas=fbsb("Panulirus argus")
fishbas

#### after that, we select the r.priors, either as a text (select one of "Very low","Low","Medium","High") 
#### or as a range (for example c(0.45, 1). The first part of the CMSY algorithm runs to estimate viable k prior range
#### that correspond to the r priors you set. Check the corresponding plot for more information.
#rk_priors_2(Catch.obj,r.priors,  n=5000,Plot=T)
  
rk_priors=rk.priors(Cobj,c(0.3,0.8))
#rk_priors=rk.priors(Cobj,"Medium")



#### 5th step: Deal with bk priors. nbk is the number of b/k priors to use, 1= only start, 2=start and intermediate,
#### 3= start, intermediate and end. 
#### for start, intermediate and end priors, put either one of the stock_states, or a range of values (for example: c(0.2,0,8))
#### int_bio_year is the year for intermediate b/k prior

Stock_states=c("Very low","Low","Medium","Sustainable","Unexploited")

Bpriors=Biom_priors(Cobj,nbk=3,start_bio=c(0.85,1.05),
            int_bio=c(0.01,0.1), int_bio_year=2000,end_bio=c(0.01,0.1), Plot=T)
  

###### 6th step: consolidate the stock object, give a stock name, the species scientific name, and add catch, biomass, rk and bk objects.
###### the fishbase/sealifebase object is optional. 

CLA_stock=CLA_stock_maker(Stock="Panu_arg_Bahamas_R2",
                Species="Clupea harengus",
                Catch_object=Cobj,
                Biomass_object=Bobj,
                rk_object=rk_priors,
                bk_object=Bpriors) 
  


###### 7th step: Run the fit model
fit.obj=CLA.fit(CLA_stock)


###### 8th step: inform the stock object with the fit outcomes

CLA_stock_2=extract.CLA.fit(fit.obj,CLA_stock)

###### 9th step: See summary outcomes

xx=CLA.summary(CLA_stock_2)

###### 10th step: See and save if you want all the plots

ggrk.plot(CLA_stock_2,fit.obj,"BSM") 

###if you want to save a pic:
#ggsave(filename="rk.tiff", width = 16, height =10, units = "cm",  dpi = 300)

ggcatch.plot(CLA_stock_2,"BSM",Management=F)
  
ggbk.plot(CLA_stock_2,"BSM",Management=F)

ggFFmsy.plot(CLA_stock_2,"BSM",Management=F)
ggparabola.plot(CLA_stock_2,"BSM")

ggkobe.plot(CLA_stock_2,fit.obj,"BSM",Management=F)
  
ggmanagement.plot(CLA_stock_2,fit.obj,"BSM")
ggprorposterior.plot(CLA_stock_2,fit.obj,"BSM")

ggpdiagnostics.plot(CLA_stock_2,fit.obj,"BSM")

###### 11th step: Run retrospective. ret_steps is the number of years to go back

retr=retro.fit(CLA_stock_2,ret_steps=3,"BSM")

###### See retrospective outcomes
ggretro.plot(retr)

ggkiel.plot(CLA_stock_2,METHOD="BSM")


###### 12th step: run Projection to the future and see outcomes (latewr add mohns rho)

cla.proj=CLA.forward(fit.obj,CLA_stock_2,nyears=5,status.quo_years=1,interim.quant = c("Catch", "F")[2],quant = c("Catch", "F")[2],Manual_Scenarios=NULL)
xx=interim.summary(cla.proj)

ggforecast.FFMSY(cla.proj,CI=T)

ggforecast.bk(CLA_stock_2,cla.proj)

ggforecast.catch(cla.proj)

xx2=advice.summary(cla.proj)

gg_summary.plot(CLA_stock_2,fit.obj,METHOD="BSM")


