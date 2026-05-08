

#----------------------------------------------
#  FUNCTIONS ----
#----------------------------------------------
# 
#----------------------------------------------


AMSY_stkobj_maker=function(Stock,Species,Common_name=NA,Cpue_obj,mvnlogrk,Resilience,rpriors,bk.yr,bk.low,bk.high,priorkq,fbslb=NULL,Comments=NA) {
  
  inp=Cpue_obj$bt_data
  
  inp=inp[,c( "yr","bt","bt_iterp",
              "bt_smthd","ecreep")]#,"Stock_objID"
  
  inp$bt_cv=Cpue_obj[["bt_param"]]$CPUE_CV
  
  inp$Stock_objID=NA
  stkID_clnms=c("Stock","Species","Common_name","Comments","Start_yr","End_yr","Ecreep_YN",
                "Start_yr_ecreep","Ecreep","Smooth_YN","Smooth_bw","Resilience","r.low","r.hi",
                "FBSLB_info_r","FBSLB_info_Resilience","FBSLB_page","bk_prior_yr","bk_prior_low",
                "bk_prior_high","prior.kq.low","prior.kq.hi")
  
  ID=data.frame(matrix(nrow = 1, ncol = length(stkID_clnms)))
  colnames(ID)=stkID_clnms
  ID$Stock=Stock
  ID$Species=Species
  ID$Common_name=Common_name
  ID$Comments=Comments
  ID$Start_yr=Cpue_obj[["bt_param"]]$bio_start_yr 
  ID$End_yr=Cpue_obj[["bt_param"]]$bio_end_yr  
  ID$Ecreep_YN=Cpue_obj[["bt_param"]]$ecreep
  ID$Start_yr_ecreep=Cpue_obj[["bt_param"]]$ecreep_yr
  ID$Ecreep=Cpue_obj[["bt_param"]]$ecreepvalue
  ID$Smooth_YN=ifelse(Cpue_obj[["bt_param"]]$ecreepvalue==1,F,T)
  ID$Smooth_bw=Cpue_obj[["bt_param"]]$smoother_bw 
  ID$Resilience=Resilience
  ID$r.low=rpriors[1]
  ID$r.hi=rpriors[2]
  ID$FBSLB_info_r=ifelse(is.null(fbslb),NA,fbslb[1])
  ID$FBSLB_info_Resilience=ifelse(is.null(fbslb),NA,fbslb[2])
  ID$FBSLB_page=ifelse(is.null(fbslb),NA,fbslb[6])
  ID$bk_prior_yr=bk.yr
  ID$bk_prior_low=bk.low
  ID$bk_prior_high=bk.high
  ID$prior.kq.low=priorkq[1]
  ID$prior.kq.hi=priorkq[2]
  posteriors_clmns=  c("rv.est","kqv.est","BBmsy.end","FFmsy.end","MSYq.est","n.v")
  
  output_posteriors_ = data.frame(matrix(nrow =3, ncol = length(posteriors_clmns))) #nrow(fstck_Catch_Data)
  colnames(output_posteriors_)=posteriors_clmns
  
  row.names(output_posteriors_)=c("median","95% CIlow","95% CI.high")
  
  
  cqt_clmns=c("cqt.median","cqt.lcl","cqt.ucl")
  cqt_output_timeseries_ = data.frame(matrix(nrow =length(inp$yr)-1, ncol = length(cqt_clmns))) #nrow(fstck_Catch_Data)
  
  colnames(cqt_output_timeseries_)=cqt_clmns
  
  cpuet_clmns=c("cpuet.median", "cpuet.lcl", "cpuet.ucl")
  cpuet_output_timeseries_ = data.frame(matrix(nrow =length(inp$yr), ncol = length(cpuet_clmns))) #nrow(fstck_Catch_Data)
  colnames(cpuet_output_timeseries_)=cpuet_clmns
  
  FFmsy_clmns=c("Ft","Ft.lcl","Ft.ucl","FFmsy","FFmsy.lcl","FFmsy.ucl")
  FFmsy_output_timeseries_ = data.frame(matrix(nrow =length(inp$yr)-1, ncol = length(FFmsy_clmns))) #nrow(fstck_Catch_Data)
  colnames(FFmsy_output_timeseries_)=FFmsy_clmns
  
  AMSY_object=list(DATA=inp,
                   ID=ID,
                   mvnlogrk=mvnlogrk,
                   Outputs=list(
                     outp_1=output_posteriors_,
                     cqt_=cqt_output_timeseries_ ,
                     cpuet_= cpuet_output_timeseries_,
                     FFmsy_= FFmsy_output_timeseries_,
                     cpuet.sel= NA,  
                     cqt.sel=NA))
  
  return(AMSY_object)
}  

mvnlog.rk=function(r.low,r.high,prior.kq,n.p=50000,n.trial=30) {
  prior.r=c(r.low,r.high)
  mean.log.r=mean(log(prior.r))
  sd.log.r=(log(prior.r[2])-log(prior.r[1]))/4  # assume range covers 4 SD
  
  prior.kq=prior.kq[c(1,2)]
  mean.log.kq <- mean(log(prior.kq))
  sd.log.kq   <- (log(prior.kq[2])-log(prior.kq[1]))/4 # assume range covers 4 SD
  mvn.log.rk <- mvn(n=n.p,mean.log.r=mean.log.r,sd.log.r=sd.log.r,mean.log.kq=mean.log.kq,sd.log.kq=sd.log.kq)
  return(mvn.log.rk)
}

prior_kq=function(CPUE_df,Bk.yr,Bk_low,Bk_high) {
  colnames(CPUE_df)=c("yr","cpue")
  Bk.yr.i=Bk.yr
  max.cpue     <- sort(CPUE_df$cpue[!is.na(CPUE_df$cpue)])[length(CPUE_df$cpue[!is.na(CPUE_df$cpue)])-1]
  
  prior.Bk=as.numeric(c(Bk_low,Bk_high))
  # us relative range of B/k as relative range for kq
  mean.prior.Bk   <- mean(prior.Bk)
  rr.prior.Bk     <- (mean.prior.Bk-prior.Bk[1])/mean.prior.Bk
  
  prior.kq.low.1  <- (1-rr.prior.Bk)*CPUE_df$cpue[CPUE_df$yr==Bk.yr.i]/mean.prior.Bk
  prior.kq.hi.1   <- (1+rr.prior.Bk)*CPUE_df$cpue[CPUE_df$yr==Bk.yr.i]/mean.prior.Bk
  
  # mean.log.r=mean(log(prior.r))
  # sd.log.r=(log(prior.r[2])-log(prior.r[1]))/4  # assume range covers 4 SD
  
  # kq must be > max cpue unless near unexploited
  prior.kq.low.2  <- ifelse(prior.kq.low.1 < max.cpue,ifelse(mean.prior.Bk >= 0.85,0.9*max.cpue,max.cpue),prior.kq.low.1)
  # increase lower kq prior if cpue is small and flat
  
  x <- na.omit(CPUE_df$cpue)
  if ((max(x) / min(x)) < 2) {
    # if((max(CPUE_df$cpue[!is.na(CPUE_df$cpue)])/min(CPUE_df$cpue[!is.na(CPUE_df$cpue)]))<2) {
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
  
  return(prior.kq_)
}



AMSY_stock_maker_empty_shiny=function(inpt,Stock,yrcol,btcol,cvcol=NA,do_u_have_cv="No") {
  
  data_clnms=c("yr","bt","bt_iterp","bt_smthd","ecreep","bt_cv","Stock_objID")
  inp = data.frame(matrix(nrow = nrow(inpt), ncol = length(data_clnms)))
  colnames(inp)=data_clnms
  
  inp$Stock=Stock
  inp$yr=as.integer(inpt[,yrcol])
  inp$bt= as.numeric(inpt[,btcol])
  if (do_u_have_cv=="Yes"){
    inp$bt_cv=  as.numeric(inpt[,cvcol]) } 
  
  stkID_clnms=c("Stock","Species","Common_name","Comments","Start_yr","End_yr","Ecreep_YN",
                "Start_yr_ecreep","Ecreep","Smooth_YN","Smooth_bw","Resilience","r.low","r.hi",
                "FBSLB_info_r","FBSLB_info_Resilience","FBSLB_page","bk_prior_yr","bk_prior_low",
                "bk_prior_high","prior.kq.low","prior.kq.hi")
  
  ID = data.frame(matrix(nrow = 1, ncol = length(stkID_clnms)))
  colnames(ID)=stkID_clnms
  ID$Stock=Stock
  
  
  posteriors_clmns=  c("rv.est","kqv.est","BBmsy.end","FFmsy.end","MSYq.est","n.v")
  
  output_posteriors_ = data.frame(matrix(nrow =3, ncol = length(posteriors_clmns))) #nrow(fstck_Catch_Data)
  colnames(output_posteriors_)=posteriors_clmns
  
  row.names(output_posteriors_)=c("median","95% CIlow","95% CI.high")
  
  
  cqt_clmns=c("cqt.median","cqt.lcl","cqt.ucl")
  cqt_output_timeseries_ = data.frame(matrix(nrow =length(inp$yr)-1, ncol = length(cqt_clmns))) #nrow(fstck_Catch_Data)
  
  colnames(cqt_output_timeseries_)=cqt_clmns
  
  cpuet_clmns=c("cpuet.median", "cpuet.lcl", "cpuet.ucl")
  cpuet_output_timeseries_ = data.frame(matrix(nrow =length(inp$yr), ncol = length(cpuet_clmns))) #nrow(fstck_Catch_Data)
  colnames(cpuet_output_timeseries_)=cpuet_clmns
  
  FFmsy_clmns=c("Ft","Ft.lcl","Ft.ucl","FFmsy","FFmsy.lcl","FFmsy.ucl")
  FFmsy_output_timeseries_ = data.frame(matrix(nrow =length(inp$yr)-1, ncol = length(FFmsy_clmns))) #nrow(fstck_Catch_Data)
  colnames(FFmsy_output_timeseries_)=FFmsy_clmns
  
  AMSY_object=list(DATA=inp,
                   ID=ID,
                   mvnlogrk=NA,
                   Outputs=list(
                     outp_1=output_posteriors_,
                     cqt_=cqt_output_timeseries_ ,
                     cpuet_= cpuet_output_timeseries_,
                     FFmsy_= FFmsy_output_timeseries_,
                     cpuet.sel= NA,  
                     cqt.sel=NA))
  return(AMSY_object)
}

Bio_obj=function(inp,start_yr=NA, end_yr=NA,smoother_bw=1,ecreep=F,ecreep_yr=NA,ecreepvalue=2,Biom_type=c("CPUE","Biomass")[1],CPUE_CV=0.3,Plot=T,ShowCV=T) {
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
  if (!all(is.na(temp_bio_obj$ct))){
    temp_bio_obj=temp_bio_obj[!is.na(temp_bio_obj$ct),]}
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
  sigma.cpue=ifelse(is.na(mean(AMSY.obj[["DATA"]]$bt_cv)),3, mean(AMSY.obj[["DATA"]]$bt_cv))
  cdat_=AMSY.obj[["DATA"]]
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
  max.cpue     <- sort(cdat_$bt_smthd)[length(cdat_$bt_smthd)-1]
  prior.Bk=as.numeric(c(AMSY.obj[["ID"]]$bk_prior_low,  AMSY.obj[["ID"]]$bk_prior_high))
  MCA1 <-  SchaeferCPUE(yr=cdat_$yr,
                        cpue=cdat_$bt_smthd,
                        cpue.raw = cdat_$bt,
                        ri=ri1, kqi=kqi1, sigR=sigma.r[res.i],nyr=nyr,max.cpue=max.cpue,res=res,Bk.yr.i=Bk.yr.i,prior.Bk=prior.Bk,sigma.cpue=sigma.cpue, filter=TRUE) #><> correct PE input
  MCA_pre=check_SchaeferCPUE(MCA1,cdat_$bt_smthd,
                             cdat_$bt,cdat_$yr,sigma.r[res.i],AMSY.obj[["mvnlogrk"]],
                             mean.log.r=mean.log.r,sd.log.r=sd.log.r,mean.log.kq=mean.log.kq,sd.log.kq=sd.log.kq,res=res,Bk.yr.i=Bk.yr.i,prior.Bk=prior.Bk,sigma.cpue=sigma.cpue)
  MCA=MCA_pre[[1]]
  #  MCA=as.data.frame(MCA)
  return(MCA)
}

AMSY.products=function(AMSY.obj,AMSY_fit.obj) {
  
  MCA=AMSY_fit.obj
  cdat_=AMSY.obj[["DATA"]]
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

SchaeferCPUE<-function(yr, cpue,cpue.raw, ri, kqi, sigR,nyr,max.cpue,res,Bk.yr.i,prior.Bk,sigma.cpue, filter){
  n.trial      <- 30 # times each year is calculated with new random error terms for r and cpue
#sigma.cpue   <- 0.3 # observation error for cpue
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

check_SchaeferCPUE=function(MCA1,cpue,cpue.raw,yr,sigR,mvn.log.rk,mean.log.r,sd.log.r,mean.log.kq,sd.log.kq,res,Bk.yr.i,prior.Bk,sigma.cpue) {
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
    MCA2 <-  SchaeferCPUE(yr=yr, cpue=cpue,cpue.raw, ri=ri2, kqi=kqi2, sigR=sigR_,nyr=nyr,max.cpue=max.cpue,res=res,Bk.yr.i=Bk.yr.i,prior.Bk=prior.Bk,sigma.cpue = sigma.cpue, filter=TRUE)
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
  
  cdat_=AMSY.prod[["DATA"]]
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
  cdat_=AMSY.prod[["DATA"]]
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
  cdat_=AMSY.prod[["DATA"]]
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
  cdat_=AMSY.prod[["DATA"]]
  
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
  nyr=length(AMSY.object[["DATA"]]$yr)
  res=AMSY.object[["ID"]]$Resilience
  sigma.cpue=ifelse(is.na(mean(AMSY.object[["DATA"]]$bt_cv)),3, mean(AMSY.object[["DATA"]]$bt_cv))
  
  for (i in 1:retro_range) {
    inp=AMSY.object[["DATA"]][AMSY.object[["DATA"]]$yr<=AMSY.object[["DATA"]]$yr[nyr]-i,  ]
    start_yr=AMSY.object[["ID"]]$Start_yr
    end_yr=AMSY.object[["ID"]]$End_yr
    smoother_bw=AMSY.object[["ID"]]$Smooth_bw
    ecreep=AMSY.object[["ID"]]$Ecreep_YN
    ecreep_yr=AMSY.object[["ID"]]$Start_yr_ecreep
    ecreepvalue=AMSY.object[["ID"]]$Ecreep
    CPUE_CV=mean(AMSY.object[["DATA"]]$bt_cv,na.rm = T)
    
    cdat_retr[[i]]= Bio_obj(inp,start_yr,end_yr,smoother_bw,ecreep=F,ecreep_yr,ecreepvalue,Biom_type=c("CPUE","Biomass")[1],CPUE_CV,Plot=F,ShowCV=F)
  }
  nyr_retros=list()
  for (i in 1:retro_range) {  
    nyr_retros[[i]]<- length(cdat_retr[[i]][["bt_data"]]$yr[!is.na(cdat_retr[[i]][["bt_data"]]$bt)])
  }
  priorkq_retro=list()
  max.cpue_retros=list()
  min.cpue_retros=list()
  for (i in 1:retro_range) {
    max.cpue_retros[[i]]<- sort(cdat_retr[[i]][["bt_data"]]$bt_smthd[!is.na(cdat_retr[[i]][["bt_data"]]$bt_smthd)])[length(cdat_retr[[i]][["bt_data"]]$bt_smthd[!is.na(cdat_retr[[i]][["bt_data"]]$bt_smthd)])-1]
    min.cpue_retros[[i]]<- min(cdat_retr[[i]][["bt_data"]]$bt_smthd[!is.na(cdat_retr[[i]][["bt_data"]]$bt_smthd)],na.rm = T)
  }
  minmax.cpue_retros=list(max.cpue_retros=max.cpue_retros,min.cpue_retros=min.cpue_retros)
  prior.r=c(AMSY.object[["ID"]]$r.low,AMSY.object[["ID"]]$r.hi)
  Bk.yr.i=AMSY.object[["ID"]]$bk_prior_yr
  prior.Bk=as.numeric( c(AMSY.object[["ID"]]$bk_prior_low,AMSY.object[["ID"]]$bk_prior_high))
  
  for (i in 1:retro_range) {
    CPUE_df= cdat_retr[[i]][["bt_data"]]
    CPUE_df=CPUE_df[,c("yr", "bt_smthd")]
    Bk.yr=AMSY.object[["ID"]]$bk_prior_yr
    Bk_low=AMSY.object[["ID"]]$bk_prior_low
    Bk_high=AMSY.object[["ID"]]$bk_prior_high
    priorkq_retro[[i]]= prior_kq(CPUE_df,Bk.yr,Bk_low,Bk_high)
  }
  
  mvn.log.rk=list()
  for (i in 1:retro_range) {
    prior.kq=priorkq_retro[[i]][c(1,2)]
    r.low=AMSY.object[["ID"]]$r.low 
    r.high=AMSY.object[["ID"]]$r.hi
    n.p          <- 50000 # number of r-kq pairs to be analysed; will be doubled if too few viable pairs are found
    n.trial      <- 30 # times each year is calculated with new random error terms for r and cpue
    mvn.log.rk[[i]]= mvnlog.rk(r.low,r.high, priorkq_retro[[i]],n.p=n.p,n.trial=n.trial) 
  }
  retro.obj=list()
  retro.obj_fit=list()
  retro.products=list()
  FFmsy.retrospective=list() #retrospective analysis
  BBmsy.retrospective=list() #retrospective analysis
  years.retrospective=list()#retrospective analysis
  
  
  for (i in 1:retro_range) {
    retro.obj[[i]]=  AMSY_stkobj_maker(AMSY.object[["ID"]]$Stock,AMSY.object[["ID"]]$Species,
                                       AMSY.object[["ID"]]$Common_name,
                                       cdat_retr[[i]],mvn.log.rk[[i]],AMSY.object[["ID"]]$Resilience,prior.r,Bk.yr.i,
                                       prior.Bk[1],prior.Bk[2],priorkq_retro[[i]],fbslb=NULL,
                                       AMSY.object[["ID"]]$Comments)
    
    retro.obj_fit[[i]]=AMSY.run( retro.obj[[i]])
    retro.products[[i]]=AMSY.products(retro.obj[[i]],retro.obj_fit[[i]])
  } 
  FFmsy.retrospective=list()
  BBmsy.retrospective=list()
  
  for (i in 1:retro_range) {
    xx=retro.products[[i]][["Outputs"]][["FFmsy_"]]
    xx$yr=as.integer(row.names(xx))
    xx$ID=i
    
    cpuet_=retro.products[[i]][["Outputs"]][["cpuet_"]]
    outp=retro.products[[i]][["Outputs"]][["outp_1"]]
    Bkt        <-data.frame(yr=as.integer(row.names(retro.products[[i]][["Outputs"]][["cpuet_"]])),Bkt=cpuet_$cpuet.median/(0.5*outp$kqv.est),ID=i)
    FFmsy.retrospective[[i]]=xx
    BBmsy.retrospective[[i]]=Bkt
    
  }
  FFmsy.retrospective=data.table::rbindlist(FFmsy.retrospective)
  BBmsy.retrospective=data.table::rbindlist(BBmsy.retrospective)
  retro_outcomes=list(FFmsy.retrospective=FFmsy.retrospective,BBmsy.retrospective=BBmsy.retrospective)
  AMSY.object[["Retro"]]=retro_outcomes
  AMSY.object[["ID"]]$Retro_range=retro_range
  return(AMSY.object)
}


ggplot.retro=function(Retro.object) {
  cdat_=Retro.object[["DATA"]]
  ffmsy_retro=Retro.object[["Retro"]][["FFmsy.retrospective"]]
  yr_=cdat_$yr
  nyr=length(yr_)
  FFmsy_= Retro.object[["Outputs"]][["FFmsy_"]]
  max.y_ffmsy <- max(c(1.2,FFmsy_$FFmsy.ucl),na.rm=T)
  cpuet_=Retro.object[["Outputs"]][["cpuet_"]]
  outp=Retro.object[["Outputs"]][["outp_1"]]
  
  bbmsy_retro=Retro.object[["Retro"]][["BBmsy.retrospective"]]
  Bkt<- cpuet_$cpuet.median/(0.5*outp$kqv.est)
  
  def_year=data.frame(yr=yr_ ,Bkt=Bkt,ID=rep(0,length(yr_)))
  bbmsy_retro=rbind(def_year,bbmsy_retro)
  max.y_bbmsy <- max(c(1.2,1.1*Bkt, 1.1*bbmsy_retro$Bkt ),na.rm=T)
  
  
  my_y_title1 <-bquote(atop("F/Fmsy Retrospective"~"for"~the~stock~bold(.(Retro.object[["ID"]]$Stock))~of~italic(.(Retro.object[["ID"]]$Species))))
  my_y_title2 <-bquote(atop("B/Bmsy Retrospective"~"for"~the~stock~bold(.(Retro.object[["ID"]]$Stock))~of~italic(.(Retro.object[["ID"]]$Species))))
  
  
  def_year_fmsy=data.frame(yr=yr_[1:(nyr-1)] ,FFmsy=FFmsy_$FFmsy,ID=rep(0,length(yr_[1:(nyr-1)])))
  ffmsy_retro=rbind(def_year_fmsy,ffmsy_retro[,c("yr","FFmsy","ID")])
  
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
  return(pic_all)
}

ggbk_priors=function(Stock, Species,Cpue_obj,prior.kq,bk.year,bk.low,bk.high) {
  nyr=length(Cpue_obj[["bt_data"]]$yr)
  my_y_title <-bquote(atop(CPUE~and~expert~priors~"for"~the~stock~bold(.(Stock))~of~italic(.(Species))))
  start_yr =min(as.integer(Cpue_obj[["bt_data"]]$yr),na.rm = T)
  end_yr    =max(as.integer(Cpue_obj[["bt_data"]]$yr),na.rm = T)
  mean.bk=mean(c(bk.low,bk.high)) ###### start_f
  
  div_f=  Cpue_obj[["bt_data"]]$bt_smthd[Cpue_obj[["bt_data"]]$yr==bk.year]/mean.bk
  scaled_bkpriors=div_f*c(bk.low,bk.high)
  p_1=ggplot2::ggplot()+
    ggplot2::geom_errorbar(aes(x=bk.year, ymin=scaled_bkpriors[1], ymax=scaled_bkpriors[2],color="C"), size=2)+
    ggplot2::geom_line(data=Cpue_obj[["bt_data"]],ggplot2::aes(x=yr,y=bt_smthd,color="A"),size=1)+
    ggplot2::geom_point(data=Cpue_obj[["bt_data"]],ggplot2::aes(x=yr,y=bt_smthd),fill="#F8766D",shape=21,size=2)+
    ggplot2::theme_classic()+
    ggplot2::scale_color_manual(values=c("#F8766D","blue"),labels=c("CPUE","Selected B/k prior"))+
    # ggplot2::scale_fill_manual(values=c("#F8766D"),labels=c("CPUE","k prior range for selected year","Bmsy prior range"))+
    ggplot2::scale_y_continuous(limits = c(0,NA))+
    ggplot2::labs(y="CPUE",x="Year",fill="",color="",title =my_y_title)+
    ggplot2::theme(legend.position="bottom")
  
  return(p_1)
}

#---------------------------------------------
# END OF FUNCTIONS
#---------------------------------------------
