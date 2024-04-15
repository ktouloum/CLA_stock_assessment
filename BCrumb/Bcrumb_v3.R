# Instructions

#1. downoad and install Rtools from: https://cran.r-project.org/bin/windows/Rtools/

#2. downoad and install JAGS from:https://sourceforge.net/projects/mcmc-jags/

#3. downoad and install necessary packages, by running the following lines, after removing the hashtag (#)
#list.of.packages <- c("shinyWidgets", "DT","shiny","shinydashboard","R2jags","coda","gplots","rjags","reshape2","shinycssloaders")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#4. Press the  "Run App" button to start the app

library(shinyWidgets)
library(DT)
library(shiny)
library(shinydashboard)
library(R2jags)  # Interface with JAGS
library(coda) 
library(gplots)
library(rjags)
library(reshape2)
library(shinycssloaders)

#################################### Working directory
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# path=getwd()
linebreaks <- function(n){HTML(strrep(br(), n))}
'%!in%'=Negate('%in%')

shinyUI <- shinydashboard::dashboardPage(skin = "yellow",
                                         shinydashboard::dashboardHeader(title = "Bcrumb"),
                                         shinydashboard::dashboardSidebar(
                                           shinydashboard::sidebarMenu(
                                             shinydashboard::menuItem("1. Data input", tabName = "main_page", icon = shiny::icon("file-import")),
                                             shinydashboard::menuItem("2. Run the model", tabName = "Run_model", icon = shiny::icon("play")),
                                             shinydashboard::menuItem("- Additional info", tabName = 'addit_info', icon = shiny::icon("info")),
                                             tags$hr(style = "border-top: 3px solid #000000;"),
                                             shinyWidgets::actionBttn(inputId = "quit", label = "Quit",style = "jelly", color = "danger")
                                           ) #sidebarMenu
                                         ), #dashboardSidebar
                                         shinydashboard::dashboardBody(
                                           shinydashboard::tabItems(
                                             ################################################################
                                             #################  PAGE 1 INTRO  UPLOAD DATA  ##################
                                             ################################################################
                                             shinydashboard::tabItem(tabName = "main_page",
                                                                     shiny::fluidRow(
                                                                       column(width=10,offset=0.5,shinydashboard::box(title = "Bcrumb ",  solidHeader = TRUE,status="warning",
                                                                                                                     uiOutput("intro_text"),
                                                                                                                       width=12))),
                                                                     shiny::fluidRow(
                                                                       column(width=4,
                                                                                               shinydashboard::box(title = "Data upload",  collapsible = TRUE,
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
                                                                                                                   shinyWidgets::pickerInput(inputId = "Yr_col",label = "Specify the 'Year' column",
                                                                                                                                             choices = c(""),options = list(
                                                                                                                                               title = "Define the year column")),
                                                                                                                   conditionalPanel(condition = "input.Yr_col!=''",
                                                                                                                   shinyWidgets::actionBttn(inputId="button_start",label ="Plot CPUE columns",
                                                                                                                                            style = "unite",size = "sm",icon = icon("paper-plane"),
                                                                                                                                            no_outline=F,block=F,color="success")),
                                                                                                                    width=12),
                                                                              conditionalPanel(condition = "input.button_start",
                                                                              shinydashboard::box(title = "Set assumed uncertainty (CV) for each CPUE",  collapsible = TRUE,#collapsed = T,
                                                                                                  column(width = 12,uiOutput("sliders")),
                                                                                                  width=12)),
                                                                              conditionalPanel(condition ="input.button_start",
                                                                                               shinydashboard::box(title = "Name your stock and press button to continue", solidHeader = F,status = "warning",background = "yellow",
                                                                                                   textInput("stock_ID", h4("stock ID:"), ""),
                                                                                                   shinyWidgets::actionBttn(inputId="button_1",label ="Store selected files for processing",
                                                                                                              style = "unite",size = "sm",icon = icon("paper-plane"),
                                                                                                              no_outline=F,block=F,color="success"),
                                                                                                   width=12))),
                                                                       column(width=8,
                                                                              shiny::fluidRow(
                                                                                                 shinydashboard::box(title = "Uploaded data", solidHeader = TRUE,status = "warning",collapsible = TRUE,
                                                                                                                     DT::dataTableOutput("contents"),width=12),
                                                                                                 conditionalPanel(condition ="input.button_start",
                                                                                                 shinydashboard::box(title = "CPUE plot", solidHeader = TRUE,status = "warning",
                                                                                                                     column(width = 2,
                                                                                                                            tags$b("Select series to be combined"),
                                                                                                                            uiOutput("onoff")),
                                                                                                                     column(width = 10, 
                                                                                                                            shinycssloaders::withSpinner(shiny::plotOutput("CPUE_input_plot"))),
                                                                                                                     
                                                                                                                     width = 12,height=600))),
                                                                              shiny::fluidRow(
                                                                                conditionalPanel(condition = "input.button_1",
                                                                                                 shinydashboard::valueBoxOutput("Stock_infobox_1",width = 12)))
                                                                       )
                                                                     )
                                                                     
                                             ),
                                             ################################################################
                                             ###################### PAGE 2 UPLOAD DATA ######################
                                             ################################################################
                                             shinydashboard::tabItem(tabName = "Run_model", #tabItem2
                                                                     shiny::fluidRow(
                                                                       column(width=4, 
                                                                       conditionalPanel(condition = "input.button_1",
                                                                                        shinydashboard::box(collapsible = F,background = "yellow",
                                                                                            # uiOutput("cond_ifmany"),
                                                                                            shinyWidgets::actionBttn(inputId="Run_Bcrumb",label =" Start",
                                                                                                       style = "unite",size = "md",icon = icon("paper-plane"),
                                                                                                       no_outline=F,block=F,color="success"),
                                                                                            # shinyBS::bsTooltip("Run_Bcrumb", title="Press to run Bcrumb",
                                                                                            #           placement = "bottom", trigger = "hover",
                                                                                            #           options = NULL),
                                                                                            width = 12))),
                                                                       column(width=8, 
                                                                       shinydashboard::valueBoxOutput("adv_up1",width =12))
                                                                     ),
                                                                     shiny::fluidRow(
                                                                       conditionalPanel(condition = "input.Run_Bcrumb",
                                                                       shinyWidgets::progressBar(
                                                                         id = "prog_bar",value = 0,total = 100,title = "",
                                                                         display_pct = TRUE,striped = T,# size="sm",
                                                                         status="success"))
                                                                     ),

                                                                                             shiny::fluidRow(
                                                                                              column(width = 6,
                                                                                                verbatimTextOutput("summary1") 
                                                                                                ),
                                                                                              column(width = 6,
                                                                                                     verbatimTextOutput("summary2") 
                                                                                              )),
                                                                     shiny::fluidRow(
                                                                       column(width=4, 
                                                                              conditionalPanel(condition = "input.Run_Bcrumb",
                                                                                               shinydashboard::box(collapsible = F,
                                                                                                                   shinyWidgets::downloadBttn("downloadIDdata", "Download comb CPUE",
                                                                                                                                              color = "primary",
                                                                                                                                              style = "unite",
                                                                                                                                              no_outline=F,block=F
                                                                                                                   ),
                                                                                                linebreaks(3),
                                                                                                tags$b("Download a graph"),
                                                                                               shinyWidgets::materialSwitch(
                                                                                                 inputId = "DL_picA",
                                                                                                 label = "Download",
                                                                                                 value = FALSE,status = "info" ),
                                                                              conditionalPanel(condition = "input.DL_picA",
                                                                                               tags$b("Select graph"),
                                                                                               shinyWidgets::pickerInput(inputId = "Run_select_pic",
                                                                                                                         label = "",choices =c("A","B","C","D"),
                                                                                                                         options = list(title = "Select graph"),
                                                                                                                         choicesOpt = list(
                                                                                                                           subtext = c(
                                                                                                                             "Input CPUE",
                                                                                                                             "Combined CPUE",
                                                                                                                             "Diagnostics",
                                                                                                                             "Joint Index"
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
                                                                                               shinyWidgets::downloadBttn("download_pic_A", "Download graph",
                                                                                                                          color = "primary",style = "unite",
                                                                                                                          no_outline=F,block=F)), width = 12))
                                                                              ),
                                                                       column(width=8, 
                                                                              conditionalPanel(condition = "input.Run_Bcrumb",
                                                                              shinydashboard::box(collapsible = F,
                                                                                                  align="center",
                                                                                                  shinycssloaders::withSpinner(shiny::plotOutput("Fits_Bcrumb")),
                                                                                                  shiny::h5("Estimated trend in relative abundance (solid black line) with 95% credibility intervals (grey shading) from the hierarchical state-space model fit to indices of relative abundance."),
                                                                                                  width = 12),
                                                                                                  linebreaks(2),
                                                                                                  shinydashboard::box(collapsible = F,
                                                                                                                      align="center",
                                                                                                                      shinycssloaders::withSpinner(shiny::plotOutput("Resids_Bcrumb")),
                                                                              shiny::h5("Combined residuals plot of multiple indices with boxplots indicating the median and quantiles of all residuals available for any given year."),
                                                                              width = 12),
                                                                              shinydashboard::box(collapsible = F,
                                                                                                  align="center",
                                                                                                  shinycssloaders::withSpinner(shiny::plotOutput("Index_Bcrumb")),
                                                                                                  shiny::h5("Combined index of relative abundance with error bars denoting the 95% credibility intervals"),
                                                                                                  width = 12))#,height = 400
                                                                              ))
                                                                              ),
                                             shinydashboard::tabItem(tabName = "addit_info",
                                                                     #tags$hr(style = "border-top: 2px solid #000000;"),
                                                                     shiny::fluidRow(shiny::h3(tags$b("References:")),
                                                                     shiny::h4("Winker, H., Carvalho, F., Kapur, M., 2018. JABBA: Just Another Bayesian Biomass Assessment. Fisheries Research 204, 275-288. doi: 10.1016/j.fishres.2018.03.010")),
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
  
  shinyWidgets::useSweetAlert()

    #url_r= a("Link", href="https://doi.org/10.1016/j.fishres.2018.03.010",rel="search")
  
 # output$intro_text=renderUI({ shiny::h4(tagList("Bcrumb applies Bayesian hierarchical MCMC modelling to combine different time series of CPUE into one harmonized series with confidence limits. Bcrumb was developed by Henning Winker and was first published as part of JABBA, Winker et al. 2018,", url_r))})
  output$intro_text=renderUI({ shiny::h4(tagList("Bcrumb applies Bayesian hierarchical MCMC modelling to combine different time series of CPUE into one harmonized series with confidence limits. Bcrumb was developed by Henning Winker and was first published as part of JABBA, Winker et al. 2018, https://doi.org/10.1016/j.fishres.2018.03.010"))})
  
  inp_data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round,5)
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
  
  # observe({
  # updateSliderInput(session, "n_CPUEs", value = (ncol(inp_data())-1),min = 1, max =(ncol(inp_data())-1))
  #   })
  
  CPUE_obj=eventReactive(input$button_start,{
    req(inp_data())
    zzz=as.data.frame(inp_data())
    colnames(zzz)[colnames(zzz)==input$Yr_col]="yr"
   
     zzz[,-1] = t(t(as.matrix(zzz[,-1]))/apply(as.matrix(zzz[,-1]),2,mean,na.rm=TRUE))
     
    # zzz <- sapply(zzz, as.numeric)
    return(zzz)
  })
  
  
    output$onoff <- renderUI({
    req(inp_data())
   # CPUEs <- as.integer(input$n_CPUEs) # default 2
    df_clnms=  colnames(inp_data())[colnames(inp_data()) %!in% c("Year","YEAR","year", "yr", "YR")]
    CPUEs <-length(df_clnms) # default 2
       if (input$Yr_col!="") {
     lapply(1:CPUEs, function(i) {
      shinyWidgets::prettySwitch(
        inputId = paste0("CPUE_onoff_", i),
        label =df_clnms[i],
        slim = T,
        value = T,
        status = "info") 
    })}
    
  })
  
  Active_CVs <- reactive({
    req(CPUE_obj())
    #CPUEs <- as.numeric(input$n_CPUEs)
    df_clnms=  colnames(CPUE_obj())[colnames(CPUE_obj())!="yr"]
    CPUEs=length(df_clnms)
    onoffs_=c(rep(NA,CPUEs))
    for (i in 1:CPUEs) {
      if (input[[ paste0("CPUE_onoff_", i)]]==T) {
        onoffs_[i]=1 } else {
          onoffs_[i]=0 
        }
      }
    Active_CVs_b <- data.frame(CPUE_onoff_ID=paste0("CPUE_onoff_",1:CPUEs) ,df_clnms=df_clnms,onoff=onoffs_)
    return(Active_CVs_b)
  })
  Active_CVs <- Active_CVs %>% debounce(1000)
  
  
  output$sliders <- renderUI({
    req(Active_CVs())
          shiny::validate(need( as.integer(length(Active_CVs()$onoff[Active_CVs()$onoff==1]))>0, "Select at least one index!"))
    Active_CVs_=Active_CVs()
    
    CPUEs <- as.integer(length(Active_CVs_$onoff[Active_CVs_$onoff==1])) # default 2
    CPUEs_names <-Active_CVs_$df_clnms[Active_CVs_$onoff==1] # default 2
    
    lapply(1:CPUEs, function(i) {
      sliderInput(inputId = CPUEs_names[i], label =Active_CVs_$df_clnms[Active_CVs_$df_clnms== CPUEs_names[i]],
                  min = 0, max = 1, value = 0.2, step = 0.05)
    })
    })

    CVs <- reactive({
      req(Active_CVs())
      req(CPUE_obj())
      
      Active_CVs_=Active_CVs()
      CPUEs <- as.integer(length(Active_CVs_$onoff[Active_CVs_$onoff==1])) # default 2
      CPUEs_names <-Active_CVs_$df_clnms[Active_CVs_$onoff==1] # default 2

      # df_clnms=  colnames(CPUE_obj())[colnames(CPUE_obj())!="yr"]
      CPUEs=length(CPUEs_names)
       CVs_ <- sapply(1:CPUEs, function(i) {
        as.numeric(input[[CPUEs_names[i]]])
      })
      CIs_ <- sapply(1:CPUEs, function(i) {
        1.96*as.numeric(input[[ CPUEs_names[i]]])
      })
      CVs_b <- data.frame(df_clnms=CPUEs_names,CPUEs=paste0("CPUE_", 1:CPUEs), CVs=CVs_,CIs=CIs_)
    })
    
    CVs <- CVs %>% debounce(500)
  # output$test_orf <- renderTable( CVs())
    
    run_pictures <- reactiveValues()

   observe({
        shiny::validate(need(input$Yr_col!="", "Upload your data, indicate Year column!"))
     shiny::validate(need(input$button_start>0, "Upload your data, indicate Year column!"))

      shiny::validate(need(ncol(CVs())>1, "Upload your data, indicate Year column!"))

    req(CPUE_obj())
    req(Active_CVs())
    req(CVs())
    stck=CPUE_obj()
    if (input$Yr_col!=""){
     # stck=tidyr::gather(stck,key=cpue,value=val,-yr)
      stck=reshape2::melt(stck, na.rm = FALSE, value.name = "val",variable.name="cpue",id="yr")#
      #stck=stck%>%dplyr::left_join(CVs(),by=c("cpue"="df_clnms"))
      stck <- merge(stck, CVs(), by.x = "cpue", by.y = "df_clnms")
      #stck=stck%>%dplyr::left_join(Active_CVs(),by=c("cpue"="df_clnms"))
      stck <- merge(stck,Active_CVs(), by.x = "cpue", by.y = "df_clnms")
      stck=stck[stck$onoff==1,]

      run_pictures$pic_A= ggplot2::ggplot(data=stck, ggplot2::aes(x=yr, y=val, col=cpue)) +
      ggplot2::geom_line(ggplot2::aes(x=yr, y=val, col=cpue))+
      ggplot2::geom_point(ggplot2::aes(x=yr, y=val, col=cpue),size=2)+
      ggplot2::geom_errorbar(ggplot2::aes(x=yr,ymin=val-CIs, ymax=val+CIs, col=cpue), width=.5) +
      ggplot2::theme_classic()+
      ggplot2::labs(y="Normalized Index", x="Year",col="")+
        ggplot2::theme(legend.position="bottom")#+
     # ggplot2::theme(text = ggplot2::element_text(size = 18))+
     # ggplot2::scale_x_continuous(breaks = seq(min(stck$yr),max(stck$yr),5))
    }
  })
   
  output$CPUE_input_plot= shiny::renderPlot({
    shiny::validate(need(input$Yr_col!="", "Upload your data, indicate Year column!"))
    run_pictures$pic_A},height = 500)

  output$Stock_infobox_1=shinydashboard::renderValueBox({
    shinydashboard::valueBox(shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the object ", input$stock_ID,br(),
                                                                         "You can know proceed to 'Run the model'"))),
                             shiny::icon("thumbs-up", lib = "glyphicon"),
                             color = "green") })

  Final_stock=eventReactive(input$button_1,{
    req( CPUE_obj())
    req(CVs())
    final_names=c("yr",CVs()$df_clnms)
    CPUE_obj_=CPUE_obj()[,final_names]
    CPUE_obj_$check=rowSums(CPUE_obj_,na.rm = T)
    CPUE_obj_$check=CPUE_obj_$check-CPUE_obj_$yr
    xx=min(CPUE_obj_$yr[CPUE_obj_$check>0])
    CPUE_obj_=CPUE_obj_[CPUE_obj_$yr>=xx,]
    CPUE_obj_=CPUE_obj_[1:(ncol(CPUE_obj_)-1)]
   # CPUE_obj_= CPUE_obj_[cumsum(complete.cases(CPUE_obj_[2:ncol(CPUE_obj_)])) !=0, ]
       final_stock=list(CPUE_Data= CPUE_obj_,
                     CVs=CVs(),
                     stock_ID=input$stock_ID)
        return(final_stock)
  })
  
  
   
   # # shinyBS::toggleModal(session, "popup_Page_1", toggle = "toggle")
   # 
   # Stock_obj <- reactiveValues()
   # Stock_obj$Stocks=data.frame(ID=1:200,Stock=rep(NA,200))
   # Stock_obj$CPUE_ID <- template_CPUE_ID
   # Stock_obj$CPUE_data <- template_cdat
   # shinyWidgets::useSweetAlert()
   # 
   # ###### Create advise info boxes
   output$adv_up1= shinydashboard::renderValueBox({
     shinydashboard::valueBox(
       shiny::h4("Run Bcrumb"),shiny::h4("You have created a Bcrumb object for ", input$stock_ID, ", containing ", (ncol(Final_stock()[["CPUE_Data"]])-1), "CPUE indices.", br(),
                                                            "Press 'Start' button to run the model") ,
       icon = shiny::icon("info-circle"),
      color = "olive") })

  #  output$test_orf <- renderTable(Final_stock()[["CVs"]])
   
  Run_model=eventReactive(input$Run_Bcrumb,{
    req(Final_stock())
    ni <- 10000 # Number of iterations
    nt <- 2 # Steps saved
    nb <- 1000 # Burn-in
    nc <- 2 # number of chains
    parameters <- c("mean.r", "sigma","r", "Y.est","Ntot","q","r.proj")
    abundance = TRUE
    sigma.proc = TRUE
    sigma.est = TRUE
    prjr.type="all"   
    proj.stoch=FALSE  
    proc.pen = c(0.5,log(0.5),log(2))

    igamma = c(0.001,0.001) #specify inv-gamma parameters

    # Process error check
    gamma.check = 1/rgamma(1000,igamma[1],igamma[2])
    # check mean process error + CV
    mu.proc = sqrt(mean(gamma.check)); CV.proc = sd(sqrt(gamma.check))/mean(sqrt(gamma.check))


    dat=Final_stock()[["CPUE_Data"]]
    CVs=Final_stock()[["CVs"]]
    fixed.obsE=CVs$CVs
    indices = colnames(dat)[2:ncol(dat)]
    n.indices = max(length(indices),1)

    years=dat$yr
    styr = min(years)
    endyr = max(years)
    n.years = length(years)

    conv.I = as.numeric(as.matrix(dat[,-1]))
    I_y=matrix(conv.I,nrow=n.years,ncol=n.indices)
  #  cpue.CV=
      se = dat
    for(i in 1:n.indices){
      se[,i+1] = fixed.obsE[i]
    }
    conv.se = as.numeric(as.matrix(se[,-1]))
    se2 = matrix(conv.se^2,n.years,n.indices)#/2

    # Carrying capacity settings
    Ks = 5
    #Start Year
    styr = years[1]
    # prediction years
    pyears = 0 # +2 required for avergaging start + end with y = -1 and +1
    prjr = 1:n.years

    # set up year vectors
    year <- years[1]:(years[length(years)] + pyears)
    yr = years[1]:(years[length(years)])
    nT = length(year)
    mp.assess = c(1,nT)     #start and end midpoints for population trend assessment

    # creat prediction matrix for observations
    I_y= rbind(I_y,matrix(NA,pyears,(n.indices))) # now always
    # No Zero allowed in log modell
    I_y[I_y==0]=0.00001
    se2= rbind(se2,matrix(0.1,pyears,(n.indices))) # now always

    #><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
    # Setup model for population counts
    #><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>

    Ninit = NULL
    # get starting values Ninit
    for(i in 1:n.indices){
      Ninit[i] = stats::na.omit(I_y[,i])[1]
    }
    #><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
    # Setup model for relative abundance
    #><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
    #find first time-series with first CPUE
    q1.y = c(1:length(year))[is.na(apply(I_y,1,mean,na.rm=TRUE))==FALSE][1] #first year with CPUE
    q1.I =  which.max(I_y[q1.y,])
    qs = c(q1.I,c(1:(ncol(I_y)))[-q1.I])

    options(warn=0)

    qI_y = as.matrix(I_y[,qs])
    qse2 = as.matrix(se2[,qs])

    q.init = 1
    if(n.indices>1) for(i in 2:n.indices){q.init[i] = mean(qI_y[,i],na.rm=TRUE)/mean(qI_y[,1],na.rm=TRUE)}

    # Bundle data
    jags.data <- list(y = log(qI_y),SE2=qse2, logY1 = log(qI_y[1,1]), T = length(year),EY = n.years,nI=n.indices,sigma.fixed=ifelse(sigma.proc==TRUE,0,sigma.proc),igamma=igamma,penSig=0, Ks = Ks,prjr=prjr,proc.pen=proc.pen)

    # Initial values
    inits <- function(){list(isigma2.est=runif(1,20,100), itau2=runif(1,80,200), mean.r = rnorm(0,0.2),iq = 1/q.init)}
    sink(paste0("jabba2bcrumb.jags"))
    cat("
    model {
      
      # Prior specifications  
      eps <- 0.0000000000001 # small constant    
      
      iq[1] ~ dgamma(1000,1000)
      q[1] <-  pow(iq[1],-1)
      logq[1] <- log(1)
      for(i in 2:nI){
      iq[i] ~ dgamma(0.001,0.001)
      q[i] <- pow(iq[i],-1)
      logq[i] <-  log(q[i])
      }
      
      
      ")
    
    if(sigma.proc==TRUE){
      cat("
        # Process variance
        isigma2 <- isigma2.est 
        sigma2 <- pow(isigma2,-1)
        sigma <- sqrt(sigma2)
        fakesigma.fixed <- sigma.fixed # Prevent unused variable error msg    
        ",append=TRUE)  
    }else{ cat(" 
      isigma2 <- pow(sigma.fixed+eps,-2) 
             sigma2 <- pow(isigma2,-1)
             sigma <- sqrt(sigma2)
             
             ",append=TRUE)}
    
    if(sigma.est==TRUE){
      cat("
        # Obsevation variance
        # Observation error
        itau2~ dgamma(0.001,0.001)
        tau2 <- 1/itau2
        
        
        for(i in 1:nI)
        {
        for(t in 1:T)
        {
        var.obs[t,i] <- SE2[t,i]+tau2
        ivar.obs[t,i] <- 1/var.obs[t,i]
        # note total observation error (TOE)     
        TOE[t,i] <- sqrt(var.obs[t,i])
        
        }}
        ",append=TRUE)  
    }else{ cat(" 
      # Obsevation variance
             # Observation error
             itau2~ dgamma(2,2)
             tau2 <- 1/itau2
             
             
             for(i in 1:nI)
             {
             for(t in 1:T)
             {
             var.obs[t,i] <- SE2[t,i] # drop tau2
             fake.tau[t,i] <- tau2
             
             ivar.obs[t,i] <- 1/var.obs[t,i]
             # note total observation error (TOE)     
             TOE[t,i] <- sqrt(var.obs[t,i])
             
             }}
             
             ",append=TRUE)}
    
    # Run rest of code  
    cat("  
      # Process variance prior
      isigma2.est ~ dgamma(igamma[1],igamma[2])
      pen.sigma <- ifelse(sigma>proc.pen[1],log(sigma)-log(proc.pen[1]),0) 
      penSig  ~ dnorm(pen.sigma,pow(0.2,-2))
      
      # Priors and constraints
      logY.est[1] ~ dnorm(logY1, 1)       # Prior for initial population size
      
      mean.r ~ dnorm(0, 0.001)             # Prior for mean growth rate
      
      # Likelihood
      # State process
      for (t in 1:(EY-1)){
      rdev[t] ~ dnorm(0, isigma2)T(proc.pen[2],proc.pen[3])
      r[t] <- mean.r+rdev[t]-0.5*sigma2   
      logY.est[t+1] <- logY.est[t] + r[t] 
      }

      ",append=TRUE)
    
    if(prjr.type=="mean"){
      cat("  
      r.proj <- mean.r  
      prjr.dummy <- prjr   
    ",append=TRUE)}else{
      cat(" 
        r.proj <- mean(mean.r+rdev[prjr]-0.5*sigma2) 
        ",append=TRUE)  
    }
    
    
    if(proj.stoch==FALSE){
      cat("
      for (t in EY:(T)){
      rdev[t] ~ dnorm(0, pow(0.01,-2))
      r[t] <- ifelse(logY.est[t]>(max(logY.est[1:(EY-1)])+log(Ks)),0,r.proj) 
      logY.est[t+1] <- logY.est[t]+r[t]   
      }
      ",append=TRUE)} else {
        cat("
      for (t in EY:(T)){
            rdev[t] ~ dnorm(0, pow(isigma2,-2))T(0.5*proc.pen[2],0.5*proc.pen[3])
            r[t] <- ifelse(logY.est[t]>(max(logY.est[1:(EY-1)])+log(Ks)),0,r.proj+rdev[t]-0.5*sigma2) 
            logY.est[t+1] <- logY.est[t] + r[t] 
            #devK[t]  <- ifelse(logY.est[t+1]>log(Ks),logY.est[t+1]-log(Ks),0) # penalty if Y > K 
      }
      ",append=TRUE)}  
    
    cat("

      #for(t in (EY):T){ # Apply penalty for projections only
      #penK[t] ~ dnorm(devK[t],pow(0.1,-2))
      #}
      
      # Observation process
      for (t in 1:EY) {
      for(i in 1:nI){
      y[t,i] ~ dnorm(logY.est[t]+logq[i], ivar.obs[t,i])
      }}
      for (t in (EY+1):T) {
      for(i in 1:nI){
      y[t,i] ~ dnorm(logY.est[t]+logq[i],pow(0.001,-2)) # does not effect projections
      }}
      

      # Population sizes on real scale
      for (t in 1:T) {
      Y.est[t] <- exp(logY.est[t])
      Ntot[t] <- Y.est[t]
      }
      
  } 
      ",fill = TRUE)
    sink()
    
    BCrumb.mod <- R2jags::jags(jags.data, inits, parameters, paste0("jabba2bcrumb.jags"),
                               n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, quiet = T,progress.bar="none")
    outp_=list(BCrumb.mod=BCrumb.mod,I_y=I_y,qs=qs,n.indices=n.indices,se2=se2,indices=indices)
    })

  observeEvent(input$Run_Bcrumb, {
    req(Run_model()) ####mhpvs na to kanw based on stock object?
    Run_model_done <- showNotification(paste("Message:", "Model run completed"), duration = 5)
  })
  
  
  
  Model_outputs=eventReactive(input$Run_Bcrumb,{
    req(Final_stock())
    req(Run_model())
    #options(scipen=999)
    parameters <- c("mean.r", "sigma","r", "Y.est","Ntot","q","r.proj")

    dat=Final_stock()[["CPUE_Data"]]
    BCrumb.mod=Run_model()[["BCrumb.mod"]]
    n.indices=Run_model()[["n.indices"]]
    qs=Run_model()[["qs"]]

    years=dat$yr
    styr = min(years)
    endyr = max(years)
    n.years = length(years)

    end.yr=n.years

    stock_ID=Final_stock()[["stock_ID"]]
        #-------------------------------------------------------------------
    # Capture results
    #------------------------------------------------------------------
    posteriors=BCrumb.mod$BUGSoutput$sims.list
    par.dat= data.frame(posteriors[parameters[c(1:2)]])
    geweke = coda::geweke.diag(data.frame(par.dat))
    pvalues <- 2*pnorm(-abs(geweke$z))

    heidle = coda::heidel.diag(data.frame(par.dat))

    # Capture Results
    results = round(t(cbind(apply(par.dat,2,quantile,c(0.025,0.5,0.975)))),3)
    pars = data.frame(Median = results[,2],LCI=results[,1],UCI=results[,3],Geweke.p=round(pvalues,3),Heidelberger.p=round(heidle[,3],3))   ######### Somehow outputs

     # get individual trends
    fitted <- lower <- upper <- mat.or.vec(end.yr,(n.indices))
    fitted <- lower <- upper <- as.null()
    for (t in 1:end.yr){
      fitted[t] <- median(posteriors$Y.est[,t])
      lower[t] <- quantile(posteriors$Y.est[,t], 0.025)
      upper[t] <- quantile(posteriors$Y.est[,t], 0.975)}

    Nfit <- logN.se <- Nlow <- Nhigh <- as.numeric()
    # get total pop size
    for (t in 1:n.years){
      Nfit[t] =  median(posteriors$Ntot[,t])
      Nlow[t] = quantile(posteriors$Ntot[,t],0.025)
      Nhigh[t] = quantile(posteriors$Ntot[,t],0.975)
      logN.se[t] = sd(posteriors$Ntot[,t])
    }
    CPUE_fitted=data.frame(years=years, fitted=fitted, lower=lower,upper= upper)   ######Output

      q.adj = apply(posteriors$q,2,median)

    DIC =round(BCrumb.mod$BUGSoutput$DIC,1)
    # get residuals
    Resids = NULL
    for(i in 1:n.indices){
      Resids =rbind(Resids,log(dat[,qs[i]+1]/q.adj[i])-log(fitted))   ######################OUTPUT
    }
    Nobs =length(as.numeric(Resids)[is.na(as.numeric(Resids))==FALSE])
    RMSE = round(100*sqrt(sum(Resids^2,na.rm =TRUE)/(Nobs-1)),1)



    #><> carry over residual structure
    trj = posteriors$Y.est[,1:end.yr]
    mures = apply(Resids,2,mean,na.rm=T)
    mures[!is.numeric(mures)] = NA
    index.posterior =exp(t(t(log(trj))+mures))
    Index = apply(index.posterior,2,median)
    log.se = apply(log(index.posterior),2,sd)
    index.CIs = apply(index.posterior,2,quantile,c(0.025,0.975),na.rm=T)
    joint.index = data.frame(Stock = rep(stock_ID,n.years),
                             yr=years,trend=Nfit,index=Index,cv=log.se,lci=index.CIs[1,],uci=index.CIs[2,])

    # Produce statistice describing the Goodness of the Fit

    GOF = data.frame(Value = c(as.character(as.integer(n.indices)),as.character(as.integer(Nobs)),as.character(RMSE),as.character(DIC))) ########OUTPUT somehow
row.names(GOF)= c("Time series","Observations","Root mean square error (RMSE)","Deviance information criterion (DIC)")
colnames(GOF)=""
    abundance.est=data.frame(Stock = rep(stock_ID,n.years),yr=years,cpue=Nfit,log.se=logN.se,cpue.lcl=Nlow,cpue.ucl=Nhigh)########OUTPUT somehow

   outp_B=list(abundance.est=abundance.est,joint.index=joint.index,CPUE_fitted=CPUE_fitted,Resids=Resids,GOF=GOF,pars=pars,years=years)
  })

  observeEvent(input$Run_Bcrumb, {
    req(Model_outputs()) ####mhpvs na to kanw based on stock object?
    Model_outputs_done <- showNotification(paste("Message:", "Model outputs extracted"), duration = 5)
  })
  
  
  output$summary1 <- renderPrint({
   req(Model_outputs())
    print( Model_outputs()[["pars"]])
  })

  output$summary2 <- renderPrint({
    req(Model_outputs())
    print( Model_outputs()[["GOF"]])
  })

# output$test_orf <- renderTable(as.data.frame(Model_outputs()[["Resids"]]))
 observeEvent(input$Run_Bcrumb,{
   req(Model_outputs())
   req(Final_stock())
   req(Run_model())
   #options(scipen=999)
   n.indices=Run_model()[["n.indices"]]
   indices=Run_model()[["indices"]]

   I_y=Run_model()[["I_y"]]
   qs=Run_model()[["qs"]]
   BCrumb.mod=Run_model()[["BCrumb.mod"]]
   posteriors=BCrumb.mod$BUGSoutput$sims.list
   q.adj = apply(posteriors$q,2,median)

   CPUE_fitted=as.data.frame(Model_outputs()[["CPUE_fitted"]])

   years=Model_outputs()[["years"]]

   df = data.frame(matrix(nrow = length(years), ncol = n.indices+1))
   colnames(df)=c("yr",indices)
   df$yr=years
   for(i in 1:n.indices){
     df[,i+1]=I_y[,qs[i]]/q.adj[i]
   }
   #df=df%>%tidyr::gather(key=cpue,value = val,-yr)
   df=reshape2::melt(df, na.rm = FALSE, value.name = "val",variable.name="cpue",id="yr")#
   

   run_pictures$pic_B= ggplot2::ggplot()+
      ggplot2::geom_line(data=df,ggplot2::aes(x=yr,y=val,col=cpue))+
     ggplot2::geom_point(data=df,ggplot2::aes(x=yr,y=val,col=cpue))+
     ggplot2::geom_ribbon(data=CPUE_fitted,ggplot2::aes(x=years,ymin=lower, ymax=upper),col="gray",alpha=0.2)+
     ggplot2::geom_line(data=CPUE_fitted,ggplot2::aes(x=years,y=fitted),linewidth=1.2)+
     ggplot2::theme_classic()+
     ggplot2::labs(x="Year",y="Abundance index",col="")+
     ggplot2::theme(legend.position="bottom")
 })

 output$Fits_Bcrumb= shiny::renderPlot({
   run_pictures$pic_B},height = 350)

 output$adv_up_pic_1=shinydashboard::renderValueBox({
   shinydashboard::valueBox(shiny::h4("The bold curve are the combined CPUE time series, with the gray area indicating a plausible 95% confidence interval."),
                            shiny::icon("info-circle"),
                            color = "aqua") })

 output$adv_up_pic_2=shinydashboard::renderValueBox({
   shinydashboard::valueBox(shiny::h4("[Henning to provide text]"),
                            shiny::icon("info-circle"),
                            color = "aqua") })

 observeEvent(input$Run_Bcrumb,{
   req(Model_outputs())
   req(Run_model()) #><> no need
    n.indices=Run_model()[["n.indices"]]
   indices=Run_model()[["indices"]]
   if(n.indices==1) {
        Resids_=as.data.frame(t(Model_outputs()[["Resids"]]))
   } else {
     Resids_=as.data.frame(Model_outputs()[["Resids"]])
   }
   years=Model_outputs()[["years"]]


  names(Resids_)=years
#   colnames(Resids_)=indices

  # Resids_$years=years
   
    Resids_$indices=indices

   #Resids_2=Resids_%>%gather(key=Year,value = val,-indices)
   Resids_2=reshape2::melt(Resids_, na.rm = FALSE, value.name = "val",variable.name="Year",id="indices")#
   
   run_pictures$pic_C= ggplot2::ggplot()+
     ggplot2::geom_hline(ggplot2::aes(yintercept=0),linetype="dashed")+
     ggplot2::geom_boxplot(data=Resids_2, ggplot2::aes(x=as.factor(Year),y=val),outlier.shape = NA)+
     ggplot2::geom_point(data=Resids_2, ggplot2::aes(x=Year,y=val,fill=indices),shape=21,size=3)+
     ggplot2::geom_segment(data=Resids_2, ggplot2::aes(x=Year,xend=Year,y=0,yend=val,col=indices))+
     #ggplot2::geom_smooth(data=Resids_2[!is.na(Resids_2$val),],ggplot2::aes(x=as.integer(as.factor(Year)),y=val),col="black",method=loess,se = F)+
     #ggplot2::scale_x_discrete(breaks = seq(min(years),max(years),5))+
     ggplot2::labs(x="Year",y="log Residuals")+
     ggplot2::theme_bw()+
     ggplot2::theme(legend.position="bottom")+
     ggplot2::theme(axis.text.x =ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
 })
  output$Resids_Bcrumb= shiny::renderPlot({
   run_pictures$pic_C},height = 350)


# ><> new plot
  observeEvent(input$Run_Bcrumb,{
    req(Model_outputs())
    joint.index=as.data.frame(Model_outputs()[["joint.index"]])

    dfi = joint.index[!is.na(joint.index$index),]

    run_pictures$pic_D= ggplot2::ggplot(data=dfi)+
      ggplot2::geom_line(ggplot2::aes(x=yr,y=index),col=2)+
      ggplot2::geom_errorbar(ggplot2::aes(x=yr,y=index,ymin=lci, ymax=uci),col=1, width=.5)+
      ggplot2::geom_point(ggplot2::aes(x=yr,y=index),shape = 21,col=1,fill="white",size=2)+
      ggplot2::theme_classic()+
      ggplot2::labs(x="Year",y="Index",col="")

  })

  output$Index_Bcrumb= shiny::renderPlot({
    run_pictures$pic_D},height = 350)


 output$downloadIDdata <- downloadHandler(
   filename = function() {
         paste(Final_stock()[["stock_ID"]],"_Stock_", Sys.Date(), ".csv", sep = "")
       },
   content = function(file) {
     write.csv(Model_outputs()[["joint.index"]], file, row.names = FALSE)
   }
 )

 observeEvent(input$Run_Bcrumb, {
   for (i in 1:100) {
     shinyWidgets::updateProgressBar(
       session = session,
       id = "prog_bar",
       value = i, total = 100,
       title = paste("Process", trunc(i/100)))
    Sys.sleep(0.02)
   }
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

session$onSessionEnded(function() {
  stopApp()
})
 
}



shinyApp(shinyUI, shinyServer)#, ...