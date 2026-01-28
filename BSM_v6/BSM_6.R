
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
library(purrr)
library(rmarkdown)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# path=getwd()
'%!in%' <- function(x,y)!('%in%'(x,y))
linebreaks <- function(n){HTML(strrep(br(), n))}

source("CLA_functions2.R")
#source("New_functions.R")

options(scipen = 100)
nn_file<- "ffnn.bin" # file containing neural networks trained to estimate B/k priors

#####NEW
species_DB=readRDS("species_DB.rds")
TEST_DATA=readRDS("TEST_DATA.rds")


only_catch=c("Acadian_redfish_Gulf_of_Maine_Georges_Bank","Aust_pec_Namaqua","Belo_bel_TunisianPGSidra","Boop_boo_AzoresCanariesMadeira", "Boop_boo_GulfGuineaIs",        
             "Bore_sai_Kara","Brac_aur_GulfGuineaW","Brev_pec_RioGrande","Bros_bro_NGrandBanks")
TEST_DATA=TEST_DATA[names(TEST_DATA) %!in% only_catch]
Stock_names=names(TEST_DATA)
test_CATCH_ID=purrr::map(TEST_DATA, function(x) {
  purrr::pluck(x, 'input','Stock_info')
})
test_CATCH_ID=rbindlist(test_CATCH_ID)
test_CATCH_ID= tibble::as_tibble(test_CATCH_ID[,c("Continent","Region","Subregion","Group","ScientificName" ,"Stock")])

################################################################
#########################   SHINY UI    ########################
################################################################ 

shinyUI <- shinydashboard::dashboardPage(#skin = "purple",
  shinydashboard::dashboardHeader(title = "BSM"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Welcome", tabName = "main_page", icon = shiny::icon("door-open")),
      shinydashboard::menuItem("1. Data input", tabName = "Dt_in_page", icon = shiny::icon("file-import"),
                               shinydashboard::menuItem('1a. Load data',
                                                        tabName = 'Upld_dt_page',
                                                        icon = shiny::icon('upload')),
                               shinydashboard::menuItem('1b. Explore existing stocks',
                                                        tabName = 'Expl_dt_page',
                                                        icon = shiny::icon('list')),
                               shinydashboard::menuItem('1c. Load stock object',
                                                        tabName = 'Expl_stobj_page',
                                                        icon = shiny::icon('cube'))
      ),
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
    tags$head(tags$script(HTML("$(document).on('click', '.needed', function () {
                                Shiny.onInputChange('last_btn',this.id);
                             });"))),
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
                                                                                                     no_outline=F,block=F,color="primary",class="needed"),
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
                                                 shinydashboard::valueBoxOutput("Stock_infobox_1",width = 12)))#,
                              
                              # shiny::fluidRow( actionButton("save", "save"),
                              #                  textOutput("lastButtonCliked")
                              #                  )
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
                                                                                    no_outline=F,block=F,color="primary",class="needed"),
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
      ###################### PAGE 2c stock object_load ###############
      ################################################################
      shinydashboard::tabItem(tabName = "Expl_stobj_page", #tabItem2
                              shiny::fluidRow(
                                column(width=4,
                                                        shinydashboard::box(title = "Load Stock object",  collapsible = TRUE,
                                                                            fileInput("file_rds", "Press 'Browse' to choose .rds file",multiple = TRUE,
                                                                                      accept = c(".rds")),
                                                                            shinyWidgets::actionBttn(inputId="button_3",label =" Create Stock Object",
                                                                                                     style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                     no_outline=F,block=F,color="primary",class="needed"),
                                                                            width=12),
                                     conditionalPanel(condition = "input.button_3",
                         shinydashboard::valueBoxOutput("Stock_infobox_load_stock_object",width = 12))),
                                column(width=8,
                                                          shinydashboard::box(title = "Loaded Stock object info", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                                                                              DT::dataTableOutput("See_loaded_obj"),width=12),
                                                          shinydashboard::box(title = "Loaded Stock object parameters", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                                                                               DT::dataTableOutput("See_loaded_param"),width=12),
                                       column(width=6,                 
                                       shinydashboard::box(title = "Catch plot", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                                                                          shiny::plotOutput("loaded_catch_plot"),width=12)),
                                       column(width=6,                 
                                              shinydashboard::box(title = "CPUE plot", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                                                                         shiny::plotOutput("loaded_biomass_plot"),width=12)),
                                       )
                         )
                         ),

      ################################################################
      ###################### PAGE 4 Prepare Data  ####################
      ################################################################
      tabItem(tabName = "Pdata",
              fluidRow( h3("Explore and prepare the data")),
              fluidRow(
                conditionalPanel(condition = "input.button_1 |input.button_2| input.button_3",
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
                                                              inputId = "Smooth_Catch_switch",
                                                              slim = T,
                                                              label = "Edit",
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.Smooth_Catch_switch",
                                                                             sliderInput(inputId = "bw_inpt", label = "",
                                                                                         min = 1, max = 6, value =1, step =1)),
                                                            tags$b("Change CV to reflect uncertainty in catch"),
                                                            helpText("Note: Minimum of 0.1 recommended"),
                                                            shinyWidgets::prettySwitch(
                                                              inputId = "catch_cw_switch",
                                                              slim = T,
                                                              label = "Edit",
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.catch_cw_switch",
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
                                                              inputId = "Smooth_bt_switch",
                                                              slim = T,
                                                              label = "Edit",
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.Smooth_bt_switch",
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
                                                                                                              options = NULL),
                                                                                           tags$br(),
                                                                                           conditionalPanel(condition = "input.procc_rpriors",
                                                                                                            shinycssloaders::withSpinner(htmlOutput("You_have_selected"))),
                                                                                            width = 12)
                                                                       ),
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
                                                                                                                        min = 0.01, max = 1.5, value = c(0.2, 0.8),step=0.01,sep = ""),
                                                                                                            helpText("Note: r low and r high priors are an optional set of parameters to specify the range of intrinsic growth rate for the selected species. If no values are provided, the range will be based on Resilience. If values are given, the Resilience choise will be ignored.")
                                                                                           ),
                                                                                           shinyWidgets::materialSwitch(
                                                                                             inputId = "Acc_rpriors",
                                                                                             label = " Accept r prior and continue",
                                                                                             status = "primary"),
                                                                                           width=12))
                                                      ),
                                                column(width = 5,
                                                       conditionalPanel(condition ="input.procc_rpriors",
                                                          shinydashboard::box(collapsible = F, 
                                                                   shinycssloaders::withSpinner(shiny::plotOutput(outputId = "RK_priors_pic")),
                                                                   width =12)),
                                                ),       
                                               column(width = 3,
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
                                                                                                              options = NULL),width = 12)
                                                                       ),
                                                      conditionalPanel(condition = "input.con_fbase",
                                                                       shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("FB_resil",width =12))),
                                                      conditionalPanel(condition = "input.procc_rpriors",
                                                                       shinydashboard::valueBoxOutput("Res_rpriors",width = 12))
                                                       )
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
                                                        shinycssloaders::withSpinner(shiny::plotOutput(outputId = "Biomass_plot_3"))#,
                                                      #  tableOutput("TESTNEURAL") ######################
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
                                                                            tags$hr(style = "border-top: 3px solid #000000;"),
                                                                            downloadButton("report", "Generate report"),
                                                                            width=12))),
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
                                                                              shiny::h5("The plot shows as black curve the catch and as dashed horizontal line the estimate of MSY, both with indication of 95% CI. The dots indicate the catch data provided to the model."),
                                                                              width = 12))),
                                       shiny::fluidRow(
                                         conditionalPanel(condition = "input.Start_run",
                                                          shinydashboard::box(collapsible = F,
                                                                              shinycssloaders::withSpinner(shiny::plotOutput("Pred_biom_plot")),
                                                                              shiny::h5("Plot of estimated biomass (blue), with dotted lines indicating the 2.5th and 97.5th percentiles. Vertical purple lines indicate the prior biomass ranges and the grey dots indicate the scaled CPUE. The dashed horizontal line indicates Bmsy and the dotted line indicates Blim."),
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
                                                                              shiny::h5(tagList("Surplus production equilibrium parabola, with reduced productivity at small stock sizes. Overlaid are the corresponding modeled results for relative catch and stock size (red).", actionLink(inputId = "open_helpTablerk", label = "(More Guidance)"))),
                                                                              width = 12))),
                                        shiny::fluidRow(
                                                conditionalPanel(condition = "input.Start_run",
                                                                 shinydashboard::box(collapsible = F,
                                                                                     shinycssloaders::withSpinner(shiny::plotOutput("kiel_plot")),
                                                                                     shiny::h5("Kiel-plot of biomass (blue), catch (black) and maximum sustainable Fmsy-catch given the biomass (green), for easy comparison. The dotted lines indicate the absolute maximum sustainable catch (MSY) and the minimum biomass that can produce MSY. Note that Fmsy is reduced linearly if biomass falls below Bmsy and becomes zero below half of Bmsy."),
                                                                                     width = 12))), 
                                       shiny::fluidRow(
                                         column(width = 12,align="center",offset=1,
                                                conditionalPanel(condition = "input.Start_run",
                                                                 shinydashboard::box(collapsible = F,
                                                                                     shinycssloaders::withSpinner(shiny::plotOutput("kobe_plot")),
                                                                                     shiny::h5("The Kobe plot combines the time series of stock size (B/Bmsy on the X-axis) and exploitation (F/Fmsy on the Y-axis). The colors identify combinations of stock size and exploitation as: Green = sustainable; Yellow = overfished; Orange = subject to overfishing; Red = overfished and subject to overfishing. The black line shows the time series of stock status and exploitation, and the shaded areas give the plausible confidence intervals for the last year as detailed in the legend."),
                                                                                     width = 10))))
                                )
                              ), 
                              shinyBS::bsModal(id='popup_Page_helpTablerk',
                                               title ='There are three states to be observed, according to Walters et al. 2008 (doi:https://doi.org/10.1139/F08-170):',
                                               trigger="open_helpTablerk",
                                               size = "large",
                                               shiny::tableOutput('helpTablerk')
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
                                               shinycssloaders::withSpinner(shiny::plotOutput('Alexandros5'))),
                              shinyBS::bsModal(id='modalExample79',
                                               title ='Summary plot',
                                               trigger="button_summary",
                                               size = "large",
                                               shinycssloaders::withSpinner(shiny::plotOutput('Summary_plot')))
                              ),
      
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
                                                                              checkbox = T),
                                                                            numericInput(inputId="Sq_years", label="Number of years for estimating Status quo values", value=1, min = 1, max = 5, step = 1),
                                                                            tags$hr(style = "border-top: 1px solid #999797;"),
                                                                            
                                                                             conditionalPanel(condition = "input.Start_run",
                                                                         shinyWidgets::actionBttn(inputId="Start_forecast",label =" Start",
                                                                                                  style = "unite",size = "md",icon = shiny::icon("paper-plane"),
                                                                                                  no_outline=F,block=F,color="primary")),
                                                                         shinyBS::bsTooltip("Start_forecast", title="Press to run the forecast",
                                                                           placement = "bottom", trigger = "hover",
                                                                           options = NULL),
                                                                         tags$hr(style = "border-top: 1px solid #999797;"),
                                                                                width = 12),
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
                                                        )
                                                 ),
                                column(width = 8,#offset=2,
                                       conditionalPanel(condition = "input.Start_run",
                                                        shinydashboard::valueBoxOutput("Run_forecast_info1",width=6)
                                       ),
                                       conditionalPanel(condition = "input.Start_forecast",
                                                        shinydashboard::box(title = "Assumptions for the interim year",align="center", solidHeader = TRUE,status="primary",collapsible = F,  
                                                                            textOutput("interim_text"),
                                                                            shinycssloaders::withSpinner(tableOutput("interim_assumptions")),
                                                                            width = 12)
                                       ),
                                       conditionalPanel(condition = "input.Start_forecast",
                                                        shinydashboard::box(title = "Annual scenarios", solidHeader = TRUE,status="primary",align="center",
                                                                            shinycssloaders::withSpinner(tableOutput("advice_outcomes"))
                                                                            ,width = 12)) 
                                )),
                              shiny::fluidRow(
                                column(width = 4,
                                       conditionalPanel(condition = "input.Start_forecast",
                            shinydashboard::box(title = "Confidence intervals in pics", solidHeader = TRUE,status="primary",align="left",
                                                         shinyWidgets::materialSwitch(
                                                          inputId = "for_CI",
                                                          label = "Show CI in figures",
                                                          status = "primary"),width = 12)) 
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
                              #tableOutput("test_orf"), ######################
                              shiny::fluidRow(shiny::h3(tags$b("CLA app team:"))),
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
  created_stock_object=eventReactive(input$button_2,{
    if(input$Crtd_StckID_input=="") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Input a stock ID!",
        type = "error")
    }  else {
      if(input$button_2 > 0) {
        Creatd_Stck_obj=CLA_stock_maker_empty_shiny(inp_data(),input$Crtd_StckID_input,input$Yr_col,input$ct_col,input$bt_col,input$cv_col,input$do_u_have_cv)
        Creatd_Stck_obj$input$Input_parameters$Ecreep_cpue=FALSE
        Creatd_Stck_obj$input$Input_parameters$Smooth_cpue=FALSE
        Creatd_Stck_obj$input$Stock_info$ScientificName=input$Sp_Scient
        Creatd_Stck_obj$input$Stock_info$Name =Common_name()
        Creatd_Stck_obj$input$Stock_info$Continent=input$Area_Cont
        Creatd_Stck_obj$input$Stock_info$Region =input$Area_Reg
        Creatd_Stck_obj$input$Stock_info$Subregion =input$Area_SbReg
        Creatd_Stck_obj$input$Stock_info$Group  =input$Sp_Group
        Creatd_Stck_obj$input$Input_parameters$StartYear =min(as.integer(inp_data()[,input$Yr_col]),na.rm = T)
        Creatd_Stck_obj$input$Input_parameters$EndYear  =max(as.integer(inp_data()[,input$Yr_col]),na.rm = T)
        Creatd_Stck_obj$input$Input_parameters$btype=input$btype
      }
    }
    
    return(Creatd_Stck_obj)
  })
  
  output$Stock_infobox_1=shinydashboard::renderValueBox({
    shinydashboard::valueBox(shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the stock object ", input$Crtd_StckID_input,br(),
                                                                         " of the species ",tags$em(input$Sp_Scient), ".",br(),
                                                                         "You can know proceed to the 'Prepare data' tab."))),
                             shiny::icon("thumbs-up", lib = "glyphicon"),
                             color = "light-blue") })
 
  output$Catch_plot2= shiny::renderPlot({
    ggplot2::ggplot(data= created_stock_object()[["input"]][["Input_data"]], ggplot2::aes(x=yr, y=ct)) +
      ggplot2::geom_line(color="blue",size=1)+ggplot2::labs(y="Catch", x="Year")+
      ggplot2::geom_point(color="black",size=2)+ggplot2::theme_classic()+
      ggplot2::scale_y_continuous(limits=c(0,NA))
  }, height = 200)
  
  output$Biomass_plot2= shiny::renderPlot({
    ggplot2::ggplot(data= created_stock_object()[["input"]][["Input_data"]], ggplot2::aes(x=yr, y=bt)) +
      ggplot2::geom_line(color="blue",size=1)+ggplot2::labs(y="CPUE", x="Year")+
      ggplot2::geom_point(color="black",size=2)+ggplot2::theme_classic()+
      ggplot2::scale_y_continuous(limits=c(0,NA))
  }, height = 200)
  
  
  ###### EXPLORE EXISTING STOCKS
  ###### EXPLORE EXISTING STOCKS
  ###### EXPLORE EXISTING STOCKS
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
      shiny::h4("Info"),shiny::h5(if (input$txt1 %!in% c(Stock_names)) {
        "Select a stock"} else {
        TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Stock_info"]][["Name"]]}),
      icon = shiny::icon("info-circle"),
      color = "aqua") })
  
  output$txtout2=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Previous user-provided settings"),shiny::h5(if (input$txt1 %!in% c(Stock_names)) {
        "Select a stock"} else {HTML(paste(tags$b("Start, End year: "), TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$StartYear, "-",
                                           TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$EndYear,tags$br(),
                                           tags$b("Resilience, r priors: "),TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$Resilience,
                                           ", ",TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$r.low,"-",
                                           TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$r.hi,tags$br(),
                                           if( !is.na(TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$stb.low) | !is.na(TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$stb.hi)) {
                                             paste(tags$b("start B/k priors: "), TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$stb.low, "-",
                                                   TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$stb.hi,tags$br())},
                                           if( !is.na(TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$intb.low) | !is.na(TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$intb.hi) |
                                               !is.na(TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$int.yr)) {
                                             paste(tags$b("Intermediate B/k priors: "), TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$intb.low, "-",
                                                   TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$intb.hi, ", ",  TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$int.yr,tags$br())},
                                           if( !is.na(TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$endb.low) | !is.na(TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$endb.hi)) {
                                             paste(tags$b("End B/k priors: "),TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$endb.low, "-",
                                                   TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Input_parameters"]]$endb.hi, sep="")}
        ))
        }),
      icon = shiny::icon("info-circle"),
      color = "aqua") })
  
  output$txtout3=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Source"),shiny::h5(if (input$txt1 %!in% c(Stock_names)) {
        "Select a stock"} else {TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Stock_info"]]$Source}) ,
      icon = shiny::icon("info-circle"),
      color = "aqua") })
  
  output$txtout4=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Additional info"),shiny::h5(if (input$txt1 %!in% c(Stock_names)) {
        "Select a stock"} else {TEST_DATA[names(TEST_DATA)==input$txt1][[1]][["input"]][["Stock_info"]]$Comments}) ,
      icon = shiny::icon("info-circle"),
      color = "aqua") })
  
  output$Catch_plot= shiny::renderPlot({
    validate(
      need(input$txt1 %in% Stock_names, 'Choose a valid Stock from the last column of the above Data Table')
    )
    dt=TEST_DATA[names(TEST_DATA)==input$txt1]
    dt=dt[[1]][["input"]][["Input_data"]]
    ggplot2::ggplot(data=dt[dt$ct>0,], ggplot2::aes(x=yr, y=ct, group=1)) +
      ggplot2::geom_line(color="blue",size=1)+ggplot2::labs(y="Catch", x="Year")+
      ggplot2::geom_point(color="black",size=2)+ggplot2::theme_classic()+
      ggplot2::scale_y_continuous(limits=c(0,NA)) 
  }, height = 200)
  
  output$Biomass_plot= shiny::renderPlot({
    validate(
      need(input$txt1 %in% Stock_names, 'Choose a valid Stock from the last column of the above Data Table')
    )
    dt=TEST_DATA[names(TEST_DATA)==input$txt1]
    dt=dt[[1]][["input"]][["Input_data"]]
    p2= ggplot2::ggplot(data=dt[dt$bt>0,], ggplot2::aes(x=yr, y=bt, group=1)) +
      ggplot2::geom_line(color="blue",size=1)+ggplot2::labs(y="CPUE", x="Year")+
      ggplot2::geom_point(color="black",size=2)+ggplot2::theme_classic()+
      ggplot2::scale_y_continuous(limits=c(0,NA)) 
    p2
  }, height = 200)
  
  
  
  Selected_stock_object=eventReactive(input$button_1,{
    # if(input$procc_rpriors > 0 ) {
    #   shinyWidgets::sendSweetAlert(
    #     session = session,title = "Warning",
    #     text = "If you have already run an analysis for a stock and you want to run a new one for another stock, it is advised to reload the app and start from scratch.", type = "warning")
    # }
    if(input$button_1 > 0) {
      if(input$txt1 %!in% Stock_names) {
        shinyWidgets::sendSweetAlert(
          session = session,title = "Error...",
          text = "Oups !", type = "error")
      } else {
        sel_stock_obj=TEST_DATA[[which(names(TEST_DATA)==input$txt1)]]
         }
    }
    sel_stock_obj$input$Input_parameters$Ecreep_cpue[is.na(sel_stock_obj$input$Input_parameters$Ecreep_cpue)]=FALSE
    sel_stock_obj$input$Input_parameters$Smooth_cpue[is.na(sel_stock_obj$input$Input_parameters$Smooth_cpue)]=FALSE
  return(sel_stock_obj)
  })
  

 # output$see_stockID=renderTable({Stock_obj$Catch_ID})
  
  output$Stock_infobox_2=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the stock object ", Selected_stock_object()[["input"]][["Stock_info"]][["Stock"]],
                                                  " of the species ",tags$em(Selected_stock_object()[["input"]][["Stock_info"]][["ScientificName"]]), ".",br(),
                                                  "You can know proceed to the 'Prepare data' tab."))),
      shiny::icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue") })
  
  # AEK=reactive({
  #    controp=tryCatch(Selected_stck1()[["Catch_ID"]][1,"ScientificName"], error=function(err) NA)
  #  })
  
  loaded_stock_object=reactive({
    req(input$file_rds)
    df <- readRDS(input$file_rds$datapath) 
    return(df)
  })
  
  output$See_loaded_obj <-  DT::renderDataTable({
    req(loaded_stock_object())
    xx=loaded_stock_object()$input$Stock_info
    
    DT::datatable( xx)
  })
  output$See_loaded_param <-  DT::renderDataTable({
    req(loaded_stock_object())
    xx=as.data.frame(t(loaded_stock_object()$input$Input_parameters))
    colnames(xx)[1]="val"
    
    DT::datatable( xx,
                   options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
  })
  
  output$loaded_catch_plot <-renderPlot({
    req(loaded_stock_object())
    xx=loaded_stock_object()$input$Input_data
    ggplot()+geom_line(data = xx,aes(x=yr,y=ct),col="blue")+
      geom_point(data = xx,aes(x=yr,y=ct))+scale_y_continuous(limits = c(0,NA))+labs(x="Year",y="Catch")+theme_classic()
  })
  
  
  
  output$loaded_biomass_plot <-renderPlot({
    req(loaded_stock_object())
    xx=loaded_stock_object()$input$Input_data
    ggplot()+geom_line(data = xx,aes(x=yr,y=bt),col="blue")+
      geom_point(data = xx,aes(x=yr,y=bt))+scale_y_continuous(limits = c(0,NA))+labs(x="Year",y="CPUE")+theme_classic()
  })
  
  
  output$Stock_infobox_load_stock_object=shinydashboard::renderValueBox({
    shinydashboard::valueBox(shiny::h4("Success!"),shiny::h5(HTML(paste0("You have loaded the stock object ", loaded_stock_object()$input$Stock_info$Stock,br(),
                                                                         " of the species ",tags$em(loaded_stock_object()$input$Stock_info$ScientificName), ".",br(),
                                                                         "You can know proceed to the 'Prepare data' tab."))),
                             shiny::icon("thumbs-up", lib = "glyphicon"),
                             color = "light-blue") })
  
  ################################################################
  ###################### PAGE 4 PREPARE DATA  #####################
  ################################################################
  
  #########SELECT STOCK TO WORK WITH PRIORS

  Final_stock=eventReactive(input$prepare_data,{
   if (input$last_btn=="button_1") {
     fs=Selected_stock_object()
   } else if (input$last_btn=="button_2") {
     fs=created_stock_object()
   } else if (input$last_btn=="button_3" & isTruthy(input$file_rds)) {
     fs=loaded_stock_object()
   }
    return(fs)
  })
 # Final_stock <- Final_stock %>% debounce(1000)
  
  output$Stock_infobox_prepare=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Stock object"),shiny::h5(HTML(paste0("You have selected the stock ",
                                                      tags$b(Final_stock()$input$Stock_info$Stock), " of the species ",
                                                      tags$b(tags$em(Final_stock()$input$Stock_info$ScientificName))
      ))),
      icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "light-blue") })
  
  
  
  observe({   #TRICK to erase things
    req(Final_stock())
    xxx=ifelse(Final_stock()[["input"]][["Input_parameters"]]$Smooth_K_catch==1,FALSE,TRUE)
     shinyWidgets::updatePrettySwitch(session, "Smooth_Catch_switch", value =xxx)
     
  })
  
  observe({
    req(Final_stock())
    xxx=ifelse(Final_stock()[["input"]][["Input_parameters"]]$Smooth_K_catch==1,1,Final_stock()[["input"]][["Input_parameters"]]$Smooth_K_catch)
    updateSliderInput(session, "bw_inpt", value =xxx)
  })
  
  
  observe({
    req(Final_stock())
    xxx=ifelse(Final_stock()[["input"]][["Input_parameters"]]$Smooth_K_cpue==1,1,Final_stock()[["input"]][["Input_parameters"]]$Smooth_K_cpue)
    updateSliderInput(session, "bw_inpt_bt", value =xxx)
  })
  

  
  observe({
    req(Final_stock())
    xxx=ifelse(is.na(Final_stock()[["input"]][["Input_parameters"]]$CV_cpue) |
                 Final_stock()[["input"]][["Input_parameters"]]$CV_cpue==0.2,
               0.2, Final_stock()[["input"]][["Input_parameters"]]$CV_cpue)
    updateSliderInput(session, "CPUE_CV", value =xxx)
  })
  

  observe({
    req(Final_stock())
    xxx=ifelse(Final_stock()[["input"]][["Input_parameters"]]$CV_catch==0.15,F,T)
    shinyWidgets::updatePrettySwitch(session, "catch_cw_switch", value = xxx)
  })
  
  observe({
    req(Final_stock())
    xxx=ifelse(Final_stock()[["input"]][["Input_parameters"]]$CV_catch==0.15 | is.na(Final_stock()[["input"]][["Input_parameters"]]$CV_catch),0.15,Final_stock()[["input"]][["Input_parameters"]]$CV_catch)
    updateSliderInput(session, "Catch_CV", value =xxx)
  })
  
  observe({
    req(Final_stock())
    xxx=ifelse(Final_stock()[["input"]][["Input_parameters"]]$Smooth_cpue==F | Final_stock()[["input"]][["Input_parameters"]]$Smooth_cpue=="FALSE",F,T)
    shinyWidgets::updatePrettySwitch(session, "Smooth_bt_switch", value = xxx)
  })
  
  observe({   #TRICK to erase things
    req(Final_stock())
    mnyr=min(Final_stock()$input$Input_data$yr,na.rm = T)
    mxyr= max(Final_stock()$input$Input_data$yr[!is.na(Final_stock()$input$Input_data$ct)],na.rm = T)
   starty=ifelse(is.na(Final_stock()$input$Input_parameters$StartYear),mnyr,Final_stock()$input$Input_parameters$StartYear)
   endy=ifelse(is.na(Final_stock()$input$Input_parameters$EndYear) ,mxyr,Final_stock()$input$Input_parameters$EndYear)
    updateSliderInput(session, "yr_slct", value = c(starty,endy),min = mnyr, max =mxyr)
  })

  observe({   #TRICK to erase things
    req(Final_stock())
    req(input$yr_slct)
    mnyr=input$yr_slct[1]
    maxnyr=input$yr_slct[2]
    start.yr.bio  =Final_stock()$input$Input_data[c("yr","bt")]$yr[which(Final_stock()$input$Input_data[c("yr","bt")]$bt>0)[1]]
    if (mnyr>=start.yr.bio) {
      AAA=mnyr} else {
        AAA=start.yr.bio
      }
    end.yr=Final_stock()$input$Input_data[c("yr","bt")]$yr[max(which(Final_stock()$input$Input_data[c("yr","bt")]$bt>0 & !is.na(Final_stock()$input$Input_data[c("yr","bt")]$bt)))]
    selestart=ifelse(is.na(Final_stock()[["input"]][["Input_parameters"]][["StartYear_bt"]]),AAA,Final_stock()[["input"]][["Input_parameters"]][["StartYear_bt"]])
    seleend=ifelse(is.na(Final_stock()[["input"]][["Input_parameters"]][["EndYear_bt"]]),end.yr,Final_stock()[["input"]][["Input_parameters"]][["EndYear_bt"]])
   if (seleend>=maxnyr){
     seleend=maxnyr
   }
      updateSliderInput(session, "bio_yr_slct", value = c(selestart,seleend) ,min = AAA, max =maxnyr)
  })
  
  observe({
    req(Final_stock())
    xxx=ifelse(Final_stock()[["input"]][["Input_parameters"]]$Ecreep_cpue=="FALSE" | 
                 Final_stock()[["input"]][["Input_parameters"]]$Ecreep_cpue==FALSE |
                 is.na(Final_stock()[["input"]][["Input_parameters"]]$Ecreep_cpue),
               FALSE,TRUE)
    updatePrettySwitch(session, "ecreepyes", value =xxx)
  })
  
  observe({
    req(Final_stock())
    xxx=ifelse(Final_stock()[["input"]][["Input_parameters"]]$CV_cpue==0.2 | is.na(Final_stock()[["input"]][["Input_parameters"]]$CV_cpue),FALSE,TRUE)
    updatePrettySwitch(session, "upMaterialbcv", value =xxx)
  })

  observe({
    req(Final_stock())
    xxxx=ifelse(is.na(Final_stock()[["input"]][["Input_parameters"]]$Ecreep_Year),input$bio_yr_slct[1],Final_stock()[["input"]][["Input_parameters"]]$Ecreep_Year)
    updateSliderInput(session, "ecreep_year", value =xxxx,min =input$bio_yr_slct[1], max =input$bio_yr_slct[2]-1)
    })
  
  observe({
    req(Final_stock())
    xxxx=ifelse(Final_stock()[["input"]][["Input_parameters"]]$Ecreep_value==2,2,Final_stock()[["input"]][["Input_parameters"]]$Ecreep_value)
    updateSliderInput(session, "ecreepslider", value =xxxx)
  })
  
  #  url_palom= a("Link", href="https://doi.org/10.5751/ES-11136-240331")
  output$help_ecreep=renderUI({ shiny::h5(tagList("Note: Over time, fishers become more efficient at catching fish; a 2% increase in catchabilty per year is common (Palomares and Pauly 2019), https://doi.org/10.5751/ES-11136-240331",))})# url_palom

  Catch_obj_=reactive({
    req(Final_stock())
    inp=Final_stock()[["input"]][["Input_data"]][,c("yr","ct","bt")]
    ct_obj= Catch_obj(inp,start_yr=input$yr_slct[1], end_yr=input$yr_slct[2],smoother_bw=input$bw_inpt,Catch_CV=input$Catch_CV,Plot=F,ShowCV=input$catch_cw_switch)
    return(ct_obj)
     })
    
    output$Check_Catch_plot= shiny::renderPlot({
      req(Catch_obj_())
      print(Catch_obj_()$pic)
    })#, height = 340
   
    output$When_happy=shinydashboard::renderValueBox({
      shinydashboard::valueBox( shiny::h4("Message"),shiny::h4("When you are happy with data preparation, move on to the '3. Priors' tab where you can also save the modified data."),
                                icon = shiny::icon("envelope"),color = "light-blue")})
      
    bio_obj=reactive({
      req(Final_stock())
      inp=Final_stock()[["input"]][["Input_data"]][,c("yr","ct","bt")]
      temp_bio_obj =Bio_obj(inp,start_yr=input$bio_yr_slct[1], end_yr=input$bio_yr_slct[2],smoother_bw=input$bw_inpt_bt,ecreep=input$ecreepyes,ecreep_yr=input$ecreep_year,ecreepvalue=input$ecreepslider,Biom_type=Final_stock()$input$Input_parameters$btype,CPUE_CV=input$CPUE_CV,Plot=F,ShowCV=input$upMaterialbcv) 
      return(temp_bio_obj)
    })
    
    bio_obj <- bio_obj %>% debounce(1000)
    
    output$Check_Biomass_plot= shiny::renderPlot({
      validate(need(!all(is.na(bio_obj()$bt_data$bt)), 'No CPUE data available'))
      req(bio_obj())
      print(bio_obj()$pic)
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
 
    output$You_have_selected=renderUI({
     shiny::h5(HTML(paste0("You have selected the stock ",
                                                        tags$b(Final_stock()$input$Stock_info$Stock), " of the species ",
                                                        tags$b(tags$em(Final_stock()$input$Stock_info$ScientificName)),"."
        ))) })
    
    Fishbase_text=eventReactive(input$con_fbase,{
      req(Final_stock())
      results= fbsb(Final_stock()$input$Stock_info$ScientificName)
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
          shiny::h4("FishBase/SeaLifeBase info"),tagList(HTML(paste0("According to FishBase/SeaLifeBase, ", tags$em(Final_stock()$input$Stock_info$ScientificName), " has prior r range:", tags$b(Fishbase_text()[1]),tags$br(), "<font color=\"#cc0000\"><b>"," For more info press: ",url_r(), "</b></font>",tags$br(), "Since no information on resilience is available in FishBase/SeaLifeBase, using Medium resilience for 'regular' species and Low resilience for large or deep-water species is a reasonable starting point. Very low resilience is found in very large species and High resilience is found in species which reproduce in their first year." ))),
          icon = shiny::icon("info-circle"),
          color = "aqua") } else {
            shinydashboard::valueBox(
              shiny::h4("FishBase/SeaLifeBase info"),tagList(HTML(paste0("According to FishBase/SeaLifeBase, ", tags$em(Final_stock()$input$Stock_info$ScientificName), " has "," Resilience: ",
                                                                         tags$b(Fishbase_text()[2]), " Prior r range:", tags$b(Fishbase_text()[1]),tags$br(), "<font color=\"#cc0000\"><b>"," For more info press: ",url_r(), "</b></font>"))),
              icon = shiny::icon("info-circle"),
              color = "aqua")
          }
    })
    
    observe({
      req(Final_stock())
      xxx=ifelse(is.na(Final_stock()[["input"]][["Input_parameters"]]$Resilience),"",Final_stock()[["input"]][["Input_parameters"]]$Resilience)
      updateSelectInput(session, "resilience_in", selected =xxx)
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
              if (is.na(Final_stock()$input$Input_parameters$r.low)) {
                rlow=input$priors_rrange[1] } else {
                rlow=Final_stock()$input$Input_parameters$r.low}
              if (is.na(Final_stock()$input$Input_parameters$r.hi)) {
                rhigh=input$priors_rrange[2] } else {
                rhigh=Final_stock()$input$Input_parameters$r.hi}
              # rlow=input$priors_rrange[1]
              # rhigh= input$priors_rrange[2]
              }
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
    
    observe({
      updateSelectInput(session, "resilience_in", selected=Fishbase_text()[5])
    })
    
    rk_obj=reactive({
      req(Catch_obj_())
   #   req(temp_rpriors())
      r_K_priors=as.numeric(c(input$priors_rrange[1],input$priors_rrange[2]))
    rk=rk.priors(Catch_obj_(),r_K_priors)
    return(rk)
    })
    
    output$RK_priors_pic= shiny::renderPlot({
      req(rk_obj())
      print(rk_obj()$pic)
    })#, height = 340
    
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
               if (is.na(Final_stock()$input$Input_parameters$stb.low)) {
                    ebk_stat_low=input$man_Bk_start_A[1] } else {
                      ebk_stat_low=Final_stock()$input$Input_parameters$stb.low}
                  if (is.na(Final_stock()$input$Input_parameters$stb.hi)) {
                    ebk_stat_high=input$man_Bk_start_A[2] } else {
                      ebk_stat_high=Final_stock()$input$Input_parameters$stb.hi}
                  # ebk_stat_low=input$man_Bk_start_A[1]
                  # ebk_stat_high= input$man_Bk_start_A[2]
                   }
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
                  if (is.na(Final_stock()$input$Input_parameters$intb.low)) {
                    ebk_stat_low=input$man_Bk_int_A[1] } else {
                      ebk_stat_low=Final_stock()$input$Input_parameters$intb.low}
                  if (is.na(Final_stock()$input$Input_parameters$intb.hi)) {
                    ebk_stat_high=input$man_Bk_int_A[2] } else {
                      ebk_stat_high=Final_stock()$input$Input_parameters$intb.hi}  }
                  # if (is.na(Final_stock()$input$Input_parameters$int.yr)) {
                  #   ebk_stat_year=as.integer(floor((Catch_obj_()[["ct_data"]][c("yr","ct_smthd")]$yr[which(Catch_obj_()[["ct_data"]][c("yr","ct_smthd")]$ct_smthd>0)[1]]+max(as.integer(Catch_obj_()[["ct_data"]]$yr),na.rm = T))/2)) } else {
                  #     ebk_stat_year=Final_stock()$input$Input_parameters$int.yr}
                  # # ebk_stat_low=input$man_Bk_start_A[1]
                  # # ebk_stat_high= input$man_Bk_start_A[2]
              
      expert_bk_ind_priors=c(expert_bk_ind_,ebk_stat_low,ebk_stat_high)
    })
    
    
    temp_expert_bk_ind_year=reactive({
      if (is.na(Final_stock()$input$Input_parameters$int.yr)) {
        ebk_stat_year=as.integer(floor((Catch_obj_()[["ct_data"]][c("yr","ct_smthd")]$yr[which(Catch_obj_()[["ct_data"]][c("yr","ct_smthd")]$ct_smthd>0)[1]]+max(as.integer(Catch_obj_()[["ct_data"]]$yr),na.rm = T))/2)) } else {
          ebk_stat_year=Final_stock()$input$Input_parameters$int.yr}
      # ebk_stat_low=input$man_Bk_start_A[1]
      # ebk_stat_high= input$man_Bk_start_A[2]
      
      expert_bk_ind_year_priors=c(ebk_stat_year)
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
                  if (is.na(Final_stock()$input$Input_parameters$endb.low)) {
                    ebk_end_low=input$man_Bk_end_A[1] } else {
                      ebk_end_low=Final_stock()$input$Input_parameters$endb.low}
                  if (is.na(Final_stock()$input$Input_parameters$endb.hi)) {
                    ebk_end_high=input$man_Bk_end_A[2] } else {
                      ebk_end_high=Final_stock()$input$Input_parameters$endb.hi}
                  # ebk_end_low=input$man_Bk_end_A[1]
                  # ebk_end_high= input$man_Bk_end_A[2]
                  }
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
      req()
      updateSliderInput(session, "man_Bk_int_year_A", value = as.numeric(temp_expert_bk_ind_year()[1]),min = min(Catch_obj_()[["ct_data"]]$yr), max =max(Catch_obj_()[["ct_data"]]$yr))
    })
    
    observe({
      start.yr =Catch_obj_()[["ct_data"]][c("yr","ct_smthd")]$yr[which(Catch_obj_()[["ct_data"]][c("yr","ct_smthd")]$ct_smthd>0)[1]]
      end.yr    =max(as.integer(Catch_obj_()[["ct_data"]]$yr),na.rm = T)
      updateSliderInput(session, "bk_priors_year", value =start.yr,min =start.yr, max =end.yr)
    })
    
    #LBB_data <- LBB_data %>% debounce(1000)
    
    output$Check_priors_plot= shiny::renderPlot({
      # validate(
      #   need(isTruthy(input$resilience_in), "You han't set resilience for the stock neither FishBase/SeaLifeBase provided this information. Press 'Change r prior' and select a 'resilience' option from the dropdown list to be able to continue.")
      # )
      req(Catch_obj_())
      start_yr =Catch_obj_()[["ct_data"]][c("yr","ct_smthd")]$yr[which(Catch_obj_()[["ct_data"]][c("yr","ct_smthd")]$ct_smthd>0)[1]]
      end_yr    =max(as.integer(Catch_obj_()[["ct_data"]]$yr),na.rm = T)
      
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
    
    
    observe({
    if (is.na(Final_stock()$input$Input_parameters$nbk) |Final_stock()$input$Input_parameters$nbk==3 ) {
      xxx="All three (recommended)" } else if (Final_stock()$input$Input_parameters$nbk==2) {
        xxx= "Start & intermediate" } else {
          xxx=1
        }
    updateAwesomeRadio(session, "nbk_", selected=xxx)
    }) 
    
    NBK=reactive({
    xx=ifelse(input$nbk_=="All three (recommended)",3,ifelse(input$nbk_=="Start & intermediate",2,1))
    return(xx)
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
    
    Bk_obj=reactive({
      req(NBK())
      req(Catch_obj_())
      bkobj=Biom_priors(Catch_obj_(),nbk=NBK(),start_bio= c(input$man_Bk_start[1],input$man_Bk_start[2]),
                        int_bio=c(input$man_Bk_int[1],input$man_Bk_int[2]),int_bio_year=input$man_Bk_int_year,
                        end_bio= c(input$man_Bk_end[1],input$man_Bk_end[2]), Plot=F)
      return(bkobj)
    })

    output$You_can_proceed_to_run=shinydashboard::renderValueBox({
      shinydashboard::valueBox( shiny::h4("You can now proceed to'Run the model' tab"),shiny::h5(" "),
                                icon = shiny::icon("envelope"),color = "light-blue")})

    output$Biomass_plot_3= shiny::renderPlot({
      req(Bk_obj())
      print(Bk_obj()$pic)
    })   
    
    CLA_object=eventReactive(input$Save_priors,{
    req(Catch_obj_())
    req(bio_obj())
    req(Bk_obj())
    req(rk_obj())
    #req(Final_stock())
    
    if (isTruthy(input$con_fbase)) {
      FT=Fishbase_text()} else {
        FT=NULL
      }
    
    claobj=CLA_stock_maker(Stock= Final_stock()$input$Stock_info$Stock,
                              Species=Final_stock()$input$Stock_info$ScientificName,
                              Catch_object=Catch_obj_(),
                              Biomass_object=bio_obj(),
                              rk_object=rk_obj(),
                              bk_object=Bk_obj(),
                              fbslb=FT,
                           MAN_ANN_bk="MAN") 
    claobj$input$Stock_info$Name = Final_stock()$input$Stock_info$Name
    claobj$input$Stock_info$Continent=   Final_stock()$input$Stock_info$Continent
    claobj$input$Stock_info$Region =  Final_stock()$input$Stock_info$Region
    claobj$input$Stock_info$Subregion =  Final_stock()$input$Stock_info$Subregion 
    claobj$input$Stock_info$Group  =  Final_stock()$input$Stock_info$Group 
    claobj$input$Stock_info$Comments  =  Final_stock()$input$Stock_info$Comments 
    claobj$input$Stock_info$Source  =  Final_stock()$input$Stock_info$Source 
    
    return(claobj)
    })
    
    output$TESTNEURAL=renderTable({Fishbase_text()})

    middle_outplts=eventReactive(input$Save_priors==T,{
      if (input$nbk_=="Only start") {
        outpts_m=data.frame(range=c(paste0(CLA_object()[["input"]][["Input_parameters"]]$StartYear, "-", CLA_object()[["input"]][["Input_parameters"]]$EndYear),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$StartYear_bt, "-", CLA_object()[["input"]][["Input_parameters"]]$EndYear_bt),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$r.low, "-",CLA_object()[["input"]][["Input_parameters"]]$r.hi),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$stb.low, "-",CLA_object()[["input"]][["Input_parameters"]]$stb.hi)
                                    #paste0(format(CLA_object()[["input"]][["Input_parameters"]]$q_low,digits=3), "-",format(CLA_object()[["input"]][["Input_parameters"]]$q_high,digits=3))
        ))
        row.names(outpts_m)=c("Analysis","cpue", "Prior for r","Start B/k prior")#,"q prior","Interm. B/k prior","End B/k prior"
      } else if (input$nbk_=="Start & intermediate") {
        outpts_m=data.frame(range=c(paste0(CLA_object()[["input"]][["Input_parameters"]]$StartYear, "-", CLA_object()[["input"]][["Input_parameters"]]$EndYear),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$StartYear_bt, "-", CLA_object()[["input"]][["Input_parameters"]]$EndYear_bt),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$r.low, "-",CLA_object()[["input"]][["Input_parameters"]]$r.hi),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$stb.low, "-",CLA_object()[["input"]][["Input_parameters"]]$stb.hi),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$intb.low, "-",CLA_object()[["input"]][["Input_parameters"]]$intb.hi)
                                    # paste0(format(CLA_object()[["input"]][["Input_parameters"]]$q_low,digits=3), "-",format(CLA_object()[["input"]][["Input_parameters"]]$q_high,digits=3))
        ),
        year=c("","","","",CLA_object()[["input"]][["Input_parameters"]]$int.yr))#,"",""
        row.names(outpts_m)=c("Analysis","cpue","Prior for r","Start B/k prior","Interm. B/k prior")#,"q prior"
      } else {
        outpts_m=data.frame(range=c(paste0(CLA_object()[["input"]][["Input_parameters"]]$StartYear, "-", CLA_object()[["input"]][["Input_parameters"]]$EndYear),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$StartYear_bt, "-", CLA_object()[["input"]][["Input_parameters"]]$EndYear_bt),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$r.low, "-",CLA_object()[["input"]][["Input_parameters"]]$r.hi),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$stb.low, "-",CLA_object()[["input"]][["Input_parameters"]]$stb.hi),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$intb.low, "-",CLA_object()[["input"]][["Input_parameters"]]$intb.hi),
                                    paste0(CLA_object()[["input"]][["Input_parameters"]]$endb.low, "-",CLA_object()[["input"]][["Input_parameters"]]$endb.hi)
                                    # paste0(format(CLA_object()[["input"]][["Input_parameters"]]$q_low,digits=3), "-",format(CLA_object()[["input"]][["Input_parameters"]]$q_high,digits=3))
        ),
        year=c("","","","",CLA_object()[["input"]][["Input_parameters"]]$int.yr,""))#,""
        row.names(outpts_m)=c("Analysis","cpue","Prior for r","Start B/k prior","Interm. B/k prior","End B/k prior")#,"q prior"
      }            
      return(outpts_m) 
    })
 
    output$middle_outputs <-renderPrint({middle_outplts()
    })
    
    output$param_sofar <-renderText({paste("Parameterization so far for the stock",
                                           tags$b(CLA_object()[["input"]][["Stock_info"]]$Stock), "of", 
                                           tags$em(CLA_object()[["input"]][["Stock_info"]]$ScientificName))
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
      updateTextInput(session, "obj.name",value=paste0(Final_stock()$input$Stock_info$Stock, "_V1") ) #, placeholder=paste0("e.g.  ",Final_stock()[["Catch_ID"]]$Stock, "_V1")
    })

    object_NAME=reactive({
      req(CLA_object())
      if (input$obj.name=="") {
        name=paste0(CLA_object()$input$Stock_info$Stock, "_V1")
      } else {
        name=input$obj.name
      }
      return(name)
    })
    
    observeEvent(input$Save_priors, {
      req(object_NAME())
      CLA_obj <- CLA_object()
      nm=object_NAME()
      
      
      dir.create( paste0(dir.name(),"/BSM_wd"))
      dir=paste0(paste0(dir.name(),"/BSM_wd/"))
      
      saveRDS(CLA_obj,file =paste0(dir,"/","data_prep_CLA_object_", object_NAME(), "_A.rds"))
      Save_done <- showNotification(paste("Message:",  "The stock object with the input parameterization has been saved in ", paste0(dir.name(),"/BSM_wd/",object_NAME(), ".rds")), duration = 10)
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
    })
    
    output$created_objects <- renderUI({
      renderTable({as.data.frame( objects()[!is.na( objects()$A.A),])})
    })
    
    ###################### RUN THE MODEL  
    ###################### RUN THE MODEL  
    ###################### RUN THE MODEL  
    ###################### RUN THE MODEL  
    
    observe({
      output$Zavarakatranemia=renderUI({ shiny::h4(tagList("Run the model and get the results for the stock ",  tags$b(CLA_object()[["input"]][["Stock_info"]][["Stock"]]), "of",  tags$em(CLA_object()[["input"]][["Stock_info"]][["ScientificName"]])))})
    }) 
    
    output$Run_infobox_patient=shinydashboard::renderValueBox({
      shinydashboard::valueBox( shiny::h4("Run the model"),shiny::h5("Press 'Start' button to run the model.
              It takes some time to do all the necessary calculations, so be patient!"),
                                icon = shiny::icon("envelope"),
                                color = "light-blue")
    })
    
    BSM_run=eventReactive(input$Start_run,{
      CLA.fit(CLA_object(),METHOD="BSM")
    })
    
    observeEvent(input$Start_run, {
      req(BSM_run())
      Model_run <- showNotification(paste("Message: ", "Model run completed"), duration = 5)
    })
    
    ##################### EXTRACT OUTCOMES TO STOCK OBJECT
    CLA_object_final=eventReactive(input$Start_run,{
      CLA_object_final_=extract.CLA.fit(BSM_run(),CLA_object())
    })
    
    observeEvent(input$Start_run, {
      req(CLA_object_final())
      Model_run <- showNotification(paste("Message: ", "Model outcomes extracted"), duration = 5)
    })
    
    observeEvent(input$Start_run,{   #SEE AFTER 5
      req(CLA_object_final())
      start.yr =CLA_object_final()[["input"]][["Input_parameters"]][["StartYear"]]
      end.yr    =CLA_object_final()[["input"]][["Input_parameters"]][["EndYear"]]
      nyr=end.yr-start.yr+1
      nyr_mid=ceiling(nyr/2)
      updateSliderInput(session, "retro_range", value =ifelse(nyr_mid<3,nyr_mid,3),  max =nyr_mid) 
    })
    
    run_pictures <- reactiveValues()
    observeEvent(input$Start_run,{
      run_pictures$pic_A=ggrk.plot(CLA_object_final(),BSM_run(),"BSM") 
      pic_A_ready <- showNotification(paste("Message: ", "r-k paired graph ready"), duration = 5)
    })
    
    output$rk_space= shiny::renderPlot({
      run_pictures$pic_A})#,height = 230
    
    ####################### Stock info
    ####################### Stock info
    ####################### Stock info
    ####################### Stock info
    
    output$ModOutp_a1=renderUI({HTML(paste0(tags$b("Species: "),tags$em(CLA_object_final()[["input"]][["Stock_info"]]$ScientificName), ", stock:",  CLA_object_final()[["input"]][["Stock_info"]]$Stock))})
    output$ModOutp_a2=renderUI({HTML(paste0(tags$b("Common name: "),tags$em(CLA_object_final()[["input"]][["Stock_info"]]$Name)))})
    
    output$ModOutp_a3=renderUI({HTML(paste0(tags$b("Region: "),CLA_object_final()[["input"]][["Stock_info"]]$Region, ", ", CLA_object_final()[["input"]][["Stock_info"]]$Subregion))})
    output$ModOutp_a4=renderUI({HTML(paste0(tags$b("Catch data used from years: "),CLA_object_final()[["input"]][["Input_parameters"]]$StartYear,
                                            "-", CLA_object_final()[["input"]][["Input_parameters"]]$EndYear,", abundance: ", CLA_object_final()[["input"]][["Input_parameters"]]$btype))})
    
    output$ModOutp_a5=renderUI({HTML(paste0(tags$b("Prior initial rel. biomass: "),format(round(as.numeric(CLA_object_final()[["input"]][["Input_parameters"]]$stb.low),2),digits = 3), "-", format(as.numeric(CLA_object_final()[["input"]][["Input_parameters"]]$stb.hi),digits = 3)))})
    
    output$ModOutp_a6=renderUI({
      if (input$nbk_!="Only start") {
        HTML(paste0(tags$b("Prior intermediate rel. biomass: "),format(as.numeric(CLA_object_final()[["input"]][["Input_parameters"]]$intb.low),digits = 3), "-", format(as.numeric(CLA_object_final()[["input"]][["Input_parameters"]]$intb.hi),digits = 3), ", ", CLA_object_final()[["input"]][["Input_parameters"]]$int.yr))
      }
    })
    output$ModOutp_a7=renderUI({
      if (input$nbk_=="All three (recommended)") {
        HTML(paste0(tags$b("Prior final rel. biomass: "),format(as.numeric(CLA_object_final()[["input"]][["Input_parameters"]]$endb.low),digits = 3), "-", format(as.numeric(CLA_object_final()[["input"]][["Input_parameters"]]$endb.hi),digits = 3)))
      }
    })
    
    output$ModOutp_a8=renderUI({HTML(paste0(tags$b("Prior range for r: "),CLA_object_final()[["input"]][["Input_parameters"]]$r.low, "-", CLA_object_final()[["input"]][["Input_parameters"]]$r.hi))})
    #output$ModOutp_a9=renderUI({HTML(paste0(tags$b("Derived prior range for k: "),format(round(r.k_priors()[["rkpriors"]]$prior.k.low),digits = 3), "-", format(round(r.k_priors()[["rkpriors"]]$prior.k.hi),digits = 3)))})
    output$ModOutp_a10=renderUI({HTML(paste0(tags$b("Derived prior MSY: "),format(round(as.numeric(CLA_object_final()[["input"]][["Input_parameters"]]$MSY_prior)),digits = 3)))})
    output$ModOutp_a11=renderUI({HTML(paste0(tags$b("Derived prior range for q: "),format(CLA_object_final()[["input"]][["Input_parameters"]]$q_low,digits = 3), "-", format(CLA_object_final()[["input"]][["Input_parameters"]]$q_high,digits = 3)))})
    
    ####################### Model outputs
    ####################### Model outputs
    ####################### Model outputs
    ####################### Model outputs
    output$ModOutp_b1=renderUI({HTML(paste0(tags$b("r= "),format(CLA_object_final()[["output"]][["output_posteriors"]]$r_post[1],digits = 3),
                                            ", ",tags$b("95% CL ="),format(CLA_object_final()[["output"]][["output_posteriors"]]$r_post[2],digits = 3), "-",
                                            format(CLA_object_final()[["output"]][["output_posteriors"]]$r_post[3],digits = 3)))})
    output$ModOutp_b2=renderUI({HTML(paste0(tags$b("k= "),format(round(CLA_object_final()[["output"]][["output_posteriors"]]$k_post[1]),digits = 3),
                                            ", ",tags$b("95% CL ="),format(round(CLA_object_final()[["output"]][["output_posteriors"]]$k_post[2]),digits = 3), "-",
                                            format(round(CLA_object_final()[["output"]][["output_posteriors"]]$k_post[3]),digits = 3)))})
    
    output$ModOutp_b3=renderUI({HTML(paste0(tags$b("MSY= "),format(round(CLA_object_final()[["output"]][["output_posteriors"]]$MSY_post[1]),digits = 3),
                                            ", ",tags$b("95% CL ="),format(round(CLA_object_final()[["output"]][["output_posteriors"]]$MSY_post[2]),digits = 3), "-",
                                            format(round(CLA_object_final()[["output"]][["output_posteriors"]]$MSY_post[3]),digits = 3)))})
    
    output$ModOutp_b4=renderUI({HTML(paste0(tags$b("Relative biomass in last year = "),format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$bk, n=1),digits = 3),
                                            "k, 2.5th perc = ",format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$bk_low, n=1),digits = 3),", 97.5th perc = ",
                                            format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$bk_high, n=1),digits = 3)))})
    
    output$ModOutp_b5=renderUI({HTML(paste0(tags$b("Exploitation F/(r/2) in last year = "),format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$FFmsy, n=1),digits = 3),
                                            ", 2.5th perc = ",format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$FFmsy_low, n=1),digits = 3),", 97.5th perc = ",
                                            format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$FFmsy_high, n=1),digits = 3)))})
    
    output$ModOutp_b6=renderUI({HTML(paste0(tags$b("q= "),format(CLA_object_final()[["output"]][["output_posteriors"]]$q_post[1],digits = 3),
                                            ", ",tags$b("95% CL ="),format(CLA_object_final()[["output"]][["output_posteriors"]]$q_post[2],digits = 3), "-",
                                            format(CLA_object_final()[["output"]][["output_posteriors"]]$q_post[3],digits = 3)))})
    
    output$ModOutp_23=renderUI({
      if(CLA_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_correction_note[1]=="Fmsy was corrected downward to account for reduced recruitment."){
        HTML(paste0(tags$b("Fmsy ="),format(CLA_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1],digits = 3),
                    ", 95% CL =",format(CLA_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2],digits = 3),"-",
                    format(CLA_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3],digits = 3)," - Fmsy was corrected downward to account for reduced recruitment."))} else {
                      HTML(paste0(tags$b("Fmsy ="),format(CLA_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[1],digits = 3),
                                  ", 95% CL =",format(CLA_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[2],digits = 3),"-",
                                  format(CLA_object_final()[["output"]][["output_posteriors"]]$Fmsy_post_corrected[3],digits = 3)))
                    }
    })
    
    output$ModOutp_25=renderUI({
      HTML(paste0(tags$b("MSY ="),format(round(CLA_object_final()[["output"]][["output_posteriors"]]$MSY_post[1]),digits = 2),
                  ", 95% CL =",format(round(CLA_object_final()[["output"]][["output_posteriors"]]$MSY_post[2]),digits = 2),"-",
                  format(round(CLA_object_final()[["output"]][["output_posteriors"]]$MSY_post[3]),digits = 2)))#}
    })
    
    output$ModOutp_26=renderUI({
      HTML(paste0(tags$b("Bmsy ="),format(round(CLA_object_final()[["output"]][["output_posteriors"]]$Bmsy_post[1]),digits = 2),
                  ", 95% CL =",format(round(CLA_object_final()[["output"]][["output_posteriors"]]$Bmsy_post[2]),digits = 2),"-",
                  format(round(CLA_object_final()[["output"]][["output_posteriors"]]$Bmsy_post[3]),digits = 2)))#}
    })
    
    output$ModOutp_27=renderUI({
      HTML(paste0(tags$b("Biomass in last year ="),format(round(tail(CLA_object_final()[["output"]][["output_timeseries"]]$B,1)),digits = 2),
                  ", 2.5th perc =",format(round(tail(CLA_object_final()[["output"]][["output_timeseries"]]$B_low,1)),digits = 2),", 97.5 perc =",
                  format(round(tail(CLA_object_final()[["output"]][["output_timeseries"]]$B_high,1)),digits = 2)))#}
    })
    
    output$ModOutp_28=renderUI({
      HTML(paste0(tags$b("B/Bmsy in last year  ="),format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$BBmsy,1),digits = 3),
                  ", 2.5th perc =",format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$BBmsy_low,1),digits = 3),", 97.5 perc =",
                  format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$BBmsy_high,1),digits = 3)))#}
    })
    
    output$ModOutp_29=renderUI({
      HTML(paste0(tags$b("Fishing mortality in last year  ="),format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$f,1),digits = 3),
                  ", 2.5th perc =",format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$f_low,1),digits = 3),", 97.5 perc =",
                  format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$f_high,1),digits = 3)))#}
    })
    
    output$ModOutp_30=renderUI({
      HTML(paste0(tags$b("Exploitation F/Fmsy in last year  ="),format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$FFmsy,1),digits = 3),
                  ", 2.5th perc =",format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$FFmsy_low,1),digits = 3),", 97.5 perc =",
                  format(tail(CLA_object_final()[["output"]][["output_timeseries"]]$FFmsy_high,1),digits = 3)))#}
    }) 
    
    ############ Catch plot
    observeEvent(input$Start_run,{
      run_pictures$pic_B=ggcatch.plot(CLA_object_final(),METHOD = "BSM")
      pic_B_ready <- showNotification(paste("Message: ", "Catch graph ready"), duration = 5)
    })
    
    output$Catch_plot_final= shiny::renderPlot({
      run_pictures$pic_B})
    
    observeEvent(input$Start_run,{
      run_pictures$pic_C=ggbk.plot(CLA_object_final(),"BSM") 
      
      pic_C_ready <- showNotification(paste("Message: ", "B/k graph ready"), duration = 5)
    })
    
    output$Pred_biom_plot= shiny::renderPlot({
      run_pictures$pic_C
    })
    
    ###### FFmsy PLOT
    observeEvent(input$Start_run,{
      run_pictures$pic_D=ggFFmsy.plot(CLA_object_final(),"BSM") 
      pic_D_ready <- showNotification(paste("Message: ", "F/Fmsy graph ready"), duration = 5)
      
    })
    
    output$Pred_FFmsy_plot= shiny::renderPlot({
      run_pictures$pic_D
    })
    
    ###### PARABOLA PLOT
    observeEvent(input$Start_run,{
      run_pictures$pic_E=ggparabola.plot(CLA_object_final(),"BSM") 
      pic_E_ready <- showNotification(paste("Message: ", "Stock equillibrium graph ready"), duration = 5)
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

    ##### Kiel plot
    observeEvent(input$Start_run,{
      run_pictures$pic_M=ggkiel.plot(CLA_object_final(),"BSM") 
      pic_M_ready <- showNotification(paste("Message: ", "Kiel-plot ready"), duration = 5)
    })
    
    output$kiel_plot= shiny::renderPlot({
      run_pictures$pic_M
    })
    
    
    ###### KOBE PLOT
    observeEvent(input$Start_run,{
      # withProgress(message = 'Making kobe plot', value = 0, {
      #   # Number of times we'll go through the loop
      #   n <- 10
      #   
      #   for (i in 1:n) {
         run_pictures$pic_F=ggkobe.plot(CLA_object_final(),BSM_run() ,"BSM") 
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/n, detail = paste("Doing part", i))
      # 
      # # Pause for 0.1 seconds to simulate a long computation.
      # Sys.sleep(0.01)
      #   }
      # })
pic_F_ready <- showNotification(paste("Message: ", "Kobe plot graph ready"), duration = 5)
    })
    
    output$kobe_plot= shiny::renderPlot({
      run_pictures$pic_F
    })

    observeEvent(input$Start_run,{
      run_pictures$pic_G= ggmanagement.plot(CLA_object_final(),BSM_run(),"BSM")
      pic_G_ready <- showNotification(paste("Message: ", "Management graph ready"), duration = 5)
    })
    
    output$Alexandros5= shiny::renderPlot({
      run_pictures$pic_G
    })
    
    observeEvent(input$Start_run,{
      run_pictures$pic_H= ggprorposterior.plot(CLA_object_final(),BSM_run(),"BSM") 
      pic_G_ready <- showNotification(paste("Message: ", "Prior-Posterior graph ready"), duration = 5)
    })
    
    output$PriorPosterior <- shiny::renderPlot({
      run_pictures$pic_H
    })
    
    observeEvent(input$Start_run,{
      run_pictures$pic_I= ggpdiagnostics.plot(CLA_object_final(),BSM_run(),"BSM")
      pic_I_ready <- showNotification(paste("Message: ", "Diagnostics graph ready"), duration = 5)
    })
    
    output$Diagnostics.plot <- shiny::renderPlot({
      run_pictures$pic_I
    })
    
    CLA_retro=eventReactive(input$Retrospective,{
      req(CLA_object_final())
      xx= retro.fit(CLA_object_final(),input$retro_range,"BSM")
    })
    
    observeEvent(input$Retrospective,{
      req(CLA_retro())
      run_pictures$pic_J=ggretro.plot(CLA_retro())
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
      req(CLA_object_final())
      req( run_pictures$pic_A)
      CLA_object <- CLA_object_final()
        nm=object_NAME()
      dir.create(paste0(dir.name(),"/BSM_wd/outputs"))
      dir.create(paste0(dir.name(),"/BSM_wd/outputs/",nm))
      device_="png"
      
      ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","rk_pic."),device_),plot=run_pictures$pic_A, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","Catch_and_MSY."),device_),plot=run_pictures$pic_B, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","Stock_size."),device_),plot=run_pictures$pic_C, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","Exploitation_rate."),device_),plot=run_pictures$pic_D, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","Equilibrium_curve."),device_),plot=run_pictures$pic_E, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","Kobe_plot."),device_),plot=run_pictures$pic_F, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","Management_graphs."),device_),plot=run_pictures$pic_G, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","prior_posterior_distributions."),device_),plot=run_pictures$pic_H, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
      saveRDS(CLA_object,file =paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","data_compl_CLA_object_",gsub(" ", "_",CLA_object[["input"]][["Stock_info"]][["ScientificName"]]), "_",Sys.Date(), "_final.rds"))
        write.csv(cbind(CLA_object[["input"]][["Stock_info"]],CLA_object[["input"]][["Input_parameters"]]), paste0(dir.name(),"/BSM_wd/outputs/",nm,"/input_parameters.csv"), row.names = F)
      write.csv(CLA_object[["output"]][["output_timeseries"]], paste0(dir.name(),"/BSM_wd/outputs/",nm,"/output_timeseries.csv"), row.names = TRUE)
      write.csv(CLA_object[["output"]][["output_posteriors"]], paste0(dir.name(),"/BSM_wd/outputs/",nm,"/output_posteriors.csv"), row.names = TRUE)
      
      Save_done <- showNotification(paste("Message: ", "All the outcomes are saved in your working directory"), duration = 10)
      
    })
    
    observeEvent(input$Start_run, {
      device_="png"
      nm=object_NAME()
      #xxx= gg_summary.plot(CLA_object_final(),CLA_object_final()[["rk_object"]],BSM_run(),"BSM")
      run_pictures$pic_K= gg_summary.plot(CLA_object_final(),BSM_run(),"BSM")
          ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","Summary_pic."),device_),plot= run_pictures$pic_K, device =device_, width = 25, height =18, units = "cm",  dpi = 300)
      Save_done <- showNotification(paste("Message: ", "Summary outcomes are saved in your working directory"), duration = 5)
    })
    

    output$Summary_plot <- shiny::renderPlot({
      run_pictures$pic_K
    })

    observeEvent(input$Retrospective, {
      req( run_pictures$pic_J)
        nm=object_NAME()
      device_="png"
      ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","retrospective_pic."),device_),plot=run_pictures$pic_J, device =device_, width = 18, height =9, units = "cm",  dpi = 300)
      Save_done <- showNotification(paste("Message: ", "Retrospective outcomes are saved in your working directory"), duration = 5)
    })
    
    observe({
      req(CLA_object_final())
        nm=object_NAME()
      output$working_directory=renderUI({ shiny::h4(tagList("All your outcomes are stored in the directory: ",  tags$b(paste0(dir.name(),"/BSM_wd/outputs/",nm))))})
    })
    
    output$download_pic_A <- downloadHandler(
      filename = function(){
        paste("pic_",input$Run_select_pic,Sys.Date(),".", input$format_picA,sep="")},
      content = function(file) {
        ggplot2::ggsave(file,plot=run_pictures[[paste0("pic_",input$Run_select_pic)]],
                        device =  input$format_picA, width =  input$width_pic_A, height = input$height_pic_A, units = "cm",  dpi = as.integer(input$dpi_pic_A))
      })
    
    output$helpTablerk=renderTable({data.frame("Case"=c("(1)","(2)","(3)"),
                                               "Trend"=c("Stationary:","Upward trend or clockwise hooks or circles:", "Downward of counter-clockwise hooks or circles:"),
                                               "Explenation"=
                                                 c("biomass fluctuates around a mean, resulting in surplus production fluctuating around a point on the curve. If the point is below Bmsy, then catches should be reduced.",
                                                   "surplus production increased followed by stock increase followed by decrease in production and biomass. If it is a circle, then this is probably caused by extraordinary good recruitment and catches should not be increased because future recruitment will be less and biomass will decrease again. If it is a hook that stays up, then this is caused by a strong decrease in catch. Keeping the new catch level will keep biomass at the present level.",
                                                   "Downward of counter-clockwise hooks or circles: extraordinary recruitment caused temporary non-sustainable increase in surplus production that was fished out by strongly increased catches taking more that the increased surplus production and thus shrinking biomass. Catches should be strongly reduced.")
    )})
    
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
    
    CLA_FW=reactive({
      req(switch_on())
      if (input$Manual_scenarios==F) {
        CLA_FW_=CLA.forward(BSM_run(),CLA_object_final(),nyears=input$N_prj_years,status.quo_years=input$Sq_years,interim.quant =input$interim_par,quant =input$ForC,Manual_Scenarios=NULL )} else {
           scen_names= c(paste0("Scen_",seq(1,input$Scen_years,1)))
          mscen <- sapply(1:input$Scen_years, function(i) {
            as.numeric(input[[ scen_names[i]]])
          })
          CLA_FW_=CLA.forward(BSM_run(),CLA_object_final(),status.quo_years=input$Sq_years,interim.quant =input$interim_par,quant =input$ForC,Manual_Scenarios=mscen )
        }
      return(CLA_FW_)
    })
    
    CLA_FW <- CLA_FW %>% debounce(500)
    
    cla_fwd_summary= reactive({
      xx=interim.summary(CLA_FW())
      return(xx)
    })
    
    output$interim_text <-renderText({
      req(cla_fwd_summary())
      cla_fwd_summary()[["info"]]
    })
    
    output$interim_assumptions= renderTable({
      req(cla_fwd_summary())
      cla_fwd_summary()[["proj.summary"]]
    })
    
    cla_fwd_outcomes= reactive({
      xx=advice.summary(CLA_FW())
      return(xx)
    })

    output$advice_outcomes= renderTable({
      req(cla_fwd_outcomes())
      xx=cla_fwd_outcomes()
      return(xx)
    })
    
    ffmsy_forecast=reactive({
      req(CLA_FW())
   xx=ggforecast.FFMSY(CLA_FW(),CI=input$for_CI)
   return(xx)
    })
    
    output$FFMSY_forecast= shiny::renderPlot({
      req(CLA_FW())
      req(ffmsy_forecast())
      ffmsy_forecast()
    })
    
    bk_fwd=reactive({
      req(CLA_FW())
      xx=ggforecast.bk(CLA_object_final(),CLA_FW(),CI=input$for_CI)
      return(xx)
    })
    
    output$bk_forecast= shiny::renderPlot({
      req(CLA_FW())
      req(bk_fwd())
      bk_fwd()
    })
    
    catch_fwd=reactive({
      req(CLA_FW())
      xx=ggforecast.catch(CLA_FW(),CI=input$for_CI)
      return(xx)
    })

    output$catch_forecast= shiny::renderPlot({
      req(catch_fwd())
      catch_fwd()
    })

    observe({
      req(CLA_FW())
        nm=object_NAME()
       device_="png"
       ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","ffmsy_forecast."),device_),plot=ffmsy_forecast(), device =device_, width = 16, height =10, units = "cm",  dpi = 300)
       ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","bk_forecast."),device_),plot=bk_fwd(), device =device_, width = 16, height =10, units = "cm",  dpi = 300)
       ggsave(filename=paste0(paste0(dir.name(),"/BSM_wd/outputs/",nm,"/","catch_forecast."),device_),plot=catch_fwd(), device =device_, width = 16, height =10, units = "cm",  dpi = 300)
       write.csv(CLA_FW()[["forward_df"]], paste0(dir.name(),"/BSM_wd/outputs/",nm,"/output_FORWARDs.csv"), row.names = TRUE)
      Save_done <- showNotification(paste("Message: ", "The forecast outcomes are saved in your working directory"), duration = 10)
    })
    
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(obj = CLA_object_final(),
                       pic=    run_pictures$pic_K,
                       pic_F=    run_pictures$pic_F,
                       pic_I= run_pictures$pic_I
        )
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
  observe({
    if (input$quit == 1) stopApp()
  })
  
}

shinyApp(ui=shinyUI, server=shinyServer)
