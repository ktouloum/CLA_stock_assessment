
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
cinfo=readRDS("AMSY_cinfo.rds")
species_DB=readRDS("AMSY_species_DB.rds")

AMSY_DATA=readRDS("AMSY_data.rds")
Stock_names=names(AMSY_DATA)

linebreaks <- function(n){HTML(strrep(br(), n))}
options(digits=3) # displays all numbers with three significant digits as default
options(scipen=100) # displays all numbers with three significant digits as default

'%!in%'=Negate('%in%')
test_CPUE_ID=cinfo[,c("Stock", "ScientificName", "Resilience", "Bk.pr")] 

source("AMSY_aux_functions.R")

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
    tags$head(tags$script(HTML("$(document).on('click', '.needed', function () {
                                Shiny.onInputChange('last_btn',this.id);
                             });"))),
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
                                                                                                     no_outline=F,block=F,color="success",class="needed"),
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
                                      no_outline=F,block=F,color="success",class="needed"),
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
                                                                                         min = 1900, max = 2028, value = c(1950, 2028),step=1,sep = "")),
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
                                                                                         min = 1900, max = 2028, value = 2000,step=1,sep = ""),      
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
                                                                                         min = 1, max = 6, value =1, step =1)),
                                                            tags$b("Add minimum realistic CV for CPUE"),
                                                            shinyWidgets::prettySwitch(
                                                              inputId = "upMaterialbcv",
                                                              slim = T,
                                                              label = "Edit",
                                                              value = FALSE,status = "info" ),
                                                            conditionalPanel(condition = "input.upMaterialbcv",
                                                                             sliderInput(inputId = "CPUE_CV", label = "",
                                                                                         min = 0.05, max = 0.5, value =0.3, step =0.05))
                                                              ,width =12))
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
                                                                                                 width=12))
                                                            ,width=4))),
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
                                                                                         min = 1900, max = 2028, value = 2020,step=1,sep = ""),
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
                                                            width=12))),
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
                                 h3("Select single B/k prior to be used by AMSY"))),
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
                                                                                                  min = 1900, max = 2028, value = 2000,step=1,sep = ""),      
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
  created_stock_object=eventReactive(input$button_2,{
    if(input$Crtd_StckID_input=="") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Input a stock ID!",
        type = "error")
    }  else {
      if(input$button_2 > 0) {
        Creatd_Stck_obj=AMSY_stock_maker_empty_shiny(inp_data(),input$Crtd_StckID_input,input$Yr_col,input$CPUE_col,cvcol=NA,do_u_have_cv="No")
        Creatd_Stck_obj$ID$Ecreep_YN=FALSE
        Creatd_Stck_obj$ID$Smooth_YN=FALSE
        Creatd_Stck_obj$ID$Species=input$Sp_Scient
        Creatd_Stck_obj$ID$Common_name =input$Sp_Comm
        Creatd_Stck_obj$ID$Comments=input$Stock_comment
        Creatd_Stck_obj$ID$Start_yr =min(as.integer(inp_data()[,input$Yr_col]),na.rm = T)
        Creatd_Stck_obj$ID$End_yr =max(as.integer(inp_data()[,input$Yr_col]),na.rm = T)
      }
    }
    return(Creatd_Stck_obj)
  })
  
   output$Stock_infobox_1=shinydashboard::renderValueBox({
    shinydashboard::valueBox(shiny::h4("Success!"),shiny::h5(HTML(paste0("You have created the stock object ", input$Crtd_StckID_input,br(),
                                                                         " of the species ",tags$em(input$Sp_Scient), ".",br(),
                                                                         "You can know proceed to the 'Prepare data' tab."))),
                             shiny::icon("thumbs-up", lib = "glyphicon"),
                             color = "green") })


  output$Biomass_plot2= shiny::renderPlot({
    ggplot2::ggplot(data=created_stock_object()$DATA, ggplot2::aes(x=yr, y=bt )) +
      ggplot2::geom_line(color="blue",linewidth=1)+
      ggplot2::labs(y="CPUE", x="Year")+
      ggplot2::geom_point(color="black",size=2)+
      ggplot2::theme_classic()+
      ggplot2::scale_y_continuous(limits = c(0,1.1*max(created_stock_object()$DATA$bt,na.rm = T)))+
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
      need(input$txt1 %in% Stock_names, 'Choose a valid Stock from the last column of the above Data Table')
    )
    dat=AMSY_DATA[[which(names(AMSY_DATA)==input$txt1)]]$DATA
    p2= ggplot2::ggplot(data= dat,ggplot2::aes(x=yr, y=bt, group=1)) +
      ggplot2::geom_line(color="blue",size=1)+
      ggplot2::labs(y="CPUE", x="Year")+
      ggplot2::geom_point(color="black",size=2)+
      ggplot2::theme_classic()+
      ggplot2::scale_y_continuous(limits = c(0,1.1*max(dat$bt,na.rm=T)))+
      ggplot2::theme(text = ggplot2::element_text(size = 16))   
    p2
  }, height = 300)

  Selected_stock_object=eventReactive(input$button_1,{
    if(input$button_1 > 0) {
      if(input$txt1 %!in% Stock_names) {
        shinyWidgets::sendSweetAlert(
          session = session,title = "Error...",
          text = "Oups !", type = "error")
      } else {
        sel_stock_obj=AMSY_DATA[[which(names(AMSY_DATA)==input$txt1)]]
      }
    }
    sel_stock_obj$ID$Ecreep_YN[is.na(sel_stock_obj$ID$Ecreep_YN)]=FALSE
    sel_stock_obj$ID$Smooth_YN[is.na(sel_stock_obj$ID$Smooth_YN)]=FALSE
    return(sel_stock_obj)
  })

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
  
  #########SELECT STOCK TO WORK WITH PRIORS
  Final_stock=eventReactive(input$prepare_data,{
    if (input$last_btn=="button_1") {
      fs=Selected_stock_object()
    } else if (input$last_btn=="button_2") {
      fs=created_stock_object()
    }# else if (input$last_btn=="button_3" & isTruthy(input$file_rds)) {
     # fs=loaded_stock_object()
   # }
    return(fs)
  })
  
  output$Stock_infobox_prepare=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Stock object"),shiny::h5(HTML(paste0("You have selected the stock ",
                                                      tags$b(Final_stock()[["ID"]][1,"Stock"]), " of the species ",
                                                      tags$b(tags$em(Final_stock()[["ID"]][1,"Species"]))
      ))),
      icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "green") })
  
  
  observe({
    req(Final_stock())
    start.yr =Final_stock()[["DATA"]][c("yr","bt")]$yr[which(Final_stock()[["DATA"]][c("yr","bt")]$bt>0)[1]]
    end.yr    =max(as.integer(Final_stock()[["DATA"]]$yr),na.rm = T)
    updateSliderInput(session, "CPUE_yr_slct", value = c(start.yr,end.yr),min = start.yr, max =end.yr)
  })
  
  #url_palom= a("Link", href="https://doi.org/10.5751/ES-11136-240331")
  output$help_ecreep=renderUI({ shiny::h5(tagList("Note: Over time, fishers become more efficient at catching fish; a 2% increase in catchabilty per year is common (Palomares and Pauly 2019), https://doi.org/10.5751/ES-11136-240331"))})
 
  observe({
    req(Final_stock())
    xxxx=ifelse(is.na(Final_stock()[["ID"]]$Start_yr_ecreep),input$CPUE_yr_slct[1],Final_stock()[["ID"]]$Start_yr_ecreep)
    updateSliderInput(session, "ecreep_year", value =xxxx,min =input$CPUE_yr_slct[1], max =input$CPUE_yr_slct[2]-1)
  })
  
  CPUE_obj=reactive({
    req(Final_stock())
    inp= Final_stock()[["DATA"]][,c("yr","bt")] ############## SEE WHEN CHANGE OBJECT
   # colnames(inp)=c("yr","bt")
    temp_bio_obj =Bio_obj(inp,start_yr=input$CPUE_yr_slct[1], end_yr=input$CPUE_yr_slct[2],smoother_bw=input$bw_inpt,ecreep=input$ecreepyes,ecreep_yr=input$ecreep_year,ecreepvalue=input$ecreepslider,Biom_type="CPUE",CPUE_CV=input$CPUE_CV,Plot=F,ShowCV=input$upMaterialbcv) 
    return(temp_bio_obj)
  })

  d.cpue.raw=reactive({
    req(CPUE_obj())
    cpue_obj=CPUE_obj()[["bt_data"]]
    nyr=length(cpue_obj$yr)
    d.cpue.raw_   <- max(diff( cpue_obj$bt_smthd[!is.na(cpue_obj$bt_smthd)] )/ cpue_obj$bt_smthd[!is.na(cpue_obj$bt_smthd)][1:(nyr-1)],na.rm = T)
    return(d.cpue.raw_)
  })
  
  observe({
    if (d.cpue.raw()>1.5) {
      output$smooth_recom=renderUI({ shiny::h5(HTML(paste0( "<font color=\"#cc0000\"><b>",tags$b("Smoothing is recommended for this stock!"))))})} else {
        output$smooth_recom=renderUI({ shiny::h5(HTML(paste0( "")))})
      }
  })

  output$Check_pdata_plot= shiny::renderPlot({
    validate(need(!all(is.na(CPUE_obj()$bt_data$bt_smthd)), 'No CPUE data available'))
    req(CPUE_obj())
    print(CPUE_obj()$pic)
  })

  output$When_happy=shinydashboard::renderValueBox({
    shinydashboard::valueBox( shiny::h4("Message"),shiny::h4("When you are happy with data preparation, move on to the '3. Priors' tab. "),
                              icon = shiny::icon("envelope"),color = "green")})
  
  # ################################################################
  # ###################### PAGE 5 PRIOR ENTRY  #####################
  # ################################################################
  # 
  #########SELECT STOCK TO WORK WITH PRIORS
  observeEvent(input$button_1,{   #TRICK to erase things
    shinyWidgets::updatePrettySwitch(session, "Acc_FBpriors", value = T)
    shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
    shinyWidgets::updateMaterialSwitch(session, "go_to_d", value = F)
   # reset("FB_resil")
    })
  
  observeEvent(input$button_2,{   #TRICK to erase things
    shinyWidgets::updatePrettySwitch(session, "Acc_FBpriors", value = T)
    shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
    shinyWidgets::updateMaterialSwitch(session, "go_to_d", value = F)
   # reset("FB_resil")
     })

  observeEvent(input$procc_rpriors,{   #TRICK to erase things
    shinyWidgets::updateMaterialSwitch(session, "Acc_rpriors", value = F)
  }) 

    output$Stock_infobox_patience=shinydashboard::renderValueBox({
      shinydashboard::valueBox( shiny::h4("Start priors processing"),shiny::h5("Press 'Start' to enter resilience and r priors. You will also have the option to press 'Connect' button to connect to FishBase or SeaLifeBase
                                                                             for extraction of prior information. You will then have the 
                                                                             option to edit that information. Be patient since the 
                                                                             connection may take a moment. 
                                                                             Don't forget to press the 'Save the priors'
                                                                             button when you're done."),
                              icon = shiny::icon("envelope"),color = "green")})

  output$Stock_infobox_3=shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      shiny::h4("Stock object"),shiny::h5(HTML(paste0("You have selected the stock ",
                                                      tags$b(Final_stock()[["ID"]][1,"Stock"]), " of the species ",
                                                      tags$b(tags$em(Final_stock()[["ID"]][1,"Species"]))
      ))),
      icon = shiny::icon("thumbs-up", lib = "glyphicon"),color = "green") })

  Fishbase_text=eventReactive(input$con_fbase,{
    req(Final_stock())
    results= fbsb(Final_stock()[["ID"]][1,"Species"])
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
        shiny::h4("FishBase info"),tagList(HTML(paste0("According to FishBase/SeaLifeBase, ", tags$em(Final_stock()[["ID"]][1,"ScientificName"]), " has prior r range:", tags$b(Fishbase_text()[1]),tags$br(), "<font color=\"#cc0000\"><b>"," For more info visit: ",Fishbase_text()[6], "</b></font>",tags$br(), "Since no information on resilience is available in FishBase/SeaLifeBase, using Medium resilience for 'regular' species and Low resilience for large or deep-water species is a reasonable starting point. Very low resilience is found in very large species and High resilience is found in species which reproduce in their first year." ))),
        icon = shiny::icon("info-circle"),
        color = "aqua") } else {
          shinydashboard::valueBox(
            shiny::h4("FishBase info"),tagList(HTML(paste0("According to FishBase/SeaLifeBase, ", tags$em(Final_stock()[["ID"]][1,"ScientificName"]), " has "," Resilience: ",
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
    updateSliderInput(session, "man_Bk_int_year", value =as.integer(floor((min(as.integer(CPUE_obj()[["bt_data"]]$yr),na.rm = T)+max(as.integer(CPUE_obj()[["bt_data"]]$yr),na.rm = T))/2)),min = min(CPUE_obj()[["bt_data"]]$yr), max =max(CPUE_obj()[["bt_data"]]$yr))
      })
  
  observe({
    start.yr =min(as.integer(CPUE_obj()[["bt_data"]]$yr),na.rm = T)
    end.yr    =max(as.integer(CPUE_obj()[["bt_data"]]$yr),na.rm = T)
    updateSliderInput(session, "bk_priors_year", value =start.yr,min =start.yr, max =end.yr)
  })
  
  #LBB_data <- LBB_data %>% debounce(1000)

  output$Check_priors_plot= shiny::renderPlot({
    validate(
      need(isTruthy(input$resilience_in), "You han't set resilience for the stock neither FishBase/SeaLifeBase provided this information. Press 'Change r prior' and select a 'resilience' option from the dropdown list to be able to continue.")
    )
    req(CPUE_obj())
      start_yr =min(as.integer(CPUE_obj()[["bt_data"]]$yr),na.rm = T)
    end_yr    =max(as.integer(CPUE_obj()[["bt_data"]]$yr),na.rm = T)
    
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
       ffpriors=c(input$man_Bk_start,min(CPUE_obj()[["bt_data"]]$yr))} else if (input$select_123=="Second") {
         ffpriors=c(input$man_Bk_int,input$man_Bk_int_year)} else if (input$select_123=="Third") {
           ffpriors=c(input$man_Bk_end,max(CPUE_obj()[["bt_data"]]$yr))}
   } else  {
     ffpriors=c(as.numeric(bk_priors_used()[c(2,3)]),input$bk_priors_year)
       }
       return(ffpriors)
   })
 
# Deside_on_Bkpriors <- Deside_on_Bkpriors %>% debounce(1000)
 prior.kq=reactive({
   req(CPUE_obj())
   req(Deside_on_Bkpriors())
   CPUE_df=CPUE_obj()[["bt_data"]]
   CPUE_df=CPUE_df[,c("yr", "bt_smthd")]
   
   Bk.yr=Deside_on_Bkpriors()[3]
   Bk_low=Deside_on_Bkpriors()[1]
   Bk_high=Deside_on_Bkpriors()[2]
   prior.kq_= prior_kq(CPUE_df,Bk.yr,Bk_low,Bk_high)
 })
 
 mvnlogrk=eventReactive(input$Save_priors==T,{
   n.p          <- 50000 # number of r-kq pairs to be analysed; will be doubled if too few viable pairs are found
   n.trial      <- 30 # times each year is calculated with new random error terms for r and cpue
   req(prior.kq())
   req(final_rpriors())
   r.low=final_rpriors()[1]
   r.high=final_rpriors()[2]
   prior.kq=prior.kq()
   
   mvn.log.rk_= mvnlog.rk(r.low,r.high,prior.kq,n.p=n.p,n.trial=n.trial) 
   return(mvn.log.rk_)
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
    nyr=length(CPUE_obj()[["bt_data"]]$yr)
#d.cpue.raw   <- max(diff( CPUE_obj()$CPUE[!is.na(CPUE_obj()$CPUE)] )/ CPUE_obj()$CPUE[!is.na(CPUE_obj()$CPUE)][1:(nyr-1)],na.rm = T)

my_y_title <-bquote(atop(CPUE~and~expert~priors~"for"~the~stock~bold(.(Final_stock()[["ID"]][1,"Stock"]))~of~italic(.(Final_stock()[["ID"]][1,"Species"]))))

start_yr =min(as.integer(CPUE_obj()[["bt_data"]]$yr),na.rm = T)
end_yr    =max(as.integer(CPUE_obj()[["bt_data"]]$yr),na.rm = T)

if (input$Id004=="Select one of the B/k priors") {
  if(input$select_123=="First") {
    start_f=mean(input$man_Bk_start)
    div_f=CPUE_obj()[["bt_data"]]$bt_smthd[1]/start_f
    } else if (input$select_123=="Second") {
      start_f=mean(input$man_Bk_int)
      div_f=CPUE_obj()[["bt_data"]]$bt_smthd[CPUE_obj()[["bt_data"]]$yr==Deside_on_Bkpriors()[3]]/start_f} else if (input$select_123=="Third") {
        start_f=mean(input$man_Bk_end)
        div_f=CPUE_obj()[["bt_data"]]$bt_smthd[CPUE_obj()[["bt_data"]]$yr==Deside_on_Bkpriors()[3]]/start_f}
} else  {
  start_f=mean( Deside_on_Bkpriors()[c(1,2)])
  div_f=CPUE_obj()[["bt_data"]]$bt_smthd[CPUE_obj()[["bt_data"]]$yr==Deside_on_Bkpriors()[3]]/start_f
}

start=div_f*input$man_Bk_start
ind=input$man_Bk_int*div_f
end=input$man_Bk_end*div_f

desided= Deside_on_Bkpriors()[c(1,2)]*div_f
temp=data.frame(yr=c(start_yr,input$man_Bk_int_year,end_yr),ln=c(mean(start),mean(ind),mean(end)))
max.y=max(1.05*c(CPUE_obj()[["bt_data"]]$bt_smthd,start,ind,end),na.rm = T)

   p_1=ggplot2::ggplot()+
    ggplot2::geom_line(data=CPUE_obj()[["bt_data"]],ggplot2::aes(x=yr,y=bt_smthd,color="A"),size=1)+
    ggplot2::geom_point(data=CPUE_obj()[["bt_data"]],ggplot2::aes(x=yr,y=bt_smthd),fill="#F8766D",shape=21,size=2)+
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

 output$param_sofar <-renderText({paste("Parameterization so far for the stock",  tags$b(Final_stock()[["ID"]][1,"Stock"]), "of",  tags$em(Final_stock()[["ID"]][1,"Species"]))
 })
 
 AMSY_object=eventReactive(input$Save_priors,{
   req(CPUE_obj())
   req(Final_stock())
   req(mvnlogrk())
   req(final_rpriors())
   
   if (isTruthy(input$con_fbase)) {
     FBSLB=Fishbase_text()
   } else {
     FBSLB=NULL
   }
   
   AMSY.object=  AMSY_stkobj_maker(Final_stock()[["ID"]]$Stock,Final_stock()[["ID"]]$Species,
                                   Final_stock()[["ID"]]$Common_name,
                                   CPUE_obj(),mvnlogrk(),temp_rpriors()[1],final_rpriors(),Deside_on_Bkpriors()[3],
                                   Deside_on_Bkpriors()[1],Deside_on_Bkpriors()[2],prior.kq(),fbslb=FBSLB,
                                   Final_stock()[["ID"]]$Comments)
   return(AMSY.object)
 })

 middle_outplts=eventReactive(input$Save_priors==T,{
 #  req(AMSY_object())
   AMSY.object=AMSY_object()
   outpts_m=data.frame(value=c(paste0(AMSY.object[["ID"]]$Start_yr, "-", AMSY.object[["ID"]]$End_yr),
                               paste0(format(round(min(AMSY.object[["DATA"]]$bt_smthd),2),digits = 3), "-", format(round(max(AMSY.object[["DATA"]]$bt_smthd),2),digits = 3)),
                               AMSY.object[["ID"]]$Smooth_YN,
                               paste0(input$ecreepyes, ifelse(input$ecreepyes==T, paste(", value=", AMSY.object[["ID"]]$Ecreep, "%"),"")),
                             #  paste0(format(AMSY.object[["ID"]]$Resilience,digits = 3)),
                               paste0(format(AMSY.object[["ID"]]$r.low,digits = 3), "-",format(AMSY.object[["ID"]]$r.hi,digits = 3)),
                               paste0(format(round(quantile(exp(AMSY.object[["mvnlogrk"]][,1]),0.01),2),digits = 3),"-",format(round(quantile(exp(AMSY.object[["mvnlogrk"]][,1]),0.99),2),digits = 3)),
                               paste0(format(2*as.numeric(AMSY.object[["ID"]]$bk_prior_low),digits = 3),"-",format(2*as.numeric( AMSY.object[["ID"]]$bk_prior_high),digits = 3)),
                               # paste0(format(bk_priors_used()[1],digits = 3)),
                               paste0(format(AMSY.object[["ID"]]$bk_prior_low,digits = 3),"-",format(AMSY.object[["ID"]]$bk_prior_high,digits = 3)),
                               paste0(format(round(AMSY.object[["ID"]]$prior.kq.low,2),digits = 3),"-",format(round( AMSY.object[["ID"]]$prior.kq.hi,2),digits = 3))))
   
   row.names(outpts_m)=c("CPUE data for years","CPUE range",
                         "Smooth","Ecreep",
                       #  "Resilience",
                         "Prior for r",
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
     dr="."  }
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
   updateTextInput(session, "obj.name",value=paste0(Final_stock()[["ID"]]$Stock, "_V1") ) #, placeholder=paste0("e.g.  ",Final_stock()[["Catch_ID"]]$Stock, "_V1")
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
 
   observeEvent(input$Save_priors,{
     output$Zavarakatranemia=renderUI({ shiny::h3(tagList("Run the model and get the results for the stock ",  tags$b(AMSY_object()[["ID"]]$Stock), "of",  tags$em(Final_stock()[["ID"]]$Species)))})
   })
 
  output$Run_infobox_patient=shinydashboard::renderValueBox({
    shinydashboard::valueBox( shiny::h4("Run the model"),shiny::h5("Press 'Start' button to run the model.
              It takes some time to do all the necessary calculations, so be patient!"),
                              icon = shiny::icon("envelope"),
                              color = "green")
  })

    SchaeferCPUE_run=eventReactive(input$Start_run,{
    req(AMSY_object())
    amsyrun=AMSY.run(AMSY_object())
  })

    SchaeferCPUE_products_A=eventReactive(input$Start_run,{
      req(AMSY_object())
      req(SchaeferCPUE_run())
      prds=AMSY.products(AMSY_object(),SchaeferCPUE_run())
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
                         year=c("","(in cpue units)","(in cpue units)","","",paste0("(",as.character(SchaeferCPUE_products_A()[["DATA"]]$yr[length(SchaeferCPUE_products_A()[["DATA"]]$yr)-1]),")"),paste0("(",as.character(SchaeferCPUE_products_A()[["DATA"]]$yr[length(SchaeferCPUE_products_A()[["DATA"]]$yr)]),")"))
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
    # if (input$Id049=="A") {
    nm=object_NAME()#} else if (input$Id049=="B") {
    #    nm=input$Id081}
    dir.create(paste0(dir.name(),"/AMSY/outputs"))
    dir.create(paste0(dir.name(),"/AMSY/outputs/",nm))
    device_="tiff"
      cdat_=AMSY_object[["DATA"]]
      yr=cdat_$yr
      nyr=length(yr)
      cqt_=AMSY_object[["Outputs"]][["cqt_"]]
      outp=AMSY_object[["Outputs"]][["outp_1"]]
      ribb_min1=cqt_$cqt.lcl[2:(nyr-1)]/outp$MSYq.est
      obj_1_ribbon=data.frame(Year=yr[2:(nyr-1)],Catch_MSY_low = ribb_min1,Catch_MSY_high = cqt_$cqt.ucl[2:(nyr-1)]/outp$MSYq.est)
      obj_1_data=data.frame(Year=yr[1:(nyr-1)],Catch_MSY = cqt_$cqt.median[1:(nyr-1)]/outp$MSYq.est)

      FFmsy_=AMSY_object[["Outputs"]][["FFmsy_"]]
      max.y <- max(c(1.2,FFmsy_$FFmsy.ucl),na.rm=T)
      ribb_min2=FFmsy_$FFmsy.lcl[2:(nyr-1)]

      obj_2_ribbon=data.frame(Year=yr[2:(nyr-1)],FFmsy_low = ribb_min2,FFmsy_high = FFmsy_$FFmsy.ucl[2:(nyr-1)])
      obj_2_data=data.frame(Year=yr[1:(nyr-1)],FFmsy = FFmsy_$FFmsy)

      cpuet_=AMSY_object[["Outputs"]][["cpuet_"]]
      outp=AMSY_object[["Outputs"]][["outp_1"]]

      Bkt<- cpuet_$cpuet.median/(0.5*outp$kqv.est)
      ribb_min3=cpuet_$cpuet.lcl[2:nyr]/(0.5*outp$kqv.est)
      obj_3_ribbon=data.frame(Year=yr[2:(nyr)],BBmsy_low = ribb_min3,BBmsy_high = cpuet_$cpuet.ucl[2:nyr]/(0.5*outp$kqv.est))
      obj_3_data=data.frame(Year=yr,BBmsy = Bkt)
      df_list <- list(obj_1_ribbon, obj_1_data, obj_2_ribbon,obj_2_data,obj_3_ribbon,obj_3_data)
      #merge all data frames together
      df_fin=Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
      df_fin=cbind(df_fin,cdat_[,2:ncol(cdat_)])
      df_fin$Stock=AMSY_object[["ID"]]$Stock
      df_fin=df_fin[,c("Year","Stock","bt","bt_iterp","bt_smthd","ecreep",	"bt_cv","Catch_MSY","Catch_MSY_low",	"Catch_MSY_high","FFmsy","FFmsy_low",	"FFmsy_high","BBmsy",	"BBmsy_low",	"BBmsy_high")]
     colnames(df_fin)=c("Year","Stock","CPUE_Raw","CPUE_interpolated","CPUE_smoothed",	"ecreep",	"CPUE_cv","Catch_MSY","Catch_MSY_low",	"Catch_MSY_high","FFmsy","FFmsy_low",	"FFmsy_high","BBmsy",	"BBmsy_low",	"BBmsy_high")
     
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","rk_pic."),device_),plot=run_pictures$pic_A, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","Relative_Catch."),device_),plot=run_pictures$pic_B, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","FFmsy_plot."),device_),plot=run_pictures$pic_C, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","BBmsy_plot."),device_),plot=run_pictures$pic_D, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","Kobe_plot."),device_),plot=run_pictures$pic_E, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    # ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","Kobe_plot."),device_),plot=run_pictures$pic_F, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    # ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","Management_graphs."),device_),plot=run_pictures$pic_G, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    # ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","prior_posterior_distributions."),device_),plot=run_pictures$pic_H, device =device_, width = 16, height =10, units = "cm",  dpi = 300)
    save(AMSY_object,  file =paste0(dir.name(),"/AMSY/outputs/",nm,"/","AMSY_obj_",gsub(" ", "_",AMSY_object[["ID"]][["Stock"]]), "_",Sys.Date(), ".RData"))
    write.csv(AMSY_object[["DATA"]], paste0(dir.name(),"/AMSY/outputs/",nm,"/input_timeseries.csv"), row.names = TRUE)
    write.csv(AMSY_object[["Outputs"]][["outp_1"]], paste0(dir.name(),"/AMSY/outputs/",nm,"/outputs.csv"), row.names = TRUE)
    write.csv(df_fin, paste0(dir.name(),"/AMSY/outputs/",nm,"/output_timeseries.csv"), row.names = TRUE)
    # write.csv(AMSY_object[["Outputs"]][["cpuet_"]], paste0(dir.name(),"/AMSY/outputs/",nm,"/cpuet.csv"), row.names = TRUE)
    # write.csv(AMSY_object[["Outputs"]][["cqt_"]], paste0(dir.name(),"/AMSY/outputs/",nm,"/cqt.csv"), row.names = TRUE)
    Save_done <- showNotification(paste("Message: ", "All the outcomes are saved in your working directory"), duration = 10)
  })
  
  observeEvent(input$Retrospective, {
    req( run_pictures$pic_F)
    # if (input$Id049=="A") {
      nm=object_NAME()#} else if (input$Id049=="B") {
    #    nm=input$Id081}
    device_="tiff"
    ggsave(filename=paste0(paste0(dir.name(),"/AMSY/outputs/",nm,"/","retrospective_pic."),device_),plot=run_pictures$pic_F, device =device_, width = 18, height =9, units = "cm",  dpi = 300)
    Save_done <- showNotification(paste("Message: ", "Retrospective outcomes are saved in your working directory"), duration = 5)
  })

  observe({
    req(SchaeferCPUE_products_A())
    # if (input$Id049=="A") {
    nm=object_NAME()#} else if (input$Id049=="B") {
    #    nm=input$Id081}
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