
pkgTest <- function(x){
  if (!require(x,character.only = TRUE)){
    install.packages(x,dep=TRUE)
  }
}
pkgTest("shiny")
library(shiny)
pkgTest("DT")
library(DT)
pkgTest("deSolve")
library(deSolve)
pkgTest("TTR")
library(TTR)
pkgTest("forecast")
library(forecast)
pkgTest("xlsx")
library(xlsx)
pkgTest("plotly")
library(plotly)
pkgTest("ggthemes")
library(ggthemes)
pkgTest("ggplot2")
library(ggplot2)
pkgTest("cowplot")
library(cowplot)
pkgTest("shinyBS")
library(shinyBS)
pkgTest("shinyjs")
library(shinyjs)
pkgTest("dplyr")
library(dplyr)
pkgTest("magrittr")
library(magrittr)

server <- function(input,output){

  #Primer botÃ³n
  observeEvent(input$boton1,{
    #Quita la interfaz de introducciÃ³n
    removeUI(selector =  '#placeholder')
    
    #Cargar los valores que necesito para el checkbox
    nombresBiolo = (cargarDatos$bioloGeneral[,1])
    
    
    #Se crea la interfaz a insertar
    interfaceInsertar <- div(id = 'placeholder2',theme = shinytheme("lumen"),
                             fluidRow(h2(strong("Model parametrization",style = "font-family: 'Source Sans Pro';
                                                color: #FFFF; 
                                                padding: 20px;
                                                text-align: center"),align = "center")),
                             hr(),
                             verticalLayout(
                               #GÃ©neros
                               div(fluidRow(
                                 column(4, wellPanel(style="background-color: #DCF2E7",h4(strong("Gender")), hr(), "This model considers two genders. However, you may analyze only one of them if you want.")),
                                 column(2, fluidRow(wellPanel(h4(strong("Types:")),hr(),h5("Male"), h5("Female"),style="height:170px;background-color: #E6F1F9"))),
                                 column(6, wellPanel(style="background-color: #F6F3EC",h4(strong("Description")),hr(),"Since our goal is to create an epidemiological compartmentalized model, we first divide the population according to gender, age group and biological compartment."))  
                               ), style="padding-left: 35px;padding-right: 35px;"),
                               hr(),
                               #Rangos de edad
                               div(fluidRow(
                                 column(3, wellPanel(style="background-color: #F6F3EC",h4(strong("Age Group")), hr(), "This model considers different types of age groups.")),
                                 column(style="padding-left: 3px;padding-right: 3px",3, wellPanel(style = "background-color: #DCF2E7",fluidRow(column(3,br(),h5("Min"),style = "height:42px"),column(8,numericInput(inputId = "minUsuario", label = "",value = 1, min = 0),style = "height:42px")),
                                                                                                  fluidRow(column(3,br(),h5("Max"),style = "height:42px"),column(8, numericInput(inputId = "maxUsuario", label = "",value = 1, min = 0),style = "height:42px")),
                                                                                                  fluidRow(column(3,br(),h5("Step"),style = "height:46px"),column(8, numericInput(inputId = "stepUsuario", label = "",value = 1, min = 0),style = "height:46px"))
                                 )),
                                 column(6, wellPanel(style="background-color: #E6F1F9",h4(strong("Description")),hr(),"Divisions by age range are the modeler's decision. For this, it is necessary to choose the minimum and maximum value of age and the size of the age range."))  
                               ), style="padding-left: 35px;padding-right: 35px;"),
                               hr(),
                               #Compartimientos biolÃ³gicos
                               div(fluidRow(
                                 column(3, wellPanel(style="background-color: #E6F1F9",h4(strong("Biological Compartment")), hr(), "This model considers different types of biological compartments.")),
                                 column(style="padding-left: 3px;padding-right: 3px",3, wellPanel(style = "background-color: #F6F3EC",checkboxGroupInput(inputId = "bioUsuario",label = "",choices = nombresBiolo)
                                 )),
                                 column(6, wellPanel(style="background-color: #DCF2E7",h4(strong("Description")),hr(),"Biological Compartments in wich the population is divided is a modele's decision. For this, it is necessary to choose al of the biological compartments that are considered in the model."))  
                               ), style="padding-left: 35px;padding-right: 35px;"),
                               br(),
                               div(id='interfaceBoton2',column(1,actionButton(inputId = "boton2", label = "Previous")),column(1,actionButton(inputId = "boton3", label = "Next"),offset=10))
                               
                             )
                             )
    
    
    #Agrega la interfaz que creÃ© arriba
    insertUI(selector = '#placeholder1', 
             ui = interfaceInsertar, 
             where = "afterEnd")
  })
  
  #Segundo BotÃ³n
  observeEvent(input$boton2,{
    
    #Quita la interfaz de parÃ¡metros
    removeUI(selector =  '#placeholder2')
    
    #Inserta la interfaz de introducciÃ³n
    
    intro <- div(id = 'placeholder',
                 fluidRow(h2(strong("Automated Evaluation of Public Health Policies for Cervical Cancer Prevention and Surveillance Tool",style = "font-family: 'Source Sans Pro';
                                color: #FFFF; 
                                padding: 20px;
                                text-align: center"),align = "center")),
                 hr(),
                 sidebarLayout(
                   sidebarPanel(style="background-color: #DCF2E7",
                                h3(strong("Authors",style = "color: #FFFF")),hr(),h4("Daniela Angulo"),br(),
                                h5("Master's degree in Industrial Engineering from the Universidad de Los Andes, with an emphasis on Operations Research. This tool is part of my master's degree project called cost effectiveness analysis for public policies against cervical cancer in Colombia. This work was developed together with Professor Ivan Mura."),
                                h5("E-mail: kd.angulo2295@uniandes.edu.co"),
                                br(),
                                br()
                   ),
                   mainPanel(wellPanel(style="background-color: #DCF2E7",
                                       h3(strong("Introduction")),hr(),"Cervical cancer (CC) is the second leading cause of cancer-related deaths among women, caused most commonly by Human Papillomavirus (HPV) infection.
                        The proper definition of public health policies for prevention and surveillance is of paramount importance to ensure cost-effective disease control strategies are deployed, which make the best usage of the limited available resources. However, predicting the long-term effects of vaccination and screening programs is no trivial at all, as it builds upon the ability
                                       to compound the uncertainties  associated with the results of interventions on a growing population such as the Colombian one. We propose a compartmentalized epidemiological simulation model based on differential equations, which represents population dynamics, HPV transmission within the population, likelihood of infection clearance, virus induced appearance of precancerous 
                                       lesions and eventually of CC, as well as the immunity gained with vaccination and the likelihood of early detection provided by screening policies. The model is implemented into this open software tool that allows evaluating the predicted effects of public health policies against HPV."))
                   
                   ),column(1,actionButton(inputId = "boton1", label = "Next"),offset=11)
                 ,style="padding-left: 35px;",style="padding-right: 35px;")
    
    insertUI(selector = '#placeholder1', 
             ui = intro)
    
  })

  #Tercer botÃ³n
  observeEvent(input$boton3,{
    #Quita la interfaz de parÃ¡metros
  
    if(input$maxUsuario < input$minUsuario || input$stepUsuario > (input$maxUsuario-input$minUsuario)){
      showModal(modalDialog(title = "Model parametrization error",paste("Please select a maximum age range that is greater or equal to the minimum age range.", "Please select a step that is less or equal to the number of age ranges."),easyClose = TRUE,footer = NULL))
    }
    validate(
      need(input$maxUsuario >= input$minUsuario ,""),
      need(input$stepUsuario <= (input$maxUsuario-input$minUsuario), "")
    )
    
    
    removeUI(selector =  '#placeholder2')
    
    #Cargar los valores que necesito para el checkbox
    nombresBiolo <- (cargarDatos$bioloGeneral[,1]) 
    nombresBiolo[transM+1] <- "Vaccinated"
    nombresBiolo[transM+2] <- "Sum"
    
    #Cargar datos que necesito para el panel de screening
    primaryTest1 <- (cargarDatos$sensibilidad[,1]) 
    primaryTest1[length(primaryTest1)+1] <- "None" 
    
    #Crear interfaz de soluciÃ³n
    
    interfazSol <- div(id = 'placeholder3',theme = shinytheme("lumen"),
                       tags$style(HTML(".tabbable > .nav > li > a {background-color: #FAF9FA;  color:black}")),
     fluidRow(h2(strong("Cervical Cancer Predictions",style = "font-family: 'Source Sans Pro';
                                color: #FFFF; 
                                padding: 20px;
                                text-align: center"),align = "center")),
      #hr(),
      tabsetPanel(
        tabPanel("Policy Tab",br(),
        verticalLayout(
          div(style = ";padding-left: 35px;padding-right: 35px;",wellPanel(style="background-color: #FAFAFA",h4(strong("Vaccines")),
                                                                           tags$hr(),
                                                                           fluidRow(style = ";padding-left: 45px;padding-right: 45px;",
                                                                                    column(6,h5(strong("Which gender(s) you want to apply the vaccine to?"))),
                                                                                    column(3,checkboxGroupInput("vacSex",label = NULL, c("Female" = "Mujeres","Men" = "Hombres"), inline = TRUE), offset = 0),
                                                                                    column(1, actionButton("vacunasVali", "Add Vaccination Policy"))
                                                                                    ),
                                                                           div(hr(style = ";padding-left: 55px;padding-right: 55px;")),
                                                                           splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "padding: 10px"),
                                                                             wellPanel(style="background-color: #F6F3EC",
                                                                                       h5(strong("Parameters for Females:")),
                                                                                       hr(),
                                                                                       h6("Select the age ranges of people to apply the vaccine to:"),
                                                                                       div(style = "padding-left: 20px",sliderInput("vacRangeFemale", label = NULL, value = c(15,30), min = 1, max = 100, width = '80%')),
                                                                                       h6("Select the porcentage of people to apply the vaccine to:"),
                                                                                       div(style = "padding-left: 20px",sliderInput("vacPorcentageFemale", label = NULL, value = 0.15, min = 0, max = 1, width = '80%')),
                                                                                       h6("Select the years to apply the vaccine:"),
                                                                                       div(style = "padding-left: 20px",sliderInput("vacYearsFemale", label = NULL, value = c(5,10), min = 1, max = 50, width = '80%'))
                                                                                       ),
                                                                             wellPanel(style="background-color: #F6F3EC",
                                                                                       h5(strong("Parameters for Males:")),
                                                                                       hr(),
                                                                                       h6("Select the age ranges of people to apply the vaccine to:"),
                                                                                       div(style = "padding-left: 20px",sliderInput("vacRangeMale", label = NULL, value = c(15,30), min = 1, max = 100, width = '80%')),
                                                                                       h6("Select the porcentage of people to apply the vaccine to:"),
                                                                                       div(style = "padding-left: 20px",sliderInput("vacPorcentageMale", label = NULL, value = 0.15, min = 0, max = 1, width = '80%')),
                                                                                       h6("Select the years to apply the vaccine:"),
                                                                                       div(style = "padding-left: 20px",sliderInput("vacYearsMale", label = NULL, value = c(5,10), min = 1, max = 50, width = '80%'))
                                                                                       )
                                                                           )
                                                                           )),
          hr(),
          div(style = ";padding-left: 35px;padding-right: 35px;",wellPanel(style="background-color: #FAFAFA",h4(strong("Screening")),
                                                                           tags$hr(),
                                                                           verticalLayout(
                                                                             fluidRow(style = ";padding-left: 45px;padding-right: 45px;",
                                                                                      column(9, "The first box to enter a number is the initial age at which the test is started, the second at the highest age at which the test is performed, and the third refers to how often the test is performed."),
                                                                                      column(1, actionButton("screeningVali", "Add Screening Policy"),offset = 0)
                                                                             ),
                                                                             br(),
                                                                             wellPanel(style="background-color: #F6F3EC",
                                                                                      fluidRow(style = ";padding-left: 45px;padding-right: 45px;",
                                                                                                column(2,h5(strong("Primary test"))),
                                                                                                column(3,selectInput("primaryTest",label = NULL,primaryTest1,selected = "None")),
                                                                                                column(2,numericInput("iniPrimaryTest",label = NULL,min=0,max=100,step=1,value=25,width='100%'),offset = 0),
                                                                                                column(2,numericInput("maxPrimaryTest",label = NULL,min=0,max=100,step=1,value=65,width='100%'),offset = 0),
                                                                                                column(2,numericInput("stepPrimaryTest",label = NULL,min=0,max=100,step=1,value=5,width='100%'),offset = 0)
                                                                                                ),
                                                                                      br(),
                                                                                      fluidRow(style = ";padding-left: 45px;padding-right: 45px;",
                                                                                               column(2,h5(strong("Triage Test"))),
                                                                                               column(3,selectInput("triageTest",label = NULL,primaryTest1,selected = "None")),
                                                                                               column(2,numericInput("iniTriage",label = NULL,min=0,max=100,step=1,value=25,width='100%'),offset = 0),
                                                                                               column(2,numericInput("maxTriage",label = NULL,min=0,max=100,step=1,value=65,width='100%'),offset = 0),
                                                                                               column(2,numericInput("stepTriage",label = NULL,min=0,max=100,step=1,value=5,width='100%'),offset = 0)
                                                                                               ),
                                                                                      br(),
                                                                                      fluidRow(style = ";padding-left: 45px;padding-right: 45px;",
                                                                                               column(2,h5(strong("Follow Up"))),
                                                                                               column(3,selectInput("followUp",label =NULL,primaryTest1,selected = "None")),
                                                                                               column(4, "Time (in months) from negative triage test to follow up:"),
                                                                                               column(2,numericInput("timeFollowUp",label = NULL,min=0,max=600,step=1,value=6,width='100%'),offset = 0)
                                                                                               )
                                                                                       )
                                                                           )
                                                                           ))
        )
        ),
        
        tabPanel("Population Predictions",br(),sidebarLayout(
        sidebarPanel(style="background-color: #EAFAF2",width = 4,
                     sliderInput(inputId = "num",  label = h5(strong("Choose the simulation length:")),  value = 20, min = 1, max = 50),
                     tags$hr(),
                     h5(strong("Choose the gender:")),
                     column(12,tags$div(align = "left",class = "multicol",checkboxGroupInput(inputId= "sexoUsuario", label = NULL,c("Female"="Mujeres","Sum"="sumSexo","Male"="Hombres"),inline = FALSE, selected  = "Mujeres"))),
                     tags$hr(),
                     h5(strong("Choose the Biological Compartment:")),
                     column(12,tags$div(align = "left",class = "multicol",checkboxGroupInput(inputId= "biologicalCompUsuario", label = NULL,nombresBiolo,inline = FALSE, selected = 1))),
                     tags$hr(),
                     sliderInput(inputId = "edadUsuario", value=c(15,30),label= h5(strong("Choose the age to be analyzed:")), min=1, max=100),
                     tags$hr(),
                     fluidRow(column(2,offset = 3,
                                     actionButton('changeGraphBtn', 'Update Graph and table')))
                     
        ),
        
        mainPanel(tabsetPanel(
          tabPanel("Graphs",br(),plotlyOutput("grafica",width = "100%", height = "100%"),htmlOutput("textoGrafica")),tabPanel("Tables",dataTableOutput("dataTable"),downloadButton('downloadData', 'Download')),
          (tags$style(HTML("
                            #textoGrafica {
                              font-size: 14px;
                            }
                            ")))
          ))
      )),
                tabPanel("Public Health Indicators",br(), sidebarLayout(
                  sidebarPanel(style="background-color: #E6F1F9",width = 4,
                               h5(strong("Choose the biological compartments to show:")),selectizeInput(inputId = "variablesH", label = NULL, choices = vectorNombresP,multiple = TRUE),
                               hr(),
                               h5(strong("Choose the health indicators to show:")),selectizeInput(inputId = "indicadoresH", label = NULL, choices = c("Deaths","Cases","Incidence","Deaths per Million","Death Rate"), selected = NULL ,multiple = TRUE),
                               hr(),
                               fluidRow(column(2,offset = 3,
                               actionButton('changeGraphBtn1', 'Update Graph'))),
                               br(),
                               fluidRow(column(2,
                               downloadButton("downloadIndicatorsPlot","Save Figure")),column(2,downloadButton("downloadIndicators","Download Data"),offset = 3))
                               ),
                  mainPanel(tabsetPanel(tabPanel("Graphs",br(),plotlyOutput("medidasPlot")),tabPanel("Table",dataTableOutput("dataTableMedidas"))))
                )
                         
        ),
      tabPanel("Policy comparison",br(),sidebarLayout(
        sidebarPanel(style="background-color: #fdffeb",width = 4,
          h5(strong("Please upload the policy  files you want to compare:")),
          fileInput("politica1", label = NULL,accept=".csv"),
          fileInput("politica2", label = NULL,accept=".csv"),
          fileInput("politica3", label = NULL,accept=".csv"),
          fileInput("politica4", label = NULL,accept=".csv"),
          tags$hr(),
          h5(strong("Choose the biological compartments to show:")),selectizeInput(inputId = "variablesPoliticaComparar", label = NULL, choices = vectorNombresP,multiple = TRUE),
          #hr(),
          h5(strong("Choose the health indicators to show:")),selectizeInput(inputId = "indicadoresPoliticaComparar", label = NULL, choices = c("Deaths","Cases","Incidence","Deaths per Million","Death Rate"), selected = NULL ,multiple = TRUE),
          hr(),
          fluidRow(column(2,offset = 3,actionButton('changeGraphBtn2', 'Update Graph')))
        ),
        mainPanel(
          br(),
          plotlyOutput("graficaComparacion")
        )
      ))
      ),
      br(),
      column(1,actionButton(inputId = "boton4", label = "Previous")),column(1,actionButton(inputId = "boton5", label = "Close"),offset=10)
   )
    
    
    #Agrega la interfaz que creÃ© arriba
    insertUI(selector = '#placeholder1', 
             ui = interfazSol)
    
  })
  
  #Cuarto BotÃ³n
  observeEvent(input$boton4,{
    #Quita la interfaz de parÃ¡metros
    removeUI(selector =  '#placeholder3')
    
    
    
    #Crear interfaz de soluciÃ³n
    insertUI(selector = '#placeholder1', 
             ui = interfaceInsertar)
  
  })
  
  #Quinto botÃ³n
  observeEvent(input$boton5,{
    stopApp()
  })
 
 
 #-------------ValidacÃ³n de la polÃ�tica-----------------------------------
 #------Vacunas------------------
 observeEvent(input$vacunasVali,{
   if(length(input$vacSex) == 2){
     if(input$vacRangeFemale[2] > edades || input$vacRangeMale[2] > edades ){
       showModal(modalDialog(title = "Model Parametrization Error",
                             paste("Please select age ranges (for both genders) that are less o equal to the maximun age range"),easyClose = TRUE,footer = NULL))
     }else{
       showModal(modalDialog(title = "Vaccination Policy",
                             paste("Vaccination policy will apply for men and women"),easyClose = TRUE,footer = NULL))  
     }
     
   }else if(length(input$vacSex) == 0){
     showModal(modalDialog(tittle = "Vaccination Policy",
                           paste0("No vaccination policy will apply. If you want a policy you must select the gender(s) to which you will apply them.")))
   }else{
     if(input$vacSex == "Mujeres"){
       if(input$vacRangeFemale[2] > edades){
         showModal(modalDialog(title = "Model Parametrization Error",
                               paste("Please select the age range for women that is less o equal to the maximun age range"),easyClose = TRUE,footer = NULL))
       }else{
       showModal(modalDialog(title = "Vaccination Policy",
                             paste("Vaccination policy will apply only for Females"),easyClose = TRUE,footer = NULL))
       }
    }else{
      if(input$vacRangeMale[2] > edades){
        showModal(modalDialog(title = "Model Parametrization Error",
                              paste("Please select the age range for men that is less o equal to the maximun age range"),easyClose = TRUE,footer = NULL))
      }else{
        showModal(modalDialog(title = "Vaccination Policy",
                              paste("Vaccination policy will apply only for Males"),easyClose = TRUE,footer = NULL))
      }
     }
   }
   
 })
 
 #-------Screening---------------
 observeEvent(input$screeningVali,{
   
   if(input$primaryTest == "None"){
     showModal(modalDialog(title = "Screening Policy",
                           paste("No screening policy will apply."),easyClose = TRUE,footer = NULL))
   }else{
     if(input$triageTest == "None"){
       if(input$iniPrimaryTest > edades || input$maxPrimaryTest > edades || input$maxPrimaryTest < input$iniPrimaryTest || input$stepPrimaryTest > (input$maxPrimaryTest-input$iniPrimaryTest)){
         showModal(modalDialog(title = "Model Parametrization Error",
                               paste("The parameters of initial age range, final age range and/or step for the Primary test are not in agreement with the data. Please enter them again."),easyClose = TRUE,footer = NULL))
       }else{
         showModal(modalDialog(title = "Screening Policy",
                               paste("Primary test will apply for the screening policy."),easyClose = TRUE,footer = NULL))
       }
       
     }else{
       if(input$followUp != "None"){
         if( input$iniPrimaryTest > edades || input$maxPrimaryTest > edades || input$maxPrimaryTest < input$iniPrimaryTest || input$stepPrimaryTest > (input$maxPrimaryTest-input$iniPrimaryTest) || input$iniTriage > edades || input$maxTriage > edades || input$maxTriage < input$iniTriage || input$stepTriage > (input$maxTriage-input$iniTriage)){
           showModal(modalDialog(title = "Model Parametrization Error",
                                 paste("The parameters of initial age range, final age range and/or step for the Primary test or the Triage are not in agreement with the data or the time (in months) from negative triage test to follow up is bigger than the maximum simulation legth (50 years). Please enter them again."),easyClose = TRUE,footer = NULL))
         }else{
           showModal(modalDialog(title = "Screening Policy",
                                 paste("Primary test, Trige and Follow Up will apply for the screening policy."),easyClose = TRUE,footer = NULL))
         } 
       }else{
         if(input$iniPrimaryTest > edades || input$maxPrimaryTest > edades || input$maxPrimaryTest < input$iniPrimaryTest || input$stepPrimaryTest > (input$maxPrimaryTest-input$iniPrimaryTest) || input$iniTriage > edades || input$maxTriage > edades || input$maxTriage < input$iniTriage || input$stepTriage > (input$maxTriage-input$iniTriage)){
           showModal(modalDialog(title = "Model Parametrization Error",
                                 paste("The parameters of initial age range, final age range and/or step for the Primary test or the Triage are not in agreement with the data. Please enter them again."),easyClose = TRUE,footer = NULL))
         }else{
           showModal(modalDialog(title = "Screening Policy",
                               paste("Primary test and Trige will apply for the screening policy."),easyClose = TRUE,footer = NULL))
         }
       }
     }
   }
 })
 
 #-----Leer datos---------------------------------------------------------------------------
 cargarDatos <- reactiveValues(
   nacimientos = as.matrix(read.xlsx("ParametrosGenerales.xlsx",2, dec =",", as.data.frame = TRUE)),
   parametros = as.matrix(read.xlsx("ParametrosGenerales.xlsx",1, dec =",", as.data.frame = TRUE)),
   transicionesM = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",3, dec =",", as.data.frame = TRUE)),
   transicionesH = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",4, dec =",", as.data.frame = TRUE)),
   otros = as.matrix(read.table("generales.txt", header = FALSE, dec=",")),
   bioloGeneral = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",5,dec =",", as.data.frame = TRUE)),
   parejasSexuales = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",6,dec =",", as.data.frame = TRUE)),
   sensibilidad = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",7,dec =",", as.data.frame = TRUE)),
   cobertura = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",8,dec =",", as.data.frame = TRUE))
  )
 
 #----Generar los nuevos datos-------------------------------------------------------------
 isolate({
   
   #Rango de la simulaciÃ³n
   simu <- 50
   #GÃ©neros
   gen = 2
   #Leyendo los dataFrames
   transicionesM <- as.matrix.data.frame(cargarDatos$transicionesM[,-ncol(cargarDatos$transicionesM)])
   transicionesH <- as.matrix.data.frame(cargarDatos$transicionesH[,-ncol(cargarDatos$transicionesH)])
   parametros <- as.matrix.data.frame(cargarDatos$parametros[,-ncol(cargarDatos$parametros)])
   nacimientos <- as.matrix.data.frame(cargarDatos$nacimientos[,-ncol(cargarDatos$nacimientos)])
   
   
   #Cargar datos del archivo otros
   compart <- as.numeric(cargarDatos$otros[1,1])
   nacDesde = as.numeric(cargarDatos$otros[4,1])
   nacHasta = as.numeric(cargarDatos$otros[5,1])
   muerteBiolo = cbind(as.numeric(cargarDatos$bioloGeneral[,2]),as.numeric(cargarDatos$bioloGeneral[,3]))
   edadesFert = nacHasta-nacDesde
   te <- as.numeric(cargarDatos$otros[2,1])
   probMujer <- as.numeric(cargarDatos$otros[3,1])
   edades <- (ncol(parametros))/compart
   
   
   #Cargar datos de las matrices de transiciÃ³nd de probabilidad
   transM <- as.numeric(cargarDatos$otros[6,1])
   transH <- as.numeric(cargarDatos$otros[7,1])
   #vector que carga toda la informaciÃ³n de los estadÃ�os biolÃ³gicos
   vectorT <- cbind(seq(1,transM,by=1),cargarDatos$bioloGeneral[,4],cargarDatos$bioloGeneral[,5],cargarDatos$bioloGeneral[,6])
   #Vector que hace los suscpetibles
   vectorS <- subset(vectorT,vectorT[,2]=="S")
   vectorPI <- subset(vectorT,vectorT[,2]=="PI")
   vectorSPI <- rbind(vectorS,vectorPI)
   
   #Vector que carga la info de los estadÃ�os biolÃ³gicos
   vectorT <- cbind(seq(1,transM,by=1),cargarDatos$bioloGeneral[,4])
   vectorI <- subset(vectorT,vectorT[,2]=="I")
   vectorLP <- subset(vectorT, vectorT[,2]=="LP")
   vectorC <- subset(vectorT, vectorT[,2]=="C")
   vectorILPC <- rbind(vectorI,vectorLP,vectorC)
   vectorIncidence <- rbind(vectorLP,vectorC)
   
   #Nombres Progress
   vectorNT <- cbind(cargarDatos$bioloGeneral[,1],cargarDatos$bioloGeneral[,4])
   vectorNombresP <- subset(vectorNT[,1], vectorNT[,2] == "LP" | vectorNT[,2] == "C")
   
   #Cargar Datos de parejas sexuales
   parejasSexualesM <- cbind(cargarDatos$parejasSexuales[,1],cargarDatos$parejasSexuales[,3])
   parejasSexualesH <- cbind(cargarDatos$parejasSexuales[,1],cargarDatos$parejasSexuales[,2])
   
   #Cargar datos para el screening
   coberturaTamizaje <- cbind(as.numeric(cargarDatos$cobertura[,1]),as.numeric(cargarDatos$cobertura[,2]),as.numeric(cargarDatos$cobertura[,3]),as.numeric(cargarDatos$cobertura[,4]))
   sensibilidadPrubeas <- cbind(cargarDatos$sensibilidad[,1],as.numeric(cargarDatos$sensibilidad[,2]),as.numeric(cargarDatos$sensibilidad[,3]))
   
   #Valor para costos
   costoVacuna = as.numeric(cargarDatos$otros[8,1])
   
   #Generar vectores de muertes y fertilidad
   tasaMuerteTotales<- matrix(seq(1),nrow = simu,ncol=edades*compart)
   
   L=c(1:simu)
   n.init  <- 0.01
   Kd.init <- 0.01
   #---Generando tasas de muerte
   #Progress bar
   pb <- winProgressBar(title = "Progress Bar for Death Rate Data Processing", label = " ",min = 0, max = ncol(parametros), width = 300)
   for(i in 1:(ncol(parametros))){
     Human <- as.double(parametros[-(1),i])
     # fitting suavizamiento exponencial
     tasaMuerteiFuturo <- forecast(HoltWinters(Human, beta=FALSE, gamma=FALSE),h=9)
     for (j in 1:simu){
       tasaMuerteTotales[j,i] = tasaMuerteiFuturo$mean[1]
     }
     setWinProgressBar(pb, i,label=paste(round(i/ncol(parametros)*100, 0), "% data processed" ))
   }
   close(pb)
   
   #Trayendo el total de poblaciÃ³n de cada Compartimiento
   Humano<-rep(0,times=ncol(parametros))
   vacunadosHombres<- rep(0,times=edades)
   vacunadosMujeres<- rep(0,times=edades)
   progresionesDetectadas <- rep(0, times = edades*nrow(vectorLP))
   costoVacunacion = 0
   costoPrimaryIni = 0
   costoTriageIni = 0
   costoFollowUpIni = 0
   efectividadIni = 0
   incidenciaIni = rep(0,times = nrow(vectorIncidence))
   for(i in 1:(ncol(parametros))){
     Humano[i]=as.numeric(parametros[1,i])
   }
   
   #---Generando tasas de nacimiento ------------
   tasaNacimientosTotales<- matrix(seq(1),nrow = simu,ncol=ncol(nacimientos))
   pb <- winProgressBar(title = "Progress Bar for Fertility Rate Data Processing", label = " ",min = 0, max = ncol(nacimientos), width = 300)
   # fitting Suavizamiento exponencial
   for(i in 1:(ncol(nacimientos))){
     nac <- as.numeric(nacimientos[,i])
     tasaNacimientoiFuturo <- forecast(HoltWinters(nac, beta=FALSE, gamma=FALSE),h=9)
     for (j in 1:simu){
       tasaNacimientosTotales[j,i] =  tasaNacimientoiFuturo$mean[1]
     }
     setWinProgressBar(pb, i,label=paste(round(i/ncol(nacimientos)*100, 0), "% data processed" ))
   }
   close(pb)
   
 })
 
 #----Resolver las ecuaciones diferenciales------------------------------------------------
 data <- reactive({
   
   a = 50
   nacimientosTotales = 0
   
   #Porcentaje de vacunaciÃ³n
   vacMujeres = matrix(0, ncol = edades , nrow = 50)
   vacHombres = matrix(0, ncol = edades , nrow = 50)
   #Edades de vacunaciÃ³n
   edadesVacFemale <- seq(input$vacRangeFemale[1],input$vacRangeFemale[2],by=1)
   edadesVacMale <- seq(input$vacRangeMale[1],input$vacRangeMale[2],by=1)
   #AÃ±os de VacunaciÃ³n
   yearsVacFemale  <- seq(input$vacYearsFemale[1],input$vacYearsFemale[2],by=1)
   yearsVacMale <- seq(input$vacYearsMale[1],input$vacYearsMale[2],by=1)
   input$vacYearsFemale[1]
   input$vacYearsFemale[2]
   if(length(input$vacSex) == 2){
     for(i in 1:50){
       for(j in 1:edades){
         if(j %in% edadesVacFemale && i %in% yearsVacFemale){
           vacMujeres[i,j] = input$vacPorcentageFemale
         }
         if(j %in% edadesVacMale && i %in% yearsVacMale){
           vacHombres[i,j] = input$vacPorcentageMale
         }
       }
     }
   }else if(length(input$vacSex) == 1 && input$vacSex == "Mujeres"){
     for(i in 1:50){
       for(j in 1:edades){
         if(j %in% edadesVacFemale && i %in% yearsVacFemale){
           vacMujeres[i,j] = input$vacPorcentageFemale
         }
       }
     }
   }else if(length(input$vacSex) == 1 && input$vacSex == "Hombres"){
     for(i in 1:50){
       for(j in 1:edades){
         if(j %in% edadesVacMale && i %in% yearsVacMale){
           vacHombres[i,j] = input$vacPorcentageMale
         }
       }
     }
   }
  
   
  #----------Vectores para screening-------------------
   xij <- rep(0,times=edades)
   triageij <- rep(0,times=edades)
   folowUpHay <- 0
   seguimiento <- input$followUp
   sensibilidadPrimary = 0
   sensibilidadTriage = 0
   sensibilidadFollowUp = 0
   costoPrimary = 0
   costoTriage = 0
   costoFollowUp = 0
     contadorPrimi2 = 0
     contadorPrimi = 1
     if(input$primaryTest != "None"){
       for(i in input$iniPrimaryTest:input$maxPrimaryTest){
         contadorPrimi2 = contadorPrimi2+1
         xij[i] = contadorPrimi
         contadorPrimi = 0
         if(contadorPrimi2%%input$stepPrimaryTest == 0){
           contadorPrimi = 1
         }
       }
       contadorPrimi2 = 0
       contadorPrimi = 1
       if(input$triageTest != "None"){
         for(i in input$iniTriage:input$maxTriage){
           contadorPrimi2 = contadorPrimi2+1
           triageij[i] = contadorPrimi
           contadorPrimi = 0
           if(contadorPrimi2%%input$stepTriage == 0){
             contadorPrimi = 1
           }
         }
       }
       
     }
    
     for(i in 1:(length(sensibilidadPrubeas)/3)){
       if(input$primaryTest != "None" && input$primaryTest == sensibilidadPrubeas[i,1]){
         sensibilidadPrimary = as.numeric(sensibilidadPrubeas[i,2])
         costoPrimary = as.numeric(sensibilidadPrubeas[i,3])
       }
       if(input$triageTest != "None" && input$triageTest == sensibilidadPrubeas[i,1]){
         sensibilidadTriage = as.numeric(sensibilidadPrubeas[i,2])
         costoTriage = as.numeric(sensibilidadPrubeas[i,3])
       }
       if(input$followUp != "None" && input$followUp == sensibilidadPrubeas[i,1]){
         sensibilidadFollowUp = as.numeric(sensibilidadPrubeas[i,2])
         costoFollowUp = as.numeric(sensibilidadPrubeas[i,3])
         folowUpHay = 0
       }
     }
     
  #---Generando vector de diferenciales--------
   dH<-rep(0,times=ncol(parametros))
   dvH<-rep(0,times=edades)
   dvM<-rep(0,times=edades)
   dvPD<-rep(0,times=edades*nrow(vectorLP))
   dCostoVacunacion = 0
   dCostoPrimary = 0
   dCostoTriage = 0
   dCostoFollowUp = 0
   dEfectividad = 0
   dIncidencia= rep(0, times = nrow(vectorIncidence))
   vectorNoProgressNoSusc <- seq(1,transM, by = 1)[-c(as.numeric(vectorLP[,1]))]
   vectorNoProgressNoSusc <- vectorNoProgressNoSusc[-c(as.numeric(vectorS[,1]))]
   vectorProgress <-seq(1,transM, by = 1)[c(as.numeric(vectorLP[,1]))]
   vectorSusc <- seq(1,transM, by = 1)[c(as.numeric(vectorS[,1]))]
   
  #-- Escribiendo ecuaciones diferenciales-------------    
   poblacion <- function(t, y, parameters) {
     with(as.list(y), {
       #Matrices de probabilidad
       auxiliarCostoVacuna = 0
       auxiliarCostoPrimary = 0
       auxiliarCostoTriage = 0
       auxiliarCostoFollowUp = 0
       auxiliarEfectividad = 0
       auxiliarIncidence = rep(0, times = nrow(vectorIncidence))
       
       mujeresTotales <- sum(y[((edades*compart/2)+1):(edades*2*transM)])+ sum(y[(transM*2*edades+edades+1):(transM*2*edades+edades*2)]) + sum(y[(compart*edades+edades*2+1):(compart*edades+edades*2+nrow(vectorLP)*edades)])
       hombresTotales <- sum(y[1:(edades*compart/2)]) + sum(y[(transM*2*edades+1):(transM*2*edades+edades)])
       mujeresTotalesInfectadas <- 0
       hombresTotalesInfectados <- 0
       for(k in as.numeric(vectorILPC[,1])){
         for(l in ((edades*compart/2)+((k-1)*edades)+1):((edades*compart/2)+((k-1)*edades)+edades)){
          mujeresTotalesInfectadas = mujeresTotalesInfectadas + y[l]
         }
       }
       mujeresTotalesInfectadas = mujeresTotalesInfectadas + sum(y[(compart*edades+edades*2+1):(compart*edades+edades*2+nrow(vectorLP)*edades)])
       for(k in as.numeric(vectorILPC[,1])){
         for(l in (((k-1)*edades)+1):(((k-1)*edades)+edades)){
           hombresTotalesInfectados = hombresTotalesInfectados + y[l]
         }
       }
       
       newTransH <- transicionesH
       contadorTrans1 <- 0
       contadorTransi <- 0
       #Matriz de Hombres: 
       for(i in as.numeric(vectorSPI[,1])){
         contadorTransi = contadorTransi + 1
         seqT <- seq(((as.numeric(parejasSexualesH[1,1])-1)*(transM+1))+1+i, 1+(transM+1)*edades ,by=transM+1)
         contadorTrans1 = 0
         for(j in seqT){
           contadorTrans1 = contadorTrans1 +1 
           newTransH[j,as.numeric(vectorI[,1])]= 1-exp(-(mujeresTotalesInfectadas/mujeresTotales)*as.numeric(parejasSexualesH[contadorTrans1,2])*as.numeric(vectorSPI[contadorTransi,3])*1)
           newTransH[j,i]= 1-(1-exp(-(mujeresTotalesInfectadas/mujeresTotales)*as.numeric(parejasSexualesH[contadorTrans1,2])*as.numeric(vectorSPI[contadorTransi,3])*1))
          }
       }
       newTransM <- transicionesM
       contadorTrans1 <- 0
       contadorTransi <- 0
       #Matriz de Mujeres: 
       for(i in as.numeric(vectorSPI[,1])){
         contadorTransi = contadorTransi + 1
         seqT <- seq(((as.numeric(parejasSexualesM[1,1])-1)*(transM+1))+1+i, 1+(transM+1)*edades ,by=transM+1)
         contadorTrans1 = 0
         for(j in seqT){
           contadorTrans1 = contadorTrans1 +1 
           newTransM[j,as.numeric(vectorI[,1])]= 1-exp(-(hombresTotalesInfectados/hombresTotales)*as.numeric(parejasSexualesM[contadorTrans1,2])*as.numeric(vectorSPI[contadorTransi,4])*te)
           newTransM[j,i]= 1-(1-exp(-(hombresTotalesInfectados/hombresTotales)*as.numeric(parejasSexualesM[contadorTrans1,2])*as.numeric(vectorSPI[contadorTransi,4])*te))
         }
       }
      
       #contador para fertilidad
       contador3 = 0
       #---------Generando nacimientos------------
       for(l in seq((edades*compart/2),(edades*(compart-1)),by=edades)){
         for(k in nacDesde:nacHasta){
           contador3 = contador3 + 1
           nacimientosTotales = nacimientosTotales + y[l+k]*tasaNacimientosTotales[t,contador3]
         } 
       }
       #Generando nacimientos para las vacunadas
       contador3 = 0
       for(w in nacDesde:nacHasta){
         contador3 = contador3 + 1
         nacimientosTotales = nacimientosTotales + y[transM*2*edades+edades+w]*tasaNacimientosTotales[t,contador3]
       }
       #Generando nacimiento para las detectadas
       contador3 = 0
       conti = 0
       for(l in 1:nrow(vectorLP)){
         conti = conti +1
         for(w in nacDesde:nacHasta){
           contador3 = contador3 + 1
           nacimientosTotales = nacimientosTotales + y[transM*2*edades+edades*2+(conti-1)*edades + w]*tasaNacimientosTotales[t,contador3]
         }
       }
       
       #contador lleva la cuenta de cuÃ¡l compartimiento biolÃ³gico es
       contador=0
  
       #------------------Ecuaciones Humano-------------------------------------------------------------
       # el primer for recorre cada nÃºmero de edad (ej edad=3 entonces 0,3,6,9)
       
       
       for(j in seq(0,edades*(compart-1),by=edades)){
         contador = contador +1
         #---Generando ecuaciones diferenciales por estadÃ�o biolÃ³gico
         
         
         if(any(contador ==  as.numeric(vectorS[,1]))){
           dH[1+j] =  (1-probMujer)*(nacimientosTotales)-((tasaMuerteTotales[t,1+j]+muerteBiolo[contador,1])*y[1+j])-vacHombres[t,1]*y[1+j] -te*(y[1+j]-min(1,(tasaMuerteTotales[t,1+j]+muerteBiolo[contador,1]+vacHombres[t,1]))*y[1+j])
           dvH[1] =   vacHombres[t,1]*y[1+j] - tasaMuerteTotales[t,1+j]*y[transM*2*edades+1]- te*(y[transM*2*edades+1]-(tasaMuerteTotales[t,1+j]*y[transM*2*edades+1]))
           auxiliarCostoVacuna = auxiliarCostoVacuna + vacHombres[t,1]*y[1+j]
         }else if(any(contador ==  transM + as.numeric(vectorS[,1]))){
           dH[1+j] =  (probMujer)*(nacimientosTotales)-((tasaMuerteTotales[t,1+j]+muerteBiolo[contador-transM,2])*y[1+j]) -vacMujeres[t,1]*y[1+j] -te*(y[1+j]-min(1,(tasaMuerteTotales[t,1+j]+muerteBiolo[contador-transM,2]+vacMujeres[t,1]))*y[1+j])
           dvM[1] =   vacMujeres[t,1]*y[1+j] - tasaMuerteTotales[t,1+j]*y[transM*2*edades+edades+1]- te*(y[transM*2*edades+edades+1]-(tasaMuerteTotales[t,1+j]*y[transM*2*edades+edades+1]))
           auxiliarCostoVacuna = auxiliarCostoVacuna + vacMujeres[t,1]*y[1+j]
         }else{
           dH[1+j]=0
         }
         
         
         #----Ecuaciones generales rangos medios de edad
         if(edades >2){
           if(contador <= compart/2){
             
             for (i in 2:(edades-1)){
               #secuencias para las transiciones
               seq1 <- seq(((i-2)*(transH))+i, ((i-2)*(transH))+i+transH-1,by=1)
               seqState <- seq(0+i-1,edades*(compart/2),by=edades)
               if(any(contador ==  as.numeric(vectorS[,1]))){
                 dH[i+j] = te*(sum(as.numeric(newTransH[seq1,contador])*(y[seqState]*(1-tasaMuerteTotales[t,seqState]-muerteBiolo[contador,1]-vacHombres[t,i-1])))) -vacHombres[t,i]*y[i+j] -(tasaMuerteTotales[t,i+j]+muerteBiolo[contador,1])*y[i+j] -te*(y[i+j]-(tasaMuerteTotales[t,i+j]+muerteBiolo[contador,1]+vacHombres[t,i])*y[i+j])
                 dvH[i] =  te*(y[transM*2*edades+i-1]-(tasaMuerteTotales[t,i-1]*y[transM*2*edades+i-1])) +vacHombres[t,i]*y[i+j] -tasaMuerteTotales[t,i]*y[transM*2*edades+i]- te*(y[transM*2*edades+i]-(tasaMuerteTotales[t,i]*y[transM*2*edades+i]))
                 auxiliarCostoVacuna = auxiliarCostoVacuna + vacHombres[t,i]*y[i+j]
                }else{
                 dH[i+j] = te*(sum(as.numeric(newTransH[seq1,contador])*(y[seqState]*(1-tasaMuerteTotales[t,seqState]-muerteBiolo[contador,1])))) -(tasaMuerteTotales[t,i+j]+muerteBiolo[contador,1])*y[i+j] -te*(y[i+j]-min(1,(tasaMuerteTotales[t,i+j]+muerteBiolo[contador,1]))*y[i+j])
                }
                 
             }
           }else{ #acÃ¡ empiezan las mujeres
             for (i in 2:(edades-1)){
               
               
               #secuencias para las transiciones
               seq1 <- seq(((i-2)*(transM))+i, ((i-2)*(transM))+i+transH-1,by=1)
               seqState <- seq(edades*(compart/2)+i-1,edades*(compart),by=edades)
               seqDetected <- seq(edades*compart+edades*2+ i-1, edades*compart+edades*2+edades*(nrow(vectorLP)-1) +i-1,by=edades)
              
               if(any(contador == transM + as.numeric(vectorS[,1]))){
                 dH[i+j] = te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1]))))-vacMujeres[t,i]*y[i+j] + te*(sum(as.numeric(newTransM[seq1[vectorNoProgressNoSusc],contador-compart/2])*(y[seqState[vectorNoProgressNoSusc]]*(1-tasaMuerteTotales[t,seqState[vectorNoProgressNoSusc]]-muerteBiolo[contador-transM,2]))))-(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j] -te*(y[i+j]-(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2]+vacMujeres[t,i])*y[i+j]) 
                           
                 #Vacunas
                 dvM[i] = te*(y[transM*2*edades+edades+i-1]-tasaMuerteTotales[t,i-1]*y[transM*2*edades+edades+i-1]) + vacMujeres[t,i]*y[i+j] - tasaMuerteTotales[t,i]*y[transM*2*edades+edades+i]-te*(y[transM*2*edades+edades+i]-(tasaMuerteTotales[t,i]*y[transM*2*edades+edades+i]))
                 auxiliarCostoVacuna = auxiliarCostoVacuna + vacMujeres[t,i]*y[i+j]
                 
               }else if(any(contador == transM + as.numeric(vectorPI[,1]))){
                 dH[i+j] = te*(sum(as.numeric(newTransM[seq1[vectorNoProgressNoSusc],contador-compart/2])*(y[seqState[vectorNoProgressNoSusc]]*(1-tasaMuerteTotales[t,seqState[vectorNoProgressNoSusc]]-muerteBiolo[contador-transM,2])))) -(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j] -te*(y[i+j]-(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j])+
                          te*xij[i-1]*as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary*(1-triageij[i-1])*sum(y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))+
                          te*xij[i-1]*as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary*(triageij[i-1]*sensibilidadTriage*triageij[i-1])*sum(y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))+
                          te*(1-xij[i-1])*sum(as.numeric(newTransM[seq1[vectorProgress],contador-compart/2])*(y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2])))+
                          te*(xij[i-1])*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary)*sum(as.numeric(newTransM[seq1[vectorProgress],contador-compart/2])*(y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2])))+
                          te*(xij[i-1]*as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary*xij[i-1]*triageij[i-1]*(1-triageij[i-1]*sensibilidadTriage))*sum(as.numeric(newTransM[seq1[vectorProgress],contador-compart/2])*(y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2])))+
                          ((input$timeFollowUp/12))*sensibilidadFollowUp*folowUpHay*sum(y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))+
                          ((input$timeFollowUp/12))*folowUpHay*(1-sensibilidadFollowUp*folowUpHay)*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                          te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1]))))+(1-((input$timeFollowUp/12)))*folowUpHay*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))

               }else if(any(contador == transM + as.numeric(vectorI[,1]))){
                 dH[i+j] = te*(sum(as.numeric(newTransM[seq1[vectorNoProgressNoSusc],contador-compart/2])*(y[seqState[vectorNoProgressNoSusc]]*(1-tasaMuerteTotales[t,seqState[vectorNoProgressNoSusc]]-muerteBiolo[contador-transM,2])))) -(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j] -te*(y[i+j]-(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j])+
                           te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                           te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                           te*(xij[i-1]*as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary*(triageij[i-1]*(1-sensibilidadTriage)))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                           (((input$timeFollowUp/12))*folowUpHay*(1-sensibilidadFollowUp*folowUpHay))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                           ((1-(input$timeFollowUp/12))*folowUpHay)*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                           te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1]))))+((1-((input$timeFollowUp/12)))*folowUpHay)*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))

               }else if(any(contador == transM + as.numeric(vectorLP[,1]))){
                  vectorProgressc <-seq(1,transM, by = 1)[c(as.numeric(vectorC[,1]))]
                  dH[i+j] = te*(sum(as.numeric(newTransM[seq1[vectorNoProgressNoSusc],contador-compart/2])*(y[seqState[vectorNoProgressNoSusc]]*(1-tasaMuerteTotales[t,seqState[vectorNoProgressNoSusc]]-muerteBiolo[contador-transM,2])))) -(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j] -te*(y[i+j]-min(1,(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2]))*y[i+j])+
                    te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*(as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary)*(triageij[i-1])*(1-sensibilidadTriage)*(1-folowUpHay))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    (((input$timeFollowUp/12))*folowUpHay*(1-folowUpHay*sensibilidadFollowUp))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1])))) 

                  auxiliarCostoPrimary = auxiliarCostoPrimary + xij[i]*as.numeric(coberturaTamizaje[i,2])*y[i+j]
                  auxiliarCostoTriage = auxiliarCostoPrimary + xij[i]*as.numeric(coberturaTamizaje[i,2])*sensibilidadPrimary*triageij[i]*y[i+j]
                  auxiliarCostoFollowUp = auxiliarCostoFollowUp + xij[i]*as.numeric(coberturaTamizaje[i,2])*sensibilidadPrimary*triageij[i]*sensibilidadTriage*y[i+j]
                  auxiliarEfectividad = auxiliarEfectividad + xij[i]*as.numeric(coberturaTamizaje[i,2])*sensibilidadPrimary*y[i+j]*as.numeric(coberturaTamizaje[i,4])
                  auxiliarIncidence[contador-transM - nrow(vectorIncidence)] = auxiliarIncidence[contador-transM - nrow(vectorIncidence)] + te*(sum(as.numeric(newTransM[seq1[vectorNoProgressNoSusc],contador-compart/2])*(y[seqState[vectorNoProgressNoSusc]]*(1-tasaMuerteTotales[t,seqState[vectorNoProgressNoSusc]]-muerteBiolo[contador-transM,2])))) + 
                    te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2])) +
                    te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*(as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary)*(triageij[i-1])*(1-sensibilidadTriage)*(1-folowUpHay))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    (((input$timeFollowUp/12))*folowUpHay*(1-folowUpHay*sensibilidadFollowUp))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1])))) 

                }else if(any(contador == transM + as.numeric(vectorC[,1]))){
                  vectorProgressc <-seq(1,transM, by = 1)[c(as.numeric(vectorC[,1]))]
                  dH[i+j] = te*(sum(as.numeric(newTransM[seq1[vectorProgressc],contador-compart/2])*(y[seqState[vectorProgressc]]*(1-tasaMuerteTotales[t,seqState[vectorProgressc]]-muerteBiolo[contador-transM,2])))) -(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j] -te*(y[i+j]-min(1,(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2]))*y[i+j])+
                    te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary*triageij[i-1]*(1-sensibilidadTriage))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))
                    te*(((input$timeFollowUp/12))*folowUpHay*(1-folowUpHay*sensibilidadFollowUp))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1])))+ te*((1-(input$timeFollowUp/12))*folowUpHay)*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2])))

                    auxiliarIncidence[contador-transM - nrow(vectorIncidence)] = auxiliarIncidence[contador-transM - nrow(vectorIncidence)] + te*(sum(as.numeric(newTransM[seq1[vectorProgressc],contador-compart/2])*(y[seqState[vectorProgressc]]*(1-tasaMuerteTotales[t,seqState[vectorProgressc]]-muerteBiolo[contador-transM,2]))))+
                              te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                              te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                              te*(xij[i-1]*as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary*triageij[i-1]*(1-sensibilidadTriage))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))
                              te*(((input$timeFollowUp/12))*(1-folowUpHay*sensibilidadFollowUp))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))
                              #te*(1-((input$timeFollowUp/12))*folowUpHay)*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))

               }
             }
           }
         }
         
         i = i+1
         #EcuaciÃ³n Ãºltimo rango de edad
         if (contador <= compart/2){
           #secuencias para las transiciones
           seq1 <- seq(((i-2)*(transH))+i, ((i-2)*(transH))+i+transH-1,by=1)
           seqState <- seq(0+i-1,edades*(compart/2),by=edades)
           i = i+1
           seq2 <- seq(((i-2)*(transH))+i, ((i-2)*(transH))+i+transH-1,by=1)
           seqState2 <- seq(0+i-1,edades*(compart/2),by=edades)
             #Ecuaciones
           if(any(contador ==  as.numeric(vectorS[,1]))){
             dH[edades+j] = te*(sum(as.numeric(newTransH[seq1,contador])*(y[seqState]*(1-tasaMuerteTotales[t,seqState]-muerteBiolo[contador,1]-vacHombres[t,i-1]))))+
               te*(sum(as.numeric(newTransH[seq2,contador])*(y[seqState2]*(1-tasaMuerteTotales[t,seqState2]-muerteBiolo[contador,1])))) -
               (tasaMuerteTotales[t,edades+j]+muerteBiolo[contador,2])*y[edades+j]-
               te*(y[edades+j]-min(1,(tasaMuerteTotales[t,edades+j]+muerteBiolo[contador,1]+vacHombres[t,edades]))*y[edades+j])-vacHombres[t,edades]*y[edades+j]
             
             dvH[edades] =  te*(y[transM*2*edades+edades-1]-(tasaMuerteTotales[t,edades-1]*y[transM*2*edades+edades-1]))+ vacHombres[t,edades]*y[edades+j] - tasaMuerteTotales[t,edades]*y[transM*2*edades+edades]
             auxiliarCostoVacuna = auxiliarCostoVacuna + vacHombres[t,edades]*y[edades+j]
            
           }else{
             dH[edades+j] = te*(sum(as.numeric(newTransH[seq1,contador])*(y[seqState]*(1-tasaMuerteTotales[t,seqState]-muerteBiolo[contador,1]))))+
               te*(sum(as.numeric(newTransH[seq2,contador])*(y[seqState2]*(1-tasaMuerteTotales[t,seqState2]-muerteBiolo[contador,1])))) -
               (tasaMuerteTotales[t,edades+j]+muerteBiolo[contador,2])*y[edades+j]-
               te*(y[edades+j]-min(1,(tasaMuerteTotales[t,edades+j]+muerteBiolo[contador,1]))*y[edades+j])
           }
             
           
         } else{
           #secuencias para las transiciones
           seq1 <- seq(((i-2)*(transM))+i, ((i-2)*(transM))+i+transH-1,by=1)
           seqState <- seq(edades*(compart/2)+i-1,edades*(compart),by=edades)
           i = i+1
           seq2 <- seq(((i-2)*(transM))+i, ((i-2)*(transM))+i+transH-1,by=1)
           seqState2 <- seq(edades*(compart/2)+i-1,edades*(compart),by=edades)
           seqDetected <- seq(edades*compart+edades*2+ edades-1, edades*compart+edades*2+edades*(nrow(vectorLP)-1) +edades-1,by=edades)
           
           if(any(contador ==  transM + as.numeric(vectorS[,1]))){
           dH[edades+j] = -vacMujeres[t,edades]*y[edades+j]  +te*(sum(as.numeric(newTransM[seq1,contador-compart/2])*(y[seqState]*(1-tasaMuerteTotales[t,seqState]-muerteBiolo[contador-compart/2,2] -vacMujeres[t,i-1]))))+
             te*(sum(as.numeric(newTransM[seq2,contador-compart/2])*(y[seqState2]*(1-tasaMuerteTotales[t,seqState2]-muerteBiolo[contador-compart/2,2])))) -
             (tasaMuerteTotales[t,edades+j]+muerteBiolo[contador-transM,2])*y[edades+j]-
             te*(y[edades+j]-min(1,(tasaMuerteTotales[t,edades+j]+muerteBiolo[contador-compart/2,1]+vacMujeres[t,edades]))*y[edades+j])
           
             dvM[edades] = te*(y[transM*2*edades+edades+edades-1]-tasaMuerteTotales[t,edades-1]*y[transM*2*edades+edades+edades-1]) + vacMujeres[t,edades]*y[edades+j] - tasaMuerteTotales[t,edades]*y[transM*2*edades+edades+edades]-te*(y[transM*2*edades+edades+edades]-(tasaMuerteTotales[t,edades])*y[transM*2*edades+edades+edades])
             auxiliarCostoVacuna = auxiliarCostoVacuna + vacMujeres[t,edades]*y[edades+j]
           }else{
             dH[edades+j] = te*(sum(as.numeric(newTransM[seq1[vectorNoProgressNoSusc],contador-compart/2])*(y[seqState[vectorNoProgressNoSusc]]*(1-tasaMuerteTotales[t,seqState[vectorNoProgressNoSusc]]-muerteBiolo[contador-compart/2,2]))))+
               te*(sum(as.numeric(newTransM[seq2,contador-compart/2])*(y[seqState2]*(1-tasaMuerteTotales[t,seqState2]-muerteBiolo[contador-compart/2,2])))) -
               (tasaMuerteTotales[t,edades+j]+muerteBiolo[contador-transM,2])*y[edades+j]-
               te*(y[edades+j]-min(1,(tasaMuerteTotales[t,edades+j]+muerteBiolo[contador-compart/2,1]))*y[edades+j])+
               te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,edades-1])))) 
               
           }
        }
         
         
       }# cierra el ciclo en los j
      dCostoVacunacion = auxiliarCostoVacuna
      dCostoPrimary = auxiliarCostoPrimary
      dCostoTriage = auxiliarCostoTriage
      dCostoFollowUp = auxiliarCostoFollowUp
      dEfectividad = auxiliarEfectividad
      for(i in 1:nrow(vectorIncidence)){
        dIncidencia[i] = auxiliarIncidence[i]
      }
      ayudaLP = 1
       
       for(j in 1:nrow(vectorLP)){
         dvPD[1+edades*(j-1)] = 0
         for(i in 2:edades){
           seqDetected <- seq(edades*compart+edades*2+ i-1, edades*compart+edades*2+edades*(nrow(vectorLP)-1) +i-1,by=edades)
           seqState <- seq(edades*(compart/2)+i-1,edades*(compart),by=edades)
           seq1 <- seq(((i-2)*(transM))+i, ((i-2)*(transM))+i+transH-1,by=1)
           dvPD[i+edades*(j-1)] = te*(((input$timeFollowUp/12)))*sum((y[seqDetected])*as.numeric(newTransM[seq1[vectorProgress],as.numeric(vectorLP[,1])]))+
                                  (te*(1-(tasaMuerteTotales[t,i+(as.numeric(vectorLP[j,1])-1)*edades]+muerteBiolo[as.numeric(vectorLP[j,1]),2]))*sum((y[seqState[vectorProgress]])*as.numeric(newTransM[seq1[vectorProgress],as.numeric(vectorLP[j,1])])))*(xij[i-1]*as.numeric(coberturaTamizaje[edades-1,2])*sensibilidadPrimary*triageij[i-1]*(1-sensibilidadTriage)*folowUpHay)-
                                  te*(((input$timeFollowUp/12))*folowUpHay)*y[compart*edades+edades*2+i+edades*(j-1)]*(1-(tasaMuerteTotales[t,i+as.numeric(vectorLP[j,1])*edades]+muerteBiolo[as.numeric(vectorLP[j,1]),2]))-
                                  te*y[compart*edades+edades*2+i+edades*(j-1)]*((tasaMuerteTotales[t,i+(as.numeric(vectorLP[j,1])-1)*edades]+muerteBiolo[as.numeric(vectorLP[j,1]),2]))+te*((1-((input$timeFollowUp/12)))*folowUpHay)*y[compart*edades+edades*2+i+edades*(j-1)]*(1-(tasaMuerteTotales[t,i+(as.numeric(vectorLP[ayudaLP,1])-1)*edades]+muerteBiolo[as.numeric(vectorLP[ayudaLP,1]),2]))
         }
       }
       
       setWinProgressBar(pb, t,label=paste(round(t/50*100, 0), "% simulation that has been runed" ))
       list(c(dH,dvH,dvM, dvPD,dCostoVacunacion,dCostoPrimary,dCostoTriage,dCostoFollowUp,dEfectividad,dIncidencia))
     })
   }
   parameters <- c(te ,edades)
   stateInicial<- c(Humano,vacunadosHombres,vacunadosMujeres,progresionesDetectadas,costoVacunacion,costoPrimaryIni,costoTriageIni,costoFollowUpIni,efectividadIni,incidenciaIni)
   times      <- seq(1, 50, by = 1)
   pb <- winProgressBar(title = "Progress Bar for Simulation", label = " ",min = 0, max = a, width = 300)
   out <- deSolve::ode(y = stateInicial, times = times, func = poblacion, parms = parameters)
   close(pb)
   #--------------Sacando Medidas de DesempeÃ±o----------------------------------
   #matriz donde guarda la soluciÃ³n 
  
   bioloGeneral = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",5,dec =",", as.data.frame = TRUE))
   nombresBiolo <- bioloGeneral[,1]
   nombresFile <- vector(length = edades*length(nombresBiolo)*2)
   nombresFileVacunasH <- vector(length=edades)
   nombresFileVacunasM <- vector(length=edades)
   nombresFileIncidence <- vector(length = nrow(vectorIncidence))
   seqNombre <- seq(1,edades*length(nombresBiolo)*2,edades)
   seqEdad <- seq(1,edades,1)
   ayuda1 <- 0
   contador <- 1
   contador2 <- 1
   for(i in 1:length(nombresFile)){
     if(contador == (edades+1)){
       contador = 1
       contador2 = contador2 +1
     }
     ayuda1 = ayuda1 +1 
     if(i <= (length(nombresFile)/2)){
       nombresFile[ayuda1] = paste0("M_",nombresBiolo[contador2],"_", contador)
     }else{
       nombresFile[ayuda1] = paste0("F_",nombresBiolo[contador2-transM],"_",contador)
     }
     contador = contador +1
   }
   for(i in 1:edades){
     nombresFileVacunasH[i] = paste0("M_Vaccines_",i)
   }
   for(i in 1:edades){
     nombresFileVacunasM[i] = paste0("F_Vaccines_",i)
   }
   
   
   for(i in 1:nrow(vectorIncidence)){
     nombresFileIncidence[i] = i
   }
   
   dat <- matrix(rep(0),nrow=50,ncol=(ncol(parametros)+edades*2 + 1 + 3 + 1 + nrow(vectorIncidence)))
   colnames(dat) <- c(nombresFile,nombresFileVacunasH,nombresFileVacunasM,"Vaccination Cost", "Primary Test Cost", "Triage Test Cost", "Follow Up Test Cost", "Efetivness",nombresFileIncidence)
      
   #Por edad
   for(i in 1:(ncol(parametros)+edades*2)){
     dat[,i]=out[,i+1]
   }
   
   #Agregando los Detectados a los Progress
   contadorPL = 0
   for(j in 1:nrow(vectorLP)){
     contadorPL = contadorPL + 1
     for (i in 1:edades){
       dat[,edades*compart/2+edades*(as.numeric(vectorLP[j,1])-1) + i] = dat[,edades*compart/2+edades*(as.numeric(vectorLP[j,1])-1) + i] + out[,1+edades*compart+edades*2+edades*(contadorPL-1) + i]
     }
   }
   #Arreglando los datos de los costos y la efetividad
   dat[1,ncol(parametros)+edades*2 + 1] = out[1,edades*(compart+2+nrow(vectorLP))+2]*costoVacuna
   dat[1,ncol(parametros)+edades*2 + 2] = out[1,edades*(compart+2+nrow(vectorLP))+3]*costoPrimary
   dat[1,ncol(parametros)+edades*2 + 3] = out[1,edades*(compart+2+nrow(vectorLP))+4]*costoTriage
   dat[1,ncol(parametros)+edades*2 + 4] = out[1,edades*(compart+2+nrow(vectorLP))+5]*costoFollowUp
   dat[1,ncol(parametros)+edades*2 + 5] = out[1,edades*(compart+2+nrow(vectorLP))+6]
   
   
   for(i in 2:50){
     dat[i,ncol(parametros)+edades*2 + 1] = (out[i,edades*(compart+2+nrow(vectorLP))+2]*costoVacuna - sum(dat[1:(i-1),edades*(compart+2)+1]))
     dat[i,ncol(parametros)+edades*2 + 2] = (out[i,edades*(compart+2+nrow(vectorLP))+3]*costoPrimary - sum(dat[1:(i-1),edades*(compart+2)+2]))
     dat[i,ncol(parametros)+edades*2 + 3] = (out[i,edades*(compart+2+nrow(vectorLP))+4]*costoTriage - sum(dat[1:(i-1),edades*(compart+2)+3]))
     dat[i,ncol(parametros)+edades*2 + 4] = (out[i,edades*(compart+2+nrow(vectorLP))+5]*costoFollowUp - sum(dat[1:(i-1),edades*(compart+2)+4]))
     dat[i,ncol(parametros)+edades*2 + 5] = (out[i,edades*(compart+2+nrow(vectorLP))+6] - sum(dat[1:(i-1),edades*(compart+2)+5]))
   }
   
   #Agregando la incidencia
   for(i in 1:nrow(vectorIncidence)){
     dat[1,ncol(parametros)+edades*2 + 5+ i] = out[1,edades*(compart+2+nrow(vectorLP))+6 + i]
   }
   for(i in 1:nrow(vectorIncidence)){
     for(j in 2:50){
       dat[j,ncol(parametros)+edades*2 + 5+ i] = (out[j,edades*(compart+2+nrow(vectorLP))+6+i] - sum(dat[1:(j-1),edades*(compart+2)+5+i])) 
     }
   }
   
   return(dat)
  
 })
 
 #----------------------------GrÃ¡ficas-----------------------------------------------------
 
 buildPlot <- reactive({
   
   input$changeGraphBtn 
   
   #ValidaciÃ³n de los datos
   
   isolate({
     validate(
       need(length(input$sexoUsuario)>=2 || input$sexoUsuario[1] == "Mujeres" || input$sexoUsuario[1] == "Hombres", 'Please select at least one gender and update the graph'),
       need(input$biologicalCompUsuario != "Sum", 'Please select at least one biological compartment and update the graph'),
       need(input$edadUsuario[1] >0 && input$edadUsuario[2] <= edades, 'Please select a valid age range and update the graph')
     )
     
     
     #GrÃ¡fica que muestra el total de la poblaciÃ³n para un tipo especÃ�fico de compartimiento
     #TÃ�tulo de la grÃ¡fica
     sexoIndice <- 0
     #entra al primer if si no estÃ¡ seleccionado el sum
     if(length(input$sexoUsuario)==2 && ((input$sexoUsuario[1]=="Mujeres"||input$sexoUsuario[1]=="Hombres")&&(input$sexoUsuario[2]=="Mujeres"||input$sexoUsuario[2]=="Hombres"))){
       titulo <- paste0("Number of Males and Females")
       sexoIndice <- 3
     }else if((length(input$sexoUsuario)==1 && input$sexoUsuario[1]=="Hombres") || (length(input$sexoUsuario)==2 && input$sexoUsuario[1]=="Hombres" && input$sexoUsuario[2]=="sumSexo")){
         titulo <- paste0("Number of Males")
         sexoIndice <- 2
     }else if(length(input$sexoUsuario) == 3){ #entra a este else if si estÃ¡n los tres
       titulo <- paste0("Total number of Males and Females")
       sexoIndice <- 4
     }else if((length(input$sexoUsuario)==1 && input$sexoUsuario[1]=="Mujeres") || (length(input$sexoUsuario)==2 && (input$sexoUsuario[1]=="Mujeres" && input$sexoUsuario[2]=="sumSexo"))){
         titulo <- paste0("Number of Females")
         sexoIndice <- 1
     }else if(length(input$sexoUsuario)==2 && input$sexoUsuario[2]=="Hombres" && input$sexoUsuario[1]=="sumSexo"){
       titulo <- paste0("Number of Males")
       sexoIndice <- 2
     }
     
     #Leer el input de biologicalCompUsuario
     nombresBiolo <- (cargarDatos$bioloGeneral[,1]) 
     nombresBiolo[transM+1] <- "Vaccinated"
     nombresBiolo[transM+2] <- "Sum"
     inputBiolo <- vector()
     filasBiolo <- 0
     
     for(j in 1:length(input$biologicalCompUsuario)){
       for(i in 1:(transM+2)){
         if(input$biologicalCompUsuario[j] == nombresBiolo[i]){
           filasBiolo= filasBiolo + 1
           inputBiolo[filasBiolo] = i
         }
       }
     }
     
     ap <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("year","value", "biologicalCompartment","gender"))
     ayuda <-0
     
     #Sacando el rango de edad
     rangoEdad <- seq(input$edadUsuario[1],input$edadUsuario[2],by = 1)
     
     #Â¿Sum en el biological compartment?
     sumBiolo <- FALSE
     hastaBio <- length(input$biologicalCompUsuario)
     bioloCompSum <- inputBiolo
     for(m in 1:length(bioloCompSum)){
       if(bioloCompSum[m]==(transM + 2)){
         sumBiolo <- TRUE
         hastaBio <- length(input$biologicalCompUsuario)-1
       }
     }
     
     #Sacar los datos que necesito de el resultado de las ecuaciones diferenciales
         for(k in 1:hastaBio) {
           totalTipo = seq(1,input$num,by=1)
           totalTipo1 = seq(1,input$num,by=1)
           for(i in 1:input$num){
             if(sexoIndice==2){
               if(inputBiolo[k] !=(transM+1)){
                 for(j in rangoEdad){
                   totalTipo[i] = totalTipo[i] + data()[i,j+edades*((inputBiolo[k])-1)]
                 }
               }else{
                 for(j in rangoEdad){
                   totalTipo[i] = totalTipo[i] + data()[i,j+2*transM*edades]
                 }
               }
             }else if(sexoIndice==1){
               if(inputBiolo[k] !=(transM+1)){
                 for(j in rangoEdad){
                   totalTipo[i] = totalTipo[i] + data()[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]
                 }
               }else{
                 for(j in rangoEdad){
                   totalTipo[i] = totalTipo[i] + data()[i,j+2*transM*edades+edades]
                 }
               }
             }else if(sexoIndice==4){
               if(inputBiolo[k] !=(transM+1)){
                 for(j in rangoEdad){
                   totalTipo[i] = totalTipo[i] + data()[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]+
                     data()[i,j+edades*((inputBiolo[k])-1)]
                 }
               }else{
                 for(j in rangoEdad){
                   totalTipo[i] = totalTipo[i] + data()[i,j+2*transM*edades+edades] + data()[i,j+2*transM*edades]
                 }
               }
               
             }else if (sexoIndice ==3){
               if(inputBiolo[k] !=(transM+1)){
                 for(j in rangoEdad){
                   totalTipo1[i] = totalTipo1[i] + data()[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]
                 }
                 for(j in rangoEdad){
                   totalTipo[i] = totalTipo[i] + data()[i,j+edades*((inputBiolo[k])-1)]
                 }
               }else{
                 for(j in rangoEdad){
                   totalTipo1[i] = totalTipo1[i] + data()[i,j+2*transM*edades+edades]
                 }
                 for(j in rangoEdad){
                   totalTipo[i] = totalTipo[i] + data()[i,j+2*transM*edades]
                 }
               }
               
             }
             totalTipo[i] = round(totalTipo[i]-i,digits = 2)
             totalTipo1[i] = round(totalTipo1[i]-i, digits = 2)
           }
           
           ayuda <- nrow(ap)
           nombre <- nombresBiolo[inputBiolo[k]]
           
           #Guardando los valores en el data frame para graficar
           
           if(sexoIndice==3){ 
             for(i in 1:input$num){
               ap[i+ayuda,1] = i
               ap[i+ayuda,2] = totalTipo[i]
               ap[i+ayuda,3] = nombre
               ap[i+ayuda,4] = "Men"
             }
             ayuda <- nrow(ap)
             for(i in 1:input$num){
               ap[i+ayuda,1] = i
               ap[i+ayuda,2] = totalTipo1[i]
               ap[i+ayuda,3] = nombre
               ap[i+ayuda,4] = "Women"
             }
           }else{
             for(i in 1:input$num){
               ap[i+ayuda,1] = i
               ap[i+ayuda,2] = totalTipo[i]
               ap[i+ayuda,3] = nombre
               ap[i+ayuda,4] = ""
             }
           }
         }
         
     
      if(sumBiolo==FALSE){
        if(sexoIndice != 3){
          ggideal_point <- ggplot(ap, aes(x=year,y=value/1000000,colour=biologicalCompartment))
        }else{
          ggideal_point <- ggplot(ap, aes(x=year,y=value/1000000,group=interaction(biologicalCompartment,gender),colour=biologicalCompartment,linetype=factor(gender)))
        }
      }else if (sumBiolo == TRUE){
        if(sexoIndice != 3){
          ap2 <- setNames(data.frame(matrix(ncol = 2, nrow = input$num)), c("year","value"))
          ap2[,2]<-0
          for(l in 1:nrow(ap)){
            ap2[ap[l,1],1]=ap[l,1]
            ap2[ap[l,1],2]=ap2[ap[l,1],2]+ap[l,2]
          }
          ggideal_point <- ggplot(ap2, aes(x=year,y=value/1000000, colour="#C9A086"))
        }else{
          ap2 <- setNames(data.frame(matrix(ncol = 3, nrow = 2*input$num)), c("year","value","gender"))
          ap2[,2]<-0
          for(l in 1:nrow(ap)){
            if(ap[l,4]=="Men"){
              ap2[ap[l,1],1]=ap[l,1]
              ap2[ap[l,1],2]=ap2[ap[l,1],2]+ap[l,2]
              ap2[ap[l,1],3]="Men"
            }else if(ap[l,4]=="Women"){
              ap2[ap[l,1]+input$num,1]=ap[l,1]
              ap2[ap[l,1]+input$num,2]=ap2[ap[l,1]+input$num,2]+ap[l,2]
              ap2[ap[l,1]+input$num,3]="Women"
            }
          }
          ggideal_point <- ggplot(ap2, aes(x=year,y=value/1000000,colour=gender))
        }
      }
         
     ggideal_point <- ggideal_point + geom_line() + geom_point(shape=5)+
       labs(x = "Year", y= "Unit: Millions", title = titulo) +
       scale_colour_hue("",l = 70, c = 150) +
       theme(legend.title=element_blank(),legend.text=element_text(size=8),axis.title.y = element_text(size=12), legend.position = c(0.8, 0.2),axis.text.x  = element_text(vjust=0.5, size=10),axis.text.y  = element_text(vjust=0.5, size=10),axis.title.x = element_text(size = 12)) +
       background_grid(major = "xy", minor = "xy",colour.major = "grey90",colour.minor = "grey90") +
       panel_border() + scale_x_continuous(breaks=seq(1, input$num, 1)) 
     
 })
  
   
   #Formato de los ejes
   f1 <- list(size = 13,color = "grey")
   f2 <- list(size = 11,color = "black")
   al <- list(titlefont = f1,showticklabels = TRUE,tickfont = f2,exponentformat = "E")
   #Margenes
   m <- list(l = 50,r = 110,b = 100,t = 50,pad = 4,autoexpand = FALSE)
   
   # Convert ggplot object to plotly
   gg <- plotly_build(ggideal_point)%>%layout(autosize  = FALSE,width = 620, height = 450,margin=m,xaxis = al, yaxis = al)
   gg
   
   
 })
 
 output$grafica <-  renderPlotly({
   
   buildPlot()
   
 })
 
 buildMedidas <- reactive({
   input$changeGraphBtn1
   isolate({
     validate(
       need(length(input$variablesH)>=1 , 'Please select at least one variable to show'),
       need(length(input$indicadoresH)>=1 , 'Please select at least one indicator to show')
     )
     
       bioloGeneral = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",5,dec =",", as.data.frame = TRUE))
       nombresBiolo <- bioloGeneral[,1]
       medidasData <- data.frame(Year = integer(),BiologicalCompartment = character(),Indicator = double(),Value = double(), stringsAsFactors = FALSE)
       #Que pueda hacer el analisis por diferentes tipos de biolo?
       contadorAyuda = 0
       for(j in 1:length(input$variablesH)){
         cancerPos <- which(nombresBiolo == input$variablesH[j])
         desde <- (edades*compart/2)+(cancerPos-1)*edades + 1
         hasta <- (edades*compart/2)+(cancerPos-1)*edades + edades
           for(l in 1:input$num){
             contadorAyuda = contadorAyuda +1
             medidasData[contadorAyuda,4] = round(sum(data()[l,(desde:hasta)])*muerteBiolo[cancerPos,2],digits=2)
             medidasData[contadorAyuda,3] = "Deaths"
             medidasData[contadorAyuda,1] = l
             medidasData[contadorAyuda,2] = input$variablesH[j]
           }
         
           for(l in 1:input$num){
             contadorAyuda = contadorAyuda +1
             medidasData[contadorAyuda,4] = round(sum(data()[l,(desde:hasta)]),digits=2)
             medidasData[contadorAyuda,3] = "Cases"
             medidasData[contadorAyuda,1] = l
             medidasData[contadorAyuda,2] = input$variablesH[j]
           }
         for(l in 1:input$num){
           contadorAyuda = contadorAyuda +1
           medidasData[contadorAyuda,4] = ((sum(data()[l,(desde:hasta)])*muerteBiolo[cancerPos,2])/(sum(data()[l,((edades*compart/2)+1):(edades*compart)])+sum(data()[l,(edades*compart+edades+1):(edades*compart+edades*2)])))*100000*10
           medidasData[contadorAyuda,3] = "Deaths per Million"
           medidasData[contadorAyuda,1] = l
           medidasData[contadorAyuda,2] = input$variablesH[j]
         }
         for(l in 2:(input$num)){
           contadorAyuda = contadorAyuda +1
           medidasData[contadorAyuda,4] = data()[l,edades*(compart+2)+5+cancerPos-nrow(vectorIncidence)]
           medidasData[contadorAyuda,4] = round(medidasData[contadorAyuda,4],digits=2)
           medidasData[contadorAyuda,3] = "Incidence"
           medidasData[contadorAyuda,1] = l
           medidasData[contadorAyuda,2] = input$variablesH[j]
         }
         for(l in 1:input$num){
           contadorAyuda = contadorAyuda +1
           medidasData[contadorAyuda,4] = ((sum(data()[l,(desde:hasta)])*muerteBiolo[cancerPos,2])/(sum(data()[l,((edades*compart/2)+1):(edades*compart)])+sum(data()[l,(edades*compart+edades+1):(edades*compart+edades*2)])))*100000
           medidasData[contadorAyuda,3] = "Death Rate"
           medidasData[contadorAyuda,1] = l
           medidasData[contadorAyuda,2] = input$variablesH[j]
         }
         
       }
       if (!is.null(input$indicadoresH)) {
         medidasData %<>%
           filter(Indicator %in% input$indicadoresH)
       }
   })
   medidasData
 })
 
 buildPlotMedidas <- reactive({
   
   p1 <- ggplot(buildMedidas()) + aes(x = Year, y = Value)
   p1 <- p1 + aes(group = BiologicalCompartment, col = BiologicalCompartment) + facet_wrap(~Indicator, scales = "free_y", ncol = 2) + geom_point() +
     geom_line(show.legend = FALSE) +
     theme_few(11) +
     theme(legend.text=element_text(size=7),axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5), plot.title = element_text(face="bold")) +
     guides(color = guide_legend(title = "", ncol = 1,nrow=4)) +
     xlab("Years") + ylab("") +
     theme(panel.grid.minor = element_line(colour="grey", size=0.25),panel.grid.major.x = element_line(colour="grey", size=0.25),panel.grid.major.y = element_line(colour="grey", size=0.25),legend.title=element_blank())+
     scale_colour_hue("",l = 70, c = 150) 
  
   
   #Formato de los ejes
   f1 <- list(size = 13,color = "grey")
   f2 <- list(size = 11,color = "black")
   al <- list(titlefont = f1,showticklabels = TRUE,tickfont = f2,exponentformat = "E")
   #Margenes
   m <- list(l = 80,r = 100,b = 100,t = 50,pad = 8,autoexpand = FALSE)
   
   # Convert ggplot object to plotly
   gg <- plotly_build(p1)%>%layout(autosize  = FALSE,width = 620, height = 450,margin=m,xaxis = al, yaxis = al)
   gg
   
 })
 
 output$medidasPlot <- renderPlotly({
   buildPlotMedidas()})
 
 buildComparacion <- reactive({
   input$changeGraphBtn2
   isolate({
     validate(
       need(!is.null(input$politica1) || !is.null(input$politica2) || !is.null(input$politica3) || !is.null(input$politica4) , "Please select at least one policy"),
       need(length(input$variablesPoliticaComparar) >=1 , "Please select at least variable to show"),
       need(length(input$indicadoresPoliticaComparar) >=1 , "Please select at least one indicator to show")
     )
     
     bioloGeneral = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",5,dec =",", as.data.frame = TRUE))
     nombresBiolo <- bioloGeneral[,1]
     medidasData <- data.frame(Year = integer(),BiologicalCompartment = character(),Indicator = double(),Value = double(),politica = character(), stringsAsFactors = FALSE)
     #cargando los datos de la polÃ�tica
     if(!is.null(input$politica1)){
       datosPolitica1 <- as.matrix(read.csv(input$politica1$datapath))[,-1]
     }else{
       datosPolitica1 <- matrix(NA,ncol=edades*compart + edades*2 + 5 + nrow(vectorIncidence),nrow=50)
     }
     
     if(!is.null(input$politica2)){
       datosPolitica2 <- as.matrix(read.csv(input$politica2$datapath))[,-1]
     }else{
       datosPolitica2 <- matrix(NA,ncol=edades*compart + edades*2 + 5 + nrow(vectorIncidence),nrow=50)
     }
     
     if(!is.null(input$politica3)){
       datosPolitica3 <- as.matrix(read.csv(input$politica3$datapath))[,-1]
     }else{
       datosPolitica3 <- matrix(NA,ncol=edades*compart + edades*2 + 5 + nrow(vectorIncidence),nrow=50)
     }
     
     if(!is.null(input$politica4)){
       datosPolitica4 <- as.matrix(read.csv(input$politica4$datapath))[,-1]
     }else{
       datosPolitica4 <- matrix(NA,ncol=edades*compart + edades*2 + 5 + nrow(vectorIncidence),nrow=50)
     }
     
     #Que pueda hacer el analisis por diferentes tipos de biolo?
     contadorAyuda = 0
     for(m in 1:4){
       if(m==1){
         datosPoliticaUsar <- datosPolitica1
       }else if (m == 2){
         datosPoliticaUsar <- datosPolitica2
       }else if (m == 3){
         datosPoliticaUsar <- datosPolitica3
       }else{
         datosPoliticaUsar <- datosPolitica4
       }
       
       for(j in 1:length(input$variablesPoliticaComparar)){
         cancerPos <- which(nombresBiolo == input$variablesPoliticaComparar[j])
         desde <- (edades*compart/2)+(cancerPos-1)*edades + 1
         hasta <- (edades*compart/2)+(cancerPos-1)*edades + edades
         for(l in 1:input$num){
           contadorAyuda = contadorAyuda +1
           medidasData[contadorAyuda,4] = round(sum(datosPoliticaUsar[l,(desde:hasta)])*muerteBiolo[cancerPos,2],digits=2)
           medidasData[contadorAyuda,3] = "Deaths"
           medidasData[contadorAyuda,1] = l
           medidasData[contadorAyuda,2] = input$variablesPoliticaComparar[j]
           medidasData[contadorAyuda,5] = paste0("Policy ",m)
         }
         
         for(l in 1:input$num){
           contadorAyuda = contadorAyuda +1
           medidasData[contadorAyuda,4] = round(sum(datosPoliticaUsar[l,(desde:hasta)]),digits=2)
           medidasData[contadorAyuda,3] = "Cases"
           medidasData[contadorAyuda,1] = l
           medidasData[contadorAyuda,2] = input$variablesPoliticaComparar[j]
           medidasData[contadorAyuda,5] = paste0("Policy ",m)
         }
         for(l in 1:input$num){
           contadorAyuda = contadorAyuda +1
           medidasData[contadorAyuda,4] = ((sum(datosPoliticaUsar[l,(desde:hasta)])*muerteBiolo[cancerPos,2])/(sum(datosPoliticaUsar[l,((edades*compart/2)+1):(edades*compart)])+sum(datosPoliticaUsar[l,(edades*compart+edades+1):(edades*compart+edades*2)])))*100000*10
           medidasData[contadorAyuda,3] = "Deaths per Million"
           medidasData[contadorAyuda,1] = l
           medidasData[contadorAyuda,2] = input$variablesPoliticaComparar[j]
           medidasData[contadorAyuda,5] = paste0("Policy ",m)
         }
         for(l in 2:(input$num)){
           contadorAyuda = contadorAyuda +1
           medidasData[contadorAyuda,4] = datosPoliticaUsar[l,edades*(compart+2)+5+cancerPos-nrow(vectorIncidence)]
           medidasData[contadorAyuda,4] = round(medidasData[contadorAyuda,4],digits=2)
           medidasData[contadorAyuda,3] = "Incidence"
           medidasData[contadorAyuda,1] = l
           medidasData[contadorAyuda,2] = input$variablesPoliticaComparar[j]
           medidasData[contadorAyuda,5] = paste0("Policy ",m)
         }
         for(l in 1:input$num){
           contadorAyuda = contadorAyuda +1
           medidasData[contadorAyuda,4] = ((sum(datosPoliticaUsar[l,(desde:hasta)])*muerteBiolo[cancerPos,2])/(sum(datosPoliticaUsar[l,((edades*compart/2)+1):(edades*compart)])+sum(datosPoliticaUsar[l,(edades*compart+edades+1):(edades*compart+edades*2)])))*100000
           medidasData[contadorAyuda,3] = "Death Rate"
           medidasData[contadorAyuda,1] = l
           medidasData[contadorAyuda,2] = input$variablesPoliticaComparar[j]
           medidasData[contadorAyuda,5] = paste0("Policy ",m)
         }
         
       }
     }
     if (!is.null(input$indicadoresPoliticaComparar)) {
       medidasData %<>%
         filter(Indicator %in% input$indicadoresPoliticaComparar)
     }
   })
   
   medidasData <- na.omit(medidasData)
   medidasData
   
 })
 
 buildPlotComparacion <- reactive({
   
   p1 <- ggplot(buildComparacion()) + aes(x = Year, y = Value)
   p1 <- p1 + aes(group = BiologicalCompartment, shape = BiologicalCompartment, col = politica) + facet_wrap(~Indicator, scales = "free_y", ncol = 2) + geom_point() +
     geom_line(show.legend = FALSE) +
     theme_few(11) +
     theme(legend.text=element_text(size=7),axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5), plot.title = element_text(face="bold")) +
     guides(color = guide_legend(title = "", ncol = 1,nrow=4)) +
     xlab("Years") + ylab("") +
     theme(panel.grid.minor = element_line(colour="grey", size=0.25),panel.grid.major.x = element_line(colour="grey", size=0.25),panel.grid.major.y = element_line(colour="grey", size=0.25),legend.title=element_blank())+
     scale_colour_hue("",l = 70, c = 150) 
   
   
   #Formato de los ejes
   f1 <- list(size = 13,color = "grey")
   f2 <- list(size = 11,color = "black")
   al <- list(titlefont = f1,showticklabels = TRUE,tickfont = f2,exponentformat = "E")
   #Margenes
   m <- list(l = 80,r = 130,b = 100,t = 50,pad = 8,autoexpand = FALSE)
   
   # Convert ggplot object to plotly
   gg <- plotly_build(p1)%>%layout(autosize  = FALSE,width = 620, height = 450,margin=m,xaxis = al, yaxis = al)
   gg
   
 })
 
 output$graficaComparacion <- renderPlotly({
   buildPlotComparacion()
 })
 
 output$textoGrafica <- renderUI({
   input$changeGraphBtn
   isolate({
     validate(
       need(length(input$sexoUsuario)>=2 || input$sexoUsuario[1] == "Mujeres" || input$sexoUsuario[1] == "Hombres", ""),
       need(input$biologicalCompUsuario != "Sum", ""),
       need(input$edadUsuario[1] >0 && input$edadUsuario[2] <= edades, "")
     )
     texto = paste("The total vaccination cost is: ", round(sum(data()[1:input$num, edades*(compart+2)+1])))
     texto2 = paste("The total Primary Test cost is: ", round(sum(data()[1:input$num, edades*(compart+2)+2])))
     texto3 = paste0("The total Triage is: ", round(sum(data()[1:input$num, edades*(compart+2)+3])))
     texto4 = paste0("The total Follow Up cost is: ", round(sum(data()[1:input$num, edades*(compart+2)+4])))
     valor = round(sum(data()[1:input$num, edades*(compart+2)+1])) + round(sum(data()[1:input$num, edades*(compart+2)+2])) + round(sum(data()[1:input$num, edades*(compart+2)+3])) +round(sum(data()[1:input$num, edades*(compart+2)+4]))
     texto5 = paste0("The total cost is: ", valor)
     texto6 = paste0("The total effectiveness is: ", round(sum(data()[1:input$num, edades*(compart+2)+5])))
   })
   HTML(paste(texto, texto2,texto3,texto4,texto5,texto6,sep = '<br/>'))
   
 })
 

 #---------------------------------------------Tablas----------------------------------------
  
 buildTable <- reactive({
   input$changeGraphBtn 
   
   #ValidaciÃ³n de los datos
   
   isolate({
     validate(
       need(length(input$sexoUsuario)>=2 || input$sexoUsuario[1] == "Mujeres" || input$sexoUsuario[1] == "Hombres", 'Please select at least one gender and update the graph'),
       need(input$biologicalCompUsuario != "Sum", 'Please select at least one biological compartment and update the table'),
       need(input$edadUsuario[1] >0 && input$edadUsuario[2] <= edades, 'Please select a valid age range and update the table')
     )
     
     sexoIndice <- 0
     input$sexoUsuario
     #entra al primer if si no estÃ¡ seleccionado el sum
     if(length(input$sexoUsuario)==2 && ((input$sexoUsuario[1]=="Mujeres"||input$sexoUsuario[1]=="Hombres")&&(input$sexoUsuario[2]=="Mujeres"||input$sexoUsuario[2]=="Hombres"))){
       sexoIndice <- 3
     }else if((length(input$sexoUsuario)==1 && input$sexoUsuario[1]=="Hombres") || (length(input$sexoUsuario)==2 && input$sexoUsuario[1]=="Hombres" && input$sexoUsuario[2]=="sumSexo")){
       sexoIndice <- 2
     }else if(length(input$sexoUsuario) == 3){ #entra a este else if si estÃ¡n los tres
       sexoIndice <- 4
     }else if((length(input$sexoUsuario)==1 && input$sexoUsuario[1]=="Mujeres") || (length(input$sexoUsuario)==2 && (input$sexoUsuario[1]=="Mujeres" && input$sexoUsuario[2]=="sumSexo"))){
       sexoIndice <- 1
     }else if(length(input$sexoUsuario)==2 && input$sexoUsuario[2]=="Hombres" && input$sexoUsuario[1]=="sumSexo"){
       sexoIndice <- 2
     }
     
     if(sexoIndice == 3){
        ap <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Year","Value", "Biological Comp.","Gender"))
     }else{
       ap <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Year","Value", "Biological Comp."))
     }
     
     #Leer el input de biologicalCompUsuario
     nombresBiolo <- (cargarDatos$bioloGeneral[,1]) 
     nombresBiolo[transM+1] <- "Vaccinated"
     nombresBiolo[transM+2] <- "Sum"
     inputBiolo <- vector()
     filasBiolo <- 0
     
     for(j in 1:length(input$biologicalCompUsuario)){
       for(i in 1:(transM+2)){
         if(input$biologicalCompUsuario[j] == nombresBiolo[i]){
           filasBiolo= filasBiolo + 1
           inputBiolo[filasBiolo] = i
         }
       }
     }
     
     
     ayuda <-0
     
     rangoEdad <- seq(input$edadUsuario[1],input$edadUsuario[2],by = 1)
     
     #Â¿Sum en el biological compartment?
     sumBiolo <- FALSE
     hastaBio <- length(input$biologicalCompUsuario)
     bioloCompSum <- inputBiolo
     for(m in 1:length(bioloCompSum)){
       if(bioloCompSum[m]==(transM + 2)){
         sumBiolo <- TRUE
         hastaBio <- length(input$biologicalCompUsuario)-1
       }
     }
     
     #Sacar los datos que necesito de el resultado de las ecuaciones diferenciales
     for(k in 1:hastaBio) {
       totalTipo = seq(1,input$num,by=1)
       totalTipo1 = seq(1,input$num,by=1)
       for(i in 1:input$num){
         if(sexoIndice==2){
           if(inputBiolo[k] !=(transM+1)){
             for(j in rangoEdad){
               totalTipo[i] = totalTipo[i] + data()[i,j+edades*((inputBiolo[k])-1)]
             }
           }else{
             for(j in rangoEdad){
               totalTipo[i] = totalTipo[i] + data()[i,j+2*transM*edades]
             }
           }
         }else if(sexoIndice==1){
           if(inputBiolo[k] !=(transM+1)){
             for(j in rangoEdad){
               totalTipo[i] = totalTipo[i] + data()[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]
             }
           }else{
             for(j in rangoEdad){
               totalTipo[i] = totalTipo[i] + data()[i,j+2*transM*edades+edades]
             }
           }
         }else if(sexoIndice==4){
           if(inputBiolo[k] !=(transM+1)){
             for(j in rangoEdad){
               totalTipo[i] = totalTipo[i] + data()[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]+
                 data()[i,j+edades*((inputBiolo[k])-1)]
             }
           }else{
             for(j in rangoEdad){
               totalTipo[i] = totalTipo[i] + data()[i,j+2*transM*edades+edades] + data()[i,j+2*transM*edades]
             }
           }
           
         }else if (sexoIndice ==3){
           if(inputBiolo[k] !=(transM+1)){
             for(j in rangoEdad){
               totalTipo1[i] = totalTipo1[i] + data()[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]
             }
             for(j in rangoEdad){
               totalTipo[i] = totalTipo[i] + data()[i,j+edades*((inputBiolo[k])-1)]
             }
           }else{
             for(j in rangoEdad){
               totalTipo1[i] = totalTipo1[i] + data()[i,j+2*transM*edades+edades]
             }
             for(j in rangoEdad){
               totalTipo[i] = totalTipo[i] + data()[i,j+2*transM*edades]
             }
           }
           
         }
         totalTipo[i] = round(totalTipo[i]-i,digits = 2)
         totalTipo1[i] = round(totalTipo1[i]-i, digits = 2)
       }
     
       ayuda <- nrow(ap)
       
       nombre <- nombresBiolo[inputBiolo[k]]
       
       #Guardando los valores en el data frame para graficar
       
       if(sexoIndice==3){ #si entra al if es porque sum estÃ¡ seleccionado
         for(i in 1:input$num){
           ap[i+ayuda,1] = i
           ap[i+ayuda,2] = totalTipo[i]
           ap[i+ayuda,3] = nombre
           ap[i+ayuda,4] = "Men"
         }
         ayuda <- nrow(ap)
         for(i in 1:input$num){
           ap[i+ayuda,1] = i
           ap[i+ayuda,2] = totalTipo1[i]
           ap[i+ayuda,3] = nombre
           ap[i+ayuda,4] = "Women"
         }
       }else{
         for(i in 1:input$num){
           ap[i+ayuda,1] = i
           ap[i+ayuda,2] = totalTipo[i]
           ap[i+ayuda,3] = nombre
         }
       }
     }
     
   }) 
   
   ap
 }) 
 
 output$dataTable <-  renderDataTable({
    buildTable()
  })
 
 output$dataTableMedidas <-  renderDataTable({
   buildMedidas()
 })
 
 
 #--------Descargar datoooos-------------------
 output$downloadData <- downloadHandler(
   filename = "Cervical Cancer Simulation",
   content = function(file) {
     write.csv(data(), file)
   }
 )
 
 output$downloadIndicatorsPlot <- downloadHandler(
   filename = function() {
     "cancerDataPlot.pdf"
   },
   
   content = function(file) {
     pdf(file = file,
         width = 10,
         height = 10)
     print(buildPlotMedidas())
     dev.off()
   })
 
 output$downloadIndicators <- downloadHandler(
   filename = "Cervical Cancer Simulation",
   content = function(file) {
     write.csv(buildMedidas(), file)
   }
 )
 
 
 

}



