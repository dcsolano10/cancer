pkgTest <- function(x){
  if (!require(x,character.only = TRUE)){
    install.packages(x,dep=TRUE)
  }
}

pkgTest("shinythemes")
library(shinythemes)

fluidPage( 
  tags$head(tags$style(HTML(".multicol{font-size:14px;
                                                  height:auto;
                            -webkit-column-count: 2;
                            -moz-column-count: 2;
                            column-count: 2;
                            }
                            
                            div.checkbox {margin-top: 0px;}"))),
  #Cargar el tema
  theme = shinytheme("lumen"),
  br(),
  #Crear la interfaz
  
  div(id = 'placeholder1'),
  div(id = 'placeholder',
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
  ,style="padding-left: 35px;",style="padding-right: 35px;"),
  
  hr(),
  div(fluidRow(column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50)),
           column(1,img(src= "ccImage.png",height=50,width=50))
  ))
)

