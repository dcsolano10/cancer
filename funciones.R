
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

args <- commandArgs(TRUE)
vacRangeFemaleIni<-as.double(args[1])
vacRangeFemaleEnd<-as.double(args[2])
vacRangeMaleIni<-as.double(args[3])
vacRangeMaleEnd<-as.double(args[4])
vacYearsFemaleIni<-as.double(args[5])
vacYearsFemaleEnd<-as.double(args[6])
vacYearsMaleIni<-as.double(args[7])
vacYearsMaleEnd<-as.double(args[8])
if(args[9]==1){
  vacSex<-c("Mujeres") 
}else{
  vacSex<-c("Hombres","Mujeres")
}
vacPorcentageFemale<-as.double(args[10])
vacPorcentageMale<-as.double(args[11])
primaryTest<- args[12]
iniPrimaryTest<-as.double(args[13])
maxPrimaryTest<-as.double(args[14]) 
stepPrimaryTest<-as.double(args[15])
triageTest<- args[16]
iniTriage<-as.double(args[17])
maxTriage<-as.double(args[18])
stepTriage<-as.double(args[19])
followUp<-args[20]
timeFollowUp<-as.double(args[21])
nomArch<-args[22]

#-----Leer datos---------------------------------------------------------------------------
  nacimientos = as.matrix(read.xlsx("ParametrosGenerales.xlsx",2, dec =",", as.data.frame = TRUE))
  parametros = as.matrix(read.xlsx("ParametrosGenerales.xlsx",1, dec =",", as.data.frame = TRUE))
  transicionesM = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",3, dec =",", as.data.frame = TRUE))
  transicionesH = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",4, dec =",", as.data.frame = TRUE))
  otros = as.matrix(read.table("generales.txt", header = FALSE, dec=","))
  bioloGeneral = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",5,dec =",", as.data.frame = TRUE))
  parejasSexuales = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",6,dec =",", as.data.frame = TRUE))
  sensibilidad = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",7,dec =",", as.data.frame = TRUE))
  cobertura = as.matrix(read.xlsx2("ParametrosGenerales.xlsx",8,dec =",", as.data.frame = TRUE))

#----Generar los nuevos datos-------------------------------------------------------------

  
  #Rango de la simulaci�³n
  simu <- 50
  #G�©neros
  gen = 2
  #Leyendo los dataFrames
  transicionesM <- as.matrix.data.frame(transicionesM[,-ncol(transicionesM)])
  transicionesH <- as.matrix.data.frame(transicionesH[,-ncol(transicionesH)])
  parametros <- as.matrix.data.frame(parametros[,-ncol(parametros)])
  nacimientos <- as.matrix.data.frame(nacimientos[,-ncol(nacimientos)])
  
  
  #Cargar datos del archivo otros
  compart <- as.numeric(otros[1,1])
  nacDesde = as.numeric(otros[4,1])
  nacHasta = as.numeric(otros[5,1])
  muerteBiolo = cbind(as.numeric(bioloGeneral[,2]),as.numeric(bioloGeneral[,3]))
  edadesFert = nacHasta-nacDesde
  te <- as.numeric(otros[2,1])
  probMujer <- as.numeric(otros[3,1])
  edades <- (ncol(parametros))/compart
  
  
  #Cargar datos de las matrices de transiciÃ³nd de probabilidad
  transM <- as.numeric(otros[6,1])
  transH <- as.numeric(otros[7,1])
  #vector que carga toda la informaciÃ³n de los estadÃ�os biolÃ³gicos
  vectorT <- cbind(seq(1,transM,by=1),bioloGeneral[,4],bioloGeneral[,5],bioloGeneral[,6])
  #Vector que hace los suscpetibles
  vectorS <- subset(vectorT,vectorT[,2]=="S")
  vectorPI <- subset(vectorT,vectorT[,2]=="PI")
  vectorSPI <- rbind(vectorS,vectorPI)
  
  #Vector que carga la info de los estados biologicos
  vectorT <- cbind(seq(1,transM,by=1),bioloGeneral[,4])
  vectorI <- subset(vectorT,vectorT[,2]=="I")
  vectorLP <- subset(vectorT, vectorT[,2]=="LP")
  vectorC <- subset(vectorT, vectorT[,2]=="C")
  vectorILPC <- rbind(vectorI,vectorLP,vectorC)
  vectorIncidence <- rbind(vectorLP,vectorC)
  
  #Nombres Progress
  vectorNT <- cbind(bioloGeneral[,1],bioloGeneral[,4])
  vectorNombresP <- subset(vectorNT[,1], vectorNT[,2] == "LP" | vectorNT[,2] == "C")
  
  #Cargar Datos de parejas sexuales
  parejasSexualesM <- cbind(parejasSexuales[,1],parejasSexuales[,3])
  parejasSexualesH <- cbind(parejasSexuales[,1],parejasSexuales[,2])
  
  #Cargar datos para el screening
  coberturaTamizaje <- cbind(as.numeric(cobertura[,1]),as.numeric(cobertura[,2]),as.numeric(cobertura[,3]),as.numeric(cobertura[,4]))
  sensibilidadPrubeas <- cbind(sensibilidad[,1],as.numeric(sensibilidad[,2]),as.numeric(sensibilidad[,3]))
  
  #Valor para costos
  costoVacuna = as.numeric(otros[8,1])
  
 
  L=c(1:simu)
  n.init  <- 0.01
  Kd.init <- 0.01
  
  generarTasasMuerte <- function(){
    #Generar vectores de muertes y fertilidad
    tasaMuerteTotales<- matrix(seq(1),nrow = simu,ncol=edades*compart)
    
    for(i in 1:(ncol(parametros))){
      Human <- as.double(parametros[-(1),i])
      # fitting suavizamiento exponencial
      tasaMuerteiFuturo <- forecast(HoltWinters(Human, beta=FALSE, gamma=FALSE),h=9)
      for (j in 1:simu){
        tasaMuerteTotales[j,i] = tasaMuerteiFuturo$mean[1]
      }
    }
    return(tasaMuerteTotales)
  }
  tasaMuerteTotales<-generarTasasMuerte()
  
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
  generarTasasNacimiento <- function(){
    tasaNacimientosTotales<- matrix(seq(1),nrow = simu,ncol=ncol(nacimientos))
    # fitting Suavizamiento exponencial
    for(i in 1:(ncol(nacimientos))){
      nac <- as.numeric(nacimientos[,i])
      tasaNacimientoiFuturo <- forecast(HoltWinters(nac, beta=FALSE, gamma=FALSE),h=9)
      for (j in 1:simu){
        tasaNacimientosTotales[j,i] =  tasaNacimientoiFuturo$mean[1]
      }
    }
    return(tasaNacimientosTotales)
  }

  tasaNacimientosTotales<-generarTasasNacimiento() 
  #----Resolver las ecuaciones diferenciales------------------------------------------------
  
 data <- function(vacRangeFemaleIni,vacRangeFemaleEnd,vacRangeMaleIni,vacRangeMaleEnd, vacYearsFemaleIni,vacYearsFemaleEnd, vacYearsMaleIni,vacYearsMaleEnd, vacSex, vacPorcentageFemale, vacPorcentageMale, primaryTest, iniPrimaryTest, maxPrimaryTest, stepPrimaryTest, triageTest, iniTriage, maxTriage, stepTriage,followUp, timeFollowUp){
    
    a = 50;
    nacimientosTotales = 0;
    
    #Porcentaje de vacunaciÃ³n
    vacMujeres = matrix(0, ncol = edades , nrow = 50)
    vacHombres = matrix(0, ncol = edades , nrow = 50)
    #Edades de vacunaciÃ³n
    edadesVacFemale <- seq(vacRangeFemaleIni,vacRangeFemaleEnd,by=1)
    edadesVacMale <- seq(vacRangeMaleIni,vacRangeMaleEnd,by=1)
    #AÃ±os de VacunaciÃ³n
    yearsVacFemale  <- seq(vacYearsFemaleIni,vacYearsFemaleEnd,by=1)
    yearsVacMale <- seq(vacYearsMaleIni,vacYearsMaleEnd,by=1)
    
    
    if(length(vacSex) == 2){
      for(i in 1:50){
        for(j in 1:edades){
          if(j %in% edadesVacFemale && i %in% yearsVacFemale){
            vacMujeres[i,j] = vacPorcentageFemale
          }
          if(j %in% edadesVacMale && i %in% yearsVacMale){
            vacHombres[i,j] = vacPorcentageMale
          }
        }
      }
    }else if(length(vacSex) == 1 && vacSex == "Mujeres"){
      for(i in 1:50){
        for(j in 1:edades){
          if(j %in% edadesVacFemale && i %in% yearsVacFemale){
            vacMujeres[i,j] = vacPorcentageFemale
          }
        }
      }
    }else if(length(vacSex) == 1 && vacSex == "Hombres"){
      for(i in 1:50){
        for(j in 1:edades){
          if(j %in% edadesVacMale && i %in% yearsVacMale){
            vacHombres[i,j] = vacPorcentageMale
          }
        }
      }
    }
    
    
    #----------Vectores para screening-------------------
    xij <- rep(0,times=edades)
    triageij <- rep(0,times=edades)
    folowUpHay <- 0
    seguimiento <- followUp
    sensibilidadPrimary = 0
    sensibilidadTriage = 0
    sensibilidadFollowUp = 0
    costoPrimary = 0
    costoTriage = 0
    costoFollowUp = 0
    contadorPrimi2 = 0
    contadorPrimi = 1
    if(primaryTest != "None"){
      for(i in iniPrimaryTest:maxPrimaryTest){
        contadorPrimi2 = contadorPrimi2+1
        xij[i] = contadorPrimi
        contadorPrimi = 0
        if(contadorPrimi2%%stepPrimaryTest == 0){
          contadorPrimi = 1
        }
      }
      contadorPrimi2 = 0
      contadorPrimi = 1
      if(triageTest != "None"){
        for(i in iniTriage:maxTriage){
          contadorPrimi2 = contadorPrimi2+1
          triageij[i] = contadorPrimi
          contadorPrimi = 0
          if(contadorPrimi2%%stepTriage == 0){
            contadorPrimi = 1
          }
        }
      }
      
    }
    
    for(i in 1:(length(sensibilidadPrubeas)/3)){
      if(primaryTest != "None" && primaryTest == sensibilidadPrubeas[i,1]){
        sensibilidadPrimary = as.numeric(sensibilidadPrubeas[i,2])
        costoPrimary = as.numeric(sensibilidadPrubeas[i,3])
      }
      if(triageTest != "None" && triageTest == sensibilidadPrubeas[i,1]){
        sensibilidadTriage = as.numeric(sensibilidadPrubeas[i,2])
        costoTriage = as.numeric(sensibilidadPrubeas[i,3])
      }
      if(followUp != "None" && followUp == sensibilidadPrubeas[i,1]){
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
                    ((timeFollowUp/12))*sensibilidadFollowUp*folowUpHay*sum(y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))+
                    ((timeFollowUp/12))*folowUpHay*(1-sensibilidadFollowUp*folowUpHay)*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1]))))+(1-((timeFollowUp/12)))*folowUpHay*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))
                  
                }else if(any(contador == transM + as.numeric(vectorI[,1]))){
                  dH[i+j] = te*(sum(as.numeric(newTransM[seq1[vectorNoProgressNoSusc],contador-compart/2])*(y[seqState[vectorNoProgressNoSusc]]*(1-tasaMuerteTotales[t,seqState[vectorNoProgressNoSusc]]-muerteBiolo[contador-transM,2])))) -(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j] -te*(y[i+j]-(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j])+
                    te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary*(triageij[i-1]*(1-sensibilidadTriage)))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    (((timeFollowUp/12))*folowUpHay*(1-sensibilidadFollowUp*folowUpHay))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    ((1-(timeFollowUp/12))*folowUpHay)*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1]))))+((1-((timeFollowUp/12)))*folowUpHay)*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))
                  
                }else if(any(contador == transM + as.numeric(vectorLP[,1]))){
                  vectorProgressc <-seq(1,transM, by = 1)[c(as.numeric(vectorC[,1]))]
                  dH[i+j] = te*(sum(as.numeric(newTransM[seq1[vectorNoProgressNoSusc],contador-compart/2])*(y[seqState[vectorNoProgressNoSusc]]*(1-tasaMuerteTotales[t,seqState[vectorNoProgressNoSusc]]-muerteBiolo[contador-transM,2])))) -(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j] -te*(y[i+j]-min(1,(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2]))*y[i+j])+
                    te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*(as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary)*(triageij[i-1])*(1-sensibilidadTriage)*(1-folowUpHay))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    (((timeFollowUp/12))*folowUpHay*(1-folowUpHay*sensibilidadFollowUp))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1])))) 
                  
                  auxiliarCostoPrimary = auxiliarCostoPrimary + xij[i]*as.numeric(coberturaTamizaje[i,2])*y[i+j]
                  auxiliarCostoTriage = auxiliarCostoPrimary + xij[i]*as.numeric(coberturaTamizaje[i,2])*sensibilidadPrimary*triageij[i]*y[i+j]
                  auxiliarCostoFollowUp = auxiliarCostoFollowUp + xij[i]*as.numeric(coberturaTamizaje[i,2])*sensibilidadPrimary*triageij[i]*sensibilidadTriage*y[i+j]
                  auxiliarEfectividad = auxiliarEfectividad + xij[i]*as.numeric(coberturaTamizaje[i,2])*sensibilidadPrimary*y[i+j]*as.numeric(coberturaTamizaje[i,4])
                  auxiliarIncidence[contador-transM - nrow(vectorIncidence)] = auxiliarIncidence[contador-transM - nrow(vectorIncidence)] + te*(sum(as.numeric(newTransM[seq1[vectorNoProgressNoSusc],contador-compart/2])*(y[seqState[vectorNoProgressNoSusc]]*(1-tasaMuerteTotales[t,seqState[vectorNoProgressNoSusc]]-muerteBiolo[contador-transM,2])))) + 
                    te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2])) +
                    te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*(as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary)*(triageij[i-1])*(1-sensibilidadTriage)*(1-folowUpHay))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    (((timeFollowUp/12))*folowUpHay*(1-folowUpHay*sensibilidadFollowUp))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1])))) 
                  
                }else if(any(contador == transM + as.numeric(vectorC[,1]))){
                  vectorProgressc <-seq(1,transM, by = 1)[c(as.numeric(vectorC[,1]))]
                  dH[i+j] = te*(sum(as.numeric(newTransM[seq1[vectorProgressc],contador-compart/2])*(y[seqState[vectorProgressc]]*(1-tasaMuerteTotales[t,seqState[vectorProgressc]]-muerteBiolo[contador-transM,2])))) -(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2])*y[i+j] -te*(y[i+j]-min(1,(tasaMuerteTotales[t,i+j]+muerteBiolo[contador-transM,2]))*y[i+j])+
                    te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary*triageij[i-1]*(1-sensibilidadTriage))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))
                  te*(((timeFollowUp/12))*folowUpHay*(1-folowUpHay*sensibilidadFollowUp))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(sum(as.numeric(newTransM[seq1[vectorSusc],contador-compart/2])*(y[seqState[vectorSusc]]*(1-tasaMuerteTotales[t,seqState[vectorSusc]]-muerteBiolo[contador-transM,2]-vacMujeres[t,i-1])))+ te*((1-(timeFollowUp/12))*folowUpHay)*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2])))
                  
                  auxiliarIncidence[contador-transM - nrow(vectorIncidence)] = auxiliarIncidence[contador-transM - nrow(vectorIncidence)] + te*(sum(as.numeric(newTransM[seq1[vectorProgressc],contador-compart/2])*(y[seqState[vectorProgressc]]*(1-tasaMuerteTotales[t,seqState[vectorProgressc]]-muerteBiolo[contador-transM,2]))))+
                    te*(xij[i-1]*(1-as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(1-xij[i-1])*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))+
                    te*(xij[i-1]*as.numeric(coberturaTamizaje[i-1,2])*sensibilidadPrimary*triageij[i-1]*(1-sensibilidadTriage))*sum((y[seqState[vectorProgress]]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))
                  te*(((timeFollowUp/12))*(1-folowUpHay*sensibilidadFollowUp))*sum((y[seqDetected]*(1-tasaMuerteTotales[t,seqState[vectorProgress]]-muerteBiolo[contador-transM,2]))*as.numeric(newTransM[seq1[vectorProgress],contador-compart/2]))
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
            dvPD[i+edades*(j-1)] = te*(((timeFollowUp/12)))*sum((y[seqDetected])*as.numeric(newTransM[seq1[vectorProgress],as.numeric(vectorLP[,1])]))+
              (te*(1-(tasaMuerteTotales[t,i+(as.numeric(vectorLP[j,1])-1)*edades]+muerteBiolo[as.numeric(vectorLP[j,1]),2]))*sum((y[seqState[vectorProgress]])*as.numeric(newTransM[seq1[vectorProgress],as.numeric(vectorLP[j,1])])))*(xij[i-1]*as.numeric(coberturaTamizaje[edades-1,2])*sensibilidadPrimary*triageij[i-1]*(1-sensibilidadTriage)*folowUpHay)-
              te*(((timeFollowUp/12))*folowUpHay)*y[compart*edades+edades*2+i+edades*(j-1)]*(1-(tasaMuerteTotales[t,i+as.numeric(vectorLP[j,1])*edades]+muerteBiolo[as.numeric(vectorLP[j,1]),2]))-
              te*y[compart*edades+edades*2+i+edades*(j-1)]*((tasaMuerteTotales[t,i+(as.numeric(vectorLP[j,1])-1)*edades]+muerteBiolo[as.numeric(vectorLP[j,1]),2]))+te*((1-((timeFollowUp/12)))*folowUpHay)*y[compart*edades+edades*2+i+edades*(j-1)]*(1-(tasaMuerteTotales[t,i+(as.numeric(vectorLP[ayudaLP,1])-1)*edades]+muerteBiolo[as.numeric(vectorLP[ayudaLP,1]),2]))
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
    
  }

 # data(vacRangeFemaleIni,vacRangeFemaleEnd,vacRangeMaleIni,vacRangeMaleEnd, vacYearsFemaleIni,vacYearsFemaleEnd, vacYearsMaleIni,vacYearsMaleEnd, vacSex, vacPorcentageFemale, vacPorcentageMale, primaryTest, iniPrimaryTest, maxPrimaryTest, stepPrimaryTest, triageTest, iniTriage, maxTriage, stepTriage,followUp, timeFollowUp)
 #dat<- data(15,30,15,30,5,10,5,10,c("Hombres","Mujeres"),0.15,0.15,"HPV-DNA", 25,65,5,"Citology",25,65,5,"Citology",6)
 dat<- data(vacRangeFemaleIni,vacRangeFemaleEnd,vacRangeMaleIni,vacRangeMaleEnd, vacYearsFemaleIni,vacYearsFemaleEnd, vacYearsMaleIni,vacYearsMaleEnd, vacSex, vacPorcentageFemale, vacPorcentageMale, primaryTest, iniPrimaryTest, maxPrimaryTest, stepPrimaryTest, triageTest, iniTriage, maxTriage, stepTriage,followUp, timeFollowUp)
 
  #----------------------------GrÃ¡ficas-----------------------------------------------------
  
  buildPlot <- function(sexoUsuario, biologicalCompUsuario, edadUsuario, num){
    
      
      #GrÃ¡fica que muestra el total de la poblaciÃ³n para un tipo especÃ�fico de compartimiento
      #TÃ�tulo de la grÃ¡fica
      sexoIndice <- 0
      #entra al primer if si no estÃ¡ seleccionado el sum
      if(length(sexoUsuario)==2 && ((sexoUsuario[1]=="Mujeres"||sexoUsuario[1]=="Hombres")&&(sexoUsuario[2]=="Mujeres"||sexoUsuario[2]=="Hombres"))){
        titulo <- paste0("Number of Males and Females")
        sexoIndice <- 3
      }else if((length(sexoUsuario)==1 && sexoUsuario[1]=="Hombres") || (length(sexoUsuario)==2 && sexoUsuario[1]=="Hombres" && sexoUsuario[2]=="sumSexo")){
        titulo <- paste0("Number of Males")
        sexoIndice <- 2
      }else if(length(sexoUsuario) == 3){ #entra a este else if si estÃ¡n los tres
        titulo <- paste0("Total number of Males and Females")
        sexoIndice <- 4
      }else if((length(sexoUsuario)==1 && sexoUsuario[1]=="Mujeres") || (length(sexoUsuario)==2 && (sexoUsuario[1]=="Mujeres" && sexoUsuario[2]=="sumSexo"))){
        titulo <- paste0("Number of Females")
        sexoIndice <- 1
      }else if(length(sexoUsuario)==2 && sexoUsuario[2]=="Hombres" && sexoUsuario[1]=="sumSexo"){
        titulo <- paste0("Number of Males")
        sexoIndice <- 2
      }
      
      #Leer el input de biologicalCompUsuario
      nombresBiolo <- (bioloGeneral[,1]) 
      nombresBiolo[transM+1] <- "Vaccinated"
      nombresBiolo[transM+2] <- "Sum"
      inputBiolo <- vector()
      filasBiolo <- 0
      
      for(j in 1:length(biologicalCompUsuario)){
        for(i in 1:(transM+2)){
          if(biologicalCompUsuario[j] == nombresBiolo[i]){
            filasBiolo= filasBiolo + 1
            inputBiolo[filasBiolo] = i
          }
        }
      }
      
      ap <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("year","value", "biologicalCompartment","gender"))
      ayuda <-0
      
      #Sacando el rango de edad
      rangoEdad <- seq(edadUsuario[1],edadUsuario[2],by = 1)
      
      #Â¿Sum en el biological compartment?
      sumBiolo <- FALSE
      hastaBio <- length(biologicalCompUsuario)
      bioloCompSum <- inputBiolo
      for(m in 1:length(bioloCompSum)){
        if(bioloCompSum[m]==(transM + 2)){
          sumBiolo <- TRUE
          hastaBio <- length(biologicalCompUsuario)-1
        }
      }
      
      #Sacar los datos que necesito del resultado de las ecuaciones diferenciales
      for(k in 1:hastaBio) {
        totalTipo = seq(1,num,by=1)
        totalTipo1 = seq(1,num,by=1)
        for(i in 1:num){
          if(sexoIndice==2){
            if(inputBiolo[k] !=(transM+1)){
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+edades*((inputBiolo[k])-1)]
              }
            }else{
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+2*transM*edades]
              }
            }
          }else if(sexoIndice==1){
            if(inputBiolo[k] !=(transM+1)){
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]
              }
            }else{
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+2*transM*edades+edades]
              }
            }
          }else if(sexoIndice==4){
            if(inputBiolo[k] !=(transM+1)){
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]+
                  dat[i,j+edades*((inputBiolo[k])-1)]
              }
            }else{
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+2*transM*edades+edades] + dat[i,j+2*transM*edades]
              }
            }
            
          }else if (sexoIndice ==3){
            if(inputBiolo[k] !=(transM+1)){
              for(j in rangoEdad){
                totalTipo1[i] = totalTipo1[i] + dat[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]
              }
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+edades*((inputBiolo[k])-1)]
              }
            }else{
              for(j in rangoEdad){
                totalTipo1[i] = totalTipo1[i] + dat[i,j+2*transM*edades+edades]
              }
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+2*transM*edades]
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
          for(i in 1:num){
            ap[i+ayuda,1] = i
            ap[i+ayuda,2] = totalTipo[i]
            ap[i+ayuda,3] = nombre
            ap[i+ayuda,4] = "Men"
          }
          ayuda <- nrow(ap)
          for(i in 1:num){
            ap[i+ayuda,1] = i
            ap[i+ayuda,2] = totalTipo1[i]
            ap[i+ayuda,3] = nombre
            ap[i+ayuda,4] = "Women"
          }
        }else{
          for(i in 1:
              num){
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
          ap2 <- setNames(data.frame(matrix(ncol = 2, nrow = num)), c("year","value"))
          ap2[,2]<-0
          for(l in 1:nrow(ap)){
            ap2[ap[l,1],1]=ap[l,1]
            ap2[ap[l,1],2]=ap2[ap[l,1],2]+ap[l,2]
          }
          ggideal_point <- ggplot(ap2, aes(x=year,y=value/1000000, colour="#C9A086"))
        }else{
          ap2 <- setNames(data.frame(matrix(ncol = 3, nrow = 2*num)), c("year","value","gender"))
          ap2[,2]<-0
          for(l in 1:nrow(ap)){
            if(ap[l,4]=="Men"){
              ap2[ap[l,1],1]=ap[l,1]
              ap2[ap[l,1],2]=ap2[ap[l,1],2]+ap[l,2]
              ap2[ap[l,1],3]="Men"
            }else if(ap[l,4]=="Women"){
              ap2[ap[l,1]+num,1]=ap[l,1]
              ap2[ap[l,1]+num,2]=ap2[ap[l,1]+num,2]+ap[l,2]
              ap2[ap[l,1]+num,3]="Women"
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
        panel_border() + scale_x_continuous(breaks=seq(1, num, 1)) 
      
    
    
    
    #Formato de los ejes
    f1 <- list(size = 13,color = "grey")
    f2 <- list(size = 11,color = "black")
    al <- list(titlefont = f1,showticklabels = TRUE,tickfont = f2,exponentformat = "E")
    #Margenes
    m <- list(l = 50,r = 110,b = 100,t = 50,pad = 4,autoexpand = FALSE)
    
    # Convert ggplot object to plotly
    gg <- plotly_build(ggideal_point)%>%layout(autosize  = FALSE,width = 620, height = 450,margin=m,xaxis = al, yaxis = al)
    gg
  }
  
  buildPlot(c("Mujeres","Hombres"), bioloGeneral[,1],c(5,35),30)
  
  #---------------------------------------------Tablas----------------------------------------
  
  buildTable <- function(sexoUsuario, biologicalCompUsuario, edadUsuario, num){

    #ValidaciÃ³n de los datos
    
      validate(
        need(length(sexoUsuario)>=2 || sexoUsuario[1] == "Mujeres" || sexoUsuario[1] == "Hombres", 'Please select at least one gender and update the graph'),
        need(biologicalCompUsuario != "Sum", 'Please select at least one biological compartment and update the table'),
        need(edadUsuario[1] >0 && edadUsuario[2] <= edades, 'Please select a valid age range and update the table')
      )
      
      sexoIndice <- 0
      sexoUsuario
      #entra al primer if si no estÃ¡ seleccionado el sum
      if(length(sexoUsuario)==2 && ((sexoUsuario[1]=="Mujeres"||sexoUsuario[1]=="Hombres")&&(sexoUsuario[2]=="Mujeres"||sexoUsuario[2]=="Hombres"))){
        sexoIndice <- 3
      }else if((length(sexoUsuario)==1 && sexoUsuario[1]=="Hombres") || (length(sexoUsuario)==2 && sexoUsuario[1]=="Hombres" && sexoUsuario[2]=="sumSexo")){
        sexoIndice <- 2
      }else if(length(sexoUsuario) == 3){ #entra a este else if si estÃ¡n los tres
        sexoIndice <- 4
      }else if((length(sexoUsuario)==1 && sexoUsuario[1]=="Mujeres") || (length(sexoUsuario)==2 && (sexoUsuario[1]=="Mujeres" && sexoUsuario[2]=="sumSexo"))){
        sexoIndice <- 1
      }else if(length(sexoUsuario)==2 && sexoUsuario[2]=="Hombres" && sexoUsuario[1]=="sumSexo"){
        sexoIndice <- 2
      }
      
      if(sexoIndice == 3){
        ap <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Year","Value", "Biological_Comp","Gender"))
      }else{
        ap <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Year","Value", "Biological_Comp"))
      }
      
      #Leer el input de biologicalCompUsuario
      nombresBiolo <- (bioloGeneral[,1]) 
      nombresBiolo[transM+1] <- "Vaccinated"
      nombresBiolo[transM+2] <- "Sum"
      inputBiolo <- vector()
      filasBiolo <- 0
      
      for(j in 1:length(biologicalCompUsuario)){
        for(i in 1:(transM+2)){
          if(biologicalCompUsuario[j] == nombresBiolo[i]){
            filasBiolo= filasBiolo + 1
            inputBiolo[filasBiolo] = i
          }
        }
      }
      
      
      ayuda <-0
      
      rangoEdad <- seq(edadUsuario[1],edadUsuario[2],by = 1)
      
      #Â¿Sum en el biological compartment?
      sumBiolo <- FALSE
      hastaBio <- length(biologicalCompUsuario)
      bioloCompSum <- inputBiolo
      for(m in 1:length(bioloCompSum)){
        if(bioloCompSum[m]==(transM + 2)){
          sumBiolo <- TRUE
          hastaBio <- length(biologicalCompUsuario)-1
        }
      }
      
      #Sacar los datos que necesito de el resultado de las ecuaciones diferenciales
      for(k in 1:hastaBio) {
        totalTipo = seq(1,num,by=1)
        totalTipo1 = seq(1,num,by=1)
        for(i in 1:num){
          if(sexoIndice==2){
            if(inputBiolo[k] !=(transM+1)){
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+edades*((inputBiolo[k])-1)]
              }
            }else{
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+2*transM*edades]
              }
            }
          }else if(sexoIndice==1){
            if(inputBiolo[k] !=(transM+1)){
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]
              }
            }else{
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+2*transM*edades+edades]
              }
            }
          }else if(sexoIndice==4){
            if(inputBiolo[k] !=(transM+1)){
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]+
                  dat[i,j+edades*((inputBiolo[k])-1)]
              }
            }else{
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+2*transM*edades+edades] + dat[i,j+2*transM*edades]
              }
            }
            
          }else if (sexoIndice ==3){
            if(inputBiolo[k] !=(transM+1)){
              for(j in rangoEdad){
                totalTipo1[i] = totalTipo1[i] + dat[i,(compart*edades/2)+j+edades*((inputBiolo[k])-1)]
              }
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+edades*((inputBiolo[k])-1)]
              }
            }else{
              for(j in rangoEdad){
                totalTipo1[i] = totalTipo1[i] + dat[i,j+2*transM*edades+edades]
              }
              for(j in rangoEdad){
                totalTipo[i] = totalTipo[i] + dat[i,j+2*transM*edades]
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
          for(i in 1:num){
            ap[i+ayuda,1] = i
            ap[i+ayuda,2] = totalTipo[i]
            ap[i+ayuda,3] = nombre
            ap[i+ayuda,4] = "Men"
          }
          ayuda <- nrow(ap)
          for(i in 1:num){
            ap[i+ayuda,1] = i
            ap[i+ayuda,2] = totalTipo1[i]
            ap[i+ayuda,3] = nombre
            ap[i+ayuda,4] = "Women"
          }
        }else{
          for(i in 1:num){
            ap[i+ayuda,1] = i
            ap[i+ayuda,2] = totalTipo[i]
            ap[i+ayuda,3] = nombre
          }
        }
      }
      
     
    
    ap
  }
  
  tabla=buildTable(c("Mujeres","Hombres"), bioloGeneral[,1],c(5,35),30)
  write.csv(tabla, file=nomArch, sep=",")
  num<-30
  texto = paste("The total vaccination cost is: ", round(sum(dat[1:num, edades*(compart+2)+1])))
  texto2 = paste("The total Primary Test cost is: ", round(sum(dat[1:num, edades*(compart+2)+2])))
  texto3 = paste0("The total Triage is: ", round(sum(dat[1:num, edades*(compart+2)+3])))
  texto4 = paste0("The total Follow Up cost is: ", round(sum(dat[1:num, edades*(compart+2)+4])))
  valor = round(sum(dat[1:num, edades*(compart+2)+1])) + round(sum(dat[1:num, edades*(compart+2)+2])) + round(sum(dat[1:num, edades*(compart+2)+3])) +round(sum(dat[1:num, edades*(compart+2)+4]))
  texto5 = paste0("The total cost is: ", valor)
  texto6 = paste0("The total effectiveness is: ", round(sum(dat[1:num, edades*(compart+2)+5])))
  
cFinal<-paste(texto, texto2,texto3,texto4,texto5,texto6,sep = '<br/>')
print(cFinal)
  
