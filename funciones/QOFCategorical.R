#Autor: Arturo Jesús Laflor Hernández

#Fecha: 23-03-2017

#Descripción: Genera una tabla con información para el análisis de 

#calidad de un dataframe con variables categóricas

QOfCategoricalF=function(datasetCF){
  
  #codigo para hacer las pruebas
  
  #datasetCF<- cbind.data.frame(Genero=d_preproc[,3],d_preproc[,26:30],CofS=d_preproc[,57])
  
  
  
  #datasetCF<-responses[,28:48]
  
  
  
  calc_mode=function(x,primsec=1){
    
    #temp<-table(datasetC$SH3)
    
    temp<-table(x)
    
    tableorder<-order(temp,decreasing = T)
    
    #vecnames<-names(table)[temp==max(temp)]
    
    vecvalues<-names(temp[tableorder[primsec]])
    
  }
  
  calc_frecmode=function(x,primsec=1){
    
    #temp<-table(datasetC$SH3)
    
    temp<-table(x)
    
    tableorder<-order(temp,decreasing = T)
    
    vecvalues<-temp[tableorder[primsec]]
    
  }
  
  
  
  
  
  Feature<-colnames(datasetCF)
  
  Count<- apply(datasetCF,2,FUN = function(x) length(x))
  
  Miss<- apply(datasetCF,2,FUN = function(x)  sum(is.na(x)))
  
  Card<- apply(datasetCF,2,FUN = function(x) length(levels(factor(x))))
  
  Mode<- apply(datasetCF,2, FUN=calc_mode,primsec=1)
  
  ModeFrec<- apply(datasetCF,2, FUN=calc_frecmode,primsec=1)
  
  ModePerc<-paste(round(ModeFrec/Count*100,digits = 2),"%",sep = "")
  
  Mode2<-apply(datasetCF,2, FUN=calc_mode,primsec=2)
  
  Mode2Frec<-apply(datasetCF,2, FUN=calc_frecmode,primsec=2)
  
  Mode2Perc<-paste(round(Mode2Frec/Count*100,digits = 2),"%",sep = "")
  
  
  
  # #calcula moda de una columna
  
  # getmode <- function(v) {
  
  #   uniqv <- unique(datasetCF$SH1)
  
  #   m<-uniqv[which.max(tabulate(match(datasetCF$Genero, uniqv)))]
  
  # }
  
  
  
  TQCategoricalFeatures<-data.frame(Count,Miss,Card,Mode,ModeFrec,ModePerc,Mode2,Mode2Frec,Mode2Perc)
  
  
  
}