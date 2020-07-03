#Autor: Arturo Jesús Laflor Hernández

#Fecha: 23-03-2017

#Descripción: Genera una tabla con información para el análisis de 

#calidad de un dataframe con variables continuas



QOfContinuousF=function(datasetCF){
  
  #codigo para hacer las pruebas
  
  #datasetCF<- cbind.data.frame(DD1=d_preproc[,2],d_preproc[,10:13],d_preproc[,58:62])
  
  
  
  
  
  #na.rm=TRUE "Exclude na values from the analysis"
  
  #para prueba
  
  #datasetCF<-cbind.data.frame(responses$DD1,responses$SQ1,responses$SQ2,responses$SQ3,responses$SQ4)
  
  
  
  Feature<-colnames(datasetCF)
  
  Count<- apply(datasetCF,2,FUN = function(x) length(x)) 
  
  Miss<- apply(datasetCF,2,FUN = function(x)  sum(is.na(x)))
  
  Card<- apply(datasetCF,2,FUN = function(x) length(levels(factor(x))))
  
  Min<-apply(datasetCF,2,FUN = function(x) min(x,na.rm = TRUE))
  
  Qrt1<-apply(datasetCF,2,FUN = function(x) round(quantile(as.numeric(x),.25,na.rm = TRUE),digits = 2))
  
  Median<-apply(datasetCF,2,FUN = function(x) round(quantile(as.numeric(x),.5,na.rm = TRUE),digits = 2))
  
  Qrt3<-apply(datasetCF,2,FUN = function(x) round(quantile(as.numeric(x),.75,na.rm = TRUE),digits = 2))
  
  Max <-apply(datasetCF,2,FUN = function(x) max(x,na.rm = TRUE))
  
  Mean<-apply(datasetCF,2,FUN = function(x) round(mean(as.numeric(x),na.rm = TRUE),digits = 2))
  
  Sdev<-apply(datasetCF,2,FUN = function(x) round(sd(as.numeric(x),na.rm = TRUE),digits = 2))
  
  
  
  TQContinuousFeatures<-data.frame(Count,Miss,Card,Min,Qrt1,Median,Qrt3,Max,Mean,Sdev)
  
  
  
}