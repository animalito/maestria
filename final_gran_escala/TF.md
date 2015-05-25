
Algoritmos de Gran Escala:  
Trabajo Final
========================================================
author: Andrea Fernández, Andrea García y Mario Becerra
date: 25/05/15

Elegir una base de datos para clasificación
========================================================

Para la elaboración de este trabajo se eligió una base de declaratorias de desastres de SEGOB. La variable dependiente toma el valor de 1 si fue declarado (desastre, contingencia climática o  emergencia), 0 si no fue declarado. 

Para las covariabes utilizamos variables geográficas (como nivel de riesgo a inundación, deslave, huracán y  sequía) y variables socioeconómicas tales como tasa de alfabetización, nivel de hacinamiento, tamaño de la locaidad, entre otras. 


EDA
========================================================
La distribución por Grado de Marginación nos muestra que los grados altos tienen más declaratorias. 

![](/img/dec_GM.png)


EDA
========================================================
En cuanto al tipo de fenómeno  la mayor parte de las declaratorias se concentran en lluvias y sequías.


![](/img/tipo.png)



Adaptar el código de SVM para el clúster 
========================================================
Para el Cluster ......



Puntos Interiores 
========================================================




Regresión Logística
========================================================


```r
confusionMatrix(predicted.class, test$Dependiente)
```

```
Confusion Matrix and Statistics

          Reference
Prediction   0   1
         0 254 158
         1 354 592
                                          
               Accuracy : 0.623           
                 95% CI : (0.5966, 0.6488)
    No Information Rate : 0.5523          
    P-Value [Acc > NIR] : 7.793e-08       
                                          
                  Kappa : 0.2136          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.4178          
            Specificity : 0.7893          
         Pos Pred Value : 0.6165          
         Neg Pred Value : 0.6258          
             Prevalence : 0.4477          
         Detection Rate : 0.1870          
   Detection Prevalence : 0.3034          
      Balanced Accuracy : 0.6035          
                                          
       'Positive' Class : 0               
                                          
```


Problemas 
========================================================
El principal problema fue que no entendiamos cómo funcionaba MPI, además la última versión de MPI cambió sus comandos 

![](/img/esquema_mpi.png)


