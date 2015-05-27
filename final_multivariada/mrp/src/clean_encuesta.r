setwd("/home/animalito/Documents/opi/analysis-mx/morelos_2/extras/mrp")

## Cargamos librerias
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

## Lectura de datos
mor_13 <- readRDS("data/mor_13.rds")
mor_14 <- readRDS("data/mor_14.rds")

## Funciones auxiliares para recategorizar las variables para postestratificacion
# Generamos edad identica
gedad <- function(x){
    ifelse(x > 130, NA,
           ifelse(x > 50, "e50mas",
                  ifelse(x > 30, "e30a49",
                         ifelse(x > 18, "e18a29",
                                ifelse(x > 12, "e12a17", NA)))))
}
# Generamos escolaridad identica
gescolaridad <- function(x){
    x <- as.character(x)
    ifelse(x == "Secundaria", "secundaria",
           ifelse(x == "Primaria", "primaria",
                  ifelse(x == "Licenciatura", "prepaomas",
                         ifelse(x == "Posgrado", "prepaomas", "ninguno"))))
}
# Generamos ocupacion identica
gocupacion13 <- function(x){
    x <- as.character(x)
    ifelse(x == "No", "notrabaja",
           ifelse(x == "Si", "trabaja", NA))
}
gocupacion14 <- function(x){
    x <- as.character(x)
    ifelse(x =="Estudias y trabajas", "trabaja",
           ifelse(x == "Trabajas", "trabaja", "notrabaja"))
}
# Generamos genero identico
ggenero <- function(x){
    str_trim(tolower(as.character(x)))
}

## Transformacion de tablas
mor_13 <- mor_13 %>%
    mutate(edad = gedad(edad),
           escolaridad = gescolaridad(escolaridad),
           ocupacion = gocupacion13(trabajas),
           genero = ggenero(genero)
           )

mor_14 <- mor_14 %>%
    mutate(edad = gedad(edad),
           escolaridad = gescolaridad(escolaridad),
           ocupacion = gocupacion14(ocupacion),
           genero = ggenero(genero)
    )

## Recodificacion de variables para mrp *las respuestas deben ser 0 o 1*
gdelito <- function(x){
    x <- as.character(x)
    ifelse(x == "Sí", 1, 0)
}

mor_13 <- mutate(mor_13,
                 victimizacion = gdelito(p17))

mor_14 <- mutate(mor_14,
                 victimizacion = gdelito(p27))

