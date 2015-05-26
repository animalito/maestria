## Cargo datos: se utilizan los codigos que se mostraron en el paso 1
source("clean_census.r")
source("clean_encuesta.r")

## Cargo librerias
library(mrpdata)
library(mrp)

# Implementacion para 2013
mrp.simple13 <- mrp(victimizacion ~ idcolonia + edad + genero,
                  data=mor_13,
                  population=mod.4,
                  pop.weights="value")
print(100*poststratify(mrp.simple13, ~ genero+edad), digits=4)
print(100*poststratify(mrp.simple13, ~ idcolonia+genero), digits=2)

# Implementacion para 2014
mrp.simple14 <- mrp(victimizacion ~ idcolonia + edad + genero,
                    data=mor_14,
                    population=mod.4,
                    pop.weights="value")
print(100*poststratify(mrp.simple14, ~ genero+edad), digits=4)
print(100*poststratify(mrp.simple14, ~ idcolonia+genero), digits=2)

