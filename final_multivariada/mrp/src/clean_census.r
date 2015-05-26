setwd("/home/animalito/Documents/opi/analysis-mx/morelos_2/extras/mrp")

library(plyr)
library(dplyr)
library(tidyr)
library(assertthat)
census <- readRDS("data/mrp_census.rds")
census <- census[!duplicated(census$idcolonia),]

################################################################################
# G: edad x colonia (dim=4x63)
mod.1 <- census %>%
    mutate("e12a17"=pob7+pob9
           , "e18a29"=pob11-pob9
           , "e30a49"=pob14
           , "e50mas"=pob15+pob23) %>%
    dplyr::select(idcolonia, e12a17, e18a29, e30a49, e50mas) %>%
    tidyr::gather(key, value, -idcolonia)

# quito negativos
mod.1$value[mod.1$value < 0] <- 0
mod.1 <- na.omit(mod.1)
mod.1 <- dplyr::rename(mod.1, edad=key)
assert_that(dim(mod.1)[1]==63*4)

################################################################################
# G: trabajo x colonia
mod.2 <- census %>%
    mutate(trabaja=eco4
           , notrabaja=pob19-eco4) %>%
    dplyr::select(idcolonia, trabaja, notrabaja) %>%
    tidyr::gather(key, value, -idcolonia)

# quito negativos
mod.2$value[mod.2$value < 0] <- 0
mod.2 <- na.omit(mod.2)
mod.2 <- dplyr::rename(mod.2, ocupacion=key)
assert_that(dim(mod.2)[1]==63*2)

################################################################################
# G: escolaridad x colonia
mod.3 <- census %>%
    mutate(ninguno=edu31
           , primaria=edu34
           , secundaria = edu37
           , prepaomas = edu40
           ) %>%
    dplyr::select(idcolonia, ninguno, primaria, secundaria, prepaomas) %>%
    tidyr::gather(key, value, -idcolonia)

# quito negativos
mod.3$value[mod.3$value < 0] <- 0
mod.3 <- na.omit(mod.3)
mod.3 <- dplyr::rename(mod.3, escolaridad=key)
assert_that(dim(mod.3)[1]==63*4)

################################################################################
# G: genero x edad x colonia
mod.4 <- census %>%
    mutate(
        mujer_e12a17 = pob37 + pob39
        , mujer_e18a29 = pob41 - pob39
        , mujer_e30a49 = pob45
        , mujer_e50omas = pob46 + pob54
        , hombre_e12a17 = pob63 + pob65
        , hombre_e18a29 = pob67 - pob65
        , hombre_e30a49 = pob70
        , hombre_e50omas = pob71 + pob79
        ) %>%
    dplyr::select(idcolonia, mujer_e12a17, mujer_e18a29, mujer_e30a49, mujer_e50omas, hombre_e12a17, hombre_e18a29, hombre_e30a49, hombre_e50omas) %>%
    tidyr::gather(key, value, -idcolonia) %>%
    tidyr::separate(key, c("genero", "edad"), sep="_")

mod.4$value[mod.4$value < 0] <- 0
mod.4 <- na.omit(mod.4)
assert_that(dim(mod.4)[1]==63*4*2)

################################################################################
# G: genero x ocupacion x colonia
mod.5 <- census %>%
    mutate(
        mujer_trabaja = eco5
        , mujer_notrabaja = pob50 - eco5
        , hombre_trabaja = eco6
        , hombre_notrabaja = pob75 - eco6
        ) %>%
    dplyr::select(idcolonia, mujer_trabaja, mujer_notrabaja, hombre_trabaja, hombre_notrabaja) %>%
    tidyr::gather(key, value, -idcolonia) %>%
    tidyr::separate(key, c("genero", "ocupacion"), sep="_")

mod.5$value[mod.5$value < 0] <- 0
mod.5 <- na.omit(mod.5)
assert_that(dim(mod.5)[1]==63 * 2 * 2)


################################################################################
### No tan los datos para manzana! :O
# G: genero x escolaridad x colonia
# mod.6 <- census %>%
#     mutate(
#         mujer_ninguno = edu32
#         , mujer_primaria = edu35
#         , mujer_secundaria = edu38
#         , mujer_prepaomas = edu41
#         , hombre_ninguno = edu33
#         , hombre_primaria = edu36
#         , hombre_secundaria = edu39
#         , hombre_prepaomas = edu42
#         ) %>%
#     dplyr::select(idcolonia, mujer_ninguno, mujer_primaria, mujer_secundaria, mujer_prepaomas, hombre_ninguno, hombre_primaria, hombre_secundaria, hombre_prepaomas) %>%
#     tidyr::gather(key, value, -idcolonia) %>%
#     tidyr::separate(variable, c("genero", "escolaridad"), sep="_")
#
# mod.6$value[mod.6$value < 0] <- 0
# assert_that(dim(mod.6)[1]==63*4*2)