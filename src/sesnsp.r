library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(testit)

calculateRate <- function(x, rate, control.pop) {
    if(is.numeric(x)) {
        x <- ifelse(control.pop != 0, (rate/control.pop)*x, 0)
    }
    return(x)
}

setwd("~/Dropbox/maestria/maestria/src")
data <- read.table('../data/segob_secretariadoEjecutivoIncidenciaDelictiva_municipios.csv', dec='.', header=T, sep=',',quote = "\"'", stringsAsFactors=F)
names(data) <- tolower(names(data))

data[,9:21] <- lapply(data[,9:21],function(x){as.numeric(gsub(",", "", x))})

#dim(data)

# cataloguin de claves de estados
estados <- read.csv('../data/catalogo_estados.csv', header=T)

data <- merge(data, estados[,c('estado','merge')], by.x="entidad", by.y="merge", all.x=T, all.y=F)

#dim(data)

# clean up general
data$cvegeo <- paste0(
    substring(as.character(data$estado + 100),2,3) ,
    substring(as.character(data$clave.municipio + 1000),2,4))

data$concepto <- str_trim(tolower(data$concepto))
data$modalidad <- str_trim(tolower(data$modalidad))
data$tipo <- str_trim(tolower(data$tipo))
data$subtipo <- str_trim(tolower(data$subtipo))

#
data <- subset(data, año==2013)
# data$total <- as.numeric(data$total)
# all
all <- data %>%
    group_by(cvegeo) %>%
    summarise(todos=sum(total, na.rm=T))

# homicidios dolosos modalidad=homicidios, tipo=dolosos
homicidios <- filter(data, modalidad=="homicidios", tipo=="dolosos") %>%
    group_by(cvegeo) %>%
    summarise(homicidios=sum(total,na.rm=T))

# robo comun modalidad = robo comun
robos <- filter(data, subtipo=="de vehiculos") %>%
    group_by(cvegeo) %>%
    summarise(robos=sum(total, na.rm=T))

# juntamos
dependent <- merge(merge(all,homicidios), robos)

# mergeamos con la base de results

df <- read.csv('../data/base.csv', header=T)
df$cvegeo <- substring(as.character(df$cvegeo + 100000),2,6)
df_m <- merge(df[,c('cvegeo','admin1','admin2','inegi_cpv_pob1')], dependent, by="cvegeo", all.x=T, all.y=F)

# saco tasas
df_m <-mutate(df_m,
              todos_r=calculateRate(todos, rate=100000, control.pop=inegi_cpv_pob1),
              homicidios_r=calculateRate(homicidios, rate=100000, control.pop=inegi_cpv_pob1),
              robos_r=calculateRate(robos, rate=100000, control.pop=inegi_cpv_pob1))

write.csv(df_m, '../data/dependent.csv')

#assert("total de delitos del fuero comun en 2013 es 1681077", df_m$todos, na.rm=T) == 1681077)

# mergeamos con results

results <- readRDS('../data/results.rds')

df_f <- merge(results, df_m, by="cvegeo", all.x=T, all.y=F)
saveRDS(df_f, '../data/model.rds')
