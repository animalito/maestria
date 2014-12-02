library(knitr)
library(dplyr)
library(plyr)
library(rMaps)
library(maptools)
library(rgdal)
library(ggplot2)
library(Hmisc)
library(RColorBrewer)
library(randomForest)
library(pscl)

# Leemos los datos mas recientes

rm(list=ls())
ls()
gc()
cat <- tbl_df(read.csv('data/catalogo.csv'))
dep<- tbl_df(read.csv('data/dependent.csv'))
datos <- tbl_df(read.csv('data//base.csv'))
results <- tbl_df(read.csv('data//results.csv'))
datos <- filter(datos, cvegeo %in% unique(results$cvegeo))


# Defino los factores de riesgo en bonito para titulos
fr <- unique(cat$factor_de_riesgo[!is.na(cat$factor_de_riesgo)])
fr <- fr[which(fr!='violencia')]
factores <- list(embarazo_temprano = "Embarazo temprano",
                 marginacion_exclusion_social = "Marginación y exclusión social",
                 falta_oportunidades_laborales_informalidad_desocupacion = "Falta de oportunidades laborales, informalidad y desocupación",
                 espacios_publicos_insuficiente_deteriorado = "Espacios públicos insuficientes y deteriorados",
                 capital_social_participacion_incipiente = "Capital social y participación incipiente",
                 desercion_escolar = "Deserción escolar",
                 consumo_abuso_drogas_ilegales = "Consumo y abuso de drogas legales e ilegales",
                 ambientes_familiares_deteriorados_problematicos = "Ambientes familiares deteriorados y problemáticos")

#tabla de número de municipios por estado

numero_municipios <- datos %>% dplyr::select(cvegeo, admin1,  admin2 ) %>%
  group_by(admin1) %>% dplyr::summarise(num_mun=n())  

numero_municipios$id <- 1:nrow(numero_municipios)

edo_shp <- readOGR("data/estados_ligero/." , layer="Mex_Edos")

edo_shp@data$id <- rownames(edo_shp@data)

edo_df <- edo_shp %>%
  fortify(region = "id") %>%
  mutate(id = as.numeric(id)+1) # hacemos el id numérica

edo_df <- tbl_df(left_join(edo_df,numero_municipios))

head(edo_df)

p <- theme(axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank(),
           panel.background=element_blank(),
           panel.border=element_blank(),
           panel.grid.major=element_blank(),
           panel.grid.minor=element_blank(),
           plot.background=element_blank())

print(ggplot(data = edo_df, aes(long, lat, group = group)) + 
  geom_polygon(colour='black', aes(fill=num_mun)) + coord_fixed() + p +
  ggtitle('Número de municipios urbanos') + labs(fill='Número') )

##############

claves_mun <-  datos %>% dplyr::select(cvegeo, admin1,  admin2  ,latitude ,longitude)
results <- left_join(results, claves_mun) 
results <- left_join(results, numero_municipios[,c('admin1','id')])
results <- results[,sort(names(results))] 

ids <- c("admin1","admin2" , "cvegeo", "latitude" ,  "longitude", 'id'  )
vars <- setdiff(names(results), ids)

##### Agregado de violencia

head(datos)

head(cat)

vars_violencias <- cat[grep(cat$descripcion_variable, 
                      pattern = 'violencia|homicidios|delitos'),'nombre_variable_modelo']
write.csv(vars_violencias, 'variables_agregacion.csv')

vars_violencia <- names(datos) %in% vars_violencias$nombre_variable_modelo
vars_violencia[1]  <- T

datos_violencia <- datos[,c(vars_violencia)] 
datos_violencia[,'inegi_cpv_pob1' ] <- datos[,'inegi_cpv_pob1']


limp <- function(col){
  as.numeric(as.character(gsub( x=col ,pattern = ',',replacement = '')))
}

datos_violencia_limp <-datos_violencia %>%  mutate_each( funs(limp))

head(datos_violencia_limp)
summary(datos_violencia_limp)

mun_na <- datos_violencia_limp[is.na(datos_violencia_limp$segob_sesnsp_otrosdelitos_amenazas_amenazas),]$cvegeo

municipios_na <- as.data.frame(datos[ datos$cvegeo %in% mun_na, c('admin1','admin2') ])

municipios_na

####
library(tidyr)
(datos_violencia_limp <- tbl_df(datos_violencia_limp ) %>% gather( y,valor,segob_sesnsp_otrosdelitos_amenazas_amenazas:segob_sesnsp_robocomun_sinviolencia_sindatos)
)



respuesta <- datos_violencia_limp %>% group_by(cvegeo) %>%
  dplyr::summarise(y= 100000*sum(valor, na.rm=F)/mean(inegi_cpv_pob1, na.rm=F)  )

dep$X <- NULL
names(dep)
respuesta_plot <- left_join(respuesta, claves_mun)
respuesta_plot_todas <- left_join(respuesta_plot, dep)

respuesta_edo <- respuesta_plot_todas %>% group_by(admin1) %>%
  dplyr::summarise(y= mean( todos_r , na.rm=T) , 
                   y_homicidios= mean( homicidios_r , na.rm=T) , 
                   y_robos= mean( robos_r , na.rm=T) 
                   )

respuesta_edo
####


res_edos <- left_join(edo_df,respuesta_edo)

print(ggplot(data = res_edos , aes(long, lat, group = group)) + 
  geom_polygon(colour='black', 
               aes(fill= factor(cut2(y, g=5), labels =                                      c('Muy bajo','Bajo','Medio','Alto', 'Muy alto')) ))  + coord_fixed() + p +
  ggtitle('Promedio estatal del agregado por cien mil habitantes') + 
  labs(fill='') + scale_fill_brewer(  palette = 'YlOrRd'))

print(ggplot(data = res_edos , aes(long, lat, group = group)) + 
  geom_polygon(colour='black', 
               aes(fill= factor(cut2(y_homicidios, g=5), labels =                                      c('Muy bajo','Bajo','Medio','Alto', 'Muy alto')) ))  + coord_fixed() + p + ggtitle('Promedio estatal de homicidios por cien mil habitantes') + 
  labs(fill='') + scale_fill_brewer(  palette = 'YlOrRd'))

print(ggplot(data = res_edos , aes(long, lat, group = group)) + 
  geom_polygon(colour='black', 
               aes(fill= factor(cut2(y_robos, g=5), labels =                                      c('Muy bajo','Bajo','Medio','Alto', 'Muy alto')) )) + coord_fixed() + p +
  ggtitle('Promedio estatal de robos por cien mil habitantes') + 
  labs(fill='') + scale_fill_brewer(  palette = 'YlOrRd'))

###### Modelado
vars <- setdiff(names(results),c('id','admin2','longitude','latitude'))

respuesta_plot_todas$y <- respuesta_plot_todas$todos_r

dat_mod <- left_join(results[,vars],respuesta_plot_todas[c('cvegeo','y')])
names(dat_mod)
unique(dat_mod$kmeans_ambientes_familiares_deteriorados_problematicos)


dat_mod <- as.data.frame(dat_mod )

for(k in grep(names(dat_mod),pattern='kmeans') ){ 
  lev <-  unique(dat_mod[,k] , na.rm=T) 
  lev <- lev[!is.na(lev)]
  lev <- paste0('clase',lev)
  dat_mod[,k]  <- factor(as.character(dat_mod[,k] ),
                         labels = lev)
} 

summary(dat_mod)
l_ply( vars[11:length(vars)],function(var){
  print(var)
  p <- ggplot(dat_mod, aes_string(x=var , y='y'))+geom_point() +
     geom_smooth(method = "lm", size = 1.5, colour='#8B0000') + theme_bw()
  print(p)
})

p <- ggplot(dat_mod, aes_string( x='y'))+geom_density( colour="#8B0000",fill= "#8B0000") +theme_bw()
print(p)

p <- ggplot(dat_mod, aes( x=y+1))+geom_density( colour="#8B0000",fill= "#8B0000") +theme_bw() + scale_x_log10() + xlab('y+1 | log')
print(p)

l_ply( vars[11:length(vars)],function(var){
  print(var)
  p <- ggplot(dat_mod, aes_string( x=var))+
    geom_density( colour="#8B0000",fill= "#8B0000") +theme_bw()
  print(p)
})

####modelos


summary(mod <- glm(y ~. - admin1-cvegeo ,dat_mod[, -grep(names(dat_mod),pattern = 'kme')], family='gaussian'))

summary(
  mod_log <- glm(log(1+y) ~. - admin1-cvegeo ,
                 dat_mod[, -grep(names(dat_mod),pattern = 'kme')],
                 family='gaussian')
  )

summary(mod_poiss <-
          glm( y ~. - admin1-cvegeo ,
               dat_mod[, -grep(names(dat_mod),pattern = 'kme')],
               family='poisson') 
        )

summary(mod_q_poiss <- glm( y ~. - admin1-cvegeo ,dat_mod[, -grep(names(dat_mod),pattern = 'kme')], family='quasipoisson'))
summary(mod_poiss_log <- glm( log(y+1) ~. - admin1-cvegeo ,dat_mod[, -grep(names(dat_mod),pattern = 'kme')], family='poisson'))
summary(mod_q_poiss_log <- glm( log(y+1) ~. - admin1-cvegeo ,dat_mod[, -grep(names(dat_mod),pattern = 'kme')], family='quasipoisson'))

dat_mod_zero <- dat_mod
dat_mod_zero$y <- as.integer(floor(dat_mod_zero$y)) 
min(dat_mod_zero$y)
mod_zero <- zeroinfl(
  y ~. - admin1-cvegeo , 
  data=dat_mod_zero[, -grep(names(dat_mod),pattern = 'kme')] , dist = "poisson"
  )
summary(mod_zero)

# Y aqui, para obtener a pie la devianza y comparemos...
2*(logLik(mod_zero) - logLik(update(mod_zero, . ~ 1)))

modelos <-1:6
names(modelos) <- c('mod','mod_log',
                    'mod_poiss','mod_q_poiss',
                    'mod_poiss_log','mod_q_poiss_log')


(comp_mods <- ldply(modelos, function(m){
  mm <- get(names(modelos)[m] )
  data.frame(AIC=AIC(mm), Dev=deviance(mm), BIC=BIC(mm)) 
}))


print(plot( mod_log ))

print(plot( mod_poiss ))

print(plot( mod_q_poiss ))

#en nigun log se cumplen los supuestos
#en los dos Poiss se portan bien en AIC y cumplen razonablemente


####Seleccion de variables

bosq <- randomForest(y ~. - admin1-cvegeo ,na.omit(dat_mod[, -grep(names(dat_mod),pattern = 'kme')]), importance=T)
print(bosq)

varImpPlot(bosq,type = 1)

imp <- data.frame(bosq$importance)
imp$var <- rownames(imp) 
imp <- imp%>%dplyr::arrange( IncNodePurity)
head(imp,7)$var


varImpPlot(bosq,type = 2)


mod_upd <- 
  glm( y ~. - admin1-cvegeo  - 
         PC1_pca_desercion_escolar -
         PC2_pca_espacios_publicos_insuficiente_deteriorado -
         PC2_pca_consumo_abuso_drogas_ilegales -
         PC1_pca_espacios_publicos_insuficiente_deteriorado -
         PC1_pca_embarazo_temprano -
         PC1_pca_consumo_abuso_drogas_ilegales -
         PC2_pca_embarazo_temprano,
       dat_mod[, -grep(names(dat_mod),pattern = 'kme')],
       family='gaussian') 
deviance(mod) < deviance(mod_upd)
deviance(mod) 
deviance(mod_upd)

summary(mod_upd)



mod_poiss_upd <- 
  glm( y ~. - admin1-cvegeo  - 
         PC1_pca_desercion_escolar -
         PC2_pca_espacios_publicos_insuficiente_deteriorado -
         PC2_pca_consumo_abuso_drogas_ilegales -
         PC1_pca_espacios_publicos_insuficiente_deteriorado -
         PC1_pca_embarazo_temprano -
         PC1_pca_consumo_abuso_drogas_ilegales -
         PC2_pca_embarazo_temprano,
       dat_mod[, -grep(names(dat_mod),pattern = 'kme')],
       family='poisson') 

deviance(mod_poiss) < deviance(mod_poiss_upd)
deviance(mod_poiss) 
deviance(mod_poiss_upd)
plot(mod_poiss_upd)

summary(mod_poiss_upd)

mod_q_poiss_upd <- 
  glm( y ~. - admin1-cvegeo  - 
         PC1_pca_desercion_escolar -
         PC2_pca_espacios_publicos_insuficiente_deteriorado -
         PC2_pca_consumo_abuso_drogas_ilegales -
         PC1_pca_espacios_publicos_insuficiente_deteriorado -
         PC1_pca_embarazo_temprano -
         PC1_pca_consumo_abuso_drogas_ilegales -
         PC2_pca_embarazo_temprano,
       dat_mod[, -grep(names(dat_mod),pattern = 'kme')],
       family='quasipoisson') 

deviance(mod_q_poiss) < deviance(mod_q_poiss_upd)
deviance(mod_q_poiss) 
deviance(mod_q_poiss_upd)

plot(mod_poiss_upd)

deviance(mod_q_poiss_upd)
deviance(mod_poiss_upd)



##Finalmente

modelos <-1:6
names(modelos) <- c('mod','mod_upd',
                    'mod_poiss','mod_poiss_upd',
                    'mod_q_poiss','mod_q_poiss_upd'
                    )
(comp_mods <- ldply(modelos, function(m){
  mm <- get(names(modelos)[m] )
  data.frame(AIC=AIC(mm), Dev=deviance(mm), BIC=BIC(mm)) 
}))

summary(mod_poiss_upd)
qplot( predict(mod_poiss_upd),mod_poiss_upd$y)
qplot( predict(mod_poiss),mod_poiss$y)

qplot( predict(mod_upd),mod_upd$y) + geom_abline(slope=1) 
qplot( predict(mod),mod$y)



