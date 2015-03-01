    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "data/estados_ligero/.", layer: "Mex_Edos"
    ## with 32 features and 1 fields
    ## Feature type: wkbPolygon with 2 dimensions
    ## [1] "PC1_pca_ambientes_familiares_deteriorados_problematicos"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    ## [1] "PC1_pca_capital_social_participacion_incipiente"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-2.png)

    ## [1] "PC1_pca_consumo_abuso_drogas_ilegales"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-3.png)

    ## [1] "PC1_pca_desercion_escolar"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-4.png)

    ## [1] "PC1_pca_embarazo_temprano"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-5.png)

    ## [1] "PC1_pca_espacios_publicos_insuficiente_deteriorado"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-6.png)

    ## [1] "PC1_pca_falta_oportunidades_laborales_informalidad_desocupacion"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-7.png)

    ## [1] "PC1_pca_marginacion_exclusion_social"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-8.png)

    ## [1] "PC2_pca_ambientes_familiares_deteriorados_problematicos"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-9.png)

    ## [1] "PC2_pca_capital_social_participacion_incipiente"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-10.png)

    ## [1] "PC2_pca_consumo_abuso_drogas_ilegales"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-11.png)

    ## [1] "PC2_pca_desercion_escolar"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-12.png)

    ## [1] "PC2_pca_embarazo_temprano"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-13.png)

    ## [1] "PC2_pca_espacios_publicos_insuficiente_deteriorado"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-14.png)

    ## [1] "PC2_pca_falta_oportunidades_laborales_informalidad_desocupacion"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-15.png)

    ## [1] "PC2_pca_marginacion_exclusion_social"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-16.png)
![](./pres_files/figure-markdown_strict/unnamed-chunk-1-17.png)

    ## [1] "PC1_pca_ambientes_familiares_deteriorados_problematicos"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-18.png)

    ## [1] "PC1_pca_capital_social_participacion_incipiente"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-19.png)

    ## [1] "PC1_pca_consumo_abuso_drogas_ilegales"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-20.png)

    ## [1] "PC1_pca_desercion_escolar"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-21.png)

    ## [1] "PC1_pca_embarazo_temprano"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-22.png)

    ## [1] "PC1_pca_espacios_publicos_insuficiente_deteriorado"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-23.png)

    ## [1] "PC1_pca_falta_oportunidades_laborales_informalidad_desocupacion"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-24.png)

    ## [1] "PC1_pca_marginacion_exclusion_social"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-25.png)

    ## [1] "PC2_pca_ambientes_familiares_deteriorados_problematicos"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-26.png)

    ## [1] "PC2_pca_capital_social_participacion_incipiente"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-27.png)

    ## [1] "PC2_pca_consumo_abuso_drogas_ilegales"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-28.png)

    ## [1] "PC2_pca_desercion_escolar"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-29.png)

    ## [1] "PC2_pca_embarazo_temprano"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-30.png)

    ## [1] "PC2_pca_espacios_publicos_insuficiente_deteriorado"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-31.png)

    ## [1] "PC2_pca_falta_oportunidades_laborales_informalidad_desocupacion"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-32.png)

    ## [1] "PC2_pca_marginacion_exclusion_social"

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-33.png)

    ## 
    ## Call:
    ##  randomForest(formula = y ~ . - admin1 - cvegeo, data = na.omit(dat_mod[,      -grep(names(dat_mod), pattern = "kme")]), importance = T) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 5
    ## 
    ##           Mean of squared residuals: 178986.3
    ##                     % Var explained: 36.36

![](./pres_files/figure-markdown_strict/unnamed-chunk-1-34.png)
