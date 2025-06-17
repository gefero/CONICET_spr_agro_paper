library(tidyverse)
library(countrycode)

## Cargamos la tabla de datos de FAO
fao <- read_csv('./data/FAOSTAT_data_es_8-11-2023.csv') %>%
        janitor::clean_names(.) %>%
        mutate(codigo_area_m49 = as.numeric(codigo_area_m49))

## Código de categoría ocupacional
# 21100 => Asalariados
# 21107 => Self employed
# 21110 => No clasificados
# 21091 => Total

## Limpieza
fao <- fao %>%
        mutate(iso3c = countrycode(codigo_area_m49, origin = "un", destination = "iso3c"),
               indicador = case_when(
                       codigo_indicador == 21091 ~ 'total',
                       codigo_indicador == 21110 ~ 'no clasificado',
                       codigo_indicador == 21107 ~ 'autoempleado',
                       codigo_indicador == 21100 ~ 'asalariados'),
               #ano = case_when(iso3c=="LUX" & ano == 2011 & indicador == "asalariados" ~ 2012,
        #                       TRUE ~ ano),
        #       codigo_ano = case_when(iso3c=="LUX" & codigo_ano == 2011 & indicador == "asalariados" ~ 2012,
        #                              TRUE ~ codigo_ano),
        )


# fao %>%
#         filter(codigo_indicador == 21100 & (ano >= 2010 & ano <= 2022)) %>%
#         arrange(codigo_area_m49, ano, codigo_indicador) %>%
#         group_by(codigo_area_m49) %>%
#         summarise(n = n(),
#                   min = min(ano),
#                   max = max(ano))
#  
# 
# lux <- fao %>%
#         mutate(id = row_number()) %>%
#         filter(area=="Luxemburgo" )
#         
#         filter(indicador %in% c("asalariados", "total") & (ano >= 2012 & ano <= 2022)) %>%
#         group_by(iso3c, indicador, area) %>% 
#         summarise(min_year = min(ano),
#                   max_year = max(ano),
#                   n = n(),
#                   mean = mean(valor, na.rm=TRUE)
#         ) %>%
#         pivot_wider(
#                 names_from = indicador,
#                 values_from = c(mean)
#         )

## Agrego tasa de asalarización por año-país                
fao_agg <- fao %>%
        filter(indicador %in% c("asalariados", "total") & (ano >= 2005 & ano <= 2015)) %>%
        arrange(iso3c, ano, indicador) %>%
        group_by(iso3c, indicador, area) %>% 
        summarise(min_year = min(ano),
                  max_year = max(ano),
                  n = n(),
                  mean = mean(valor, na.rm=TRUE)
                  ) %>%
        ungroup() %>%
        mutate(n = case_when(iso3c == "LUX" ~ 10,
                             TRUE ~ n),
               min_year = case_when(iso3c == "LUX" ~ 2012,
                                    TRUE ~ min_year)) %>% # COCINO LUXEMBURGO
 pivot_wider(
                names_from = indicador,
                values_from = mean
        ) %>%
        mutate(p_asalariados_agro = asalariados/total*100)

## Proceso data Banco Mundial
library(wbstats)

wb_agro <- wb_data(indicator = "SL.AGR.EMPL.ZS",
        start_date = 2005,
        end_date = 2015)

wb_agro_agg <- wb_agro %>%
        drop_na(SL.AGR.EMPL.ZS) %>%
        group_by(iso3c) %>%
        summarise(p_agro = mean( SL.AGR.EMPL.ZS, na.rm=TRUE))

## Cargo tipología de países
countries <- read_csv('./data/country_classification.csv')
#countries <- countries %>% rename(p_asalariados_total=asalariados)

final <- fao_agg %>%
        left_join(wb_agro_agg, by = "iso3c") %>%
        left_join(countries, by = "iso3c")

final <- final %>%
        mutate(clst_pimsa_code = str_sub(cluster_pimsa, 1,2))



#fao_agg %>%
#        ggplot(aes(y=p_asalariados_agro, x=p_asalariados_total)) +
#                #geom_point() +
#        ggrepel::geom_text_repel(aes(label=iso3c), max.overlaps = 50) +
#                facet_wrap(~C5)

#library(plotly)
labels <- c("Capit. desarrollado en ext. y prof.",
"Capit. ext. reciente c/rasgos de desarrollo en prof.",
"Capit. en extensión con peso del campo",
"Capit. de escasa extensión con peso del campo",
"Pequeña prop. en el campo")

grey_scale <- c("#1b1b1b", "#484848", "#727272", "#a2a2a2","#dddddd")





p1 <- final %>%
        ggplot(aes(y=p_asalariados_agro, x=p_agro, 
                   color=clst_pimsa_code)) +
        xlim(0,100) +
        ylim(0,100) +
        geom_point() +
        scale_color_viridis_d()+
        labs(x='% población ocupada en Agro (World Bank Data)',
             y='% asalariados en Agro (FAOSTAT)',
             color="Tipo desarrollo capitalista") +
        theme_minimal() +
        theme(
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                legend.position = "bottom"
                )


ggsave('./paper_material/plots/grafico1.jpg', p1, 
       width = 8, height=8,
       bg="white")

library(gt)
final %>%
        select(iso3c, country)  %>%
        gt() %>%
        tab_header(
                title = md("**Países incluidos en los cálculos de Gráfico 1 y Tabla 2**"),
                subtitle = md("N = 136")
        ) %>%
        cols_label(
                country = "País",
                iso3c  = "ISO3C"
        ) %>%        
        gtsave('./paper_material/paises_G1_T2.rtf')

#write_csv('./paper_material/paises_G1_T2.csv')
