library(tidyverse)
library(Rilostat)
library(gt)
horas <- get_ilostat("HOW_TEMP_SEX_ECO_NB_A",
                     lang = "es",
                     type = "both",
                     filters = list(
                             timefrom = 2005,
                             timeto = 2015,
                             sex = "SEX_T")
                     ) %>%
        rename(iso3c = ref_area)

var_region <- read_csv('./data/country_classification.csv') %>%
          mutate(clst_pimsa_code = str_sub(cluster_pimsa, 1,2)) 

var_region %>%
        filter(!is.na(cluster_pimsa)) %>%
        select(cluster_pimsa, iso3c, country) %>%
        group_by(cluster_pimsa) %>%
        gt() %>%
        tab_header(
        title = md("**Países incluidos en los cálculos de Tabla 1**"),
        subtitle = md("N = 187")) %>%
        cols_label(
                cluster_pimsa = "Tipo de país",
                country = "País",
                iso3c  = "ISO3C"
        ) %>%        
        gtsave('./paper_material/paises_T1.rtf')


horas1 <- horas %>%
        left_join(var_region) %>%
        select(names(var_region), everything()) %>%
        filter(grepl("_AGGREGATE_", classif1)) %>%
        mutate(classif1.label = (str_trim(str_extract(classif1.label, "(?<=:).*")))) %>%
        mutate(classif1.label = case_when(
                classif1 == "ECO_AGGREGATE_TOTAL" ~ "8. Total",
                classif1 == "ECO_AGGREGATE_AGR" ~ "1. Agricultura",
                classif1 == "ECO_AGGREGATE_MAN" ~ "2. Manufactura",
                classif1 == "ECO_AGGREGATE_CON" ~ "3. Construcción",
                classif1 == "ECO_AGGREGATE_MEL" ~ "4. Minería, elect., gas, agua",
                classif1 == "ECO_AGGREGATE_MKT" ~ "5. Comercio, transp., aliment, hoteles, servicios empr.",
                classif1 == "ECO_AGGREGATE_PUB" ~ "6. Adm. pública, servicios soc. y comun.",
                TRUE ~ "9. Sin clasificar")
        ) %>%
        mutate(classif_agg = if_else(classif1 == "ECO_AGGREGATE_AGR", "Agr", "Resto"))


horas1 %>%
        mutate(clst_pimsa_code = if_else(is.na(clst_pimsa_code), "C9. Sin datos",
                                         clst_pimsa_code)) %>%
        group_by(iso3c, cluster_pimsa, clst_pimsa_code, classif1.label, classif_agg) %>%
        summarise(hours = mean(obs_value, na.rm=TRUE),
                  n = n())  %>%
        ungroup() %>%
        ggplot() +
                geom_boxplot(aes(x=classif1.label, y=hours, fill=classif_agg)) +
                facet_wrap(~clst_pimsa_code) +
                labs(y="Cant. horas",
                     x="Rama",
                     fill="Rama agr.") +
                theme_minimal() +
                coord_flip() +
                theme(text = element_text(size=12.3),
                      legend.position = "bottom")

ggsave('./paper_material/plots/grafico4.jpg',  
       width = 10, height=8,
       bg="white")

horas1 %>%
        select(iso3c, country, cluster_pimsa) %>%
        distinct() %>%
        group_by(cluster_pimsa) %>%
        arrange(cluster_pimsa) %>%
        gt() %>%
        tab_header(
                title = md("**Países incluidos en los cálculos de Gráfico 3**"),
                subtitle = md("N = 146")
        ) %>%
        cols_label(
                country = "País",
                iso3c  = "ISO3C"
        ) %>%        
        gtsave('./paper_material/paises_G3.rtf')
