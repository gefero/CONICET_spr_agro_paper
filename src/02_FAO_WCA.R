library(tidyverse)
library(countrycode)
library(patchwork)

### Agrego data CNA 2002 Argentina
format_cna2002_arg <- function(){
        arg2002_sl <- read_csv('./data/arg_cna2002_eap_sin_limites.csv') %>%
                mutate(item = "Holdings without land",
                       hectareas = 0) %>%
                select(anio, provincia, item, EAP_sin_limites,
                       hectareas) %>%
                rename(Number = EAP_sin_limites, 
                       Area = hectareas)
        
        
        arg2002 <- read_csv('./data/arg_cna2002.csv')
        arg2002 <- arg2002 %>%
                mutate(EAP_con_limites = as.numeric(str_replace(EAP_con_limites, "-", "0"))) %>%
                mutate(EAP_con_limites = as.numeric(str_remove_all(EAP_con_limites, " "))) %>%
                #mutate(item = case_when(
                #        rango_superficie == "0 - 5"  ~ "1. Land size 0-5",
                #        rango_superficie %in% c("5.1 - 10", "10.1 - 25",
                #                                "25.1 - 50", "50.1 - 100") ~ "2. Land size 5-100",
                #        rango_superficie %in% c("100.1 - 200", "200.1 - 500") ~ "3. Land size 100.1-500",
                #        TRUE ~ "4. Land size >500")
                #) %>%
                select(-rango_superficie, -provincia_id) %>%
                rename(Number = EAP_con_limites, 
                       Area = hectareas)
        
        arg2002 <- arg2002 %>%
                bind_rows(arg2002_sl) %>%
                select(provincia, item, Number, Area) %>%
                pivot_longer(Number:Area,
                             names_to = "element") %>%
                group_by(item, element) %>%
                summarise(value = sum(value)) %>%
                bind_rows(
                        tibble(
                                item = c("Holdings_","Holdings_"),
                                item_recoded = c("Holdings", "Holdings"),
                                element = c("Area","Number"),
                                value=c(0,333533))
                ) %>%
                mutate(area_code = 9,
                       area_code_m49 = "32",
                       area = "Argentina",
                       item_code = NA,
                       element_code = NA,
                       wca_round_code = 2000,
                       wca_round = 2000,
                       census_year = "2002",
                       census_year_code = "2002",
                       unit = if_else(element == "Area", "ha", "No"),
                       flag = "Added GFR",
                       note = "Added GFR")
        return(arg2002)
        
}

## Cargo dataset
wca <- read_csv('./data/World_Census_Agriculture_E_All_Data_(Normalized).csv') %>%
        janitor::clean_names()

## Ordeno variables de region y agrego
wca <- wca %>%
        mutate(
                area_code_m49 = as.numeric(str_replace(area_code_m49, "'", "")), 
                iso3c = countrycode(area_code_m49, origin="un",  destination = "iso3c")
        ) %>%
        mutate(iso3c = case_when(
                is.na(iso3c) & area == "China" ~ "CHN",
                is.na(iso3c) & area == "China, Taiwan Province of" ~ "TWN",
                is.na(iso3c) & area == "Czechoslovakia" ~ "CSK",
                is.na(iso3c) & area == "Yugoslav SFR" ~ "YUG",
                TRUE ~ iso3c
        ))


## Cargo dataset arg cna 2002
arg2002 <- format_cna2002_arg() %>%
        mutate(
                area_code_m49 = as.numeric(str_replace(area_code_m49, "'", "")), 
                iso3c = countrycode(area_code_m49, origin="un",  destination = "iso3c")
        ) %>%
        select(all_of(names(wca)))


### Join con variables de región
var_region <- read_csv('./data/country_classification.csv') %>%
        mutate(clst_pimsa_code = str_sub(cluster_pimsa, 1,2))

table(var_region$cluster_pimsa, var_region$clst_pimsa_code)

wca <- wca %>%
        bind_rows(arg2002)

## Normalizo variables de fecha y país
wca <- wca %>%
        mutate(
                census_year_final = as.numeric(case_when(
                        census_year_code == "0000" ~ as.character(wca_round),
                        nchar(census_year_code) > 4 ~ str_sub(census_year_code, 1, 4),
                        TRUE ~ census_year_code)
                )
                
        ) %>%
        mutate(census_decade = census_year_final - (census_year_final %% 10)) 

wca <- wca %>%
        left_join(var_region) %>%
        mutate(cluster_pimsa = if_else(is.na(cluster_pimsa), "C9. Sin datos", cluster_pimsa),
               income_group = if_else(is.na(income_group), "99_Sin_datos", income_group),
               income_group_2 = if_else(is.na(income_group_2), "99_Sin_datos", income_group_2),
        )

## Creación escalas  de tamaño
### 0-5 (270030, 270031, 270032, 270033, 270034, 270035)
### 5-10
### 10-20
### 20-50
### 50-100
### 100-200
### 200-500
### 500-1000
### Mas de 1000 (2700303, 2700305), 2700304

### 0-5
### 5-100
### 100-500
### Más de 500


### Genera tabla de items para recodificar
items <- wca %>%
        select(item_code, item) %>%
        distinct() %>%
        mutate(item_recoded = case_when(
                item_code %in% c(270030, 270031, 270032, 270033, 270034, 270035) ~ 'Holding with land size 0-5',
                item_code %in% c(2700303, 2700305, 2700304) ~ 'Holding with land size >=1000',
                item_code ==  27002 ~ "9_holdings",
                #TRUE ~ paste(str_sub(item_code, -2),"_", item)
                TRUE ~ item
                )
        ) %>%
        mutate(item_recoded = str_to_sentence(str_replace(item_recoded, "Holding with |Holdings with |", ""))) %>%
        mutate(item_recoded = case_when(
                item_recoded == "Holdings without land" ~ "No land",
                TRUE ~ item_recoded)
        ) %>%
        mutate(item_recoded_agg = case_when(
                item_recoded %in% c("Land size 0-5", "Land size 0-<5") ~ "1. Land size 0-5",
                item_recoded %in% c("Land size 5-<10", "Land size 10-<20", "Land size 20-<50", "Land size 50-<100") ~ "2. Land size 5-100",
                item_recoded %in% c("Land size 100-<200", "Land size 200-<500", "Land size 200-<500") ~ "3. Land size 100.1-500",
                item_recoded %in% c("Land size 500-<1000", "Land size >=1000") ~ "4. Land size >500",
                item_recoded %in% c("No land") ~ "0. No land",
                TRUE ~ item_recoded)
               )


### Genera tabla final de eaps
wca_eaps <- wca %>%
        filter((str_detect(item, "Holdings with") | item_code =="27002") & element %in% c("Number", "Area")) %>%
        left_join(items) %>%
        select(area_code:area, iso3c:clst_pimsa_code, 
               wca_round, wca_round_code, census_year, census_year_code, 
               item_code, item, item_recoded, item_recoded_agg, element, unit, value)


#arg <- wca_eaps %>% filter(iso3c == "ARG") %>% arrange(census_year_final)

wca_eaps <- wca_eaps %>%
        select(-unit) %>%
        pivot_wider(
                names_from = element,
                values_from = value,
                values_fill = 0
        ) %>%
        rename(number = Number,
               surf = Area)


wca_eaps <- wca_eaps %>%
        mutate(surf = case_when(
                surf == 0 & item_recoded_agg == "1. Land size 0-5" ~ number*2.5,
                surf == 0 & item_recoded_agg == "2. Land size 5-100" ~ number*47.5,
                surf == 0 & item_recoded_agg == "3. Land size 100.1-500" ~ number*200,
                surf == 0 & item_recoded_agg == "4. Land size >500" ~ number*600,
                TRUE ~ surf
        ))

# ### Test 1 de cobertura
# test_cobertura_income <- wca_eaps %>%
#         filter(item_recoded == "9_holdings") %>%
#         group_by(income_group_2, census_decade) %>%
#         summarise(n_reg = n(),
#                   n_eaps=sum(number),
#                   n_surf=sum(surf)
#                   #max_wca_rc = max(census_decade),
#                   #min_wca_rc = min(census_decade)
#                   ) %>%
#         arrange(income_group_2)
# 
# (test_cobertura_income %>%
#         ggplot() + 
#                 geom_line(aes(x=census_decade, y=n_reg_raw, color=income_group),
#                           show.legend = FALSE) + 
#                 theme_minimal() + 
#                 facet_wrap(~income_group) +
#                 labs(x="Ronda de Censos Agrop.",
#                      y="N",
#                      title="Cantidad registros")) +
# (test_cobertura_income %>%
#         ggplot() + 
#                  geom_line(aes(x=census_decade, y=n_reg_wei, color=income_group)) + 
#                  theme_minimal() + 
#                  facet_wrap(~income_group)+
#          labs(x="Ronda de Censos Agrop.",
#               y="N",
#               title="Cantidad de explotaciones")) +
# plot_layout(axes = "collect")
#         
# 
# test_cobertura_pimsa <- wca_eaps %>%
#         filter(item_recoded == "9_holdings" & element == "Number") %>%
#         group_by(cluster_pimsa, wca_round_code) %>%
#         summarise(n_reg_raw=n(),
#                   n_reg_wei=sum(value)
#                   #max_wca_rc = max(wca_round_code),
#                   #min_wca_rc = min(wca_round_code)
#         ) %>%
#         arrange(cluster_pimsa)
# 
# (test_cobertura_pimsa %>%
#                 ggplot() + 
#                 geom_line(aes(x=wca_round_code, y=n_reg_raw, color=cluster_pimsa),
#                           show.legend = FALSE) + 
#                 theme_minimal() + 
#                 facet_wrap(~cluster_pimsa) +
#                 labs(x="Ronda de Censos Agrop.",
#                      y="N",
#                      title="Cantidad registros")) +
#         (test_cobertura_pimsa %>%
#                  ggplot() + 
#                  geom_line(aes(x=wca_round_code, y=n_reg_wei, color=cluster_pimsa), show.legend = FALSE) + 
#                  theme_minimal() + 
#                  facet_wrap(~cluster_pimsa)+
#                  labs(x="Ronda de Censos Agrop.",
#                       y="N",
#                       title="Cantidad de explotaciones")) +
#         plot_layout(axes = "collect")
# 

# small_holder_potrait <- c("Ethiopia", "Ghana", "Kenya", "Malawi",
#                           "Niger", "Nigeria", "Uganda", "Tanzania",
#                           "Bangladesh", "Cambodia", "Indonesia", "Nepal", "Vietnam",
#                           "Bolivia", "Nicaragua", "Guatemala", "Panama", 
#                           "Albania", "Tajikistan")
# 
# var_region %>%
#         filter(country == "Guatemala")
# 
# 
# wca_eaps %>%
#         mutate(sh_potrait = if_else(
#                 country %in% small_holder_potrait,
#                 "In SH potrait",
#                 "Out SH potrait"
#         )) %>%
#         filter(item_recoded_agg == '9_holdings') %>%
#         filter(census_decade %in% c(2000)) %>%
#         group_by(cluster_pimsa, sh_potrait) %>%
#         summarise(n = sum(number),
#                   s = sum(surf)) %>%
#         pivot_wider(names_from = sh_potrait,
#                     values_from=c(n,s)) %>%
#         ungroup() %>%
#         mutate(rep_n =100 * `n_In SH potrait` / (`n_In SH potrait` + `n_Out SH potrait`),
#                rep_s =100 * `s_In SH potrait` / (`s_In SH potrait` + `s_Out SH potrait`)
#                ) %>%
#         select(cluster_pimsa, rep_n, rep_s)
# 
# 
# wca_eaps %>%
#         filter(census_decade %in% c(1980, 1990, 2000, 2010)) %>%
#         group_by(wca_round_code) %>%
#         summarise(
#                 min = min(census_year_final),
#                 max = max(census_year_final)
#                   )



## GRAFICO 2
wca_eaps %>%
        filter(wca_round_code %in% c(1980, 1990, 2000, 2010)) %>%
        mutate(wca_round_code = as.character(wca_round_code)) %>%
        filter(item_recoded_agg == '9_holdings') %>%
        group_by(cluster_pimsa, clst_pimsa_code, wca_round_code) %>%
        summarise(
                exp = sum(number),
                surf = sum(surf)
                  ) %>%
        ungroup() %>%
bind_rows(
        wca_eaps %>%
        filter(wca_round_code %in% c(1980, 1990, 2000, 2010)) %>%
        mutate(wca_round_code = as.character(wca_round_code)) %>%
        filter(item_recoded_agg == '9_holdings') %>%
        filter(iso3c != "RUS") %>%
        filter(cluster_pimsa == "C1. Cap. avanzado") %>%
        group_by(cluster_pimsa, clst_pimsa_code, wca_round_code) %>%
        summarise(
                exp = sum(number),
                surf = sum(surf)
        ) %>%
        ungroup() %>%
        mutate(clst_pimsa_code = "C1 (excluida Rusia)")
) %>%
bind_rows(
        wca_eaps %>%
                filter(wca_round_code %in% c(1980, 1990, 2000, 2010)) %>%
                mutate(wca_round_code = as.character(wca_round_code)) %>%
                filter(item_recoded_agg == '9_holdings') %>%
                filter(iso3c != "CHN") %>%
                filter(cluster_pimsa == "C3. Cap. extensión c/peso campo") %>%
                group_by(cluster_pimsa, clst_pimsa_code, wca_round_code) %>%
                summarise(
                        exp = sum(number),
                        surf = sum(surf)
                        ) %>%
                        ungroup() %>%
                        mutate(clst_pimsa_code = "C3 (excluida China)")
        ) %>%
bind_rows(
        wca_eaps %>%
                filter(wca_round_code %in% c(1980, 1990, 2000, 2010)) %>%
                mutate(wca_round_code = as.character(wca_round_code)) %>%
                filter(item_recoded_agg == '9_holdings') %>%
                filter(iso3c != "IND") %>%
                filter(cluster_pimsa == "C4. Cap. escasa extensión c/peso campo") %>%
                group_by(cluster_pimsa, clst_pimsa_code, wca_round_code) %>%
                summarise(
                        exp = sum(number),
                        surf = sum(surf)
                        ) %>%
                ungroup() %>%
                mutate(clst_pimsa_code = "C4 (excluida India)")
        ) %>%
        mutate(mean_surf = surf/exp) %>% 
        mutate(clst_pimsa_code = if_else(is.na(clst_pimsa_code), 
                                         "C9. Sin datos", clst_pimsa_code)
        ) %>%
        ggplot() + 
                geom_line(aes(x=wca_round_code, y=exp, 
                              group=clst_pimsa_code,
                              color=clst_pimsa_code), show.legend = FALSE) +
                labs(x="Ronda de censos agropecuarias (FAO)",
                     y="Explotaciones") +
        scale_y_continuous(labels = scales::comma) +
                facet_wrap(~clst_pimsa_code) +
        theme_minimal() +
        theme(text=element_text(size=12))

ggsave('./paper_material/plots/grafico2.jpg',
       width = 8, height=8,
       bg="white")



library(gt)

wca_eaps %>%
        filter(wca_round %in% c(1980, 1990, 2000, 2010)) %>%
        filter(item_recoded_agg == '9_holdings') %>%
        select(cluster_pimsa, iso3c, country, wca_round) %>%
        group_by(cluster_pimsa, country) %>%
        summarise(years = list(wca_round)) %>%
        drop_na() %>%
        gt() %>%
        tab_header(
                title = md("**Países incluidos en los cálculos de Gráfico 2**"),
        ) %>%
        cols_label(
                country = "País",
                cluster_pimsa  = "Tipo de país",
                years = "Años con dato"
        ) %>%
        gtsave('./paper_material/paises_G2.rtf')


## GRAFICO 3
wca_eaps_agg <- wca_eaps %>%
        filter(wca_round_code %in% c(1990, 2000, 2010, 2020)) %>% 
        filter(item_recoded != '9_holdings') %>%
        mutate(item_recoded_agg_spa = case_when(
                item_recoded_agg == "0. No land" ~ "0. Sin tierra",
                item_recoded_agg == "1. Land size 0-5" ~ "1. 0-5 has",
                item_recoded_agg == "2. Land size 5-100" ~ "2. 5.1-100 has",
                item_recoded_agg == "3. Land size 100.1-500" ~ "3. 100.1-500 has",
                item_recoded_agg == "4. Land size >500" ~ "4. > 500 has")
        ) %>%
        mutate(clst_pimsa_code = if_else(is.na(clst_pimsa_code), 
                                         "C9. Sin datos", clst_pimsa_code) 
               ) %>%
        group_by(cluster_pimsa, clst_pimsa_code, wca_round_code, item_recoded_agg_spa) %>%
        summarise(number = sum(number),
                  surf = sum(surf)) %>% 
        ungroup() %>%
        mutate(mean_area = surf / number) %>%
        group_by(cluster_pimsa, clst_pimsa_code, wca_round_code) %>%
        mutate(p_area = surf / sum(surf) * 100,
               p_number = number / sum(number) * 100)

wca_eaps_agg %>%
        mutate(wca_round_code = as.character(wca_round_code)) %>%
        ggplot() + 
        geom_col(aes(x=wca_round_code, y=p_number, fill=item_recoded_agg_spa)) +
        scale_fill_viridis_d() +
        facet_wrap(~clst_pimsa_code) + 
        labs(x="Ronda de censos agropecuarios (FAO)",
             y= "% expl.",
             fill="Estrato de tamaño (agrupado)") +
        theme_minimal() +
        theme(text=element_text(size=14), 
              legend.position = "bottom")


ggsave('./paper_material/plots/grafico3.jpg',
       width = 11, height=8,
       bg="white")

wca_eaps %>%
        filter(wca_round_code %in% c(1990, 2000, 2010, 2020)) %>% 
        filter(item_recoded != '9_holdings') %>%
        select(cluster_pimsa, iso3c, country) %>%
        arrange(cluster_pimsa, country) %>%
        distinct()

wca_eaps %>%
        filter(wca_round_code %in% c(1990, 2000, 2010, 2020)) %>% 
        filter(item_recoded != '9_holdings') %>%
        select(cluster_pimsa, iso3c, country, wca_round) %>%
        arrange(cluster_pimsa, country, wca_round) %>%
        distinct() %>%
        group_by(cluster_pimsa, country) %>%
        summarise(years = list(wca_round)) %>%
        drop_na() %>%
        gt() %>%
        cols_label(
                country = "País",
                cluster_pimsa  = "Tipo de país",
                years = "Años con dato"
        ) %>%
        tab_header(
                title = md("**Países incluidos en los cálculos de Gráfico 3**"),
        ) %>%
        gtsave('./paper_material/paises_G3.rtf')


# 
# wca_eaps_agg %>%
#         mutate(wca_round_code = as.character(wca_round_code)) %>%
#         ggplot() + 
#         geom_col(aes(x=wca_round_code, y=p_area, fill=item_recoded_agg)) +
#         scale_fill_viridis_d() +
#         facet_wrap(~cluster_pimsa) + 
#         labs(x="Ronda de censos agropecuarios (FAO)",
#              y= "% sup.",
#              fill="Estrato de tamaño (agrupado)") +
#         theme_minimal()
# 
# wca_eaps_agg %>%
#         mutate(wca_round_code = as.character(wca_round_code)) %>%
#         ggplot() + 
#         geom_line(aes(x=wca_round_code, y=mean_area, 
#                       color=cluster_pimsa,
#                       group=cluster_pimsa)) +
#         scale_color_viridis_d("M") +
#         facet_wrap(~item_recoded_agg, scales="free_y") + 
#         labs(x="Ronda de censos agropecuarios (FAO)",
#              y= "%",
#              fill="Estrato de tamaño (agrupado)") +
#         theme_minimal()
# 
# wca_eaps %>%
#         filter(wca_round_code %in% c(1980, 1990, 2000, 2010, 2020)) %>% 
#         #mutate(wca_round_code = as.character(wca_round_code)) %>%
#         filter(item_recoded == '9_holdings') %>%
#         group_by(iso3c, area, cluster_pimsa, wca_round_code) %>%
#         summarise(number = sum(number),
#                   surf = sum(surf)) %>%
#         mutate(mean_surf = surf / number) %>%
#         arrange(cluster_pimsa, iso3c) %>%
#         print(n=500)
# 

        


# wca_eaps_agg %>%
#         filter(item_recoded_agg %in% c("0. No land", "1. Land size 0-5")) %>%
#         group_by(cluster_pimsa, wca_round_code) %>%
#         summarise(p = sum(p_number)) %>%
#         print(n=100)
# 
# wca_eaps %>%
#         filter(wca_round_code %in% c(1990, 2000, 2010, 2020)) %>% 
#         #mutate(wca_round_code = as.character(wca_round_code)) %>%
#         filter(item_recoded == '9_holdings') %>%
#        # filter(element == "Number") %>%
#         group_by(cluster_pimsa, wca_round_code) %>%
#         summarise(n=sum(number)) %>%
#         ggplot() + 
#                 geom_smooth(aes(x=wca_round_code, y=n, color=cluster_pimsa, group=cluster_pimsa)) +
#                 scale_color_viridis_d() +
#                 theme_minimal()













# ### Diseño de panel de paises
# iso3c1990 <- wca_eaps %>%
#         filter(item_recoded != '9_holdings') %>%
#         filter(wca_round_code == 1990) %>%
#         select(iso3c) %>%
#         distinct() %>%
#         pull()
# 
# iso3c2000 <- wca_eaps %>%
#         filter(item_recoded != '9_holdings') %>%
#         filter(wca_round_code == 2000) %>%
#         select(iso3c) %>%
#         distinct() %>%
#         pull()
# 
# iso3c2010 <- wca_eaps %>%
#         filter(item_recoded != '9_holdings') %>%
#         filter(wca_round_code == 2010) %>%
#         select(iso3c) %>%
#         distinct() %>%
#         pull()
# 
# 
# iso3c_1990_2010 <- intersect(intersect(iso3c2010, iso3c2000), iso3c1990)
# 
# ### Datos de panel
# wca_eaps_panel_agg <- wca_eaps %>%
#         #filter(census_decade > 1980 & census_decade < 2020) %>%
#         filter(iso3c %in% iso3c_1990_2010 & (wca_round_code < 2020 & wca_round_code > 1980)) %>% 
#         filter(item_recoded != '9_holdings') %>%
#         group_by(cluster_pimsa, wca_round_code, item_recoded_agg, element, unit) %>%
#         summarise(n = sum(value)) %>%
#         ungroup() %>%
#         select(-unit) %>%
#         pivot_wider(
#                 names_from = element,
#                 values_from = n,
#                 values_fill = 0
#         ) %>%
#         group_by(cluster_pimsa, wca_round_code) %>%
#         mutate(p_area = Area / sum(Area) * 100,
#                p_number = Number / sum(Number) * 100)
# 
# wca_eaps_panel_agg %>%
#         mutate(wca_round_code = as.character(wca_round_code)) %>%
#         filter(!is.na(cluster_pimsa)) %>%
#         ggplot() + 
#         geom_col(aes(x=wca_round_code, y=p_number, fill=item_recoded_agg)) +
#         scale_fill_viridis_d() +
#         facet_wrap(~cluster_pimsa) + 
#         labs(x="Ronda de censos agropecuarios (FAO)",
#              y= "%",
#              fill="Estrato de tamaño (agrupado)") +
#         theme_minimal()
# 
