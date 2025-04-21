library(Rilostat)
library(tidyverse)
#toc <- get_ilostat_toc()

## Inestabilidad

inest_rama <- get_ilostat(id="EMP_TEMP_SEX_ECO_MJH_NB_A",
                          lang = "es",
                          type = "both",
                          filters = list(
                                  timefrom = 2005,
                                  timeto = 2015,
                                  sex = "SEX_T")) %>%
        rename(iso3c = ref_area)

inest_rama_agg <- inest_rama %>% 
        #filter(sex=="SEX_T" & !grepl("_AGGREGATE_", classif1) & (time >=2005 & time <=2015) & classif2!="MJH_AGGREGATE_TOTAL") %>%
        filter(!grepl("_AGGREGATE_", classif1) & classif2!="MJH_AGGREGATE_TOTAL") %>%
        group_by(ref_area, classif1, classif2) %>%
        summarise(
                mean=mean(obs_value, na.rm=TRUE)
        ) %>%
        mutate(prop_mean = mean/sum(mean),
               tot_abs = sum(mean)) %>%
        ungroup() 


countries <- read_csv('../PIMSA_pobreza/data/proc/paises_clustering_final.csv') ## Archivo disponible a demanda
var_region <- read_csv('.data/country_classification.csv')

inest_rama_agg %>%
        rename(iso3c=ref_area) %>%
        filter(classif2=="MJH_AGGREGATE_MULTI") %>%
        left_join(var_region) %>%
        group_by(cluster_pimsa, classif1) %>%
        summarise(p_mean = 100*mean(prop_mean, na.rm=TRUE),
                  p_median = 100*median(prop_mean, na.rm=TRUE),
                  p_wmean = 100*weighted.mean(prop_mean, tot_abs, na.rm=TRUE)) %>%
        ungroup() %>%
        select(cluster_pimsa,classif1,p_mean) %>%
        pivot_wider(names_from = classif1,
                    values_from=p_mean) %>%
        drop_na() %>%
        select(cluster_pimsa, ECO_SECTOR_AGR, ECO_SECTOR_NAG, ECO_SECTOR_IND, ECO_SECTOR_SER, ECO_SECTOR_X, ECO_SECTOR_TOTAL)


inest_rama_agg
        

#cat_ocup_rama <- get_ilostat(id="EMP_TEMP_SEX_STE_ECO_NB_A")
#write_csv(cat_ocup_rama, './data/ILOSTAT_cat_ocup_rama_full.csv')

# cat_ocup_rama %>%
#         filter(sex=="SEX_T" & classif1=="STE_AGGREGATE_TOTAL" & classif2=="ECO_SECTOR_TOTAL") %>%
#         group_by(ref_area) %>%
#         summarise(years_data=n(),
#                   year_min=as.numeric(min(time)),
#                   year_max=as.numeric(max(time))) %>%
#         ungroup() %>%
#         mutate(p_years_data = years_data / (year_max-year_min)) %>%
#         filter(year_min >= 1995) %>%
#         nrow()
# 
# 

