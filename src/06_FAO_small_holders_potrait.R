library(tidyverse)
library(rvest)

url <- "https://www.fao.org/family-farming/data-sources/dataportrait/indicator-details/en/?ind=83450"
pagina <- read_html(url)

tablas <- pagina %>% html_elements("table")

eaps <- tablas[[1]] %>% html_table(fill = TRUE)

colnames(eaps) <- eaps[1, ]  # Asigna la primera fila como nombres de columnas
eaps <- eaps[-1, ]           # Elimina las primeras 2 filas
colnames(eaps) <- c("region", "pais_anio", "pequeñas_explotaciones", "otras_explotaciones", "total_explotaciones")

eaps<- eaps %>%
        filter(pais_anio != "Farm size » Total number of holdings") %>%
        filter(pais_anio != "")



eaps <- eaps %>%
        mutate(across(pequeñas_explotaciones:total_explotaciones, ~ as.numeric(gsub(",", "", .))))


###
# ------------------------------------------
# Scraping de tabla de ingresos - FAO (rvest)
# URL: https://www.fao.org/family-farming/data-sources/dataportrait/income/en/
# ------------------------------------------



# 2. Leer la página
url <- "https://www.fao.org/family-farming/data-sources/dataportrait/income/en/"
pagina <- read_html(url)

# 3. Extraer las tablas
tablas <- pagina %>% html_elements("table")
cat("Número de tablas encontradas:", length(tablas), "\n")

# 4. Elegir y convertir la tabla (ajustar índice si es necesario)
income <- tablas[[1]] %>% html_table(fill = TRUE)

# 5. Limpiar encabezados
colnames(income) <- income[2, ]
income <- income[-1, ]

# 6. Renombrar columnas (ajustar si cambia el contenido)
colnames(income) <- c(
        "region",
        "pais_anio",
        "indicador",
        "pequeñas_explotaciones",
        "otras_explotaciones",
        "total_explotaciones"
)

# 7. Convertir columnas numéricas (eliminar comas y convertir a numérico)
income <- income %>%
        filter(pequeñas_explotaciones != "Smaller farm")

income <- income %>%
        mutate(across(pequeñas_explotaciones:total_explotaciones, ~ as.numeric(gsub(",", "", .))))


income %>% group_by(indicador) %>% summarise(max_peq = max(pequeñas_explotaciones),
                                             min_peq = min(pequeñas_explotaciones),
                                             med_peq = median(pequeñas_explotaciones),
                                             mean_peq = mean(pequeñas_explotaciones),
                                             max_rst = max(otras_explotaciones),
                                             min_rst = min(otras_explotaciones),
                                             med_rst = median(otras_explotaciones),
                                             mean_rst = mean(otras_explotaciones)
                                             )
