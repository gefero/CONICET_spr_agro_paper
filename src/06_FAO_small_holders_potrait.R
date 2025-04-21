library(tidyverse)
library(rvest)

url <- "https://www.fao.org/family-farming/data-sources/dataportrait/indicator-details/en/?ind=83450"
pagina <- read_html(url)

tablas <- pagina %>% html_elements("table")

eaps <- tablas[[1]] %>% html_table(fill = TRUE)
head(gdp_tabla)

colnames(eaps) <- eaps[1, ]  # Asigna la primera fila como nombres de columnas
eaps <- eaps[-1, ]           # Elimina la primera fila
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
colnames(income) <- income[1, ]
income <- income[-1, ]

# 6. Renombrar columnas (ajustar si cambia el contenido)
colnames(income) <- c(
        "region",
        "pais_anio",
        "ingreso_hogar",
        "porcentaje_ingreso_cultivos",
        "porcentaje_ingreso_agricola",
        "porcentaje_ingreso_salarios_agr",
        "porcentaje_ingreso_no_agr",
        "porcentaje_ingreso_iransferencias",
        "porcentaje_ingreso_otros",
        "tasa_pobreza"
)

# 7. Convertir columnas numéricas (eliminar comas y convertir a numérico)
income <- income %>%
        filter(porcentaje_ingreso_cultivos != "Smaller_farm") %>%
        mutate(across(ingreso_hogar:tasa_pobreza, ~ as.numeric(gsub(",", "", .))))

# 8. Vista previa
print(head(income))

# 9. (Opcional) Filtrar datos por región
mi_tabla %>% filter(Región == "Sub-Saharan Africa")
