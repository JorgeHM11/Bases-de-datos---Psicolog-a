
# Cargar librerías --------------------------------------------------------
library(pacman)
p_load(tidyverse, haven, openxlsx)


# Importar base de datos --------------------------------------------------
data <- read_sav("Database.sav")


# Identificar categorías según lo reportado en el artículo ----------------
frecuencia <- function(variable){
  transform(table(variable), 
            Porcentaje = round(prop.table(Freq)*100, 1)) %>% 
    arrange(desc(Freq))
} 

frecuencia(data$I_01_Gender)     # Male = 0 , Female = 1
frecuencia(data$program_binary)  # Graduate = 0, Undergradute = 1
frecuencia(data$I_09_UT)         # Public = 0 , Private = 1
frecuencia(data$sleep_quality_binary) # Good = 0, Poor = 1

# Transformar variables ---------------------------------------------------
data_clean <- data %>% 
  #transformar variables y renombrarlas
  mutate(ans_cat = case_when(Anx_total  <  10 ~ "No",
                             Anx_total >= 10 ~ "Sí"),
         dep_cat = case_when(Dep_total  <  13 ~ "Sí",
                             Dep_total >= 13 ~ "No"),
         miedo_cat = case_when(fear_total <  8 ~ "No",
                               fear_total >= 8 ~ "Sí"),
         sueño_cat   = case_when(sleep_quality_binary == 0 ~ "Buena",
                                 sleep_quality_binary == 1 ~ "Mala"),
         sexo        = case_when(I_01_Gender == 0 ~ "Hombre",
                                 I_01_Gender == 1 ~ "Mujer"),
         grado_acd   = case_when(program_binary == 0 ~ "Posgrado",
                                 program_binary == 1 ~ "Pregrado"),
         tipo_univ   = case_when(I_09_UT  == 0 ~ "Público",
                                 I_09_UT  == 1 ~ "Privado")
         ) %>% 
  #renombrar el resto de variables
  rename(ansiedad  = Anx_total,
         depresion = Dep_total,
         miedo     = fear_total,
         sueño     = sleep_total) %>% 
  #quitar variables que no se usarán para el análisis
  select(-c(serialno, I_01_Gender:program_binary)) %>% 
  #mover las variables sociodemográficas al principio
  relocate(c(sexo:tipo_univ), .before = ansiedad)


# Verificar los valores con los reportados en el artículo -----------------
frecuencia(data$Anx_cat)  
frecuencia(data$Dep_cat)  
frecuencia(data$fear_cat)  


# Exportar base de datos --------------------------------------------------
write.xlsx(data_clean, "data1_clean.xlsx")






