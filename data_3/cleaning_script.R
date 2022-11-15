
# Cargar librerías --------------------------------------------------------
library(pacman)
p_load(tidyverse, openxlsx)

# Importar base -----------------------------------------------------------
data_1 <- read.xlsx("Dataset_3.xlsx", sheet = 1) 
data_2 <- read.xlsx("Dataset_3.xlsx", sheet = 2) 

# Renombrar totales y totales categóricos
data_1 <- rename(data_1, total_ase = TOTAL, cat_ase = CODING)
data_2 <- rename(data_2, total_bur = TOTAL, cat_bur = NEW.CODING)

# Unir bases --------------------------------------------------------------
data <- inner_join(data_1, data_2, by = "NO")


# Verificar categorías de variable "SCHOOL" -------------------------------
frecuencia <- function(variable){
  transform(table(variable), 
            Porcentaje = round(prop.table(Freq)*100, 1)) %>% 
    arrange(desc(Freq))
} 
frecuencia(data$SCHOOL)


# Transformar variables ---------------------------------------------------

data_clean <- data %>% 
  #transformar variables y renombrarlas
  mutate(cat_ase   = case_when(cat_ase  == 1 ~ "Bajo",
                               cat_ase  == 2 ~ "Moderado",
                               cat_ase  == 3 ~ "Alto"),
         cat_bur   = case_when(cat_bur  == 1 ~ "Leve",
                               cat_bur  == 2 ~ "Moderado",
                               cat_bur  == 3 ~ "Severo"),
         sexo      = case_when(SEX      == 1 ~ "Hombre",
                               SEX      == 2 ~ "Mujer"),
         convive   = case_when(LIVE     == 1 ~ "Solo",
                               LIVE     == 2 ~ "Con familia"),
         escuela   = case_when(SCHOOL   == 1 ~ "Ciencias",
                               SCHOOL   == 2 ~ "Ciencias Sociales",
                               SCHOOL   == 3 ~ "Vocación salud",
                               SCHOOL   == 4 ~ "Vocación no salud")
  ) %>% 
  #renombrar el resto de variables
  rename(edad  = AGE,
         ase_total = total_ase,
         bur_total = total_bur,
         ase_cat   = cat_ase,
         bur_cat   = cat_bur) %>% 
  rename_with(~ paste("ase", 1:31, sep = ""), X1:X31) %>% 
  rename_with(~ paste("bur", 1:24, sep = ""), Y1:Y24) %>% 
  #quitar variables que no se usarán para el análisis
  select(-c(SEX, LIVE, SCHOOL, NO, INITIAL)) %>% 
  #mover las variables sociodemográficas al principio
  relocate(c(sexo:escuela), .before = edad)


# Exportar data -----------------------------------------------------------
write.xlsx(data_clean, "data3_clean.xlsx")


