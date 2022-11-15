
# Cargar librerías --------------------------------------------------------
pacman::p_load("tidyverse", "janitor")

# Importar data -----------------------------------------------------------
data <- rio::import("COVID_data.csv")

# Transformar variables ---------------------------------------------------
data_limpia <- data |> 
  #renombrar variables
  rename(
     Domicilio = Residence,
     Educacion = Education,
     Sexo      = Sex,
     Edad      = Years_old,
     C19_propio= Self_sick,
     C19_otros = Know_recode
  ) |> 
  rename_with(~ paste("Severidad", 1:3, sep = "_"), Severity_1:Severity_3) |> 
  rename_with(~ paste("Reactancia", 1:11, sep = "_"), Reactance_1:Reactance_11) |> 
  rename_with(~ paste("Actitud", 1:3, sep = "_"), Attitude_1:Attitude_3) |> 
  rename_with(~ paste("NorSocial", 1:3, sep = "_"), Social_norms_1:Social_norms_3) |> 
  rename_with(~ paste("NorMoral", 1:3, sep = "_"), Moral_norms_1:Moral_norms_3) |> 
  rename_with(~ paste("Control", 1:4, sep = "_"), Beh_control_1:Beh_control_4) |> 
  rename_with(~ paste("Intencion", 1:3, sep = "_"), Intention_1:Intention_3) |> 
  # Modificar categorias de variables demográficas
  mutate(
    Domicilio = case_when(
      Domicilio == 1 ~ "Poblado",
      Domicilio == 2 ~ "Ciudad_pequeña",
      Domicilio == 3 ~ "Ciudad_mediana",
      Domicilio == 4 ~ "Ciudad_grande",
      Domicilio == 5 ~ "Ciudad_enorme"
    ),
    Educacion = case_when(
      Educacion == 1 ~ "Primaria",
      Educacion == 2 ~ "Técnica",
      Educacion == 3 ~ "Secundaria",
      Educacion == 4 ~ "Post-secundaria",
      Educacion == 5 ~ "Bachillerato",
      Educacion == 6 ~ "Universitaria",
    ),
    Sexo = case_when(
      Sexo == 0 ~ "Hombre",
      Sexo == 1 ~ "Mujer"
    ),
    C19_propio = case_when(
      C19_propio == 2 ~ "No",
      C19_propio == 1 ~ "Si",
    ),
    C19_otros = case_when(
      C19_otros == 0 ~ "No",
      C19_otros == 1 ~ "Si",
    )
  ) |> 
  #quitar variables que no se usarán para el análisis
  select(-c(Id, Survey_finish_time, Age, Know, Reactance:Intention)) |> 
  #mover las variables sociodemográficas al principio
  relocate(c(Domicilio:C19_otros), .before = Severidad_1)

# Exportar data -----------------------------------------------------------
rio::export(data_limpia, "data5_clean.csv")
