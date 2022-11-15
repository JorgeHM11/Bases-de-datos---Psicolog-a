
# Cargar librerías --------------------------------------------------------
pacman::p_load("tidyverse", "janitor")

# Importar data -----------------------------------------------------------
data <- read_csv("shamiri_imputed_dataset.csv")

# Transformar variables ---------------------------------------------------

data <- data |> 
  mutate(
    PHQ_Total  = PHQ1 + PHQ2 + PHQ3 + PHQ4 + 
                 PHQ5 + PHQ6 + PHQ7 + PHQ8,
    GAD_Total  = GAD1 + GAD2 + GAD3 + GAD4 +
                 GAD5 + GAD6 + GAD7,
    MSSS_Total = MSSS1 + MSSS2 + MSSS3 + MSSS4 +
                 MSSS5 + MSSS6 + MSSS7 + MSSS8 +
                 MSSS9 + MSSS10+ MSSS11+ MSSS12,
    MSSS_Total = MSSS_Total/12
  )

# Private sub-county: Starays & Elite
# Public national: AGHS & AHS 
# Public county: Olympic

data_limpia <- data |> 
  mutate(
    Gender = case_when(
      Gender == "M" ~ "Hombre",
      Gender == "F" ~ "Mujer"
    ),
    School = case_when(
      School %in% c("Starays", "Elite") ~ "Privada distrital",
      School %in% c("AGHS", "AHS") ~ "Publica nacional",
      School == "Olympic" ~ "Publica municipal"
    ),
    Tribe = case_when(
      Tribe == "Minority" ~ "Minoritaria",
      Tribe == "Majority" ~ "Predominante"
    )
  ) |> 
  #renombrar el resto de variables
  rename(
    sexo    = Gender,
    edad    = Age,
    escuela = School,
    etnia   = Tribe,
    DEP_Total   = PHQ_Total,
    MSPSS_Total = MSSS_Total 
  ) |> 
  rename_with(~ paste("DEP", 1:8, sep = ""), PHQ1:PHQ8) |> 
  rename_with(~ paste("MSPSS", 1:12, sep = ""), MSSS1:MSSS12) |> 
  # quitar variables que no se usarán para el análisis
  select(-c(ParticipantID)) |> 
  #mover las variables sociodemográficas al principio
  relocate(c(etnia:edad, escuela), .before = DEP1) 

# Exportar data -----------------------------------------------------------
rio::export(data_limpia, "data4_clean.csv")
