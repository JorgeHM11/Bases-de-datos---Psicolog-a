
# Cargar librerías --------------------------------------------------------
pacman::p_load("tidyverse", "janitor")

# Importar data -----------------------------------------------------------
data <- read_csv("ER_NegAffect_health_mediation_data.csv")

# Transformar variables ---------------------------------------------------

tabyl(data, Gender) # Male = 1, Female = 2
tabyl(data, Ethnicity) 
tabyl(data, College) 

data_limpia <- data |> 
  mutate(
    Gender = case_when(
      Gender == 1 ~ "Hombre",
      Gender == 2 ~ "Mujer"
    )
  ) |> 
  #renombrar el resto de variables
  rename(
    Sexo = Gender,
    Edad = Age,
    ValCogn = ER_Reapp,
    SupExp = ER_Supp,
    Ansiedad = AnxSum,
    Depresion = DepSum,    
    Bien_gen = HealSum,
    RE_ValCog = ER_Reapp,  
    RE_SupExp = ER_Supp,   
    PROMIS_Ans_1 = AnxDep_1,
    PROMIS_Ans_2 = AnxDep_2,
    PROMIS_Ans_3 = AnxDep_3,
    PROMIS_Ans_4 = AnxDep_4,
    PROMIS_Dep_5 = AnxDep_5,
    PROMIS_Dep_6 = AnxDep_6,
    PROMIS_Dep_7 = AnxDep_7,
    PROMIS_Dep_8 = AnxDep_8,
    PROMIS_BG_1  = HealGen,  
    PROMIS_BG_2  = HealQLife,
    PROMIS_BG_3  = HealMent, 
    PROMIS_BG_4  = HealSoc
  ) |> 
  # quitar variables que no se usarán para el análisis
  select(-c(respID, Ethnicity, Race, College, Social_1:SocEmoSupport,
            AnxDep, ER_SitSelect, ER_Distract))

# Exportar_data -----------------------------------------------------------
writexl::write_xlsx(data_limpia, "base_2.xlsx")

