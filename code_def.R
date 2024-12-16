rm(list=ls())
library(readr)
library(xtable)
library(patchwork)
library(stargazer)
library(tidyverse)
library(survival)
library(wbstats)
options(scipen= 999)


# Import data -------------------------------------------------------------
ministerial_replacement <- read.csv("Data/new_database_ministerial_replacement_with_wikipedia.csv")
ministerial_replacement$correccion_fechadeentrada <- as.Date(ministerial_replacement$correccion_fechadeentrada)
ministerial_replacement$informacion_personal_nacimiento_date <- as.Date(ministerial_replacement$informacion_personal_nacimiento_date)

ministerial_replacement$dummy_replacement_without_censoring <- 1

# Calculate the age when assumed using year difference
ministerial_replacement$age_when_assume <- as.numeric(difftime(ministerial_replacement$correccion_fechadeentrada,
                                                               ministerial_replacement$informacion_personal_nacimiento_date,
                                                               units = "days")) / 365.25  # divide by 365.25 to account for leap years

# Adjust the calculation to get complete years
ministerial_replacement$age_when_assume <- floor(ministerial_replacement$age_when_assume)

ministerial_replacement<- ministerial_replacement %>%
  mutate(regiones = case_when(
    provincia_nacimiento == "Buenos Aires" ~ "Buenos Aires",
    provincia_nacimiento == "Córdoba" ~ "Córdoba",
    provincia_nacimiento == "Mendoza" ~ "Mendoza",
    provincia_nacimiento == "Santa Fe" ~ "Santa Fe",
    provincia_nacimiento == "Entre Ríos" ~ "Entre Ríos",
    provincia_nacimiento %in% c("Jujuy", "Salta", "Tucumán", "Catamarca",
                                "La Rioja", "Santiago del Estero") ~"NOA",
    provincia_nacimiento %in% c("Formosa", "Chaco", "Corrientes", "Misiones") ~"NEA (sin ER y SF)",
    provincia_nacimiento %in% c("San Juan", "San Luis") ~"Cuyo (sin MDZ)",
    provincia_nacimiento %in% c("La Pampa", "Neuquen", "Río Negro",
                                "Tierra del Fuego", "Chubut",
                                "Santa Cruz") ~"Patagonia"
  ))

ministerial_replacement <- ministerial_replacement %>%
  mutate(
    Department = case_when(
      clasificacion_area_amplio == "Área Interior-Exteriores-Gabinete" ~ "Interior-Exterior-Cabinet",
      clasificacion_area_amplio == "Área Defensa" ~ "Defense",
      clasificacion_area_amplio == "Área Cultura-Ciencia-Educación" ~ "Culture-Science-Education",
      clasificacion_area_amplio == "Área Salud-Seguridad"  ~ "Health-Security",
      clasificacion_area_amplio == "Área Justicia-Otros" ~ "Justice-Others",
      clasificacion_area_amplio == "Área Económica"  ~ "Economic",
      clasificacion_area_amplio == "Área Obrás Públicas-Transporte" ~ "Infraestructure-Transportation",
      clasificacion_area_amplio == "Área Social"  ~ "Social",
      
    )
  )

V_Dem_CY_Full_Others_v14 <- read_csv("Data/V-Dem-CY-FullOthers-v14_csv_YyKfizl/V-Dem-CY-Full+Others-v14.csv")
V_Dem_ARG <- V_Dem_CY_Full_Others_v14[V_Dem_CY_Full_Others_v14$country_name == "Argentina", ]
V_Dem_ARG <- V_Dem_ARG %>%
  select(country_name, year,
         e_cow_exports, e_cow_imports, e_gdp, e_gdppc,e_miinflat, e_total_fuel_income_pc,
         e_total_oil_income_pc,e_total_resources_income_pc) %>%
  filter(year >= 1862)

gini_wb <- wb_data("SI.POV.GINI",country = "ARG", return_wide = F)
ministerial_replacement <- ministerial_replacement %>%
  left_join(V_Dem_ARG, by = c("Year"="year")) %>%
  left_join(gini_wb %>% select(date, gini_index = value), 
            by = c("Year"="date"))

# Convert dates to the proper format if not already
ministerial_replacement$admin_start <- as.Date(ministerial_replacement$comienzo_administracion)
ministerial_replacement$admin_end <- as.Date(ministerial_replacement$fin_administracion)
ministerial_replacement$minister_start <- as.Date(ministerial_replacement$correccion_fechadeentrada)
ministerial_replacement$minister_end <- as.Date(ministerial_replacement$correccion_fechadesalida)

# Calculate the total duration of the administration
ministerial_replacement$admin_duration <- as.numeric(ministerial_replacement$admin_end - ministerial_replacement$admin_start)

# Calculate the scaled start and end dates of the minister's term
ministerial_replacement$minister_start_scaled <- (ministerial_replacement$minister_start - ministerial_replacement$admin_start) / ministerial_replacement$admin_duration
ministerial_replacement$minister_end_scaled <- (ministerial_replacement$minister_end - ministerial_replacement$admin_start) / ministerial_replacement$admin_duration
ministerial_replacement$minister_term_duration_scaled <- (ministerial_replacement$minister_end - ministerial_replacement$minister_start) / ministerial_replacement$admin_duration
ministerial_replacement <- ministerial_replacement %>%
  mutate(e_gdp_lag = lag(e_gdp, n = 1, default = NA),
         e_gdppc_lag = lag(e_gdppc, n = 1, default = NA),
         e_miinflat_lag = lag(e_miinflat, n = 1, default = NA),
         e_cow_exports_lag = lag(e_cow_exports , n = 1, default = NA),
         gini_index_lag = lag(gini_index , n = 1, default = NA)
  )

ministerial_secu<- ministerial_replacement[ministerial_replacement$Department == "Health-Security", ]

ministerial_replacement <- ministerial_replacement %>%
  mutate(Department_security_health_separated = case_when(
    Department == "Health-Security" & ministerio %in% c("seguridad", "justicia, seguridad y derechos humanos") ~ "Security",
    Department == "Health-Security" & ministerio %in% c("salud", "salud pública", "salud pública y medio ambiente") ~ "Health",
    T ~ Department
  )
           )


# Table n°1 ---------------------------------------------------------------

ministerial_replacement_table <- as.data.frame(ministerial_replacement)

ministerial_replacement_table$comienzo_administracion  <- as.Date(ministerial_replacement_table$comienzo_administracion, format = "%Y-%m-%d")
ministerial_replacement_table$fin_administracion <- as.Date(ministerial_replacement_table$fin_administracion, format = "%Y-%m-%d")

ministerial_replacement_table$diferencia_dias_adm <- as.numeric(ministerial_replacement_table$fin_administracion - ministerial_replacement_table$comienzo_administracion)
ministerial_replacement_table$duration_minister_in_yrs_adm = as.duration(ministerial_replacement_table$comienzo_administracion %--% ministerial_replacement_table$fin_administracion) / dyears(1)


dates_ministers <- ministerial_replacement_table %>%
  group_by(presidente) %>%
  summarise(dates = paste0("(", comienzo_administracion, " - ", fin_administracion, ")"),
            tipo_regimen, duration_minister_in_yrs_adm ) %>%
  distinct(presidente, .keep_all = T)

cantidad_ministros <- ministerial_replacement_table %>%
  group_by(presidente) %>%
  summarise(count_ministers = n(),
            median_duration_minister = median(duration_minister_in_yrs, na.rm = TRUE))

cantidad_ministros_replacement<- ministerial_replacement_table %>%
  filter(dummy_replacement==1) %>%
  group_by(presidente) %>%
  summarise(count_ministers_replacement = n())

head(ministerial_replacement_table)
table_def <- cantidad_ministros %>%
  left_join(cantidad_ministros_replacement) %>%
  mutate(count_ministers_replacement = ifelse(is.na(count_ministers_replacement), 0, count_ministers_replacement),
        percentage = count_ministers_replacement/count_ministers ) %>%
  left_join(dates_ministers) %>%
 # mutate(presidente = paste0(presidente, " ", dates)) %>%
  select(presidente, tipo_regimen, duration_minister_in_yrs_adm, median_duration_minister )
table_def
xtable(table_def)
# Data Section: Descriptive Analysis --------------------------------------------------------------------
paste0("First Gov (date):",  min(ministerial_replacement$correccion_fechadesalida))
paste0("Last Gov (date):",  max(ministerial_replacement$correccion_fechadesalida))

paste0("Median of days in office for ministers (general): ", median(ministerial_replacement$diferencia_dias, na.rm = T))
#"Median of days in office for ministers (general): 372"
# 1 año y 12 días
  
paste0("Median of days in office for ministers (Democracy): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$tipo_regimen == "Democracia"], na.rm = T))
#"Median of days in office for ministers (Democracy): 484"
# 1 año y 3 meses y medio / 119 dias

paste0("Median of days in office for ministers (Autoritarismo): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$tipo_regimen == "Autoritarismo"], na.rm = T))
#"Median of days in office for ministers (Democracy): 263"
# 9 meses

#   Cabinets ------------------------------------------------------------------------------------------

paste0("Median of days in office for ministers (Social): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$clasificacion_area_amplio == "Área Social"], na.rm = T))
#[1] "Median of days in office for ministers (Social): 272"

paste0("Median of days in office for ministers (Economy): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$clasificacion_area_amplio == "Área Económica"], na.rm = T))
#"Median of days in office for ministers (Economy): 276"

paste0("Median of days in office for ministers (Interior-Exteriores-Gabinete): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$clasificacion_area_amplio == "Área Interior-Exteriores-Gabinete"], na.rm = T))
#[1] "Median of days in office for ministers (Interior-Exteriores-Gabinete): 349"

paste0("Median of days in office for ministers (Justicia-Otros): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$clasificacion_area_amplio == "Área Justicia-Otros"], na.rm = T))
#[1] "Median of days in office for ministers (Justicia-Otros): 360"


paste0("Median of days in office for ministers (Cultura-Ciencia-Educación): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$clasificacion_area_amplio == "Área Cultura-Ciencia-Educación"], na.rm = T))
#[1] "Median of days in office for ministers (Cultura-Ciencia-Educación): 441"
p
aste0("Median of days in office for ministers (Defensa): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$clasificacion_area_amplio == "Área Defensa"], na.rm = T))
#[1] "Median of days in office for ministers (Defensa): 452.5"

paste0("Median of days in office for ministers (Obrás Públicas-Transporte): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$clasificacion_area_amplio == "Área Obrás Públicas-Transporte"], na.rm = T))
#[1] "Median of days in office for ministers (Obrás Públicas-Transporte): 469.5"

paste0("Median of days in office for ministers (Salud-Seguridad): ", median(ministerial_replacement$diferencia_dias[ministerial_replacement$clasificacion_area_amplio == "Área Salud-Seguridad"], na.rm = T))
#[1] "Median of days in office for ministers (Salud-Seguridad): 569"



# Plots in Descriptive Analysis -------------------------------------------


# Figure 1 ----------------------------------------------------------------

p1 <- ministerial_replacement %>%
  ggplot() +
  geom_density(aes(x1_administracion)) +
  labs(
    x = "previous administration (X1)") +
  theme_bw()

p2 <- ministerial_replacement %>%
  ggplot() +
  geom_density(aes(x1_regimen)) +
  labs(x = "previous regime (X1)") +
  theme_bw()

plot1 <- p1+p2 +
  plot_annotation(title = 'Plot n°1.a.:  Amount of days from the previous administration/regime to resignation')
plot1
ggsave(filename = "./Figures/Exploration/Density_Histogram/density_X1_next_administration_next_regime.png",
       dpi=700,  width=15, height=6)

# Figure 2 ----------------------------------------------------------------

p1 <- ministerial_replacement %>%
  ggplot() +
  geom_density(aes(x2_administracion)) +
  labs(
    x = "to the next administration (X2)") +
  theme_bw()

p2 <- ministerial_replacement %>%
  ggplot() +
  geom_density(aes(x2_regimen)) +
  labs(x = "the next regime (X2)") +
  theme_bw()

plot1 <- p1+p2 +
  plot_annotation(title = 'Plot n°2.a.: Amount of days from replacement to ...')
plot1
ggsave(filename = "./Figures/Exploration/Density_Histogram/density_X2_next_administration_next_regime.png",
       dpi=700,  width=15, height=6)

# Figure 3 AND 4 ----------------------------------------------------------------


p1 <- ministerial_replacement %>%
  ggplot() +
  geom_density(aes(x1_administracion)) +
  labs(
    x = "previous administration (X1)") +
  theme_bw()+ 
  facet_wrap(~Department)

p2 <- ministerial_replacement %>%
  ggplot() +
  geom_density(aes(x1_regimen)) +
  labs(x = "previous regime (X1)") +
  theme_bw()+ 
  facet_wrap(~Department)

plot3 <- p1+p2 
plot3
ggsave(filename = "./Figures/Exploration/Density_Histogram/density_X1_next_administration_next_regime_facet_department.png",
       dpi=700,  width=15, height=6)
  
p1 <- ministerial_replacement %>%
  ggplot() +
  geom_density(aes(x2_administracion)) +
  labs(
    x = "to the next administration (X2)") +
  theme_bw() + 
  facet_wrap(~Department)

p2 <- ministerial_replacement %>%
  ggplot() +
  geom_density(aes(x2_regimen)) +
  labs(x = "the next regime (X2)") +
  theme_bw() +
  facet_wrap(~Department)


plot4 <- p1+p2 #+
  #plot_annotation(title = 'Plot n°3.a.: Amount of days from replacement to ...')
plot4
ggsave(filename = "./Figures/Exploration/Density_Histogram/density_X2_next_administration_next_regime_FACET_DEPARTMENT.png",
       dpi=700,  width=15, height=6)
# Measure hypotesis -------------------------------------------------------

# Model 1 -----------------------------------------------------------------

# Create the survival object
ministerial_replacement$correccion_fechadeentrada <- as.Date(ministerial_replacement$correccion_fechadeentrada)
ministerial_replacement$correccion_fechadesalida <- as.Date(ministerial_replacement$correccion_fechadesalida)

# Calcular la duración en días
ministerial_replacement$duracion_dias <- as.numeric(ministerial_replacement$correccion_fechadesalida - ministerial_replacement$correccion_fechadeentrada)

# Model 1 : Administration
surv_object <- Surv(time = ministerial_replacement$duracion_dias, 
                    event = ministerial_replacement$dummy_replacement)
cox_model_administration <- coxph(surv_object ~ x1_administracion + x2_administracion, data = ministerial_replacement)
summary(cox_model_administration)

# Model 2 : Regimen
cox_model_regimen <- coxph(surv_object ~ x1_regimen+ x2_regimen, data = ministerial_replacement)
summary(cox_model_regimen)


# Use stargazer to format output for LaTeX, including individual titles
stargazer(cox_model_administration, cox_model_regimen,
          dep.var.labels = c("Cox Proportional Hazards"),  # Custom label for the dependent variable
          dep.var.caption = "Model Description",  # Custom caption for the dependent variable description
          type = "text", covariate.labels =  c("Administration (X1)", "Administration (X2)", "Regime (X1)", "Regime (X2)"), # Output type 
          title = "Comparison of Cox Models",  # General title for the table
          model.names = TRUE,  # Ensures that model names are included
          model.numbers = FALSE,  # Optional: remove the model numbering if preferred
          add.lines = list(c("Model Title", "Administration Model", "Regime Model")),
          omit.stat = c("rsq", "adj.rsq", "ser")
)


# Plot hypothesis 1 --------------------------------------------------------


library(survival)
library(survminer)

ministerial_replacement$comienzo_administracion <- as.Date(ministerial_replacement$comienzo_administracion)
ministerial_replacement$comienzo_administracion
ministerial_replacement <- ministerial_replacement %>%
  mutate(
    days_to_next_admin = comienzo_administracion - correccion_fechadesalida,
    group = ifelse(days_to_next_admin <= 30 & days_to_next_admin >= 1, 
                   "Near Change of Admin", "Not Near Change of Admin")
  )

# Assuming your data is in 'minister_data'
ministerial_replacement$surv_object <- with(ministerial_replacement, Surv(as.numeric(correccion_fechadesalida - correccion_fechadeentrada), dummy_replacement))

# Fit Kaplan-Meier survival curves
km_fit <- survfit(surv_object ~ group, data = ministerial_replacement)

# Plot the survival curves
library(ggplot2)
ggsurvplot(km_fit, data = ministerial_replacement, pval = TRUE, conf.int = TRUE,
           xlab = "Time (days)", ylab = "Survival Probability",
           legend.title = "Group",
           legend.labs = c("Near Change of Admin", "Not Near Change of Admin"))


# Robustness test ---------------------------------------------------------
# Abrir dispositivo de gráfico para guardar la imagen
#install.packages("gridBase")
library(survival)
library(grid)
library(gridBase)
library(gridExtra)

# Save the plot for Model 1: Administration
png("~/GitHub/Ministers-Resignation-Leads-Democracy-Inestability/Political Economy/Code/Figures/Robustness Test/Proportional_Hazards_Assumption_Check_Administration.png")
cox.zph_administration <- cox.zph(cox_model_administration)
plot(cox.zph_administration, xlab = "Time", ylab = "Scaled Schoenfeld Residuals",
 #    main = "Proportional Hazards Assumption Check for Model 1: Administration"
     )
dev.off()  # Close the device to finalize the file

# Save the plot for Model 2: Regimen
png("~/GitHub/Ministers-Resignation-Leads-Democracy-Inestability/Political Economy/Code/Figures/Robustness Test/Proportional_Hazards_Assumption_Check_Regimen.png")
cox.zph_regimen <- cox.zph(cox_model_regimen)
plot(cox.zph_regimen, xlab = "Time", ylab = "Scaled Schoenfeld Residuals",
   #  main = "Proportional Hazards Assumption Check for Model 2: Regimen"
   )
dev.off()  # Close the device to finalize the file

print(cox.zph_administration)
print(cox.zph_regimen)


# Example: Making x1_administracion time-dependent
time_ = ministerial_replacement$duracion_dias

ministerial_replacement$x1_time_interaction <- ministerial_replacement$x1_administracion * log(time_)

cox_model_timedependent <- coxph(Surv(duracion_dias, dummy_replacement_without_censoring) ~ x2_administracion + x1_time_interaction, data = ministerial_replacement)

summary(cox_model_timedependent)

stargazer(cox_model_timedependent,
          dep.var.labels = c("Cox Proportional Hazards"),  # Custom label for the dependent variable
          dep.var.caption = "Model Description",  # Custom caption for the dependent variable description
          type = "latex", covariate.labels =  c("Administration (X2)", "Administration (X1) Time interaction"), # Output type 
          title = "Comparison of Cox Models",  # General title for the table
          model.names = TRUE,  # Ensures that model names are included
          model.numbers = FALSE,  # Optional: remove the model numbering if preferred
          add.lines = list(c("", "Administration Model")),
          omit.stat = c("rsq", "adj.rsq", "ser")
)

cox.zph_administration_time_dependant <- cox.zph(cox_model_timedependent)
print(cox.zph_administration_time_dependant)

library(boot)
# Bootstrap for Model 1: Administration
boot_cox_administration <- function(data, indices) {
  d <- data[indices, ]  # resample with replacement
  fit <- coxph(surv_object ~ x1_administracion + x2_administracion, data = d)
  return(coef(fit))
}
results_administration <- boot(ministerial_replacement, boot_cox_administration, R=1000)

boot_cox_administration2 <- function(data, indices) {
  d <- data[indices, ]  # resample with replacement
  fit <- coxph(surv_object ~ x2_administracion + x1_time_interaction, data = d)
  return(coef(fit))
}
results_regimen <- boot(ministerial_replacement, boot_cox_administration2, R=1000)

# Bootstrap for Model 2: Regimen
boot_cox_regimen <- function(data, indices) {
  d <- data[indices, ]  # resample with replacement
  fit <- coxph(surv_object ~ x1_regimen + x2_regimen, data = d)
  return(coef(fit))
}
results_regimen <- boot(ministerial_replacement, boot_cox_regimen, R=1000)


# Model 2 -----------------------------------------------------------------

library(dplyr)
library(survival)

# Assuming 'minister_data' is your dataset
minister_data <- ministerial_replacement %>%
  mutate(
    # Assuming regime_change_date is the date a regime change is observed and needs to be calculated or is available
    regime_change_observed = as.Date(fin_regimen),
    start_date_administration = as.Date(comienzo_administracion),
    end_date_administration = as.Date(fin_administracion),
    end_date_minister = as.Date(correccion_fechadesalida),
    
    # Calculate time to event or censoring
    duration_to_regime_change = as.numeric(regime_change_observed - end_date_minister, units = "days"),
    
    # Event indicator: 1 if regime change observed, 0 otherwise (censored)
    event_indicator_regime = ifelse(is.na(regime_change_observed), 0, 1),
    
    start_date_minister = as.Date(correccion_fechadeentrada),
    next_admin_change_date = as.Date(fin_administracion), 
    
    # Calculate duration to administration change or censoring
    duration_to_admin_change = ifelse(!is.na(next_admin_change_date),
                                      as.numeric(next_admin_change_date - start_date_minister, units = "days"),
                                      as.numeric(end_date_minister - start_date_minister, units = "days")),
    
    # Event indicator: 1 if administration change is observed, 0 if censored
    event_indicator_administration = ifelse(!is.na(next_admin_change_date) & next_admin_change_date <= end_date_minister, 1, 0),
    
    # Minister of Economy or Defense
    is_econ_def = ifelse(Department %in% c("Economic"), "Economic-Social", "Others")
  )

# Convert dates and times appropriately
minister_data$surv_object_regime <- with(minister_data, Surv(time = duration_to_regime_change , event = event_indicator_regime))
minister_data$surv_object_administration <- with(minister_data, Surv(time = duration_to_admin_change , event = event_indicator_administration))

minister_data$Department <- factor(minister_data$Department, 
                                                  levels = c( "Justice-Others", 
                                                            "Culture-Science-Education",
                                                             "Infraestructure-Transportation",  
                                                            "Defense", "Health-Security","Interior-Exterior-Cabinet", "Social", "Economic"))
# Fitting the Cox model with the corrected survival object
#baseline_model_regimen <- coxph(surv_object_regime ~  Department +, data = minister_data)
#baseline_model_administration <- coxph(surv_object_administration ~ Department, data = minister_data)

#cox_model_regimen <- coxph(surv_object_regime ~ Department #+ e_gdp.x + e_gdppc.x + e_miinflat.x
#                           , data = minister_data)
#cox_model_administration <- coxph(surv_object_administration ~  Department  #+ e_gdp.x + e_gdppc.x + e_miinflat.x
#                                  , data = minister_data)

#stargazer(baseline_model_administration, cox_model_administration, 
#          dep.var.labels = c("Cox Proportional Hazards"),  # Custom label for the dependent variable
#          dep.var.caption = "Model Description",  # Custom caption for the dependent variable description
#          type = "text",#covariate.labels =  c("Administration (X1)", "Administration (X2)",
                        #                       "Portfolio: Culture, Science and Education", 
                        #                       "Portfolio: Infraestructure Transportation",
                        #                       "Portfolio: Defense",
#                        #                       "Portfolio: Health and Security",
                        #                       "Portfolio: Interior, Exterior and Cabinet",
                        #                       "Portfolio: Social",
                        #                       "Portfolio: Economy",
                        #                       "GDP",
                        #                       "GDP (per capita)",
                        #                       "Inflation Rate"), # Output type 
#          title = "Comparison of Cox Models",  # General title for the table
#          model.names = TRUE,  # Ensures that model names are included
#          model.numbers = FALSE,  # Optional: remove the model numbering if preferred
#          add.lines = list(c( "Administration Baseline", "Administration Model")),
#          omit.stat = c("rsq", "adj.rsq", "ser")
#)


# Robustness check --------------------------------------------------------
# Adding economic covariates
#extended_model_regimen <- coxph(surv_object_regime ~ x1_regimen + x2_regimen + Department_security_health_separated + e_gdp.x + e_gdppc.x + e_miinflat.x, data = minister_data)
#summary(extended_model_regimen)

# Checking proportional hazards assumption
#ph_test <- cox.zph(extended_model_regimen)
#print(ph_test)

# Bootstrap resampling for robust confidence intervals
#library(boot)
#boot_cox <- function(data, indices) {
#  d <- data[indices,] # resample with replacement
#  fit <- coxph(surv_object_regime ~ x1_regimen + x2_regimen + Department_security_health_separated, data = d)
#  return(coef(fit))#
#}

#results <- boot(minister_data, boot_cox, R = 1000)
#boot.ci(results, type = "bca")


# Check Department of health and security ---------------------------------

minister_data$surv_object_administration <- with(minister_data, Surv(time = duration_to_admin_change , event = event_indicator_administration))
minister_data$surv_object_regime <- with(minister_data, Surv(time = duration_to_regime_change , event = event_indicator_regime))

model_regimen_department_health_security  <- coxph(surv_object_regime ~ x1_regimen + x2_regimen + Department_security_health_separated, data = minister_data)
model_administration_department_health_security <- coxph(surv_object_administration ~ x1_administracion + x2_administracion + Department_security_health_separated, data = minister_data)

stargazer(model_administration_department_health_security, model_regimen_department_health_security, 
          dep.var.labels = c("Cox Proportional Hazards"),  # Custom label for the dependent variable
          dep.var.caption = "Model Description",  # Custom caption for the dependent variable description
          type = "text",
          covariate.labels =  c("Administration (X1)", "Administration (X2)",
                                "Regime (X1)", "Regime (X2)",
                                                "Portfolio: Defense",
                                                "Portfolio: Economy",
                                                "Portfolio: Health",
                                             "Portfolio: Infraestructure and Transportation",
                                                 "Portfolio: Interior, Exterior and Cabinet",
                                                "Portfolio: Justice and Others", 
         
                                               "Portfolio: Security", 
                                               "Portfolio: Social"), # Output type 
          title = "Comparison of Cox Models",  # General title for the table
          model.names = TRUE,  # Ensures that model names are included
          model.numbers = FALSE,  # Optional: remove the model numbering if preferred
          add.lines = list(c( "Administration Baseline", "Administration Model")),
          omit.stat = c("rsq", "adj.rsq", "ser")
)



stargazer(baseline_model_regimen, cox_model_regimen, 
          dep.var.labels = c("Cox Proportional Hazards"),  # Custom label for the dependent variable
          dep.var.caption = "Model Description",  # Custom caption for the dependent variable description
          type = "text",
          title = "Comparison of Cox Models",  # General title for the table
          model.names = TRUE,  # Ensures that model names are included
          model.numbers = FALSE,  # Optional: remove the model numbering if preferred
          add.lines = list(c( "Administration Baseline", "Administration Model")),
          omit.stat = c("rsq", "adj.rsq", "ser")
)

# Model 3: Hazard rate by department --------------------------------------

cox_model <- coxph(Surv(duration_minister_in_yrs, dummy_replacement) ~ x1_administracion + x2_administracion  + tipo_regimen + Department  , 
                   data = ministerial_replacement)
summary_cox <- summary(cox_model)

stargazer(cox_model, 
          dep.var.labels = c("Cox Proportional Hazards"),  # Custom label for the dependent variable
          type = "text", covariate.labels =  c("Administration (X1)", "Administration (X2)",
                                               "Regime type", "Portfolio: Defense", 
                                               "Portfolio: Economy", 
                                               "Portfolio: Health and Security", 
                                               "Portfolio: Infraestructure and Transportation", 
                                               "Portfolio: Interior, Exterior and Cabinet", "Portfolio: Justice and Others",
                                               "Portfolio: Social"), # Output type 
          title = "Comparison of Cox Models",  # General title for the table
          model.names = TRUE,  # Ensures that model names are included
          model.numbers = FALSE,  # Optional: remove the model numbering if preferred
          add.lines = list(c( "Administration Baseline", "Administration Model")),
          omit.stat = c("rsq", "adj.rsq", "ser")
)


# Accumulative resignations -----------------------------------------------

minister_data <- minister_data %>%
  mutate(
    fin_administracion = as.Date(fin_administracion, format = "%Y-%m-%d"),
    correccion_fechadesalida = as.Date(correccion_fechadesalida, format = "%Y-%m-%d")
  ) %>%
  arrange(fin_administracion) %>%
  group_by(presidente) %>%
  mutate(acum_renuncias_30d = sum(correccion_fechadesalida >= (fin_administracion - 30) & correccion_fechadesalida < fin_administracion))

# Creando el objeto de supervivencia
surv_object <- with(minister_data, Surv(time = duration_to_admin_change, event = event_indicator_administration))

# Ajustando el modelo de Cox
cox_model <- coxph(surv_object ~  x1_administracion + x2_administracion+ Department + acum_renuncias_30d, data = minister_data)
summary(cox_model)


# Contar acumulativo por departmanto --------------------------------------
library(tidyr)


renuncias_departamento <- minister_data %>%
  filter(correccion_fechadesalida >= (fin_administracion - 30) & correccion_fechadesalida < fin_administracion) %>%
  count(fin_administracion, Department, name = "renuncias")

# Transformar a formato ancho
renuncias_wide <- pivot_wider(renuncias_departamento,
                              names_from = Department, 
                              values_from = renuncias, values_fill = 0)

minister_data_new <- minister_data %>%
  left_join(renuncias_wide, by = c("presidente", "fin_administracion")) %>%
  mutate(
    `Infraestructure-Transportation` = ifelse(is.na(`Infraestructure-Transportation`), 0, `Infraestructure-Transportation`),
    `Defense` = ifelse(is.na(`Defense`), 0, `Defense`),
    `Interior-Exterior-Cabinet` = ifelse(is.na(`Interior-Exterior-Cabinet`), 0, `Interior-Exterior-Cabinet`),
    `Social` = ifelse(is.na(`Social`), 0, `Social`),
    `Economic` = ifelse(is.na(`Economic`), 0, `Economic`),
    `Justice-Others` = ifelse(is.na(`Justice-Others`), 0, `Justice-Others`),
    `Culture-Science-Education` = ifelse(is.na(`Culture-Science-Education`), 0, `Culture-Science-Education`),
    `Health-Security` = ifelse(is.na(`Health-Security`), 0, `Health-Security`)
  )
cox_model_administration <- coxph(Surv(time = duration_to_admin_change, event = event_indicator_administration) ~ 
                       `Infraestructure-Transportation` + Defense + `Interior-Exterior-Cabinet` +
                       Social + Economic + `Justice-Others` + `Culture-Science-Education` + 
                       `Health-Security` + acum_renuncias_30d, 
                     data = minister_data_new)

cox_model_regimen <- coxph(Surv(time = duration_to_regime_change, event = event_indicator_regime) ~ 
                     `Infraestructure-Transportation` + Defense + `Interior-Exterior-Cabinet` +
                     Social + Economic + `Justice-Others` + `Culture-Science-Education` + 
                     `Health-Security` + acum_renuncias_30d, 
                   data = minister_data_new)
summary(cox_model)

stargazer(cox_model_administration, cox_model_regimen, 
          dep.var.labels = c("Cox Proportional Hazards"),  # Custom label for the dependent variable
          type = "text", covariate.labels =  c(  "Portfolio: Infraestructure and Transportation", 
                                               "Portfolio: Defense", 
                               "Portfolio: Interior, Exterior and Cabinet", 
                                 "Portfolio: Social",
                                               "Portfolio: Economy", 
                                               "Portfolio: Justice and Others", 
                                               "Portfolio: Culture, Science and Education", 
                                               "Portfolio: Health and Security",
                                               "# Replacements in last 30 days"), # Output type 
          title = "Comparison of Cox Models",  # General title for the table
          model.names = TRUE,  # Ensures that model names are included
          model.numbers = FALSE,  # Optional: remove the model numbering if preferred
          add.lines = list(c( "Administration Model", "Regimen Model")),
          omit.stat = c("rsq", "adj.rsq", "ser")
)


library(survival)
library(grid)
library(gridBase)
library(gridExtra)

# Save the plot for Model 1: Administration
png("~/GitHub/Ministers-Resignation-Leads-Democracy-Inestability/Political Economy/Code/Figures/Robustness Test/Proportional_Hazards_Assumption_Check_Administration_H2.png")
cox.zph_administration <- cox.zph(cox_model_administration)
plot(cox.zph_administration, xlab = "Time", ylab = "Scaled Schoenfeld Residuals",
     #    main = "Proportional Hazards Assumption Check for Model 1: Administration"
)
dev.off()  # Close the device to finalize the file

# Save the plot for Model 2: Regimen
png("~/GitHub/Ministers-Resignation-Leads-Democracy-Inestability/Political Economy/Code/Figures/Robustness Test/Proportional_Hazards_Assumption_Check_Regimen_H2.png")
cox.zph_regimen <- cox.zph(cox_model_regimen)
plot(cox.zph_regimen, xlab = "Time", ylab = "Scaled Schoenfeld Residuals",
     #  main = "Proportional Hazards Assumption Check for Model 2: Regimen"
)
dev.off()  # Close the device to finalize the file

print(cox.zph_administration)
print(cox.zph_regimen)



# Checking proportional hazards assumption
ph_test_administration <- cox.zph(cox_model_administration)
print(ph_test_administration)

ph_test_regimen <- cox.zph(cox_model_regimen)
print(ph_test_regimen)

# Bootstrap resampling for robust confidence intervals
library(boot)
boot_cox_regime <- function(data, indices) {
  d <- data[indices,] # resample with replacement
  fit <- coxph(Surv(time = duration_to_regime_change, event = event_indicator_regime) ~ 
                 `Infraestructure-Transportation` + Defense + `Interior-Exterior-Cabinet` +
                 Social + Economic + `Justice-Others` + `Culture-Science-Education` + 
                 `Health-Security` + acum_renuncias_30d, data = minister_data_new)
  return(coef(fit))
}

results_regime<- boot(minister_data, boot_cox_regime, R = 1000)
boot.ci(results_regime, type = "bca")

boot_cox_administration<- function(data, indices) {
  d <- data[indices,] # resample with replacement
  fit <- coxph(Surv(time = duration_to_administration_change, event = event_indicator_administration) ~ 
                 `Infraestructure-Transportation` + Defense + `Interior-Exterior-Cabinet` +
                 Social + Economic + `Justice-Others` + `Culture-Science-Education` + 
                 `Health-Security` + acum_renuncias_30d, data = minister_data_new)
  return(coef(fit))
}

results_regime<- boot(minister_data, boot_cox_administration, R = 1000)
boot.ci(results_regime, type = "bca")



# Plot model 2 ------------------------------------------------------------

# Install and load necessary package
library(dplyr)
library(survival)
library(ggplot2)
library(survminer)

# Assuming `minister_data_new` is your original dataset
# Using dplyr to calculate medians for appropriate columns
minister_data_new <- as.data.frame(minister_data_new)
median_values <- minister_data_new %>%
  select(where(is.numeric)) %>%  # Select only numeric columns
  summarise(across(everything(), median, na.rm = TRUE))

# Print to see the median values calculated
print(median_values)

median_values %>% select(duration_minister_in_yrs ,duration_to_admin_change )
# Creating a data frame with 'acum_renuncias_30d' from 1 to 10
scenarios_df <- data.frame(acum_renuncias_30d = 1:10)
scenarios_df <- scenarios_df %>%
  mutate(across(setdiff(names(median_values), "acum_renuncias_30d"), 
                ~ median_values[[cur_column()]], .names = "{.col}"))

# Check the scenarios data frame
print(scenarios_df)

# Robustness check --------------------------------------------------------

# Check for each model
cox.zph_regimen <- cox.zph(cox_model_regimen)
plot(cox.zph_regimen)
cox.zph_administration <- cox.zph(cox_model_administration)
plot(cox.zph_administration)

# Global test for Schoenfeld residuals
print(summary(cox.zph_regimen))
print(summary(cox.zph_administration))

# Interaction affects the model?
cox_model_regimen_interaction <- coxph(surv_object_regime ~ (x1_administracion + x2_administracion + Department)^2, data = minister_data)
summary(cox_model_regimen_interaction)
