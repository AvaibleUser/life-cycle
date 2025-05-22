# --------------------------
# Librerías necesarias
# --------------------------
library(tidyverse)
library(ggplot2)
library(plotly)
library(leaflet)
library(waffle)
library(ggridges)
library(treemap)
library(viridis)
library(survival)
library(survminer)
library(ggwordcloud)
library(catppuccin)

dark_theme <- theme_minimal() +
  theme(
    text = element_text(color = "#a6adc8"),
    plot.background = element_rect(fill = "#1e1e2e"),
    plot.title = element_text(color = "#cdd6f4", face = "bold"),
    axis.text = element_text(color = "#a6adc8"),
    axis.title = element_text(color = "#cdd6f4", face = "bold"),
    strip.text = element_text(color = "#cdd6f4"),
    legend.text = element_text(color = "#cdd6f4"),
    legend.title = element_text(color = "#cdd6f4", face = "bold"),
    legend.background = element_rect(fill = "#1e1e2e"),
    legend.box.background = element_rect(fill = "#1e1e2e"),
    panel.grid = element_line(color = "#7f849c"),
  )

# --------------------------
# Funciones auxiliares
# --------------------------

# Reads a csv file with the structure of the variables
# and returns a list of dataframes with the variables
read_vars <- function(file) {
  data <- readr::read_csv(
    file,
    skip = 2,
    col_names = FALSE,
    col_types = "ccc"
  )
  vars <- list()
  current_df <- NULL
  current_name <- NULL

  for (i in seq_len(nrow(data))) {
    if (!is.na(data[i, 1]) && data[i, 1] != "") {
      if (!is.null(current_df)) {
        names(current_df) <- c("code", "label")
        vars[[current_name]] <- current_df
      }
      current_name <- as.character(data[i, 1])
      current_df <- data.frame()
    }

    current_df <- rbind(
      current_df,
      data.frame(data[i, 2], data[i, 3])
    )
  }
  if (!is.null(current_df)) {
    names(current_df) <- c("code", "label")
    vars[[current_name]] <- current_df
  }

  vars
}

read_data <- function(files) {
  loaded_data <- purrr::map_dfr(
    files,
    ~ readr::read_csv(
      .x,
      col_types = readr::cols(.default = "c")
    )
  )
  data <- dplyr::bind_rows(loaded_data)
  data$"_id" <- NULL
  data
}

read_complete_data <- function(target) {
  files <- list.files(
    pattern = paste("bd", target, "202[1-3]\\.csv", sep = ""),
    recursive = TRUE,
    full.names = TRUE
  )
  data <- read_data(files)
  vars <- read_vars(paste("data/variables-", target, ".csv", sep = ""))
  names(vars) <- colnames(data)
  list(data = data, vars = vars)
}

# --------------------------
# Carga y limpieza de datos
# --------------------------

# Nacimientos
birth <- read_complete_data("nacimientos")

# Defunciones
death <- read_complete_data("defunciones")

# --------------------------
# GRÁFICA 1:
# Evolución mensual de nacimientos y defunciones (Área apilada)
# --------------------------

evolution <- bind_rows(
  birth$data %>%
    count(year = `Añoocu`, month = `Mesocu`) %>%
    mutate(type = "birth"),
  death$data %>%
    count(year = `Añoocu`, month = `Mesocu`) %>%
    mutate(type = "death")
) %>%
  mutate(year = as.numeric(year), month = as.numeric(month)) %>%
  arrange(year, month)

ggplot(evolution, aes(x = month, y = n, fill = type)) +
  geom_area(position = "identity") +
  facet_wrap(~year, ncol = 1) +
  scale_fill_catppuccin(palette = "mocha", alpha = 0.8) +
  labs(
    title = "Evolución Mensual Comparada de Nacimientos y Defunciones",
    x = "Mes", y = "Cantidad", fill = "Tipo"
  ) +
  dark_theme

# --------------------------
# GRÁFICA 2:
# Relación entre edad materna y peso neonatal (Densidad 2D)
# --------------------------

to_kg <- function(lbs, ozs) {
  as.numeric(lbs) * 0.453592 + as.numeric(ozs) * 0.0283495
}

births_kg <- birth$data %>%
  mutate(weight = to_kg(`Libras`, `Onzas`)) %>%
  mutate(mothers_age = as.numeric(`Edadm`)) %>%
  filter(`Libras` != "99", `Onzas` != "99", `Edadm` != 999) %>%
  select(weight, mothers_age)

ggplot(births_kg, aes(x = mothers_age, y = weight)) +
  geom_hex(bins = 30) +
  scale_fill_viridis(option = "C", alpha = 0.8) +
  labs(
    title = "Densidad de Edad Materna vs Peso al Nacer",
    x = "Edad de la Madre", y = "Peso (kg)", fill = "Nacimientos"
  ) +
  dark_theme

# --------------------------
# GRÁFICA 3:
# Análisis de supervivencia por grupo étnico de los padres (Kaplan-Meier)
# --------------------------

survived <- death$data %>%
  inner_join(death$vars$`Puedif`, by = c("Puedif" = "code")) %>%
  mutate(age = as.numeric(`Edadif`)) %>%
  select(pueblo = label, age) %>%
  filter(pueblo != "Ignorado", age != 999)

fit <- survfit(Surv(age) ~ pueblo, data = survived)

ggsurvplot(fit,
  data = survived,
  title = "Supervivencia por Grupo Étnico",
  xlab = "Edad",
  ylab = "Probabilidad de supervivencia",
  legend.title = "Pueblo",
  legend.labs = c("Garifuna", "Xinca", "Mestizo / Ladino", "Otro", "Xinka"),
  risk.table = TRUE,
  legend = "right",
  ggtheme = dark_theme,
  tables.col = "strata",
  risk.table.title = "Supervivencia",
)

# --------------------------
# GRÁFICA 4:
# Proporción de causas de muerte por grupo de edad (Treemap)
# --------------------------

causes <- death$data %>%
  mutate(age_group = cut(as.numeric(`Edadif`),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  )) %>%
  count(`Caudef`, age_group) %>%
  inner_join(death$vars$`Caudef`, by = c("Caudef" = "code")) %>%
  select(cause = label, age_group, n) %>%
  group_by(age_group) %>%
  top_n(5, n)

treemap(causes,
  index = c("age_group", "cause"),
  vSize = "n",
  title = "Principales Causas de Muerte por Grupo de Edad"
)

# --------------------------
# GRÁFICA 5:
# Distribución de ocupaciones parentales (Redes)
# --------------------------

ocupation <- birth$data %>%
  select(father_ocupation = `Ocupap`, mother_ocupation = `Ocupam`) %>%
  inner_join(birth$vars$`Ocupap`, by = c("father_ocupation" = "code")) %>%
  select(father_ocupation = label, mother_ocupation) %>%
  inner_join(birth$vars$`Ocupam`, by = c("mother_ocupation" = "code")) %>%
  select(father_ocupation, mother_ocupation = label) %>%
  gather(parents, ocupation) %>%
  count(parents, ocupation) %>%
  top_n(20, n)

ggplot(ocupation, aes(label = ocupation, size = n, color = parents)) +
  geom_text_wordcloud_area(shape = "square", eccentricity = 0.35) +
  scale_size_area(max_size = 50, trans = power_trans(0.55)) +
  facet_wrap(~parents, ncol = 1, labeller = as_labeller(c(
    `father_ocupation` = "Ocupación del Padre",
    `mother_ocupation` = "Ocupación de la Madre"
  ))) +
  labs(title = "Ocupaciones más Comunes de Padres vs Madres") +
  dark_theme
