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
library(catppuccin)

dark_theme <- theme_minimal() +
  theme(
    text = element_text(color = "#a6adc8"),
    plot.background = element_rect(fill = "#1e1e2e"),
    plot.title = element_text(color = "#cdd6f4", face = "bold"),
    axis.text = element_text(color = "#a6adc8"),
    axis.title = element_text(color = "#cdd6f4", face = "bold"),
    legend.text = element_text(color = "#cdd6f4"),
    legend.title = element_text(color = "#cdd6f4", face = "bold"),
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
