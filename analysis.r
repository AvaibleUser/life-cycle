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
        vars[[current_name]] <- c(vars[[current_name]], current_df)
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
    vars[[current_name]] <- c(vars[[current_name]], current_df)
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
