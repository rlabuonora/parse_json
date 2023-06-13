library(tidyverse)
library(jsonlite)
library(purrr)
library(dplyr)
library(jsonlite)
library(tidyr)
library(WriteXLS)
library(glue)
library(fs)
library(openxlsx)




procesar_mes <- function(archivo) {
  
  df <- fromJSON(archivo)$releases %>% 
    select(-parties, -tag) %>% 
    unnest(awards, names_repair="universal") %>% 
    unnest(documents, names_repair="universal") %>% 
    unnest(items, names_repair="universal") %>% 
    unnest(suppliers, names_repair="universal")
  
  df  %>% 
    select(-tender) %>% 
    mutate(buyer_id = buyer$id, buyer_name = buyer$name) %>% 
    select(-buyer) %>% 
    mutate(classification_id = classification$id,
           classification_desc=classification$description) %>% 
    select(-classification) %>% 
    mutate(unit_id=unit$id, unit_name=unit$name, unit_value=unit$value) %>%
    select(-unit) %>% 
    mutate(unit_value_amount=unit_value$amount, unit_value_currency=unit_value$currency) %>%
    select(-unit_value) %>% 
    select(-value) %>% 
    select(url, datePublished, quantity, 
           name, buyer_id, buyer_name, 
           classification_id, classification_desc,
           starts_with("unit"))

  
}

procesar_dir <- function(year) {
  
  dir <- glue::glue('./json/{year}')
  archivos <- list.files(dir, pattern="^a.*\\.json", full.names = TRUE)
  
  res <- archivos %>% 
    as.list() %>% 
    purrr::map(~procesar_mes(.x)) %>% 
    bind_rows()
  
  res
}



exportar_excel <- function(df, dest) {
  
  
  wb <- createWorkbook()
  
  # Add a worksheet to the workbook
  addWorksheet(wb, "Datos")
  
  # Write the data frame to the worksheet
  writeData(wb, "Datos", df)
  
  
  s <- createStyle(numFmt = "0.00")
  addStyle(wb, 1, style = s, rows = 2:6, cols = 25, gridExpand = TRUE)
  
  
  # Save the workbook to an Excel file
  saveWorkbook(wb, dest, overwrite = TRUE)
}

compras_df <- as.list(2020:2022) %>% 
  purrr::map(~procesar_dir(.x)) %>% 
  bind_rows()


exportar_excel(compras_df, "./xlsx/2020-2022.xlsx")
