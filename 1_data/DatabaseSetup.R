library(tidyverse)
library(this.path)
convert_to_numeric <- function(value) {
  clean_value <- gsub("R\\$", "", value)
  clean_value <- gsub("\\.", "", clean_value)
  clean_value <- gsub(",", ".", clean_value)
  return(as.double(clean_value))
}
setwd(this.dir())
rawDir <- "../0_raw/"
raisDb <- read.csv(paste0(rawDir,"municipal_public_workers_rais.csv"))

transferFiles <- c("transferências_para_municípios_1999_2003.csv","transferências_para_municípios_2004_2008.csv","transferências_para_municípios_2009_2013.csv","transferências_para_municípios_2014_2018.csv","transferências_para_municípios_2019_2023.csv")
transferPaths <- file.path(rawDir, transferFiles)

# Change original transfer files enconding using a temporary directory
utf8Paths <- file.path(tempdir(), basename(transferPaths))
for (i in seq_along(transferPaths)) {
  text <- read_file(transferPaths[i], locale = locale(encoding = "Windows-1252"))
  write_file(text, utf8Paths[i])  # write in UTF-8
}
transferDf <- utf8Paths %>%
  lapply(function(file) read.csv(file, encoding = "UTF-8", sep = ";", colClasses = "character")) %>%
  bind_rows()
# Converting the transfer value to a numeric column
transferDf <- transferDf %>% mutate(transfer_value = convert_to_numeric(Valor.Consolidado))
transferDf <- transferDf %>% rename(municipality_id = Código.IBGE, municipality_name = Município, year = Ano, state = UF)
transferDf <- transferDf %>% select(c(municipality_id, municipality_name, year, state, transfer_value))

# Change the original population file enconding using a temp dir
utf8path <- paste0(tempdir(), "population1998_2023.csv")
text <- read_file(paste0(rawDir, "population1998_2023.csv"), locale = locale(encoding = "Windows-1252"))
write_file(text, utf8path)
populationDf <- read.csv(utf8path, encoding = "UTF-8", sep = ";", skip = 3, header = T)