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

total_revenues <- read.csv(paste0(rawDir, "receitas.csv"))
total_revenues <- total_revenues %>% mutate(municipality_id = as.character(municipality_id))

local_taxes <- read.csv(paste0(rawDir, "local_taxes.csv"))
local_taxes <- local_taxes %>% mutate(municipality_id = as.character(municipality_id))

electoral_data <- read.csv(paste0(rawDir, "electoral_data.csv"))
electoral_data <- electoral_data %>%
  rename(
    "municipality_id" = "id_municipio",
    "previous_election_year" = "ano"
  )
electoral_data <- electoral_data %>% mutate(municipality_id = as.character(municipality_id))
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
populationDf <- populationDf %>% mutate(across(everything(), as.character)) %>% slice(1:5570) %>% 
                                pivot_longer(!Município, names_to = "year", values_to = "population")
populationDf <- populationDf %>% mutate(year=str_replace(year, "X",""))
populationDf <- populationDf %>% extract(col = Município,
                                        into = c("municipality_id_6", "municipality"),
                                        regex = "^(\\d+)\\s+(.*)$")
populationDf <- populationDf %>% mutate(population=str_replace(population, "-","0"))
# We will now calculate the theoretical FPM transfers using populationDf and the transferDf
# To do so, we will need to calculate the FPM coefficients from 1997, due to compplementary law 91, which establishes a mechanism to avoid abrupt reductions in FPM transfers for municipalities for a few years.
# The FPM mechanism uses fixed coefficients, that are a step function of the population
# It also uses a fixed state share on the transfers
  assign_coeff <- function(pop_col){
    coeff = case_when(
      {{pop_col}} > 1 & {{pop_col}} <= 10188 ~ 0.6,
      {{pop_col}} >= 10189 & {{pop_col}} <= 13584 ~ 0.8,
      {{pop_col}} >= 13585 & {{pop_col}} <= 16980 ~ 1,
      {{pop_col}} >= 16981 & {{pop_col}} <= 23772 ~ 1.2,
      {{pop_col}} >= 23773 & {{pop_col}} <= 30564 ~ 1.4,
      {{pop_col}} >= 30565 & {{pop_col}} <= 37356 ~ 1.6,
      {{pop_col}} >= 37357 & {{pop_col}} <= 44148 ~ 1.8,
      {{pop_col}} >= 44149 & {{pop_col}} <= 50940 ~ 2,
      {{pop_col}} >= 50941 & {{pop_col}} <= 61128 ~ 2.2,
      {{pop_col}} >= 61129 & {{pop_col}} <= 71316 ~ 2.4,
      {{pop_col}} >= 71317 & {{pop_col}} <= 81504 ~ 2.6,
      {{pop_col}} >= 81505 & {{pop_col}} <= 91692 ~ 2.8,
      {{pop_col}} >= 91693 & {{pop_col}} <= 101880 ~ 3,
      {{pop_col}} >= 101881 & {{pop_col}} <= 115464 ~ 3.2,
      {{pop_col}} >= 115465 & {{pop_col}} <= 129048 ~ 3.4,
      {{pop_col}} >= 129049 & {{pop_col}} <= 142632 ~ 3.6,
      {{pop_col}} >= 142633 & {{pop_col}} <= 156216 ~ 3.8,
      {{pop_col}} > 156216 & {{pop_col}} <= 99999999 ~ 4,
      TRUE ~ 0
    )
    return(coeff)
  }

state_coeff <- tibble(
  state = c("AC", "AL", "AP", "AM", "BA", "DF", "CE", "ES", "GO", "MA", 
            "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
            "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
  coeffState = c(0.263, 2.088, 0.139, 1.245, 9.270, 0.000, 4.586, 1.760, 3.732,
              3.972, 1.895, 1.500, 14.185, 3.295, 3.194, 7.286, 4.795, 2.402,
              2.738, 2.432, 7.301, 0.746, 0.085, 4.200, 14.262, 1.334, 1.296)
)

population1996 <- read.csv(paste0(rawDir,"population1996.csv"))
population1996 <- population1996 %>% filter(!is.na(populacao))
population1996 <- population1996 %>% mutate(coeff1997 = assign_coeff(populacao))
population1996 <- population1996 %>% rename(municipality_id=id_municipio)
population1996 <- population1996 %>% mutate(municipality_id=as.character(municipality_id))
transferDf <- transferDf %>% left_join(population1996 %>% select(municipality_id, coeff1997), by="municipality_id")

populationDf <- populationDf %>% mutate(population = as.integer(population))
populationDf <- populationDf %>% mutate(coeff = assign_coeff(population))
populationDf <- populationDf %>% select(-c("municipality"))
# We use the population from the year prior to the transfer to calculate the coefficientes
populationDf <- populationDf %>% mutate(year = as.character(as.integer(year)+1))

transferDf <- transferDf %>% group_by(year) %>% mutate(totalFPMyear = sum(transfer_value)) %>% ungroup()
transferDf <- transferDf %>% left_join(state_coeff, by="state")
transferDf <- transferDf %>% mutate(municipality_id_6 = str_sub(municipality_id,1,6))

transferDf <- transferDf %>% left_join(populationDf, by=c("year", "municipality_id_6"))
transferDf <- transferDf %>% mutate(year=as.integer(year))
# Creating the softening mechanism
capital_codes <- c(
  "1100205", "1302603", "1200401", "5002704", "1600303", "5300108",
  "1400100", "5103403", "1721000", "3550308", "2211001", "3304557",
  "1501402", "5208707", "2927408", "4205407", "2111300", "2704302",
  "4314902", "4106902", "3106200", "2304400", "2611606", "2507507",
  "2800308", "2408102", "3205309"
)
transferDf <- transferDf %>% mutate(redutor = case_when(
            year < 1999 ~ 0,
            year == 1999 ~ 0.2,
            year == 2000 ~ 0.4,
            year == 2001 ~ 0.3,
            year == 2002 ~ 0.4,
            year == 2003 ~ 0.5,
            year == 2004 ~ 0.6,
            year == 2005 ~ 0.7,
            year == 2006 ~ 0.8,
            year == 2007 ~ 0.9,
            year > 2007 ~ 1,
            TRUE ~ NA_real_)
            )
transferDf <- transferDf %>% mutate(coeff1997 = replace_na(coeff1997,0))
transferDf <- transferDf %>% mutate(diff = (coeff1997 * (1 - redutor)) + (coeff * redutor),
                                    diff = if_else(coeff1997 < coeff, coeff, diff),
                                    coeff_e = diff + coeff)
transferDf <- transferDf %>% filter(!municipality_id %in% capital_codes)
transferDf <- transferDf %>% group_by(year, state) %>% mutate(stateCoeffSum = sum(coeff_e)) %>% ungroup()
transferDf <- transferDf %>% mutate(fpmLaw = (totalFPMyear*(coeffState/100)*0.864*coeff_e)/stateCoeffSum)

coeff_intervals <- data.frame(
  coeff = c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4),
  lower_cutoff = c(1, 10188, 13584, 16980, 23772, 30564, 37356, 44148, 50940, 61128, 71316, 81504, 91692, 101880, 115464, 129048, 142632, 156216),
  upper_cutoff = c(10188, 13584, 16980, 23772, 30564, 37356, 44148, 50940, 61128, 71316, 81504, 91692, 101880, 115464, 129048, 142632, 156216, 99999999)
)

transferDf <- transferDf %>% left_join(coeff_intervals, by = "coeff")
transferDf <- transferDf %>% mutate(
  lower_diff = abs(population - lower_cutoff),
  upper_diff = abs(population - upper_cutoff),
  closest_cutoff = case_when(
    coeff == 0 ~ NA_real_,
    lower_diff < upper_diff ~ lower_cutoff,
    upper_diff < lower_diff ~ upper_cutoff,
    lower_diff == upper_diff ~ pmin(lower_cutoff, upper_cutoff)
  )) %>% select(-lower_diff, -upper_diff)
transferDf <- transferDf %>% mutate(margin = population-closest_cutoff)
microrregion <- read.csv(paste0(rawDir,"microrregion.csv"))
microrregion <- microrregion %>% mutate(municipality_id = as.character(municipality_id))
transferDf <- transferDf %>% left_join(microrregion, by = "municipality_id")
transferDf <- transferDf %>%
  mutate(cutoffId = case_when(
    closest_cutoff == 10188   ~ 1,
    closest_cutoff == 13584   ~ 2,
    closest_cutoff == 16980   ~ 3,
    closest_cutoff == 23772   ~ 4,
    closest_cutoff == 30564   ~ 5,
    closest_cutoff == 37356   ~ 6,
    closest_cutoff == 44148   ~ 7,
    closest_cutoff == 50940   ~ 8,
    closest_cutoff == 61128   ~ 9,
    closest_cutoff == 71316   ~ 10,
    closest_cutoff == 81504   ~ 11,
    closest_cutoff == 91692   ~ 12,
    closest_cutoff == 101880  ~ 13,
    closest_cutoff == 115465  ~ 14,
    closest_cutoff == 129048  ~ 15,
    closest_cutoff == 142632  ~ 16,
    closest_cutoff == 156216  ~ 17,
    TRUE ~ 0
  ))
raisDb$municipality_id <- as.character(raisDb$municipality_id)
transferDf <- transferDf %>% left_join(raisDb, by=c("year", "municipality_id"))
# Adding local finances information
transferDf <- transferDf %>% left_join(local_taxes, by=c("year", "municipality_id"))
transferDf <- transferDf %>% left_join(total_revenues, by=c("year", "municipality_id"))
# Deflating variables
deflator <- data.frame(
  "year" = c(
    1999,
    2000,
    2001,
    2002,
    2003,
    2004,
    2005,
    2006,
    2007,
    2008,
    2009,
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017,
    2018,
    2019,
    2020,
    2021,
    2022,
    2023
  ),
  "Deflator" = c(
    1,
    1/1.09615360,
    1/1.16106430,
    1/1.24953430,
    1/1.43030490,
    1/1.54054160,
    1/1.65467150,
    1/1.74899180,
    1/1.80125110,
    1/1.88341210,
    1/1.99338670,
    1/2.08492950,
    1/2.20988280,
    1/2.34729140,
    1/2.49175120,
    1/2.63092280,
    1/2.81871320,
    1/3.12049290,
    1/3.28756260,
    1/3.38142410,
    1/3.50912410,
    1/3.65621500,
    1/3.82290410,
    1/4.21970000,
    1/4.46335900
  )
)
transferDf <- transferDf %>% left_join(deflator, by = "year")
transferDf <- transferDf %>%
  mutate(
    transfer_value =  transfer_value*Deflator,
    fpmLaw = fpmLaw*Deflator,
    total_revenue = total_revenue*Deflator,
    local_taxes = local_taxes*Deflator,
    total_wage_temporaries = total_wage_temporaries*Deflator,
    total_wage_others = total_wage_others*Deflator
  )
# Electoral Data
election_years <- c(1996, 2000, 2004, 2008, 2012, 2016, 2020)
indices <- findInterval(transferDf$year, election_years, left.open = TRUE)
transferDf$previous_election_year <- election_years[indices]
transferDf <- transferDf %>% left_join(electoral_data, by=c("previous_election_year", "municipality_id"))
saveRDS(transferDf, "fullSample.rds")