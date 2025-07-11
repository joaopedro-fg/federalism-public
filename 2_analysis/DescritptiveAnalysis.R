library(geobr)
library(sf)
library(tidyverse)
library(this.path)
library(ggpubr)
setwd(this.dir())
dataDir <- "../1_data/"
figureDir <- "../3_figures/"
brasil_municipalities <- read_municipality(showProgress = FALSE)
fullSample <- readRDS(paste0(dataDir,"fullSample.rds"))
fullSample <- fullSample %>% filter(year <= 2022)
# Figures for comissionate/temporaries analysis
sample2022 <- fullSample %>% filter(year == 2022)
sample2022 <- sample2022 %>% mutate(temporaries = replace_na(temporaries, 0))
sample2022 <- sample2022 %>% mutate(prop = temporaries/total_employees)
brasil_municipalities <- brasil_municipalities %>% mutate(code_muni = as.character(code_muni))
plotDB <- brasil_municipalities %>% left_join(sample2022, by = c("code_muni" = "municipality_id"))
plotDB <- plotDB %>% mutate(logEmployees = log(temporaries+1))
plotProp <- plotDB %>% ggplot() + geom_sf(aes(fill = prop))+theme_void() +labs(fill = "Proportion of Temporary Employees") + theme(legend.position = "bottom") + scale_fill_distiller(palette = "GnBu")
plotTotal <- plotDB %>% ggplot() + geom_sf(aes(fill = logEmployees))+theme_void() +labs(fill = "Total of Temporary Employees (Log)") + theme(legend.position = "bottom") + scale_fill_distiller(palette = "GnBu")
finalPlotComissionate <- ggarrange(plotTotal, plotProp, ncol = 2, nrow = 1)
ggsave(finalPlotComissionate, file=paste0(figureDir, "ComissionatesDescriptive.png"), width=8, height=4)
# Descriptive Figure - FPM 2023
sample2022 <- sample2022 %>% filter(population <= 50000)
sample2022$Valor <- sample2022$transfer_value/1e6
pFPMExercise <- ggplot(sample2022, aes(population, Valor)) +
  geom_vline(xintercept = 10188, linetype="dotted", color = "red") +
  geom_vline(xintercept = 13584 , linetype="dotted", color = "red") +
  geom_vline(xintercept = 16980 , linetype="dotted", color = "red") +
  geom_vline(xintercept = 23772 , linetype="dotted", color = "red") +
  geom_vline(xintercept = 30564 , linetype="dotted", color = "red") +
  geom_vline(xintercept = 37356 , linetype="dotted", color = "red") +
  geom_vline(xintercept = 44148  , linetype="dotted", color = "red") +
  ylab('FPM Transfers (in R$ million, 1999 prices)') +
  xlab('Population') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point()
ggsave(pFPMExercise, file=paste0(figureDir, "fpm2022.png"), width=7, height=4)
# Figures for FPM
plotDB <- plotDB %>% mutate(logFPM = log(transfer_value))
plotDB <- plotDB %>% mutate(logFPMLaw = log(fpmLaw))
plotDB <- plotDB %>% mutate(corrFPM = transfer_value/transfer_value)
plotLaw <- plotDB %>% ggplot() + geom_sf(aes(fill = logFPMLaw))+theme_void() +labs(fill = "Law Implied FPM (Log)") + theme(legend.position = "bottom") + scale_fill_distiller(palette = "GnBu")
plotActual <- plotDB %>% ggplot() + geom_sf(aes(fill = logFPM))+theme_void() +labs(fill = "Actual FPM Received (Log)") + theme(legend.position = "bottom") + scale_fill_distiller(palette = "GnBu")
finalPlotFPM <- ggarrange(plotLaw, plotActual, ncol = 2, nrow = 1)
ggsave(finalPlotFPM, file=paste0(figureDir, "FPMDescriptive.png"), width=8, height=4)
# Descriptive Figures for local public finances
classify_population_range <- function(population) {
  case_when(
    population <= 10188 ~ "Until \n 10.188",
    population >= 10189 & population <= 13584 ~ "From 10.189 \n to 13.584",
    population >= 13585 & population <= 16980 ~ "From 13.585 \n to 16.980",
    population >= 16981 & population <= 23772 ~ "From 16.981 \n to 23.772",
    population >= 23773 & population <= 30564 ~ "From 23.773 \n to 30.564",
    population >= 30565 & population <= 37356 ~ "From 30.565 \n to 37.356",
    population >= 37357 & population <= 44148 ~ "From 37.357 \n to 44.148",
    population >= 44149 & population <= 50940 ~ "From 44.149 \n to 50.940",
    population >= 50941 & population <= 61128 ~ "From 50.941 \n to 61.128",
    population >= 61129 & population <= 71316 ~ "From 61.129 \n to 71.316",
    population >= 71317 & population <= 81504 ~ "From 71.317 \n to 81.504",
    population >= 81505 & population <= 91692 ~ "From 81.505 \n to 91.692",
    population >= 91693 & population <= 101880 ~ "From 91.693 \n to 101.880",
    population >= 101881 & population <= 115464 ~ "From 101.881 \n to 115.464",
    population >= 115465 & population <= 129048 ~ "From 115.465 \n to 129.048",
    population >= 129049 & population <= 142632 ~ "From 129.049 \n to 142.632",
    population >= 142633 & population <= 156216 ~ "From 142.633 \n to 156.216",
    population > 156216 ~ "Above \n 156.216",
    TRUE ~ NA_character_ # Handles any cases not covered, like NA population values
  )
}
population_levels_ordered <- c(
  "Until \n 10.188",
  "From 10.189 \n to 13.584",
  "From 13.585 \n to 16.980",
  "From 16.981 \n to 23.772",
  "From 23.773 \n to 30.564",
  "From 30.565 \n to 37.356",
  "From 37.357 \n to 44.148",
  "From 44.149 \n to 50.940",
  "From 50.941 \n to 61.128",
  "From 61.129 \n to 71.316",
  "From 71.317 \n to 81.504",
  "From 81.505 \n to 91.692",
  "From 91.693 \n to 101.880",
  "From 101.881 \n to 115.464",
  "From 115.465 \n to 129.048",
  "From 129.049 \n to 142.632",
  "From 142.633 \n to 156.216",
  "Above \n 156.216"
)
fullSample <- fullSample %>% mutate(faixa = classify_population_range(population))
fullSample <- fullSample %>%
  mutate(faixa = factor(faixa, levels = population_levels_ordered, ordered = TRUE))
g <-  fullSample %>% filter(!is.na(total_revenue)) %>% group_by(faixa) %>% summarise(prop = (sum(transfer_value)/sum(total_revenue))*100)
p <- ggplot(data = g, aes(x = faixa, y = prop)) + geom_bar(stat="identity", fill = "#afeeee") + xlab("Population Range") + ylab("% of Revenue \n from FPM") + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1.05, hjust=1))
ggsave(p,file=paste0(figureDir, "localFinances.png"), width=9.5, height=4)
# Time Series Comissionates
timeSeries <- fullSample %>%
  filter(!is.na(temporaries)) %>%
  group_by(year) %>%
  summarise(total = sum(temporaries))
pTimeSeries <- ggplot(timeSeries, aes(year, total)) + 
  geom_line(color = "#000080") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Year") +
  ylab("Quantity of Temporary/Comissioned Employees")
ggsave(pTimeSeries,file=paste0(figureDir, "TimeSeriesComissionate.png"), width=7, height=4)
