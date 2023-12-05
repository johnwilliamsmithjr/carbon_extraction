## soil_extraction.R
## Author: John W. Smith
## DESCRIPTION: this R script is intended to demonstrate
## a pipeline to extract soil carbon measurements using
## NEON megapit data

## load packages
library(neonstore)
library(tidyverse)
library(lubridate)

## Download bieogeochemistry soil data to get carbon concentration
## Download physical soil data to get bulk density
data_product_soil <- "DP1.00096.001"
## set site
site <- 'UNDE'

## download data product for given site
neon_download(product = data_product_soil, site = site)

## access mgp_perbiogeosample and mgp_perbulksample
## using the neon_store function
neon_store('mgp_perbiogeosample')
neon_store('mgp_perbulksample')

## read in data using neon_read
mgc_perbiogeosample <- neon_read('mgp_perbiogeosample', site = site)
mgp_perbulksample <- neon_read('mgp_perbulksample', site = site)

## extract bulk density
bulk_density <- mgp_perbulksample %>%
  filter(bulkDensSampleType == "Regular") %>%
  select(horizonName,bulkDensExclCoarseFrag)

## extract horizon carbon
horizon_carbon <- mgc_perbiogeosample %>%
  filter(biogeoSampleType == "Regular") %>%
  select(horizonName,biogeoTopDepth,biogeoBottomDepth,carbonTot)

## inner join horizon carbon with bulk density using horizonName
## compute depth of layers and perform unit conversion
horizon_combined <- inner_join(horizon_carbon,bulk_density, by = "horizonName") %>%
  #Convert volume in g per cm3 to mass per area in g per cm2 by multiplying by layer thickness
  mutate(horizon_soil_g_per_cm2 = (biogeoBottomDepth - biogeoTopDepth) * bulkDensExclCoarseFrag) %>%
  #Units of carbon are g per Kg soil but we have bulk density in g per cm2 so convert Kg soil to g soil
  mutate(CTot_g_per_g_soil = carbonTot*(1/1000),  #Units are g C per g soil
         horizon_C_g_percm2 = CTot_g_per_g_soil*horizon_soil_g_per_cm2, #Units are g C per cm2
         horizon_C_kg_per_m2 = horizon_C_g_percm2 * 10000 / 1000) %>% #Units are g C per m2
  select(-CTot_g_per_g_soil,-horizon_C_g_percm2) %>%
  arrange(biogeoTopDepth)

## sum across each horizon to get total site soil carbon
## in kgC/m^2
site_soil_carbon <- horizon_combined %>%
  summarize(soilC_gC_m2 = sum(horizon_C_kg_per_m2),
            date = unique(mgc_perbiogeosample$collectDate))
