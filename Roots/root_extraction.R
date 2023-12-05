## root_extraction.R
## Author: John W. Smith
## DESCRIPTION: this R script is intended to demonstrate
## a pipeline to extract root carbon measurements using
## NEON data

## load packages
library(neonstore)
library(tidyverse)
library(lubridate)

## root chemistry data product
roots_DP <- 'DP1.10067.001'

## set site
site <- 'UNDE'

## use neon_download from neonstore to read in data
neon_download(product = roots_DP, site = site)

## access bbc_percore-basic and bbc_rootmass-basic
## using the neon_store function
neon_store('bbc_percore-basic')
neon_store('bbc_rootmass-basic')


## read in bbc_percore
bbc_percore <- neon_read('bbc_percore-basic', site = site, product = roots_DP)

## read in rootmass data
rootmass <- neon_read('bbc_rootmass-basic', site = site, product = roots_DP)

## extract year
rootmass$year = year(rootmass$collectDate)

## set variables for liveDryMass, deadDryMass, unkDryMass, area
rootmass$liveDryMass <- rep(0, nrow(rootmass))
rootmass$deadDryMass <- rep(0, nrow(rootmass))
rootmass$unkDryMass <- rep(0, nrow(rootmass))
rootmass$area <- rep(NA, nrow(rootmass))

for (i in 1:nrow(rootmass)){
  ## match by sample ID
  ind <- which(bbc_percore$sampleID == rootmass$sampleID[i])
  ## extract core sample area
  rootmass$area[i] <- bbc_percore$rootSampleArea[ind]
  ## categorize mass as live, dead, or unknown
  if (is.na(rootmass$rootStatus[i])){
    rootmass$unkDryMass[i] <- rootmass$dryMass[i]
  } else if (rootmass$rootStatus[i] == 'live'){
    rootmass$liveDryMass[i] <- rootmass$dryMass[i]
  } else if (rootmass$rootStatus[i] == 'dead'){
    rootmass$deadDryMass[i] <- rootmass$dryMass[i]
  } else{
    rootmass$unkDryMass[i] <- rootmass$dryMass[i]
  }
}

##
site_roots <- rootmass %>%
  ## filter plotID to only our plots of interest
  filter(plotID %in% c('UNDE_037', 'UNDE_043', 'UNDE_065')) %>%
  ## group by year
  group_by(year) %>%
  ## sum live, dead, unknown root masses. multiply by
  ## .5 for conversion to kgC/m^2
  summarize(mean_kgCperm2_live = .5*sum(liveDryMass/area, na.rm = TRUE)/1000,
            mean_kgCperm2_dead = .5*sum(deadDryMass/area, na.rm = TRUE)/1000,
            mean_kgCperm2_unk = .5*sum(unkDryMass/area, na.rm = TRUE)/1000,
            year_total = sum(c(mean_kgCperm2_dead, mean_kgCperm2_live, mean_kgCperm2_unk)) / length(unique(plotID)),
            med_date = median(collectDate))

