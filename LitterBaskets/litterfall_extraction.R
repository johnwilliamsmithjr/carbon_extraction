## load packages
library(neonstore)
library(tidyverse)
library(lubridate)

site = 'UNDE'

## set data products
DP_litterfall <- "DP1.10033.001"
DP_soil_physics <- "DP1.00096.001"

## download data products for litterfall, soil physics
neon_download(product = DP_litterfall, site = site)
neon_download(product = DP_soil_physics, site = site)

## access ltr_pertrap and ltr_massdata-basic
## using the neon_store function
neon_store('ltr_pertrap')
neon_store('ltr_massdata-basic')

## read in litter per trap data for the site
## using neon_table. store as trap_table
trap_table <-  neon_table("ltr_pertrap", site = site)

## read in mass data for the site using
## neon_table. store as mass_table
mass_table <- neon_table('ltr_massdata-basic', site = site)

## subset trap table
trap_table <- trap_table %>%
  select(trapID,trapType,trapSize,plotType)

## perform a left join of trap table to mass table
## by trapID
mass_table <- mass_table %>%
  left_join(trap_table , by = "trapID")

## mutate time interval, interval length,
## mass per day, and mass per meter squared per day
## to the mass_table dataframe
## subset only necessary quantities, group traps, and
## summarize the total litter amount
mass_table <- mass_table %>%
  mutate(interval = interval(start = setDate, end = collectDate),
         int_length = int_length(interval)/(60*60*24),
         mass_per_day = dryMass/int_length,
         mass_per_m2_per_day = mass_per_day/trapSize) %>%
  select(trapID,setDate,collectDate,mass_per_m2_per_day,functionalGroup,trapType) %>%
  group_by(trapID,setDate,collectDate,trapType) %>%
  summarize(total_litter = sum(mass_per_m2_per_day))

## mutate a dateRange variable
mass_table$dateRange <- paste(mass_table$setDate, mass_table$collectDate, sep = '-')

## create litter dataframe by grouping by
## dateRange, so that baskets set out from
## the same dates form groups. summarize
## total litter by averaging within group
litters <- mass_table %>%
  group_by(dateRange) %>%
  summarize(mean(total_litter))

## create year_time sequence from the earliest setDate to the latest setDate
year_time <- seq(from = min(mass_table$setDate), to = max(mass_table$setDate), by = "1 day")

## initialize a daily_mass matrix
daily_mass <- matrix(NA, nrow = c(length(unique(mass_table$trapID))), ncol = length(year_time))

## identify unique trapIDs
trapID_list <- unique(mass_table$trapID)

## double for loop over unique trapIDs and dates
for(j in 1:length(trapID_list)){
  for(i in 1:length(year_time)){
    ## identify indices of traps that were in the field during the
    ## given date
    index <- which(mass_table$setDate < year_time[i] & mass_table$collectDate >=  year_time[i] & mass_table$trapID == trapID_list[j])
    if(length(index) > 0)
      ## if any indices exist, compute the mean and store it
      daily_mass[j,i] <- mean(unlist(as.numeric(mass_table$total_litter[index])))
  }
}
## replace any possible NaN with NA
daily_mass[is.nan(daily_mass)] <- NA

## convert daily mass to a dataframe
daily_mass <- as.data.frame((daily_mass))

## initialize plotID column
daily_mass$plotID <- rep(NA, nrow(daily_mass))

## extract plotID by string splitting the
## trapID_list
for (i in 1:length(daily_mass$plotID)){
  daily_mass$plotID[i] <- strsplit(x = trapID_list[i], split = '_')[[1]][2]
}

## create plot_daily_mass data.frame
plot_daily_mass <- daily_mass %>%
  ## filter to only plotIDs of interest
  filter(plotID %in% c('037', '043', '065')) %>%
  ## group_by plotID
  group_by(plotID) %>%
  ## summarize using mean
  summarise_at(paste0('V', 1:(ncol(daily_mass)-1)), mean, na.rm = TRUE)

## more elegant attempt i'm working on
#litterfall2 <- plot_daily_mass %>%
#                select(-plotID) %>%
#                summarise_at(paste0('V', 1:(ncol(daily_mass)-1)), mean, na.rm = TRUE)

## extract colMeans
litterfall <- as.data.frame(.5*colMeans(plot_daily_mass[2:(ncol(daily_mass)-1)], na.rm = TRUE))
## set colnames
colnames(litterfall) <- 'kgCm2d'
## mutate day
litterfall$day <- year_time
