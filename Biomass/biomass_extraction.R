## biomass_extraction.R
## Author: John W. Smith
## DESCRIPTION: this R script is intended to demonstrate
## a pipeline to extract various descriptions of biomass
## carbon using NEON data.

## load packages
library(neonstore)
library(tidyverse)
library(lubridate)
library(allodb)

## choose site ID
unde_id <- 'UNDE_065' ## 037, 043, 065

## choose site
site = 'UNDE'

## set data product to DP1.10098.001
## this is NEONs Vegetation structure data
data_product <- "DP1.10098.001"

## set latitude and longitude coordinates for unde
coords_unde <- c(46.2339, -89.5373)

## download the vegetation data for UNDE
neon_download(product = data_product, site = site)

## access data using neon_store
neon_store('vst_apparentindividual')

neon_store('vst_mappingandtagging-basic')

neon_store('vst_perplotperyear-basic')


## use neon_read to extract the desired data:

## individual data
ind_table <- neon_table('vst_apparentindividual-basic', site = site)

## mapping and tagging data
map_tag_table <- neon_table('vst_mappingandtagging-basic', site = site)

## plot level data
plot_table <- neon_table('vst_perplotperyear-basic', site = site)

## create category for live trees
live_category <- unique(ind_table$plantStatus)[stringr::str_detect(unique(ind_table$plantStatus),
                                                                   pattern = "Live")]
## create category for dead trees
dead_category <- c('Standing dead', 'Downed', 'Dead, broken bole', 'No longer qualifies', 'Dead')


## extract genus and species information
genus_species <- unlist(str_split_fixed(map_tag_table$scientificName, " ", 3))

## mutate genus and species to map tag table
## for allodb
map_tag_table <- map_tag_table %>%
  mutate(GENUS = genus_species[,1],
         SPECIES = genus_species[,2])

## subset map_tag_table
select_map_tag_table <- map_tag_table %>%
  select(individualID, scientificName, GENUS, SPECIES)

## subset plot_table
select_plot_table <- plot_table %>%
  select(plotID,totalSampledAreaTrees,plotType) %>%
  distinct(plotID, .keep_all = TRUE)

## subset individual table
select_ind_table <- ind_table %>%
  select(individualID, plotID, date, stemDiameter,plantStatus, measurementHeight, eventID)

## combine three subsetted tables to create combined_table
combined_table <- inner_join(select_ind_table, select_map_tag_table, by = "individualID") %>%
  arrange(plotID,individualID)

combined_table <- inner_join(combined_table, select_plot_table, by = c("plotID")) %>%
  arrange(individualID)

#combined_table <- combined_table[order(combined_table$date),]

#ind_table <- ind_table[order(ind_table$date),]

ind_table$plantStatus[is.na(ind_table$plantStatus)] <- 'NoData'

## create plot event date dataset for given plots
##
plot_ev_date <- combined_table %>%
  filter(plotType == 'tower' & plotID %in% c('UNDE_037', 'UNDE_043', 'UNDE_065')) %>%
  group_by(plotID, eventID) %>%
  summarise(med_date = median(date, na.rm = TRUE))

## subset combined_table to include only tower plots,
## mutate year, and filter to plot ID of interest
combined_table_tower <- combined_table %>%
  mutate(year = year(date)) %>%
  arrange(individualID,eventID) %>%
  filter(plotType == "tower") %>%
  filter(plotID == unde_id) %>%
  mutate(stemDiameter = ifelse(individualID == 'NEON.PLA.D05.UNDE.05321' & eventID == 'vst_UNDE_2019', 15.4, stemDiameter))
## note: final mutate statement was to fix a confirmed
## data entry error

# create plot individual id dataframe using
## combined_table_tower
plot_ind_id <- combined_table_tower %>%
  group_by(individualID) %>%
  summarise(plotID = first(plotID))

## perform pivoting plant status for the
## combined_table_tower dataframe
## also performs:
## - filtering of measurement height
## - arranges rows by measurementHeight
## - summarizes measurementHeight, plantStatus to be first observations
## (in case of multiple observations)

pivot_combined_table_tower <- combined_table_tower %>%
  filter(measurementHeight >= 130 | plantStatus %in% dead_category) %>%
  group_by(individualID, eventID) %>%
  arrange(desc(measurementHeight), stemDiameter) %>%
  summarize(measurementHeight = first(measurementHeight),
            plantStatus = first(plantStatus),
            .groups = "drop")  %>%
  select(individualID, eventID, plantStatus) %>%
  pivot_wider(names_from = individualID, values_from = plantStatus, values_fill = NA) %>%
  pivot_longer(cols = -c("eventID") , names_to = "individualID", values_to = "plantStatus")

## perform additional filtering on combined_table_tower
## summarize operations take care of cases of multiple observations
## mutate statements on stemDiameter perform linear interpolation when
## between observed values

filtered_ctt <- combined_table_tower %>%
  filter(measurementHeight >= 130 | plantStatus %in% dead_category) %>%
  select(individualID, eventID, plantStatus, measurementHeight, stemDiameter, SPECIES, GENUS, date, totalSampledAreaTrees) %>%
  group_by(individualID, eventID) %>%
  arrange(desc(measurementHeight), stemDiameter) %>%
  summarize(measurementHeight = first(measurementHeight),
            stemDiameter = first(stemDiameter),
            SPECIES = first(SPECIES),
            GENUS = first(GENUS),
            date = first(date),
            totalSampledAreaTrees = first(totalSampledAreaTrees)
  ) %>%
  mutate(stemDiameter = ifelse(eventID != first(eventID) & !is.na(lag(stemDiameter)) & stemDiameter < lag(stemDiameter), lag(stemDiameter), stemDiameter),
         stemDiameter = ifelse(eventID != last(eventID) & is.na(stemDiameter) & !is.na(lag(stemDiameter)) & !is.na(lead(stemDiameter)),
                               .5*(lag(stemDiameter) + lead(stemDiameter)), stemDiameter),
         stemDiameter = ifelse(eventID == last(eventID) & is.na(stemDiameter) & !is.na(lag(stemDiameter)), lag(stemDiameter), stemDiameter)) %>%
  ungroup()

## create dataframe to store the biomass
## start with pivot_combined_table_tower, and
## leftjoin filtered_ctt, plot_ind_id, plot_ev_date
## perform sanity checks on plantStatus to make sure
## that living plants that were previously "dead" do not
## get counted towards standing dead biomass
## create flags for ingrowth, growth, removal, standing dead,
## and "zombie" (trees that become alive after being dead)

biomass_df <- pivot_combined_table_tower %>%
  left_join(filtered_ctt, by = c('individualID', 'eventID')) %>%
  left_join(plot_ind_id, by = 'individualID') %>%
  left_join(plot_ev_date, by = c('plotID', 'eventID')) %>%
  group_by(individualID) %>%
  arrange(eventID) %>%
  mutate(plantStatus = ifelse((eventID != last(eventID)) & (plantStatus %in% dead_category) & (lead(plantStatus) %in% live_category), "Live", plantStatus),
         plantStatus = ifelse((eventID != last(eventID)) & !is.na(lag(plantStatus)) & !is.na(lead(plantStatus))  & (lag(plantStatus) %in% live_category) & (is.na(plantStatus)) & (lead(plantStatus) %in% live_category), "Live", plantStatus),
         plantStatus = ifelse((eventID != last(eventID)) & !is.na(lag(plantStatus)) & !is.na(lead(plantStatus))  & (lag(plantStatus) %in% live_category) & (is.na(plantStatus)) & (lead(plantStatus) %in% dead_category), "Dead", plantStatus),
         plantStatus = ifelse((eventID != last(eventID)) & !is.na(lead(plantStatus))  & (lead(plantStatus) == 'Standing dead') & (is.na(plantStatus) | (plantStatus %in% dead_category)), "Standing dead", plantStatus),
         GENUS = ifelse(eventID != first(eventID) & is.na(GENUS), lag(GENUS), GENUS),
         SPECIES = ifelse(eventID != first(eventID) & is.na(SPECIES), lag(SPECIES), SPECIES),
         totalSampledAreaTrees = ifelse(eventID != first(eventID) & is.na(totalSampledAreaTrees), lag(totalSampledAreaTrees), totalSampledAreaTrees),
         stemDiameter = ifelse(eventID != first(eventID) & !is.na(lag(stemDiameter)) & stemDiameter < lag(stemDiameter), lag(stemDiameter), stemDiameter),
         stemDiameter = ifelse(eventID != last(eventID) & is.na(stemDiameter) & !is.na(lag(stemDiameter)) & !is.na(lead(stemDiameter)),
                               .5*(lag(stemDiameter) + lead(stemDiameter)), stemDiameter),
         stemDiameter = ifelse(eventID == last(eventID) & is.na(stemDiameter) & !is.na(lag(stemDiameter)), lag(stemDiameter), stemDiameter), ## revisit this
         turnover_flag = (eventID != first(eventID)) & (lag(plantStatus) %in% live_category) & (plantStatus %in% dead_category | is.na(plantStatus)),
         growth_status = (eventID != first(eventID)) & (lag(plantStatus) %in% live_category) & (plantStatus %in% live_category),
         ingrowth_status = (eventID != first(eventID)) & (is.na(lag(plantStatus))) & (plantStatus %in% live_category),
         stemDiameter = ifelse((ingrowth_status == TRUE) & is.na(stemDiameter) & (eventID != last(eventID)), lead(stemDiameter), stemDiameter),
         stemDiameter = ifelse((growth_status == TRUE) & is.na(stemDiameter), lag(stemDiameter), stemDiameter),
         removal_flag = (eventID != first(eventID)) & (lag(plantStatus) %in% live_category) & (plantStatus == 'Removed'),
         standing_dead_flag = (plantStatus == 'Standing dead'),
         zombie_flag = (plantStatus %in% live_category) & (lag(plantStatus) %in% dead_category),
         date = as_date(ifelse(is.na(date), med_date, date)),
         duration = int_length(interval(start = lag(date), end = date)) / (60*60*24) )#,

## the following code chunk is fairly unintuitive, and I am
## in search of a more succinct way to make it work. so, if
## anyone sees this and has an idea, please reach out.

## we start by creating a dummy "reference" biomass_df
## we then apply the same tidy operations as above to our
## biomass dataframe. the reason is that the turnover flags,
## standing dead flags, etc may not be accurately captured because
## of the presence of zombie trees. when we eliminate some zombie trees,
## new ones may appear. so we use a while loop to iterate through until
## the tidyverse operations do not change the dataframe anymore.

ref_biomass_df <- 1
n_iter = 1
while (!all(biomass_df == ref_biomass_df, na.rm = TRUE)){
  ref_biomass_df <- biomass_df
  biomass_df <- biomass_df %>%
    group_by(individualID) %>%
    arrange(eventID) %>%
    mutate(plantStatus = ifelse((eventID != last(eventID)) & (plantStatus %in% dead_category) & (lead(plantStatus) %in% live_category), "Live", plantStatus),
           plantStatus = ifelse((eventID != last(eventID)) & !is.na(lag(plantStatus)) & !is.na(lead(plantStatus))  & (lag(plantStatus) %in% live_category) & (is.na(plantStatus)) & (lead(plantStatus) %in% live_category), "Live", plantStatus),
           plantStatus = ifelse((eventID != last(eventID)) & !is.na(lag(plantStatus)) & !is.na(lead(plantStatus))  & (lag(plantStatus) %in% live_category) & (is.na(plantStatus)) & (lead(plantStatus) %in% dead_category), "Dead", plantStatus),
           plantStatus = ifelse((eventID != last(eventID)) & !is.na(lead(plantStatus))  & (lead(plantStatus) == 'Standing dead') & (is.na(plantStatus) | (plantStatus %in% dead_category)), "Standing dead", plantStatus),
           GENUS = ifelse(eventID != first(eventID) & is.na(GENUS), lag(GENUS), GENUS),
           SPECIES = ifelse(eventID != first(eventID) & is.na(SPECIES), lag(SPECIES), SPECIES),
           totalSampledAreaTrees = ifelse(eventID != first(eventID) & is.na(totalSampledAreaTrees), lag(totalSampledAreaTrees), totalSampledAreaTrees),
           stemDiameter = ifelse(eventID != first(eventID) & !is.na(lag(stemDiameter)) & stemDiameter < lag(stemDiameter), lag(stemDiameter), stemDiameter),
           stemDiameter = ifelse(eventID != last(eventID) & is.na(stemDiameter) & !is.na(lag(stemDiameter)) & !is.na(lead(stemDiameter)),
                                 .5*(lag(stemDiameter) + lead(stemDiameter)), stemDiameter),
           stemDiameter = ifelse(eventID == last(eventID) & is.na(stemDiameter) & !is.na(lag(stemDiameter)), lag(stemDiameter), stemDiameter), ## revisit this
           turnover_flag = (eventID != first(eventID)) & (lag(plantStatus) %in% live_category) & (plantStatus %in% dead_category | is.na(plantStatus)),
           growth_status = (eventID != first(eventID)) & (lag(plantStatus) %in% live_category) & (plantStatus %in% live_category),
           ingrowth_status = (eventID != first(eventID)) & (is.na(lag(plantStatus))) & (plantStatus %in% live_category),
           stemDiameter = ifelse((ingrowth_status == TRUE) & is.na(stemDiameter) & (eventID != last(eventID)), lead(stemDiameter), stemDiameter),
           stemDiameter = ifelse((growth_status == TRUE) & is.na(stemDiameter), lag(stemDiameter), stemDiameter),
           removal_flag = (eventID != first(eventID)) & (lag(plantStatus) %in% live_category) & (plantStatus == 'Removed'),
           standing_dead_flag = (plantStatus == 'Standing dead'),
           zombie_flag = (plantStatus %in% live_category) & (lag(plantStatus) %in% dead_category),
           date = as_date(ifelse(is.na(date), med_date, date)),
           duration = int_length(interval(start = lag(date), end = date)) / (60*60*24) )#,
  n_iter <- n_iter + 1
}

## once we are done, we use the get_biomass function
## from the allodb package to compute the biomass.
biomass_df <- biomass_df %>%
  mutate(biomass = get_biomass(dbh = stemDiameter, genus = GENUS, species = SPECIES, coords = coords_unde))


## extract different types of biomass carbon using
## biomass_df

## yearly plot live carbon totals
yearly_plct <- biomass_df %>%
  group_by(eventID) %>%
  filter(plantStatus %in% live_category & zombie_flag == FALSE) %>%
  summarise(kgCm2 = .5 * sum(biomass/totalSampledAreaTrees, na.rm = TRUE))

## yearly standing dead biomass
yearly_standing_dead <- biomass_df %>%
  group_by(eventID) %>%
  filter(standing_dead_flag == TRUE & zombie_flag == FALSE) %>%
  summarise(kgCm2 = .5 * sum(biomass/totalSampledAreaTrees, na.rm = TRUE))

## yearly turnover
yearly_turnover <- biomass_df %>%
  mutate(duration = duration,
         date = date,
         last_date = lag(date)) %>%
  group_by(individualID) %>%
  arrange(eventID) %>%
  mutate(t_biomass = lag(biomass),
         t_biomass_rate = lag(biomass)/duration,
         totalSampledAreaTrees = lag(totalSampledAreaTrees),
         t_biomass_rate_kgCm2d = .5*t_biomass_rate/totalSampledAreaTrees) %>%
  filter(turnover_flag == TRUE & zombie_flag == FALSE)

## yearly aggregated turnover
yearly_turnover_agg <- yearly_turnover %>%
  group_by(eventID) %>%
  summarise(kgCm2 = .5 * sum(t_biomass/(totalSampledAreaTrees), na.rm = TRUE))

## yearly biomass growth
yearly_growth <- biomass_df %>%
  mutate(duration = duration,
         date = date,
         last_date = lag(date)) %>%
  group_by(individualID) %>%
  arrange(eventID) %>%
  mutate(growth_biomass = (biomass - lag(biomass)),
         growth_biomass_rate = (biomass - lag(biomass))/duration,
         growth_biomass_rate_kgCm2d = .5*growth_biomass_rate/totalSampledAreaTrees) %>%
  filter(growth_status == TRUE & zombie_flag == FALSE)

## growth dataframe aggregated to yearly timescale
yearly_growth_agg <- yearly_growth %>%
  group_by(eventID) %>%
  summarise(kgCm2 = .5*sum(growth_biomass/totalSampledAreaTrees, na.rm = TRUE))

## create yearly ingrowth dataframe
## ingrowth describes the phenomenon when small trees
## become large enough to be sampled during a survey

yearly_ingrowth <- biomass_df %>%
  mutate(duration = duration,
         date = date,
         last_date = lag(date)) %>%
  filter(ingrowth_status == TRUE & zombie_flag == FALSE) %>%
  group_by(individualID) %>%
  arrange(eventID) %>%
  mutate(ingrowth_biomass = biomass,
         ingrowth_biomass_rate = biomass/duration,
         ingrowth_biomass_rate_kgCm2d = .5*ingrowth_biomass_rate/totalSampledAreaTrees)

## ingrowth data aggregated to yearly timescale

yearly_aggregated_ingrowth <- yearly_ingrowth %>%
  group_by(eventID) %>%
  summarise(kgCm2 = .5*(sum(ingrowth_biomass/totalSampledAreaTrees, na.rm = TRUE)))

## yearly removal dataframe
## removal describes the process of removing
## dead trees
yearly_removal <- biomass_df %>%
  group_by(individualID) %>%
  arrange(eventID) %>%
  mutate(rem_biomass = lag(biomass)/duration,
         totalSampledAreaTrees = lag(totalSampledAreaTrees)) %>%
  filter(removal_flag == TRUE & zombie_flag == FALSE)

## aggregate removal to yearly timescale
yearly_removal <- yearly_removal %>%
  group_by(eventID) %>%
  summarise(kgCm2 = .5 * sum(rem_biomass/(totalSampledAreaTrees), na.rm = TRUE))

## the following commented code is to verify that carbon was extracted correctly
## print the difference in yearly plot live carbon from start to end
## print((yearly_plct$kgCm2[nrow(yearly_plct)] - yearly_plct$kgCm2[1]))
## this should be equal to the sum of the ingrowth + the growth - the turnover
## print(sum(yearly_aggregated_ingrowth$kgCm2, na.rm = TRUE) + sum(yearly_growth_agg$kgCm2, na.rm = TRUE) -
##        sum(yearly_turnover_agg$kgCm2, na.rm = TRUE))
