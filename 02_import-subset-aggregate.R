#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: FEBUXOSTAT - Q1 2017 - Q1 2020	                                                                          #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JUAN HINCAPIE-CASTILLO, PHARMD, PHD, MS                                    #
# SCRIPT: 02_import_subset_aggregate.R                                                                    			  #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

## ---------------------------------------------------------------- ##
## PULL PRESCRIPTIONS FOR EVERY STATE FOR:                          ##
## (1) FEBUXOSTAT                                                   ##
## ---------------------------------------------------------------- ##

## ------------- ##
## Load packages ##
## ------------- ##

library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(purrr)
library(readr)
library(stringr)
library(tidylog)
library(viridis)

## ----------------------------- ##
## Step 1: Pull in SDUD dataset. ##
## ----------------------------- ##

sdud_2017_2020 <-
  read_rds(
    file = loc
  ) %>%
  filter(year %in% c(2017:2019) | year == 2020 & quarter == 1)

## Checking year pull.

sdud_2017_2020 %>%
  distinct(year, quarter) %>%
  arrange(year, desc(quarter))

## ------------------------------------------------------------------ ##
## Step 2: Flag records using generic name.                           ##
## ------------------------------------------------------------------ ##

sdud_2017_2020_gennmes <- 
  sdud_2017_2020 %>%
  mutate(
    febuxostat_flag = case_when(
      str_detect(string = gennme_c, pattern = regex("febuxostat", ignore_case = TRUE)) ~ "1",
      TRUE ~ "0"
    )
  )

## Checking flags.

sdud_2017_2020_gennmes %>%
  select(febuxostat_flag) %>%
  map(., janitor::tabyl)

## ----------------------------------------------------##
## Step 3: Aggregate by state, year, quarter and sum.  ##
## ----------------------------------------------------##

sdud_2017_2020_febux_flags <-
  sdud_2017_2020_gennmes %>%
  filter(
    febuxostat_flag == "1"
  )

## Create data set with ndc, generic names, and product names included.

ndc_gen_brand_names <- 
  sdud_2017_2020_febux_flags %>%
  distinct(gennme_c, prodnme, ndc)

## Also should output an aggregate by drug.

sdud_2017_2020_febux_rx <-
  sdud_2017_2020_febux_flags %>%
  group_by(state, year, quarter, gennme_c, suppression_used) %>%
  summarize(total_prescriptions = sum(number_of_prescriptions))

## Keeping the Suppression Used column once I get direction from JHC.

sdud_2017_2020_febux <- 
  sdud_2017_2020_febux_flags %>%
  group_by(state, year, quarter, suppression_used) %>%
  summarize(total_prescriptions = sum(number_of_prescriptions))

## ---------------------------------------------------- ##
## Step 4: Plot number of prescriptions over time.      ##
## ---------------------------------------------------- ##

## Make plots for overall numbers.

rx_by_year_quarter <-
  ggplot(data = sdud_2017_2020_febux) +
  geom_col(aes(x = paste0(year, "-", quarter), y = total_prescriptions), fill = "forestgreen", alpha = 0.95) +
  scale_y_continuous(labels = scales::comma) + 
  theme_ipsum_rc(axis_title_just = "ct") +
  ggtitle("Number of Prescriptions by Year and Quarter") +
  xlab("Year-Quarter") +
  ylab("Total Number of Prescriptions") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black", size = 10),
    axis.title.y = element_text(color = "black", size = 10),
  ) +
  coord_cartesian(expand = FALSE)

rx_by_year_quarter_state <-
  ggplot(data = sdud_2017_2020_febux) +
  geom_col(aes(x = paste0(year, "-", quarter), y = total_prescriptions, fill = state), alpha = 0.95) +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) + 
  theme_ipsum_rc(axis_title_just = "ct") +
  ggtitle("Number of Prescriptions by Year, Quarter, and State") +
  xlab("Year-Quarter") +
  ylab("Total Number of Prescriptions") +
  theme(
    axis.text.x = element_text(color = "black", angle = 90, size = 10, hjust = 0.25, vjust = 0.25),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black", size = 10),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "none"
  ) +
  facet_wrap(~state) +
  coord_cartesian(expand = FALSE)

## Make plots for individual drugs.

rx_by_year_quarter_drug <- 
  ggplot(data = sdud_2017_2020_febux_rx) +
  geom_col(aes(x = paste0(year, "-", quarter), y = total_prescriptions, fill = gennme_c)) +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  theme_ipsum_rc(axis_title_just = "ct") +
  ggtitle("Number of Prescriptions by Generic Name, Year, and Quarter") +
  xlab("Year-Quarter") +
  ylab("Total Number of Prescriptions") +
  labs(fill = "Generic Name") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12)
  ) +
  coord_cartesian(expand = FALSE)

## ------------------------------ ##
## Step 5: Output data and plots  ##
## ------------------------------ ##

## Write out data to clean folder.

write_csv(x = ndc_gen_brand_names, file = "./data/clean/febuxostat-drugs-generic-brand-ndc.csv")
write_csv(x = sdud_2017_2020_febux, file = "./data/clean/febuxostat-drugs-q1-2017-q1-2020-year-quarter-state.csv")
write_csv(x = sdud_2017_2020_febux_rx, file = "./data/clean/febuxostat-drugs-q1-2017-q1-2020-year-quarter-state-drug.csv")

## Save plots to the plots folder.

png(filename = "./plots/z-drugs-q1-2017-q1-2020-by-year-quarter.png", width = 16, height = 9, units = "in", res = 1200)
rx_by_year_quarter
dev.off()

png(filename = "./plots/z-drugs-q1-2017-q1-2020-by-year-quarter-state.png", width = 18, height = 12, units = "in", res = 1200)
rx_by_year_quarter_state
dev.off()

png(filename = "./plots/z-drugs-q1-2017-q1-2020-by-year-quarter-drug.png", width = 16, height = 9, units = "in", res = 1200)
rx_by_year_quarter_drug
dev.off()