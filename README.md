# Brief Introduction
This document explains the scripts used for the multi-state population projection
model. The model is composed of a number of scripts that together provide population projection according to different scenarios at the U.S. state level.

These scripts are:
- R scripts
  - Dom_Mig_Bi_Standardizer.R
  - Scenario_Construction.R
  - Multi_State_Model_Bilateral.R
  - First_Pop_Add.R
  - Pop_Dis_Vis.R
  - Regional_Migration_Vis.R
  - National_Pop_Vis_IIASA.R
  - Fert_Int_Mig.R
  - Single_State_Vis.R
- Python scripts
  - state_level_pop_vis.py
  - state_level_upscaled_vis.py

This set includes scripts for data preparation, running the model itself, and generating different additional results from the model and its initial outputs.

# More Details on Scripts
In order to produce results, the scripts should be run in a particular order. In the following, we describe this order and what each scripts does.

1. Dom_Mig_Bi_Standardizer.R
  - Standardizes bilateral migration rates. It takes the original master table containing bilateral
	migration rates and creates two csv files per state. One is in-migration that includes migration rates to each state (including DC) from all other states at 1-year intervals for 101 age groups. The other is out-migration that includes migration rates from each state to all other states following the same specifications.
2. Scenario_Construction.R
  - Creates scenarios (SSP2, SSP3 and SSP5) based on assumptions on TFR, life expectancy at age 0,
	international migration and domestic migration by incorporating historical records and IIASA's equivalent projections. This script should be run if SSP-based scenarios are required for projection. For the other scenarios, this script is not needed, and they can be accounted for by modifying the initial parameters of the main script.
3. Multi_State_Model_Bilateral.R
  - This is the main script that projects population by states for the U.S. The model projects different state-level population estimates based on the mechanical scenarios (domestic migration: 0, 0.5, 2, international migration: 0 and 1) or SSP-based scenarios (SSP2, SSP3 and SSP5).
