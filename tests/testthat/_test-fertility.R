library(dplyr)
library(statepop)

context("Fertility rate testing")


path <- "C:\\Users\\deci859\\PycharmProjects\\IM3\\statepop\\statepop\\inputs"

# Path to results folder
resultsPath <- file.path(path, "No_Mig")

# Path to state-level inputs folder
inputsPath  <- file.path(path, "State_Inputs")

outputPath <- "C:\\Users\\deci859\\PycharmProjects\\IM3\\statepop\\tests\\data\\comp_data\\tot_fert.csv"

#regUAll <- list.dirs(file.path(path, "State_Inputs"), recursive = FALSE, full.names = FALSE)
regUAll <- c("9-CT", "23-ME", "25-MA", "33-NH", "44-RI", "50-VT", "34-NJ", "36-NY", "42-PA", "17-IL", "18-IN", "26-MI", "39-OH",
             "55-WI", "19-IA", "20-KS", "27-MN", "29-MO", "31-NE", "38-ND", "46-SD", "10-DE", "11-DC", "12-FL", "13-GA", "24-MD",
             "37-NC", "45-SC", "51-VA", "54-WV", "1-AL", "21-KY", "28-MS", "47-TN", "5-AR", "22-LA", "40-OK", "48-TX", "4-AZ",
             "8-CO", "16-ID", "30-MT", "32-NV", "35-NM", "49-UT", "56-WY", "2-AK", "6-CA", "15-HI", "41-OR", "53-WA")



test_that("Expect equal output", {

  options("scipen"=100, "digits"=4) # to force R not to use scientific notations


  # Read in comparison dataframe
  comp_fert <- readRDS("C:\\Users\\deci859\\PycharmProjects\\IM3\\statepopR\\tests\\data\\comp_data\\tot.fert.rds")
  print(dim(comp_fert))

  # Execute fertility using default settings, save as RDS for comparison, and reimport
  fert <- fertility(inputsPath=inputsPath, regUAll=regUAll, cur.scenario="Constant_rate", vis=F)
  saveRDS(fert, "C:\\Users\\deci859\\PycharmProjects\\IM3\\statepopR\\tests\\data\\comp_data\\tot_fert.rds")
  fert_rds <- readRDS("C:\\Users\\deci859\\PycharmProjects\\IM3\\statepopR\\tests\\data\\comp_data\\tot_fert.rds")
  print(dim(fert_rds))

  # Compare two dataframes
  test_fert <- all_equal(target=comp_fert, current=fert_rds)

  print(test_fert)
  testequal <- all.equal(comp_fert, fert)
  print(testequal)

  expect_true(test_fert)

})

