
context("Baseline population testing")

# TODO:  remove later with a test set of inputs that are smaller
path <- "/Users/d3y010/projects/statepop/inputs"

# Path to results folder
resultsPath <- file.path(path, "No_Mig")

# Path to state-level inputs folder
inputsPath  <- file.path(path, "State_Inputs")

regUAll <- c("9-CT", "23-ME", "25-MA", "33-NH", "44-RI", "50-VT", "34-NJ", "36-NY", "42-PA", "17-IL", "18-IN", "26-MI", "39-OH",
             "55-WI", "19-IA", "20-KS", "27-MN", "29-MO", "31-NE", "38-ND", "46-SD", "10-DE", "11-DC", "12-FL", "13-GA", "24-MD",
             "37-NC", "45-SC", "51-VA", "54-WV", "1-AL", "21-KY", "28-MS", "47-TN", "5-AR", "22-LA", "40-OK", "48-TX", "4-AZ",
             "8-CO", "16-ID", "30-MT", "32-NV", "35-NM", "49-UT", "56-WY", "2-AK", "6-CA", "15-HI", "41-OR", "53-WA")



test_that("Expect equal output from the basepop function", {

  options("scipen"=100, "digits"=4) # to force R not to use scientific notations

  # Read in comparison dataframe
  comp_pop <- readRDS("/Users/d3y010/repos/temp/statepop/tests/data/comp_data/tot.state.net.mig.rds")

  # Execute basepop using default settings
  sim_pop <- basepop(inputsPath=inputsPath, regUAll=regUAll)

  # Compare two dataframes
  test_pop <- dplyr::all_equal(target=comp_pop, current=sim_pop)

})
