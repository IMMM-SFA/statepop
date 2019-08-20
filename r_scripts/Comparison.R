# Author:     Hamidreza Zoraghein
# Date:       2019-01-21
# Purpose:    Compares the results of the simple and bilateral models in terms of net migration. It plots the net migration estimates per
#             projection year and state for both simple and bilateral models. 


###############
#*** Paths ***#
###############
path <- "C:/Users/Hamidreza.Zoraghein/Desktop/Sensitivity_Analysis"

simple.model <- file.path(path, "Simple")         # Path to results of the simple model 
bi.model     <- file.path(path, "Bilateral")      # Path to results of the bilateral model 
comp.results <- file.path(path, "Comp_Results")   # Path to comparison results


#######################
#*** Load packages ***#
#######################
# Various packages
if ("ggplot2" %in% installed.packages())
{
  library(ggplot2)
} else {
  install.packages("ggplot2")
  library(ggplot2)
}


#############################
#*** Set general options ***#
#############################
options("scipen"=100, "digits"=4) # to force R not to use scientific notations
#options(warn=2)  # treat warnings as errors


###################################
#*** Declare general variables ***#
###################################

#* Specify regions
regUAll <- c("9-CT", "23-ME", "25-MA", "33-NH", "44-RI", "50-VT", "34-NJ", "36-NY", "42-PA", "17-IL", "18-IN", "26-MI", "39-OH", 
             "55-WI", "19-IA", "20-KS", "27-MN", "29-MO", "31-NE", "38-ND", "46-SD", "10-DE", "11-DC", "12-FL", "13-GA", "24-MD", 
             "37-NC", "45-SC", "51-VA", "54-WV", "1-AL", "21-KY", "28-MS", "47-TN", "5-AR", "22-LA", "40-OK", "48-TX", "4-AZ", 
             "8-CO", "16-ID", "30-MT", "32-NV", "35-NM", "49-UT", "56-WY", "2-AK", "6-CA", "15-HI", "41-OR", "53-WA")

#* Specify scenario
scenUAll     <- c("Reg_Scenario", "Zero_Int_Mig", "Half_Scenario", "Doub_Scenario", "SSP2", "SSP3", "SSP5")
cur.scenario <- "SSP5"


#* The comparison folder for the current scenario
cur.comp.results <- file.path(comp.results, cur.scenario)


#* Generate Directories
if(!file.exists(comp.results)) {dir.create(comp.results)}         # Parent output directory
if(!file.exists(cur.comp.results)) {dir.create(cur.comp.results)} # Current output directory


#* Scenario-related results 
simple.scen.folder <- file.path(simple.model, cur.scenario)
bi.scen.folder     <- file.path(bi.model, cur.scenario)


#* Define starting year and end year
yearStart <- 2010                # Base year
yearEnd   <- 2100                # Last year for which population is projected
steps     <- yearEnd - yearStart # each projection step can be thought of as the resulting year (e.g., projection step 1 projects values for year 1 using input for year 0)


#####################################################
#*** Derive national-level changes based on SSPs ***#
#####################################################

# Create the two dataframes holding net migration values per model
for (region in regUAll){
  state.name <- substr(region, nchar(region)-1, nchar(region))
  
  # Read the net migration csv files and load them as dataframes
  # Simple model
  simple.mig.csv <- file.path(simple.scen.folder, region, paste0(region, "_proj_mig.csv"))
  simple.mig.df  <- read.csv(simple.mig.csv, stringsAsFactors = F, check.names = F)
  
  # Bilateral model
  bi.mig.csv <- file.path(bi.scen.folder, region, paste0(region, "_proj_net_mig.csv"))
  bi.mig.df  <- read.csv(bi.mig.csv, stringsAsFactors = F, check.names = F)
  
  # Sum the total net migration values per year
  sum.net.sim.mig <- unname(apply(simple.mig.df[, seq(5, ncol(simple.mig.df))], 2, sum))
  sum.net.bi.mig  <- unname(apply(bi.mig.df[, seq(5, ncol(bi.mig.df))], 2, sum))
  
  # Create a dataframe that holds net migration values for the two models and all years
  Year        <- rep(seq(2010, 2100), times = 2)
  model       <- factor(rep(c("Simple", "Bilateral"), each = steps + 1))
  sum.mig.df  <- data.frame(Year, c(sum.net.sim.mig, sum.net.bi.mig), model)
  colnames(sum.mig.df) <- c("Year", "Migration", "Model")
  
  # Create the comparison plot  
  cur.plot <- ggplot(data = sum.mig.df) +
                geom_line(aes(x = Year, y = Migration / 100, col = Model), size = 2) +
                labs(x = "Year", 
                     y = "Net Migration / 100",
                title = paste("Net Migration Values over Time for", state.name)) +
                scale_x_continuous(breaks = seq(2010, 2100, by = 10)) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
                theme_bw() +
                theme(plot.title   = element_text(size = 25, face = "bold", hjust = 0.5), 
                      axis.text.x  = element_text(size = 18, angle = 45, margin = margin(15,0,0,0)), 
                      axis.text.y  = element_text(size = 18),
                      axis.title.x = element_text(size = 20),
                      axis.title.y = element_text(size = 20, margin = margin(0,10,0,0)),
                      legend.title =  element_blank(),
                      legend.text  = element_text(size = 15),
                      legend.position  = "bottom",
                      legend.spacing.x = unit(1.0, 'cm'),
                      plot.margin = margin(1, 0.8, 1, 0.8, "cm"),
                      panel.grid.major = element_blank(),
                      axis.line = element_line(colour = "black")) 
  
  ggsave(filename = file.path(cur.comp.results, paste0(region, ".jpg")), plot = cur.plot, width = 12, height = 8)
}

  
