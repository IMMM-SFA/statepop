# Author:     Hamidreza Zoraghein
# Date:       2019-01-23
# Purpose:    Plots the adjustment factors applied to domestic migration rates in the simple model.


###############
#*** Paths ***#
###############
path <- "C:/Users/Hamidreza.Zoraghein/Desktop/Sensitivity_Analysis"
simple.model <- file.path(path, "Simple")   # Path to results of the simple model 


#######################
#*** Load packages ***#
#######################
# Various packages
library(ggplot2)
library(reshape2)


#############################
#*** Set general options ***#
#############################
options("scipen"=100, "digits"=4) # to force R not to use scientific notations
#options(warn=2)  # treat warnings as errors


###################################
#*** Declare general variables ***#
###################################

#* Specify scenario
scenUAll     <- c("Reg_Scenario", "Zero_Int_Mig", "Half_Scenario", "Doub_Scenario", "SSP2", "SSP3", "SSP5")
cur.scenario <- "SSP5"


#* Scenario-related results 
simple.scen.folder <- file.path(simple.model, cur.scenario)
simple.scen.csv    <- file.path(simple.scen.folder, "state_adj_factors.csv")


#* Define starting year and end year
yearStart <- 2010                # Base year
yearEnd   <- 2100                # Last year for which population is projected
steps     <- yearEnd - yearStart # each projection step can be thought of as the resulting year (e.g., projection step 1 projects values for year 1 using input for year 0)


###################
#*** Main Body ***#
###################

# Read the csv containing the adjustment factors according to the current scenario
adj.scen.df  <- read.csv(simple.scen.csv, stringsAsFactors = F, check.names = F)

# Create two empty vectors to hold the adjustment factors later
inc.factors  <- numeric(0)
dec.factors  <- numeric(0)

# Read the adjustment factors per year
years <- seq(yearStart, yearEnd)
for (year in years){
  cur.inc.factor <- sort(as.numeric(names(sort(table(as.numeric(adj.scen.df[adj.scen.df[, 1] == year, -1])), decreasing = TRUE)[1:2])), decreasing = TRUE)[1]
  cur.dec.factor <- sort(as.numeric(names(sort(table(as.numeric(adj.scen.df[adj.scen.df[, 1] == year, -1])), decreasing = TRUE)[1:2])), decreasing = TRUE)[2]
  inc.factors <- c(inc.factors, cur.inc.factor)
  dec.factors <- c(dec.factors, cur.dec.factor)
}

# Create the final dataftame that has years along with their corresponding adjustment factors
sum.adj.factors           <- data.frame(years, inc.factors, dec.factors)
colnames(sum.adj.factors) <- c("Year", "Inc_Factor", "Dec_factor")

# Prepare the dataframe for plotting
fin.adj.factors <- melt(sum.adj.factors, id.vars = "Year", variable.name = "Factor_Type", value.name = "Factor_Value")


# Create the plot  
cur.plot <- ggplot(data = fin.adj.factors[fin.adj.factors$Year %in% seq(yearStart, yearEnd, 5),]) +
  geom_point(aes(x = Year, y = Factor_Value, col = Factor_Type), size = 4) +
  labs(x = "Year", 
       y = "Factor",
       color = "Factor Type",
       title = paste("Adjustment Factors to Domestic Net Migration Rates for", cur.scenario)) +
  scale_x_continuous(breaks = seq(2010, 2100, by = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(labels = c("Increasing Factor", "Decreasing Factor"), values = c("red", "blue")) +
  theme_bw() +
  theme(plot.title   = element_text(size = 16, face = "bold", hjust = 0.5), 
        axis.text.x  = element_text(size = 14, angle = 45, margin = margin(10,0,0,0)), 
        axis.text.y  = element_text(size = 14),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15, margin = margin(0,10,0,0)),
        plot.margin = margin(1, 0.8, 1, 0.8, "cm"),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black")) 

ggsave(filename = file.path(simple.scen.folder, paste0("Adjustment_Factors_", cur.scenario, ".jpg")), plot = cur.plot, width = 12, height = 8)



