#' Function to generate baseline population matrix.
#'
#' @param pathIn      Input folder containing state subdirectories
#' @param scenario    Target population scenario
#' @param regUAll     Array containing state subfolder names
#' @return            DataFrame containing baseline population matrix
#' @export
#'
f.basepop <- function(pathIn, scenario, regUAll)

#############################################
#*** Generate baseline population matrix ***#
#############################################

# Loop over regions to update their base year population with international and state-level migrations
for (regU in 1:length(regUAll)){

  #* Generate paths
  pathIn    <- file.path(inputsPath, regUAll[regU])  # Input data directory

  #* Scenario data (The Constant_rate scenario. )
  scenarioS <- paste0(scenUAll[1], ".csv")
  scenario  <- read.csv(file.path(pathIn, scenarioS), check.names = F, stringsAsFactors = F)  # Input data directory

  #* Base Population data
  datBPS <- "basePop.csv"          # file containing baseline population
  datBP  <- read.csv(file.path(pathIn, datBPS), check.names = F, stringsAsFactors = F)

  # Variables
  bpAge <- "age"                      # Age categories (numeric, starting with 0)
  bpFR  <- "rural_female"             # Rural female population
  bpFU  <- "urban_female"             # Urban female population
  bpMR  <- "rural_male"               # Rural male population
  bpMU  <- "urban_male"               # Urban male population

  #* International migration data
  # Note: The international migration rates input data needs to be in relative rates (summing up to 1)
  datNetMigS  <- "intMig.csv"           # file containing international migration rates
  datNetMig   <- read.csv(file.path(pathIn, datNetMigS), check.names = F, stringsAsFactors = F)

  # Variables
  netMigAge  <- "age"                   # Age variable
  netMigF    <- "net_female"            # Proportion of net migration for each age group for females
  netMigM    <- "net_male"              # Proportion of net migration for each age group for males

  # Annual total net international migration counts for 5-year periods
  nMigFt     <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd), "nim_F"]) # female net international migrants
  nMigMt     <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd), "nim_M"]) # male net international migrants

  # Update the base year population with the international migration
  fBP   <- c(rbind(datBP[1:num.ages, bpFU], datBP[1:num.ages, bpFR])) # females -> urban0, rural0, urban1, rural1,...
  mBP   <- c(rbind(datBP[1:num.ages, bpMU], datBP[1:num.ages, bpMR])) # males
  matBP <- as.matrix(c(fBP,mBP))                                      # combines female and male pieces


  # Spread migrant numbers according to profile
  if (int.mig == 1){
    datNetMig[, "nmUF"] <- datNetMig[, netMigF] * nMigFt[1]   # urban females
    datNetMig[, "nmRF"] <- datNetMig[, netMigF] * nMigFt[1]   # rural females
    datNetMig[, "nmUM"] <- datNetMig[, netMigM] * nMigMt[1]   # urban males
    datNetMig[, "nmRM"] <- datNetMig[, netMigM] * nMigMt[1]   # rural males
  } else {
    datNetMig[, "nmUF"] <- datNetMig[, netMigF] * 0   # urban females
    datNetMig[, "nmRF"] <- datNetMig[, netMigF] * 0   # rural females
    datNetMig[, "nmUM"] <- datNetMig[, netMigM] * 0   # urban males
    datNetMig[, "nmRM"] <- datNetMig[, netMigM] * 0   # rural males
  }


  # Update the base year population with the international migration rates
  matBP_upd <- matBP + c(rbind(datNetMig[1:num.ages, "nmUF"], datNetMig[1:num.ages, "nmRF"]),
                         rbind(datNetMig[1:num.ages, "nmUM"], datNetMig[1:num.ages, "nmRM"]))


  # Populate the data frames that store base-year population and its updated population by international migration
  ini.all.base.pop[, regUAll[regU]]         <- matBP
  upd.all.base.pop[, regUAll[regU]]         <- matBP_upd
  ini.all.base.pop[is.na(ini.all.base.pop)] <- 0
  upd.all.base.pop[is.na(upd.all.base.pop)] <- 0

  # Keep the international migration for the current state
  cur.int.mig                     <- matBP_upd - matBP
  cur.int.mig[is.na(cur.int.mig)] <- 0
  tot.int.mig[0:(4*num.ages), regUAll[regU]] <- cur.int.mig
}

# Calculate the total in out and net migration numbers for all states
in.migration  <- f.in.dom.mig.calc(inputsPath, upd.all.base.pop, scen.factor)
out.migration <- f.out.dom.mig.calc(inputsPath, upd.all.base.pop, scen.factor)
net.migration <- in.migration - out.migration # People entered a state minus those who left

# Store the population before applying the domestic migration
tot.pop.no.dom <- rbind(tot.pop.no.dom, upd.all.base.pop)

#Update the base year population with the state-level migration
upd.all.base.pop   <- upd.all.base.pop + net.migration

# Store net/in/out migration values in the base year
tot.state.in.mig  <- rbind(tot.state.in.mig, in.migration)
tot.state.out.mig <- rbind(tot.state.out.mig, out.migration)
tot.state.net.mig <- rbind(tot.state.net.mig, net.migration)

return(tot.state.net.mig)
