#' Function to generate mortality matrices.
#'
#' @param inputsPath     Input folder containing state subdirectories
#' @param regUAll     Array containing state subfolder names
#' @param cur.scenario    Current scenario rate (constant rate, etc.)
#' @param datMortS30S     UN Standard 30-year life table
#' @param datMortS100S    UN Standard 100-year life table
#' @param yearStart       Start year for data processing
#' @param yearEnd         Through year for data processing
#' @param gen.output Optional, supply file path+name to generate output csv.
#' @param vis             (Boolean) Should details for projection model adjustment be printed?
#' @importFrom multistate f.linIntE f.e0 f.lIntC f.mxMax
#' @return            Mortality matrix (1).
#' @export
mortality <- function(inputsPath, regUAll, datMortS30S, datMortS100S,
                      yearStart=2010, yearEnd=2100, cur.scenario="Constant_rate", gen.output=NULL, vis=F){

  steps <- yearEnd - yearStart
  datMortS100  <- read.csv(datMortS100S, check.names = F, stringsAsFactors = F)
  datMortS30   <- read.csv(datMortS30S, check.names = F, stringsAsFactors = F)

  # Initialize total matrices
  tot.dfdx <- NULL
  tot.dfLx <- NULL
  tot.dfmx <- NULL
  tot.dfTx <- NULL
  tot.dfSx <- NULL
  tot.dfex <- NULL

  for (regU in 1:length(regUAll)){

    #* Generate paths
    pathIn      <- file.path(inputsPath, regUAll[regU])  # Input data directory

    #* Scenario data
    scenarioS   <- paste0(cur.scenario, ".csv")
    scenario    <- read.csv(file.path(pathIn, scenarioS), check.names = F, stringsAsFactors = F)  # Input data directory

    #* Mortality data
    # Note: The variable names need to be the same in all three mortality data files
    datMortS    <- "mortality.csv"  # file containing mortality data for particular region for standard (preferable close to the base year)
    datMort     <- read.csv(file.path(pathIn, datMortS), check.names = F, stringsAsFactors = F)

    # Variables
    lxAge <- "age"                          # Age categories (numeric, starting with 0)
    lxFR  <- "lx_rural_female"              # Life expectancy rural females in 100000 people
    lxFU  <- "lx_urban_female"              # Life expectancy urban females in 100000 people
    lxMR  <- "lx_rural_male"                # Life expectancy rural male in 100000 people
    lxMU  <- "lx_urban_male"                # Life expectancy urban male in 100000 people

    #* Rename variables
    # Standard
    names(datMort)[names(datMort) == lxFR] <- "lxFRs" # rural females
    names(datMort)[names(datMort) == lxFU] <- "lxFUs" # urban females
    names(datMort)[names(datMort) == lxMR] <- "lxMRs" # rural males
    names(datMort)[names(datMort) == lxMU] <- "lxMUs" # urban males

    # UN e0=100 (life expectancy at age 0 is 100 years)
    names(datMortS100)[names(datMortS100) == lxFR] <- "lxFRs100" # rural females for e0=100
    names(datMortS100)[names(datMortS100) == lxFU] <- "lxFUs100" # urban females for e0=100
    names(datMortS100)[names(datMortS100) == lxMR] <- "lxMRs100" # rural males for e0=100
    names(datMortS100)[names(datMortS100) == lxMU] <- "lxMUs100" # urban males for e0=100

    # UN e0=30 (life expectancy at age 0 is 30 years)
    names(datMortS30)[names(datMortS30) == lxFR] <- "lxFRs30"    # rural females for e0=30
    names(datMortS30)[names(datMortS30) == lxFU] <- "lxFUs30"    # urban females for e0=30
    names(datMortS30)[names(datMortS30) == lxMR] <- "lxMRs30"    # rural males for e0=30
    names(datMortS30)[names(datMortS30) == lxMU] <- "lxMUs30"    # urban males for e0=30

    #* Prepare df
    datMort <- datMort[, c(lxAge, "lxFRs", "lxFUs", "lxMRs", "lxMUs")]            # Reduce df to relevant variables
    datMort <- merge(datMort, merge(datMortS30, datMortS100, by=lxAge), by=lxAge) # Merge UN e0=100 and UN e0=30 values into datMort


    #*** Compute lx values using linear interpolation
    #* Initialize data frames and variables
    dflx <- datMort                  # data frame to hold interpolated lx values
    e0n  <- "e0df1"                  # beginning of name (e.g., "e0df1") of tables of expected life expectancy values (e0) interpolated for each projection year
    gl   <- c("FU","FR","MU","MR")   # gender groups for which interpolation should be performed


    # Life expectancies at age zero (e0) for projection period according to the scenario
    # Note: Start with e0 of baseline year and provide expected e0 values for 5-year intervals; single year e0 values will be linearly interpolated
    e0EFU <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd),"m_UF"])      # urban females
    e0EFR <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd),"m_RF"])      # rural females
    e0EMU <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd),"m_UM"])      # urban males
    e0EMR <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd),"m_RM"])      # rural males


    # Modify the constant rate scenario mortality assumptions if current scenario is something else
    if (cur.scenario != 'Constant_rate'){

      # Retrive the national-level changes of female life expectancy according to the current scenario
      # Currently urban and rural are assumed equal
      urb.mor.f.rate <- cumprod(scenario.table[, "F_Mor_change"])
      rur.mor.f.rate <- cumprod(scenario.table[, "F_Mor_change"])

      # Retrive the national-level changes of male life expectancy according to the current scenario
      # Currently urban and rural are assumed equal
      urb.mor.m.rate <- cumprod(scenario.table[, "M_Mor_change"])
      rur.mor.m.rate <- cumprod(scenario.table[, "M_Mor_change"])

      # Apply the national-level rates to the state-level life expectancy assumptions
      # Female
      e0EFU   <- urb.mor.f.rate[1:(length(urb.mor.f.rate) - 1)] * e0EFU[1]
      e0df1FU <- data.frame(year = 0:90, e0 = c(e0EFU[1], e0EFU))
      e0EFR   <- rur.mor.f.rate[1:(length(rur.mor.f.rate) - 1)] * e0EFR[1]
      e0df1FR <- data.frame(year = 0:90, e0 = c(e0EFR[1], e0EFR))

      # Male
      e0EMU   <- urb.mor.m.rate[1:(length(urb.mor.m.rate) - 1)] * e0EMU[1]
      e0df1MU <- data.frame(year = 0:90, e0 = c(e0EMU[1], e0EMU))
      e0EMR   <- rur.mor.m.rate[1:(length(rur.mor.m.rate) - 1)] * e0EMR[1]
      e0df1MR <- data.frame(year = 0:90, e0 = c(e0EMR[1], e0EMR))

    } else {

      # Linearly interpolate 1-year life expectancies between 5-year intervals
      e0df1FU <- f.linIntE(e0EFU, "e0", si = T, steps=steps) # Urban females
      e0df1FR <- f.linIntE(e0EFR, "e0", si = T, steps=steps) # Rural females
      e0df1MU <- f.linIntE(e0EMU, "e0", si = T, steps=steps) # Urban males
      e0df1MR <- f.linIntE(e0EMR, "e0", si = T, steps=steps) # Rural males
    }


    #* Loop over time steps
    for(t in 0:steps){

      if(vis){cat("\n\nGenerating lx values for year:", t+yearStart)}

      #* Loop over gender groups
      for(g in 1:length(gl)){

        if(vis){cat("\nProcessing gender group:",gl[g],"; Iterations...")}

        #* Obtain e0 values
        e0df1 <- get(paste0(e0n, gl[g]))                                   # Obtain the table containing the e0 values at 1-year interval for the particular gender group according to the scenario
        e0t   <- e0df1[e0df1$year == t, "e0"]                              # e0 value for year t from scenario
        lxs   <- paste0("lx",gl[g], "s")                                   # gender and region specific lx value used as sandard
        e0s   <- f.e0(dflx, lxs, lxAge, gl[g], e0df1[e0df1$year==0, "e0"]) # e0 value from the mortality dataframe of the region

        #* Choose correct UN lx variable for interpolation
        if(e0t >= e0s)
        {                                             # when e0 for time t in scenario is larger than e0 from data for standard
          lxUN <- paste0("lx", gl[g], "s100")         # variable name of lx values to be used for interpolation
          e0UN <- f.e0(dflx, lxUN, lxAge, gl[g], 100) # e0=100 from UN data

        }else if(e0t < e0s){                          # when e0 for time t in scenario is smaller than e0 from data for standard; if a decline in e0 is assumed over time
          lxUN <- paste0("lx", gl[g], "s30")          # variable name of lx values to be used for interpolation
          e0UN <- f.e0(dflx, lxUN, lxAge, gl[g], 30)  # e0=30 from UN data
        }

        #* Linearily interpolate lx values based on e0 values
        dflx[, paste0("lx", gl[g], t)] <- f.lIntC(df = dflx, x1 = lxs, x2 = lxUN, y1 = e0s, y2 = e0UN, yi = e0t)

        # Compute e0tp from the newly modified observed lx values after standardizing based on the scenario
        e0tp  <- f.e0(dflx, paste0("lx", gl[g], t), lxAge, gl[g], e0t)
        ediff <- abs(e0tp - e0t)  # Compute difference between e0 (user specified) and e0 prime (from data)
        te    <- 0.01             # theshold of e0 (numeric) above which an iterative adjustment is performed

        #*** Perform iterative adjustment
        # Note: the scaler is adjusted incrementally until the e0 difference between the scenario e0 value and the e0 value from within the data is less than te
        if(ediff > te){
          count  <- 1       # Set counter
          e0tAdj <- e0t     # generate a copy of the e0t value that will be used/modified in the adjustment process

          # Perform while loop
          while(ediff > te){

            #* Specify the size of the increment by which e0tAdj is changed
            if(ediff >= 0.7){
              si <- 0.5
            }else if(ediff >= 0.2 & ediff < 0.7){
              si <- 0.1
            }else if(ediff >= 0.05 & ediff < 0.2){
              si <- 0.05
            }else if(ediff < 0.05){
              si <- 0.01
            }

            #* Adjust scaler value
            if(e0tp > e0t){         # When e0tp (generated data) is larger than e0t (scenario)
              e0tAdj <- e0tAdj - si
            }else if(e0tp < e0t){   # When e0tp (generated data) is smaller than e0t (scenario)
              e0tAdj <- e0tAdj + si
            }

            #* Linearily interpolate lx values based on updated e0 values
            dflx[, paste0("lx", gl[g], t)] <- f.lIntC(df = dflx, x1 = lxs, x2 = lxUN, y1 = e0s, y2 = e0UN, yi = e0tAdj)

            # Recompute e0tp from the newly generated lx values
            e0tp  <- f.e0(dflx, paste0("lx", gl[g], t), lxAge, gl[g], e0t)
            ediff <- abs(e0tp - e0t)  # Compute difference between e0 (user specified) and e0 prime (from data)

            #* Update counter
            if(vis){cat(count, " ")} # print counter
            count <- count+1
          }
        }
      }
    }


    # Returns a warning if negative values are produced in lx table
    if(any(dflx<0)) {warning("lx data frame contains negative values ", regUAll[regU]," ",scenUAll[scenU])}


    #*** Compute life table variables
    # Note: input lx values need to be provided in 100000 of people; variable lx in form "lxFU0"

    #* Initialize variables
    e0n <- "e0df1"                # beginning of name (e.g., "e0df1") of tables of expected life expectancy values (e0)
    gl  <- c("FU","FR","MU","MR") # list of gender groups for which computation should be performed (e.g., "FU","FR","MU","MR")

    #* Initialize data frames to hold results
    dfdx <- data.frame(age = dflx[,lxAge], stringsAsFactors = F)
    dfLx <- data.frame(age = dflx[,lxAge], ax = c(0.33, rep(0.5, nrow(dflx)-1)), stringsAsFactors = F) # ax= Percentage of survivals
    dfmx <- data.frame(age = dflx[,lxAge], stringsAsFactors = F)
    dfTx <- data.frame(age = dflx[,lxAge], stringsAsFactors = F)
    dfSx <- data.frame(age = dflx[,lxAge], stringsAsFactors = F)
    dfex <- data.frame(age = dflx[,lxAge], stringsAsFactors = F)

    #* Loop over time steps
    for(t in 0:steps){

      for(g in 1:length(gl)){

        #* Deaths per age group (dx)
        # Note: dx=lx0-lx1;
        dfdx[, paste0("dx", gl[g], t)]                 <- NA
        dfdx[1:(nrow(dfdx)-1), paste0("dx", gl[g], t)] <- dflx[1:(nrow(dflx)-1), paste0("lx", gl[g], t)] - dflx[2:nrow(dflx), paste0("lx", gl[g], t)]

        #* Exposure to death per person year (Lx)
        # Note: Lx=(lx1*1)+(dx0*0.5); similar to Lx=0.33*(lx0+lx1)
        dfLx[, paste0("Lx", gl[g], t)]                 <- NA
        dfLx[1:(nrow(dfLx)-1), paste0("Lx", gl[g], t)] <- (dflx[2:nrow(dflx), paste0("lx", gl[g], t)]*1) + (dfdx[1:(nrow(dfdx)-1), paste0("dx", gl[g], t)]*dfLx[1:(nrow(dfLx)-1), "ax"])

        #* Mortality rate (mx); the percentage of individuals in a certain age group that died in the last year
        # Note: mx=dx/Lx
        dfmx[, paste0("mx", gl[g], t)] <- dfdx[, paste0("dx", gl[g], t)] / dfLx[, paste0("Lx", gl[g], t)]

        #* Compute mx for last age category (e.g., 100+)
        # Note: based on user specified life expectancy at age 0 (for the base year??)
        e0df1                        <- get(paste0(e0n, gl[g]))                      # obtain the table containing the e0 values for the particular gender group
        mm                           <- f.mxMax(gl[g],e0df1[e0df1[,"year"]==t,"e0"]) # obtain mx max value
        maxy                         <- max(dfmx[,"age"])                            # obtain the max year
        dfmx[dfmx[, "age"] == maxy,
             paste0("mx", gl[g], t)] <- mm                                           # assign the mx value

        #* Compute Lx value for oldest age category (Lxmax)
        # Note: Lxmax=lxmax/mxmax
        dfLx[dfLx[, "age"] == maxy, paste0("Lx", gl[g], t)] <- dflx[dflx[, "age"]==maxy, paste0("lx", gl[g], t)]/mm

        #* Cumulative person years of survival
        # Note: Cumulative Lx from oldest to youngest age group
        dfTx[, paste0("Tx", gl[g], t)] <- rev(cumsum(rev(dfLx[, paste0("Lx", gl[g], t)])))

        #* Generate proportion of individuals that survived to the next age group (Sx)
        # Note: Sx=Lx1/Lx0; Smax-1=S99=T100+/T99; Smax=0
        dfSx[, paste0("Sx", gl[g], t)] <- NA
        dfSx[1:(nrow(dfSx)-1),
             paste0("Sx", gl[g], t)]   <- dfLx[2:(nrow(dfLx)), paste0("Lx", gl[g], t)] / dfLx[1:(nrow(dfLx)-1), paste0("Lx", gl[g], t)]
        maxy1                          <- max(dfSx[, "age"])-1 # prior to last year
        dfSx[dfSx[, "age"]==maxy1,
             paste0("Sx", gl[g], t)]   <- dfTx[dfTx[, "age"] == maxy, paste0("Tx", gl[g], t)] / dfTx[dfTx[, "age"] == maxy1, paste0("Tx", gl[g], t)]
        dfSx[dfSx[, "age"]==maxy,
             paste0("Sx", gl[g], t)]   <- 0  # assign 0 value for Sx value of last age group because no one survived

        #* Compute life expectancy by age (ex)
        # Note: ex=Tx/lx
        dfex[, paste0("ex", gl[g], t)] <- dfTx[, paste0("Tx", gl[g], t)] / dflx[, paste0("lx", gl[g], t)]
      }
    }
    # TODO: not currently in use:
    # Populate the total matrix with the current region
    tot.dfdx <- rbind(tot.dfdx, dfdx)  # insert comments about usage
    tot.dfLx <- rbind(tot.dfLx, dfLx)
    tot.dfmx <- rbind(tot.dfmx, dfmx)  # in use
    tot.dfTx <- rbind(tot.dfTx, dfTx)
    tot.dfSx <- rbind(tot.dfSx, dfSx)
    tot.dfex <- rbind(tot.dfex, dfex)
  }

  if (!is.null(gen.output)) {
    # Write the total mortality rate (% per age over last year) table to a csv file
    write.csv(tot.dfmx, gen.output, row.names = FALSE)
  }

  # we don't ever use the above matrices again (except for tot.dfmx)
  return(tot.dfmx)
}
