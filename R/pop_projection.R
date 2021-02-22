#' Function to project population migration totals
#'
#' @param inputsPath     Input folder containing state subdirectories
#' @param regUAll     Array containing state subfolder names
#' @param yearStart       Start year for data processing
#' @param yearEnd         Through year for data processing
#' @param int.mig         International migration rate (boolean, False = not applied)
#' @param cur.scenario    Current scenario rate (constant rate, etc.)
#' @param scen.factor     Target population scenario
#' @param num.ages        Come back to fix
#' @importFrom multistate f.linIntE f.ltm f.lBx
#' @importFrom Matrix bdiag
#' @param generate.output Optional, supply file path+name to generate output csv.
#' @export
pop.projection <- function(inputsPath, regUAll, yearStart=2010, yearEnd=2100, scen.factor=0, num.ages=101,
                           int.mig=0, cur.scenario="Constant_rate", gen.output=NULL){

  # Set variables
  steps <- yearEnd - yearStart

  # Path to standard life tables (used for linear interpolation of lx values)
  datMortS30S  <- "C:\\Users\\deci859\\PycharmProjects\\IM3\\statepop\\statepop\\inputs\\AllRegions_mortality_UNe030.csv"
  datMortS100S <- "C:\\Users\\deci859\\PycharmProjects\\IM3\\statepop\\statepop\\inputs\\AllRegions_mortality_UNe0100.csv"

  #* The dataframe that contains updated population based on international and state-level migrations at each step
  upd.pop <- as.data.frame(matrix(0, nrow = num.ages * 4, ncol = length(regUAll)))
  colnames(upd.pop) <- regUAll

  # Initialize a dataframe for holding international migration for all years and states
  tot.int.mig           <- data.frame(matrix(NA, nrow = 4*num.ages*(steps+1), ncol = length(regUAll)))
  colnames(tot.int.mig) <- regUAll

  # This dataframe holds population values before applying domestic migration. It is necessary to disaggregate
  # domestic migration to/from each state across all other states
  tot.pop.no.dom    <- NULL
  # These dataframes will hold population projections and net/in/out state-level migrations for all states and years
  tot.projection    <- NULL
  tot.state.net.mig <- NULL
  tot.state.in.mig  <- NULL
  tot.state.out.mig <- NULL

  # Import some basepop data
  upd.all.base.pop <- basepop(inputsPath=inputsPath, regUAll=regUAll, yearStart=yearStart, yearEnd=yearEnd,
                              num.ages=num.ages, cur.scenario=cur.scenario, scen.factor=scen.factor,
                              int.mig=int.mig, gen.output=gen.output)

  # Import fertility data
  tot.fert = fertility(inputsPath=inputsPath, regUAll=regUAll, yearStart=yearStart, yearEnd=yearEnd,
                       cur.scenario=cur.scenario, gen.output=gen.output)

  # Import mortality data
  tot.dfmx = mortality(inputsPath=inputsPath, regUAll=regUAll, datMortS30S=datMortS30S, datMortS100S=datMortS100S,
                       yearStart=yearStart, yearEnd=yearEnd, cur.scenario=cur.scenario, gen.output=gen.output)

  # Loop over regions to update their base year population with international and state-level migrations
  for (regU in 1:length(regUAll)){
    #* Generate paths
    pathIn    <- file.path(inputsPath, regUAll[regU])  # Input data directory

    #* Base Population data
    datBPS <- "basePop.csv"          # file containing baseline population
    datBP  <- read.csv(file.path(pathIn, datBPS), check.names = F, stringsAsFactors = F)
    bpAge <- "age"                      # Age categories (numeric, starting with 0)
    bpFR  <- "rural_female"             # Rural female population
    bpFU  <- "urban_female"             # Urban female population
    bpMR  <- "rural_male"               # Rural male population
    bpMU  <- "urban_male"               # Urban male population

    # Update the base year population with the international migration
    fBP   <- c(rbind(datBP[1:num.ages, bpFU], datBP[1:num.ages, bpFR])) # females -> urban0, rural0, urban1, rural1,...
    mBP   <- c(rbind(datBP[1:num.ages, bpMU], datBP[1:num.ages, bpMR])) # males
    matBP <- as.matrix(c(fBP,mBP))                                      # combines female and male pieces

    }

  # The outer loop that goes through years
  for (t in 1:steps){

    #* Initialize results matrix for the current year
    colN    <- c("age", "female", "urban") # generate vector of column names
    matProj <- as.data.frame(matrix(nrow = nrow(upd.all.base.pop), ncol = length(colN), dimnames = list(NULL, colN)))

    #* Add initial values to results matrix
    matProj[, "age"]         <- rep(c(rbind(datBP[, bpAge], datBP[,bpAge])), times=2) # age groups
    matProj[, "female"]      <- rep(c(1, 0), each = nrow(matProj) / 2) # gender
    matProj[, "urban"]       <- rep(c(1, 0), times = nrow(matProj) / 2) # urban vs. rural
    matProj[is.na(matProj)]  <- 0


    for (regU in 1:length(regUAll)){

      # Data preparation for Projection of the current state

      #*** Declare required variables

      #* Generate paths
      pathIn      <- file.path(inputsPath, regUAll[regU])  # Input data directory

      #* Scenario data
      scenarioS   <- paste0(cur.scenario, ".csv")
      scenario    <- read.csv(file.path(pathIn, scenarioS), check.names = F, stringsAsFactors = F)  # Input data directory

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

      # Modify the constant rate scenario international migration assumptions if current scenario is something else
      if (cur.scenario != "Constant_rate"){

        # Retrive the national-level changes of net international migration according to the current scenario
        net.int.mig.f.rate <- cumprod(scenario[, "Int_Mig_change"])
        net.int.mig.m.rate <- cumprod(scenario[, "Int_Mig_change"])

        # Apply the national-level rates to the state-level net inernational migration assumptions
        nMigFt <- net.int.mig.f.rate[1:(length(net.int.mig.f.rate) - 1)] * nMigFt[1]
        nmdf1F <- data.frame(year = 0:90, nm = c(nMigFt[1], nMigFt))
        nMigMt <- net.int.mig.m.rate[1:(length(net.int.mig.m.rate) - 1)] * nMigMt[1]
        nmdf1M <- data.frame(year = 0:90, nm = c(nMigMt[1], nMigMt))

      } else {

        # Linearly interpolate 1-year total net international migration counts between 5-year intervals
        nmdf1F <- f.linIntE(nMigFt, "nm", si = T, steps=steps)
        nmdf1M <- f.linIntE(nMigMt, "nm", si = T, steps=steps)
      }

      # Spread migrant numbers according to profile
      if (int.mig == 1){
        datNetMig[, "nmUF"] <- datNetMig[, netMigF] * nmdf1F[nmdf1F[, "year"] == t, "nm"]   # urban females
        datNetMig[, "nmRF"] <- datNetMig[, netMigF] * nmdf1F[nmdf1F[, "year"] == t, "nm"]   # rural females
        datNetMig[, "nmUM"] <- datNetMig[, netMigM] * nmdf1M[nmdf1M[, "year"] == t, "nm"]   # urban males
        datNetMig[, "nmRM"] <- datNetMig[, netMigM] * nmdf1M[nmdf1M[, "year"] == t, "nm"]   # rural males
      } else {
        datNetMig[, "nmUF"] <- datNetMig[, netMigF] * 0   # urban females
        datNetMig[, "nmRF"] <- datNetMig[, netMigF] * 0   # rural females
        datNetMig[, "nmUM"] <- datNetMig[, netMigM] * 0   # urban males
        datNetMig[, "nmRM"] <- datNetMig[, netMigM] * 0   # rural males
      }

      #* Domestic urban/rural migration data
      #* Urban/rural migration data in the base year (it's currently assumes that there is no urban/rural migration)
      datDomMigS  <- "domMig.csv" # file containing domestic migration rates
      datDomMig   <- read.csv(file.path(pathIn, datDomMigS), check.names=F, stringsAsFactors=F)

      # Variables
      domMigAge <- "age"                    # Age variable (numeric, starting with 0)
      ruMigF    <- "R-U_female"             # Female rural to urban migration
      ruMigM    <- "R-U_male"               # Male rural to urban migration
      urMigF    <- "U-R_female"             # Female urban to rural migration
      urMigM    <- "U-R_male"               # Male urban to rural migration

      # Rename variables
      names(datDomMig)[names(datDomMig) == ruMigF] <- "dmFRU0" # females rural to urban
      names(datDomMig)[names(datDomMig) == urMigF] <- "dmFUR0" # females urban to rural
      names(datDomMig)[names(datDomMig) == ruMigM] <- "dmMRU0" # males rural to urban
      names(datDomMig)[names(datDomMig) == urMigM] <- "dmMUR0" # males urban to rural

      #Currently we assume no urban/rural migration
      datDomMig[is.na(datDomMig)] <- 0

      #* Sex ratio at birth according to the scenario
      # Note: Number of males born for every 100 females (e.g., 1.08 for 108 males born for 100 females) for every 5-year interval
      srR <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd),"sr_R"]) # rural sex ratio
      srU <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd),"sr_U"]) # urban sex ratio

      #* Linearly interpolate sex ratios between 5-year time steps at 1-year interval according to the scenario
      # Note: This generates a data frame with a "year" and "sr" column
      srDf1R <- f.linIntE(srR, "sr", si=T, steps=steps) # rural
      srDf1U <- f.linIntE(srU, "sr", si=T, steps=steps) # urban

      #* Compute Proportion male and females at birth according to the scenario
      # Rural
      srDf1R$propM <- (srDf1R$sr / 100) / (1 + (srDf1R$sr / 100))
      srDf1R$propF <- 1 - srDf1R$propM
      # Urban
      srDf1U$propM <- (srDf1U$sr / 100) / (1 + (srDf1U$sr / 100))
      srDf1U$propF <- 1 - srDf1U$propM

      #* Compute matrix representation of mortality variables for the current state
      # Note: function writes relevant variables to global environment
      dfmx <- tot.dfmx[seq(1 + (regU - 1) * num.ages, regU * num.ages), ]
      mlF  <- f.ltm(dfmx, datDomMig, t, "F") # Females
      mlM  <- f.ltm(dfmx, datDomMig, t, "M") # Males

      #* Compute matrix representation of Birth rates (Bx)
      # Note: the female Sx values are used for the Bx computation for both males and females
      datFert <- tot.fert[seq(1 + (regU - 1) * num.ages, regU * num.ages), ]
      mBxF    <- f.lBx(datFert, srDf1R, srDf1U, mlF$Lx, mlF$lx, mlF$Sx, t, "F")  # females
      mBxM    <- f.lBx(datFert, srDf1R, srDf1U, mlM$Lx, mlM$lx, mlF$Sx, t, "M")  # males

      #* Generate transition matrix
      # Diagonal matrix pieces of Survival
      # Note: for the matrix computation step of the projection it is necessary that the transition matrix is quadratic
      #       in this case it is necessary to add two "0" value columns to account for the merge of the Bx and Sx matrices
      #       use bdiag() function from Matrix package to construct a block diagonal matrix
      mSxFd <- cbind(as.matrix(bdiag(mlF$Sx)), 0, 0) # females
      mSxMd <- cbind(as.matrix(bdiag(mlM$Sx)), 0, 0) # males

      # Horizontal matrix pieces of Birth
      mBxFd <- cbind(do.call(cbind, mBxF), 0, 0)     # females
      mBxMd <- cbind(do.call(cbind, mBxM), 0, 0)     # males

      # Generate quadrant pieces of transition matrix
      mat11 <- rbind(mBxFd, mSxFd)                                                             # female births + female survival
      mat12 <- matrix(0, nrow = nrow(mat11), ncol = ncol(mat11))                               # no values
      mat21 <- rbind(mBxMd, matrix(0, nrow = (nrow(mat11) - nrow(mBxMd)), ncol = ncol(mat11))) # male births -> the new born boys are now computed based on the population of reproductive females
      mat22 <- rbind(matrix(0, nrow = (nrow(mat11) - nrow(mSxMd)), ncol = ncol(mat11)), mSxMd) # male survival

      # Combine quadrant pieces to complete matrix
      matTs                     <- rbind(cbind(mat11, mat12), cbind(mat21, mat22))
      matTs[is.na(matTs)]       <- 0       # recode all "NaN" values to 0
      matTs[is.infinite(matTs)] <- 0       # recodes all Inf and -Inf values to 0


      #* Project population
      # Note: For matrix multiplication A*B the number of columns in A must be identical to
      #       the rows in B and the resulting matrix will have the number of raws of A
      #       and the number of columns of B

      # For the first year, the transition matrix is applied to the base year population updated by international and state-level migration
      if (t == 1){
        matProj[, paste0("pop_", regUAll[regU], "_", t + yearStart)] <- matTs%*%as.matrix(upd.all.base.pop[, regUAll[regU]])

        # For the other years the transition matrix is applied to the projected population of the previous year updated by international and state-level migration
      } else {
        matProj[, paste0("pop_", regUAll[regU], "_", t + yearStart)] <- matTs%*%as.matrix(upd.pop[, regUAll[regU]])

      }

      # The projected population of the current year has already been saved in matProj

      # This step updates population based on international and state-level migrations for the next year

      # Update the population with the international migration
      matBP <- as.matrix(matProj[, paste0("pop_", regUAll[regU], "_", t + yearStart)])  # combines female and male pieces

      # Update the projected population with the international migration
      int.mig.vec <- c(rbind(datNetMig[1:num.ages, "nmUF"], datNetMig[1:num.ages, "nmRF"]),
                       rbind(datNetMig[1:num.ages, "nmUM"], datNetMig[1:num.ages, "nmRM"]))

      matBP_upd   <- matBP + int.mig.vec
      matBP_upd[seq(2, nrow(matBP_upd), 2)] <- 0

      # Populate the data frames that store base-year population and its update population by international migration
      upd.pop[, regUAll[regU]] <- matBP_upd
      upd.pop[is.na(upd.pop)]  <- 0

      # Keep the international migration for the current state
      int.mig.vec[seq(2, length(int.mig.vec), 2)] <- 0
      tot.int.mig[((4*t*num.ages)+1):(4*(t+1)*num.ages), regUAll[regU]] <- int.mig.vec

    }
  }

    # Now that one year projection for all states has finished, state-level migration can be applied
    if (cur.scenario != "Constant_rate"){
      # State-level migration values are adjusted based on a factor that can be temporally variable
      # SSP2: The factor is 1 and constant
      # SSP3: The factor reduces from 1 to 0.5 gradually
      # SSP5: The factor increases fron 1 to 2 until the year with maximum international migration and them remains constant

      dom.mig.factor <- scenario.table[, "Dom_Mig_Factor"]

      # Calculate the total in out and net migration numbers for all states
      in.migration  <- f.in.dom.mig.calc(inputsPath, upd.pop, dom.mig.factor[t+1])
      out.migration <- f.out.dom.mig.calc(inputsPath, upd.pop, dom.mig.factor[t+1])
      net.migration <- in.migration - out.migration # People entered a state minus those who left

    } else {
      # The factor is constant based on the mechanical scenario (half, reg, double)
      # Calculate the total in out and net migration numbers for all states
      in.migration  <- f.in.dom.mig.calc(inputsPath, upd.pop, scen.factor)
      out.migration <- f.out.dom.mig.calc(inputsPath, upd.pop, scen.factor)
      net.migration <- in.migration - out.migration # People entered a state minus those who left
    }

    # Store the population before applying the domestic migration
    tot.pop.no.dom <- rbind(tot.pop.no.dom, upd.pop)

    # Update the projected population with the state-level migration, this will be used for the next year
    upd.pop <- upd.pop + net.migration

    # Add the results of the current year for all states to the dataframe that contains population for all years (tot.projection)
    colnames(matProj) <- c("age", "female", "urban", regUAll)
    tot.projection    <- rbind(tot.projection, matProj)

    # Add net/in/out migration values
    tot.state.in.mig  <- rbind(tot.state.in.mig, in.migration)
    tot.state.out.mig <- rbind(tot.state.out.mig, out.migration)
    tot.state.net.mig <- rbind(tot.state.net.mig, net.migration)

  if (!is.null(gen.output)) {
    # Write the total baseline population table to a csv file
    write.csv(tot.state.net.mig, gen.output, row.names = FALSE)
  }

  return(tot.projection)
}
