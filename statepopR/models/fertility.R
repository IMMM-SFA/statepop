#' Function to generate fertility matrix.
#'
#' @param pathIn          Input folder containing state subdirectories
#' @param cur.scenario    Target population scenario
#' @param regUAll         Array containing state subfolder names
#' @param gen.output      Optional, supply file path+name to generate output csv.
#' @importFrom multistate f.linIntE f.lInt
#' @return                DataFrame containing fertility matrix
#' @export
#'
fertility <- function(pathIn, regUAll, cur.scenario="Constant_rate", gen.output=NULL){

  #Initialize the overall fertility table
  tot.fert <- NULL

  for (regU in 1:length(regUAll)){

    cat(paste("\nFertility for", regUAll[regU]))

    #* Generate paths
    pathIn      <- file.path(inputsPath, regUAll[regU])  # Input data directory

    #* Scenario data
    scenarioS   <- paste0(cur.scenario, ".csv")
    scenario    <- read.csv(file.path(pathIn, scenarioS), check.names = F, stringsAsFactors = F)  # Input data directory

    #* Fertility data in the base year
    datFertS    <- "fertility.csv"         # file containing fertility data
    datFert     <- read.csv(file.path(pathIn, datFertS), check.names = F, stringsAsFactors = F)

    # Variables
    fAge  <- "age"                      # Variable for ages (numeric, starting with 0)
    asfrR <- "rural"                    # Rural population ASFR (age specific fertility rate)
    asfrU <- "urban"                    # Urban population ASFR (age specific fertility rate)

    #* Rename age specific fertility rate variables to tpFx
    # Note: tpFx = proportional TFR; sums up to TFR; this measure feeds into Leslie matrix
    names(datFert)[names(datFert) == asfrR] <- "tpFxRs"   # rural
    names(datFert)[names(datFert) == asfrU] <- "tpFxUs"   # urban

    #Replicate urban fertility rates with rural
    datFert["tpFxRs"] <- datFert["tpFxUs"]

    # Expected total fertility rate according to the scenario
    # Note: TFR in 5-year intervals
    tfrER <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd),"f_R"]) # rural total fertility rates
    tfrEU <- as.numeric(scenario[scenario$year%in%c(yearStart:yearEnd),"f_U"]) # urban total fertility rates

    # Modify the constant rate scenario fertility assumptions if current scenario is something else
    if (cur.scenario != scenUAll[1]){

      # Retrive the national-level changes of tfr according to the current scenario
      # Currently urban and rural are assumed equal
      net.fer.r.rate <- cumprod(scenario.table[, "TFR_change"])
      net.fer.u.rate <- cumprod(scenario.table[, "TFR_change"])

      # Apply the national-level rates to the state-level tfr assumptions
      # Currently urban and rural are assumed equal
      tfrDf1R <- net.fer.r.rate[1:(length(net.fer.r.rate) - 1)] * tfrER[1]
      tfrDf1R <- data.frame(year = 0:90, tfr = c(tfrER[1], tfrDf1R))
      tfrDf1U <- net.fer.u.rate[1:(length(net.fer.u.rate) - 1)] * tfrEU[1]
      tfrDf1U <- data.frame(year = 0:90, tfr = c(tfrEU[1], tfrDf1U))


    } else {

      #* Linearily interpolate expected TFR values between 5-year time steps for 1-year intervals and according to the scenario
      # Note: This generates a data frame with a "year" and "tfr" column
      tfrDf1R <- f.linIntE(tfrER, "tfr", si=T) # rural; si=T first value is starting value; si=F last value is starting value
      tfrDf1U <- f.linIntE(tfrEU, "tfr", si=T) # urban
    }


    #* Compute fertility variables for standard
    rt <- c("R","U") # "R" = rural; "U" = urban
    for(r in 1:length(rt)){

      #* Compute tfr for standard
      tfrS <- sum(datFert[, paste0("tpFx", rt[r], "s")], na.rm=T)

      #* Compute pFx from age specific fertility rate
      # Note: pFx = Proportion fertility for each age group relative to all age groups; summing up to 1
      datFert[, paste0("pFx", rt[r], "s")] <- datFert[, paste0("tpFx", rt[r], "s")] / tfrS

      #* Compute cpFx
      # Note: cpFx = cumulated proportion fertility up to exact age x -> sum of age-specific fertility rates up to age x
      datFert[, paste0("cpFx", rt[r], "s")] <- cumsum(datFert[, paste0("pFx", rt[r], "s")])

      #* Compute tcpFx
      # Note: tcpFx = cumulative proportion of TFR for Standard up to exact age x -> sum of tfr up to age x
      datFert[, paste0("tcpFx", rt[r], "s")] <- datFert[, paste0("cpFx", rt[r], "s")]*tfrS
    }

    #* Compute IQR (Inter Quartile Range) for standard
    # Note: Exact ages for the 75%ile and 25%ile are derived using linear interpolation
    iqrAgeRS <- f.lInt(datFert, "cpFxRs", fAge, 0.75) - f.lInt(datFert, "cpFxRs", fAge, 0.25) # rural
    iqrAgeUS <- f.lInt(datFert, "cpFxUs", fAge, 0.75) - f.lInt(datFert, "cpFxUs", fAge, 0.25) # urban

    #* Compute Median Age of standard
    medAgeRS <- f.lInt(datFert, "cpFxRs", fAge, 0.50) # rural
    medAgeUS <- f.lInt(datFert, "cpFxUs", fAge, 0.50) # urban

    #*** Compute future fertility schedules by standardizing them according to the scenario
    # Note: User defines whether to use the Brass Relational model or a simple scaling approach
    rt <- c("R","U")     # "R" = rural; "U" = urban

    if(useBrassf){ # uses the Brass Relational Gompertz model to compute future fertility schedules

      #* Loop over years for which fertility data is needed
      for(t in 0:steps){
        # t=0
        if(vis){cat(t + yearStart," ")}

        #* Loop over region types (rural/urban)
        for(r in 1:(length(rt))){
          # r=1

          #* Obtain df of 1-year TFR values from scenario
          tfrDf1 <- get(paste0("tfrDf1", rt[r]))

          #* Declare TFR values used in fertility schedule computation
          tfrS <- sum(datFert[, paste0("tpFx", rt[r], "s")], na.rm = T)      # standard
          tfrF <- tfrDf1[tfrDf1[, "year"]==t, "tfr"]                     # future year

          #* Compute IQR of Future year (t)
          # Note: Formula is based on empirically derived relationship between TFR and IQR
          iqrAgeF <- 6.1 + 0.466*tfrF + 0.136*tfrF^2

          #* Compute beta value
          # Note: When beta=1 then there is no change; the smaller the beta the more dispersed the curve of the schedule; beta ranges from 0.65 to 1.5 (Zeng et al., 2000)
          iqrAgeS <- get(paste0("iqrAge", rt[r], "S")) # obtain prior computed IQR of standard for either rural or urban
          beta    <- iqrAgeS / iqrAgeF

          #* Compute Median Age of Future year (t)
          # Note: Formula is based on empirically derived relationship between TFR and Median Age
          if(tfrF <= 4.5){
            medAgeF <- 30.8-4.64*tfrF + 1.05*tfrF^2
          }else{
            medAgeF <- 31.18 # when tfr of future year is larger than 4.5 we fix the median age at 31.18 years
          }

          #* Linearly interpolate tcpFx value of Standard for Median Age of Future year
          tcpFxMedAge <- f.lInt(datFert, fAge, paste0("tcpFx", rt[r], "s"), medAgeF)

          #* Compute alpha value
          # Note: the smaller the alpha the later the fertility process; normal alpha rage -0.5 to 0.5 (Zeng et al., 2000)
          alpha <- -log(-log(0.5)) - beta*(-log(-log(tcpFxMedAge / tfrS)))

          #* Compute YHxT of Future
          # Note: YHxT is computed by using alpha and beta values together with the Standard (see Equation 4 in Zeng et al. (2000))
          datFert[, paste0("yHxT", rt[r], t)] <- alpha + beta*(-log(-log(round(datFert[, paste0("tcpFx", rt[r], "s")]/tfrS, 7)))) # Note: rounding to the 7th decimal point is necessary to avoid introduction of NA values because R introduces random numbers after the 15 decimal point which cause problems with the -log-log transformation

          #* Compute cpFx for Future
          # Note: cpFx = cumulative proportional fertility by age group
          datFert[, paste0("cpFx", rt[r], t)] <- exp(-exp(-datFert[, paste0("yHxT", rt[r], t)]))

          #* Compute tcpFx for Future
          # Note: tcpFx = cumulative proportion of TFR by age group
          datFert[, paste0("tcpFx", rt[r], t)] <- tfrF*datFert[, paste0("cpFx", rt[r], t)]

          #* Compute tpFx for Future (fx=H(x)-H(x-1))
          # Note: tpFx = proportion of TFR by age group; the tpFx values are used in the Leslie matrix
          datFert[, paste0("tpFx", rt[r], t)] <- c(0, datFert[2:nrow(datFert), paste0("tcpFx", rt[r], t)] - datFert[1:(nrow(datFert)-1), paste0("tcpFx", rt[r], t)])

          #*** Apply middle year value
          # Note: This step is necessary only if the underlying fertility data comes from 5-year age category data
          df5y <- datFert[datFert[, fAge] == 21, paste0("pFx", rt[r], "s")] == datFert[datFert[, fAge] == 22, paste0("pFx", rt[r], "s")] # tests similarity between age 21 and 22 of standard
          if(df5y){

            # Add variable that reflects middle age for each five-year group
            fmaS    <- seq(2, max(datFert[, fAge], na.rm = T), 5)                       # sequence of middle ages
            fmaSE   <- rep(fmaS, each = 5)                                              # sequence of middle age years
            fmaSmax <- rep(max(datFert[, fAge], na.rm = T), times = (nrow(datFert) - length(fmaSE))) # sequence of max age alues for rows that don't belong to a full 5-age group (e.g., age 100)
            datFert[, "ageM"] <- c(fmaSE, fmaSmax)

            # Obtain middle age fertility value
            fma <- datFert[datFert[, fAge]%in%unique(datFert[, "ageM"]), c(fAge, paste0("tpFx", rt[r], t))]

            # Apply middle age fertility value
            datFert <- datFert[, !names(datFert)%in%paste0("tpFx", rt[r], t)] # remove original variable
            datFert <- merge(datFert, fma, by.x = "ageM", by.y = fAge)        # merge in new variable
            datFert <- datFert[order(datFert[, fAge]),]                       # order datFert

            # Adjust estimates to meet target tfr
            # Note: this step is needed because the middle age assignment distorts the tfr value
            tfrDat <- sum(datFert[, paste0("tpFx", rt[r], t)])                # compute data specific tfr after conducting middle age assignment
            tfrDiffRate <- tfrF / tfrDat                                      # compute difference rate
            datFert[, paste0("tpFx", rt[r], t)] <- datFert[, paste0("tpFx", rt[r], t)]*tfrDiffRate # ajust fertility rates by difference rate
          }
        }
      }
    } else { # Uses simple scaling of the standard fertility schedule

      #* Loop over years for which fertility data is needed
      for(t in 0:steps){
        # t=0
        if(vis){cat(t + yearStart," ")}

        #* Loop over region types (rural/urban)
        for(r in 1:(length(rt))){
          # r=1

          #* Obtain df of 1-year TFR values from scenario
          tfrDf1 <- get(paste0("tfrDf1", rt[r]))

          #* Declare TFR values used in fertility schedule computation
          tfrS <- sum(datFert[, paste0("tpFx", rt[r], "s")])  # standard
          tfrF <- tfrDf1[tfrDf1[, "year"] == t, "tfr"]        # future year

          # Adjust estimates to meet target tfr
          tfrDiffRate <- tfrF / tfrS                                                               # compute difference rate
          datFert[, paste0("tpFx", rt[r], t)] <- datFert[, paste0("tpFx", rt[r], "s")]*tfrDiffRate # ajust fertility rates by difference rate
        }
      }
    }

    # Reduce to needed variables
    datFert <- datFert[, c(fAge, paste0(rep(c("tpFxU", "tpFxR"), each = steps + 1), 0:steps))]

    tot.fert <- rbind(tot.fert, datFert)
  }

  if (!is.null(gen.output)) {
    # Write the total fertility table to a csv file
    write.csv(tot.fert, gen.output, row.names = FALSE)
  }

  return(tot.fert)
}
