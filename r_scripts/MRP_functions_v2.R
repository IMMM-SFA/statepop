# Author:     Raphael Nawrotzki
# Date:       2014-11-20
# Purpose:    Collection of functions used in multiregional projection (MRP) model

#*** Linear interpolation
# df = data frame
# x = variable in df that contains val
# y = variable in df for which value should be interpolated
# val = value on x for which a corresponding value of y should be found
# returns = y value that corresponds to x val
#
f.lInt=function(df,x,y,xval){
  
  # Limite df to important variables
  df=df[,c(y,x)]
  
  # Find two values on x closest to val
  df$diff=abs(df[,x]-xval) #generates difference measure
  pick=sort(df[df$diff%in%sort(df$diff)[1:2],x]) #picks the two x value closest to val
  
  # Define distance measures
  A1=pick[2]-pick[1] #difference between two x values
  A2=xval-pick[1] #difference between smalles x value and val
  B1=df[df[,x]==pick[2],y]-df[df[,x]==pick[1],y] #difference between respective y values
  
  # Linear interpolation y value
  # Note: A1/A2=B1/B2 -> B2=B1*A2/A1
  B2=(B1*A2/A1)+df[df[,x]==pick[1],y]
  
  return(B2)
}

#*** Function to perfomr linear interpolation of rates/values
# Note: Uses 5-year intervals to compute values for 1-year intervals
#       Uses the f.lInt function
# x = vector of values
# valN = name of variable (str) to hold 1-year values
# si = should the first value be used for the start of the first interval (e.g., year=0) -> TRUE; or at the center of the first interval (e.g., year=2) -> FALSE
# Example: x=e0EFR; valN="e0"; si=T
#
f.linIntE=function(x,valN,si){
  
  #* Generate 5-year df
  if(si==T){
    df5=data.frame(year=seq(0,(length(x)*5)-1,5),stringsAsFactors=F)    # scenario value assigned at the start of the 5-year interval (e.g., year 0 for interval 0-4)
  }else if (si==F){
    df5=data.frame(year=seq(2,(length(x)*5)-3,5),stringsAsFactors=F)    # scenario value assigned at the middle of the 5-year interval (e.g., year 2 for interval 0-4)
  }
  df5[,valN]=x # adds values to df
  
  #* Generate 1-year df
  df1=data.frame(year=0:steps,stringsAsFactors=F)
  df1[,valN]=NA #generates empty vector to hold interpolated values
  
  # Perform interpolation
  for(y in 1:nrow(df1)){
    # y=1
    df1[y,valN]=f.lInt(df=df5,x="year",y=valN,xval=df1$year[y])
  }
  return(df1)
}

#*** Linear interpolation of columns
# df = data frame containing x1 and x2
# x1 = variable containing the x values at level y1
# x2 = variable containing the x values at level y2
# y1 = level of y for x1 (number)
# y2 = level of y for x2 (number)
# yi = level of y for which x should be interpolated (number)
# returns: vector containing the imputed values of xi
# Example: df=df; x1=paste0("lx",g,"0"); x2=paste0("lx",g,"e100"); y1=e0s; y2=e0e100; yi=e0t
#
f.lIntC <- function(df, x1, x2, y1, y2, yi){
  # Note: (x2-x1)/(xi-x1)=(y2-y1)/(yi-y1) |solve for xi
  #       xi=x1+(x2-x1)*((yi-y1)/(y2-y1))
  
  # Limit df to important variables
  df <- df[, c(x1, x2)]
  
  # Generate scaler consisting of y values
  scaler <- (yi - y1) / (y2 - y1)
  
  # Compute xi values
  xi <- df[, x1] + (df[, x2] - df[, x1]) * scaler
  
  return(xi)
}

#*** Compute mx value last age category (e.g., 100+)
# Note: based on user specified life expectancy at age 0 for the base year
# g = specify gender (str); "F" or "FR" or "FU" = female (general, rural, urban); "M" or "MR" or "MU" = male (general, rural, urban)
# e0u = life expectancy at age zero (numeric)
# Example: g="FR"; e0u=72.72
#
f.mxMax <- function(g, e0u){
  if(g == "F" | g == "FR" | g == "FU"){
    if(e0u <= 70){
      mm <- -0.0000018*e0u^3 + 0.0003520*e0u^2 - 0.023099*e0u + 1.0650
    }else if(e0u > 70){
      mm <- 0.00000029*e0u^3 - 0.0218*e0u + 2.0386
    }
  } else if(g == "M"|g == "MR"|g == "MU"){
    if(e0u <= 70){
      mm <- -0.0000021*e0u^3 + 0.0003771*e0u^2 - 0.023437*e0u + 1.11422
    }else if(e0u > 70){
      mm <- 0.00000037*e0u^3 - 0.025077*e0u + 2.2893914
    }
  }
  return(mm)
}

#*** Compute e0 (life expectancy at age zero) values from lx values of the observed mortality dataframe
# Note: This function depends on the f.lifeTab function and uses the same inputs
# df = data frame containing lx values
# lx = variable (str) containing lx values in 100,000 people
# age = variable (str) containing age information
# g = specify gender (str); "F" or "FR" or "FU" = female (general, rural, urban); "M" or "MR" or "MU" = male (general, rural, urban); only needed for mx max computation
# e0u = life expectancy at age zero e0 (numeric)
# Example: df=datMort; lx="lxFR0"; age=mAge; g="FR"; e0u=62.96
#
f.e0 <- function(df, lx, age, g, e0u){
  
  # Restrict data to needed variables
  df <- df[,c(age,lx)]
  
  #* Percentage of survivals
  df$ax <- c(0.33, rep(0.5, nrow(df) - 1))
  
  #* Deaths per age group (dx)
  # Note: dx=lx0-lx1;
  df[, "dx"] <- NA
  df[1:(nrow(df)-1), "dx"] <- df[1:(nrow(df)-1), lx] - df[2:nrow(df), lx] 
  
  #* Exposure to death per person year (Lx)
  # Note: Lx=(lx1*1)+(dx0*0.5); similar to Lx=0.33*(lx0+lx1)
  df[,"Lx"] <- NA
  df[1:(nrow(df) - 1), "Lx"] <- (df[2:nrow(df), lx] * 1) + (df[1:(nrow(df) - 1), "dx"] * df[1:(nrow(df) - 1), "ax"])
  
  #* Mortality rate (mx); the percentage of individuals in a certain age group that died in the last year
  # Note: mx=dx/Lx
  df[, "mx"] <- df[, "dx"] / df[, "Lx"]
  
  #* Compute mx for last age category (e.g., 100+)
  # Note: based on user specified life expectancy at age 0 for the base year
  mm   <- f.mxMax(g, e0u)             # compute mx max value
  maxy <- max(df[, age])              # obtain maximum age group
  df[df[, age] == maxy, "mx"] <- mm  # assign mx max value
  
  #* Compute Lx value for oldest age category (Lxmax) 
  # Note: Lxmax=lxmax/mxmax
  df[df[, age] == maxy, "Lx"] <- df[df[, age] == maxy, lx] / mm
  
  #* Cumulative person years of survival
  # Note: Cumulative Lx from oldest to youngest age group
  df[, "Tx"] <- rev(cumsum(rev(df[, "Lx"])))
  
  #* Generate proportion of individuals that survived to the next age group (Sx)
  # Note: Sx=Lx1/Lx0; Smax-1=S99=T100+/T99; Smax=0
  df[, "Sx"] <- NA
  df[1:(nrow(df) - 1), "Sx"] <- df[2:nrow(df), "Lx"] / df[1:(nrow(df) - 1), "Lx"]
  maxy1 <- max(df[, age]) - 1 # obtain second to last year
  df[df[, age] == maxy1, "Sx"] <- df[df[, age] == maxy, "Tx"] / df[df[, age] == maxy1, "Tx"]
  df[df[, age] == maxy, "Sx"]  <- 0 # assign 0 value for Sx value of last age group because no one survived
  
  #* Compute life expectancy by age (ex)
  # Note: ex=Tx/lx
  df[,"ex"] <- df[, "Tx"] / df[, lx]
  
  #* Obtain life expectancy at birth (e0)
  e0result <- df[df[, age] == 0, "ex"]
  
  return(e0result)
}

#*** Replace IDs
# Note: faster version than repLab; use when dfRep is much larger than dfLabs and dfRep; 
#       allows for multiple matches
# dfLabs = data frame containing labels
# dfRep = data frame containing the variable to be relabeled
# varO = variable in dfLabs containing the original ID
# varN = variable in dfLabs containing the new ID
# varR = variable in dfRep containing the original ID that should be replaced with the new ID
# Example: dfLabs=regID; dfRep=regMig; varO="regID2"; varN="regID1"; varR="origiamreg"
#
f.repID=function(dfLabs,dfRep,varO,varN,varR){
  for(i in 1:nrow(dfLabs)){
    # i=1
    
    # Find matches of first value in varO in varR
    mat=grep(paste0("^",dfLabs[i,varO],"$"),dfRep[,varR])
    
    # Replace match
    if(any(mat)){
      dfRep[mat,varR]=dfLabs[i,varN]
    }
  }
  return(dfRep)
}

#*** Moving average
# Note: Assumes that data frame is sorted correctly
# df = data frame
# var = variable for which moving average should be computed
# vart = time variable that uniquely identifies a time step within each group (same length)
# ws =  window size (default=3); must be odd number (integer)
# Example: df=nmdat; var="nmr_total"; vart="agecat1"; ws=5
f.movAv=function(df,var,vart,ws=3){
  
  # Declare variable containing center cell
  df$cent=df[,var]
  
  mi=min(df[,vart]) # min of time variable
  mx=max(df[,vart]) # max of time variable
  
  if(ws==3){
    
    # Lag and lead variables
    df$lead1=c(NA,df[1:(nrow(df)-1),var]) # lead by 1 cell
    df[df$agecat1==mi,"lead1"]=NA         # assigns value that was lead from the prior case a NA value (avoids case overlap)
    
    df$lag1=c(df[2:nrow(df),var],NA)    # lag by 1 cell
    df[df$agecat1==mx,"lag1"]=NA        # assigns value that was lagged from the following case a NA value (avoids case overlap)
    
    # Recompute input variable
    df[,var]=rowMeans(df[,c("cent","lead1","lag1")],na.rm=T)
    
    # Remove temporary computing variables
    df=df[,!names(df)%in%c("cent","lead1","lag1")]
    
  }else if(ws==5){
    
    # Lag and lead variables
    df$lead1=c(NA,df[1:(nrow(df)-1),var])       # lead by 1 cell
    df[df$agecat1==mi,"lead1"]=NA               # assigns value that was lead from the prior case a NA value (avoids case overlap)
    df$lead2=c(NA,NA,df[1:(nrow(df)-2),var])    # lead by 2 cells
    df[df$agecat1%in%c(mi:(mi+1)),"lead2"]=NA   # assigns value that was lead from the prior case a NA value (avoids case overlap)
    
    df$lag1=c(df[2:nrow(df),var],NA)            # lag by 1 cell
    df[df$agecat1==mx,"lag1"]=NA                # assigns value that was lagged from the following case a NA value (avoids case overlap)
    df$lag2=c(df[3:nrow(df),var],NA,NA)         # lag by 2 cell
    df[df$agecat1%in%c((mx-1):mx),"lag2"]=NA    # assigns value that was lagged from the following case a NA value (avoids case overlap)
    
    # Recompute input variable
    df[,var]=rowMeans(df[,c("cent","lead1","lead2","lag1","lag2")],na.rm=T)
    
    # Remove temporary computing variables
    df=df[,!names(df)%in%c("cent","lead1","lead2","lag1","lag2")]
    
  }
  
  return(df)
}

#*** Function to compute mortality variables as matrices
# dfMort = Data Frame containing mortality variables in the form "mx"+gender+region+year (e.g., "mxFR2")
# dfDomMig = Data Frame containing variables on domestic (internal) migration propabilities in the form "dm"+gender+region+year (e.g., "dmMUR0")
# t = year for which mortality data is needed in the projection
# g = gender "F"=female; "M"=male
# Example: dfMort=dfmx; dfDomMig=datDomMig; t=10; g="M"
#
f.ltm=function(dfMort,dfDomMig,t,g){
  
  #* Initialize lists
  Mx=list()
  Zx=list()
  lx=list()
  Lx=list()
  Sx=list()
  
  #* Initialize matrix tools
  im=diag(2)          # identity matrix
  lm0=im*100000       # lx for age 0
  
  #* Generate matrices of Mx 
  # Note: Domestic migration rates do not change over time
  for(a in 1:nrow(dfMort)){
    # a=95
    Mx[[a]]=matrix(0,2,2)
    Mx[[a]][1,1]=dfMort[a,paste0("mx",g,"U",t-1)]+dfDomMig[a,paste0("dm",g,"UR",0)] # urban mortality + urban-to-rural migration
    Mx[[a]][2,2]=dfMort[a,paste0("mx",g,"R",t-1)]+dfDomMig[a,paste0("dm",g,"RU",0)] # rural mortality + rural-to-urban migration
  }
  
  #* Compute matrix Zx (survival rate)
  # Note: With I as the identity matrix, the computation of (I-Mg/2)/(I+Mg/2) is performed only for the diagonal cases (mortality rate + migration rate)
  #       The NaN values at positions [2,1] and [1,2] are replaced by the positive rural-to-urban and urban-to-rural migration rates
  for(a in 1:length(Mx)){
    # a=1
    Zx[[a]]=(im-(Mx[[a]]/2))/(im+(Mx[[a]]/2)) # regular division
    Zx[[a]][1,2]=dfDomMig[a,paste0("dm",g,"RU",0)] # rural-to-urban migration
    Zx[[a]][2,1]=dfDomMig[a,paste0("dm",g,"UR",0)] # urban-to-rural migration
  }
  
  #* Compute matrix lx
  # Note: lx+1=Zx*lx
  for(a in 1:length(Mx)){
    # a=1
    if(a==1){ # assign 100,000 to first diagonal matrix
      lx[[a]]=lm0
    }else if(a>1){
      lx[[a]]=Zx[[a-1]]%*%lx[[a-1]] 
    }
  }
  
  #* Compute matrix Lx
  # Note: Lx=(lx+1+lx)/2
  #       Matrix division is conducted by multiplying (%*%) by the inverse matrix (solve(Mat))
  for(a in 1:(length(Mx))){
    # a=1
    if(a<length(Mx)){
      Lx[[a]]=(lx[[a]]+lx[[a+1]])/2
    }else if(a==length(Mx)){ # for last age category: L100=l100/M100
      Lx[[a]]=lx[[a]]%*%solve(Mx[[a]])
    }
  }
  
  #* Compute matrix Sx
  # Note: Sx=Lx+1/Lx; Sx value for last age group (e.g., 100+) is undefined - but not needed for projection
  for(a in 1:(length(Mx)-1)){
    # a=1
    Sx[[a]]=Lx[[a+1]]%*%solve(Lx[[a]])
    
    # Set domestic migration to 0 after age 85
    if(a>85){
      Sx[[a]][2,1]=0 # urban-to-rural migration
      Sx[[a]][1,2]=0 # rural-to-urban migration
    }
  }
  
  # Sx max; Use the prior to last age group values (e.g., 98) for the last age group (e.g., 99) to avoid Sx values >1
  maxP=length(Sx) # obtain maximum position for Sx (e.g., 100 for age group 99)
  Sx[[maxP]]=Sx[[maxP-1]]
  
  
  #* Add relevant outputs to named list
  rl=list(lx=lx,Lx=Lx,Sx=Sx)
  
  return(rl)
}

#*** Function to compute matrix representation of birth rates (Bx)
# Note: Element 1 in list represents age group 0, Element 2 represents age group 1, etc.
# dfFert = Data Frame containing fertility variables in the form "tpFx"+region+step (e.g., "tpFxR2")
# dfSrR = Data frame containing sex ratios for rural area (variables: propM and propF)
# dfSrU = Data frame containing sex ratios for urban area (variables: propM and propF)
# Lx = nested list containing maxtrices of Lx values
# lx = nested list containing maxtrices of lx values
# SxF = nested list containing maxtrices of Sx values for females
# t = time step used in the projection
# g = gender "F"=female; "M"=male
# Example: dfFert=datFert; dfSrR=srDf1R; dfSrU=srDf1U; Lx=mlF$Lx; lx=mlF$lx; SxF=mlF$Sx; t=1; g="F"
#
f.lBx=function(dfFert,dfSrR,dfSrU,Lx,lx,SxF,t,g){
  # t=1
  
  # Initialize lists
  Fx=list()
  Bx=list()
  
  #* Generate matrix representation of fertility (Fx)
  for(a in 1:nrow(dfFert)){
    # a=1
    Fx[[a]]=matrix(0,2,2)
    Fx[[a]][1,1]=dfFert[a,paste0("tpFxU",t-1)]*dfSrU[dfSrU[,"year"]==t-1,paste0("prop",g)] # urban fertility * sex ratio
    Fx[[a]][2,2]=dfFert[a,paste0("tpFxR",t-1)]*dfSrR[dfSrR[,"year"]==t-1,paste0("prop",g)] # rural fertility * sex ratio
  }
  
  #* Compute matrix of Birth rates (Bx)
  # Note: Bx=L0/l0*0.5*(Fx+(Fx+1*SxF))
  for(a in 1:length(SxF)){
    # a=21
    Bx[[a]]=(Lx[[1]]%*%solve(lx[[1]]))*0.5*(Fx[[a]]+(Fx[[a+1]]%*%SxF[[a]]))
  }
  
  return(Bx)
}

#*** Compute birth rates
# g = gender "FR"=rural female; "FU"=urban female; "MR"=rural male; "MU"=urban male
# t = time step used in the projection
# Example: g="FR"; t=1
#
f.cbr=function(g,t){
  
  #* Compute correct fertility rates
  if(g=="FR"){
    Fx=datFert[,paste0("tpFxR",t-1)]*srDf1R[srDf1R[,"year"]==t-1,"propF"]  # females rural
  }else if(g=="FU"){
    Fx=datFert[,paste0("tpFxU",t-1)]*srDf1U[srDf1U[,"year"]==t-1,"propF"]  # females urban
  }else if(g=="MR"){
    Fx=datFert[,paste0("tpFxR",t-1)]*srDf1R[srDf1R[,"year"]==t-1,"propM"]  # males rural
  }else if(g=="MU"){
    Fx=datFert[,paste0("tpFxU",t-1)]*srDf1U[srDf1U[,"year"]==t-1,"propM"]  # males urban
  }
  
  Sx=dfSx[,paste0("Sx",g,t-1)]                                            # survival rate (Sx)
  L0=dfLx[dfLx[,"age"]==0,paste0("Lx",g,t-1)]
  l0=dflx[dflx[,"age"]==0,paste0("lx",g,t-1)]
  B1=0.5*(Fx[1:length(Fx)-1]+Fx[2:length(Fx)]*Sx[1:length(Sx)-1])         # pure births
  B2=L0/l0*0.5*(Fx[1:length(Fx)-1]+Fx[2:length(Fx)]*Sx[1:length(Sx)-1])   # births after mortality adjustment
  B1=c(B1,0) # add zero at end
  B2=c(B2,0) # add zero at end
  Bx=list(B1=B1,B2=B2)
  
  return(Bx)
}

#' @rdname f.gstack
#' @title Stack data to use for ggplot2
#' @description This function "stacks" the data for use in ggplot2.
#' It is recommended to reduce the variables in the df to those needed for the plotting.
#' @param df data frame with the variables to be stacked
#' @param id ID variable that is used (str)
#' @export
#' 
f.gstack=function(df,id){
  stacked=list()
  nameL=names(df)[!names(df)%in%id]
  stacked[[1]]=rep(df[,id], times=length(nameL))
  stacked[[2]]=rep(nameL,each=nrow(df))
  stacked[[3]]=unlist(c(df[,nameL]),use.names=F)
  names(stacked)=c(id,"variable","value")
  stacked=as.data.frame(stacked,row.names=NULL,stringsAsFactors=F)
  return(stacked)
}

#' @rdname f.multiplot
#' @title Multiple plot function
#' @description This function generates a multi-graph plot and saves it as a png or tiff file.
#' If the saving mode is activated the multi-graph plot will not show up in the graphing console.
#' @param saveF boolean; shall the graph be saved (TRUE) or displayed (FALSE)
#' @param  file path and file name to the directory where the multi-graph plot should be saved
#' @param  plots a list of ggplot objects (list)
#' @param  cols Number of columns in layout
#' @param  rowFill shall the matrix layout be filled by row? (default TRUE)
#' @param  layout A matrix specifying the layout (if present, 'cols' is ignored)
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @param  pt plot type (e.g., png, tiff); default=png
#' @param  w width of multi-graph plot (default = 20 cm)
#' @param  h height of multi-graph plot (default = 15 cm)
#' @param  u unit of multi-graph plot (default = "cm")
#' @param  r resolution of multi-graph plot (default = 300 ppi)
#' @export
#' 
f.multiplot=function(saveF=TRUE,file,plots,cols=1,rowFill=TRUE,layout=NULL, 
                     pt="png", w=20, h=15, u="cm", r=300){
  require(grid)
  numPlots=length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  numRows=ceiling(numPlots/cols)
  if(is.null(layout)){
    layout=matrix(seq(1, cols*numRows), ncol=cols, nrow=numRows, byrow=rowFill)
  }
  
  # Set up the page
  if(saveF){
    if(pt=="png"){
      png(file,width=w, height=h, units=u, res=r)
    }else if(pt=="tiff"){
      tiff(file,width=w, height=h, units=u, res=r)
    }
  } 
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow(layout), ncol(layout))))
  
  # Make each plot, in the correct location
  # Note: Gets the i,j matrix positions of the regions that contain this subplot
  for(i in 1:numPlots){
    matchidx=as.data.frame(which(layout==i, arr.ind=TRUE)) #arr.ind=T forces which to return both column and row numbers of the particular object
    print(plots[[i]], vp=viewport(layout.pos.row=matchidx$row,layout.pos.col=matchidx$col)) #prints the graph in the particular layout field
  }
  if(saveF) dev.off()
  return()
}

#*** Compute net migration population
# dfr=data frame containing the net migration rates
# Example: df=dfnm
#
f.cnmpop=function(df){
  
  for(i in 1:length(regUAll)){
    # i=1
    # cat(" ",regUAll[i])
    
    # Obtain population data
    matpop=get(paste0("matProj_",regUAll[i])) # obtain complete matrix
    yearpop=matpop[,paste0("pop_",t+yearStart)] # obtain year specific population projections
    
    # Obtain column names in df for origin "i"
    coln=names(df)[grep(paste0("^",regUAll[i],"-.*"),names(df))]
    
    # Compute net migration population for origin "i"
    # Note: multiplies the nmr by the origin population (at risk pop)
    df[,coln]=sapply(df[,coln],function(x) x*yearpop)
  }
  
  return(df)
}

#*** Compute emigration population (EM)
# df=data frame of net migration counts
# Example: df=dfnmp
#
f.cempop=function(df){
  
  # Generate template to be filled
  dfem=df[,c("age","female","urban")]
  
  # Loop over regions
  for(i in 1:length(regUAll)){
    # i=1
    # cat(" ",regUAll[i])
    
    # Obtain column names in df for origin "i"
    coln=names(df)[grep(paste0("^",regUAll[i],"-.*"),names(df))]
    
    # Obtain all emigrants that migrated for origin "i"
    t1=df[,c("age","female","urban",coln)]
    t1$em=rowSums(df[,coln])  # compute row sums
    dfem[,regUAll[i]]=t1$em   # assign summed value to region column
  }
  
  return(dfem)
}

#*** Compute immigration population (IM)
# df=data frame of net migration counts
# Example: df=dfnmp
#
f.cimpop=function(df){
  
  # Generate template to be filled
  dfim=df[,c("age","female","urban")]
  
  for(i in 1:length(regUAll)){
    # i=1
    # cat(" ",regUAll[i])
    
    # Obtain column names in df for destination "i"
    coln=names(df)[grep(paste0("^.*","-",regUAll[i],"$"),names(df))]
    
    # Obtain urbanization rate
    pu=urtab[urtab$year==t,names(urtab)==regUAll[i]] # proportion urban
    
    # Obtain all net migrants for destination "i"
    t1=df[,c("age","female","urban",coln)]
    t1$im=rowSums(df[,coln]) # Compute row sums
    
    # Sum rural and urban immigrants
    ag=aggregate(t1[,"im"],t1[c("age","female")],FUN="sum") 
    
    # Compute rural immigrants using urbanization rate
    agr=ag
    agr$im=agr$x*(1-pu) # compute rural migrants
    agr$urban=0 # assign "0" for rural
    
    # Compute urban immigrants using urbanization rate
    agu=ag
    agu$im=agr$x*pu # compute rural migrants
    agu$urban=1 # assign "1" for urban
    
    # Combine data sets
    agc=rbind(agu,agr)
    agc=agc[order(-agc$female,agc$age,-agc$urban),] # order data frame
    
    # assign summed value to region column
    dfim[,regUAll[i]]=agc$im 
  }
  
  return(dfim)
  
}


#*** Adjusts the net migration rates so that the total migration value across all states come close to 0
#
f.state.level.mig.adj <- function(mast.pop.df, mig.factor, input.path){
  
  # Define the initial parameters within the function
  num.ages <- 101
  regions  <- colnames(mast.pop.df)
  
  # The dataframe that will contain net migration values for all states
  mast.mig.df           <- as.data.frame(matrix(0, nrow = 4 * num.ages, ncol = 51))
  colnames(mast.mig.df) <- colnames(mast.pop.df)
  
  # The dataframe that will contain adjustment factors for all states
  adj.factors.df            <- as.data.frame(matrix(0, 1, ncol = 51))
  colnames(adj.factors.df)  <- colnames(mast.pop.df)
  
  # Retrieve the current state-level migration rates
  for (region in 1:length(regions)) {
    base.mig <- read.csv(file.path(input.path, regions[region], "stateMig.csv"), check.names = F, stringsAsFactors = F)
    mast.mig.df[, regions[region]] <- c(rbind(base.mig[1:num.ages, "net_female"], base.mig[1:num.ages, "net_female"]),
                                        rbind(base.mig[1:num.ages, "net_male"], base.mig[1:num.ages, "net_male"]))
  }
  
  # Apply the factor based on half scenario (0.5), regular scenario (1), double scenario (2) or no domestic migration scenario (0)
  mast.mig.df <- mast.mig.df * mig.factor
  
  # Calculate migration net numbers
  mast.net.df <- mast.pop.df * mast.mig.df 
  
  # Calculate the total net migration value, this value should get close to 0 by adjustment
  sum(mast.net.df)
  
  # At each time, adjustment is done by introducing 1 +/- 0.00005 
  new.step <- 0.00005
  i <- 1
  while (sum(mast.net.df) > 100 | sum(mast.net.df) < -100){
    
    # The adjustment is done for those states whose proportion of migration is lower than 5%
    # If the current total net migration is positive, decrease in-migration and increase out-migration 
    if (sum(mast.net.df) > 0){
      for (col in colnames(mast.net.df)){
        if ((sum(mast.net.df[, col]) >= 0) & (sum(abs(mast.net.df[, col])) / sum(abs(mast.pop.df[, col])) < 0.05)){
          # Apply the factor to current rates
          mast.mig.df[, col]     <- mast.mig.df[, col] * (1 - new.step)
          # Save the current adjustment factor
          adj.factors.df[, col]  <- (1 - new.step)^i
        } else if ((sum(mast.net.df[, col]) < 0) & (sum(abs(mast.net.df[, col])) / sum(abs(mast.pop.df[, col])) < 0.05)){
          mast.mig.df[, col]     <- mast.mig.df[, col] * (1 + new.step)
          adj.factors.df[, col]  <- (1 + new.step)^i
        }
      }
    } else if (sum(mast.net.df) < 0){
      # If the current total net migration is negative, increase in-migration and decrease out-migration 
      for (col in colnames(mast.net.df)){
        if ((sum(mast.net.df[, col]) >= 0) & (sum(abs(mast.net.df[, col])) / sum(abs(mast.pop.df[, col])) < 0.05)){
          mast.mig.df[, col]     <- mast.mig.df[, col] * (1 + new.step)
          adj.factors.df[, col]  <- (1 + new.step)^i
        } else if ((sum(mast.net.df[, col]) < 0) & (sum(abs(mast.net.df[, col])) / sum(abs(mast.pop.df[, col])) < 0.05)) {
          mast.mig.df[, col]     <- mast.mig.df[, col] * (1 - new.step)
          adj.factors.df[, col]  <- (1 - new.step)^i
        }
      }
    }
    
    mast.net.df <- mast.pop.df * mast.mig.df
    sum(mast.net.df)
    cat("\n ", sum(mast.net.df))
    i <- i + 1
  }
  
  # Output three datasets: 1-adjusted migration rates, resulting net migration values and adjustment factors
  output <- list(mig.rate = mast.mig.df, net.mig = mast.net.df, adj.fac = adj.factors.df)
  
  return(output)
  
}



f.in.dom.mig.calc <- function(input.path, pop.dataframe, scen.factor){
  
  # The final output dataframe that holds total in-migration numbers for all states
  tot.tot.in           <- data.frame(matrix(0, nrow = nrow(pop.dataframe), ncol = ncol(pop.dataframe)))
  colnames(tot.tot.in) <- colnames(pop.dataframe)
  
  states <- colnames(pop.dataframe)
  for (state in states){
    
    # Read the csv file holding in migration rates to the current state
    cur.in.path <- file.path(input.path, state, paste0(state, "_in_mig.csv"))
    cur.in.mig  <- read.csv(cur.in.path, stringsAsFactors = F, check.names = F)
    
    # Retrieve population of all states that contributed migration to the current one
    from.states  <- colnames(pop.dataframe)[which(colnames(pop.dataframe) != state)]
    cur.from.pop <- pop.dataframe[, from.states]
    
    # Create a dataframe to hold in migration rates of all states contributing to the current one
    cur.from.mig           <- data.frame(matrix(0, nrow = nrow(cur.from.pop), ncol = ncol(cur.from.pop)))
    colnames(cur.from.mig) <- colnames(cur.from.pop)
    
    # Populate the in migration dataframe with the in migration rates of all contributing states
    for (col in colnames(cur.from.mig)){
      
      # The current state that is contributing population to the focal state (state)
      cur.from.state  <- substr(col, nchar(col) - 1, nchar(col))
      
      # Female rates, rural is assumed 0
      cur.from.mig[seq(1, nrow(cur.from.pop) / 2 , 2), col] <- cur.in.mig[cur.in.mig$from == cur.from.state & cur.in.mig$gender == "f",
                                                                          ncol(cur.in.mig)]
      
      # Male rates, rural is assumed 0
      cur.from.mig[seq((nrow(cur.from.pop) * 0.5) + 1, nrow(cur.from.pop), 2), col] <- cur.in.mig[cur.in.mig$from == cur.from.state & cur.in.mig$gender == "m",
                                                                                                  ncol(cur.in.mig)]
    }
    
    # Multiplication gives us the in migration numbers from each contributing state
    cur.in.net <- (cur.from.mig * scen.factor) * cur.from.pop
    
    # Sum the total in migration numbers across all contributing states, now we have one total value per age group
    cur.tot.in <- data.frame(apply(cur.in.net, 1, sum, na.rm = T))
    
    # Add the values of the current focal state to the output dataframe
    tot.tot.in[, state] <- cur.tot.in
  }
  
  return(tot.tot.in)
} 


f.out.dom.mig.calc <- function(input.path, pop.dataframe, scen.factor){
  
  # The final output dataframe that holds total out-migration numbers for all states
  tot.tot.out           <- data.frame(matrix(0, nrow = nrow(pop.dataframe), ncol = ncol(pop.dataframe)))
  colnames(tot.tot.out)  <- colnames(pop.dataframe)
  
  states <- colnames(pop.dataframe)
  for (state in states){
    
    # Read the csv file holding out migration rates from the current state
    cur.out.path <- file.path(input.path, state, paste0(state, "_out_mig.csv"))
    cur.out.mig  <- read.csv(cur.out.path, stringsAsFactors = F, check.names = F)
    
    # Retrieve population of all states that received out-migration from the current one
    to.states  <- colnames(pop.dataframe)[which(colnames(pop.dataframe) != state)]
    cur.to.pop <- pop.dataframe[, state]
    
    # Create a dataframe to hold out migration rates of all states receiving from the current one
    cur.to.mig           <- data.frame(matrix(0, nrow = nrow(pop.dataframe), ncol = length(to.states)))
    colnames(cur.to.mig) <- to.states
    
    # Populate the out migration dataframe with the out migration rates of all receiving states
    for (col in colnames(cur.to.mig)){
      
      # The current state that is receiving population from the focal state (state)
      cur.to.state  <- substr(col, nchar(col) - 1, nchar(col))
      
      # Female rates, rural is assumed 0
      cur.to.mig[seq(1, nrow(pop.dataframe) / 2 , 2), col] <- cur.out.mig[cur.out.mig$to == cur.to.state & cur.out.mig$gender == "f",
                                                                          ncol(cur.out.mig)]
                                                                          
      # Male rates, rural is assumed 0
      cur.to.mig[seq((nrow(pop.dataframe) * 0.5) + 1, nrow(pop.dataframe), 2), col] <- cur.out.mig[cur.out.mig$to == cur.to.state & cur.out.mig$gender == "m",
                                                                                                   ncol(cur.out.mig)]
    }  
    
    # Multiplication gives us the out migration numbers to each receiving state
    cur.out.net <- (cur.to.mig * scen.factor) * cur.to.pop
    
    # Sum the total out migration numbers across all receiving states, now we have one total value per age group
    cur.tot.out <- data.frame(apply(cur.out.net, 1, sum, na.rm = T))
    
    # Add the values of the current focal state to the output dataframe
    tot.tot.out[, state] <- cur.tot.out
  }
  
  return(tot.tot.out)
} 


f.in.state.dom.mig.calc <- function(input.path, state, pop.dataframe, scen.factor){
  
  # Read the csv file holding in migration rates to the current state
  cur.in.path <- file.path(input.path, state, paste0(state, "_in_mig.csv"))
  cur.in.mig  <- read.csv(cur.in.path, stringsAsFactors = F, check.names = F)
  
  # Retrieve population of all states that contributed migration to the current one
  from.states  <- colnames(pop.dataframe)[which(colnames(pop.dataframe) != state)]
  cur.from.pop <- pop.dataframe[, from.states]
  
  # Create a dataframe to hold in migration rates of all states contributing to the current one
  cur.from.mig           <- data.frame(matrix(0, nrow = nrow(cur.from.pop), ncol = ncol(cur.from.pop)))
  colnames(cur.from.mig) <- colnames(cur.from.pop)
  
  # Populate the in migration dataframe with the in migration rates of all contributing states
  for (col in colnames(cur.from.mig)){
    
    # The current state that is contributing population to the focal state (state)
    cur.from.state  <- substr(col, nchar(col) - 1, nchar(col))
    
    # Female rates, rural is assumed 0
    cur.from.mig[seq(1, nrow(cur.from.pop) / 2 , 2), col] <- cur.in.mig[cur.in.mig$from == cur.from.state & cur.in.mig$gender == "f",
                                                                        ncol(cur.in.mig)]
    
    # Male rates, rural is assumed 0
    cur.from.mig[seq((nrow(cur.from.pop) * 0.5) + 1, nrow(cur.from.pop), 2), col] <- cur.in.mig[cur.in.mig$from == cur.from.state & cur.in.mig$gender == "m",
                                                                                                ncol(cur.in.mig)]
  }
  
  # Multiplication gives us the in migration numbers from each contributing state
  cur.in.net <- (cur.from.mig * scen.factor) * cur.from.pop
  
  return(cur.in.net)
} 


f.out.state.dom.mig.calc <- function(input.path, state, pop.dataframe, scen.factor){
  
  # Read the csv file holding out migration rates from the current state
  cur.out.path <- file.path(input.path, state, paste0(state, "_out_mig.csv"))
  cur.out.mig  <- read.csv(cur.out.path, stringsAsFactors = F, check.names = F)
  
  # Retrieve population of all states that received out-migration from the current one
  to.states  <- colnames(pop.dataframe)[which(colnames(pop.dataframe) != state)]
  cur.to.pop <- pop.dataframe[, state]
  
  # Create a dataframe to hold out migration rates of all states receiving from the current one
  cur.to.mig           <- data.frame(matrix(0, nrow = nrow(pop.dataframe), ncol = length(to.states)))
  colnames(cur.to.mig) <- to.states
  
  # Populate the out migration dataframe with the out migration rates of all receiving states
  for (col in colnames(cur.to.mig)){
    
    # The current state that is receiving population from the focal state (state)
    cur.to.state  <- substr(col, nchar(col) - 1, nchar(col))
    
    # Female rates, rural is assumed 0
    cur.to.mig[seq(1, nrow(pop.dataframe) / 2 , 2), col] <- cur.out.mig[cur.out.mig$to == cur.to.state & cur.out.mig$gender == "f",
                                                                        ncol(cur.out.mig)]
    
    # Male rates, rural is assumed 0
    cur.to.mig[seq((nrow(pop.dataframe) * 0.5) + 1, nrow(pop.dataframe), 2), col] <- cur.out.mig[cur.out.mig$to == cur.to.state & cur.out.mig$gender == "m",
                                                                                                 ncol(cur.out.mig)]
  }  
  
  # Multiplication gives us the out migration numbers to each receiving state
  cur.out.net <- (cur.to.mig * scen.factor) * cur.to.pop
  
  
  return(cur.out.net)
} 




