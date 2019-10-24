#' run current and future ASMR
#'
#' This function will generate daily asmr values for current and future periods.
#'
#' The function uses the following approach:
#'
#'
#'Input:
#'@param stnData climate station data
#'@param soils default or user-defined soils data
#'@param bgc corresponding BGC variant for stnData (e.g., "SBSmk1)
#'@param future TRUE/FALSE if TRUE(default), then future scenarios are run for bgc
#'@param years either "random" or custom vector of years, as integers
#'@param lat latitude in degrees, either 50,55 or 60
#'@import dplyr tidyr magrittr stringr
#'@keywords internal
#'@examples
#'asmrRun(PrinceGeorge,bgc="SBSmk1")

asmrRun=function(stnData,soils=soilsData,future=TRUE,bgc, years="random",lat=55) {

  # STEP 1: select years for analysis
  # If user specieis that years should be selected at random:
  if (years=="random") {

    clim<-stnData[stnData$year%in%yearSelect(stnData),]

  } else {

    # else use the years vector they provide
    clim<-stnData[stnData$year%in%years,]

    }

  # STEP 2: run asmrCalc() for current period
  asmrCurrent=asmrCalc(clim,soils,latitude=lat)


  # STEP 3 if future is true, calculate asmr for future periods/scenarios
  if(isTRUE(future)) {

    futureClim<-asmrFuture(clim,bgc)

    asmrFut<-lapply(names(futureClim),function(x) asmrCalc(futureClim[[x]],soils,latitude=lat))
    names(asmrFut)<-names(futureClim)


  } else {

    asmrFut=NULL

  }


  # FINAL OUTPUT
  return(list(current=asmrCurrent,future=asmrFut,year=unique(clim$year)))


}

