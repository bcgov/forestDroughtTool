#' summarise and classify asmr
#'
#' This function will summarise monthly asmr values and classes for current and future periods.
#'
#' The function uses output from asmrRun():
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
#'asmrSumm(PrinceGeorge,bgc="SBSmk1")
#'

asmrSumm<-function(stnData,bgc, soils=soilsData,future=TRUE,years="random",lat=55){

 # Run asmr
  asmr1<-asmrRun(stnData,soils,future,bgc,years,lat)

  # STEP 1: CURRENT DATA
    # Summarise monthly data
    # Include an annual row at the bottom
    curr.asmr<-asmr1$current %>%
      group_by(month) %>%
      summarise_at(vars(ends_with(".ASMR")),mean) %>%
      ungroup() %>%
      rbind(c(13,colMeans(.)[-1])) %>%
      mutate(month=cut(month,breaks=13,labels=c(month.abb,"Annual"))) %>%
      mutate(Scenario="current") %>%
      mutate(period="normal") %>%
      dplyr::select(Scenario,period,everything())

    # STEP 2: FUTURE DATA

    # Define function to compile future data
    future.comp<-function(x) {

      x %>%
        group_by(Scenario, period, month) %>%
        summarise_at(vars(ends_with(".ASMR")),mean) %>%
        ungroup() %>%
        rbind(data.frame(Scenario=x$Scenario[1],period=x$period[1],month=13,summarise_at(x,vars(ends_with(".ASMR")),mean))) %>%
        mutate(month=cut(month,breaks=13,labels=c(month.abb,"Annual"))) %>%
        return()
      }


    # Apply functions to future data
    if(isTRUE(future)) {

      asmrCompiled<-
        sapply(names(asmr1$future),function(x) future.comp(asmr1$future[[x]]),USE.NAMES=TRUE,simplify=FALSE) %>%
        bind_rows(.id=NULL) %>%
        rbind(curr.asmr)

    } else {asmrCompiled=curr.asmr}

  # Add in BGC

    asmrCompiled%<>%
      mutate(bgc=bgc) %>%
      dplyr::select(bgc,everything())


    # Cut table into classes
    # Define function
    asmrCut<-function(x) cut(x,breaks=c(0,asmrClass$asmrUL),labels=asmrClass$class)

    asmrCompClass<-
    asmrCompiled %>%
      mutate_at(vars(ends_with(".ASMR")),asmrCut)

  return(list(asmr=asmrCompiled,class=asmrCompClass,year=asmr1$year))

}
