#' Calculate future ASMR
#'
#' This function will adjust observed daily data for future periods.
#'
#' The function uses the following approach:
#'
#' We use the normal (1961-1990) monthly data from ClimateBC (see ?bgcClim) to establish a baseline climate
#' for a given BGC unit.  The difference between normal period data and future data is calculated, and then
#' and the difference, expressed in percentage for precipitation and degrees for temperatures, is then applied to the daily data for a given climate station.
#'
#' Future scenarios are calculated for three scenarios:
#' \itemize{
#'   \item RCP 4.5
#'   \item RCP 8.5
#'   \item Mean of RCP 4.5 and 8.5
#' }
#'
#' and three time periods:
#' \itemize{
#'   \item 2025
#'   \item 2055
#'   \item 2085
#' }
#'
#'Input:
#'@param stnData climate station data
#'@param bgc corresponding BGC variant for stnData (e.g., "SBSmk1)
#' @import dplyr tidyr magrittr stringr
#' @export
#' @examples
#'futureASMR(PrinceGeorge,"SBSmk1")
#'

futureASMR<-function(stnData,bgc) {

  # Step 1: Prepare future climate data

  # Create a 4.5/8.5 mean calculation
  x<-

    # Filter for BGC unit
    dplyr::filter(bgcClim,BGC==bgc) %>%
    group_by(period) %>%
    summarize_if(is.numeric,mean) %>%
    .[-1,] %>%
    ungroup() %>%
    mutate(BGC=bgc,Scenario="MeanRCP") %>%
    dplyr::select(BGC,period,Scenario,everything()) %>%
    rbind(filter(bgcClim,BGC==bgc))

  # Determine temperature changes for each future scenario
  futureTemp <-
    x %>%
    select(starts_with("Tmax"),starts_with("Tmin")) %>%
    apply(2,function(y) y-y[x$period%in%"1961 - 1990"]) %>%
    as_tibble() %>%
    mutate(Scenario=x$Scenario,period=x$period) %>%
    dplyr::select(period,Scenario,everything()) %>%
    filter(as.numeric(period)>2000)

  # Determine precipitation changes for each future scenario
  # and join with futureTemp
  futureClim <-
    x %>%
        select(starts_with("PPT")) %>%
        apply(2,function(y) y/y[x$period%in%"1961 - 1990"]) %>%
        as_tibble() %>%
        mutate(Scenario=x$Scenario,period=x$period) %>%
        mutate(id=paste(Scenario,period,sep=".")) %>%
        dplyr::select(id,period,Scenario,everything()) %>%
        filter(as.numeric(period)>2000) %>%
        inner_join(futureTemp,by=c("period","Scenario")) %>%

  # Formatting
      pivot_longer(cols=PPT01:Tmin12,names_to="var") %>%
      mutate(month=as.integer(stringr::str_sub(var,-2))) %>%
      mutate(var=stringr::str_sub(var,end=-3))

# Step 2: define climate scenario subfunction
  climSubf1<-function(id) { #use id from futureClim

      scen=str_split_fixed(id,pattern=fixed("."),n=2)[1]
      per=str_split_fixed(id,pattern=fixed("."),n=2)[2]

      # start with futureClim data frame
      # filtered for scenario and period
      filter(futureClim,Scenario==scen & period==as.character(per)) %>%

      # get Tmax, Tmin and PPT into separate columns and add future prefix
      pivot_wider(names_from=var,values_from=value,names_prefix="future.") %>%

      # join with climate station data
      right_join(stnData,by="month") %>%

      # adjust current data with future data
      mutate(ppt=ppt*future.PPT) %>%
      mutate(tmx=tmx+future.Tmax) %>%
      mutate(tmn=tmn+future.Tmin) %>%

      # select final columns
      dplyr::select(period,Scenario,date,year,month,day,tmn,tmx,ppt) %>%

      # run asmrCalc()
      asmrCalc() %>%

      # retun
      return()

  }

# Step 3: apply it to station data for each scenario/time period

  x<-lapply(unique(futureClim$id),climSubf1)
  names(x)<-unique(futureClim$id)

# Step 4: return
  return(x)

}
