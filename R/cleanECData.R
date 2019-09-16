#' Clean Environment Canada daily climate data
#'
#' This function takes a pre-formatted Environment Canada daily climate dataset
#' and prepares it for use in the asmrCalc() function.
#'
#'@param climateData Climate data must be imported correctly.  Need to have year, month, day, tmn, tmx and ppt columns.
#'
#' @import dplyr tidyr imputeTS
#' @export
#' @examples
#'
#' require(weathercan)
#' smithersData<-
#' weathercan::weather_dl(station_ids=487,interval="day",start="1955-01-01",end="1995-12-31") %>%
#' select(stn=station_name,date,tmn=min_temp,tmx=max_temp,ppt=total_precip,year,month,day)
#' cleanECData(smithersData)

cleanECData<-function(climateData) {


# Omit years with 10 or more consecutive missing values in any climate variable
daily_NA_fill<-
  climateData %>%
  group_by(year) %>%
  summarise(pptNA=imputeTS::statsNA(ppt,printOnly=FALSE)$naGapLongest,
            tmxNA=imputeTS::statsNA(tmx,printOnly=FALSE)$naGapLongest,
            tmnNA=imputeTS::statsNA(tmn,printOnly=FALSE)$naGapLongest) %>%
  mutate(pptNA, pptNA = ifelse(is.na(pptNA), 0, pptNA)) %>%
  mutate(tmxNA, tmxNA = ifelse(is.na(tmxNA), 0, tmxNA)) %>%
  mutate(tmnNA, tmnNA = ifelse(is.na(tmnNA), 0, tmnNA)) %>%
  filter(pptNA<10&tmxNA<10&tmnNA<10) %>%
  select(year) %>%

  # Impute NA values using adjacent values
  left_join(climateData,by="year") %>%
  mutate(ppt2=imputeTS::na_locf(ppt),ppt3=imputeTS::na_locf(ppt,option="nocb")) %>%
  mutate(tmx2=imputeTS::na_locf(tmx),tmx3=imputeTS::na_locf(tmx,option="nocb")) %>%
  mutate(tmn2=imputeTS::na_locf(tmn),tmn3=imputeTS::na_locf(tmn,option="nocb")) %>%
  rowwise() %>%
  mutate(ppt_filled=mean(ppt2,ppt3)) %>%
  mutate(tmx_filled=mean(tmx2,tmx3)) %>%
  mutate(tmn_filled=mean(tmn2,tmn3)) %>%
  dplyr::select(year,month,day,ppt_filled,tmx_filled,tmn_filled)

return(daily_NA_fill)

}

