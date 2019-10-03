#' Select years to use in ASMR summaries
#'
#' This function implements a random approach to selecting 10 years to pass to
#' summariseASMR() function.
#'
#' The function uses the following approach:
#'
#' #' \itemize{
#'   \item Data from 1961-1990 climate normal period are preferred
#'   \item No data from outside 1940-2000 period are used
#'   \item 10 years are selected randomly to generate ASMR summaries.  If there are 10+ years within the 1961-1990 period, years will be randomly selected from that time period.
#'
#'
#' }
#'
#'@param asmrData Output from asmrCalc() function.
#'@param excl data boundaries (usually 1940-2000, doesn`t need to be set)
#'@param win ideal climate normal period (usually 1961-1990, doesn`t need to be set)
#'@param yrs number of years to randomly select (usually 10, doesn`t need to be set)
#' @import dplyr tidyr magrittr
#' @export
#' @examples
#' asmrSelect(asmrCalc(PrinceGeorge))
#'


asmrSelect<-function(asmrData,excl=c(1940,2000),win=c(1961,1990),yrs=10) {

  # First, exclude any years outside the 1940-2000 time period
  asmrData%<>%
    filter(year>=min(excl) & year<=max(excl))

  # Data filtered for ideal climate normal period
    x1<-
      asmrData %>%
      filter(year>=min(win) & year<=max(win))

  # Create if - then statement
  if(length(unique(x1$year))<=yrs) { # if there are less than 10 years data in the climate normal period

    # 1. How many extra years do you require?
    no.years<-yrs-length(unique(x1$year))

    # 2. Create a new climate data frame with data outside the ideal climate normal period
    x2<-asmrData[!asmrData$year%in%(min(win):max(win)), ]
    x2<-x2[x2$year%in%sample(unique(x2$year),no.years),]

    # 3. Put the datasets together
    x.final=rbind(x1,x2)

  } else { # Select 10 random years within ideal climate normal period

    x.final<-x1[x1$year%in%sample(unique(x1$year),10),]

  }

  return(list(asmr=summariseASMR(x.final),
              years=sort(unique(x.final$year))
              ) # close list
         ) # close return

}
