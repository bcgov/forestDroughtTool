#' Summarise monthly ASMR values
#'
#' This function summarises monthly ASMR values from the
#' asmrCalc() function.
#'
#'@param asmrData Output from asmrCalc() function.
#' @import dplyr tidyr
#' @export
#' @examples
#' summariseASMR((asmrCalc(PrinceGeorge))
#'

summariseASMR<-function(asmrData) {

  summ.asmr<-
    asmrData %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(S1.mean=round(mean(S1.ASMR),3),
              S2.mean=round(mean(S2.ASMR),3),
              S3.mean=round(mean(S3.ASMR),3),
              S4.mean=round(mean(S4.ASMR),3),
              S5.mean=round(mean(S5.ASMR),3),
              S1.sd=round(sd(S1.ASMR),3),
              S2.sd=round(sd(S2.ASMR),3),
              S3.sd=round(sd(S3.ASMR),3),
              S4.sd=round(sd(S4.ASMR),3),
              S5.sd=round(sd(S5.ASMR),3)) %>%
    dplyr::mutate(month=month.abb[month])

  return(summ.asmr)

}
