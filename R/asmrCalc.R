#' ASMR calculation
#'
#' This function uses daily climate and soils data to estimate daily
#' water balance for five different soil moisture regimes.
#'
#' Function returns a time series of monthly ASMR values for five sites.
#' Use the summariseASMR() function to create monthly summaries.
#'
#'@param climateData Climate data must be imported correctly.
#'@param latitude Latitude can be one of three numbers (50,55,60)
#'@param soils This can be the soilsData variable as already defined.  If you want to use
#'different parameters, then modify the soilsData variable.
#' @import dplyr tidyr
#' @keywords internal
#' @examples
#' asmrCalc(PrinceGeorge)




asmrCalc<-function(climateData,latitude=55,soils=soilsData) {

# CONSTANTS
# TACA uses the following constants:
  albedo=0.15
  T.kelvin=273.15
  Emissivity=0.96
  SBconstant=0.0000000567

# DAYLENGTH
# Monthly daylength lookup table for three latitudes
  daylength.table=data.frame(N50=c(8.3,9.8,11.6,13.5,15.2,16.1,15.7,14.3,12.4,10.5,8.8,7.9),
                             N55=c(7.5,9.4,11.5,13.8,15.9,17.1,16.6,14.7,12.4,10.2,8.1,6.9),
                             N60=c(6.4,8.8,11.4,14.2,16.8,18.4,17.7,15.4,12.5,9.7,7.1,5.6))

  # Daylength calculation
  daylength=daylength.table[,grep(paste("N",latitude,sep=""),names(daylength.table))]

# EXTRACT SOILS INFORMATION
  root=soils$root
  CF=soils$CF
  AWSC1=soils$AWSC1
  FC=soils$FC
  AFC=soils$AFC
  AWSC=soils$AWSC
  infRate=soils$infRate

# ADDITIONAL CLIMATE VARIABLES
  clim <-climateData %>%

    # Average daily temperature
    rowwise() %>%
    mutate(tav=(tmn+tmx)/2) %>%
    ungroup() %>%

    # Snow (if TAV<=0, snow=PPT, otherwise snow=0)
    mutate(snow=ifelse(tav<=0,ppt,0)) %>%  # Create snow column

    # Other variables, see user guide for more  info
    mutate(Esat=0.61078*exp(17.269*tmx/(237+tmx))) %>%
    mutate(Edew=0.61078*exp(17.269*tmn/(237+tmn))) %>%
    mutate(VDD=(Esat-Edew)*1) %>%
    mutate(ET2=0) %>%

    # Radiation is measured in watts/m2/day, and drives evapotranspiration modeling
    mutate(Rn=((((1-albedo)*(0.7*((1-exp(-1*(0.036*exp(-0.154*abs(tmx-tmn)))*((abs(tmx-tmn)^2.4)))))*daylength[month])+(0.465*(VDD/(T.kelvin+tav)))+((Emissivity*(SBconstant*((T.kelvin+tav)^4)))))-(SBconstant*((T.kelvin-38)^4))))) %>%
    mutate(Rn2=Rn/11.576) %>%
    mutate(tav2=238.8/(595.5-(0.55*tav))) %>%

    # Hargreaves potential evapotranspiration
    mutate(HarPET=0.0135*(tav+17.78)*Rn2*tav2) %>%
    # eliminate negative values in Hargreaves
    mutate(HarPET=replace(HarPET,which(HarPET<0),0)) %>%
    # Adjust to 0 for days with TMX<=0
    mutate(PET=ifelse(tmx<=0,0,HarPET))

  # SNOWBANK
    # Create snowbank dataframe
    for (j in 1:nrow(clim)) {

      if (j==1) clim$snowbank=NA
      # Starting snowbank is calculated as snowfall from November 15:December 31
      # of last year in the climate dataset
      snowStart=ifelse(j==1,sum(clim[(nrow(clim)-46):nrow(clim),"snow"]),clim$snowbank[j-1])

      clim$snowbank[j]=ifelse(ifelse(clim$snow[j]+snowStart-clim$PET[j]<0,0,clim$snow[j]+snowStart-clim$PET[j])-(ifelse(clim$tav[j]<0,0,((0.026*clim$Rn[j])+(0.2*clim$tav[j]))/10))<0,0,ifelse(clim$snow[j]+snowStart-clim$PET[j]<0,0,clim$snow[j]+snowStart-clim$PET[j])-(ifelse(clim$tav[j]<0,0,((0.026*clim$Rn[j])+(0.2*clim$tav[j]))/10)))

    }

  # DAILY WATER BALANCE
  clim<-
    clim %>%
    mutate(pPET=ifelse(snow>0,snow-PET,ppt+snowbank-PET))

  # First pPET value is calculated differently
  clim$pPET[1]=ifelse(clim$ppt[1]==0,clim$snowbank[1]-clim$PET[1],clim$ppt[1]-clim$PET[1])

# DAILY ASMR CALCULATIONS

  # Initialize climate run
  DST=matrix(data=0,ncol=5,nrow=nrow(clim)); DST[1,]=clim$pPET[1]
  AET=matrix(data=0,ncol=5,nrow=nrow(clim)); AET[1,]=ifelse(clim$snowbank[1]>0,clim$PET[1],ifelse(clim$ppt[1]>clim$PET[1],clim$PET[1],clim$ppt[1]+abs(DST[1,])))
  RunS=matrix(data=0,ncol=5,nrow=nrow(clim)) # first row is zero
  ST=matrix(data=0,ncol=5,nrow=nrow(clim)); ST[1,]=AFC

  for (j in 2:nrow(clim)) {

    for (k in 1:5) {
      DST[j,k]=ifelse(clim$pPET[j]>0,clim$pPET[j],ifelse(ST[j-1,k]<=0,0,ifelse(ST[j-1,k]<abs(clim$pPET[j]),ST[j-1,k]*(-1),clim$pPET[j])))
      AET[j,k]=ifelse(clim$snowbank[j]>0,clim$PET[j],ifelse(clim$ppt[j]>clim$PET[j],clim$PET[j],clim$ppt[j]+abs(DST[j,k])))
    }

    #RunS
    RunS[j,1]=ifelse(ST[j-1,1]>AWSC[1],infRate[1],0)
    RunS[j,2]=ifelse(ST[j-1,2]>AWSC[2],infRate[2],0)
    RunS[j,3]=ifelse(infRate[3]<0,ifelse(RunS[j,1]+RunS[j,2]>0,infRate[3],ifelse(clim$ppt[j-1]==0,0,ifelse(clim$ppt[j-1]>infRate[3],infRate[3],clim$ppt[j-1]))),ifelse(ST[j-1,3]>AWSC[3],infRate[3],0))
    RunS[j,4]=ifelse(ST[j-1,4]>AWSC[4],infRate[4],0)
    RunS[j,5]=ifelse(infRate[5]<0,ifelse(sum(RunS[j,1:4])>0,infRate[5],ifelse(clim$ppt[j-1]==0,0,ifelse(clim$ppt[j-1]>infRate[5],infRate[5],clim$ppt[j-1]))),ifelse(ST[j-1,5]>AWSC[5],infRate[5],0))

    for (k in 1:5) {
      ST[j,k]=ifelse(ifelse((ifelse(ST[j-1,k]<0,0,ST[j-1,k]+DST[j,k]))<0,0,(ifelse(ST[j-1,k]<0,0,ST[j-1,k]+DST[j,k])))>AFC[k],AFC[k],ifelse((ifelse(ST[j-1,k]<0,0,ST[j-1,k]+DST[j,k]))<0,0,(ifelse(ST[j-1,k]<0,0,ST[j-1,k]+DST[j,k]))))-RunS[j,k]
    }
  }

  ASMR=matrix(data=1,nrow=nrow(clim),ncol=5)
  for (j in 1:nrow(clim)) {
    for (k in 1:5) {
      ASMR[j,k]=ifelse(clim$PET[j]<=0,1,AET[j,k]/clim$PET[j])
    }
  }

  colnames(ASMR)=paste("S",1:5,".ASMR",sep="")

  # Assign output to final data frame
  final<-data.frame(clim,ASMR) %>%
    select(date,year,month,day,everything()) # Reorder

    return(final)

}
