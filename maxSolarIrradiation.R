

# ########################################################################### #
# This script is based on:                                                    #
# Oscar Perpiñán (2012). solaR: Solar Radiation and Photovoltaic              #
# Systems with R, Journal of Statistical Software, 50(9), 1-32. URL           #
# http://www.jstatsoft.org/v50/i09/.                                          #
#                                                                             #
# This script uses solaR.                                                     #
# solaR download: https://cran.r-project.org/web/packages/solaR/index.html    #
# ########################################################################### #

maxSolarIrradiation <- function(lat,elePVdeg,aciPVdeg,year)
{
  
library(solaR)

#--------------------------------------------------------------------------------------------------------------------
#input:

#lat = 48.4010822
#latitude of location
#lat = 48.4010822 for Ulm

ps = 1.367
#power of the sun [kW/m^2]
#ps = 1.367 is the solar constant

#elePVdeg = 30
#elevation angle of the PV-panel in degrees
#0° => the PV-panel is flat on the ground | 90° => the PV-panel is standing straight facing towards the acimuth angle
elePVrad = elePVdeg/180*pi

#aciPVdeg = 0
#acimuth angle of the PV-panel in degrees
#0° => south | 90° => west | 180° => north | 270° => east
aciPVrad = aciPVdeg/180*pi

#year = 2020
#year that is to be simulated

#--------------------------------------------------------------------------------------------------------------------

fBTd = fBTd(mode = "serie", start=paste('01-01-',year,sep=''), end=paste('31-12-',year,sep=''), format='%d-%m-%Y')
#daily time base

numberofdays = length(fBTd)
#number of days in the specific year

fSolD = fSolD(lat=lat, BTd = fBTd, method='michalsky')
#Daily apparent movement of the Sun from the Earth

calcSol = calcSol(lat=lat, BTd=fBTd, sample='min',  EoT=TRUE, keep.night=TRUE, method='michalsky')
#Apparent movement of the Sun from the Earth for every minute

data = matrix(data = NA, nrow = numberofdays*24*4, ncol = 6)
#data is a matrix containing all desired information
#data[,1] => 1 => the sun is (partially) above the horizon 0 => the sun is below the horizon
#data[,2] => cosine of current solar zenith angle
#data[,3] => current solar elevation angle (radians)
#data[,4] => current solar acimuth angle (radians)
#data[,5] => resulting incidence angle
#data[,6] => resulting maximung incoming solar power

k = 1
while (k <= (numberofdays*24*4)) {
  #this loop is converting the data for every minute to data for every quarter of an hour
  
  #1
  if (sum(calcSol@solI[(15*(k-1)+1):(15*k),2]) != 0) {
      data[k,1] = 1
  } else {
    data[k,1] = 0
  }
  #This part of the loop is simply marking a quarter of an hour as a sun hour in data[,1] if the sun is shining at any minute
  
  #2
  data[k,2] = mean(calcSol@solI[(15*(k-1)+1):(15*k),3])
  data[k,3] = mean(calcSol@solI[(15*(k-1)+1):(15*k),4])
  data[k,4] = mean(calcSol@solI[(15*(k-1)+1):(15*k),5])
  #This part of the loop is computing the mean of the angles of 15 minutes and adding it as a quarter of an hour to our data matrix
  
  #3
  data[k,5] = acos(data[k,2] * cos(elePVrad) + sin(elePVrad) * sin(acos(data[k,2])) * cos(data[k,4] - aciPVrad))
  #Michalsky's algorithm
  #https://www.nrel.gov/docs/fy08osti/34302.pdf p.11
  #This part of the loop is computing the incidence angle
  
  #4
  if (data[k,1] == 1) {
    if (cos(data[k,5]) > 0) {
      data[k,6] = cos(data[k,5]) * ps
      #data[,6] contains the maximum incoming solar power
    } else {
      data[k,6] = 0
  }
  } else {
    data[k,6] = 0
  }
  
  k = k + 1
}


sum(data[,6])/4
#this shows the maximum incoming solar energy [kWh/(a * m^2)] throughout the entire year on 1 m^2

# Aggreate to Hour 

dataHour = matrix(data = NA, nrow = numberofdays*24, ncol = 6)
for (k in 1:(numberofdays*24))
{
  dataHour[k,1:6] = colMeans(data[((k-1)*4+1):(k*4),])
}

return(dataHour)

}