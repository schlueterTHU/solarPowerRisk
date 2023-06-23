
# ############################################################ #
# Pfade die angepasst werden müssen
df = read.table('nbg.txt',sep=';',header = T)
temperatur = read.table('/tempNbg.txt',sep=';',header = T)
source("maxSolarIrradiation.R")

# ganz am Ende noch zwei write.csv

# Quelle:
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/solar/

# ############################################################ #

# ############################################################ #
# Input required
Peakleistung = 10
lat  = 49.46  # Nürnberg

# ############################################################ #


# ------------------------------------------ #

# gamma = Aufstellrichtung (0? => south | 90? => west | 180? => north | 270? => east)
gamma = 0
# Neigung der Panels= 30°C
beta = 30

# ----------------------------------------------------------------
# Source: Climate Data Center
# Haben leider nur ein paar Stützstellen Deutschlandweit
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/solar/


# FD_LBERG = Diffuse Strahlung
# FG_Lberg = Global Strahlung
# SD_Lberg = Sonnenstundendauer
zenith_DWD = df$ZENIT

# Time Stamp der Irradiation aufbereiten
# Achtung: DWD Daten sind J/m2 --> umrechnen in Wh/m^2 voa 2,78
diffuseRadiation    = (df$FD_LBERG)*2.78
globalRadiation     = (df$FG_LBERG)*2.78
timeStampIrradiation = df$MESS_DATUM
jahrI    = as.numeric(substr(timeStampIrradiation,1,4))
monatI   = as.numeric(substr(timeStampIrradiation,5,6))
tagI     = as.numeric(substr(timeStampIrradiation,7,8))
stundeI  = as.numeric(substr(timeStampIrradiation,9,10))

Irr = matrix(NA,nrow = length(diffuseRadiation),ncol= 9)
Irr[,1] = jahrI
Irr[,2] = monatI
Irr[,3] = tagI
Irr[,4] = stundeI
Irr[,5] = globalRadiation
Irr[,6] = globalRadiation - diffuseRadiation  # direkte
Irr[,7] = diffuseRadiation
Irr[,8] = zenith_DWD


# Errorhandling and missing values
missingVec1 = (Irr[,5] < 0)
missingVec2 = (Irr[,7] < 0)
missingVec3 = (Irr[,5] < 0) | (Irr[,7] < 0)
errorVec1   = Irr[,6] < 0

leaveOutVec = missingVec1 + missingVec2 + missingVec3 + errorVec1
Irr[,9] = leaveOutVec

rm(timeStampIrradiation,df,globalRadiation,diffuseRadiation,monatI,stundeI,tagI)

# --------------------------------------------------------------- #
# Falls Irradiation Data Mitten im Jahr anfangen, schneide sie ab
# --------------------------------------------------------------- #

checker = (Irr[1,2] == 1)*(Irr[1,3] == 1)*(Irr[1,4] == 0)
k = 1
while ( checker == 0) # 0 = False
{
  k = k+1
  checker = (Irr[k,2] == 1)*(Irr[k,3] == 1)*(Irr[k,4] == 0)
}
  
Irr = Irr[k:length(jahrI),]  
rm(checker)

# --------------------------------------------------------------- #
# Falls Irradiation Data Mitten im Jahr aufhören, schneide sie ab
# --------------------------------------------------------------- #
d = dim(Irr)
k = d[1]
checker = (Irr[k,2] == 12)*(Irr[k,3] == 31)*(Irr[k,4] == 23)
while ( checker == 0) # 0 = False
{
  k = k-1
  checker = (Irr[k,2] == 12)*(Irr[k,3] == 31)*(Irr[k,4] == 23)
}

Irr  = Irr[1:k,]
rm(checker)
# ------------------------------------------------------------------- #
# Ergebnis hier: ganze Jahre!                                         #
# muss also nur noch zusammen fügen mit den Sonnenwinkeln             #
# ------------------------------------------------------------------- #

d = dim(Irr)
# Jahre für Wetter-Metadaten
jahrSeq = seq(Irr[1,1],Irr[d[1],1],1)




# ---------------------------------------------------------- # 
#  maxSolarIrradiation nur für Schaltjahr und Normales Jahr
#  und dann fasse zusammen
normalesJahr = maxSolarIrradiation(lat,beta,gamma,2021)
schaltJahr   = maxSolarIrradiation(lat,beta,gamma,2020)
# ---------------------------------------------------------- #

schaltJahrListe = c(1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024, 2028, 2032)

if (sum(jahrSeq[1] == schaltJahrListe) == 1)
{
  X = schaltJahr[,c(2,4)]
} else
{
  X = normalesJahr[,c(2,4)]
}

for (k in 2:length(jahrSeq))
{
  if (sum(jahrSeq[k] == schaltJahrListe) == 1)
  {
    X_zwischen = schaltJahr
  } else
  {
    X_zwischen = normalesJahr
  }
  #data[,1] => 1 => the sun is (partially) above the horizon 0 => the sun is below the horizon
  #data[,2] => cosine of current solar zenith angle
  #data[,3] => current solar elevation angle (radians)
  #data[,4] => current solar acimuth angle (radians)
  #data[,5] => resulting incidence angle
  #data[,6] => resulting maximung incoming solar power
  
  # We need column #2, #4 for now
  X = rbind(X,X_zwischen[,c(2,4)])
}
rm(X_zwischen,normalesJahr,schaltJahr,schaltJahrListe)


# check if Time Shift zwischen Deutschland und maxSolarIrraidation
# test1 = (acos(X[(170*24+1):(24*171),1])*180/pi) < 90
# test2 = Irr[(170*24+1):(24*171),5]
# X und Irradiation sollten die gleiche Länge etc. haben

# ----------------------------------------------------------------- #
# Hier Temperatur und die obige Matrix abgleichen bzgl. Time Stamp
# ----------------------------------------------------------------- #

timeStampTemp = (temperatur$MESS_DATUM)
jahrT    = floor(timeStampTemp/1000000)
monatT   = floor((timeStampTemp - jahrT*1000000)/10000)
tagT     = floor((timeStampTemp - jahrT*1000000 - monatT*10000)/100)
stundeT  = timeStampTemp - jahrT*1000000 - monatT*10000 - tagT*100

tempData = matrix(NA,nrow=length(jahrT),ncol = 5)
tempData[,1] = jahrT
tempData[,2] = monatT
tempData[,3] = tagT
tempData[,4] = stundeT
tempData[,5] = temperatur$TT_TU

rm(temperatur,timeStampTemp,jahrT,monatT,stundeT,tagT)
# Temperatur Time Series eigentlich immer länger



tempMatrix = tempData[tempData[,1] == jahrSeq[1],]
for (k in 2:length(jahrSeq))
{
  tempMatrix = rbind(tempMatrix,tempData[tempData[,1] == jahrSeq[k],])
}
rm(tempData)

# --------------
theta_t = acos(X[,1]*cos(pi*beta/180) + sin(acos(X[,1]))*sin(pi*beta/180)*cos(X[,2] - gamma*pi/180)  )
# Test = ist identisch
#theta_t2 = acos(cos(Irr[,8]*pi/180) *cos(pi*beta/180) + sin(Irr[,8]*pi/180)*sin(pi*beta/180)*cos(X[,2] - gamma*pi/180)  )

# --------------------
# # compute the Albedo for the reflexive irradiation

angle = (acos(X[,1])*180/pi)

albedo = (angle < 60)*20 + (angle >= 60)*(100 - (8/3)*(90-angle))
albedo[albedo >100] = 0  # then the sun is below the horizon
coeff = (albedo/200)*(1 - cos(beta*pi/180))
Irr_reflexive = coeff*Irr[,5]

coeff_directTilt = (cos(theta_t)/X[,1])
# Error Handling
coeff_directTilt[abs(coeff_directTilt) > 200] = 0
coeff_directTilt[coeff_directTilt < 0] = 0
IrrDirect_tilt  = Irr[,6]*coeff_directTilt
IrrDiffuse_tilt = Irr[,7]*0.5*(1 + cos(pi*beta/180))

Irr_tot = Irr_reflexive+IrrDiffuse_tilt+IrrDirect_tilt

# plot(Irr_tot[Irr[,9] == 0],type="l")

# Verwende SolarEdge Module
tempSurf = tempMatrix[,5] + 0.03*(Irr_tot)
eta = 0.189*(1 - 0.004*(tempSurf - 25))
Modulgroesse = 1.6
Modulleistung = 300
Dachflaeche = Peakleistung*Modulgroesse/Modulleistung
Theta_t = Dachflaeche*Irr_tot*cos(theta_t)

PV_final = Theta_t*eta
PV_final[PV_final < 0] = 0


# ---------------------------------- #
# Aggregation 
# ---------------------------------- #

monthAggregate = matrix(NA,nrow=(length(jahrSeq)+2),ncol=13)
for (k in 1:length(jahrSeq))
{
  monthAggregate[k,1] = jahrSeq[k]
  Y = cbind(Irr[Irr[,1] == jahrSeq[k],2], PV_final[Irr[,1] == jahrSeq[k]],Irr[Irr[,1] == jahrSeq[k],9])
  for (j in 1:12)
  {
    # schneide spezifisches Monat raus
    monat = sum(Y[Y[,1] == j,3])
    if (monat > 0) # dann haben wir einen Fehler im Monat --> rauslassen zunächst
    {
      #      monthAggregate[k,j+1]  bleibt NA
    }else
    {
      monthAggregate[k,j+1] = sum(Y[Y[,1] == j,2])
    }
    rm(monat)
  }
}

for (k in 2:13)
{
 monthAggregate[length(jahrSeq)+1,k] = mean(monthAggregate[1:length(jahrSeq),k],na.rm = TRUE)
 monthAggregate[length(jahrSeq)+2,k] = sd(monthAggregate[1:length(jahrSeq),k],na.rm = TRUE)
}


# To Do: 
# 2. Was tun mit den vielen fehlenden Werten? Kein einziges Jahr das durchgehend Werte hat!
#    kann die nicht einfach rauswerfen

# -------------------------------------------------------------------- #

monthIndex = matrix(NA,nrow=12,ncol=2)
monthIndex[,1] = seq(1,12,1)
monthIndex[1,2] = 31
monthIndex[2,2] = 28
monthIndex[3,2] = 31
monthIndex[4,2] = 30
monthIndex[5,2] = 31
monthIndex[6,2] = 30
monthIndex[7,2] = 31
monthIndex[8,2] = 31
monthIndex[9,2] = 30
monthIndex[10,2] = 31
monthIndex[11,2] = 30
monthIndex[12,2] = 31
# Generate index to filter out specific days


aktuellesMonat = matrix(NA,nrow =monthIndex[1,2],ncol=2)
aktuellesMonat[,1] = monthIndex[1,1]
aktuellesMonat[,2] = seq(1,monthIndex[1,2],1)
dayIndex = aktuellesMonat

for(k in 2:12)
{
  aktuellesMonat = matrix(NA,nrow =monthIndex[k,2],ncol=2)
  aktuellesMonat[,1] = monthIndex[k,1]
  aktuellesMonat[,2] = seq(1,monthIndex[k,2],1)
  dayIndex = rbind(dayIndex,aktuellesMonat)
}

# Daily aggregate --> dann fehlen weniger Tage. Ignoriere 29.Feb
dayAggregate = matrix(NA,nrow=(length(jahrSeq)+3),ncol=366)
for (k in 1:length(jahrSeq))
{
  dayAggregate[k,2] = jahrSeq[k]
  for (j in 1:365)
  {
    idxDay =  (Irr[,1] == jahrSeq[k] & Irr[,2] ==dayIndex[j,1]   &  Irr[,3] ==  dayIndex[j,2])
    pvMenge = PV_final[idxDay==TRUE]
    redFlag = Irr[idxDay==TRUE,9] # hier sind die Fehler abgespeichert
    if (sum(redFlag) > 0) # hier gibt's irgendwo mindestens einen fehlenden Wert
    {
      # bleibt ein NA
    }else
    {
     dayAggregate[k,j+1] = sum(pvMenge) 
    }
  }
}

NA_mat = is.na(dayAggregate)
for (k in 2:366)
{
  dayAggregate[length(jahrSeq)+1,k] = mean(dayAggregate[1:length(jahrSeq),k],na.rm = TRUE)
  dayAggregate[length(jahrSeq)+2,k] = sd(dayAggregate[1:length(jahrSeq),k],na.rm = TRUE)
  # Confidence Bands
  n = sum(NA_mat[1:length(jahrSeq),k]==0)
  dayAggregate[length(jahrSeq)+3,k] = qt(0.975,n-1)* dayAggregate[length(jahrSeq)+2,k]/sqrt(n)
}

write.csv(monthAggregate,"C:/Users/schlueter/Desktop/Klaus_Simulation/resultsMonth.csv")
write.csv(dayAggregate,"C:/Users/schlueter/Desktop/Klaus_Simulation/resultsDay.csv")
