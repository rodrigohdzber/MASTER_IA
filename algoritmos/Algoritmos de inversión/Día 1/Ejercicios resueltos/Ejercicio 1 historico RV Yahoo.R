# Consigue el histórico de renta variable de las siguientes empresas
# Recomendación: Yahoo Finance
# Tiempo objetivo: 30 minutos

library(quantmod)

activos_a_descargar<-c("EMBR3.SA", "ENBR3.SA", "EQTL3.SA", "CPI.L", "CRH.L", "DC.L", 
                       "DCC.L", "DGE.L", "HEN3.DE", "IFX.DE","LHA.DE", "LIN.DE", "MRK.DE")

for (activo in activos_a_descargar){
  print(activo)
  getSymbols(activo,src='yahoo')
}

class(CPI.L)


















