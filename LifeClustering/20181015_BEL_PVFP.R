
BEL_PVFP <- function(DB){
  
  # Compute the vectors of inflation and actualization (beginning and end of the year)
  # We assume that these three vectors are computed using one single vector of financial return
  Inflation <- NULL
  Actu_End <- NULL
  Actu_Begin <- NULL
  
  for (t in 1:110)
  {
    Inflation[t] <- ifelse(t==1,1,Inflation[t-1] *(1+FinancialReturn[t-1,2]))
    Actu_Begin[t] <- ifelse(t==1,1,Actu_Begin[t-1]/(1+FinancialReturn[t-1,2]))
    Actu_End[t] <- ifelse(t==1,1/(1+FinancialReturn[1,2]),Actu_End[t-1]/(1+FinancialReturn[t,2]))
  }
  
  BEL <- NULL
  PVFP <- NULL
  
  # Loop on all the rows of the database  
  for (i in 1:nrow(DB))
  {
    
    # Initialisation of the different vectors
    PolicyNr <- NULL
    NrDeath <- NULL
    Reserve_Euro <- NULL
    Reserve_UC <- NULL
    Lapse_UC <- NULL
    Lapse_Euro <- NULL
    Death_UC <- NULL
    Death_Euro <-NULL
    FinancialProduct <- NULL
    ProfitSharing <- NULL
    GrossUC <-NULL
    AssetManagerCost <- NULL
    CostOutstandingUC <- NULL
    Total_Reserve <- NULL
    RetrocessionAM <- NULL
    Commission_Euros <- NULL
    Commission_UC <- NULL
    Result <- NULL

    # Compute the age of the modelpoint in 2019
    Age <- ComputationYear - DB[i,"Year_Birth"]
    Wholelapse <- 110 - Age
    
    # Reserves (in UC and in Euro)
    Reserve_UC[1] <- DB[i,"MP_UL"] 
    Reserve_Euro[1] <- DB[i,"MP_Euro"]  
    
    for (t in 1:Wholelapse)
    {
      # Apply mortality
      PolicyNr[t] <- ifelse(t==1,1,PolicyNr[t-1]-NrDeath[t-1])
      NrDeath[t] <- PolicyNr[t] * Mortality[Age+t-1,2]
      
      # Paiements for lapses
      Lapse_UC[t] <- ifelse(t == Wholelapse, Reserve_UC[t], Reserve_UC[t] * LapseRate)
      Lapse_Euro[t] <- ifelse(t == Wholelapse, Reserve_Euro[t], Reserve_Euro[t] * LapseRate)
      
      # Paiement for death
      Death_UC[t] <- (Reserve_UC[t]-Lapse_UC[t])*NrDeath[t]/PolicyNr[t]
      Death_Euro[t] <- (Reserve_Euro[t]- Lapse_Euro[t] )*NrDeath[t]/PolicyNr[t]
      
      # Revaluation (in euros)
      FinancialProduct[t] <- (Reserve_Euro[t]-Lapse_Euro[t]-Death_Euro[t]) *FinancialReturn[t,2]  
      ProfitSharing[t] <- (Reserve_Euro[t]-Lapse_Euro[t]-Death_Euro[t]) * 
        max(0,FinancialReturn[t,2]*DB[i,"Profit_Sharing"]/100- DB[i,"Manag_Fees"]/100)
      
      # Revaluation (UC)
      GrossUC[t] <- (Reserve_UC[t]-Lapse_UC[t]-Death_UC[t]) * FinancialReturn[t,2]  
      AssetManagerCost[t] <- (Reserve_UC[t]-Lapse_UC[t]-Death_UC[t]) * DB[i,"Cost_Asset_manag"]/100
      CostOutstandingUC[t] <- (Reserve_UC[t]-Lapse_UC[t]-Death_UC[t]) * DB[i,"Manag_Fees"]/100
      
      # Reserve end of the year
      Reserve_UC[t+1] <- Reserve_UC[t]-Lapse_UC[t]-Death_UC[t] +  GrossUC[t]-AssetManagerCost[t]-CostOutstandingUC[t]
      Reserve_Euro[t+1] <- Reserve_Euro[t]-Lapse_Euro[t]-Death_Euro[t]+ProfitSharing[t]
      Total_Reserve[t+1] <- Reserve_UC[t+1] + Reserve_Euro[t+1]
      
      RetrocessionAM[t] <- AssetManagerCost[t]  * DB[i,"Cost_retro"]/100
      Commission_Euros[t] <- Reserve_Euro[t+1] * DB[i,"Comm_MP_Euro"]/100
      Commission_UC[t] <- Reserve_UC[t+1] * DB[i,"Comm_MP_UL"]/100
      
      # Result (before taxation)
      Result[t] <- FinancialProduct[t]-ProfitSharing[t]+CostOutstandingUC[t]+RetrocessionAM[t]-Commission_Euros[t]-Commission_UC[t]
      
    }
    # Apply actualisation
    BEL[i] <- sum(c(Lapse_UC,Lapse_Euro,Death_UC,Death_Euro)*Actu_Begin[1:Wholelapse])+ sum(c(AssetManagerCost,-RetrocessionAM,Commission_Euros,Commission_UC)*Actu_End[1:Wholelapse])
    PVFP[i] <- sum(Result * Actu_End[1:Wholelapse])
  }
  return(list(BEL=BEL, PVFP=PVFP))
}