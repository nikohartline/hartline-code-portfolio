# FUNCIONS FOR INTERFACE DEVELOPED BY RENATO MOLINA, NIKO HARTLINE, AND JULIANO PALACIOS

######################################
# Establish the price for markets
######################################

priceFunction<-function(a,b,H,c){
  # a Choke price vector
  # b Slope vector
  # H Harvest limit
  # c aggregated marginal cost
  
  mp=array(NA,c(1,2)) # Market price
  mh=array(NA,c(1,2)) # Market harvest
  
  # Estimate interior and corner solutions
  if(a[1]!=a[2]){# Demands have different choke prices
    # Rank demands as a function of their choke prices
    max=which(a==max(a)) # Max choke price
    min=which(a==min(a)) # Min choke price
    if(b[1]==b[2] && b[1]==0){# Both perfectly elastic
      mp[min]=a[min] # No harvest for this sucker
      mp[max]=a[max]-b[max]*H # Since perfectly elastic no need to worry for the price hitting bottom
      mh[min]=0
      mh[max]=H
    }else{# There is a kink
      # First evaluate at which point the agg demand kinks
      kink=(a[max]-a[min])/b[max]
      # After this kink the demand is aggregated, before it's just the one with the max choke price
      # We need to know if we have a corner solution
      if(H>kink){# Interior solution
        A=(a[1]*b[2]+a[2]*b[1])/(b[1]+b[2]) # kinda projecting the agg demand
        B=b[1]*b[2]/(b[1]+b[2]) # kinda the choke price for the agg demand
        mp[]=max(A-B*H,A*B/(B+c)) # makes sure prices don't go to zero if we allocate a bunch of quota
        mh=(a-mp)/b
      }else{# The fishery targets only one market
        mp[min]=a[min]
        mp[max]=max(a[max]-b[max]*H,a[max]*b[max]/(b[max]+c)) # Just making sure we don't have backward bending stuff ;)
        mh[min]=0 # Sorry folks but your market kinda sucks
        mh[max]=H
      } 
    }
  }else if(a[1]==a[2]){# Equal choke prices
    if(b[1]==b[2] && b[1]==0){# Both perfectly elastic
      mp[]=a[1]# All get the same price, no need to worry about prices going to zero
      mh=H/2 # Both demand curves are identical
    }else{ #The kink is at the choke price
      A=(a[1]*b[2]+a[2]*b[1])/(b[1]+b[2]) # kinda projecting the agg demand
      B=b[1]*b[2]/(b[1]+b[2]) # kinda the choke price for the agg demand
      mp[]=max(A-B*H,A*B/(B+c)) # blah blah blah the price and stuff
      mh=(a-mp)/b
    }
  }
  mp[ mp<0 ] <- 0 # just in case the I got tangled with all these corners
  mh[ mh<0 ] <- 0
  return(list(market_price=mp,market_harvest=mh))
}

###############################
# Profit and harvest
###############################

profitHarvest<-function(p,c,alpha,beta,TAC,tau){
  # p Price per subsector
  # c Marginal per subsector
  # alpha Fleet share
  # beta Subsector share
  # tau price of share
  
  if(tau==0){ # There is no trading
    h=min(p/c,alpha*beta*TAC) # Profit maximizing harvest
  }else{
    h=(p-tau)/c # Optimal harvest
  }
  pi=p*h-c/2*h^2-tau*(h-alpha*beta*TAC) # Profits including trading
  h<-max(0,h)
  return(list(harvest=h,profit=pi))
}

###############################
# Simulate markets 0--3
###############################

simulateMarkets<-function(a,b,marg.cost,TAC,alpha,beta,m){
  # a Choke price vector
  # b Slope vector
  # marg.cost Marginal cost matrix
  # TAC Total Allowable Catch
  # alpha Fleet share
  # beta Subsector share
  # m Scenario indicator
  
  mh=array(NA,c(1,2)) # Market harvest
  mp=array(NA,c(1,2))# Market price
  sh=array(NA,c(2,2)) # Subsector harvest
  spi=array(NA,c(2,2)) # Subsector profits
  ir=array(NA,c(2,2)) # Inframarginal rents
  rr=array(NA,c(2,2)) # Resource rents
  
  if(m==0){ # Status quo
    for (i in 1:2){
      if(i==1){ # Open access for artisanals
        A=1/sum(1/marg.cost[i,]) # Agg marg cost
        mh[i]=a[i]/(b[i]+A)
        mp[i]=max(a[i]-b[i]*mh[i],a[i]*b[i]/(b[i]+A)) # Prices don't dive with the quota
        mp[ mp<0 ] <- 0
        mh[ mh<0 ] <- 0
        for (j in 1:2){
          sh[i,j]  = mp[i]/marg.cost[i,j]
          spi[i,j] = 0 # All rents dissipated
        }
      }else{
        A=1/sum(1/marg.cost[i,]) # Agg marg cost
        mh[i]=TAC
        mp[i]=max(a[i]-b[i]*mh[i],a[i]*b[i]/(b[i]+A)) # Same stuff
        for (j in 1:2){
          PH=profitHarvest(mp[i],marg.cost[i,j],1,beta[i,j],TAC,0) #DIFERENCIA DE CONDIGO
          sh[i,j]  = PH$harvest
          spi[i,j] = PH$profit
        }
      }
      for(j in 1:2){
        rr[i,j]=min(spi[i,]) # Resource rent
        ir[i,j]=spi[i,j]-rr[i,j] # Inframarginal rent
      }
    }
  }else if(m==1){ # Quota for artisanals 
    for (i in 1:2){
      A=1/sum(1/marg.cost[i,]) # Agg marg cost
      mh[i]=TAC*alpha[i]
      mp[i]=max(a[i]-b[i]*mh[i],a[i]*b[i]/(b[i]+A)) # Price control
      mp[ mp<0 ] <- 0
      mh[ mh<0 ] <- 0
      for (j in 1:2){
        PH=profitHarvest(mp[i],marg.cost[i,j],alpha[i],beta[i,j],TAC,0)
        sh[i,j]  = PH$harvest
        spi[i,j] = PH$profit
      }
    }
    rr=array(min(spi),c(2,2)) # Resource rent
    ir=spi-rr # Inframarginal rent
  }else if(m==2){ # Open markets
    A=1/sum(1/marg.cost) # Agg marg cost
    PF=priceFunction(a,b,TAC,A) # No need to do the price thin here as it is already included
    mh=PF$market_harvest
    mp=PF$market_price
    # Effective price
    if(mp[1]==mp[2]){
      rp=mp[1]
    }else{
      rp=max(mp)
    }
    for (i in 1:2){
      for (j in 1:2){
        PF=profitHarvest(rp,marg.cost[i,j],alpha[i],beta[i,j],TAC,0)
        sh[i,j]  = PF$harvest
        spi[i,j] = PF$profit
      } # Gotta have both values before estimating the min
      for(j in 1:2){
        rr[i,j]=min(spi[i,]) # Resource rent
        ir[i,j]=spi[i,j]-rr[i,j] # Inframarginal rent
      }
    }
  }else if(m==3){
    A=1/sum(1/marg.cost) # Agg marg cost
    PF=priceFunction(a,b,TAC,A)
    mh=PF$market_harvest
    mp=PF$market_price
    # Effective price
    if(mp[1]==mp[2]){
      rp=mp[1]
    }else{
      rp=max(mp)
    }
    A=sum(1/marg.cost)
    tau=max(rp-TAC/A,0) # Price of quota
    for (i in 1:2){
      for (j in 1:2){
        PF=profitHarvest(rp,marg.cost[i,j],alpha[i],beta[i,j],TAC,tau)
        sh[i,j]  = PF$harvest
        spi[i,j] = PF$profit
      }
    }
    rr=tau*sh # Resource rent
    ir=spi-rr # Inframarginal rent
  }
  vs=rr+ir
  vf=rowSums(vs)
  V=sum(vf)
  return(list(market_harvest=mh,
              market_price=mp,
              sector_harvest=sh,
              sector_profit=spi,
              v_sector=vs,
              v_fleet=vf,
              v_fish=V)
         )
}

###############################
# Labor estimation 
###############################
laborEstimation<-function(h,cap,crew){
  # 200 labor days / labor in thousands
  L=(h*10^6/cap/200*crew)/1000
  return(L)
}
###############################
# Demand simulator
###############################

demandSimulator<-function(a,b){
  Captura=seq(0,20,.01)
  CHD=a[1]-b[1]*Captura
  HP=a[2]-b[2]*Captura
  D=data.frame(Captura,CHD,HP)
  D=D[D[,2]>0,]
  D=D[D[,3]>0,]
  return(D)
}

###############################
# Artisanal Processing
###############################
#VARIABLES NEEDED

#Efficiency rates ####
EF_FM= 1/4.5 #Amount that goes to FM market (50%) AND efficiency (100% right now) FUENTE: Matías
EF_FO= 1/7 #Amount that goes to FO market (50%) AND efficiency (100% right now) FUENTE: Matías
EF_RFM= 1/7 #Residual FM efficiency (Matias)
EF_RFO= 1/7 #Residual Oil efficiency (Matias)
EF_PC= 0.3 #Production DHC processing (canning) (Avadi, 2014) (Avadi, 2015 has a higher (50%))
EF_PF= 0.75 #Production DHC processing (freezing) (Avadi, 2015) Gutted
EF_PU= 0.68 #Production DHC processing (curing) (Avadi, 2014) (Avadi, 2015 has a mutch lower (19%))
EF_PS= 0.92 #Production DHC processing (salted/fresh) (Avadi, 2014)

#Labor values per product (ratio)
#Labor for Fish Meal and Fish Oil are in line ~440
LR_C= 8032/95589 #Total labor / total production (Avadi, 2015) #Considera que solo el 50% corresponde a anchoveta por eso la diferencia
  #16063/191177 #Total labor / total production (Chirstensen) 
LR_F= 1827/43985 #Total labor / total production (Avadi, 2015) #Considera que solo el 10% corresponde a anchoveta por eso la diferencia
  #18267/439851 #Total labor / total production (Chirstensen)
LR_U= 2515/9772 #Total labor / total production (Avadi, 2015)
  #2515/9772 #Total labor / total production (Chirstensen)
LR_S= 338/3450 #Total labor / total production (Avadi, 2015)
  #338/3450 #Total labor / total production (Chirstensen)

#Revenues per product (ratio)
RR_C= 101224000/95589 #Total revenue / total production (Avadi, 2015)
  #248965000/191177 #Total revenue / total production (Chirstensen)
RR_F= 81006000/43985 #Total revenue / total production (Avadi, 2015)
  #810063000/439851 #Total revenue / total production (Chirstensen)
RR_U= 24909000/9772 #Total revenue / total production (Avadi, 2015)
  #26579000/9772 #Total revenue / total production (Chirstensen)
RR_S= 13370000/3450 #Total revenue / total production (Avadi)
  #13370000/3450 #Total revenue / total production (Chirstensen)
  
#Costs per product (ratio)
CR_C= 78955000/95589 #Total cost / total production (Avadi, 2015)
  #155112000/191177 #Total cost / total production (Chirstensen)
CR_F= 66318000/43985 #Total cost / total production (Avadi, 2015)
  #663176000/439851 #Total cost / total production (Chirstensen)
CR_U= 17492000/9772 #Total cost / total production (Avadi, 2015)
  #13208000/9772 #Total cost / total production (Chirstensen)
CR_S= 8815000/3450 #Total cost / total production (Avadi, 2015)
  #8815000/3450 #Total cost / total production (Chirstensen)


# References #

#Avadí, Á., Fréon, P., & Quispe, I. (2014). Environmental assessment of Peruvian anchoveta food products: is less refined better? The International Journal of Life Cycle Assessment, 19(6), 1276–1293. http://doi.org/10.1007/s11367-014-0737-y

# Avadí, Á., & Fréon, P. (2015). A set of sustainability performance indicators for seafood: Direct human consumption products from Peruvian anchoveta fisheries and freshwater aquaculture. Ecological Indicators, 48, 518–532. http://doi.org/10.1016/j.ecolind.2014.09.006

#Christensen, V., la Puente, de, S., Sueiro, J. C., Steenbeek, J., & Majluf, P. (2014). Valuing seafood_ The Peruvian fisheries sector. Marine Policy, 44(C), 302–311. http://doi.org/10.1016/j.marpol.2013.09.022



ProcessingOutputArt=function(H_DHC,pctg=T,disc1=10,disc2=40){

  if(pctg){ProductShare=c(.791,.102,.103,.004)}else{ProductShare=c(.25,.25,.25,.25)} #This leaves only two options for the product share. Current product share or even split (25% per process)
  
  DHC_ProductAmount=ProductShare*H_DHC
  
  DHC_Plant=((100-disc1)/100)*DHC_ProductAmount # DHC directed toward Plant
  
  DHC_Discard=list()
  DHC_Discard[["Plant"]]=DHC_ProductAmount*(disc1/100) #DHC directed toward fish meal (discarded)
  
  DHC_Production=((100-disc2)/100)*DHC_Plant #Plant output directed toward production (60% is typical)
  
  DHC_Discard[["Production"]]=(disc2/100)*DHC_Plant #Production directed toward fish meal (40% is usually discarded)
  
  ProcessingEfficiency=c(EF_PC,EF_PF,EF_PU,EF_PS) # Canned, frozen, curred, salted rates of undiscarded for each industry
  
  DHC_FinalProduct=(ProcessingEfficiency)*DHC_Production #The final product amount that makes it through the respective type of processing
  
  DHC_Discard[["FinalProduct"]]=((1-ProcessingEfficiency)*DHC_Production)*EF_RFM #product discard- low quality towards fish meal (1/7th by weight goes to low quality fishmeal)
  
  DHC_Fishmeal=(DHC_Discard$Plant+DHC_Discard$Production)*EF_FM #amount of fish meal produced (high quality fish meal)
  
  DHC_Aceite=(DHC_Discard$Plant+DHC_Discard$Production)*EF_FO #should be less than Fishmeal (missing the value for it)-- Is it the same number? 
  
  DHC_Employment=c(LR_C,LR_F,LR_U,LR_S) #Per ton. 
  
  DHC_Revenue=c(RR_C,RR_F,RR_U,RR_S) #In freezing, estimates are 10% according to Freon
  
  DHC_Cost=c(CR_C,CR_F,CR_U,CR_S) # In freezing, estimates are 10% according to Freon
  
  DataOutput=data.frame(matrix(data=rbind(DHC_FinalProduct,
                                          DHC_Aceite,
                                          DHC_Fishmeal,
                                          DHC_Discard$FinalProduct,
                                          DHC_FinalProduct*DHC_Employment,
                                          DHC_FinalProduct*DHC_Revenue,
                                          DHC_FinalProduct*DHC_Cost,
                                          DHC_FinalProduct*(DHC_Revenue-DHC_Cost)),
                               nrow=8,ncol=4))
  names(DataOutput)=c("Canning","Freezing","Curing","Salted")
  row.names(DataOutput)=c("DHC_Production (t)","DHC_Aceite (t)","DHC_Fishmeal (t)","DHC_LQ_Fishmeal (t)","DHC_Employment (# of people)","DHC_Revenue (USD$)","DHC_Cost (USD$)","DHC_Profit (USD$)")
    
  return(DataOutput)
}
###############################
# One time evaluation
###############################

#a,b,marg.cost,TAC,alpha,beta,m

singleEvaluation<-function(a,b,marg.cost,TAC,alpha,beta,cap,crew,pctg=T,disc1=10,disc2=40){
  
  # Empty variables: 
  R=list()      # General list with results
  H_DHC=list()  # Harvest directed toward DHC
  H_FM=list()   # Harvest directed toward FM
  H_F=list()    # Harvest total from fishing
  P_DHC=list()  # Price DHC
  P_FM=list()   # Price FM
  H_AS=list()   # Harvest small scale artisanal
  H_AL=list()   # Harvest large scale artisanal
  H_IS=list()   # Harvest small scale industrial
  H_IL=list()   # Harvest large scale industrial
  PI_AS=list()  # Profit small scale artisanal
  PI_AL=list()  # Profit large scale artisanal
  PI_IS=list()  # Profit small scale industrial
  PI_IL=list()  # Profit large scale industrial
  V_AS=list()   # Value small scale artisanal
  V_AL=list()   # Value large scale artisanal
  V_IS=list()   # Value small scale industrial
  V_IL=list()   # Value large scale industrial 
  V_A=list()    # Value artisanal
  V_I=list()    # Value industrial
  V_F=list()    # Value fishery
  L_AS=list()   # Labor small scale artisanal
  L_AL=list()   # Labor large scale artisanal
  L_IS=list()   # Labor small scale industrial
  L_IL=list()   # Labor large scale industrial
  L_A=list()    # Labor artisanal
  L_I=list()    # Labor industrial
  L_F=list()    # Labor fishery
  PR_PC=list()  # *Production DHC processing (canning)
  PR_PF=list()  # *Production DHC processing (freezing)
  PR_PU=list()  # *Production DHC processing (curing)
  PR_PS=list()  # *Production DHC processing (salted/fresh)
  PI_PC=list()  # *Profit DHC processing (canning)
  PI_PF=list()  # *Profit DHC processing (freezing)
  PI_PU=list()  # *Profit DHC processing (curing)
  PI_PS=list()  # *Profit DHC processing (salted/fresh)
  PR_DL=list()  # *Discard low quality DHC processing
  PR_DH=list()  # *Discard high quality fishmeal DHC processing
  PR_DA=list()  # *Discard high quality aceite DHC processing
  PI_DL=list()  # *Discard low quality DHC profit
  PI_DH=list()  # *Discard high quality DHC fishmeal profit
  PI_DA=list()  # *Discard high quality DHC aceite profit
  L_DL=list()   # *Discard low quality DHC labor
  L_DH=list()   # *Discard high quality fishmeal DHC labor
  L_DA=list()   # *Discard high quality aceite DHC labor
  L_PC=list()   # *Labor DHC processing (canning)
  L_PF=list()   # *Labor DHC processing (freezing)
  L_PU=list()   # *Labor DHC processing (curing)
  L_PS=list()   # *Labor DHC processing (salted/fresh)
  PR_FM=list()  # *Production of Fish Meal 
  PR_FO=list()  # *Production of Fish Oil
  PI_FM=list()  # *Profit of Fish Meal 
  PI_FO=list()  # *Profit of Fish Oil
  L_FM=list()   # *Labor of Fish Meal 
  L_FO=list()   # *Labor of Fish Oil
  PR_P=list()   # *Total Production (processing)
  L_P=list()    # *Total Labor (processing)
  PI_P=list()   # *Total Profit (processing)
  
  # Provide names for the scenarios
  Escenario=c('Status Quo','Cuota MC','Cuota MA','Transferible')
  # Assign the respective results
  for (k in 1:4){
    R[[k]]=simulateMarkets(a,b,marg.cost,TAC,alpha,beta,k-1)
    H_DHC[[k]]=R[[k]][[1]][1]
    H_FM[[k]]=R[[k]][[1]][2]
    H_F[[k]]=H_DHC[[k]]+H_FM[[k]]
    P_DHC[[k]]=R[[k]][[2]][1]
    P_FM[[k]]=R[[k]][[2]][2]
    H_AS[[k]]=R[[k]][[3]][1,1]
    H_AL[[k]]=R[[k]][[3]][1,2]
    H_IS[[k]]=R[[k]][[3]][2,1]
    H_IL[[k]]=R[[k]][[3]][2,2]
    PI_AS[[k]]=R[[k]][[4]][1,1]
    PI_AL[[k]]=R[[k]][[4]][1,2]
    PI_IS[[k]]=R[[k]][[4]][2,1]
    PI_IL[[k]]=R[[k]][[4]][2,2]
    V_AS[[k]]=R[[k]][[5]][1,1]
    V_AL[[k]]=R[[k]][[5]][1,2]
    V_IS[[k]]=R[[k]][[5]][2,1]
    V_IL[[k]]=R[[k]][[5]][2,2]
    V_A[[k]]=R[[k]][[6]][1]
    V_I[[k]]=R[[k]][[6]][2]
    V_F[[k]]=R[[k]][[7]]
    L_AS[[k]]=laborEstimation(H_AS[[k]],cap[1,2],crew[1,2])
    L_AL[[k]]=laborEstimation(H_AL[[k]],cap[1,1],crew[1,1])
    L_IS[[k]]=laborEstimation(H_IS[[k]],cap[2,2],crew[2,2])
    L_IL[[k]]=laborEstimation(H_IL[[k]],cap[2,1],crew[2,1])
    L_A[[k]]=L_AS[[k]]+L_AL[[k]]
    L_I[[k]]=L_IS[[k]]+L_IL[[k]]
    L_F[[k]]=L_A[[k]]+L_I[[k]]
    Processing=ProcessingOutputArt(H_DHC[[k]],pctg,disc1,disc2)
    PR_PC[[k]]=Processing[1,1]
    PR_PF[[k]]=Processing[1,2]
    PR_PU[[k]]=Processing[1,3]
    PR_PS[[k]]=Processing[1,4]
    PI_PC[[k]]=Processing[8,1]
    PI_PF[[k]]=Processing[8,2]
    PI_PU[[k]]=Processing[8,3]
    PI_PS[[k]]=Processing[8,4]
    PR_DL[[k]]=sum(Processing[4,])
    PR_DH[[k]]=sum(Processing[3,])
    PR_DA[[k]]=sum(Processing[2,])
    PI_DL[[k]]=sum(Processing[4,])* 2370
    PI_DH[[k]]=sum(Processing[3,])* 2370
    PI_DA[[k]]=sum(Processing[2,])* 2370
    L_DL[[k]]=sum(Processing[4,])*(604/136585) #THIS NUMBER WILL CHANGE
    L_DH[[k]]=sum(Processing[3,])*1/100 #THIS NUMBER WILL CHANGE
    L_DA[[k]]=sum(Processing[2,])*1/100 #THIS NUMBER WILL CHANGE
    L_PC[[k]]=Processing[5,1]
    L_PF[[k]]=Processing[5,2]
    L_PU[[k]]=Processing[5,3]
    L_PS[[k]]=Processing[5,4]
    PR_FM[[k]]=H_FM[[k]]*EF_FM   
    PR_FO[[k]]=H_FM[[k]]*EF_FO
    PI_FM[[k]]=PR_FM[[k]]* 2370
    PI_FO[[k]]=PR_FO[[k]]* 2370
    
    #### NOTA JULIANO - 29/Jun ####
    # Por alguna razón el fish meal ratio (las siguientes dos líneas) estaban comentadas y no activas. Los datos vienen de:
    #Christensen, V., la Puente, de, S., Sueiro, J. C., Steenbeek, J., & Majluf, P. (2014). Valuing seafood_ The Peruvian fisheries sector. Marine Policy, 44(C), 302–311. http://doi.org/10.1016/j.marpol.2013.09.022
    
    L_FM[[k]]=PR_FM[[k]]*(12550/1617497)  #Christensen V.
    L_FO[[k]]=PR_FO[[k]]*(1/100)   #THIS NUMBER WILL CHANGE, for now...
    ##########################
    
    PR_P[[k]]=PR_PC[[k]]+PR_PF[[k]]+PR_PU[[k]]+PR_PS[[k]]+PR_FM[[k]]+PR_FO[[k]]+PR_DA[[k]]+PR_DH[[k]]+PR_DL[[k]]
    L_P[[k]]=L_PC[[k]]+L_PF[[k]]+L_PU[[k]]+L_PS[[k]]+L_FM[[k]]+L_FO[[k]]+L_DA[[k]]+L_DH[[k]]+L_DL[[k]]
    PI_P[[k]]=PI_PC[[k]]+PI_PF[[k]]+PI_PU[[k]]+PI_PS[[k]]+PI_FM[[k]]+PI_FO[[k]]+PI_DA[[k]]+PI_DH[[k]]+PI_DL[[k]]
  }
  # Stack values as a data frame
  DB.CURRENT=data.frame(
    Harvest_DHC=c(H_DHC[[1]],H_DHC[[2]],H_DHC[[3]],H_DHC[[4]]),
    Harvest_FM=c(H_FM[[1]],H_FM[[2]],H_FM[[3]],H_FM[[4]]),
    Harvest_F=c(H_F[[1]],H_F[[2]],H_F[[3]],H_F[[4]]),
    Price_DHC=c(P_DHC[[1]],P_DHC[[2]],P_DHC[[3]],P_DHC[[4]]),
    Price_FM=c(P_FM[[1]],P_FM[[2]],P_FM[[3]],P_FM[[4]]),
    Harvest_AS=c(H_AS[[1]],H_AS[[2]],H_AS[[3]],H_AS[[4]]),
    Harvest_AL=c(H_AL[[1]],H_AL[[2]],H_AL[[3]],H_AL[[4]]),
    Harvest_IS=c(H_IS[[1]],H_IS[[2]],H_IS[[3]],H_IS[[4]]),
    Harvest_IL=c(H_IL[[1]],H_IL[[2]],H_IL[[3]],H_IL[[4]]),
    Value_AS=c(V_AS[[1]],V_AS[[2]],V_AS[[3]],V_AS[[4]]),
    Value_AL=c(V_AL[[1]],V_AL[[2]],V_AL[[3]],V_AL[[4]]),
    Value_IS=c(V_IS[[1]],V_IS[[2]],V_IS[[3]],V_IS[[4]]),
    Value_IL=c(V_IL[[1]],V_IL[[2]],V_IL[[3]],V_IL[[4]]),
    Value_A=c(V_A[[1]],V_A[[2]],V_A[[3]],V_A[[4]]),
    Value_I=c(V_I[[1]],V_I[[2]],V_I[[3]],V_I[[4]]),
    Value_F=c(V_F[[1]],V_F[[2]],V_F[[3]],V_F[[4]]),
    Labor_AS=c(L_AS[[1]],L_AS[[2]],L_AS[[3]],L_AS[[4]]),
    Labor_AL=c(L_AL[[1]],L_AL[[2]],L_AL[[3]],L_AL[[4]]),
    Labor_IS=c(L_IS[[1]],L_IS[[2]],L_IS[[3]],L_IS[[4]]),
    Labor_IL=c(L_IL[[1]],L_IL[[2]],L_IL[[3]],L_IL[[4]]),
    Labor_A=c(L_A[[1]],L_A[[2]],L_A[[3]],L_A[[4]]),
    Labor_I=c(L_I[[1]],L_I[[2]],L_I[[3]],L_I[[4]]),
    Labor_F=c(L_A[[1]]+L_I[[1]],L_A[[2]]+L_I[[2]],L_A[[3]]+L_I[[3]],L_A[[4]]+L_I[[4]]),
    Production_PC=c(PR_PC[[1]],PR_PC[[2]],PR_PC[[3]],PR_PC[[4]]),
    Production_PF=c(PR_PF[[1]],PR_PF[[2]],PR_PF[[3]],PR_PF[[4]]),
    Production_PU=c(PR_PU[[1]],PR_PU[[2]],PR_PU[[3]],PR_PU[[4]]),
    Production_PS=c(PR_PS[[1]],PR_PS[[2]],PR_PS[[3]],PR_PS[[4]]),
    Profit_PC=c(PI_PC[[1]],PI_PC[[2]],PI_PC[[3]],PI_PC[[4]]),
    Profit_PF=c(PI_PF[[1]],PI_PF[[2]],PI_PF[[3]],PI_PF[[4]]),
    Profit_PU=c(PI_PU[[1]],PI_PU[[2]],PI_PU[[3]],PI_PU[[4]]),
    Profit_PS=c(PI_PS[[1]],PI_PS[[2]],PI_PS[[3]],PI_PS[[4]]),
    Production_DL=c(PR_DL[[1]],PR_DL[[2]],PR_DL[[3]],PR_DL[[4]]),
    Production_DH=c(PR_DH[[1]],PR_DH[[2]],PR_DH[[3]],PR_DH[[4]]),
    Production_DA=c(PR_DA[[1]],PR_DA[[2]],PR_DA[[3]],PR_DA[[4]]),
    Profit_DL=c(PI_DL[[1]],PI_DL[[2]],PI_DL[[3]],PI_DL[[4]]),
    Profit_DH=c(PI_DH[[1]],PI_DH[[2]],PI_DH[[3]],PI_DH[[4]]),
    Profit_DA=c(PI_DA[[1]],PI_DA[[2]],PI_DA[[3]],PI_DA[[4]]),
    Labor_DL=c(L_DL[[1]],L_DL[[2]],L_DL[[3]],L_DL[[4]]),
    Labor_DH=c(L_DH[[1]],L_DH[[2]],L_DH[[3]],L_DH[[4]]),
    Labor_DA=c(L_DA[[1]],L_DA[[2]],L_DA[[3]],L_DA[[4]]),
    Labor_PC=c(L_PC[[1]],L_PC[[2]],L_PC[[3]],L_PC[[4]]),
    Labor_PF=c(L_PF[[1]],L_PF[[2]],L_PF[[3]],L_PF[[4]]),
    Labor_PU=c(L_PU[[1]],L_PU[[2]],L_PU[[3]],L_PU[[4]]),
    Labor_PS=c(L_PS[[1]],L_PS[[2]],L_PS[[3]],L_PS[[4]]),
    Production_FM=c(PR_FM[[1]],PR_FM[[2]],PR_FM[[3]],PR_FM[[4]]),
    Production_FO=c(PR_FO[[1]],PR_FO[[2]],PR_FO[[3]],PR_FO[[4]]),
    Profit_FM=c(PI_FM[[1]],PI_FM[[2]],PI_FM[[3]],PI_FM[[4]]),
    Profit_FO=c(PI_FO[[1]],PI_FO[[2]],PI_FO[[3]],PI_FO[[4]]),
    Labor_FM=c(L_FM[[1]],L_FM[[2]],L_FM[[3]],L_FM[[4]]),
    Labor_FO=c(L_FO[[1]],L_FO[[2]],L_FO[[3]],L_FO[[4]]),
    Production_P=c(PR_P[[1]],PR_P[[2]],PR_P[[3]],PR_P[[4]]),
    Labor_P=c(L_P[[1]],L_P[[2]],L_P[[3]],L_P[[4]]),
    Profit_P=c(PI_P[[1]],PI_P[[2]],PI_P[[3]],PI_P[[4]]),
    Escenario=Escenario
  )
  DB.CURRENT$Escenario<-factor(DB.CURRENT$Escenario,levels = Escenario)
  # This thing is ready to go! =)
  return(DB.CURRENT)
}


###############################
# Tradeoff function
###############################

tradeoffFunction<-function(a,b,marg.cost,TAC,beta,cap,crew,maxalpha){
  
  # All the same variables but max alpha, this one caps the share that artisanals
  # can get
  # First let's build a sequence of arbitrary shares
  S=c(seq(0,maxalpha,.005)) # All alphas will be a function of this sequence
  N=length(S) # Number of simulations
  
  # We're gonna stack our original output data frame for each simulation wooohooo! a lot of fun.
  
  for(n in 1:N){
    alpha=c(S[n],1-S[n]) # Set the alpha for the nth simulation
    if(n==1){
      DB=singleEvaluation(a,b,marg.cost,TAC,alpha,beta,cap,crew) # Main data frame
    }else{
      TDB=singleEvaluation(a,b,marg.cost,TAC,alpha,beta,cap,crew) # Additional rows
      DB=rbind(DB,TDB) # Update this bad boy
    }
  }
  return(DB)
}

###############################
# Optimization (of course we need to optimize something)
###############################

# Objective function for optimization
objectiveFunction<-function(a,b,marg.cost,TAC,alpha.star,beta,gamma,m){
  # Variables are the same, except for gamma that is a weight for values
  alpha=c(alpha.star,1-alpha.star)
  R=simulateMarkets(a,b,marg.cost,TAC,alpha,beta,m)
  z=-(gamma*log(R$v_fleet[1])+(1-gamma)*log(R$v_fleet[2]))
  return(z)
}

# Optimization Routine
optimalAllocation<-function(a,b,marg.cost,TAC,beta,gamma,m){
  R=optimize(objectiveFunction, c(0, 1), tol = 0.0001, 
             a=a,
             b=b,
             marg.cost=marg.cost,
             TAC=TAC,
             beta=beta,
             gamma=gamma,
             m=m)
  alpha.star=R$minimum
  return(alpha.star)
}

# Optimization loop for the 3 last scenarios

optimizationLoop<-function(a,b,marg.cost,TAC,beta,gamma,cap,crew){
  alpha.star=array(NA,c(1,3))
  for (i in 1:3){
    alpha.star[i]=optimalAllocation(a,b,marg.cost,TAC,beta,gamma,i)
  }
  DB.OPTIMAL=optimalEvaluation(a,b,marg.cost,TAC,alpha.star,beta,cap,crew)
  return(DB.OPTIMAL)
}


###############################
# Optimized evaluation (Different alphas for each scenario)
###############################

#a,b,marg.cost,TAC,alpha,beta,m

optimalEvaluation<-function(a,b,marg.cost,TAC,alpha.star,beta,cap,crew){
  
  # Empty variables: 
  R=list()      # General list with results
  H_DHC=list()  # Harvest directed toward DHC
  H_FM=list()   # Harvest directed toward FM
  H_F=list()    # Harvest total from fishing
  P_DHC=list()  # Price DHC
  P_FM=list()   # Price FM
  H_AS=list()   # Harvest small scale artisanal
  H_AL=list()   # Harvest large scale artisanal
  H_IS=list()   # Harvest small scale industrial
  H_IL=list()   # Harvest large scale industrial
  PI_AS=list()  # Profit small scale artisanal
  PI_AL=list()  # Profit large scale artisanal
  PI_IS=list()  # Profit small scale industrial
  PI_IL=list()  # Profit large scale industrial
  V_AS=list()   # Value small scale artisanal
  V_AL=list()   # Value large scale artisanal
  V_IS=list()   # Value small scale industrial
  V_IL=list()   # Value large scale industrial 
  V_A=list()    # Value artisanal
  V_I=list()    # Value industrial
  V_F=list()    # Value fishery
  L_AS=list()   # Labor small scale artisanal
  L_AL=list()   # Labor large scale artisanal
  L_IS=list()   # Labor small scale industrial
  L_IL=list()   # Labor large scale industrial
  L_A=list()    # Labor artisanal
  L_I=list()    # Labor industrial
  L_F=list()    # Labor fishery
  PR_PC=list()  # *Production DHC processing (canning)
  PR_PF=list()  # *Production DHC processing (freezing)
  PR_PU=list()  # *Production DHC processing (curing)
  PR_PS=list()  # *Production DHC processing (salted/fresh)
  PI_PC=list()  # *Profit DHC processing (canning)
  PI_PF=list()  # *Profit DHC processing (freezing)
  PI_PU=list()  # *Profit DHC processing (curing)
  PI_PS=list()  # *Profit DHC processing (salted/fresh)
  PR_DL=list()  # *Discard low quality DHC processing
  PR_DH=list()  # *Discard high quality fishmeal DHC processing
  PR_DA=list()  # *Discard high quality aceite DHC processing
  PI_DL=list()  # *Discard low quality DHC profit
  PI_DH=list()  # *Discard high quality DHC fishmeal profit
  PI_DA=list()  # *Discard high quality DHC aceite profit
  L_DL=list()   # *Discard low quality DHC labor
  L_DH=list()   # *Discard high quality fishmeal DHC labor
  L_DA=list()   # *Discard high quality aceite DHC labor
  L_PC=list()   # *Labor DHC processing (canning)
  L_PF=list()   # *Labor DHC processing (freezing)
  L_PU=list()   # *Labor DHC processing (curing)
  L_PS=list()   # *Labor DHC processing (salted/fresh)
  PR_FM=list()  # *Production of Fish Meal 
  PR_FO=list()  # *Production of Fish Oil
  PI_FM=list()  # *Profit of Fish Meal 
  PI_FO=list()  # *Profit of Fish Oil
  L_FM=list()   # *Labor of Fish Meal 
  L_FO=list()   # *Labor of Fish Oil
  PR_P=list()   # *Total Production (processing)
  L_P=list()    # *Total Labor (processing)
  PI_P=list()   # *Total Profit (processing)
  
  # Provide names for the scenarios
  Escenario=c('Cuota MC','Cuota MA','Transferible')
  # Assign the respective results
  for (k in 1:3){
    alpha=c(alpha.star[k],1-alpha.star[k]) # Here we use our optimal vector
    R[[k]]=simulateMarkets(a,b,marg.cost,TAC,alpha,beta,k)
    H_DHC[[k]]=R[[k]][[1]][1]
    H_FM[[k]]=R[[k]][[1]][2]
    H_F[[k]]=H_FM[[k]]+H_DHC[[k]]
    P_DHC[[k]]=R[[k]][[2]][1]
    P_FM[[k]]=R[[k]][[2]][2]
    H_AS[[k]]=R[[k]][[3]][1,1]
    H_AL[[k]]=R[[k]][[3]][1,2]
    H_IS[[k]]=R[[k]][[3]][2,1]
    H_IL[[k]]=R[[k]][[3]][2,2]
    PI_AS[[k]]=R[[k]][[4]][1,1]
    PI_AL[[k]]=R[[k]][[4]][1,2]
    PI_IS[[k]]=R[[k]][[4]][2,1]
    PI_IL[[k]]=R[[k]][[4]][2,2]
    V_AS[[k]]=R[[k]][[5]][1,1]
    V_AL[[k]]=R[[k]][[5]][1,2]
    V_IS[[k]]=R[[k]][[5]][2,1]
    V_IL[[k]]=R[[k]][[5]][2,2]
    V_A[[k]]=R[[k]][[6]][1]
    V_I[[k]]=R[[k]][[6]][2]
    V_F[[k]]=R[[k]][[7]]
    L_AS[[k]]=laborEstimation(H_AS[[k]],cap[1,2],crew[1,2])
    L_AL[[k]]=laborEstimation(H_AL[[k]],cap[1,1],crew[1,1])
    L_IS[[k]]=laborEstimation(H_IS[[k]],cap[2,2],crew[2,2])
    L_IL[[k]]=laborEstimation(H_IL[[k]],cap[2,1],crew[2,1])
    L_A[[k]]=L_AS[[k]]+L_AL[[k]]
    L_I[[k]]=L_IS[[k]]+L_IL[[k]]
    L_F[[k]]=L_A[[k]]+L_I[[k]]
    Processing=ProcessingOutputArt(H_DHC[[k]])
    PR_PC[[k]]=Processing[1,1]
    PR_PF[[k]]=Processing[1,2]
    PR_PU[[k]]=Processing[1,3]
    PR_PS[[k]]=Processing[1,4]
    PI_PC[[k]]=Processing[8,1]
    PI_PF[[k]]=Processing[8,2]
    PI_PU[[k]]=Processing[8,3]
    PI_PS[[k]]=Processing[8,4]
    PR_DL[[k]]=sum(Processing[4,])
    PR_DH[[k]]=sum(Processing[3,])
    PR_DA[[k]]=sum(Processing[2,])
    PI_DL[[k]]=sum(Processing[4,]) *2370 #Valor de la tonelada de harina
    PI_DH[[k]]=sum(Processing[3,])*2370 #Valor de la tonelada de harina
    PI_DA[[k]]=sum(Processing[2,])*2370
    L_DL[[k]]=sum(Processing[4,])#
    L_DH[[k]]=sum(Processing[3,])#
    L_DA[[k]]=sum(Processing[2,])#
    L_PC[[k]]=Processing[5,1]
    L_PF[[k]]=Processing[5,2]
    L_PU[[k]]=Processing[5,3]
    L_PS[[k]]=Processing[5,4]
    PR_FM[[k]]=H_FM[[k]]*EF_FM # CHANGE THESE SAME AS ABOVE FUNCTION
    #### NOTA JULIANO - 29/Jun ####
    PR_FO[[k]]=H_FM[[k]]*EF_FO #
    PI_FM[[k]]=PR_FM[[k]]* 2370
    PI_FO[[k]]=PR_FO[[k]]* 2370
    L_FM[[k]]=PR_FM[[k]]*(12550/1617497)
    L_FO[[k]]=PR_FO[[k]]*(1/100)
    PR_P[[k]]=PR_PC[[k]]+PR_PF[[k]]+PR_PU[[k]]+PR_PS[[k]]+PR_FM[[k]]+PR_FO[[k]]+PR_DA[[k]]+PR_DH[[k]]+PR_DL[[k]]
    L_P[[k]]=L_PC[[k]]+L_PF[[k]]+L_PU[[k]]+L_PS[[k]]+L_FM[[k]]+L_FO[[k]]+L_DA[[k]]+L_DH[[k]]+L_DL[[k]]
    PI_P[[k]]=PI_PC[[k]]+PI_PF[[k]]+PI_PU[[k]]+PI_PS[[k]]+PI_FM[[k]]+PI_FO[[k]]+PI_DA[[k]]+PI_DH[[k]]+PI_DL[[k]]
  }
  # Stack values as a data frame
  DB.CURRENT=data.frame(
    Harvest_DHC=c(H_DHC[[1]],H_DHC[[2]],H_DHC[[3]]),
    Harvest_FM=c(H_FM[[1]],H_FM[[2]],H_FM[[3]]),
    Harvest_F=c(H_F[[1]],H_F[[2]],H_F[[3]]),
    Price_DHC=c(P_DHC[[1]],P_DHC[[2]],P_DHC[[3]]),
    Price_FM=c(P_FM[[1]],P_FM[[2]],P_FM[[3]]),
    Harvest_AS=c(H_AS[[1]],H_AS[[2]],H_AS[[3]]),
    Harvest_AL=c(H_AL[[1]],H_AL[[2]],H_AL[[3]]),
    Harvest_IS=c(H_IS[[1]],H_IS[[2]],H_IS[[3]]),
    Harvest_IL=c(H_IL[[1]],H_IL[[2]],H_IL[[3]]),
    Value_AS=c(V_AS[[1]],V_AS[[2]],V_AS[[3]]),
    Value_AL=c(V_AL[[1]],V_AL[[2]],V_AL[[3]]),
    Value_IS=c(V_IS[[1]],V_IS[[2]],V_IS[[3]]),
    Value_IL=c(V_IL[[1]],V_IL[[2]],V_IL[[3]]),
    Value_A=c(V_A[[1]],V_A[[2]],V_A[[3]]),
    Value_I=c(V_I[[1]],V_I[[2]],V_I[[3]]),
    Value_F=c(V_F[[1]],V_F[[2]],V_F[[3]]),
    Labor_AS=c(L_AS[[1]],L_AS[[2]],L_AS[[3]]),
    Labor_AL=c(L_AL[[1]],L_AL[[2]],L_AL[[3]]),
    Labor_IS=c(L_IS[[1]],L_IS[[2]],L_IS[[3]]),
    Labor_IL=c(L_IL[[1]],L_IL[[2]],L_IL[[3]]),
    Labor_A=c(L_A[[1]],L_A[[2]],L_A[[3]]),
    Labor_I=c(L_I[[1]],L_I[[2]],L_I[[3]]),
    Labor_F=c(L_A[[1]]+L_I[[1]],L_A[[2]]+L_I[[2]],L_A[[3]]+L_I[[3]]),
    Production_PC=c(PR_PC[[1]],PR_PC[[2]],PR_PC[[3]]),
    Production_PF=c(PR_PF[[1]],PR_PF[[2]],PR_PF[[3]]),
    Production_PU=c(PR_PU[[1]],PR_PU[[2]],PR_PU[[3]]),
    Production_PS=c(PR_PS[[1]],PR_PS[[2]],PR_PS[[3]]),
    Profit_PC=c(PI_PC[[1]],PI_PC[[2]],PI_PC[[3]]),
    Profit_PF=c(PI_PF[[1]],PI_PF[[2]],PI_PF[[3]]),
    Profit_PU=c(PI_PU[[1]],PI_PU[[2]],PI_PU[[3]]),
    Profit_PS=c(PI_PS[[1]],PI_PS[[2]],PI_PS[[3]]),
    Production_DL=c(PR_DL[[1]],PR_DL[[2]],PR_DL[[3]]),
    Production_DH=c(PR_DH[[1]],PR_DH[[2]],PR_DH[[3]]),
    Production_DA=c(PR_DA[[1]],PR_DA[[2]],PR_DA[[3]]),
    Profit_DL=c(PI_DL[[1]],PI_DL[[2]],PI_DL[[3]]),
    Profit_DH=c(PI_DH[[1]],PI_DH[[2]],PI_DH[[3]]),
    Profit_DA=c(PI_DA[[1]],PI_DA[[2]],PI_DA[[3]]),
    Labor_DL=c(L_DL[[1]],L_DL[[2]],L_DL[[3]]),
    Labor_DH=c(L_DH[[1]],L_DH[[2]],L_DH[[3]]),
    Labor_DA=c(L_DA[[1]],L_DA[[2]],L_DA[[3]]),
    Labor_PC=c(L_PC[[1]],L_PC[[2]],L_PC[[3]]),
    Labor_PF=c(L_PF[[1]],L_PF[[2]],L_PF[[3]]),
    Labor_PU=c(L_PU[[1]],L_PU[[2]],L_PU[[3]]),
    Labor_PS=c(L_PS[[1]],L_PS[[2]],L_PS[[3]]),
    Production_FM=c(PR_FM[[1]],PR_FM[[2]],PR_FM[[3]]),
    Production_FO=c(PR_FO[[1]],PR_FO[[2]],PR_FO[[3]]),
    Profit_FM=c(PI_FM[[1]],PI_FM[[2]],PI_FM[[3]]),
    Profit_FO=c(PI_FO[[1]],PI_FO[[2]],PI_FO[[3]]),
    Labor_FM=c(L_FM[[1]],L_FM[[2]],L_FM[[3]]),
    Labor_FO=c(L_FO[[1]],L_FO[[2]],L_FO[[3]]),
    Production_P=c(PR_P[[1]],PR_P[[2]],PR_P[[3]]),
    Labor_P=c(L_P[[1]],L_P[[2]],L_P[[3]]),
    Profit_P=c(PI_P[[1]],PI_P[[2]],PI_P[[3]]),
    Escenario=Escenario,
    alpha_art=c(alpha.star[1],alpha.star[2],alpha.star[3])
  )
  DB.CURRENT$Escenario<-factor(DB.CURRENT$Escenario,levels = Escenario)
  # This thing is ready to go! =)
  return(DB.CURRENT)
}
