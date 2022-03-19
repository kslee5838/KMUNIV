  
  # st_test (Stationary Test)
  library(zoo);library(dplyr);library(ggplot2);library(urca) ;library(psych)
  
  PPPData_edit2_raw <- read.csv("~/KMUNIV/PPPData_edit2_raw.csv")
  kmu1<-log(PPPData_edit2_raw[,-c(1:2,19,21)]) # PPPData 로그값  

  
  # Calculate number of rows and col
  rows<-length(kmu1[,1])
  cols<-length(kmu1[1,])
  #Remove header and save each column to a matrix
  for ( i in 1:rows){
    for ( j in 1:cols){
      if(kmu1[i,j]=="-Inf"){
        kmu1[i,j]=c(0)
      }
    }
  }
  
  index1<-seq(as.Date("2010-01-01"),as.Date("2018-09-01"),by="month") # 2010-01-01부터 2018-09-01  
  index2<-seq(as.Date("2009-12-01"),as.Date("2018-09-01"),by="month") # 2009-12-01부터 2018-09-01
  kmu2<-zoo(kmu1,index1)
  par(mfrow=c(1,2))
  plot(kmu2$kg_z31,col=c("blue"),lwd=2)
  
  ur.df(kmu2$kg_z31,type="none",lags=1)
  ur.df(diff(kmu2$kg_z31),type="none",lags=1)
  plot(diff(kmu2$kg_z31),col=c("green"),lwd=2) # 색깔은 1부터 6까지 ?   
  
  plot(cbind(kmu2$kg_z31,diff(kmu2$kg_z31)),screen=1,lty=c("dotted","solid"),col=c("red","blue"),
       xlab='z31',ylab='diff(z31)')
  legend(as.Date("2010-05-01"),4.5,c("z31","diff(z31)"),lty=c(2,1),col=c("red","blue"))
  
  # 15개, Korea PPP ####
  kmu_kor_cheapest <- read.csv("~/KMUNIV/kmu_kor_cheapest.csv",skip=2,header=T,stringsAsFactors = F)
  kmu_kor_competitiveness <- read.csv("~/KMUNIV/kmu_kor_competitiveness.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_cost <- read.csv("~/KMUNIV/kmu_kor_cost.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_CostofLiving<- read.csv("~/KMUNIV/kmu_kor_CostofLiving.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_CPI <- read.csv("~/KMUNIV/kmu_kor_CPI.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_Deflation <- read.csv("~/KMUNIV/kmu_kor_Deflation.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_ExchangeRate <- read.csv("~/KMUNIV/kmu_kor_ExchangeRate.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_Expensive <- read.csv("~/KMUNIV/kmu_kor_Expensive.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_FallingPrices <- read.csv("~/KMUNIV/kmu_kor_FallingPrices.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_Inflation <- read.csv("~/KMUNIV/kmu_kor_Inflation.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_PriceIndex <- read.csv("~/KMUNIV/kmu_kor_PriceIndex.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_RateofInflation <- read.csv("~/KMUNIV/kmu_kor_RateofInflation.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_RetailPriceIndex <- read.csv("~/KMUNIV/kmu_kor_RetailPriceIndex.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_RisingPrices <- read.csv("~/KMUNIV/kmu_kor_RisingPrices.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_SOBIJA <- read.csv("~/KMUNIV/kmu_kor_SOBIJA.csv", stringsAsFactors=FALSE,skip=2)
  
  # 16개, USA PPP #####
  kmu_usa_cheapest <- read.csv("~/KMUNIV/kmu_usa_cheapest.csv",skip=2,header=T,stringsAsFactors = F) 
  kmu_usa_competitiveness <- read.csv("~/KMUNIV/kmu_usa_competitiveness.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_cost <- read.csv("~/KMUNIV/kmu_usa_cost.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_CostofLiving<- read.csv("~/KMUNIV/kmu_usa_CostofLiving.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_CPI <- read.csv("~/KMUNIV/kmu_usa_CPI.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_Deflation <- read.csv("~/KMUNIV/kmu_usa_Deflation.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_ExchangeRate <- read.csv("~/KMUNIV/kmu_usa_ExchangeRate.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_Expensive <- read.csv("~/KMUNIV/kmu_usa_Expensive.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_FallingPrices <- read.csv("~/KMUNIV/kmu_usa_FallingPrices.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_Inflation <- read.csv("~/KMUNIV/kmu_usa_Inflation.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_PriceIndex <- read.csv("~/KMUNIV/kmu_usa_PriceIndex.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_RateofInflation <- read.csv("~/KMUNIV/kmu_usa_RateofInflation.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_RetailPriceIndex <- read.csv("~/KMUNIV/kmu_usa_RetailPriceIndex.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_RisingPrices <- read.csv("~/KMUNIV/kmu_usa_RisingPrices.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_CurrentInflation <- read.csv("~/KMUNIV/kmu_usa_CurrentInflation.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_SOBIJA <- read.csv("~/KMUNIV/kmu_usa_SOBIJA.csv", stringsAsFactors=FALSE,skip=2)
  
  # 19 개  Korea MM ####
  kmu_kor_MCash <- read.csv("~/KMUNIV/kmu_kor_MCash.csv",skip=2,header=T,stringsAsFactors = F) 
  kmu_kor_MCheckingAccount <- read.csv("~/KMUNIV/kmu_kor_MCheckingAccount.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MCheckCard <- read.csv("~/KMUNIV/kmu_kor_MCheckCard.csv", stringsAsFactors=FALSE,skip=2)
  # kmu_kor_MNeedCash<- read.csv("~/KMUNIV/kmu_kor_MNeedCash.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MBank <- read.csv("~/KMUNIV/kmu_kor_MBank.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MNeedJob <- read.csv("~/KMUNIV/kmu_kor_MNeedJob.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MResume <- read.csv("~/KMUNIV/kmu_kor_MResume.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MRecruitmentAgency <- read.csv("~/KMUNIV/kmu_kor_MRecruitmentAgency.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MInterview <- read.csv("~/KMUNIV/kmu_kor_MInterview.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MJobVacancy <- read.csv("~/KMUNIV/kmu_kor_MJobVacancy.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MBuyaStock <- read.csv("~/KMUNIV/kmu_kor_MBuyaStock.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MDonate <- read.csv("~/KMUNIV/kmu_kor_MDonate.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MSave <- read.csv("~/KMUNIV/kmu_kor_Msave.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MRestaurant <- read.csv("~/KMUNIV/kmu_kor_Mrestaurant.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MLuxury <- read.csv("~/KMUNIV/kmu_kor_MLuxury.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MInvest <- read.csv("~/KMUNIV/kmu_kor_MInvest.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MVacation<- read.csv("~/KMUNIV/kmu_kor_MVacation.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MSpendMoney<- read.csv("~/KMUNIV/kmu_kor_MSpendMoney.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MATM<- read.csv("~/KMUNIV/kmu_kor_MATM.csv", stringsAsFactors=FALSE,skip=2)
  kmu_kor_MNeedCredit<- read.csv("~/KMUNIV/kmu_kor_MNeedCredit.csv", stringsAsFactors=FALSE,skip=2)
  
  # 20 개  USA MM ####
  kmu_usa_MCash <- read.csv("~/KMUNIV/kmu_usa_MCash.csv",skip=2,header=T,stringsAsFactors = F) 
  kmu_usa_MCheckingAccount <- read.csv("~/KMUNIV/kmu_usa_MCheckingAccount.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MDebitCard <- read.csv("~/KMUNIV/kmu_usa_MDebitCard.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MNeedCash<- read.csv("~/KMUNIV/kmu_usa_MNeedCash.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MBank <- read.csv("~/KMUNIV/kmu_usa_MBank.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MNeedJob <- read.csv("~/KMUNIV/kmu_usa_MNeedJob.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MResume <- read.csv("~/KMUNIV/kmu_usa_MResume.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MRecruitmentAgency <- read.csv("~/KMUNIV/kmu_usa_MRecruitmentAgency.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MInterview <- read.csv("~/KMUNIV/kmu_usa_MInterview.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MJobVacancy <- read.csv("~/KMUNIV/kmu_usa_MJobVacancy.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MBuyaStock <- read.csv("~/KMUNIV/kmu_usa_MBuyaStock.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MDonate <- read.csv("~/KMUNIV/kmu_usa_MDonate.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MSave <- read.csv("~/KMUNIV/kmu_usa_Msave.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MRestaurant <- read.csv("~/KMUNIV/kmu_usa_Mrestaurant.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MLuxuary <- read.csv("~/KMUNIV/kmu_usa_MLuxuary.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MInvest <- read.csv("~/KMUNIV/kmu_usa_MInvest.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MVacation<- read.csv("~/KMUNIV/kmu_usa_MVacation.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MSpendMoney<- read.csv("~/KMUNIV/kmu_usa_MSpendMoney.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MATM<- read.csv("~/KMUNIV/kmu_usa_MATM.csv", stringsAsFactors=FALSE,skip=2)
  kmu_usa_MNeedCredit<- read.csv("~/KMUNIV/kmu_usa_MNeedCredit.csv", stringsAsFactors=FALSE,skip=2)
  
  kmu3<-cbind(kmu_kor_cheapest[,2],        # PPP Korea 원자료 cbind #### 
              kmu_kor_competitiveness[,2], 
              kmu_kor_cost[,2] ,
              kmu_kor_CostofLiving[,2],
              kmu_kor_CPI[,2], 
              kmu_kor_Deflation[,2],
              kmu_kor_ExchangeRate[,2],
              kmu_kor_Expensive [,2],
              kmu_kor_FallingPrices[,2] ,
              kmu_kor_Inflation[,2], 
              kmu_kor_PriceIndex[,2],
              kmu_kor_RateofInflation[,2] ,
              kmu_kor_RetailPriceIndex[,2],
              kmu_kor_RisingPrices[,2] ,
              kmu_kor_SOBIJA[,2])
 
  kmu4<-cbind(kmu_usa_cheapest[,2],kmu_usa_competitiveness[,2],kmu_usa_cost [,2],
              kmu_usa_CostofLiving[,2],kmu_usa_CPI [,2],kmu_usa_Deflation [,2],
              kmu_usa_ExchangeRate[,2],kmu_usa_Expensive [,2],
              kmu_usa_FallingPrices[,2],kmu_usa_Inflation[,2],
              kmu_usa_PriceIndex [,2],kmu_usa_RateofInflation[,2],kmu_usa_RetailPriceIndex[,2],
              kmu_usa_RisingPrices [,2],kmu_usa_CurrentInflation[,2],kmu_usa_SOBIJA [,2])
  
  kmu5<-cbind(kmu_usa_MCash[,2],
              kmu_kor_MCheckingAccount [,2],
              kmu_kor_MCheckCard[,2],
              kmu_kor_MBank[,2], 
              kmu_kor_MNeedJob[,2], 
              kmu_kor_MResume[,2], 
              kmu_kor_MRecruitmentAgency[,2],
              kmu_kor_MInterview[,2], 
              kmu_kor_MJobVacancy[,2],
              kmu_kor_MBuyaStock[,2], 
              kmu_kor_MDonate[,2], 
              kmu_kor_MSave[,2], 
              kmu_kor_MRestaurant[,2],
              kmu_kor_MLuxury[,2], 
              kmu_kor_MInvest[,2], 
              kmu_kor_MVacation[,2],
              kmu_kor_MSpendMoney[,2],
              kmu_kor_MATM[,2],
              kmu_kor_MNeedCredit[,2])
  
  
  kmu6<- cbind(kmu_usa_MCash[,2],kmu_usa_MCheckingAccount[,2],kmu_usa_MDebitCard [,2],
              kmu_usa_MNeedCash[,2],kmu_usa_MBank[,2], kmu_usa_MNeedJob[,2], 
              kmu_usa_MResume [,2], kmu_usa_MRecruitmentAgency[,2],
              kmu_usa_MInterview [,2],kmu_usa_MJobVacancy[,2],
              kmu_usa_MBuyaStock [,2],kmu_usa_MDonate [,2],
              kmu_usa_MSave[,2], kmu_usa_MRestaurant[,2],
              kmu_usa_MLuxuary [,2],kmu_usa_MInvest [,2],
              kmu_usa_MVacation[,2],kmu_usa_MSpendMoney[,2],
              kmu_usa_MATM[,2],kmu_usa_MNeedCredit[,2])
   
  kmu7<-cbind(kmu3,kmu4,kmu5,kmu6)
  
  kmu8<-log(kmu3)*100%>%zoo(index2)
  kmu9<-log(kmu4)*100%>%zoo(index2)
  kmu10<-log(kmu5)*100%>%zoo(index2)
  kmu11<-log(kmu6)*100%>%zoo(index2)
  
  
  ur.df(kmu8[,1],type="none",lags=1)
  ur.df(diff(kmu5[,1]),type="none",lags=1)
  par(mfrow=c(1,2))
  plot(kmu8[,1],col=c("blue"),lwd=2)
  plot(diff(kmu8[,1]),col=c("green"),lwd=2)
  
  #### kmu8(15개), kmu9(16개) kmu10(19개), kmu11(20개) 에서 -INF 가각 제거후 ur.df 각각 실행 ############## 
  #
            rows<-length(kmu8[,1])
            cols<-length(kmu8[1,])
            for ( i in 1:rows){
              for ( j in 1:cols){
                if(kmu8[i,j]=="-Inf"){
                  kmu8[i,j]=c(0)
                }
              }
            }
  
  rows<-length(kmu9[,1])
  cols<-length(kmu9[1,])
  for ( i in 1:rows){
    for ( j in 1:cols){
      if(kmu9[i,j]=="-Inf"){
        kmu9[i,j]=c(0)
                }
          }
  }
            
                rows<-length(kmu10[,1])
                cols<-length(kmu10[1,])
                for ( i in 1:rows){
                  for ( j in 1:cols){
                    if(kmu10[i,j]=="-Inf"){
                      kmu10[i,j]=c(0)
                    }
                  }
                }
                
    rows<-length(kmu11[,1])
    cols<-length(kmu11[1,])
          for ( i in 1:rows){
            for ( j in 1:cols){
              if(kmu11[i,j]=="-Inf"){
                 kmu11[i,j]=c(0)
                    }
                  }
                }
  
  
    for (i in 1:19){                            # ur.df(원자료) i의 갯수 15개, 16개, 19개,20개 각각체크  
              vi<-ur.df(kmu10[,i],type="none",lags=1)
              print(vi@teststat)
              print(vi@cval[1])
            }  
 print("pause")           
            for (i in 1:19){                    # ur.df테스트 (diff 자료로 변환 )   
              vii<-ur.df(diff(kmu10[,i]),type="none",lags=3)
              print(vii@teststat)
              print(vii@cval[1])
            }
  #
  #######여기까지 #################################################
  
 
 kmu13<-diff(kmu8)
 kmu14<-diff(kmu9)
 kmu15<-diff(kmu10)
 kmu16<-diff(kmu11)
 
 colnames(kmu8)<-c("z1","z2","z3",'z4','z5','z6','z7','z8','z9','z10','z11','z12','z13','z14','z15')
 colnames(kmu9)<-c("z21","z22","z23",'z24','z25','z26','z27','z28','z29','z30','z31','z32','z33','z34','z35','z36')
 colnames(kmu10)<-c("z41","z42","z43",'z44','z45','z46','z47','z48','z49','z50','z51','z52','z53','z54','z55',
                    'z56','z57','z58','z59')
 colnames(kmu11)<-c("z61","z62","z63",'z64','z65','z66','z67','z68','z69','z70','z71','z72','z73','z74',
                    'z75','z76','z77','z78','z79','z80')
 
  #### fa.pararell
  #### factanal
  #### fa

   par(mfrow=c(1,1))
   prcomp(diff(kmu8))%>%plot()
   factanal(diff(kmu8), 7,rotation="varimax",scores="regression") # 또는 rotation="promax"
   fa.parallel(diff(kmu9), fm = 'minres', fa = 'fa')              # Factoring method fm="minres" will do a minimum residual as will fm="uls"
   fa.parallel(diff(kmu9), fm = 'minres', fa = 'pc')              # fa='fa'또는 'pc'
   fa.parallel(diff(kmu11), fm = 'ml', fa = 'fa')                 # fm="ml" a maximum likelihood factor analysis
   fa.parallel(diff(kmu9), fm = 'ml', fa = 'pc')      # diff(kmu8) 7 factors, 4 factors,5 factors,4 factors
   fa(diff(kmu8),nfactors = 3,rotate = "oblimin",fm="minres") 
     
   factanal(diff(kmu9), 7,rotation="varimax",scores="regression")
   acf(diff(kmu8),lag.max =NULL,type=c("correlation"),plot=F,na.action=na.pass)

   kmu12<-diff(kmu8[,1])  # tryACF 탭의 오브젝트로 이용되고 있음.    

   kmu17<-factanal(diff(kmu8), 7,rotation="varimax",scores="regression")
   kmu18<-factanal(diff(kmu9), 4,rotation="varimax",scores="regression")
   kmu19<-factanal(diff(kmu10), 5,rotation="varimax",scores="regression")
   kmu20<-factanal(diff(kmu9), 4,rotation="varimax",scores="regression")

   kmu17_Factor1<-kmu17$scores[,"Factor1"]%>%zoo(index1)
   kmu17_Factor2<-kmu17$scores[,"Factor2"] 
   kmu17_Factor3<-kmu17$scores[,"Factor3"]
   kmu17_Factor4<-kmu17$scores[,"Factor4"]
   kmu17_Factor5<-kmu17$scores[,"Factor5"]
   kmu17_Factor6<-kmu17$scores[,"Factor6"]
   kmu17_Factor7<-kmu17$scores[,"Factor7"]
   
   kmu18_Factor1<-kmu18$scores[,"Factor1"]
   kmu18_Factor2<-kmu18$scores[,"Factor2"] 
   kmu18_Factor3<-kmu18$scores[,"Factor3"]
   kmu18_Factor4<-kmu18$scores[,"Factor4"]
   
   kmu19_Factor1<-kmu19$scores[,"Factor1"]
   kmu19_Factor2<-kmu19$scores[,"Factor2"] 
   kmu19_Factor3<-kmu19$scores[,"Factor3"]
   kmu19_Factor4<-kmu19$scores[,"Factor4"]
   kmu19_Factor5<-kmu19$scores[,"Factor5"]
   
   kmu20_Factor1<-kmu20$scores[,"Factor1"]
   kmu20_Factor2<-kmu20$scores[,"Factor2"] 
   kmu20_Factor3<-kmu20$scores[,"Factor3"]
   kmu20_Factor4<-kmu20$scores[,"Factor4"]
   
   kmu21<-append(PPPData_edit2_raw$CPI_KOR, 89.269, after=0)%>%zoo(index2)  # ur.df 테스트 위해서 전월 수치 필요함    
   kmu22<-append(PPPData_edit2_raw$CPI_USA, 215.949, after=0)%>%zoo(index2) # 상 동   
   kmu23<-append(PPPData_edit2_raw$DEXKOUS, 1162.5, after=0)%>%zoo(index2) 
   
   kmu24<-log(kmu21)*100%>%zoo(index2) # kmu21, 소비자물가지수 한국,ur.df 테스트 전   
   kmu25<-log(kmu22)*100%>%zoo(index2) # kmu22, 소비자물가지수 미국,ur.df 테스트 전  
   kmu26<-log(kmu23)*100%>%zoo(index2) # kmu23, 환율  
   
    for (i in 1:2){                    # ur.df테스트 (diff 자료로 변환 )->그런데 
      vii<-ur.df(diff(kmu24[,i]),type="none",lags=3)
      print(vii@teststat)
      print(vii@cval[1])
    }
   
    for (i in 1:2){                    
      vii<-ur.df(diff(kmu25[,i]),type="none",lags=3)
      print(vii@teststat)
      print(vii@cval[1])
    }
   
   for (i in 1:2){                    # ur.df테스트 (diff 자료로 변환 )     
     vii<-ur.df(diff(kmu26[,i]),type="none",lags=3)
     print(vii@teststat)
     print(vii@cval[1])
   }
   
   
   P_T <-kmu24                      # kmu24, 소비자물가지수 한국, diff(log(kmu21)*100)       
   P_T_STAR<-kmu25                  # kmu25, 소비자물가지수 미국  
   S_T<-kmu26                       # kmu26, 환율  
   Y_T<-zoo(PPPData_edit2_raw$Y,index1)         # change(환율) 즉 환율에 대한 log값의 diff 
   
  
   ########## diff(log) 인지 log(diff) 인지?? 후자인 경우 음수가 발생하여 diff 할 수 없음 
   ########## stationary 의 판별은 원자료가 있을때 이며, 로그값이 먼저 나와야 한다. diff 값을 깔아주고 
   ########## 로그를 하는 것이 맞지 않다고 여겨 김, 2019-04-07 (일요일) 오후 3:44
   
   cbind(kmu17_Factor1,kmu17_Factor2,kmu17_Factor3,kmu17_Factor4,kmu17_Factor5,kmu17_Factor6,kmu17_Factor7,
         P_T,P_T_STAR,S_T,Y_T,kmu18_Factor1,kmu18_Factor2,kmu18_Factor3,kmu18_Factor4)
   
   
   kmu27_rwNull<-data.frame(kmu27_y_t=c(1:105))
   kmu27_rwNull$kmu27_y_t <-Y_T
   kmu27_rwNull$kmu27_y_t_drift<-Y_T
   kmu27_rwNull$kmu27_obs <-Y_T
   kmu27_rwNull<-zoo(kmu27_rwNull,index1)
   
   library(forecast)
   kmu27_rwNull_driftless<-rwf(kmu27_rwNull$kmu27_obs[1:54,],h=51)
   kmu27_rwNull$kmu27_y_t[55:105]<-c(-0.83274) # No change
   kmu27_rwNull_drift<-rwf(kmu27_rwNull$kmu27_obs[1:54,],h=51,drift = T)
   kmu28_test<-summary(kmu27_rwNull_drift)
   kmu28_test$`Point Forecast`
   kmu27_rwNull$kmu27_y_t_drift[55:105]<-kmu28_test$`Point Forecast`
   
   #### PPP (1-1)####
   # load("~/FX/fff.RData")
   
   # kmu27_rwNull
   # kmu27_rwNull_drift
   # kmu27_rwNull_driftless     TELL DEFFERENCE????????????????????????? 
   