  
  # st_test (Stationary Test)
  library(zoo);library(dplyr);library(ggplot2);library(urca)
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
  
  kmu8<-log(kmu3)*100
  kmu9<-log(kmu4)*100
  kmu10<-log(kmu5)*100
  kmu11<-log(kmu6)*100
  
  
  ur.df(kmu8[,1],type="none",lags=1)
  ur.df(diff(kmu5[,1]),type="none",lags=1)
  par(mfrow=c(1,2))
  plot(kmu8[,1],col=c("blue"),lwd=2)
  plot(diff(kmu8[,1]),col=c("green"),lwd=2)
  
  #### kmu8(15개), kmu9(16개) kmu10(19개), kmu11(20개) 에서 -INF 가각 제거후 ur.df 각각 실행 ############## 
  
            rows<-length(kmu11[,1])
            cols<-length(kmu11[1,])
            for ( i in 1:rows){
            for ( j in 1:cols){
                if(kmu11[i,j]=="-Inf"){
                  kmu11[i,j]=c(0)
                }
              }
            }
  
  
            for (i in 1:20){                            # ur.df(원자료) i으ㅏ 갯수 15개, 16개, 19개,20개 가각체크  
              vi<-ur.df(kmu11[,i],type="none",lags=1)
              print(vi@teststat)
              print(vi@cval[1])
            }  
            
            for (i in 1:20){                            # ur.df테스트 (diff 자료로 변환 )   
              vii<-ur.df(diff(kmu11[,i]),type="none",lags=1)
              print(vii@teststat)
              print(vii@cval[1])
            }
  
  #######여기까지 #################################################
ffff