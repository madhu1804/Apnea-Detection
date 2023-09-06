{\rtf1\ansi\ansicpg1252\cocoartf2639
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;\f1\froman\fcharset0 Times-Roman;\f2\froman\fcharset0 TimesNewRomanPSMT;
}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red0\green0\blue0;\red255\green255\blue255;
}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0\c84706;\cssrgb\c0\c0\c0;\cssrgb\c100000\c100000\c100000;
}
\paperw11900\paperh16840\margl1440\margr1440\vieww28600\viewh18000\viewkind0
\deftab720
\pard\pardeftab720\partightenfactor0

\f0\fs32 \cf2 \expnd0\expndtw0\kerning0
library(readr) \cf3 \
\pard\pardeftab720\sl313\partightenfactor0

\f1 \cf3 \cb4 raindata <- read_csv("C:/Users/admin/Documents/raindata.csv")
\f2\fs29\fsmilli14667 \
\pard\pardeftab720\sl300\partightenfactor0

\f1\fs26\fsmilli13333 \cf3 \'a0
\f2\fs29\fsmilli14667 \
\pard\pardeftab720\partightenfactor0

\f0\fs37\fsmilli18667 \cf3 \cb1 View(raindata) 
\fs32 \

\fs37\fsmilli18667 s = sum(is.na(raindata)) 
\fs32 \

\fs37\fsmilli18667 raindata <- na.omit(raindata) -
\fs32 \

\fs37\fsmilli18667 x<-raindata[order(raindata$humidity),c(7,9)] 
\fs32 \
\pard\pardeftab720\sl399\sa213\partightenfactor0

\f2\fs37\fsmilli18667 \cf3 x
\fs29\fsmilli14667 \
\pard\pardeftab720\partightenfactor0

\f0\fs37\fsmilli18667 \cf3 y<-raindata[order(raindata$temparature),c(4,9)] 
\fs32 \

\fs37\fsmilli18667 y 
\fs32 \

\fs37\fsmilli18667 z<-raindata[order(raindata$pressure),c(2,9)] 
\fs32 \

\fs37\fsmilli18667 z 
\fs32 \

\fs37\fsmilli18667 p<-raindata[order(raindata$windspeed),c(12,9)] 
\fs32 \

\fs37\fsmilli18667 p 
\fs32 \

\fs37\fsmilli18667 q<-raindata[order(raindata$dewpoint),c(6,9)] 
\fs32 \

\fs37\fsmilli18667 q 
\fs32 \
\pard\pardeftab720\sl399\sa213\partightenfactor0

\f2\fs37\fsmilli18667 \cf3 r<-raindata[order(raindata$rainfall),c(9)]
\fs29\fsmilli14667 \
\pard\pardeftab720\partightenfactor0

\f0\fs37\fsmilli18667 \cf3 r 
\fs32 \

\fs37\fsmilli18667 hist(raindata$humidity,main="histogram of humidity", 
\fs32 \

\fs37\fsmilli18667 xlab = "Humidity", 
\fs32 \

\fs37\fsmilli18667 border="blue",col = "green", 
\fs32 \

\fs37\fsmilli18667 xlim = c(36,98),las=0, 
\fs32 \

\fs37\fsmilli18667 breaks = 10) 
\fs32 \

\fs37\fsmilli18667 hist(raindata$temparature,main="histogram of temparature", 
\fs32 \

\fs37\fsmilli18667 xlab = "temperature", 
\fs32 \

\fs37\fsmilli18667 border="blue",col = "green", 
\fs32 \

\fs37\fsmilli18667 freq = FALSE, 
\fs32 \

\fs37\fsmilli18667 xlim = c(4,33), 
\fs32 \

\fs37\fsmilli18667 las=1, 
\fs32 \
\pard\pardeftab720\sl399\sa213\partightenfactor0

\f2\fs37\fsmilli18667 \cf3 breaks = 10)
\fs29\fsmilli14667 \
\pard\pardeftab720\partightenfactor0

\f0\fs37\fsmilli18667 \cf3 hist(raindata$pressure,main="histogram of pressure", 
\fs32 \

\fs37\fsmilli18667 xlab = "pressure", 
\fs32 \

\fs37\fsmilli18667 border="blue",col = "green", 
\fs32 \

\fs37\fsmilli18667 xlim = c(998,1035), 
\fs32 \

\fs37\fsmilli18667 las=1, 
\fs32 \

\fs37\fsmilli18667 breaks = 10) 
\fs32 \

\fs37\fsmilli18667 hist(raindata$windspeed,main="histogram of windspeed", 
\fs32 \

\fs37\fsmilli18667 xlab = "windspeed", 
\fs32 \

\fs37\fsmilli18667 border="blue",col = "green", 
\fs32 \

\fs37\fsmilli18667 xlim = c(5,60), 
\fs32 \

\fs37\fsmilli18667 las=1, 
\fs32 \

\fs37\fsmilli18667 breaks = 10) 
\fs32 \

\fs37\fsmilli18667 hist(raindata$winddirection,main="histogram of winddirection", 
\fs32 \

\fs37\fsmilli18667 xlab = "winddirection", 
\fs32 \

\fs37\fsmilli18667 border="blue",col = "green", 
\fs32 \

\fs37\fsmilli18667 xlim = c(10,350), 
\fs32 \

\fs37\fsmilli18667 las=1, 
\fs32 \

\fs37\fsmilli18667 breaks = 10) 
\fs32 \

\fs37\fsmilli18667 library(ineq) 
\fs32 \

\fs37\fsmilli18667 g<-ineq(raindata$humidity,type="Gini") 
\fs32 \
\pard\pardeftab720\sl399\sa213\partightenfactor0

\f2\fs37\fsmilli18667 \cf3 g
\fs29\fsmilli14667 \
\pard\pardeftab720\partightenfactor0

\f0\fs37\fsmilli18667 \cf3 # The Gini coefficient can then be thought of as the ratio of the area that lies between the line of equality and the Lorenz curve (marked A in the diagram) over the total area under the line of equality (marked A and B in the diagram); 
\fs32 \

\fs37\fsmilli18667 plot(Lc(raindata$humidity),col="purple",lwd=2)#gini index 
\fs32 \

\fs37\fsmilli18667 h<-ineq(raindata$temparature,type="Gini") 
\fs32 \

\fs37\fsmilli18667 h 
\fs32 \

\fs37\fsmilli18667 plot(Lc(raindata$temparature),col="green",lwd=2)#gini index 
\fs32 \

\fs37\fsmilli18667 i<-ineq(raindata$dewpoint,type="Gini") 
\fs32 \

\fs37\fsmilli18667 i 
\fs32 \

\fs37\fsmilli18667 plot(Lc(raindata$dewpoint),col="blue",lwd=2)#gini index 
\fs32 \

\fs37\fsmilli18667 j<-ineq(raindata$windspeed,type="Gini") 
\fs32 \

\fs37\fsmilli18667 j 
\fs32 \

\fs37\fsmilli18667 plot(Lc(raindata$windspeed),col="pink",lwd=2)#gini index 
\fs32 \

\fs37\fsmilli18667 k<-ineq(raindata$pressure,type="Gini") 
\fs32 \

\fs37\fsmilli18667 k 
\fs32 \

\fs37\fsmilli18667 plot(Lc(raindata$pressure),col="orange",lwd=2)#gini index 
\fs32 \

\fs37\fsmilli18667 a<-ineq(raindata$humidity,type="entropy") 
\fs32 \

\fs37\fsmilli18667 a 
\fs32 \

\fs37\fsmilli18667 plot(Lc(raindata$humidity),col="purple",lwd=2) 
\fs32 \

\fs37\fsmilli18667 b<-ineq(raindata$temparature,type="entropy") 
\fs32 \

\fs37\fsmilli18667 b 
\fs32 \
\pard\pardeftab720\sl399\sa213\partightenfactor0

\f2\fs37\fsmilli18667 \cf3 plot(Lc(raindata$temparature),col="green",lwd=2)
\fs29\fsmilli14667 \
\pard\pardeftab720\partightenfactor0

\f0\fs37\fsmilli18667 \cf3 c<-ineq(raindata$dewpoint,type="entropy") 
\fs32 \

\fs37\fsmilli18667 c<-0.06 
\fs32 \

\fs37\fsmilli18667 plot(Lc(raindata$dewpoint),col="blue",lwd=2) 
\fs32 \

\fs37\fsmilli18667 d<-ineq(raindata$windspeed,type="entropy") 
\fs32 \

\fs37\fsmilli18667 d 
\fs32 \

\fs37\fsmilli18667 plot(Lc(raindata$windspeed),col="pink",lwd=2) 
\fs32 \

\fs37\fsmilli18667 e<-ineq(raindata$pressure,type="entropy") 
\fs32 \

\fs37\fsmilli18667 e 
\fs32 \

\fs37\fsmilli18667 plot(Lc(raindata$pressure),col="orange",lwd=2) 
\fs32 \

\fs37\fsmilli18667 x1<-c(g,h,i,j,k) 
\fs32 \

\fs37\fsmilli18667 x2<-c(a,b,c,d,e) 
\fs32 \

\fs37\fsmilli18667 plot(x1,type="o",col="blue",ylim=c(0.0,0.4)) 
\fs32 \

\fs37\fsmilli18667 par(new=TRUE) 
\fs32 \

\fs37\fsmilli18667 lines(x2,type="o",col="red") 
\fs32 \

\fs37\fsmilli18667 data<-raindata 
\fs32 \
\pard\pardeftab720\sl399\sa213\partightenfactor0

\f2\fs37\fsmilli18667 \cf3 str(data)
\fs29\fsmilli14667 \
\pard\pardeftab720\partightenfactor0

\f0\fs37\fsmilli18667 \cf3 data$rainfallf<-factor(data$rainfall) 
\fs32 \

\fs37\fsmilli18667 #To ensure all results are reproducable 
\fs32 \
\pard\pardeftab720\sl399\sa213\partightenfactor0

\f2\fs37\fsmilli18667 \cf3 set.seed(12)
\fs29\fsmilli14667 \
\pard\pardeftab720\partightenfactor0

\f0\fs37\fsmilli18667 \cf3 pd<-sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2)) 
\fs32 \

\fs37\fsmilli18667 train<-data[pd==1,] 
\fs32 \

\fs37\fsmilli18667 validate<-data[pd==2,] 
\fs32 \

\fs37\fsmilli18667 library(partykit) 
\fs32 \

\fs37\fsmilli18667 library(tree) 
\fs32 \

\fs37\fsmilli18667 library(party) 
\fs32 \

\fs37\fsmilli18667 tree1<-ctree(rainfallf~humidity+dewpoint+windspeed+sunshine,data=train) 
\fs32 \

\fs37\fsmilli18667 tree1 
\fs32 \

\fs37\fsmilli18667 #Conditional inference tree 
\fs32 \

\fs37\fsmilli18667 plot(tree1,main="Conditional Inference Tree") 
\fs32 \

\fs37\fsmilli18667 #table of prediction errors 
\fs32 \

\fs37\fsmilli18667 tabl<-table(predict(tree1), train$rainfallf) 
\fs32 \

\fs37\fsmilli18667 ctr<-sum(diag(tabl))/sum(tabl) 
\fs32 \

\fs37\fsmilli18667 ctr 
\fs32 \

\fs37\fsmilli18667 # Estimated class probabilities 
\fs32 \

\fs37\fsmilli18667 predict(tree1,validate,type="prob") 
\fs32 \

\fs37\fsmilli18667 predict(tree1,validate) 
\fs32 \

\fs37\fsmilli18667 raindata$rainfall <- ifelse(raindata$cloud < 88, 'no', 'yes') 
\fs32 \

\fs37\fsmilli18667 raindata$rainfall <- as.factor(raindata$rainfall) 
\fs32 \
\pard\pardeftab720\sl399\sa213\partightenfactor0

\f2\fs37\fsmilli18667 \cf3 table(raindata$rainfall)
\fs29\fsmilli14667 \
\pard\pardeftab720\partightenfactor0

\f0\fs37\fsmilli18667 \cf3 set.seed(123) 
\fs32 \

\fs37\fsmilli18667 samp <- sample(nrow(raindata), 0.6 * nrow(raindata)) 
\fs32 \

\fs37\fsmilli18667 train <- raindata[samp, ] 
\fs32 \

\fs37\fsmilli18667 test <- raindata[-samp, ] 
\fs32 \

\fs37\fsmilli18667 library(randomForest) 
\fs32 \

\fs37\fsmilli18667 model <- randomForest(rainfall ~ sunshine+temparature+windspeed+pressure+dewpoint+humidity - cloud, data = train) 
\fs32 \

\fs37\fsmilli18667 fit.rf <- randomForest(rainfall ~ sunshine+temparature+windspeed+pressure+dewpoint+humidity - cloud, data = train) 
\fs32 \

\fs37\fsmilli18667 print(fit.rf) 
\fs32 \

\fs37\fsmilli18667 importance(fit.rf) 
\fs32 \

\fs37\fsmilli18667 #plot(fit.rf) 
\fs32 \

\fs37\fsmilli18667 #plot( importance(fit.rf), lty=2, pch=16) 
\fs32 \

\fs37\fsmilli18667 #lines(importance(fit.rf)) 
\fs32 \

\fs37\fsmilli18667 varImpPlot(fit.rf,type=2) 
\fs32 \

\fs37\fsmilli18667 imp = importance(fit.rf) 
\fs32 \

\fs37\fsmilli18667 #take cloud as ref 
\fs32 \

\fs37\fsmilli18667 pred <- predict(model, newdata = test) 
\fs32 \

\fs37\fsmilli18667 pred 
\fs32 \

\fs37\fsmilli18667 tab=table(pred, test$rainfall) 
\fs32 \

\fs37\fsmilli18667 tab 
\fs32 \

\fs37\fsmilli18667 rnd<-sum(diag(tab))/sum(tab) 
\fs32 \

\fs37\fsmilli18667 rnd 
\fs32 \

\fs37\fsmilli18667 ctr 
\fs32 \
\pard\pardeftab720\sl399\sa213\partightenfactor0

\f2\fs37\fsmilli18667 \cf3 rnd
\fs29\fsmilli14667 \
}