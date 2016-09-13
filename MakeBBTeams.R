library(dplyr)
Scoring<-read.csv("TotalScoring.csv")
Teams<-group_by(Scoring,Team,Year)
Teams<-summarize(Teams,Own=mean(Own),Opp=mean(Opp),SDOwn=sd(Own),SDOpp=sd(Opp))

Teams$Own.CBM<-apply(Teams,1,function(y)
  counter.balanced.median(Scoring[Scoring$Year==y[2]& Scoring$Team== y[1],3]))
Teams$Opp.CBM<-apply(Teams,1,function(y)
  counter.balanced.median(Scoring[Scoring$Year==y[2]& Scoring$Team== y[1],4]))
WLData<-read.csv("WLData.csv")
names(WLData)[1:2]<-c("Year","Team")
Teams<-merge(Teams,WLData)
Teams$WP<-Teams$W/(Teams$W+Teams$L)
Teams$P1<-Teams$Own^1.87969970703125/(Teams$Own^1.87969970703125+Teams$Opp^1.87969970703125)
Teams$P2<-Teams$Own.CBM^1.52496337890625/(Teams$Own.CBM^1.52496337890625+Teams$Opp.CBM^1.52496337890625)
Teams$Dif<-Teams$Own-Teams$Own.CBM+Teams$Opp.CBM-Teams$Opp
x<-lm(WP~P1+Dif,Teams)
Teams$P3<-(Teams$Own^1.914306640625/(Teams$Own^1.914306640625+Teams$Opp^1.914306640625))*0.99871826171875-Teams$Dif*0.03375244140625


Teams$P4<-apply(Teams,1,function(y)
  ExpectedWins(Scoring[Scoring$Year==y[2]&Scoring$Team==y[1],3],
  Scoring[Scoring$Year==y[2]&Scoring$Team==y[1],4]))

write.csv(Teams,"Teams.csv")
