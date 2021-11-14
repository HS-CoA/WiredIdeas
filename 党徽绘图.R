#参考B站视频BVBV1tv411H7iT，up主是SuperSodaSea
library(tidyverse)
library(ggplot2)
library(ggforce)
r1=(3.5^2+16)^0.5
r2=(36+15.5^2)^0.5
d <- data.frame(x=c(0,0,32,32,28,32,7.5,18.5,3,16,12.5,11,16,16,16,16,1,10,
                       26.5,15.5,15.5,15.5,3.5,2.5,5,3,10.5,14.5,11),
                   y=c(32,0,0,32,0,4,14.5,25.5,19,28,32,27,16,32,0,18,8,16.5,
                       16.5,16.5,5.5,22,10.5,2.5,3,5,17.5,21.5,27))
c <- data.frame(Internationale=c(0), LeCommunisme=c(0))

ggplot(d)+
  geom_segment(data=c,aes(x=LeCommunisme,y=Internationale,xend=0,yend=0),colour="red")+
  geom_segment(data=d,aes(x=x[5],y=y[5],xend=x[6],yend=y[6]),colour="red")+
  geom_segment(data=d,aes(x=x[7],y=y[7],xend=x[9],yend=y[9]),colour="red")+
  geom_segment(data=d,aes(x=x[8],y=y[8],xend=x[10],yend=y[10]),colour="red")+
  geom_segment(data=d,aes(x=x[27],y=y[27],xend=x[5],yend=y[5]),colour="red")+
  geom_segment(data=d,aes(x=x[27],y=y[27],xend=x[7],yend=y[7]),colour="red")+
  geom_segment(data=d,aes(x=x[28],y=y[28],xend=x[6],yend=y[6]),colour="red")+
  geom_segment(data=d,aes(x=x[28],y=y[28],xend=x[8],yend=y[8]),colour="red")+
  geom_segment(data=d,aes(x=x[29],y=y[29],xend=x[9],yend=y[9]),colour="red")+
  geom_arc(aes(x0=12.5, y0=32, r=r1, start=2.422762653968337, end=3.446285307604803), 
           color="red")+
  geom_arc(aes(x0=16, y0=16, r=16, start=0, end=3.141592653589793), 
           color="red")+
  geom_arc(aes(x0=16, y0=18, r=18, start=3.141592653589793, end=4.22243165413014), 
           color="red")+
  geom_arc(aes(x0=10, y0=16.5, r=r2, start=0.37729097520288, end=1.570796326794897), 
           color="red")+
  geom_arc(aes(x0=15.5, y0=16.5, r=11.1207701385946, start=1.570796326794897, end=3.141592653589793), 
           color="red")+
  geom_arc(aes(x0=15.5, y0=22, r=16.5, start=3.141592653589793, end=3.989654732570766), 
           color="red")+
  geom_segment(data=d,aes(x=x[17],y=y[17],xend=x[23],yend=y[23]),colour="red")+
  geom_circle(aes(x0=2.5, y0=2.5, r=2.5), color="red")
  
#英特纳雄耐尔一定要实现！

