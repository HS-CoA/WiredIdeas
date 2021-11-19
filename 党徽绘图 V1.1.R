#参考B站视频BVBV1tv411H7iT，up主是SuperSodaSea
library(tidyverse)
library(ggplot2)
library(ggforce)
myarc1 <- function(xp,yp,x1,y1,x2,y2,x3,y3){
  #笔记：构建函数 my_function<-function(){}
  #目标函数：以A点(xp,yp)为圆心，B点(x1,y1)为圆弧起点画圆，与直线CD：C(x2,y2),
  #D(x3,y3)交与E点(x4,y4)和F(x5,y5)，使圆弧停于E点，取圆弧BE。
  #开始求圆弧和直线交点  
  k=((y2-y3)/(x2-x3))#计算CD斜率
  b=y2-(k*x2)#计算CD截距
  r=((x1-xp)^2+(y1-yp)^2)^0.5#计算圆弧对应完整圆的半径
  a=k^2+1
  b2=2*(b*k-k*yp-xp)
  c=xp^2+(b-yp)^2-r^2#标准一元二次方程求根
  delta=b2^2-4*a*c#验证是否有实根
  x4=(b2*(-1)+((delta)^0.5))/(2*a)
  x5=(b2*(-1)-((delta)^0.5))/(2*a)
  y4=k*x4+b
  y5=k*x5+b#求交点坐标
  xc=x5
  yc=y5
  #开始计算弧度来使用geom_circle绘图
  #计算点弧度
  de=acos((yc-yp)/r)
  if (xc>=xp)
    m=de
  if (xc<xp)
    m=((2*pi)-de)
  print(m)
}#直线与圆左边交点弧度
myarc2 <- function(xp,yp,x1,y1,x2,y2,x3,y3){
  #笔记：构建函数 my_function<-function(){}
  #目标函数：以A点(xp,yp)为圆心，B点(x1,y1)为圆弧起点画圆，与直线CD：C(x2,y2),
  #D(x3,y3)交与E点(x4,y4)和F(x5,y5)，使圆弧停于E点，取圆弧BE。
  #开始求圆弧和直线交点  
  k=((y2-y3)/(x2-x3))#计算CD斜率
  b=y2-(k*x2)#计算CD截距
  r=((x1-xp)^2+(y1-yp)^2)^0.5#计算圆弧对应完整圆的半径
  a=k^2+1
  b2=2*(b*k-k*yp-xp)
  c=xp^2+(b-yp)^2-r^2#标准一元二次方程求根
  delta=b2^2-4*a*c#验证是否有实根
  x4=(b2*(-1)+((delta)^0.5))/(2*a)
  x5=(b2*(-1)-((delta)^0.5))/(2*a)
  y4=k*x4+b
  y5=k*x5+b#求交点坐标
  xc=x4
  yc=y4
  #开始计算弧度来使用geom_circle绘图
  #计算点弧度
  de=acos((yc-yp)/r)
  if (xc>=xp)
    m=de
  if (xc<xp)
    m=((2*pi)-de)
  print(m)
}#直线与圆右边交点弧度
myx1 <- function(xp,yp,x1,y1,x2,y2,x3,y3){
  #笔记：构建函数 my_function<-function(){}
  #目标函数：以A点(xp,yp)为圆心，B点(x1,y1)为圆弧起点画圆，与直线CD：C(x2,y2),
  #D(x3,y3)交与E点(x4,y4)和F(x5,y5)，使圆弧停于E点，取圆弧BE。
  #开始求圆弧和直线交点  
  k=((y2-y3)/(x2-x3))#计算CD斜率
  b=y2-(k*x2)#计算CD截距
  r=((x1-xp)^2+(y1-yp)^2)^0.5#计算圆弧对应完整圆的半径
  a=k^2+1
  b2=2*(b*k-k*yp-xp)
  c=xp^2+(b-yp)^2-r^2#标准一元二次方程求根
  delta=b2^2-4*a*c#验证是否有实根
  x4=(b2*(-1)+((delta)^0.5))/(2*a)
  x5=(b2*(-1)-((delta)^0.5))/(2*a)
  y4=k*x4+b
  y5=k*x5+b#求交点坐标
  print(x5)
}
myy1 <- function(xp,yp,x1,y1,x2,y2,x3,y3){
  #笔记：构建函数 my_function<-function(){}
  #目标函数：以A点(xp,yp)为圆心，B点(x1,y1)为圆弧起点画圆，与直线CD：C(x2,y2),
  #D(x3,y3)交与E点(x4,y4)和F(x5,y5)，使圆弧停于E点，取圆弧BE。
  #开始求圆弧和直线交点  
  k=((y2-y3)/(x2-x3))#计算CD斜率
  b=y2-(k*x2)#计算CD截距
  r=((x1-xp)^2+(y1-yp)^2)^0.5#计算圆弧对应完整圆的半径
  a=k^2+1
  b2=2*(b*k-k*yp-xp)
  c=xp^2+(b-yp)^2-r^2#标准一元二次方程求根
  delta=b2^2-4*a*c#验证是否有实根
  x4=(b2*(-1)+((delta)^0.5))/(2*a)
  x5=(b2*(-1)-((delta)^0.5))/(2*a)
  y4=k*x4+b
  y5=k*x5+b#求交点坐标
  print(y5)
}
ys=(((10-16)^2+(16.5-32)^2)^0.5)+10#S点横坐标
rrns=((16-10)^2+(32-16.5)^2)^0.5#圆RNS半径
rkjl=((16-12.5)^2+(28-32)^2)^0.5#圆KJL半径
rtsu=ys-15.5#圆TSU半径
yu=16.5-rtsu#U点纵坐标
rx=((16-2.5)^2+(18-2.5)^2)^0.5-18
m1=myarc1(12.5,32,16,28,3,19,6,22)#锤子弧线左边点L
m2=myarc2(12.5,32,16,28,12.5,32,16,28)#锤子弧线右边点J
m3=myarc1(16,18,16,0,18.5,25.5,7.5,14.5)#镰刀外弧左下部分，Q点
m4=myarc2(10,16.5,16,32,10,16.5,16,32)#镰刀内弧右上部分，N点
m5=myarc1(15.5,22,15.5,yu,18.5,25.5,7.5,14.5)#镰刀内弧左下部分，W点
xw=myx1(15.5,22,15.5,yu,18.5,25.5,7.5,14.5)
yw=myy1(15.5,22,15.5,yu,18.5,25.5,7.5,14.5)
xq=myx1(16,18,16,0,18.5,25.5,7.5,14.5)
yq=myy1(16,18,16,0,18.5,25.5,7.5,14.5)
print(xw)
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
  geom_arc(data=d,aes(x0=12.5,y0=32,r=rkjl,start=m2,end=m1),colour="red")+
  geom_arc(data=d,aes(x0=16,y0=16,r=16,start=0,end=pi),colour="red")+
  geom_arc(data=d,aes(x0=16,y0=18,r=18,start=pi,end=m3),colour="red")+
  geom_arc(data=d,aes(x0=10,y0=16.5,r=rrns,start=m4,end=(pi/2)),colour="red")+
  geom_arc(data=d,aes(x0=15.5,y0=16.5,r=rtsu,start=(pi/2),end=pi),colour="red")+
  geom_arc(data=d,aes(x0=15.5,y0=22,r=(22-yu),start=pi,end=m5),colour="red")+
  geom_segment(data=d,aes(x=xw,y=yw,xend=xq,yend=yq),colour="red")+
  geom_circle(data=d,aes(x0=2.5,y0=2.5,r=rx),color="red")
  