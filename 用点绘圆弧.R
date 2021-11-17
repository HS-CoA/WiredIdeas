#笔记：构建函数 my_function<-function(){}
#目标函数：以A点(xp,yp)为圆心，B点(x1,y1)为圆弧起点画圆，与直线CD：C(x2,y2),
#D(x3,y3)交与E点(x4,y4)和F(x5,y5)，使圆弧停于E点，取圆弧BE。
myarc <- function(xp,yp, x1,y1, x2,y2, x3,y3){
  library(tidyverse)
  library(ggplot2)
  library(ggforce)#加载绘图所需的ggplot2包
#开始求圆弧和直线交点  
  k=((y2-y3)/(x2-x3))#计算CD斜率
  b=y2-(k*x2)#计算CD截距
  r=((x1-xp)^2+(y1-yp)^2)^0.5#计算圆弧对应完整圆的半径
  a=k^2+1
  b2=2*(b*k-k*yp-xp)
  c=xp^2+(b-yp)^2-r^2#标准一元二次方程求根
  delta=b2^2-4*a*c#验证是否有实根
    if (delta < 0) print("直线和圆没交点，画不到那里")
    if (delta == 0)
      y4=b2*(-1)/(2*a)
      x4=(y4-b)/k
      xc=x4
      yc=y4
    if (delta>0)
      y4=(b2*(-1)+((delta)^0.5))/(2*a)
      y5=(b2*(-1)-((delta)^0.5))/(2*a)
      x4=(y4-b)/k
      x5=(y5-b)/k#求交点坐标
      print(paste0("选择交点：","E(",x4,",",y4,"  F(",x5,",",y5,")","（输入大写字母！）"))
      choice1 = readline()
      if (choice1=="E")
        xc=x4
        yc=y4
      if(choice1=="F")
        xc=x5
        yc=y5
  
  #开始计算弧度来使用geom_circle绘图
  #零弧度的圆上点A2点(xp,yp+r)，计算圆弧起始点弧度
  d0=((y1-(yp+r))^2+(x1-xp)^2)^0.5#计算BA2线段长
  degree0=acos(d0/(2*r))#计算劣弧对应角度的一半
  m01=2*degree0*r#劣弧长，劣弧弧度
  m02=(2*pi*r)-m01#优弧长，优弧弧度
  if (x1>=xp)
    ma2=m01
  if (x1<xp)
    ma2=m02
  #计算圆弧终止点弧度
  d=((y1-yc)^2+(x1-xc)^2)^0.5#计算BE线段长
  degree=acos(d/(2*r))#计算劣弧对应角度的一半
  m1=2*degree*r#劣弧长，劣弧弧度
  m2=(2*pi*r)-m1#优弧长，优弧弧度    
  print(paste("选择优弧输入Y，选择劣弧输入L")) 
  choice2=readline()
  if (choice2=="L")
    mend=m1
  if (choice2=="Y")
    mend=m2
  
  arcdata<-data.frame(x=c(xp,yp,r,ma2,mend))
  print(paste(xp,yp,r,ma2,mend))
  ggplot()+
    geom_arc(data=arcdata,aes(x0=x[1],y0=x[2],r=x[3],start=x[4],end=x[5]), color="red")
}
