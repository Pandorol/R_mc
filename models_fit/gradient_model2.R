POW_MSE <-function(params,y_real,x_real)
{
	if(any(params<0)||any(params>1)){return(1e6)}
	y_pred = params["a"]*(params["b"]*x_real+1)^(-params["c"])
	return(sum((y_pred-y_real)^2)/length(x_real))
}
POW_MSE_PLEAD <-function(params,y_real,x_real)
{
	x=x_real;y=y_real;a=params["a"];
	b=params["b"];c=params["c"];
	expPOW = expression((y-a*(b*x+1)^(-c))^2)
	PLEAD<-c(sum(eval(D(expPOW,"a")))/length(x),sum(eval(D(expPOW,"b")))
	/length(x),sum(eval(D(expPOW,"c")))/length(x))
	names(PLEAD) = c("a","b","c")
	return(PLEAD)
}

POW_MSE_OPTIM <-function(params,y_real,x_real,a)
{
	p = params
	step = a*POW_MSE_PLEAD(p,y_real,x_real)
	while(any(abs(step)>1e-6))
	{
		p=p-step
		step = a*POW_MSE_PLEAD(p,y_real,x_real)
	}
	return(p)
}
POW_AIC<-function(params,y_real,x_real)
{
	return(2*length(params)+length(x_real)*log(POW_MSE(params,y_real,x_real)))
}


y_real = c(0.93,0.88,0.86,0.66,0.47,0.34)
x_real = c(0.0035,1,2,7,14,42)
params = c(1,0.5,0.7)#初始值
names(params) = c("a","b","c")
params_out = POW_MSE_OPTIM(params,y_real=y_real,x_real=x_real,1e-4)
y_pred = params_out["a"]*(params_out["b"]*c(0:max(x_real))+1)^(-params_out["c"])
print(POW_AIC(params_out,y_real,x_real))
x11()
#设置参数par()
#cex.axis：坐标轴刻度文字的缩放倍数
#cex.lab：坐标轴标签(名称)的缩放倍数
par(cex.axis=1.2,cex.lab=1.4)
#mar：以数值向量表示边界大小，顺序为"下、左、上、右"，单位为英分，默认值c(5, 4, 4, 2)+0.1
#las：标签的字体是否平行(=0)或者垂直(=2)坐标轴
par(mar=(c(5,5,3,2)+0.1),las=1)
# xaxt="n"  禁用x轴的刻度线
#xlim x坐标轴的范围，只写出最小值和最大值
plot(x_real,y_real,xlab="Retention Interval (Days)",ylab="Proportion Items Retained",
main="y=a(bx+1)^c",
ylim=c(0.3,1),xlim=c(0,43),xaxt="n",type="n")
lines(c(0:max(x_real)),y_pred,lwd=2)
points(x_real,y_real,pch=21,bg="dark grey",cex=2)
dev <-y_pred[x_real+1]
for(x in c(1:length(x_real)))
{
	lines(c(x_real[x],x_real[x]),c(dev[x],y_real[x]),lwd=1)
}
axis(1,at=c(0:43))



