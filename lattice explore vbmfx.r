#lattice exploration of Vanguard Bond Fund (vbmfx)
#returns since 1990

require(quantmod)
require(lattice)
require(latticeExtra)
require(PerformanceAnalytics)

getSymbols("VBMFX",from="1896-01-01",to=Sys.Date(),adjust=TRUE)

#make monthly so little more manageable and meaningful
VBMFX.monthly <- to.monthly(VBMFX)[,4]
index(VBMFX.monthly) <- as.Date(index(VBMFX.monthly))

#get 1 month rate of change
VBMFX.monthly.roc <- ROC(VBMFX.monthly,type="discrete",n=1)
VBMFX.monthly.roc[1,]<-0
colnames(VBMFX.monthly.roc) <- "VBMFX Monthly Return"

#use table.CalendarReturns to quickly get yearly return numbers
#we will use this to color boxes red or green based on positive or negative
year.returns <- table.CalendarReturns(VBMFX.monthly.roc)[,c(13),drop=FALSE]

#factor dates by years %Y returns 4-digit year
year.group <- factor(format(index(VBMFX.monthly.roc),"%Y"))

#use yearly return to indicate whether year was up or down
colors <- ifelse(year.returns>0,"darkolivegreen4","indianred4")
qqmath(~VBMFX.monthly.roc[,1],main="Vanguard Bond Fund QQPlot")
qqmath(~VBMFX.monthly.roc[,1],groups=year.group,col=colors,
	cex=1.25,pch=19,type="b",main="Vanguard Bond Fund QQPlot by Year") 

#would like to color background of panels according to year up or down
#but I could not achieve the desired result
qqmath(~VBMFX.monthly.roc[,1]|year.group,aspect="xy",
	prepanel=prepanel.qqmathline,
	panel= function(x,...) {
		panel.qqmathline(x,...)
		panel.qqmath(x,...)
	},
	main="Vanguard Bond Fund QQPlot by Year Panel",
	ylab="VBMFX Monthly Return")
#		one of my attempts at accomplishing shading by up or down
#		par.settings=list(panel.background=list(
#			col=function(x,...) {
#				colors[VBMFX.monthly.roc[,1]==x[1]]
#			})))


densityplot(~VBMFX.monthly.roc[,1],groups=year.group,col=colors,lwd=2,
	plot.points="rug",
	xlab = "VBMFX Monthly Return",
	main="Vanguard Bond Fund Density Plot of Monthly Returns") +
	layer(panel.abline(v = 0,lwd=2))

bwplot(year.group~VBMFX.monthly.roc[,1],pch=19,col="black",fill=colors,
	par.settings=list(
		plot.symbol=list(pch=19,col="gray"),
		box.umbrella=list(col="gray"),
		box.rectangle=list(col="gray")),
	xlab = "VBMFX Monthly Return",
	main="Vanguard Bond Fund Boxplot of Monthly Returns")


#now change colors to be red or green based on month up or down
#instead of year up or down
newcolors <- ifelse(VBMFX.monthly.roc[,1]>0,"darkolivegreen4","indianred4")

dotplot(year.group~VBMFX.monthly.roc[,1],
	auto.key = list(space="right"),
	col=newcolors,pch=19,type = c("p", "h"),cex=1.2,
	xlab = "VBMFX Monthly Return",
	main="Vanguard Bond Fund Dotplot of Monthly Returns")+
	layer(panel.abline(v = 0,lwd=2))+
	as.layer(bwplot(~VBMFX.monthly.roc[,1],
		pch=10,col="gray",
		par.settings=list(
			plot.symbol=list(pch=21,col="gray"),
			box.umbrella=list(col="gray",lwd=2),
			box.rectangle=list(col="gray",lwd=2))),
		x.same=TRUE,y.same=FALSE)


dotplot(year.group~VBMFX.monthly.roc[,1],
	auto.key = list(space="right"),
	col=newcolors,pch=19,type = c("p", "h"),cex=1.2,
	xlab = "VBMFX Monthly Return",
	main="Vanguard Bond Fund Dotplot of Monthly Returns") +
	layer(panel.abline(v = 0,lwd=2)) +
	bwplot(year.group~VBMFX.monthly.roc[,1],pch=15,col="gray",
		par.settings=list(
			plot.symbol=list(pch=21,col="gray"),
			box.umbrella=list(col="gray",lty=2,lwd=2),
			box.rectangle=list(col="gray",lwd=2)))  #col=colors)))


CalmarRatio(VBMFX.monthly.roc)