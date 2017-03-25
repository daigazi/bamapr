## ---- API demo ----
## ---- remapBDemo ----
## ---- pauseFunction ----
pause <- function(){
    invisible(readline("\nPress <return> to continue: "))
}
library(REmap)
library(bamapr)
## Test for Bmap
################################################################################
####                    福州三环路线                                  ##########
################################################################################

data(fzdat)

set.seed(1234)
index=which(threeRing_n$line$color=="#FF0000")
pointData = data.frame(sample(threeRing_n$point$index[index],2),
                       color = c(rep("red",2)))
remapB(center = get_city_coord("福州"),
       zoom = 14,
       title = "福州三环",
       color = "googlelite",
       markPointData = pointData,
       markPointTheme = markPointControl(symbol = 'circle',
                                         symbolSize = 15,
                                         effect = T),
       markLineData = threeRing_n[[2]],
       markLineTheme = markLineControl(smoothness = 0,lineWidth=3,
                                       effect = F,lineType = "solid",
                                       symbolSize = c(0,0)),
       geoData = threeRing_n$point[c("Lon","Lat","index")])


web_path="http://www.iconpng.com/png/windows8_icons2/road_worker.png"
remapB(center = get_city_coord("福州"),
       zoom = 14,
       title = "福州三环",
       color = "googlelite",
       markPointData = pointData,
       markPointTheme = markPointControl(
           symbol = paste("image://",web_path,sep=""),
                                         symbolSize = 30,
                                         effect = F),
       markLineData = threeRing_n[[2]],
       markLineTheme = markLineControl(smoothness = 0,lineWidth=3,
                                       effect = F,lineType = "solid",
                                       symbolSize = c(0,0)),
       geoData = threeRing_n$point[c("Lon","Lat","index")])



################################################################################
####                    福州公交路线                                  ##########
################################################################################

pause()
cat("公交路线及其站点查询\n")
road="1路"
x=getBusRoute(keywords=road)
head(x)
ll=getBusRoute(keywords=road,getcoord=T,gettime=T)
cat("公交路线早晚班时间\n")
ll[[2]]
cat("公交路线路径地图\n")
y=ll[[3]]
y[1,1]=gsub(pattern = "\\]",replacement = "",x = y[1,1])
mtx=matrix(as.numeric(unlist(strsplit(y[1,1],split = ","))),ncol=2,byrow=T)
#转换为百度地图坐标
mtx=geoconv(geocode = mtx,from = 3, to = 5)
geodata=mtx2geoData(mtx,name = road)
#geodata先经度后维度
#col=rgb(red =252,green = 249,blue =0.5 )
linedata=mtx2lineData(mtx = mtx,name = road,color ="#00F5FF")


remapB(center = get_city_coord("福州"),
       zoom = 14,
       title = paste("福州",road,"公交",sep=""),
       color = "googlelite",

       markLineData =linedata,
       markLineTheme = markLineControl(smoothness = 0,lineWidth=3,
                                       effect = F,lineType = "solid",
                                       symbolSize = c(0,0)),
       geoData = geodata[,c(2,1,3)])


################################################################################
####                    批量爬取福鼎道路信息                          ##########
################################################################################
# rectangel=matrix(c(120.208958,27.327244,120.232389,27.30612),byrow = T,nrow = 2)
# df=getSearchPlus(mtx = rectangel,word = '道路',types = '19',Tobaidu = T)
# adcode=amapcity[amapcity$名称=="福鼎市",]$adcode
# geoData=list()
# lineData=list()
# for(i in 1:nrow(df)){
#         tmp=getRoadCoor(id = df[i,1],tobaidu = T,name=df[i,2])
#         geoData[[i]]=tmp$geodata
#         lineData[[i]]=tmp$linedata
# }
#
# geoData1=do.call(rbind,geoData)
# lineData1=do.call(rbind,lineData)
data(fds)
geoData1=na.omit(geoData1)

lineData1=na.omit(lineData1)
lineData1$color="#00F5FF"

remapB(center = get_city_coord("福鼎"),
       zoom = 15,
       title = "批量抓取的福鼎某个区域道路",
       color = "googlelite",
       markLineData = lineData1,
       markLineTheme = markLineControl(smoothness = 0,lineWidth=2,
                                       effect = F,lineType = "solid",
                                       symbolSize = c(0,0)),

       geoData = geoData1)


################################################################################
####                    交通拥堵情况                                  ##########
################################################################################
pause()

if (!require(devtools)) install.packages("devtools")
devtools::install_github('lchiffon/wordcloud2')

data(road)
library(jiebaR)
wk = worker(stop_word='data/stopword.txt')
roadQ=table(wk[road])
library(wordcloud2)
wordcloud2(roadQ)

pause()
pointData=data.frame(name=as.factor(bus[,1]),color="#7CCD7C")
geodata=bus[,c(4,5,1)]
remapB(center = get_city_coord("福州"),
       zoom = 14,
       title = "福州公交",
       color = "googlelite",
       markPointData = pointData,
       markPointTheme = markPointControl(symbol = 'pin',
                                         symbolSize = 5,
                                         effect = F),
       geoData = geodata)

################################################################################
####                    福州公交站点                                  ##########
################################################################################
pause()
data(fzbus)
library(REmap)
bus$name=paste(bus$name,bus$id,sep="")
pointData=data.frame(name=bus[,"name"],color="#7CCD7C")
geodata=bus[,c("lon_bd","lat_bd","name")]
# geodata$lon=as.numeric(geodata$lon)
# geodata$lat=as.numeric(geodata$lat)
#转换成百度地图
lonlat=data.frame(geoconv(geodata[c(1:2)]))
colnames(lonlat)=c("lon_bd","lat_bd")
bus=cbind(bus,lonlat)
remapB(center = get_city_coord("福州"),
       zoom = 14,
       title = "福州公交站分布图",
       color = "googlelite",
       markPointData = pointData,
       markPointTheme = markPointControl(symbol = 'pin',
                                         symbolSize = 5,
                                         effect = F),
       geoData = geodata)
cat("福州全市（含五区八县）共",nrow(bus),"个公交车站\n")
route=unlist(strsplit(bus$address,split = "[;/\\.]"))
route=route[!duplicated(route)]
route=route[-38]
route_dy=route[-grep(pattern = "停运",x = route)]
cat("福州全市（含五区八县）共",length(route_dy),"个公交线路\n")



################################################################################
####                    主要道路画图                                  ##########
################################################################################

pause()

# road=list.files("data/fz")
# point=data.frame(matrix(rep(NA,5),ncol=5))
# colnames(point)=c("Lon","Lat","index","name","direction")
# line=data.frame(matrix(rep(NA,4),ncol=4))
# colnames(line)=c("origin","destination","color","symbol")
# for(i in 1:length(road)){
#
#     path0=paste("data/fz/",road[i],sep="")
#     filename=unlist( strsplit(road[i],split = "-+"))
#     name=unlist( strsplit(road[i],split = "\\."))[1]
#     direction=unlist( strsplit(filename[2],split = "\\."))[1]
#     tmp=cleanData(path=path0,name =name,direction = direction)
#     point=rbind(point,tmp$point)
#     line=rbind(line,tmp$line)
# }
# point=na.omit(point)
# line=na.omit(line)
data(fzroad)
library(REmap)
remapB(center = get_city_coord("福州"),
       zoom = 14,
       title = "福州路网",
       color = "googlelite",
       markLineData =line,
       markLineTheme = markLineControl(smoothness = 0,lineWidth=5,
                                       effect = F,lineType = "solid",
                                       symbolSize = c(0,0)),
       geoData = point[,c(1,2,3)])

################################################################################
####                    OD对期望图                                    ##########
################################################################################
pause()

data(fzbus)
bus_sample=bus[sample(1:nrow(bus),size = 50,replace = F),]
c=matrix(rep(NA,nrow(bus_sample)^2),ncol=nrow(bus_sample))
colnames(c)=bus_sample$name
rownames(c)=bus_sample$name
c[is.na(c)]="#9F79EE"
library(reshape2)
lineData3=melt(c)
colnames(lineData3)=c("origin","destination","color")
pointData3=data.frame(name=as.factor(bus_sample[,"name"]),color="#7CCD7C")
geoData3=bus_sample[,c("lon_bd","lat_bd","name")]

remapB(center = get_city_coord("福州"),
       zoom = 14,
       title = "OD对期望图",
       color = "googlelite",
       markPointData = pointData3,
       markPointTheme = markPointControl(symbol = 'pin',
                                         symbolSize = 2,
                                         effect = F),
       markLineData = lineData3,
       markLineTheme = markLineControl(smoothness = 0,lineWidth=0.1,
                                       effect = F,lineType = "solid",
                                       symbolSize = c(0,0)),

       geoData = geoData3)

################################################################################
####                    批量抓取的福鼎某个区域道路                    ##########
################################################################################
pause()
data(fds)
geoData1=na.omit(geoData1)

lineData1=na.omit(lineData1)
lineData1$color="#00F5FF"

remapB(center = get_city_coord("福鼎"),
       zoom = 15,
       title = "批量抓取的福鼎某个区域道路",
       color = "googlelite",
       markLineData = lineData1,
       markLineTheme = markLineControl(smoothness = 0,lineWidth=2,
                                       effect = F,lineType = "solid",
                                       symbolSize = c(0,0)),

       geoData = geoData1)