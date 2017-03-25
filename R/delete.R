
################################################################################
####                    图吧公交线路经纬度                            ##########
################################################################################

# MapBarBus$location=NA
#  for(i in 1:nrow(MapBarBus)){
#      print(i)
#      a=getCoordinateA(address=MapBarBus$station[i],city="福州",key=amap_key)
#      if(length(a)>0){
#          MapBarBus$location[i]=a
#      }
#
#  }


################################################################################
####                             画公交线                             ##########
################################################################################

#out=busplot(MapBarBus,bus)


# library(REmap)
# remapB(center = get_city_coord("福州"),
#        zoom = 14,
#        title = "福州1号线",
#        color = "googlelite",
#        markLineData =out$linedata,
#        markLineTheme = markLineControl(smoothness = 0,lineWidth=2,
#                                        effect = F,lineType = "solid",
#                                        symbolSize = c(0,0)),
#        geoData = out$geodata)



################################################################################
####                             cleanData.R                          ##########
################################################################################
# path="C:/Users/dai_jl/Desktop/nsz.txt"
# map_ak = 'GG1i5bId2iQUUavr3HyrkpA2'
# dat=cleanData(path =path )
# path1="C:/Users/dai_jl/Desktop/ssz.txt"
# dat1=cleanData(path =path1 )
# fzdat=cleanData(path =path1,data=dat,direction = "ssz" )
# point=dat1$point
# line=dat1$line
#library(REmap)


################################################################################
####                            制作geodata数据                       ##########
################################################################################

# lineData=mtx2lineData(mtx,color ="blue")
# library(REmap)
#
# color=c("#FFC1C1","	#FF6EB4","#EEB422","#DC143C",
#         "#CD8162","#C0FF3E","#ABABAB","#0A0A0A","#FFFF00")
# mtx=list();geoData=list();lineData=list()
# for(i in length(routeout)){
#     mtx[i]=polyline2mtx(routeout[i])
#     geoData[i]=mtx2geoData(mtx)
#     lineData[i]=mtx2lineData(mtx,color =color[i])
# }
#
#
# remapB(center = get_city_coord("福州"),
#        zoom = 14,
#        title = "福州1号线",
#        color = "googlelite",
#        markLineData =lineData,
#        markLineTheme = markLineControl(smoothness = 0,lineWidth=5,
#                                        effect = F,lineType = "solid",
#                                        symbolSize = c(2,4)),
#        geoData = geoData)


################################################################################
####                            读取路网数据                          ##########
################################################################################
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
#
# library(REmap)
# remapB(center = get_city_coord("福州"),
#        zoom = 14,
#        title = "福州路网",
#        color = "googlelite",
#        markLineData =line,
#        markLineTheme = markLineControl(smoothness = 0,lineWidth=5,
#                                        effect = F,lineType = "solid",
#                                        symbolSize = c(0,0)),
#        geoData = point[,c(1,2,3)])

################################################################################
####                        监视某条路的路况                          ##########
################################################################################

# tmp=point[grep(point$name,pattern = "鼓屏路"),]
#
# Roadgrid()


#  install.packages("devtools")
#  install.packages("stringi")
#  install.packages("pbapply")
#  install.packages("Rcpp")
#  install.packages("RcppProgress")
#  library(devtools)
#  install_github("qinwf/cidian")
#  library(cidian)
#  decode_scel(scel = "data/fzroad.scel",cpp = TRUE)
#  decode_scel(scel = "data/fzbusstation.scel",cpp = TRUE)
#    path0=.libPaths()
#    path=paste(path0,"/jiebaRD/dict/user.dict.utf8")
#  scan(file="C:\\Program Files\\R\\R-3.3.2\\library\\jiebaRD\\dict\\user.dict.utf8",
#            what=character(),nlines=150,sep='\n',
#            encoding='utf-8',fileEncoding='utf-8')
#
# wk=worker()
# wk[road]
# wk=worker(stop_word='data/stopword.txt')
# wk[road]
# new_user_word(wk,words = df_road$name)
# wk[road]

################################################################################
####                        指定范围的拥堵路段名称                    ##########
################################################################################
# location='119.229335,26.122166;119.59875,25.87036'
# df=getTrafficStatusPlus(location = location)
# index=grep(df$description,pattern = "缓慢|拥堵|堵塞")
# a=df$description[index]
# b=unlist(strsplit(a,split = "；"))
# index=grep(b,pattern = "缓慢|拥堵|堵塞")
# c=b[index]
# N=length(c)
# d=unlist(strsplit(c,split = "："))[seq(from=1,to=N*2,by=2)]
# df=table(d)
# library(wordcloud2)
# wordcloud2(df,size = 0.3)

################################################################################
####                        测试抓取福州道路名称                      ##########
################################################################################

# rectangel=matrix(c(118.784364,26.478515,119.892801,25.436929),byrow = T,nrow = 2) #bustation
# rectangel=matrix(c(119.298106,26.095873,119.338494,26.061215),byrow =T,nrow = 2) #test
# geturl0(keywords = "道路",shape = "text",city = "福州",types = "道路名")
# df=getSearch(shape = "text",keywords = "道路",city = "福州",types = "19")
# df=getSearchPlus(mtx = rectangel,word = "道路",types = "19",Tobaidu = F)
# #将道路名字加入用户自定义词典
# df_road=df[nchar(df$address)<=3,] #地址是xx区的
#  #自定义词典
#  wk=worker()
#  new_user_word(wk,words = df_road$name)
################################################################################
####                    抓取公交路线上的所有站点                      ##########
################################################################################


# df=getBusRoute(keywords = "青口-甘蔗")
# df=getBusRoute(keywords = "189路快线")
# df=getBusRoute(keywords = "38路区间车")
# df=getBusRoute(keywords = "连江12路")
# df=getBusRoute(keywords = "空港快线连江专线")
#
# rectangel=matrix(c(118.784364,26.478515,119.892801,25.436929),byrow = T,nrow = 2) #bustation
# rectangel=matrix(c(119.277691,26.08631,119.346184,26.054545),byrow =T,nrow = 2) #test
# bus=getSearchPlus(mtx=rectangel,grid=5000,word = "公交站",Tobaidu=F,types = "交通设施服务")
# bus=bus[bus$cityname=="福州市",]
# bus_route=unlist(strsplit(bus$address,split = "[;/.]"))
# bus_route=bus_route[!duplicated(bus_route)]
# index=grep(bus_route,pattern = "路")
# bus_route=bus_route[index]
# index=grep(bus_route,pattern = "公交|旁|对面|米|号|停运|中心")
# bus_route=bus_route[-index]
#
# bus=bus[grep(bus$name,pattern = "\\(公交站\\)"),]
#
# bus_list=list()
#
# for(i in 1:length(bus_route)){
#         bus_list[[i]]=getBusRoute(citycode = 350100,keywords = bus_route[i])
#         if(i %in% seq(from=20,to=400,by=10)){
#              Sys.sleep(120)
#         }else{
#                 Sys.sleep(10)
#         }
#
#         print(i)
# }
#
# busstation=do.call(rbind,bus_list)
# busstation=busstation[grep("[0-9]",busstation$lat),]

