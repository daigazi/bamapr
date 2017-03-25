keylist <- function(path="data/key.txt"){
        if(!file.exists(path)){
                stop(cat("please create your's key txt\n") )

        }else{
                key <- readLines(path,encoding = "utf-8")
                key <- key[grep("[0-0a-zA-Z]",key)]
                baidu_index=grep("baidukey",key)
                amap_index=grep("amapkey",key)
                key_real=key[-c(amap_index,baidu_index)]
                baidu_num= amap_index-baidu_index-1
                amap_num<- length(key)-amap_index
                df=data.frame(key=key_real,
                              map=rep(c("baidukey","amapkey"),c(baidu_num,amap_num)))
        }
        return(df)

}

# geoconvA  单个Key支持2000次/天调用
# 路径规划  单个Key支持1000次/天调用
# 行政区域查询 普通用户：单个Key支持1000次/天调用，1000次/分钟调用。
# 搜索  普通用户：单个Key支持1000次/天调用
# IP定位 单个Key支持1万次/分钟调用。日调用次数不限量
# 抓路服务   单个Key支持1万次/分钟调用。日调用次数不限量
# 静态地图  普通用户：单个Key4万次/分钟调用，每日不限。
# 坐标转换 单个Key支持6万次/分钟调用。每日不限。
# 天气查询 单个Key支持6万次/分钟调用。日调用次数不限量
# 输入提示  普通用户：单个Key支持1000次/天调用
# 交通态势  普通用户：单个Key支持1000次/天调用


# Place API 是一套免费使用的API接口，调用次数限制为10万次/天。
# Place Suggestion API 1个ak支持10万次/天。
# Geocoding API 默认配额6000次/天
# Direction API v2.0  未认证开发者默认支持2000次/天（与Direction API v1.0共享配额，http和https共享配额）
# Route Matrix API v2.0 2000条路线/天
# 坐标转换服务每日请求次数上限为10万次
# 全景静态图API 每个ak可每天最多可调用10万次该服务