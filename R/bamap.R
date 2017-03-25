bamap <- setRefClass(Class = "bamap",
                     fields  =list(
                         status="character",
                         infocode="character",
                         lng="numeric",
                         lat="numeric",
                         id="character",
                         bmapkey="list",
                         amapkey="list",
                         digits="integer",
                         myheader="vector",
                         geomtx = "matrix",
                         grid="numeric",
                         polyline="character"),

                     methods=list(
                         initialize=function(){
                             bmapkey <<- list("GG1i5bId2iQUUavr3HyrkpA2",
                                              "9ekpeaGilBUCyeggxXEZLoTVSvo0MPhj",
                                              "NRwZ8tgLfNqGsxBwTBWGExCOK3SDO7G6",
                                              "Ucf5KFXt1suagbZ564NfS15C7ccXx4Pn",
                                              "oTQk0Lu3oMpoGYYPU2aHRI6Xwv0w0mXq",
                                              "k70Wa3UTWcxAtPAkRH5dRcqtVS5R011g")
                             amapkey <<-list("71b002162d43fea08afd5b5e31b030af",
                                             "672859115f4e772dff9c6bd9102877ec",
                                             "33a5842d14540217d508cb422918b9ff",
                                             "1138f93a447b7a7f56293af12b74d89c")
                             geomtx<<-matrix(c(119.117295,26.201212,119.649092,25.840692),
                                          byrow = T,nrow = 2)
                             polyline<<-'119.278861,26.106208;119.336454,26.078304'
                             grid<<- 5000
                             digits<<-10L
                             myheader <<- c(`User-Agent` = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0",
                                            Accept = "*/*", `Accept-Language` = "zh-CN,zh;q=0.8,en-US;q=0.5,en;q=0.3",
                                            Connection = "keep-alive", `Accept-Charset` = "GB2312,utf-8;q=0.7,*;q=0.7")
                         },
                         testaccess=function(url){
                             url_a=list()
                             url_b=list()
                         },
                         errortable=function(){
                             url_a="http://lbs.amap.com/api/webservice/guide/tools/info/"
                             url_b="http://lbsyun.baidu.com/index.php?title=webapi/errorcode"
                             doc_a=XML::htmlParse(url_a,encoding = "UTF-8")
                             doc_b=XML::htmlParse(url_b,encoding = "UTF-8")
                             a=XML::readHTMLTable(doc_a,header = T)[[1]]
                             a=a[,-1]
                             colnames(a)=c("infocode","return","explain","resolve")
                             b=XML::readHTMLTable(doc_b,header = T)[[1]]
                             colnames(b)=c("status","define","explain")
                             return(errortable=list(amap=a,bmap=b))
                         }))



