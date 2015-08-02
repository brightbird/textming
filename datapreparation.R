library(Rwordseg)
##安装词库
installDict("DICTIONALY/electricity.scel",dictname = "electricity")
installDict("DICTIONALY/electricity2.scel",dictname = "electricity2")
listDict()

##-----(一)分词-----
segmentCN("data/data.txt")

#添加词汇“窃电”
insertWords(c("窃电","业扩报装","不完整","非抽样"))

##重新分词
segmentCN("data/data.txt")

##运行完后请detach()包，removeWords()函数与tm包中的同名函数冲突。
detach("package:Rwordseg", unload=TRUE)



##-----（二）创建语料库-----
##分词之后生成一个列表变量，用列表变量构建语料库
##这部分是读入分词后的文件，然后用TM包进行整理，清洗，变换成用于分析的"语料库"

#1、读入分词后的文本
mydoc<-read.csv(file="data/data.segment.txt",header = FALSE, stringsAsFactors =FALSE,sep ="",encoding = "UTF-8")
str(mydoc)
View(mydoc)
#2、建立语料库（这里读取文本到变量，根据文本变量来建立语料库）
library(tm)
mydoc.vec<-VectorSource(mydoc)
##动态语料库
mydoc.corpus<-Corpus(mydoc.vec)

# ##移除数字和空字符
# ##removeNumbers = function(x) { ret = gsub("[0-9０１２３４５６７８９]","",x) }
# ##移除空字符
# mysegdata<-as.vector(as.matrix(mysegdata))
# mysegdata<-mysegdata[mysegdata!=""]

##去除停止词

#读取停用词，挨个转换到一个列表
data_stw<-read.csv(file="DICTIONALY/stop_words_chinese.txt",header = FALSE, stringsAsFactors =FALSE,sep ="")

stopwords_CN=c(NULL)
for(i in 1:dim(data_stw)[1]){
  stopwords_CN=c(stopwords_CN,data_stw[i,1])
}
#
head(stopwords_CN)
#删除停用词
mydoc.corpus<-tm_map(mydoc.corpus,removeWords,stopwords_CN)      


#4、进一步清洗数据
#mydoc.corpus<-tm_map(mydoc.corpus,removeNumbers)                #删除数字
mydoc.corpus<-tm_map(mydoc.corpus,stripWhitespace)                #删除空白


#第三部分：进行内容分析
#到这里要分析的数据已经准备好了，可以进行各种分析了，下面以聚类分析为例。
#1、建立TDM矩阵（TDM就是"词语×文档"的矩阵）
##设置一些建立矩阵的参数，用变量control来存储参数,控制如何抽取文档
control<-list(removePunctuation=T,minDocFreq=5,wordLengths = c(1, Inf))    


mydoc.tdm<-TermDocumentMatrix(mydoc.corpus,control)

##查看原来有多少词
length(mydoc.tdm$dimnames$Terms)  

##去掉低于80%的稀疏词
tdm_removed<-removeSparseTerms(mydoc.tdm, 0.80) 

length(tdm_removed$dimnames$Terms)

##查看高频词
findFreqTerms(mydoc.tdm,lowfreq = 10)

##查看与某个词的相关系数
findAssocs(mydoc.tdm,"窃电",0.5)

##数据框格式转换

##词云绘制
library(wordcloud)
m <-as.matrix(tdm_removed)
m
v<-sort(rowSums(m), decreasing = TRUE)
v
set.seed(4363)
wordcloud(names(v), v, min.freq = 50)


