raw = function(data){
  # 刪除Q = NA
  data$quadrat = as.numeric(data$quadrat)
  complete_rows = complete.cases(data$quadrat)
  data = data[complete_rows, ]
  
  # 所有物種數
  rowname = unique(data$sp)
  
  # 選出存活的
  data = data[which(data$status == "A"),]
  
  # 排列資料
  data = data[order(data$quadrat),]
  
  # 製造 raw data
  data$quadrat = data$quadrat+1
  
  list = list()
  raw = list()
  a = rep(0,25) # plot
  for(i in 1:49){
    q = rep(i*100,25)
    a = c(a,q)
  }
  a = a+rep(seq(1,25),50)
  
  for(i in a){
    list[i] = list(data.frame(table(data[which(data$quadrat==i),3])))
    list[[i]]$Freq = as.numeric(list[[i]]$Freq > 0)
    raw[i] <- list(merge(data.frame(Var1 = rowname), list[[i]], all.x = TRUE))
    raw[[i]]$Freq[is.na(raw[[i]]$Freq)] = 0
  }
  raw_data = c(raw[[1]]$Freq)
  for(i in a[-1]){
    raw_data = cbind(raw_data,raw[[i]]$Freq) 
  }
  return(raw_data)
}
load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree1.rdata") 
load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree8.rdata") 
data1 = raw(bci.tree1)
data8 = raw(bci.tree8)
data1 = data.frame(data1)
data2 = data.frame(data8)

write.csv(data1, "D:\\nagisa\\NAGISA\\學校\\碩班\\LAB\\00 研究\\00 code\\data\\BCI\\bci.tree\\BCI1.csv")
write.csv(data2, "D:\\nagisa\\NAGISA\\學校\\碩班\\LAB\\00 研究\\00 code\\data\\BCI\\bci.tree\\BCI8.csv")

# # BCI data 整理 (tree)
# ##### function #####
# # a = 0,1,2,...49
# count = function(data){
#   data$quadrat = as.numeric(data$quadrat)
#   complete_rows = complete.cases(data$quadrat)
#   data = data[complete_rows, ]
#   
#   # 所有物種數
#   rowname = unique(data$sp)
#   
#   # 選出存活的
#   data = data[which(data$status == "A"),]
#   
#   return(data)
# }
# 
# # read data
# load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree1.rdata") # 81-83
# load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree2.rdata") # 85
# load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree3.rdata") # 90-92
# load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree4.rdata") # 95-96
# load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree5.rdata") # 00-01
# load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree6.rdata") # 05-26
# load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree7.rdata") # 11-10
# load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.tree/bci.tree8.rdata") #13,15-16
# 
# load("D:/nagisa/NAGISA/學校/碩班/LAB/00 研究/00 code/data/BCI/bci.spptable.rdata") # 1412種物種
# 
# ###### check Q quantity >> 每筆資料的區塊數皆相同 #####
# length(unique(bci.tree1$quadrat))
# table(bci.tree1$quadrat)
# length(unique(bci.tree1$sp))
# table(bci.tree1$sp)
# unique(bci.tree1$sp) == unique(bci.tree2$sp)
# 
# ##### 物種數 #####
# l = count(data)
# i = count(data_)
# length(unique(l$sp))
# length(unique(i$sp))
# 
# length(unique(c(l$sp,i$sp))) # 物種數量
# common_rows = intersect(l$sp,i$sp)
# length(common_rows) # 共同種數量
# 
# 
# ###### 整理 #####
# # data
# data = bci.tree1
# data_ = bci.tree8
# 
# 
# 
# # result
# data1 = raw(bci.tree1)
# data2 = raw(bci.tree2)
# data3 = raw(bci.tree3)
# data4 = raw(bci.tree4)
# data5 = raw(bci.tree5)
# data6 = raw(bci.tree6)
# data7 = raw(bci.tree7)
# data8 = raw(bci.tree8)
# 
# 
