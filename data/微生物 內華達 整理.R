library(dplyr)
library(stringr)
################################# read data ####################################
data = read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\微生物 內華達\\SEKIPaper2_2023_dryad.csv")
# data = select(data,enter.name,macrosite,plot = plot..,location = Aspect...A)
data = select(data,enter.name,macrosite,plot = plot..,location = elev..ft.)
############################### function #######################################
raw = function(DATA,data){
  species = unique(DATA$enter.name)
  # plot
  plot = data$plot
  plot_num = c(1:length(unique(data$plot)))
  for (i in seq_along(plot_num)) {
    plot[plot == unique(plot)[i]] <- plot_num[i]
  }
  data$plot = plot
  
  # 製造raw data
  list = list()
  raw = list()
  # a = plot_num
  
  for(i in plot_num){
    list[i] = list(data.frame(table(data[which(data$plot==i),1])))
    list[[i]]$Freq = as.numeric(list[[i]]$Freq > 0)
    raw[i] <- list(merge(data.frame(Var1 = species), list[[i]], all.x = TRUE))
    raw[[i]]$Freq[is.na(raw[[i]]$Freq)] = 0
  }
  raw_data = c(raw[[1]]$Freq)
  for(i in plot_num[-1]){
    raw_data = cbind(raw_data,raw[[i]]$Freq) 
  }
  raw_data = data.frame(raw_data)
  rownames(raw_data) = raw[[1]]$Var1
  return(raw_data)
}
################################################################################
data$location = data$location*0.3048

foothill = data[which(data$location<=1200),]
lower_conifer = data[which(data$location>1200 & data$location<=2440),]
upper_conifer = data[which(data$location>2440 & data$location<=2750),]
high_country = data[which(data$location>2750),]
length(unique(foothill$plot)) # 67 plots
length(unique(lower_conifer$plot)) # 100 plot
length(unique(upper_conifer$plot)) # 17 plots
length(unique(high_country$plot)) # 69 plot
data_F = raw(data,foothill)
data_L = raw(data,lower_conifer)
data_U = raw(data,upper_conifer)
data_H = raw(data,high_country)

write.csv(data_F, "D:\\nagisa\\NAGISA\\學校\\碩班\\研究\\00 code\\data\\微生物 內華達\\data1.csv")
write.csv(data_L, "D:\\nagisa\\NAGISA\\學校\\碩班\\研究\\00 code\\data\\微生物 內華達\\data2.csv")
write.csv(data_U, "D:\\nagisa\\NAGISA\\學校\\碩班\\研究\\00 code\\data\\微生物 內華達\\data3.csv")
write.csv(data_H, "D:\\nagisa\\NAGISA\\學校\\碩班\\研究\\00 code\\data\\微生物 內華達\\data4.csv")

############################### clean data #####################################
# plot 編號
# class(data$plot)
# length(unique(data$plot)) # 253 plots
# plot = data$plot
# plot_num = c(1:length(unique(data$plot)))
# for (i in seq_along(plot_num)) {
#   plot[plot == unique(plot)[i]] <- plot_num[i]
# }
# data$plot = plot

# 物種
# class(data$enter.name)
# length(unique(data$enter.name)) # 254 種
# species = unique(data$enter.name)

# macrosite
# unique(data$macrosite)
# length(unique(data$macrosite))
# SP = data[which(data$macrosite == "Sequoia National Park"),]
# OSP = data[-which(data$macrosite == "Sequoia National Park"),]

# location
# unique(data$location)
# table(data$location)
# N = data[str_detect(data$location,"north"),]
# S = data[str_detect(data$location,"south"),]
# W = data[str_detect(data$location,"west"),]
# E = data[str_detect(data$location,"east"),]
# length(unique(N$plot)) # 25 plots
# length(unique(S$plot)) # 26 plot
# length(unique(W$plot)) # 25 plots
# length(unique(E$plot)) # 26 plot
# data_N = raw(data,N)
# data_S = raw(data,S)
# data_W = raw(data,W)
# data_E = raw(data,E)
# 
# data$location = data$location*0.3048
# foothill = data[which(data$location<=1200),]
# lower_conifer = data[which(data$location>1200 & data$location<=2440),]
# upper_conifer = data[which(data$location>2440 & data$location<=2750),]
# high_country = data[which(data$location>2750),]
# length(unique(foothill$plot)) # 67 plots
# length(unique(lower_conifer$plot)) # 100 plot
# length(unique(upper_conifer$plot)) # 17 plots
# length(unique(high_country$plot)) # 69 plot
# data_F = raw(data,foothill)
# data_L = raw(data,lower_conifer)
# data_U = raw(data,upper_conifer)
# data_H = raw(data,high_country)
# 
# data$location = data$location*0.3048
# lower = data[which(data$location<=2440),]
# upper= data[which(data$location>2440),]
# length(unique(lower$plot)) # 167 plot
# length(unique(upper$plot)) # 86 plots
# data_L = raw(data,lower)
# data_U = raw(data,upper)

