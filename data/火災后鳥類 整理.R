library(dplyr)
library(stringr)
################################# read data ####################################
data = read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\火災后鳥類\\Connell_et_al._Occurrence_data_Climate_fire.csv")
################################################################################
data_dry = data[which(data$Rain_period=="aBigDry_" ),]
data_wet = data[which(data$Rain_period=="bBigWet_" ),]
data_PostBigWet = data[which(data$Rain_period=="cPostBigWet_" ),]

data_dry = data.frame(t(data_dry[,7:81]))
data_wet = data.frame(t(data_wet[,7:81]))
data_PostBigWet = data.frame(t(data_PostBigWet[,7:81]))

data1 = data.frame(data_dry)
data2 = data.frame(data_wet)
data3 = data.frame(data_PostBigWet)

write.csv(data1, "D:\\nagisa\\NAGISA\\學校\\碩班\\研究\\00 code\\data\\火災后鳥類\\data1.csv")
write.csv(data2, "D:\\nagisa\\NAGISA\\學校\\碩班\\研究\\00 code\\data\\火災后鳥類\\data2.csv")
write.csv(data3, "D:\\nagisa\\NAGISA\\學校\\碩班\\研究\\00 code\\data\\火災后鳥類\\data3.csv")

# ################################################################################
table(data$Site)
table(data$Rain_period) # 雨
table(data$Veg_type) # 植被


unique(data$Site)
unique(data$Rain_period)
unique(data$Veg_type)

length(unique(data$Site))

################################################################################
p1 = rowSums(data1)[which(rowSums(data1)>0)]/ncol(data1)
p2 = rowSums(data2)[which(rowSums(data2)>0)]/ncol(data2)
p3 = rowSums(data3)[which(rowSums(data3)>0)]/ncol(data3)

mean(p1);mean(p2);mean(p3)
sd(p1)/mean(p1);sd(p2)/mean(p2);sd(p3)/mean(p3)
