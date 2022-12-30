#load bathymetry data. 
#area and volume of each 1m section for each lake
#created 2021-02-02

#JP
bath.jp = read.csv("dbBathymetry/bathy_csvs/JP_vol.csv",header=TRUE)
colnames(bath.jp)[1] = "LakeID" 
bath.jp[,2:5] = sapply(bath.jp[,2:5],as.numeric)
str(bath.jp)
#write csv
write.csv(bath.jp, "library/bath.jp.csv", row.names=FALSE)

#SC
bath.sc = read.csv("dbBathymetry/bathy_csvs/SC_vol.csv",header=TRUE)
colnames(bath.sc)[1] = "LakeID" 
bath.sc[,2:5] = sapply(bath.sc[,2:5],as.numeric)
str(bath.sc)
#write csv
write.csv(bath.sc, "library/bath.sc.csv", row.names=FALSE)

#BB
bath.bb = read.csv("dbBathymetry/bathy_csvs/BB_vol.csv",header=TRUE)
colnames(bath.bb)[1] = "LakeID" 
bath.bb[,2:5] = sapply(bath.bb[,2:5],as.numeric)
str(bath.bb)
#write csv
write.csv(bath.bb, "library/bath.bb.csv", row.names=FALSE)

#WH
bath.wh = read.csv("dbBathymetry/bathy_csvs/WH_vol.csv",header=TRUE)
colnames(bath.wh)[1] = "LakeID" 
bath.wh[,2:5] = sapply(bath.wh[,2:5],as.numeric)
str(bath.wh)
#write csv
write.csv(bath.wh, "library/bath.wh.csv", row.names=FALSE)

#load other lake info metadata
bath.all = read.csv("dbBathymetry/bathy_csvs/Lake_descriptions.csv",header=T)
colnames(bath.all)[1] = "LakeID" 
str(bath.all)
bath.all$Date = as.Date(bath.all$Date)
bath.all[,c(3,6)] = sapply(bath.all[,c(3,6)],as.numeric)
#write csv
write.csv(bath.all, "library/bath.metadata.csv", row.names=FALSE)



