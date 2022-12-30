# compile bathymetry csvs
#2021-01-11


#JP, already done!

#BB
library(plyr)
temp = "dbBathymetry/bathyBB/csvs"
data = list.files(path=temp, pattern="*.csv", full.names=TRUE)
bb.bathy = ldply(data, read_csv)
colnames(bb.bathy) = c("lat","lon","depth_m")
str(bb.bathy)
write.csv(bb.bathy, "dbBathymetry/bathyBB/BB_bathycoords_tracks.csv", row.names=FALSE)

#SC
temp = "dbBathymetry/bathySC/csvs"
data = list.files(path=temp, pattern="*.csv", full.names=TRUE)
sc.bathy = ldply(data, read_csv)
colnames(sc.bathy) = c("lat","lon","depth_m")
str(sc.bathy)
write.csv(sc.bathy, "dbBathymetry/bathySC/SC_bathycoords_tracks.csv", row.names=FALSE)


#WH
temp = "dbBathymetry/bathyWH/csvs"
data = list.files(path=temp, pattern="*.csv", full.names=TRUE)
wh.bathy = ldply(data, read_csv)
colnames(wh.bathy) = c("lat","lon","depth_m")
str(wh.bathy)
write.csv(wh.bathy, "dbBathymetry/bathyWH/WH_bathycoords_tracks.csv", row.names=FALSE)







