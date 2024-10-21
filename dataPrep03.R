# ekisupato stations and bus stops extract
#####
#clear memory
rm(list=ls())
gc();gc();

library(dplyr)
library(sf)

# ekisupato stops
stop_sf2=st_read("E:/WorkDir01/prog/R/2024/2024_ekisupato/data/shape/stop_sf2.shp")

#Kansai administration boundary
adm.bnd.prf.Kansai.simpl2.wgs84=st_read("data/shp/adm.bnd.prf.Kansai.simpl2.wgs84.shp")

# select stops in kansai
tI=st_intersects(adm.bnd.prf.Kansai.simpl2.wgs84,stop_sf2) %>% unlist() %>% sort()
stops.kansai=stop_sf2[tI,]
save(stops.kansai,file="data/ekispato.stops.kansai.xdr")

plot(adm.bnd.prf.Kansai.simpl2.wgs84$geometry)
plot(stops.kansai,add=T,col="blue")

# UEA in kansai
uea.shp01.kansai=st_read("data/shp/uea.shp01.kansai.shp")

uea.osaka.shp=uea.shp01.kansai %>% filter(mea_mea==27100)

st_crs(stops.kansai)
uea.osaka.shp.wgs84=st_transform(uea.osaka.shp,4326)
plot(uea.osaka.shp.wgs84$geometry)
tI=st_intersects(uea.osaka.shp.wgs84,stops.kansai) %>% unlist() %>% sort()
stops.osaka=stops.kansai[tI,]
plot(stops.osaka,add=T,col="blue")


stops.osaka.df= stops.osaka %>% st_drop_geometry()
write.csv(stops.osaka.df,file="data/stops.osaka.df.csv")
write.csv(stops.osaka.df,file="data/stops.osaka.df.CP932.csv", fileEncoding = "CP932")

#####


# ekisupato stations and bus stops extract
#####
#clear memory
rm(list=ls())
gc();gc();
