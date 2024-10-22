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
load("data/ekispato.stops.kansai.xdr") # stops.kansai

plot(adm.bnd.prf.Kansai.simpl2.wgs84$geometry)
plot(stops.kansai,add=T,col="blue")

# UEA in kansai
uea.shp01.kansai=st_read("data/shp/uea.shp01.kansai.shp")

uea.osaka.shp=uea.shp01.kansai %>% filter(mea_mea==27100)

# st_crs(stops.kansai)
uea.osaka.shp.wgs84=st_transform(uea.osaka.shp,4326)
plot(uea.osaka.shp.wgs84$geometry)
save(uea.osaka.shp.wgs84,file="data/uea.osaka.shp.wgs84.xdr")

tI=st_intersects(uea.osaka.shp.wgs84,stops.kansai) %>% unlist() %>% sort()
stops.osaka=stops.kansai[tI,]
plot(stops.osaka,add=T,col="blue")
save(stops.osaka,file="data/stops.osaka.xdr")

stops.osaka.df= stops.osaka %>% st_drop_geometry()
write.csv(stops.osaka.df,file="data/stops.osaka.df.csv")
write.csv(stops.osaka.df,file="data/stops.osaka.df.CP932.csv", fileEncoding = "CP932")

#####

# Osaka UEA
# ekisupato stations and bus stops extract
# omit bus stops within 800m radius of rail station
# omit stops which is far from urban area (no population within certain radius based on census data 2015)
#####
#clear memory
rm(list=ls())
gc();gc();

library(dplyr)
library(sf)

load("data/stops.osaka.xdr") # stops.osaka

stops.osaka.rail=stops.osaka %>% filter(type==1)
stops.osaka.bus=stops.osaka %>% filter(type==8)

# omit bus stops within 800m radius of rail station
system.time({ # 7.31
  dist01=st_distance(stops.osaka.rail,stops.osaka.bus)
})
# dim(dist01)
# dist01[1:10,1:10]
min.dist.bus=apply(dist01,2,min)
tI=which(min.dist.bus<800)
length(tI)

stops.osaka.bus.use=stops.osaka.bus[-tI,]
stops.osaka.bus.omit=stops.osaka.bus[tI,]

load("data/uea.osaka.shp.wgs84.xdr") # uea.osaka.shp.wgs84

plot(uea.osaka.shp.wgs84$geometry)
plot(stops.osaka.bus.use$geometry,add=T,col="blue")
plot(stops.osaka.bus.omit$geometry,add=T,col="green")
plot(stops.osaka.rail$geometry,add=T,col="red")


# omit stops which is far from urban area (no population within certain radius based on census data 2015)
# E:/WorkDir01/data/GIS/Japan/e-stat/mesh_unzip.R
# population raster
tstr=paste0("E:/WorkDir01/data/GIS/Japan/e-stat/地図で見る統計/国勢調査/2015/5次メッシュ/raster/Kansai01.tif")
rram=raster(tstr)

uea.osaka.shp.JGD2000=st_transform(uea.osaka.shp.wgs84,4612)
rram.osaka=crop(rram,uea.osaka.shp.JGD2000) %>% mask(uea.osaka.shp.JGD2000)

# center of raster cells and its value
tI=which(values(rram.osaka)>0)
# length(tI)
popm.osaka.sf=xyFromCell(rram.osaka,tI,spatial=T) %>% st_as_sf()
plot(popm.osaka.sf,add=T,col="black",pch=20,cex=0.2)

popm.osaka.sf.wgs84=st_transform(popm.osaka.sf,4326)
system.time({ # 233.67 
  dist02b=st_distance(popm.osaka.sf.wgs84,stops.osaka.bus.use)
  dist02r=st_distance(popm.osaka.sf.wgs84,stops.osaka.rail)
})

# nearest bus stops from center of grid cells are used
tIb2=apply(dist02b,1,which.min) %>% unique() %>% sort()
tIr2=apply(dist02r,1,which.min) %>% unique() %>% sort()
# length(tIb2)
# dim(dist02b)
stops.osaka.bus.use2=stops.osaka.bus.use[tIb2,]
stops.osaka.bus.omit2=stops.osaka.bus.use[-tIb2,]
stops.osaka.rail.use2=stops.osaka.rail[tIr2,]
plot(stops.osaka.bus.omit2$geometry,add=T,col="yellow")
# dim(stops.osaka.bus.use2)


# shops
tdir="D:/大阪大学/教育/2024/研究室学生/M2_槌野/data/"
tstr=paste0(tdir,"kansai_Project.shp") # shops
shop.sf=st_read(tstr)
plot(shop.sf$geometry,add=T,col="orange")
tI=st_intersects(uea.osaka.shp.wgs84,shop.sf) %>% unlist() %>% sort()
shop.osaka.sf=shop.sf[tI,]
plot(shop.osaka.sf$geometry,add=T,col="orange")

# system.time({ # 15.45  
#   dist03b=st_distance(shop.osaka.sf,stops.osaka.bus.use)
#   dist03r=st_distance(shop.osaka.sf,stops.osaka.rail)
# })
# 
# tIb3=apply(dist03b,1,which.min) %>% unique() %>% sort()
# # length(tIb3)
# tIr3=apply(dist03r,1,which.min) %>% unique() %>% sort()
# # length(tIr3)
# 
# stops.osaka.bus.use.shopdest=stops.osaka.bus.use[tIb3,]
# stops.osaka.rail.shopdest=stops.osaka.rail[tIr3,]

# ?fasterize
# library(stars)

# rram.osaka.stars=st_as_stars(rram.osaka)
# shop.stars=st_rasterize(shop.sf,rram.osaka.stars)

shop.osaka.ras=rasterize(shop.sf,rram.osaka,fun="count",field="kansai_pbf") %>% 
  crop(uea.osaka.shp.JGD2000) %>% mask(uea.osaka.shp.JGD2000)

plot(shop.osaka.ras)
tI=which(values(shop.osaka.ras)>0)
shop.osaka.grid.sf=xyFromCell(shop.osaka.ras,tI,spatial=T) %>% st_as_sf()

plot(uea.osaka.shp.wgs84$geometry)
plot(shop.osaka.grid.sf,add=T,col="black",pch=20,cex=0.2)

shop.osaka.grid.sf.wgs84=st_transform(shop.osaka.grid.sf,4326)
system.time({ # 11.47   
  dist03b=st_distance(shop.osaka.grid.sf.wgs84,stops.osaka.bus.use)
  dist03r=st_distance(shop.osaka.grid.sf.wgs84,stops.osaka.rail)
})

tIb3=apply(dist03b,1,which.min) %>% unique() %>% sort()
# length(tIb3)
tIr3=apply(dist03r,1,which.min) %>% unique() %>% sort()
# length(tIr3)

stops.osaka.bus.use.shopdest=stops.osaka.bus.use[tIb3,]
stops.osaka.rail.shopdest=stops.osaka.rail[tIr3,]

plot(stops.osaka.bus.use.shopdest$geometry,add=T,col="blue")
# plot(stops.osaka.bus.omit$geometry,add=T,col="green")
plot(stops.osaka.rail.shopdest$geometry,add=T,col="red")



# medical facilities
tdir="D:/大阪大学/教育/2024/研究室学生/M2_槌野/data/"
tstr=paste0(tdir,"kansai_osm_points_med.shp") # 
med.sf=st_read(tstr)
# plot(med.sf$geometry,add=T,col="purple")
tI=st_intersects(uea.osaka.shp.wgs84,med.sf) %>% unlist() %>% sort()
med.osaka.sf=med.sf[tI,]
plot(med.osaka.sf$geometry,add=T,col="purple")

med.osaka.ras=rasterize(med.sf,rram.osaka,fun="count",field="kansai_osm") %>% 
  crop(uea.osaka.shp.JGD2000) %>% mask(uea.osaka.shp.JGD2000)
# plot(med.osaka.ras)
tI=which(values(med.osaka.ras)>0)
med.osaka.grid.sf=xyFromCell(med.osaka.ras,tI,spatial=T) %>% st_as_sf()

plot(uea.osaka.shp.wgs84$geometry)
plot(med.osaka.grid.sf,add=T,col="black",pch=20,cex=0.2)

med.osaka.grid.sf.wgs84=st_transform(med.osaka.grid.sf,4326)
system.time({ # 11.47   
  dist04b=st_distance(med.osaka.grid.sf.wgs84,stops.osaka.bus.use)
  dist04r=st_distance(med.osaka.grid.sf.wgs84,stops.osaka.rail)
})

tIb4=apply(dist04b,1,which.min) %>% unique() %>% sort()
# length(tIb3)
tIr4=apply(dist04r,1,which.min) %>% unique() %>% sort()
# length(tIr3)

stops.osaka.bus.use.meddest=stops.osaka.bus.use[tIb4,]
stops.osaka.rail.meddest=stops.osaka.rail[tIr4,]

plot(stops.osaka.bus.use.meddest$geometry,add=T,col="blue")
plot(stops.osaka.rail.meddest$geometry,add=T,col="red")

# stops at residential grids
stops.osaka.bus.use2
stops.osaka.rail.use2
stops.res=rbind(stops.osaka.rail.use2,stops.osaka.bus.use2)

# stops at destination: shops and medical facilities
stops.osaka.bus.use.shopdest
stops.osaka.rail.shopdest

stops.osaka.bus.use.meddest
stops.osaka.rail.meddest

stops.bus.des=rbind(stops.osaka.bus.use.shopdest,stops.osaka.bus.use.meddest) %>% unique()
stops.rail.des=rbind(stops.osaka.rail.shopdest,stops.osaka.rail.meddest) %>% unique()

stops.des=rbind(stops.rail.des,stops.bus.des)

# assume to omit OD pair which is more than 60 km in beelihne

dist05=st_distance(stops.res,stops.des)
hist(dist05)
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x
}
dist05=clean_units(dist05)
tIrc=which(dist05>0 & dist05<40000, arr.ind = T)
dim(tIrc)

stop.odpn=data.frame(ori=stops.res$Name_sc[tIrc[,1]],des=stops.des$Name_sc[tIrc[,2]])
head(stop.odpn)
write.csv(stop.odpn,file="data/stop.odpn.csv")

#####
