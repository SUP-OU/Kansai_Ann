# data preparation 01

#####
#clear memory
rm(list=ls())
gc();gc();


library(dplyr)
library(sf)

# 2010 Kinki PT trip generation/attraction dataset.
# E:/WorkDir01/prog/R/2023/2023_KansaiNTL/UEA02.R
# load("E:/WorkDir01/prog/R/2023/2023_KansaiNTL/data/kpt10.tga.sf.xdr") # kpt10.tga.sf, 4612
# plot(kpt10.tga.sf$geometry)

# # census small area 2015
# tdir="E:/WorkDir01/data/GIS/Japan/e-stat/境界データ/小地域/2015/国勢調査/down_cen_15_smallzone/unzip/"
# tfn=list.files(tdir,pattern="shp")
# 
# # kansai prefecture codes
# kpc=formatC(25:30,width=2,flag="0")
# # kfn=paste0("h27ka",kpc,".shp")
# cenz.Kansai.L=as.list(rep(NA,length(kpc)))
# system.time({ # 14.05 sec
#   for(ii in 1:length(kpc)){ # ii=1
#     tstr=paste0(tdir,"h27ka",kpc[ii],".shp")
#     tka=st_read(tstr) # EPSG:4612
#     tka=st_make_valid(tka)
#     cenz.Kansai.L[[ii]]=tka
#     save(tka,file=paste0("data/census2015/tka_",kpc[ii],".xdr"))
#   }
# })
# 
# cenz.Kansai=do.call(rbind,cenz.Kansai.L)
# save(cenz.Kansai,file="data/census2015/cenz.Kansai.xdr")

# st_write(cenz.Kansai,dsn="data/shp/cenz.Kansai.2015.shp", layer_options = "ENCODING=cp932")
# load("data/census2015/cenz.Kansai.xdr") # cenz.Kansai

# # simplified Kansai region
# tstr="D:/大阪大学/教育/2024/ゼミ/スタートアップゼミ/R/data/shp/adm.bnd.prf.Kansai.simpl2.shp"
# adm.bnd.prf.Kansai.simpl2=st_read(tstr)
# adm.bnd.prf.Kansai.simpl2.wgs84=st_transform(adm.bnd.prf.Kansai.simpl2,4326)
# st_write(adm.bnd.prf.Kansai.simpl2.wgs84,dsn="data/shp/adm.bnd.prf.Kansai.simpl2.wgs84.shp", layer_options = "ENCODING=cp932")
adm.bnd.prf.Kansai.simpl2.wgs84=st_read("data/shp/adm.bnd.prf.Kansai.simpl2.wgs84.shp")
adm.bnd.prf.Kansai.simpl2=st_transform(adm.bnd.prf.Kansai.simpl2.wgs84,6668) # JGD2011

# OSM
# tstr="E:/WorkDir01/prog/R/2023/2023_OSM_Ext/data/shape01/kansai.pbf.highway2.shp"
# kansai.pbf.highway2=st_read(tstr)
# # st_crs(kansai.pbf.highway2)
# tI=st_intersects(kansai.pbf.highway2,adm.bnd.prf.Kansai.simpl2.wgs84) %>% lapply(length) %>% unlist()
# kansai.pbf.highway3=kansai.pbf.highway2[which(tI>0),]
# # plot(kansai.pbf.highway3$geometry,col="blue")
# st_write(kansai.pbf.highway3,dsn="data/shp/kansai.pbf.highway3.shp", layer_options = "ENCODING=cp932")
# kansai.pbf.highway3=st_read("data/shp/kansai.pbf.highway3.shp")


# UEA urban employment area
# tstr="E:/WorkDir01/data/GIS/Japan/都市雇用圏/2015/MEAmap2.shp"
# MEAmap2=st_read(tstr)
# 
# # UEA
# #F:\WorkDir01\prog\R\2020\2020_NationalLandPlanning
# # tdir="G:/香川大学/02教育/2020_学生/奥村/data2020-2021/okumura/都市雇用圏-ok/"
# tstr="F:/WorkDir01/prog/R/2020/2020_NationalLandPlanning/data/shape/MEAmap2.shp"
# uea.shp=st_read(tstr)
# 
# # UEA name
# tstr="F:/WorkDir01/prog/R/2020/2020_NationalLandPlanning/data/tDa7_01.csv"
# uea.name=read.csv(tstr,header=T,stringsAsFactors = F,encoding = "UTF-8")
# uea.shp01=left_join(uea.shp,uea.name,by=c("mea_mea"="X.U.FEFF.CODE"))
# tstr="E:/WorkDir01/prog/R/2022/2022_E-STAT_API/data/uea.shp01.xdr"
# load(tstr) # uea.shp01
# st_write(uea.shp01,dsn="data/shp/uea.shp01.shp", layer_options = "ENCODING=cp932")
# 
# adm.bnd.prf.Kansai.simpl3=st_transform(adm.bnd.prf.Kansai.simpl2,4612)
# uea.cnt=st_centroid(uea.shp01)
# tI=st_within(uea.cnt,adm.bnd.prf.Kansai.simpl3) %>% lapply(length) %>% unlist()
# 
# uea.shp01.kansai=uea.shp01[which(tI>0),]
# 
# # st_crs(uea.shp01)
# # st_crs(adm.bnd.prf.Kansai.simpl2)
# st_write(uea.shp01.kansai,dsn="data/shp/uea.shp01.kansai.shp", layer_options = "ENCODING=cp932")
# uea.shp01.kansai=st_read("data/shp/uea.shp01.kansai.shp")

# kpt10.tga.sf=st_read(tstr)
# st_crs(kpt10.tga.sf)=4612
# kpt10.tga.sf=st_make_valid(kpt10.tga.sf)
# st_crs(tka)

# urban employment area
# E:\WorkDir01\data\GIS\Japan\都市雇用圏\2015


library(raster)
library(fasterize)
library(ggplot2)
library(ggspatial)

###
# night time light
# E:/WorkDir01/prog/R/2023/2023_KansaiNTL/UEA02.R
# yyv=2012:2022
# yy=11
# tstr=paste0("E:/WorkDir01/prog/R/2023/2023_KansaiNTL/data/tDNB.msk.",yyv[yy],".xdr")
# load(tstr) # tDNB.msk
# save(tDNB.msk,file="data/tDNB.msk.2022.xdr")
# load("data/tDNB.msk.2022.xdr") # tDNB.msk


# basemap=ggplot(data = adm.bnd.prf.Kansai.simpl2.wgs84) +
#   geom_sf(data=adm.bnd.prf.Kansai.simpl2.wgs84,fill="white",lwd=0.2,color="gray")+
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          height=unit(3,"cm"),width=unit(3,"cm"),
#                          pad_x = unit(1, "cm"), pad_y = unit(2, "cm"),
#                          style = north_arrow_fancy_orienteering(text_size=24)) +
#   theme(text = element_text(size = 24),legend.key.width=unit(1,"cm"),legend.key.height=unit(3,"cm"))
# 
# 
# tDNB.msk.df <- tDNB.msk %>% #raster data
#   as.data.frame(xy = TRUE) %>% na.omit()
# names(tDNB.msk.df)[3]="DNB"
# 
# bp2=c(10,50,100,200)
# NTL.map <- basemap +
#   geom_sf(fill = NA) +
#   geom_raster(data = tDNB.msk.df, aes(x, y, fill = DNB)) +
#   theme_bw()+
#   theme(text = element_text(size = 24),legend.key.width=unit(1,"cm"),legend.key.height=unit(3,"cm"))+
#   scale_fill_viridis_c(breaks=bp2,labels=bp2,option = "viridis", trans = scales::pseudo_log_trans(sigma=5,base=2),
#                        name=bquote(.("nW")/cm^2/sr),limits=c(0,150),oob = scales::squish)+
#   xlab(NULL) + ylab(NULL)
# 
# tstr=paste0("figs/map01/NTL_",yyv[yy],".png")
# ggsave(tstr, plot = NTL.map, width=1000,height=1000,units="px",dpi=72)
# 
# 
# # writeRaster(tDNB.msk.df, "data/tif/DNB_2022_Kansai", format = "GTiff")
# writeRaster(tDNB.msk,filename="data/tif/DNB_2022_Kansai", format="GTiff", overwrite=TRUE)

# adm.bnd.prf.Kansai.simpl2.wgs84
# tDNB=crop(DNB,adm.bnd.prf.Kansai.simpl2.wgs84)

###
# building height
# GHSL/GHS-BUILT-H
tstr="F:/data/GIS/World/GHSL/GHS-BUILT-H/GHS_BUILT_H_AGBH_E2018_GLOBE_R2023A_54009_100_V1_0.tif"
GHS_BUILT=raster(tstr)

# adm.bnd.prf.Kansai.simpl2.mol=st_transform(adm.bnd.prf.Kansai.simpl2,9001)
adm.bnd.prf.Kansai.simpl2.mol=st_transform(adm.bnd.prf.Kansai.simpl2,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
# plot(adm.bnd.prf.Kansai.simpl2.mol$geometry)
tGHS_BUILT=crop(GHS_BUILT,adm.bnd.prf.Kansai.simpl2.mol)
# plot(tGHS_BUILT)
# plot(adm.bnd.prf.Kansai.simpl2.mol$geometry,add=T)
# crs(GHS_BUILT)
# st_crs(adm.bnd.prf.Kansai.simpl2)

# reproject to lat-lon
# tGHS_BUILT.wgs84=projectRaster(tGHS_BUILT,method="ngb",crs="+proj=longlat +datum=WGS84 +no_defs ") %>% crop(tbnd.sf)
system.time({ # 23.74 sec
  tGHS_BUILT.jgd2011=projectRaster(tGHS_BUILT,method="ngb",crs=6668) %>% crop(adm.bnd.prf.Kansai.simpl2)
})
# plot(tGHS_BUILT.jgd2011)
# plot(adm.bnd.prf.Kansai.simpl2$geometry,add=T)

tm2=adm.bnd.prf.Kansai.simpl2 %>% mutate(flg=1)
adm.bnd.prf.Kansai.simpl2.ras=fasterize(tm2,tGHS_BUILT.jgd2011,field="flg",fun="first")

adm.bnd.prf.Kansai.simpl2.ras[adm.bnd.prf.Kansai.simpl2.ras==0]=NA
tGHS_BUILT.jgd2011.msk=mask(tGHS_BUILT.jgd2011,adm.bnd.prf.Kansai.simpl2.ras)
plot(tGHS_BUILT.jgd2011.msk)
plot(adm.bnd.prf.Kansai.simpl2$geometry,add=T)


tGHS_BUILT.jgd2011.msk.df <- tGHS_BUILT.jgd2011.msk %>% #raster data
  as.data.frame(xy = TRUE) %>% na.omit()
names(tGHS_BUILT.jgd2011.msk.df)[3]="BUILT"

bp2=c(5,10,15,30)
tGHS_BUILT.map <- basemap +
  geom_sf(fill = NA) +
  geom_raster(data = tGHS_BUILT.jgd2011.msk.df, aes(x, y, fill = BUILT)) +
  theme_bw()+
  theme(text = element_text(size = 24),legend.key.width=unit(1,"cm"),legend.key.height=unit(3,"cm"))+
  scale_fill_viridis_c(breaks=bp2,labels=bp2,option = "viridis", trans = scales::pseudo_log_trans(sigma=5,base=2),
                       # name=bquote(.("nW")/cm^2/sr),limits=c(0,35),oob = scales::squish)+
                        name="meter",limits=c(0,35),oob = scales::squish)+
  xlab(NULL) + ylab(NULL)

tstr=paste0("figs/map01/tGHS_BUILT_",yyv[yy],".png")
ggsave(tstr, plot = NTL.map, width=1000,height=1000,units="px",dpi=72)


# writeRaster(tDNB.msk.df, "data/tif/DNB_2022_Kansai", format = "GTiff")
writeRaster(tGHS_BUILT.jgd2011.msk,filename="data/tif/tGHS_BUILT.jgd2011.msk", format="GTiff", overwrite=TRUE)

###
# download modis data
# https://cran.r-project.org/web/packages/MODISTools/vignettes/modistools-vignette.html

library(MODISTools)
pds=mt_products() # check product list

###
# land surface temperature
# 15  MOD11A2     MODIS/Terra Land Surface Temperature and Emissivity (LST) 8-Day L3 Global 1 km SIN Grid     8 day              1000
# https://lpdaac.usgs.gov/products/mod11a1v061/
bands=mt_bands(product = "MOD11A2") # 

# load("data/tUc.sf2.xdr") # tUc.sf2, UN agg mt 1million pop @2020
# jj=grep("Tokyo",tUc.sf2$Urban.Agglomeration) # case of tokyo
# txy=st_coordinates(tUc.sf2[jj,])
# # tUc.sf2$Urban.Agglomeration[jj]
# 
# tstr=paste0("data/RDTM/tTD2_",tUc.sf2$ISO3[jj],"_",tUc.sf2$Index[jj],".xdr")
# load(tstr) # tTD2

# txy=st_centroid(adm.bnd.prf.Kansai.simpl2.wgs84) %>% st_coordinates() %>% apply(2,mean)
txy=st_bbox(adm.bnd.prf.Kansai.simpl2.wgs84) %>% st_as_sfc %>% st_centroid %>% st_coordinates()
# https://stackoverflow.com/questions/50978948/how-to-recover-from-timeout-error-in-modistools
# dates=mt_dates(product = "MOD11A2",lat=txy[2],lon=txy[1]) # dates of the data
dates=mt_dates(product = "MOD11A2",lat=txy[2],lon=txy[1]) # dates of the data
# range(dates$calendar_date)
# head(dates)

yyv0=grep("2018",dates$calendar_date)
yyv=821+(0:11)*4
# yyv=c(13,17,21)
# dates[yyv,]

# area including the target region in the sinusoidal system
# rbb=extent(tTD2) # extent of target (based on time distance)
rbb=extent(adm.bnd.prf.Kansai.simpl2.wgs84)
# bounding box sf
# tbnd=st_bbox(c(xmin=rbb[1],ymin=rbb[3],xmax=rbb[2],ymax=rbb[4]),src=st_crs(4326))
# tbnd.sf=st_as_sfc(tbnd) %>% st_set_crs(4326) %>% st_sf()
tbnd.sf=st_bbox(adm.bnd.prf.Kansai.simpl2.wgs84) %>% st_as_sfc %>% st_sf()
tbnd.sf.sn=st_transform(tbnd.sf,crs="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs ")
# plot(tbnd.sf.sn$geometry)
# extent of bounding box
tbsnv.ll=extent(tbnd.sf)
tbsnv.sn=extent(tbnd.sf.sn)
# mt_to_raster
# Modis tool has limit in downloading size
DD2v=c((tbsnv.sn[2]-tbsnv.sn[1]),(tbsnv.sn[4]-tbsnv.sn[3]))/10^3 #%>% ceiling() # 
DD2v=ceiling(DD2v) # extent size in km

# the reading segment should be less than 50 km to avoid the data transfer limit
# divide into segment
nnx=ceiling(DD2v[1]/50)
nny=ceiling(DD2v[2]/50)

# segment size (degree)
ddx=(tbsnv.ll[2]-tbsnv.ll[1])/nnx
ddy=(tbsnv.ll[4]-tbsnv.ll[3])/nny

# center of each segment (lat-lon)
xxcv=tbsnv.ll[1]+(ddx/2)*(((1:nnx)*2-1))
yycv=tbsnv.ll[3]+(ddy/2)*(((1:nny)*2-1))

# segment size (km)
ddx.sn=ceiling(DD2v[1]/nnx)
ddy.sn=ceiling(DD2v[2]/nny)

MOD11A2_01L=as.list(rep(NA,2))
for(idn in 1:2){ # day / night
  MOD11A2_01L[[idn]]=as.list(rep(NA,length(yyv)))
  for(iit in 1:length(yyv)){ 
    MOD11A2_01L[[idn]][[iit]]=as.list(rep(NA,nnx))
    for(iix in 1:nnx){
      MOD11A2_01L[[idn]][[iit]][[iix]]=as.list(rep(NA,nny))
    }
  }
}

system.time({ # 8315.66 sec
  for(iit in 1:length(yyv)){
    for (iix in 1:nnx){
      for(iiy in 1:nny){ # iix=7;iiy=1
        cat("iit=",iit,"iix=",iix,",iiy=",iiy,"\n")
        MOD11A2_01L[[1]][[iit]][[iix]][[iiy]]=mt_subset(product = "MOD11A2",lat=yycv[iiy],lon=xxcv[iix],
                                                        band="LST_Day_1km",start=dates[yyv[iit],2],end=dates[yyv[iit],2],
                                                        km_lr=ddx.sn,km_ab=ddy.sn,site_name="Kansai",
                                                        out_dir="data/modis")
        MOD11A2_01L[[2]][[iit]][[iix]][[iiy]]=mt_subset(product = "MOD11A2",lat=yycv[iiy],lon=xxcv[iix],
                                                        band="LST_Night_1km",start=dates[yyv[iit],2],end=dates[yyv[iit],2],
                                                        km_lr=ddx.sn,km_ab=ddy.sn,site_name="Kansai",
                                                        out_dir="data/modis")
      }
    }
  }
})
# tUc.sf2$Urban.Agglomeration

# observation time: hour*10
MOD11A2_TT_L=as.list(rep(NA,2))
for(idn in 1:2){ # day / night
  MOD11A2_TT_L[[idn]]=as.list(rep(NA,length(yyv)))
  for(iit in 1:length(yyv)){ 
    MOD11A2_TT_L[[idn]][[iit]]=as.list(rep(NA,nnx))
    for(iix in 1:nnx){
      MOD11A2_TT_L[[idn]][[iit]][[iix]]=as.list(rep(NA,nny))
    }
  }
}

system.time({ # 8315.66 sec: not yet run
  for(iit in 1:length(yyv)){
    for (iix in 1:nnx){
      for(iiy in 1:nny){ # iix=7;iiy=1
        cat("iit=",iit,"iix=",iix,",iiy=",iiy,"\n")
        MOD11A2_TT_L[[1]][[iit]][[iix]][[iiy]]=mt_subset(product = "MOD11A2",lat=yycv[iiy],lon=xxcv[iix],
                                                        band="Day_view_time",start=dates[yyv[iit],2],end=dates[yyv[iit],2],
                                                        km_lr=ddx.sn,km_ab=ddy.sn,site_name="Kansai",
                                                        out_dir="data/modis")
        MOD11A2_TT_L[[2]][[iit]][[iix]][[iiy]]=mt_subset(product = "MOD11A2",lat=yycv[iiy],lon=xxcv[iix],
                                                        band="Night_view_time",start=dates[yyv[iit],2],end=dates[yyv[iit],2],
                                                        km_lr=ddx.sn,km_ab=ddy.sn,site_name="Kansai",
                                                        out_dir="data/modis")
      }
    }
  }
})

# tstr=paste0("data/MOD17A3HGF_01L.xdr")
# tstr=paste0("data/MOD17A3HGF_01L_",tUc$ISO3,"_",tUc$Urban.Agglomeration,".xdr")
tstr=paste0("data/MOD11A2_01L_Kansai.xdr")
save(MOD11A2_01L,file=tstr)
load(tstr) # MOD17A3HGF_01L

# observation time
tstr=paste0("data/MOD11A2_TT_L_Kansai.xdr")
save(MOD11A2_TT_L,file=tstr)
load(tstr) # MOD17A3HGF_01L


# MOD17A3HGF_01L[[iit]][[iix]][[iiy]] %>% head

# convert data to raster: sinusoidal
system.time({ # 207.24 sec
  tmp0.rs0L=as.list(rep(NA,2))
  for (idn in 1:2){
    tmp0.rs0L[[idn]]=as.list(rep(NA,length(yyv)))
  }
  for(iit in 1:length(yyv)){
    tmp1.rs0L.d=tmp1.rs0L.n=as.list(rep(NA,nnx))
    for (iix in 1:nnx){ # iix=1
      tmp2.rs0L.d=tmp2.rs0L.n=as.list(rep(NA,nny))
      for(iiy in 1:nny){ # iix=iiy=1 
        cat("iit=",iit,"iix=",iix,",iiy=",iiy,"\n")
        # aa=mt_to_terra(df=MOD11A2_01L[[1]][[iit]][[iix]][[iiy]],reproject=F)
        # bb=as(aa,"Raster")
        # tmp2.rs0L.d[[iiy]]=mt_to_raster(df=MOD11A2_01L[[1]][[iit]][[iix]][[iiy]],reproject=F)
        # tmp2.rs0L.n[[iiy]]=mt_to_raster(df=MOD11A2_01L[[2]][[iit]][[iix]][[iiy]],reproject=F)
        tmp2.rs0L.d[[iiy]]=mt_to_terra(df=MOD11A2_01L[[1]][[iit]][[iix]][[iiy]],reproject=F) %>% as("Raster")
        tmp2.rs0L.n[[iiy]]=mt_to_terra(df=MOD11A2_01L[[2]][[iit]][[iix]][[iiy]],reproject=F) %>% as("Raster")
      }
      tmp1.rs0L.d[[iix]]=do.call(merge,tmp2.rs0L.d)
      tmp1.rs0L.n[[iix]]=do.call(merge,tmp2.rs0L.n)
    }
    tmp0.rs0L[[1]][[iit]]=do.call(merge,tmp1.rs0L.d) # day
    tmp0.rs0L[[2]][[iit]]=do.call(merge,tmp1.rs0L.n) # night
  }
  
})


# basemap.sinu=ggplot(data = tbnd.sf.sn) +
#   geom_sf(data=tbnd.sf.sn,fill="white",lwd=0.2,color="gray")+
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          height=unit(3,"cm"),width=unit(3,"cm"),
#                          pad_x = unit(1, "cm"), pad_y = unit(2, "cm"),
#                          style = north_arrow_fancy_orienteering(text_size=24)) +
#   theme(text = element_text(size = 24),legend.key.width=unit(1,"cm"),legend.key.height=unit(3,"cm"))


# MOD11A2.df <- tmp0.rs0L[[1]][[iit]] %>% #raster data
# MOD11A2.df <- tmp0.rs0L[[2]][[iit]] %>% #raster data
#   as.data.frame(xy = TRUE) %>% na.omit()
# names(MOD11A2.df)[3]="LST"
# 
# bp2=c(260,270,280,290,300)
# MOD11A2.map <- basemap.sinu +
#   geom_sf(fill = NA) +
#   geom_raster(data = MOD11A2.df, aes(x, y, fill = LST)) +
#   theme_bw()+
#   theme(text = element_text(size = 24),legend.key.width=unit(1,"cm"),legend.key.height=unit(3,"cm"))+
#   scale_fill_viridis_c(breaks=bp2,labels=bp2,option = "viridis", trans = scales::pseudo_log_trans(sigma=5,base=2),
#                        name="kelvin",limits=c(260,300),oob = scales::squish)+
#   xlab(NULL) + ylab(NULL)
values(tmp0.rs0L[[1]][[iit]]) %>% head()

# library(RColorBrewer)
# # display.brewer.all()
# brks=c(0,26:30*10)
# # myCol = terrain.colors(1:length(brks))
# myCol = brewer.pal(length(brks),"Spectral") %>% rev()
# plot(tmp0.rs0L[[1]][[iit]],breaks=brks,col=myCol)
# plot(tmp0.rs0L[[1]][[iit]],breaks=brks)
# ?mt_to_raster
# ?mt_to_terra

# tmp2.rs0L[[iiy]] %>% values()
# reproject to lat-lon
system.time({
  MOD11A2.wgs84.ras.01L=as.list(rep(NA,2))
  for (idn in 1:2){
    MOD11A2.wgs84.ras.01L[[idn]]=as.list(rep(NA,length(yyv)))
    for(iit in 1:length(yyv)){
      # MCD12Q1.wgs84.ras.01L[[iit]]=projectRaster(tmp0.rs0L[[iit]],method="ngb",crs="+proj=longlat +datum=WGS84 +no_defs ")
      # MOD17A3HGF.wgs84.ras.01L[[iit]]=projectRaster(tmp0.rs0L[[iit]],method="ngb",crs="+proj=longlat +datum=WGS84 +no_defs ") %>% crop(tbnd.sf)
      MOD11A2.wgs84.ras.01L[[idn]][[iit]]=projectRaster(tmp0.rs0L[[idn]][[iit]],method="ngb",crs="+proj=longlat +datum=WGS84 +no_defs ") %>% 
        crop(tbnd.sf) # %>% resample(tTD2,method="ngb")
      # values(MOD11A2.wgs84.ras.01L[[iit]])[which(values(MOD11A2.wgs84.ras.01L[[iit]])==32766/10^4)]=NA # water bodies=NA
    }
  }
})

# MOD11A2.wgs84.ras.01L[[idn]][[iit]]
# range(values(MOD11A2.wgs84.ras.01L[[idn]][[iit]]))
# 
# brks=c(0,26:30*10)
# plot(MOD11A2.wgs84.ras.01L[[idn]][[iit]],breaks=brks)

MOD11A2.df <- MOD11A2.wgs84.ras.01L[[idn]][[iit]] %>% #raster data
  as.data.frame(xy = TRUE) %>% na.omit()
names(MOD11A2.df)[3]="LST"

bp2=c(260,270,280,290,300)
MOD11A2.map <- basemap +
  geom_sf(fill = NA) +
  geom_raster(data = MOD11A2.df, aes(x, y, fill = LST)) +
  theme_bw()+
  theme(text = element_text(size = 24),legend.key.width=unit(1,"cm"),legend.key.height=unit(3,"cm"))+
  scale_fill_viridis_c(breaks=bp2,labels=bp2,option = "viridis", trans = scales::pseudo_log_trans(sigma=5,base=2),
                       name="kelvin",limits=c(260,300),oob = scales::squish)+
  xlab(NULL) + ylab(NULL)


tstr=paste0("data/MOD11A2.wgs84.ras.01L_Kansai.xdr")
save(MOD11A2.wgs84.ras.01L,file=tstr) # annual NPP list for years (2012,2016,2020)

adm.bnd.prf.Kansai.MOD11A2.wgs84.simpl2.ras=fasterize(tm2,MOD11A2.wgs84.ras.01L[[1]][[1]],field="flg",fun="first")
adm.bnd.prf.Kansai.MOD11A2.wgs84.simpl2.ras[adm.bnd.prf.Kansai.MOD11A2.wgs84.simpl2.ras==0]=NA



# dates[yyv,2]
for(yyi in 1:length(yyv)){ # yyi=1
  tras=mask(MOD11A2.wgs84.ras.01L[[1]][[yyi]],adm.bnd.prf.Kansai.MOD11A2.wgs84.simpl2.ras)
  tstr=paste0("data/tif/MOD11A2.wgs84_Kansai_day_",dates[yyv[yyi],2],".xdr")
  writeRaster(tras,filename=tstr, format="GTiff", overwrite=TRUE)
}
# plot(tras)
# plot(adm.bnd.prf.Kansai.simpl2$geometry,add=T)


#####


###
# 16      MOD13Q1                    MODIS/Terra Vegetation Indices (NDVI/EVI) 16-Day L3 Global 250m SIN Grid    16 day
#####
#clear memory
rm(list=ls())
gc();gc();

library(dplyr)
library(sf)

library(raster)
library(fasterize)
library(ggplot2)
library(ggspatial)
adm.bnd.prf.Kansai.simpl2.wgs84=st_read("data/shp/adm.bnd.prf.Kansai.simpl2.wgs84.shp")
adm.bnd.prf.Kansai.simpl2=st_transform(adm.bnd.prf.Kansai.simpl2.wgs84,6668) # JGD2011
adm.bnd.prf.Kansai.simpl2.mol=st_transform(adm.bnd.prf.Kansai.simpl2,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
tm2=adm.bnd.prf.Kansai.simpl2 %>% mutate(flg=1)
library(MODISTools)
pds=mt_products() # check product list
txy=st_bbox(adm.bnd.prf.Kansai.simpl2.wgs84) %>% st_as_sfc %>% st_centroid %>% st_coordinates()
rbb=extent(adm.bnd.prf.Kansai.simpl2.wgs84)
# bounding box sf
# tbnd=st_bbox(c(xmin=rbb[1],ymin=rbb[3],xmax=rbb[2],ymax=rbb[4]),src=st_crs(4326))
# tbnd.sf=st_as_sfc(tbnd) %>% st_set_crs(4326) %>% st_sf()
tbnd.sf=st_bbox(adm.bnd.prf.Kansai.simpl2.wgs84) %>% st_as_sfc %>% st_sf()
tbnd.sf.sn=st_transform(tbnd.sf,crs="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs ")
# plot(tbnd.sf.sn$geometry)
# extent of bounding box
tbsnv.ll=extent(tbnd.sf)
tbsnv.sn=extent(tbnd.sf.sn)
# mt_to_raster
# Modis tool has limit in downloading size
DD2v=c((tbsnv.sn[2]-tbsnv.sn[1]),(tbsnv.sn[4]-tbsnv.sn[3]))/10^3 #%>% ceiling() # 
DD2v=ceiling(DD2v) # extent size in km

# the reading segment should be less than 50 km to avoid the data transfer limit
# divide into segment
nnx=ceiling(DD2v[1]/50)
nny=ceiling(DD2v[2]/50)

# segment size (degree)
ddx=(tbsnv.ll[2]-tbsnv.ll[1])/nnx
ddy=(tbsnv.ll[4]-tbsnv.ll[3])/nny

# center of each segment (lat-lon)
xxcv=tbsnv.ll[1]+(ddx/2)*(((1:nnx)*2-1))
yycv=tbsnv.ll[3]+(ddy/2)*(((1:nny)*2-1))

# segment size (km)
ddx.sn=ceiling(DD2v[1]/nnx)
ddy.sn=ceiling(DD2v[2]/nny)


bands=mt_bands(product = "MOD13Q1") # 
dates=mt_dates(product = "MOD13Q1",lat=txy[2],lon=txy[1]) # dates of the data
yyv0=grep("2018",dates$calendar_date)
yyv=412+(0:11)*2
# dates[yyv,]


# Vegetation Indices (NDVI/EVI)
system.time({ # 8315.66 sec
  for(iit in 1:length(yyv)){
    for (iix in 1:nnx){
      for(iiy in 1:nny){ # iix=1;iiy=1;iit=1
        cat("iit=",iit,"iix=",iix,",iiy=",iiy,"\n")
        
        tstr=paste0("data/modis/MOD13Q1_NDVI_iit_",iit,"_iix_",iix,"_iiy_",iiy,".xdr")
        if(!file.exists(tstr)){
          tMOD13Q1=mt_subset(product = "MOD13Q1",lat=yycv[iiy],lon=xxcv[iix],
                             band="250m_16_days_NDVI",start=dates[yyv[iit],2],end=dates[yyv[iit],2],
                             km_lr=ddx.sn,km_ab=ddy.sn,site_name="Kansai",
                             out_dir="data/modis")
          save(tMOD13Q1,file=tstr)
        }
        
        tstr=paste0("data/modis/MOD13Q1_EVI_iit_",iit,"_iix_",iix,"_iiy_",iiy,".xdr")
        if(!file.exists(tstr)){
          tMOD13Q1=mt_subset(product = "MOD13Q1",lat=yycv[iiy],lon=xxcv[iix],
                             band="250m_16_days_EVI",start=dates[yyv[iit],2],end=dates[yyv[iit],2],
                             km_lr=ddx.sn,km_ab=ddy.sn,site_name="Kansai",
                             out_dir="data/modis")
          save(tMOD13Q1,file=tstr)
        }
      }
    }
  }
})



# "250m_16_days_NDVI"
# "250m_16_days_EVI"
MOD13Q1_01L=as.list(rep(NA,2))
for(idn in 1:2){ # NDVI / EVI
  MOD13Q1_01L[[idn]]=as.list(rep(NA,length(yyv)))
  for(iit in 1:length(yyv)){ 
    MOD13Q1_01L[[idn]][[iit]]=as.list(rep(NA,nnx))
    for(iix in 1:nnx){
      MOD13Q1_01L[[idn]][[iit]][[iix]]=as.list(rep(NA,nny))
    }
  }
}


# Vegetation Indices (NDVI/EVI)
system.time({ # 8315.66 sec
  for(iit in 1:length(yyv)){
    for (iix in 1:nnx){
      for(iiy in 1:nny){ # iix=7;iiy=1
        cat("iit=",iit,"iix=",iix,",iiy=",iiy,"\n")
        MOD13Q1_01L[[1]][[iit]][[iix]][[iiy]]=mt_subset(product = "MOD13Q1",lat=yycv[iiy],lon=xxcv[iix],
                                                        band="250m_16_days_NDVI",start=dates[yyv[iit],2],end=dates[yyv[iit],2],
                                                        km_lr=ddx.sn,km_ab=ddy.sn,site_name="Kansai",
                                                        out_dir="data/modis")
        MOD13Q1_01L[[2]][[iit]][[iix]][[iiy]]=mt_subset(product = "MOD13Q1",lat=yycv[iiy],lon=xxcv[iix],
                                                        band="250m_16_days_EVI",start=dates[yyv[iit],2],end=dates[yyv[iit],2],
                                                        km_lr=ddx.sn,km_ab=ddy.sn,site_name="Kansai",
                                                        out_dir="data/modis")
      }
    }
  }
})

iit=1; iix=1; iiy=1
MOD13Q1_01L[[1]][[iit]][[iix]][[iiy]]


tstr=paste0("data/MOD11A2_01L_Kansai.xdr")
save(MOD13Q1_01L,file=tstr)
load(tstr) # MOD17A3HGF_01L



# MOD17A3HGF_01L[[iit]][[iix]][[iiy]] %>% head

# convert data to raster: sinusoidal
system.time({ # 207.24 sec
  tmp0.rs0L=as.list(rep(NA,2))
  for (idn in 1:2){
    tmp0.rs0L[[idn]]=as.list(rep(NA,length(yyv)))
  }
  for(iit in 1:length(yyv)){
    tmp1.rs0L.d=tmp1.rs0L.n=as.list(rep(NA,nnx))
    for (iix in 1:nnx){ # iix=1
      tmp2.rs0L.d=tmp2.rs0L.n=as.list(rep(NA,nny))
      for(iiy in 1:nny){ # iix=iiy=1 
        cat("iit=",iit,"iix=",iix,",iiy=",iiy,"\n")
        tmp2.rs0L.d[[iiy]]=mt_to_terra(df=MOD13Q1_01L[[1]][[iit]][[iix]][[iiy]],reproject=F) %>% as("Raster")
        tmp2.rs0L.n[[iiy]]=mt_to_terra(df=MOD13Q1_01L[[2]][[iit]][[iix]][[iiy]],reproject=F) %>% as("Raster")
      }
      tmp1.rs0L.d[[iix]]=do.call(merge,tmp2.rs0L.d)
      tmp1.rs0L.n[[iix]]=do.call(merge,tmp2.rs0L.n)
    }
    tmp0.rs0L[[1]][[iit]]=do.call(merge,tmp1.rs0L.d) # day
    tmp0.rs0L[[2]][[iit]]=do.call(merge,tmp1.rs0L.n) # night
  }
  
})

# reproject to lat-lon
system.time({
  MOD13Q1.wgs84.ras.01L=as.list(rep(NA,2))
  for (idn in 1:2){
    MOD13Q1.wgs84.ras.01L[[idn]]=as.list(rep(NA,length(yyv)))
    for(iit in 1:length(yyv)){
      MOD13Q1.wgs84.ras.01L[[idn]][[iit]]=projectRaster(tmp0.rs0L[[idn]][[iit]],method="ngb",crs="+proj=longlat +datum=WGS84 +no_defs ") %>% 
        crop(tbnd.sf) # %>% resample(tTD2,method="ngb")
    }
  }
})

MOD13Q1.df <- MOD13Q1.wgs84.ras.01L[[idn]][[iit]] %>% #raster data
  as.data.frame(xy = TRUE) %>% na.omit()
names(MOD13Q1.df)[3]="VI"

range()
bp2=c(260,270,280,290,300)
MOD11A2.map <- basemap +
  geom_sf(fill = NA) +
  geom_raster(data = MOD11A2.df, aes(x, y, fill = LST)) +
  theme_bw()+
  theme(text = element_text(size = 24),legend.key.width=unit(1,"cm"),legend.key.height=unit(3,"cm"))+
  scale_fill_viridis_c(breaks=bp2,labels=bp2,option = "viridis", trans = scales::pseudo_log_trans(sigma=5,base=2),
                       name="kelvin",limits=c(260,300),oob = scales::squish)+
  xlab(NULL) + ylab(NULL)


tstr=paste0("data/MOD11A2.wgs84.ras.01L_Kansai.xdr")
save(MOD11A2.wgs84.ras.01L,file=tstr) # annual NPP list for years (2012,2016,2020)

adm.bnd.prf.Kansai.MOD11A2.wgs84.simpl2.ras=fasterize(tm2,MOD11A2.wgs84.ras.01L[[1]][[1]],field="flg",fun="first")
adm.bnd.prf.Kansai.MOD11A2.wgs84.simpl2.ras[adm.bnd.prf.Kansai.MOD11A2.wgs84.simpl2.ras==0]=NA



# dates[yyv,2]
for(yyi in 1:length(yyv)){ # yyi=1
  tras=mask(MOD11A2.wgs84.ras.01L[[1]][[yyi]],adm.bnd.prf.Kansai.MOD11A2.wgs84.simpl2.ras)
  tstr=paste0("data/tif/MOD11A2.wgs84_Kansai_day_",dates[yyv[yyi],2],".xdr")
  writeRaster(tras,filename=tstr, format="GTiff", overwrite=TRUE)
}


#####



