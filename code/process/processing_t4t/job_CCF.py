# -*- coding: latin-1 -*- 

################################################################################
# Purpose: Apply the time for time estimation of cloud effects
# License: GPL v3
# Authors: Ramdane Alkama - Dec. 2020
################################################################################


import cdms2 as cdms, MV2 as MV, os.path
import numpy as N, cdutil, os, sys,random
import  interpol 
import os.path


cdms.setNetcdfShuffleFlag(0) 
cdms.setNetcdfDeflateFlag(0) 
cdms.setNetcdfDeflateLevelFlag(0)

# ----------------------------- #
 # --------------------------- #
  #       Parametres          #
 # --------------------------- #
# ----------------------------- #

nbzone = 5 # 5  16

latrange=(-56,80,'cc') # (-56,80,'cc') (-56,0,'cc') (0,80,'cc')
lonrange=(-180,180,'cc')

# ----------------------------- #
 # --------------------------- #
  #            def            #
 # --------------------------- #
# ----------------------------- #

cloudfile='/home/alkamro/Downloads/ESACCI-L3X_CLOUD-JRC-MODIS_AQUA-fv2.0_%i%02i.nc'

resolution=0.05 # 0.05 deg

if nbzone == 5 : zones=[1,2,3,4]
if nbzone == 16: zones=[1,2,4,5,7,8,9,10,11,12,15]
if latrange[0]<0: 
  lims=str(abs(latrange[0]))+'S'
else:
  lims=str(abs(latrange[0]))+'N'

if latrange[1]<0: 
  limn=str(abs(latrange[1]))+'S'
else:
  limn=str(abs(latrange[1]))+'N'


# ----------------------------- #
 # --------------------------- #
  #      functions            #
 # --------------------------- #
# ----------------------------- #

def readforest(yy):
 f=cdms.open('/ESS_EarthObs/DATA_PRODUCTS/LandCover/ESACCI-LandCover/PFT-tableZ-005dd/ESACCI-LC-L4-LCCS-Map-300m-P1Y-aggregated-0.050000Deg-%i-v2.0.7.nc'%yy)
 v1=f('EFO',lat=latrange,lon=lonrange,squeeze=1)*100
 v2=f('DFO',lat=latrange,lon=lonrange,squeeze=1)*100
 f.close()
 v1[N.isnan(v1)] = 0.
 v2[N.isnan(v2)] = 0.
 v=v1+v2
 return v

def read(filein,varin):
      fin=cdms.open(filein,'r')
      tab=fin(varin,lat=latrange,lon=lonrange,squeeze=1)
      fin.close()
      return tab

def read2(filein,varin,mm_):
      fin=cdms.open(filein,'r')
      tab=fin(varin,cdms.timeslice(mm_-1,mm_),lat=latrange,lon=lonrange,squeeze=1)
      fin.close()
      return tab

def scatter(temp,treef2,treef1,surf,msk):

      tab    =N.zeros((nbzone,101,101))
      tabsurf=N.zeros((nbzone,101,101))
      CloudCov2D=N.zeros((temp.shape[0],temp.shape[1]))
      lati=temp.getLatitude()[:]
      loni=temp.getLongitude()[:]
      latt=N.multiply.outer(lati,loni/loni)
      lonn=N.multiply.outer(lati/lati,loni)
      lonn=cdms.createVariable(lonn,fill_value = 1.e+20,dtype='f',axes =surf.getAxisList())
      latt=cdms.createVariable(latt,fill_value = 1.e+20,dtype='f',axes =surf.getAxisList())

      for jj in zones :

        mms = MV.masked_not_equal(msk,jj)
        mms = mms/mms
        Ytmp= temp  * mms
        Xtmp2= treef2 * mms
        Xtmp1= treef1 * mms
        mms = (Ytmp*Xtmp1*Xtmp2+9999.)/(Ytmp*Xtmp1*Xtmp2+9999.)

        Ytmp= Ytmp*mms
        Xtmp1= Xtmp1*mms
        Xtmp2= Xtmp2*mms
        Stmp= surf* mms
        lon=lonn*mms
        lat=latt*mms

        # --------------------
        #  start fortran loop
        # --------------------

        Ytmp=Ytmp.compressed()
        Xtmp1=Xtmp1.compressed()
        Xtmp2=Xtmp2.compressed()
        Stmp=Stmp.compressed()
        lat=lat.compressed()
        lon=lon.compressed()

        if len(lon)>10:
          for abtest in [Ytmp,Xtmp1,Xtmp2,Stmp,lat,lon]:
             if N.isnan(abtest).any():
               print("NaNs in data")
               sys.exit()
          totnorm,Surf_step,cloud2d=interpol.paris(Ytmp,Xtmp1,Xtmp2,Stmp,lat,lon,temp.shape[0],temp.shape[1],resolution,latrange[0],lonrange[0])
          tab[jj-1,:,:]=MV.where((Surf_step[:,:]+tabsurf[jj-1,:,:])!=0,(tab[jj-1,:,:]*tabsurf[jj-1,:,:]+totnorm[:,:]*Surf_step[:,:])/(Surf_step[:,:]+tabsurf[jj-1,:,:]),0)

          tabsurf[jj-1,:,:]=tabsurf[jj-1,:,:]+Surf_step[:,:]

        else:

          cloud2d =N.zeros((temp.shape[0],temp.shape[1]))

        CloudCov2D=CloudCov2D[:,:]+cloud2d[:,:]

        # ------------------
        #  end fortran loop
        # ------------------

      tab     = MV.masked_equal(tab    ,0)
      tabsurf = MV.masked_equal(tabsurf,0)

      return tab, tabsurf,CloudCov2D


tabax=N.array(range(1,101+1,1),dtype='f')
zonax = cdms.createAxis(tabax,id='to')
zonax.long_name = 'percent of tree'
zonax.units = '-'

zonax2 = cdms.createAxis(tabax,id='from')
zonax2.long_name = 'percent of tree'
zonax2.units = '-'

tabax3=N.array(range(1,nbzone+1,1),dtype='f')
zonax3 = cdms.createAxis(tabax3,id='climzonaxe')
zonax3.long_name = 'climate zone axe'
zonax3.units = '-'

zclim=read('/ESS_EarthObs/CLIMATE_DATA/koppen-Geiger/koppen-Geiger_7200x3600_%s_masked.nc' %(str(nbzone)),'climzone')
zclim=MV.masked_equal(zclim,0)

surface = read('/ESS_EarthObs/CLIMATE_DATA/CONST/3Min/areacella.nc','areacella')

  # ----------------------------- #
   # --------------------------- #
    #          Calculs          #
   # --------------------------- #
  # ----------------------------- #

for yrdeb in range(2004,2014,1):#(2013,2002,-1):  
 for yf in range(yrdeb+1,2015,1):  
  print yrdeb,yf
  ftree_1=readforest(yrdeb)
  ftree_2=readforest(yf)
  msk=MV.masked_less_equal(ftree_2+ftree_1,0)
  msk=msk/msk
  ftree_1=ftree_1*msk
  ftree_2=ftree_2*msk
  for month in [1,2,3,4,5,6,7,8,9,10,11,12]:
    print month

    if month < 1 or month > 12 :
      print 'month %i must be between 1 and 12' %(month)
      sys.exit()

    else:

      rep='/ESS_Dacloudets/USERS/Alkama/CLOUDS/Classic/%i-%i/'%(yrdeb,yf)
      os.system("mkdir -p %s"%(rep))
      
      for var in ['cfc_all']:
        if not (os.path.exists(rep+'%s_scater_%i%i%02i_%s_%s.nc' %(var,yrdeb,yf,month,lims,limn))):
          fw=cdms.open(rep+'%s_scater_%i%i%02i_%s_%s.nc' %(var,yrdeb,yf,month,lims,limn),'w')

          Cloud_2  = MV.masked_less(read(cloudfile %(yf,month),var),0)
          Cloud_1  = MV.masked_less(read(cloudfile %(yrdeb,month),var),0)

          dCloudCov = cdms.createVariable(Cloud_2-Cloud_1,fill_value = 1.e+20,dtype='f',axes =Cloud_2.getAxisList())

          del Cloud_2,Cloud_1

          tot,totsurf,CloudCov2D=scatter(dCloudCov,ftree_1,ftree_2,surface,zclim)

          tot=MV.masked_greater(tot,1.e18)
          tot=MV.masked_less(tot,-1.e18)
          totsurf=MV.masked_greater(totsurf,1.e18)
          totsurf=MV.masked_less(totsurf,-1.e18)

          tot=cdms.createVariable(tot,fill_value = 1.e+20,dtype='f',axes =[zonax3,zonax,zonax2])
          totsurf=cdms.createVariable(totsurf,fill_value = 1.e+20,dtype='f',axes =[zonax3,zonax,zonax2])

          fw.write(tot,    id='dlta_cloud')
          fw.write(totsurf,id='area')
          CloudCov2D=N.ma.masked_invalid(CloudCov2D)
          CloudCov2D[CloudCov2D.mask]=0
          CloudCov2D=cdms.createVariable(CloudCov2D,fill_value = 1.e+20,dtype='f',axes =ftree_1.getAxisList())
          fw.write(CloudCov2D,id='Dcloud')

          fw.close()

