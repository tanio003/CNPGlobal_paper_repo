# script to create nc file "cnp_hist_ssp370_full.nc" for CNP output from CESM2-lens prediction
# Make sure you run the entire drake files and load the variables first

ncpath <- "/Users/tatsurotanioka/Desktop/Project/Tanioka21_CNP_Global/CNPGlobal_paper_repo/data/CNP_ModelOutputs/CESM2_LENS_GAM_CNP/"
ncname <- "cnp_hist_ssp370_full"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "cp"  

# create and write the netCDF file -- ncdf4 version
# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(cesm_lonlat_info$lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(cesm_lonlat_info$lat)) 

# define variables
fillvalue <- 1e32
dlname <- "C:P historic"
cphist_def <- ncvar_def("cphist","molar",list(londim,latdim),fillvalue,dlname,prec="single")
dlname <- "C:P future"
cpfuture_def <- ncvar_def("cpfuture","molar",list(londim,latdim),fillvalue,dlname,prec="single")
dlname <- "N:P historic"
nphist_def <- ncvar_def("nphist","molar",list(londim,latdim),fillvalue,dlname,prec="single")
dlname <- "C:P future"
npfuture_def <- ncvar_def("npfuture","molar",list(londim,latdim),fillvalue,dlname,prec="single")

# create netCDF file and put arrays
if (file.exists(ncfname)) {
  #Delete file if it exists
  file.remove(ncfname)
}
ncout <- nc_create(ncfname,list(cphist_def,cpfuture_def,nphist_def,npfuture_def),force_v4=TRUE)

# put variables
ncvar_put(ncout,cphist_def,CNP_gam_cesm$pred_cp_historic_full)
ncvar_put(ncout,cpfuture_def,CNP_gam_cesm$pred_cp_SSP370_full)
ncvar_put(ncout,nphist_def,CNP_gam_cesm$pred_np_historic_full)
ncvar_put(ncout,npfuture_def,CNP_gam_cesm$pred_np_SSP370_full)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")

# Get a summary of the created file:
ncout

# close the file, writing data to disk
nc_close(ncout)
