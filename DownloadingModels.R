library(RCurl)
library(httr)

dest = "sbml_download"
dest_tar = paste(dest, "tar", sep=".")
dest_tar_bz2 = paste(dest_tar, "bz2", sep=".")

base_url = "ftp://ftp.ebi.ac.uk/pub/databases/biomodels/weekly_archives/2012/"
arc = "BioModels-Database-weekly-2012-07-16-sbmls"
arc_tar_bz2 = paste(arc, ".tar.bz2", sep="")
download.file(paste(base_url, arc_tar_bz2, sep=""), dest_tar_bz2)

system(paste("bunzip2 ", dest_tar_bz2))
system(paste("tar xvf ", dest_tar))

file.rename(arc, "sbml_models")
