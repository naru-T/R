
# 1. DL data via Dropbox link
#see：https://stackoverflow.com/questions/31955995/how-to-download-file-any-form-from-dropbox-using-r

# Create Dropbox link:　https://www.dropbox.com/XXXXX/YYYY.zzz?dl=0
# Change last part from "dl=0" to "raw=1":　https://www.dropbox.com/XXXXX/YYYY.zzz?raw=1
# load it
temp <- tempfile()
download.file("https://www.dropbox.com/XXXXX/YYYY.zzz?raw=1",temp)
load(temp) # or rst <- raster(temp) etc...

# 2. DL data via Google drive link
#see: https://qiita.com/rot-z/items/299ac40361690c51ce1d
# Create Google drive link:　https://drive.google.com/open?id=<FILE ID> or https://drive.google.com/file/d/<FILE ID>/view?usp=sharing
# Change as like:　https://drive.google.com/uc?id=<FILE ID>
# load it
temp <- tempfile()
download.file("https://drive.google.com/uc?id=<FILE ID>",temp)
load(temp) # or rst <- raster(temp) etc...

