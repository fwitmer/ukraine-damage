##########################################################################
# 17 Mar 2025
#
##########################################################################

library(magick) # for image_animate function

###################################
# read all png files in inDir and combine them into a gif with an appropriate prefix
#   ASSUMES folder name to be the same as the file prefixes
###################################
make_gif <- function(analysisDir, folder, prefix=NULL) {
  #inDir <- outDir
  inDir <- file.path(analysisDir, folder)
  if (!dir.exists(inDir))
    print(paste("ERROR, directory not found:", inDir))
  
  # List the PNG files in the output directory
  png_files <- list.files(inDir, pattern = "*.png", full.names = TRUE)
  last_file <- png_files[length(png_files)]
  # add pause by repeating last file
  png_files <- c(png_files, rep(last_file, 10)) 
  
  # Read the images and create the GIF
  gif <- image_read(png_files) %>%
    image_animate(fps = 2)  # Adjust frames per second as needed
  
  if (is.null(prefix)) {
    outGifFile <- file.path(inDir, paste0(folder, ".gif")) # use folder as the file prefix
  } else {
    outGifFile <- file.path(inDir, paste0(prefix, ".gif"))
  }
  print(paste("Writing file to:", outGifFile))
  image_write(gif, outGifFile)
}
