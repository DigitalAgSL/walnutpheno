library(magick)
library(tidyverse)

#Set working directory (Update this path to your folder with images and coordinates file!)
setwd("~/your/directory/path")

#Load in coordinates file
coordinates <- read.csv("File S3.csv")
x_dim=coordinates$X.coordinate
y_dim=coordinates$Y.coordinate
row_names = rep(c("01","02","03","04","05","06","07","08","09","10"), times = 10)
col_names = rep(c("01","02","03","04","05","06","07","08","09","10"), each = 10)
rotation <- c()
left_edge <- c()
top_edge <- c()

#List image files
file_list = list.files(pattern = ".jpg")

#Iteratively process through all images in the working directory and creates a CSV file for each image
for (k in 1:length(file_list)){
  image1=image_read(file_list[k])
  image1_tiff=image_convert(image1, "tiff")
  print(k)
  
  #Determine rotation by looking at the top 20% and bottom 20% of lens corrected image
  image1_top <- image_crop(image1_tiff, "3300x624 + 0 + 0")
  rt=apply(image1_top[[1]][1,,],2,as.numeric)
  bt=apply(image1_top[[1]][3,,],2,as.numeric)
  mrrt=apply(rt,1,mean)#mean red by row
  mrbt=apply(bt,1,mean)#mean blue by row
  left_edget=which.max((mrbt/mrrt)[1:750])
  image1_bottom <- image_crop(image1_tiff, "3300x624 + 0 + 2496")
  rb=apply(image1_bottom[[1]][1,,],2,as.numeric)
  bb=apply(image1_bottom[[1]][3,,],2,as.numeric)
  mrrb=apply(rb,1,mean)#mean red by row
  mrbb=apply(bb,1,mean)#mean blue by row
  left_edgeb=which.max((mrbb/mrrb)[1:750])
  rotation[k] <- left_edget-left_edgeb
  image1_tiff=image_rotate(image1_tiff,360 - (rotation[k]*0.01851852))
  
  #Determine tray edges for cropping
  r=apply(image1_tiff[[1]][1,,],2,as.numeric)
  g=apply(image1_tiff[[1]][2,,],2,as.numeric)
  b=apply(image1_tiff[[1]][3,,],2,as.numeric)
  mcr=apply(r,2,mean)#mean red by column
  mcb=apply(b,2,mean)#mean blue by column
  top_edge[k]=which.max((mcb/mcr)[1:450])
  mrr=apply(r,1,mean)#mean red by row
  mrb=apply(b,1,mean)#mean blue by row
  left_edge[k]=which.max((mrb/mrr)[1:750])
  image1_tiff=image_crop(image1_tiff,geometry=paste("2780x2780+",left_edge[k],"+",top_edge[k],sep=""))
  results = matrix(NA,100,37)
  colnames(results) = c("size", "mean_r", "mean_g", "mean_b", "mean_l", "mean_a", "mean_b","10_r","25_r","50_r","75_r","90_r","10_g","25_g","50_g","75_g","90_g","10_b","25_b","50_b","75_b","90_b","10_l","25_l","50_l","75_l","90_l","10_a","25_a","50_a","75_a","90_a","10_b.1","25_b.1","50_b.1","75_b.1","90_b.1")
  for (i in 1:100){
    swatch1 <- image_crop(image1_tiff, geometry = paste("270x270+",x_dim[i],"+",y_dim[i],sep=""))
    r_values=apply(swatch1[[1]][1,,],2,as.numeric)
    g_values=apply(swatch1[[1]][2,,],2,as.numeric)
    b_values=apply(swatch1[[1]][3,,],2,as.numeric)
    mask=ifelse(r_values-b_values>(-40),1,0)
    if (sum(mask)<500) next
    r_values[mask==0] =NA
    g_values[mask==0] =NA
    b_values[mask==0] =NA
    out1=matrix(data=c(r_values,g_values,b_values),ncol=3,byrow=FALSE) 
    out1=out1[!is.na(out1[,1]),]
    rgb_means1=apply(out1,2,mean)
    test=convertColor(out1,from="sRGB",to="Lab")
    lab_means1=apply(test,2,mean)
    tilesr = quantile(out1[,1], probs = c(0.1,0.25,0.5,0.75,0.9))
    tilesg = quantile(out1[,2], probs = c(0.1,0.25,0.5,0.75,0.9))
    tilesb = quantile(out1[,3], probs = c(0.1,0.25,0.5,0.75,0.9))
    tilesl = quantile(test[,1], probs = c(0.1,0.25,0.5,0.75,0.9))
    tilesa = quantile(test[,2], probs = c(0.1,0.25,0.5,0.75,0.9))
    tilesb.1 = quantile(test[,3], probs = c(0.1,0.25,0.5,0.75,0.9))
    results[i,2:4] = rgb_means1
    results[i,5:7] = lab_means1
    results[i,8:12] = tilesr
    results[i,13:17] = tilesg
    results[i,18:22] = tilesb
    results[i,23:27] = tilesl
    results[i,28:32] = tilesa
    results[i,33:37] = tilesb.1
    results[i,1] = sum(mask)
  }
  
  image_name = gsub(".jpg", "", file_list[k])
  kernel_name = paste(image_name, col_names, row_names, sep = "_")
  results = data.frame(kernel_name, results)
  write.csv(results, file = paste(image_name, "results.csv", sep = "_"), quote = F, row.names = F )
}

#append csv files 
filenames <- list.files(pattern="*.csv")
filenames <- filenames[filenames != "File S3.csv"]

## Merge listed files from the path above
all_results <- do.call("rbind",lapply(filenames,FUN=function(files){ read.csv(files)}))
write.csv(all_results, "combined_results.csv", quote = F, row.names = F )

#Delete individual files if needed
file.remove(filenames)
