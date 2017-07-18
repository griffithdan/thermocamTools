stackThermocamRaw <- function(files){

  rst_list <- lapply(X = files, FUN = function(x){readThermocamRaw(file = x, output = "raster")})
  rst_list <- stack(rst_list)

}  



        
  
  
  
  
  