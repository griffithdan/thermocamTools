readThermocamRaw <- function(file, output = "raster", AtmosT = 20, RH = 45){

  # INDICES FOR CALCULATING POSITIONS IN BINARY FILE. TABLE FROM: http://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/FLIR.html#CameraInfo
    index_table <- data.frame(
      Index = c(32,36,40,44,48,52,60,88,92,96,112,116,120,124,128,144,148,152,156,160,164,168,172,212,244,260,276,368,400,416,436,492,508,540,776,780,824,828,900,912,1116),
      Name = c("Emissivity","ObjectDistance","ReflectedApparentTemperature","AtmosphericTemperature","IRWindowTemperature","IRWindowTransmission","RelativeHumidity","PlanckR1","PlanckB","PlanckF","AtmosphericTransAlpha1","AtmosphericTransAlpha2","AtmosphericTransBeta1","AtmosphericTransBeta2","AtmosphericTransX","CameraTemperatureRangeMax","CameraTemperatureRangeMin","CameraTemperatureMaxClip","CameraTemperatureMinClip","CameraTemperatureMaxWarn","CameraTemperatureMinWarn","CameraTemperatureMaxSaturated","CameraTemperatureMinSaturated","CameraModel","CameraPartNumber","CameraSerialNumber","CameraSoftware","LensModel","LensPartNumber","LensSerialNumber","FieldOfView","FilterModel","FilterPartNumber","FilterSerialNumber","PlanckO","PlanckR2","RawValueMedian","RawValueRange","DateTimeOriginal","FocusStepCount","FocusDistance"),
      Size = c(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,32,16,16,92,32,16,20,56,16,32,236,4,4,4,4,4,4,4),
      Type = c("real","real","real","real","real","real","real","real","real","real","real","real","real","real","real","real","real","real","real","real","real","real","real","char","char","char","char","char","char","char","char","char","char","char","int","real","int","int","int","real","real")
    )

  # OPEN RAW FILE
    img.rb <- file(file, "rb") 
      rawdata <- readBin(img.rb, "raw", n = file.info(file)$size, size = 1, endian = "little")
    close(img.rb)

  # POSITION INFORMATION FOR SECTIONS, BACK CALCULATED BY BRUTE FORCE 
  # (I.E., WHAT OFFSET PRODCUES A STRING CALLED "FLIR a325SC", AND WHAT REGION PRODUCES A SAMPLE IMAGE [I USED FIRST IMAGE])
    cam_info_offset <- 320
    startraw <- 2780
    image_width <- 320
    image_height <- 240
    image_pixel <- image_width * image_height * 2
    
  # LOOP THROUGH THE PARAMETERS AND EXTRACT DATA FROM BINARY FILE FOR EACH
    camera_info <- vector("list", nrow(index_table))
    for(i in 1:nrow(index_table)){
      if(index_table[i,"Type"] == "char"){
          block <- readRaw(file, 
                       width = index_table[i,"Size"], 
                       offset = cam_info_offset + index_table[i,"Index"], 
                       nbytes = index_table[i,"Size"], 
                       machine = "hex", 
                       human = "char", 
                       size = 1, endian = "little",
                       signed = TRUE)
          tmp <- blockString(block)}
      if(index_table[i,"Type"] == "int"){
          block <- readRaw(file, 
                       width = index_table[i,"Size"], 
                       offset = cam_info_offset + index_table[i,"Index"], 
                       nbytes = index_table[i,"Size"], 
                       machine = "hex", 
                       human = "int", 
                       size = index_table[i,"Size"], 
                       endian = "little",
                       signed = TRUE)
          tmp <- blockValue(block)}
      if(index_table[i,"Type"] == "real"){
          block <- readRaw(file, 
                       width = 4, 
                       offset = cam_info_offset + index_table[i,"Index"], 
                       nbytes = index_table[i,"Size"], 
                       machine = "hex", 
                       human = "real", 
                       size = index_table[i,"Size"], 
                       endian = "little",
                       signed = TRUE)
          tmp <- blockValue(block)
        if(grepl("Temperature", index_table[i,"Name"])){
          tmp <- round(tmp - 273.15, 2)
        }
        if(grepl("Humidity", index_table[i,"Name"])){
          tmp <- round(tmp * 100)
        }
      }
          camera_info[[i]] <- tmp
    }
    names(camera_info) <- index_table$Name  

  # GET THE ACTUAL IMAGE OUT
    block <- readRaw(file, 
                     width = 2, 
                     offset = startraw, 
                     nbytes = image_pixel, 
                     machine = "hex", 
                     human = "int", 
                     size = 2, 
                     endian = "little",
                     signed = TRUE)
    rawimg <- matrix(data = blockValue(block), nrow = image_height, ncol = image_width, byrow = T)
    rawimg <- t(rawimg)
    rawimg <- rawimg[,ncol(rawimg):1]  
    
    camera_info$Date <- file.mtime(file) # date, from system # Seems dates are wrong, so use system dates
    
  # CONVERT THE RAW IMAGE FROM THE BINARY REPRESENTAITON INTO A TEMPERATURE.
  # - THIS SECTION IS MORE OR LESS TAKEN FROM THE READ FUNCTION IN THE PACKAGE Thermimage
  # - FROM Thermimage COMES THE raw2temp() FUNCTION WHICH TAKE THE PARAMETERS BELOW AND THEN CORRECTS THE TEMP VALUES
    Emissivity        <- camera_info$Emissivity                    # Image Saved Emissivity - should be ~0.95 or 0.96
    ObjectEmissivity  <- 0.96                                      # Object Emissivity - should be ~0.95 or 0.96
    PlanckR1          <- camera_info$PlanckR1                      # Planck R1 constant for camera  
    PlanckB           <- camera_info$PlanckB                       # Planck B constant for camera  
    PlanckF           <- camera_info$PlanckF                       # Planck F constant for camera
    PlanckO           <- camera_info$PlanckO                       # Planck O constant for camera
    PlanckR2          <- camera_info$PlanckR2                      # Planck R2 constant for camera
    OD                <- camera_info$ObjectDistance                # object distance in metres
    FD                <- camera_info$FocusDistance                 # focus distance in metres
    ReflT             <- camera_info$ReflectedApparentTemperature  # Reflected apparent temperature
    # AtmosT            <- camera_info$AtmosphericTemperature        # Atmospheric temperature
    IRWinT            <- camera_info$IRWindowTemperature           # IR Window Temperature
    IRWinTran         <- camera_info$IRWindowTransmission          # IR Window transparency
    # RH                <- camera_info$RelativeHumidity              # Relative Humidity
    h                 <- camera_info$RawThermalImageHeight         # sensor height (i.e. image height)
    w                 <- camera_info$RawThermalImageWidth          # sensor width (i.e. image width)  
  
  # EXPORT IMAGE AND ATTRIBUTES  
    img <- raw2temp(rawimg,ObjectEmissivity,OD,ReflT,AtmosT,IRWinT,IRWinTran,RH,PlanckR1,PlanckB,PlanckF,PlanckO,PlanckR2)
      camera_info$FieldOfView <- camera_info$DateTimeOriginal <- NULL
      camera_info$ObjectEmissivity <- ObjectEmissivity

    if(output == "raster"){
      img <- t(img)
      img <- raster(img[nrow(img):1,])
      #attr(x = img, which = "info") <- camera_info
      return(img)
    }  
    if(output == "matrix"){
      attr(x = img, which = "info") <- camera_info
      return(img)
    }
      
}  



        
  
  
  
  
  