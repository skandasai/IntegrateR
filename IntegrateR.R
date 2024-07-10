# library(EBImage)
# library(RColorBrewer)
# library(dplyr)
# library(shiny)
# library(shinythemes)
# library(ggplot2)
# library(plyr)
# library(doParallel)
# library(DT)
# library(ggpubr)
# library(shinycssloaders)
# 
# 
# 
# IntegrateR <- function(blue_image, green_image) {
#   print(blue_image)
#   # Load your DAPI and GFP images here
#   #Dapi images
#   # images_dapi = list.files(path = blue_image, pattern = '.tif')
#   # imgpath_dapi = c()
#   # for (i in 1:length(images_dapi)){
#   #   imgpath_dapi <- append(imgpath_dapi,paste(blue_image, images_dapi[i], sep = ""))
#   # }
#   dapi_image <- readImage(files = blue_image)
#   #display(dapi_image, all = TRUE)
#   
#   #GFP images
#   # images_gfp = list.files(path = green_image, pattern = '.tif')
#   # imgpath_gfp = c()
#   # for (i in 1:length(images_gfp)){
#   #   imgpath_gfp <- append(imgpath_gfp,paste(green_image, images_gfp[i], sep = ""))
#   # }
#   #
#   gfp_image <- readImage(files = green_image)
#   display(gfp_image)
#   
#   # Step 1: Max Projection DAPI
#   display(dapi_image)
#   Maxproj_dapi <- apply(dapi_image, 1:2, max)
#   display(Maxproj_dapi)
#   
#   
#   # Step 2: Max Projection GFP
#   Maxproj_gfp <- apply(gfp_image, 1:2, max)
#   display(Maxproj_gfp)
#   
#   # Step 3: Identify primary objects from GFP Projection
#   z <- bwlabel(Maxproj_gfp)
#   z = thresh(Maxproj_gfp, 10, 10, 0.05)
#   z = opening(z, makeBrush(5, shape='disc'))
#   # z = channel(z, 'asgreen')
#   display(z, title='GFP cell')
#   
#   # Step 4: Identify primary objects from DAPI projection
#   y <- bwlabel(Maxproj_dapi)
#   y = thresh(Maxproj_dapi, 10, 10, 0.05)
#   y = fillHull(y)
#   y = opening(y, makeBrush(5, shape='disc'))
#   # y = channel(y, 'asblue')
#   display(y, title='Cell nuclei binary mask')
#   
#   # Step 5: Make and use GFP mask to separate nuclei
#   jointimage <- rgbImage(green = z, blue = y)
#   display(jointimage)
#   
#   ctmask <- opening(Maxproj_gfp>0.1, makeBrush(5, shape='disc'))
#   
#   cmask = propagate(Maxproj_gfp, seeds=y, mask=ctmask)
#   
#   #display(ctmask, all=TRUE)
#   
#   segmented = paintObjects(cmask, jointimage, col='#ff00ff')
#   segmented = paintObjects(y, segmented, col='#ffff00')
#   segmented = paintObjects(ctmask, segmented, col = c('yellow'))
#   
#   table(segmented)
#   display(segmented)
#   
#   
#   GFP_Cells <- which(ctmask == TRUE, arr.ind = TRUE)
#   length(GFP_Cells)/2
#   GFP_Cells[384,1]
#   
#   Gfpnuclei <- dapi_image@.Data
#   NonGFPnuclei <- dapi_image@.Data
#   
#   
#   znumber <- 1
#   i = TRUE
#   #producing 2 images
#   while (znumber <= 21) {
#     #Non GFP nuclei
#     for (i in (length(GFP_Cells)/2)){
#       NonGFPnuclei[GFP_Cells[i,1],GFP_Cells[i,2],3,znumber] <- 0
#     }
#     
#     #GFP nuclei
#     for (rows in 1:nrow(Gfpnuclei[,,3,znumber])) {
#       for (columns in 1:ncol(Gfpnuclei[,,3,znumber])) {
#         if (ctmask[rows, columns] == FALSE){
#           Gfpnuclei[rows,columns,3,znumber] <- 0
#         }
#       }
#     }
#     znumber = znumber + 1
#     print(znumber)
#   }
#   
#   # # Step 6: Get Median Intensity of each GFP nuclei over the z-stack
#   display(Gfpnuclei)
#   #make bwlabel
#   Gfpnucleitest <- bwlabel(Gfpnuclei)
#   gfpnucleitable <- table(Gfpnucleitest)
#   
#   #Get rid of objects that have less than 100 pixels
#   Gfpnucleitest[Gfpnucleitest %in% as.numeric(names(gfpnucleitable)[gfpnucleitable < 100])] <- 0
#   GFPcellstest <- as.numeric(names(table(Gfpnucleitest)))[-1]
#   length(GFPcellstest)
#   display(Gfpnucleitest)
#   #produce dataframe with gfp cells and median intensity
#   gfpdata <- NULL
#   for (i in 1:21){
#     df <- as.data.frame(computeFeatures.moment(Gfpnucleitest[,,3,i]))
#     df$z <- i
#     fffffff <- Gfpnuclei[,,3,i]
#     df$intensity <- sapply(GFPcellstest, function(x) mean(fffffff[Gfpnucleitest[,,3,i] == x]))
#     for (h in 1:4){
#       df$cellid[h] <- h
#     }
#     if (is.null(gfpdata)){
#       gfpdata <- df
#     } else {
#       gfpdata <- rbind(gfpdata, df)
#     }
#   }
#   gfpdata
#   
#   # # Step 7: Calculate median of Non-GFP nuclei median intensity over each layer of z-stack
#   NonGfpnucleitest <- bwlabel(NonGFPnuclei)
#   NonGfpnucleitest = thresh(NonGFPnuclei, 10, 10, 0.05)
#   NonGfpnucleitest = fillHull(NonGfpnucleitest)
#   NonGfpnucleitest = opening(NonGfpnucleitest, makeBrush(5, shape='disc'))
#   display(NonGfpnucleitest, title='Cell nuclei binary mask')
#   nongfpintensitystack <- NULL
#   for (i in 1:21){
#     if (is.null(nongfpintensitystack)){
#       nongfpintensitystack <- data.frame(z = i, i = mean(NonGFPnuclei[,,3,i]))
#     } else {
#       u <- data.frame(z = i, i = mean(NonGFPnuclei[,,3,i]))
#       nongfpintensitystack <- rbind(nongfpintensitystack,u)
#     }
#   }
#   nongfpintensitystack
#   # # Step 8: Plot Median of Non-GFP nuclei, find peak
#   intensityplotnongfp <- ggplot(data = nongfpintensitystack, aes(x = z, y = i))+
#     geom_line()+
#     geom_point()+
#     ggtitle("Monolayer")
#   
#   # # Step 9: Plot median intensity of each GFP nuclei, find peak
#   
#   intensityplotgfp <- ggplot(data = gfpdata, aes(x = z, y = intensity, group = cellid))+
#     geom_line(aes(color = cellid))+
#     geom_point()+
#     ggtitle("Individual GFP cells")
#     scale_color_continuous()
# 
#   # # Step 10: Identify integrated GFP peaks
#   intensity_peak <- c()
#   for (i in 1:max(gfpdata$cellid)){
#     test <- gfpdata %>%
#       filter(cellid == i)
#     intensity_peak <- c(intensity_peak, which(test$intensity == max(test$intensity)))
#   }
#   which(nongfpintensitystack$i == max(nongfpintensitystack$i))
#   # # Output results or further analysis as needed
#   #
#   
#   combined <- list(nongfpintensitystack, gfpdata)
#   
#   return(combined)
# }
# 
# ui <- navbarPage("RPE CELL INTEGRATION PROGRAM",
#                  theme = shinytheme("sandstone"),
#                  tabPanel("Integration Plots",
#                           sidebarLayout(
#                             sidebarPanel(
#                               fileInput("image_upload_b", "Upload Blue TIF Images", accept = ".tif", multiple = TRUE),
#                               fileInput("image_upload_g", "Upload Green TIF Images", accept = ".tif", multiple = TRUE)
#                             ),
#                             mainPanel(
#                               withSpinner(
#                                 plotOutput("image_plot")),
#                               tableOutput("monolayer"),
#                               tableOutput("gfpcells")
#                             )
#                           )
#                  )
# )
# 
# server <- function(input, output, session) {
#   
#   metadata <- reactive({
#     req(input$image_upload_b$datapath)
#     req(input$image_upload_g$datapath)
#     IntegrateR(input$image_upload_b$datapath, input$image_upload_g$datapath)
#   })
#   
#   output$monolayer <- renderTable({
#     
#     require(metadata())
#     
#     return(metadata()[[1]])
#     
#   })
#   
#   output$gfpcells <- renderTable({
#     
#     require(metadata())
#     
#     return(metadata()[[2]])
#     
#   })
#   
#   output$image_plot <- renderPlot({
#     req(metadata())
#     
#     nongfpintensitystack <- metadata()[[1]]
#     
#     gfpdata <- metadata()[[2]]
#     
#     intensityplotnongfp <- ggplot(data = nongfpintensitystack, aes(x = z, y = i))+
#       geom_line()+
#       geom_point()+
#       ggtitle("Monolayer")
#     
# 
#     intensityplotgfp <- ggplot(data = gfpdata, aes(x = z, y = intensity, group = cellid))+
#       geom_line(aes(color = cellid))+
#       geom_point()+
#       ggtitle("Individual GFP cells")
#     scale_color_continuous()
#     
#     figure <- ggarrange(intensityplotgfp, intensityplotnongfp)
#     figure
#     return(figure)
#     
#   })
# }
# 
# shinyApp(ui = ui, server = server)




library(EBImage)
library(RColorBrewer)
library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(plyr)
library(doParallel)
library(DT)
library(ggpubr)
library(shinycssloaders)

IntegrateR <- function(blue_image, green_image) {
  # Load DAPI and GFP images
  dapi_image <- readImage(files = blue_image)
  gfp_image <- readImage(files = green_image)
  
  # Max Projection DAPI
  Maxproj_dapi <- apply(dapi_image, 1:2, max)
  
  # Max Projection GFP
  Maxproj_gfp <- apply(gfp_image, 1:2, max)
  
  # Identify primary objects from GFP Projection
  z <- bwlabel(Maxproj_gfp)
  z = thresh(Maxproj_gfp, 10, 10, 0.05)
  z = opening(z, makeBrush(5, shape = 'disc'))
  
  # Identify primary objects from DAPI projection
  y <- bwlabel(Maxproj_dapi)
  y = thresh(Maxproj_dapi, 10, 10, 0.05)
  y = fillHull(y)
  y = opening(y, makeBrush(5, shape = 'disc'))
  
  # Make and use GFP mask to separate nuclei
  jointimage <- rgbImage(green = z, blue = y)
  ctmask <- opening(Maxproj_gfp > 0.1, makeBrush(5, shape = 'disc'))
  cmask <- propagate(Maxproj_gfp, seeds = y, mask = ctmask)
  
  segmented <- paintObjects(cmask, jointimage, col = '#ff00ff')
  segmented <- paintObjects(y, segmented, col = '#ffff00')
  segmented <- paintObjects(ctmask, segmented, col = 'yellow')
  
  GFP_Cells <- which(ctmask == TRUE, arr.ind = TRUE)
  Gfpnuclei <- dapi_image@.Data
  NonGFPnuclei <- dapi_image@.Data
  
  for (znumber in 1:21) {
    for (i in 1:(length(GFP_Cells) / 2)) {
      NonGFPnuclei[GFP_Cells[i, 1], GFP_Cells[i, 2], 3, znumber] <- 0
    }
    for (rows in 1:nrow(Gfpnuclei[,,3,znumber])) {
      for (columns in 1:ncol(Gfpnuclei[,,3,znumber])) {
        if (ctmask[rows, columns] == FALSE) {
          Gfpnuclei[rows, columns, 3, znumber] <- 0
        }
      }
    }
  }
  
  Gfpnucleitest <- bwlabel(Gfpnuclei)
  gfpnucleitable <- table(Gfpnucleitest)
  Gfpnucleitest[Gfpnucleitest %in% as.numeric(names(gfpnucleitable)[gfpnucleitable < 100])] <- 0
  GFPcellstest <- as.numeric(names(table(Gfpnucleitest)))[-1]
  
  gfpdata <- NULL
  for (i in 1:21) {
    df <- as.data.frame(computeFeatures.moment(Gfpnucleitest[,,3,i]))
    df$z <- i
    fffffff <- Gfpnuclei[,,3,i]
    df$intensity <- sapply(GFPcellstest, function(x) mean(fffffff[Gfpnucleitest[,,3,i] == x]))
    df$cellid <- 1:nrow(df)
    gfpdata <- rbind(gfpdata, df)
  }
  
  NonGfpnucleitest <- bwlabel(NonGFPnuclei)
  NonGfpnucleitest <- thresh(NonGFPnuclei, 10, 10, 0.05)
  NonGfpnucleitest <- fillHull(NonGfpnucleitest)
  NonGfpnucleitest <- opening(NonGfpnucleitest, makeBrush(5, shape = 'disc'))
  
  nongfpintensitystack <- data.frame(z = 1:21, i = sapply(1:21, function(i) mean(NonGFPnuclei[,,3,i])))
  
  intensityplotnongfp <- ggplot(data = nongfpintensitystack, aes(x = z, y = i)) +
    geom_line() +
    geom_point() +
    ggtitle("Monolayer")
  
  intensityplotgfp <- ggplot(data = gfpdata, aes(x = z, y = intensity, group = cellid)) +
    geom_line(aes(color = as.factor(cellid))) +
    geom_point() +
    ggtitle("Individual GFP cells") +
    scale_color_continuous()
  
  intensity_peak <- sapply(unique(gfpdata$cellid), function(i) {
    test <- gfpdata %>% filter(cellid == i)
    which(test$intensity == max(test$intensity))
  })
  
  combined <- list(nongfpintensitystack, gfpdata)
  return(combined)
}

ui <- navbarPage("RPE CELL INTEGRATION PROGRAM",
                 theme = shinytheme("sandstone"),
                 tabPanel("Integration Plots",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("image_upload_b", "Upload Blue TIF Images", accept = ".tif", multiple = TRUE),
                              fileInput("image_upload_g", "Upload Green TIF Images", accept = ".tif", multiple = TRUE),
                              numericInput("cellCount", "Write how many cells were plated")
                            ),
                            mainPanel(
                              withSpinner(plotOutput("image_plot"))
                            )
                          )
                 ),
                 tabPanel("Monolayer data",
                          mainPanel(
                            tableOutput("monolayer")
                          )
                 ),
                 tabPanel("GFP cells",
                          mainPanel(
                            tableOutput("gfpcells")
                          )
                 )
                 
)

server <- function(input, output, session) {
  metadata <- reactive({
    req(input$image_upload_b$datapath)
    req(input$image_upload_g$datapath)
    IntegrateR(input$image_upload_b$datapath, input$image_upload_g$datapath)
  })
  
  output$monolayer <- renderTable({
    req(metadata())
    metadata()[[1]]
  })
  
  output$gfpcells <- renderTable({
    req(metadata())
    metadata()[[2]]
  })
  
  output$image_plot <- renderPlot({
    req(metadata())
    nongfpintensitystack <- metadata()[[1]]
    gfpdata <- metadata()[[2]]
    
    intensityplotnongfp <- ggplot(data = nongfpintensitystack, aes(x = z, y = i)) +
      geom_line() +
      geom_point() +
      ggtitle("Monolayer")
    
    intensityplotgfp <- ggplot(data = gfpdata, aes(x = z, y = intensity, group = cellid)) +
      geom_line(aes(color = as.factor(cellid))) +
      geom_point() +
      ggtitle("Individual GFP cells")

    ggarrange(intensityplotgfp, intensityplotnongfp)
  })
}

shinyApp(ui = ui, server = server)
