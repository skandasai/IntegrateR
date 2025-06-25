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
                              fileInput("image_upload_b", "Upload Monolayer TIF Images", accept = ".tif", multiple = TRUE),
                              fileInput("image_upload_g", "Upload Integration cell TIF Images", accept = ".tif", multiple = TRUE),
                              numericInput("cellCount", "Write how many cells were plated", value = 10000)
                            ),
                            mainPanel(
                              withSpinner(plotOutput("image_plot")),
                              tableOutput("integrationcheck")
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
  
  output$integrationcheck <- renderTable({
    req(metadata())

    monolayer <- metadata()[[1]]

    gfpdata <- metadata()[[2]]

    peak <- which.max(monolayer[,2])

    minimum <- peak-2

    maximum <- peak+2

    highcellid <- max(gfpdata$cellid)
    
    integratedcells <- NULL
    
    for (i in 1:highcellid){
      
      tempdataframe <- gfpdata %>% 
        filter(cellid == i)
      
      gfpzpeak <- which.max(tempdataframe$intensity)
      
      if (gfpzpeak >= minimum & gfpzpeak <= maximum){
        if (is.null(integratedcells) == TRUE){
          integratedcells <- c("True")
        } else {
          integratedcells <- append(integratedcells, "True")
        } 
      } else{
        if (is.null(integratedcells) == TRUE){
          integratedcells <- c("False")
        } else {
          integratedcells <- append(integratedcells, "False")
        } 
      }
      
    }
    
    integrated_table <- data.frame(cellid = c(1:highcellid), integrated = integratedcells)
    
    return(integrated_table)
    
  })
  
}

shinyApp(ui = ui, server = server)



