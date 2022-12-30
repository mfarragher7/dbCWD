### GND_curvefitting_THL
### THLeach, leachth@miamioh.edu
### July 6, 2016

### adapted by Mary Lofton, melofton@vt.edu
### July 27, 2019

## fitting a generalized normal distribution to a chl profile 
## and generate ChlF estimates from curve to be used for plotting

#load necessary packages
#pacman::p_load automatically installs and loads all required packages
#install.packages('pacman')
pacman::p_load(plyr, ggplot2, dplyr)

#source dependent scripts
source('scripts/LoftonDCMscrpits/Script_S2_GND_fit_functions.R')

# read in example data
    data = read.csv("scripts/LoftonDCMscrpits/Example_fluorescence_profile_Script_S1.csv")
      
################################################
# this ddply can be used if you have a file with multiple lakes (or dates if you change the "lake" 
    # in line 16 to be the name of the column that identifies the each unique profile in your df.

    output.df = ddply(data, "lake", function (data){
                    DCM.fit    = fit.GND(data$depth, data$chl)
                    DCM.r2.fit = r2.fit(DCM.fit$par, data$depth, data$chl) 
                    DCM.depth  = DCM.fit$par[3]
                    DCM.std    = DCM.fit$par[4]
                    Max.depth = max(data$depth)
                    add.par    = DCM.fit$par[1]
                    multi.par  = DCM.fit$par[2]
                    DCM.shape = DCM.fit$par[5]
                    chl.max.value = max(data$chl)
                    DCM.depth.manual = data[which.max(data$chl),"depth"]
                    # calculate DCM top depth, will set to 0 if this is above surface of lake
                        DCM.breadth.top = c()
                        if (DCM.depth - DCM.std < 0) {
                                DCM.breadth.top = 0
                        } else if (DCM.depth - DCM.std >= 0) {
                            DCM.breadth.top = DCM.depth - DCM.std
                        }
                    # calculate the DCM bottom depth, will set to max. depth of profile if below
                        DCM.breadth.bottom = c() # 
                        if (DCM.depth + DCM.std > max(data$depth)) {
                            DCM.breadth.bottom = max(data$depth)
                        } else {
                            DCM.breadth.bottom = DCM.depth + DCM.std
                        }
                    # calculate peak width
                        peak.width = DCM.breadth.bottom - DCM.breadth.top
                    # calculate standardized peak width
                        stand.peak.width = peak.width/Max.depth
                    # calculate avg. chl-a
                        chl.avg = mean(data$chl, na.rm = TRUE)
    data.frame(DCM.r2.fit = round(DCM.r2.fit, digits = 3), 
            DCM.depth.curvefit  = round(DCM.depth, digits = 3), 
            DCM.std    = round(DCM.std, digits = 3),
            DCM.shape  = round(DCM.shape, digits =3),
            add.par    = add.par, 
            multi.par  = multi.par,
            DCM.breadth.top = round(DCM.breadth.top, digits = 3),
            DCM.breadth.bottom = round(DCM.breadth.bottom, digits = 3),
            peak.width = round(peak.width, digits = 3),
            stand.peak.width = round(stand.peak.width, digits = 3),
            Max.depth = Max.depth,
            chl.max.value = chl.max.value,
            chl.avg = round(chl.avg, digits = 2),
            DCM.depth.manual = DCM.depth.manual
            )
})

write.csv(output.df, "./peak_width_results.csv",row.names = FALSE)
    
#####################################################################
# Generate estimates of chl at each depth based upon the fitted curve and create a plot


  ## Add a column, fill with NA values
  data$chl_a_curve = NA

        for(i in 1:nrow(output.df)){
  
                depths = data[data$lake == output.df[i,]$lake, ]$depth
  
                estim.values = output.df[i,]$add.par + output.df[i,]$multi.par * 
                dpgnorm(depths, mean=output.df[i,]$DCM.depth.curvefit, sigma=output.df[i,]$DCM.std, p = output.df[i,]$DCM.shape)
    
                data[data$lake == output.df[i,]$lake, ]$chl_a_curve = estim.values  

                # write title
                titlename = as.character(output.df[i,]$lake)
                # create plot
                pp0 = ggplot() +
                    geom_line(data = data[data$lake == output.df[i,]$lake, ], aes(x = depth, y = chl), size = 0.75, color = "springgreen3")+
                    geom_line(data = data[data$lake == output.df[i,]$lake, ], aes(x = depth, y = chl_a_curve), size = 0.5)+
                    geom_point(data = output.df[i,], aes(x = DCM.depth.manual, y =chl.max.value), size = 2, color = "blue")+
                    geom_segment(data = output.df[i,], aes(x = DCM.breadth.top, y = chl.max.value/3, xend = DCM.breadth.bottom, yend = chl.max.value/3, colour = "red"))+
                    geom_point(data = output.df[i,], aes(x = DCM.breadth.top, y =chl.max.value/3), size = 2, color = "red")+
                    geom_point(data = output.df[i,], aes(x = DCM.breadth.bottom, y =chl.max.value/3), size = 2, color = "red")+
                    scale_y_continuous()+
                    scale_x_reverse()+
                    ggtitle(titlename)+
                    guides(color=FALSE)+
                    labs(x = "Depth (m)", y = "ChlF")+
                    coord_flip()+ 
                    theme_bw() 
                
                # filename - this will write to the plot example folder 
                    filename = paste0("./",output.df[i,]$lake, "_peak_width.png")
                # write plot to file
                    ppi = 300
                    png(file = filename, width = 3*ppi, height = 3*ppi, res = ppi)
                    print(pp0)
                    dev.off()
        }


