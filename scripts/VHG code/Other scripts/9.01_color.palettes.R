#easy way to save color palettes
#just uhh copy and paste into whatever script




#nice colors to sue for something else
"darkseagreen1"
"darkslategrey"





library(RColorBrewer)

#modify BrBG ####
brewer.pal(n=11,name='BrBG')

display.brewer.pal(n=11,name='BrBG')
# "#543005" "#8C510A" "#BF812D" "#DFC27D" "#F6E8C3" "#F5F5F5" "#C7EAE5" "#80CDC1" "#35978F" "#01665E" "#003C30"

#rearrange and add some others
brown.and.blue = c('#F6E8C3',   #1
                   '#DFC27D',   #2
                   '#BF812D',   #3
                   '#8C510A',   #4
                   '#543005',   #5
                   '#000000',   #7
                   '#003C30',   #9
                   '#01665E',   #10
                   '#35978F',   #11
                   '#80CDC1',   #12
                   '#C7EAE5')   #13

#test plot
df = data.frame('points'=c(58,58,58,58,58,58,58,58,58,58,58))
barplot(height=df$points, col=brown.and.blue)


#eventually make it like this
#color brewer source code
BrBG = switch(n-2,            
              rgb(c(216,245,90),
                  c(179,245,180),
                  c(101,245,172),maxColorValue=255),
              rgb(c(166,223,128,1),
                  c(97,194,205,133),
                  c(26,125,193,113),maxColorValue=255),
              rgb(c(166,223,245,128,1),
                  c(97,194,245,205,133),
                  c(26,125,245,193,113),maxColorValue=255),
              rgb(c(140,216,246,199,90,1),
                  c(81,179,232,234,180,102),
                  c(10,101,195,229,172,94),maxColorValue=255),
              rgb(c(140,216,246,245,199,90,1),
                  c(81,179,232,245,234,180,102),
                  c(10,101,195,245,229,172,94),maxColorValue=255),
              rgb(c(140,191,223,246,199,128,53,1),
                  c(81,129,194,232,234,205,151,102),
                  c(10,45,125,195,229,193,143,94),maxColorValue=255),
              rgb(c(140,191,223,246,245,199,128,53,1),
                  c(81,129,194,232,245,234,205,151,102),
                  c(10,45,125,195,245,229,193,143,94),maxColorValue=255),
              rgb(c(84,140,191,223,246,199,128,53,1,0),
                  c(48,81,129,194,232,234,205,151,102,60),
                  c(5,10,45,125,195,229,193,143,94,48),maxColorValue=255),
              rgb(c(84,140,191,223,246,245,199,128,53,1,0),
                  c(48,81,129,194,232,245,234,205,151,102,60),
                  c(5,10,45,125,195,245,229,193,143,94,48),maxColorValue=255))




#pairs ####
g = c('darkseagreen2','darkseagreen4')
b = c('deepskyblue2','deepskyblue4')
y = c('goldenrod1','goldenrod4')
r = c('tomato1','tomato4')
pairs = c(g,b,y,r)
barplot(seq(1:8), col=pairs)
