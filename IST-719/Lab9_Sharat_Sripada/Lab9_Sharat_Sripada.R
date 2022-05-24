# 3D visualization using RGL
#
library(igraph)
library(rgl)
my.dir <- '/users/venkatasharatsripada/Downloads/'
load(paste0(my.dir, 'ist719NetworkObject.rda'))

coords <- layout_with_kk(g, dim=3)
# Creates a 3D object
rglplot(g, layout=coords)

l <- layout_with_kk(g) # Makes a layout with 2-dim by default
# Set the x,y,z axes
V(g)$label <- " "
V(g)$x <- l[,1]
V(g)$y <- l[,2]
V(g)$z <- V(g)$bet     # Show the betweeness on the z-axis 
rglplot(g)

# Another way of doing this
V(g)$x <- coords[,1]
V(g)$y <- coords[,2]
V(g)$z <- coords[,3]
rglplot(g)

par3d(windowRect=c(100,100,640,640)) # same as par() in conventional plots

rgl.bringtotop()
rgl.bg(color='black')
rglplot(g)

# Using the viewpoint to set the view angle/point
rgl.viewpoint(0,20)
E(g)$color <- 'yellow'   # Give color to the links/edges
E(g)$width <- 0.5        # Give more weights to the links/edges
V(g)$label <- ''   
rglplot(g)

 # RGL and network animation
# Goal: Save an image of the network and programatically move the network around + Animation

library(stringr)
library(animation)
library(plotrix)
library(rgl)
library(igraph)
out.dir <- '/users/venkatasharatsripada/Downloads/'

my.rgl.out <- paste0(out.dir,"Network3DVisualization.png")
rgl.snapshot(filename = my.rgl.out)

# Create an animation
max.loops <- 60
my.angle <- rescale(1:max.loops,c(-90,90))
par3d(windowRect=c(100,100,500,500)) # same as par() in conventional plots
# window-size - 500 x 500
rgl.bringtotop()
rgl.bg(color='black')
rgl.viewpoint(0,0,zoom=.7)

for (i in 1:max.loops){
  rgl.viewpoint(theta=-my.angle[i], phi=my.angle[i]*0.7
                ,zoom=.75 - i/(max.loops*1.7))
  #theta - angle spinning around y
  #phi - angle spinning around x
  
}

## ffmpeg 
images.out <- paste0(out.dir, 'out/')
par3d(windowRect=c(100,100,500,500)) # same as par() in conventional plots
# window-size - 500 x 500
rgl.bringtotop()
rgl.bg(color='black')
rgl.viewpoint(0,0,zoom=.7)
my.angle <- rescale(1:max.loops,c(-180,180))
rgl.viewpoint(0,0,zoom=.7)

max.loops <- 40
for (i in 1:max.loops){
  rgl.viewpoint(theta=my.angle[i], phi=0
                ,zoom=.75 - i/(max.loops*1.7))
  #theta - angle spinning around y
  #phi - angle spinning around x
  snapshot.fname <- paste0(images.out, 'network', str_pad(i, width = 4
                                                          , side='left'
                                                          , pad='0')
                           , '.png')
  rgl.snapshot(filename = snapshot.fname)
}

# Use the animation pkg alongwith ffmpeg to read the network files we 
# created above 
ani.options(interval=0.1)
imgs <- list.files(images.out, pattern='*.png')
saveGIF({
  for(img in imgs){
    im <- magick::image_read()
  }
})





