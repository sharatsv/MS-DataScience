# 
library(rgl)
library(scatterplot3d)

n <- 1000
x <- rnorm(n)
y <- ((2*x)^2)/10 + rnorm(n, mean=0, sd=0.2)
z <- sqrt(abs(x)) + rnorm(n, mean=0, sd=0.2)

scatterplot3d(x,y,z,pch=16,type='h')
plot3d(x,y,z,col='red',size=3) # creates a 3D you can interact with
plot3d(x,y,z,col='gold',size=1,type='s') # creates a 3D you can interact with
rgl.bg(color='black')
rgl.viewpoint(0,0,zoom=.7)

rgl.clear(type='shapes') # clear the shapes when adding new objects

spheres3d(4*x, 4*y, 4*z, radius=.1, col='gold')

rgl.light(theta=0, phi=0
          ,viewpoint.rel = T
          , ambient='#FFFFFF'
          , diffuse = '#FFFFFF'
          , specular = '#FFFFFF')

# ambient - color of light 
# diffuse - is what is being reflected back
# specular - shinny part of the glass that would reflect

rgl.clear(type='lights')
light3d(diffuse='gray75'
        , specular='gray75'
        , viewpoint.rel=F)

rgl.light( ambient='#444444'
          , diffuse = '#0000FF'
          , specular = '#FF0000')

rgl.viewpoint(0,0,zoom=0.2)

open3d()
par3d()$windowRect # Create a canvas to work in

lines3d(x=c(-2,2), y=c(0,0), z=c(0,0), col='red',lwd=1)
text3d(2.2,0,0,"x",color='red',cex=1,adj=0)

lines3d(x=c(0,0), y=c(-2,2), z=c(0,0), col='green',lwd=1)
text3d(0,2.2,0,"y",color='green',cex=1,adj=0)

lines3d(x=c(0,0), y=c(0,0), z=c(-2,2), col='blue',lwd=1)
text3d(0,0,2.2,"y",color='blue',cex=1,adj=0)

wire3d(cube3d())

wire3d(scale3d(cube3d(),2,2,2), col='red')

rgl.ids() #Tells you all the objects/ids in the space
rgl.pop(id=56) # To drop a certain item/shape

rgl.clear(type='shapes')
shapelist3d(tetrahedron3d(), 2, 0, 0, size=.5, color='tan')

material3d(alpha=.2, shininess=75, emision='blue') # shininess makes the object almost transparent in some angles
shapelist3d(cube3d(), 0, 2, 0, size=.2, color='cadetblue')

material3d(alpha=1, shininess=5, emision='black') # shininess makes the object almost transparent in some angles
shapelist3d(octahedron3d(), 0, 0, 2.5, size=.9, color='red')

material3d(alpha=.1, shininess=50, emision='gray75') # shininess makes the object almost transparent in some angles
shapelist3d(icosahedron3d(), 2, 0, 2.5, size=2, color='orange')


# Simple scene - Building a lamp-post
web.dir <- '/Users/venkatasharatsripada/Downloads'
open3d()
material3d(alpha=.1, shininess=50, emision='black')
rgl.bg(color='black')

n <- 10
x <- 0
y <- 0
z.start <- 0
z.end <- 10

rgl.clear(type='shapes')
M <- matrix(c(rep(x,n), rep(y,n)
            , seq(from=z.start, to=z.end
            , length.out=n)),nrow=n,byrow=F)
M2 <- cylinder3d(center=M, radius=.2, sides = 50, closed=-2)
shade3d(M2, alpha=1, color='gray45')

z.end <- 0.25
M <- matrix(c(rep(x,n), rep(y,n)
              , seq(from=z.start, to=z.end
                    , length.out=n)),nrow=n,byrow=F)
M2 <- cylinder3d(center=M, radius=1,5, sides = 50, closed=-2)
shade3d(M2, alpha=1, color='gray45')


material3d(alpha=1, shininess=50, emision='tan') # shininess makes the object almost transparent in some angles
spheres3d(x,y,10.5,radius=.25,col='yellow')

material3d(alpha=.5, shininess=50, emision='black') # shininess makes the object almost transparent in some angles
shapelist3d(cuboctahedron3d(),x,y,10.5,size=.575,color='gold')

material3d(alpha=1, shininess=50, emision='black') # shininess makes the object almost transparent in some angles
M <- matrix(c(rep(x,n), rep(y,n)
              , seq(from=11, to=11.15
                    , length.out=n)),nrow=n,byrow=F)

M4 <- cylinder3d(center=M, radius=.75, sides = 50, closed=-2)
shade3d(M4, alpha=1, color='gray45')

# Add a particle/cloud around the post
ns <- 100
xs <- rnorm(ns, 0, 3)
ys <- rnorm(ns, 0, 3)
zs <- rnorm(ns, 9, 2)
rs <- rnorm(ns, 1, .3)
material3d(alpha=1, shininess=100, emision='black') # shininess makes the object almost transparent in some angles
particles3d(xs,ys,zs,radius = rs,color='white')

writeWebGL(dir=web.dir, filename=index.html)
filename <- writeWebGL(dir=web.dir,width=500,reuse=T)


# Package mgmt
tmp <- installed.packages()
ip <- as.data.frame(tmp, stringsAsFactors = F)
table(ip$LibPath)

# View lib paths
.libPaths()

# Get all pkgs related to a str
ip$Package[grep(pattern='^gg', ip$Package)] # all pks relate to gg

# Detach from pkgs you are not using
detach<>
  
  
