figure12.1 <- function() {

m <- factor(months(zoo::as.yearmon(time(datasets::sunspots))),
            levels=month.name)
plot(m, datasets::sunspots, axes=FALSE)
axis(2)
axis(1, at=1:12, labels=FALSE)


}
figure12.2 <- function() {

m <- factor(months(zoo::as.yearmon(time(datasets::sunspots))),
            levels=month.name)
plot(m, datasets::sunspots, axes=FALSE)
axis(2)
axis(1, at=1:12, labels=FALSE)

vps <- gridBase::baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(month.name, 
          x=unit(1:12, "native"), y=unit(-1, "lines"),
          just="right", rot=60)
popViewport(3)




}
figure12.3 <- function() {
hc <- hclust(dist(USArrests), "ave")
dend1 <- as.dendrogram(hc)
dend2 <- cut(dend1, h=70)



x <- 1:4
y <- 1:4
height <- factor(round(sapply(dend2$lower, 
                              attr, "height")))



space <- 1.2 * max(stringWidth(rownames(USArrests)))
dendpanel <- function(x, y, subscripts, ...) {
  pushViewport(viewport(gp=gpar(fontsize=8)),
               viewport(y=unit(0.95, "npc"), width=0.9,
                        height=unit(0.95, "npc") - space,
                        just="top"))
  par(plt=gridBase::gridPLT(), new=TRUE, ps=8)
  plot(dend2$lower[[subscripts]], axes=FALSE)
  popViewport(2)
}



trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
plot.new()
print(xyplot(y ~ x | height, subscripts=TRUE, 
             xlab="", ylab="",
             strip=strip.custom(style=4), 
             scales=list(draw=FALSE), 
             panel=dendpanel),
      newpage=FALSE)




}
figure12.4 <- function() {

m <- factor(months(zoo::as.yearmon(time(datasets::sunspots))),
            levels=month.name)
plot(m, datasets::sunspots, axes=FALSE)
axis(2)
axis(1, at=1:12, labels=FALSE)


plot(m, datasets::sunspots)


}
figure12.5 <- function() {

m <- factor(months(zoo::as.yearmon(time(datasets::sunspots))),
            levels=month.name)
plot(m, datasets::sunspots, axes=FALSE)
axis(2)
axis(1, at=1:12, labels=FALSE)


dev.control("enable")
plot(m, datasets::sunspots)
grid.echo()
grid.edit("graphics-plot-1-bottom-axis-labels-1", 
          y=unit(-1, "lines"), hjust=1, vjust=0.5, rot=60)


}
figure12.6 <- function() {
hc <- hclust(dist(USArrests), "ave")
dend1 <- as.dendrogram(hc)
dend2 <- cut(dend1, h=70)



x <- 1:4
y <- 1:4
height <- factor(round(sapply(dend2$lower, 
                              attr, "height")))



dendpanel <- function(x, y, subscripts, ...) {
  pushViewport(viewport(gp=gpar(fontsize=8)),
               viewport(y=unit(0.95, "npc"), 
                        height=unit(0.95, "npc"),
                        just="top"))
  grid.echo(function() {
                par(mar=c(5.1, 0, 1, 0))
                plot(dend2$lower[[subscripts]], axes=FALSE)
            },
            newpage=FALSE, 
            prefix=paste0("dend-", panel.number()))
  popViewport(2)
}


xyplot(y ~ x | height, subscripts=TRUE, 
       xlab="", ylab="",
       strip=strip.custom(style=4), 
       scales=list(draw=FALSE), 
       panel=dendpanel)


}
figure12.7 <- function() {



cpfun <- function() {
    coplot(lat ~ long | depth, datasets::quakes, pch=16, cex=.5,
           given.values=rbind(c(0, 400), c(300, 700)))
}
pushViewport(viewport(y=0, height=.7, just="bottom"))
grid.echo(cpfun, newpage=FALSE, prefix="cp")
upViewport()

pushViewport(viewport(y=1, height=.33, just="top"))
gg <- ggplot(datasets::quakes) + geom_histogram(aes(x=depth)) +
      theme(axis.title.x = element_blank())
print(gg, newpage=FALSE)
upViewport()


}
figure3.1 <- function() {
par(oma=rep(3, 4), bg="gray80")
plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
box("outer", col="gray")
# set clipping to figure region
par(xpd=TRUE)
# deliberately draw a stupidly large rectangle
rect(-1, -1, 2, 2, col="gray90")
box("figure")
# set clipping back to plot region
par(xpd=FALSE)
# deliberately draw a stupidly large rectangle
rect(-1, -1, 2, 2, col="gray80")
box("plot", lty="dashed")
text(.5, .5, "Plot Region")
mtext("Figure Region", side=3, line=2)
for (i in 1:4)
    mtext(paste("Outer margin", i), side=i, line=1, outer=TRUE)



}
figure3.2 <- function() {
par(oma=rep(3, 4), mfrow=c(3,2), bg="gray80")
for (i in 1:6) {
    if (i == 3) {
      omar <- par(mar=c(2, 2, 2, 1))  
      plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
      par(xpd=TRUE)
      rect(-1, -1, 2, 2, col="gray90")
      box("figure")
      par(xpd=FALSE)
      rect(-1, -1, 2, 2, col="gray80")
      box("plot", lty="dashed")
      text(.5, .5, "Current Plot Region", cex=1.5)
      mtext("Current Figure Region", side=3)
      par(omar)
    } else {
      omar <- par(mar=rep(0, 4))  
      plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
      par(xpd=TRUE)
      rect(-1, -1, 2, 2, col="gray90")
      box("figure")
      text(.5, .5, paste("Figure", i), cex=1.5)
      par(omar)
    }
}
box("outer", col="gray")
for (i in 1:4)
    mtext(paste("Outer margin", i), side=i, line=1, outer=TRUE)



}
figure3.3 <- function() {
par(mar=c(3, 6, 2, 2), xaxs="i", yaxs="i", xpd=FALSE, las=1)
    plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
    box("figure")
    rect(0, 0, 1, 1, col="light gray", border="gray")
    axis(1, at=c(0, 1), c("", ""))
    mtext("Min x-value", side=1, adj=0, line=1)
    mtext("Max x-value", side=1, adj=1, line=1)
    axis(2, at=c(0, 1), c("", ""))
    mtext("Min y-value", side=2, at=0, adj=1, line=1)
    mtext("Max y-value", side=2, at=1, adj=1, line=1)
    lines(c(.6, .6, 0), c(0, .6, .6), lty="dashed")
    text(.6, .6, expression(paste("The location ", 
            group("(",list(x[i], y[i]),")"))), pos=3)
    points(.6, .6, pch=16)
    axis(1, at=.6, "")
    mtext(expression(x[i]), side=1, at=.6, line=.7)
    axis(2, at=.6, "")
    mtext(expression(y[i]), side=2, at=.6, line=.7)
        



}
figure3.4 <- function() {
pushViewport(viewport(layout=grid.layout(3, 1, 
  heights=unit(rep(1, 3), c("null", "cm", "null")))))
pushViewport(viewport(layout.pos.row=1))
grid.rect()
pushViewport(plotViewport(c(5, 5, 3, 2), xscale=c(0, 11)))
grid.rect(gp=gpar(col="gray"))
grid.text("Current Plot", gp=gpar(col="gray"))
grid.rect(0, unit(-5, "lines"), 1, unit(5, "lines"),
          just=c("left", "bottom"), gp=gpar(col="gray", fill="light gray"))
grid.text("Figure\nMargin\n1", y=unit(-2.5, "lines"))
grid.lines(c(0, 1), c(0, 0))
grid.segments(c(0, 1), c(0, 0), c(0, 1), unit(c(.5, .5), "lines"))
grid.text(c("xmin", "xmax"), c(0, 1), unit(c(1, 1), "lines"))
grid.lines(c(0, 0), unit(c(-1, -4), "lines"))
grid.segments(c(0, 0), unit(c(-1, -4), "lines"), 
              unit(c(-.5, -.5), "lines"), unit(c(-1, -4), "lines"))
grid.text(c("0 lines", "3 lines"),
          unit(c(-1, -1), "lines"), unit(c(-1, -4), "lines"),
          just=c("right", "bottom"))
popViewport(2)
pushViewport(viewport(layout.pos.row=3))
grid.rect()
pushViewport(plotViewport(c(5, 5, 3, 2), yscale=c(0, 11)))
grid.rect(gp=gpar(col="gray"))
grid.text("Current Plot", gp=gpar(col="gray"))
grid.rect(unit(-5, "lines"), 0, unit(5, "lines"), 1,
          just=c("left", "bottom"), gp=gpar(col="gray", fill="light gray"))
grid.text("Figure\nMargin\n2", x=unit(-2.5, "lines"))
grid.lines(c(0, 0), c(0, 1))
grid.segments(c(0, 0), c(0, 1), unit(c(.5, .5), "lines"), c(0, 1))
grid.text(c("ymin", "ymax"), unit(c(1, 1), "lines"), c(0, 1), just="left")
grid.lines(unit(c(0, -3), "lines"), c(0, 0))
grid.segments(unit(c(0, -3), "lines"), c(0, 0), 
              unit(c(0, -3), "lines"), unit(c(-.5, -.5), "lines"))
grid.text(c("0 lines", "3 lines"),
          unit(c(0, -3), "lines"), unit(c(-1, -1), "lines"),
          rot=90, just=c("right", "bottom"))
popViewport(2)
popViewport()



}
figure3.5 <- function() {
pushViewport(viewport(layout=grid.layout(3, 1, 
  heights=unit(c(1, 1, 1), c("null", "cm", "null")))))

# First page
pushViewport(viewport(layout.pos.row=3, 
  layout=grid.layout(3, 4, 
    widths=unit(c(2.5, 1, 1, 1), c("cm", "null", "null", "cm")),
    heights=unit(c(1, 1, 2.5), c("cm", "null", "cm")))))
grid.rect(gp=gpar(col="black"))
for (i in 2) {
  for (j in 2:3) {
    pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
    grid.rect(gp=gpar(col="gray"))
      pushViewport(plotViewport(c(2, 2, 1, 1), xscale=c(0, 11),
        gp=gpar(col="gray")))
      grid.rect(gp=gpar(col="gray"))
      grid.text(paste("Plot", j - 1))
      popViewport()      
    popViewport()
  }
}
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.rect(gp=gpar(col="gray", fill="light gray"))
grid.text("Outer\nMargin\n2")
grid.lines(c(1, 1), c(0, 1))
grid.segments(c(1, 1), c(0, 1), 
              unit(1, "npc") + unit(c(.5, .5), "lines"),
              c(0, 1))
grid.text(0:1, 
          unit(1, "npc") + unit(c(1, 1), "lines"),
	  c(0, 1))
grid.lines(unit(1, "npc") - unit(c(0, 3), "lines"), c(0, 0))
grid.segments(unit(1, "npc") - unit(c(0, 3), "lines"), 
	      c(0, 0), 
              unit(1, "npc") - unit(c(0, 3), "lines"), 
	      unit(c(-.5, -.5), "lines"))
grid.text(c("0 lines", "3 lines"),
          unit(1, "npc") - unit(c(0, 3), "lines"),
          unit(c(-1, -1), "lines"),
          rot=90, just=c("right", "bottom"))
popViewport(2)

# Second page
pushViewport(viewport(layout.pos.row=1, 
  layout=grid.layout(3, 4, 
    widths=unit(c(2.5, 1, 1, 1), c("cm", "null", "null", "cm")),
    heights=unit(c(1, 1, 2.5), c("cm", "null", "cm")))))
grid.rect(gp=gpar(col="black"))
for (i in 2) {
  for (j in 2:3) {
    pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
    grid.rect(gp=gpar(col="gray"))
      pushViewport(plotViewport(c(2, 2, 1, 1), xscale=c(0, 11),
        gp=gpar(col="gray")))
      grid.rect(gp=gpar(col="gray"))
      grid.text(paste("Plot", j - 1))
      popViewport()      
    popViewport()
  }
}
pushViewport(viewport(layout.pos.row=3, layout.pos.col=2:3))
grid.rect(gp=gpar(col="gray", fill="light gray"))
grid.text("Outer Margin 1")
grid.lines(c(0, 1), c(1, 1))
grid.segments(c(0, 1), 
              unit(c(1, 1), "npc"),
	      c(0, 1),
              unit(c(1, 1), "npc") + unit(.5, "lines"))
# grid.rect(c(0, 1), 
# 	  unit(c(1, 1), "npc") + unit(1, "lines"),
# 	  unit(c(1, 1), "strwidth", list("0", "1")),
# 	  unit(c(1, 1), "strheight", list("0", "1")),
# 	  gp=gpar(col=NULL, fill="white"))
grid.text(c(0, 1), 
          c(0, 1),
          unit(c(1, 1), "npc") + unit(1, "lines"))
grid.lines(c(0, 0), unit(1, "npc") - unit(c(1, 4), "lines"))
grid.segments(c(0, 0), 
              unit(1, "npc") - unit(c(1, 4), "lines"), 
	      unit(c(-.5, -.5), "lines"),
              unit(1, "npc") - unit(c(1, 4), "lines"))
grid.text(c("0 lines", "3 lines"),
          unit(c(-1, -1), "lines"),
	  unit(1, "npc") - unit(c(1, 4), "lines"),
          just=c("right", "bottom"))
popViewport(2)

popViewport()



}
figure3.6 <- function() {
EU1992 <- window(datasets::EuStockMarkets, 1992, 1993)
par(lty="dashed")
plot(EU1992[,"DAX"], ylim=range(EU1992))
lines(EU1992[,"CAC"], lty="solid")
lines(EU1992[,"FTSE"])


}
figure3.7 <- function() {
par(mar=rep(0, 4), cex=0.7)
plot.new()
plot.window(c(0.05, 0.95), 0:1)
family <- c("sans", "serif", "mono")
face <- 1:4
for (i in 1:4)
  for (j in 1:3) {
    par(family=family[j], lheight=1.5)
    text(seq(.15, .85, length=4)[i],
         seq(.25, .75, length=3)[j],
         paste("family=\"", family[j], "\"\nfont=", face[i], sep=""),
         font=face[i])
  }
segments(.02, c(.125, .375, .625, .875), 
         .98, c(.125, .375, .625, .875), col="gray")
segments(.02, c(.125, .375, .625, .875) - .01, 
         .02, c(.125, .375, .625, .875) + .01, col="gray")
segments(.98, c(.125, .375, .625, .875) - .01, 
         .98, c(.125, .375, .625, .875) + .01, col="gray")
rect(c(.27, .5, .73) - .01,
     .1,
     c(.27, .5, .73) + .01,
     .9, col="white", border=NA)



}
figure3.8 <- function() {
par(mar=rep(0, 4), xaxs="i", yaxs="i", cex=0.8)
plot.new()
par(new=TRUE)
grid.rect(gp=gpar(col="gray"))
ncol <- 4
nrow <- 4
xadj <- c(1, 0.5, NA, 0)
yadj <- c(1, 0.5, NA, 0)
size <- unit(3, "mm")
for (i in 1:nrow) {
  for (j in 1:ncol) {
    x <- i/(nrow + 1)
    y <- j/(ncol + 1)
    xu <- unit(x, "npc")
    yu <- unit(y, "npc")
    grid.segments(unit.c(xu - size, xu),
                  unit.c(yu, yu - size),
                  unit.c(xu + size, xu),
                  unit.c(yu, yu + size),
		  gp=gpar(col="gray"))
    text(x, y, paste("c(", xadj[j], ", ", yadj[i], ")", sep=""),
         adj=c(xadj[j], yadj[i]))
  }
}



}
figure3.9 <- function() {
ncol <- 6
nrow <- 1
grid.rect(gp=gpar(col="gray"))
for (i in 1:nrow) {
  for (j in 1:ncol) {
    x <- unit(j/(ncol+1), "npc")
    y <- unit(i/(nrow + 1), "npc")
    pch <- (i - 1)*ncol + j - 1
    grid.points(x + unit(3, "mm"), y, 
      pch=pch, gp=gpar(fill="gray"))
    grid.text(pch, x - unit(3, "mm"), y, gp=gpar(col="gray"))
  }
}



}
figure3.10 <- function() {
x <- -5:5
y <- -x^2 + 25
plottype <- function(type) {
  par(mar=c(1, 0, 1, 0), pty="s")
  plot.new()
  plot.window(c(-6, 6), c(-2, 27))
  box(col="gray")
  points(x, y, type=type)
  mtext(paste("type=\"", type, "\"", sep=""))
}



par(mfrow=c(3, 2))
plottype("p")
plottype("l")
plottype("b")
plottype("o")
plottype("h")
plottype("s")



}
figure3.11 <- function() {
axisfun <- function(mgp=c(3, 1, 0), xaxs="r", tcl=-.5,
                    mgpcol="black", xaxscol="black", tclcol="black") {
  par(mar=c(4, 1, 0, 1), mgp=mgp, xaxs=xaxs, tcl=tcl, pty="s")
  plot.new()
  box(col="gray")
  text(.5, .75, paste("mgp=c(", paste(mgp, collapse=", "), ")", sep=""),
       col=mgpcol)
  text(.5, .5, paste("xaxs=\"", xaxs, "\"", sep=""),
       col=xaxscol)
  text(.5, .25, paste("tcl=", tcl, sep=""),
       col=tclcol)
  axis(1, at=c(0, .5, 1))
  title(xlab="X-axis Label")
}



par(mfrow=c(2, 2))
axisfun()
axisfun(mgp=c(2, 0.3, 0), tcl=0.2, xaxscol="gray")
axisfun(xaxs="i", mgpcol="gray", tclcol="gray")



}
figure3.12 <- function() {

par(oma=rep(3, 4))
vps <- gridBase::baseViewports()
# Annotation helper function
annWidth <- function(x, y, lab, above=TRUE, horiz=TRUE) {
  grid.lines(x=x, y=y, 
             arrow=arrow(ends="both", angle=10, type="closed",
                         length=unit(3, "mm")), 
             gp=gpar(fill="black"))
  nl <- length(lab)
  if (nl > 1) {
    y <- y + unit(c(-0.5, 0.5), "lines")
    if (horiz) {
      vjust <- 1:0
      hjust <- 0.5
      rot <- 0
    } else {
      hjust <- 1:0
      vjust <- 0.5
      rot <- 90
    }
  } else {
    hjust <- 0.5
    rot <- 0
    if (above) {
      y <- y + unit(0.5, "lines")
      vjust <- 0
    } else {
      y <- y - unit(0.5, "lines")
      vjust <- 1
    }
  }
  grid.text(lab,
            x=0.5*sum(x),
            y=y, hjust=hjust, vjust=vjust, rot=rot,
            gp=gpar(fontfamily="mono", cex=1))
}
# Annotate whole page
grid.rect(gp=gpar(col="gray", fill="gray80"))
annWidth(0:1, unit(1, "npc") - unit(1.5, "lines"), "din[1]")
# grid.lines(x=0.5)
annWidth(unit(c(0, 3), "lines"), unit(0.7, "npc"), c("omi[2]", "oma[2]"))
annWidth(unit(1, "npc") - unit(c(0, 3), "lines"),
         unit(0.7, "npc"), c("omi[4]", "oma[4]"))
annWidth(unit(c(0, 3), "lines"), unit(0.3, "npc"), 
         "omd[1]", above=FALSE)
annWidth(unit.c(unit(0, "npc"),
                unit(1, "npc") - unit(3, "lines")),
         unit(2, "lines"), "omd[2]",
         above=FALSE)
# Annotate figure region
pushViewport(do.call("vpStack", vps[1:2]))
grid.rect(gp=gpar(fill="gray90"))
annWidth(0:1, unit(1, "npc") - unit(1.5, "lines"), "fin[1]")
annWidth(unit(c(0, 4.1), "lines"), unit(0.6, "npc"), c("mai[2]", "mar[2]"))
annWidth(unit(1, "npc") - unit(c(0, 2.1), "lines"),
         unit(0.6, "npc"), c("mai[4]", "mar[4]"), horiz=FALSE)
annWidth(unit(c(0, 4.1), "lines"), unit(0.4, "npc"), 
         "plt[1]", above=FALSE)
annWidth(unit.c(unit(0, "npc"),
                unit(1, "npc") - unit(2.1, "lines")),
         unit(4, "lines"), "plt[2]",
         above=FALSE)
# Annotate plot region
pushViewport(vps[[3]])
grid.rect(gp=gpar(lty="dashed", fill="gray80"))
annWidth(0:1, unit(1, "npc") - unit(1.5, "lines"), "pin[1]")
popViewport(3)



}
figure3.13 <- function() {
grid.lshow <- function(i, j, lab, order, nrow, ncol, heights, respect) {
  pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
  pushViewport(viewport(width=unit(1, "npc") - unit(2, "lines"),
               height=unit(1, "npc") - unit(3, "lines"),
	       y=unit(3, "lines"), just="bottom", 
    layout=grid.layout(nrow, ncol, heights=heights, 
      respect=respect)))
  grid.rect(gp=gpar(col="gray"))
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      pushViewport(viewport(layout.pos.row=i, layout.pos.col=j))
      grid.rect()
      grid.text(order[i, j])
      popViewport()
    }
  }
  popViewport()
  grid.text(lab, y=unit(2, "lines"))
  popViewport()
}
pushViewport(viewport(layout=grid.layout(2, 2)))
grid.lshow(1, 1, "(a)", cbind(c(1, 3, 5), c(2, 4, 6)), 3, 2, rep(1, 3), 
  FALSE)
grid.lshow(1, 2, "(b)", cbind(c(6, 4, 2), c(5, 3, 1)), 3, 2, rep(1, 3), 
  FALSE)
grid.lshow(2, 1, "(c)", matrix(c(1, 2), ncol=1), 2, 1, c(2, 1), FALSE)
grid.lshow(2, 2, "(d)", matrix(c(1, 2), ncol=1), 2, 1, c(2, 1), TRUE)
popViewport()



}
figure3.14 <- function() {
grid.lshow <- function(i, j, lab, locs, nrow, ncol, heights, respect) {
  pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
  pushViewport(viewport(width=unit(1, "npc") - unit(2, "lines"),
               height=unit(1, "npc") - unit(3, "lines"),
	       y=unit(3, "lines"), just="bottom", 
    layout=grid.layout(nrow, ncol, heights=heights, 
      respect=respect)))
  grid.rect(gp=gpar(col="gray"))
  for (i in locs) {
      pushViewport(viewport(layout.pos.row=i$rows, layout.pos.col=i$cols))
      grid.rect()
      grid.text(i$lab)
      popViewport()
  }
  popViewport()
  grid.text(lab, y=unit(2, "lines"))
  popViewport()
}
pushViewport(viewport(layout=grid.layout(2, 2)))
grid.lshow(1, 1, "(a)", 
  list(
    list(rows=1, cols=1, lab=1),
    list(rows=3, cols=1, lab=2)),
  3, 1,
  unit(c(2, 0.5, 1), c("null", "cm", "null")), 
  TRUE)
grid.lshow(1, 2, "(b)", 
  list(
    list(rows=1, cols=1, lab=1),
    list(rows=3, cols=1:2, lab=2),
    list(rows=1, cols=2, lab=3)), 
  3, 2,
  unit(c(2, 0.5, 1), c("null", "cm", "null")), 
  TRUE)
grid.lshow(2, 1, "(c)", 
  list(
    list(rows=1, cols=1, lab=1),
    list(rows=3, cols=1:2, lab=2),
    list(rows=1, cols=2, lab=3)), 
  3, 2,
  unit(c(2, 0.5, 1), c("null", "cm", "null")), 
  cbind(c(0, 0, 1), c(0, 0, 0)))
popViewport()



}
figure3.15 <- function() {
par(mfrow=c(1, 2), mar=c(1, 1, 2, 1))
par(cex=0.7)
EUdays <- window(datasets::EuStockMarkets, c(1992,1), c(1992,10))
plot(EUdays[,"DAX"], ylim=range(EUdays), ann=FALSE, 
     axes=FALSE, type="l", col="gray")

points(EUdays[,"DAX"])
lines(EUdays[,"CAC"], col="gray")
points(EUdays[,"CAC"], pch=2)
lines(EUdays[,"FTSE"], col="gray")
points(EUdays[,"FTSE"], pch=3)


box(col="gray")
mtext("points() & lines()", side=3, line=0.5)
x <- 1:5
y <- x
plot(x, y, ann=FALSE, axes=FALSE, col="gray", pch=16)

text(x[-3], y[-3], c("right", "top", "bottom", "left"), 
     pos=c(4, 3, 1, 2))
text(3, 3, "overlay")

box(col="gray")
mtext("text()", side=3, line=0.5)



}
figure3.16 <- function() {
t <- seq(60, 360, 30)
x <- cos(t/180*pi)*t/360
y <- sin(t/180*pi)*t/360




source(system.file("extra", "as.raster.R", package="RGraphics"))
  ## Silly warning from pixmap::pixmap() about cellres=NULL
suppressWarnings(
  rlogo <- pixmap::read.pnm(system.file("pictures/logo.pgm", 
                              package="pixmap")[1]))



par(mfrow=c(1, 2), mar=c(1, 1, 2, 1))
par(cex=0.7)

t <- seq(60, 360, 30)
x <- cos(t/180*pi)*t/360
y <- sin(t/180*pi)*t/360


par(mfrow=c(3, 3), mar=rep(1, 4), pty="s")
plot(x, y, pch=16, col="gray",
     xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
     axes=FALSE, ann=FALSE)
box(col="gray")
mtext("lines()", side=3, line=.6, cex=.7, family="mono")
lines(x, y)

plot(x, y, pch=16, col="gray",
     xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
     axes=FALSE, ann=FALSE)
box(col="gray")
mtext("segments()", side=3, line=.6, cex=.7, family="mono")
segments(0, 0, x, y)

plot(x, y, pch=16, col="gray",
     xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
     axes=FALSE, ann=FALSE)
box(col="gray")
mtext("arrows()", side=3, line=.6, cex=.7, family="mono")
arrows(0, 0, x[-1], y[-1], length=.1)

plot(x, y, pch=16, col="gray",
     xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
     axes=FALSE, ann=FALSE)
box(col="gray")
mtext("xspline()", side=3, line=.6, cex=.7, family="mono")
xspline(x, y, shape=1)

plot(x, y, pch=16, col="gray",
     xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
     axes=FALSE, ann=FALSE)
box(col="gray")
mtext("rect()", side=3, line=.6, cex=.7, family="mono")
rect(min(x), min(y), max(x), max(y), col="gray")

plot(x, y, pch=16, col="gray",
     xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
     axes=FALSE, ann=FALSE)
box(col="gray")
mtext("polygon()", side=3, line=.6, cex=.7, family="mono")
polygon(x, y, col="gray")

plot(x, y, pch=16, col="gray",
     xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
     axes=FALSE, ann=FALSE)
box(col="gray")
mtext("polypath()", side=3, line=.6, cex=.7, family="mono")
polypath(c(x, NA, .5*x), c(y, NA, .5*y),
         col="gray", rule="evenodd")

plot(x, y, pch=16, col="gray",
     xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
     axes=FALSE, ann=FALSE)
box(col="gray")
mtext("xspline()", side=3, line=.6, cex=.7, family="mono")
xspline(x, y, shape=1, open=FALSE, col="gray")

plot(x, y, pch=16, col="gray",
     xlim=c(-.6, 1.1), ylim=c(-1.1, .6),
     axes=FALSE, ann=FALSE)
box(col="gray")
mtext("rasterImage()", side=3, line=.6, cex=.7, family="mono")
rasterImage(rlogo,
            x - .1, y - .1,
            x + .1, y + .1)





}
figure3.17 <- function() {
par(mfrow=c(1, 2), mar=c(1, 1, 2, 1), pty="s")
par(cex=0.7)
plot(datasets::cars, ann=FALSE, axes=FALSE, col="gray", pch=16)

lmfit <- lm(dist ~ speed, datasets::cars)
abline(lmfit)
arrows(15, 90, 19, predict(lmfit, data.frame(speed=19)),
       length=0.1)
text(15, 90, "Line of best fit", pos=2)

box(col="gray")

mtext("abline() & arrows()", side=3, line=0.5)
plot(datasets::cars, ann=FALSE, axes=FALSE, col="gray", pch=16)

rug(datasets::cars$dist, side=2)

box(col="gray")

mtext("rug()", side=3, line=0.5)



}
figure3.18 <- function() {
angle <- seq(0, 2*pi, length=13)[-13]
x <- 0.15*cos(angle)
y <- 0.5 + 0.3*sin(angle)
par(mar=rep(0, 4))
plot.new()
box("outer", col="gray")
polygon(0.25 + x, y, col="gray")
text(0.75 + x[c(1, 5, 9)], y[c(1, 5, 9)], "NA", col="gray")
x[c(1, 5, 9)] <- NA
y[c(1, 5, 9)] <- NA
polygon(0.75 + x, y, col="gray")




}
figure3.19 <- function() {
par(mar=c(2, 2, 1, 1))
par(mfrow=c(2, 1), xpd=NA)

plot(datasets::EuStockMarkets[,"DAX"], type="l", axes=FALSE,
     xlab="", ylab="", main="")
box(col="gray")
mtext("Mid 1991", adj=0, side=3)
lines(x=c(1995, 1995, 1996, 1996), 
      y=c(-1000, 6000, 6000, -1000), 
      lwd=3, col="gray")
mtext("DAX", side=2, line=0)

plot(datasets::EuStockMarkets[,"FTSE"], type="l", axes=FALSE,
     xlab="", ylab="", main="")
box(col="gray")
mtext("Mid 1998", adj=1, side=3)
mtext("1995", at=1995.5, side=1)
lines(x=c(1995, 1995, 1996, 1996), 
      y=c(7000, 2500, 2500, 7000), 
      lwd=3, col="gray")
mtext("FTSE", side=2, line=0)




}
figure3.20 <- function() {
par(mfrow=c(2, 1), mar=c(5, 3, 2, 1), cex=0.5, pty="s")
with(iris,
     plot(Sepal.Length, Sepal.Width, 
          pch=as.numeric(Species), cex=1.2))
legend(6.1, 4.4, c("setosa", "versicolor", "virginica"), 
       cex=1.5, pch=1:3)

barplot(VADeaths[1:2,], angle=c(45, 135), density=30, 
        col="black", names=c("RM", "RF", "UM", "UF"))
legend(0.4, 38, c("55-59", "50-54"), cex=1.5,
       angle=c(135, 45), density=30)




}
figure3.21 <- function() {
nhtempCelsius <- 5*(nhtemp - 32)/9

plot(nhtempCelsius, axes=FALSE, ann=FALSE, ylim=c(0, 13))

axis(2, at=seq(0, 12, 4))
mtext("Degrees Centigrade", side=2, line=3)

axis(1)
axis(4, at=seq(0, 12, 4), labels=seq(0, 12, 4)*9/5 + 32)
mtext(" Degrees Fahrenheit", side=4, line=3)
box()




}
figure3.22 <- function() {
par(mar=rep(1, 4))
plot(0:1, 0:1, type="n", axes=FALSE, ann=FALSE)
usr <- par("usr")
pin <- par("pin")
xcm <- diff(usr[1:2])/(pin[1]*2.54)
ycm <- diff(usr[3:4])/(pin[2]*2.54)

rect(0, 0, 1, 1, col="white")
segments(seq(1, 8, 0.1)*xcm, 0,
         seq(1, 8, 0.1)*xcm, 
         c(rep(c(0.5, rep(0.25, 4), 
                 0.35, rep(0.25, 4)),
               7), 0.5)*ycm)
text(1:8*xcm, 0.6*ycm, 0:7, adj=c(0.5, 0))
text(8.2*xcm, 0.6*ycm, "cm", adj=c(0, 0))




}
figure3.23 <- function() {
layout(matrix(1:2, ncol=1), heights=1:2/6.5)
par(cex=0.7)
drunkenness <- ts(c(3875, 4846, 5128, 5773, 7327, 
                    6688, 5582, 3473, 3186,
                    rep(NA, 51)),
                  start=1912, end=1971)

# Have to copy-and-paste to shrink the mtext text (arggh!)
par(mar=c(5, 6, 2, 4))
plot(drunkenness, lwd=3, col="gray", ann=FALSE, las=2)
mtext("Drunkenness\nRelated Arrests", side=2, line=3.5, cex=0.7)
par(new=TRUE)
plot(nhtemp, ann=FALSE, axes=FALSE)
mtext("Temperature (F)", side=4, line=3, cex=0.7)
title("Using par(new=TRUE) or par(usr=...)")
axis(4)

par(mar=c(5, 4, 4, 2))
with(trees, 
     {
       plot(Height, Volume, pch=3,
            xlab="Height (ft)", 
            ylab=expression(paste("Volume ", (ft^3))))
       symbols(Height, Volume, circles=Girth/12, 
               fg="gray", inches=FALSE, add=TRUE)
     })

mtext("symbols(..., add=TRUE)", font=2, side=3, line=1)



}
figure3.24 <- function() {
x <- as.numeric(time(nhtemp))
y <- as.numeric(nhtemp)
n <- length(x)
mean <- mean(y)



x <- as.numeric(time(nhtemp))
y <- as.numeric(nhtemp)
n <- length(x)
mean <- mean(y)

par(mfrow=c(2,2), mar=c(3, 3, 1, 1))
plot(x, y, type="n", axes=FALSE, ann=FALSE)
polygon(c(x[1], x, x[n]), c(min(y), y, min(y)), 
        col="gray", border=NA)

box(col="gray")
plot(x, y, type="n", axes=FALSE, ann=FALSE)
polygon(c(x[1], x, x[n]), c(min(y), y, min(y)), 
        col="gray", border=NA)

usr <- par("usr")
rect(usr[1], usr[3], usr[2], mean, col="white", border=NA)

box(col="gray")
plot(x, y, type="n", axes=FALSE, ann=FALSE)
polygon(c(x[1], x, x[n]), c(min(y), y, min(y)), 
        col="gray", border=NA)

usr <- par("usr")
rect(usr[1], usr[3], usr[2], mean, col="white", border=NA)

lines(x, y)

box(col="gray")
plot(x, y, type="n", axes=FALSE, ann=FALSE)
polygon(c(x[1], x, x[n]), c(min(y), y, min(y)), 
        col="gray", border=NA)

usr <- par("usr")
rect(usr[1], usr[3], usr[2], mean, col="white", border=NA)

lines(x, y)

abline (h=mean, col="gray")
box()
axis(1)
axis(2) 




}
figure3.25 <- function() {
par(mfrow=c(1, 2), mar=c(3, 3, 1, 1), cex=0.7)
y <- sample(1:10)
midpts <- barplot(y, col=" light gray")
width <- diff(midpts[1:2])/4
left <- rep(midpts, y - 1) - width
right <- rep(midpts, y - 1) + width
heights <- unlist(apply(matrix(y, ncol=10), 
                        2, seq))[-cumsum(y)]
segments(left, heights, right, heights,
         col="white")

with(ToothGrowth, 
     {
       boxplot(len ~ supp, border="gray", 
               col="light gray", boxwex=0.5)
       points(jitter(rep(1:2, each=30), 0.5), 
              unlist(split(len, supp)),
              cex=0.5, pch=16)
     })




}
figure3.26 <- function() {
par(cex=.7)
pairs(iris[1:2], 
      diag.panel=function(x, ...) { 
          boxplot(x, add=TRUE, axes=FALSE,
                  at=mean(par("usr")[1:2])) 
      }, 
      text.panel=function(x, y, labels, ...) { 
          mtext(labels, side=3, line=0) 
      })





}
figure3.27 <- function() {
par(mar=rep(0, 4))
z <- 2 * volcano        
x <- 10 * (1:nrow(z))   
y <- 10 * (1:ncol(z))   
trans <- persp(x, y, z, zlim=c(0, max(z)),
               theta = 150, phi = 12, lwd=.5,
               scale = FALSE, axes=FALSE)

clines <- contourLines(x, y, z)
lapply(clines,
       function(contour) {
           lines(trans3d(contour$x, contour$y, 0, trans))
       })




}
figure3.28 <- function() {
plot.new()
plot.window(range(pressure$temperature), 
            range(pressure$pressure))
plot.xy(pressure, type="p")
box()
axis(1)
axis(2)


}
figure3.29 <- function() {
groups <- dimnames(Titanic)[[1]]
males <- Titanic[, 1, 2, 2]
females <- Titanic[, 2, 2, 2]

par(mar=c(0.5, 3, 0.5, 1))

plot.new()
plot.window(xlim=c(-200, 200), ylim=c(-1.5, 4.5))

ticks <- seq(-200, 200, 100)
y <- 1:4
h <- 0.2

lines(rep(0, 2), c(-1.5, 4.5), col="gray")
segments(-200, y, 200, y, lty="dotted")
rect(-males, y-h, 0, y+h, col="dark gray")
rect(0, y-h, females, y+h, col="light gray")
mtext(groups, at=y, adj=1, side=2, las=2)
par(cex.axis=0.8, mex=0.5)
axis(1, at=ticks, labels=abs(ticks), pos=0)

tw <- 1.5*strwidth("females")
rect(-tw, -1-h, 0, -1+h, col="dark gray")
rect(0, -1-h, tw, -1+h, col="light gray")
text(0, -1, "males", pos=2)
text(0, -1, "females", pos=4)

box("inner", col="gray")



}
plot.newclass <- function(x, y=NULL, 
                          main="", sub="",
                          xlim=NULL, ylim=NULL,
                          axes=TRUE, ann=par("ann"),
                          col=par("col"),
                          ...) {
    xy <- xy.coords(x, y)
    if (is.null(xlim))
        xlim <- range(xy$x[is.finite(xy$x)])
    if (is.null(ylim))
        ylim <- range(xy$y[is.finite(xy$y)])
    opar <- par(no.readonly=TRUE)
    on.exit(par(opar))
    plot.new()
    plot.window(xlim, ylim, ...)
    points(xy$x, xy$y, col=col, ...)
    if (axes) {
        axis(1)
        axis(2)
        box()
    }
    if (ann) 
        title(main=main, sub=sub, 
              xlab=xy$xlab, ylab=xy$ylab, ...)
}


figure6.1 <- function() {
pushViewport(viewport(layout=grid.layout(2, 2), gp=gpar(cex=0.6, fill=NA)))
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
pushViewport(plotViewport(c(5, 4, 2, 2)))
pushViewport(dataViewport(pressure$temperature, 
                          pressure$pressure,
                          name="plotRegion"))

grid.points(pressure$temperature, pressure$pressure, 
  gp=gpar(cex=0.5))
grid.rect()
grid.xaxis()
grid.yaxis()
grid.text("temperature", y=unit(-3, "line"))
grid.text("pressure", x=unit(-3, "line"), rot=90)

popViewport(3)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
pushViewport(plotViewport(c(5, 4, 2, 2)))
pushViewport(dataViewport(pressure$temperature, 
                          pressure$pressure,
                          name="plotRegion"))

grid.points(pressure$temperature, pressure$pressure, pch=2, 
  gp=gpar(cex=0.5))
grid.rect()
grid.xaxis()
grid.yaxis()
grid.text("temperature", y=unit(-3, "line"))
grid.text("pressure", x=unit(-3, "line"), rot=90)

popViewport(3)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
pushViewport(plotViewport(c(5, 4, 2, 2)))
pushViewport(dataViewport(pressure$temperature, 
                          pressure$pressure,
                          name="plotRegion"))

grid.points(pressure$temperature, pressure$pressure, pch=2, 
  gp=gpar(cex=0.5))
grid.rect()
grid.xaxis()
grid.yaxis()
grid.text("temperature", y=unit(-3, "line"))
grid.text("pressure", x=unit(-3, "line"), rot=90)

upViewport(2)
grid.rect(gp=gpar(lty="dashed"))

downViewport("plotRegion")
grid.text("Pressure (mm Hg)\nversus\nTemperature (Celsius)",
          x=unit(150, "native"), y=unit(600, "native"))




}
figure6.2 <- function() {
grid.rect(gp=gpar(col="gray"))
grid.circle(x=seq(0.1, 0.9, length=100), 
            y=0.5 + 0.4*sin(seq(0, 2*pi, length=100)),
            r=abs(0.1*cos(seq(0, 2*pi, length=100))))




}
figure6.3 <- function() {
grid.rect(gp=gpar(col="gray"))
grid.circle(c(.1, .3, .4, .6, .7, .9), 
            c(.25, .75), r=unit(1, "mm"),
            gp=gpar(col=NA, fill="gray"))
grid.curve(x1=.1, y1=.25, x2=.3, y2=.75)
grid.curve(x1=.4, y1=.25, x2=.6, y2=.75,
           square=FALSE, ncp=8, curvature=.5)
grid.curve(x1=.7, y1=.25, x2=.9, y2=.75,
           square=FALSE, angle=45, shape=-1)




}
figure6.4 <- function() {
grid.rect(gp=gpar(col="gray"))
angle <- seq(0, 2*pi, length=50)
grid.lines(x=seq(0.1, 0.5, length=50), 
           y=0.5 + 0.3*sin(angle), arrow=arrow())
grid.segments(6:8/10, 0.2, 7:9/10, 0.8,
              arrow=arrow(angle=15, type="closed"))




}
figure6.5 <- function() {
grid.rect(gp=gpar(col="gray"))
angle <- seq(0, 2*pi, length=10)[-10]
grid.polygon(x=0.25 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
             gp=gpar(fill="gray"))
grid.polygon(x=0.75 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
             id=rep(1:3, each=3),
             gp=gpar(fill="gray"))




}
figure6.6 <- function() {
grid.rect(gp=gpar(col="gray"))
angle <- seq(0, 2*pi, length=10)[-10]
grid.path(x=0.25 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
          gp=gpar(fill="gray"))
grid.path(x=c(0.75 + 0.15*cos(angle), .7, .7, .8, .8),
          y=c(0.5 + 0.3*sin(angle),  .4, .6, .6, .4), 
          id=rep(1:2, c(9, 4)),
          gp=gpar(fill="gray"))




}
figure6.7 <- function() {
grid.rect(gp=gpar(col="gray"))
pushViewport(viewport(gp=gpar(col="gray")))
grid.text("very snug", 0.4, unit(1, "in"), just=c("left", "bottom"))
grid.lines(x=0.4, y=unit(0:1, "in"), arrow=arrow(ends="both",
                                       length=unit(1, "mm")))
grid.text("1 inch", unit(0.4, "npc") + unit(0.5, "line"), 
  unit(0.5, "in"), rot=90)
grid.lines(x=c(0, 0.4), y=unit(1, "in"), arrow=arrow(ends="both",
                                           length=unit(1, "mm")))
grid.text(unit(0.4, "npc"), 0.2, unit(1, "in") + unit(0.5, "line"))
popViewport()
pushViewport(viewport(gp=gpar(fill=NA)))
grid.rect(x=unit(0.4, "npc"), y=unit(1, "in"),
          width=stringWidth("very snug"), 
          height=unit(1, "line"),
          just=c("left", "bottom"))




}
figure6.8 <- function() {
grid.rect(gp=gpar(col="gray"))
pushViewport(viewport(gp=gpar(fontsize=10)))
grid.rect(x=0.33, height=0.7, width=0.2, gp=gpar(fill="black"))
grid.rect(x=0.66, height=0.7, width=0.2)
grid.text("grid.rect()", x=0.66, rot=90)
grid.text("grid.rect(gp=gpar(fill=\"black\"))", x=0.33, rot=90, 
  gp=gpar(fontsize=8, col="white"))
popViewport()



}
figure6.9 <- function() {
grid.rect(gp=gpar(col="gray"))
levels <- round(seq(90, 10, length=25))
grays <- paste("gray", c(levels, rev(levels)), sep="")
grid.circle(x=seq(0.1, 0.9, length=100), 
            y=0.5 + 0.4*sin(seq(0, 2*pi, length=100)),
            r=abs(0.1*cos(seq(0, 2*pi, length=100))),
            gp=gpar(col=grays))




}
figure6.10 <- function() {
grid.rect(gp=gpar(col="gray"))
angle <- seq(0, 2*pi, length=11)[-11]
grid.polygon(x=0.25 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
             id=rep(1:2, c(7, 3)),
             gp=gpar(fill=c("gray", "white")))
angle[4] <- NA
grid.polygon(x=0.75 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle), 
             id=rep(1:2, c(7, 3)),
             gp=gpar(fill=c("gray", "white")))

angle <- seq(0, 2*pi, length=11)[4]
grid.text("NA", x=0.75 + 0.15*cos(angle), y=0.5 + 0.3*sin(angle),
          gp=gpar(col="gray"))



}
figure6.11 <- function() {
vp1 <- 
viewport(x=unit(0.4, "npc"), y=unit(1, "cm"),
         width=stringWidth("very very snug indeed"), 
         height=unit(6, "line"),
         just=c("left", "bottom"))

grid.show.viewport(scale.col="gray", border.fill="white", vp.col="black", vp.fill="gray", vp1)
grid.rect(gp=gpar(col="white", fill=NA, lwd=3))
pushViewport(viewport(.5, .5, .8, .8))
pushViewport(vp1)
grid.rect(gp=gpar(fill=NA))
grid.text("very very snug indeed", 
          gp=gpar(col="white"))
popViewport(2)



}
figure6.12 <- function() {
grid.rect(gp=gpar(col="gray"))
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))
pushViewport(viewport(width=0.8, height=0.5, angle=10, 
             name="vp1"))
grid.rect()
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))




}
figure6.13 <- function() {
grid.rect(gp=gpar(col="gray"))
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))
pushViewport(viewport(width=0.8, height=0.5, angle=10, 
             name="vp1"))
grid.rect()
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))

pushViewport(viewport(width=0.8, height=0.5, angle=10, 
             name="vp2"))
grid.rect()
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))




}
figure6.14 <- function() {
grid.rect(gp=gpar(col="gray"))
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))
pushViewport(viewport(width=0.8, height=0.5, angle=10, 
             name="vp1"))
grid.rect()
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))

pushViewport(viewport(width=0.8, height=0.5, angle=10, 
             name="vp2"))
grid.rect()
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))

popViewport()
grid.text("bottom-right corner", 
          x=unit(1, "npc") - unit(1, "mm"),
          y=unit(1, "mm"), just=c("right", "bottom"))




}
figure6.15 <- function() {
pushViewport(viewport(gp=gpar(fill=NA)))
grid.rect(gp=gpar(col="gray"))
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))
pushViewport(viewport(width=0.8, height=0.5, angle=10, 
             name="vp1"))
grid.rect()
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))

pushViewport(viewport(width=0.8, height=0.5, angle=10, 
             name="vp2"))
grid.rect()
grid.text("top-left corner", x=unit(1, "mm"),
          y=unit(1, "npc") - unit(1, "mm"), 
          just=c("left", "top"))

popViewport()
grid.text("bottom-right corner", 
          x=unit(1, "npc") - unit(1, "mm"),
          y=unit(1, "mm"), just=c("right", "bottom"))

upViewport()
grid.text("bottom-right corner", 
          x=unit(1, "npc") - unit(1, "mm"),
          y=unit(1, "mm"), just=c("right", "bottom"))
downViewport("vp1")
grid.rect(width=unit(1, "npc") + unit(2, "mm"),
          height=unit(1, "npc") + unit(2, "mm"))




}
figure6.16 <- function() {
pushViewport(viewport(layout=grid.layout(1, 3)))
pushViewport(viewport(layout.pos.col=1, gp=gpar(fill=NA)))
grid.rect(width=.9, height=.9, gp=gpar(col="gray"))
pushViewport(viewport(width=.5, height=.5, clip="on"))
grid.rect()
grid.circle(r=.7, gp=gpar(lwd=20))

popViewport(2)
pushViewport(viewport(layout.pos.col=2, gp=gpar(fill=NA)))
grid.rect(width=.9, height=.9, gp=gpar(col="gray"))
pushViewport(viewport(width=.5, height=.5, clip="on"))
grid.rect()
grid.circle(r=.7, gp=gpar(lwd=20))

pushViewport(viewport(clip="inherit"))
grid.circle(r=.7, gp=gpar(lwd=10, col="gray"))

popViewport(3)
pushViewport(viewport(layout.pos.col=3, gp=gpar(fill=NA)))
grid.rect(width=.9, height=.9, gp=gpar(col="gray"))
pushViewport(viewport(width=.5, height=.5, clip="on"))
grid.rect()
grid.circle(r=.7, gp=gpar(lwd=20))

pushViewport(viewport(clip="inherit"))
grid.circle(r=.7, gp=gpar(lwd=10, col="gray"))

pushViewport(viewport(clip="off"))
grid.circle(r=.7)
popViewport(3)

popViewport()



}
figure6.17 <- function() {
circText <- function(lab, x, y, suffix) {
    grid.circle(x, y, r=unit(3, "mm"), 
                name=paste(lab, suffix, sep="-"))
    grid.text(lab, x, y,
              gp=if (lab == "ROOT") gpar(cex=.7) else NULL)
}
edge <- function(a, b, angle) {
    grid.segments(grobX(a, angle), grobY(a, angle),
                  grobX(b, 180 + angle), grobY(b, 180 + angle),
                  arrow=arrow(length=unit(2, "mm"),
                    type="closed"),
                  gp=gpar(fill="black"))
}
grid.newpage()
pushViewport(viewport(width=.9, height=.9,
                      layout=grid.layout(2, 2),
                      gp=gpar(cex=.5)))
pushViewport(viewport(layout.pos.col=1, 
                      layout.pos.row=1))
grid.rect(width=.9, height=.9, gp=gpar(col="gray"))
circText("ROOT", .5, .8, 1)
circText("A", .3, .6, 1)
circText("B", .5, .6, 1)
circText("C", .7, .6, 1)
edge("ROOT-1", "A-1", 225)
edge("ROOT-1", "B-1", 270)
edge("ROOT-1", "C-1", 315)
popViewport()
pushViewport(viewport(layout.pos.col=2, 
                      layout.pos.row=1))
grid.rect(width=.9, height=.9, gp=gpar(col="gray"))
circText("ROOT", .5, .8, 2)
circText("A", .5, .6, 2)
circText("B", .5, .4, 2)
circText("C", .5, .2, 2)
edge("ROOT-2", "A-2", 270)
edge("A-2", "B-2", 270)
edge("B-2", "C-2", 270)
popViewport()
pushViewport(viewport(layout.pos.col=1, 
                      layout.pos.row=2))
grid.rect(width=.9, height=.9, gp=gpar(col="gray"))
circText("ROOT", .5, .8, 3)
circText("A", .5, .6, 3)
circText("B", .4, .4, 3)
circText("C", .6, .4, 3)
edge("ROOT-3", "A-3", 270)
edge("A-3", "B-3", 244)
edge("A-3", "C-3", 296)
popViewport()



}
figure6.18 <- function() {
grid.rect(gp=gpar(col="gray"))
pushViewport(viewport(gp=gpar(fill="gray", fontsize=10)))
grid.text("viewport(gp=gpar(fill=\"gray\"))", y=0.925)
grid.rect(x=0.33, height=0.7, width=0.2)
grid.text("grid.rect()", x=0.33, rot=90)
grid.rect(x=0.66, height=0.7, width=0.2, gp=gpar(fill="black"))
grid.text("grid.rect(gp=gpar(fill=\"black\"))", x=0.66, rot=90, 
  gp=gpar(fontsize=8, col="white"))
popViewport()



}
figure6.19 <- function() {
labelvp <- function(name, col="gray", tcol="white", clipOff=TRUE) {
  seekViewport(name)
  if (clipOff)
    pushViewport(viewport(clip="off"))
  grid.rect(gp=gpar(col=col, lwd=5, fill=NA))
  grid.rect(x=0, y=1, width=unit(1, "strwidth", name) + unit(2, "mm"),
    height=unit(1, "line"), just=c("left", "top"),
    gp=gpar(fill=col, col=NA))
  grid.text(name, x=unit(1, "mm"), y=unit(1, "npc") - unit(1, "mm"),
    just=c("left", "top"), gp=gpar(col=tcol))
  upViewport(0)
}



vplay <- grid.layout(3, 3, 
                     respect=rbind(c(0, 0, 0), 
                                   c(0, 1, 0), 
                                   c(0, 0, 0)))



pushViewport(viewport(width=0.95, height=0.95))
grid.rect(gp=gpar(col="light gray"))
pushViewport(viewport(layout=vplay))

pushViewport(viewport(layout.pos.col=2, name="col2"))
upViewport()
pushViewport(viewport(layout.pos.row=2, name="row2"))

labelvp("col2", "black")
labelvp("row2")



}
figure6.20 <- function() {
unitlay <- 
  grid.layout(3, 3, 
              widths=unit(c(1, 1, 2), 
                          c("in", "null", "null")), 
              heights=unit(c(3, 1, 1), 
                           c("line", "null", "null")))



pushViewport(viewport(gp=gpar(cex=0.8)))
grid.show.layout(unitlay, bg="white", 
                 cell.border="black", cell.fill="gray90", 
                 label.col="black", unit.col="black",
                 newpage=FALSE)
grid.rect(gp=gpar(col="white", lwd=3, fill=NA))
popViewport()



}
figure6.21 <- function() {
gridfun <- function() {
  pushViewport(viewport(layout=grid.layout(1, 2)))
  pushViewport(viewport(layout.pos.col=1))
  grid.rect()
  grid.text("black")
  grid.text("&", x=1)
  popViewport()
  pushViewport(viewport(layout.pos.col=2, clip="on"))
  grid.rect(gp=gpar(fill="black"))
  grid.text("white", gp=gpar(col="white"))
  grid.text("&", x=0, gp=gpar(col="white"))
  popViewport(2)
}



grid.rect(gp=gpar(col="gray"))
w <- unit(1, "npc") - unit(15, "mm")
x <- unit.c(unit(5, "mm"),
            unit(5, "mm") + 1/3*w,
            unit(5, "mm") + 1/3*w + unit(5, "mm"),
	    unit(1, "npc") - unit(5, "mm"))
y <- unit.c(unit(5, "mm"),
            unit(5, "mm") + 2/3*w,
            unit(5, "mm") + 2/3*w + unit(5, "mm"),
	    unit(1, "npc") - unit(5, "mm"))
grid.segments(x, 0, x, 1,
  gp=gpar(col="gray", lty="dashed"))
grid.segments(0, y, 1, y,
  gp=gpar(col="gray", lty="dashed"))
pushViewport(
  viewport(
    layout=grid.layout(5, 5, 
                       widths=unit(c(5, 1, 5, 2, 5),
                                   c("mm", "null", "mm",
                                     "null", "mm")),  
                       heights=unit(c(5, 1, 5, 2, 5),
                                    c("mm", "null", "mm",
                                      "null", "mm")))))
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
gridfun()
popViewport()

pushViewport(viewport(layout.pos.col=4, layout.pos.row=4))
gridfun()
popViewport(2)




}
figure6.22 <- function() {

n <- 7
primtest2 <- function(nas, na) {
  angle <- seq(0, 2*pi, length=n+1)[-(n+1)]
  y <- 0.5 + 0.4*sin(angle)
  x <- 0.5 + 0.4*cos(angle)
  if (any(nas))
    grid.text(paste("NA", (1:n)[nas], sep=""),
              x[nas], y[nas], gp=gpar(col="gray"))
  x[nas] <- na
  y[nas] <- na
  grid.polygon(x, y, gp=gpar(fill="light gray", col=NA))
  grid.lines(x, y, arrow=arrow(), gp=gpar(lwd=5))
  grid.move.to(x[1], y[1])
  for (i in 2:n) {
    grid.line.to(x[i], y[i], gp=gpar(col="white"))
  }
}
celltest <- function(r, c, nas, na) {
  pushViewport(viewport(layout.pos.col=c,
                        layout.pos.row=r))
  primtest2(nas, na)
  grid.rect(width=0.9, height=0.9, gp=gpar(col="gray", fill=NA))
  popViewport()
}
cellnas <- function(i) {
  temp <- rep(FALSE, n)
  temp[i] <- TRUE
  temp[n-3+i] <- TRUE
  temp
}
pushViewport(viewport(width=.8, height=.8, 
                      layout=grid.layout(2, 2),
                      gp=gpar(cex=0.7)))
celltest(1, 1, rep(FALSE, n), NA)
celltest(1, 2, cellnas(1), NA)
celltest(2, 1, cellnas(2), NA)
celltest(2, 2, cellnas(3), NA)
popViewport()



}
figure6.23 <- function() {
trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
print(
xyplot(mpg ~ disp | factor(gear), data=mtcars,
       panel=function(subscripts, ...) {
           grid.text(paste("n =", length(subscripts)),
                     unit(1, "npc") - unit(1, "mm"),
                     unit(1, "npc") - unit(1, "mm"),
                     just=c("right", "top"))
           panel.xyplot(subscripts=subscripts, ...)
       })

)



}
figure6.24 <- function() {
trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
grid.newpage()
pushViewport(viewport(x=0, width=.4, just="left"))
print(barchart(table(mtcars$gear)),
      newpage=FALSE)
popViewport()
pushViewport(viewport(x=.4, width=.6, just="left"))
print(xyplot(mpg ~ disp, data=mtcars,
             group=gear, 
             auto.key=list(space="right"),
             par.settings=list(
                 superpose.symbol=list(pch=c(1, 3, 16),
                                       fill="white"))),
      newpage=FALSE)
popViewport()



}
figure6.25 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg



print(
ggplot(mtcars2, aes(x=disp, y=mpg)) +
    geom_point()

)
# Navigate to ROOT viewport so that this code works for example(figure6.25)
# in 'RGraphics' package
upViewport(0)
grid.force()
panelvp <- grid.grep("panel", grobs=FALSE, 
                     viewports=TRUE, grep=TRUE)
downViewport(panelvp)
grid.text(paste("n =", nrow(mtcars2)),
          x=unit(1, "npc") - unit(1, "mm"), 
          y=unit(1, "npc") - unit(1, "mm"),
          just=c("right", "top"))




}
figure6.26 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg



grid.newpage()
pushViewport(viewport(x=0, width=1/3, just="left"))
print(ggplot(mtcars2, aes(x=trans)) + 
      geom_bar(),
      newpage=FALSE)
popViewport()
pushViewport(viewport(x=1/3, width=2/3, just="left"))
print(ggplot(mtcars2, aes(x=disp, y=mpg)) +
      geom_point(aes(color=trans)) +
      scale_color_manual(values=gray(2:1/3)),
      newpage=FALSE)
popViewport()



}
figure1.2 <- function() {


#
#  Comment:
# 
#  Examples of the use of standard high-level plotting functions.
# 
#  In each case, extra output is also added using low-level 
#  plotting functions.
#


par(mfrow=c(3, 2))

# Scatterplot
x <- c(0.5, 2, 4, 8, 12, 16)
y1 <- c(1, 1.3, 1.9, 3.4, 3.9, 4.8)
y2 <- c(4, .8, .5, .45, .4, .3)
par(las=1, mar=c(4, 4, 2, 4), cex=.7)
plot.new()
plot.window(range(x), c(0, 6))
lines(x, y1)
lines(x, y2)
points(x, y1, pch=16, cex=2)
points(x, y2, pch=21, bg="white", cex=2)
par(col="gray50", fg="gray50", col.axis="gray50")
axis(1, at=seq(0, 16, 4))
axis(2, at=seq(0, 6, 2))
axis(4, at=seq(0, 6, 2))
box(bty="u")
mtext("Travel Time (s)", side=1, line=2, cex=0.8)
mtext("Responses per Travel", side=2, line=2, las=0, cex=0.8)
mtext("Responses per Second", side=4, line=2, las=0, cex=0.8)
text(4, 5, "Bird 131")
par(mar=c(5.1, 4.1, 4.1, 2.1), col="black", fg="black", col.axis="black")

# Histogram
# Random data
Y <- rnorm(50)
# Make sure no Y exceed [-3.5, 3.5]
Y[Y < -3.5 | Y > 3.5] <- NA
x <- seq(-3.5, 3.5, .1)
dn <- dnorm(x)
par(mar=c(4.5, 4.1, 3.1, 0))
hist(Y, breaks=seq(-3.5, 3.5), ylim=c(0, 0.5), 
     col="gray80", freq=FALSE)
lines(x, dnorm(x), lwd=2)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Barplot
# Modified from example(barplot)
par(mar=c(2, 3.1, 2, 2.1))
midpts <- barplot(VADeaths, 
                  col=gray(0.1 + seq(1, 9, 2)/11), 
                  names=rep("", 4))
mtext(sub(" ", "\n", colnames(VADeaths)),
      at=midpts, side=1, line=0.5, cex=0.5)
text(rep(midpts, each=5), apply(VADeaths, 2, cumsum) - VADeaths/2,
     VADeaths, 
     col=rep(c("white", "black"), times=3:2), 
     cex=0.8)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Boxplot
# Modified example(boxplot) - itself from suggestion by Roger Bivand
par(mar=c(3, 4.1, 2, 0))
     boxplot(len ~ dose, data = ToothGrowth,
             boxwex = 0.25, at = 1:3 - 0.2,
             subset= supp == "VC", col="white",
             xlab="",
             ylab="tooth length", ylim=c(0,35))
     mtext("Vitamin C dose (mg)", side=1, line=2.5, cex=0.8)
     boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
             boxwex = 0.25, at = 1:3 + 0.2,

             subset= supp == "OJ")
     legend(1.5, 9, c("Ascorbic acid", "Orange juice"), 
            fill = c("white", "gray"), 
            bty="n")
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Persp
# Almost exactly example(persp)
    x <- seq(-10, 10, length= 30)
     y <- x
     f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
     z <- outer(x, y, f)
     z[is.na(z)] <- 1
# 0.5 to include z axis label
par(mar=c(0, 0.5, 0, 0), lwd=0.5)
     persp(x, y, z, theta = 30, phi = 30, 
 
           expand = 0.5)
par(mar=c(5.1, 4.1, 4.1, 2.1), lwd=1)

# Piechart
# Example 4 from help(pie)
par(mar=c(0, 2, 1, 2), xpd=FALSE, cex=0.5)
     pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
     names(pie.sales) <- c("Blueberry", "Cherry",
         "Apple", "Boston Cream", "Other", "Vanilla")
     pie(pie.sales, col = gray(seq(0.3,1.0,length=6))) 




}
figure1.3 <- function() {

#
# Comment:
#
# A sophisticated example of adding further output to a basic plot.
# 
# Most of the functions defined are just for calculating values
# relevant to the data analysis.  
# 
# The function plotPars() is the one of interest for seeing how
# the drawing of the plot is done.
#


params <- function(N, breaks, p=seq(0.001, 1, length=100)) {
  list(N=N, T=1/breaks, p=p, q=1-p)
}

pdfcomp <- function(comp, params) {
  n <- params$T
  p <- params$p
  q <- params$q
  y <- round(comp/n)
  choose(n, comp)*p^comp*q^(n-comp) / (1 - q^n)
}

# Expected num sherds (for a vessel) [=completeness]
expcomp <- function(params) {
  params$T*params$p/(1-params$q^params$T)
}

# Variance of num sherds (for a vessel)
varcomp <- function(params) {
  n <- params$T
  p <- params$p
  q <- params$q
  # From Johnson & Kotz
  (n*p*q / (1 - q^n)) - (n^2*p^2*q^n / (1 - q^n)^2)
  # n^2 times Thomas Yee's formula
  # n^2*((p*(1 + p*(n - 1)) / (n*(1 - q^n))) - (p^2 / (1 - q^n)^2))
}

# Expected value of completeness (for a sample of vessels)
expmeancomp <- function(params) {
  expcomp(params)
}

# Variance of completeness (for a sample of vessels)
# Use the expected number of vessels in sample as denominator
varmeancomp <- function(params) {
  varcomp(params)/(numvess(params))
}

numvess <- function(params) {
  params$N*(1-params$q^params$T)
}

ecomp <- function(p, T, comp) {
  q <- 1 - p
  T*p/(1 - q^T) - comp
}

estN <- function(comp, broke, n) {
  T <- 1/broke
  n / (1 - (1 - uniroot(ecomp, c(0.00001, 1), T=T, comp=comp)$root)^T)
}

nvessscale <- function(params, xlim, ylim, new=TRUE) {
  if (new)
    par(new=TRUE)
  plot(0:1, c(1, params$N), type="n", axes=!new, ann=FALSE,
       xlim=xlim, ylim=ylim)
}

compscale <- function(params, xlim, ylim, new=TRUE) {
  if (new)
    par(new=TRUE)
  plot(0:1, c(1, params$T), type="n", axes=!new, ann=FALSE,
       xlim=xlim, ylim=ylim)
}

lowerCI <- function(p, N, breaks, lb) {
  params <- params(N, breaks, p)
  expmeancomp(params) - 2*sqrt(varmeancomp(params)) - lb
}

upperCI <- function(p, N, breaks, lb) {
  params <- params(N, breaks, p)
  expmeancomp(params) + 2*sqrt(varmeancomp(params)) - lb
}

critP <- function(comp, params) {
  c(uniroot(lowerCI, c(0.00001, 1), N=params$N,
            breaks=1/params$T, lb=max(comp))$root,
    if (upperCI(0.00001, params$N, 1/params$T, min(comp)) > 0) 0
    else uniroot(upperCI, c(0.00001, 1), N=params$N,
                 breaks=1/params$T, lb=min(comp))$root)
}

anncomp <- function(params, comp, xlim, ylim, cylim) {
  cp <- critP(comp, params)
  nv <- numvess(params(params$N, 1/params$T, cp))
  nvessscale(params, xlim, ylim)
  polygon(c(cp[2], cp[2], 0, 0, cp[1], cp[1]),
          c(0, nv[2], nv[2], nv[1], nv[1], 0),
          col="gray90", border=NA)
  text(0, nv[1], paste(round(nv[1]),
                       " (", round(100*nv[1]/params$N), "%)", sep=""),
       adj=c(0, 0), col="gray")
  text(0, nv[2], paste(round(nv[2]), 
                       " (", round(100*nv[2]/params$N), "%)", sep=""),
       adj=c(0, 1), col="gray")
  compscale(params, xlim, cylim)
  segments(1, min(comp), cp[2], comp, col="gray")
  segments(1, max(comp), cp[1], comp, col="gray")
  text(1, comp, paste(comp, collapse="-"), adj=c(1, 0), col="gray")
}

plotPars <- function(params, comp, xlim=NULL, ylim=NULL) {
  mean <- expmeancomp(params)
  var <- 2*sqrt(varmeancomp(params))
  lb <- mean - var
  ub <- mean + var
  par(mar=c(5, 4, 4, 4))
  if (is.null(ylim))
    cylim <- ylim
  else
    cylim <- c(1 + ((ylim[1] - 1)/(params$N - 1))*(params$T - 1),
               1 + ((ylim[2] - 1)/(params$N - 1))*(params$T - 1))
  nvessscale(params, xlim, ylim, new=FALSE)
  compscale(params, xlim, cylim)
  polygon(c(params$p, rev(params$p)), c(lb, rev(ub)),
          col="gray90", border=NA)
  anncomp(params, comp, xlim, ylim, cylim)
  nvessscale(params, xlim, ylim)
  mtext("Number of Vessels", side=2, line=3)
  mtext("Sampling Fraction", side=1, line=3)
  lines(params$p, numvess(params))
  par(new=TRUE)
  compscale(params, xlim, cylim)
  mtext("Completeness", side=4, line=3)
  axis(4)
  lines(params$p, mean, lty="dashed")
  lines(params$p, lb, lty="dotted")
  lines(params$p, ub, lty="dotted")
  mtext(paste("N = ", round(params$N),
              "     brokenness = ", round(1/params$T, 3), sep=""),
        side=3, line=2)
}

par(cex=0.8, mar=c(3, 3, 3, 3))
p6 <- params(estN(1.2, 0.5, 200), 0.5)
plotPars(p6, 1.2)
nvessscale(p6, NULL, NULL)
pcrit <- 1 - (1 - 200/estN(1.2, 0.5, 200))^(1/p6$T)
lines(c(0, pcrit), c(200, 200))
lines(c(pcrit, pcrit), c(200, 0))



}
figure1.4 <- function() {
# Produce a plot of tiger populations with picture as background
# Source: http://www.globaltiger.org/population.htm
year <- c(1993, 1996, 1998, 2001)
minpop <- c(20, 50, 50, 115)
maxpop <- c(50, 240, 240, 150)

PostScriptTrace(system.file("extra", "tiger.ps", 
                            package="RGraphics"))
tiger <- grImport::readPicture("tiger.ps.xml")[-1]


source(system.file("extra", "grayify.R", package="RGraphics"))

# grid.newpage()
pushViewport(plotViewport(c(3, 2, 2, 1)),
             viewport(xscale=c(1991, 2003), yscale=c(0, 250)))
grid.rect()
# tiger backdrop in gray
grImport::grid.picture(tiger, x=0.45, FUN=grayify, min=.8)
grid.xaxis(at=year, gp=gpar(cex=0.7))
grid.yaxis(gp=gpar(cex=0.7))
# black bars
grid.rect(x=unit(year, "native"), y=0,
          width=unit(1, "native"), height=unit(maxpop, "native"),
          just="bottom", gp=gpar(fill="black"))
# tiger in bars
tigerGrob <- grImport::pictureGrob(tiger, x=0.45, 
 FUN=grImport::grobify)
# Start from 2 because bar 1 does not overlap with tiger
for (i in 2:length(year)) {
    grid.clip(x=unit(year[i], "native"), y=0,
              width=unit(1, "native"), height=unit(maxpop[i], "native"),
              just="bottom")
    # tiger backdrop (shift slightly to left so get one eye in one bar)
    grid.draw(tigerGrob)
}
grid.clip()
# redo bar borders
grid.rect(x=unit(year, "native"), y=0,
          width=unit(1, "native"), height=unit(maxpop, "native"),
          just="bottom", gp=gpar(fill=NA))
grid.text("Estimated Population (max.) of Bengal Tigers\n(in Bhutan)",
          y=unit(1, "npc") + unit(1, "lines"))
popViewport(2)



}
figure1.5 <- function() {

#
# Comment:
#
# A slightly modified version of Figure 1.1 from 
# Cleveland's book "Visualizing Data"
#


trellis.par.set(list(fontsize=list(text=6),
	             par.xlab.text=list(cex=1.5),
                     add.text=list(cex=1.5),
                     superpose.symbol=list(cex=.5)))
key <- simpleKey(levels(lattice::barley$year), space = "right")
key$text$cex <- 1.5
print(
     dotplot(variety ~ yield | site, data = lattice::barley, groups = year,
             key = key,
             xlab = "Barley Yield (bushels/acre) ",
             aspect=0.5, layout = c(1,6), ylab=NULL)
)



}
figure1.6 <- function() {

#
# Comment:
#
# Inspired by Figure 3.3 from 
# Wickham's book "ggplot2"
#

print(
ggplot(data=ggplot2::mpg, aes(x=displ, y=hwy, shape=factor(cyl))) + 
    geom_point() +
    stat_smooth(method="lm", colour="black") +
    scale_shape_manual(values=c(1, 16, 3, 17)) + 
    theme_bw() 
)



}
figure1.7 <- function() {

#
# Comment:
#
# map positioned correctly;  this provides an example of calling a 
# plotting function to perform calculations but do no drawing (see the
# second call to the map() function).
#
# Makes use of the "maps", "mapdata", and "mapproj" packages to draw the maps.
#

par(mar=rep(1, 4))
maps::map("mapdata::nzHires", fill=TRUE, col="gray80",
    regions=c("North Island", "South Island", "Stewart Island"))
points(174.75, -36.87, pch=16, cex=2,
       col=rgb(0,0,0,.5))
arrows(172, -36.87, 174, -36.87, lwd=3)
text(172, -36.87, "Auckland  ", adj=1, cex=2)
# mini world map as guide
maplocs <- maps::map(projection="sp_mercator", wrap=TRUE, lwd=0.1, 
               col="gray", ylim=c(-60, 75),
               interior=FALSE, orientation=c(90, 180, 0), add=TRUE,
               plot=FALSE)
xrange <- range(maplocs$x, na.rm=TRUE)
yrange <- range(maplocs$y, na.rm=TRUE)
aspect <- abs(diff(yrange))/abs(diff(xrange))
# customised to 6.5 by 4.5 figure size
par(fig=c(0.99 - 0.5, 0.99, 0.01, 0.01 + 0.5*aspect*4.5/6.5), 
    mar=rep(0, 4), new=TRUE)
plot.new()
plot.window(xlim=xrange,
            ylim=yrange)
maps::map(projection="sp_mercator", wrap=TRUE, lwd=0.5, ylim=c(-60, 75),
    interior=FALSE, orientation=c(90, 180, 0), add=TRUE)
symbols(-.13, -0.8, circles=1, inches=0.1, add=TRUE, bg=rgb(0,0,0,.2))
box()


}
figure1.8 <- function() {
notrun <- function() {
    # 'AABA' distributed with 'RGraphics' package 
    quantmod::getSymbols("AABA")
    dump("AABA", "AABA.R")
}
quantmod::chartSeries(AABA, subset='2007::2008-01'
 )


}
figure1.9 <- function() {

# CLASSIFICATION
# fitting
data("GlaucomaM", package = "TH.data", envir=environment())
glau <- GlaucomaM
levels(glau$Class) <- c("glau", "norm")
fm.class <- party::ctree(Class ~ ., data = glau)

# visualization
pushViewport(viewport(gp=gpar(cex=0.6)))
plot(fm.class, new=FALSE, terminal.panel=myNode)
popViewport()



}
figure1.10 <- function() {

#
# Comment:
#
# Some simple ideas as a basis for meta-analysis plots.
# 
# The code is modular so that something similar could be achieved
# with different data quite simply.  The actual drawing for these data
# only occurs in the last 10 or so lines of code.
#


# The horizontal gap between columns with content
colgap <- unit(3, "mm")

# The data for column 1
# 
# Of course, many other possible ways to represent the data
# One advantage with this way is that col1$labels can be used
# directly in the calculation of the column widths for the
# main table (see below)
#
# NOTE:  textGrobs are used here so that the fontface (bold in
# some cases) is associated with the label.  In this way, the
# calculation of column widths takes into account the font face.
col1 <- list(labels=
             list(textGrob("Centre", x=0, just="left",
                           gp=gpar(fontface="bold", col="white")),
                  textGrob("Thailand", x=0, just="left"),
                  textGrob("Philippines", x=0, just="left"),
                  textGrob("All in situ", x=0, just="left",
                           gp=gpar(fontface="bold.italic")),
                  textGrob("Colombia", x=0, just="left"),
                  textGrob("Spain", x=0, just="left"),
                  textGrob("All invasive", x=0, just="left",
                           gp=gpar(fontface="bold.italic")),
                  textGrob("All", x=0, just="left",
                           gp=gpar(fontface="bold"))),
             rows=c(1, 5, 6, 8, 11, 12, 14, 16))

# Labels in col 1 which are not used to calculate the
# column width (they spill over into col 2)
col1plus <- list(labels=
                 list(textGrob("Carcinoma in situ", x=0, just="left",
                               gp=gpar(fontface="bold.italic")),
                      textGrob("Invasive cancer", x=0, just="left",
                               gp=gpar(fontface="bold.italic"))),
                 rows=c(4, 10))

# Data for column 2
col2 <- list(labels=
             list(textGrob("Cases", x=1, just="right",
                           gp=gpar(fontface="bold", col="white")),
                  textGrob("327", x=1, just="right"),
                  textGrob("319", x=1, just="right"),
                  textGrob("1462", x=1, just="right",
                           gp=gpar(fontface="bold")),
                  textGrob("96", x=1, just="right"),
                  textGrob("115", x=1, just="right"),
                  textGrob("211", x=1, just="right",
                           gp=gpar(fontface="bold")),
                  textGrob("1673", x=1, just="right",
                           gp=gpar(fontface="bold"))),
             rows=c(1, 5, 6, 8, 11, 12, 14, 16))

# Data for column 3 (width specified as a physical size below)
col3 <- list(OR=c(0.72, 1.27, 1.17, 2.97, 1.86, 2.01, 1.20),
             LL=c(0.52, 0.87, 1.03, 1.42, 0.46, 1.09, 1.07),
             UL=c(1.00, 1.85, 1.32, 6.21, 7.51, 3.71, 1.35),
             rows=c(5, 6, 8, 11, 12, 14, 16),
             # "s" means summary, "n" means normal
             type=c("n", "n", "s", "n", "n", "s", "s"))

# Sizes of boxes
information <- sqrt(1 / ((log(col3$UL) - log(col3$OR))/1.96))
col3$sizes <- information/max(information)

# Width of column 3
col3width <- unit(1.5, "inches")

# Range on the x-axis for column 3
col3$range <- c(0, 4)

# Function to draw a cell in a text column
drawLabelCol <- function(col, j) {
  for (i in 1:length(col$rows)) {
    pushViewport(viewport(layout.pos.row=col$rows[i], layout.pos.col=j))
    # Labels are grobs containing their location so just
    # have to grid.draw() them
    grid.draw(col$labels[[i]])
    popViewport()
  }
}

# Function to draw a non-summary rect-plus-CI
drawNormalCI <- function(LL, OR, UL, size) {
  # NOTE the use of "native" units to position relative to
  # the x-axis scale, and "snpc" units to size relative to
  # the height of the row
  # ("snpc" stands for "square normalised parent coordinates"
  #  which means that the value is calculated as a proportion
  #  of the width and height of the current viewport and the
  #  physically smaller of these is used)
  grid.rect(x=unit(OR, "native"),
            width=unit(size, "snpc"), height=unit(size, "snpc"),
            gp=gpar(fill="black"))
  # Draw arrow if exceed col range
  # convertX() used to convert between coordinate systems
  if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) > 1)
    grid.lines(x=unit(c(LL, 1), c("native", "npc")), y=.5,
               arrow=arrow(length=unit(0.05, "inches")))
  else {
    # Draw line white if totally inside rect
    lineCol <- if ((convertX(unit(OR, "native") + unit(0.5*size, "lines"),
                             "native", valueOnly=TRUE) > UL) &&
                   (convertX(unit(OR, "native") - unit(0.5*size, "lines"),
                             "native", valueOnly=TRUE) < LL))
      "white"
    else
      "black"
    grid.lines(x=unit(c(LL, UL), "native"), y=0.5,
               gp=gpar(col=lineCol))
  }
}

# Function to draw a summary "diamond"
drawSummaryCI <- function(LL, OR, UL, size) {
  # Not sure how to calc the heights of the diamonds so
  # I'm just using half the height of the equivalent rect
  grid.polygon(x=unit(c(LL, OR, UL, OR), "native"),
               y=unit(0.5 + c(0, 0.25*size, 0, -0.25*size), "npc"))
}

# Function to draw a "data" column
drawDataCol <- function(col, j) {
  pushViewport(viewport(layout.pos.col=j, xscale=col$range))
  grid.lines(x=unit(1, "native"), y=0:1)
  # Assume that last value in col is "All"
  grid.lines(x=unit(col$OR[length(col$OR)], "native"),
             y=0:1, gp=gpar(lty="dashed"))
  grid.xaxis(gp=gpar(cex=0.6))
  grid.text("OR", y=unit(-2, "lines"))
  popViewport()
  for (i in 1:length(col$rows)) {
    pushViewport(viewport(layout.pos.row=col$rows[i], layout.pos.col=j,
                          xscale=col$range))
    if (col$type[i] == "n")
      drawNormalCI(col$LL[i], col$OR[i], col$UL[i], col$sizes[i])
    else
      drawSummaryCI(col$LL[i], col$OR[i], col$UL[i], col$sizes[i])
    popViewport()
  }
}

# Draw the table
#
# The table is just a big layout
#
# All rows are the height of 1 line of text
# 
# Widths of column 1 and 2 are based on widths of labels in
# col$labels and col2$labels 
pushViewport(viewport(layout=grid.layout(16, 5,
                        widths=
                        unit.c(max(unit(rep(1, 8), "grobwidth", col1$labels)),
                               colgap,
                               max(unit(rep(1, 8), "grobwidth", col2$labels)),
                               colgap,
                               col3width),
                        heights=unit(c(1, 0, rep(1, 14)), "lines"))))
pushViewport(viewport(layout.pos.row=1))
grid.rect(gp=gpar(col=NA, fill="black"))
popViewport()
for (i in c(8, 14, 16)) {
    pushViewport(viewport(layout.pos.row=i))
    grid.rect(gp=gpar(col=NA, fill="gray80"))
    popViewport()
}
drawLabelCol(col1, 1)
drawLabelCol(col1plus, 1)
drawLabelCol(col2, 3)
drawDataCol(col3, 5)
popViewport()
                          



}
figure1.11 <- function() {

#
# Comment:
#
# Code by Arden Miller (Department of Statistics, The University of Auckland).
# 
# Lots of coordinate transformations being done "by hand".
# This code is not really reusable;  just a demonstration that very 
# pretty results are possible if you're sufficiently keen.
#


par(mfrow=c(2, 1), pty="s", mar=rep(1, 4)) 
# Create plotting region and plot outer circle
plot(c(-1.1, 1.2), c(-1.1, 1.2),
     type="n", xlab="", ylab="", 
     xaxt="n", yaxt="n", cex.lab=2.5)
angs <- seq(0, 2*pi, length=500)
XX <- sin(angs)
YY <- cos(angs)
lines(XX, YY, type="l")

# Set constants
phi1 <- pi*2/9
k1 <- sin(phi1)
k2 <- cos(phi1)

# Create gray regions
obsphi <- pi/12
lambdas <- seq(-pi, pi, length=500)
xx <- cos(pi/2 - obsphi)*sin(lambdas)
yy <- k2*sin(pi/2 - obsphi)-k1 * cos(pi/2 - obsphi)*cos(lambdas)
polygon(xx, yy, col="gray")
lines(xx, yy, lwd=2)
theta1sA <- seq(-obsphi, obsphi, length=500)
theta2sA <- acos(cos(obsphi)/cos(theta1sA))
theta1sB <- seq(obsphi, -obsphi, length=500)
theta2sB <-  -acos(cos(obsphi)/cos(theta1sB))
theta1s <- c(theta1sA, theta1sB)
theta2s <- c(theta2sA, theta2sB)
xx <- cos(theta1s)*sin(theta2s+pi/4)
yy <- k2*sin(theta1s)-k1*cos(theta1s)*cos(theta2s+pi/4)
polygon(xx, yy, col="gray")
lines(xx, yy, lwd=2)
xx <- cos(theta1s)*sin(theta2s-pi/4)
yy <- k2*sin(theta1s)-k1*cos(theta1s)*cos(theta2s-pi/4)
polygon(xx, yy, col="gray")
lines(xx, yy, lwd=2)

# Plot longitudes
vals <- seq(0, 7, 1)*pi/8
for(lambda in vals){
sl <- sin(lambda)
cl <- cos(lambda)
phi <- atan(((0-1)*k2*cl)/(k1))
angs <- seq(phi, pi+phi, length=500)
xx <- cos(angs)*sl
yy <- k2*sin(angs)-k1*cos(angs)*cl
lines(xx, yy, lwd=.5)
}

# Grey out polar cap
phi <- 5.6*pi/12
lambdas <- seq(-pi, pi, length=500)
xx <- cos(phi)*sin(lambdas)
yy <- k2*sin(phi)-k1 * cos(phi)*cos(lambdas)
polygon(xx, yy, col="gray")

# Plot Latitudes
vals2 <- seq(-2.8, 5.6, 1.4)*pi/12
for(phi in vals2){
  if (k1*sin(phi) > k2 * cos(phi)) 
    crit <- pi 
  else 
    crit <- acos((-k1*sin(phi))/(k2*cos(phi)))
  lambdas <- seq(-crit, crit, length=500)
  xx <- cos(phi)*sin(lambdas)
  yy <- k2*sin(phi)-k1 * cos(phi)*cos(lambdas)
  lines(xx, yy, lwd=.5)
}


# Plots axes and label
lines(c(0.00, 0.00), c(k2*sin(pi/2), 1.11), lwd=4)
lines(c(0.00, 0.00), c(-1, -1.12), lwd=4)
a2x <- sin(-pi/4)
a2y <- cos(-pi/4)*(-k1)
lines(c(a2x, 1.5*a2x), c(a2y, 1.5*a2y), lwd=4)
k <- sqrt(a2x^2+a2y^2)
lines(c(-a2x/k, 1.2*(-a2x/k)), c(-a2y/k, 1.2*(-a2y/k)), lwd=4)
a3x <- sin(pi/4)
a3y <- cos(pi/4)*(-k1)
lines(c(a3x, 1.5*a3x), c(a3y, 1.5*a3y), lwd=4)
k <- sqrt(a3x^2+a3y^2)
lines(c(-a3x/k, 1.2*(-a3x/k)), c(-a3y/k, 1.2*(-a3y/k)), lwd=4)
text(0.1, 1.12, expression(bold(X[1])))
text(-1.07, -.85, expression(bold(X[2])))
text(1.11, -.85, expression(bold(X[3])))

# set plot region and draw outer circle
plot(c(-1.1, 1.2),  c(-1.1, 1.2),
     type="n", xlab="", ylab="", 
     xaxt="n", yaxt="n", cex.lab=2.5)
angs <- seq(0, 2*pi, length=500)
XX <- sin(angs)
YY <- cos(angs)
lines(XX, YY, type="l")

# set constants
phi1 <- pi*2/9
k1 <- sin(phi1)
k2 <- cos(phi1)
obsphi <- pi/24

# create X2X3 gray region and plot boundary
crit <- acos((-k1*sin(obsphi))/(k2 * cos(obsphi)))
lambdas <- seq(-crit, crit, length=500)
xx1 <- cos(obsphi)*sin(lambdas)
yy1 <- k2*sin(obsphi)-k1 * cos(obsphi)*cos(lambdas)
obsphi <-  -pi/24
crit <- acos((-k1*sin(obsphi))/(k2 * cos(obsphi)))
lambdas <- seq(crit, -crit, length=500)
xx3 <- cos(obsphi)*sin(lambdas)
yy3 <- k2*sin(obsphi)-k1 * cos(obsphi)*cos(lambdas)
ang1 <-  atan(xx1[500]/yy1[500])
ang2 <- pi+atan(xx3[1]/yy3[1])
angs <- seq(ang1, ang2, length=50)
xx2 <- sin(angs)
yy2 <- cos(angs)
ang4 <-  atan(xx1[1]/yy1[1])
ang3 <-  -pi+ atan(xx3[500]/yy3[500])
angs <- seq(ang3, ang4, length=50)
xx4 <- sin(angs)
yy4 <- cos(angs)
xxA <- c(xx1, xx2, xx3, xx4)
yyA <- c(yy1, yy2, yy3, yy4)
polygon(xxA, yyA, border="gray", col="gray")
xx1A <- xx1
yy1A <- yy1
xx3A <- xx3
yy3A <- yy3

# create X1X3 gray region and plot boundary
obsphi <- pi/24
crit <- pi/2-obsphi
theta1sA <- c(seq(-crit, crit/2, length=200), seq(crit/2, crit, length=500))
theta2sA <- asin(cos(crit)/cos(theta1sA))
theta1sB <- seq(crit, crit/2, length=500)
theta2sB <-  pi-asin(cos(crit)/cos(theta1sB))
theta1s <- c(theta1sA, theta1sB)
theta2s <- c(theta2sA, theta2sB)
vals <- k1*sin(theta1s)+k2*cos(theta1s)*cos(theta2s+pi/4)
xx1 <- cos(theta1s[vals>=0])*sin(theta2s[vals>=0]+pi/4)
yy1 <- k2*sin(theta1s[vals>=0])-k1*cos(theta1s[vals>=0])*cos(theta2s[vals>=0]+pi/4)
theta2s <-  -theta2s
vals <- k1*sin(theta1s)+k2*cos(theta1s)*cos(theta2s+pi/4)
xx3 <- cos(theta1s[vals>=0])*sin(theta2s[vals>=0]+pi/4)
yy3 <- k2*sin(theta1s[vals>=0])-k1*cos(theta1s[vals>=0])*cos(theta2s[vals>=0]+pi/4)
rev <- seq(length(xx3), 1, -1)
xx3 <- xx3[rev]
yy3 <- yy3[rev]
ang1 <-  pi+atan(xx1[length(xx1)]/yy1[length(yy1)])
ang2 <-  pi+atan(xx3[1]/yy3[1])
angs <- seq(ang1, ang2, length=50)
xx2 <- sin(angs)
yy2 <- cos(angs)
ang4 <-  pi+atan(xx1[1]/yy1[1])
ang3 <-  pi+atan(xx3[length(xx3)]/yy3[length(yy3)])
angs <- seq(ang3, ang4, length=50)
xx4 <- sin(angs)
yy4 <- cos(angs)
xxB <- c(xx1, -xx2, xx3, xx4)
yyB <- c(yy1, -yy2, yy3, yy4)
polygon(xxB, yyB, border="gray", col="gray")
xx1B <- xx1
yy1B <- yy1
xx3B <- xx3
yy3B <- yy3

# create X1X2 gray region and plot boundary
vals <- k1*sin(theta1s)+k2*cos(theta1s)*cos(theta2s-pi/4)
xx1 <- cos(theta1s[vals>=0])*sin(theta2s[vals>=0]-pi/4)
yy1 <- k2*sin(theta1s[vals>=0])-k1*cos(theta1s[vals>=0])*cos(theta2s[vals>=0]-pi/4)
theta2s <-  -theta2s
vals <- k1*sin(theta1s)+k2*cos(theta1s)*cos(theta2s-pi/4)
xx3 <- cos(theta1s[vals>=0])*sin(theta2s[vals>=0]-pi/4)
yy3 <- k2*sin(theta1s[vals>=0])-k1*cos(theta1s[vals>=0])*cos(theta2s[vals>=0]-pi/4)
rev <- seq(length(xx3), 1, -1)
xx3 <- xx3[rev]
yy3 <- yy3[rev]
ang1 <-  pi+atan(xx1[length(xx1)]/yy1[length(yy1)])
ang2 <-  pi+atan(xx3[1]/yy3[1])
angs <- seq(ang1, ang2, length=50)
xx2 <- sin(angs)
yy2 <- cos(angs)
ang4 <-  pi+atan(xx1[1]/yy1[1])
ang3 <-  pi+atan(xx3[length(xx3)]/yy3[length(yy3)])
angs <- seq(ang3, ang4, length=50)
xx4 <- sin(angs)
yy4 <- cos(angs)
xx <- c(xx1, -xx2, xx3, xx4)
yy <- c(yy1, -yy2, yy3, yy4)
polygon(xx, yy, border="gray", col="gray")
xx1C <- xx1
yy1C <- yy1
xx3C <- xx3
yy3C <- yy3


# plot boundaries to gray regions
lines(xx1C[2:45], yy1C[2:45], lwd=2)
lines(xx1C[69:583], yy1C[69:583], lwd=2)
lines(xx1C[660:1080], yy1C[660:1080], lwd=2)
lines(xx3C[13:455], yy3C[13:455], lwd=2)
lines(xx3C[538:1055], yy3C[538:1055], lwd=2)
lines(xx3C[1079:1135], yy3C[1079:1135], lwd=2)
lines(xx1A[6:113], yy1A[6:113], lwd=2)
lines(xx1A[153:346], yy1A[153:346], lwd=2)
lines(xx1A[389:484], yy1A[389:484], lwd=2)
lines(xx3A[1:93], yy3A[1:93], lwd=2)
lines(xx3A[140:362], yy3A[140:362], lwd=2)
lines(xx3A[408:497], yy3A[408:497], lwd=2)
lines(xx1B[2:45], yy1B[2:45], lwd=2)
lines(xx1B[69:583], yy1B[69:583], lwd=2)
lines(xx1B[660:1080], yy1B[660:1080], lwd=2)
lines(xx3B[13:455], yy3B[13:455], lwd=2)
lines(xx3B[538:1055], yy3B[538:1055], lwd=2)
lines(xx3B[1079:1135], yy3B[1079:1135], lwd=2)

# Plot longitudes
vals <- seq(-7, 8, 1)*pi/8
for(lambda in vals){
  sl <- sin(lambda)
  cl <- cos(lambda)
  phi <- atan(((0-1)*k2*cl)/(k1))
  angs <- seq(phi, 5.6*pi/12, length=500)
  xx <- cos(angs)*sl
  yy <- k2*sin(angs)-k1*cos(angs)*cl
  lines(xx, yy, lwd=.5)
}


# Plot Latitudes
# vals2 <- seq(-2.8, 5.6, 1.4)*pi/12
vals2 <- c(-1.5, 0, 1.5, 3.0, 4.5, 5.6)*pi/12
for(phi in vals2){
  if (k1*sin(phi) > k2 * cos(phi)) 
    crit <- pi 
  else 
    crit <- acos((-k1*sin(phi))/(k2*cos(phi)))
  lambdas <- seq(-crit, crit, length=500)
  xx <- cos(phi)*sin(lambdas)
  yy <- k2*sin(phi)-k1 * cos(phi)*cos(lambdas)
  lines(xx, yy, lwd=.5)
}


# create lines for X1X2- and X1X3-planes
lambda <- pi/4
sl <- sin(lambda)
cl <- cos(lambda)
phi <- atan(((0-1)*k2*cl)/(k1))
angs <- seq(phi, pi+phi, length=500)
xx <- cos(angs)*sl
yy <- k2*sin(angs)-k1*cos(angs)*cl
lines(xx, yy, lwd=2)
lambda <- 3*pi/4
sl <- sin(lambda)
cl <- cos(lambda)
phi <- atan(((0-1)*k2*cl)/(k1))
angs <- seq(phi, pi+phi, length=500)
xx <- cos(angs)*sl
yy <- k2*sin(angs)-k1*cos(angs)*cl
lines(xx, yy, lwd=2)

# create line for X2X3-plane
phi <- 0
crit <- acos((-k1*sin(phi))/(k2 * cos(phi)))
lambdas <- seq(-crit, crit, length=500)
xx <- cos(phi)*sin(lambdas)
yy <- k2*sin(phi)-k1 * cos(phi)*cos(lambdas)
lines(xx, yy, lwd=2)

# create axes
lines(c(0.00, 0.00), c(k2*sin(pi/2), 1.11), lwd=4)
lines(c(0.00, 0.00), c(-1, -1.12), lwd=4)
a2x <- sin(-pi/4)
a2y <- cos(-pi/4)*(-k1)
lines(c(a2x, 1.5*a2x), c(a2y, 1.5*a2y), lwd=4)
a3x <- sin(pi/4)
a3y <- cos(pi/4)*(-k1)
lines(c(a3x, 1.5*a3x), c(a3y, 1.5*a3y), lwd=4)
k <- sqrt(a3x^2+a3y^2)
lines(c(-a3x/k, 1.2*(-a3x/k)), c(-a3y/k, 1.2*(-a3y/k)), lwd=4)
k <- sqrt(a2x^2+a2y^2)
lines(c(-a2x/k, 1.2*(-a2x/k)), c(-a2y/k, 1.2*(-a2y/k)), lwd=4)


# add text
text(-1.07, -.85, expression(bold(X[2])))
text(1.11, -.85, expression(bold(X[3])))
text(0.1, 1.12, expression(bold(X[1])))

lines(XX, YY, type="l")




}
figure1.12 <- function() {
 uoaBlue <- "#00467f"
uoaLCH <- colorspace::coords(as(colorspace::hex2RGB(uoaBlue), "polarLUV")) 
 hues <- c(uoaLCH[3], uoaLCH[3] + 90, uoaLCH[3] + 180, uoaLCH[3] + 270)
darks <- hcl(hues, uoaLCH[2], uoaLCH[1])
lights <- hcl(hues, uoaLCH[2], 80)
arr <- arrow(length=unit(2, "mm"), type="closed")
document <- function(label, x=.5, y=.5,
                     w=unit(2, "cm"), h=unit(3, "cm"), d=unit(6, "mm"),
                     ann=TRUE, border=NA, fill=NULL, name="script") {
    if (is.null(fill)) {
        fill=lights[1]
    }
    pushViewport(viewport(x, y, w, h, name="scriptvp"))
    grid.polygon(unit.c(unit(0, "npc"),
                        unit(0, "npc"),
                        unit(1, "npc"),
                        unit(1, "npc"),
                        d,
                        d,
                        unit(0, "npc")),
                 unit.c(unit(1, "npc") - d,
                        unit(0, "npc"),
                        unit(0, "npc"),
                        unit(1, "npc"),
                        unit(1, "npc"),
                        unit(1, "npc") - d,
                        unit(1, "npc") - d),
                 gp=gpar(col=border, fill=fill),
                 name=name)
    grid.polygon(unit.c(unit(0, "npc"),
                        d,
                        d),
                 unit.c(unit(1, "npc") - d,
                        unit(1, "npc") - d,
                        unit(1, "npc")),
                 gp=gpar(col=NA, fill=darks[1]))
    grid.text(label)
    if (ann) {
        grid.rect(y=0, height=unit(2, "lines"), just="top",
                  gp=gpar(col=NA, fill=darks[1]))
        grid.segments(0, 0, 1, 0, gp=gpar(col="white", lwd=2))
        grid.text("SCRIPT", gp=gpar(col="white"),
                  y=unit(-1, "lines"))
    }
    upViewport()
}
module <- function(label, nin, nout,
                   x=.5, y=.5, w=unit(3, "cm"), h=unit(4, "cm"),
                   ann=TRUE, border=NA,
                   labelin=TRUE, labelout=TRUE, inoutext=.2,
                   arrowsin=TRUE, arrowsout=TRUE) {
    pushViewport(viewport(x, y, w, h, name="modulevp"))
    grid.rect(gp=gpar(col=border, 
 fill=lights[2]))
    document(label, ann=FALSE, border=darks[1], name=label)
    if (arrowsin) {
        arrin <- arr
    } else {
        arrin <- NULL
    }
    if (arrowsout) {
        arrout <- arr
    } else {
        arrout <- NULL
    }
    if (nin > 0) {
        grid.segments(-inoutext, 1:nin/(nin+1),
                      grobX(rectGrob(vp="scriptvp"), 180), 1:nin/(nin+1),
                      gp=gpar(lwd=3, col="black"), arrow=arrin)
        if (is.character(labelin)) {
            lab <- labelin
            fam <- "mono"
        } else if (labelin) {
            lab <- "IN"
            fam <- "sans"
        } else {
            lab <- NULL
        }
        if (!is.null(lab)) {
            grid.text(lab, unit(-inoutext, "npc") - unit(2, "mm"),
                      1:nin/(nin+1),
                      just="right", gp=gpar(cex=.7, fontfamily=fam))
        }
    }
    if (nout > 0) {
        grid.segments(grobX(rectGrob(vp="scriptvp"), 0),
                      1:nout/(nout+1), 1 + inoutext, 1:nout/(nout+1),
                      gp=gpar(lwd=3, col="black"), arrow=arrout)
        if (is.character(labelout)) {
            lab <- labelout
            fam <- "mono"
        } else if (labelout) {
            lab <- "OUT"
            fam <- "sans"
        } else {
            lab <- NULL
        }
        if (!is.null(lab)) {
            grid.text(lab, unit(1 + inoutext, "npc") + unit(2, "mm"),
                      1:nout/(nout+1),
                      just="left", gp=gpar(cex=.7, fontfamily=fam))
        }
    }
    if (ann) {
        grid.rect(y=0, height=unit(2, "lines"), just="top",
                  gp=gpar(col=NA, fill=darks[2]))
        grid.segments(0, 0, 1, 0, gp=gpar(col="white", lwd=2))
        grid.text("MODULE", gp=gpar(col="white"),
                  y=unit(0, "npc") - unit(1, "lines"))
    }
    upViewport()
}
pipeline <- function(x=.5, y=.5, width=unit(10, "cm"), height=unit(6, "cm"),
                     modules=TRUE) {
    pushViewport(viewport(x, y, width=width, height=height,
                          name="pipelinevp"))
    grid.rect(gp=gpar(col=NA, fill=lights[4]))
    if (modules) {
        module("R", 1, 2, x=1/4, labelout=FALSE, inoutext=.5, arrowsout=FALSE,
               ann=FALSE, border=darks[2])
        module("Python", 2, 1, x=3/4, labelin=FALSE, inoutext=.5,
               ann=FALSE, border=darks[2])
    }
    grid.rect(y=0, height=unit(2, "lines"), just="top",
#               gp=gpar(col=NA, fill=hcl(hue, sat, 40, trans)))
              gp=gpar(col=NA, fill=darks[4]))
    grid.segments(0, 0, 1, 0, gp=gpar(col="white", lwd=2))
    grid.text("PIPELINE", gp=gpar(col="white"),
              y=unit(0, "npc") - unit(1, "lines"))
    upViewport()
}
# grid.newpage()
pipeline(modules=FALSE, width=unit(10, "cm"), height=unit(10, "cm"))
downViewport("pipelinevp")
# Module 1
module("R", 0, 0, x=.2, labelin=FALSE, labelout=FALSE,
       ann=FALSE, border=darks[2])
downViewport("scriptvp")
# inputs
document(".csv", x=-.95, y=.4,
         w=unit(1, "cm"), h=unit(1.5, "cm"), d=unit(3, "mm"),
         ann=FALSE, border="black", fill="grey80")
grid.segments(-.55, .4, 0, .4, arrow=arr, gp=gpar(lwd=3))
# outputs
grid.curve(1, .7, 1.6, .9, arrow=arr, gp=gpar(lwd=3))
grid.rect(x=1.6, y=1.05, width=unit(1, "cm"), height=unit(1, "lines"),
          gp=gpar(fill="black"))
grid.text("01011", x=1.6, y=1.05,
          gp=gpar(cex=.7, fontfamily="mono", col="white"))
upViewport(2) # script
# MARK
memtopx <- convertX(grobX(nullGrob(x=1.6, y=1.2, vp=vpPath("modulevp", "scriptvp")), 0),
                    "in")
memtopy <- convertY(grobY(nullGrob(x=1.6, y=1.2, vp=vpPath("modulevp", "scriptvp")), 0),
                    "in")
csvbotx <- convertX(grobX(nullGrob(x=1.9, y=-.5, vp=vpPath("modulevp", "scriptvp")), 0),
                    "in")
csvboty <- convertY(grobY(nullGrob(x=1.9, y=-.5, vp=vpPath("modulevp", "scriptvp")), 0),
                    "in")
downViewport("scriptvp")
grid.curve(1, .3, 1.9, .1, curv=-1, arrow=arr, gp=gpar(lwd=3))
document(".csv", x=1.9, y=-.2,
         w=unit(1, "cm"), h=unit(1.5, "cm"), d=unit(3, "mm"),
         ann=FALSE, border="black", fill="grey80")
upViewport(2) # script
# Module 2
module("R", 0, 0, x=.65, y=.75, labelin=FALSE, labelout=FALSE,
       ann=FALSE, border=darks[2])
grid.curve(memtopx, memtopy,
           grobX(rectGrob(vp=vpPath("modulevp", "scriptvp")), 180),
           grobY(rectGrob(vp=vpPath("modulevp", "scriptvp")), 180),
           curv=-1, arrow=arr, gp=gpar(lwd=3))
downViewport("scriptvp")
grid.curve(1, .7, 1.75, .5, curv=-1, arrow=arr, gp=gpar(lwd=3))
document(".xml", x=1.75, y=.2,
         w=unit(1, "cm"), h=unit(1.5, "cm"), d=unit(3, "mm"),
         ann=FALSE, border="black", fill="grey80")
grid.curve(1.75, -.1, .2, -.5, curv=-1, inflect=TRUE, gp=gpar(lwd=3))
upViewport(2) # script
# MARK
curvex <- convertX(grobX(nullGrob(x=.2, y=-.5, vp=vpPath("modulevp", "scriptvp")), 0),
                    "in")
curvey <- convertY(grobY(nullGrob(x=.2, y=-.5, vp=vpPath("modulevp", "scriptvp")), 0),
                    "in")
# Module 3
module("Python", 0, 0, x=.8, y=.25, labelin=FALSE, labelout=FALSE,
       ann=FALSE, border=darks[2])
grid.curve(csvbotx, csvboty,
           grobX(rectGrob(vp=vpPath("modulevp", "scriptvp")), 220),
           grobY(rectGrob(vp=vpPath("modulevp", "scriptvp")), 220),
           curv=1, arrow=arr, gp=gpar(lwd=3))
grid.curve(curvex, curvey,
           grobX(rectGrob(vp=vpPath("modulevp", "scriptvp")), 160),
           grobY(rectGrob(vp=vpPath("modulevp", "scriptvp")), 160),
           curv=1, arrow=arr, gp=gpar(lwd=3))
downViewport("scriptvp")
grid.segments(1, .4, 1.55, .4, arrow=arr, gp=gpar(lwd=3))
document(".pdf", x=1.95, y=.4,
         w=unit(1, "cm"), h=unit(1.5, "cm"), d=unit(3, "mm"),
         ann=FALSE, border="black", fill="grey80")
upViewport(2) # script
upViewport() # pipeline


}
figure1.13 <- function() {
pic <- jpeg::readJPEG(system.file("extra", "AfterTheBombs.jpg", 
                            package="RGraphics"))

w <- 1024 # 578
h <- 768 # 500
bg <- pic # [1:h, (1024 - w):1024]

unknown <- 8.7
total <- 9.1
known <- total - unknown

theta0 <- pi/4
thetaN <- theta0 + 2*pi*unknown/total
theta <- seq(theta0, thetaN, length.out=100)
x <- 0.3*c(0, cos(theta)) + 0.5
y <- 0.3*c(0, sin(theta)) + 0.35

# grid.newpage()
grid.raster(bg)
pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
                      gp=gpar(cex=1.2)))
grid.polygon(x, y, gp=gpar(col=NA, fill=rgb(.67, 0, .11, .7)))
label1 <- textGrob("UNACCOUNTED\nFOR",
                   unit(.2, "npc") - unit(2, "mm"),
                   unit(.6, "npc") + unit(2, "mm"),
                   gp=gpar(cex=1.4, fontface="bold"),
                   just=c("right", "bottom"))
grid.rect(.2, .6, just=c("right", "bottom"),
          width=grobWidth(label1) + unit(4, "mm"),
          height=grobHeight(label1) + unit(4, "mm"),
          gp=gpar(col=NA, fill=rgb(1, 1, 1, .5)))
grid.draw(label1)
label2 <- textGrob("ACCOUNTED\nFOR", 
                   unit(.8, "npc") + unit(2, "mm"),
                   unit(.6, "npc") + unit(2, "mm"),
                   gp=gpar(cex=1.4, fontface="bold"),
                   just=c("left", "bottom"))
grid.rect(.8, .6, just=c("left", "bottom"),
          width=grobWidth(label2) + unit(4, "mm"),
          height=grobHeight(label2) + unit(4, "mm"),
          gp=gpar(col=NA, fill=rgb(1, 1, 1, .5)))
grid.draw(label2)
grid.segments(c(.2, .8), .6,
              c(.3, .7), .5,
              gp=gpar(lty="dotted", lwd=2))
heading <- textGrob("The Department of Defense is unable to account for the use of
$8.7 billion of the $9.1 billion it spent on reconstruction in Iraq",
                    x=unit(0.5, "cm"),
                    y=unit(3, "lines"),
                    just=c("left", "top"),
                    gp=gpar(cex=1, col="white"))
pushViewport(viewport(x=0, y=1,
                      just=c("left", "top"),
                      height=grobHeight(heading) + unit(4, "lines"),
                      width=grobWidth(heading) + unit(1, "cm")))
grid.rect(gp=gpar(fill="black"))
grid.segments(x0=unit(0.5, "cm"),
              x1=unit(1, "npc") - unit(0.5, "cm"),
              y0=unit(1, "npc") - unit(2, "lines"),
              y1=unit(1, "npc") - unit(2, "lines"),
              gp=gpar(col="grey50", lwd=2))
grid.text("That's 96 Percent",
          x=unit(0.5, "cm"),
          y=unit(1, "npc") - unit(1, "lines"),
          just="left",
          gp=gpar(fontface="bold", col="white"))
grid.draw(heading)
popViewport(2)



}
figure13.1 <- function() {



print(
bwplot(voice.part ~ height, data=lattice::singer, 
       xlab="Height (inches)",
       par.settings=list(box.rectangle=list(col="black"), 
                         box.umbrella=list(col="black"), 
                         plot.symbol=list(col="black")))
)
# grid.ls()
# Looks like boxes are all called <something>bwplot.box.polygon<something>
# Create linear gradient
fill <- linearGradient(c("black", "white"),
                       y0=.5, y1=.5,
                       gradientUnits="coords")
# Register gradient now so it applies to the whole page
registerGradientFill("br", fill)
# Fill each box with gradient
grid.gradientFill("bwplot.box.polygon", label=rep("br", 17), grep=TRUE,
                  group=FALSE)


}
figure13.2 <- function() {

bwplot(voice.part ~ height, data=lattice::singer, 
       xlab="Height (inches)",
       par.settings=list(box.rectangle=list(col="black"), 
                         box.umbrella=list(col="black"), 
                         plot.symbol=list(col="black")))
grid.export()


}
figure13.3 <- function() {
gradient <- linearGradient(c("black", "white", "black"), 
                           x0=0, y0=.5, x1=1, y1=.5)


grid.rect(name="r")


gradient <- linearGradient(c("black", "white", "black"), 
                           x0=0, y0=.5, x1=1, y1=.5)


grid.gradientFill("r", gradient)


grid.export()


}
figure13.4 <- function() {
grid.rect(name="r1")
rg1 <- radialGradient(c("white", "black"))
grid.gradientFill("r1", rg1)


grid.rect(name="r2")
rg2 <- radialGradient(c("white", "black", "white"),
                      stops=c(0, .25, 1),
                      fx=.25, fy=.5)
grid.gradientFill("r2", rg2)


# grid.newpage()
pushViewport(viewport(x=1/4, width=.4, height=.8))
grid.rect(name="r1")
rg1 <- radialGradient(c("white", "black"))
grid.gradientFill("r1", rg1)
popViewport()
pushViewport(viewport(x=3/4, width=.4, height=.8))
grid.rect(name="r2")
rg2 <- radialGradient(c("white", "black", "white"),
                      stops=c(0, .25, 1),
                      fx=.25, fy=.5)
grid.gradientFill("r2", rg2)
popViewport()


}
figure13.5 <- function() {
dots <- pattern(circleGrob(r=.3, gp=gpar(fill="black")))


grid.rect(name="r1")
grid.patternFill("r1", dots)


dotgrid <- pattern(circleGrob(r=.3, gp=gpar(fill="black")),
                   x=.5, y=.5, 
                   width=unit(1, "cm"), height=unit(1, "cm"))
grid.rect(name="r2")
grid.patternFill("r2", dotgrid)


# grid.newpage()
pushViewport(viewport(x=1/4, width=.4, height=.8))
grid.rect(name="r1")
grid.patternFill("r1", dots)
popViewport()
pushViewport(viewport(x=3/4, width=.4, height=.8))
dotgrid <- pattern(circleGrob(r=.3, gp=gpar(fill="black")),
                   x=.5, y=.5, 
                   width=unit(1, "cm"), height=unit(1, "cm"))
grid.rect(name="r2")
grid.patternFill("r2", dotgrid)
popViewport()


}
figure13.6 <- function() {
c <- circleGrob(r=.25, gp=gpar(col=NA, fill="white"))
r <- rectGrob(x=c(1, 1, 3, 3)/4, y=c(1, 3, 3, 1)/4,
              width=.3, height=.3, 
              gp=gpar(col=NA, fill="grey"))
p <- pattern(gTree(children=gList(r, c)),
             x=.5, y=.5, 
             width=unit(2, "cm"), height=unit(2, "cm"))
grid.rect(name="r3")
grid.patternFill("r3", p)



cxc <- ggplot(mtcars, aes(x = factor(cyl))) +
       geom_bar(width = 1, colour = "black") +
       coord_polar(theta = "y")
gg <- ggplotGrob(cxc)
p <- pattern(gg, x=.5, y=.5, 
             width=unit(4, "cm"), height=unit(4, "cm"))
grid.rect(name="r4")
grid.patternFill("r4", p)


# grid.newpage()
pushViewport(viewport(x=1/4, width=.4, height=.8))
c <- circleGrob(r=.25, gp=gpar(col=NA, fill="white"))
r <- rectGrob(x=c(1, 1, 3, 3)/4, y=c(1, 3, 3, 1)/4,
              width=.3, height=.3, 
              gp=gpar(col=NA, fill="grey"))
p <- pattern(gTree(children=gList(r, c)),
             x=.5, y=.5, 
             width=unit(2, "cm"), height=unit(2, "cm"))
grid.rect(name="r3")
grid.patternFill("r3", p)
popViewport()
pushViewport(viewport(x=3/4, width=.4, height=.8))

cxc <- ggplot(mtcars, aes(x = factor(cyl))) +
       geom_bar(width = 1, colour = "black") +
       coord_polar(theta = "y")
gg <- ggplotGrob(cxc)
p <- pattern(gg, x=.5, y=.5, 
             width=unit(4, "cm"), height=unit(4, "cm"))
grid.rect(name="r4")
grid.patternFill("r4", p)
popViewport()


}
figure13.7 <- function() {
feSimple <- filterEffect(feGaussianBlur(sd=3))
grid.rect(name="r1", width=.8, height=.8)
grid.filter("r1", feSimple)


offset <- feOffset("SourceAlpha", result="offOut",
                   dx=unit(2, "mm"), dy=unit(-2, "mm"))
blur <- feGaussianBlur("offOut", sd=3, result="gaussOut")
blend <- feBlend("SourceGraphic", "gaussOut")
feComplex <- filterEffect(list(offset, blur, blend))
grid.rect(name="r2", width=.8, height=.8,
          gp=gpar(fill="white"))
grid.filter("r2", feComplex) 


# grid.newpage()
pushViewport(viewport(x=1/4, width=.4, height=.8))
feSimple <- filterEffect(feGaussianBlur(sd=3))
grid.rect(name="r1", width=.8, height=.8)
grid.filter("r1", feSimple)
popViewport()
pushViewport(viewport(x=3/4, width=.4, height=.8))
offset <- feOffset("SourceAlpha", result="offOut",
                   dx=unit(2, "mm"), dy=unit(-2, "mm"))
blur <- feGaussianBlur("offOut", sd=3, result="gaussOut")
blend <- feBlend("SourceGraphic", "gaussOut")
feComplex <- filterEffect(list(offset, blur, blend))
grid.rect(name="r2", width=.8, height=.8,
          gp=gpar(fill="white"))
grid.filter("r2", feComplex) 
popViewport()


}
figure13.8 <- function() {
gradient <- linearGradient(c("black", "white", "black"), 
                           x0=0, y0=.5, x1=1, y1=.5)


grid.rect(name="r1")
grid.gradientFill("r1", gradient)
cp <- clipPath(circleGrob())
grid.clipPath("r1", cp)


grid.rect(name="r2")
grid.gradientFill("r2", gradient)
cp <- clipPath(circleGrob(x=1:3/4, r=.3))
grid.clipPath("r2", cp)


# grid.newpage()
pushViewport(viewport(x=1/4, width=.4, height=.8))
grid.rect(gp=gpar(col="grey"))
grid.rect(name="r1")
grid.gradientFill("r1", gradient)
cp <- clipPath(circleGrob())
grid.clipPath("r1", cp)
popViewport()
pushViewport(viewport(x=3/4, width=.4, height=.8))
grid.rect(gp=gpar(col="grey"))
grid.rect(name="r2")
grid.gradientFill("r2", gradient)
cp <- clipPath(circleGrob(x=1:3/4, r=.3))
grid.clipPath("r2", cp)
popViewport()


}
figure13.9 <- function() {
gradient <- linearGradient(c("black", "white", "black"), 
                           x0=0, y0=.5, x1=1, y1=.5)


circlesOnBlack <- 
    gTree(children=gList(rectGrob(gp=gpar(fill="black")),
                         circleGrob(x=1:3/4, r=.3, 
                                    gp=gpar(col=NA,
                                            fill="white"))))
m <- mask(circlesOnBlack)


grid.rect(name="r2")
grid.gradientFill("r2", gradient)
grid.mask("r2", m)


# grid.newpage()
pushViewport(viewport(x=1/4, width=.4, height=.8))
grid.rect(gp=gpar(col="grey"))
grid.draw(circlesOnBlack)
popViewport()
pushViewport(viewport(x=3/4, width=.4, height=.8))
grid.rect(gp=gpar(col="grey"))
grid.rect(name="r2")
grid.gradientFill("r2", gradient)
grid.mask("r2", m)
popViewport()


}
figure13.10 <- function() {
gradient <- linearGradient(c("black", "white", "black"), 
                           x0=0, y0=.5, x1=1, y1=.5)


circlesOnBlack <- 
    gTree(children=gList(rectGrob(gp=gpar(fill="black")),
                         circleGrob(x=1:3/4, r=.3, 
                                    gp=gpar(col=NA,
                                            fill="white"))))
m <- mask(circlesOnBlack)


grayGradient <-
    gTree(children=gList(gradientFillGrob(rectGrob(), 
                                          gradient)))
m <- mask(grayGradient)


masked <- maskGrob(circlesOnBlack, m)
grid.draw(masked)


# grid.newpage()
pushViewport(viewport(x=1/4, width=.4, height=.8))
grid.rect(gp=gpar(col="grey"))
grid.draw(grayGradient)
popViewport()
pushViewport(viewport(x=3/4, width=.4, height=.8))
grid.rect(gp=gpar(col="grey"))
masked <- maskGrob(circlesOnBlack, m)
grid.draw(masked)
popViewport()


}
figure13.11 <- function() {
gradientBBox <- linearGradient(c("black", "white", "black"), 
                               gradientUnits="bbox",
                               x0=0, y0=.5, x1=1, y1=.5)


grid.rect(1:2/3, 1:2/3, width=1/3, height=.2, name="r2")
grid.gradientFill("r2", gradientBBox)
grid.export()


}
figure13.12 <- function() {
gradientPage <- linearGradient(c("black", "white", "black"), 
                           gradientUnits="coords",
                           x0=0, y0=.5, x1=1, y1=.5)


grid.rect(1:2/3, 1:2/3, width=1/3, height=.2, name="r2")
grid.gradientFill("r2", gradientPage)
grid.export()


}
figure13.13 <- function() {
gradientPage <- linearGradient(c("black", "white", "black"), 
                           gradientUnits="coords",
                           x0=0, y0=.5, x1=1, y1=.5)


pushViewport(viewport(width=1/3, name="vp"))
registerGradientFill("g", gradientPage)
upViewport()
grid.rect(1:2/3, 1:2/3, width=1/3, height=.2, name="r2")
grid.gradientFill("r2", label="g")
grid.export()


}
figure13.14 <- function() {
stacks <- getSVGFonts()
stacks


stacks$serif <- c("Satisfy", "serif")
setSVGFonts(stacks)


pdf(NULL, width=2, height=1)
grid.text("hello", gp=gpar(fontfamily="serif"))
svg <- grid.export(NULL)$svg
dev.off()


root <- 
    XML::xmlRoot(svg, "svg:svg",
            namespaces=c(svg="http://www.w3.org/2000/svg"))
url <- 
    "url('https://fonts.googleapis.com/css?family=Satisfy');"
styleNode <- 
    XML::newXMLNode("style", 
               attrs=c(type="text/css"),
               paste("@import", url))
invisible(XML::newXMLNode("defs", styleNode, parent=root))


}
figure5.1 <- function() {
print(
qplot(temperature, pressure, data=pressure)

)


}
figure5.2 <- function() {
print(
qplot(temperature, pressure, data=pressure,
      main="Vapor Pressure of Mercury",
      geom=c("point", "line"))

)


}
figure5.3 <- function() {
# grid.newpage()
layvp <- viewport(layout=grid.layout(1, 5,
                    heights=unit(1, "inch"),
                    widths=unit(c(1, .5), "inch")),
                  name="vplay")
vpi <- function(i) {
    viewport(layout.pos.col=i, name=paste("vp", i, sep=""))
}
pushViewport(layvp)
pushViewport(viewport(layout.pos.col=1:5))
grid.rect(width=1.1, gp=gpar(col=NA, fill="gray"))
popViewport()
pushViewport(vpi(1))
upViewport()
pushViewport(vpi(3))
upViewport()
pushViewport(vpi(5))
upViewport(2)
for (i in c(1, 3, 5)) {
    grid.roundrect(height=.5,
                   vp=paste("vplay::vp", i, sep=""),
                   name=paste("rr", i, sep=""),
                   gp=gpar(fill="white"))
}
grid.text("data", vp="vplay::vp1")
grid.text("aesthetic", vp="vplay::vp3")
grid.text("geom", vp="vplay::vp5")
arr <- arrow(length=unit(3, "mm"), type="closed")
grid.segments(grobX("rr1", 0), .5,
              grobX("rr3", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr3", 0), .5,
              grobX("rr5", 180), .5,
              arrow=arr, gp=gpar(fill="black"))



}
figure5.4 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


p


print(
p
)


}
figure5.5 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


print(
p + geom_point(aes(x=disp, y=mpg))

)


print(
p + geom_point(aes(x=disp, y=mpg, shape=gear),
               size=4) +
    theme(legend.position="none")
)


print(
p + geom_text(aes(x=disp, y=mpg, label=gear))

)


lmcoef <- coef(lm(mpg ~ disp, mtcars2))



print(
p + geom_point(aes(x=disp, y=mpg)) +
    geom_abline(intercept=lmcoef[1], slope=lmcoef[2])

)


}
figure5.6 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


print(
p + geom_point(aes(x=disp, y=mpg)) +
    scale_y_continuous(name="miles per gallon") +
    scale_x_continuous(name="displacement (cu.in.)")
)


print(
p + geom_point(aes(x=disp, y=mpg)) +
    scale_y_continuous(limits=c(0, 40)) 

)


print(
p + geom_point(aes(x=disp, y=mpg, 
                   color=trans), size=4) +
    scale_color_manual(values=c(automatic=gray(2/3),
                                manual=gray(1/3)))

)


}
figure5.7 <- function() {
# grid.newpage()
layvp <- viewport(layout=grid.layout(1, 7,
                    heights=unit(1, "inch"),
                    widths=unit(c(1, .5), "inch")),
                  name="vplay")
vpi <- function(i) {
    viewport(layout.pos.col=i, name=paste("vp", i, sep=""))
}
pushViewport(layvp)
pushViewport(viewport(layout.pos.col=1:7))
grid.rect(width=1.1, gp=gpar(col=NA, fill="gray"))
popViewport()
pushViewport(vpi(1))
upViewport()
pushViewport(vpi(3))
upViewport()
pushViewport(vpi(5))
upViewport()
pushViewport(vpi(7))
upViewport(2)
for (i in c(1, 3, 5, 7)) {
    grid.roundrect(height=.5,
                   vp=paste("vplay::vp", i, sep=""),
                   name=paste("rr", i, sep=""),
                   gp=gpar(fill="white"))
}
grid.text("data", vp="vplay::vp1")
grid.text("scale", vp="vplay::vp3")
grid.text("aesthetic", vp="vplay::vp5")
grid.text("geom", vp="vplay::vp7")
arr <- arrow(length=unit(3, "mm"), type="closed")
grid.segments(grobX("rr1", 0), .5,
              grobX("rr3", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr3", 0), .5,
              grobX("rr5", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
# grid.curve(grobX("rr1", 27), grobY("rr1", 27), 
#            grobX("rr5", 153), grobY("rr5", 153),
#            square=FALSE, ncp=8, curvature=-.3,
#            arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr5", 0), .5,
              grobX("rr7", 180), .5,
              arrow=arr, gp=gpar(fill="black"))



}
figure5.8 <- function() {
# grid.newpage()
layvp <- viewport(layout=grid.layout(1, 9,
                    heights=unit(1, "inch"),
                    widths=unit(c(1, .5), "inch")),
                  name="vplay")
vpi <- function(i) {
    viewport(layout.pos.col=i, name=paste("vp", i, sep=""))
}
pushViewport(layvp)
pushViewport(viewport(layout.pos.col=1:9))
grid.rect(width=1.1, gp=gpar(col=NA, fill="gray"))
popViewport()
pushViewport(vpi(1))
upViewport()
pushViewport(vpi(3))
upViewport()
pushViewport(vpi(5))
upViewport()
pushViewport(vpi(7))
upViewport()
pushViewport(vpi(9))
upViewport(2)
for (i in c(1, 3, 5, 7, 9)) {
    grid.roundrect(height=.5,
                   vp=paste("vplay::vp", i, sep=""),
                   name=paste("rr", i, sep=""),
                   gp=gpar(fill="white"))
}
grid.text("data", vp="vplay::vp1")
grid.text("scale", vp="vplay::vp3")
grid.text("stat", vp="vplay::vp5")
grid.text("aesthetic", vp="vplay::vp7")
grid.text("geom", vp="vplay::vp9")
arr <- arrow(length=unit(3, "mm"), type="closed")
grid.segments(grobX("rr1", 0), .5,
              grobX("rr3", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr3", 0), .5,
              grobX("rr5", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr5", 0), .5,
              grobX("rr7", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr7", 0), .5,
              grobX("rr9", 180), .5,
              arrow=arr, gp=gpar(fill="black"))



}
figure5.9 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


print(
p + geom_bar(aes(x=trans))

)


update_geom_defaults("smooth", aes(color="black"))
print(
p + geom_smooth(aes(x=disp, y=mpg))

)


}
figure5.10 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


print(
p + geom_point(aes(x=disp, y=mpg, shape=trans)) +
    scale_shape_manual(values=c(1, 3))

)


print(
ggplot(mtcars2, aes(x=disp, y=mpg)) + 
    geom_point() +
    stat_smooth(aes(group=trans),
                method="lm")

)


}
figure5.11 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


print(
p + geom_bar(aes(x=trans, fill=factor(cyl)),
             color="black") +
    scale_fill_manual(values=gray(1:3/3))

)


print(
p + geom_bar(aes(x=trans, fill=factor(cyl)),
             color="black",
             position="dodge") +
    scale_fill_manual(values=gray(1:3/3))

)


print(
p + geom_bar(aes(x=trans, fill=factor(cyl)),
             color="black",
             position="fill") +
    scale_fill_manual(values=gray(1:3/3))

)


}
figure5.12 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


print(
p + geom_point(aes(x=disp, y=mpg)) + 
    scale_y_continuous(trans="log", 
                       breaks=seq(10, 40, 10)) +
    scale_x_continuous(trans="log", 
                       breaks=seq(100, 400, 100)) +
    geom_line(aes(x=disp, y=mpg), stat="smooth", 
              method="lm")
)


print(
p + geom_point(aes(x=disp, y=mpg)) + 
    scale_x_continuous(trans="log") +
    scale_y_continuous(trans="log") +
    geom_line(aes(x=disp, y=mpg), stat="smooth", 
              method="lm") +
    coord_trans(x="exp", y="exp")
)


print(
p + geom_bar(aes(x="", fill=trans)) +
    scale_fill_manual(values=gray(1:2/3))
)


print(
p + geom_bar(aes(x="", fill=trans)) +
    scale_fill_manual(values=gray(1:2/3)) +
    coord_polar(theta="y") 

)


}
figure5.13 <- function() {
# grid.newpage()
layvp <- viewport(layout=grid.layout(1, 11,
                    heights=unit(1, "inch"),
                    widths=unit(c(1, .5), "inch")),
                  name="vplay")
vpi <- function(i) {
    viewport(layout.pos.col=i, name=paste("vp", i, sep=""))
}
pushViewport(layvp)
pushViewport(viewport(layout.pos.col=1:11))
grid.rect(width=1.1, gp=gpar(col=NA, fill="gray"))
popViewport()
pushViewport(vpi(1))
upViewport()
pushViewport(vpi(3))
upViewport()
pushViewport(vpi(5))
upViewport()
pushViewport(vpi(7))
upViewport()
pushViewport(vpi(9))
upViewport()
pushViewport(vpi(11))
upViewport(2)
for (i in c(1, 3, 5, 7, 9, 11)) {
    grid.roundrect(height=.5,
                   vp=paste("vplay::vp", i, sep=""),
                   name=paste("rr", i, sep=""),
                   gp=gpar(fill="white"))
}
grid.text("data", vp="vplay::vp1")
grid.text("scale", vp="vplay::vp3")
grid.text("stat", vp="vplay::vp5")
grid.text("aesthetic", vp="vplay::vp7")
grid.text("geom", vp="vplay::vp9")
grid.text("coord", vp="vplay::vp11")
arr <- arrow(length=unit(3, "mm"), type="closed")
grid.segments(grobX("rr1", 0), .5,
              grobX("rr3", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr3", 0), .5,
              grobX("rr5", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr5", 0), .5,
              grobX("rr7", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr7", 0), .5,
              grobX("rr9", 180), .5,
              arrow=arr, gp=gpar(fill="black"))
grid.segments(grobX("rr9", 0), .5,
              grobX("rr11", 180), .5,
              arrow=arr, gp=gpar(fill="black"))



}
figure5.14 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


print(
p + geom_point(aes(x=disp, y=mpg)) +
    facet_wrap(~ gear, nrow=1)

)


}
figure5.15 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


print(
p + geom_point(aes(x=disp, y=mpg)) +
    theme_bw()
)


print(
p + geom_point(aes(x=disp, y=mpg)) +
    theme(axis.title.y=element_text(angle=0, vjust=.5))
)


print(
p + geom_point(aes(x=disp, y=mpg)) +
    theme(axis.title.y=element_blank())
)


print(
p + geom_point(aes(x=disp, y=mpg)) +
    labs(title="Vehicle Fuel Efficiency")
)


}
figure5.16 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$gear <- as.factor(mtcars$gear)
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL
mtcars2$wt <- NULL
mtcars2$hp <- NULL
mtcars2$qsec <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg


p <- ggplot(mtcars2)


print(
p + geom_point(aes(x=disp, y=mpg)) +
    geom_hline(yintercept=29)

)


gcLimits <- 
    data.frame(category=c("2WD car",
                 "4WD car",
                 "2WD small pick-up truck",
                 "4WD small pick-up truck",
                 "2WD std pick-up truck",
                 "4WD std pick-up truck"),
               limit=c(29, 24, 20, 18, 17, 16))


print(
p + geom_point(aes(x=disp, y=mpg)) +
    geom_hline(data=gcLimits, 
               aes(yintercept=limit),
               linetype="dotted") +
    geom_text(data=gcLimits,
              aes(y=limit + .1, label=category),
              x=70, hjust=0, vjust=0, size=3)

)


}
figure11.1 <- function() {



}
figure11.2 <- function() {
# Data from Land Information New Zealand
# See Moon/provenance.txt and Moon/moon.R
lowTides <-
    c("00:55", "01:47", "02:38", "03:29", "04:20", "05:11", "06:05", 
      "07:01", "08:00", "09:01", "10:01", "10:58", "11:51", "00:16", 
      "01:03", "01:44", "02:23", "03:00", "03:36", "04:12", "04:49", 
      "05:29", "06:12", "07:02", "07:59", "09:02", "10:06", "11:08", 
      "12:08", "00:34", "01:29")
phases <- data.frame(date=c("2010-01-01 08:13", "2010-01-07 23:40",
                       "2010-01-15 08:12", "2010-01-23 23:54",
                       "2010-01-30 07:18"),
                     phase=c("Full", "3Q", "New", "1Q", "Full"))
phases$date <- as.POSIXct(as.character(phases$date))
lowTideDate <- as.POSIXct(paste("2010-01-", 
                                sprintf("%02d", 1:31), 
                                " ", lowTides,
                                sep=""))
lowTideHour <- as.POSIXct(paste("2010-01-01 ", 
                                lowTides,
                                sep=""))

mainHours <- ISOdatetime(2010, 1, 1,
                         c(0, 4, 8, 12), 
                         rep(0, 4), 
                         rep(0, 4))




moon <- jpeg::readJPEG(system.file("extra", "GPN-2000-000473.jpg",
                             package="RGraphics"))


moonPhase <- function(x, y, phase, size=.05) {
  # size is in inches
  n <- 17
  angle <- seq(0, 2*pi, length=n)
  xx <- x + cos(angle)*xinch(size)
  yy <- y + sin(angle)*yinch(size)
  if (phase == "New")
    fill <- "black"
  else
    fill <- "white"
  polygon(xx, yy, col=fill)
  if (phase == "1Q")
    polygon(xx[(n/4):(n*3/4) + 1],
            yy[(n/4):(n*3/4) + 1],
            col="black")
  if (phase == "3Q")
    polygon(xx[c(1:(n/4 + 1), (n*3/4 + 1):n)],
            yy[c(1:(n/4 + 1), (n*3/4 + 1):n)],
            col="black")
}


# Original image from NASA
# http://grin.hq.nasa.gov/ABSTRACTS/GPN-2000-000473.html
rasterMoon <- jpeg::readJPEG(system.file("extra", "GPN-2000-000473.jpg", 
                                         package="RGraphics"))
par(pin=c(3.5, 1.75), oma=c(0, 3, 0, 0), xaxs="i", yaxs="i", cex=.7)
plot.new()
rect(0, 0, 1, 1, col="black")
rasterImage(rasterMoon, .25, 0, .75, 1*813/703)
par(new=TRUE, xaxs="r", yaxs="r", las=1)
plot(lowTideDate, lowTideHour, type="n",
     ylim=range(mainHours), axes=FALSE, ann=FALSE)
# dashed reference lines
abline(v=phases$date,
       col="white", lty="dashed")
for (subset in list(1:13, 14:29, 30:31)) {
  lines(lowTideDate[subset], lowTideHour[subset],
        lwd=2, col="white")
  points(lowTideDate[subset], lowTideHour[subset],
         pch=16, col="white")
}
box()
axis.POSIXct(1, lowTideDate)
axis.POSIXct(2, at=mainHours, format="%H:%M")
mtext("Time of Low Tide (NZDT)", side=2, line=4, las=0, cex=.7)
mtext("Auckland, New Zealand January 2010", side=1, line=3, cex=.7)
axis(3, at=phases$date, labels=FALSE)
par(xpd=NA)
ymax <- par("usr")[4]
for (i in 1:nrow(phases))
    moonPhase(phases$date[i], ymax + yinch(.2), 
              phases$phase[i])
mtext("Phases of the Moon", side=3, line=3, cex=.7)




}
figure11.3 <- function() {

moon <- jpeg::readJPEG(system.file("extra", "GPN-2000-000473.jpg",
                             package="RGraphics"))


grid.raster(moon, x=0, y=1, height=.5, just=c("left", "top"))
grid.raster(moon, x=1, y=.75, 
            width=.5, height=.25, just="right")
for (i in seq(10, 90, 10)) {
  pushViewport(viewport(x=i/100, y=.25, width=.2, height=.2, 
                        angle=i - 10))
  grid.raster(moon)
  popViewport()
}


}
figure11.4 <- function() {

PostScriptTrace(system.file("extra", "comic_moon.ps",
                            package="RGraphics"),
                "comic_moon.xml")


vectorMoon <- grImport::readPicture("comic_moon.xml")


grImport::grid.picture(vectorMoon)
grImport::grid.picture(vectorMoon, 
                       x=0, y=1, just=c("left", "top"),
                       width=.2, height=.2)
grImport::grid.picture(vectorMoon, 
                       x=1, y=1, just=c("right", "top"),
                       width=.3, height=.1, distort=TRUE)


grid.rect()
grImport::grid.picture(vectorMoon)
grImport::grid.picture(vectorMoon, 
                       x=0, y=1, just=c("left", "top"),
                       width=.2, height=.2)
grImport::grid.picture(vectorMoon, 
                       x=1, y=1, just=c("right", "top"),
                       width=.3, height=.1, distort=TRUE)


}
figure11.5 <- function() {
# Data from Land Information New Zealand
# See Moon/provenance.txt and Moon/moon.R
lowTides <-
    c("00:55", "01:47", "02:38", "03:29", "04:20", "05:11", "06:05", 
      "07:01", "08:00", "09:01", "10:01", "10:58", "11:51", "00:16", 
      "01:03", "01:44", "02:23", "03:00", "03:36", "04:12", "04:49", 
      "05:29", "06:12", "07:02", "07:59", "09:02", "10:06", "11:08", 
      "12:08", "00:34", "01:29")
phases <- data.frame(date=c("2010-01-01 08:13", "2010-01-07 23:40",
                       "2010-01-15 08:12", "2010-01-23 23:54",
                       "2010-01-30 07:18"),
                     phase=c("Full", "3Q", "New", "1Q", "Full"))
phases$date <- as.POSIXct(as.character(phases$date))
lowTideDate <- as.POSIXct(paste("2010-01-", 
                                sprintf("%02d", 1:31), 
                                " ", lowTides,
                                sep=""))
lowTideHour <- as.POSIXct(paste("2010-01-01 ", 
                                lowTides,
                                sep=""))

mainHours <- ISOdatetime(2010, 1, 1,
                         c(0, 4, 8, 12), 
                         rep(0, 4), 
                         rep(0, 4))







grid.moonPhase <- function(x, y, phase, size=unit(.05, "in")) {
  n <- 17
  angle <- seq(0, 2*pi, length=n)
  xx <- x + cos(angle)*size
  yy <- y + sin(angle)*size
  if (phase == "New")
    fill <- "black"
  else
    fill <- "white"
  grid.polygon(xx, yy, gp=gpar(fill=fill))
  if (phase == "1Q")
      grid.polygon(xx[(n/4):(n*3/4) + 1],
                   yy[(n/4):(n*3/4) + 1],
                   gp=gpar(fill="black"))
  if (phase == "3Q")
      grid.polygon(xx[c(1:(n/4 + 1), (n*3/4 + 1):n)],
                   yy[c(1:(n/4 + 1), (n*3/4 + 1):n)],
                   gp=gpar(fill="black"))
}

# grid.newpage()
pushViewport(viewport(gp=gpar(cex=0.7)),
             plotViewport(c(4, 5, 3, 1)),
             dataViewport(as.numeric(lowTideDate), 
                          as.numeric(mainHours)))
vectorMoon <- 
    grImport::readPicture(system.file("extra", "comic_moon.ps.xml",
                            package="RGraphics"))
grImport::grid.picture(vectorMoon)
grid.segments(unit(phases$date, "native"), 0,
              unit(phases$date, "native"), 1,
              gp=gpar(lty="dashed"))
for (subset in list(1:13, 14:29, 30:31)) {
  grid.lines(lowTideDate[subset], lowTideHour[subset],
             default.units="native", 
             gp=gpar(lwd=2))
  grid.points(lowTideDate[subset], lowTideHour[subset],
              pch=16, size=unit(2, "mm"))
}
grid.rect(gp=gpar(fill=NA))
xTicks <- seq(min(lowTideDate), max(lowTideDate), by="week")
grid.xaxis(at=xTicks, label=format(xTicks, "%b %d"))
grid.yaxis(at=mainHours, label=format(mainHours, "%H:%M"))
grid.text("Time of Low Tide (NZDT)", 
          x=unit(-4, "lines"), rot=90)
grid.text("Auckland, New Zealand January 2010", 
          y=unit(-3, "lines"))
grid.xaxis(main=FALSE, at=phases$date, label=FALSE)
for (i in 1:nrow(phases))
    grid.moonPhase(unit(phases$date[i], "native"),
                   unit(1, "npc") + unit(1, "lines"), 
                   phases$phase[i])
grid.text("Phases of the Moon", 
          y=unit(1, "npc") + unit(2, "lines"))
popViewport(2)


}
figure11.6 <- function() {





PostScriptTrace(system.file("extra", "comic_moon.ps",
                            package="RGraphics"),
                "comic_moon.xml")


vectorMoon <- grImport::readPicture("comic_moon.xml")


grImport::grid.picture(vectorMoon[1:4])



grImport::grid.picture(vectorMoon, use.gc=FALSE)



}
figure11.7 <- function() {

PostScriptTrace(system.file("extra", "comic_moon.ps",
                            package="RGraphics"),
                "comic_moon.xml")


vectorMoon <- grImport::readPicture("comic_moon.xml")


grImport::picturePaths(vectorMoon[1:6], fill="white", 
             freeScales=TRUE, nr=2, nc=3)



}
figure11.8 <- function() {



rsvg::rsvg_svg(system.file("extra", "comic_moon.svg", 
         package="RGraphics"),
         "comic_moon_cairo.svg")



moonSVG <- grImport2::readPicture("comic_moon_cairo.svg")


grImport2::grid.picture(moonSVG)


grid.rect()
grImport2::grid.picture(moonSVG)


}
figure11.9 <- function() {
importtest <- function() {
    grid.rect(gp=gpar(col=NA, fill="grey"))
    grid.text("This should not be visible")
    grid.raster(matrix(0:1, ncol=5, nrow=2, byrow=TRUE), 
                interpolate=FALSE)
}
importtest()


}
figure11.10 <- function() {
PostScriptTrace(system.file("extra", "importtest.ps", package="RGraphics"),
                "importtest.xml")
test <- grImport::readPicture("importtest.xml")
grImport::grid.picture(test)


}
figure11.11 <- function() {



rsvg::rsvg_svg(system.file("extra", "importtest.svg", package="RGraphics"),
               "importtest-cairo.svg")
test <- grImport2::readPicture("importtest-cairo.svg")
grImport2::grid.picture(test)


}
figure11.12 <- function() {



rsvg::rsvg_svg(system.file("extra", "moon-26619.svg", 
                     package="RGraphics"),
         "full-moon.svg")
moon <- grImport2::readPicture("full-moon.svg")


grImport2::grid.picture(moon)


}
figure11.13 <- function() {
rsvg::rsvg_svg(system.file("extra", "moon-26619.svg", 
                     package="RGraphics"),
         "full-moon.svg")
moon <- grImport2::readPicture("full-moon.svg")


grImport2::grid.picture(moon, ext="gridSVG")
grid.export("moon3gridsvg.svg")


}
figure11.14 <- function() {
# Data from Land Information New Zealand
# See Moon/provenance.txt and Moon/moon.R
lowTides <-
    c("00:55", "01:47", "02:38", "03:29", "04:20", "05:11", "06:05", 
      "07:01", "08:00", "09:01", "10:01", "10:58", "11:51", "00:16", 
      "01:03", "01:44", "02:23", "03:00", "03:36", "04:12", "04:49", 
      "05:29", "06:12", "07:02", "07:59", "09:02", "10:06", "11:08", 
      "12:08", "00:34", "01:29")
phases <- data.frame(date=c("2010-01-01 08:13", "2010-01-07 23:40",
                       "2010-01-15 08:12", "2010-01-23 23:54",
                       "2010-01-30 07:18"),
                     phase=c("Full", "3Q", "New", "1Q", "Full"))
phases$date <- as.POSIXct(as.character(phases$date))
lowTideDate <- as.POSIXct(paste("2010-01-", 
                                sprintf("%02d", 1:31), 
                                " ", lowTides,
                                sep=""))
lowTideHour <- as.POSIXct(paste("2010-01-01 ", 
                                lowTides,
                                sep=""))

mainHours <- ISOdatetime(2010, 1, 1,
                         c(0, 4, 8, 12), 
                         rep(0, 4), 
                         rep(0, 4))



rsvg::rsvg_svg(system.file("extra", "moon-26619.svg", 
                     package="RGraphics"),
         "full-moon.svg")
moon <- grImport2::readPicture("full-moon.svg")


pushViewport(viewport(gp=gpar(cex=0.7)),
             plotViewport(c(4, 5, 3, 1)),
             dataViewport(as.numeric(lowTideDate), 
                          as.numeric(mainHours)))
grid.rect(gp=gpar(fill="black"))
grImport2::grid.picture(moon, ext="gridSVG", x=.65)
grid.segments(unit(phases$date, "native"), 0,
              unit(phases$date, "native"), 1,
              gp=gpar(lty="dashed", col="white"))
for (subset in list(1:13, 14:29, 30:31)) {
  grid.lines(lowTideDate[subset], lowTideHour[subset],
             default.units="native", 
             gp=gpar(lwd=2, col="white"))
  grid.points(lowTideDate[subset], lowTideHour[subset],
              pch=16, size=unit(2, "mm"),
              gp=gpar(col="white"))
}
xTicks <- seq(min(lowTideDate), max(lowTideDate), by="week")
grid.xaxis(at=xTicks, label=format(xTicks, "%b %d"))
grid.yaxis(at=mainHours, label=format(mainHours, "%H:%M"))
grid.text("Time of Low Tide (NZDT)", 
          x=unit(-4, "lines"), rot=90)
grid.text("Auckland, New Zealand January 2010", 
          y=unit(-3, "lines"))
grid.xaxis(main=FALSE, at=phases$date, label=FALSE)


}
figure8.1 <- function() {
grid.text("underlined text", y=.5, just="bottom")
w <- stringWidth("underlined text")
grid.segments(unit(.5, "npc") - 0.5*w, 
              unit(.5, "npc") - unit(1, "mm"),
              unit(.5, "npc") + 0.5*w, 
              unit(.5, "npc") - unit(1, "mm"))


grid.rect(gp=gpar(col="grey"))
grid.text("underlined text", y=.5, just="bottom")
w <- stringWidth("underlined text")
grid.segments(unit(.5, "npc") - 0.5*w, 
              unit(.5, "npc") - unit(1, "mm"),
              unit(.5, "npc") + 0.5*w, 
              unit(.5, "npc") - unit(1, "mm"))


}
textCorners <- function(x) {
    list(xl=grobX(x, 180), xr=grobX(x, 0),
         yb=grobY(x, 270), yt=grobY(x, 90))
}

grid.utext <- function(label, x=.5, y=.5, ..., 
                       name="utext") {
    grid.text(label, x, y, ..., name=paste0(name, ".label")) 
    corners <- textCorners(paste0(name, ".label"))
    grid.segments(corners$xl, corners$yb - unit(.2, "lines"), 
                  corners$xr, corners$yb - unit(.2, "lines"), 
                  gp=gpar(lex=get.gpar("cex")),
                  name=paste0(name, ".underline")) 
}


figure8.3 <- function() {
textCorners <- function(x) {
    list(xl=grobX(x, 180), xr=grobX(x, 0),
         yb=grobY(x, 270), yt=grobY(x, 90))
}

grid.utext <- function(label, x=.5, y=.5, ..., 
                       name="utext") {
    grid.text(label, x, y, ..., name=paste0(name, ".label")) 
    corners <- textCorners(paste0(name, ".label"))
    grid.segments(corners$xl, corners$yb - unit(.2, "lines"), 
                  corners$xr, corners$yb - unit(.2, "lines"), 
                  gp=gpar(lex=get.gpar("cex")),
                  name=paste0(name, ".underline")) 
}


grid.utext("underlined text")


grid.newpage()
grid.rect(gp=gpar(col="grey"))
grid.utext("underlined text")


}
figure8.4 <- function() {
pushViewport(viewport(y=.5, height=.5, just="bottom",
                      gp=gpar(cex=1)))
grid.utext("underlined text")
popViewport()
pushViewport(viewport(y=0, height=.5, just="bottom",
                      gp=gpar(cex=0.5)))
grid.utext("underlined text")
popViewport()


pushViewport(viewport(x=0, width=unit(2, "in"), just="left"))
grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(y=.5, height=.5, just="bottom",
                      gp=gpar(cex=1)))
grid.utext("underlined text")
popViewport()
pushViewport(viewport(y=0, height=.5, just="bottom",
                      gp=gpar(cex=0.5)))
grid.utext("underlined text")
popViewport()
popViewport()
pushViewport(viewport(x=1, width=unit(2, "in"), just="right"))
grid.rect(gp=gpar(col="grey"))
grid.utextabs <- function(label, x=.5, y=.5, ..., name="utext") {
    grid.text(label, x, y, ..., name=paste0(name, ".label"))
    corners <- textCorners(paste0(name, ".label"))
    grid.segments(corners$xl, corners$yb - unit(1, "mm"), 
                  corners$xr, corners$yb - unit(1, "mm"), 
                  name=paste0(name, ".underline"))
}
pushViewport(viewport(y=.5, height=.5, just="bottom",
                      gp=gpar(cex=1)))
grid.utextabs("underlined text")
popViewport()
pushViewport(viewport(y=0, height=.5, just="bottom",
                      gp=gpar(cex=0.5)))
grid.utextabs("underlined text")
popViewport()
popViewport()


}
figure8.5 <- function() {
grid.utext("underlined text")
grid.edit("utext.underline", gp=gpar(lwd=3, lineend="butt"))


}
utextvp <- function(label, x, y, ..., name="utextvp") {
    w <- stringWidth(label)
    viewport(x, y, width=w, height=unit(1, "lines"),
             ..., name=name) 
}

grid.utextvp <- function(label, x=.5, y=.5, ..., 
                         name="utext") {    
    pushViewport(utextvp(label, x, y, ...)) 
    grid.text(label, y=0, just="bottom", 
              name=paste0(name, ".label"))
    grid.segments(0, unit(-.2, "lines"),
                  1, unit(-.2, "lines"),
                  name=paste0(name, ".underline")) 
    upViewport() 
} 


figure8.7 <- function() {
utextvp <- function(label, x, y, ..., name="utextvp") {
    w <- stringWidth(label)
    viewport(x, y, width=w, height=unit(1, "lines"),
             ..., name=name) 
}

grid.utextvp <- function(label, x=.5, y=.5, ..., 
                         name="utext") {    
    pushViewport(utextvp(label, x, y, ...)) 
    grid.text(label, y=0, just="bottom", 
              name=paste0(name, ".label"))
    grid.segments(0, unit(-.2, "lines"),
                  1, unit(-.2, "lines"),
                  name=paste0(name, ".underline")) 
    upViewport() 
} 


grid.utextvp("underlined text", angle=20)


grid.newpage()
grid.rect(gp=gpar(col="grey"))
grid.utextvp("underlined text", angle=20)


}
figure8.8 <- function() {
utextvp <- function(label, x, y, ..., name="utextvp") {
    w <- stringWidth(label)
    viewport(x, y, width=w, height=unit(1, "lines"),
             ..., name=name) 
}

grid.utextvp <- function(label, x=.5, y=.5, ..., 
                         name="utext") {    
    pushViewport(utextvp(label, x, y, ...)) 
    grid.text(label, y=0, just="bottom", 
              name=paste0(name, ".label"))
    grid.segments(0, unit(-.2, "lines"),
                  1, unit(-.2, "lines"),
                  name=paste0(name, ".underline")) 
    upViewport() 
} 


grid.utextvp("underlined text", angle=20)


downViewport("utextvp")
grid.segments(0, unit(-.3, "lines"), 1, unit(-.3, "lines"))


grid.newpage()
grid.rect(gp=gpar(col="grey"))
grid.utextvp("underlined text", angle=20)
downViewport("utextvp")
grid.segments(0, unit(-.3, "lines"), 1, unit(-.3, "lines"))


}
figure8.9 <- function() {
utextvp <- function(label, x, y, ..., name="utextvp") {
    w <- stringWidth(label)
    viewport(x, y, width=w, height=unit(1, "lines"),
             ..., name=name) 
}

grid.utextvp <- function(label, x=.5, y=.5, ..., 
                         name="utext") {    
    pushViewport(utextvp(label, x, y, ...)) 
    grid.text(label, y=0, just="bottom", 
              name=paste0(name, ".label"))
    grid.segments(0, unit(-.2, "lines"),
                  1, unit(-.2, "lines"),
                  name=paste0(name, ".underline")) 
    upViewport() 
} 


grid.utextvp("underlined text", angle=20)


grid.edit("utext.label", label="le texte soulign\U00E9")


}
utextChildren <- function(label, x, y, just, name) {
    t <- textGrob(label, x, y, just=just,
                  name=paste0(name, ".label"))
    corners <- textCorners(t)
    s <- segmentsGrob(corners$xl, 
                      corners$yb - unit(.2, "lines"),
                      corners$xr, 
                      corners$yb - unit(.2, "lines"),
                      name=paste0(name, ".underline"))
    gList(t, s)
}
    
utextStatic <- function(label,
                        x=.5, y=.5, default.units="npc",
                        just="centre", name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    kids <- utextChildren(label, x, y, just, name)
    gTree(label=label, x=x, y=y, just=just, 
          children=kids, cl="utextStatic", name=name)
}


figure8.11 <- function() {
utextChildren <- function(label, x, y, just, name) {
    t <- textGrob(label, x, y, just=just,
                  name=paste0(name, ".label"))
    corners <- textCorners(t)
    s <- segmentsGrob(corners$xl, 
                      corners$yb - unit(.2, "lines"),
                      corners$xr, 
                      corners$yb - unit(.2, "lines"),
                      name=paste0(name, ".underline"))
    gList(t, s)
}
    
utextStatic <- function(label,
                        x=.5, y=.5, default.units="npc",
                        just="centre", name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    kids <- utextChildren(label, x, y, just, name)
    gTree(label=label, x=x, y=y, just=just, 
          children=kids, cl="utextStatic", name=name)
}


ug <- utextStatic("underlined text")
grid.draw(ug)


grid.newpage()
grid.rect(gp=gpar(col="grey"))
ug <- utextStatic("underlined text")
grid.draw(ug)


}
editDetails.utextStatic <- function(x, specs) {
    if (any(names(specs) %in% 
        c("label", "x", "y", "just"))) {
        kids <- utextChildren(x$label, x$x, x$y, 
                              x$just, x$name)
        x <- setChildren(x, kids)
    }
    x   
}


figure8.13 <- function() {
ug <- utextStatic("underlined text")
grid.draw(ug)


grid.edit("utext", label="le texte soulign\U00E9")


}
figure8.14 <- function() {
ug <- utextStatic("underlined text")
grid.draw(ug)


grid.edit("utext.underline", gp=gpar(lty="dashed"))


}
utextvpChildren <- function(label, name) {
    t <- textGrob(label, y=0, just="bottom", 
                  vp=paste0(name, ".vp"),
                  name=paste0(name, ".label"))
    s <- segmentsGrob(0, unit(-.2, "lines"),
                      1, unit(-.2, "lines"),
                      vp=paste0(name, ".vp"),
                      name=paste0(name, ".underline"))
    gList(t, s)
}
    
utextvpStatic <- function(label, x=.5, y=.5, 
                          default.units="npc",
                          angle=0, just="centre", 
                          name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    kids <- utextvpChildren(label, name) 
    kidsvp <- utextvp(label, x, y, just=just, angle=angle, 
                      name=paste0(name, ".vp"))
    gTree(label=label, x=x, y=y, just=just, angle=angle,
          children=kids, childrenvp=kidsvp, 
          cl="utextvpStatic", name=name)
}

editDetails.utextvpStatic <- function(x, specs) {
    if (any(names(specs) %in% 
            c("label", "x", "y", "just", "angle"))) {
        kids <- utextvpChildren(x$label, x$name)
        kidsvp <- utextvp(x$label, x$x, x$y, 
                          just=x$just, angle=x$angle,
                          name=paste0(x$name, ".vp"))
        x$childrenvp <- kidsvp
        x <- setChildren(x, kids)
    }
    x   
}


figure8.16 <- function() {
utextvpChildren <- function(label, name) {
    t <- textGrob(label, y=0, just="bottom", 
                  vp=paste0(name, ".vp"),
                  name=paste0(name, ".label"))
    s <- segmentsGrob(0, unit(-.2, "lines"),
                      1, unit(-.2, "lines"),
                      vp=paste0(name, ".vp"),
                      name=paste0(name, ".underline"))
    gList(t, s)
}
    
utextvpStatic <- function(label, x=.5, y=.5, 
                          default.units="npc",
                          angle=0, just="centre", 
                          name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    kids <- utextvpChildren(label, name) 
    kidsvp <- utextvp(label, x, y, just=just, angle=angle, 
                      name=paste0(name, ".vp"))
    gTree(label=label, x=x, y=y, just=just, angle=angle,
          children=kids, childrenvp=kidsvp, 
          cl="utextvpStatic", name=name)
}

editDetails.utextvpStatic <- function(x, specs) {
    if (any(names(specs) %in% 
            c("label", "x", "y", "just", "angle"))) {
        kids <- utextvpChildren(x$label, x$name)
        kidsvp <- utextvp(x$label, x$x, x$y, 
                          just=x$just, angle=x$angle,
                          name=paste0(x$name, ".vp"))
        x$childrenvp <- kidsvp
        x <- setChildren(x, kids)
    }
    x   
}


ug <- utextvpStatic("underlined text", angle=20)
grid.draw(ug)


grid.newpage()
grid.rect(gp=gpar(col="grey"))
ug <- utextvpStatic("underlined text", angle=20)
grid.draw(ug)


}
figure8.17 <- function() {
utextvpChildren <- function(label, name) {
    t <- textGrob(label, y=0, just="bottom", 
                  vp=paste0(name, ".vp"),
                  name=paste0(name, ".label"))
    s <- segmentsGrob(0, unit(-.2, "lines"),
                      1, unit(-.2, "lines"),
                      vp=paste0(name, ".vp"),
                      name=paste0(name, ".underline"))
    gList(t, s)
}
    
utextvpStatic <- function(label, x=.5, y=.5, 
                          default.units="npc",
                          angle=0, just="centre", 
                          name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    kids <- utextvpChildren(label, name) 
    kidsvp <- utextvp(label, x, y, just=just, angle=angle, 
                      name=paste0(name, ".vp"))
    gTree(label=label, x=x, y=y, just=just, angle=angle,
          children=kids, childrenvp=kidsvp, 
          cl="utextvpStatic", name=name)
}

editDetails.utextvpStatic <- function(x, specs) {
    if (any(names(specs) %in% 
            c("label", "x", "y", "just", "angle"))) {
        kids <- utextvpChildren(x$label, x$name)
        kidsvp <- utextvp(x$label, x$x, x$y, 
                          just=x$just, angle=x$angle,
                          name=paste0(x$name, ".vp"))
        x$childrenvp <- kidsvp
        x <- setChildren(x, kids)
    }
    x   
}


ug <- utextvpStatic("underlined text", angle=20)
grid.draw(ug)


grid.edit("utext", label="le texte soulign\U00E9")


}
figure8.18 <- function() {
utextvpChildren <- function(label, name) {
    t <- textGrob(label, y=0, just="bottom", 
                  vp=paste0(name, ".vp"),
                  name=paste0(name, ".label"))
    s <- segmentsGrob(0, unit(-.2, "lines"),
                      1, unit(-.2, "lines"),
                      vp=paste0(name, ".vp"),
                      name=paste0(name, ".underline"))
    gList(t, s)
}
    
utextvpStatic <- function(label, x=.5, y=.5, 
                          default.units="npc",
                          angle=0, just="centre", 
                          name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    kids <- utextvpChildren(label, name) 
    kidsvp <- utextvp(label, x, y, just=just, angle=angle, 
                      name=paste0(name, ".vp"))
    gTree(label=label, x=x, y=y, just=just, angle=angle,
          children=kids, childrenvp=kidsvp, 
          cl="utextvpStatic", name=name)
}

editDetails.utextvpStatic <- function(x, specs) {
    if (any(names(specs) %in% 
            c("label", "x", "y", "just", "angle"))) {
        kids <- utextvpChildren(x$label, x$name)
        kidsvp <- utextvp(x$label, x$x, x$y, 
                          just=x$just, angle=x$angle,
                          name=paste0(x$name, ".vp"))
        x$childrenvp <- kidsvp
        x <- setChildren(x, kids)
    }
    x   
}


ug <- utextvpStatic("underlined text", angle=20)
grid.draw(ug)


grid.edit("utext.underline", gp=gpar(lty="dashed"))


}
utextDynamic <- function(label,
                         x=.5, y=.5, default.units="npc",
                         just="centre", name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    gTree(label=label, x=x, y=y, just=just, 
          cl="utextDynamic", name=name)
}

makeContent.utextDynamic <- function(x) {
    kids <- utextChildren(x$label, x$x, x$y, 
                          just=x$just, x$name) 
    setChildren(x, kids) 
}


figure8.20 <- function() {
utextDynamic <- function(label,
                         x=.5, y=.5, default.units="npc",
                         just="centre", name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    gTree(label=label, x=x, y=y, just=just, 
          cl="utextDynamic", name=name)
}

makeContent.utextDynamic <- function(x) {
    kids <- utextChildren(x$label, x$x, x$y, 
                          just=x$just, x$name) 
    setChildren(x, kids) 
}


ug <- utextDynamic("underlined text")
grid.draw(ug)


grid.newpage()
grid.rect(gp=gpar(col="grey"))
ug <- utextDynamic("underlined text")
grid.draw(ug)


}
figure8.21 <- function() {
utextChildren <- function(label, x, y, just, name) {
    t <- textGrob(label, x, y, just=just,
                  name=paste0(name, ".label"))
    corners <- textCorners(t)
    s <- segmentsGrob(corners$xl, 
                      corners$yb - unit(.2, "lines"),
                      corners$xr, 
                      corners$yb - unit(.2, "lines"),
                      name=paste0(name, ".underline"))
    gList(t, s)
}
    
utextStatic <- function(label,
                        x=.5, y=.5, default.units="npc",
                        just="centre", name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    kids <- utextChildren(label, x, y, just, name)
    gTree(label=label, x=x, y=y, just=just, 
          children=kids, cl="utextStatic", name=name)
}


utextDynamic <- function(label,
                         x=.5, y=.5, default.units="npc",
                         just="centre", name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    gTree(label=label, x=x, y=y, just=just, 
          cl="utextDynamic", name=name)
}

makeContent.utextDynamic <- function(x) {
    kids <- utextChildren(x$label, x$x, x$y, 
                          just=x$just, x$name) 
    setChildren(x, kids) 
}


ug <- utextDynamic("underlined text")
grid.draw(ug)


grid.edit("utext", gp=gpar(col="grey"))


}
figure8.22 <- function() {
utextDynamic <- function(label,
                         x=.5, y=.5, default.units="npc",
                         just="centre", name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    gTree(label=label, x=x, y=y, just=just, 
          cl="utextDynamic", name=name)
}

makeContent.utextDynamic <- function(x) {
    kids <- utextChildren(x$label, x$x, x$y, 
                          just=x$just, x$name) 
    setChildren(x, kids) 
}


ug <- utextDynamic("underlined text")
grid.draw(ug)


grid.edit("utext", gp=gpar(col="grey"))


grid.edit("utext", label="le texte soulign\U00E9")


}
figure8.23 <- function() {
utextDynamic <- function(label,
                         x=.5, y=.5, default.units="npc",
                         just="centre", name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    gTree(label=label, x=x, y=y, just=just, 
          cl="utextDynamic", name=name)
}

makeContent.utextDynamic <- function(x) {
    kids <- utextChildren(x$label, x$x, x$y, 
                          just=x$just, x$name) 
    setChildren(x, kids) 
}


ug <- utextDynamic("underlined text")
grid.draw(ug)


grid.edit("utext", gp=gpar(col="grey"))


grid.edit("utext", label="le texte soulign\U00E9")


grid.force()


grid.edit("utext.underline", gp=gpar(lwd=3))


}
utextvpDynamic <- function(label,
                           x=.5, y=.5, default.units="npc",
                           just="centre", angle=0, 
                           name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    gTree(label=label, x=x, y=y, just=just, angle=angle, 
          cl="utextvpDynamic", name=name)
}

makeContext.utextvpDynamic <- function(x) {
    x$childrenvp <- utextvp(x$label, x$x, x$y, 
                            just=x$just, angle=x$angle,
                            name=paste0(x$name, ".vp"))
    x
}

makeContent.utextvpDynamic <- function(x) {
    kids <- utextvpChildren(x$label, x$name)
    setChildren(x, kids)
}


figure8.25 <- function() {
utextvpDynamic <- function(label,
                           x=.5, y=.5, default.units="npc",
                           just="centre", angle=0, 
                           name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    gTree(label=label, x=x, y=y, just=just, angle=angle, 
          cl="utextvpDynamic", name=name)
}

makeContext.utextvpDynamic <- function(x) {
    x$childrenvp <- utextvp(x$label, x$x, x$y, 
                            just=x$just, angle=x$angle,
                            name=paste0(x$name, ".vp"))
    x
}

makeContent.utextvpDynamic <- function(x) {
    kids <- utextvpChildren(x$label, x$name)
    setChildren(x, kids)
}


ug <- utextvpDynamic("underlined text", angle=20)
grid.draw(ug)


grid.newpage()
grid.rect(gp=gpar(col="grey"))
ug <- utextvpDynamic("underlined text", angle=20)
grid.draw(ug)


}
figure8.26 <- function() {
utextvpDynamic <- function(label,
                           x=.5, y=.5, default.units="npc",
                           just="centre", angle=0, 
                           name="utext") {
    if (!is.unit(x)) x <- unit(x, default.units)
    if (!is.unit(y)) y <- unit(y, default.units)
    gTree(label=label, x=x, y=y, just=just, angle=angle, 
          cl="utextvpDynamic", name=name)
}

makeContext.utextvpDynamic <- function(x) {
    x$childrenvp <- utextvp(x$label, x$x, x$y, 
                            just=x$just, angle=x$angle,
                            name=paste0(x$name, ".vp"))
    x
}

makeContent.utextvpDynamic <- function(x) {
    kids <- utextvpChildren(x$label, x$name)
    setChildren(x, kids)
}


ug <- utextvpDynamic("underlined text", angle=20)
grid.draw(ug)


grid.force()


downViewport("utext.vp")
grid.segments(0, unit(-.3, "lines"), 1, unit(-.3, "lines"))


grid.rect(gp=gpar(col="grey"))
ug <- utextvpDynamic("underlined text", angle=20)
grid.draw(ug)
downViewport("utext.vp")
grid.segments(0, unit(-.3, "lines"), 1, unit(-.3, "lines"))


}
xDetails.utextvpDynamic <- function(x, theta) {
    h <- unit(1, "npc") + unit(.2, "lines")
    grobX(rectGrob(height=h, y=1, just="top",
                   vp=paste0(x$name, ".vp")), theta)
}

yDetails.utextvpDynamic <- function(x, theta) {
    h <- unit(1, "npc") + unit(.2, "lines")
    grobY(rectGrob(height=h, y=1, just="top",
                   vp=paste0(x$name, ".vp")), theta)
}


figure8.28 <- function() {
xDetails.utextvpDynamic <- function(x, theta) {
    h <- unit(1, "npc") + unit(.2, "lines")
    grobX(rectGrob(height=h, y=1, just="top",
                   vp=paste0(x$name, ".vp")), theta)
}

yDetails.utextvpDynamic <- function(x, theta) {
    h <- unit(1, "npc") + unit(.2, "lines")
    grobY(rectGrob(height=h, y=1, just="top",
                   vp=paste0(x$name, ".vp")), theta)
}


ug <- utextvpDynamic("underlined text")
grid.draw(ug)
grid.circle(.1, .8, r=unit(1, "mm"), gp=gpar(fill="black"))
grid.segments(.1, .8, 
              grobX("utext", 180), grobY("utext", 270))


grid.rect(gp=gpar(col="grey"))
ug <- utextvpDynamic("underlined text")
grid.draw(ug)
grid.circle(.1, .8, r=unit(1, "mm"), gp=gpar(fill="black"))
grid.segments(.1, .8, 
              grobX("utext", 180), grobY("utext", 270))


}
splitString <- function(text) {
  strings <- strsplit(text, " ")[[1]]
  if (length(strings) < 2)
    return(text)
  newstring <- strings[1]
  linewidth <- stringWidth(newstring)
  gapwidth <- stringWidth(" ")
  availwidth <- 
    convertWidth(unit(1, "npc"), 
                 "in", valueOnly=TRUE) 
  for (i in 2:length(strings)) {
    width <- stringWidth(strings[i])
    if (convertWidth(linewidth + gapwidth + width, 
                     "in", valueOnly=TRUE) <
        availwidth) {
      sep <- " "
      linewidth <- linewidth + gapwidth + width
    } else {
      sep <- "\n"
      linewidth <- width
    }
    newstring <- paste(newstring, strings[i], sep=sep)
  }
  newstring
}   


figure8.30 <- function() {
splitString <- function(text) {
  strings <- strsplit(text, " ")[[1]]
  if (length(strings) < 2)
    return(text)
  newstring <- strings[1]
  linewidth <- stringWidth(newstring)
  gapwidth <- stringWidth(" ")
  availwidth <- 
    convertWidth(unit(1, "npc"), 
                 "in", valueOnly=TRUE) 
  for (i in 2:length(strings)) {
    width <- stringWidth(strings[i])
    if (convertWidth(linewidth + gapwidth + width, 
                     "in", valueOnly=TRUE) <
        availwidth) {
      sep <- " "
      linewidth <- linewidth + gapwidth + width
    } else {
      sep <- "\n"
      linewidth <- width
    }
    newstring <- paste(newstring, strings[i], sep=sep)
  }
  newstring
}   


text <- "The quick brown fox jumps over the lazy dog."
grid.text(splitString(text), 
          x=0, y=1, just=c("left", "top")) 



splitText <- splitTextGrob(text, name="splitText")
grid.draw(splitText)



pushViewport(viewport(layout=grid.layout(2, 2)))
pushViewport(viewport(layout.pos.col=1))
pushViewport(viewport(width=0.5, height=0.9))
grid.rect(gp=gpar(col="gray"))
text <- "The quick brown fox jumps over the lazy dog."
grid.text(splitString(text), 
          x=0, y=1, just=c("left", "top")) 

popViewport(2)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
pushViewport(viewport(height=0.8))
grid.rect(gp=gpar(col="gray"))
splitText <- splitTextGrob(text, name="splitText")
grid.draw(splitText)

popViewport(2)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
pushViewport(viewport(height=0.8))
grid.rect(gp=gpar(col="gray"))
grid.draw(editGrob(splitText, gp=gpar(cex=1.5)))
popViewport(2)
popViewport()



}
splitTextGrob <- function(text, ...) {
    gTree(text=text, cl="splitText", ...)
}

makeContent.splitText <- function(x) {
    setChildren(x, gList(textGrob(splitString(x$text),
                                  x=0, y=1, 
                                  just=c("left", "top"))))
}


figure8.32 <- function() {
faceA(.5, .5, width=.1, height=.1)
angle <- seq(0, 2*pi, length=9)[-9]
for (i in angle) {
  x <- 0.5 + 0.3*cos(i)
  y <- 0.5 + 0.3*sin(i)
  faceA(x, y, 0.2*x, 0.2*y)
}
grid.rect(width=.9, height=.9, gp=gpar(col="gray", fill=NA))



}
faceA <- function(x, y, width, height) {
  pushViewport(viewport(x=x, y=y, 
                        width=width, height=height))
  grid.rect() 
  grid.circle(x=c(0.25, 0.75), y=0.75, r=0.1)
  grid.lines(x=c(0.33, 0.67), y=0.25) 
  popViewport()
}

faceB <- function(x, y, width, height) {
  pushViewport(viewport(x=x, y=y, 
                        width=width, height=height))
  grid.draw(rectGrob()) 
  grid.draw(circleGrob(x=c(0.25, 0.75), y=0.75, r=0.1))
  grid.draw(linesGrob(x=c(0.33, 0.67), y=0.25)) 
  popViewport()
}


faceC <- function(x, y, width, height) {
    gTree(childrenvp=viewport(x=x, y=y,
                              width=width, height=height,
                              name="face"),
          children=gList(rectGrob(vp="face"), 
                         circleGrob(x=c(0.25, 0.75), 
                                    y=0.75, r=0.1, 
                                    vp="face"),
                         linesGrob(x=c(0.33, 0.67), y=0.25,
                                   vp="face"))) 
}

faceD <- function(x, y, width, height) {
  grid.grabExpr({
                  pushViewport(viewport(x=x, y=y,
                                        width=width, 
                                        height=height))
                  grid.rect()
                  grid.circle(x=c(0.25, 0.75), 
                              y=0.75, r=0.1)
                  grid.lines(x=c(0.33, 0.67), y=0.25)
                  popViewport()
                })
}


figure1.1 <- function() {
plot(pressure)
text(150, 600, 
     "Pressure (mm Hg)\nversus\nTemperature (Celsius)")



}
figure7.1 <- function() {
grid.rect(gp=gpar(col="gray"))
grid.circle(r=.4, name="mycircle")
grid.edit("mycircle", 
          gp=gpar(fill="grey"))

grid.remove("mycircle")




}
figure7.2 <- function() {

print(
xyplot(mpg ~ disp, mtcars)
)


}
figure7.3 <- function() {
suffix <- rep(c("odd", "even"), 4)
names <- paste0("circle.", suffix)
names


grid.rect(gp=gpar(col="gray"))
for (i in 1:8)
  grid.circle(name=names[i], r=(9 - i)/20, 
              gp=gpar(col=NA, fill=gray(i/10)))

grid.edit("circle.odd", gp=gpar(fill="gray10"), 
          global=TRUE)

grid.edit("circle", gp=gpar(col="gray", fill="gray90"), 
          grep=TRUE, global=TRUE) 




}
figure7.4 <- function() {
labels <- c("\"xaxis1\"\nxaxis gTree", "\"major\"\nlines grob", 
            "\"ticks\"\nlines grob", "\"labels\"\ntext grob")
names <- c("", "major", "ticks", "labels")
boxheight <- unit(2.5, "line")
boxwidth <- unit(1.2, "in")
pushViewport(viewport(layout=grid.layout(2, 3)))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.text(labels[1])
grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
           0.5, gp=gpar(col="gray"))
grid.roundrect(height=boxheight, 
               width=boxwidth, # 1.2*stringWidth(labels[1]),
               r=unit(2, "mm"),
               gp=gpar(fill=NA))
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.text(labels[2])
grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
           0.5, gp=gpar(col="gray"))
grid.roundrect(height=boxheight, 
               width=boxwidth, # 1.2*stringWidth(labels[2]),
               r=unit(2, "mm"),
               gp=gpar(fill=NA))
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
grid.text(labels[3])
grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
           0.5, gp=gpar(col="gray"))
grid.roundrect(height=boxheight, 
               width=boxwidth, # 1.2*stringWidth(labels[3]),
               r=unit(2, "mm"),
               gp=gpar(fill=NA))
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=3))
grid.text(labels[4])
grid.lines(unit(0.5, "npc") + unit.c(-0.5*boxwidth, 0.5*boxwidth),
           0.5, gp=gpar(col="gray"))
grid.roundrect(height=boxheight, 
               width=boxwidth, # 1.2*stringWidth(labels[4]),
               r=unit(2, "mm"),
               gp=gpar(fill=NA))
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.move.to(x=0.5, y=unit(0.5, "npc") - 0.5*boxheight)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.line.to(x=0.5, y=unit(0.5, "npc") + 0.5*boxheight,
             arrow=arrow(angle=10, length=unit(3, "mm")))
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.move.to(x=0.5, y=unit(0.5, "npc") - 0.5*boxheight)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
grid.line.to(x=0.5, y=unit(0.5, "npc") + 0.5*boxheight,
             arrow=arrow(angle=10, length=unit(3, "mm")))
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.move.to(x=0.5, y=unit(0.5, "npc") - 0.5*boxheight)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=3))
grid.line.to(x=0.5, y=unit(0.5, "npc") + 0.5*boxheight,
             arrow=arrow(angle=10, length=unit(3, "mm")))
popViewport()



}
figure7.5 <- function() {
grid.rect(gp=gpar(col="gray"))
pushViewport(viewport(just="bottom", gp=gpar(cex=0.7)))
grid.xaxis(name="axis1", at=1:4/5)
grid.ls()

grid.edit("axis1", at=1:3/4)

grid.edit(gPath("axis1", "labels"), rot=45)

popViewport()



}
figure7.6 <- function() {
grid.rect(gp=gpar(col="gray"))
pushViewport(viewport(just="bottom", gp=gpar(cex=0.7)))
grid.xaxis(name="axis2", at=1:4/5)

grid.edit("axis2", gp=gpar(col="gray"))
grid.edit("axis2::labels", gp=gpar(col="black"))
popViewport()



}
figure7.7 <- function() {
grid.circle(r=0.3, gp=gpar(fill="gray80"), 
            name="mycircle")
grid.edit("mycircle", gp=gpar(lwd=5))
grid.edit("mycircle", gp=gpar(lty="dashed"))



grid.rect(gp=gpar(col="gray"))
grid.circle(r=0.3, gp=gpar(fill="gray80"), 
            name="mycircle")
grid.edit("mycircle", gp=gpar(lwd=5))
grid.edit("mycircle", gp=gpar(lty="dashed"))




}
figure7.8 <- function() {
pushViewport(viewport(xscale=c(0, 100)))
grid.rect(name="rect")
grid.xaxis(name="axis3")


grid.rect(gp=gpar(col="grey"))
pushViewport(viewport(just="bottom", width=.8, height=.4, gp=gpar(cex=.7)))
pushViewport(viewport(xscale=c(0, 100)))
grid.rect(name="rect")
grid.xaxis(name="axis3")


}
figure7.9 <- function() {
r1 <- rectGrob(height=.2, gp=gpar(fill="black"), name="r1")
r2 <- rectGrob(width=.2, gp=gpar(fill="grey"), name="r2")
r3 <- rectGrob(width=.4, height=.4, gp=gpar(fill="white"),
               name="r3")
gt <- gTree(children=gList(r1, r2, r3), name="gt")
grid.draw(gt)


grid.reorder("gt", c("r3", "r2", "r1"))


grid.rect(gp=gpar(col="gray"))
r1 <- rectGrob(height=.2, gp=gpar(fill="black"), name="r1")
r2 <- rectGrob(width=.2, gp=gpar(fill="grey"), name="r2")
r3 <- rectGrob(width=.4, height=.4, gp=gpar(fill="white"),
               name="r3")
gt <- gTree(children=gList(r1, r2, r3), name="gt")
grid.draw(gt)
grid.reorder("gt", c("r3", "r2", "r1"))



}
figure7.10 <- function() {
tg1 <- textGrob("Sample")
rg1 <- rectGrob(x=rep(0.5, 2),
                width=1.1*grobWidth(tg1), 
                height=1.3*grobHeight(tg1),
                gp=gpar(col=c("gray60", "white"), 
                        lwd=c(3, 1)))



pushViewport(viewport(layout=grid.layout(1, 7,
                                         heights=unit(1.25, "in"),
                                         widths=unit(rep(c(1, 1.25), length=7),
                                                     rep(c("null", "in"),
                                                           length=7)))))
pushViewport(viewport(layout.pos.col=2, gp=gpar(fill=NA)))
grid.rect(gp=gpar(col="gray", fill=NA))
grid.draw(tg1)
grid.draw(rg1)

popViewport()
pushViewport(viewport(layout.pos.col=4, gp=gpar(fill=NA)))
grid.rect(gp=gpar(col="gray", fill=NA))
pushViewport(viewport(gp=gpar(cex=2)))
grid.draw(tg1)
grid.draw(rg1)
popViewport()

popViewport()
pushViewport(viewport(layout.pos.col=6, gp=gpar(fill=NA)))
grid.rect(gp=gpar(col="gray", fill=NA))
pushViewport(viewport(gp=gpar(cex=2)))
grid.draw(tg1)
popViewport()
grid.draw(rg1)

popViewport()
popViewport()



}
figure7.11 <- function() {
tg1 <- textGrob("Sample", name="tg1")
rg1 <- rectGrob(width=1.1*grobWidth("tg1"), 
                height=1.3*grobHeight("tg1"),
                gp=gpar(col="gray60", lwd=3))
rg2 <- rectGrob(width=1.1*grobWidth(tg1), 
                height=1.3*grobHeight(tg1),
                gp=gpar(col="white"))



grid.rect(gp=gpar(col="gray"))
pushViewport(viewport(gp=gpar(cex=1.5, fill=NA)))
grid.draw(tg1)
grid.draw(rg1)
grid.draw(rg2)

grid.edit("tg1", grep=TRUE, global=TRUE, 
          label="Different text")

popViewport()



}
figure7.12 <- function() {
grid.rect(gp=gpar(col="gray"))
pushViewport(viewport(gp=gpar(fill=NA)))
grid.circle(.25, .5, r=unit(1, "mm"), 
            gp=gpar(fill="black"))
grid.text("A label", .75, .5)
grid.rect(.75, .5, 
          width=stringWidth("A label") + unit(2, "mm"),
          height=unit(1, "line"),
          name="labelbox")

grid.segments(.25, .5, 
              grobX("labelbox", 180), .5,
              arrow=arrow(angle=15, type="closed"),
              gp=gpar(fill="black"))





}
figure7.13 <- function() {
pushViewport(viewport(gp=gpar(fill=NA)))
vptop <- viewport(width=.9, height=.4, y=.75,
                  name="vptop")
vpbot <- viewport(width=.9, height=.4, y=.25,
                  name="vpbot")
pushViewport(vptop)
upViewport()
pushViewport(vpbot)
upViewport()

grid.rect(vp="vptop")
grid.lines(1:50/51, runif(50), vp="vptop")
grid.rect(vp="vpbot")
grid.lines(1:50/51, runif(50), vp="vpbot")

grid.null(x=.2, y=.95, vp="vptop", name="tl")
grid.null(x=.4, y=.95, vp="vptop", name="tr")
grid.null(x=.2, y=.05, vp="vpbot", name="bl")
grid.null(x=.4, y=.05, vp="vpbot", name="br")

grid.polygon(unit.c(grobX("tl", 0),
                    grobX("tr", 0),
                    grobX("br", 0),
                    grobX("bl", 0)),
             unit.c(grobY("tl", 0),
                    grobY("tr", 0),
                    grobY("br", 0),
                    grobY("bl", 0)),
             gp=gpar(col="gray", lwd=3))




}
figure7.14 <- function() {
label <- textGrob("A\nPlot\nLabel ",
                  x=0, just="left")
x <- seq(0.1, 0.9, length=50)
y <- runif(50, 0.1, 0.9)
gplot <- 
  gTree(
    children=gList(rectGrob(gp=gpar(col="gray60",
                                    fill="white")),
                   linesGrob(x, y), 
                   pointsGrob(x, y, pch=16, 
                              size=unit(1.5, "mm"))),
    vp=viewport(width=unit(1, "npc") - unit(5, "mm"), 
                height=unit(1, "npc") - unit(5, "mm")))



layout <- grid.layout(1, 2,
                      widths=unit(c(1, 1), 
                                  c("null", "grobwidth"),
                                  list(NULL, label)))



grid.rect(gp=gpar(col="gray60", fill="gray90"))
pushViewport(viewport(layout=layout))
pushViewport(viewport(layout.pos.col=2))
grid.draw(label)
popViewport()
pushViewport(viewport(layout.pos.col=1))
grid.draw(gplot)
popViewport(2)




}
figure7.15 <- function() {

print(
xyplot(mpg ~ disp, mtcars)

)
grid.edit("[.]xlab$", grep=TRUE, 
          label="Displacement",
          x=unit(1, "npc"), just="right",
          gp=gpar(fontface="bold"))
grid.edit("[.]ylab$", grep=TRUE,
          label="Miles per Gallon",
          y=unit(1, "npc"), just="right",
          gp=gpar(fontface="bold"))




}
figure7.16 <- function() {
mtcars2 <- mtcars
mtcars2$trans <- factor(mtcars$am, 
                        levels=0:1, 
                        labels=c("automatic", "manual"))
mtcars2$am <- NULL
mtcars2$vs <- NULL
mtcars2$drat <- NULL
mtcars2$carb <- NULL

# To keep R CMD check happy
mpg <- mtcars2$mpg



update_geom_defaults("smooth", aes(color="black"))
print(
ggplot(mtcars2, aes(x=disp, y=mpg)) +
    geom_point() +
    geom_smooth(method=lm)

)
# Navigate to ROOT viewport so that this code works for example(figure6.25)
# in 'RGraphics' package
upViewport(0)
grid.force()
panelvp <- grid.grep("panel", grobs=FALSE, 
                     viewports=TRUE, grep=TRUE)
panelvp
downViewport(panelvp)
sline <- grid.get(gPath("smooth", "polyline"),
                  grep=TRUE)
grid.segments(.7, .8, 
              grobX(sline, 45), grobY(sline, 45),
              arrow=arrow(angle=10, type="closed"),
              gp=gpar(fill="black"))
grid.text("line of best fit", .71, .81,
          just=c("left", "bottom"))




}
figure1.14 <- function() {
size <- unit(.5, "in")
node <- function(text, x, y, fill="grey") {
    grid.circle(x, y, r=size, gp=gpar(fill=fill), name=text)
    grid.text(text, x, y)
}
edge <- function(from, fangle, to, tangle, ends="last") {
    grid.segments(grobX(from, fangle), grobY(from, fangle),
                  grobX(to, tangle), grobY(to, tangle),
                  arrow=arrow(angle=15, type="closed", ends=ends),
                  gp=gpar(fill="black"))
}
curve <- function(from, fangle, to, tangle, curv, ends="last") {
    x1 <- convertX(grobX(from, fangle), "in")
    y1 <- convertY(grobY(from, fangle), "in")
    x2 <- convertX(grobX(to, tangle), "in")
    y2 <- convertY(grobY(to, tangle), "in")
    grid.curve(x1, y1, x2, y2,
               curvature=curv, ncp=5, square=FALSE,
               arrow=arrow(angle=15, type="closed", ends=ends),
               gp=gpar(fill="black"))
}
grid.newpage()
node("grDevices", .5, 3/5)
node("graphics", .5, 4/5)
node("grid", .5, 2/5)
node("lattice", 2/5, 1/5)
node("ggplot2", 3/5, 1/5)
node("grImport", 1/5, 3/5, fill="white")
node("grImport2", 1/5, 1.5/5, fill="white")
node("gridBase", 3.5/5, 3/5, fill="white")
node("gridGraphics", 4.5/5, 2.5/5, fill="white")
node("gridSVG", 4/5, 1.5/5, fill="white")
edge("grDevices", 90, "graphics", 270)
edge("grDevices", 270, "grid", 90)
edge("grid", 240, "lattice", 70)
edge("grid", 300, "ggplot2", 110)
edge("grImport2", 20, "grid", 200)
edge("grid", 340, "gridSVG", 160)
edge("grImport", 35, "graphics", 215)
edge("grImport", 325, "grid", 145)
edge("grid", 45, "gridBase", 225, ends="both")
edge("graphics", 315, "gridBase", 135, ends="both")
edge("gridGraphics", 190, "grid", 10)
curve("graphics", 20, "gridGraphics", 90, -.5)


}
figure10.1 <- function() {
names <- c("blank", "solid", "dashed", "dotted", "dotdash",
  "longdash", "twodash", "", "", "13", "F8", "431313", "22848222")
hgap <- unit(5, "mm")
pushViewport(viewport(layout=grid.layout(17, 5,
  widths=unit.c(unit(1, "strwidth", "Integer") + unit(3, "mm"), 
    hgap, unit(1.5, "inches"),
    hgap, max(unit(rep(1, length(names)), "strwidth", data=as.list(names))) + unit(3, "mm")),
  heights=unit(c(1.2, 1, 1, rep(1.2, 7), 1, 1, rep(1.2, 4), 1), "lines"))))
pushViewport(viewport(layout.pos.col=1:5, layout.pos.row=1:17))
grid.rect(width=1.3, height=1.3, gp=gpar(col="gray"))
popViewport()
for (i in 1:17) {
	if (i == 1) {
	  pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))  
	  grid.text("Integer", gp=gpar(fontface="bold"), just="right", x=1)
	  popViewport()
	  pushViewport(viewport(layout.pos.col=3, layout.pos.row=i))  
	  grid.text("Sample line", gp=gpar(fontface="bold"))
	  popViewport()
	  pushViewport(viewport(layout.pos.col=5, layout.pos.row=i))  
	  grid.text("String", gp=gpar(fontface="bold"), just="left", x=0)
	  popViewport()
	  pushViewport(viewport(layout.pos.row=i))  
	  grid.lines(c(0, 1), 0)
	  popViewport()
        } else if (i == 3) {
	    pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))  
	    grid.text("Predefined", just="left", x=0, 
              gp=gpar(fontface="italic"))
	    popViewport()          
        } else if (i == 12) {
	    pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))  
	    grid.text("Custom", just="left", x=0, 
              gp=gpar(fontface="italic"))
	    popViewport()          
	} else if ((i > 3 && i < 11) || 
                   (i > 12 && i < 17)) {
	  if (i < 11) {
	    pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))  
	    grid.text(i-4, just="right", x=1)
	    popViewport()
	  }
	  if (nchar(names[i-3])) {
		  pushViewport(viewport(layout.pos.col=5, layout.pos.row=i))  
		  grid.text(paste("\"", names[i-3], "\"", sep=""), x=0, just="left")
		  popViewport()
		  pushViewport(viewport(layout.pos.col=3, layout.pos.row=i))  
		  grid.lines(c(0, 1), 0.5, gp=gpar(lty=names[i-3], lwd=2))
		  popViewport()
	  }
        }
	if (i == 17) {
	    	  pushViewport(viewport(layout.pos.row=i))  
		  grid.lines(c(0, 1), 0)
		  popViewport()
	}
}
popViewport()



}
figure10.2 <- function() {
x <- c(.3, .7, .3)
y <- c(.2, .5, .8)
grid.rect(gp=gpar(col="gray"))
grid.lines(x, y, gp=gpar(lwd=40, lineend="square",
                   linejoin="mitre", col="black"))
grid.lines(x, y, gp=gpar(lwd=40, col="gray50"))
                   # lineend="round", linejoin="round"
grid.lines(x, y, gp=gpar(lwd=40, lineend="butt",
                   linejoin="bevel", col="gray80"))
grid.points(x, y, default.units="npc", pch=16, gp=gpar(cex=0.5))




}
figure10.3 <- function() {
ncol <- 6
nrow <- 5
grid.rect(gp=gpar(col="gray"))
for (i in 1:nrow) {
  for (j in 1:ncol) {
    x <- unit(j/(ncol+1), "npc")
    y <- unit(i/(nrow + 1), "npc")
    pch <- (i - 1)*ncol + j - 1
    if (pch > 25) 
      pch <- c("A", "b", ".", "#")[pch - 25]
    grid.points(x + unit(3, "mm"), y, 
      pch=pch, gp=gpar(fill="gray"))
    grid.text(pch, x - unit(3, "mm"), y, gp=gpar(col="gray"))
  }
}



}
figure10.4 <- function() {
grid.rect(gp=gpar(col="gray"))
grid.text("hello", x=1/4)
grid.text("hello", x=2/4, 
          gp=gpar(fontfamily="serif"))
grid.text("hello", x=3/4, 
          gp=gpar(fontfamily="serif", fontface="bold"))


}
figure10.5 <- function() {
grid.rect(gp=gpar(col="gray"))
chars <- sprintf("\\#H%04d", 861:866)
chars
grid.text(chars, x=1:6/7, gp=gpar(fontfamily="HersheySans"))


}
figure10.6 <- function() {
flubber <- Type1Font("flubber", 
                     rep(system.file("extra", "flubber.afm",
                                     package="RGraphics"), 4),
                     encoding="WinAnsi.enc")
pdfFonts(flubber=flubber)
pdf("flubber.pdf", width=4.5, height=.5)
grid.rect(gp=gpar(col="gray"))
grid.text("hello", gp=gpar(fontfamily="flubber"))
dev.off()
embedFonts("flubber.pdf", outfile="flubber-embedded.pdf", 
           fontpaths=system.file("extra", package="RGraphics"))


}
figure10.7 <- function() {
cairo_pdf("cairo.pdf", width=4.5, height=.5)
grid.rect(gp=gpar(col="gray"))
grid.text("m\U0101ori", gp=gpar(fontfamily="Ubuntu"))
dev.off()


}
figure10.8 <- function() {

tikzDevice::tikz("params-tikz.tex", width=4.5, height=.5)
grid.rect(gp=gpar(col="grey"))
grid.text("hello \\TeX{} $\\sum_{i=1}^n x_i$")
dev.off()


}
figure10.9 <- function() {

sysfonts::font_add_google("Special Elite", "elite")
grid.rect(gp=gpar(col="grey"))
showtext::showtext_begin()
grid.text("hello", gp=gpar(fontfamily="elite"))
showtext::showtext_end()


}
figure10.10 <- function() {
cairo_pdf("cairo-faces.pdf", width=4.5, height=.5)
grid.rect(gp=gpar(col="grey"))
grid.text("hello", x=1/5, 
          gp=gpar(fontfamily="Ubuntu"))
grid.text("hello", x=2/5, 
          gp=gpar(fontfamily="Ubuntu", fontface="bold"))
grid.text("hello", x=3/5, 
          gp=gpar(fontfamily="Ubuntu", fontface="italic"))
grid.text("hello", x=4/5, 
          gp=gpar(fontfamily="Ubuntu Condensed"))
dev.off()


}
figure10.11 <- function() {
# placeholder


}
figure10.12 <- function() {
pdfFonts(CM=Type1Font("CM",
                      system.file("extra", 
                                  c("cmr10.afm",
                                    "cmbx10.afm",
                                    "cmsl10.afm",
                                    "cmbxsl10.afm",
                                    "cmsyase.afm"),
                                  package="RGraphics"),
           # Had to use special encoding file to force
           # char94 to be named "circumflex" not "asciicircum"
           # (bug in encoding loading in devPS.c ?)
                       encoding=system.file("extra", "myenc.enc",
                                            package="RGraphics")))
# Just for the temperature expression
# use default symbol font for the degree symbol
pdfFonts(CM2=Type1Font("CM2",
           system.file("extra",
                       c("cmr10.afm",
                         "cmbx10.afm",
                         "cmsl10.afm",
                         "cmbxsl10.afm",
                         "s050000l.afm"),
                       package="RGraphics")))



h <- 0.01
drawexpr <- function(expr, y, exprFamily="CM") {
  grid.text(paste("expression(", expr, ")", sep=""), .5, y-h, 
            just="top", gp=gpar(fontfamily="mono", cex=0.75))
  grid.text(parse(text=expr), .5, y+h, 
            just="bottom", gp=gpar(fontfamily=exprFamily))
}
drawexpr("z[i] == sqrt(x[i]^2 + y[i]^2)", 1/5)
drawexpr("hat(beta) == (X^t * X)^{-1} * X^t * y", 2/5)
drawexpr("bar(x) == sum(frac(x[i], n), i==1, n)", 3/5)
drawexpr("paste(\"Temperature (\", degree, \"C) in 2003\")", 4/5,
         exprFamily="CM2")
grid.rect(gp=gpar(col="gray", fill=NA))



}
figure10.13 <- function() {
grid.rect(gp=gpar(col="gray"))
grid.text(expression("We can make text "*
                     italic("emphasized")*
                     " or "*
                     bold("strong")))


}
figure2.1 <- function() {
plot(pressure)
plot(pressure$temperature, pressure$pressure)
plot(pressure ~ temperature, data=pressure)


}
figure2.2 <- function() {
par(mfrow=c(2, 2), cex=0.6, mar=c(4, 4, 1, 1))
plot(pressure, type="p")
plot(pressure, type="l")
plot(pressure, type="b")
plot(pressure, type="h")




}
figure2.3 <- function() {
  par(mfrow=c(2, 2), cex=0.6, mar=c(4, 4, 4, 2), mex=0.8)
  plot(lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings),
       id.n=1, cex.caption=0.8, which=1:4,
       panel=function(...) { panel.smooth(..., col.smooth="gray") })



}
figure2.4 <- function() {




subset <- sample(1:150, 20)
cS <- as.character(Sp <- iris$Species[subset])
cS[Sp == "setosa"] <- "S"
cS[Sp == "versicolor"] <- "V"
cS[Sp == "virginica"] <- "g"
ai <- cluster::agnes(iris[subset, 1:4])

par(mfrow=c(2, 1), cex=0.5, pty="s")
plot(ai, which=1, col=c("gray90", "gray"), labels = cS)
plot(ai, which=2, labels = cS)



}
figure2.5 <- function() {
plottitle <- function(plotfun, funarg, outer=FALSE, cex=.7, line=1) {
    ncp <- nchar(plotfun)
    nca <- nchar(funarg)
    mtext(paste(plotfun, "(", 
                paste(rep(" ", nca), collapse=""),
                ")", sep=""),
          family="mono", cex=cex, line=line, font=2, outer=outer)
    mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                funarg, " ", sep=""),
          family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
}
plot2title <- function(plotfun, funarg, 
                       extrafn, extraarg, 
                       outer=FALSE, cex=.7, line=.5) {
    ncp <- nchar(plotfun)
    nca <- nchar(funarg)
    ncep <- nchar(extrafn)
    ncea <- nchar(extraarg)
    mtext(paste(plotfun, 
                "(",  paste(rep(" ", nca), collapse=""),
                ")\n", 
                extrafn, "(",
                paste(rep(" ", ncea), collapse=""),
                ")", sep=""),
          family="mono", cex=cex, line=line, font=2, outer=outer)
    mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                funarg, " \n", 
                paste(rep(" ", ncep + 1), collapse=""),
                extraarg, " ", sep=""),
          family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
}
dohplot <- function(plotfn, ..., funarg, 
                    extrafn=NULL, extraarg=NULL, namefudge=FALSE,
                    main="", xlab="", ylab="", axes=FALSE, box=TRUE) {
    if (is.null(xlab) || is.null(ylab)) {
        do.call(plotfn,
                list(..., main=""))
    } else if (is.null(axes)) {
        do.call(plotfn,
                list(..., main="", xlab="", ylab=""))
    } else {
        do.call(plotfn,
                list(..., main="", axes=FALSE, xlab="", ylab=""))
    }
    if (is.null(extrafn)) {
        plottitle(plotfn, funarg)
    } else {
        plot2title(if (namefudge) paste(" ", plotfn, sep="") else plotfn, 
                   funarg, extrafn, extraarg)
    }
    if (box)
        box() # col="gray")
}



par(mfrow=c(3, 4), mar=c(1, 1, 3, 1), mex=.7, mgp=c(3, 100, 100))
dohplot("plot", (1:10)^2, funarg="numeric",
        xlim=c(0, 11), ylim=c(-10, 110))
dohplot("plot", table(rep(1:3, 1:3)), funarg="table", 
        lwd=2, xlim=c(0, 4), ylim=c(0, 4))
# Empty
plot.new()
plot.new()
dohplot("barplot", table(rep(1:3, 1:3)), funarg="", 
        extrafn="plot", extraarg="factor",
        xlim=c(-1, 5), ylim=c(0, 4), names.arg="")
dohplot("pie", c(1, 2, 4), funarg="", col=gray(1:3/4), cex=.7,
        labels="", axes=NULL)
dohplot("dotchart", 1:3, 
        funarg="numeric", pch=21, bg="gray", axes=NULL,
        lcolor="black", xlim=c(0, 4))
# Empty
plot.new()
dohplot("boxplot", (1:10)^2, funarg="numeric", 
        col="gray", ylim=c(-10, 110))
dohplot("hist", (1:100)^2, funarg="", col="gray", 
        breaks=6,
        xlim=c(-1000, 11000), ylim=c(0, 50))
dohplot("stripchart", (1:10)^2, funarg="numeric", 
        extrafn="plot", extraarg="~x",
        method="stack",
        cex=1, xlim=c(-10, 110), ylim=c(-1, 3), pch=21, bg="gray")
# stem()
plot.new()
txt <- capture.output(stem((1:10)^2))[-2]
text(.05, (1:length(txt))/(length(txt) + 1), txt, adj=0, family="mono", cex=.7)
box() # col="gray")
plottitle("stem", "")



}
figure2.6 <- function() {
plottitle <- function(plotfun, funarg, outer=FALSE, cex=.7, line=1) {
    ncp <- nchar(plotfun)
    nca <- nchar(funarg)
    mtext(paste(plotfun, "(", 
                paste(rep(" ", nca), collapse=""),
                ")", sep=""),
          family="mono", cex=cex, line=line, font=2, outer=outer)
    mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                funarg, " ", sep=""),
          family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
}
plot2title <- function(plotfun, funarg, 
                       extrafn, extraarg, 
                       outer=FALSE, cex=.7, line=.5) {
    ncp <- nchar(plotfun)
    nca <- nchar(funarg)
    ncep <- nchar(extrafn)
    ncea <- nchar(extraarg)
    mtext(paste(plotfun, 
                "(",  paste(rep(" ", nca), collapse=""),
                ")\n", 
                extrafn, "(",
                paste(rep(" ", ncea), collapse=""),
                ")", sep=""),
          family="mono", cex=cex, line=line, font=2, outer=outer)
    mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                funarg, " \n", 
                paste(rep(" ", ncep + 1), collapse=""),
                extraarg, " ", sep=""),
          family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
}
dohplot <- function(plotfn, ..., funarg, 
                    extrafn=NULL, extraarg=NULL, namefudge=FALSE,
                    main="", xlab="", ylab="", axes=FALSE, box=TRUE) {
    if (is.null(xlab) || is.null(ylab)) {
        do.call(plotfn,
                list(..., main=""))
    } else if (is.null(axes)) {
        do.call(plotfn,
                list(..., main="", xlab="", ylab=""))
    } else {
        do.call(plotfn,
                list(..., main="", axes=FALSE, xlab="", ylab=""))
    }
    if (is.null(extrafn)) {
        plottitle(plotfn, funarg)
    } else {
        plot2title(if (namefudge) paste(" ", plotfn, sep="") else plotfn, 
                   funarg, extrafn, extraarg)
    }
    if (box)
        box() # col="gray")
}



set.seed(1500)
# mgp draws the axes miles off the page
par(mfrow=c(4, 4), mar=c(1, 1, 3, 1), mex=.7, mgp=c(3, 100, 100))
dohplot("plot", 1:10, (1:10)^2, funarg="num,num", 
        pch=21, bg="gray", 
        xlim=c(0, 11), ylim=c(-10, 110))
x <- rnorm(10000)
dohplot("smoothScatter", x, x + rnorm(10000)/3,  funarg="",
        nbin=64, colramp=function(n) { gray(n:1/(n + 1)) },
        xlim=c(-5, 5), ylim=c(-5, 5))
x <- sample(1:4, 20, replace=TRUE)
y <- x + sample(0:1, 20, replace=TRUE)
dohplot("sunflowerplot", x, y,
        funarg="", seg.col="black", size=.07,
        xlim=c(0, 5), ylim=c(0, 6))
# Empty gap 
plot.new()
dohplot("boxplot", list((1:10)^2, 120 - (1:10)^2), funarg="list", 
        extrafn="plot", extraarg="fac,num", col="gray", boxwex=0.5,
        ylim=c(-10, 130))
dohplot("barplot", rbind(1:3, (1:3)^2), funarg="matrix",
        xlim=c(0, 4), ylim=c(0, 13))
dohplot("barplot", rbind(1:3, (1:3)^2), funarg="matrix",
        beside=TRUE,
        xlim=c(0, 10), ylim=c(0, 11))
# Empty gap for dotchart
plot.new()
fig <- par("fig")
dohplot("stripchart", list((1:10)^2, 140 - (1:10)^2), funarg="list",
        extrafn="plot", extraarg="num,fac",
        xlim=c(-10, 150), ylim=c(0, 3), pch=21, bg="gray", cex=1)
dohplot("spineplot", 
        rep(1:3, each=6), 
        factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3))),
        funarg="num,fac", box=FALSE)
dohplot("cdplot", 
        rep(1:3, each=6), 
        factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3))), 
        funarg="", axes=NULL, box=FALSE)
# Empty gap 
plot.new()
dohplot("spineplot", 
        factor(rep(1:3, each=6)), 
        factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3))),
        funarg="fac,fac", off=5,
        extrafn="plot", extraarg="fac,fac",
        namefudge=TRUE,
        box=FALSE)
dohplot("assocplot", 
        table(rep(1:2, each=3),
              c(rep(1:2, 1:2), rep(1:2, 2:1))),
        funarg="",
        col=c("black", "gray"), axes=NULL)
dohplot("fourfoldplot", 
        table(rep(1:2, each=3),
              c(rep(1:2, 1:2), rep(1:2, 2:1))),
        color=gray(1:2/3),
        # NOTE: can't make 'space' too small or font size of labels
        # goes to zero and get error from ghostscript
        funarg="", xlab=NULL, box=FALSE, space=0.03)
dohplot("mosaicplot", 
        table(factor(rep(1:3, each=6)), 
              factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3)))),
        funarg="", off=10, 
        extrafn="plot", extraarg="table", 
        cex.axis=.1, axes=NULL, box=FALSE)
# Put dotchart in gap
par(fig=c(fig[1] - .1, fig[2:4]), new=TRUE)
dotdata <- rbind(1:3, (1:3)^2) # rbind(table(gy), table(gx))
colnames(dotdata) <- c(" ", "  ", "   ")
dohplot("dotchart", dotdata, funarg="matrix",
        labels="", pch=c(16, 21), bg="gray",
        lcolor="black", xlim=c(0, 13), box=FALSE, gcolor=NA, axes=NULL)



}
figure2.7 <- function() {
plottitle <- function(plotfun, funarg, outer=FALSE, cex=.7, line=1) {
    ncp <- nchar(plotfun)
    nca <- nchar(funarg)
    mtext(paste(plotfun, "(", 
                paste(rep(" ", nca), collapse=""),
                ")", sep=""),
          family="mono", cex=cex, line=line, font=2, outer=outer)
    mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                funarg, " ", sep=""),
          family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
}
plot2title <- function(plotfun, funarg, 
                       extrafn, extraarg, 
                       outer=FALSE, cex=.7, line=.5) {
    ncp <- nchar(plotfun)
    nca <- nchar(funarg)
    ncep <- nchar(extrafn)
    ncea <- nchar(extraarg)
    mtext(paste(plotfun, 
                "(",  paste(rep(" ", nca), collapse=""),
                ")\n", 
                extrafn, "(",
                paste(rep(" ", ncea), collapse=""),
                ")", sep=""),
          family="mono", cex=cex, line=line, font=2, outer=outer)
    mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                funarg, " \n", 
                paste(rep(" ", ncep + 1), collapse=""),
                extraarg, " ", sep=""),
          family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
}
dohplot <- function(plotfn, ..., funarg, 
                    extrafn=NULL, extraarg=NULL, namefudge=FALSE,
                    main="", xlab="", ylab="", axes=FALSE, box=TRUE) {
    if (is.null(xlab) || is.null(ylab)) {
        do.call(plotfn,
                list(..., main=""))
    } else if (is.null(axes)) {
        do.call(plotfn,
                list(..., main="", xlab="", ylab=""))
    } else {
        do.call(plotfn,
                list(..., main="", axes=FALSE, xlab="", ylab=""))
    }
    if (is.null(extrafn)) {
        plottitle(plotfn, funarg)
    } else {
        plot2title(if (namefudge) paste(" ", plotfn, sep="") else plotfn, 
                   funarg, extrafn, extraarg)
    }
    if (box)
        box() # col="gray")
}



# mgp draws the axes miles off the page
par(mfrow=c(3, 4), mar=c(1, 1, 3, 1), mex=.7, mgp=c(3, 100, 100))
mdf <- cbind(3:6, (3:6)^2, (3:6)^3)
names(mdf) <- c("Y1", "Y2", "Y3")
aaa <- seq(0, pi, length=10)
xxx <- rep(aaa, 10)
yyy <- rep(aaa, each=10)
zzz <- sin(xxx) + sin(yyy)
# gap for pairs(matrix)
plot.new()
dohplot("matplot", mdf[order(mdf[, 1]), ], funarg="", 
        pch=21:23, bg=c("white", "black", "gray"), type="o",
        col="black", xlim=c(0, 6), ylim=c(-10, 230))
df <- USJudgeRatings[1:6, ]
rownames(df) <- NULL
dohplot("stars", df, funarg="", ncol=2, lwd=1,
        len=.8, col.stars=rep("gray", 13), mar=c(1, 1, 3, 1))
# gap
plot.new()
dohplot("image", matrix(zzz, ncol=10), funarg="", col=gray(1:12/13))
dohplot("contour", matrix(zzz, ncol=10), funarg="", 
        levels=seq(0, 2, .25), labcex=.4)
# gap for filled.contour(matrix)
plot.new()
dohplot("persp", matrix(zzz, ncol=10), funarg="",
        theta=30, phi=45, col="gray")
dohplot("symbols", xxx, yyy, funarg="",
        circles=zzz, inches=.03, axes=NULL)
# gap for coplot(y ~ x | g)
plot.new()
dohplot("mosaicplot", 
        table(factor(rep(1:3, each=6)), 
              factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3)))),
        funarg="", cex.axis=.1, off=10, axes=NULL,
        box=FALSE)



}
figure2.8 <- function() {
par(mfrow=c(2, 2), mar=c(2.5, 2, 1, 1), cex=0.6)
boxplot(decrease ~ treatment, data = OrchardSprays,
        log = "y", col="light gray")
boxplot(decrease ~ treatment, data = OrchardSprays,
        log = "y", col="light gray", 
        boxwex=0.5)

par(las=2, xpd=NA)
barplot(VADeaths[1:2,], angle = c(45, 135), 
        density = 20, col = "gray",
        names=c("RM", "RF", "UM", "UF"))
barplot(VADeaths[1:2,], angle = c(45, 135), 
        density = 20, col = "gray",
        names=c("RM", "RF", "UM", "UF"),
        horiz=TRUE)




}
figure2.9 <- function() {
par(mfrow=c(2, 2), mar=c(2, 2, 1, 1), cex=0.6)
y <- rnorm(20)
plot(y, type="l", lwd=3)
plot(y, type="l", col="gray")
plot(y, type="l", lty="dashed")
plot(y, type="l", ylim=c(-4, 4))




}
figure2.10 <- function() {
par(cex=.5)
plot(function(x) { 
         sin(x)/x 
     }, 
     from=-10*pi, to=10*pi, 
     xlab="", ylab="", n=500)




par(mfrow=c(1, 2))
par(mar=c(7, 0, 3, 1))
par(mex=0.7)

hc <- hclust(dist(USArrests), "ave")
dend1 <- as.dendrogram(hc)
dend2 <- cut(dend1, h=70)
par(cex=0.7)
par(mar=c(1, 0, 2, 8))
#  dend2$lower is *NOT* a dendrogram, but a list of .. :
plot(dend2$lower[[3]], 
  horiz = TRUE, type = "tr", axes=FALSE, cex=0.8)
par(mar=c(8, 0, 2, 0))
# "inner" and "leaf" edges in different type & color :
plot(dend2$lower[[2]], 
     edgePar = list(col=c("black", "gray")), edge.root=TRUE, 
     axes=FALSE, cex=0.8)



}
figure4.1 <- function() {

trellis.par.set(list(dot.symbol=list(pch=1)))
print(
xyplot(pressure ~ temperature, pressure)

)



}
figure4.2 <- function() {
tplot <- xyplot(pressure ~ temperature, pressure)




trellis.par.set(list(dot.symbol=list(pch=1)))
print(
xyplot(pressure ~ temperature, pressure,
       type="o", pch=16, lty="dashed", 
       main="Vapor Pressure of Mercury")


)



}
figure4.3 <- function() {
x <- 1:5
y <- 1:5
g <- factor(1:5)
types <- c("barchart", "bwplot", "densityplot", "dotplot",
           "histogram", "qqmath", "stripplot", "qq",
           "xyplot", "levelplot", "contourplot",
           "cloud", "wireframe", "splom", "parallelplot")
angle <- seq(0, 2*pi, length=19)[-19]
xx <- cos(angle)
yy <- sin(angle)
gg <- factor(rep(1:3, each=6))

aaa <- seq(0, pi, length=10)
xxx <- rep(aaa, 10)
yyy <- rep(aaa, each=10)
zzz <- sin(xxx) + sin(yyy)


doplot <- function(name, ...) {
  do.call(name, 
          list(..., scales=list(draw=FALSE), xlab=NULL, ylab=NULL,
               strip=function(which.panel, ...) { 
                       grid.rect(gp=gpar(fill="gray90")); grid.text(name) 
                     }))
}
plot <- vector("list", 15)
plot[[1]] <- doplot("barchart", y ~ g | 1)
plot[[2]] <- doplot("bwplot", yy ~ gg | 1, 
                    par.settings=list(box.umbrella=list(lwd=0.5)))
plot[[3]] <- doplot("densityplot", ~ yy | 1)
plot[[4]] <- doplot("dotplot", g ~ y | 1)
plot[[5]] <- doplot("histogram", ~ xx | 1)
plot[[6]] <- doplot("qqmath", ~ yy | 1)
plot[[7]] <- doplot("stripplot", yy ~ gg | 1)
plot[[8]] <- doplot("qq", gg ~ yy | rep(1, 18), subset=gg != 3)
plot[[9]] <- doplot("xyplot", xx ~ yy | 1)
plot[[10]] <- doplot("levelplot", zzz ~ xxx + yyy | 1, colorkey=FALSE)
plot[[11]] <- doplot("contourplot", zzz ~ xxx + yyy | 1, labels=FALSE, cuts=8)
plot[[12]] <- doplot("cloud", zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9, 
                     par.settings=list(box.3d=list(lwd=0.1)))
plot[[13]] <- doplot("wireframe", zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9,
                     drape=TRUE, par.settings=list(box.3d=list(lwd=0.1)),
                     colorkey=FALSE)
plot[[14]] <- doplot("splom", ~ data.frame(x=xx[1:10], y=yy[1:10]) | 1, 
                     pscales=0)
plot[[15]] <- doplot("parallelplot", ~ as.data.frame(split(yy, gg)) | 1)

grid.newpage()
pushViewport(viewport(layout=grid.layout(4, 4)))
for (i in 1:15) {
  pushViewport(viewport(layout.pos.col=((i - 1) %% 4) + 1,
                        layout.pos.row=((i - 1) %/% 4) + 1))
  print(plot[[i]], newpage=FALSE, 
        panel.width=list(1.025, "inches"),
        panel.height=list(1.025, "inches"))
  popViewport()
}
popViewport()
 


}
figure4.4 <- function() {

print(
xyplot(mpg ~ disp, data=mtcars)

)



}
figure4.5 <- function() {

trellis.par.set(list(dot.symbol=list(pch=1)))
trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
print(
xyplot(mpg ~ disp | factor(gear), data=mtcars)

)



}
figure4.6 <- function() {

trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
print(
xyplot(mpg ~ disp, data=mtcars,
       group=gear, 
       auto.key=list(space="right"),
       par.settings=list(superpose.symbol=list(pch=c(1, 3, 16),
                           fill="white")))

)



}
figure4.7 <- function() {

trellis.par.set(list(dot.symbol=list(pch=1)))
print(
xyplot(mpg ~ disp | factor(gear), data=mtcars,
       layout=c(1, 3), aspect=1)

)



}
figure4.8 <- function() {

trellis.par.set(list(fontsize=list(text=10)))
trellis.par.set(list(layout.widths=list(left.padding=0, right.padding=0, ylab.axis.padding=0, axis.right=0, key.ylab.padding=0)))
plot1 <- xyplot(mpg ~ disp, data=mtcars, 
                aspect=1, xlim=c(65, 480), ylim=c(9, 35),
                subset=gear == 5)
plot2 <- xyplot(mpg ~ disp, data=mtcars, 
                aspect=1, xlim=c(65, 480), ylim=c(9, 35),
                subset=gear == 4)
plot3 <- xyplot(mpg ~ disp, data=mtcars, 
                aspect=1, xlim=c(65, 480), ylim=c(9, 35),
                subset=gear == 3)
print(plot1, position=c(0, 2/3, 1, 1), more=TRUE)
print(plot2, position=c(0, 1/3, 1, 2/3), more=TRUE)
print(plot3, position=c(0, 0, 1, 1/3))




}
figure4.9 <- function() {

trellis.par.set(list(fontsize=list(text=10)))
print(
xyplot(mpg ~ disp | factor(gear), data=mtcars,
       layout=c(3, 1), aspect=1,
       scales=list(y=list(at=seq(10, 30, 10))),
       ylab="miles per gallon",
       xlab=expression(paste("displacement (in"^3, ")")))

)



}
figure4.10 <- function() {

trellis.par.set(list(fontsize=list(text=10)))
print(
xyplot(mpg ~ disp | factor(gear), data=mtcars,
       layout=c(3, 1), aspect=1,
       panel=function(...) {
           panel.xyplot(...)
           panel.abline(h=29, lty="dashed")
           panel.text(470, 29.5, "efficiency criterion",
                      adj=c(1, 0), cex=.7)
       })

)



}
figure4.11 <- function() {

trellis.par.set(list(fontsize=list(text=10)))
gray.colors <- function(n) { 
    adjustcolor(gray(n:1/n), alpha.f=.7) 
}
print(
xyplot(mpg ~ disp | factor(gear), data=mtcars,
       layout=c(3, 1), aspect=1,
       panel=function(x, y, ...) {
           panel.lmline(x, y)
           panel.xyplot(x, y, ...)
       })

)



}
figure4.12 <- function() {

trellis.par.set(list(fontsize=list(text=9, points=8)))
show.settings()



}
