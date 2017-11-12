##~ '.panel_legend' <- function(items,pos="bottomright",x,y,cex=1.5,pch=22
                           ##~ ,box.col="transparent",bg="#FFFFFFAF"
                           ##~ ,fill=NULL,lty,lwd,angle=45,density=NULL,bty="o"
                           ##~ ,box.lwd=par("lwd"),box.lty=par("lty")
                           ##~ ,xjust=0,yjust=1,x.intersp=1,y.intersp=1
                           ##~ ,adj=c(0,0.5),text.width=NULL,text.col=par("col")
                           ##~ ,text.font=NULL,merge=TRUE,trace=FALSE
                           ##~ ,plot=TRUE,ncol=1,horiz=FALSE,title=NULL
                           ##~ ,inset=0,xpd,title.col=text.col,title.adj=0.5
                           ##~ ,seg.len=2)
'.panel_legend' <- function(items,...)
{
   if (.skipPlot(TRUE))
      return(NULL)
   arglist <- as.list(args(legend))
  # str(items)
  ## see args(legend) 
   lname <- names(items)
   iname <- sapply(items,function(x) x$name)
   if (is.null(lname))
      lname <- iname
   else if (length(ind <- which(!nchar(lname))))
      lname[ind] <- iname[ind]
   arglist[["legend"]] <- lname
   arglist[["pch"]] <- unname(sapply(items,function(x) x$pch))
  # arglist[["pt.cex"]] <- 5*unname(sapply(items,function(x) x$cex))
   arglist[["pt.bg"]] <- unname(sapply(items,function(x) x$col))
  # arglist[["col"]] <- unname(sapply(items,function(x) x$border))
   arglist[["pt.lwd"]] <- unname(sapply(items,function(x) x$lwd))
  # arglist[["lwd"]] <- arglist[["pt.lwd"]]
   if (!nchar(as.character(arglist[["x"]])))
      arglist[["x"]] <- "bottomright"
   arglist[["merge"]] <- FALSE ## '$merge : language do.lines && has.pch'
   arglist[["cex"]] <- arglist[["cex"]]*1.5
   if (is.language(arglist[["box.lwd"]]))
      arglist[["box.lwd"]] <- 0.1
   arglist[["border"]] <- unname(sapply(items,function(x) x$border))
   arglist[["fill"]] <- arglist[["pt.bg"]]
  # arglist[["angle"]] <- c(45,-45)
  # arglist[["density"]] <- 25
   if (is.language(arglist[["pt.cex"]]))
      arglist[["pt.cex"]] <- 1e-6*arglist[["cex"]]
   ##~ ret <- legend(x=pos,y=y,cex=cex,box.col=box.col,bg=bg
                ##~ ,legend=lname
                ##~ ,pch=sapply(items,function(x) x$pch)
                ##~ ,pt.cex=sapply(items,function(x) x$cex)
                ##~ ,pt.bg=sapply(items,function(x) x$col)
                ##~ ,col=sapply(items,function(x) x$border)
                ##~ ,pt.lwd=sapply(items,function(x) x$lwd)
               ##~ # ,pt.lty=sapply(items,function(x) x$lty)
                ##~ ,fill=fill,lty=lty,lwd=lwd,angle=angle,density=density,bty=bty
                           ##~ ,box.lwd=box.lwd,box.lty=box.lty
                           ##~ ,xjust=xjust,yjust=yjust
                           ##~ ,x.intersp=x.intersp,y.intersp=y.intersp
                           ##~ ,adj=adj,text.width=text.width,text.col=text.col
                           ##~ ,text.font=text.font,merge=merge,trace=trace
                           ##~ ,plot=plot,ncol=ncol,horiz=horiz,title=title
                           ##~ ,inset=inset,xpd=xpd,title.col=title.col
                           ##~ ,title.adj=title.adj,seg.len=seg.len
                ##~ )
  # str(arglist)
   ret <- do.call("legend",arglist)
   invisible(ret)
}
