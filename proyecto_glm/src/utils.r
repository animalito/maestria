library(tidyr)
library(Hmisc)
library(plyr)
library(xtable)
library(scales)
library(dplyr)
library(plyr)
library(FactoMineR)
library(data.table)
library(foreign)
library(tidyr)
library(RPostgreSQL)
library(yaml)
library(futile.logger)
library(reshape)
library(caret)
library(MASS)
library(ellipse)
library(seriation)
library(ggplot2)
library(portfolio)
library(yaml)
library(lubridate)
library(RPostgreSQL)
library(futile.logger)
library(gridExtra)
library(pander)
library(stringr)
library(data.table)
library(foreign)
library(tidyr)
library(RPostgreSQL)
library(yaml)
library(futile.logger)
library(reshape)
library(plyr)
library(dplyr)
library(xtable)
library(glmnet)
library(lars)
library(colorspace)
library(plotrix)
library(seriation)
library(MASS)
library(car)
library(pls)
library(knitr)
library(grid)

temac<-theme(axis.text = element_text(colour = "black", size=12),
             axis.text.x= element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             axis.ticks.x = element_blank(),
             panel.background = element_rect(fill="white"),
             panel.grid.minor.y = element_line(size=.05),
             panel.grid.major = element_blank(),
             plot.background = element_rect(fill="white"),
             legend.position="right",
             legend.direction="vertical",
             legend.text=element_text(size=10),
             legend.key.width=unit(.5,"cm"),
             legend.background=element_rect(size=.5),
             legend.justification = c(0,1),
             legend.box="horizontal"
)

temac2<-theme(axis.text = element_text(colour = "black", size=12),
              axis.text.x= element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.background = element_rect(fill="white"),
              panel.grid.minor.y = element_line(size=.05),
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill="white"),
              legend.position="bottom",
              legend.direction="vertical",
              legend.text=element_text(size=10),
              legend.key.width=unit(.5,"cm"),
              legend.background=element_rect(size=.5),
              legend.justification = c(0,1),
              legend.box="horizontal"
)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}