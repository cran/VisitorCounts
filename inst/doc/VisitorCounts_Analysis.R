## ---- warning=FALSE,message=FALSE---------------------------------------------
library(VisitorCounts)
data(park_visitation)

## -----------------------------------------------------------------------------
PARK1 <- park_visitation[which(park_visitation$park=="PARK"),]

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics("one.pdf")

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics("two.pdf")

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics("three.pdf")

