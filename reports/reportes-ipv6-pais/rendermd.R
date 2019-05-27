library(rmarkdown)

args <- commandArgs(trailingOnly=T)
print(args)
theCC <- args[2]
theCName <- args[3]
theRDate <- "20170814"
theOutput <- "html_document"


rmarkdown::render('ipv6uptake-parametrized-v2.Rmd', params=list(CC=theCC, CName=theCName, RDate=theRDate, output=theOutput) )
