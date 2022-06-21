---
title: "ISoP 2020 Survey Results Summary"
author: "Justin Wilkins/Tom Tensfeldt/Bill Denny"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  html_document:
    df_print: paged
    toc: yes
    toc_depth: '2'
  pdf_document:
    df_print: paged
    fig_caption: yes
    fig_height: 5
    fig_width: 5
    keep_tex: yes
    number_sections: yes
    toc: yes
    toc_depth: 2
header-includes: \usepackage{pdfpages}
---

```{r setup, echo=FALSE, error=FALSE, warning=FALSE, message=FALSE}
library(readr)
library(readxl)
library(jsonlite)
library(ggplot2)
#library(reshape2)
library(tidyverse)
library(glue)
library(gt)

# ------------------------------------------------
#' parse.reg
#' for a single un-named capture field from a regular expression
#'  return the character string representing the captured value.
#' parse regular expression result
#' @param res character string vector
#' @result result value from regexpr parse eval
#' @return dataframe with results of the captures, with named fields if possible
parse.reg <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  x <- attr(result, "capture.names")
  colnames(m) <- attr(result, "capture.names")
  m <- as.data.frame(m)
  return(m)
}

```

```{r readdata, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}

path <- file.path(getwd(), "survey2020/raw_data/")

maindatafile <- file.path(path, "main/ISoP Tools  Resources Survey.xlsx")
suppdatafile <- file.path(path, "supplementary/ISoP Tools  Resources Survey Redux.xlsx")

#' Read in main survey dataset  
main <- read_xlsx(maindatafile)

#' Read in supplimental survey dataset  
supp <- read_xlsx(suppdatafile)
head(supp)

#' Subset main dataset to provide a header  
d <- main %>% select("Respondent ID", "Start Date", "End Date", "What is your age?")

#' Rename Fields  
d <- d %>% rename(RespondentID="Respondent ID", StartDate="Start Date", EndDate="End Date", AgeRange="What is your age?")

#' Remove Empty Records from Response Category Field 
d <- d %>% filter(!is.na(RespondentID))

#' Add Question, Category and Response Fields  
d <- d %>% mutate(Question=NA, Category=NA, Response=NA)

#' Determine Response Groups  
ki <- grep("\\.\\.\\.(?<kindex>[0-9]+?)$", names(main), ignore.case=TRUE, perl=TRUE)

start=c(1, which(diff(ki) != 1 & diff(ki) != 0) + 1)
end=c(start - 1, length(ki))
nq=length(start)

ki[start]-1
ki[end]

head(as.data.frame(main), 3)
questionName <- names(main)[ki[start]-1]
response.categories <- as.data.frame(main)[1,]
qlist <- list()
for(i in 1:1) {
  cat(">> ", questionName[i], "\n")
  rrange <- c(ki[start][i]-1, ki[end][i])
  print(rrange)
  print(response.categories[rrange])
  x <- main %>% select(rrange)
  names(x) <- response.categories[rrange]
  # remove response category record
  x <- x %>% slice(2:n())
  # Add Question field
  x <- x %>% mutate(Question = questionName[i])
print(head(x))
  # Add Response Categories and Responses
  for(j in response.categories[rrange]) {
    x <- x %>% mutate(Category = j)
#    x <- d %>% bind_cols(x[,j])
  }
print(dim(x))
  print(x[1:3,])
  qlist[[questionName[i]]] <- x
}
head(d,3)

length(qlist)
dim(qlist[[1]])
dim(d)
qlist[[questionName[1]]]
questionName[1]
```{r nresponses, echo=FALSE}
nrespondees <- d %>% select(RespondentID) %>% distinct() %>% count()
```

#' There were `r nrespondees` respondents to the survey  

m <- as.data.frame(main)
head(m)
names(m)

#' Remove empty columns
main <- main %>% select(where(~ !(all(is.na(.)))))


#' Update Survey Categories in dataset  
x <- names(main)
k <- grep("^\\.\\.\\.[0-9]+?$", x, ignore.case=TRUE, perl=TRUE)
x[k]
k

j <- setdiff(1:ncol(main), k)
k
j
j<k
for (i in x[k]) {


}

# identify individual plot PNG files from input YAML
k <- grep("(.)+?_ind.png", names(pparams), ignore.case=TRUE, perl=TRUE)
if(length(k)>0) {
  individual_plot_files <- list()
  for(i in 1:length(pparams[k])) {
    x <- pparams[k][i]
    n <- names(x)
    individual_plot_files[[n]]$name  <- n
    individual_plot_files[[n]]$value <- unlist(unname(x))
    individual_plot_files[[n]]$ofile <- file.path(outputpath, individual_plot_files[[n]]$name)
    status <- file.copy(individual_plot_files[[n]]$value, individual_plot_files[[n]]$ofile)
  }
}

# identify any spaghetti plot PNG files for appendices
j <- grep("(.)+?.png", names(pparams), ignore.case=TRUE, perl=TRUE)
m <- grep("(.)+?parameters_boxplot.png", names(pparams), ignore.case=TRUE, perl=TRUE)
k <- c(k,m)
j <- setdiff(j, k) # ugly but for now
spaghetti_plot_files <- list()
if(length(j)>0) {
  for(i in 1:length(pparams[j])) {
    x <- pparams[j][i]
    n <- names(x)
    spaghetti_plot_files[[n]]$name <- n
    spaghetti_plot_files[[n]]$value <- unlist(unname(x))
    spaghetti_plot_files[[n]]$ofile <- file.path(outputpath, spaghetti_plot_files[[n]]$name)
    status <- file.copy(spaghetti_plot_files[[n]]$value, spaghetti_plot_files[[n]]$ofile)
  }
}

# identify qc report file for appendix
k <- grep("report.pdf", names(pparams), ignore.case=TRUE, perl=TRUE)
qc_report_file <- NA
if(length(k)>0) {
  qc_report_file <- pparams[k]
  ifile <- unlist(qc_report_file)
  ofile <- file.path(outputpath, names(ifile))
  zfile <- file.path('/home/docker/output', names(ifile))
  status <- file.copy(ifile, ofile)
  status <- file.copy(ifile, zfile)
}

# identify parameter plot PNG files from input YAML
k <- NA
k <- grep("(.)+?parameter(.)+?.png", names(pparams), ignore.case=TRUE, perl=TRUE)
parameter_plot_files <- list()
if(length(k)>0) {
  for(i in 1:length(pparams[k])) {
    x <- pparams[k][i]
    n <- names(x)
    parameter_plot_files[[n]]$name  <- n
    parameter_plot_files[[n]]$value <- unlist(unname(x))
    parameter_plot_files[[n]]$ofile <- file.path(outputpath, parameter_plot_files[[n]]$name)
    status <- file.copy(parameter_plot_files[[n]]$value, parameter_plot_files[[n]]$ofile)
  }
}


# set reporting parameters


```

