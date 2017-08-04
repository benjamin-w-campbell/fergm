---
title: "cran-comments.md"
author: Benjamin W. Campbell
date: August 2, 2017
output: md_document
---

This is the initial submission of this package for consideration.  

## Test environments
* local OS X install, R 3.3.3
* win-builder (devel and release)


## R CMD check results
0 errors | 0 warnings | 1 notes

### Notes
This check includes one note that pertains to a globally undefined variable that is defined internally by ggplot2's geom_density layer.  

* checking R code for possible problems ... NOTE Undefined global functions or variables: ..scaled..

## Downstream dependencies
There are currently no downstream dependencies for this package. 
