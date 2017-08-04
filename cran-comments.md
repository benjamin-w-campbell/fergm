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
0 errors | 0 warnings | 2 notes

### Notes
This check includes two notes.

 Authors@R field gives more than one person with maintainer role

  * All listed authors are creators of the package, Benjamin W. Campbell is the corresponding maintainer.  

 checking R code for possible problems ... NOTE Undefined global functions or variables: ..scaled..

  * This underfined global variable is locally defined by ggplot2.
  
## Downstream dependencies
There are currently no downstream dependencies for this package. 
