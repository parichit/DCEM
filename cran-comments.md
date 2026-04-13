## Test environments
* local OS X install, R 4.4.2
* platform: x86_64-apple-darwin20 
* arch: x86_64                      
* os: macOS Sonoma 14.6
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 3 note(s) during the devtools::check(args = c('--as-cran')) process.

* checking for hidden files and directories ... NOTE
  Found the following hidden files and directories:
    .travis.yml
    .github
  These were most likely included in error. See section ‘Package
  structure’ in the ‘Writing R Extensions’ manual.


* checking for future file timestamps ... NOTE
  unable to verify current time

* checking top-level files ... NOTE
  Non-standard files/directories found at top level:
    ‘CRAN-RELEASE’ ‘cran-comments.md’
    

## Downstream dependencies
I have also run R CMD check on downstream dependencies and all packages passed the test.
