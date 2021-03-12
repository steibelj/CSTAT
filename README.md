# Bayesian Inference using Stan: CSTAT workshop

## How to install stan

### Install rstudio, R and Rtools 
https://rstudio.com/products/rstudio/download/

https://cran.r-project.org/

https://cran.r-project.org/bin/windows/Rtools/

Note for those working with MacOS: please see suggestions below.
### Install rstan
https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

## Useful links

### Stan is the programming language that we will use to implement MCMC Bayesian inference
https://mc-stan.org/

### Rstan is the r-interfase to the stan programming language
http://mc-stan.org/rstan/

#### Several packages in R are part of the stan ecosystem
##### bayesplot
https://mc-stan.org/bayesplot/
##### rtanarm
https://mc-stan.org/rstanarm/
##### shinystan
https://mc-stan.org/shinystan/

### Installing compile tools in MacOS
Connie Rojas, a workshop participant, sent the following instructions for installing the compile tools (and stan) in macOS. Thanks for your contribution!!!

1) Download Xcode from Mac App store
Xcode (Developer Tools)
Open it and make note of the version (mines was v 12.4)

2) Download Xcode Command Line tools

https://developer.apple.com/download/more/?=command%20line%20tools 

I downloaded “Command Line Tools for Xcode 12.4” to match my Xcode version 

Download and run Installer. 

3) Download the gfortran version for your specific operating system
https://github.com/fxcoudert/gfortran-for-macOS/releases 
I have macOS BigSur v 11.1, so I downloaded the .dmg file under the "gfortran 10.2 for Big Sur (macOS 11), for Intel processors" section

Run the Installer

4) Now, follow the instructions for the Installation of R Stan 
https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started 
