FROM rocker/r-ver:3.5.1


## The MAINTAINER instruction sets the Author field of the generated images
MAINTAINER author@sample.com
## DO NOT EDIT THESE 3 lines
RUN mkdir /physionet2019
COPY ./ /physionet2019
WORKDIR /physionet2019

## Install your dependencies here using apt-get etc.


RUN R -e "install.packages('xgboost',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('matrixStats',dependencies=TRUE, repos='http://cran.rstudio.com/')"
