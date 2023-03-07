# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
	  libtiff-dev \
    libcurl4-openssl-dev \
	  libgeos-dev \
    libssl-dev \
	  cmake \
	  libharfbuzz-dev \
	  libfribidi-dev \
  	librsvg2-dev \
  	libudunits2-dev \
  	libgdal-dev \
  	libgeos-dev \
  	libproj-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## renv.lock file
COPY ./renv.lock ./renv.lock
## app folder
COPY ./ ./app

# install renv
RUN Rscript -e 'install.packages("renv")'

# set renv library path
ENV RENV_PATHS_LIBRARY renv/library

# restore packages
RUN Rscript -e 'renv::restore()'

## install main package
#RUN Rscript -e 'install.packages("devtools")'
#RUN Rscript -e 'devtools::install_github("CeMOS-Mannheim/MALDIcellassay")'

## expose port
#EXPOSE 3838

## run app on container start
#CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
