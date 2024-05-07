# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:latest

# copy necessary files
## renv.lock file
#COPY ./renv.lock ./renv.lock
## app folder
COPY ./ ./app

# install renv
#RUN Rscript -e 'install.packages("renv")'

# set renv library path
#ENV RENV_PATHS_LIBRARY renv/library

# restore packages
#RUN Rscript -e 'renv::restore()'

# install main package
RUN Rscript -e 'install.packages("devtools")'
RUN Rscript -e 'devtools::install_github("CeMOS-Mannheim/MALDIcellassay")'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
