# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    sudo \
    libudunits2-dev \
    librsvg2-dev \
    tk


## copy app folder into container
COPY ./ ./app

# install main package
RUN Rscript -e 'install.packages("devtools")'
RUN Rscript -e 'devtools::install_github("CeMOS-Mannheim/MALDIcellassay")'
RUN Rscript -e 'source("/app/install_packages.R")'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
