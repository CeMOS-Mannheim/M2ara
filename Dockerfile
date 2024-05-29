# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    sudo \
    libudunits2-dev \
    librsvg2-dev \
    tk

# Copy the req.txt file to the Docker image
COPY req.txt /req.txt

# Install R packages from req.txt
RUN R -e "packages <- readLines('/req.txt'); install.packages(packages)"

# Install the MALDIcellassay from GitHub
RUN R -e "pak::pkg_install('CeMOS-Mannheim/MALDIcellassay')"

## copy app folder into container
COPY ./ ./app

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
