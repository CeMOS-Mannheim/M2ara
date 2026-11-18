# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    sudo \
    libudunits2-dev \
    librsvg2-dev \
    tk

# Frozen CRAN snapshot, matches pak.lock (see installer/README.md)
ENV R_SNAPSHOT=2026-11-17

# Copy the repository; the app is installed as the M2ara package
COPY ./ /src
WORKDIR /src

# Install pak
RUN R -e "install.packages('pak', repos = paste0('https://packagemanager.posit.co/cran/', Sys.getenv('R_SNAPSHOT')))"

# Install reshape2 as it lead to errors with pak
RUN R -e "install.packages('reshape2', repos = paste0('https://packagemanager.posit.co/cran/', Sys.getenv('R_SNAPSHOT')))"

# Install R packages from req.txt (pinned versions)
RUN R -e "options(repos = c(CRAN = paste0('https://packagemanager.posit.co/cran/', Sys.getenv('R_SNAPSHOT')))); pak::pkg_install(readLines('req.txt'))"

# Install MALDIcellassay from the commit pinned in pak.lock
RUN Rscript tools/install-maldi-from-lock.R

# Install the M2ara package itself
RUN R CMD INSTALL --no-multiarch .

# The app reads and writes settings.csv in the working directory
WORKDIR /app

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "M2ara::runApp(host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
