# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    sudo \
    libudunits2-dev \
    librsvg2-dev \
    tk

# Frozen CRAN snapshot, matches pak.lock (see installer/README.md)
ENV R_SNAPSHOT=2026-11-17
WORKDIR /src

# --- dependencies ------------------------------------------------------------
# This layer only changes when the lockfile changes, so a code change does not
# reinstall the ~150 R packages (that took most of the image build time).
COPY req.txt pak.lock ./
COPY tools/ ./tools/

# The __linux__ repository serves precompiled binaries for the image's Ubuntu
# release; without it every package would be built from source.
RUN CODENAME=$(. /etc/os-release && echo "$VERSION_CODENAME") \
 && export R_SNAPSHOT_URL="https://packagemanager.posit.co/cran/__linux__/${CODENAME}/${R_SNAPSHOT}" \
 && echo "CRAN snapshot: ${R_SNAPSHOT_URL}" \
 && R -e "install.packages('pak', repos = Sys.getenv('R_SNAPSHOT_URL'))" \
 && R -e "install.packages('reshape2', repos = Sys.getenv('R_SNAPSHOT_URL'))" \
 && R -e "options(repos = c(CRAN = Sys.getenv('R_SNAPSHOT_URL'))); pak::pkg_install(readLines('req.txt'))"

# Install MALDIcellassay from the commit pinned in pak.lock
RUN CODENAME=$(. /etc/os-release && echo "$VERSION_CODENAME") \
 && export R_SNAPSHOT_URL="https://packagemanager.posit.co/cran/__linux__/${CODENAME}/${R_SNAPSHOT}" \
 && Rscript tools/install-maldi-from-lock.R

# --- the app package ---------------------------------------------------------
# Everything else (sources, tests, ...) changes often, so it is copied last.
COPY ./ /src
RUN R CMD INSTALL --no-multiarch .

# The app reads and writes settings.csv in the working directory
WORKDIR /app

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "M2ara::runApp(host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
