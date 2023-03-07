# Base image
FROM tenzlein/maldicellassay_shiny_base:latest

# install main package
RUN Rscript -e 'install.packages("devtools")'
RUN Rscript -e 'devtools::install_github("CeMOS-Mannheim/MALDIcellassay")'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
