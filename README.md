# M²ara - MALDI MS Bioassays Evaluation and Classification App

M²ara is a software tool to facilitate the exploration of metabolomic responses in complex matrix-assisted laser desorption/ionization mass spectrometry (MALDI MS) bioassays. The app is intented for the evaluation of metabolomic drug actions by using the mass-to-charge ratios of hundreds of metabolites and it is particularly useful in defining novel pharmacodynamic biomarkers for high-throughput applications.

M²ara is based on the R package [MALDIcellassay](https://github.com/CeMOS-Mannheim/MALDIcellassay) and extends its capabilities with a GUI and adds helpful features like clustering of curves, PCA analysis as well as the Curve Response Score (CRS) which enables fast screening for molecules regulated by drug treatment. MALDIcellassay was originally published in [Unger et. al. 2021](https://www.nature.com/articles/s41596-021-00624-z) (Nature Protocols).

## How to use

This application simplifies the analysis of Molecular High Content Screening (MHCS) MALDI-TOF MS assay data and the evaluation of complex drug actions. After your data has been loaded, you can adjust settings as needed and start the processing. From here, you can analyze your data by selecting entries in the data table, visually inspect and rank mass features using the Curve Response Score (CRS) fingerprints, and save the curve fit and peak profile of your chosen m/z value. 

This app is specifically designed for use with Bruker flex series raw data but also features support for mzML.

For more detailed information please take a look at the [Manual](manual.md) that is also available inside the app.

## How to install 

### R

Clone the GitHub repository to your local machine (please make sure to have R installed) and start the app by sourcing the `app.R` file.

```bash
git clone https://github.com/CeMOS-Mannheim/M2ara.git
```

```R
source("app.R")
```

### Docker
Install the docker container, run it and access `localhost:3838` to interact with the app.
Don't forget to mount your data when running the container!

```bash
docker pull thomasenzlein/m2ara:lastest
docker run -p 3838:3838 -v  c:/path/to/massSpecData:/mnt thomasenzlein/m2ara:lastest
```

### Stand-alone installer for Windows
Use the stand-alone installer (Windows only, no R installation needed).
The installer can be downloaded [here](https://github.com/CeMOS-Mannheim/M2ara/releases/download/1.0/MALDIcellassay_1.0.exe).

## Example data
To test the app please use the example data on [FigShare](https://dx.doi.org/10.6084/m9.figshare.25736541) following the provided information regarding settings. 

