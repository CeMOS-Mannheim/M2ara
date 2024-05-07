load_spectra <- function(dir) {
  # we need the following information:
  # 1. the spectra themself
  # 2. the concentration of the spectrum
  # 3. the spot (e.g. A1) of the spectrum on the target plate

  # currently 2. read from the file- (or folder-) name of the spectra
  # and 3. from the meta data of the spectra.
  # When we support mzML we can force the user to create a Bruker Flex like
  # folder structure (but maybe we shouldnt) but we cant rely on the spot information being in the
  # meta data.

  # We could instead have a .txt-file where meta data for conc. and spot are linked to a file name.

  # Additionally, we only support continous spectra at the moment
  # so we need to detect the type of spectrum here and throw an error if
  # centroided spectra are provided. This can (and should) be changed later
  # so that also centroided spectra can be processed by the app.


}
