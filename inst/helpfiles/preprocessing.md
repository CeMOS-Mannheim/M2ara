### Preprocessing

------------------------------------------------------------------------

Preprocessing steps (except mono-isotopic peak filtering) are applied directly after `loading` the raw data and **before** the `curve fitting` is done.

This means you can change the settings here after loading your data and apply them by hitting the **Process Spectra**-Button without the need to reload you data!

Currently smoothing (*Savitzky-Golay*) and baseline subtraction (*Top-Hat*), mono-isotopic peak filtering (*Breen et al 2000*) as well as square root transformation (*Sqrt-Transform*) are implemented.

Smoothing removes small fluctuations in the data leading to a better peak-shape which makes baseline subtraction and peak picking easier.

Baseline subtraction estimates the baseline and then removes it from the spectra. This leads to a better comparability between individual spectra.

The square root transformation of the data stabilizes the variance to overcome the potential dependency of the variance from the mean.

Mono isotopic peak filter removes all peaks which are not part of an isotopic envelop and are not the M+0 peak.

The preprocessing is done in the following order:

1.  Sqrt-Transform
2.  Smoothing
3.  Baseline subtraction
4.  Mono isotopic peak filtering is applied later after average mass spectra were calculated

Note that the square root transformation might dramatically increase the informative value of potential multivariate methods (LASSO, PCA, clustering) as all this models will otherwise suffer from potential heteroscedasticity.
