### Preprocessing

------------------------------------------------------------------------

Preprocessing steps are applied directly after `loading` the raw data and **before** the `curve fitting` is done.

This means you can change the settings here after loading your data and apply them by hitting the **Process Spectra**-Button without the need to reload you data!

Currently smoothing (*Savitzky-Golay*) and baseline subtraction (*Top-Hat*) as well as square root transformation (*Sqrt-Transform*) are implemented.

The square root transformation of the data stabilizes the variance to overcome the potential dependency of the variance from the mean.

Smoothing is removes small fluctuations in the data leading to a better peak-shape which makes baseline subtraction and peak picking easier.

Baseline subtraction estimates the baseline and then removes it from the spectra. This leads to a better comparability between individual spectra.

The preprocessing is done in the following order:

1.  Sqrt-Transfrom
2.  Smoothing
3.  Baseline subtraction

Note that the square root transformation might dramatically increase the informative value of potential multivariate methods (LASSO, PCA, clustering) as all this models will otherwise suffer from potential heteroscedasticity.
