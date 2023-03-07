### Variance filtering

------------------------------------------------------------------------

After the `binning` of the peaks from the mean spectra a intensity matrix is composed of all signal of each mean spectrum (= sample/concentration). Using this matrix the variance of each unique m/z value across all concentrations is calculated. The signals might then be `variance filtered` before the curve fitting. This assumes that signals of interest are those that show a high variance. This assumption might be violated which is why care should be taken as otherwise potential biomarkers might not show up in the results. Per default no filtering is done (method `none`). If many signals are present (because of high resolution data) this function might help to get an overview.

The following methods are implemented:

-   `mean`: Signals above the mean variance of all signals are fitted.
-   `q75`: Signals above the 75%-quartile of the variance of all signals are fitted.
-   `median`: Signals above the median (50%-quartile) variance of all signals are fitted.
-   `q25`: Signals above the 25%-quartile of the variance of all signals are fitted.
-   `none`: No filtering.

Note that although a less aggressive filtering might reveal additional (lower variance) biomarkers, it also greatly increases the overall number of features from which most do not contain useful information. Still this will lead to more flexibility in the applied multivariate models (Clustering, LASSO, PCA) increasing the risk of overfitting.

You can change the settings here after loading your data and apply them by hitting the **Process Spectra**-Button without the need to reload you data!
