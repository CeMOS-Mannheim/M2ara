### Aligment

***

To improve mass accuracy and signal strength (``mean spectrum calculation`` leads to higher signals if peaks are well aligned) a ``alignment`` may be performed. It assumes that similar peaks can be found in each spectrum. It takes place after ``single-point-recalibration`` and before ``mean spectrum calculation``. The following steps are involved:

1. A global ``mean spectrum`` is calculated as reference spectrum and the ```peak detection`` is performed on this spectrum.

2. For each peak in each single spectrum a corresponding peak in the reference spectrum is searched (within ``align tol.``).

3. For each single spectrum a linear fit is performed using the m/z values of the peaks from the single spectrum and the reference.

4. The linear function is applied to the m/z-axis of the single spectra to improve the ``alignment``.

You can change the settings here after loading your data and apply them by hitting the **Process Spectra**-Button without the need to reload you data!

### Binning

***

To compensate for small residual differences ``peak binning`` is performed as last step before ``curve fitting``.

The ```bin tol.`` is the maximal relative deviation of a peak position (m/z) to be considered as identical.

You can change the settings here after loading your data and apply them by hitting the **Process Spectra**-Button without the need to reload you data!
