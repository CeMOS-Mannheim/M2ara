### Normalization

***

Normalization helps to ensure that spectra are comparable and to compensate for different effects (e.g. heterogeneity of the matrix deposition) leading to fluctuations of the intensity independent of biological (desired) differences. 

Normalization is applied to the single spectra before ``mean spectrum calculation`` right after ``single-point recalibration``. Different methods are implemented. Usually the best practice is to include an internal standard or to use a known biological signal that should be independent of the treatment (e.g. PC34:1, m/z 760.585, that is abundantly present in cell membranes). Which is why the default method is set to ``mz`` and uses the same ``m/z`` and ``Tol.`` as is used for ``single-point recalibration``. If no such signal is present in the samples other methods can be used.

All normalization methods work in a similar way in dividing all intensities of a spectrum by a normalization factor.

The following methods are implemented:

- ``mz``: The normalization factor is the intensity of a given ``m/z`` (within ``Tol.``, see above).
- ``TIC``: Total-ion-current, the normalization factor is the sum of all intensities for a given spectrum.
- ``PQN``: First performs TIC normalization, then calculates the global median spectrum that is used calculate the quotients of all intensities of each single spectrum to the global median spectrum. The sum of these quotients are used as normalization factor.
- ``median``: The median of the intensities per spectrum is used as normalization factor.
- ``none``: Perform no normalization (usually only make sense if the spectra are already normalized!).

You can change the settings here after loading your data and apply them by hitting the **Process Spectra**-Button without the need to reload you data!

