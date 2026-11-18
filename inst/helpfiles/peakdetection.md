### Peak detection

***

Peak detection is performed at two stages of the processing:

1. To perform ``single-point recalibration`` and ``alignment`` on the level of single spectra

2. After ``mean spectrum calculation`` as feature detection for the actual ``curve fitting``.

The ``S/N-ration`` (signal-to-noise) ratio is a measure of the strength of the desired signal relative to background noise (undesired signal). Usual values are 3 (default) or 5 if you wish to only include highly probable signals to the further analysis. Internally the so called ``super smoother``-algorithm is used to determine the noise level and intensities $$Intensity > SNratio * noise$$ are considered as peaks.

You can change the settings here after loading your data and apply them by hitting the **Process Spectra**-Button without the need to reload you data!
