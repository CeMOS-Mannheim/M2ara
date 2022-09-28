### Preprocessing

***

Preprocessing steps are applied directly after ``loading`` the raw data and **before** the ``curve fitting`` is done.

This means you can change the settings here after loading your data and apply them by hitting the **Process Spectra**-Button without the need to reload you data!

Currently smoothing (*Savitzky-Golay*) and baseline subtraction (*Top-Hat*) are implemented.

If both preprocessing steps are selected, smoothing is applied first followed by the baseline removal.

