# Calibration Scripts

This folder contains scripts used for the calibration of the 2BURP project. Below is a brief description of each script and its purpose.
A complete description of the process is available in the report [(Van der Wielen & Koomen, 2024)](http://dx.doi.org/10.13140/RG.2.2.26491.53286)

## Scripts

### `00-calibration_main.R`

Main script for the calibration process. It sources the required packages and sources the other scripts in the correct order.

### `01-data_processing.R`

Loads the raw data and processes it to prepare it for calibration.

### `02-variable_selection.R`

Handles the selection of variables to be fed to the calibration model.

### `03-caret_predicitive_models.R`

Runs the calibration model using the selected variables and evaluates the model's performance.

### `04-results_table.R`

Combines the results of the calibration model into more legible tables.

## Contact

For any questions or issues, please contact t.j.a.van.der.wielen@vu.nl.
