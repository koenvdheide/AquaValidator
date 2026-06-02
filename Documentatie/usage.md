# AquaValidator usage guide

AquaValidator helps an analyst review and validate water-quality laboratory results. This guide describes the review workflow. For installation and how to launch the app, see the main README.

## Loading data

The app reads an Excel export of laboratory results. On load, the results are split into a validation list ("fiatteerlijst") and the full result set.

## The validation list

The validation list shows the samples awaiting review. Result cells are colour-coded:

- Red: the result falls outside its expected range and needs review.
- Green: a cancelled result.
- Yellow: lab numbers of samples that are on the validation list.
- Grey: other lab numbers.

Selecting "show more samples of the selected measurement point" adds the most recent samples for the same measurement point (10 by default, configurable in settings), so a flagged sample can be compared with its recent history.

## Result and ratio graphs

For the selected sample or samples, the app plots results and calculated ratios over time:

- The x-axis is the sampling date and the y-axis is the test result. The x-axis stays fixed across plots so series are comparable.
- Triangles mark out-of-range results. Bold, larger points belong to the sample you clicked.
- Ratio plots highlight the current sample's ratios in bold. They have no out-of-range triangles, because a clear out-of-range rule for a ratio is not defined.

All plots are interactive: double-click a point to see which results belong to a sample or a ratio.

## Validating

You can approve samples from any screen; you do not have to return to the validation list first. After an action, the app loads the next sample automatically.

Individual test results can be marked as duplicate or cancelled. This is done per result, in the results table, in the layout where each result has its own row. Results are selected the same way as samples.

## Exporting

Validated, cancelled, and duplicate results, together with any comments you added, are written to a CSV file. The output location is configurable through the `AQUAVALIDATOR_EXPORT_PATH` environment variable (see the README).
