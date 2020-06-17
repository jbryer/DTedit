---
title: "NEWS"
author: "David Fong"
date: "13th June 2020"
output: html_document
---

# DTedit 2.1.0
16th June 2020

## Changes (breaking)

* `dtedit`/`dteditmod` returns `reactiveValues`
  + `$thedata`, `$view.cols`, `$edit.cols`, `$edit.count`
  + compatible with `DTedit` version 1.0.0 but *not* compatible with versions 2.0.0/2.0.1

## New features

* Add parameters for text on save/cancel buttons and delete modal dialog
  + `label.save`, `label.cancel` and `text.delete.modal`
  + written by László Szakács (@cocinerox)

# DTedit 2.0.1
14th June 2020

## New features

* Vignette. Vignette also available on [RPubs](https://rpubs.com/DavidFong/DTedit).

## Improvements

* Passes checks with `devtools::check()` and [Travis CI](https://travis-ci.org/DavidPatShuiFong/DTedit).
* Code testing and coverage with `testthat`, `shinytest`, `covr` and [codecov](https://codecov.io/gh/DavidPatShuiFong/DTedit).
* Multiple examples, demonstrations and testing shiny applications.
  + Many accessible through `dtedit_*_demo` and `dtedit_test`
* Improvements in handling of input choices for `selectInputMultiple`

## Bux fixes

* inserting a new row failed with particular variable classes of columns - FIXED

# DTedit 2.0.0
7th June 2020

## New features

* Modularized version : `dteditmod` and `dteditmodUI`
* `thedata` can be a reactive, in which case `dtedit`'s copy of `thedata` will change when the reactive changes.
* `selectInputReactive` and `selectInputMultipleReactive` - 'reactive' versions of `selectInput` and `selectInputMultiple`. The valid options can dynamically change through reactivity.
* blob/raw columns which can be edited with the `fileInput` input type.
* action buttons with `action.buttons` option. Associated `callback.actionButtion` callback option.

## Changes

* `dtedit` and `dteditmod` return a list with `$thedata` and `$edit.count`.
  + `$view.cols` and `$edit.cols` are *not* returned.
* `suspendWhenHidden` is set to `FALSE`, allowing changes to be rendered when the table is not visible.

## Improvements

* add error handling to the delete modal defined in `callback.delete` option.
* `...` options to be passed to `DT::renderDataTable`
* many more examples

## Bug fixes

* `thedata` dataframe did not work if there was only one column - FIXED
* data columns can be delete during `updateData` function call if a column is empty - FIXED
* erroneous errors under certain conditions of choices for `selectInputMultiple` - FIXED
* module version `selectInput` did not show previously chosen option - FIXED (Klodian Dhana) (#1, @klodiandhana) (#febc655)

