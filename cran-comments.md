# maat 1.0.1

This is a resubmission of maat 1.0.0.

```
Please do not start the description with "This package", package name,
title or similar.
```

- Updated DESCRIPTION to not start with package name.

```
Please always explain all acronyms in the description text. e.g. CAT
```

- Updated DESCRIPTION to explain acronyms.

```
If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")
```

- We do not have references to add.

```
Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
  boundGrade.Rd: \value
  getItemExposureRate.Rd: \value
  getItemNamesPerGrade.Rd: \value
  plotModuleRoutes.Rd: \value
  simExaminees.Rd: \value
  simTheta.Rd: \value
```

- Added descriptions for returned values for all listed functions.
- `plotModuleRoutes()` is now `plot()`.

## Test environments

* Local:
* * Windows 10 (R 4.1.0)
* * Ubuntu 20.04 (R 4.1.0)
* GitHub Actions:
* * Windows Server 2019 (R-release)
* * macOS Catalina 10.15 (R-release)
* * Ubuntu 20.04 (R-release, R-devel, R-oldrel)

## R CMD check results

```
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```
