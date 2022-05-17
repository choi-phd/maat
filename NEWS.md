# maat 1.1.0

- now uses soft overlap control. See the vignette for details.
- now allows separate modules to be used for each test.
  - `loadModules()` now returns a 3-level list instead of a 2-level list for this.
  - all other functions are updated to work with 3-level module lists.
  - the example data `module_list_math` is updated to the new format.
- now allows separate configs from `createShadowTestConfig()` to be used for each module.
- `plot(output_maat)` now prints the plot for the first examinee by default.
- `examinee` objects gain a new `estimated_theta` slot to store assessment-level theta estimates.

# maat 1.0.2

* fixed to meet CRAN requirements

# maat 1.0.1

* fixed to meet CRAN requirements

# maat 1.0.0

* first release
