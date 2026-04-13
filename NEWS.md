# DCEM 2.0.6 (2026-02-15)

## Bug Fixes
* Reformatted NEWS.md to comply with CommonMark and CRAN requirements.
* Incremented version to resolve archival status.

# DCEM 2.0.5

* DCEM is published in SoftwareX: <https://doi.org/10.1016/j.softx.2021.100944>. Use `citation("DCEM")` to cite the package.
* Added the functionality to predict the cluster membership for test data.
* Fixed a minor bug in co-variance calculation during maximization.

# DCEM 2.0.4

* Added the option to get data membership (maximum posterior probability) from the output.

# DCEM 2.0.3

* Added quick start examples and use cases in the vignettes.

# DCEM 2.0.2

* Added the option to get cluster membership of data directly from the output.
* Patched the code for K-Means++ based initialization.

# DCEM 2.0.1

## Bug Fixes
* Removed the usage of floor in integer division in CPP code to resolve Solaris OS warnings.

# DCEM 2.0.0

* This is the fourth major release of the DCEM package.
* Improves the EM* implementation for faster execution based on Kurban et al. (2016) <https://doi.org/10.1007/s41060-017-0062-1>.
* Supports both EM* and traditional EM algorithm for speed-up comparison.

# DCEM 1.0.0

* Implements the EM* algorithm leveraging a heap structure to expedite execution time.

# DCEM 0.0.2

* Implements improved initialization schemes based on K-means++ (Arthur and Vassilvitskii).

# DCEM 0.0.1

* Initial stable release. 
* Support for clustering multivariate and univariate data for finite Gaussian mixture models.
