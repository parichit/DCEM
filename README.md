
**Package Overview**

Implements the Expectation Maximisation Algorithm for clustering the multivariate and univariate datasets. There are two versions of EM implemented-EM* (converge faster by avoiding revisiting the data) and EM. For more details on EM\*, see the 'References' section below.  

The package has been tested with both real and simulated datasets. The package comes bundled with a dataset for demonstration (ionosphere_data.csv). More help about the package can be seen by typing `?DCEM` in the R console (after installing the package).

**Currently, data imputation is not supported and user has to handle the missing data before using the package.**


**Contact**

*For any Bug Fixes/Feature Update(s)*

[Parichit Sharma: parishar@iu.edu]

*For Reporting Issues*

[Issues](https://github.com/parichit/DCEM/issues)

*GitHub Repository Link*

[Github Repository](https://github.com/parichit/DCEM)
  
  
**Installation Instructions**

**_Installing from CRAN_**

```
install.packages(DCEM)
```

**_Installing from the Binary Package_**

```
install.packages(DCEM_2.0.0.tgz, repos = NULL, type="source")
```

**How to use the package (An Example: working with the default bundled dataset)**

- The DCEM package comes bundled with the ionosphere_data.csv for demonstration. Help about the dataset can be seen by typing `?ionosphere_data` in the R console. Additional details can be seen at the link [Ionosphere data](https://archive.ics.uci.edu/ml/datasets/ionosphere)

- To use this dataset, paste the following code into the R console.

```
ionosphere_data = read.csv2(
  file = paste(trimws(getwd()),"/data/","ionosphere_data.csv",sep = ""),
  sep = ",",
  header = FALSE,
  stringsAsFactors = FALSE
)
```

- **_Cleaning the data_**: Before the model can be trained (`dcem_train()` function), the data must be cleaned. This simply means to remove all redundant columns (example can be label column). This dataset contains labels in the last column (35th) and only 0's in the 2nd column so let's remove them,

Paste the below code in the R session to clean the dataset.

```
ionosphere_data =  trim_data("35,2", ionosphere_data)
```

- **_Clustering the data:_** The dcem_train() learns the parameters of the Gaussian(s) from the input data. It internally calls the `dcem_cluster_mv()`or `dcem_cluster_uv()` function for multivariate and univariate data respectively. These 
functions assign(s) the probabilistic weights to the sample(s) in the dataset. 

Paste the below code in the R session to call the dcem_train() function.

```
dcem_out = dcem_train(data = ionosphere_data, threshold = 0.0001, iteration_count = 50, num_clusters = 2)
```

- **_Accessing the output:_** The list returned by the `dcem_train()` is stored in the **_dcem_out_** object. It contains the parameters associated with the clusters (Gaussian(s)). These parameters are namely - posterior probabilities, meu, sigma and priors. Paste the following code in the R session to access any/all the output parameters. 

``` 
          [1] Posterior Probabilities: `**dcem_out$prob**`: A matrix of posterior-probabilities for the 
              points in the dataset.
              
          [2] Meu(s): `**dcem_out$meu**`
              
              For multivariate data: It is a matrix of meu(s). Each row in the  
              matrix corresponds to one meu.
              
              For univariate data: It is a vector if meu(s). Each element of the vector corresponds 
              to one meu.
              
          [3] Co-variance matrices 
          
              For multivariate data: `**dcem_out$sigma**`: List of co-variance matrices.
          
              For univariate data: `**dcem_out$sigma**`: Vector of standard deviation(s).
               
          [4] Priors: `**dcem_out$prior**`: A vector of prior.
```

*How to access the help (after installing the package)*

```
?dcem_star_train
?dcem_train
?dcem_test
?DCEM
```

