# regLins

`regLins` is an R package for performing linear regression analysis with different optimization methods and provides a Shiny application for interactive analysis.

## Installation

### Prerequisites

Ensure you have R installed. You can download it from [CRAN](https://cran.r-project.org/).

### Install devtools Package

If you haven't already installed the `devtools` package, do so by running:

```r
install.packages("devtools")
```

### Install regLins from GitHub

You can install the `regLins` package directly from GitHub using the `devtools` package:

```r
library(devtools)
install_github("dzakwanalifi/regLins")
```

## Usage

### Linear Regression with Optimization

The `regLin` function allows you to perform linear regression using different optimization methods.

#### Example Usage

```r
library(regLins)

# Generate some example data
set.seed(123)
y <- rnorm(100)
X <- cbind(1, rnorm(100))

# Perform regression using the regLin function
model <- regLin(y, X, method = "kuadrat terkecil")

# Summarize the model
summary(model)
```

### Running the Shiny App

The `regLins` package includes a Shiny app for interactive analysis.

#### Running the Shiny App

```r
library(shiny)
library(regLins)

# Run the Shiny app
runApp(system.file("shinyApp", package = "regLins"))
```

#### Shiny App Interface

1. **Upload File**: Allows you to upload a CSV or Excel file.
2. **Select Variables**: Dropdown menus to select response and predictor variables.
3. **Choose Method**: Option to select the optimization method.
4. **Run Regression**: Button to run the regression analysis.
5. **Summary Tab**: Displays a summary of the regression model.
6. **Plot Tab**: Shows diagnostic plots of the regression model.

## Detailed Function Documentation

### regLin Function

Performs linear regression with optimization methods.

#### Usage

```r
regLin(y, X, method = "kuadrat terkecil")
```

#### Arguments

- `y`: A numeric vector of the response variable.
- `X`: A numeric matrix of predictor variables.
- `method`: The optimization method to be used ("kuadrat terkecil" or "kemungkinan").

#### Value

Returns an object of class `regLins`.

### summary Function

Provides a summary of the regression results.

#### Usage

```r
summary(object)
```

#### Arguments

- `object`: An object of class `regLins`.

### plot Function

Plots diagnostic plots for the regression model.

#### Usage

```r
plot(model)
```

#### Arguments

- `x`: An object of class `regLins`.
- `y`: Not used.

## Author

Muhammad Dzakwan Alifi  
dzakwan624@gmail.com

## License

This package is licensed under the GPL-3 License.

### Instructions for the User

1. **Clone the Repository**: If you want to work directly with the source code or make contributions, you can clone the repository:
   ```sh
   git clone https://github.com/dzakwanalifi/regLins.git
   ```

2. **Issues and Contributions**: If you encounter any issues or want to contribute to the development of this package, feel free to open an issue or submit a pull request on GitHub.
