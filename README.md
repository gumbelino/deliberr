# deliberr: Deliberation in R

## Overview

`deliberr` is an R package designed for analyzing deliberation data in political science and communication research. The package provides tools for measuring and evaluating deliberative quality, including the **Deliberative Reasoning Index (DRI)** and other metrics commonly used in deliberation studies.

Whether you're analyzing focus groups, town halls, deliberative polls, or online discussions, `deliberr` helps you assess the quality of deliberative discourse with both quantitative metrics and visual outputs.

## Key Features

- **Deliberative Reasoning Index (DRI)** calculation
- Statistical analysis tools for deliberation data
- Data visualization capabilities for deliberative metrics
- User-friendly graphical interface (no coding required!)
- Support for multiple data formats (CSV, Excel, etc.)

## Installation

### Requirements

- R version 3.5 or higher
- Internet connection for package installation

### Install from GitHub

You can install the development version of `deliberr` directly from GitHub using the `devtools` package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install deliberr
devtools::install_github("gumbelino/deliberr")
```

### Load the Package

After installation, load the package:

```r
library(deliberr)
```

## Usage

### For Non-Programmers: Graphical Interface

**You don't need to know how to code to use deliberr!** The package includes a user-friendly graphical interface that you can access in two ways:

**Option 1: From R**

```r
library(deliberr)
open_gui()
```

**Option 2: Web Browser (No R Installation Required)**

Visit: [https://gumbelin.shinyapps.io/deliberr/](https://gumbelin.shinyapps.io/deliberr/)

The graphical interface allows you to:

- Upload your data files
- Select variables for analysis
- Calculate deliberation metrics
- Generate visualizations
- Export results

### For R Users: Code-Based Analysis

If you're comfortable with R, you can use the package functions directly in your scripts. Documentation for all functions is available within R:

```r
# View package documentation
?deliberr

# Get help on specific functions
?dri  # Example: Help for DRI calculation
```

## Getting Started Guide for Researchers

### Step 1: Prepare Your Data

- Organize your deliberation data in a spreadsheet (Excel or CSV)
- Ensure each row represents an observation (e.g., a speaker turn, a participant)
- Include columns for relevant variables (e.g., speaker ID, statement content, coded categories)

### Step 2: Open the Interface

- If using R: Install the package and run `open_gui()`
- If using web browser: Go to https://gumbelin.shinyapps.io/deliberr/

### Step 3: Upload and Analyze

- Upload your data file through the interface
- Follow the on-screen instructions to select variables
- Run your analysis and view results
- Download outputs for your research

## Support and Documentation

### Getting Help

- **Package documentation**: Type `?deliberr` in R console
- **Function help**: Type `?function_name` (e.g., `?open_gui`)
- **Issues and bugs**: See "Reporting Bugs" section below

### Reporting Bugs

If you encounter any issues or bugs while using `deliberr`, please report them on GitHub:

1. Go to: https://github.com/gumbelino/deliberr/issues
2. Click "New Issue"
3. Provide a clear description of:
   - What you were trying to do
   - What happened (error messages, unexpected behavior)
   - Your R version (type `R.version.string` in R console)
   - Your package version (type `packageVersion("deliberr")` in R console)

For questions about usage or interpretation of results, please open a discussion on the GitHub repository.

## License

This package is licensed under the MIT License. See the LICENSE file for details.

## Author

**Gustavo Kreia Umbelino**  
Email: gumbelin@gmail.com

## Dependencies

The package automatically installs all required dependencies, including:

- dplyr, ggplot2, tidyr (data manipulation and visualization)
- shiny, DT (graphical interface)
- stats, rstatix, psych (statistical functions)
- And other supporting packages

You don't need to install these separately.

## Acknowledgments

Thank you to all researchers and practitioners who have contributed feedback and suggestions to improve this package.

---

**Questions?** Open an issue on GitHub or contact the package maintainer at gumbelin@gmail.com
