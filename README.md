# Modularity analysis
Software architecture modularity analysis for the ALICE bookkeeping system at CERN.

## Prerequisites
* ESLint CLI
* Python
* R

## Setup
* Create a ```repository``` folder in the root repository folder and populate it with the ALICE bookkeeping system.

  ```git clone git@github.com:AliceO2Group/Bookkeeping.git repository```

* Setup a Python environment for pre-processing and install the ```pandas``` package

## Getting metrics
Run the following command with the ESLint CLI. It uses ```eslint-config.json``` for selecting the rules and setting up ESLint.

```
eslint ../repository --no-eslintrc --config 'eslint-config.json' --format json --output-file out.json
```