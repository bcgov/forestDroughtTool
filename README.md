<!-- 
<a id="devex-badge" rel="Exploration" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>
-->

forestDroughtTool
============

Description of package

### Features
Currently the package has the following functions and datasets:
- `asmr()` - This is the main function, used to calculate monthly asmr and classes for current and future periods.
- `asmrCalc()` - Background function used to compute daily water balance for five relative soil moisture regimes.
- `soilsData` - Soils dataset used by asmrCalc() 
- `PrinceGeorge` - Daily 1918-2008 climate data from Prince George A climate station

### References

Nitschke, C.R., M. Amoroso, K.D. Coates and R. Astrup. 2012. The influence of climate change, site type and disturbance on stand dynamics in northwest British Columbia, Canada. Ecosphere 3 (1):11. 

Nitschke, C.R., and J.L. Innes. 2008. A Tree and Climate Assessment Tool for Modelling Ecosystem Response to Climate Change.  Ecological Modelling 210 (3): 263-277. 


### Installation

You can install `forestDroughtTool` directly from this GitHub repository using the
[remotes](https://cran.r-project.org/package=remotes) package:

  ``` r
install.packages("remotes")

remotes::install_github("bcgov/forestDroughtTool")
library(forestDroughtTool)
```

#### Example


```{r example}
## Calculate daily ASMR for five different RSMR using Prince George climate station data
x<-asmrCalc(PrinceGeorge)
```

### Project Status

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/forestDroughtTool/issues/).

### How to Contribute

If you would like to contribute to the package, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2019 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```

---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
