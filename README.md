# small.sample
This R package is able to generate a small sample of representative
points that retain the characteristics of a chosen distribution better
than independent sampling.

The figure below shows the first four moments of 10,000 samples of size
30 generated from a standard normal distribution. The red dots correspond
to samples generated using small.sample; the grey dots correspond to IID
samples.

![Figure](README_FIG.png | width=85%)

# Installing via Github
One can install the package directly from this repository:
```r
install.packages("devtools")
devtools::install_github("iris-yi-jiang/small.sample")
```
The first line above is not needed if you have `devtools` installed.
