# lm.Bios625.Package.2021
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/yt-pan/lm.Bios625.Package.2021/workflows/R-CMD-check/badge.svg)](https://github.com/yt-pan/lm.Bios625.Package.2021/actions)
  [![test-coverage](https://github.com/yt-pan/lm.Bios625.Package.2021/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/yt-pan/lm.Bios625.Package.2021/actions/workflows/test-coverage.yaml)
  [![codecov](https://codecov.io/gh/yt-pan/lm.Bios625.Package.2021/branch/main/graph/badge.svg?token=A2K2SCDI19)](https://codecov.io/gh/yt-pan/lm.Bios625.Package.2021)
  <!-- badges: end -->


### Description:
This is a toy package for assignment of Bios 625. The Package only containing a `lm_s()` function which is a mimic of R `lm()` function.

The `lm_s` function will fit a linear model using given data set. It will automatically print summary of coefficients and anova table of the model.

The function may not treat interaction terms correctly and cannot fit a model without an intercept properly.

### Usage:
```{r}
data(mtcars)
d = mtcars
m = lm_s(mpg ~ hp+wt,data = d)
```
![image](https://github.com/yt-pan/lm.Bios625.Package.2021/blob/main/Img/lm_s.png)

### Correctness:
We can compare the result of above `lm_s()` and below `lm()`. They are identical.
```{r}
n = lm(mpg ~ hp+wt,data = d)
summary(n)
anova(n)
```
![image](https://github.com/yt-pan/lm.Bios625.Package.2021/blob/main/Img/lm.png)
  
### Efficiency：
The `lm_s()` function has very poor efficiency compared with R `lm()` even if most of the operation has been vectorized.

Benchmark using package `profvis`.
```{r}
profvis({			
    for (i in 1:1000){			
        x = rnorm(1000)			
        y = rnorm(1000)			
        z = rnorm(1000)			
        m <- lm(y ~ x+z)			
        n1 <- lm_s(y ~ x+z)	
        n2 <- lm_s2(y ~ x+z)			
    }			
})
```
`lm_s` is the first implement of the `lm_s`, `lm_s2` is a more compact version of that, try more vectorized and remove automatic printing. But both of the function are hundreds time slower than the `lm()` and also consume much more memory.

![image](https://github.com/yt-pan/lm.Bios625.Package.2021/blob/main/Img/1.png)

The most time and memory consuming step is the calculation related to hat matrix, as it is a n*n matrix.

![image](https://github.com/yt-pan/lm.Bios625.Package.2021/blob/main/Img/2.png)
