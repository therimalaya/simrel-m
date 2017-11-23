---
author:
- Raju Rimal
bibliography: '../ref-db.bib'
fontfamily: palatino
fontsize: 12pt
geometry: margin=1in
header-includes:
- '\usepackage[utf8]{inputenc}'
- '\usepackage[T1]{fontenc}'
- '\usepackage{cancel}'
- '<link rel="stylesheet" href="latex.css"/>'
- '<link rel="stylesheet" href="tufte.css"/>'
- '<link rel="stylesheet" href="custom.css"/>'
- '\usepackage[style=authoryear,dashed=false]{biblatex}'
html_mathjax: 'align: left indent: 5em tagside: left font: Neo-Euler extensions: Cancel'
include-after: |
    <script type="text/x-mathjax-config">
    MathJax.Hub.Register.StartupHook("TeX Jax Ready",function () {
      MathJax.Hub.Insert(MathJax.InputJax.TeX.Definitions.macros,{
        cancel   : ["Extension","cancel"],
        bcancel  : ["Extension","cancel"],
        xcancel  : ["Extension","cancel"],
        cancelto : ["Extension","cancel"]
      });
    });
    </script>

pandoc_options: 'include-after-body:footer.html'
pandoc_variables: 'fontsize:12pt papersize:A4 geometry:margin=1in fontfamily:palatino'
papersize: A4
startup: inlineimages
title: Review on Simrel Paper
---

\usepackage[utf8]{inputenc}

\usepackage[T1]{fontenc}

\usepackage{cancel}

<link rel="stylesheet" href="latex.css"/>

<link rel="stylesheet" href="tufte.css"/>

<link rel="stylesheet" href="custom.css"/>

\usepackage[style=authoryear,dashed=false]{biblatex}

[TODO]{.todo .TODO} Corrections in Paper {#corrections-in-paper}
========================================

Introduction
------------

### Page 2:

-   *Add references [@johnson2013multivariate] along with
    [@ripley2009stochastic] and [@gamerman2006markov].*
-   **Line 2 last paragraph** ::

In particular, methods based on covariance structure has been discussed
by [@arteaga2010simulate, @arteaga2013building, @camacho2017generation],
which follows an algorithmic approach to find simulated data satisfying
the desired correlation structure.

### Page3:

**After first Paragraph**

:   

The github repository of the package at
<http://github.com/simulatr/simrel> has rich documentation with many
examples and cases along with detail description on simulation
parameters. In the following two section, discussion encircle the
mathematical framework behind. In addition, section-4 and 5 have also
discussed about the input parameters needed for simrel function in
brief. In section 4, an implementation is presented as an example of
use-cases and the final section introduce shiny web application for this
tool.

**After equation (1)**

:   

Throughout this paper, the elements of a variance-covariance matrix
$\boldsymbol{\Sigma}$ is represented by $\sigma$ with corresponding
indices. For example, a covariance matrix $\boldsymbol{\Sigma}_{xy}$ can
be written as $\boldsymbol{\Sigma}_{xy} = \left(\sigma_{x_iy_j}\right)$,
where $i$ and $j$ represents the row and column of
$\boldsymbol{\Sigma}_{xy}$.

Relevant Components
-------------------

### Page 6

-   Equation number to the expression of generating lambda from gamma.
    Subscript changed from $j$ to $i$.
-   **Added to equation of $\gamma$:** Hence, we can write
    $\Sigma_{zz} = \Lambda = \text{diag}(\lambda_1, \ldots, \lambda_p)$.
-   ~~Here we assume that $\mathbf{w}$'s are independent and has
    mutinormal distribution wnith variances 1, hence
    $\boldsymbol{\Sigma}_{ww} = \mathbf{I}_m$.~~ In similar manner, a
    parametric representation of eigenvalues corresponding to
    $\Sigma_{ww}$ is adopted as,
    $$\kappa_j = e^{-\eta(j - 1), \eta > 0 \text{ and } j = 1 \ldots m}$$
    Here, the decline of eigenvalues becomes steep as $\eta$ gets far
    from zero. At $\eta = 0$, all $\boldsymbol{w}$ will have equal
    variance. Hence we can write
    $\Sigma_{ww} = \text{diag}(\kappa_1, \ldots, \kappa_m)$.

### Page 8

-   $\cancel{\Sigma_{ww} = \mathbf{I}_{m}}$
    $$\Sigma_{ww} = \text{diag}(\kappa_1, \ldots, \kappa_m)$$
-   $\cancel{\rho_w^2 = \Sigma_{zw}^t\Lambda^{-1}\Sigma_{zw}\Sigma_{ww}^{-1}}$
    $$\rho_{w}^2 = \Sigma_{zw}^t\Lambda^{-1}\Sigma_{zw}\Sigma_{ww}^{-1} = 
       \begin{bmatrix}
         \sum_{i = 1}^p\frac{\sigma_{i1}^2}{\lambda_i\kappa_1} & \ldots & \sum_{i=1}^p\frac{\sigma_{i1}\sigma_{im}}{\lambda_i\kappa_1} \\
         \vdots & \ddots & \vdots \\
         \sum_{i=1}^p\frac{\sigma_{i1}\sigma_{im}}{\lambda_i\kappa_m} & \ldots & \sum_{i = 1}^p\frac{\sigma_{im}^2}{\lambda_i\kappa_m}
       \end{bmatrix} $$
-   $\cancel{\rho_{w_j}^2 = \sum_{i = 1}^p\frac{\sigma_{ij}^2}{\lambda_i}}$
    is replaced by,

$$\rho_{w_j}^2 = \sum_{i = 1}^p\frac{\sigma_{ij}^2}{\lambda_i\kappa_j}$$

-   $\cancel{\rho_{w_j}^2 = \sum_{i \in \mathcal{P}_j}\frac{\sigma_{ij}^2}{\lambda_i}}$

$$\rho_{w_j}^2 = \sum_{i \in \mathcal{P}_j} \frac{\sigma_{ij}^2}{\lambda_i\kappa_j}$$

-   $\cancel{\sigma_{ij} = \text{Sign}(\mathcal{S}_i)\sqrt{\frac{\rho_{wj}^2\left|\mathcal{S}_i\right|}{\sum_{k\in\mathcal{P}_j}\left|\mathcal{S}_k\right|}\lambda_i}}$
    $$\sigma_{ij} = \text{Sign}(\mathcal{S}_i)\sqrt{\frac{\rho_{wj}^2\left|\mathcal{S}_i\right|}{\sum_{k\in\mathcal{P}_j}\left|\mathcal{S}_k\right|}\lambda_i\kappa_j}$$
    for $i \in \mathcal{P}_j$ and $j = 1, \ldots m$

Implementation
--------------

### Page 13

-   `gamma` $\rightarrow \gamma$ (referred to corresponding equation)
-   **Line 8:** Here, in this example we have assumed that all $w$ 's
    have equal variance, i.e. $\Sigma_{ww} = I_m$
-   **Line 11:** Although the simulation method is well equipped to
    simulate data with $p \gg n$, for incorporating envelope estimation
    methods which are based on maximization of likelihood, we have
    chosen $n > p$ situation in the example.
-   **Figure 3 Caption added:** Here (a) is the covariance structure of
    latent space which is rotated by the block diagonal rotation matrix
    in (b) resulting the covariance structure of simulated data in (c).

Web Interface
-------------

### Page 16

-   ~~(uses `simrel` package in CRAN)~~, ~~(not yet available in
    CRAN)~~, ~~(`simrel-m`)~~.
-   **second last line** Users can also download simulated data in JSON
    and CSV format.

### Page 17

-   After Second paragraph until conclusion, major changes are done
-   Figure-5 moved to this page
-   **Figure-5 Caption:** Web interface of shiny application of
    \`simrel\`: (**a**) Buttons to trigger simulation, (**b**)
    Parameters for simulation, (**c**) Visualization of the true
    properties of simulated data (regression coefficients, true and
    estimated covariance between response and predictors components)
    (**d**) Additional analysis (**e**) Download option of simulated
    data.

### Page 18

-   Figure 5 will be updated

Conclusion
----------

-   Whether comparing methods or assessing and understanding properties
    of any methods, tools or procedure; simulated data allows controlled
    tests for researchers. However, researchers spend enormous amount of
    time for creating such simulation tools so that they can obtain
    particular nature of data. We believe that this tool along with
    R-package and the easy to use shiny web interface will become an
    assistive tool for researchers in this respect.

[TODO]{.todo .TODO} R package:: `simrel` \[3/7\] {#r-package-simrel-37}
================================================

-   \[X\] Convert `simulatr` $\rightarrow$ `simrel`
-   \[X\] Univariate simrel (`simrel`) $\rightarrow$ `unisimrel`
    function
-   \[X\] Move the repository `therimalaya/simulatr -> simulatr/simrel`
-   \[ \] Include `mbrd` function for creating fractional design from
    old simrel package
-   \[ \] Update documentation with many examples and cases
-   \[ \] Introduce about shiny application and rstudio gadget on the
    readme file
-   \[ \] Link to package documentation site

[TODO]{.todo .TODO} Shiny application for `simrel` \[0/2\] {#shiny-application-for-simrel-02}
==========================================================

-   \[ \] Fix covariance plot (bigger text, aspect ratio etc)
-   \[ \] Model fitting tab:: Coefficient comparison plot, estimation
    and prediction error plot

References {#references .unnumbered}
==========

<script type="text/x-mathjax-config">
MathJax.Hub.Register.StartupHook("TeX Jax Ready",function () {
  MathJax.Hub.Insert(MathJax.InputJax.TeX.Definitions.macros,{
    cancel   : ["Extension","cancel"],
    bcancel  : ["Extension","cancel"],
    xcancel  : ["Extension","cancel"],
    cancelto : ["Extension","cancel"]
  });
});
</script>
