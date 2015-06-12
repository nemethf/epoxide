# Epoxide
A Modular Troubleshooting Framework for SDN

Epoxide is an Emacs based modular framework, which can flexibly
combine network and software troubleshooting tools in a single
platform.  Epoxide does not try to be a complex troubleshooting
software that fully integrates all available tools, but rather a
lightweight framework that allows the ad-hoc creation of tailor-made
testing methods from predefined building blocks to test
troubleshooting hypotheses.

I. Pelle, T. Lévai, F. Németh, and A. Gulyás: "One Tool to Rule Them
All: A Modular Troubleshooting Framework for SDN (and other)
Networks", ACM Sigcomm Symposium on SDN Research (SOSR), June 17-18,
2015, Santa Clara, CA.  http://dx.doi.org/10.1145/2774993.2775014

# Installation

Add the following lines to your emacs initialization file (~/.emacs):

```
  (package-initialize)
  (add-to-list 'package-archives
               '("epoxide" . "http://nemethf.github.io/epoxide/") t)
```

Then install the `epoxide' package with M-x list-packages RET.

## Installation from the git repository

First install cogre either from the CEDET git repository or from the
package archive of epoxide (see above), then add the following lines
to your initialization file.

```
  (add-to-list 'load-path "path-to-the-epoxide-repository/src")
  (autoload 'epo-mode "epoxide")
  (add-to-list 'auto-mode-alist '("\\.tsg\\'" . epoxide-tsg-mode))
  (autoload 'tramp-mininet-setup "tramp-mininet")
  (eval-after-load 'tramp '(tramp-mininet-setup))
```

# Acknowledgement

The research leading to these results has received funding from the
European Union Seventh Framework Programme under grant agreement
No. 619609.  See https://www.fp7-unify.eu/
