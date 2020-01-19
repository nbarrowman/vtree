## Test environments
* Local Windows 7 environment, R 3.6.2
* Local macOS 10.14.6 environment, R 3.6.2
* win-builder (devel, release, and oldrelease)
* rhub default list of CRAN checkers

There was a PREPERROR on the rhub Fedora Linux build
(see https://builder.r-hub.io/status/vtree_4.0.0.tar.gz-adae766766464df4a9e6abe3d27ffdb3)

I have seen this on previous builds and it apparently relates to the librsvg-2.0 library:

```
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because librsvg-2.0 was not found. Try installing:
* deb: librsvg2-dev (Debian, Ubuntu, etc)
* rpm: librsvg2-devel (Fedora, EPEL)
* csw: librsvg_dev, sunx11_devel (Solaris)
* brew: librsvg (OSX)
If librsvg-2.0 is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a librsvg-2.0.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
```

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

