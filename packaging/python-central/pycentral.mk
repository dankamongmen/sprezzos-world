# some macros useful for packaging with pycentral

# sitedir: name of the site-packages/dist-packages directory depending on the
# python version. Call as: $(call sitedir, <python version>).

sitedir = $(if $(filter $(subst python,,$(1)), 2.3 2.4 2.5),site,dist)-packages
