# qrmdtmpl 0.0.5

* Changed qprojektreport to use German titles consistently by loading the German language from the latex-package "babel".


# qrmdtmpl 0.0.4

* Refactored common parts of document creation into a generic function and changed all format-specific functions into wrappers around the generic draft function. 
* Changed placeholder replacement from glue to stringr which seams more flexible. Placeholders are now surrounded by start and end tags to avoid confusion with markdown characters.

# qrmdtmpl 0.0.3

* Introduced placeholders into template skeleton files and draft-functions can now replace placeholders with actual values

# qrmdtmpl 0.0.2

* Corrected package name in default settings of function parameters

# qrmdtmpl 0.0.1

* Added a `NEWS.md` file to track changes to the package.
