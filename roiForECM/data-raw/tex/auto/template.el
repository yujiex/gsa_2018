(TeX-add-style-hook
 "template"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "graphicx"
    "placeins"))
 :latex)

