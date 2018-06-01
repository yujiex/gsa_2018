(TeX-add-style-hook
 "lean_image_base"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("graphicx" "demo")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "subfigure"
    "graphicx"))
 :latex)

