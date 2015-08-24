(require 'mmm-auto)
(setq mmm-global-mode 'maybe)

(mmm-add-classes
      '((bistro-sh
         :submode sh-mode
         :face mmm-declaration-submode-face
         :front "bistro\.sh[ \t\n]+{|"
         :back "|}")
        (bistro-ocaml
         :submode tuareg-mode
         :face mmm-declaration-submode-face
         :front "bistro\.ocaml[ \t\n]+{|"
         :back "|}")
        (bistro-ocamlscript
         :submode tuareg-mode
         :face mmm-declaration-submode-face
         :front "bistro\.ocamlscript[ \t\n]+{|"
         :back "|}")
        (bistro-python
         :submode python-mode
         :face mmm-declaration-submode-face
         :front "bistro\.python[ \t\n]+{|"
         :back "|}")))

(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-sh)
(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-ocaml)
(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-ocamlscript)
(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-python)

