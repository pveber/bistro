(require 'mmm-auto)
(setq mmm-global-mode 'maybe)

(mmm-add-classes
      '((bistro-sh
         :submode sh-mode
         :face mmm-declaration-submode-face
         :front "[%sh[ \t\n]+{|"
         :back "|}")
        (bistro-bash
         :submode sh-mode
         :face mmm-declaration-submode-face
         :front "[%bash[ \t\n]+{|"
         :back "|}")
        (bistro-R
         :submode ess-mode
         :face mmm-declaration-submode-face
         :front "[%R[ \t\n]+{|"
         :back "|}")
        (bistro-perl
         :submode perl-mode
         :face mmm-declaration-submode-face
         :front "[%perl[ \t\n]+{|"
         :back "|}")
        (bistro-ocaml
         :submode tuareg-mode
         :face mmm-declaration-submode-face
         :front "[%ocaml[ \t\n]+{|"
         :back "|}")
        (bistro-ocamlscript
         :submode tuareg-mode
         :face mmm-declaration-submode-face
         :front "[%ocamlscript[ \t\n]+{|"
         :back "|}")
        (bistro-python
         :submode python-mode
         :face mmm-declaration-submode-face
         :front "[%python[ \t\n]+{|"
         :back "|}")))

(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-sh)
(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-bash)
(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-ocaml)
(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-ocamlscript)
(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-python)
(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-R)
(mmm-add-mode-ext-class 'tuareg-mode nil 'bistro-perl)

