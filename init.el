(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (let ((buf (url-retrieve-synchronously
		"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		nil 'inhibit-cookies) ))
      (with-current-buffer buf
	(goto-char (point-max))
	(eval-print-last-sexp))))
  (load bootstrap-file nil nil))

(require 'package)
(add-to-list 'package-archives '("melpa"
                                 . "https://melpa.org/packages/"))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(use-package git)

(defmacro init-require (sym)
  `(load (expand-file-name (format "lisp/%s.el" (symbol-name ,sym))
                           user-emacs-directory)))

(add-to-list 'load-path
	     (let ((default-directory user-emacs-directory))
	       (expand-file-name "lisp")))

; (require 'fd-benchmark-init)

(init-require 'fd-misc)
(init-require 'fd-misc-programming)
(use-package yaml-mode)
(init-require 'fd-clipboard)
(when (eq system-type 'darwin) (init-require 'fd-macosx))
(init-require 'fd-grep)
; (require 'fd-helm)
(init-require 'fd-ivy)
; (require 'fd-selectrum)


;; Programming modes
(init-require 'fd-lsp) ; must be before all programming stuff
(require 'fd-compilation)
(init-require 'fd-python)
(init-require 'fd-haskell)
(init-require 'fd-nix)
(init-require 'fd-flycheck)
(init-require 'fd-fluidb)
(init-require 'fd-rust)
(init-require 'fd-tags)
(init-require 'fd-documents)
(init-require 'fd-org)

(init-require 'fd-cc-mode)
(init-require 'fd-lisp)
; (init-require 'fd-coq)
(init-require 'fd-company)
(init-require 'fd-git)
(init-require 'fd-factor)
(init-require 'fd-yasnippet)
(init-require 'fd-fluidb)
(init-require 'fd-racket)
(init-require 'fd-visual)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d3a63c40fddcd2369b7465beca48ddd61fa8e980e9fa88adc52c3cfc78b2b352" "18bec4c258b4b4fb261671cf59197c1c3ba2a7a47cc776915c3e8db3334a0d25" default))
 '(ivy-format-functions-alist
   '((counsel-compile-env . counsel-compile-env--format-hint)
     (counsel-kmacro . counsel--kmacro-format-function)
     (counsel-colors-web . counsel--colors-web-format-function)
     (counsel-colors-emacs . counsel--colors-emacs-format-function)
     (counsel-evil-registers . counsel--yank-pop-format-function)
     (counsel-yank-pop . counsel--yank-pop-format-function)
     (counsel-git-log . counsel--git-log-format-function)
     (counsel-faces . counsel--faces-format-function)
     (swiper-isearch . swiper-isearch-format-function)
     (swiper-all . swiper--all-format-function)
     (swiper-multi . swiper--all-format-function)
     (t . ivy-format-function-line)))
 '(lsp-ltex-version "15.2.0")
 '(org-agenda-files
   '("~/RoamNotes/20211003210234-fluidb_basic_graph.org" "/Users/cperivol/RoamNotes/20211003222815-chapter_query_planning.org" "/Users/cperivol/RoamNotes/20211003211705-chapter_evaluation.org" "/Users/cperivol/RoamNotes/20211023152129-hcntt_description.org" "/Users/cperivol/RoamNotes/20211007175630-data_layout.org" "/Users/cperivol/RoamNotes/20210916203438-antisthenis_cap.org" "/Users/cperivol/RoamNotes/20211010001110-fluidb-background.org" "/Users/cperivol/RoamNotes/20211124190009-background_intermediate_result_recycling.org" "/Users/cperivol/RoamNotes/20211124185726-background_query_processing.org" "/Users/cperivol/RoamNotes/20211017210754-qnf_columns_projections_and_aggregations.org" "/Users/cperivol/RoamNotes/20211019002843-query_preprocessing.org" "/Users/cperivol/RoamNotes/20211003194913-system_overview.org" "/Users/cperivol/RoamNotes/20211031153323-generated_code.org" "/Users/cperivol/RoamNotes/20211003200209-ra_semantics.org" "/Users/cperivol/RoamNotes/20211031145629-join_algorithm.org" "/Users/cperivol/RoamNotes/20211031181905-select_algorithm.org" "/Users/cperivol/RoamNotes/20211006213915-codegen_background.org" "/Users/cperivol/RoamNotes/20211006013034-arrows_and_profunctors.org" "/Users/cperivol/RoamNotes/20211020194149-the_planner.org" "/Users/cperivol/RoamNotes/20211001010541-planning_garbage_collection.org" "/Users/cperivol/RoamNotes/20211025205212-traversing_the_graph.org" "/Users/cperivol/RoamNotes/20211023191843-hcntt_soft_cut_eitherl.org" "/Users/cperivol/RoamNotes/20211023191514-hcntt_interleave_examples.org" "/Users/cperivol/RoamNotes/20210919180432-hcontt_logic_monad.org" "/Users/cperivol/RoamNotes/20210825172203-shape_propagators.org" "/Users/cperivol/RoamNotes/20210909200543-antisthenis_epochs_and_coepochs.org" "/Users/cperivol/RoamNotes/20211003223200-populating_the_graph.org" "/Users/cperivol/RoamNotes/20210812180017-possible_joins.org" "/Users/cperivol/Projects/phd/fluidb/doc/presentation.org" "/Users/cperivol/RoamNotes/20210802213946-thesis_qnf_selection.org" "/Users/cperivol/RoamNotes/20210807193118-qnf_size.org" "/Users/cperivol/RoamNotes/20211006012246-antisthenis_introduction.org" "/Users/cperivol/RoamNotes/20210916121156-antisthenis_cmds_functor.org" "/Users/cperivol/RoamNotes/20211011013112-introduction_to_operators.org" "/Users/cperivol/RoamNotes/20211010155450-antisthenis_processes.org" "/Users/cperivol/RoamNotes/20210914020552-mealy_arrows.org" "/Users/cperivol/RoamNotes/20210916203941-antisthenis_computation_model.org" "/Users/cperivol/RoamNotes/20211006012815-functor.org" "/Users/cperivol/RoamNotes/20211006012937-background_monad.org" "/Users/cperivol/RoamNotes/20210804182441-antisthenis.org" "/Users/cperivol/RoamNotes/20210918182241-antisthenis_zipper.org" "/Users/cperivol/RoamNotes/20211003222152-normal_get_cost.org" "/Users/cperivol/RoamNotes/20211006013406-mealy_and_moore_arrows.org" "/Users/cperivol/RoamNotes/20211006214750-primary_data_serialization_bamify.org" "/Users/cperivol/RoamNotes/20210916204302-antisthenis_cyclical_machines.org" "/Users/cperivol/RoamNotes/20210829140013-defaulting_functor.org"))
 '(safe-local-variable-values
   '((TeX-auto-local . "/Users/cperivol/Projects/phd/thesis/style")
     (TeX-style-local . "/Users/cperivol/Projects/phd/thesis/style")
     (reftex-default-bibliography "/Users/cperivol/Projects/phd/thesis/bib/zotero.bib")
     (compile-root . "/Users/cperivol/Projects/phd/thesis/")))
 '(warning-suppress-types '((comp) (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
