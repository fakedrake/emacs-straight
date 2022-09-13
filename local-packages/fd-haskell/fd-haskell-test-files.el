
(defun haskell-test-to-src ()
  (let ((cabal-file (haskell-cabal-find-file))
        (testname (file-name-base (buffer-file-name))))
    (if (or (not cabal-file)) (error "Not in haskell project.")
      (with-current-buffer (find-file-noselect cabal-file)
        (save-excursion
          (goto-char (point-min))            ;XXX: we asume the first
                                        ;hs-source-dirs encountered is
                                        ;the right one
          (let* ((src-dirs (haskell-cabal-subsection-entry-list
                            (haskell-cabal-section) "hs-source-dirs"))
                 (modules (haskell-cabal-subsection-entry-list
                           (haskell-cabal-section) "exposed-modules"))
                 (mod-paths (delete-if-not
                             (lambda (x) (string= testname (file-name-base x)))
                             (mapcar 'haskell-cabal-module-to-filename modules)))
                 (full-paths (apply 'append
                                    (mapcar
                                     (lambda (dir)
                                       (mapcar (lambda (mp)
                                                 (concat default-directory "/" dir "/" mp))
                                               mod-paths))
                                     src-dirs))))
            (delete-if-not 'file-exists-p full-paths)))))))


(defun haskell-jump-test-to-src ()
  (interactive)
  (let ((srcs (haskell-test-to-src)))
    (if (not srcs) (error "Couldn't find sources from test file (maybe not in .cabal?)." )
      (find-file (car srcs)))))

(defun haskell-toggle-src-test ()
  "toggle between source and test files."
  (interactive)
  (if (not (haskell-cabal-find-file)) (error "Not in haskell project")
    (if (member "tests" (split-string (buffer-file-name) "/"))
        (haskell-jump-test-to-src) (haskell-jump-src-to-test))))
(defun haskell-cabal--find-tags-dir-advice (old-fn)
  "Override the tags path"
  (or haskell-tags-file-dir (funcall old-fn)))
(advice-add 'haskell-cabal--find-tags-dir
            :around #'haskell-cabal--find-tags-dir-advice)

(defun haskell-jump-src-to-test ()
  "Being in source jump th the corresponding test"
  (interactive)
  (find-file (haskell-corresponding-test (buffer-file-name))))

(defun haskell-corresponding-test (path)
  "Find a corresponding test file. If the cabal file is
/a/test.cabal and path is /a/b/c/d/e/f.hs we will try in order:
/a/tests/b/c/f.hs /a/tests/b/f.hs, /a/tests/f.h,
/a/tests/b/c/d/f.hs, /a/tests/b/c/f.hs, ..."
  (if-let* ((cabal-file (haskell-cabal-find-file)))
      (let* ((cabal-dir (file-name-directory cabal-file))
             (nondir (file-name-nondirectory path))
             (subpath (save-match-data
                        (split-string
                         (replace-regexp-in-string
                          cabal-dir ""
                          (directory-file-name
                           (file-name-directory path)))
                         "/")))
             (rsubpath (reverse subpath))
             (retfn (lambda (x)
                      (concat cabal-dir
                              "tests/"
                              (mapconcat
                               'identity
                               (if (string= "src" (car x)) (cdr x) x)
                               "/")
                              (if x "/" "") nondir)))
             (defret (funcall retfn subpath))
             (ret (funcall retfn subpath)))
        ;; Reduce subpath until it's null then reduce rsubpath.
        (while (and rsubpath (not (file-exists-p ret)))
          (if (not subpath)
              (setq rsubpath (cdr rsubpath)
                    subpath (reverse rsubpath))
            (setq subpath (cdr subpath)))
          (setq ret (funcall retfn subpath)))
        (if rsubpath ret defret))
    (error "Not in a cabal project.")))

(provide 'fd-haskell-test-files)
