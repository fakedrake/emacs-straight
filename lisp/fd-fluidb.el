;; Some fluidb related code.

(add-to-list 'auto-mode-alist '("branch[0-9]*.txt" . fluidb-branch-mode))

(defvar fluidb-plan-dir nil)
(defvar-local fluidb-plan-dir-local nil)
(defvar fluidb-branch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x ]") 'fluidb-next-branch)
    (define-key map (kbd "C-x [") 'fluidb-prev-branch)
    map)
  "Keymap for using `interactive-haskell-mode'.")

(make-face 'gc )


(define-derived-mode fluidb-branch-mode fundamental-mode "fluidb-branch"
  "Some stuff for dealing with branches

\\{fluidb-branch-mode-map}"
  (setq fluidb-plan-dir
        (when (buffer-file-name (current-buffer))
          (file-name-directory (buffer-file-name (current-buffer)))))
  ;; Make sure we are switching branches on the same tree.
  (setq-local fluidb-plan-dir-local fluidb-plan-dir)
  (read-only-mode)
  (setq font-lock-defaults '(fluidb-font-lock)))


(defmacro let-subst (bindings exp)
  (pcase-dolist (`(,sym ,bind) bindings)
    (setq exp (subst bind sym exp)))
  exp)


(defun preprocess-regex (exp)
  (pcase exp
    (`(list-of ,exp1)
     `(: "[" ,(preprocess-regex exp1) (0+ "," ,(preprocess-regex exp1)) "]"))
    ('node '(: "<" (1+ digit) ">"))
    (`(,x . ,xs) `(,(preprocess-regex x) . ,(preprocess-regex xs)))
    (_ exp)))

(defmacro fluidb-rx (&rest exp)
  (rx-to-string (preprocess-regex (cons ': exp)) t))

(defvar font-lock-dependencies-face 'font-lock-dependencies-face)
(defface font-lock-dependencies-face
  '((default :foreground "LightSteelBlue" :weight bold))
  "Dependencies"
  :group 'font-lock-faces)
(defvar font-lock-materialize-face 'font-lock-materialize-face)
(defface font-lock-materialize-face
  '((default :foreground "PaleGreen" :wight bold))
  "Materialized"
  :group 'font-lock-faces)
(defvar font-lock-mat-state-face 'font-lock-mat-state-face)
(defface font-lock-mat-state-face
  '((default :foreground "yellow1" :weight bold))
  "Materialized"
  :group 'font-lock-faces)
(defvar font-lock-noderef-face 'font-lock-noderef-face)
(defface font-lock-noderef-face
  '((default :foreground "chocolate1" :weight bold))
  "Materialized"
  :group 'font-lock-faces)
(defvar font-lock-newstuff-face 'font-lock-newstuff-face)
(defface font-lock-newstuff-face
  '()
  "Materialized"
  :group 'font-lock-faces)

(setq fluidb-font-lock
      `((,(fluidb-rx (group "Materializing dependencies: ")
                     (group (list-of node)))
         (1 font-lock-dependencies-face)
         (2 font-lock-noderef-face))
        (,(fluidb-rx (group "[Before] setNodeStateSafe ")
                     (group node) " "
                     (group (or "Mat" "NoMat")))
         (1 font-lock-materialize-face)
         (2 font-lock-noderef-face)
         (3 font-lock-mat-state-face))
        (,(fluidb-rx "[NEW STUFF]") 0 font-lock-newstuff-face)
        (,(fluidb-rx node) 0 font-lock-noderef-face)))

(defun fluidb-jump-to-branch (branch &optional plan root-dir)
  "Jump to branch BRANCH of PLAN. If no PLAN is provided then
assume the last plan."
  (interactive "nBranch to jump to from final plan: ")
  (find-file (format "%s/branch%04d.txt"
                     (fluidb-infer-plan-dir plan root-dir)
                     branch)))

(defun fluidb-kill-branch-buffers ()
  "Kill all buffers that refer to branches"
  (interactive)
  (dolist (b (buffer-list))
    (when (if-let ((bn- (buffer-file-name b)))
              (let ((bn (file-name-nondirectory bn-)))
                (and bn (s-suffix? ".txt" bn) (s-prefix? "branch" bn))))
      (kill-buffer b))))

(defun fluidb-available-items (dir prefix)
  "Find all available numbers of files in a directory that have
prefix."
  (mapcar
   (lambda (x)
     (string-to-number
      (substring (file-name-base x) (length prefix))))
   (seq-filter (lambda (x) (s-prefix-p prefix x)) (directory-files dir))))

(defun fluidb-next-new-stuff ()
  "Look for the next branch with new stuff tag at the current line."
  (interactive)
  (cl-flet ((do-next-branch
             nil
             (fluidb-next-branch)
             (goto-line init-line)
             (move-beginning-of-line nil)))
    (let ((init-line (line-number-at-pos))
          (init-buffer (current-buffer)))
      (do-next-branch)
      (while (not (looking-at "\\[NEW STUFF\\]"))
        (when (>= (count-lines (point-min) (point-max)) init-line)
          (let ((buf (current-buffer)))
            (bury-buffer buf)
            (do-next-branch)))))))

(defun fluidb-available-plans (&optional root-dir)
  (fluidb-available-items (fluidb-infer-root-dir root-dir) "branches"))
(defun fluidb-available-branches (plan &optional root-dir)
  (fluidb-available-items (format "%s/branches%03d"
                                  (fluidb-infer-root-dir root-dir)
                                  plan)
                          "branch"))

(defun fluidb-go-to-branch (fn)
  (let ((l (line-number-at-pos))
        (b (current-buffer)))
    (find-file
     (format "branch%04d.txt"
             (funcall
              fn
              (string-to-number
               (concat
                (seq-filter
                 (lambda (x) (<= ?0 x ?9))
                 (file-name-nondirectory (buffer-file-name))))))))
    (with-current-buffer b
      (when (and (boundp 'fluidb-kill-on-jump) fluidb-kill-on-jump)
        (kill-buffer b)))
    (setq-local fluidb-kill-on-jump t)
    (goto-line l)))

(defun fluidb-infer-root-dir (&optional root-dir)
  (or root-dir
      (when fluidb-plan-dir-local (directory-file-name fluidb-plan-dir-local))
      (when fluidb-plan-dir (directory-file-name fluidb-plan-dir))
      "/tmp/benchmark.out.bench_branches"))

(defun fluidb-infer-plan-dir (&optional plan-number root-dir)
  (or
   (when plan-number
     (format "%s/branches%03d"
             (fluidb-infer-root-dir root-dir)
             plan-number))
   fluidb-plan-dir-local
   fluidb-plan-dir
   (format "%s/branches%03d"
           (fluidb-infer-root-dir root-dir)
           (car (last (fluidb-available-plans root-dir))))))

(defun fluidb-jump-to-graph-description (&optional plan-number root-dir)
  "Jump to a reasonable /tmp/branchesXXX/graph.txt"
  (interactive)
  (let ((dir (fluidb-infer-plan-dir plan-number root-dir)))
    (find-file (format "%s/graph.txt" dir))))

(defun fluidb-next-branch ()
  "Jump to next branch. It is assumed that the current visited
file has the name 'branchNUM0.txt'. Next branch is branchNUM1.txt
where NUM1 = NUM0 + 1"
  (interactive) (fluidb-go-to-branch #'1+))

(defun fluidb-prev-branch ()
  "Jump to previous branch. It is assumed that the current
visited file has the name 'branchNUM0.txt'. Previous branch is
branchNUM1.txt where NUM1 = NUM0 - 1"
  (interactive) (fluidb-go-to-branch (lambda (x) (- x 1))))

(defun col-substring (col-beg col-end)
  (save-restriction
    (save-excursion
      (beginning-of-line)
      (narrow-to-region (point) (save-excursion (end-of-line) (point)))
      (forward-char col-beg)
      (let ((pt (point)))
        (forward-char (- col-end col-beg))
        (buffer-substring pt (point))))))

(defun column-at (pt)
  (save-excursion
    (save-excursion (goto-char pt) (current-column))))

(defun with-acive-region (x)
  (if (region-active-p) x (error "Select a region")))

(defun assert-single-line-region-active ()
  (unless
      (and (region-active-p)
           (= (line-number-at-pos (region-end)) (line-number-at-pos (region-beginning))))
    (error "Region is not active or spans many lines.")))

(defun forward-line-pred-change (from-pt to-pt &optional direction)
  "Go forward lines until the region between the columns changes"
  (interactive "r")
  (assert-single-line-region-active)
  (let ((cb (column-at from-pt))
        (ce (column-at to-pt)))
    (cl-flet ((get-st nil (col-substring cb ce)))
      (let ((old-st (get-st)))
        (while (equal old-st (setq old-st (get-st)))
          (forward-line direction))))))

(defun backward-line-pred-change (from-pt to-pt)
  "Go backward lines until the region between the columns changes"
  (interactive "r")
  (forward-line-pred-change from-pt to-pt -1))

(defun fluidb-indent-wraptrace ()
  "Remove any indentation and indent for lines beginning with
[Before] and [After]"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((ind 0))
      (while (< (point) (point-max))
        (delete-region (point)
                       (save-excursion (skip-chars-forward " ") (point)))
        (when (looking-at "\\[After\\]") (setq ind (1- ind)))
        (dotimes (i ind) (insert "  "))
        (when (looking-at "\\[Before\\]") (setq ind (1+ ind)))
        (end-of-line) (forward-line) (beginning-of-line)))))


(defun profiterole-line (conf &optional profiterole-conf-file)
  "With the point over a line in the *.profiterole.txt file"
  (unless (s-suffix? ".profiterole.txt" (buffer-file-name))
    (error "Not in a *.profiterole.txt file"))
  (let* ((ws (s-split " " (thing-at-point 'line t) t))
         (sym (concat (nth 3 ws) " " (nth 4 ws)))
         (conf-line (concat conf ": " sym "\n")))
    (with-current-buffer (find-file-noselect
                          (or profiterole-conf-file ".profiterole.yaml"))
      (goto-char (point-max))
      (insert conf-line)
      (save-buffer))))

(defun profiterole-prof-file (file)
  "Get the prof as file."
  (let ((p (reverse (s-split "/" file))))
    (s-join
     "/"
     (reverse
      (cons (format "%s.prof" (car (s-split "\\." (car p))))
            (cdr p))))))

(defvar profiterole-update-hist nil)
(defun profiterole-update (conf)
  (interactive
   (list
    (completing-read
     "profiterole action: "
     '("bury" "omit" "fold" "rbury" "root")
     nil
     t nil 'profiterole-update-hist)))
  (profiterole-line conf)
  (profiterole-reload))

(defun profiterole-reload ()
  (interactive)
  (let ((file (profiterole-prof-file (buffer-file-name))))
    (shell-command (format "profiterole %s > /dev/null" file)))
  (revert-buffer nil t))

(defun profiterole-bury ()
  (interactive)
  (profiterole-update "bury"))

(defun profiterole-omit ()
  (interactive)
  (profiterole-update "omit"))

(setq profiterole-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b") 'profiterole-bury)
    (define-key map (kbd "C-c o") 'profiterole-bury)
    (define-key map (kbd "C-c u") 'profiterole-reload)
    map))
(define-derived-mode profiterole-mode fundamental-mode "profiterole"
  "Deal with profiterole."
  (use-local-map profiterole-mode-map)
  (read-only-mode))

(add-to-list 'auto-mode-alist '("\\.profiterole.txt\\'" . profiterole-mode))
