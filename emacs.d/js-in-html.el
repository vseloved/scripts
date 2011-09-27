(require 'js)
(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 8 parse-status) 0) ; inside string
          ((js--ctrl-statement-indentation))
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ((nth 1 parse-status)
           (let ((same-indent-p (looking-at
                                 "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn
                   (skip-syntax-backward " ")
		   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p
                          (current-column))
                         (continued-expr-p
                          (+ (current-column) (* 2 js-indent-level)
                             js-expr-indent-offset))
                         (t
                          (+ (current-column) js-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))
          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t (let (pos
                   (prev-blank? t))
               (save-excursion
                 (back-to-indentation)
                 (setq pos (current-column))
                 (while prev-blank?
                   (previous-line)
                   (setq prev-blank? (= (progn (beginning-of-line) (point))
                                        (progn (end-of-line) (point)))))
                 (unless (or prev-blank?
                             (looking-at "<"))
                   (back-to-indentation)
                   (setq pos (current-column))))
               pos)))))

(defun install-html-script-properties ()
  (with-silent-modifications
    (save-excursion
      (let* ((orig (point))
             (case-fold-search t))
        (when (re-search-backward "\\(<script?\.*>\\|\\)" nil t)
          (let* ((beg (progn (next-line)
                             (beginning-of-line)
                             (point)))
                 (case-fold-search t)
                 (next (re-search-forward "</script>" nil t)))
            (when (and next
                       (>= next orig))
              (let ((end (progn (previous-line)
                                (end-of-line)
                                (point))))
                ;; (put-text-property beg end 'syntax-table js2-mode-syntax-table)
                ;; (font-lock-fontify-region beg end)
                (let ((map (copy-keymap js2-mode-map)))
                  (define-key map [tab] 'js-indent-line)
                  (put-text-property beg end 'local-map map))))))))))

(defun add-js-in-html-hook ()
  (add-hook 'html-mode-hook
            (lambda ()
              (make-variable-buffer-local 'after-change-functions)
              (setq after-change-functions
                    (cons (lambda (&rest ignored)
                            (install-html-script-properties))
                          after-change-functions))
              (save-excursion
                (loop until (eobp) do
                      (install-html-script-properties)
                      (forward-line 1))))))
