(defface nj-search-match
  '((t :inherit isearch))
  "Face for `nj-search`.")

(defface nj-search-current-line
  '((t :inherit hl-line :extend t))
  "Face for line `nj-search-current-line`.")

(defvar nj-search-buffer nil
  "Buffer for the search.")

(defvar nj-search-start-point nil
  "Point where search started.")

(defvar nj-search-overlays nil
  "Overlays for the search.")

(defvar nj-search-line-overlay nil
  "Line overlay for the search.")

(defvar nj-search-prompt nil
  "Prompt for the search.")

(defvar nj-search-count nil
  "Count for the search.")

(defvar nj-search-direction nil
  "Direction for the search.")

(defvar nj-search--hl-line nil
  "Track if `hl-line-mode` was on before minibuffer.")

(defvar nj-search--global-hl-line nil
  "Track if `global-hl-line-mode` was on before minibuffer.")

(defvar nj-search-forward t
  "The direction for the search, if non-nil forward, otherwise backwards.")

(defmacro nj-search-with-smart-case (&rest Body)
  `(let ((case-fold-search
          (string= nj-search-prompt (downcase nj-search-prompt))))
     ,@Body))

(defun nj-search--current-index ()
  (nj-search-with-smart-case
   (save-excursion
     (let ((Result 0)
           (Pos (point)))
       (goto-char (point-min))
       (while (and (re-search-forward (regexp-quote nj-search-prompt) nil t)
                   (<= (match-beginning 0) Pos))
         (cl-incf Result))
       Result))))

(defun nj-search-message ()
  (let* ((Pos (with-current-buffer nj-search-buffer (point)))
         (Face (get-text-property Pos 'face)))
    (message "%s [%d/%d]"
             (propertize nj-search-prompt 'face Face)
             (nj-search--current-index)
             nj-search-count)))

(defun nj-search-make-overlays ()
  "Make overlays for the search."
  (save-excursion
    (goto-char (point-min))
    (let ((Count 0))
      (condition-case nil
          (nj-search-with-smart-case
           (while (re-search-forward (regexp-quote nj-search-prompt) nil t)
             (let ((Ov (make-overlay (match-beginning 0) (match-end 0))))
               (overlay-put Ov 'face 'nj-search-match)
               (push Ov nj-search-overlays))
             (cl-incf Count)))
        (invalid-regexp nil))
      (setq nj-search-count Count))))

(defun nj-search-make-line-overlay (Pos)
  "Make a line overlay at POS."
  (when nj-search-line-overlay
    (delete-overlay nj-search-line-overlay))
  (setq nj-search-line-overlay
        (make-overlay (line-beginning-position)
                      (min (1+ (line-end-position)) (point-max))))
  (overlay-put nj-search-line-overlay 'face 'nj-search-current-line))

(defun nj-search-clear-overlays ()
  "Clear overlays by the search."
  (mapc #'delete-overlay nj-search-overlays)
  (setq nj-search-overlays nil))

(defun nj-search-find-next (Start &optional Count)
  "Find next match, the starting point is START + 1."
  (let ((Result
         (save-excursion
           (goto-char (1+ Start))
           (nj-search-with-smart-case
            (or
             (re-search-forward (regexp-quote nj-search-prompt) nil t Count)
             (progn
               (goto-char (point-min))
               (re-search-forward (regexp-quote nj-search-prompt) nil t Count)))))))
    (when Result
      ;; NOTE: Cursor always at the beginning
      (goto-char (match-beginning 0))
      (recenter))
    Result))

(defun nj-search-find-prev (Start &optional Count)
  "Find previous match, the starting point is START + 1."
  (let ((Result
         (save-excursion
           (goto-char (1+ Start))
           (nj-search-with-smart-case
            (or
             (re-search-backward (regexp-quote nj-search-prompt) nil t Count)
             (progn
               (goto-char (point-max))
               (re-search-backward (regexp-quote nj-search-prompt) nil t Count)))))))
    ;; NOTE: Cursor always at the beginning
    (when Result
      (goto-char (match-beginning 0))
      (recenter))
    Result))

(defun nj-search-update ()
  (let ((Prompt (minibuffer-contents-no-properties)))
    (with-selected-window (minibuffer-selected-window)
      (nj-search-clear-overlays)
      (or (and (buffer-live-p nj-search-buffer)
               (not (string-empty-p Prompt))
               (setq nj-search-prompt Prompt)
               (nj-search-make-overlays)
               (or (if nj-search-forward
                       (nj-search-find-next nj-search-start-point)
                     (nj-search-find-prev nj-search-start-point))
                   (goto-char nj-search-start-point))
               (if (> nj-search-count 0)
                   (message "%d/%d" (nj-search--current-index) nj-search-count)
                 (message "No matches")))
          (goto-char nj-search-start-point))
      (nj-search-make-line-overlay (point)))))

(defun nj-search-exit ()
  (recenter)
  (when nj-search--hl-line
    (hl-line-mode t)
    (setq nj-search--hl-line nil))
  (when nj-search--global-hl-line
    (global-hl-line-mode t)
    (setq nj-search--global-hl-line nil))
  (when nj-search-line-overlay
    (delete-overlay nj-search-line-overlay)))

(defun nj-search-setup (Forward)
  "If FORWARD is t search forward, otherwise search backwards."
  (setq nj-search-buffer (current-buffer)
        nj-search-start-point (point))
  (setq nj-search-forward Forward))

(defun nj-search-start-minibuffer ()
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1)
    (setq nj-search--hl-line t))
  (when (bound-and-true-p global-hl-line-mode)
    ;; NOTE: Should also do this for `hl-line-mode`?
    (nj-search-make-line-overlay (point))
    (global-hl-line-mode -1)
    (setq nj-search--global-hl-line t))
  (add-hook 'post-command-hook #'nj-search-update nil t))

(defun nj-search ()
  "Start a new search with a specific DIRECTION."
  (unwind-protect
      (condition-case nil
          (minibuffer-with-setup-hook #'nj-search-start-minibuffer
            ;; TODO: Add a custom history
            (if nj-search-forward
                (read-string "Search: ")
              (read-string "Search backwards: ")))
        (quit
         (nj-search-clear-overlays)))
    (nj-search-exit)))

(defun nj-search-forward ()
  (interactive)
  (nj-search-setup t)
  (nj-search))

(defun nj-search-backwards ()
  (interactive)
  (nj-search-setup nil)
  (nj-search))

(defun nj-search-next (Count)
  (interactive "p")
  (if nj-search-forward
      (when (nj-search-find-next (point) Count)
        (nj-search-message))
    (when (nj-search-find-prev (point) Count)
      (nj-search-message))))

(defun nj-search-prev (Count)
  (interactive "p")
  (if nj-search-forward
      (when (nj-search-find-prev (point) Count)
        (nj-search-message))
    (when (nj-search-find-next (point) Count)
      (nj-search-message))))

(defun nj-search-word (Count)
  (nj-search-clear-overlays)
  (let ((Prompt (word-at-point)))
    (if (not Prompt)
        (message "No word under cursor")
      (setq nj-search-prompt Prompt)
      (nj-search-make-overlays)
      (if nj-search-forward
          (nj-search-find-next (point) Count)
        (nj-search-find-prev (point) Count)))
    Prompt))

(defun nj-search-word-forward (Count)
  (interactive "p")
  (nj-search-setup t)
  (when (nj-search-word Count)
    (nj-search-message)))

(defun nj-search-word-backwards (Count)
  (interactive "p")
  (nj-search-setup nil)
  (when (nj-search-word Count)
    (nj-search-message)))

(define-minor-mode nj-search-mode "nj-search")

(provide 'nj-search-mode)
