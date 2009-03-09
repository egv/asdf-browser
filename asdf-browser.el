;;;; Public Domain. I won't even bother including the absurdly long
;;;; public domain statement nor the warranty disclaimer.

;;;; Sample use:
;;;; (define-key lisp-mode-map (kbd "C-C C-a") 'asdf-browser)

(require 'cl)
(require 'slime)

(defface asdf-browser-title-face
    '((t (:foreground "cyan")))
  nil)

(defface asdf-browser-module-face
    '((t (:foreground "cyan")))
  nil)

(defface asdf-browser-file-face
    '((t ))
  nil)

(defun asdf-browser-get-overlay-prop (point prop)
  (loop for i in (overlays-at point)
        thereis (overlay-get i prop)))

(defun asdf-browser-newline ()
  (interactive)
  (let ((foo (asdf-browser-get-overlay-prop (point) 'lisp-file)))
    (if foo
        (find-file foo)
        (error "Item not clickable"))))

(defun asdf-browser-pop ()
  (interactive)
  (let ((foo (pop asdf-browser-stack)))
    (if foo
        (progn (setq asdf-browser-inhibit-stack t)
               (asdf-browser-go foo))
        (error "Empty browser stack"))))

(defun asdf-browser-reload ()
  (interactive)
  (set-buffer (asdf-browser-ensure-buffer))
  (setq asdf-browser-inhibit-stack t)
  (asdf-browser-go asdf-browser-system))

(define-derived-mode asdf-browser-mode nil "asdf-browser"
  "Major mode for the asdf browser. Requires SLIME."
  (define-key asdf-browser-mode-map (kbd "RET") 'asdf-browser-newline)
  (define-key asdf-browser-mode-map "l" 'asdf-browser-pop)
  (define-key asdf-browser-mode-map "r" 'asdf-browser-reload)
  (define-key asdf-browser-mode-map "g" 'asdf-browser)
  (set (make-variable-buffer-local 'asdf-browser-stack) nil)
  (set (make-variable-buffer-local 'asdf-browser-system) nil)
  (set (make-variable-buffer-local 'asdf-browser-inhibit-stack) nil)
  (setq buffer-read-only t))

(defun asdf-browser-system-exists-p (system)
  (slime-eval
   `(cl:not (cl:null (asdf:find-system (cl:make-symbol ,system) nil)))))

(defun asdf-browser-get-components (system k)
  "Open all files in an ASDF system in Emacs"
  (unless (slime-connected-p)
    (error "Must be connected to an inferior Lisp")) 
  (slime-eval-async
   `(cl:labels ((aux (foo)
                  (cl:typecase foo
                    (asdf:module
                       (cl:list* :module
                                 (cl:slot-value foo 'asdf::name)
                                 (cl:mapcar (cl:function aux)
                                            (cl:slot-value
                                             foo 'asdf::components))))
                    (asdf:cl-source-file
                       (cl:list :cl-source-file
                                (cl:slot-value foo 'asdf::name)
                                (cl:namestring
                                 (asdf:component-pathname foo)))))))
      (cl:let ((system (asdf:find-system (cl:make-symbol ,system))))
        (cl:when system
          (cl:list :name (cl:slot-value system 'asdf::name)
                   :data (cl:mapcar (cl:function aux)
                                    (cl:slot-value system 'asdf::components))
                   :version (cl:and (cl:slot-boundp system 'asdf::version)
                                    (cl:slot-value system 'asdf::version))))))
   k))

(defun asdf-browser-line-coords ()
  (save-excursion (forward-line -1)
                  (values (line-beginning-position)
                          (line-end-position))))

(defun asdf-browser-k (system)
  (lexical-let ((system system))
    (lambda (foo)
      (unless foo
        (error "No such system: %s" foo))
      (setq buffer-read-only nil)
      (let ((depth 0)
            (first nil)
            (buf (asdf-browser-ensure-buffer)))
        (labels ((pad ()
                   (dotimes (i depth)
                     (insert " ")))
                 (aux (stuff)
                   (pad)
                   (ecase (car stuff)
                     (:module
                        (slime-insert-propertized
                         '(face asdf-browser-module-face)
                         (format "%s\n" (cadr stuff)))
                        (let ((depth (1+ depth)))
                          (mapc #'aux (cdddr stuff))))
                     (:cl-source-file
                        (or first (setq first (point)))
                        (slime-insert-propertized
                         '(face asdf-browser-file-face)
                         (format "%s\n" (cadr stuff)))
                        (multiple-value-bind (start end)
                            (asdf-browser-line-coords)
                          (overlay-put (make-overlay start end)
                                       'lisp-file (caddr stuff)))))))
          (erase-buffer)
          (slime-insert-propertized '(face asdf-browser-title-face)
                                    (format "ASDF System: %s %s\n\n"
                                            (getf foo :name)
                                            (or (getf foo :version) "")))
          (mapc #'aux (getf foo :data))
          (when first
            (goto-char first)))
        (setq buffer-read-only t)
        (setq asdf-browser-system system)
        (switch-to-buffer buf)))))

(defun asdf-browser-insert-into-buffer (system)
  (asdf-browser-get-components system (asdf-browser-k system)))

(defun asdf-browser-ensure-buffer ()
  (let ((buf (get-buffer-create "*ASDF Browser*")))
    (set-buffer buf)
    (unless (eq major-mode 'asdf-browser-mode)
      (asdf-browser-mode))
    buf))

(defun asdf-browser-go (system)
  (if (asdf-browser-system-exists-p system)
      (let ((buf (asdf-browser-ensure-buffer)))
        (cond (asdf-browser-inhibit-stack
               (setq asdf-browser-inhibit-stack nil))
              (t
               (when (and asdf-browser-system
                          (not (string= asdf-browser-system system)))
                 (push asdf-browser-system asdf-browser-stack))))
        (asdf-browser-insert-into-buffer system))
      (error "No such system: %s" system)))

(defun asdf-browser-prompt ()
  (if (and (boundp 'slime-modeline-package)
           slime-modeline-package)
      slime-modeline-package
      (substring-no-properties (word-at-point))))

(defun asdf-browser (&optional system)
  (interactive)
  (let ((prompt (asdf-browser-prompt)))
    (asdf-browser-go (or system (read-from-minibuffer "System: " prompt)))))
