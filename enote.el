;; enote.el --- Ass
;;; Commentary:
;; idgaf

;;; Code:

(defcustom enote-frame-background-color "black"
  "User-defined background color for the frame."
  :type 'string
  :group 'frame-parameters)

(defcustom enote-frame-foreground-color "white"
  "User-defined foreground color for the frame."
  :type 'string
  :group 'frame-parameters)

(defcustom enote-frame-min-width 30
  "Minimum width for enote-frame."
  :type 'integer
  :group 'frame-parameters)

(defcustom enote-frame-min-height 5
  "Minimum height for enote-frame."
  :type 'integer
  :group 'frame-parameters)

(defcustom enote-frame-max-width 100
  "Maximum width for enote-frame."
  :type 'integer
  :group 'frame-parameters)

(defcustom enote-frame-max-height 20
  "Maximum height for enote-frame."
  :type 'integer
  :group 'frame-parameters)

(defvar frame-parameters
  `((border-width . 1)
    (outer-border-width . 1)
    (internal-border-width . 1)
    (child-frame-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (unsplittable . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t)
    (background-color . ,enote-frame-background-color)
    (foreground-color . ,enote-frame-foreground-color))
  "Default child frame parameters.")

(require 'json)
(eval-when-compile
  (require 'cl-lib))

(defvar called-line 0)
(defvar called-file-name "")
(defvar enote-file ".enote"
  "The name of the enote file.")

(defvar enote-frame nil
  "Reference to the currently active enote frame.")

(defvar enote-buffer nil
  "Reference to the currently active enote buffer.")

(defun get-line-and-file-name ()
  "Output the name of the current file."
  (interactive)
  (setq called-line (line-number-at-pos))
  (let ((current-file (buffer-file-name)))
    (when current-file
        (setq called-file-name current-file))))

(defun calculate-cursor-position ()
  "Calculate the position of the cursor in pixel coordinates."
  (let* ((cursor-pos (posn-x-y (posn-at-point)))
         (edge (window-inside-pixel-edges))
         (border (or (alist-get 'internal-border-width frame-parameters) 0))
         (x (+ (car edge) (- (or (car cursor-pos) 0) border)))
         (y (+ (cadr edge) (+ (or (cdr cursor-pos) 0) border))))
    (cons x (+ y 20))))

(defun update-enote-frame-size ()
  "Update the size of the enote frame based on the current window size."
  (when enote-frame
    (let* ((window-width (window-width))
           (window-height (window-height))
           (new-width (max enote-frame-min-width (min window-width enote-frame-max-width)))
           (new-height (max enote-frame-min-height (min window-height enote-frame-max-height))))
      (set-frame-size enote-frame new-width new-height))))

(defun create-enote-frame ()
  "Create an empty enote frame."
  (interactive)
  
  (get-line-and-file-name)

  (let* ((parent (selected-frame))
         (cursor-pos (calculate-cursor-position))
         (parent-width (frame-width parent))
         (parent-height (frame-height parent))
         (width (min enote-frame-max-width
                      (max enote-frame-min-width
                           (round (* parent-width 0.2)))))
         (height (min enote-frame-max-height
                       (max enote-frame-min-height
                            (round (* parent-height 0.2))))))
    (let ((frame
           (make-frame `((parent-frame . ,parent)
                         ,@frame-parameters
                         (width . ,width)
                         (height . ,height)
                         (top . ,(cdr cursor-pos))
                         (left . ,(car cursor-pos))))))
      (when frame
        (setq enote-frame frame)
        (setq-default cursor-type 'box)
        (raise-frame frame)
        (message "Enote frame created")
        (create-enote-buffer "*enote-buffer*" frame)))))

(defun enote--read-note ()
  "Read notes from the enote file and return them as a list."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string)
        notes)
    (setq notes (make-hash-table :test 'equal))

    (when (file-exists-p enote-file)
      (with-temp-buffer
        (insert-file-contents enote-file)
        (unless (zerop (buffer-size))
          (ignore-errors
            (setq notes (json-read)))))
      (unless notes
        (message "Error reading JSON; returning empty notes.")))
    (gethash "notes" notes)))

(defun enote--find-note ()
  "Find a note based on `called-file-name` and `called-line`.
Returns the content of the note if found, otherwise returns nil."
  (let ((notes (enote--read-note))
        (note-table (make-hash-table :test 'equal)))
    
    (dolist (note notes)
      (let ((file (gethash "file" note))
            (line (gethash "line" note))
            (content (gethash "content" note)))
        (puthash (cons file line) content note-table)))

    (gethash (cons called-file-name called-line) note-table)))

(defun enote--read-to-buffer (buffer)
  "Read to BUFFER from enote--read-note function."
  (when (buffer-live-p buffer)
    (let ((note-content (enote--find-note)))
      (with-current-buffer buffer
        ;; Only erase the buffer if there's content to insert
        (when note-content
          (erase-buffer)
          (insert note-content))
        ;; Provide feedback if no note content is found
        (unless note-content
          (message "New note for line %d" called-line))))))

(defun create-enote-buffer (buffer-name parent-frame)
  "Create a new buffer named BUFFER-NAME and switch to it in PARENT-FRAME."
  (let ((new-buffer (generate-new-buffer buffer-name)))
    (with-current-buffer new-buffer
      (setq mode-line-format nil
            header-line-format nil
            tab-line-format nil
            tab-bar-format nil
            display-line-numbers nil
            left-fringe-width nil
            right-fringe-width nil
            left-margin-width 0
            right-margin-width 0)

      (message "Reading to buffer")
      (enote--read-to-buffer new-buffer)
      (setq enote-buffer new-buffer))

    (with-selected-frame parent-frame
      (switch-to-buffer enote-buffer))))

(defun enote--save-buffer-to-file ()
  "Save the current buffer content as a note in the enote file."
  (interactive)
  (message "Saving...")
  (let* ((note-content (buffer-string))
         (notes (enote--read-note))
         (note-exists nil))
    
    (if (or (string= note-content "") (string= called-file-name ""))
        (progn
          (if (string= called-file-name "")
              (message "Cannot create note for an empty buffer.")
            (message "Buffer content is empty, nothing to save.")))
      (when (or (not (file-exists-p enote-file))
                (= (nth 7 (file-attributes enote-file)) 0))
        (message "Enote file does not exist or is empty. Creating an empty .enote file.")
        (with-temp-buffer
          (insert "{\"notes\": []}")
          (if (write-region (point-min) (point-max) enote-file nil 'quiet)
              (message "Succesfully created or/and initialized .enote file")
            (message "Failed to create/initialize .enote file"))))

      (dolist (note notes)
        (when (and (string= (gethash "file" note) called-file-name)
                   (= (gethash "line" note) called-line))
          (setf (gethash "content" note) note-content)
          (setq note-exists t)
          (message "Found existing note for %s at line %d" called-file-name called-line)))

      (unless note-exists
        (let ((new-note (make-hash-table)))
          (setf (gethash "file" new-note) called-file-name)
          (setf (gethash "line" new-note) called-line)
          (setf (gethash "content" new-note) note-content)
          (push new-note notes)))

      (with-temp-buffer
        (insert "{\"notes\": ")
        (insert (json-encode notes))
        (insert "}")
        (write-region (point-min) (point-max) enote-file nil 'quiet))
      (message "Note successfully saved for %s at line %d." called-file-name called-line))))

(defun enote--make-frame ()
  "Make frame visible and manage the associated buffer."
  (interactive)
  (when (and enote-buffer (get-buffer enote-buffer))
      (progn
        (message "Killing existing buffer: %s" enote-buffer)
        (kill-buffer enote-buffer)
        (setq enote-buffer nil)))
  (get-line-and-file-name)
  (create-enote-frame))

(defun enote--kill-frame ()
  "Make frame invisible."
  (interactive)
  (when (and enote-buffer (get-buffer enote-buffer))
    (with-current-buffer enote-buffer
      (enote--save-buffer-to-file))
    (kill-buffer enote-buffer)
    (setq enote-buffer nil))
  (delete-frame enote-frame)
  (setq enote-frame nil))

(defun manage-enote-frame ()
  "Manage the visibility of the enote frame and its buffer."
  (interactive)
  (if (not enote-frame)
      (progn
        (enote--make-frame)
        (select-frame-set-input-focus enote-frame))
    (enote--kill-frame)))

(global-set-key (kbd "C-c c") 'manage-enote-frame)

(provide 'enote)
;;; enote.el ends here
