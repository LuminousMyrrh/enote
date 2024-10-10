;; enote.el --- Ass
;;; Commentary:
;; yo yo yo

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

(defvar called-line 0
  "Line from which was called enote")
(defvar called-file-name ""
  "File name from which was called enote")
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

(defun calculate-frame-position ()
  "Calculate the position of the cursor in pixel coordinates."
  (let* ((cursor-pos (posn-x-y (posn-at-point)))
         (edge (window-inside-pixel-edges))
         (border (or (alist-get 'internal-border-width frame-parameters) 0))
         (x (+ (car edge) (- (or (car cursor-pos) 0) border)))
         (y (+ (cadr edge) (+ (or (cdr cursor-pos) 0) border))))
    (cons x (+ y 20))))

(defun create-enote-frame ()
  "Create an empty enote frame."
  (interactive)
  
  (get-line-and-file-name)

  (let* ((parent (selected-frame))
         (frame-pos (calculate-frame-position))
         (parent-width (frame-width parent))
         (parent-height (frame-height parent))

         (width (min enote-frame-max-width
                      (max enote-frame-min-width
                           (round (* parent-width 0.25)))))
         (height (min enote-frame-max-height
                       (max enote-frame-min-height
                            (round (* parent-height 0.25))))))
    (let ((frame
           (make-frame `((parent-frame . ,parent)
                         ,@frame-parameters
                         (width . ,width)
                         (height . ,height)
                         (top . ,(cdr frame-pos))
                         (left . ,(car frame-pos))))))
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
Returns the content of the note and its mode if found, otherwise returns nil."
  (let ((notes (enote--read-note))
        (note-table (make-hash-table :test 'equal)))
    
    (when notes
      (maphash (lambda (file note-list)
                 (dolist (note note-list)
                   ;; Extract line, content, and mode from each note
                   (let ((line (gethash "line" note))
                         (content (gethash "content" note))
                         (mode (gethash "mode" note)))
                     ;; Store the note in the note-table using a cons of file and line as key
                     (puthash (cons file line)
                              `(,content ,mode)
                              note-table))))
               notes))

    (gethash (cons called-file-name called-line) note-table)))

(defun enote--read-to-buffer (buffer)
  "Read to BUFFER from enote--read-note function."
  (when (buffer-live-p buffer)
    (let* ((note-data (enote--find-note))
           (note-content nil)
           (note-mode nil))
      (when note-data
        (setq note-content (car note-data))
        (setq note-mode (cadr note-data)))

      (with-current-buffer buffer
        (when note-content
          (erase-buffer)
          (insert note-content)
          (when note-mode
            (funcall (intern note-mode))))
        
        (unless note-content
          (message "New note for line %d" called-line))))))

(defun create-enote-buffer (buffer-name parent-frame)
  "Create a new buffer named BUFFER-NAME and switch to it in PARENT-FRAME."
  (let ((new-buffer (generate-new-buffer buffer-name)))
    (with-selected-frame parent-frame
      (setq header-line-format nil
            tab-line-format nil
            tab-bar-format nil
            display-line-numbers nil
            left-fringe-width nil
            right-fringe-width nil
            left-margin-width 0
            right-margin-width 0)
      (with-current-buffer new-buffer
        (enote--read-to-buffer new-buffer)
        (setq enote-buffer new-buffer)
        (setq mode-line-format nil))
      
      (switch-to-buffer enote-buffer))))

(defun enote--sort-notes (notes)
  "Sort NOTES based on their line number."
  (message "Sorting...")
  (sort notes (lambda (a b)
                 (let ((line-a (gethash "line" a))
                       (line-b (gethash "line" b)))
                   (and line-a line-b
                        (< line-a line-b))))))

(defun create-new-note (new-note)
  (setf (gethash "line" new-note) called-line
        (gethash "mode" new-note) mode-name
        (gethash "content" new-note) note-content))

(defun enote--save-buffer-to-file ()
  "Save the current buffer content as a note in the enote file."
  (interactive)
  (message "Saving...")
  (let* ((note-content (buffer-string))
         (notes (enote--read-note))
         (mode-name (symbol-name major-mode))
         (note-exists nil)
         (file-sections-in-notes (if notes
                                     (gethash called-file-name notes)
                                   nil)))

    (if (or (string= note-content "") (string= called-file-name ""))
        (message (if (string= called-file-name "")
                    "Cannot create note for an empty buffer."
                  "Buffer content is empty, nothing to save."))

      (when (or (not (file-exists-p enote-file))
                (= (nth 7 (file-attributes enote-file)) 0))
        (message "Enote file does not exist or is empty. Creating an empty .enote file.")

        (with-temp-buffer
          (insert "{\"notes\": {}}")
          (condition-case err
              (if (write-region (point-min) (point-max) enote-file nil 'quiet)
                  (message "Successfully created or initialized .enote file")
                (message "Failed to create/initialize .enote file"))
            (error
              (message "Error in creating/initializing enote file: %s" err)))))

      (unless notes
        (setq notes (enote--read-note)))

      (if file-sections-in-notes
          (progn
            (dolist (note file-sections-in-notes)
              (let ((note-line (gethash "line" note))
                    (note-content-hash (gethash "content" note))
                    (note-mode-hash (gethash "mode" note)))
                (when (= note-line called-line)
                  (when (not (string= note-content note-content-hash))
                    (setf (gethash "content" note) note-content)
                    (message "Content updated"))
                  (when (not (string= mode-name note-mode-hash))
                    (setf (gethash "mode" note) mode-name)
                    (message "Mode updated"))
                  (setq note-exists t))))

            (unless note-exists
              (let ((new-note (make-hash-table)))
                (message "Creating new note for file: %s" called-file-name)
                (create-new-note new-note)
                (push new-note file-sections-in-notes)
                (puthash called-file-name file-sections-in-notes notes))))
        (message "File sections doesnt exist, creating one")
        (let ((new-note (make-hash-table)))
          (create-new-note new-note)
          (puthash called-file-name (list new-note) notes)))

      (enote--sort-notes (gethash called-file-name notes))
      (with-temp-buffer
        (insert "{ \"notes\":\n")
        (insert (json-encode notes))
        (insert "\n}")
        (write-region (point-min) (point-max) enote-file nil 'quiet))
      
      (message "Note successfully saved for %s at line %d." called-file-name called-line))))


(defun enote--list-notes ()
  "List all notes in the enote file."
  (let ((notes (enote--read-note))
        (note-list '()))
    (dolist (note notes)
      (let ((file (gethash "file" note))
            (line (gethash "line" note)))
        (push (format "%s at line %d" file line) note-list)))
    (if note-list
        (message "Notes:\n%s" (mapconcat 'identity (nreverse note-list) "\n"))
      (message "No notes found."))))

(defun enote--delete-note ()
  "Delete a selected note from the enote file."
  (interactive)
  (let ((notes (enote--read-note))
        selected-note)
    ;; List notes and prompt user to select one
    (enote--list-notes)
    (setq selected-note
          (completing-read "Select a note to delete: "
                           (mapcar (lambda (note)
                                     (format "%s at line %d"
                                             (gethash "file" note)
                                             (gethash "line" note)))
                                   notes)))
    
    ;; Extract file and line number from the selected note
    (when selected-note
      (let* ((parts (split-string selected-note " at line "))
             (file-name (car parts))
             (line-number (string-to-number (cadr parts))))
        
        ;; Remove the selected note from the notes list
        (setq notes
              (cl-remove-if
               (lambda (note)
                 (and
                  (string= file-name (gethash "file" note))
                  (= line-number (gethash "line" note))))
               notes))

        ;; Save updated notes back to the enote file
        (with-temp-buffer
          (insert "{\"notes\": ")
          (insert (json-encode notes))
          (insert "}")
          (write-region (point-min) (point-max) enote-file nil 'quiet))
        ;; Notify user of deletion
        (message "Deleted note for %s at line %d." file-name line-number)))))

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
(global-set-key (kbd "C-c g") 'enote--delete-note)


;;; ENOTE-LINE

(defun enote--next-note ()
  (interactive)
  "Move to the next line that has a note."
  (let* ((notes (enote--read-note)) ; Read all notes
         (current-line (line-number-at-pos))
         (next-line nil))
    (setq next-line
          (catch 'found
            (maphash (lambda (file note-list)
                         (dolist (note note-list)
                           (let ((line (gethash "line" note)))
                             (when (and (> line current-line)
                                        (string= file buffer-file-name))
                               (throw 'found line)))))
                     notes)))
    (if next-line
        (progn
          (setq called-line next-line)
          (goto-line next-line))
      (message "No next note" next-line))))

(defun enote--previous-note ()
  "Move to the previous line that has a note."
  (interactive)
  (let* ((notes (enote--read-note)) ; Read all notes
         (current-line (line-number-at-pos))
         (prev-line nil))
    (setq prev-line
          (catch 'found
            (maphash (lambda (file note-list)
                         (dolist (note (reverse note-list))
                           (let ((line (gethash "line" note)))
                             (when (and (< line current-line)
                                        (string= file buffer-file-name))
                               (throw 'found line)))))
                     notes)))
    (if prev-line
        (progn
          (setq called-line prev-line)
          (goto-line prev-line))
      (message "No previous note"))))

(global-set-key (kbd "C-c h") 'enote--next-note)
(global-set-key (kbd "C-c l") 'enote--previous-note)

(provide 'enote)
;;; enote.el ends here
