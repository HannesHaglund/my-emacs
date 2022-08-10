(require 'org)
(require 'org-chef)

(defun ocwp-buffer-recipes ()
  "Return a list of all recipe headings in current buffer."
  (let ((bufc (buffer-substring-no-properties (point-min) (point-max)))
        (rslt '()))
    (with-temp-buffer
      (insert bufc)
      (goto-char (point-min))
      (while (search-forward ":servings: " nil t)
        (save-excursion
          (re-search-backward "^\\*+ ")  ; Search for header
          (beginning-of-line)
          (let* ((heading-beginning (point))
                 (heading-asterix-end (progn (search-forward " ") (point)))
                 (heading-asterixes (buffer-substring heading-beginning heading-asterix-end))
                 (heading-end (if (re-search-forward (concat "^" (regexp-quote heading-asterixes)) nil t)
                                  (progn (beginning-of-line) (point)) (point-max))))
            (add-to-list 'rslt (buffer-substring heading-beginning heading-end))))))
    rslt))


(defun ocwp-heading-headline (heading)
  "Return the headline of a HEADING."
  (with-temp-buffer
    (insert heading)
    (goto-char (point-min))
    (re-search-forward "^\\*+ ")
    (let* ((a (point))
           (b (progn (end-of-line) (point))))
      (buffer-substring-no-properties a b))))


(defun ocwp-insert (heading servings)
  "Insert HEADING and call org-chef-edit-servings to SERVINGS on it."
  (save-excursion
    (let ((start (point)))
      (insert heading)
      (goto-char start)
      (forward-line 1)
      (org-chef-edit-servings servings))))


(defun ocwp-meal-plan-str (servings recipes)
  "Make a meal plan with x SERVINGS based on RECIPES from an org-chef file, return it as str."
  (with-temp-buffer
    (let* ((cur-servings 0))
      (while (< cur-servings servings)
        (let* ((rand-recipe (seq-random-elt recipes))
               (prompt (format "[%d/%d] Add servings of %s: "
                               cur-servings
                               servings
                               (ocwp-heading-headline rand-recipe)))
               (servs-to-add (read-number prompt)))
          (when (> servs-to-add 0)
            (goto-char (point-max))
            (newline)
            (ocwp-insert rand-recipe servs-to-add)
            (setq cur-servings (+ cur-servings servs-to-add))))))
    (buffer-substring-no-properties (point-min) (point-max))))


(defun ocwp-meal-plan (servings recipes)
  "Make a meal plan with x SERVINGS based on RECIPES from an org-chef file."
  (interactive (list
                (read-number "Servings: ")
                (ocwp-buffer-recipes)))
  (with-current-buffer (get-buffer-create "*ocwp-meal-plan*")
    (erase-buffer)
    (insert (ocwp-meal-plan-str servings recipes))
    (goto-char (point-min))
    (org-mode)
    (indent-buffer)
    (display-buffer (current-buffer))))

(provide 'org-chef-week-planning)