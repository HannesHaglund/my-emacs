(require 'org)
(require 'org-chef)
(require 'org-element)
(require 'cl-lib)

(defun ocwp-org-element-parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (org-element--parse-elements
     (point-min) (point-max)
     'first-section nil 'object nil (list 'org-data nil))))

(defun ocwp-two-consecutive-elements-in-list-p (lst e1 e2)
  (if (and (equal (car lst) e1) (equal (cadr lst) e2)) t
    (when (and lst (>= (length lst) 2))
      ;; Recurse
      (ocwp-two-consecutive-elements-in-list-p (cdr lst) e1 e2))))


(defun ocwp-ast-recipe-header-p (ast)
  "Return AST if AST is a recipe header, otherwise nil."
  (when (and
         ;; Top element is a headline
         (eq (car-safe ast) 'headline)
         ;; Contains 1 or more sub-headlines called Ingredients
         (cl-some (lambda (e) (and (listp e)
                                   (eq (car e) 'headline)
                                   (ocwp-two-consecutive-elements-in-list-p (nth 1 e) :raw-value "Ingredients")))
                  (cdr ast)))
    ast))                               ; Return value if t



(defun ocwp-headlines (ast)
  ;; TODO make this recursive on sublists
  ;; Right now it only finds top level headers
  (remq nil (mapcar 'ocwp-ast-recipe-header-p ast)))

(defun ocwp-nshuffle (sequence)
  (cl-loop for i from (length sequence) downto 2
           do (cl-rotatef (elt sequence (random i))
                          (elt sequence (1- i))))
  sequence)

(defvar ocwp-alphabet (remq "" (split-string "abcdefghijklmnopqrstuvwxyz" "")))

(defun ocwp-headline-letters (headlines)
  (let* ((index 0))
    (mapcar #'(lambda (_)
                (setq index (+ index 1))
                (nth (- index 1) ocwp-alphabet))
            headlines)))

(defun ocwp-headlines-titles (headlines) )

(defun ocwp-choice-prompt (headlines headline-letters)
  (concat
   (string-join (mapcar #'(lambda (headlie-title letter)
                            (format "%s) %s" letter headline-title))
                        (ocwp-headlines-titles headlines) headline-letters) "\n")
   "\n\nr) Reroll\nq) Quit\n\nChoose an option: "))

(defun ocwp-choose-once (headlines num-choices)
  (let* ((headlines-shuffled (ocwp-nshuffle headlines))
         (choices (cl-subseq headlines-shuffled 0 (1- num-choices)))
         (choice-chr (read-char-choice  (ocwp-choice-prompt choices
                                                            (ocwp-headline-letters choices))
                                        (ocwp-headline-letters choices))))

    (cond ((string= choice-chr "e") nil) ; exit
          ((string= choice-chr "r") (ocwp-choose-once headlines-num-choices)) ; recurse
                                        ; otherwise...
          ((nth (search choice-chr ocwp-alphabet) 'headlines-shuffled)))))

(defun ocwp-choose-servings ()
  (read-number "Servings: "))

(defun org-chef-week-plan (num-choices servings input-recipe-file)
  (interactive (list
                (read-number "Choices per iteration: ")
                (read-number "Servings: ")
                (if (derived-mode-p 'org-mode)
                    (buffer-file-name)
                  (read-file-name "Org cookbook: " default-directory))))
  (let* ((ast (ocwp-org-element-parse-file input-recipe-file))
         (recipes (ocwp-headlines ast))
         (choices '())
         (choices-servings '()))
    (while (> servings (apply '+ choices-servings))
      (let* ((choice (ocwp-choose-once recipes num-choices))
             (servings (ocwp-choose-servings)))
        (add-to-list 'choices choice)
        (add-to-list 'choices-servings servings)))
    choices
    ))

;; (let ((astfoo (ocwp-org-element-parse-file input-recipe-file))
;;       (recipes (ocwp-headlines))
;;       )
;;   ))
;;
;; ast))


(provide 'org-chef-week-planning)