;;; poker-hands-mode.el --- Solve the poker hands kata -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Sebastian Hans

;; Author: Sebastian Hans <github@sebastian-hans.de>
;; Version: 0.1
;; Package-Requires: ((emacs "25") (cl-lib) (dash))

;; This file is not part of GNU Emacs.
(provide 'poker-hands-mode)

(eval-when-compile (require 'cl-lib))
(require 'dash)

(defvar poker-hands-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'poker-hands-mode-eval-current-line)
    map))

(define-derived-mode poker-hands-mode nil "Poker"
  "Major mode implementing the poker hands kata (see https://codingdojo.org/kata/PokerHands/).")

(defun poker-hands-mode-eval-current-line ()
  (interactive)
  (let* ((line (thing-at-point 'line)))
    (when-let ((result (poker-hands-mode--play-line line)))
      (save-excursion
        (beginning-of-line)
        (when (re-search-forward "[[:space:]]*|" (line-end-position) 'noerror)
          (skip-chars-backward "[:space:]|")
          (delete-region (point) (line-end-position)))
        (insert " | ")
        (insert (poker-hands-mode--format-game-result result))))))

(defconst poker-hands-mode--card-regex "[[:space:]]*\\([2-9TJQKA][CDHS]\\)")
(defconst poker-hands-mode--player-regex "[[:space:]]*\\([[:graph:]]+\\)[[:space:]]*:")
(defconst poker-hands-mode--game-regex (concat "^"
                                               poker-hands-mode--player-regex
                                               poker-hands-mode--card-regex
                                               poker-hands-mode--card-regex
                                               poker-hands-mode--card-regex
                                               poker-hands-mode--card-regex
                                               poker-hands-mode--card-regex
                                               poker-hands-mode--player-regex
                                               poker-hands-mode--card-regex
                                               poker-hands-mode--card-regex
                                               poker-hands-mode--card-regex
                                               poker-hands-mode--card-regex
                                               poker-hands-mode--card-regex
                                               "[[:space:]]*\\($\\|\\)"
                                               ))

(defun poker-hands-mode--play-line (line)
  (when (string-match poker-hands-mode--game-regex line)
    (let* ((msnp (lambda (n) (match-string-no-properties n line)))
           (name-a (funcall msnp 1))
           (hand-a (list (funcall msnp 2) (funcall msnp 3) (funcall msnp 4) (funcall msnp 5) (funcall msnp 6)))
           (name-b (funcall msnp 7))
           (hand-b (list (funcall msnp 8) (funcall msnp 9) (funcall msnp 10) (funcall msnp 11) (funcall msnp 12))))
      (poker-hands-mode--play name-a hand-a name-b hand-b))))

(defun poker-hands-mode--format-game-result (result)
  (pcase result
    ((let 'tie (aref result 0)) (concat "Tie on " (format "%s" (aref result 1))))
    (_ (seq-let [who rank format-rank &rest rank-result] result
         (concat who " wins with " (funcall format-rank rank-result))))))

(defun poker-hands-mode--card-value (card)
  (case (aref card 0)
    (?2 2)
    (?3 3)
    (?4 4)
    (?5 5)
    (?6 6)
    (?7 7)
    (?8 8)
    (?9 9)
    (?T 10)
    (?J 11)
    (?Q 12)
    (?K 13)
    (?A 14)))

(defun poker-hands-mode--card-suit (card)
  (case (aref card 1)
    (?C 'clubs)
    (?D 'diamonds)
    (?H 'hearts)
    (?S 'spades)))

(defun poker-hands-mode--format-card-value (card-value)
  (case card-value
    (2 "2")
    (3 "3")
    (4 "4")
    (5 "5")
    (6 "6")
    (7 "7")
    (8 "8")
    (9 "9")
    (10 "10")
    (11 "jack")
    (12 "queen")
    (13 "king")
    (14 "ace")))

(defun poker-hands-mode--format-card-suit (card-suit)
  (case card-suit
    ('clubs "clubs")
    ('diamonds "diamonds")
    ('hearts "hearts")
    ('spades "spades")))

(defun poker-hands-mode--by-card-value (a b)
  (> (poker-hands-mode--card-value a)
     (poker-hands-mode--card-value b)))

(defun poker-hands-mode--compare-card-values (card-values-a card-values-b)
  (--reduce-from (or acc
                    (let ((left (car it))
                          (right (cdr it)))
                      (cond
                       ((> left right) (cons 'left left))
                       ((< left right) (cons 'right right)))))
              nil
              (-zip-pair card-values-a card-values-b)))

(defun poker-hands-mode--sorted-card-values-ignoring (hand values-to-ignore)
  (->> hand
       (-map 'poker-hands-mode--card-value)
       (--remove (-contains? values-to-ignore it))
       (-sort '>)))

(defun poker-hands-mode--win-by-high-card (rank rank-formatter name-a hand-a name-b hand-b values-to-ignore)
  (let* ((sorted-a (poker-hands-mode--sorted-card-values-ignoring hand-a values-to-ignore))
         (sorted-b (poker-hands-mode--sorted-card-values-ignoring hand-b values-to-ignore))
         (result (poker-hands-mode--compare-card-values sorted-a sorted-b)))
    (case (car result)
      ('left (vector name-a rank rank-formatter (cdr result)))
      ('right (vector name-b rank rank-formatter (cdr result)))
      (t (vector 'tie rank)))))

(defun poker-hands-mode--high-card-formatter (result)
  (seq-let [high-card-value] result
     (concat "high card: " (poker-hands-mode--format-card-value high-card-value))))

(defun poker-hands-mode--high-card (name-a hand-a name-b hand-b)
  (poker-hands-mode--win-by-high-card 'high-card #'poker-hands-mode--high-card-formatter name-a hand-a name-b hand-b nil))

(defun poker-hands-mode--freq (hand)
  (seq-reduce (lambda (frequencies card)
                (let* ((card-value (poker-hands-mode--card-value card))
                       (frequency (alist-get card-value frequencies 0)))
                  (setf (alist-get card-value frequencies) (1+ frequency))
                  frequencies))
              hand
              nil))

(defun poker-hands-mode--find-tuples (n hand)
  (mapcar #'car
          (seq-filter (lambda (value-frequency) (= (cdr value-frequency) n))
                      (poker-hands-mode--freq hand))))

(defun poker-hands-mode--find-single-pair (hand)
  (when-let ((pairs (poker-hands-mode--find-tuples 2 hand)))
    (when (= (length pairs) 1)
      (car pairs))))

(defun poker-hands-mode--pair-formatter (result)
  (seq-let [pair-value] result
    (concat "pair of " (poker-hands-mode--format-card-value pair-value) "s")))

(defun poker-hands-mode--pair-hc-formatter (result)
  (seq-let [high-card-value] result
    (concat "pair with high card: " (poker-hands-mode--format-card-value high-card-value))))

(defun poker-hands-mode--pair (name-a hand-a name-b hand-b)
  (let ((pair-a (poker-hands-mode--find-single-pair hand-a))
        (pair-b (poker-hands-mode--find-single-pair hand-b)))
    (cond ((and pair-a (not pair-b))
           (vector name-a 'pair #'poker-hands-mode--pair-formatter pair-a))
          ((and pair-b (not pair-a))
           (vector name-b 'pair #'poker-hands-mode--pair-formatter pair-b))
          ((and pair-a pair-b)
           (cond
            ((> pair-a pair-b) (vector name-a 'pair #'poker-hands-mode--pair-formatter pair-a))
            ((> pair-b pair-a) (vector name-b 'pair #'poker-hands-mode--pair-formatter pair-b))
            (t (poker-hands-mode--win-by-high-card 'pair-with-high-card #'poker-hands-mode--pair-hc-formatter name-a hand-a name-b hand-b (list pair-a))))))))

(defun poker-hands-mode--two-pairs-formatter (result)
  (seq-let [[pair1-value pair2-value]] result
    (concat "two pairs, "
            (poker-hands-mode--format-card-value pair1-value)
            "s and "
            (poker-hands-mode--format-card-value pair2-value)
            "s")))

(defun poker-hands-mode--two-pairs-hc-formatter (result)
  (seq-let [high-card-value] result
    (concat "two pairs with high card: " (poker-hands-mode--format-card-value high-card-value))))

(defun poker-hands-mode--two-pairs (name-a hand-a name-b hand-b)
  (let* ((pairs-a (-sort '> (poker-hands-mode--find-tuples 2 hand-a)))
         (pairs-b (-sort '> (poker-hands-mode--find-tuples 2 hand-b)))
         (num-pairs-a (length pairs-a))
         (num-pairs-b (length pairs-b)))
    (pcase (list (= 2 num-pairs-a) (= 2 num-pairs-b))
      ('(t nil) (vector name-a 'two-pairs #'poker-hands-mode--two-pairs-formatter pairs-a))
      ('(nil t) (vector name-b 'two-pairs #'poker-hands-mode--two-pairs-formatter pairs-b))
      ('(t t) (pcase (car (poker-hands-mode--compare-card-values pairs-a pairs-b))
                ('left (vector name-a 'two-pairs #'poker-hands-mode--two-pairs-formatter pairs-a))
                ('right (vector name-b 'two-pairs #'poker-hands-mode--two-pairs-formatter pairs-b))
                (_ (poker-hands-mode--win-by-high-card 'two-pairs-with-high-card #'poker-hands-mode--two-pairs-hc-formatter name-a hand-a name-b hand-b pairs-a)))))))

(defun poker-hands-mode--three-of-a-kind-formatter (result)
  (seq-let [triple-value] result
    (concat "three " (poker-hands-mode--format-card-value triple-value) "s")))

(defun poker-hands-mode--three-of-a-kind (name-a hand-a name-b hand-b)
  (let* ((triple-a (poker-hands-mode--find-tuples 3 hand-a))
         (triple-b (poker-hands-mode--find-tuples 3 hand-b))
         (triple-a? (not (null triple-a)))
         (triple-b? (not (null triple-b))))
    (pcase (list triple-a? triple-b?)
      ('(t nil) (vector name-a 'three-of-a-kind #'poker-hands-mode--three-of-a-kind-formatter (car triple-a)))
      ('(nil t) (vector name-b 'three-of-a-kind #'poker-hands-mode--three-of-a-kind-formatter (car triple-b)))
      ('(t t) (if (> (car triple-a) (car triple-b))
                  (vector name-a 'three-of-a-kind #'poker-hands-mode--three-of-a-kind-formatter (car triple-a))
                (vector name-b 'three-of-a-kind #'poker-hands-mode--three-of-a-kind-formatter (car triple-b)))))))

(defun poker-hands-mode--straight? (hand)
  (let ((sorted-hand (-sort 'poker-hands-mode--by-card-value hand)))
    (not (not (seq-reduce (lambda (prev-value card)
                            (and prev-value
                                 (when (= (poker-hands-mode--card-value card) (1- prev-value))
                                   (1- prev-value))))
                          sorted-hand
                          (1+ (poker-hands-mode--card-value (car sorted-hand))))))))

(defun poker-hands-mode--straight-formatter (result)
  "straight")

(defun poker-hands-mode--straight-hc-formatter (result)
  (seq-let [high-card-value] result
    (concat "straight with high card: " (poker-hands-mode--format-card-value high-card-value))))

(defun poker-hands-mode--straight (name-a hand-a name-b hand-b)
  (let* ((straight?-a (poker-hands-mode--straight? hand-a))
         (straight?-b (poker-hands-mode--straight? hand-b)))
    (pcase (list straight?-a straight?-b)
      ('(t nil) (vector name-a 'straight #'poker-hands-mode--straight-formatter))
      ('(nil t) (vector name-b 'straight #'poker-hands-mode--straight-formatter))
      ('(t t) (poker-hands-mode--win-by-high-card 'straight-with-high-card #'poker-hands-mode--straight-hc-formatter name-a hand-a name-b hand-b nil)))))

(defun poker-hands-mode--flush-suit (hand)
  (let ((suit (poker-hands-mode--card-suit (car hand))))
    (when (seq-every-p (lambda (card) (eq (poker-hands-mode--card-suit card) suit)) hand)
      suit)))

(defun poker-hands-mode--flush-formatter (result)
  (seq-let [suit] result
    (concat "flush of " (poker-hands-mode--format-card-suit suit))))

(defun poker-hands-mode--flush-hc-formatter (result)
  (seq-let [high-card-value suit] result
    (concat "flush of "
            (poker-hands-mode--format-card-suit suit)
            " with high card: "
            (poker-hands-mode--format-card-value high-card-value))))

(defun poker-hands-mode--flush (name-a hand-a name-b hand-b)
  (let* ((flush-suit-a (poker-hands-mode--flush-suit hand-a))
         (flush-suit-b (poker-hands-mode--flush-suit hand-b)))
    (cond
     ((and flush-suit-a (not flush-suit-b)) (vector name-a 'flush #'poker-hands-mode--flush-formatter flush-suit-a))
     ((and (not flush-suit-a) flush-suit-b) (vector name-b 'flush #'poker-hands-mode--flush-formatter flush-suit-b))
      ((and flush-suit-a flush-suit-b)
       (let ((high-card-result
              (poker-hands-mode--win-by-high-card 'flush-with-high-card #'poker-hands-mode--flush-hc-formatter name-a hand-a name-b hand-b nil)))
         (pcase (aref high-card-result 0)
           ('tie high-card-result)
           ((pred (string= name-a)) (vconcat high-card-result (list flush-suit-a)))
           ((pred (string= name-b)) (vconcat high-card-result (list flush-suit-b)))))))))

(defun poker-hands-mode--full-house-formatter (result)
  (seq-let [triple-value pair-value] result
    (concat "full house, "
            (poker-hands-mode--format-card-value triple-value)
            " over "
            (poker-hands-mode--format-card-value pair-value))))

(defun poker-hands-mode--full-house (name-a hand-a name-b hand-b)
  (let* ((triples-a (poker-hands-mode--find-tuples 3 hand-a))
         (triples-b (poker-hands-mode--find-tuples 3 hand-b))
         (pairs-a (poker-hands-mode--find-tuples 2 hand-a))
         (pairs-b (poker-hands-mode--find-tuples 2 hand-b))
         (full-house-a? (not (not (and triples-a pairs-a))))
         (full-house-b? (not (not (and triples-b pairs-b)))))
    (pcase (list full-house-a? full-house-b?)
      ('(t nil) (vector name-a 'full-house #'poker-hands-mode--full-house-formatter (car triples-a) (car pairs-a)))
      ('(nil t) (vector name-b 'full-house #'poker-hands-mode--full-house-formatter (car triples-b) (car pairs-b)))
      ('(t t) (if (> (car triples-a) (car triples-b))
                  (vector name-a 'full-house #'poker-hands-mode--full-house-formatter (car triples-a) (car pairs-a))
                (vector name-b 'full-house #'poker-hands-mode--full-house-formatter (car triples-b) (car pairs-b)))))))

(defun poker-hands-mode--four-of-a-kind-formatter (result)
  (seq-let [quadruple-value] result
    (concat "four " (poker-hands-mode--format-card-value quadruple-value) "s")))

(defun poker-hands-mode--four-of-a-kind (name-a hand-a name-b hand-b)
  (let* ((quadruple-a (poker-hands-mode--find-tuples 4 hand-a))
         (quadruple-b (poker-hands-mode--find-tuples 4 hand-b))
         (quadruple-a? (not (null quadruple-a)))
         (quadruple-b? (not (null quadruple-b))))
    (pcase (list quadruple-a? quadruple-b?)
      ('(t nil) (vector name-a 'four-of-a-kind #'poker-hands-mode--four-of-a-kind-formatter (car quadruple-a)))
      ('(nil t) (vector name-b 'four-of-a-kind #'poker-hands-mode--four-of-a-kind-formatter (car quadruple-b)))
      ('(t t) (if (> (car quadruple-a) (car quadruple-b))
                  (vector name-a 'four-of-a-kind #'poker-hands-mode--four-of-a-kind-formatter (car quadruple-a))
                (vector name-b 'four-of-a-kind #'poker-hands-mode--four-of-a-kind-formatter (car quadruple-b)))))))

(defun poker-hands-mode--straight-flush-formatter (result)
  (seq-let [suit] result
    (concat "straight flush of " (poker-hands-mode--format-card-suit suit))))

(defun poker-hands-mode--straight-flush-hc-formatter (result)
  (seq-let [high-card-value suit] result
    (concat "straight flush of "
            (poker-hands-mode--format-card-suit suit)
            " with high card: "
            (poker-hands-mode--format-card-value high-card-value))))

(defun poker-hands-mode--straight-flush (name-a hand-a name-b hand-b)
  (let* ((straight?-a (poker-hands-mode--straight? hand-a))
         (straight?-b (poker-hands-mode--straight? hand-b))
         (flush-suit-a (poker-hands-mode--flush-suit hand-a))
         (flush-suit-b (poker-hands-mode--flush-suit hand-b))
         (straight-flush?-a (and flush-suit-a straight?-a))
         (straight-flush?-b (and flush-suit-b straight?-b)))
    (pcase (list straight-flush?-a straight-flush?-b)
      ('(t nil) (vector name-a 'straight-flush #'poker-hands-mode--straight-flush-formatter flush-suit-a))
      ('(nil t) (vector name-b 'straight-flush #'poker-hands-mode--straight-flush-formatter flush-suit-b))
     ('(t t)
      (let ((high-card-result
             (poker-hands-mode--win-by-high-card 'straight-flush-with-high-card #'poker-hands-mode--straight-flush-hc-formatter name-a hand-a name-b hand-b nil)))
        (pcase (aref high-card-result 0)
          ('tie high-card-result)
          ((pred (string= name-a)) (vconcat high-card-result (list flush-suit-a)))
          ((pred (string= name-b)) (vconcat high-card-result (list flush-suit-b)))))))))

(defun poker-hands-mode--play (name-a hand-a name-b hand-b)
  (seq-reduce (lambda (result rank)
                (or result (funcall rank name-a hand-a name-b hand-b)))
              '(poker-hands-mode--straight-flush
                poker-hands-mode--four-of-a-kind
                poker-hands-mode--full-house
                poker-hands-mode--flush
                poker-hands-mode--straight
                poker-hands-mode--three-of-a-kind
                poker-hands-mode--two-pairs
                poker-hands-mode--pair
                poker-hands-mode--high-card)
              nil))
