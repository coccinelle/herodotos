;; org-view-link.el -- support "view" protocol in org mode
;;
;; Author: Rene Rydhof Hansen <rrh@cs.aau.dk>
;;
;; Revision: 0.6
;;
;; Time-stamp: <2008-04-28 11:51:50 rrh>
;;
;; History
;;
;; Revision 0.6: Changed column calc. (weird);
;;               added experimental org-view-link-all
;; Revision 0.5: Fixed one-off end-column; added recenter (annoying); quiet mode
;; Revision 0.4: TABs detected; changed to forward-char
;; Revision 0.3: Changed to 1-based columns


;; TODO
;;
;;  * "Re-centering" (do it right!)
;;  * Re-factoring
;;  * Connecting target regions
;;

;;-----------------------------------------------------------------
;; Introductions
;;-----------------------------------------------------------------

;;
(require 'org)

;; register our handler for "view" protocols
(org-add-link-type "view" 'org-view-link-open)


;;-----------------------------------------------------------------
;; Faces
;;-----------------------------------------------------------------
(defface ovl-face1
  '((((background light)) (:background "yellow"))
    (((background dark)) (:foreground "yellow")))
  "Face for highlights.")

(defface ovl-face2
  '((((background light)) (:background "green"))
    (((background dark)) (:foreground "green")))
  "Face for highlights.")

(defface ovl-face3
  '((((background light)) (:background "red"))
    (((background dark)) (:foreground "red")))
  "Face for highlights.")

(defface ovl-face4
  '((((background light)) (:background "blue"))
    (((background dark)) (:foreground "blue")))
  "Face for highlights.")

(defface ovl-face5
  '((((background light)) (:background "gray91"))
    (((background dark)) (:foreground "gray91")))
  "Face for highlights.")

;; Default face
(defvar org-view-link-default-face 'ovl-face1)


;;-----------------------------------------------------------------
;; Utility functions
;;-----------------------------------------------------------------

;;
;(defun to-pos (lin col)
;  "Converts a line/column pair to a buffer position."
;  (save-excursion
;    (when (and lin col)
;      (goto-line lin)
;      (move-to-column col)
;      (point))))

;; 
;;(defun view-link-calc-overlay-block (lin1 &optional col1 lin2 col2)
;;  "Calculate coordinates of target overlay."
;;  (if (not lin1) (error "No target line specified"))
;;  (let ((newcol1 (- (or col1 1) 1))
;;	(newlin1 (min lin1 (or lin2 lin1)))
;;	(newlin2 (max lin1 (or lin2 lin1)))
;;	newcol2 from to)
;;    (save-excursion
;;      (goto-line lin1)
;;      (beginning-of-line)
;;      (forward-char newcol1)
;;      (setq from (point))
;;      (goto-line newlin2)
;;      (beginning-of-line)
;;      (if col2
;;	  (forward-char col2)
;;	(forward-line 1)
;;	(beginning-of-line))
;;      (setq to (point))
;;      (list from to))))

(defun view-link-calc-overlay-block (lin1 &optional col1 lin2 col2)
  "Calculate coordinates of target overlay.
NOTE: columns are 0-based; begin column is inclusive and end column is exclussive."
  (if (not lin1) (error "No target line specified"))
  (let ((newcol1 (or col1 0))
	(newlin1 (min lin1 (or lin2 lin1)))
	(newlin2 (max lin1 (or lin2 lin1)))
	newcol2 from to)
    (save-excursion
      (goto-line lin1)
      (beginning-of-line)
      (forward-char newcol1)
      (setq from (point))
      (goto-line newlin2)
      (beginning-of-line)
      (if (and col2 (> col2 0))
	  (forward-char col2)
	(forward-line 1)
	(beginning-of-line))
      (setq to (point))
      (list from to))))

;; Create an overlay
(defun view-link-make-overlay (from to &optional name priority)
  "Creates an org-view-link overlay."
  (let ((ovl (make-overlay from to nil t t)))
    (overlay-put ovl 'org-view-overlay (or name t))
    (when priority (overlay-put ovl 'priority priority))
    ovl))

;; Return a list of org-view-link overlays in a given region
(defun view-link-find-overlays (from to &optional name)
  "Find org-view-link overlays in a region."
  (let ((overlays (overlays-in from to))
	found)
    (dolist (ovl overlays found)
      (if (overlay-get ovl 'org-view-overlay)
	  (setq found (cons ovl found))))))

;; Kill all org-view-link overlays in a given region
(defun view-link-kill-overlays (from to &optional name)
  "Remove all org-view-link overlays in a given region."
  (let ((overlays (view-link-find-overlays from to)))
    (dolist (ovl overlays)
      (delete-overlay ovl))))


;;-----------------------------------------------------------------
;;
;;-----------------------------------------------------------------

;; Highlight target region
(defun view-link-highlight-target (lin1 col1 lin2 col2 &optional name face)
  "Highlights the target of a view link."
  (let* ((blk  (view-link-calc-overlay-block lin1 col1 lin2 col2))
	 (from (car blk))
	 (to   (cadr blk))
	 (fac  (or face org-view-link-default-face))
	 ovl)
    (unless (view-link-find-overlays from to)
;      (message "[OVL] Creating new overlay.")
      (setq ovl (view-link-make-overlay from to name))
;      (message (concat "[OVL] Setting face: " (face-name fac)))
      (overlay-put ovl 'face fac))))


;; The view protocol handler. Register with org-mode.
;;
;; TODO
;;  * generalise and decentralise the key/val parsing
;;
(defun org-view-link-open (args)
  "Foo Bar"
  (let (path cmd linbeg linend colbeg colend face name)
    (if (not (string-match "^[^:]+" args))
	(error "Couldn't find file path")
      (setq path (match-string 0 args))
;      (message (concat "[OVL] Found path = " path))
      (while (string-match "::\\([a-zA-Z]+\\)=\\([^:]+\\)" args (match-end 0))
	(setq key (match-string 1 args))
	(setq val (match-string 2 args))
;	(message (concat "[OVL] Found: " key " = " val))
	(cond
	 ((string= key "linb") (setq linbeg (string-to-number val)))
	 ((string= key "line") (setq linend (string-to-number val)))
	 ((string= key "colb") (setq colbeg (string-to-number val)))
	 ((string= key "cole") (setq colend (string-to-number val)))
	 ((string= key "face") (setq face (read val)))
	 ((string= key "name") (setq name val))
	 )
;	(message 
;	 (concat "[OVL] face = " (if (facep face) (face-name face) "NIL")))
	)
      (save-selected-window
	(find-file-other-window path)
	(widen)
	(if linbeg
	    (view-link-highlight-target linbeg colbeg linend colend name face))
	(if linbeg (goto-line linbeg))
	(recenter 1)
	(if (and colbeg (> colbeg 0)) (forward-char (- colbeg 1))))
      )))
     

;; Open all links in a subtree.
;; FIXME: should only open OVL links
;; FIXME: find better way to re-center (currently opens first found link last
;;        and relies on re-centering in org-view-open-link
(defun org-view-link-all ()
  "Open all view links in a subtree."
  (save-excursion
    (org-narrow-to-subtree)
    (org-back-to-heading)
    (let (saved)
      (while (re-search-forward org-bracket-link-regexp nil t)
	(if saved
	    (org-open-at-point)
	  (setq saved (point))))
      (when saved
	(goto-char saved)
	(org-open-at-point)))
    (widen)))

(defun org-view-key ()
  "Convenient for keyboard bindings."
  (org-show-subtree)
  (org-view-link-all))

(defun org-view-mouse ()
  "Convenient for mouse bindings."
  (interactive)
  (org-show-subtree)
  (org-view-link-all))

;; TO DO
;;
;; org-replace-escapes
;; use global properties to set faces, colours, etc.
;; org-find-overlays
;; org-open-link-in-string
;;


(provide 'org-view-link)

