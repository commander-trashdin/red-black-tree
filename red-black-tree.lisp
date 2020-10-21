(defpackage :rb-tree
  (:use :cl))

(in-package :rb-tree)


;;; Container definitions

(defmacro with-snames (&body body)
  (labels ((dotsplit (str)
             (loop :with acc := (make-array 0 :adjustable t :element-type 'base-char)
                   :with res := '()
                   :for char :across str
                   :if (char= #\. char)
                     :do (push acc res)
                         (setf acc (make-array 0 :adjustable t :element-type 'base-char))
                   :else :do (vector-push-extend char acc)
                   :finally (push acc res)
                            (return res)))
           (transform (list)
             (destructuring-bind (fst . rst) list
               (if rst
                   `(slot-value ,(transform rst) ',(intern (string-upcase fst) 'rb-tree))
                   (intern (string-upcase fst) 'rb-tree))))
           (rec (ls)
             (mapcar (lambda (object)
                       (cond ((and (symbolp object) (not (keywordp object)))
                              (transform (dotsplit (string object))))
                             ((consp object)
                              (rec object))
                             (t object)))
                     ls)))
    `(progn ,@(rec body))))


(deftype color () `(member :red :black))

(defclass rb-node ()
 ((parent :initform nil :initarg :parent :type (or rb-node null))
  (key :initarg :key)
  (color :initform :black :initarg :color :type color)
  (left :initform nil :initarg :left :type (or rb-node null))
  (right :initform nil :initarg :right :type (or rb-node null))))


(defclass red-black-tree ()
  ((root
    :type (or null rb-node)
    :initform nil)
   (black-heigt
    :type (integer 0)
    :initform 0)
   (size
    :type (integer 0)
    :initform 0)))


(defun get-grand (node)
  (with-snames
    node.parent.parent))

(defun get-sibling (node)
  (with-snames
    (let ((parent node.parent))
      (and parent
         (if (eq node parent.left)
             parent.right
             parent.left)))))


(defun get-uncle (node)
  (with-snames
    (get-sibling node.parent)))

(defun rotate-left (node tree)
  "Rotates left at the node; resetes the tree root if needed"
  (with-snames
    (with-slots (parent left right) node
      (let ((new right))
        (setf right new.left)
        (when new.left
          (setf new.left.parent node))
        (setf new.parent parent)
        (cond ((not parent)
               (setf tree.root new))
              ((eq node parent.left)
               (setf parent.left new))
              (t (setf parent.right new)))
        (setf new.left node
              parent new)))))


(defun rotate-right (node tree)
  "Rotates right at the node; resetes the tree root if needed"
  (with-snames
    (with-slots (parent left right) node
      (let ((new left))
        (setf left new.right)
        (when new.right
          (setf new.right.parent node))
        (setf new.parent parent)
        (cond ((not parent)
               (setf tree.root new))
              ((eq node parent.right)
               (setf parent.right new))
              (t (setf parent.left new)))
        (setf new.right node
              parent new)))))


(defmethod print-object ((obj rb-node) stream)
  (with-snames
    (format stream "Key:~s, Color:~s, Parent:~s~%Left:" obj.key obj.color (when obj.parent obj.parent.key))
    (print obj.left stream)
    (format stream "~%Right:")
    (print obj.right stream)))


(defmethod print-object ((obj red-black-tree) stream)
  (with-snames
    (format stream "Tree:")
    (print obj.root stream)))



(defun insert (tree key)
  "Insert the key to the tree in its appropriate position and fix the tree"
  (with-snames
    (with-slots (root black-heigt size) tree
      (let ((cur root)
            (next nil))
        (loop :while cur
              :do (setf next cur)
              :if (< key cur.key)
                :do (setf cur cur.left)
              :else :do (setf cur cur.right))
        (let ((new (make-instance 'rb-node :parent next
                                           :color (if next :red :black) ;;TODO can get faster
                                           :key key)))
          (cond ((not next)
                 (setf root new))
                ((< key next.key)
                 (setf next.left new))
                ((> key next.key)
                 (setf next.right new)))
          (when (and next next.parent)
            (fix-insertion new tree)))))))



(defun fix-insertion (node tree)
  (with-snames
    (loop :while (eq :red node.parent.color) ;;TODO any guarantee that it will never be nil?
          :for p := node.parent
          :if (eq p p.parent.right)
            :do (let ((uncle p.parent.left))
                  (if (eq uncle.color :red)
                      (setf uncle.color :black ;; so-called case 3.1
                            p.color :black
                            p.parent.color :red
                            node p.parent)
                      (progn                   ;; case 3.2
                        (when (eq node p.left) ;; case 3.2.2 -> 3.2.1
                          (setf node p)
                          (rotate-right node tree))
                        (setf p.color :black ;; case 3.2.1
                              p.parent.color :red)
                        (rotate-left p.parent tree))))
          :else
            :do (let ((uncle p.parent.right))
                  (if (eq uncle.color :red)
                      (setf uncle.color :black ;; mirror case 3.1
                            p.color :black
                            p.parent.color :red
                            node p.parent)
                      (progn                    ;; case 3.2
                        (when (eq node p.right) ;; case 3.2.2 -> 3.2.1
                          (setf node p)
                          (rotate-left node tree))
                        (setf p.color :black ;; case 3.2.1
                              p.parent.color :red)
                        (rotate-right p.parent tree))))
          :never (eq node tree.root))
    (setf tree.root.color :black)))
