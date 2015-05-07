;;;;
;;;; Мой простой менеджер таймлайна.
;;;;
;;;;
(in-package :net.viva-calman.timeline)

;;;
;;; Глобальные переменные
;;;
(defparameter *current-timeline*)
(defvar *current-id* 0)

;;;
;;; Константы
;;;
(defconstant +num-of-elt+ (* 12 24 7))

;;;
;;; Классы
;;;
;(defclass timeslice ()
;  ;; Класс, реализующий единичную последовательность
;  ((id :initarg :id
;       :accessor id
;       :initform *current-id*
;       :documentation "id of timeslice")





;;;
;;; Функции
;;;

(defun genempty ()
  ;; Генерация пустого массива
  (make-array +num-of-elt+ :initial-element 0))

(defun fillarray (id
		  begin
 		  length)
  ;; Заполнение массива
  (loop for i from begin to (+ begin length -1) do (setf (elt *timeline* i) id)))

(defun checkfill (begin
		  length)
  ;; Проверка последовательности на непрерывность
  (let ((recycle 0)
	(leng (+ begin length -1)))
    (when (> leng +num-of-elt+)
      (setf recycle (- +num-of-elt+ leng))
      (setf leng (- leng recycle)))
    
    
    
    
  
