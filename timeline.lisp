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

(defun gen-empty ()
  ;; Генерация пустого массива
  (make-array +num-of-elt+ :initial-element 0))

(defun fill-array (id
		  begin
 		  length)
  ;; Заполнение массива
  (loop for i from begin to (+ begin length -1) do (setf (elt *timeline* i) id)))

(defun check-fill (begin
		  length)
  ;; Проверка последовательности на непрерывность
  (format t "~a - ~a" begin length)
  (loop for i from begin to (+ begin length -1) do (unless (= (elt *timeline* i) 0)
						     (return-from check-fill 0)))
  (return-from check-fill 1))

(defun check-overload (begin
		       length)
  ;; Проверка переполнения
  (if (> +num-of-elt+ (+ begin length -1))
      (check-fill begin length)
      (let* ((tail (- (+ begin length) +num-of-elt+))
	     (head (- length tail)))
	(and (check-fill begin head) (check-fill 0 tail)))))
