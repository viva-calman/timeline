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
(defconstant +num-of-elt+ (- (* 12 24 7) 1))

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
						     (return-from check-fill nil)))
  (return-from check-fill 1))

(defun check-overload (begin
		       length)
  ;; Проверка переполнения
  (let ((head)
	(tail)
	(inter (+ begin length -1)))
    (if (< +num-of-elt+ inter)
	(progn
	  (format t "overload~%")
	  (setf tail (- inter +num-of-elt+))
	  (setf head (- length tail)))
	(setf head length))    
    (return-from check-overload (list begin head tail))))
		     

(defun operate-array (id
		      begin
		      length)
  ;; Обработка запроса на заполнение
  (let ((diap (check-overload begin length)))
    (if (or (not (third diap)) (= (third diap) 0))
	(if (check-fill begin
			length)
	    (fill-array id
			begin
			length)
	    (show-message "Интервалы перекрываются"))
	(if (and (check-fill (first diap)
			     (second diap))
		 (check-fill 0
			     (third diap)))
    	    (progn
	      (format t "!")
	      (fill-array id
			  (first diap)
			  (second diap))
	      (fill-array id
			  0
			  (third diap)))
	    (show-message "Интервалы перекрываются")))))
	
	



;;;
;;; Служебные функции
;;;

(defun show-message (mess)
  ;; Отображение сообщения
  (format t "~a~%" mess))
