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
(defconstant +noe+ (* 12 24 7))
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
  (make-array +noe+ :initial-element 0))

(defun fill-array (id
		  begin
 		  length)
  ;; Заполнение массива
  (loop for i from begin to (+ begin length -1) do (setf (elt *timeline* i) id)))

(defun check-fill (begin
		  length)
  ;; Проверка последовательности на непрерывность
  (loop for i from begin to (+ begin length -1) do (unless (= (elt *timeline* i) 0)
						     (return-from check-fill i)))
  (return-from check-fill nil))

(defun check-overload (begin
		       length)
  ;; Проверка переполнения
  (let ((head)
	(tail)
	(inter (+ begin length -1)))
    (if (< +num-of-elt+ inter)
	(progn
	  (setf tail (- inter +num-of-elt+))
	  (setf head (- length tail)))
	(setf head length))    
    (return-from check-overload (list begin head tail))))

(defun input-operate (id
		      beginlist
		      length)
  ;; Внесение введенных данных в массив
  (loop for i in beginlist do (operate-array id i length)))
  

(defun operate-array (id
		      begin
		      length)
  ;; Обработка запроса на заполнение
  (let ((diap (check-overload begin length)))
    (if (or (not (third diap)) (= (third diap) 0))
	(if (not (check-fill begin
			     length))
	    (fill-array id
			begin
			length)
	    (progn
	      (show-message "Интервалы перекрываются")
	      (show-message "Точка перекрытия:")
	      (nice-time (parse-time
			  (convert-interval
			   (check-fill begin
				       length))))))
	(if (and (not (check-fill (first diap)
				  (second diap)))
		 (not (check-fill 0
				  (third diap))))
    	    (progn
	      (fill-array id
			  (first diap)
			  (second diap))
	      (fill-array id
			  0
			  (third diap)))
	    (progn
	      (show-message "Интервалы перекрываются")
	      (show-message "Точка перекрытия:")
	      (nice-time (parse-time
			  (convert-interval
			   (or (check-fill (first diap)
					   (second diap))
			       (check-fill 0
					   (third diap))))))))))))
	
	



;;;
;;; Служебные функции
;;;

(defun show-message (mess)
  ;; Отображение сообщения
  (format t "~a~%" mess))

(defun convert-interval (interval)
  ;; Перевод номера интервала в читаемый вид
  (let* ((hour 12)
	 (day (* hour 24))
	 (dw)
	 (h)
	 (m))
    (setf dw (floor interval day))
    (setf h (floor (- interval (* day dw)) hour))
    (setf m (* (- interval (* day dw) (* hour h)) 5))
    (return-from convert-interval (list dw h m))))

(defun parse-time (timelist)
  ;; Красивый вывод таймстампа
  (let ((out-dw)
	(out-h)
	(out-m))
    (cond
      ((= (first timelist) 0)
       (setf out-dw "Пн."))
      ((= (first timelist) 1)
       (setf out-dw "Вт."))
      ((= (first timelist) 2)
       (setf out-dw "Ср."))
      ((= (first timelist) 3)
       (setf out-dw "Чт."))
      ((= (first timelist) 4)
       (setf out-dw "Пт."))
      ((= (first timelist) 5)
       (setf out-dw "Сб."))
      ((= (first timelist) 6)
       (setf out-dw "Вс."))
      (t (setf out-dw "Свячельник")))
    (if (< (second timelist) 10)
	(setf out-h (concatenate 'string (write-to-string 0) (write-to-string (second timelist))))
	(setf out-h (second timelist)))
    (if (< (third timelist) 10)
	(setf out-m (concatenate 'string (write-to-string 0) (write-to-string (third timelist))))
	(setf out-m (third timelist)))
    (return-from parse-time (list out-dw out-h out-m))))

(defun nice-time (timelist)
  ;; Отображение времени
  (format t "~a,~3t~a:~a~%" (first timelist) (second timelist) (third timelist)))

(defun read-input (prompt)
  ;; Чтение пользовательского ввода
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun timeinput ()
  ;; Ввод времени
  (let ((output))
    (show-message "Введите время начала отрезка, в формате ЧЧ:ММ
Разделитель может быть любым, минуты округляются автоматически до ближайшего числа, кратного пяти")
    (push (parse-timeinput (read-input ">")) output)
    (show-message "Введите время окончания отрезка" )
    (push (do
	   ((out (parse-timeinput (read-input ">")) (parse-timeinput (read-input ">"))))
	   ((< (first output) out) out)) output)
    (show-message "Укажите дни недели, на которые применяется этот отрезок. Можно указать диапазон, через дефис или последовательно в любой форме и с любым разделителем, кроме дефиса")
    (push (parse-dayofweek (read-input ">")) output)
    (return-from timeinput output)))

(defun gen-interval-list (days
			  hours)
  ;; Объединение времени ввода и дней недели
  (let ((intlist (gen-day-interval (loop for i in (second days)
				      collect (parse-integer i))))
	(intlist2 (loop for i in (second days)
		      collect (parse-integer i))))
    (cond
      ((= (first days) 1)
       (loop for i in intlist collect (+ i hours)))
      ((= (first days) 2)
       (loop for i in intlist2 collect (+ (* (- i 1) (* 12 24)) hours))))))
    


(defun gen-day-interval (intlist)
  ;; День в интервалы
  (loop for i in intlist collect (* (- i 1) (* 12 24)))) 
	  
			      

(defun parse-timeinput (timestring)
  ;; Функция, разбирающая пользовательский ввод времени.
  (let ((parsed-string (first (cl-ppcre:all-matches-as-strings "\\d{1,2}\\D+\\d{1,2}" timestring))))
    (if (not (eq parsed-string nil))
	(convert-time (cl-ppcre:all-matches-as-strings "\\d{1,2}" parsed-string))
	(show-message "Неверный формат времени"))))

(defun convert-time (timelist)
  ;; Перевод времени в интервалы
  (let ((hour (parse-integer (first timelist)))
	(minute (parse-integer (second timelist)))
	(inter))
    (if (or (> hour 23) (> minute 59))
	(show-message "Неверное значение времени"))
    (setf inter (* hour 12))
    (if (> (mod minute 5) 2)
	(setf inter (+ inter (truncate minute 5) 1))
	(setf inter (+ inter (truncate minute 5))))
    (return-from convert-time inter)))

(defun parse-dayofweek (dow)
  ;; Обработка введенного значения дня недели
  (let ((workdow (cl-ppcre:regex-replace-all "\\D|[890]" dow ""))
	(diap (cl-ppcre:all-matches-as-strings "\\d"(first (cl-ppcre:all-matches-as-strings "[1-7]-[1-7]" dow)))))
    (cond
      ((cl-ppcre:all-matches-as-strings "[1-7]-[1-7]" dow)
       (return-from parse-dayofweek (loop  for i from (parse-integer (first diap))
				       to (parse-integer (second diap))
				       collect (write-to-string i))))
      ((cl-ppcre:all-matches-as-strings "\\d{1,7}" workdow)
       (return-from parse-dayofweek (delete-duplicate (cl-ppcre:all-matches-as-strings "\\d" workdow ))))
      (t (return-from parse-dayofweek (list "1" "2" "3" "4" "5" "6" "7"))))))
    


(defun delete-duplicate (worklist)
  ;; Удаление повторяющихся элементов из списка
  (let ((retlist))
    (loop for (x . y) on worklist do (cond
				       ((member x retlist :test 'equal)
					(continue))
				       (t (push x retlist))))
    (return-from delete-duplicate retlist)))
  

					
    
	
	     
    
	     
	  
      
    
	 
