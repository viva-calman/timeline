;;;;
;;;; Мой простой менеджер таймлайна.
;;;;
;;;;
(in-package :net.viva-calman.timeline)

;;;
;;; Глобальные переменные
;;;
(defvar *current-timeline*)
(defvar *current-id* 0)

;;;
;;; Константы
;;;
(defconstant +num-of-elt+ (- (* 12 24 7) 1))
(defconstant +noe+ (* 12 24 7))
;;;
;;; Классы
;;;
(defclass timeline ()
  ;; класс, реализующий таймлайн
  ((timeline :initarg :timeline
	     :accessor timeline
	     :initform (gen-empty)
	     :documentation "Timeline array")
   (timeslices :initarg :timeslices
	       :accessor timeslices
	       :initform (list (list 0 "Свободно"))
	       :documentation "Array of timeslices")
   (current-id :initarg :current-id
	       :accessor current-id
	       :initform 1
	       :documentation "Current id of timeslice")))

;;;
;;; Обобщенные функции
;;;
(defgeneric serialize-line (timeline)
  (:documentation "Сериализация таймлайна"))

(defgeneric write-tline (timeline)
  (:documentation "Сохранение таймлайна"))

(defgeneric read-tline (timeline)
  (:documentation "Загрузка таймлайна"))

(defgeneric clear-line (timeline)
  (:documentation "Удаление несуществующих записей"))

(defgeneric edit-line (timeline)
  (:documentation "Редактирование таймлайна"))

(defgeneric view-line (timeline
		       id)
  (:documentation "Вывод таймлайна"))

(defgeneric add-line (timeline)
  (:documentation "Добавление новой записи"))

(defgeneric extract-task (timeline
			  id)
  (:documentation "Получение заголовка задачи по id"))
  

;;;
;;; Методы
;;;
(defmethod add-line ((timeline timeline))
  ;; Добавление новой записи
  (with-accessors ((timeline timeline)
		   (timeslices timeslices)
		   (current-id current-id)) timeline
    (show-message "Заголовок")
    (push (list current-id (read-input ">")) timeslices)
    (input-operate current-id (interval-input (timeinput)) timeline)
    (incf current-id)))

(defmethod serialize-line ((timeline timeline))
  ;; Преобразование объекта в lisp-форму
  (with-slots ((timeline timeline)
	       (timeslices timeslices)
	       (current-id current-id)) timeline
    (list timeline timeslices current-id)))


(defmethod extract-task ((timeline timeline)
			 id)
  ;; Извлечение записи из массива по id
  (with-slots ((timeslices timeslices)) timeline
    (second (first (remove-if-not #'(lambda (c) (= (first c) id)) timeslices))))) ;; Stupid

(defmethod view-line ((timeline timeline)
		      id)
  ;; Отображение таймлайна
  (with-slots ((timeline timeline)) timeline
    (cond
      ((= id -1)
       (task-list timeline))
      (t (task-list timeline id)))))
	       


;;;
;;; Функции
;;;
(defun init-timeline ()
  ;; Инициализация нового таймлайна
  (setf *current-timeline* (make-instance 'timeline)))

(defun deserialize-line (arr)
  ;; Преобразование lisp-формы в объект
  (setf *current-timeline* (make-instance 'timeline
					  :timeline (first arr)
					  :timeslices (second arr)
					  :current-id (third arr))))

	  
(defun gen-empty ()
  ;; Генерация пустого массива
  (make-array +noe+ :initial-element 0))

(defun fill-array (id
		   begin
		   length
		   timeline)
  ;; Заполнение массива
  (loop for i from begin to (+ begin length -1) do (setf (elt timeline i) id)))

(defun check-fill (begin
		   length
		   timeline)
  ;; Проверка последовательности на непрерывность
  (loop for i from begin to (+ begin length -1) do (unless (= (elt timeline i) 0)
						     (return-from check-fill i)))
  (return-from check-fill nil))

(defun delete-slice (timeline
		     id)
  ;; Обнуление полей с определенным id
  (loop for i from 0 to 2015 do (if (= (elt timeline i) id)
				    (setf (elt timeline i) 0))))

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
		      timeline)
  ;; Внесение введенных данных в массив
  (loop for i in (first beginlist) do (operate-array id i (second beginlist) timeline)))
  
(defun operate-array (id
		      begin
		      length
		      timeline
		      &optional (edit t edit-supplied-p))
  ;; Обработка запроса на заполнение
  (let ((diap (check-overload begin length)))
    (if (or (not (third diap)) (= (third diap) 0))
	(if (or edit-supplied-p (not (check-fill begin
						 length
						 timeline)))
	       
	    (fill-array id
			begin
			length
			timeline)
	    (progn
	      (show-message "Интервалы перекрываются")
	      (show-message "Точка перекрытия:")
	      (nice-time (parse-time
			  (convert-interval
			   (check-fill begin
				       length
				       timeline))))))
	(if (or edit-supplied-p (and (not (check-fill (first diap)
						      (second diap)
						      timeline))
				    (not (check-fill 0
						     (third diap)
						     timeline))))
    	    (progn
	      (fill-array id
			  (first diap)
			  (second diap)
			  timeline)
	      (fill-array id
			  0
			  (third diap)
			  timeline))
	    (progn
	      (show-message "Интервалы перекрываются")
	      (show-message "Точка перекрытия:")
	      (nice-time (parse-time
			  (convert-interval
			   (or (check-fill (first diap)
					   (second diap)
					   timeline)
			       (check-fill 0
					   (third diap)
					   timeline)))))))))))
	
	



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

(defun task-list (timeline &optional (id -1 id-supplied-p))
  ;; Вывод всех имеющихся задач
  (let ((intervals (list (list 0 (elt timeline 0))))
	(flag (elt timeline 0)))
    (loop for i from 0 to 2015 do (let ((cur-elt (elt timeline i)))
				    (unless (= flag cur-elt)
				      (push i (first intervals))
				      (push (list i cur-elt) intervals)
				      (setf flag cur-elt))))
    (push 2015 (first intervals))
    (loop for i in (reverse intervals) do (cond
					    ((and (= id -1) (not id-supplied-p))
					     (nice-time
					      (parse-time (convert-interval (second i)))
					      (parse-time (convert-interval (first i)))
					      (third i)))
					    ((= id (third i))
					     (nice-time
					      (parse-time (convert-interval (second i)))
					      (parse-time (convert-interval (first i)))
					      (third i)))))))

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

(defun nice-time (timelist-b &optional
			       timelist-e
			       (id 0 id-supplied-p))
  ;; Отображение времени
  (if id-supplied-p
      (format t "~a,~3t~a:~a - ~a,~3t~a:~a   ~a~%" (first timelist-b)
	  (second timelist-b)
	  (third timelist-b)
	  (first timelist-e)
	  (second timelist-e)
	  (third timelist-e)
	  (extract-task *current-timeline* id))
    (format t "~a,~3t~a:~a" (first timelist-b)
	  (second timelist-b)
	  (third timelist-b))))
	  
;(defun extract-task (id)
;  ;; Заглушка. вывод задачи по id
;  (if (= id 0)
;      (setf id "Свободное время"))
;  (return-from extract-task id))

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
  (let ((intlist (gen-day-interval (loop for i in days
				      collect (parse-integer i)))))
    (loop for i in intlist collect (+ i hours))))
    
(defun interval-input (inter)
  ;; Служебная функция, обертка для ввода
  (return-from interval-input (list (gen-interval-list (first inter) (third inter)) (- (second inter) (third inter)))))

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
  

					
    
	
	     
    
	     
	  
      
    
	 
