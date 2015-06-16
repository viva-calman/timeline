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
(defgeneric deserialize-line (timeline)
  (:documentation "Сериализация таймлайна"))

(defgeneric clear-line (timeline)
  (:documentation "Удаление несуществующих записей"))

(defgeneric get-slice-id (timeline
			  id)
  (:documentation "Возвращает id слайса по id в таймлайне"))

(defgeneric edit-line (timeline
		       id)
  (:documentation "Редактирование таймлайна"))

(defgeneric clear-slicelist (timeline)
  (:documentation "Очистка несуществующих записей"))

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

(defmethod get-slice-id ((timeline timeline)
			 id)
  ;; Возаращет id таймлайна по id  в массиве
  (with-slots ((timeline timeline)) timeline
    (elt timeline (first id))))

(defmethod deserialize-line ((timeline timeline))
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
	       
(defmethod edit-line ((timeline timeline)
		      id)
  ;; Удаление записей из таймлайна
  (with-accessors ((timeline timeline)) timeline
    (if (= id 0)
	(show-message "Свободное время, удалять нечего")
	(delete-slice timeline
		      id)))
  (clear-slicelist timeline))
	
(defmethod clear-slicelist ((timeline timeline))
  ;; Удаление из списка слайсов несуществующих более записей
  (with-accessors ((timeline timeline)
		   (timeslices timeslices)) timeline
    (let ((new-slicelist))
      (loop for i in timeslices do (let ((result))
				     (if (delete-not-exists
					  (delete-duplicate
					   (loop for j from 0 upto 2015
					      collect (elt timeline j)))
					   i)
					  (push i new-slicelist))))
      (setf timeslices new-slicelist))))
    
		     


;;;
;;; Функции
;;;

(defun init-timeline ()
  ;; Инициализация нового таймлайна
  (setf *current-timeline* (make-instance 'timeline)))

(defun serialize-line (arr)
  ;; Преобразование lisp-формы в объект
  (setf *current-timeline* (make-instance 'timeline
					  :timeline (first arr)
					  :timeslices (second arr)
					  :current-id (third arr))))

(defun save-timeline (timeline)
  ;; Сохранение таймлайна
  (with-open-file (out "tl.db"
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
	(print (deserialize-line timeline) out))))
		       
(defun load-timeline ()
  ;; Загрузка таймлайна
  (restart-case
      (progn
	(with-open-file (in "tl.db")
	  (with-standard-io-syntax
	    (serialize-line (read in))
	    (show-message "Загрузка завершена"))))
    (file-error () (if-not-exists))))

(defun user-interfase ()
  ;; Пользовательский интерфейс
  (show-message "Загрузка нового таймлайна")
  (handler-bind ((sb-int:simple-file-error #'(lambda (c)
					       (invoke-restart 'file-error))))
    (load-timeline)
    (loop (progn
	    (show-menu)
	    (save-timeline *current-timeline*))
       (if (not (y-or-n-p "Продолжить? [y/n]: ")) (return)))))

(defun show-menu ()
  ;; Отображение меню
  (show-message "Выберите действие:
1: Отображение таймлайна (по умолчанию)
2: Добавление записи
3: удаление записей")
  (let ((ans (or (parse-integer (read-input ">") :junk-allowed t) -1)))
    (cond
      ((= ans 2)
       (add-line *current-timeline*))
      ((= ans 3)
       (edit-line *current-timeline* (input-id)))
      (t (view-line *current-timeline* -1)))))
	
(defun input-id ()
  ;; Ввод значения для удаления записи
  (show-message "Введите время и день недели, на которые приходится нужный отрезок")
  (do
   ((ans (get-slice-id *current-timeline* (get-int-id (get-slice-by-time)))))
   ((> ans -1) ans)
    (setf ans (get-slice-id *current-timeline* (get-int-id (get-slice-by-time))))))

(defun if-not-exists ()
  ;;  Если файла не существует...
  (init-timeline)
  (save-timeline *current-timeline*)
  (show-message "Создан новый пустой таймлайн"))
	  
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
    (push (repeat-timeinput) output)
    (show-message "Введите время окончания отрезка" )
    (push (do
	   ((out (repeat-timeinput) (repeat-timeinput)))
	   ((< (first output) out) out)) output)
    (show-message "Укажите дни недели, на которые применяется этот отрезок. Можно указать диапазон, через дефис или последовательно в любой форме и с любым разделителем, кроме дефиса")
    (push (parse-dayofweek (read-input ">")) output)
    (return-from timeinput output)))

(defun repeat-timeinput ()
  ;; повторный ввод времени, если ввод был произведен неправильно
  (do
   ((ans (parse-timeinput (read-input ">"))))
   ((not (equal ans nil)) ans)
    (setf ans (parse-timeinput (read-input "=>")))))

(defun get-slice-by-time ()
  ;; Получение интервала по заданному времени
  (let ((output))
    (show-message "Введите время")
    (push (repeat-timeinput) output)
    (show-message "Введите день недели")
    (push (list
	   (write-to-string
	    (do
	     ((ans (or (parse-integer (read-input "=>") :junk-allowed t) -1)))
	     ((and (< 0 ans) (> 8 ans)) ans)
	      (setf ans (or (parse-integer (read-input "=>") :junk-allowed t) -1)))))
	   output)))


(defun get-int-id (tl)
  ;; получаем номер интервала
  (gen-interval-list (first tl) (second tl)))

(defun gen-interval-list (days
			  hours)
  ;; Объединение времени ввода и дней недели
  (let ((intlist (gen-day-interval (loop for i in days
				      collect (parse-integer i)))))
    (loop for i in intlist collect (+ i hours))))
    
(defun interval-input (inter)
  ;; Служебная функция, обертка для ввода
  (return-from interval-input (list
			       (gen-interval-list (first inter)
						  (third inter))
			       (- (second inter)
				  (third inter)))))

(defun gen-day-interval (intlist)
  ;; День в интервалы
  (loop for i in intlist collect (* (- i 1) (* 12 24)))) 
	  
			      

(defun parse-timeinput (timestring)
  ;; Функция, разбирающая пользовательский ввод времени.
  (let ((parsed-string (first (cl-ppcre:all-matches-as-strings "^\\d{1,2}\\D+\\d{1,2}" timestring))))
    (if (not (eq parsed-string nil))
	(convert-time (cl-ppcre:all-matches-as-strings "\\d{1,2}" parsed-string))
	(progn
	  (show-message "Неверный формат времени")
	  (return-from parse-timeinput nil)))))

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

(defun delete-not-exists (checklist
			  eltlist)
  ;; Проверка на существование элемента таймслайсов
  (let ((sid (first eltlist)))
    (loop for i in checklist do (if (= i sid)
				    (return-from delete-not-exists eltlist)))))

(defun delete-duplicate (worklist)
  ;; Удаление повторяющихся элементов из списка
  (let ((retlist))
    (loop for (x . y) on worklist do (cond
				       ((member x retlist :test 'equal)
					(continue))
				       (t (push x retlist))))
    (return-from delete-duplicate retlist)))
  

					
    
	
	     
    
	     
	  
      
    
	 
