;;; Объявим имя файла, в котором будут храниться диалоги
(defvar *filename* "~/dialogs.db")

;;; Стандартные ответы, если существующая цепочка диалогов закончилась
(defvar *default-ansvers*
	'(("Давай сменим тему")
		("Честно говоря, я не уверен")
		("Сложно сказать что-то определенное")
		("Я склонен согласиться с тобой")
		("Что-то мне скучно")
		("Я хотел рассказать шутку про это, но забыл ее")
		("Мы же не собираемся обсуждать это всерьез")
		("Ну все возможно")
		("Я не понимаю, о чем речь")
		("Мне надо сказать что-то умное, но я не умею")))

;;; Стоп-фраза для выхода из программы
(defvar *end-phrase* "Пока")

;;; Начальное дерево вариантов (если файл с диалогами пустой)
;;; Формат дерева вариантов: список, в котором первый элемент - строка. У начально контекста вместо строки nil
;;; Остальные элементы - деревья возможных ответов
;;; Пример, как может выглядеть дерево:
;;;          "Как дела?"
;;;         /     |     \
;;;  "Хорошо"  "Плохо"  "Нормально, а у тебя?"
;;;               |                  |       \
;;;        "Что случилось?"  "Все в порядке"  "У меня все супер"
(defvar *dialogs* (list () (list "Привет")))

;;; Переменная для ввода данных
(defvar *input* ())

;;; Еще одна переменная :)
(defvar *one-more-var* ())

;;; Ссылка на текущий контекст, т.е. в каком месте цепочки диалога мы находимся
(defvar *current-dialog* *dialogs*)

;;; Процедура сохранения дерева диалогов в файл
(defun save-dialogs (filename)
	(with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *dialogs* out))))

;;; Загрузка дерева диалогов из файла
(defun load-dialogs (filename)
  (with-open-file (in filename 
					:if-does-not-exist nil)
    (if in 
		(with-standard-io-syntax
			(setf *dialogs* (read in))))))

;;; Добавление новой фразы в текущий контекст
(defun add-phrase (phrase) 
	(setf (cdr (last *current-dialog*)) (cons (list phrase) nil)))

;;; Функция, ищущая фразу в контексте
(defun find-phrase (phrase dialog)
	(dolist (x (rest dialog)) 
		(if (string-equal phrase (first x)) (return x))))

;;; Процедура, выводящая случайный ответ из контекста
(defun write-random-answer (dialog)
	(setf *current-dialog* (nth  (random (length dialog)) dialog)) ; меняем текущий контекст на выводимую фразу
	(format t "~a~%" (first *current-dialog*))
	(force-output *query-io*))	

;;; основная функция
(defun start-dialog ()	
;; Подготовка данных
	(setf *random-state* (make-random-state t)) ; Задаем новое ядро рандомизатора
	(load-dialogs *filename*)					; Загружаем диалоги из файла
	(setf *current-dialog* *dialogs*)			; Устанавливаем начальный контекст диалога
	;; Далее следует основной цикл программы
	(loop
		;; Если мы находимся в пустом контексте 
		;; (сразу после запуска программы или после достижения тупика в ветке диалога)
		;; то с вероятность 0,5 программа перехватывает инициативу и сама начинает диалог
		(if (and (eql *current-dialog* *dialogs*) 
				(= 0 (random 2))) 
			(write-random-answer (rest *dialogs*))) ; Выводим случайную фразу из запомненных
		;; Считываем ввод пользователя
		(format *query-io* "--> ")				; Выводим строку приглашения
		(force-output *query-io*)				; Без этой строчки приходится еще раз нажимать Enter до начала считывания
		(setf *input* (read-line *query-io*))	; Считываем строку ввода
		;; Если введена стоп-фраза, выходим из программы
		(if (string-equal *input* *end-phrase*) 
			(progn
				(save-dialogs *filename*)		; Сохраняем диалоги в файл
				(format t "~a~%" "Еще увидимся.")
				(return)))
		;; Если это не стоп-фраза, ищем ее в текущем контексте
		(setf *one-more-var* (find-phrase *input* *current-dialog*))
		;; Если фраза не найдена, добавляем ее
		;; Пишем случайный ответ по умолчанию
		;; И возвращаемся в пустой контекст
		(if (null *one-more-var*)
			(progn
				(add-phrase *input*)				
				(write-random-answer *default-ansvers*)
				(setf *current-dialog* *dialogs*)))
		;; Если фраза найдена, но ответов на нее нет
		;; Делаем то же самое, но без добавления фразы
		(if (= 1 (length *one-more-var*))
			(progn
				(write-random-answer *default-ansvers*)
				(setf *current-dialog* *dialogs*)))
		;; Если на фразу есть хотя бы один ответ
		;; Выводим случайный ответ из запомненных
		(if (> (length *one-more-var*) 1)
			(write-random-answer (rest *one-more-var*)))))

;;; Эта строка создает исполняемый .exe файл
(sb-ext:save-lisp-and-die "C:/Projects/clisp/chatbot/chatbot.exe" :toplevel #'start-dialog :executable t)