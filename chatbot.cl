;;; ������� ��� �����, � ������� ����� ��������� �������
(defvar *filename* "~/dialogs.db")

;;; ����������� ������, ���� ������������ ������� �������� �����������
(defvar *default-ansvers*
	'(("����� ������ ����")
		("������ ������, � �� ������")
		("������ ������� ���-�� ������������")
		("� ������� ����������� � �����")
		("���-�� ��� ������")
		("� ����� ���������� ����� ��� ���, �� ����� ��")
		("�� �� �� ���������� ��������� ��� �������")
		("�� ��� ��������")
		("� �� �������, � ��� ����")
		("��� ���� ������� ���-�� �����, �� � �� ����")))

;;; ����-����� ��� ������ �� ���������
(defvar *end-phrase* "����")

;;; ��������� ������ ��������� (���� ���� � ��������� ������)
;;; ������ ������ ���������: ������, � ������� ������ ������� - ������. � �������� ��������� ������ ������ nil
;;; ��������� �������� - ������� ��������� �������
;;; ������, ��� ����� ��������� ������:
;;;          "��� ����?"
;;;         /     |     \
;;;  "������"  "�����"  "���������, � � ����?"
;;;               |                  |       \
;;;        "��� ���������?"  "��� � �������"  "� ���� ��� �����"
(defvar *dialogs* (list () (list "������")))

;;; ���������� ��� ����� ������
(defvar *input* ())

;;; ��� ���� ���������� :)
(defvar *one-more-var* ())

;;; ������ �� ������� ��������, �.�. � ����� ����� ������� ������� �� ���������
(defvar *current-dialog* *dialogs*)

;;; ��������� ���������� ������ �������� � ����
(defun save-dialogs (filename)
	(with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *dialogs* out))))

;;; �������� ������ �������� �� �����
(defun load-dialogs (filename)
  (with-open-file (in filename 
					:if-does-not-exist nil)
    (if in 
		(with-standard-io-syntax
			(setf *dialogs* (read in))))))

;;; ���������� ����� ����� � ������� ��������
(defun add-phrase (phrase) 
	(setf (cdr (last *current-dialog*)) (cons (list phrase) nil)))

;;; �������, ������ ����� � ���������
(defun find-phrase (phrase dialog)
	(dolist (x (rest dialog)) 
		(if (string-equal phrase (first x)) (return x))))

;;; ���������, ��������� ��������� ����� �� ���������
(defun write-random-answer (dialog)
	(setf *current-dialog* (nth  (random (length dialog)) dialog)) ; ������ ������� �������� �� ��������� �����
	(format t "~a~%" (first *current-dialog*))
	(force-output *query-io*))	

;;; �������� �������
(defun start-dialog ()	
;; ���������� ������
	(setf *random-state* (make-random-state t)) ; ������ ����� ���� �������������
	(load-dialogs *filename*)					; ��������� ������� �� �����
	(setf *current-dialog* *dialogs*)			; ������������� ��������� �������� �������
	;; ����� ������� �������� ���� ���������
	(loop
		;; ���� �� ��������� � ������ ��������� 
		;; (����� ����� ������� ��������� ��� ����� ���������� ������ � ����� �������)
		;; �� � ����������� 0,5 ��������� ������������� ���������� � ���� �������� ������
		(if (and (eql *current-dialog* *dialogs*) 
				(= 0 (random 2))) 
			(write-random-answer (rest *dialogs*))) ; ������� ��������� ����� �� �����������
		;; ��������� ���� ������������
		(format *query-io* "--> ")				; ������� ������ �����������
		(force-output *query-io*)				; ��� ���� ������� ���������� ��� ��� �������� Enter �� ������ ����������
		(setf *input* (read-line *query-io*))	; ��������� ������ �����
		;; ���� ������� ����-�����, ������� �� ���������
		(if (string-equal *input* *end-phrase*) 
			(progn
				(save-dialogs *filename*)		; ��������� ������� � ����
				(format t "~a~%" "��� ��������.")
				(return)))
		;; ���� ��� �� ����-�����, ���� �� � ������� ���������
		(setf *one-more-var* (find-phrase *input* *current-dialog*))
		;; ���� ����� �� �������, ��������� ��
		;; ����� ��������� ����� �� ���������
		;; � ������������ � ������ ��������
		(if (null *one-more-var*)
			(progn
				(add-phrase *input*)				
				(write-random-answer *default-ansvers*)
				(setf *current-dialog* *dialogs*)))
		;; ���� ����� �������, �� ������� �� ��� ���
		;; ������ �� �� �����, �� ��� ���������� �����
		(if (= 1 (length *one-more-var*))
			(progn
				(write-random-answer *default-ansvers*)
				(setf *current-dialog* *dialogs*)))
		;; ���� �� ����� ���� ���� �� ���� �����
		;; ������� ��������� ����� �� �����������
		(if (> (length *one-more-var*) 1)
			(write-random-answer (rest *one-more-var*)))))

;;; ��� ������ ������� ����������� .exe ����
(sb-ext:save-lisp-and-die "C:/Projects/clisp/chatbot/chatbot.exe" :toplevel #'start-dialog :executable t)