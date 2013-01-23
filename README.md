Lisp
====

my lisp programm

(defparameter *vertex* (make-hash-table :test #'equal))
(defparameter *arc* (make-hash-table :test #'equal))
(defparameter *graph* ())

(defun vertex-add (id info)
    (if (eql (nth-value 1 (gethash id *vertex*)) nil)
  (setf (gethash id *vertex*) info)))


(defun vertex-del (id)
  (if (eql (nth-value 1 (gethash id *vertex*)) nil)
      (format t "Такой вершины не существует!~&")
      	(remhash id *vertex*)))


(defun vertex-info ()
  (progn
    (format t "Все существующие вершины графа~&")
    (maphash #'(lambda (k v) (format t "ID: ~A Входной тип данных: ~A Выходной тип данных: ~A~&" k (car v) (cadr v))) *vertex*)))


(defun vertex-update (id newid info)
  (if (eql (nth-value 1 (gethash id *vertex*)) nil)
      (format t "Такой вершины не существует!~&")
	(if (eql (nth-value 1 (gethash newid *vertex*)) t)
     	 (format t "Вершина с таким именем существует!~&")
	 	(progn
      			(setf (gethash newid *vertex*) info)
        		(remhash id *vertex*)))))

(defun arc-add (id start end)
  ( let ((t1 (cadr (gethash start *vertex*))) (t2 (car (gethash end *vertex*))))
    (and
      (if (eql (nth-value 1 (gethash id *arc*)) nil)
       (if (eql (nth-value 1 (gethash start *vertex*)) t)
        (if (eql (nth-value 1 (gethash end *vertex*)) t)
         (if (eql t1 t2)	 			
	  (setf (gethash id *arc*) (list start end))
         (format t "Выходной тип  вершины ~a: ~a и Входной тип вершины ~a: ~a не совпадают~%" start t1 end t2))  
        (format t "Конечной вершины ~A не существует~&" end))
       (format t "Начальной вершины ~A не существует~&" start))
      (format t "Дуга ~A уже существует~&" id)))))

(defun arc-del (id)
  (if (eql (nth-value 1 (gethash id *arc*)) nil)
      (format t "Такой дуги не существует!~&")
	 (progn
		(format t "Дуга ~A удалена~&" id)
      		(remhash id *arc*))))

(defun arc-info ()
  (progn
    (format t "Все существующие дуги графа~&")
    (maphash #'(lambda (k v) (format t "ID: ~A Начальная вершина: ~A Конечная вершина: ~A~&" k (car v) (cadr v))) *arc*)))


(defun graph-info ()
( let ((start-list ()))
(defparameter *graph* ())
  (progn    
    (loop for k being the hash-keys in *arc* using (hash-value v)
       do (progn
		(if (eql (member (car v) start-list) nil)
		(progn
			(setf start-list (nconc start-list (list (car v))))			
                	(setf *graph* (acons (car v) (list(cadr v)) *graph*)))				
	        (RPLACD (assoc (car v) *graph*) (cdr (nconc (assoc (car v) *graph*) (list(cadr v)))))) )))
  (setf *graph* (reverse *graph*))
  (format t "Граф выглядит так: ~A~&" *graph*)))


----------------------------------------
Таблицы и переменные :
*vertex* хэш-таблица вершин графа
*arc* хэш-таблица дуг графа
*graph* Граф
----------------------------------------
Функции : 
vertex-add  Функция добавления новой вершины, задается id и информация (информация содержит в виде списка входной тип данный, выходной тип данных ...) 
vertex-del  Функция удаления вершины
vertex-info Информация о существующих вершинах
vertex-update Функция изменения вершины
arc-add  Функция добавления дуги, на вход принимает имя (название, ид...), начальную и конечную вершины. Производится проверка на существование \
вершины с таким именем, существование начальной и конечной вершины в хэш-таблице вершин *vertex*, а также сопоставляются типы входных/выходных данных для этих вершин.
arc-del  Функция удаления дуги
arc-info Информация о существующих дугах
graph-info Функция создания графа на основе существующих дуг

---------------------------------------
Примеры использования функций :
(vertex-add '1 '(type1 type2))
(vertex-add '2 '(type2 type3))
(vertex-add '3 '(type3 nil))
(vertex-add '4 '(type2 type4))
(vertex-add '5 '(type2 type5))
(vertex-add '7 '(type4 type6))

(vertex-del '2 )

(vertex-info)

(vertex-update '3 '4 '(type3 nil))

(arc-add '1 '1 '3)
(arc-add '1 '1 '2)
(arc-add '1 '1 '2)
(arc-add '2 '21 '2)
(arc-add '2 '3 '32)
(arc-add '2 '2 '3)
(arc-add '3 '1 '4)
(arc-add '4 '1 '5)
(arc-add '5 '4 '7)

(arc-del '2 )
(arc-del '4 )
(arc-del '22 )

(arc-info)

(graph-info)
*graph*
