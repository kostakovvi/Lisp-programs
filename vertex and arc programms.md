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
    (maphash #'(lambda (k v) (format t "ID: ~A Входной тип данных: ~A Выходной тип данных: ~A Значение выходного типа: ~A~&" k (car v) (cadr v) (caddr v))) *vertex*)))


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
    (format t "Все существующие дуги графа:~&")
    (maphash #'(lambda (k v) (format t "ID: ~A Начальная вершина: ~A Конечная вершина: ~A~&" k (car v) (cadr v))) *arc*)))


(defun graph-update ()
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

(defun converter (id)
( let ((t1 (list (car (gethash id *vertex*)) (cadr (gethash id *vertex*)))) (t2 )) 
    (if (eql (nth-value 1 (gethash id *vertex*)) nil) (format t "Такой вершины не существует!~&")
	(progn 
		(loop for k being the hash-keys in *arc* using (hash-value v)
       			do (progn 
				(if (equal (cadr v) id)					
					(setf t2  (append t2  (list (caddr (gethash (car v) *vertex*)))))
				;(format t "Вершина : ~A не является конечной для какой-либо дуги~&" id)
				)))
		(setf t2 (anyfunction t2))
		(setf t1 (nconc t1 t2))
		(setf (gethash id *vertex*) t1)))))

(defun anyfunction (x) (loop for i in x   summing i into total
 finally (return  ( list total))))

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
graph-update Функция создания графа на основе существующих дуг
converter Функция-обработчик, применяет id вершины и в поле выходных данных для данной вершины заносит значение, основанное на результатах вершин-начал дуг, для который данная вершина является конечной
anyfunction Функция, передаваемая в функцию-обработчик, принимает входные значения, производит преобразование, на месте этой функции мб любая.
---------------------------------------
Примеры использования функций :
(vertex-add '1 '(type1 type2 7))
(vertex-add '2 '(type2 type3))
(vertex-add '3 '(type3 nil))
(vertex-add '4 '(type2 type4))
(vertex-add '5 '(type2 type5))
(vertex-add '6 '(type2 type6))
(vertex-add '7 '(type4 type6))
(vertex-add '8 '(type6 type7))

(vertex-del '6 )

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
(arc-add '6 '1 '6)
(arc-add '7 '6 '8)
(arc-add '8 '7 '8)

(arc-del '2 )
(arc-del '4 )
(arc-del '22 )

(arc-info)

(converter '2)
(converter '4)
(converter '5)
(converter '6)
(converter '7)
(converter '8)

(graph-update)
*graph*

----------------------------------------
Итоговые входные данные :
(vertex-add '1 '(type0 type1 7))
(vertex-add '2 '(type1 type2))
(vertex-add '2 '(type3 type2))
(vertex-add '3 '(type1 type3))
(vertex-add '4 '(type2 type4))
(vertex-add '5 '(type2 type5))
(vertex-add '6 '(type5 type6))
(vertex-add '7 '(type3 type4))
(vertex-add '8 '(type4 type8))
(vertex-add '9 '(type4 type9))
(vertex-add '10 '(type1 type4))
(vertex-add '11 '(type4 type11))
(vertex-add '12 '(type11 type12))

(vertex-add '13 '(type12 type13))
(vertex-del '13 )
(vertex-del '15 )

(vertex-info)

(arc-add '1 '1 '2)
(arc-add '2 '1 '3)
(arc-add '3 '1 '10)
(arc-add '4 '2 '5)
(arc-add '5 '2 '4)
(arc-add '6 '3 '7)
(arc-add '7 '10 '11)
(arc-add '8 '5 '6)
(arc-add '9 '4 '8)
(arc-add '10 '4 '11)
(arc-add '11 '7 '8)
(arc-add '12 '7 '9)
(arc-add '13 '11 '12)

(vertex-add '13 '(type12 type13))
(vertex-add '14 '(type13 type14))
(arc-add '14 '13 '14)
(arc-del '15)
(arc-del '14)
(vertex-del '13 )
(vertex-del '14 )

(arc-info)

*graph*
(graph-update)
*graph*

(converter '2)
(converter '3)
(converter '4)
(converter '5)
(converter '6)
(converter '7)
(converter '8)
(converter '9)
(converter '10)
(converter '11)
(converter '12)
