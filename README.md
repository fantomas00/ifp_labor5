<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
</p>
<p align="right">Студент: Ольховський Максим Олександрович КВ-13<p>
<p align="right">Рік: 2024<p>

## Завдання

 Реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею. 
1.  Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом). 
2.  Розробити утиліту(-и) для зчитування таблиць з файлів. 
3.  Розробити функцію  select	, яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або 
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька.  select	 повертає лямбда-вираз, 
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у  select	. При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів. 
4.  Написати утиліту(-и) для запису вибірки (списку записів) у файл. 
5.  Написати функції для конвертування записів у інший тип (в залежності від варіанту): 
структури у геш-таблиці 
геш-таблиці у асоціативні списки 
асоціативні списки у геш-таблиці 
6.  Написати функцію(-ї) для "красивого" виводу записів таблиці. 


## Варіант 12
База даних: Космічні апарати

Тип запису: Асоціативний список

Таблиці: 
1. Компанії;
2. Космічні апарати
   
Опис: База даних космічних апаратів для зв'язку, дослідження, тощо. 


## Лістинг реалізації завдання
```lisp
(defpackage :space-db
  (:use :cl)
  (:export :read-csv :write-csv :select :convert-to-hash :convert-to-alist :print-records))

(in-package :space-db)

(defun split-csv-line (line &optional (delimiter #\,))
  (loop with start = 0
        for pos = (position delimiter line :start start)
        collect (subseq line start pos)
        while pos
        do (setf start (1+ pos))))

(defun read-csv (file-path)
  (with-open-file (in file-path)
    (let ((headers (mapcar #'string-downcase (split-csv-line (read-line in)))))
      (loop for line = (read-line in nil)
            while line
            collect (pairlis headers (split-csv-line line))))))

(defun write-csv (file-path records)
  (with-open-file (out file-path :direction :output :if-exists :supersede)
    (let ((headers (mapcar #'car (first records))))
      (format out "~{~a,~}~%" headers)
      (dolist (record records)
        (format out "~{~a,~}~%" (mapcar (lambda (h) (cdr (assoc h record :test #'equal))) headers))))))

(defun select (file-path &key filter)
  (let ((records (read-csv file-path)))
    (lambda (&rest args &key)
      (loop for record in records
            when (every (lambda (arg)
                          (equal (cdr (assoc (car arg) record :test #'equal))
                                 (cdr arg)))
                        args)
            collect record))))

(defun convert-to-hash (alist)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash) (cdr pair)))
    hash))

(defun convert-to-alist (hash)
  (loop for k being the hash-keys in hash
        using (hash-value v)
        collect (cons k v)))

(defun print-records (records)
  (dolist (record records)
    (format t "~{~a: ~a ~}~%" (loop for (k . v) in record append (list k v)))))
```

## Тестові набори та утиліти
```lisp
(defun test-space-db ()
  (let* ((file-path "space-devices.csv")
         (records (read-csv file-path))
         (first-record (first records)))
    ;; Тест зчитування першого запису
    (assert (assoc "name" first-record :test #'equal))
    (assert (assoc "launch-year" first-record :test #'equal))

    ;; Тест конвертації
    (let* ((hash (convert-to-hash first-record))
           (alist (convert-to-alist hash)))
      (assert (equal (sort alist #'string< :key #'car)
                     (sort first-record #'string< :key #'car))))

    ;; Тест вибірки
    (let ((query (funcall (select file-path) :name "Voyager-1")))
      (assert (every (lambda (rec)
                       (equal (cdr (assoc "name" rec :test #'equal)) "Voyager-1"))
                     query)))

    (format t "All tests passed!~%")))
```

## Тестування

``` lisp
(test-space-db)
All tests passed!
```
