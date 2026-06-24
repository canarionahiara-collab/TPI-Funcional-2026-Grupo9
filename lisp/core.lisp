
;=====================================================================================================
;Se define un paquete auxiliar para que no entre en conflicto con la función timer
;definida en Portacle
;======================================================================================================
(defpackage :semaforo 
  (:use :common-lisp)
  (:export :timer :estado-semaforo))

(in-package :semaforo)
;Cuando usas la biblioteca quicklisp se deben importar primero las bibliotecas local-time y cl-json
;Para importar local-time: (ql:quickload :local-time)
;Para importar cl-json: (ql:quickload :cl-json)

;;; Cargar bibliotecas necesarias antes de compilar o ejecutar
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :local-time)
  (ql:quickload :cl-json))



;Referencias
;Definición De Tiempo Unix: https://espanol.epochconverter.com/
;Definición de Universal Time en Lisp: https://lispcookbook.github.io/cl-cookbook/dates_and_times.html
;Prompt De ChatGPT: Cómo se obtiene en lisp esto: Tiempo Unix actual (entero)





;==============================================================================================================
;FUNCIóN: get-unix-time
;NATURALEZA: IMPURA
;ESTRATEGIA: No es recursiva. No emplea funciones de orden superior. No es una función predicado
;IMPACTO: No destructiva
;ENTRADAS:
;Sin entradas

;SALIDA:
;Retorna un entero que representa el tiempo unix. Emplea la librería local-time de quicklisp
;La librería local-time debe importarse mediante (ql:quickload :local-time)
;==============================================================================================================

;
(defun get-unix-time ()
  (local-time:timestamp-to-unix (local-time:now)))


;==============================================================================================================
;FUNCIóN: universal-a-unix
;NATURALEZA: PURA
;ESTRATEGIA:  No es recursiva. No emplea funciones de orden superior. No es una función predicado
;IMPACTO: No destructiva
;ENTRADAS:
;utime: Un tiempo en tiempo universal para convertir a unix

;SALIDA:
;Retorna un entero que representa el tiempo unix.
;==============================================================================================================
(defun universal-a-unix (utime)

  (- utime 2208988800))


;===========================================================================================================
;FUNCIóN: tiempo-a-string
;NATURALEZA: PURA
;ESTRATEGIA:  No es recursiva. No emplea funciones de orden superior. No es una función predicado
;IMPACTO: No destructiva
;ENTRADAS:
;utime: Recibe el tiempo en segundos de una fecha determinada en tiempo unix
;
;SALIDA:
;Retorna un string con el formato "AAAA-MM-DD HH:MM:SS"
;==============================================================================================================
(defun tiempo-a-string (utime)
  "Convierte tiempo universal (segundos desde 1900) a cadena en hora de Buenos Aires (UTC-3)."
  (let ((utime-ajustado (- utime (* 3 3600))))   ; restamos 3 horas
    (multiple-value-bind (seg min hora dia mes anio)
        (decode-universal-time utime-ajustado 0)
      (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
              anio mes dia hora min seg))))

;==============================================================================================================
;FUNCIóN: get-tiempo-colores
;NATURALEZA: IMPURA
;ESTRATEGIA:  No es recursiva. No emplea funciones de orden superior. No es una función predicado
;IMPACTO: No destructiva
;ENTRADAS:
;  config-path: Ruta al archivo config.json (opcional, por defecto "config.json")
;SALIDA:
;  Retorna una lista con los tiempos de duración para cada color: (tiempo_rojo tiempo_amarillo tiempo_verde).
;  Si el archivo no existe, retorna la lista vacía ().
;==============================================================================================================
(defun get-tiempo-colores (&optional (config-path "config.json"))
  (let* ((base-dir (if *load-truename*
                       (make-pathname :directory (pathname-directory *load-truename*))
                       *default-pathname-defaults*))
         (full-path (merge-pathnames config-path base-dir)))
    (with-open-file (stream full-path
                            :direction :input
                            :if-does-not-exist nil)
      (if stream
          (let ((datos (json:decode-json stream)))
            (list (cdr (assoc :ROJO datos))
                  (cdr (assoc :AMARILLO datos))
                  (cdr (assoc :VERDE datos))))
          '()))))   ; <--- retorna lista vacía si no se pudo abrir

;==============================================================================================================
;FUNCIóN: get-tiempo-colores-iter
;NATURALEZA: IMPURA
;ESTRATEGIA:  No es recursiva. No emplea funciones de orden superior. No es una función predicado
;IMPACTO: No destructiva
;ENTRADAS:
;  config-path: Ruta al archivo config.json (opcional, por defecto "config.json")
;SALIDA:
;  Retorna una lista con los tiempos de duración para cada color y sus intermitencias:
;  (tiempo_rojo tiempo_amarillo tiempo_verde tiempo_iter_rojo tiempo_iter_amarillo tiempo_iter_verde).
;  Si el archivo no existe, retorna la lista vacía ().
;==============================================================================================================
(defun get-tiempo-colores-iter (&optional (config-path "config.json"))
  (let* ((base-dir (if *load-truename*
                       (make-pathname :directory (pathname-directory *load-truename*))
                       *default-pathname-defaults*))
         (full-path (merge-pathnames config-path base-dir)))
    (with-open-file (stream full-path
                            :direction :input
                            :if-does-not-exist nil)
      (if stream
          (let ((datos (json:decode-json stream)))
            (list (cdr (assoc :ROJO datos))
                  (cdr (assoc :AMARILLO datos))
                  (cdr (assoc :VERDE datos))
                  (cdr (assoc :itr datos))
                  (cdr (assoc :ita datos))
                  (cdr (assoc :itv datos))))
          '()))))   ; <--- retorna lista vacía si no se pudo abrir



;==============================================================================================================
;INICIA REQUERIMIENTO 1
;==============================================================================================================

;===========================================================================================================

;Función que muestra la información de cambio de estado
;FUNCIÓN: transicion
;NATURALEZA: PURA
;ESTRATEGIA:No utiliza recursión, no utiliza funciones de orden superior, no es una funcion predicado.
;IMPACTO: No destructiva.

;Entradas:
;color-actual: Determina el color en el que se encuentra actualmente el semÃ¡foro
;sus valores pueden ser 'en-rojo, 'en-amarillo, 'en-verde
;cambiar-a: Determina a quÃ© color debe cambiar el semÃ¡foro dependiendo del valor
;indicado en color-actual. Sus posibles valores son: 
;"cambiar-a-amarillo", "cambiar-a-verde", "cambiar-a-rojo"

;Retorno:
;Retorna una lista con dos elementos. El primero indica el color actual del semÃ¡foro
;y el segundo indica el color al cual cambia

;===========================================================================================================
(defun transicion (color-actual cambiar-a)
  (cond ((and (eq color-actual 'en-rojo) (eq cambiar-a 'amarillo)) (list color-actual "cambiar-a-amarillo"))
        ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'verde))(list color-actual "cambiar-a-verde"))
        ((and(eq color-actual 'en-verde) (eq cambiar-a 'rojo)) (list color-actual "cambiar-a-rojo"))
        (T (list color-actual 'accion-por-defecto))
  
  )
) 

;==============================================================================================================
;FINALIZA REQUERIMIENTO 1
;==============================================================================================================


;==============================================================================================================
;INICIA REQUERIMIENTO 2
;==============================================================================================================


;==============================================================================================================
;Requerimiento 2: Temporizador Automático
;FUNCIóN: timer
;NATURALEZA: PURA
;ESTRATEGIA: Utiliza funciones de orden superior y condicionales
;IMPACTO: No destructiva

;ENTRADAS:
;tiempo-unix: Es un entero que representa el tiempo unix de una fecha en particular.

;SALIDA:
;Retorna un átomo que indica el color en el que se encuentra el semáforo para el tiempo unix pasado.
;Por defecto, presupone que el tiempo cero de arranque del semáforo es para el tiempo unix inicial que es en 
;1970. Se deben pasar los datos de los tiempos. Estos datos se deben obtener del archivo config.json
;NOTA: Esta es la que recupera los datos de los tiempos desde config.json
;==============================================================================================================
(defun timer (tiempo-unix)
  (let* ((tiempos (or (get-tiempo-colores) '(90 6 120)))         ; lista (rojo amarillo verde)
         (rojo (first tiempos))
         (amarillo (second tiempos))
         (tiempo-total (reduce #'+ tiempos)))    ; suma de los tres
    (cond
      ((and (>= (mod tiempo-unix tiempo-total) 0)
            (<= (mod tiempo-unix tiempo-total) (- rojo 1)))
       'en-rojo)
      ((and (>= (mod tiempo-unix tiempo-total) rojo)
            (< (mod tiempo-unix tiempo-total) (+ rojo amarillo)))
       'en-amarillo)
      (t 'en-verde))))




;==============================================================================================================
;Requerimiento 2: Temporizador Automático
;FUNCIóN: control-timer
;NATURALEZA: PURA
;ESTRATEGIA: Utiliza funciones condicionales
;IMPACTO: No destructiva

;ENTRADAS:
;tiempo-unix: Es un entero que representa el tiempo unix de una fecha en particular.
;lista-tiempos: La lista de tiempo de los colores obtenidos desde el archivo config.json

;SALIDA:
;Retorna un mensaje de error en caso de que algunas condiciones no se cumplan.
;En caso de que ningún error ocurra, ejecuta la funcion timer

(defun control-timer (tiempo-unix lista-tiempos)
	(cond ((< tiempo-unix 0 ) "El tiempo no puede ser menor a cero")
		  ((<= (nth 0 lista-tiempos) 0) "El tiempo del semáforo en rojo no puede ser cero o menos")
      ((<= (nth 1 lista-tiempos) 0) "El tiempo del semáforo en amarillo no puede ser cero o menos")
      ((<= (nth 2 lista-tiempos) 0) "El tiempo del semáforo en verde no puede ser cero o menos")
		  (T (timer tiempo-unix))

	)
)
;==============================================================================================================
;FINALIZA REQUERIMIENTO 2
;==============================================================================================================


;==============================================================================================================
;INICIA REQUERIMIENTO 3
;==============================================================================================================



;===========================================================================================================

;Requerimiento 3: Sistema de Auditoría
;Función: estado-semaforo
;NATURALEZA: Impura
;ESTRATEGIA De Control: No utiliza recursión, no utiliza funciones de orden. Empleado condicionales. 
;superior, no es una función predicado.
;IMPACTO En Memoria: No destructiva.

;Entradas:
;data_time: Entero que indica la cantidad de segundos desde el 
;arranque del semáforo

;Retorno:
;Retorna una cadena que indica el cambio de color o indica el color en el que se encuentra el semáforo
;NOTA: Esta versión presupone que rojo=90s, amarillo=6s y verde=120s.

;===========================================================================================================
(defun estado-semaforo (tiempo-unix)
  (let* ((tiempos (get-tiempo-colores))
         (rojo (first tiempos))
         (amarillo (second tiempos))
         (total (reduce #'+ tiempos))
         (resto (mod tiempo-unix total)))
    (cond
      ((= resto rojo) "La luz ha cambiado de rojo a amarillo")
      ((= resto (+ rojo amarillo)) "La luz ha cambiado de amarillo a verde")
      ((= resto 0) "La luz ha cambiado de verde a rojo")
      (t (timer tiempo-unix)))))






;===========================================================================================================
;Requerimiento 3: Sistema de Auditoría
;Funcion: registrar-cambios
;Se encarga de mostrar en pantalla sólo las transiciones de colores
;NATURALEZA: FUNCIÓN Impura
;ESTRATEGIA De Control: No utiliza recursiÃ³n, no utiliza funciones de orden 
;superior, no es una FUNCIÓNpredicado.
;IMPACTO En Memoria: No destructiva.

;Entradas:
;tiempo-inicial: El tiempo en el cual se revisará el color en el que se encuentra el semáforo o si hay una 
;transición

;Retorno:
;Retorna una cadena que indica el tiempo en el cual se produjo la transición de colores.

;===========================================================================================================
(defun registrar-cambios (tiempo-inicial)
  ;estado puede tener: 'en-rojo, 'en-verde, 'en-amarillo, o La luz ha cambiado de rojo a amarillo
  ;La luz ha cambiado de amarillo a verde
  ;La luz ha cambiado de verde a rojo
  (let ((estado (estado-semaforo tiempo-inicial)))
	;Si estado tiene 'en-rojo, 'en-amarillo o 'en-verde, los ignora
    (unless (member estado '(en-rojo en-amarillo en-verde))
      (format t "Tiempo ~a  ->  ~a~%"
              (tiempo-a-string tiempo-inicial)
              estado))))

;(registrar_cambios(get-universal-time))




;==============================================================================================================
;FINAL REQUERIMIENTO 3
;==============================================================================================================


;==============================================================================================================
;INICIA REQUERIMIENTO 4
;==============================================================================================================



;; ========================================================
;; REQUERIMIENTO 4a: Recomendación de Ciclos
;; FUNCION: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: funcion de orden superior
;; IMPACTO: No destructiva
;; Entradas
;; lista-tiempos: Representa la lista de los tiempos obtenidas desde el archivo config.json

;;Retorno: Retorna un entero que representa el tiempo total del ciclo
;; ========================================================
(defun duracion-ciclo (lista-tiempos)
  (reduce #'+ lista-tiempos)
)


;; ========================================================
;; REQUERIMIENTO 4b: Recomendación de Ciclos
;; FUNCION: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva

;;Entradas
;;tiempo-analizado: Es un entero que representa el tiempo total del ciclo

;;Retorno: Retorna un string indicando si el tiempo del ciclo está dentro del rango
;;de tiempo recomendado
;; ========================================================
(defun recomendacion-ciclo (tiempo-analizado)
	(cond ((< tiempo-analizado 35) "Tiempo muy bajo. Se recomienda revisar el tiempo total")
		  ((> tiempo-analizado 150) "Tiempo muy alto. Se recomienda revisar el tiempo total")
		  (T "Tiempo dentro del rango [35; 150]")
		
	)
)
;; (recomendacion-ciclo(duracion-ciclo(get-tiempo-colores)))


;==============================================================================================================
;FIN REQUERIMIENTO 4
;==============================================================================================================


;==============================================================================================================
;INICIA REQUERIMIENTO 5
;==============================================================================================================

;==================================================================================================================
;Requerimiento 5: Planificación Temporal
;FUNCIÓN: ciclos-por-tiempo
;NATURALEZA: PURA
;ESTRATEGIA: Emplea funciones de orden superior (reduce)
;IMPACTO: No destructiva
;ENTRADAS

;minutos: Un entero que representa los minutos en los cuales se quiere calcular cuántos ciclos habrán.

;SALIDA:
;Retorma un entero que representa el número de ciclos presentes en los minutos indicados. Si sobran segundos
;al contabilizar los ciclos, los ignora.
;==================================================================================================================

(defun ciclos-por-tiempo (minutos)
  (floor (* 60 minutos) (reduce #'+ (get-tiempo-colores) ))

  )
                                        



;==============================================================================================================
;FINALIZA REQUERIMIENTO 5
;==============================================================================================================
 

;==============================================================================================================
;INICIA REQUERIMIENTO 6
;==============================================================================================================


;===========================================================================================================================================================================================
 ;Requerimiento 6: Informe de Distribución Temporal
 ;Función: distribucion-colores
 ;Se encarga de entregar la cantidad de colores rojos, amarillos y verde que se presentaron en un intervalo de tiempo determinado
 ;NATURALEZA: PURA
 ;ESTRATEGIA: Recursividad De Cola
 ;IMPACTO: No destructiva
 ;Entradas

 ;tiempo-inicial: El tiempo usado como referencia en el cual se inicia el conteo de los colores
 ;tiempo-analizado: El tiempo preciso en el cual se quiere saber en que color está el semáforo
 ;tiempo-max: La cantidad de segundos en el cual se quieren contar los colores desde el tiempo inicial
 ;cr: Cantidad de colores rojos
 ;ca: cantidad de colores amarillos
 ;cv: cantidad de colores verdes
 ;tr: tiempo de duración del rojo
 ;ta: tiempo de duración del amarillo
 ;tv: tiempo de duración del verde

;Salida

;Retorna una lista con la frecuencia de rojos, amarillos y verdes en el tiempo inicial indicado.
;El formato de la lista es (frecuencia-rojos frecuencia-amarillos frecuencia-verde)

 ;============================================================================================================================================================================================

                                       
(defun distribucion-colores (tiempo-inicial tiempo-analizado tiempo-max cr ca cv)

  (cond

    ((= tiempo-analizado tiempo-max) (list cr ca cv))
    ((eq (timer (+ tiempo-inicial tiempo-analizado)) 'en-rojo) (distribucion-colores tiempo-inicial (+ tiempo-analizado 1) tiempo-max (+ cr 1) ca cv))
    ((eq (timer (+ tiempo-inicial tiempo-analizado)) 'en-amarillo) (distribucion-colores tiempo-inicial (+ tiempo-analizado 1) tiempo-max cr  (+ ca 1) cv))

    (T (distribucion-colores tiempo-inicial (+ tiempo-analizado 1) tiempo-max cr ca (+ cv 1) ))




    )

  )

;===========================================================================================================================================================================================
 ;Requerimiento 6: Informe de Distribución Temporal
 ;Función: calcular-procentajes
 ;Se encarga de calcular los porcentajes de rojo, amarillo y verde tomando como entrada las frecuencias de los colores
 ;NATURALEZA: PURA
 ;ESTRATEGIA: Utiliza funciones de orden superior
 ;IMPACTO: No destructiva
 ;Entradas

 ;lista: Representa la lista con tres miembros: el total de rojos, el total de amarillos y el total de verdes obtenidos dentro de un tiempo determinado

;Salida

;Retorna una lista con los porcentajes de rojos, amarillos y verdes en un tiempo indicado.
;El formato de salida es (porcentaje_de_rojos porcentaje_de_amarillos porcentaje_de_verdes)

 ;======================================================================================================================================================================================
(defun  calcular-porcentajes (lista)

  (list (* ( / (nth 0 lista) (reduce #'+ lista)) 100.0) (* ( / (nth 1 lista) (reduce #'+ lista)) 100.0) (* ( / (nth 2 lista) (reduce #'+ lista)) 100.0)))


;==============================================================================================================
;FINALIZA REQUERIMIENTO 6
;==============================================================================================================





;==============================================================================================================
;Requerimiento: Iteración 2-Extensión 1: Intermitencia de Seguridad
;FUNCIóN: intervalo-rojo
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
;  tiempo-unix: Entero
;  total: Entero (duración total del ciclo)
;  tiempos: Lista (rojo amarillo verde iter-r iter-a iter-v)
;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en rojo.
;==============================================================================================================
(defun intervalo-rojo (tiempo-unix total tiempos)
  (let ((resto (mod tiempo-unix total))
        (rojo (first tiempos)))
    (cond ((and (>= resto 0) (<= resto (- rojo 1))) T)
          (T NIL))))

;==============================================================================================================
;Requerimiento: Iteración 2-Extensión 1: Intermitencia de Seguridad
;FUNCIóN: intervalo-iter-rojo
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
;  tiempo-unix: Entero
;  total: Entero (duración total del ciclo)
;  tiempos: Lista (rojo amarillo verde iter-r iter-a iter-v)
;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en intermitente rojo.
;==============================================================================================================
(defun intervalo-iter-rojo (tiempo-unix total tiempos)
  (let ((resto (mod tiempo-unix total))
        (rojo (first tiempos))
        (iter-r (fourth tiempos)))
    (cond ((and (>= resto rojo) (< resto (+ rojo iter-r))) T)
          (T NIL))))

;==============================================================================================================
;Requerimiento: Iteración 2-Extensión 1: Intermitencia de Seguridad
;FUNCIóN: intervalo-amarillo
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
;  tiempo-unix: Entero
;  total: Entero (duración total del ciclo)
;  tiempos: Lista (rojo amarillo verde iter-r iter-a iter-v)
;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en amarillo.
;==============================================================================================================
(defun intervalo-amarillo (tiempo-unix total tiempos)
  (let ((resto (mod tiempo-unix total))
        (rojo (first tiempos))
        (iter-r (fourth tiempos))
        (amarillo (second tiempos)))
    (cond ((and (>= resto (+ rojo iter-r)) (< resto (+ rojo iter-r amarillo))) T)
          (T NIL))))

;==============================================================================================================
;Requerimiento: Iteración 2-Extensión 1: Intermitencia de Seguridad
;FUNCIóN: intervalo-iter-amarillo
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
;  tiempo-unix: Entero
;  total: Entero (duración total del ciclo)
;  tiempos: Lista (rojo amarillo verde iter-r iter-a iter-v)
;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en intermitente amarillo.
;==============================================================================================================
(defun intervalo-iter-amarillo (tiempo-unix total tiempos)
  (let ((resto (mod tiempo-unix total))
        (rojo (first tiempos))
        (iter-r (fourth tiempos))
        (amarillo (second tiempos))
        (iter-a (fifth tiempos)))
    (cond ((and (>= resto (+ rojo iter-r amarillo))
                (< resto (+ rojo iter-r amarillo iter-a))) T)
          (T NIL))))

;==============================================================================================================
;Requerimiento: Iteración 2-Extensión 1: Intermitencia de Seguridad
;FUNCIóN: intervalo-verde
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
;  tiempo-unix: Entero
;  total: Entero (duración total del ciclo)
;  tiempos: Lista (rojo amarillo verde iter-r iter-a iter-v)
;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en verde.
;==============================================================================================================
(defun intervalo-verde (tiempo-unix total tiempos)
  (let ((resto (mod tiempo-unix total))
        (rojo (first tiempos))
        (iter-r (fourth tiempos))
        (amarillo (second tiempos))
        (iter-a (fifth tiempos))
        (verde (third tiempos)))
    (cond ((and (>= resto (+ rojo iter-r amarillo iter-a))
                (< resto (+ rojo iter-r amarillo iter-a verde))) T)
          (T NIL))))

;==============================================================================================================
;Requerimiento: Iteración 2-Extensión 1: Intermitencia de Seguridad
;FUNCIóN: timer-iter
;NATURALEZA: PURA
;ESTRATEGIA: Emplea condicionales y funciones predicado
;IMPACTO: No destructiva
;ENTRADAS:
;  tiempo-unix: Entero que indica la cantidad de segundos desde el epoch
;SALIDA:
;Retorna el color en el que se encuentra el semáforo
;==============================================================================================================
(defun timer-iter (tiempo-unix)
  (let* ((tiempos (or (get-tiempo-colores-iter) '(90 6 120 3 3 3)))  
         (total (reduce #'+ tiempos)))
    (cond 
      ((intervalo-rojo tiempo-unix total tiempos) 
       'en-rojo)
      ((intervalo-iter-rojo tiempo-unix total tiempos) 
       'en-intermitente-rojo)
      ((intervalo-amarillo tiempo-unix total tiempos) 
       'en-amarillo)
      ((intervalo-iter-amarillo tiempo-unix total tiempos) 
       'en-intermitente-amarillo)
      ((intervalo-verde tiempo-unix total tiempos) 
       'en-verde)
      (t 
       'en-intermitente-verde))))



;===========================================================================================================
;Requerimiento: Iteración 2-Extensión 1: Intermitencia de Seguridad
;Función: estado-semaforo-iter

;NATURALEZA: Función Impura
;ESTRATEGIA De Control: No utiliza recursión, no utiliza funciones de orden 
;superior, no es una función predicado. Emplea condicionales
;IMPACTO En Memoria: No destructiva.

;Entradas:


;Retorno:
;Retorna una cadena que indica el cambio de color o indica el color en el que se encuentra el semáforo


;===========================================================================================================

(defun estado-semaforo-iter (tiempo-unix)
  "Retorna el mensaje de cambio si el tiempo coincide con un punto de transición,
   o el color normal (usando timer-iter) en cualquier otro caso.
   Los tiempos se obtienen desde get-tiempo-colores-iter."
  (let* ((tiempos (get-tiempo-colores-iter))   ; lista (rojo amarillo verde iter-r iter-a iter-v)
         (rojo (first tiempos))
         (amarillo (second tiempos))
         (verde (third tiempos))
         (iter-r (fourth tiempos))
         (iter-a (fifth tiempos))
         (total (reduce #'+ tiempos))   ; suma de todos los tiempos
         (resto (mod tiempo-unix total)))
    (cond
      ((= resto rojo)
       "La luz ha cambiado de rojo a rojo-intermitente")
      ((= resto (+ rojo iter-r))
       "La luz ha cambiado de rojo-intermitente a amarillo")
      ((= resto (+ rojo iter-r amarillo))
       "La luz ha cambiado de amarillo a amarillo-intermitente")
      ((= resto (+ rojo iter-r amarillo iter-a))
       "La luz ha cambiado de amarillo-intermitente a verde")
      ((= resto (+ rojo iter-r amarillo iter-a verde))
       "La luz ha cambiado de verde a verde-intermitente")
      ((= resto 0)
       "La luz ha cambiado de verde-intermitente a rojo")
      (t
       (timer-iter tiempo-unix)))))

;==============================================================================================================
;INICIO Extensión 2: Persistencia de Datos
;==============================================================================================================




;==============================================================================================================
;Requerimiento:  Extensión 2: Persistencia de Datos
;Funcion: informe
;Permite almacenar en un archivo de texto los datos de las transiciones
;NATURALEZA: Función Impura
;ESTRATEGIA De Control: No utiliza recursión, no utiliza funciones de orden 
;superior, no es una función predicado.
;IMPACTO En Memoria: No destructiva.
;Entrada:

;tiempo-inicial: El tiempo en el cual se quiere analizar si hubo una transición
;archivo: La ruta del archivo. Se coloca una ruta por defecto

;Salida: Muestra un mensaje con el estado en que se encuentra el semáforo. Si es una transición, almacena en
;un archivo de texto la fecha y hora de la transición, así como la transición de la que se trata.
;==============================================================================================================




(defun informe (tiempo-inicial &optional (archivo "registro_semaforo_iter.txt"))
  (let ((estado (estado-semaforo tiempo-inicial)))
    (unless (member estado '(en-rojo en-amarillo en-verde))
      (with-open-file (out archivo
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (format out "~%Tiempo ~a  ->  ~a~%"
                (tiempo-a-string tiempo-inicial)
                estado)
        (finish-output out)))))


;==============================================================================================================
;Requerimiento:  Extensión 2: Persistencia de Datos
;Funcion: informe-iter
;Permite almacenar en un archivo de texto los datos de las transiciones con intermitencia
;NATURALEZA: Función Impura
;ESTRATEGIA De Control: No utiliza recursión, no utiliza funciones de orden 
;superior, no es una función predicado.
;IMPACTO En Memoria: No destructiva.
;Entrada:

;tiempo-inicial: El tiempo en el cual se quiere analizar si hubo una transición
;archivo: La ruta del archivo. Se coloca una ruta por defecto

;Salida: Muestra un mensaje con el estado en que se encuentra el semáforo. Si es una transición, almacena en
;un archivo de texto la fecha y hora de la transición, así como la transición de la que se trata.
;==============================================================================================================
(defun informe-iter (tiempo-inicial &optional (archivo "registro_semaforo_iter.txt"))
  (let ((estado (estado-semaforo-iter tiempo-inicial)))
    (unless (member estado '(en-rojo en-amarillo en-verde en-intermitente-rojo en-intermitente-amarillo en-intermitente-verde))
      (with-open-file (out archivo
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (format out "~%Tiempo ~a  ->  ~a~%"
                (tiempo-a-string tiempo-inicial)
                estado)
        (finish-output out)))))

;==============================================================================================================
;Requerimiento:  Extensión 2: Persistencia de Datos
;Funcion: guardar-informe
;Permite almacenar en un archivo de texto los datos de las transiciones que se producen desde una fecha inicial hasta
;una cierta cantidad de segundos después de esa fecha.

;NATURALEZA: Función Impura
;ESTRATEGIA De Control: Recursividad de cola
;IMPACTO En Memoria: No destructiva.
;Entrada:

;tiempo-inicial: El tiempo desde el cual se quiera analizar las transiciones
;tiempo-analizado: El tiempo en el cual se quiere saber si hubo una transicion o no
;tiempo-max: El tiempo máximo en segundos hasta donde se quieren analizar las transiciones

;Salida: Grabará un archivo de texto con los tiempos en los cuales ocurrió las transiciones
;==============================================================================================================


(defun guardar-informe (tiempo-inicial tiempo-analizado tiempo-max)
  (when (< tiempo-analizado tiempo-max)
    (informe-iter (+ tiempo-inicial tiempo-analizado))
    (guardar-informe  tiempo-inicial (1+ tiempo-analizado) tiempo-max)))


;==============================================================================================================
;Requerimiento:  Extensión 2: Persistencia de Datos
;Funcion: guardar-informe-iter
;Permite almacenar en un archivo de texto los datos de las transiciones que se producen desde una fecha inicial hasta
;una cierta cantidad de segundos después de esa fecha. Las transiciones son con intermitencia

;NATURALEZA: Función Impura
;ESTRATEGIA De Control: Recursividad de cola
;IMPACTO En Memoria: No destructiva.
;Entrada:

;tiempo-inicial: El tiempo desde el cual se quiera analizar las transiciones
;tiempo-analizado: El tiempo en el cual se quiere saber si hubo una transicion o no
;tiempo-max: El tiempo máximo en segundos hasta donde se quieren analizar las transiciones

;Salida: Grabará un archivo de texto con los tiempos en los cuales ocurrió las transiciones
;==============================================================================================================
(defun guardar-informe-iter (tiempo-inicial tiempo-analizado tiempo-max)
  (when (< tiempo-analizado tiempo-max)
    (informe-iter (+ tiempo-inicial tiempo-analizado))
    (guardar-informe-iter  tiempo-inicial (1+ tiempo-analizado) tiempo-max)))


;==============================================================================================================
;FINALIZA Extensión 2: Persistencia de Datos
;==============================================================================================================


;; =============================================================================================================
;;Requerimiento 7: Aseguramiento de la calidad
; EJEMPLOS DE USO (aseguramos que se ejecuten en el paquete :semaforo)
;; =============================================================================================================
(in-package :semaforo)

(transicion 'en-rojo 'verde)

(transicion 'en-rojo 'amarillo)

(timer (get-unix-time))

(estado-semaforo (get-unix-time))

(registrar-cambios (get-unix-time))

(duracion-ciclo (get-tiempo-colores))

(recomendacion-ciclo 120)

(ciclos-por-tiempo 15)

(calcular-porcentajes (distribucion-colores (get-unix-time) 0 3600 0 0 0))

;;(informe (get-unix-time))

;;(guardar-informe (get-unix-time) 0 300)

;;(guardar-informe-iter (get-unix-time) 0 300)