;IMPORTANTE!!!!
;Cuando usas la biblioteca quicklisp se deben importar primero las bibliotecas local-time y cl-json
;Para importar local-time: (ql:quickload :local-time)
;Para importar cl-json: (ql:quickload :cl-json)

(ql:quickload :local-time)
(ql:quickload :cl-json)




;Referencias
;Definición De Tiempo Unix: https://espanol.epochconverter.com/
;Definición de Universal Time en Lisp: https://lispcookbook.github.io/cl-cookbook/dates_and_times.html
;Prompt De ChatGPT: Cómo se obtiene en lisp esto: Tiempo Unix actual (entero)


;==============================================================================================================
;FUNCIóN: get-unix-time
;NATURALEZA: PURA
;ESTRATEGIA: No se usan estructuras de control.
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
;INICIA REQUERIMIENTO 1
;==============================================================================================================

;===========================================================================================================

;Función que muestra la informaciÃ³n de cambio de estado
;funcion: transicion
;Naturaleza: FunciÃ³n Pura
;Estrategia De Control: No utiliza recursiÃ³n, no utiliza funciones de orden 
;superior, no es una funciÃ³n predicado.
;Impacto En Memoria: No destructiva.

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
(defun transicion(color-actual cambiar-a)
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

;=====================================================================================================================

;Función: timer-transition (se le cambia el nombre, para que no choque con los nombres de otras bibliotecas) 
; Permite obtener el color en el que está el semáforo para un tiempo  en segundos pasado como parámetro

;Naturaleza: FunciÃ³n Pura
;Estrategia: No utiliza recursiÃ³n, no utiliza funciones de orden superior, no es una funcion predicado
;Impacto: No destructiva

;Entradas:
;data-time: Entero que indica la cantidad de segundos desde el 
;arranque del semÃ¡foro

;Retorno:
;Retorna una constante que indica el estado en el que se encuentra el 
;semÃ¡foro en el tiempo pasado.
;NOTA: Este supone que el tiempo total del ciclo rojo->amarillo->verde->rojo es 216 
;y que los tiempos son rojo=90s, amarillo=6s, verde=120s. Esta fue la primera que se hizo

;====================================================================================================================

(defun timer-transition(data-time)

  (cond 
    ((< (mod data-time 216) 90) 'en-rojo )
    ( (and (>= (mod data-time 216) 90) (<= (mod data-time 216) 95)) 'en-amarillo)
     ( (and (>= (mod data-time 216) 96) (<= (mod data-time 216) 215)) 'en-verde)
  )
)

(timer-transition (get-universal-time))
;==============================================================================================================
;FUNCIóN: timer-2
;NATURALEZA: PURA
;ESTRATEGIA: Se utilizan condicionales.
;IMPACTO: No destructiva

;ENTRADAS:
;tiempo-unix: Es un entero que representa el tiempo unix de una fecha en particular.
;rojo: Representa el tiempo en segundos que el semáforo permanece en rojo
;amarillo: Representa el tiempo en segundos que el semáforo permanece en amarillo
;verde: Representa el tiempo en segundos que el semáforo permanece en verde.

;SALIDA:
;Retorna un átomo que indica el color en el que se encuentra el semáforo para el tiempo unix pasado.
;Por defecto, presupone que el tiempo cero de arranque del semáforo es para el tiempo unix inicial que es en 
;1970. Se deben pasar los datos de los tiempos. Estos datos se deben obtener del archivo config.json
;NOTA: Esta es la que recupera los datos de los tiempos desde config.json
;==============================================================================================================
(defun timer-2 (tiempo-unix rojo amarillo verde)
  (cond ((and (>= (mod tiempo-unix (+ rojo amarillo verde)) 0) (<= (mod tiempo-unix (+ rojo amarillo verde)) (- rojo 1))) 'en-rojo) 
		((and (>= (mod tiempo-unix (+ rojo amarillo verde)) rojo) (< (mod tiempo-unix (+ rojo amarillo verde)) (+ rojo amarillo))) 'en-amarillo)
		(T 'en-verde)

	)

)

;==============================================================================================================
;FUNCIóN: get-tiempo-colores
;NATURALEZA: PURA
;ESTRATEGIA: No se utilizan condicionales.
;IMPACTO: No destructiva
;ENTRADAS:
.

;SALIDA:
;Retorna una lista con los tiempos de duración para cada color. Los datos se leen del archivo config.json.
;El formato de la salida es (tiempo_rojo tiempo_amarillo tiempo_verde). Se requiere la bliblioteca quicklisp
;y debe importarse la biblioteca mediante (ql:quickload :cl-json)
;==============================================================================================================
(defun get-tiempo-colores ()

  (with-open-file (stream "C:/Users/Usuario/Downloads/TPI-Funcional-2026-Grupo[9]/lisp/config.json"
                          :direction :input
                          :if-does-not-exist nil)
    (unless stream
      (error "No se pudo abrir el archivo"))
    (let ((datos (json:decode-json stream))) (list (cdr (assoc :ROJO datos)) (cdr (assoc :AMARILLO datos)) (cdr (assoc :VERDE datos)) ) )))



(timer-2 (get-universal-time) (car (get-tiempo-colores)) (car (cdr (get-tiempo-colores))) (car (cddr (get-tiempo-colores))))


(defun control-timer(tiempo-unix rojo amarillo verde)
	(cond ((< tiempo-unix 0 ) "El tiempo no puede ser menor a cero")
		  ((<= rojo 0) "El tiempo del semáforo en rojo no puede ser cero o menos")
		  (T (timer-2 tiempo-unix rojo amarillo verde))

	)
)

(control-timer (get-universal-time) (car (get-tiempo-colores)) (car (cdr (get-tiempo-colores))) (car (cddr (get-tiempo-colores))))

;==============================================================================================================
;FINALIZA REQUERIMIENTO 2
;==============================================================================================================


;==============================================================================================================
;INICIA REQUERIMIENTO 3
;==============================================================================================================
;Función: estado-semaforo
;NATURALEZA: Impura
;ESTRATEGIA De Control: No utiliza recursión, no utiliza funciones de orden. Empleado condicionales. 
;superior, no es una función predicado.
;IMPACTO En Memoria: No destructiva.

;Entradas:
;data-time: Entero que indica la cantidad de segundos desde el 
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
;FUNCION: registrar-cambios
;Se encarga de mostrar en pantalla sólo las transiciones de colores
;NATURALEZA: FUNCIÓN Impura
;ESTRATEGIA: No utiliza recursiÃ³n, no utiliza funciones de orden 
;superior, no es una FUNCIÓNpredicado.
;IMPACTO: No destructiva.

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

;(registrar-cambios(get-universal-time))




;==============================================================================================================
;FINALIZA REQUERIMIENTO 3
;==============================================================================================================

;==============================================================================================================
;INICIA REQUERIMIENTO 4
;==============================================================================================================
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
;===========================================================================================================================================================================================
 
;FUNCION: recomendacion-ciclo
;Permite recomendar un tiempo adecuado para el ciclo rojo->amarillo->verde->rojo

;NATURALEZA: PURA
;ESTRATEGIA: Uso de condicionales
;IMPACTO: No destructiva

;ENTRADA

;tiempo-analizado: Un entero que define el tiempo del ciclo rojo->amarillo->verde->rojo

;SALIDA

;Retorna un string indicando si el tiempo es muy bajo, muy alto o si está dentro del rango permitido.
;=====================================================================================================================================================================================

(defun recomendacion-ciclo (tiempo-analizado)
	(cond ((< tiempo-analizado 35) "Tiempo muy bajo. Se recomienda revisar el tiempo total")
		  ((> tiempo-analizado 150) "Tiempo muy alto. Se recomienda revisar el tiempo total")
		  (T "Tiempo dentro del rango [35; 150]")
		
	)
)



;==============================================================================================================
;FIN REQUERIMIENTO 4
;==============================================================================================================


;==============================================================================================================
;INICIA REQUERIMIENTO 5
;==============================================================================================================

;==================================================================================================================
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

(ciclos-por-tiempo -4)

                                        



;==============================================================================================================
;FINALIZA REQUERIMIENTO 5
;==============================================================================================================
 

;==============================================================================================================
;INICIA REQUERIMIENTO 6
;==============================================================================================================


;===========================================================================================================================================================================================
 ;FUNCION: distribucion-colores
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

                                       
(defun distribucion-colores (tiempo-inicial tiempo-analizado tiempo-max cr ca cv tr ta tv)

  (cond

    ((= tiempo-analizado tiempo-max) (list cr ca cv))
    ((eq (timer-2 (+ tiempo-inicial tiempo-analizado) tr ta tv) 'en-rojo) (distribucion-colores tiempo-inicial (+ tiempo-analizado 1) tiempo-max (+ cr 1) ca cv tr ta tv))
    ((eq (timer-2 (+ tiempo-inicial tiempo-analizado) tr ta tv) 'en-amarillo) (distribucion-colores tiempo-inicial (+ tiempo-analizado 1) tiempo-max cr  (+ ca 1) cv tr ta tv))

    (T (distribucion-colores tiempo-inicial (+ tiempo-analizado 1) tiempo-max cr ca (+ cv 1) tr ta tv))




    )

  )
;tiempo-inicial=100000, tiemp-max=3600
;100000+0--->tiempo-analizado=0  (cr=0, ca=0, cv=0)
;100000+1---->tiempo-analizado=1
;100000+2----->tiempo-analizado=2

;100000-100001-100002-100003---------------------------------------------------------------------1036000

;12:30 15/06/2026 ---->13:13 15/06/2026
;===========================================================================================================================================================================================
 ;FUNCION: calcular-procentajes
 ;Se encarga de calcular los porcentajes de rojo, amarillo y verde tomando como entrada las frecuencias de los colores
 ;NATURALEZA: PURA
 ;ESTRATEGIA: Utiliza funciones de orden superior
 ;IMPACTO: No destructiva
 ;Entradas

 ;lista: Representa la lista con tres miembros: el total de rojos, el total de verdes y el total de azul obtenidos dentro de un tiempo determinado

;Salida

;Retorna una lista con los porcentajes de rojos, amarillos y verdes en un tiempo indicado.
;El formato de salida es (porcentaje-de-rojos porcentaje-de-amarillos porcentaje-de-verdes)

 ;======================================================================================================================================================================================
(defun  calcular-porcentajes (lista)

  (list (* ( / (nth 0 lista) (reduce #'+ lista)) 100.0) (* ( / (nth 1 lista) (reduce #'+ lista)) 100.0) (* ( / (nth 2 lista) (reduce #'+ lista)) 100.0)))

(calcular-porcentajes (distribucion-colores (get-unix-time) 0 3600 0 0 0 (car (get-tiempo-colores)) (car (cdr (get-tiempo-colores))) (car (cddr (get-tiempo-colores)))))

(calcular-porcentajes '())
;==============================================================================================================
;FINALIZA REQUERIMIENTO 6
;==============================================================================================================


;==============================================================================================================
;FUNCIóN: get-tiempo-colores
;NATURALEZA: PURA
;ESTRATEGIA: No se utilizan condicionales.
;IMPACTO: No destructiva
;ENTRADAS:
.

;SALIDA:
;Retorna una lista con los tiempos de duración para cada color. Los datos se leen del archivo config.json.
;El formato de la salida es (tiempo-rojo tiempo-amarillo tiempo-verde tiempo-iter-rojo tiempo-iter-amarillo tiempo-iter-verde). Se requiere la bliblioteca quicklisp
;y debe importarse la biblioteca mediante (ql:quickload :cl-json)
;==============================================================================================================
(defun get-tiempo-colores ()

  (with-open-file (stream "c:/Users/espin/OneDrive/Documentos/LISI/Paradigma/json/config.json.txt"
                          :direction :input
                          :if-does-not-exist nil)
    (unless stream
      (error "No se pudo abrir el archivo"))
    (let ((datos (json:decode-json stream))) (list (cdr (assoc :ROJO datos)) (cdr (assoc :AMARILLO datos)) (cdr (assoc :VERDE datos)) 
	(cdr (assoc :itr  datos)) (cdr (assoc :ita datos)) (cdr (assoc :itv datos))) )))



;==============================================================================================================
;FUNCIóN: intervalo-rojo
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
.

;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en rojo.
;==============================================================================================================
(defun intervalo-rojo (tiempo-unix total rojo iter-r)

	 (cond ((and (>= (mod tiempo-unix total) 0) (<= (mod tiempo-unix total) (- rojo 1)) T) 
		(T NIL)))) 


;==============================================================================================================
;FUNCIóN: intervalo-iter-rojo
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
.

;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en intermitente rojo.
;==============================================================================================================
(defun intervalo-iter-rojo (tiempo-unix total rojo iter-r)
(cond
((and (>= (mod tiempo-unix total) rojo) (< (mod tiempo-unix total) (+ rojo iter-r))) T)
(T NIL)

)
) 


;==============================================================================================================
;FUNCIóN: intervalo-amarillo
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
.

;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en intermitente amarillo.
;==============================================================================================================
(defun intervalo-amarillo (tiempo-unix total rojo iter-r amarillo)
	(cond
	((and (>= (mod tiempo-unix total) (+ rojo iter-r)) (< (mod tiempo-unix total) (+ rojo iter-r amarillo))) T)
	(T NIL))

	
)

;==============================================================================================================
;FUNCIóN: intervalo-iter-amarillo
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
.

;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en intermitente amarillo.
;==============================================================================================================

(defun intervalo-iter-amarillo(tiempo-unix total rojo iter-r amarillo iter-a)

(cond
	((and (>= (mod tiempo-unix total) (+ rojo iter-r amarillo)) (< (mod tiempo-unix total) (+ rojo iter-r amarillo iter-a))) T)
	(T NIL))
)

;==============================================================================================================
;FUNCIóN: intervalo-verde
;NATURALEZA: PURA
;ESTRATEGIA: Funcion Predicado
;IMPACTO: No destructiva
;ENTRADAS:
;tiempo-unix: Entero que indica la cantidad de segundos 
;total: El tiempo total del ciclo
;rojo: el tiempo en rojo
;amarillo: el tiempo del amarillo
;verde: el tiempo del verde
;iter-r: el tiempo de rojo intermitente
;inter-a: el tiempo de amarillo intermitente
;iter-v: el tiempo de verde intermitente
;SALIDA:
;Retorna T si en el tiempo indicado el semáforo está en verde.
;==============================================================================================================
(defun intervalo-verde(tiempo-unix total rojo iter-r amarillo iter-a verde)

	(cond
	((and (>= (mod tiempo-unix total) (+ rojo iter-r amarillo iter-a)) (< (mod tiempo-unix total) (+ rojo iter-r amarillo iter-a verde))) T)
	(T NIL))	
)


;==============================================================================================================
;FUNCIóN: intervalo-amarillo
;NATURALEZA: PURA
;ESTRATEGIA: Emplea condicionales
;IMPACTO: No destructiva
;ENTRADAS:
;Entradas:
;tiempo-unix: Entero que indica la cantidad de segundos desde el 
;rojo: el tiempo en rojo
;amarillo: el tiempo del amarillo
;verde: el tiempo del verde
;iter-r: el tiempo de rojo intermitente
;inter-a: el tiempo de amarillo intermitente
;iter-v: el tiempo de verde intermitente

;SALIDA:
;Retorna el color en el que se encuentra el semáforo
;==============================================================================================================

(defun timer-iter (tiempo-unix rojo amarillo verde iter-r iter-a iter-v)
  (cond 
		((intervalo-rojo tiempo-unix (+ rojo amarillo verde iter-r iter-a iter-v) rojo iter-r) 'en-rojo)
		((intervalo-iter-rojo tiempo-unix (+ rojo amarillo verde iter-r iter-a iter-v) rojo iter-r) 'en-intermitente-rojo)
        ((intervalo-amarillo tiempo-unix (+ rojo amarillo verde iter-r iter-a iter-v) rojo iter-r amarillo) 'en-amarillo)
		((intervalo-iter-amarillo tiempo-unix (+ rojo amarillo verde iter-r iter-a iter-v) rojo iter-r amarillo iter-a) 'en-intermitente-amarillo)
		((intervalo-verde tiempo-unix (+ rojo amarillo verde iter-r iter-a iter-v) rojo iter-r amarillo iter-a verde) 'en-verde)
		(T 'en-intermitente-verde)

	)

)
;Ejemplo de cómo usarlo
(timer-iter (get-universal-time) (nth 0 (get-tiempo-colores)) (nth 1 (get-tiempo-colores)) (nth 2 (get-tiempo-colores)) 
(nth 3 (get-tiempo-colores)) (nth 4 (get-tiempo-colores)) (nth 5 (get-tiempo-colores)))


;===========================================================================================================

;Función que muestra en pantalla los cambios de estado
;FUNCION:traffic-light-status-from-config
;NATURALEZA: pura
;ESTRATEGIA: No utiliza recursión, no utiliza funciones de orden 
;superior, no es una función predicado. Emplea condicionales
;IMPACTO: No destructiva.

;Entradas:
;data-time: Entero que indica la cantidad de segundos desde el 
;rojo: el tiempo en rojo
;amarillo: el tiempo del amarillo
;verde: el tiempo del verde
;iter-r: el tiempo de rojo intermitente
;inter-a: el tiempo de amarillo intermitente
;iter-v: el tiempo de verde intermitente

;Retorno:
;Retorna una cadena que indica el cambio de color o indica el color en el que se encuentra el semáforo


;===========================================================================================================
(defun traffic-light-status-from-config
       (data-time rojo amarillo verde iter-r iter-a iter-v)

  (cond
    ((= (mod data-time
             (+ rojo amarillo verde iter-r iter-a iter-v))
        rojo)
     "La luz ha cambiado de rojo a rojo-intermitente")

    ((= (mod data-time
             (+ rojo amarillo verde iter-r iter-a iter-v))
        (+ rojo iter-r))
     "La luz ha cambiado de rojo-intermitente a amarillo")

    ((= (mod data-time
             (+ rojo amarillo verde iter-r iter-a iter-v))
        (+ rojo iter-r amarillo))
     "La luz ha cambiado de amarillo a amarillo-intermitente")

    ((= (mod data-time
             (+ rojo amarillo verde iter-r iter-a iter-v))
        (+ rojo iter-r amarillo iter-a))
     "La luz ha cambiado de amarillo-intermitente a verde")

    ((= (mod data-time
             (+ rojo amarillo verde iter-r iter-a iter-v))
        (+ rojo iter-r amarillo iter-a verde))
     "La luz ha cambiado de verde a verde-intermitente")

    ((= (mod data-time
             (+ rojo amarillo verde iter-r iter-a iter-v))
        0)
     "La luz ha cambiado de verde-intermitente a rojo")

    (t
     (timer-iter
      data-time
      rojo
      amarillo
      verde
      iter-r
      iter-a
      iter-v))))

(traffic-light-status-from-config
 (get-unix-time)
 (nth 0 (get-tiempo-colores))
 (nth 1 (get-tiempo-colores))
 (nth 2 (get-tiempo-colores))
 (nth 3 (get-tiempo-colores))
 (nth 4 (get-tiempo-colores))
 (nth 5 (get-tiempo-colores)))

;==============================================================================================================
;INICIO Extensión 2: Persistencia de Datos
;==============================================================================================================




;==============================================================================================================
;Funcion: informe
;Permite almacenar en un archivo de texto los datos de las transiciones
;Naturaleza: Función Impura
;Estrategia De Control: No utiliza recursión, no utiliza funciones de orden 
;superior, no es una función predicado.
;Impacto En Memoria: No destructiva.
;Entrada:

;tiempo-inicial: El tiempo en el cual se quiere analizar si hubo una transición
;archivo: La ruta del archivo. Se coloca una ruta por defecto

;Salida: Muestra un mensaje con el estado en que se encuentra el semáforo. Si es una transición, almacena en
;un archivo de texto la fecha y hora de la transición, así como la transición de la que se trata.
;==============================================================================================================




(defun informe (tiempo-inicial &optional (archivo "c:/Users/espin/OneDrive/Documentos/LISI/Paradigma/registro_semaforo.txt"))
  (let ((estado (traffic-light-status tiempo-inicial)))
    (format t "DEBUG: estado = ~a~%" estado)   ; imprime en pantalla para depuración
    (unless (member estado '(en-rojo en-amarillo en-verde))
      (with-open-file (out archivo
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (format out "~%Tiempo ~a  ->  ~a~%"
                (universal-to-datestring tiempo-inicial 0)
                estado)
        (finish-output out)))))


;==============================================================================================================
;Funcion: guardar-informe
;Permite almacenar en un archivo de texto los datos de las transiciones que se producen desde una fecha inicial hasta
;una cierta cantidad de segundos después de esa fecha.

;Naturaleza: Función Impura
;Estrategia De Control: Utiliza dotimes
;Impacto En Memoria: No destructiva.
;Entrada:

;tiempo-inicial: El tiempo en el cual se quiere analizar si hubo una transición
;ruta-archivo: La ruta del archivo. Se coloca una ruta por defecto
;tiempo-max: La cantidad de segundos desde el tiempo_inicial en el cual se contabilizarán las transiciones

;Salida: Muestra un mensaje con el estado en que se encuentra el semáforo. Si es una transición, almacena en
;un archivo de texto la fecha y hora de la transición, así como la transición de la que se trata.
;==============================================================================================================


(defun guardar-informe (i max-iter)
  (when (< i max-iter)
    (informe (+ (get-universal-time) i))
    (guardar-informe (1+ i) max-iter)))

(guardar-informe 0 3600)


;==============================================================================================================
;FINALIZA Extensión 2: Persistencia de Datos
;==============================================================================================================
