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

;Función: timer_transition (se le cambia el nombre, para que no choque con los nombres de otras bibliotecas) 
; Permite obtener el color en el que está el semáforo para un tiempo  en segundos pasado como parámetro

;Naturaleza: FunciÃ³n Pura
;Estrategia De Control: No utiliza recursiÃ³n, no utiliza funciones de orden 
;superior, no es una funciÃ³n predicado.
;Impacto En Memoria: No destructiva.

;Entradas:
;data_time: Entero que indica la cantidad de segundos desde el 
;arranque del semÃ¡foro

;Retorno:
;Retorna una constante que indica el estado en el que se encuentra el 
;semÃ¡foro en el tiempo pasado.
;NOTA: Este supone que el tiempo total del ciclo rojo->amarillo->verde->rojo es 216 
;y que los tiempos son rojo=90s, amarillo=6s, verde=120s. Esta fue la primera que se hizo

;====================================================================================================================

(defun timer_transition(data_time)

  (cond 
    ((< (mod data_time 216) 90) 'en-rojo )
    ( (and (>= (mod data_time 216) 90) (<= (mod data_time 216) 95)) 'en-amarillo)
     ( (and (>= (mod data_time 216) 96) (<= (mod data_time 216) 215)) 'en-verde)
  )
)

(timer_transition (get-universal-time))
;==============================================================================================================
;FUNCIóN: timer_2
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
(defun timer_2 (tiempo-unix rojo amarillo verde)
  (cond ((and (>= (mod tiempo-unix (+ rojo amarillo verde)) 0) (<= (mod tiempo-unix (+ rojo amarillo verde)) (- rojo 1))) 'en-rojo) 
		((and (>= (mod tiempo-unix (+ rojo amarillo verde)) rojo) (< (mod tiempo-unix (+ rojo amarillo verde)) (+ rojo amarillo))) 'en-amarillo)
		(T 'en-verde)

	)

)

;==============================================================================================================
;FUNCIóN: get_tiempo_colores
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
(defun get_tiempo_colores ()

  (with-open-file (stream "c:/Users/espin/OneDrive/Documentos/LISI/Paradigma/json/config.json.txt"
                          :direction :input
                          :if-does-not-exist nil)
    (unless stream
      (error "No se pudo abrir el archivo"))
    (let ((datos (json:decode-json stream))) (list (cdr (assoc :ROJO datos)) (cdr (assoc :AMARILLO datos)) (cdr (assoc :VERDE datos)) ) )))



(timer_2 (get-universal-time) (car (get_tiempo colores)) (car (cdr (get_tiempo_colores))) (car (cddr (get_tiempo_colores))))


(defun control_timer(tiempo-unix rojo amarillo verde)
	(cond ((< tiempo_unix 0 ) "El tiempo no puede ser menor a cero")
		  ((<= rojo 0) "El tiempo del semáforo en rojo no puede ser cero o menos")
		  (T (timer_2 tiempo-unix rojo amarillo verde))

	)
)

(control_timer (get-universal-time) (car (get_tiempo_colores)) (car (cdr (get_tiempo_colores))) (car (cddr (get_tiempo_colores))))

;==============================================================================================================
;FINALIZA REQUERIMIENTO 2
;==============================================================================================================


;==============================================================================================================
;INICIA REQUERIMIENTO 3
;==============================================================================================================

;===========================================================================================================
;FUNCIóN: universal-to-datestring
;NATURALEZA: PURA
;ESTRATEGIA: No se utilizan condicionales
;IMPACTO: No destructiva
;ENTRADAS:
;utime: Recibe el tiempo en segundos de una fecha determinada
;
;SALIDA:
;Retorna un string con el formato "AAAA-MM-DD HH:MM:SS"
;==============================================================================================================
(defun universal-to-datestring (utime &optional (timezone 0))
  (multiple-value-bind (seg min hora dia mes anio)
      (decode-universal-time utime timezone)
    (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            anio mes dia hora min seg)))


;(universal-to-datestring (get-universal-time))


;===========================================================================================================

;FunciÃ³n que muestra en pantalla los cambios de estado

;Naturaleza: FunciÃ³n Impura
;Estrategia De Control: No utiliza recursiÃ³n, no utiliza funciones de orden 
;superior, no es una funciÃ³n predicado.
;Impacto En Memoria: No destructiva.

;Entradas:
;data_time: Entero que indica la cantidad de segundos desde el 
;arranque del semÃ¡foro

;Retorno:
;Retorna una cadena que indica el cambio de color o indica el color en el que se encuentra el semáforo
;NOTA: Esta versión presupone que rojo=90s, amarillo=6s y verde=120s.

;===========================================================================================================
(defun traffic_ligth_status(data_time)

(cond
     ((= (mod data_time 216) 90) "La luz ha cambiado de rojo a amarillo" )
     ((= (mod data_time 216) 96) "La luz ha cambiado de amarillo a verde" )
     ((= (mod data_time 216) 0) "La luz ha cambiado de verde a rojo" )
     (T (timer_transition data_time))
))


;===========================================================================================================

;Función que muestra en pantalla los cambios de estado

;Naturaleza: FunciÃ³n Impura
;Estrategia De Control: No utiliza recursiÃ³n, no utiliza funciones de orden 
;superior, no es una funciÃ³n predicado.
;Impacto En Memoria: No destructiva.

;Entradas:
;data_time: Entero que indica la cantidad de segundos desde el 
;arranque del semÃ¡foro

;Retorno:
;Retorna una cadena que indica el cambio de color o indica el color en el que se encuentra el semáforo
;NOTA: Esta versión presupone que rojo=90s, amarillo=6s y verde=120s.

;===========================================================================================================
(defun traffic_ligth_status_from_config(data_time rojo amarillo verde)

(cond
     ((= (mod data_time (+ rojo amarillo verde)) rojo) "La luz ha cambiado de rojo a amarillo" )
     ((= (mod data_time (+ rojo amarillo verde)) (+ rojo amarillo)) "La luz ha cambiado de amarillo a verde" )
     ((= (mod data_time (+ rojo amarillo verde)) 0) "La luz ha cambiado de verde a rojo" )
     (T (timer_2 data_time rojo amarillo verde))
))





;===========================================================================================================

;Funcion: registrar_cambios
;Se encarga de mostrar en pantalla el color en el que se encuentra el semáforo en un momento dado o
;la transición de estados
;Naturaleza: FunciÃ³n Impura
;Estrategia De Control: No utiliza recursiÃ³n, no utiliza funciones de orden 
;superior, no es una funciÃ³n predicado.
;Impacto En Memoria: No destructiva.

;Entradas:
;tiempo-inicial: El tiempo en el cual se revisará el color en el que se encuentra el semáforo o si hay una 
;transición

;Retorno:
;Retorna una cadena que indica el cambio de color o indica el color en el que se encuentra el semáforo

;===========================================================================================================
(defun registrar_cambios(tiempo-inicial)
    
	(format t "Tiempo ~a  ->  ~a~%"
              (universal-to-datestring tiempo-inicial 0)
              (traffic_ligth_status tiempo-inicial))
)

;===========================================================================================================

;Funcion: registrar_cambios_2
;Se encarga de mostrar en pantalla sólo las transiciones de colores
;Naturaleza: FunciÃ³n Impura
;Estrategia De Control: No utiliza recursiÃ³n, no utiliza funciones de orden 
;superior, no es una funciÃ³n predicado.
;Impacto En Memoria: No destructiva.

;Entradas:
;tiempo-inicial: El tiempo en el cual se revisará el color en el que se encuentra el semáforo o si hay una 
;transición

;Retorno:
;Retorna una cadena que indica el tiempo en el cual se produjo la transición de colores.

;===========================================================================================================
(defun registrar_cambios_2 (tiempo-inicial)
  ;estado puede tener: 'en-rojo, 'en-verde, 'en-amarillo, o La luz ha cambiado de rojo a amarillo
  ;La luz ha cambiado de amarillo a verde
  ;La luz ha cambiado de verde a rojo
  (let ((estado (traffic_ligth_status tiempo-inicial)))
	;Si estado tiene 'en-rojo, 'en-amarillo o 'en-verde, los ignora
    (unless (member estado '(en-rojo en-amarillo en-verde))
      (format t "Tiempo ~a  ->  ~a~%"
              (universal-to-datestring tiempo-inicial 0)
              estado))))

;(registrar_cambios(get-universal-time))

(dotimes(segundo 320)
(registrar_cambios (+ (get-universal-time) segundo)))





;===========================================================================================================
;FUNCIóN: parse-fecha
;NATURALEZA: PURA
;ESTRATEGIA: No se utilizan condicionales
;IMPACTO: No destructiva
;ENTRADAS:
;fecha-hora-str: Es un sting en formato AAAA/MM/DD HH:MM:SS (Anio-Mes-Dia Hora:Minuto-Segundo).
;Supone que la está en UTC.
;
;SALIDA:
;Retorna cinco enteros que representan los segundos, los minutos, la hora, el día, el mes y el año del
;string pasado como parámetro.
;==============================================================================================================
(defun parse-fecha (fecha-hora-str)
 
  (flet ((to-int (s) (parse-integer s)))
    (let ((fecha (subseq fecha-hora-str 0 10))
          (hora  (subseq fecha-hora-str 11 19)))
      (multiple-value-bind (year month day)
          (values (to-int (subseq fecha 0 4))
                  (to-int (subseq fecha 5 7))
                  (to-int (subseq fecha 8 10)))
        (multiple-value-bind (hour minute second)
            (values (to-int (subseq hora 0 2))
                    (to-int (subseq hora 3 5))
                    (to-int (subseq hora 6 8)))
          (local-time:encode-timestamp 0 second minute hour day month year :offset 0))))))



;==============================================================================================================
;FUNCIóN: diferencia-tiempo
;NATURALEZA: PURA
;ESTRATEGIA: No se utilizan condicionales
;IMPACTO: No destructiva
;ENTRADAS:
;fecha-hora-str: Es un sting en formato AAAA/MM/DD HH:MM:SS (Anio-Mes-Dia Hora:Minuto-Segundo)

;SALIDA:
;Retorna un entero que representa, en segundos, la diferencia entre la fecha pasada como parámetro y
;la fecha actual.
;Ejemplo de uso: (diferencia-tiempo "2025-06-13 10:30:00")
;==============================================================================================================
(defun diferencia-tiempo (fecha-hora-str)
  
  (local-time:timestamp-difference (local-time:now)
                                   (parse-fecha fecha-hora-str)))

;la fecha en que el semáforo inició
(parse-fecha "2025-06-13 10:30:00")
(timer_2  (diferencia-tiempo "2025-06-13 10:30:00"))

;==============================================================================================================
;INICIA REQUERIMIENTO 3
;==============================================================================================================

;==============================================================================================================
;INICIA REQUERIMIENTO 4
;==============================================================================================================

;===========================================================================================================================================================================================
 
;FUNCION: recomendacion-ciclo
;Permite recomendar un tiempo adecuado para el ciclo rojo->amarillo->verde->rojo

;NATURALEZA: NO PURA
;ESTRATEGIA: Uso de condicionales
;IMPACTO: No destructiva

;ENTRADA

;tiempo_analizado: Un entero que define el tiempo del ciclo rojo->amarillo->verde->rojo

;SALIDA

;Retorna un string indicando si el tiempo es muy bajo, muy alto o si está dentro del rango permitido.
;=====================================================================================================================================================================================

(defun recomendacion-ciclo(tiempo_analizado)
	(cond ((< tiempo_analizado 35) "Tiempo muy bajo. Se recomienda revisar el tiempo total")
		  ((> tiempo_analizado 150) "Tiempo muy alto. Se recomienda revisar el tiempo total")
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
;ESTRATEGIA: Emplea funciones de orden superior (mapcar)
;IMPACTO: No destructiva
;ENTRADAS

;minutos: Un entero que representa los minutos en los cuales se quiere calcular cuántos ciclos habrán.

;SALIDA:
;Retorma un entero que representa el número de ciclos presentes en los minutos indicados. Si sobran segundos
;al contabilizar los ciclos, los ignora.
;==================================================================================================================

(defun ciclos-por-tiempo (minutos)
  (floor (* 60 minutos) (reduce #'+ (get_tiempo_colores) ))

  )

(ciclos-por-tiempo -4)

                                        



;==============================================================================================================
;FINALIZA REQUERIMIENTO 5
;==============================================================================================================
 

;==============================================================================================================
;INICIA REQUERIMIENTO 6
;==============================================================================================================


;===========================================================================================================================================================================================
 ;Función: distribucion_colores
 ;Se encarga de entregar la cantidad de colores rojos, amarillos y verde que se presentaron en un intervalo de tiempo determinado
 ;NATURALEZA: PURA
 ;ESTRATEGIA: Recursividad De Cola
 ;IMPACTO: No destructiva
 ;Entradas

 ;tiempo_inicial: El tiempo usado como referencia en el cual se inicia el conteo de los colores
 ;tiempo_analizado: El tiempo preciso en el cual se quiere saber en que color está el semáforo
 ;tiempo_max: La cantidad de segundos en el cual se quieren contar los colores desde el tiempo inicial
 ;cr: Cantidad de colores rojos
 ;ca: cantidad de colores amarillos
 ;cv: cantidad de colores verdes
 ;tr: tiempo de duración del rojo
 ;ta: tiempo de duración del amarillo
 ;tv: tiempo de duración del verde

;Salida

;Retorna una lista con la frecuencia de rojos, amarillos y verdes en el tiempo inicial indicado.
;El formato de la lista es (frecuencia_rojos frecuencia_amarillos frecuencia_verde)

 ;============================================================================================================================================================================================

                                       
(defun distribucion_colores (tiempo_inicial tiempo_analizado tiempo_max cr ca cv tr ta tv)

  (cond

    ((= tiempo_analizado tiempo_max) (list cr ca cv))
    ((eq (timer_2 (+ tiempo_inicial tiempo_analizado) tr ta tv) 'en-rojo) (distribucion_colores tiempo_inicial (+ tiempo_analizado 1) tiempo_max (+ cr 1) ca cv tr ta tv))
    ((eq (timer_2 (+ tiempo_inicial tiempo_analizado) tr ta tv) 'en-amarillo) (distribucion_colores tiempo_inicial (+ tiempo_analizado 1) tiempo_max cr  (+ ca 1) cv tr ta tv))

    (T (distribucion_colores tiempo_inicial (+ tiempo_analizado 1) tiempo_max cr ca (+ cv 1) tr ta tv))




    )

  )
;tiempo_inicial=100000, tiempo_max=3600
;100000+0--->tiempo_analizado=0  (cr=0, ca=0, cv=0)
;100000+1---->tiempo_analizado=1
;100000+2----->tiempo_analizado=2

;100000-100001-100002-100003---------------------------------------------------------------------1036000

;12:30 15/06/2026 ---->13:13 15/06/2026
;===========================================================================================================================================================================================
 ;Función: calcular_procentajes
 ;Se encarga de calcular los porcentajes de rojo, amarillo y verde tomando como entrada las frecuencias de los colores
 ;NATURALEZA: PURA
 ;ESTRATEGIA: Utiliza funciones de orden superior
 ;IMPACTO: No destructiva
 ;Entradas

 ;lista: Representa la lista con tres miembros: el total de rojos, el total de verdes y el total de azul obtenidos dentro de un tiempo determinado

;Salida

;Retorna una lista con los porcentajes de rojos, amarillos y verdes en un tiempo indicado.
;El formato de salida es (porcentaje_de_rojos porcentaje_de_amarillos porcentaje_de_verdes)

 ;======================================================================================================================================================================================
(defun  calcular_porcentajes (lista)

  (list (* ( / (nth 0 lista) (reduce #'+ lista)) 100.0) (* ( / (nth 1 lista) (reduce #'+ lista)) 100.0) (* ( / (nth 2 lista) (reduce #'+ lista)) 100.0)))

(calcular_porcentajes (distribucion_colores (get-unix-time) 0 3600 0 0 0 (car (get_tiempo_colores)) (car (cdr (get_tiempo_colores))) (car (cddr (get_tiempo_colores)))))

(calcular_porcentajes '())
;==============================================================================================================
;FINALIZA REQUERIMIENTO 6
;==============================================================================================================
