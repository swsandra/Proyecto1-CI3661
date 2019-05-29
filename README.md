# Proyecto #1: Laboratorio de Lenguajes de Programación I
## El sabio del laberinto

Modelado de un juego en el cual el programa es el sabio del laberinto, y el jugador debe seleccionar correctamente sus movimientos en el laberinto con la ayuda del sabio. Las jugadas válidas sólo pueden ser hacia la izquierda, derecha o recto, y sólo puede realizarse el movimiento si la pared en esa dirección no se ha derrumbado. El objetivo del juego es  recorrer el laberinto, que puede ser creado por el jugador o cargado de un archivo .txt, hasta encontrar la mayor cantidad de tesoros posible. 

El juego se basa en el uso de una estructura de tipo Laberinto y un programa cliente que interactúa con el jugador para recorrer el laberinto. Ambos son descritos detenidamente, desde su estructura interna hasta sus funcionalidades, en el [enunciado del proyecto](https://github.com/swsandra/Proyecto1-CI3661/blob/master/Proyecto%201%20Septiembre.pdf).

El presente README se divide en secciones de acuerdo a la organización del cliente en cuanto a algunas funciones en Laberinto.hs y algunas las opciones para el usuario que requieren explicación extensiva.

### Autores:

* Aurivan Castro (14-10205@usb.ve)
* Sandra Vera (14-11130@usb.ve)

## Sobre la Implementación

### Acerca del Laberinto

En Laberinto.hs se encuentra tanto la definición de la data Laberinto, como las funciones para manejar instancias de esta clase. En las siguientes secciones sólo son mencionadas funciones que requieran mayor información para su mejor entendimiento.


#### De las funciones de Acceso

1. `agregarLaberinto`

	Esta función agrega a un laberinto a partir de un string. En este sentido, si dicho string contiene un caracter inválido, esto es, algo diferente a 'd', 'i', 'r' para los Laberintos Trifurcacion y 'r' para los Laberintos Tesoro; entonces el mismo es "ignorado" haciendo que la función retorne el mismo laberinto que fue recibido.

2. `voltearIzquierda`, `voltearDerecha`, `irRecto`
	
	En caso de ser una operación inválida, no se realiza ninguna de las operaciones. Por ejemplo, si se está en un Tesoro donde sólo se puede seguir recto, la función `voltearIzquierda` retornará el mismo laberinto que recibió; lo mismo para cuando se quiere girar a la izquiera en una trifurcación donde a la izquiera hay Nothing.

3. `recorrerLaberinto`
	
	Si el string pasa un caracter no válido como los mencionados en `agregarLaberinto`, se pasa al siguiente caracter del string, hasta que el string esté vacío, en cuyo caso retorna el mismo laberinto que recibió.


#### De las funciones auxiliares

1. `evaluarLaberinto`
	
	Indica si el laberinto que ha recibido es un Camino sin Salida ('S'), una trifurcación que no es camino sin salida ('T') o un Tesoro ('E').

2. `reportarParedAbierta`
	
	Esta función recorre el laberinto hasta el momento a encontrarse con una pared. Se reconoce cuando el laberinto a recorrer para la siguiente letra del string de la ruta devuelve el mismo laberinto (por las razones indicadas arriba para `recorrerLaberinto`, también se realiza una validación más para segurar que el primer caracter es un movimiento válido). En este caso, debe evaluarse si el string es de al menos una letra, donde basta con anexar al laberinto un camino sin salida, o en caso contrario, se debe crear un laberinto con el string restante y luego anexarlo al laberinto principal por medio de ese primer caracter evaluado antes. Si no se encuantra con una pared, la función se llama recursivamente hasta hallarla, construyendo el laberindo de lo más alejado al principio.

	* Ejemplo: `reportarParedAbierta (Trifurcacion (Trifurcacion Nothing Nothing Nothing) Nothing Nothing) "idr"`
		
		* Primera recursión: recorrerLaberinto con "i" retorna (Trifurcacion Nothing Nothing Nothing) diferente del laberinto inicial. Unir el laberinto inicial con el recorrerLaberinto del obtenido y el tail del string.
		
		* Segunda recursión: recorrerLaberinto (Trifurcacion Nothing Nothing Nothing) con "d" retorna el mismo laberinto. Por lo que evaluo si el string es de largo 1. Como no lo es, tengo que contruir un laberinto con el tail del string actual "r", y anexarlo al Laberinto. Creamos `(Trifurcacion Nothing (Trifurcacion Nothing Nothing Nothing) Nothing)` y luego anexandolo por medio de "d" al inicial es `(Trifurcacion Nothing Nothing (Trifurcacion Nothing (Trifurcacion Nothing Nothing Nothing) Nothing) )`.
		
		* Retorno segunda recursión: anexar lo obtenido con el laberinto de la primera recursión.
		
		Resultado: `(Trifurcacion (Trifurcacion Nothing Nothing (Trifurcacion Nothing (Trifurcacion Nothing Nothing Nothing) Nothing)) Nothing Nothing)`
	
	* NOTA: Es importante señalar que una vez recorrido el laberinto, no puede devoverse, se queda en el punto indicado. Esto es debido a la interpretación propia del equipo de trabajo del proyecto dado.

3. `reportarDerrumbe`
	
	Recorre el laberinto hasta acabar con los caracteres del string de ruta. Una vez al final de la ruta, retorna la trifurcación actual con la dirección indicada por el usuario en `Nothing`. 
	
	* NOTA: Es importante señalar que una vez recorrido el laberinto, no puede devoverse, se queda en el punto indicado. Esto es debido a la interpretación propia del equipo de trabajo del proyecto dado.

4. `tomarTesoro`
	
	Recorre el laberinto de la misma manera que lo hace la función `construirLaberinto` y `reportarParedAbierta`. Se llama a sí misma de manera recursiva y va anexando los resultados.
	
	* Ejemplo: `tomarTesoro (Trifurcacion Nothing Nothing (Trifurcacion (Tesoro "ah" (Tesoro "otro" Nothing)) Nothing Nothing)) "di" (Tesoro "ah" Nothing)`
		
		* Primera vez: Nota que si se gira a la derecha con "d", no se encuentra el tesoro, por lo cual anexa el resultado de tomarTesoro con lo que se obtiene al girar a la derecha.
		
		* Segunda vez: Nota que si gira a la izquiera con "i" se encuentra el tesoro, por lo cual retorna la trifurcación quitando el tesoro de por medio y uniendo lo que está más allá de él. Así: `(Trifurcación (Tesoro "otro" Nothing) Nothing Nothing)`
		
		* Retorno a la primera: anexa el resultado a lo correspondiente.
		
		Resultado: `(Trifurcacion Nothing Nothing (Trifurcación (Tesoro "otro" Nothing))`

5. `hallarTesoro`
	
	Funciona de manera sililar a `tomarTesoro`, sólo que en vez de un `Tesoro` recibe un `String` para instanciar un tesoro e insertarlo en el laberinto. Se llama a sí misma de manera recursiva y va anexando los resultados.
	
	* Ejemplo: `hallarTesoro (Trifurcacion Nothing Nothing (Trifurcación (Tesoro "otro" Nothing)) "di" "ah"`
		
		* Primera vez: la ruta no es vacía ni cuando se gira a la derecha da el mismo laberinto (esto ocurre cuando es un movimiento inválido), por lo que se anexa al laberinto en esa dirección lo que se obtiene de llamar nuevamente la función sobre el laberinto cuando se gira a la derecha. Esto es, se llama sobre `(Trifurcación (Tesoro "otro" Nothing))`.
		
		* Segunda vez: Nota que si gira a la izquiera con "i" aún no cumple las condiciones de arriba (largo de ruta 0 o laberinto que viene igual a actual) por lo que se llama nuevamente a la función, ahora sobre `(Tesoro "otro" Nothing)`.
		
		* Tercera vez: El largo de la ruta es 0, por lo que crea el Tesoro con el string dado y une lo que quedaba del laberinto al mismo. Por lo que quedaría `(Tesoro "ah" (Tesoro "otro" Nothing)`
		
		* Retorno a la segunda: anexa el resultado a lo correspondiente y queda: `(Trifurcacion (Tesoro "ah" (Tesoro "otro" Nothing)) Nothing Nothing)`
		
		* Retorno a la primera: anexa el resultado a lo correspondiente.
		
		Resultado: `(Trifurcacion Nothing Nothing (Trifurcacion (Tesoro "ah" (Tesoro "otro" Nothing)) Nothing Nothing))`



### ACERCA DEL CLIENTE

Para representar las opciones disponibles, se definió la función `opcionesDisponibles`, la cual cumple el papel de diccionario, relacionando un número con una opción. Las mismas son impresas por medio de `printMenu`.

Con el objeto de separar la ejecución del programa del main, se implementó la función `mostrarRecibirOpciones`, que pediría una instrucción al usuario, un número relacionado en el diccionario anteriormente mencionado, y redireccionaría a la función correspondiente a esa decisión. El flujo interno está diseñado de tal manera que, al concluir la operación elegida, el usuario sea nuevamente redirigido a `mostarRecibirOpciones`.

La mayor parte de las funciones involucradas en el flujo del programa reciben el laberinto en Memoria o `laberintoEnMem`, a manera de mantener y actualizar la instancia según sea indicado. Asimismo, se valen de las funciones definidas en Laberinto.hs para conseguir los valores necesarios y poder operar correctamente.
