Integrantes:
    Aurivan Castro  14-10205
    Sandra Vera     14-11130




======== INTRODUCCIÓN ============>>>

El presente archivo tiene como propósito explicar detalles de la implementación del Proyecto correspondiente,  que no pudieron ser incluidos en los comentarios Haddock.

El texto se divide en secciones de acuerdo a la organización del cliente en cuanto a algunas funciones en Laberinto.hs y algunas las opciones para el usuario que requieren explicación extensiva.




======== ACERCA DEL LABERINTO ======>>>

En Laberinto.hs se encuentra tanto la definición de la data Laberinto, como las funciones para manejar instancias de esta clase. En las siguientes secciones sólo son mencionadas funciones que requieran mayor información para su mejor entendimiento.


*** DE LAS FUNCIONES DE ACCESO

1) "agregarLaberinto"
	Esta función agrega a un laberinto a partir de un string. En este sentido, si dicho string contiene un caracter inválido, esto es, algo diferente a 'd', 'i', 'r' para los Laberintos Trifurcacion y 'r' para los Laberintos Tesoro; entonces el mismo es "ignorado" haciendo que la función retorne el mismo laberinto que fue recibido.


*** DE LAS FUNCIONES DE ACCESO

1) "voltearIzquierda", "voltearDerecha", "irRecto"
	En caso de ser una operación inválida, no se realiza ninguna de las operaciones. Por ejemplo, si se está en un Tesoro donde sólo se puede seguir recto, la función "voltearIzquierda" retornará el mismo laberinto que recibió; lo mismo para cuando se quiere girar a la izquiera en una trifurcación donde a la izquiera hay Nothing.

2) "recorrerLaberinto"
	Si el string pasa un caracter no válido como los mencionados en "agregarLaberinto", se pasa al siguiente caracter del string, hasta que el string esté vacío, en cuyo caso retorna el mismo laberinto que recibió.


*** DE LAS FUNCIONES AUXILIARES

1) "evaluarLaberinto"
	Indica si el laberinto que ha recibido es un Camino sin Salida ('S'), una trifurcación que no es camino sin salida ('T') o un Tesoro ('E').

2) "reportarParedAbierta"
	Esta función recorre el laberinto hasta el momento a encontrarse con una pared. Se reconoce cuando el laberinto a recorrer para la siguiente letra del string de la ruta devuelve el mismo laberinto (por las razones indicadas arriba para "recorrerLaberinto", también se realiza una validación más para segurar que el primer caracter es un movimiento válido). En este caso, debe evaluarse si el string es de al menos una letra, en cuyo caso basta con anexar al laberinto un camino sin salida, o en caso contrario, se debe crear un laberinto con el string restante y luego anexarlo al laberinto principal por medio de ese primer caracter evaluado antes. Si no se encuantra con una pared, la función se llama recursivamente hasta hallarla, construyendo el laberindo de lo más alejado al principio.
	++Ejemplo: reportarParedAbierta (Trifurcacion (Trifurcacion Nothing Nothing Nothing) Nothing Nothing) "idr"
		--Primera recursión: recorrerLaberinto con "i" retorna (Trifurcacion Nothing Nothing Nothing) diferente del laberinto inicial. Unir el laberinto inicial con el recorrerLaberinto del obtenido y el tail del string.
		--Segunda recursión: recorrerLaberinto (Trifurcacion Nothing Nothing Nothing) con "d" retorna el mismo laberinto. Por lo que evaluo si el string es de largo 1. Como no lo es, tengo que contruir un laberinto con el tail del string actual "r", y anexarlo al Laberinto. Creamos (Trifurcacion Nothing (Trifurcacion Nothing Nothing Nothing) Nothing) y luego anexandolo por medio de "d" al inicial es (Trifurcacion Nothing Nothing (Trifurcacion Nothing (Trifurcacion Nothing Nothing Nothing) Nothing) ).
		--Retorno segunda recursión: anexar lo obtenido con el laberinto de la primera recursión. Resultado: (Trifurcacion (Trifurcacion Nothing Nothing (Trifurcacion Nothing (Trifurcacion Nothing Nothing Nothing) Nothing)) Nothing Nothing)
	++NOTA: Es importante señalar que una vez recorrido el laberinto, no puede devoverse, se queda en el punto indicado. Esto es debido a la interpretación propia del equipo de trabajo del proyecto dado.

3) "reportarDerrumbe"
	Recorre el laberinto hasta acabar con los caracteres del string de ruta. Una vez al final de la ruta, retorna la trifurcación actual con la dirección indicada por el usuario en "Nothing". 
	++NOTA: Es importante señalar que una vez recorrido el laberinto, no puede devoverse, se queda en el punto indicado. Esto es debido a la interpretación propia del equipo de trabajo del proyecto dado.





======== ACERCA DEL CLIENTE ======>>>

Para representar las opciones disponibles, se definió la función "opcionesDisponibles", la cual cumple el papel de diccionario, relacionando un número con una opción. Las mismas son impresas por medio de "printMenu".

Con el objeto de separar la ejecución del programa del main, se implementó la función "mostrarRecibirOpciones", que pediría una instrucción al usuario, un número relacionado en el diccionario anteriormente mencionado, y redireccionaría a la función correspondiente a esa decisión. El flujo interno está diseñado de tal manera que, al concluir la operación elegida, el usuario sea nuevamente redirigido a "mostarRecibirOpciones".

La mayor parte de las funciones involucradas en el flujo del programa reciben el laberinto en Memoria o "laberintoEnMem", a manera de mantener y actualizar la instancia según sea indicado. Asimismo, se valen de las funciones definidas en Laberinto.hs para conseguir los valores necesarios para operar correctamente.