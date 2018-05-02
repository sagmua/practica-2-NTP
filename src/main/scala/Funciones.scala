
object Funciones {


  /**
    * función que genera el triángulo de pascal
    *
    * @param columna nº de la col. del triángulo
    * @param fila nº de la fil. del triángulo
    * @return resultado
    */
  def calcularValorTrianguloPascal(fila: Int, columna: Int): Int = {
    if (columna == 0 || columna == fila) 1
    else calcularValorTrianguloPascal(fila-1, columna-1) + calcularValorTrianguloPascal(fila-1, columna)
  }

  /**
    * Función para el balanceo de parentesis dada una lista de ellos
    *
    * @param cadena lista de caracteres
    * @return si está o no balanceada
    */
  def chequearBalance(cadena: List[Char]): Boolean = {
    @annotation.tailrec
    def go(lista: List[Char], acum: Int): Boolean ={
      if (lista.isEmpty) acum == 0
      else if (acum < 0) false
      else if (lista.head == '(') go(lista.tail, acum+1)
      else if (lista.head == ')') go(lista.tail, acum-1)
      else go(lista.tail,acum)
    }
    go (cadena, 0)
  }


  /**
    * Calcula las diferentes formas de devolver el cambio
    * @param cantidad a devolver
    * @param monedas monedas disponibles
    * @return diferentes formas (en número).
    */
  def contarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int = {
    def cambios(cantidad: Int, monedas: List[Int]): Int = {
      if (cantidad == 0) 1
      else if (monedas.isEmpty) 0
      else if (cantidad < monedas.head) 0
      else {
        var acum = 0
        for (i <- 0 to cantidad by monedas.head){
          acum += cambios(cantidad-i, monedas.tail)
        }
        acum
      }
    }

    val monedasOrdenadas = monedas.sorted
    if (cantidad > 0) cambios(cantidad, monedas)
    else 0

  }

  /**
    * función para la búsqueda binaria
    * @param coleccion lista donde hacer la busqueda
    * @param aBuscar elemento a buscar
    * @param criterio para la comparación entre elementos
    * @tparam A
    * @return posición del elemento a buscar. -1 si no existe.
    */
  def busquedaBinaria[A](coleccion : Array[A], aBuscar: A,
                         criterio : (A,A) => Boolean) : Int = {

    @annotation.tailrec
    def go(coleccion : Array[A], acum: Int): Int = {
      val medio = coleccion.length / 2
      val valorMedio = coleccion(medio)
      if (valorMedio == aBuscar) acum+medio
      else if (coleccion.length == 1) -1
      else {
        if (criterio(valorMedio,aBuscar) == true){
          if (medio+1 == coleccion.length) -1
          else go(coleccion.slice(medio+1,coleccion.length),acum+medio+1)
        }
        else{
          if (medio == 0) -1
          else go(coleccion.slice(0,medio),acum)
        }
      }
    }

    if (coleccion.length == 0) -1
    else go(coleccion,0)
  }
}