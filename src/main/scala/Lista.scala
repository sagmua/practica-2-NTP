/**
  * sealed = clase sellada
  * trait = interfaz
  * @tparam A
  */
sealed trait Lista[+A]

/**
  * declaracion de lista vacia
  */
case object Nil extends Lista[Nothing]

/**
  * lista con elementos
  */
case class Constructor[+A](cabeza : A, cola : Lista[A]) extends Lista[A]


/**
  * definimos los métodos como estáticos
  */
object Lista{
  /**
    * Metodo para permitir crear listas sin usar new (hecho en clase)
    * @param elementos secuencia de elementos a incluir en la lista
    * @tparam A
    * @return
    */
  def apply[A](elementos : A*): Lista[A] = {
    if(elementos.isEmpty)
      Nil
    else
      Constructor(elementos.head, apply(elementos.tail : _*))
  }



  /**
    * Obtiene la longitud de una lista
    * @param lista
    * @tparam A
    * @return
    */
  def longitud[A](lista:Lista[A]): Int = {
    lista match {
      case Nil => 0
      case Constructor(cadena, cola) => 1 + longitud(cola)
    }
  }

  /**
    * Metodo para sumar los valores de una lista de enteros
    * @param enteros
    * @return
    */
  def sumaEnteros(enteros : Lista[Int]) : Double = {
    enteros match {
      case Nil => 0.0
      case Constructor(cabeza, cola) => cabeza.toDouble + sumaEnteros(cola)
    }
  }

  /**
    * Metodo para multiplicar los valores de una lista de enteros
    * @param enteros
    * @return
    */
  def productoEnteros(enteros: Lista[Int]) : Double = {
    enteros match {
      case Nil => 1
      case Constructor(cabeza, cola) => cabeza.toDouble * productoEnteros(cola)
    }
  }


  /**
    * Metodo para agregar el contenido de dos listas
    * @param lista1
    * @param lista2
    * @tparam A
    * @return
    */
  def concatenar[A](lista1: Lista[A], lista2: Lista[A]): Lista[A] = {
    lista1 match {
      case Nil => lista2
      case Constructor(cabeza, cola) => Constructor(cabeza, concatenar(cola, lista2))
    }
  }




  /**
    * Funcion de utilidad para aplicar una funcion de forma sucesiva a los
    * elementos de la lista con asociatividad por la derecha
    * @param lista
    * @param neutro
    * @param funcion
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRight[A, B](lista : Lista[A], neutro : B)(funcion : (A, B) => B): B = {
    lista match {
      case Nil => neutro
      case Constructor(cabeza, cola) => funcion(cabeza, foldRight(cola, neutro)(funcion))
    }
  }


  /**
    * Funcion de utilidad para aplicar una funcion de forma sucesiva a los
    * elementos de la lista con asociatividad por la derecha
    * @param lista
    * @param neutro
    * @param funcion
    * @tparam A
    * @tparam B
    * @return
    * */
  @annotation.tailrec
  def foldLeft[A, B](lista : Lista[A], neutro : B)(funcion : (A, B) => B): B = {
    lista match {
      case Nil => neutro
      case Constructor(cabeza, cola) => foldLeft(cola, funcion(cabeza, neutro))(funcion)
    }
  }


  /**
    * Suma mediante foldRight
    * @param listaEnteros
    * @return
    */
  def sumaFoldRight(listaEnteros : Lista[Int]) : Double = {
    def suma (a:Int, b:Int):Int = a+b

    foldRight(listaEnteros, 0)(suma)
  }


  /**
    * Producto mediante foldRight
    * @param listaEnteros
    * @return
    */
  def productoFoldRight(listaEnteros : Lista[Int]) : Double = {
    foldRight(listaEnteros, 1.0)((x,y)=>x*y)

  }

  /**
    * funcion para pasar de un objeto Lista a un objeto
    * List definido por Scala.
    * @param lista
    * @tparam A
    * @return
    */

  def toList[A](lista : Lista[A]):List[A] = {
    lista match {
      case Nil => List()
      case Constructor(cabeza, cola) => cabeza::toList(cola)
    }
  }


  /**
    * Reemplaza la cabeza por nuevo valor. Se asume que si la lista esta vacia
    * se devuelve una lista con el nuevo elemento
    *
    * @param lista
    * @param cabezaNueva
    * @tparam A
    * @return
    */
  def asignarCabeza[A](lista : Lista[A], cabezaNueva : A) : Lista[A] = {
    lista match {
      case Nil => Lista(cabezaNueva)
      case Constructor(cabeza, cola) => concatenar(Lista(cabezaNueva), cola)
    }
  }


  /**
    * Elimina el elemento cabeza de la lista
    * @param lista
    * @tparam A
    * @return
    */
  def tail[A](lista : Lista[A]): Lista[A] = {
    lista match {
      case Nil => lista
      case Constructor(cabeza, cola) => cola
    }
  }

/**
  * Elimina los n primeros elementos de una lista
  * @param lista lista con la que trabajar
  * @param n numero de elementos a eliminar
  * @tparam A tipo de datos
  * @return
  */

  def eliminar[A](lista : Lista[A], n: Int) : Lista[A] = {
    if (n == 0)
      lista
    else {
      lista match {
        case Nil => lista
        case Constructor(cabeza, cola) => eliminar(tail(lista), n - 1)
      }
    }
  }

  /**
  * Elimina elementos mientra se cumple la condicion pasada como
  * argumento
  * @param lista lista con la que trabajar
  * @param criterio predicado a considerar para continuar con el borrado
  * @tparam A tipo de datos a usar
  * @return
  */

  def filter[A](lista : Lista[A], criterio: A => Boolean) : Lista[A] = {
    lista match {
      case Nil => lista
      case Constructor(cabeza, cola) => {
        if(criterio(cabeza))
          concatenar(Nil, filter(cola, criterio))
        else
          concatenar(Lista(cabeza), filter(cola, criterio))
      }
    }
  }


  /**
  * Elimina el ultimo elemento de la lista. Aqui no se pueden compartir
  * datos en los objetos y hay que generar una nueva lista copiando
  * datos
  * @param lista lista con la que trabajar
  * @tparam A tipo de datos de la lista
  * @return
  */

  def eliminarUltimo[A](lista : Lista[A]) : Lista[A] = {
    lista match {
      case Nil => lista
      case Constructor(cabeza, cola) => {
        if (longitud(lista) == 1)
          Nil
        else
          concatenar(Lista(cabeza), eliminarUltimo(cola))
      }

    }
  }



}


object Prueba extends App{
  val lista = Lista(1,2,3,4,5,6,7,8)

  println(Lista.toList(Lista.eliminarUltimo(lista)))
  println(Lista.toList(Lista.eliminar(lista, 2)))

  def criterio (x:Int):Boolean = x%2 == 0

  println(Lista.toList(Lista.filter(lista, criterio)))


}
