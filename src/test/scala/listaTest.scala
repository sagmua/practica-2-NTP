import org.scalacheck.Properties
import org.scalacheck.Gen._
import org.scalacheck.Prop.{forAll, throws, AnyOperators}



object listaTest extends Properties("Test"){
  //Método de generacion de listas de valores enteros:
  val secuenciaEnteros = listOfN(10, choose(0,10))


  println(secuenciaEnteros.sample)

  property("longitud de lista") =
    forAll(secuenciaEnteros) {
      xs =>{
        val lista : Lista[Int] = Lista(xs : _*)
        val longitudList = xs.length
        val longitudLista = Lista.longitud(lista)
        //hacemos lo mismo que con == solo que miestra info de depuracion:
        longitudList ?= longitudLista
      }
    }


  property ("suma de Enteros") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)
        val sumaList = xs.map(x => x.toDouble).sum
        val sumaLista = Lista.sumaEnteros(lista)

        //se comprueba la igualdad:
        sumaList ?= sumaLista
      }
    }

  property ("multiplicacion de Enteros") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)
        val productoList = xs.map(x => x.toDouble).product
        val productoLista = Lista.productoEnteros(lista)

        //se comprueba la igualdad:
        productoList ?= productoLista
      }
    }


  property ("Concatenacion enteros") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)
        //val productoList = xs.map(x => x.toDouble).product

        val concatList = xs:::xs

        val contatLista = Lista.concatenar(lista, lista)

        //val productoLista = Lista.productoEnteros(lista)

        //se comprueba la igualdad:
        concatList ?= Lista.toList(contatLista)
      }
    }


  property ("suma rightFold") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)
        val sumaList = xs.map(x => x.toDouble).sum
        val sumaLista = Lista.sumaFoldRight(lista)

        //se comprueba la igualdad:
        sumaList ?= sumaLista
      }
    }



  property ("multiplicacion rightFold") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)
        val productoList = xs.map(x => x.toDouble).product
        val productoLista = Lista.productoFoldRight(lista)

        //se comprueba la igualdad:
        productoList ?= productoLista
      }
    }



  property ("Cambio cabeza") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)



        //Cambiamos la cabeza por 8:
        val cabezaNuevaList = xs match{
          case List()   => List(8)
          case a::c => 8::c
        }


        //ahora utilizamos la funcion de Lista:
        val cabezaNuevaLista = Lista.asignarCabeza(lista, 8)

        //se comprueba la igualdad:
        cabezaNuevaList ?= Lista.toList(cabezaNuevaLista)
      }
    }


  property ("Eliminar cabeza") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)



        //Cambiamos la cabeza por 8:
        val cabezaNuevaList = xs match{
          case List()   => xs
          case a::c => xs.tail
        }


        //ahora utilizamos la funcion de Lista:
        val cabezaNuevaLista = Lista.tail(lista)

        //se comprueba la igualdad:
        cabezaNuevaList ?= Lista.toList(cabezaNuevaLista)
      }
    }


  property ("Eliminar n elementos") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)



        //Cambiamos la cabeza por 8:
        val cabezaNuevaList = xs match{
          case List()   => xs
          case a::c => xs.drop(3)
        }


        //ahora utilizamos la funcion de Lista:
        val cabezaNuevaLista = Lista.eliminar(lista, 3)

        //se comprueba la igualdad:
        cabezaNuevaList ?= Lista.toList(cabezaNuevaLista)
      }
    }


  property ("Eliminar ultimo elemento de la lista") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)



        //Cambiamos la cabeza por 8:
        val ultimoList = xs match{
          case List()   => xs
          case a::c => xs.dropRight(1)
        }


        //ahora utilizamos la funcion de Lista:
        val ultimoLista = Lista.eliminarUltimo(lista)

        //se comprueba la igualdad:
        ultimoList ?= Lista.toList(ultimoLista)
      }
    }


  property ("Filter numeros pares") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)



        //Cambiamos la cabeza por 8:
        val paresList = xs match{
          case List()   => xs
          case a::c => xs.filter(_ % 2 != 0)
        }

        def pares (x:Int):Boolean = x%2 == 0
        //ahora utilizamos la funcion de Lista:
        val paresLista = Lista.filter(lista, pares)

        
        //se comprueba la igualdad:
        paresList ?= Lista.toList(paresLista)
      }
    }


  property ("eliminarMientras numeros pares") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)


        def par (x:Int):Boolean = x%2 == 0

        val paresList = xs.dropWhile(par)

 
        val paresLista = Lista.eliminarMientras(lista, par)

        //se comprueba la igualdad:
        paresList ?= Lista.toList(paresLista)
      }
    }





}
