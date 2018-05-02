import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{forAll, all, AnyOperators}
import org.scalacheck.Gen._

object FuncionesTest extends Properties("FuncionesTest"){

  //triángulo de Pascal:

  val MAXIMO = 20
  val coordenadasExtremos = for {
    fila <- Gen.choose(0,MAXIMO)
    columna <- Gen.oneOf(0, fila)
  } yield (fila, columna)

  property("Elementos en lados del triángulo valen 1") = {
    forAll(coordenadasExtremos) { (i) => {
      val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
      resultado == 1
    }}
  }

  val coordenadasInternas = for {
    fila <- Gen.choose(2,MAXIMO)
    columna <- Gen.choose(1,fila-1)
  } yield (fila, columna)

  property("Test triángulo") = {
    forAll(coordenadasInternas) { (i) => {
      val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
      resultado == Funciones.calcularValorTrianguloPascal(i._1 -1, i._2 -1) +
        Funciones.calcularValorTrianguloPascal(i._1 -1, i._2)
    }}
  }

  //Funcion comprobar paréntesis:

  val strGen = (n: Int) =>
    Gen.listOfN(n, Gen.oneOf('(',')',Gen.alphaChar.sample.get))

  def comprobarParentesis(lista: List[Char]): Boolean = {

    for (i <- 1 to lista.length){
      val sublista = lista.slice(0,i)
      if (sublista.count("("==) < sublista.count(")"==)) return false
    }
    return true
  }

  property("Test paréntesis") = {
    forAll(strGen(10)) { (cadena) => {

      // En este caso la propiedad de que las subcadenas tengan al menos el mismo número de paréntesis
      // de apertura que de cierre sirve para confirmar que la cadena es incorrecta, pero no para
      // detectar si la cadena está bien formada. Por tanto, en la propiedad interpreto que los resultados
      // de ambos métodos sea false o que el de la propiedad sea true.
      (!Funciones.chequearBalance(cadena) == !comprobarParentesis(cadena)) || comprobarParentesis(cadena)
    }}
  }

  //funcion para el cambio de monedas

  property("Test cambio monedas") = {
    val cambio1 = Funciones.contarCambiosPosibles(23, List(1,2,5,10)) == 52
    val cambio2 = Funciones.contarCambiosPosibles(4, List(1,2)) == 3
    val cambio3 = Funciones.contarCambiosPosibles(15, List(20,25)) == 0
    val cambio4 = Funciones.contarCambiosPosibles(8, List(2,4,8)) == 4
    val cambio5 = Funciones.contarCambiosPosibles(0, List(1,2,3,4,5)) == 0
    val cambio6 = Funciones.contarCambiosPosibles(20, List()) == 0

    all (cambio1, cambio2, cambio3, cambio4, cambio5, cambio6)
  }


  //búsqueda binaria

  val numeros = listOf(Gen.choose(-100, 100))

  property("Test búsqueda binaria") = {
    forAll(numeros) { (xs) => {
      val numerosOrdenados = xs.sorted
      val aBuscar = Gen.choose(-100,100).sample.getOrElse(0)
      val index = numerosOrdenados.indexOf(aBuscar)
      val indexBusqBinaria = Funciones.busquedaBinaria[Int](numerosOrdenados.toArray, aBuscar, _ < _)

      (index == indexBusqBinaria) || (numerosOrdenados(index) == numerosOrdenados(indexBusqBinaria))
    }}
  }
}