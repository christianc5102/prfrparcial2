def procesarRango(
  desde: Int,
  hasta: Int,
  transformar: Int => Int,
  combinar: (Int, Int) => Int,
  valorInicial: Int
): Int = {
  if (desde>hasta) then valorInicial
  else combinar(transformar(desde), procesarRango(desde+1, hasta, transformar, combinar, valorInicial))
}

def buscar(
  desde: Int,
  hasta: Int,
  condicion: Int => Boolean
): Int = {
  if (desde > hasta) then -1
  else if (condicion(desde)) then desde
  else buscar(desde + 1, hasta, condicion)
}

def contarDivisores(n: Int): Int = {
  procesarRango(1, n, x => if (n%x == 0) then 1 else 0, (a,b) => a + b, 0)
}

def maximaCantidadDivisores(desde: Int, hasta: Int): Int = {
  procesarRango(desde, hasta, contarDivisores, (a,b) => if (a>b) then a else b, 0)
}

def esPrimo(n: Int): Boolean = {
  if (n<=1) false
  else contarDivisores(n) == 2
}

def primoMasBajo(desde: Int, hasta: Int): Int = {
  buscar(desde, hasta, esPrimo)
}

def main (args: Array[String]) = {
    println(maximaCantidadDivisores(5,10))
    println(maximaCantidadDivisores(22,26))
    println(primoMasBajo(6,11))
    println(primoMasBajo(24,28))
}