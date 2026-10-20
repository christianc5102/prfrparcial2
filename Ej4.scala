abstract class Servicio(val nombre: String, val precioBase: Double) {
  def calcularPrecioTotal(): Double
}

trait Descuento {
  def porcentajeDescuento: Double
  def aplicarDescuento(monto: Double): Double = {
    monto - (monto * porcentajeDescuento / 100.0)
  }
}

class Catering(
  nombre: String,
  precioBase: Double,
  val cantidadPersonas: Int,
  val precioPorPersona: Double,
  val porcentajeDescuento: Double
) extends Servicio(nombre, precioBase) with Descuento {
    
  def calcularPrecioTotal(): Double = {
    val subtotal = precioBase+(cantidadPersonas*precioPorPersona)
    aplicarDescuento(subtotal)
  }

  def +(extra: Double): Catering = {
    new Catering(nombre, precioBase+extra, cantidadPersonas, precioPorPersona, porcentajeDescuento)
  }
}

object Catering {
  def crear(nombre: String, precioBase: Double, personas: Int, precioPorPersona: Double, descuento: Double): Catering = {
    val personasValido = if (personas > 0) then personas else 1
    val baseValido = if (precioBase > 0) then precioBase else 0.0
    val precioUnitarioValido = if (precioPorPersona > 0) then precioPorPersona else 0.0
    val descuentoValido = if (descuento >= 0 && descuento <= 100) then descuento else 0.0
    new Catering(nombre, baseValido, personasValido, precioUnitarioValido, descuentoValido)
  }
}

class Sonido(
  nombre: String,
  precioBase: Double,
  val horas: Int,
  val incluyeDJ: Boolean
) extends Servicio(nombre, precioBase) {

  def calcularPrecioTotal(): Double = {
    val costoHoras = horas * 40.0
    val extraDJ = if (incluyeDJ) then 80.0 else 0.0
    precioBase + costoHoras + extraDJ
  }

  def +(extra: Double): Sonido = {
    new Sonido(nombre, precioBase + extra, horas, incluyeDJ)
  }
}

object Sonido {
  def crear(nombre: String, precioBase: Double, horas: Int, incluyeDJ: Boolean): Sonido = {
    val horasValido = if (horas>0) then horas else 1
    val baseValido = if (precioBase > 0) then precioBase else 0.0
    new Sonido(nombre, baseValido, horasValido, incluyeDJ)
  }
}

object Main {
  def main (args: Array[String]) = {
    val catering1 = Catering.crear("Catering 1", 199.0, 20, 10.0, 8.0)
    val catering1conExtra = catering1 + 90.0

    val sonido1 = Sonido.crear("Sonido 1", 120.0, 8, true)
    val sonido1conExtra = sonido1 + 35.0

    println(catering1.calcularPrecioTotal())
    println(catering1conExtra.calcularPrecioTotal())
    println(sonido1.calcularPrecioTotal())
    println(sonido1conExtra.calcularPrecioTotal())
  }
}