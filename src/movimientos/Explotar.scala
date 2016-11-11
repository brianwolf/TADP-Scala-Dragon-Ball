package movimientos

import estados.Estado
import especie.Monstruo
import especie.Androide
import especie.Namekusein

case class Explotar() extends Movimiento {

  var danioParaAplicar = 0

  def afectarAlAtacante(estado: Estado): Unit = {

    estado.atacante.especie match {
      case Monstruo() => danioParaAplicar = estado.atacante.ki * 2
      case Androide() => danioParaAplicar = estado.atacante.ki * 3
      case _          =>
    }

    estado.atacante.ki = 0
  }

  def afectarAlDefensor(estado: Estado): Unit = {
    estado.defensor.especie match {

      case Namekusein() =>
        estado.defensor.ki -= danioParaAplicar
        if (estado.defensor.ki <= 0) estado.defensor.ki = 1
        else estado.defensor.reducirKi(danioParaAplicar)

      case _ => estado.defensor.reducirKi(danioParaAplicar)
    }
  }

  def cumpleCondiciones(estado: Estado): Boolean = {
    estado.atacante.especie match {
      case Monstruo() => true
      case Androide() => true
      case _          => false
    }
  }

}