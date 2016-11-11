package movimientos

import estados.Estado
import guerreros.Guerrero
import especie.Saiyajin
import especie.Androide

case class CargarKi() extends Movimiento() {

  def cumpleCondiciones(estado: Estado): Boolean = {
    estado.atacante.especie match {
      case _: Androide => false
      case _           => true
    }
  }

  def afectarAlAtacante(estado: Estado): Unit = {
    estado.atacante.aumentarKi(100)
  }

  def afectarAlDefensor(estado: Estado): Unit = {
  }

}


