package movimientos

import estados.Estado
import guerreros.Guerrero
import items.SemillaDelErmitanio
import guerreros.SuperSaiyajin
import guerreros.Andriode

class CargarKi() extends Movimiento() {

  def cumpleCondiciones(estado: Estado): Boolean = {
    estado.atacante match {
      case _ => true
    }
  }

  def afectarAlAtacante(atacante: Guerrero): Unit = {
    atacante match {
      case _: SuperSaiyajin => aumentarKiASuperSaiyajin(atacante.asInstanceOf[SuperSaiyajin])
      case _: Andriode      => 
      case _                => aumentarKiAUnGuerreroCualquiera(atacante)
    }
  }

  def afectarAlDefensor(defensor: Guerrero): Unit = {
    //no le hace nada al otro
  }

  private def aumentarKiASuperSaiyajin(superSaiyajin: SuperSaiyajin): Unit = {
    superSaiyajin.ki += 150 * superSaiyajin.nivel
  }

  private def aumentarKiAUnGuerreroCualquiera(atacante: Guerrero): Unit = {
    atacante.ki += 100
  }
}


