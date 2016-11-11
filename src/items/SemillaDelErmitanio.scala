package items

import guerreros.Guerrero
import estados.Estado

case class SemillaDelErmitanio() extends Item {
  def afectarAlAtacante(estado: Estado): Unit = {
    estado.atacante.ki = estado.atacante.kiMaximo
    estado.atacante.items -= SemillaDelErmitanio()
  }

  def afectarAlDefensor(estado: Estado): Unit = {
    
  }

  def cumpleCondiciones(estado: Estado): Boolean = {
    true
  }
}