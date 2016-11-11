package movimientos

import guerreros.Guerrero
import estados.Estado

case class DejarseFajar() extends Movimiento {
  
  def afectarAlAtacante(estado: Estado): Unit = {
    
  }

  def afectarAlDefensor(estado: Estado): Unit = {
    
  }

  def cumpleCondiciones(estado: Estado): Boolean = {
    true
  }
}