package items

import movimientos.Movimiento
import guerreros.Guerrero
import estados.Estado

abstract class Item() extends Movimiento {

  def afectarAlAtacante(estado: Estado): Unit

  def cumpleCondiciones(estado: Estado): Boolean

  def afectarAlDefensor(estado: Estado): Unit
  
  
}