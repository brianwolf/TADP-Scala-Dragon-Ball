package movimientos

import guerreros.Guerrero
import items.Item
import estados.Estado

case class UsarItem(var item: Item) extends Movimiento {
  
  def afectarAlAtacante(estado: Estado): Unit = {
    item.afectarAlAtacante(estado)
  }

  def afectarAlDefensor(estado: Estado): Unit = {
    item.afectarAlDefensor(estado: Estado)
  }

  def cumpleCondiciones(estado: Estado): Boolean = {
    estado.atacante.tenesEsteItem(item) && item.cumpleCondiciones(estado)
  }
  
}