package guerreros

import movimientos.Movimiento
import items.Item
import potenciadores.Potenciador

class SuperSaiyajin(
  ki: Int,
  movimientos: Set[Movimiento],
  items: Set[Enumeration],
  potenciadores: Set[Potenciador],
  var nivel: Int)
    extends Guerrero(ki, movimientos, items, potenciadores) {
}