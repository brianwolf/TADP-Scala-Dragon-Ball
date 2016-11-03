package guerreros

import movimientos.Movimiento
import items.Item
import potenciadores.Potenciador

class Humano(
  ki: Int,
  movimientos: Set[Movimiento],
  items: Set[Enumeration],
  potenciadores: Set[Potenciador])
    extends Guerrero(ki, movimientos, items, potenciadores) {
}