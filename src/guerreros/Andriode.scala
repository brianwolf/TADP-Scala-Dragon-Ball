package guerreros

import movimientos.Movimiento
import items.Item
import potenciadores.Potenciador

class Andriode(
  bateria: Int,
  movimientos: Set[Movimiento],
  items: Set[Enumeration],
  potenciadores: Set[Potenciador])
    extends Guerrero(bateria, movimientos, items, potenciadores) {
}