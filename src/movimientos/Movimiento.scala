package movimientos

import scala.util.Try
import guerreros.Guerrero
import estados.Estado
import especie.Humano
import condiciones.Condicion
import condiciones.Muerto

abstract class Movimiento() {

  def apply(estado: Estado): Estado = {

    if (estado.alguienMurio()) estado

    var estadoResultado = new Estado(estado.atacante.copy(), estado.defensor.copy())

    if (estadoResultado.atacante.sabesEsteMovimiento(this) && cumpleCondiciones(estadoResultado)) {
      afectarAlAtacante(estadoResultado)
      afectarAlDefensor(estadoResultado)
    }

    estadoResultado
  }

  def cumpleCondiciones(estado: Estado): Boolean
  def afectarAlAtacante(estado: Estado): Unit
  def afectarAlDefensor(estado: Estado): Unit
}