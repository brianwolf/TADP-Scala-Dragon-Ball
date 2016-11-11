package guerreros

import movimientos.Movimiento
import especie.Especie
import condiciones.Condicion

class SuperSaiyajin(anterior: Guerrero) extends Transformacion(anterior) {

  val nivel: Int = {
    anterior match {
      case _: SuperSaiyajin => anterior.asInstanceOf[SuperSaiyajin].nivel + 1
      case _: Guerrero      => 1
    }
  }

  kiMaximo = anterior.kiMaximo * 5 * nivel

  val guerreroBase: Guerrero = {
    anterior match {
      case _: SuperSaiyajin => anterior.asInstanceOf[SuperSaiyajin].guerreroBase
      case _: Guerrero      => anterior
    }
  }
}