package estados

import guerreros.Guerrero
import condiciones.Condicion
import condiciones.Muerto

case class Estado(
    var atacante: Guerrero,
    var defensor: Guerrero) {

  def alguienMurio(): Boolean = {
    atacante.condicion == Muerto() || defensor.condicion == Muerto()
  }

  def invertirGuerreros(): Unit = {
    var aux = atacante
    atacante = defensor
    defensor = aux
  }

  def quienMurio(): Guerrero = {
    if (atacante.condicion == Muerto()) atacante
    else defensor
  }

}
