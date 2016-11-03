package movimientos

import scala.util.Try
import guerreros.Guerrero
import estados.Estado
import guerreros.Humano

abstract case class Movimiento() {

  def apply(estado: Estado): Estado = {
    var estadoResultado: Estado = estado.copy()
    
    if (cumpleCondiciones(estado)) {
      afectarAlAtacante(estado.atacante)
      afectarAlDefensor(estado.defensor)
    }
    
    estadoResultado.defensor.contraAtaca(estadoResultado, this)
  }

  /*estos metodos los tienen que sobreescribir todos los que hereden
   * para agregarles su respectiva funcionalidad*/
  def cumpleCondiciones(estado: Estado): Boolean
  def afectarAlAtacante(atacante: Guerrero): Unit
  def afectarAlDefensor(defensor: Guerrero): Unit
}