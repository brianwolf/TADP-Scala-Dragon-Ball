package items

import estados.Estado

case class Municion(var tipo: ArmaDeFuego, var cantidad: Int) extends Item {
  
  def afectarAlAtacante(estado: Estado): Unit = {
  }

  def afectarAlDefensor(estado: Estado): Unit = {
  }

  def cumpleCondiciones(estado: Estado): Boolean = {
    true
  }
  
  def puedoUsarteConEstaArma(arma: ArmaDeFuego): Boolean = {
    arma.getClass() == tipo.getClass() && cantidad > 0 
  }
}