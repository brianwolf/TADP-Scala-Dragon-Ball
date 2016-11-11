package items

import estados.Estado
import especie.Humano
import especie.Namekusein
import condiciones.Inconciente

case class ArmaDeFuego() extends Item {

  def afectarAlAtacante(estado: Estado): Unit = {

    var municion = estado.atacante.items.find {x => 
      x.isInstanceOf[Municion] && 
      x.asInstanceOf[Municion].puedoUsarteConEstaArma(this)
      }.get.asInstanceOf[Municion].copy()
    
    estado.atacante.items -= municion
    
    municion.cantidad-=1
    
    if (municion.cantidad > 0) estado.atacante.items += municion
  }

  def afectarAlDefensor(estado: Estado): Unit = {
    estado.defensor.especie match {
      case Humano()      => estado.defensor.reducirKi(20)
      case Namekusein() => if (estado.defensor.condicion == Inconciente()) estado.defensor.reducirKi(10)
      case _             =>
    }
  }

  def cumpleCondiciones(estado: Estado): Boolean = {
    estado.atacante.items
      .filter { x => x.isInstanceOf[Municion] }
      .exists { x => x.asInstanceOf[Municion].puedoUsarteConEstaArma(this) }
  }
}