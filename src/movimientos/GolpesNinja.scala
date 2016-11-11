package movimientos

import estados.Estado
import especie.Humano
import especie.Androide

case class GolpesNinja() extends Movimiento {
  
  def afectarAlAtacante(estado: Estado): Unit = {
  }

  def afectarAlDefensor(estado: Estado): Unit = {
    
    if (estado.atacante.especie == Humano() && estado.defensor.especie == Androide()) {
      estado.atacante.reducirKi(10)
      return
    }
    
    if(estado.atacante.ki > estado.defensor.ki) {
      estado.defensor.reducirKi(20)
    }
    else{
      estado.atacante.reducirKi(20)
    }
  }

  def cumpleCondiciones(estado: Estado): Boolean = {
    true
  }
}