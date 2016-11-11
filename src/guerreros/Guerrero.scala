package guerreros
import movimientos._;
import items._;
import estados.Estado
import condiciones.Condicion
import especie.Especie
import movimientos.UsarItem
import condiciones.Muerto
import condiciones.Normal

case class Guerrero(
    var nombre: String,
    var ki: Int,
    var kiMaximo: Int,
    var movimientos: Set[Movimiento],
    var items: Set[Item],
    var especie: Especie) {

  var condicion: Condicion = Normal()
  
  def aumentarKi(cantidad: Int): Unit = {
    ki += cantidad
    if(ki > kiMaximo) ki = kiMaximo
  }

  def reducirKi(cantidad: Int): Unit = {

    ki -= cantidad
    
    if (ki <= 0) {
      ki = 0
      condicion = Muerto()
    }
  }

  def hacerMovimientoA(oponente: Guerrero, movimiento: Movimiento): Estado = {

    var estadoDespuesDelAtaque = movimiento(Estado(this.copy(), oponente.copy()))

    estadoDespuesDelAtaque.defensor.contraAtaca(estadoDespuesDelAtaque, movimiento)
  }

  def contraAtaca(estado: Estado, movimiento: Movimiento): Estado = {
    estado
  }

  def sabesEsteMovimiento(movimiento: Movimiento): Boolean = {
    movimiento match {
      case _: UsarItem => true
      case _           => movimientos.contains(movimiento)
    }

  }
  
  def tenesEsteItem(item: Item): Boolean = {
    items.contains(item)
  }

}
