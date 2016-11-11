package dominio

import org.junit.Before
import org.junit.Test
import org.junit.Assert._
import guerreros.Guerrero
import especie.Saiyajin
import condiciones.Normal
import items.Item
import guerreros.SuperSaiyajin
import movimientos.Movimiento

class TransformacionesTest {
  var goku: Guerrero = null
  var movimientosGoku: Set[Movimiento] = null
  var itemsGoku: Set[Item] = null

  @Before
  def setup() = {
    goku = new Guerrero("goku", 100, 100, null, itemsGoku, Saiyajin(true))
  }

  @Test
  def testeoDeSS2() = {

    val gokuSS2 = new SuperSaiyajin(new SuperSaiyajin(goku))

    val kiMaximoGokuSS1 = goku.kiMaximo * 5 * 1
    val kiMaximoGokuSS2 = kiMaximoGokuSS1 * 5 * 2

    assertEquals(2, gokuSS2.nivel)
    assertEquals(kiMaximoGokuSS2, gokuSS2.kiMaximo)
    assertEquals(true, gokuSS2.especie.asInstanceOf[Saiyajin].cola)
    assertEquals(goku, gokuSS2.guerreroBase)
    assertEquals(goku.ki, gokuSS2.ki)
  }
}