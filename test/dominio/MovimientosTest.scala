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
import movimientos.DejarseFajar
import movimientos.CargarKi
import items.SemillaDelErmitanio
import movimientos.Explotar
import items.ArmaDeFuego
import items.Municion
import estados.Estado
import movimientos.UsarItem
import movimientos.GolpesNinja
import com.sun.scenario.effect.InvertMask
import especie.Humano

class MovimientosTest {
  var goku: Guerrero = null
  var movimientosGoku: Set[Movimiento] = Set(new DejarseFajar, new CargarKi, new GolpesNinja)
  var itemsGoku: Set[Item] = Set(SemillaDelErmitanio())

  var vegeta: Guerrero = null
  var movimientosVegeta: Set[Movimiento] = Set(new Explotar, new CargarKi, new GolpesNinja)
  var itemsVegeta: Set[Item] = Set(SemillaDelErmitanio(), ArmaDeFuego(), Municion(new ArmaDeFuego, 3))

  @Before
  def setup() = {
    goku = new Guerrero("goku", 110, 150, movimientosGoku, itemsGoku, Saiyajin(true))
    vegeta = new Guerrero("vegeta", 100, 150, movimientosVegeta, itemsVegeta, Saiyajin(false))
  }

  @Test
  def gokuGolpeaAVegeta() = {
    val estadoGokuGolpeaAVegeta = GolpesNinja()(Estado(goku, vegeta))
    assertEquals(vegeta.ki - 20, estadoGokuGolpeaAVegeta.defensor.ki)
  }

  @Test
  def gokuGolpeaAVegetaYEsteComeUnaSemillaDelErmitanio() = {

    val estadoGolpe = GolpesNinja()(Estado(goku, vegeta))
    estadoGolpe.invertirGuerreros()
    
    val estadoVegetaComeSemillaDespuesDelGolpe = UsarItem(SemillaDelErmitanio())(estadoGolpe)

    val itemsVegetaSinSemilla = Set(ArmaDeFuego(), Municion(new ArmaDeFuego, 3))

    assertEquals(vegeta.kiMaximo, estadoVegetaComeSemillaDespuesDelGolpe.atacante.ki)
    assertEquals(itemsVegetaSinSemilla, estadoVegetaComeSemillaDespuesDelGolpe.atacante.items)
  }

  @Test
  def noPasaNadaPorqueGokuNoSabeMovimientoYVegetaNoPuedeComerSemillaPorqueNoTiene() = {
    goku = new Guerrero("goku", 110, 150, Set(), itemsGoku, Saiyajin(true))
    vegeta = new Guerrero("vegeta", 100, 150, movimientosGoku, Set(), Saiyajin(false))

    val estadoInicial = Explotar()(Estado(goku, vegeta))
    estadoInicial.invertirGuerreros()
    
    val estadoFinal = UsarItem(SemillaDelErmitanio())(estadoInicial)

    assertEquals(estadoFinal, estadoInicial)
  }

  /*esto va a romper cuando se implemente el contraataque porque vegeta contraatacaria a goku
   * cambiando su estado*/
  @Test
  def usoDeMovimientoDesdeGuerrero() = {

    goku = Guerrero("goku", 100, 200, Set(CargarKi()), Set(), Saiyajin(true))

    val estado = goku.hacerMovimientoA(vegeta, CargarKi())

    assertEquals(goku.ki + 100, estado.atacante.ki)
  }

  
  @Test
  def unHumanoDisparaAOtroHumano() = {
    val pepe = Guerrero("pepe", 50, 100, Set(), Set(ArmaDeFuego(), Municion(ArmaDeFuego(), 2)), Humano())
    val moni = Guerrero("moni", 80, 80, Set(), Set(), Humano())
    
    val primerDisparo = UsarItem(ArmaDeFuego())(Estado(pepe, moni))
    val segundoDisparo = UsarItem(ArmaDeFuego())(Estado(primerDisparo.atacante, primerDisparo.defensor))
    
    assertEquals(moni.ki - 20, primerDisparo.defensor.ki)
    assertEquals(Set(ArmaDeFuego(), Municion(ArmaDeFuego(), 1)), primerDisparo.atacante.items)
    
    assertEquals(moni.ki - 40, segundoDisparo.defensor.ki)
    assertEquals(Set(ArmaDeFuego()), segundoDisparo.atacante.items)
    
  }
  
  @Test
  def pepeMataAMoni() = {
    val pepe = Guerrero("pepe", 50, 100, Set(), Set(ArmaDeFuego(), Municion(ArmaDeFuego(), 3)), Humano())
    val moni = Guerrero("moni", 10, 80, Set(), Set(), Humano())
    
    val estadoMoniMuere = UsarItem(ArmaDeFuego()) (Estado(pepe, moni))
    val tiroPorLasDudasAlPedo = UsarItem(ArmaDeFuego()) (estadoMoniMuere)
    
    assertEquals(estadoMoniMuere.alguienMurio(), true)
    assertEquals(estadoMoniMuere.quienMurio().nombre, moni.nombre)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}