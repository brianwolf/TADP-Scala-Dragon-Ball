package guerreros
import movimientos._;
import items._;
import potenciadores.Potenciador
import estados.Estado

case class Guerrero(
    var ki: Int,
    var movimientos: Set[Movimiento],
    var items: Set[Enumeration],
    var potenciadores: Set[Potenciador]) {

  def contraAtaca(estado: Estado, movimiento: Movimiento): Estado = {
    //ver como hacer la inteligencia para saber que movimiento le hace a quien lo ataco
    estado
  }
}

/*los potenciadores son para que le pasemos los movimientos para ver si los afectan en algo
*esto es para la genkidama principalmente, porque necesitamos la cantidad de veces que 
* el gerrero uso dejarseFajar, la idea seria que al usar dejarseFajar se cree un potenciador 
* potenciadorDejarseFajar donde le pasemos el movimiento que haga el guerrero, si es dejarseFajar,
* aumenta un contador que tiene que tener, si no lo es, se elimina a si mismo de la lista,
* cuando el guerrero use genkidama, tendria que ver si esta ese potenciador, y si esta, que lo use para
* potenciar la fuerza como dice el enunciado
* 
* no se me ocurrio otra cosa :/ 
*/