package estados

import guerreros.Guerrero

case class Estado(
    var atacante: Guerrero,
    var defensor: Guerrero) {

}

/*habria que ver como se usan bien los try, asi podemos recibir un guerrero o una lista de guerreros,
*estoy puede servir en caso que implementemos combates de muchos contra muchos o algo asi, 
*por lo que puede que tambien alla movimientos que afecten a muchos oponentes
* 
* tambien lo pense como una tupla estato(atacante: Guerrero, defensor: Guerrero)
* pero no se si despues vamos a necesitar agregarle algo al estado, habria que ver que es mejor 
*/