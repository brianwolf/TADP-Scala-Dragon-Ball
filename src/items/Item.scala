package items

abstract case class Item(){
}

class Arma extends Item{ 
}

class Filosa extends Arma{
}

class Roma extends Arma{
}

class SemillaDelErmitanio extends Item{
}

/*hay que ver si conviene usar enums, aca se llaman enumeration,
 *no me salio usarlos*/
