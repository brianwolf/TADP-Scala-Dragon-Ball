package especie

sealed trait Especie

case class Humano() extends Especie

case class Saiyajin(cola: Boolean) extends Especie

case class Androide() extends Especie

case class Namekusein() extends Especie

case class Monstruo() extends Especie
