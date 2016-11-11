package condiciones

sealed trait Condicion

case class Normal() extends Condicion

case class Inconciente() extends Condicion

case class Muerto() extends Condicion