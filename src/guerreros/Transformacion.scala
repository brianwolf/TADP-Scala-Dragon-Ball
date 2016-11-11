package guerreros

import movimientos.Movimiento
import especie.Especie
import condiciones.Condicion

class Transformacion(var anterior: Guerrero)
    extends Guerrero(anterior.nombre, anterior.ki, anterior.kiMaximo, anterior.movimientos, anterior.items, anterior.especie) {
}