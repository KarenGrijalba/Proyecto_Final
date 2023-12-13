/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestProyecto extends AnyFunSuite{
    test("Prueba 1: Reconstruir cadena ingenuo"){
        type Oraculo = Seq[Char] => Boolean
        val n = 3
        val o: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'g')
        val resultado = Proyecto.reconstruirCadenaIngenuo(n, o)
        assert(resultado == Seq('a', 'c', 'g'))
    }
}
