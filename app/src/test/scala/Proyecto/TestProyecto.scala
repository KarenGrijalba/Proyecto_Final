/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package Proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestProyecto extends AnyFunSuite{
    test("Prueba 1: Reconstruir cadena ingenuo Parallel") {
        type Oraculo = Seq[Char] => Boolean
        val n = 3
        val o: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'g')
        val resultado = Proyecto.reconstruirCadenaParalelo(n, o)
        assert(resultado == Seq('a', 'c', 'g'))
    }
    test("Prueba 2: Reconstruir cadena ingenuo Parallel") {
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => s == Seq('a', 'a', 'c', 'g')
        val resultado = Proyecto.reconstruirCadenaParalelo(n, o)
        assert(resultado == Seq('a', 'a', 'c', 'g'))
    }
    test("Prueba 3: Reconstruir cadena ingenuo Parallel") {
        type Oraculo = Seq[Char] => Boolean
        val n = 5
        val o: Oraculo = (s: Seq[Char]) => s == Seq('a', 't', 'c', 'g', 't')
        val resultado = Proyecto.reconstruirCadenaParalelo(n, o)
        assert(resultado == Seq('a', 't', 'c', 'g', 't'))
    }
    test("Prueba 4: Reconstruir cadena ingenuo Parallel") {
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => s == Seq('a', 't', 'c', 'g')
        val resultado = Proyecto.reconstruirCadenaParalelo(n, o)
        assert(resultado == Seq('a', 't', 'c', 'g'))
    }


    test("Prueba 1: Reconstruir cadena ingenuo"){
        type Oraculo = Seq[Char] => Boolean
        val n = 3
        val o: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'g')
        val resultado = Proyecto.reconstruirCadenaIngenuo(n, o)
        //println(resultado)
        assert(resultado == Seq('a', 'c', 'g'))
    }
    test("Prueba 2: Reconstruir cadena ingenuo"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => s == Seq('a', 'a', 'c', 'g')
        val resultado = Proyecto.reconstruirCadenaIngenuo(n, o)
        assert(resultado == Seq('a', 'a', 'c', 'g'))
    }
    test("Prueba 3: Reconstruir cadena ingenuo"){
        type Oraculo = Seq[Char] => Boolean
        val n = 5
        val o: Oraculo = (s: Seq[Char]) => s == Seq('a', 't', 'c', 'g', 't')
        val resultado = Proyecto.reconstruirCadenaIngenuo(n, o)
        assert(resultado == Seq('a', 't', 'c', 'g', 't'))
    }
    test("Prueba 4: Reconstruir cadena ingenuo") {
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => s == Seq('a', 't', 'c', 'g')
        val resultado = Proyecto.reconstruirCadenaIngenuo(n, o)
        assert(resultado == Seq('a', 't', 'c', 'g'))
    }
    /*
    test("Prueba 4: Reconstruir cadena "){
        type Oraculo = Seq[Char] => Boolean

        //val o: Oraculo = (s: Seq[Char]) => s.forall(Seq('a', 'c', 'g').contains)
        val o: Oraculo = (s: Seq[Char]) => Seq('a', 'c', 'g').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadena(o)
        println("Resultado: " + resultado)
        assert(true)
    }*/
    test("Prueba 1: Reconstruir cadena mejorado"){
        type Oraculo = Seq[Char] => Boolean
        val n = 3
        val o: Oraculo = (s: Seq[Char]) => Seq('a', 'c', 'g').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaMejorado(n, o)
        assert(resultado == Seq('a', 'c', 'g'))
    }
    test("Prueba 2: Reconstruir cadena mejorado"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => Seq('a', 'a', 'c', 'g').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaMejorado(n, o)
        assert(resultado == Seq('a', 'a', 'c', 'g'))
    }
    test("Prueba 3: Reconstruir cadena mejorado"){
        type Oraculo = Seq[Char] => Boolean
        val n = 5
        val o: Oraculo = (s: Seq[Char]) => Seq('a', 't', 'c', 'g', 't').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaMejorado(n, o)
        assert(resultado == Seq('a', 't', 'c', 'g', 't'))
    }
    test("Prueba 4: Reconstruir cadena mejorado"){
        type Oraculo = Seq[Char] => Boolean
        val n = 5
        val o: Oraculo = (s: Seq[Char]) => Seq('a', 'g', 'c', 'a', 't').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaMejorado(n, o)
        assert(resultado == Seq('a', 'g', 'c', 'a', 't'))
    }
    test("Prueba 1: Reconstruir cadena turbo"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => Seq('a', 'c', 'g', 'c').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaTurbo(n, o)
        assert(resultado == Seq('a', 'c', 'g', 'c'))
    }
    test("Prueba 2: Reconstruir cadena turbo"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => Seq('c', 'g', 'c', 't').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaTurbo(n, o)
        assert(resultado == Seq('c', 'g', 'c', 't'))
    }
    test("Prueba 3: Reconstruir cadena turbo"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => Seq('g', 'c', 't', 'a').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaTurbo(n, o)
        assert(resultado == Seq('g', 'c', 't', 'a'))
    }
    test("Prueba 4: Reconstruir cadena turbo"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => Seq('t', 'a', 'c', 'g').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaTurbo(n, o)
        assert(resultado == Seq('t', 'a', 'c', 'g'))
    }
    test("Prueba 1: Reconstruir cadena turbo mejorada"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => Seq('a', 'c', 'g', 'c').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaTurboMejorada(n, o)
        assert(resultado == Seq('a', 'c', 'g', 'c'))
    }
    test("Prueba 2: Reconstruir cadena turbo mejorada"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => Seq('c', 'g', 'c', 't').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaTurboMejorada(n, o)
        assert(resultado == Seq('c', 'g', 'c', 't'))
    }
    test("Prueba 3: Reconstruir cadena turbo mejorada"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => Seq('g', 'c', 't', 'a').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaTurboMejorada(n, o)
        assert(resultado == Seq('g', 'c', 't', 'a'))
    }
    test("Prueba 4: Reconstruir cadena turbo mejorada"){
        type Oraculo = Seq[Char] => Boolean
        val n = 4
        val o: Oraculo = (s: Seq[Char]) => Seq('t', 'a', 'c', 'g').mkString.contains(s.mkString)
        val resultado = Proyecto.reconstruirCadenaTurboMejorada(n, o)
        assert(resultado == Seq('t', 'a', 'c', 'g'))
    }

}
