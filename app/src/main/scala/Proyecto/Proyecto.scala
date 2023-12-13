/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Proyecto{

  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {

    // Si ninguna cadena única es suficiente, generamos las cadenas de longitud n
    def generarCadenasDeLongitud(actual: Int, cadenaActual: Seq[Char]): Seq[Char] = {
      if (actual == n) {
        if (o(cadenaActual)) cadenaActual
        else Seq.empty[Char]
      } else {
        val posiblesCaracteres = Seq('a', 'c', 'g', 't')
        posiblesCaracteres.foreach { c =>
          val resultado = generarCadenasDeLongitud(actual + 1, cadenaActual :+ c)
          if (resultado.nonEmpty) resultado
        }
         Seq.empty[Char]
      }
    }

    alfabeto.foreach { w =>
      val resultado = generarCadenasDeLongitud(1, Seq(w))
      if (resultado.nonEmpty)  resultado
      if (resultado.nonEmpty)  resultado
    }

    // En caso de que no se encuentre ninguna cadena válida
    Seq.empty[Char]
  }

 }
