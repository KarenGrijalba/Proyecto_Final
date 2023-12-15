/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer


object Proyecto {

  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {

    def generarCadenas(n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(Seq())
      else
        alfabeto flatMap { elemento =>
          generarCadenas(n - 1) map { combinacion =>
            elemento +: combinacion
          }
        }
    }

    def reconstruirCadena(): Seq[Char] = {
      val cadenas = generarCadenas(n) // Genera todas las cadenas de longitud n
      //println("cadenas: " + cadenas)
      def resultado(): Seq[Char] = {
        cadenas flatMap{ cadena =>
          if (o(cadena)) cadena
          else Seq.empty[Char]
        }
      }
      resultado()
    }

    reconstruirCadena()
  }


}


