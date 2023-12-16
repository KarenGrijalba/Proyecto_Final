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

/*
  def reconstruirCadena(o: Oraculo): Seq[Char] = {
    val cadena = Seq('a', 'c')
    def resultado(): Seq[Char] = {
        if (o(cadena)) cadena
        else {
          println("No es igual")
          Seq.empty[Char]
        }
      }
    resultado()
    }*/

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {

    def generarCadenasCandidatas(k: Int, candidatas: Seq[Seq[Char]]): Seq[Char] = {
      //println(candidatas)
      if (k == n) {
        candidatas.head
      } else {
        val nuevasCandidatas = candidatas.flatMap { candidata =>
          alfabeto.flatMap { elemento =>
            if (o(candidata :+ elemento)) Seq(candidata :+ elemento)
            else Seq.empty[Seq[Char]]
          }
        }
        generarCadenasCandidatas(k + 1, nuevasCandidatas)
      }
    }

      def reconstruirCadena(): Seq[Char] = {

        def cadenasCandidatas(): Seq[Char] = {
          val cadenas = alfabeto.map(Seq(_))

          generarCadenasCandidatas(1, cadenas)
        }
        cadenasCandidatas()
      }
    reconstruirCadena()
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    def construirSubcadena(k: Int, subcadena: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) subcadena.head
      else {
        val nuevoSubcadena = subcadena.flatMap { secuencia =>
          subcadena.flatMap { elemento =>
            val nuevaSecuencia = secuencia ++ elemento
            if (o(nuevaSecuencia)) Seq(nuevaSecuencia)
            else Seq.empty[Seq[Char]]
          }
        }
        construirSubcadena(k * 2, nuevoSubcadena)
      }
    }
    val cadenas = alfabeto.map(Seq(_))
    val subcadena = construirSubcadena(1, cadenas)
    subcadena
  }

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    Seq[Char]()
  }
}


