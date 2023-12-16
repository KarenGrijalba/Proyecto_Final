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
          if (o(cadena)) cadena // Si la secuencia cumple con la condición, se agrega a la lista
          else Seq.empty[Char] // Si no cumple, se agrega una lista vacía
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

    def generarCadenasCandidatas(k: Int, candidatas: Seq[Seq[Char]]): Seq[Char] = { // Función que genera las cadenas candidatas
      if (k == n) {
        candidatas.head
      } else {
        val nuevasCandidatas = candidatas.flatMap { candidata =>
          alfabeto.flatMap { elemento =>
            if (o(candidata :+ elemento)) Seq(candidata :+ elemento)
            else Seq.empty[Seq[Char]]
          }
        }
        generarCadenasCandidatas(k + 1, nuevasCandidatas) // Se llama recursivamente con las nuevas cadenas candidatas
      }
    }

      def reconstruirCadena(): Seq[Char] = {

        def cadenasCandidatas(): Seq[Char] = {
          val cadenas = alfabeto.map(Seq(_)) // Genera las cadenas iniciales de longitud 1

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

    def filtrar(subCadena: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      subCadena.flatMap(s1 => subCadena.map(s2 => s1 ++ s2)) // Generar todas las subcadenas posibles
        .filter(s => (0 to s.length - k).forall(i => subCadena.exists(subSeq => s.drop(i).take(k) == subSeq)) && o(s)) // Filtrar las subcadenas que no cumplan con la condición
    }

    def construirSubcadena(k: Int, subCadena: Seq[Seq[Char]]): Seq[Char] = {
      if (k > n) subCadena.head
      else {
        val nuevoSubcadena = filtrar(subCadena, k / 2)
        construirSubcadena(k * 2, nuevoSubcadena)
      }
    }
    val cadena = alfabeto.map(Seq(_))
    val subcadena = construirSubcadena(2, cadena)
    subcadena
  }
}


