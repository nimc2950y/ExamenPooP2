case class Publicacion(id: Int, autor: String, texto: String, reacciones: List[String])
val publicaciones: List[Publicacion] = List(
  Publicacion(
    1,
    "Ana",
    "¡Nuevo taller de programación funcional la próxima semana!",
    List("like", "love", "like", "wow", "like")
  ),
  Publicacion(
    2,
    "Juan",
    "Recordatorio: mañana es la fecha límite para el proyecto final.",
    List("like", "like", "haha")
  ),
  Publicacion(
    3,
    "María",
    "¿Alguien recomienda buenos recursos para aprender Scala desde cero?",
    List("like", "love", "love", "wow", "haha", "like")
  ),
  Publicacion(
    4,
    "Carlos",
    "Se suspenden las clases de hoy por mantenimiento del sistema.",
    List("angry", "angry", "wow", "like")
  ),
  Publicacion(
    5,
    "Lucía",
    "Compartiendo un repositorio con ejemplos de programación reactiva.",
    List("love", "love", "like", "like", "wow")
  ),
  Publicacion(
    6,
    "Pedro",
    "No entiendo bien la diferencia entre programación declarativa e imperativa.",
    List("like", "haha", "haha")
  ),
  Publicacion(
    7,
    "Sofía",
    "Felicitaciones al equipo que ganó el hackatón de este semestre.",
    List("love", "love", "love", "wow", "like", "like")
  ),
  Publicacion(
    8,
    "Diego",
    "Subí un video explicando cómo usar map, filter y reduce en Scala.",
    List("like", "like", "wow", "wow", "haha")
  ),
  Publicacion(
    9,
    "Elena",
    "Encuesta: ¿prefieren proyectos individuales o en grupo para el curso?",
    List("like", "like", "like", "haha")
  ),
  Publicacion(
    10,
    "Fernando",
    "Actualización: ya está disponible el material de la semana 5 en el aula virtual.",
    List("like", "wow")
  ),
  Publicacion(
    11,
    "Raúl",
    "Me parece injusto el sistema de evaluación del laboratorio.",
    List("angry", "angry", "like")
  ),
  Publicacion(
    12,
    "Patricia",
    "Gracias por todas las sugerencias de recursos, ¡me han servido mucho!",
    List("love", "love", "love", "wow", "like", "like", "haha")
  )
)
case class PublicacionConPuntaje(pub: Publicacion, puntaje: Int)
object RedSocial {

  // 🎯 Estructura de datos auxiliar: Mapeo de Reacción a Puntaje
  val PUNTUACIONES_REACCION: Map[String, Int] = Map(
    "like" -> 1,
    "love" -> 3,
    "wow" -> 2,
    "haha" -> 1,
    "angry" -> -1
  )

  /*
   Función mejorada para calcular el puntaje total de una publicación.
   Utiliza .map y .sum sobre la lista de reacciones.
   */
  def puntajePub(publicacion: Publicacion): Int = {
    publicacion.reacciones
      // Mapea cada reacción a su puntaje. getOrElse(reaccion, 0) asigna 0
      // si una reacción no está definida en el mapa (útil para el futuro).
      .map(reaccion => PUNTUACIONES_REACCION.getOrElse(reaccion, 0))
      // Suma todos los puntajes.
      .sum
  }

  /* Función principal que filtra, calcula puntajes y encuentra la publicación máxima. */
  def masImpactante(publicaciones: List[Publicacion], minLongitud: Int): PublicacionConPuntaje = {
    publicaciones
      // 1. FILTRAR por longitud mínima.
      .filter(_.texto.length >= minLongitud)
      // 2. MAPEAR: Convierte a PublicacionConPuntaje, calculando el puntaje.
      .map(pub => PublicacionConPuntaje(pub, puntajePub(pub)))
      // 3. ENCONTRAR MÁXIMO (REDUCCIÓN).
      .maxBy(_.puntaje)
  }
}

@main
def main(): Unit = {
    val resultado = RedSocial.masImpactante(publicaciones, 30)
    println(s"La publicación más impactante es de: ${resultado.pub.autor}")
    println(s"Texto: ${resultado.pub.texto}")
    println(s"Puntaje total: ${resultado.puntaje}")
  }



