import scala.io.Source


object Script extends App {

    // Fichier d'input
    val instructions = io.Source.fromResource("Instruction.txt").getLines.toList
    // Position initiale et instruction de la tondeuse 1
    var Tondeuse_1 = instructions.drop(1).patch(2, Nil, 2)
    // Position initiale et instruction de la tondeuse 2
    var Tondeuse_2 = instructions.patch(0, Nil, 3)



  def Deplacement_Tondeuse(Tondeuse: List[String]) = {

    /*
    OBJECTIF :  Cette fonction a pour but de faire avancer une tondeuse selon une liste d'instructions
    préalablement donnée par l'utilisateur. Cette liste doit être dans les fichiers de l'utilisateur,
    Elle donne ainsi la position initiale de la tondeuse, puis lui indique une succession de mouvements

    INPUT : Indique le nom de la tondeuse, ie. tondeuse1 ou tondeuse2

    PARAMETERS :
          (xmax, ymax): taille maximale de la pelouse
          (xmin, ymin) : taille minimale de la pelouse
          (position_x, position_y :)  Récupère dans le fichier txt, les coordoonées X et Y de la position initiale
          orientation_cardinale : Récupère dans le fichier txt, l'orientation cardinale de la tondeuse ( N S E W )
          parcours : Récupère dans le fichier txt, le parcours d'instruction de la tondeuse

          step : Ce paramètre est le plus délicat. Consiste à définir l'orientation cardinale de la tondeuse
          selon les instructions droite, gauche ou avancé définis par l'utilisateur.

            Boucle For : Boucle qui itère sur la séquence d'instruction du fichier txt. Va faire
            avancer la tondeuse, et retourne à chaque fois sa nouvelle position, jusqu'à ce que cette
            dernière ne rencontre plus d'instructions.

    OUTPUT : La position finale de la tondeuse après son parcours d'instructions
    */

    // Definis la taille maximale de la pelouse
    val (xmax, ymax) = (instructions(0).split(" ")(0).toInt, instructions(0).split(" ")(1).toInt)
    println(s"Le terrain est de taille maximale : ${xmax} x ${ymax}")
    // Definis les positions x,y des tondeuses
    var (position_x, position_y) = (Tondeuse(0).split(" ")(0).toInt, Tondeuse(0).split(" ")(1).toInt)
    // Definis la taille minimale de la pelouse
    val (xmin, ymin) = (0,0)

    // Définis l'orientation cardinale ( N S E W )
    var orientation_cardinal: String = Tondeuse(0).split(" ")(2).toString
    // Définis le trajet
    val parcours = Tondeuse(1).toList.map(_.toString)

    if (Tondeuse == Tondeuse_1)
    {println(s"La position initiale de la tondeuse  1 est en case : ${position_x} - ${position_y} - ${orientation_cardinal} . Elle commence son mouvement." )}
    else
    { println(s"La position initiale de la tondeuse  2 est en case : ${position_x} - ${position_y} - ${orientation_cardinal} . Elle commence son mouvement." ) }

    println(s"Sa séquence d'instruction est : ${parcours}" )
    for (step <- parcours)
    {

      // Permet de coder l'orientation Nud-Sud-Est-Oust selon l'orientation à droite
      if (step == "D")
      {
        if (orientation_cardinal == "E") {orientation_cardinal = "S"}
        else if (orientation_cardinal == "O") {orientation_cardinal = "N"}
        else if (orientation_cardinal == "N") {orientation_cardinal = "E"}
        else if (orientation_cardinal == "S") {orientation_cardinal = "O"}
        else {orientation_cardinal = "Instruction inconnu, verifiez le fichier txt"}
      }

      // Permet de coder l'orientation Nud-Sud-Est-Oust selon l'orientation à gauche
      else if (step == "G")
      {
        if (orientation_cardinal == "E") {orientation_cardinal = "N"}
        else if (orientation_cardinal == "O") {orientation_cardinal = "S"}
        else if (orientation_cardinal == "N") {orientation_cardinal = "O"}
        else if (orientation_cardinal == "S") {orientation_cardinal = "E"}
        else {orientation_cardinal = "Instruction inconnu, verifiez le fichier txt"}
      }

      // Permet de coder les mouvements de la tondeuse
      else if (step == "A")
      {
        if (orientation_cardinal == "N" & position_y < ymax) {position_y = position_y + 1}
        else if (orientation_cardinal == "O" & position_x > xmin) {position_x = position_x - 1}
        else if (orientation_cardinal == "E" & position_x < xmax) {position_x = position_x + 1}
        else if (orientation_cardinal == "S" & position_y > ymin) {position_y = position_y - 1}
        else {orientation_cardinal = "Instruction inconnu, verifiez le fichier txt"}
      }

      println(s"Nouvelle position : ${position_x} - ${position_y} - ${orientation_cardinal}")
    }

    println(s"La tondeuse a terminé son déplacement. Elle se situe en case : ${position_x} - ${position_y} - ${orientation_cardinal}")
  }

  // Appel fonction
  Deplacement_Tondeuse(Tondeuse_1)
  Deplacement_Tondeuse(Tondeuse_2)
}
