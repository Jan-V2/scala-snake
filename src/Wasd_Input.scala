import java.awt.event.{ActionEvent, KeyEvent, KeyListener}
import javax.swing.{AbstractAction, JComponent, JPanel, KeyStroke}

abstract class Wasd_Input(val input_adaptor:JPanel) extends directions {

  map_dir_to_key("typed w", drcts.up)
  map_dir_to_key("typed s", drcts.down)
  map_dir_to_key("typed a", drcts.left)
  map_dir_to_key("typed d", drcts.right)

  def map_dir_to_key(keystroke_string: String, dir: WASD_Direction): Unit = {
    // With this method remapping a key will overwrite it/cause exception.
    input_adaptor.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(keystroke_string), keystroke_string)
    input_adaptor.getActionMap.put(keystroke_string, new Wasd_Action(dir))
  }

  private class Wasd_Action(val direction: WASD_Direction) extends AbstractAction {
    override def actionPerformed(actionEvent: ActionEvent): Unit = {
      on_keyboard_input(direction)
    }
  }

  def on_keyboard_input(direction: WASD_Direction)
}



trait directions{
  val drcts = new Inputs
  class Inputs{
    val up = new WASD_Direction(0)
    val down = new WASD_Direction(1)
    val left = new WASD_Direction(2)
    val right = new WASD_Direction(3)
  }

  def reverse_direction(direct: WASD_Direction): WASD_Direction ={
    direct match {
      case drcts.up => drcts.down
      case drcts.down => drcts.up
      case drcts.left => drcts.right
      case _ => drcts.left
    }
  }
  def print_direction(direct: WASD_Direction): Unit ={
    direct match {
      case drcts.up => println("up")
      case drcts.down => println("down")
      case drcts.left => println("left")
      case _ => println("right")
    }
  }
}


trait add_coord_class{
  class Coord(val x:Int, val y:Int){
    def same_as(coord:Coord): Boolean ={
      this.x == coord.x && this.y == coord.y
    }
  }
}

class WASD_Direction(val i:Int) extends AnyVal