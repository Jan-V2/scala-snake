
import java.awt.event._
import java.awt.{Color, Dimension, Font, Graphics}
import java.util
import javax.swing._

import scala.swing._
import scala.util.Random

//movement isn't consistent but i'm not sure what i could do about it.

object GuiProgramOne {
  def main(args: Array[String]) {
    SwingUtilities.invokeLater(() => { create_and_show_gui() })
  }

  def create_and_show_gui(): Unit ={
    var f = new JFrame("testing")
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    //f.setSize(500, 500)
    val canvas = new Snake_Game(500, false, false)
    f.add(canvas)
    f.pack()
    f.setResizable(false)
    f.setVisible(true)
  }

}


class Snake_Game(private val _size:Int, val self_playing: Boolean, val hidden: Boolean) extends JPanel  with add_coord_class with self_playing_snake {

  val square_size: Int = _size / 20
  private val squares_x = (_size / square_size) -2
  private val squares_y = (_size / square_size) -2
  val snake = new Snake
  val apple = new Apple
  val input = new Input(this)
  private var direct_current = input.drcts.left
  private var direct_last = direct_current
  this.setBackground(Color.black)
  override val self_player = new Self_Player(squares_x, squares_y)

  var game_ended = false
  val time_between_frames = 1000
  var time_acc = 0
  val hidden_frames_skipped = 15
  var hidden_frames_acc = 0
  var time_acc_per_frame = 25
  if (self_playing){
    time_acc_per_frame = 5
  }
  val frame_timer = new Timer(25, (_: ActionEvent) => {
    if(!game_ended){
      time_acc += 1
      if (time_acc > time_acc_per_frame){
        time_acc = 0
        if (self_playing){
          direct_current = self_player.get_next_dir(snake.pos, direct_current)
        }
        snake.update()
        if (hidden){
          if (hidden_frames_acc % hidden_frames_skipped == 0){
            draw_frame()
          }
          hidden_frames_acc += 1
        }else{
          draw_frame()
        }
      }
    }
  })
  frame_timer.start()



  def paint_square(g:Graphics, coord:Coord): Unit ={
    g.fillRect(square_size + square_size * coord.x,
      square_size + square_size * coord.y,
      square_size, square_size)
  }


  override def getPreferredSize: Dimension = {new Dimension(_size, _size)}

  override def paintComponent(g: Graphics): Unit = {
    draw_frame(g)
  }

  def draw_frame(g: Graphics = getGraphics): Unit = {
    super.paintComponent(g)
    def draw_border(): Unit ={
      val w = this.getWidth
      val h = this.getHeight

      g.setColor(Color.darkGray)
      g.fillRect(0, 0, w, square_size)
      g.fillRect(0, 0, square_size, h)
      g.fillRect(0, h - square_size, w, square_size)
      g.fillRect(w - square_size, 0, square_size, h)
    }

    def draw_lost_text(): Unit =  {
      val lost_string = "You lost"
      val font = new Font(Font.SERIF, Font.PLAIN, 100)
      val metrics = g.getFontMetrics(font)
      val x = _size /2 - metrics.stringWidth(lost_string) /2
      val y = _size/2 - metrics.getHeight /2 + metrics.getAscent
      g.setColor(Color.white)
      g.setFont(font)
      g.drawString(lost_string , x, y)
    }


    draw_border()
    snake.draw(g)
    apple.draw(g)

    if (game_ended){
      draw_lost_text()
    }
  }


  def game_lost(): Unit ={
    game_ended = true
    draw_frame()
  }


  class Snake{
    val coords: util.ArrayList[Coord] = init_snake()
    var pos: Coord = coords.get(0)


    private def init_snake():util.ArrayList[Coord] = {
      var ret = new util.ArrayList[Coord]
      val default_length = 4
      val y_pos = squares_y / 2
      val x_pos = (squares_y /2)  + default_length / 2
      for(offset <- 0 to default_length){
        val new_coord = new Coord(x_pos - offset, y_pos)
        ret.add(0, new_coord)
      }
      ret
    }

    def update(): Unit ={
      def check_collision(new_pos: Coord): Boolean ={
        if(!(new_pos.x > -1 && new_pos.x < squares_x && new_pos.y > -1 && new_pos.y < squares_y)){return false}
        coords.forEach( (e) => {
          if(e.same_as(new_pos)){
            return false
          }
        })
        true
      }

      var new_coord = new Coord(0,0)
      direct_current match {
        case input.drcts.left  =>new_coord = new Coord(pos.x -1, pos.y)
        case input.drcts.right =>new_coord = new Coord(pos.x+1, pos.y)
        case input.drcts.up    =>new_coord = new Coord(pos.x, pos.y-1)
        case _ =>new_coord = new Coord(pos.x, pos.y+1)
      }
      if(check_collision(new_coord)){
        direct_last = direct_current
        pos = new_coord
        coords.add(0, new_coord)
        if (pos.same_as(apple.coord)){
          apple.reseed()
          if (!self_playing){
            time_acc_per_frame -= 1
          }
        }else{
          coords.remove(coords.size()-1)
        }

      }else{
        game_lost()
      }
    }

    def draw(g :Graphics): Unit ={
      g.setColor(Color.red)//cus it's scala
      coords.forEach((coord) => {
        paint_square(g, coord)
      })
    }
  }

  class Apple(){
    private val random = new Random
    var coord:Coord = get_random_free_coord()

    private def get_random_free_coord(): Coord ={
      val x = random.nextInt(squares_x)
      val y = random.nextInt(squares_y)
      val new_coord = new Coord(x, y)
      snake.coords.forEach( (e) => {
        if(new_coord.same_as(e)){
          println("same")
          return get_random_free_coord()
        }
      })
      new_coord
    }

    def reseed(): Unit ={coord = get_random_free_coord()}

    def draw(g:Graphics): Unit ={
      g.setColor(Color.green)
      paint_square(g, coord)

    }
  }

  class Input(this_panel: JPanel) extends Wasd_Input(this_panel){

    override def on_keyboard_input(direction: WASD_Direction): Unit = {
      if (!self_playing){
        if (direction != reverse_direction(direct_last)){
          direct_current = direction
        }
      }
    }
  }


}


trait self_playing_snake extends directions with add_coord_class{
  val self_player: Self_Player
  class Self_Player(x_size: Int, y_size: Int){
    private val x_edge = x_size -1
    private val y_edge = y_size -1
    private val hit_x_edge  = { (x: Int) => x == x_edge || x == 0 }
    private val hit_y_edge  = { (y: Int) => y == y_edge || y == 0}


    def get_next_dir(coord: Coord, current_direct: WASD_Direction): WASD_Direction ={
      if (hit_x_edge(coord.x) && hit_y_edge(coord.y)){
        if (coord.y == 0){
          if (current_direct == drcts.up){
            if (coord.x == 0){
              return drcts.right
            } else{
              return drcts.left
            }
          }
        } else{
          if (current_direct != drcts.down){
            return drcts.up
          }
        }
      }
      if (hit_x_edge(coord.x) && current_direct != drcts.up){
        if ( current_direct != drcts.down){
          return drcts.down
        }
        if (coord.x == 0){
          return drcts.right
        }else{
          return drcts.left
        }
      }
      current_direct
    }
  }

}