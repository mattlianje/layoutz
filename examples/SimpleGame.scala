import layoutz._
import scala.util.Random

case class Enemy(x: Int, y: Int)

case class GameState(
    playerX: Int,
    playerY: Int,
    items: Set[(Int, Int)],
    score: Int,
    gameWidth: Int,
    gameHeight: Int,
    message: String,
    gameOver: Boolean,
    level: Int,
    enemies: List[Enemy] = List.empty,
    lives: Int = 3,
    tickCount: Int = 0
)

sealed trait GameMessage
case object GameMoveUp extends GameMessage
case object GameMoveDown extends GameMessage
case object GameMoveLeft extends GameMessage
case object GameMoveRight extends GameMessage
case object RestartGame extends GameMessage
case object GameTick extends GameMessage

/** Simple game - collect gems, avoid enemies */
object SimpleGame extends LayoutzApp[GameState, GameMessage] {

  private val GAME_WIDTH = 15
  private val GAME_HEIGHT = 10
  private val random = new Random()

  private def generateItems(level: Int): Set[(Int, Int)] = {
    val itemCount = 3 + level
    (1 to itemCount).map { _ =>
      (random.nextInt(GAME_WIDTH), random.nextInt(GAME_HEIGHT))
    }.toSet
  }

  private def generateEnemies(level: Int): List[Enemy] = {
    val enemyCount = 1 + level
    (1 to enemyCount).map { _ =>
      Enemy(
        x = random.nextInt(GAME_WIDTH),
        y = random.nextInt(GAME_HEIGHT)
      )
    }.toList
  }

  def init = GameState(
    playerX = GAME_WIDTH / 2,
    playerY = GAME_HEIGHT / 2,
    items = generateItems(1),
    score = 0,
    gameWidth = GAME_WIDTH,
    gameHeight = GAME_HEIGHT,
    message = "Collect all gems! Avoid the enemies!",
    gameOver = false,
    level = 1,
    enemies = generateEnemies(1),
    lives = 3,
    tickCount = 0
  )

  def update(msg: GameMessage, state: GameState): GameState = {
    if (state.gameOver && msg != RestartGame) return state

    msg match {
      case GameMoveUp =>
        val newY = math.max(0, state.playerY - 1)
        processPlayerMove(state.copy(playerY = newY))

      case GameMoveDown =>
        val newY = math.min(state.gameHeight - 1, state.playerY + 1)
        processPlayerMove(state.copy(playerY = newY))

      case GameMoveLeft =>
        val newX = math.max(0, state.playerX - 1)
        processPlayerMove(state.copy(playerX = newX))

      case GameMoveRight =>
        val newX = math.min(state.gameWidth - 1, state.playerX + 1)
        processPlayerMove(state.copy(playerX = newX))

      case RestartGame =>
        init

      case GameTick =>
        updateGameTick(state)
    }
  }

  private def processPlayerMove(state: GameState): GameState = {
    val playerPos = (state.playerX, state.playerY)

    if (state.items.contains(playerPos)) {
      val newItems = state.items - playerPos
      val newScore = state.score + 10

      if (newItems.isEmpty) {
        val newLevel = state.level + 1
        state.copy(
          items = generateItems(newLevel),
          score = newScore,
          level = newLevel,
          enemies = generateEnemies(newLevel),
          message = s"Level $newLevel! More enemies!"
        )
      } else {
        state.copy(
          items = newItems,
          score = newScore,
          message = s"Score: $newScore | Gems left: ${newItems.size}"
        )
      }
    } else {
      state
    }
  }

  private def updateGameTick(state: GameState): GameState = {
    val newTick = state.tickCount + 1

    val newEnemies = if (newTick % 4 == 0) {
      state.enemies.map(moveEnemy(_, state))
    } else {
      state.enemies
    }

    // Check if player hit by enemy
    val hitByEnemy =
      newEnemies.exists(e => e.x == state.playerX && e.y == state.playerY)

    if (hitByEnemy) {
      val newLives = state.lives - 1
      if (newLives <= 0) {
        state.copy(
          tickCount = newTick,
          enemies = newEnemies,
          lives = 0,
          gameOver = true,
          message = "💀 Game Over! Press R to restart"
        )
      } else {
        state.copy(
          tickCount = newTick,
          enemies = newEnemies,
          lives = newLives,
          message = s"💥 Hit! Lives left: $newLives"
        )
      }
    } else {
      state.copy(
        tickCount = newTick,
        enemies = newEnemies,
        message =
          s"Score: ${state.score} | Lives: ${state.lives} | Level: ${state.level}"
      )
    }
  }

  private def moveEnemy(enemy: Enemy, state: GameState): Enemy = {
    /* Simple chasing */
    val dx =
      if (state.playerX > enemy.x) 1 else if (state.playerX < enemy.x) -1 else 0
    val dy =
      if (state.playerY > enemy.y) 1 else if (state.playerY < enemy.y) -1 else 0

    val newX = math.max(0, math.min(state.gameWidth - 1, enemy.x + dx))
    val newY = math.max(0, math.min(state.gameHeight - 1, enemy.y + dy))

    enemy.copy(x = newX, y = newY)
  }

  def onKey(k: Key): Option[GameMessage] = k match {
    case Tick => Some(GameTick)

    case CharKey('w') | CharKey('W') | ArrowUpKey    => Some(GameMoveUp)
    case CharKey('s') | CharKey('S') | ArrowDownKey  => Some(GameMoveDown)
    case CharKey('a') | CharKey('A') | ArrowLeftKey  => Some(GameMoveLeft)
    case CharKey('d') | CharKey('D') | ArrowRightKey => Some(GameMoveRight)

    case CharKey('r') | CharKey('R') => Some(RestartGame)

    case _ => None
  }

  def view(state: GameState): Element = {
    val gameBoard = (0 until state.gameHeight).map { y =>
      val row = (0 until state.gameWidth)
        .map { x =>
          if (x == state.playerX && y == state.playerY) {
            "🧙"
          } else if (state.enemies.exists(e => e.x == x && e.y == y)) {
            "👹"
          } else if (state.items.contains((x, y))) {
            "💎"
          } else {
            "⬜"
          }
        }
        .mkString(" ")
      Text(row)
    }

    val stats = layout(
      s"Score: ${state.score}",
      s"Lives: ${"💖" * state.lives}",
      s"Level: ${state.level}",
      s"Gems left: ${state.items.size}"
    )

    val gameOverSection = if (state.gameOver) {
      section("💀 Game Over")(
        layout(
          s"Final Score: ${state.score}",
          s"Reached Level: ${state.level}",
          "Press R to restart!"
        )
      )
    } else {
      layout("")
    }

    layout(
      section("🎮 Gem Collector")(
        Layout(gameBoard.toList)
      ),
      br,
      section("Stats")(stats),
      br,
      section("Status")(
        layout(state.message)
      ),
      gameOverSection,
      br,
      section("Controls")(
        ul(
          "🧙 You | 👹 Enemy | 💎 Gems",
          "WASD or Arrow Keys - Move",
          "R - Restart game"
        )
      )
    )
  }
}
