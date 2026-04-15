//> using scala 3.3.7
//> using dep xyz.matthieucourt::layoutz:0.7.0

import layoutz._


case class V3(x: Double, y: Double, z: Double):
  def +(o: V3): V3     = V3(x + o.x, y + o.y, z + o.z)
  def -(o: V3): V3     = V3(x - o.x, y - o.y, z - o.z)
  def *(s: Double): V3  = V3(x * s, y * s, z * s)
  def dot(o: V3): Double = x * o.x + y * o.y + z * o.z
  def cross(o: V3): V3  = V3(y * o.z - z * o.y, z * o.x - x * o.z, x * o.y - y * o.x)
  def length: Double     = math.sqrt(this dot this)
  def normalized: V3     = { val l = length; if l < 1e-10 then V3.Zero else this * (1.0 / l) }
  def unary_- : V3      = V3(-x, -y, -z)

object V3:
  val Zero    = V3(0, 0, 0)
  val Up      = V3(0, 1, 0)
  val Forward = V3(0, 0, 1)


case class Pixel(ch: Char, r: Int, g: Int, b: Int)


case class RayState(
    theta: Double,
    phi: Double,
    dist: Double,
    morph: Double,
    morphTarget: Double,
    autoRotate: Boolean,
    tick: Int,
    frameNs: List[Long]
)

object RayState:
  def apply(): RayState =
    RayState(0.0, 0.35, 3.8, 0.0, 0.0, true, 0, List.fill(30)(80000000L))

sealed trait RayMsg
case object RTick extends RayMsg
case object RotL extends RayMsg
case object RotR extends RayMsg
case object RotU extends RayMsg
case object RotD extends RayMsg
case object ZoomIn extends RayMsg
case object ZoomOut extends RayMsg
case object ToggleAuto extends RayMsg
case object NextShape extends RayMsg

object RayMarcher extends LayoutzApp[RayState, RayMsg]:

  private val W = 60
  private val H = 28

  private val MaxSteps  = 50
  private val MaxDist   = 20.0
  private val Eps       = 0.005
  private val MorphSpeed = 0.06

  private val Ramp = " .'`^\",:;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"
  private val ShapeNames = Vector("sphere", "torus", "cube")

  private def clamp(x: Double, lo: Double, hi: Double): Double = math.max(lo, math.min(hi, x))
  private def mix(a: Double, b: Double, t: Double): Double = a * (1.0 - t) + b * t
  private def smoothstep(lo: Double, hi: Double, x: Double): Double =
    val t = clamp((x - lo) / (hi - lo), 0.0, 1.0); t * t * (3.0 - 2.0 * t)

  private def sdSphere(p: V3, r: Double): Double =
    p.length - r

  private def sdTorus(p: V3, R: Double, r: Double): Double =
    val qx = math.sqrt(p.x * p.x + p.z * p.z) - R
    math.sqrt(qx * qx + p.y * p.y) - r

  private def sdRoundBox(p: V3, b: V3, r: Double): Double =
    val q = V3(math.abs(p.x) - b.x, math.abs(p.y) - b.y, math.abs(p.z) - b.z)
    val outer = V3(math.max(q.x, 0), math.max(q.y, 0), math.max(q.z, 0)).length
    outer + math.min(math.max(q.x, math.max(q.y, q.z)), 0.0) - r

  private def scene(p: V3, morph: Double): Double =
    val t      = ((morph % 3.0) + 3.0) % 3.0
    val sphere = sdSphere(p, 1.05)
    val torus  = sdTorus(p, 0.9, 0.38)
    val cube   = sdRoundBox(p, V3(0.72, 0.72, 0.72), 0.12)
    if t < 1.0      then mix(sphere, torus, smoothstep(0, 1, t))
    else if t < 2.0 then mix(torus, cube,   smoothstep(0, 1, t - 1))
    else                  mix(cube, sphere,  smoothstep(0, 1, t - 2))

  private def calcNormal(p: V3, morph: Double): V3 =
    val e = 0.001
    V3(
      scene(V3(p.x + e, p.y, p.z), morph) - scene(V3(p.x - e, p.y, p.z), morph),
      scene(V3(p.x, p.y + e, p.z), morph) - scene(V3(p.x, p.y - e, p.z), morph),
      scene(V3(p.x, p.y, p.z + e), morph) - scene(V3(p.x, p.y, p.z - e), morph)
    ).normalized

  @annotation.tailrec
  private def march(ro: V3, rd: V3, morph: Double, t: Double = 0.0, i: Int = 0): Double =
    if i >= MaxSteps || t >= MaxDist then -1.0
    else
      val d = scene(ro + rd * t, morph)
      if d < Eps then t else march(ro, rd, morph, t + d, i + 1)

  private val LightDir = V3(0.8, 1.0, -0.6).normalized

  private def shade(ro: V3, rd: V3, morph: Double): Pixel =
    val t = march(ro, rd, morph)
    if t < 0 then return bgPixel(rd)

    val hit  = ro + rd * t
    val n    = calcNormal(hit, morph)
    val diff = math.max(n dot LightDir, 0.0)
    val refl = n * (2.0 * (n dot LightDir)) - LightDir
    val spec = math.pow(math.max(refl dot -rd, 0.0), 32.0) * 0.6
    val ao   = 1.0 - clamp(scene(hit + n * 0.1, morph) * 5.0, 0.0, 0.4)
    val lum  = clamp((0.08 + diff * 0.85 + spec) * ao, 0.0, 1.0)

    val ch = Ramp(clamp(lum * (Ramp.length - 1), 0, Ramp.length - 1).toInt)

    val nx = n.x * 0.5 + 0.5; val ny = n.y * 0.5 + 0.5; val nz = n.z * 0.5 + 0.5
    Pixel(
      ch,
      clamp(( nx * 0.55 + lum * 0.45) * 235 + 20, 0, 255).toInt,
      clamp(( ny * 0.45 + lum * 0.55) * 215 + 15, 0, 255).toInt,
      clamp(( nz * 0.50 + lum * 0.50 + 0.05) * 200 + 30, 0, 255).toInt
    )

  private def bgPixel(rd: V3): Pixel =
    val vy  = rd.y * 0.5 + 0.5
    val bg  = (18.0 - (1.0 - vy) * 8.0).max(4.0).toInt
    Pixel(' ', bg, bg, bg + 6)

  case class FrameBuffer(pixels: Array[Pixel], w: Int, h: Int) extends Element:
    def render: String =
      val sb = new java.lang.StringBuilder(w * h * 20)
      var i = 0; var y = 0
      while y < h do
        var x = 0
        while x < w do
          val p = pixels(i)
          sb.append("\u001b[38;2;").append(p.r).append(';').append(p.g).append(';').append(p.b).append('m').append(p.ch)
          i += 1; x += 1
        sb.append("\u001b[0m")
        if y < h - 1 then sb.append('\n')
        y += 1
      sb.toString

  private def renderFrame(s: RayState): FrameBuffer =
    val ro    = V3(
      s.dist * math.sin(s.theta) * math.cos(s.phi),
      s.dist * math.sin(s.phi),
      s.dist * math.cos(s.theta) * math.cos(s.phi)
    )
    val fwd   = (-ro).normalized
    val right = (fwd cross V3.Up).normalized
    val up    = right cross fwd
    val aspect = W.toDouble / H.toDouble * 0.48

    val pixels = new Array[Pixel](W * H)
    var i = 0; var py = 0
    while py < H do
      val v = 0.5 - py.toDouble / H
      var px = 0
      while px < W do
        val u  = (px.toDouble / W - 0.5) * aspect
        val rd = (fwd + right * u + up * v).normalized
        pixels(i) = shade(ro, rd, s.morph)
        i += 1; px += 1
      py += 1
    FrameBuffer(pixels, W, H)

  def init: (RayState, Cmd[RayMsg]) =
    (RayState(), Cmd.setTitle("layoutz · Ray Marcher"))

  def update(msg: RayMsg, s: RayState): (RayState, Cmd[RayMsg]) = msg match
    case RTick =>
      val now      = System.nanoTime()
      val newTheta = if s.autoRotate then s.theta + 0.035 else s.theta
      val diff     = s.morphTarget - s.morph
      val newMorph = if math.abs(diff) < 0.01 then s.morphTarget else s.morph + diff * MorphSpeed
      val ft       = (s.frameNs :+ (System.nanoTime() - now)).takeRight(30)
      s.copy(theta = newTheta, morph = newMorph, tick = s.tick + 1, frameNs = ft)

    case RotL  => s.copy(theta = s.theta - 0.15, autoRotate = false)
    case RotR  => s.copy(theta = s.theta + 0.15, autoRotate = false)
    case RotU  => s.copy(phi = math.min(s.phi + 0.1, 1.3))
    case RotD  => s.copy(phi = math.max(s.phi - 0.1, -1.3))
    case ZoomIn  => s.copy(dist = math.max(s.dist - 0.25, 2.0))
    case ZoomOut => s.copy(dist = math.min(s.dist + 0.25, 8.0))
    case ToggleAuto => s.copy(autoRotate = !s.autoRotate)
    case NextShape  =>
      val next = (math.round(s.morphTarget).toInt + 1) % 3
      s.copy(morphTarget = next.toDouble)

  def subscriptions(s: RayState): Sub[RayMsg] = Sub.batch(
    Sub.time.everyMs(80, RTick),
    Sub.onKeyPress {
      case Key.Left      => Some(RotL)
      case Key.Right     => Some(RotR)
      case Key.Up        => Some(RotU)
      case Key.Down      => Some(RotD)
      case Key.Char('+') => Some(ZoomIn)
      case Key.Char('-') => Some(ZoomOut)
      case Key.Char('a') => Some(ToggleAuto)
      case Key.Char('m') => Some(NextShape)
      case _             => None
    }
  )

  def view(s: RayState): Element =
    val fb        = renderFrame(s)
    val targetIdx = (math.round(s.morphTarget).toInt % 3 + 3) % 3
    val settled   = math.abs(s.morph - s.morphTarget) < 0.02
    val shapeName =
      if settled then ShapeNames(targetIdx)
      else
        val fromIdx = (math.round(s.morph).toInt % 3 + 3) % 3
        s"${ShapeNames(fromIdx)} → ${ShapeNames(targetIdx)}"

    val sparkData = s.frameNs.map(ns => (ns / 1e6).max(0.1))

    columns(
      layout(
        "Ray Marcher".style(Style.Bold).color(Color.BrightCyan),
        fb
      ),
      box("Camera")(
        leftAlign(layout(
          kv(
            "θ" -> f"${s.theta % (2 * math.Pi)}%.2f",
            "φ" -> f"${s.phi}%.2f",
            "zoom" -> f"${s.dist}%.1f"
          ).color(Color.BrightBlue),
          br,
          rowTight(
            "rotate: ".color(Color.BrightBlack),
            if s.autoRotate then "auto".color(Color.BrightGreen).style(Style.Bold)
            else "manual".color(Color.BrightYellow)
          ),
          br,
          s"~12 fps".color(Color.BrightYellow),
          sparkline(sparkData).color(Color.BrightCyan),
          br,
          kv("shape" -> shapeName).color(Color.BrightMagenta),
          br,
          spinner("render", s.tick / 2, SpinnerStyle.Dots).color(Color.BrightCyan),
          spinner("scene", s.tick / 3, SpinnerStyle.Earth).color(Color.BrightGreen),
          spinner("light", s.tick / 2, SpinnerStyle.Moon).color(Color.BrightYellow),
          br,
          layout(
            "←→↑↓  orbit".color(Color.BrightYellow),
            "+/-    zoom".color(Color.BrightYellow),
            "m      shape".color(Color.BrightYellow),
            "a      auto".color(Color.BrightYellow)
          ).style(Style.Dim)
        ), 22)
      ).border(Border.Round).color(Color.BrightMagenta)
    )

object RayMarcherRunner:
  def main(args: Array[String]): Unit =
    RayMarcher.run(quitKey = Key.Ctrl('Q'))
